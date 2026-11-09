#!/usr/bin/env python3
"""Run one command under a time budget, owning its whole process group (lib/bounded.sh's runner).

  bounded_exec.py --timeout SECS --kill-after SECS --status FILE -- CMD [ARGS...]

The command inherits stdin, stdout and stderr (the caller redirects them) and runs as the leader of
a new session, so it and every descendant that does not create its own session share one process
group. The outcome goes to FILE, never to the command's streams, so nothing the command prints can
fake or hide it:
  exit N      the command finished with status N (128+S when killed by signal S, as a shell reports)
  timeout     the budget expired: the group got SIGTERM, then, KILL_AFTER seconds later, SIGKILL,
              whether or not the leader had already exited, so a descendant that ignores SIGTERM
              does not outlive the attempt.
Completed attempts also reap their remaining groups; no child may retain a host slot.
Both durations must be finite numbers, the budget positive and the grace nonnegative.
This process exits 0 once FILE is written and 2 on a usage error. If it is itself cancelled
(SIGTERM, SIGHUP, SIGINT), it kills the command's group, writes no outcome, and exits 128+signal.
"""
import math
import os
import signal
import subprocess
import sys
import time


class Cancelled(Exception):
    def __init__(self, sig):
        super().__init__(sig)
        self.sig = sig


def killpg(pgid, sig):
    try:
        os.killpg(pgid, sig)
    except (ProcessLookupError, PermissionError):
        pass


def group_alive(pgid):
    try:
        os.killpg(pgid, 0)
        return True
    except ProcessLookupError:
        return False
    except PermissionError:
        return True


def duration(text, positive):
    try:
        v = float(text)
    except ValueError:
        return None
    if not math.isfinite(v) or v < 0 or (positive and v == 0):
        return None
    return v


def usage(msg):
    print(f"bounded_exec: {msg}\n{__doc__}", file=sys.stderr)
    return 2


# A private authenticated broker is the sole lifecycle owner for a command tree.
# Nested runners are clients: they never create a supervisor/session of their own.
# Descriptors travel over the Unix socket, never through ambient descriptor numbers.
TREE_KEYS = ('BOUNDED_TREE_SOCKET', 'BOUNDED_TREE_KEY', 'BOUNDED_TREE_REQUEST')


def _client(cmd, budget, kill_after, on_started, deadline, options, environment):
    from multiprocessing.connection import Client
    from multiprocessing.reduction import send_handle
    address, key, parent = (environment[k] for k in TREE_KEYS)
    with Client(address, family='AF_UNIX', authkey=bytes.fromhex(key)) as connection:
        options = dict(options)
        options['env'] = dict(environment)
        if 'cwd' not in options: options['cwd'] = os.getcwd()
        if 'umask' not in options:
            mask = os.umask(0); os.umask(mask)
            options['umask'] = mask
        descriptors = []
        for field, default in (('stdin', 0), ('stdout', 1), ('stderr', 2)):
            stream = options.get(field)
            if stream is None: stream = default
            if isinstance(stream, int) and stream < 0: continue
            if stream is not None and stream != subprocess.STDOUT:
                fd = stream if isinstance(stream, int) else stream.fileno()
                descriptors.append((field, fd))
                options.pop(field, None)
        inherited = options.pop('pass_fds', None)
        if inherited is None:
            inherited = ()
            slot = environment.get('CONCRETE_HEAVY_SLOT_FD', '')
            if slot.isdecimal():
                try: os.fstat(int(slot)); inherited = (int(slot),)
                except OSError: pass
        connection.send((cmd, budget, kill_after, deadline, options, parent,
                         [field for field, fd in descriptors], list(inherited)))
        for field, fd in descriptors: send_handle(connection, fd, 0)
        for fd in inherited: send_handle(connection, fd, 0)
        started = connection.recv()
        if on_started is not None and started is not None: on_started(started)
        return connection.recv()


class _Tree:
    def __init__(self, directory):
        from multiprocessing.connection import Listener
        import threading
        self.address = directory + '/owner'
        self.key = os.urandom(32)
        self.listener = Listener(self.address, family='AF_UNIX', authkey=self.key)
        self.listener._listener._socket.settimeout(.1)
        self.lock = threading.RLock()
        self.requests = {}
        self.groups = set()
        self.threads = set()
        self.stopping = False
        self.acceptor = threading.Thread(target=self.accept, daemon=True)
        self.acceptor.start()

    def accept(self):
        import threading
        while not self.stopping:
            try: connection = self.listener.accept()
            except TimeoutError: continue
            except (OSError, EOFError): return
            thread = threading.Thread(target=self.handle, args=(connection,), daemon=True)
            with self.lock: self.threads.add(thread)
            thread.start()

    def handle(self, connection):
        from multiprocessing.reduction import recv_handle
        handles = []
        outcome = None
        try:
            cmd, budget, grace, deadline, options, parent, fields, count = connection.recv()
            for field in fields:
                fd = recv_handle(connection); handles.append(fd); options[field] = fd
            for _ in count: handles.append(recv_handle(connection))
            inherited = handles[len(fields):]
            options['pass_fds'] = tuple(inherited)
            slot = options.get('env', {}).get('CONCRETE_HEAVY_SLOT_FD')
            if slot is not None and int(slot) in count:
                options['env']['CONCRETE_HEAVY_SLOT_FD'] = str(inherited[count.index(int(slot))])
            outcome = self.attempt(cmd, budget, grace, deadline, options, parent, connection)
        except (EOFError, BrokenPipeError, OSError):
            pass
        finally:
            # Completion is published only after reaping and closing the owner's
            # transferred copies, so success cannot race a still-held slot lock.
            for fd in handles: os.close(fd)
            try:
                if outcome is not None: connection.send(outcome)
            except (OSError, EOFError): pass
            connection.close()
            import threading
            with self.lock: self.threads.discard(threading.current_thread())

    def terminate(self, request, grace):
        with self.lock:
            selected = {request}
            while True:
                expanded = selected | {key for key, entry in self.requests.items() if entry[0] in selected}
                if expanded == selected: break
                selected = expanded
            entries = [entry for key, entry in self.requests.items() if key in selected]
            # Mark before signalling: no late request may escape cancellation.
            for entry in entries: entry[2].set()
        for parent, proc, cancelled in reversed(entries): killpg(proc.pid, signal.SIGTERM)
        end = time.monotonic() + grace
        while time.monotonic() < end:
            for entry in entries: entry[1].poll()
            self.reap_adopted()
            if not any(group_alive(e[1].pid) for e in entries): break
            time.sleep(.01)
        for parent, proc, cancelled in entries: killpg(proc.pid, signal.SIGKILL)
        end = time.monotonic() + 1
        while True:
            for entry in entries: entry[1].poll()
            self.reap_adopted()
            if not any(group_alive(e[1].pid) for e in entries): break
            if time.monotonic() >= end:
                raise RuntimeError('bounded_exec: process group not reaped')
            time.sleep(.005)

    def attempt(self, cmd, budget, grace, deadline, options, parent=None, connection=None, started=None):
        import threading
        request = os.urandom(16).hex()
        environment = dict(options.pop('env', os.environ))
        environment.update(zip(TREE_KEYS, (self.address, self.key.hex(), request)))
        options['env'] = environment
        if 'pass_fds' not in options:
            slot = environment.get('CONCRETE_HEAVY_SLOT_FD', '')
            if slot.isdecimal():
                try: os.fstat(int(slot)); options['pass_fds'] = (int(slot),)
                except OSError: pass
        cancelled = threading.Event()
        with self.lock:
            if self.stopping or (parent is not None and
                (parent not in self.requests or self.requests[parent][2].is_set())):
                if connection:
                    connection.send(None)
                return 'timeout'
            try:
                proc = subprocess.Popen(cmd, start_new_session=True, **options)
            except OSError as error:
                print(f'bounded_exec: cannot run {cmd[0]}: {error}', file=sys.stderr)
                if connection:
                    connection.send(None)
                return 'exit 127'
            self.requests[request] = [parent, proc, cancelled]
            self.groups.add(proc.pid)
        if started: started(proc.pid)
        if connection: connection.send(proc.pid)
        end = time.monotonic() + budget
        if deadline is not None: end = min(end, deadline)
        outcome = None
        try:
            while proc.poll() is None:
                disconnected = False
                if connection and connection.poll():
                    try: connection.recv()
                    except EOFError: disconnected = True
                if cancelled.is_set() or disconnected or time.monotonic() >= end:
                    self.terminate(request, grace)
                    proc.wait()
                    outcome = 'timeout'
                    break
                time.sleep(.005)
            if outcome is None:
                rc = proc.wait()
                outcome = f'exit {rc if rc >= 0 else 128 - rc}'
            return outcome
        finally:
            # Completed leaders cannot leave a background group or nested request
            # behind. This also releases every inherited heavy-slot description.
            self.terminate(request, grace)
            proc.wait()
            with self.lock: self.requests.pop(request, None)

    def reap_adopted(self):
        if sys.platform != 'linux': return
        from pathlib import Path
        with self.lock:
            leaders = {e[1].pid for e in self.requests.values()}
        for task in Path('/proc/self/task').glob('*/children'):
            try: children = task.read_text().split()
            except OSError: continue
            for value in children:
                pid = int(value)
                if pid in leaders: continue
                try:
                    if os.getpgid(pid) in self.groups: os.waitpid(pid, os.WNOHANG)
                except (ProcessLookupError, ChildProcessError): pass

    def close(self, grace):
        self.stopping = True
        with self.lock:
            roots = [key for key, entry in self.requests.items() if entry[0] is None]
        for request in roots: self.terminate(request, grace)
        self.listener.close()
        self.acceptor.join(timeout=.2)
        with self.lock: threads = list(self.threads)
        for thread in threads: thread.join(timeout=grace + 1)
        # Linux subreaper adoption lets the owner reap orphaned grandchildren,
        # including a same-group child whose leader was killed on cancellation.
        for pgid in self.groups:
            while True:
                try:
                    pid, status = os.waitpid(-pgid, os.WNOHANG)
                    if pid == 0: break
                except ChildProcessError: break


def supervise(cmd, budget, kill_after, *, on_started=None, deadline=None, **process_options):
    """One owner per tree; nested generic/scheduled attempts delegate over IPC."""
    import tempfile
    import threading
    environment = process_options.get('env', os.environ)
    if all(k in environment for k in TREE_KEYS):
        return _client(cmd, budget, kill_after, on_started, deadline, process_options, environment)
    # The root owns cancellation and every process group created by nested clients.
    previous = {}
    subreaper = None
    if sys.platform == 'linux':
        import ctypes
        library = ctypes.CDLL(None, use_errno=True)
        old = ctypes.c_int()
        if library.prctl(37, ctypes.byref(old), 0, 0, 0) != 0 or library.prctl(36, 1, 0, 0, 0) != 0:
            raise OSError('bounded_exec: cannot establish process-tree reaping')
        subreaper = (library, old.value)
    tree = None
    def cancel(sig, frame):
        raise Cancelled(sig)
    try:
        previous = {s: signal.signal(s, cancel) for s in (signal.SIGTERM, signal.SIGHUP, signal.SIGINT)}
        with tempfile.TemporaryDirectory(prefix='bounded-tree-', dir='/tmp') as directory:
            tree = _Tree(directory)
            outcome = []
            errors = []
            def run():
                try: outcome.append(tree.attempt(cmd, budget, kill_after, deadline, process_options, started=on_started))
                except BaseException as error: errors.append(error)
            worker = threading.Thread(target=run)
            worker.start()
            try:
                while worker.is_alive(): worker.join(.02)
            finally:
                for sig in previous: signal.signal(sig, signal.SIG_IGN)
                tree.close(kill_after)
                worker.join()
            if errors: raise errors[0]
            return outcome[0]
    finally:
        for sig, handler in previous.items(): signal.signal(sig, handler)
        if subreaper: subreaper[0].prctl(36, subreaper[1], 0, 0, 0)


def main(argv):
    try:
        i = argv.index("--")
    except ValueError:
        return usage("missing --")
    opts, cmd = argv[:i], argv[i + 1:]
    if len(opts) != 6 or opts[0::2] != ["--timeout", "--kill-after", "--status"] or not cmd:
        return usage("expected --timeout SECS --kill-after SECS --status FILE -- CMD...")
    budget, kill_after, status = duration(opts[1], True), duration(opts[3], False), opts[5]
    if budget is None or kill_after is None:
        return usage(f"invalid duration: --timeout {opts[1]} --kill-after {opts[3]}")

    try:
        outcome = supervise(cmd, budget, kill_after)
    except Cancelled as c:
        return 128 + c.sig
    with open(status, "w") as fh:
        fh.write(outcome + "\n")
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
