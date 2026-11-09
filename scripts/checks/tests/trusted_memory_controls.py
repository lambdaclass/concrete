"""Real syscall and exact-diagnostic controls for the executable-memory boundary.

No Lean build, network access, or installed fixture: everything lives in a private directory.
"""
import importlib.util
import json
import os
from pathlib import Path
import shutil
import subprocess
import sys
import tempfile
import unittest

LIB = Path(__file__).resolve().parents[1] / 'lib'
spec = importlib.util.spec_from_file_location('mappings', LIB / 'trusted_mappings.py')
m = importlib.util.module_from_spec(spec)
spec.loader.exec_module(m)

CODE = r"""
#define _GNU_SOURCE
#include <assert.h>
#include <fcntl.h>
#include <sys/mman.h>
#include <sys/shm.h>
#include <sys/personality.h>
#include <unistd.h>
#include <string.h>
int main(int argc, char **argv) {
 int fd = open(argv[1], O_RDONLY); assert(fd >= 0);
 const char *mode = argv[2]; void *p;
 if (!strcmp(mode,"anonymous")) assert(mmap(0,4096,PROT_EXEC,MAP_PRIVATE|MAP_ANONYMOUS,fd,0)!=MAP_FAILED);
 if (!strcmp(mode,"private")) {
  p=mmap(0,4096,PROT_READ|PROT_WRITE|PROT_EXEC,MAP_PRIVATE,fd,0); assert(p!=MAP_FAILED);
  memset(p,0,4096); /* prove the pinned page can be replaced */
 }
 if (!strcmp(mode,"shared")) assert(mmap(0,4096,PROT_READ|PROT_EXEC,MAP_SHARED,fd,0)!=MAP_FAILED);
 if (!strcmp(mode,"pkey")) {
  p=mmap(0,4096,PROT_READ|PROT_WRITE,MAP_PRIVATE|MAP_ANONYMOUS,-1,0); assert(p!=MAP_FAILED);
  assert(pkey_mprotect(p,4096,PROT_READ|PROT_EXEC,0)==0);
 }
 if (!strcmp(mode,"mprotect")) {
  p=mmap(0,4096,PROT_READ,MAP_PRIVATE,fd,0); assert(p!=MAP_FAILED);
  assert(mprotect(p,4096,PROT_WRITE|PROT_EXEC)==0);
 }
 if (!strcmp(mode,"shmat")) {
  int id=shmget(IPC_PRIVATE,4096,IPC_CREAT|0700); assert(id>=0);
  p=shmat(id,0,SHM_EXEC|SHM_RDONLY); int rc=shmctl(id,IPC_RMID,0);
  assert(p!=(void*)-1 && rc==0); assert(shmdt(p)==0);
 }
 if (!strcmp(mode,"path")) assert(mmap(0,4096,PROT_READ|PROT_EXEC,MAP_PRIVATE,fd,0)!=MAP_FAILED);
 if (!strcmp(mode,"execveat")) { char *a[]={"true",0}, *e[]={0}; execveat(AT_FDCWD,argv[1],a,e,0); assert(0); }
 if (!strcmp(mode,"empty")) { char *a[]={"true",0}, *e[]={0}; execveat(fd,"",a,e,AT_EMPTY_PATH); assert(0); }
 if (!strcmp(mode,"procmem")) { int mem=open("/proc/self/mem",O_RDWR); assert(mem>=0); close(mem); }
 return 0;
}
"""

class MemoryTests(unittest.TestCase):
 def reject(self, call, diagnostic):
  with self.assertRaises(ValueError) as error: call()
  self.assertEqual(str(error.exception), diagnostic)

 def test_structural_controls(self):
  def line(prot='PROT_EXEC', flags='MAP_PRIVATE', kind='mmap', fd='3</tmp/PROT_EXEC,payload>'):
   return f'{kind}(NULL, 4096, {prot}, {flags}, {fd}, 0) = 0x1000'
  self.assertEqual(m.executable_mappings([line(prot='PROT_READ')]),set())
  for kind in ('mmap','mmap2'):
   for flags, diagnostic in [
    ('MAP_PRIVATE|MAP_ANONYMOUS','executable anonymous or unresolved mapping'),
    ('MAP_PRIVATE|MAP_ANON','executable anonymous or unresolved mapping'),
    ('MAP_SHARED','shared executable mapping has no immutable file identity'),
    ('MAP_SHARED_VALIDATE','shared executable mapping has no immutable file identity'),
    ('MAP_PRIVATE|0x80000000','unknown mapping flags: MAP_PRIVATE|0x80000000')]:
    self.reject(lambda: m.executable_mappings([line(flags=flags,kind=kind)]),diagnostic)
  self.reject(lambda:m.executable_mappings([line(prot='PROT_EXEC|0x80')]),'unknown mapping protection: PROT_EXEC|0x80')
  self.reject(lambda:m.executable_mappings(['remap_file_pages(0x1000, 4096, 0, 0, 0) = 0']), 'unsupported memory identity mutation: remap_file_pages')
  self.reject(lambda:m.executable_mappings(['future_exec_memory(0) = 0']), 'unsupported traced memory syscall: future_exec_memory')
  self.assertEqual(m.executable_mappings([line()[:-6]+'?', '+++ killed by SIGKILL +++']),set())
  self.reject(lambda:m.executable_mappings([line()[:-6]+'?']), 'unfinished mapping calls have no result')
  for escaped, expected in [(r'/tmp/a\76b','/tmp/a>b'),(r'/tmp/\303\251','/tmp/é'),(r'/tmp/a\\b','/tmp/a\\b'),(r'/tmp/a\"b','/tmp/a"b')]:
   self.assertEqual(m.strings.decode_fd_path(escaped),expected)
  for escaped, decoded in [(r'/tmp/\n','/tmp/\n'),(r'/tmp/\t','/tmp/\t'),(r'/tmp/\302\205','/tmp/\u0085')]:
   self.reject(lambda:m.strings.decode_fd_path(escaped), 'control character in mapping path: '+json.dumps(decoded,ensure_ascii=True))

 def test_existing_examples_bound(self):
  # Production bounded() uses this exact launcher for the complete frontend tree.
  code = """import errno,mmap,resource
soft,hard=resource.getrlimit(resource.RLIMIT_AS)
assert 0 < soft <= 65536*1024*1024 and soft <= hard <= 65536*1024*1024, (soft,hard)
try: mmap.mmap(-1,hard+4096)
except OSError as error: assert error.errno==errno.ENOMEM, error
else: raise AssertionError('address-space limit did not reject allocation')
print('hard address-space limit: ENOMEM')
"""
  result=subprocess.run(['bash',str(LIB/'limited_exec.sh'),'65536',sys.executable,'-I','-c',code],check=True,text=True,capture_output=True,timeout=15)
  self.assertEqual(result.stdout.strip(),'hard address-space limit: ENOMEM')

 def test_real_syscalls(self):
  with tempfile.TemporaryDirectory() as tmp:
   d=Path(tmp); src=d/'memory.c'; exe=d/'memory'; src.write_text(CODE)
   clang=os.environ.get('CONCRETE_SLICE0_CLANG',shutil.which('clang'))
   subprocess.run([clang,'--no-default-config',str(src),'-o',str(exe)],check=True,capture_output=True,timeout=25)
   pinned=str(Path(shutil.which('lean')).resolve())
   for mode, diagnostic in [
    ('anonymous','executable anonymous or unresolved mapping'),
    ('private','writable executable mapping has no pinned file identity'),
    ('shared','shared executable mapping has no immutable file identity'),
    ('mprotect','executable mprotect has no pinned file identity'),
    ('pkey','executable pkey_mprotect has no pinned file identity'),
    ('shmat','executable shmat has no pinned file identity'),
    ('procmem','process memory file access has no pinned identity')]:
    log=d/mode
    subprocess.run(['strace',*m.strings.TRACE_FLAGS,'-o',str(log),str(exe),pinned,mode],check=True,capture_output=True,timeout=15)
    self.reject(lambda:m.executable_mappings(log.read_text().splitlines()),diagnostic)
   for mode in ('execveat','empty'):
    log=d/mode; target=str(Path(shutil.which('true')).resolve())
    subprocess.run(['strace',*m.strings.TRACE_FLAGS,'-o',str(log),str(exe),target,mode],check=True,capture_output=True,timeout=15)
    if mode=='execveat': self.assertIn(target,m.strings.parse_many([log],paths=True))
    else: self.reject(lambda:m.strings.parse_many([log]),'execveat AT_EMPTY_PATH has no audited file identity')
   for name in ('dir>é','comma,path','back\\slash','quote"path'):
    path=d/name;path.write_bytes(bytes(4096)); log=d/'path.trace'
    subprocess.run(['strace',*m.strings.TRACE_FLAGS,'-o',str(log),str(exe),str(path),'path'],check=True,capture_output=True,timeout=15)
    self.assertIn(str(path),m.executable_mappings(log.read_text().splitlines()))
   # A real successful store mapping accepted by the union must fail in the other role.
   log=d/'role'; log.write_text(f'mmap(NULL, 4096, PROT_EXEC, MAP_PRIVATE, 3<{pinned}>, 0) = 0x1000\n')
   root='/'.join(pinned.split('/')[:4]); evidence={'closure':{root:{}},'role_closures':{'native':{'clang':[]},'lean-emit':{'lean':[root]}}}
   self.reject(lambda:m.check([log],evidence,'native'),'unregistered executable mapping: '+json.dumps(pinned))
   m.check([log],evidence,'lean-emit')

if __name__=='__main__': unittest.main()
