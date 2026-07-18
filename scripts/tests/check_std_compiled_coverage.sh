#!/usr/bin/env bash
# std compiled-coverage gate (bug-039 class closure): every public std
# module gets ONE tiny behavioral program that is COMPILED and RUN — not
# interpreted. std.env had interpreter tests only, so its backend path was
# never exercised until workload 5 imported it and segfaulted (bug 039:
# import-alias rebinding). "Module has tests" must imply "module's compiled
# path ran".
#
# Fail-closed inventory: the module list is DERIVED from std/src/*.con;
# a module without a fixture here must appear in EXEMPT with a reason, so
# adding a std module without compiled coverage fails this gate.
set -uo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
C="$ROOT_DIR/.lake/build/bin/concrete"
[ -x "$C" ] || { echo "error: build first" >&2; exit 2; }
TMP=$(mktemp -d); trap 'rm -rf "$TMP"' EXIT
PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }

# Modules with no runnable public surface (exemption REASONS are load-bearing):
#   lib   — the std umbrella module declaration file (mod std { mod ... }).
#   libc  — private extern declarations only (zero `pub` items); its behavior
#           is exercised through every trusted std fn above it.
#   slice — Slice/MutSlice have NO public constructor (fields private, built
#           only by std internals; Vec has no as_slice). User code cannot
#           obtain one, so there is nothing behavioral to compile. Recorded
#           as a std gap: the first workload needing a slice pulls a public
#           constructor, and this exemption converts to a fixture.
EXEMPT="lib libc slice"

COVERED=""
# covmod <module> [prog-args...]: fixture on stdin -> $TMP/<module>/src/main.con,
# project-built (compiled, NOT interp), run with prog-args; rc==0 is the pass.
covmod(){ local mod="$1"; shift
  COVERED="$COVERED $mod"
  mkdir -p "$TMP/$mod/src"
  printf '[package]\nname = "cov_%s"\nversion = "0.1.0"\n' "$mod" > "$TMP/$mod/Concrete.toml"
  cat > "$TMP/$mod/src/main.con"
  if ! ( cd "$TMP/$mod" && "$C" build ) > "$TMP/$mod/build.log" 2>&1; then
    no "$mod (compile failed: $(grep -m1 error "$TMP/$mod/build.log" || head -1 "$TMP/$mod/build.log"))"
    return
  fi
  local rc=0
  "$TMP/$mod/cov_$mod" "$@" >/dev/null 2>&1 || rc=$?
  [ $rc -eq 0 ] && ok "$mod (compiled behavioral roundtrip)" || no "$mod (run rc=$rc)"
}

echo "=== per-module compiled behavioral fixtures ==="

covmod alloc <<'EOF'
mod main {
    import std.alloc.{heap_new, grow, dealloc};
    trusted fn roundtrip() with(Alloc) -> u8 {
        let p: *mut u64 = heap_new::<u64>();
        *p = 41;
        if *p != 41 { dealloc::<u64>(p); return 1; }
        let q: *mut u8 = grow::<u8>(0 as *mut u8, 4);
        *q = 7;
        let ok: bool = *q == 7;
        dealloc::<u64>(p);
        dealloc::<u8>(q);
        if ok { return 0; }
        return 1;
    }
    fn main() with(Std) -> u8 { return roundtrip(); }
}
EOF

covmod args hello <<'EOF'
mod main {
    import std.args.{count, get};
    fn main() with(Std) -> u8 {
        if count() != 2 { return 1; }
        let a: String = get(1);
        let want: String = "hello";
        let same: bool = string_eq(&a, &want);
        a.drop(); want.drop();
        if same { return 0; }
        return 1;
    }
}
EOF

covmod ascii <<'EOF'
mod main {
    import std.ascii.{is_digit, is_alpha, to_upper};
    fn main() with(Std) -> u8 {
        if !is_digit('7') { return 1; }
        if is_digit('x') { return 1; }
        if !is_alpha('q') { return 1; }
        if to_upper('a') != 'A' { return 1; }
        return 0;
    }
}
EOF

covmod base64 <<'EOF'
mod main {
    import std.base64.{encode, decode};
    import std.bytes.{Bytes};
    fn main() with(Std) -> u8 {
        let s: String = "hi";
        let raw: Bytes = Bytes::from_string(&s);
        s.drop();
        let enc: Bytes = encode(&raw);
        let dec: Option<Bytes> = decode(&enc);
        enc.drop();
        match dec {
            Option::None => { raw.drop(); return 1; },
            Option::Some { value } => {
                let same: bool = value.eq(&raw);
                value.drop(); raw.drop();
                if same { return 0; }
                return 1;
            },
        }
    }
}
EOF

covmod bitset <<'EOF'
mod main {
    import std.bitset.{BitSet};
    fn main() with(Std) -> u8 {
        let mut b: BitSet = BitSet::new();
        b.set(3);
        if !b.test(3) { b.drop(); return 1; }
        if b.test(4) { b.drop(); return 1; }
        b.unset(3);
        let ok: bool = !b.test(3);
        b.drop();
        if ok { return 0; }
        return 1;
    }
}
EOF

covmod bytes <<'EOF'
mod main {
    import std.bytes.{Bytes};
    fn main() with(Std) -> u8 {
        let mut b: Bytes = Bytes::new();
        b.push(104); b.push(61); b.push(105);
        let at: Option<u64> = b.index_of(61, 0);
        match at { Option::Some { value } => { if value != 1 { b.drop(); return 1; } }, Option::None => { b.drop(); return 1; } }
        match b.slice(2, 3) {
            Option::None => { b.drop(); return 1; },
            Option::Some { value } => {
                match value.to_string() {
                    Option::None => { b.drop(); return 1; },
                    Option::Some { value } => {
                        let want: String = "i";
                        let same: bool = string_eq(&value, &want);
                        value.drop(); want.drop(); b.drop();
                        if same { return 0; }
                        return 1;
                    },
                }
            },
        }
    }
}
EOF

covmod checksum <<'EOF'
mod main {
    import std.checksum.{crc32_all};
    import std.bytes.{Bytes};
    fn main() with(Std) -> u8 {
        let s: String = "123456789";
        let b: Bytes = Bytes::from_string(&s);
        s.drop();
        let c: u64 = crc32_all(&b);
        b.drop();
        if c == 3421780262 { return 0; }   // 0xCBF43926, the CRC-32 check value
        return 1;
    }
}
EOF

covmod cli one <<'EOF'
mod main {
    import std.cli.{Cli, CliResult};
    import std.vec.{Vec};
    fn main() with(Std) -> u8 {
        let mut cli: Cli = Cli::new();
        match cli.parse(1, 1) {
            CliResult::Err { error } => { discard(error); return 1; },
            CliResult::Ok { flags, positionals } => {
                flags.drop();
                let mut ps: Vec<String> = positionals;
                let p: Option<String> = ps.pop();
                ps.drop();
                match p {
                    Option::None => { return 1; },
                    Option::Some { value } => {
                        let want: String = "one";
                        let same: bool = string_eq(&value, &want);
                        value.drop(); want.drop();
                        if same { return 0; }
                        return 1;
                    },
                }
            },
        }
    }
}
EOF

covmod deque <<'EOF'
mod main {
    import std.deque.{Deque};
    fn main() with(Std) -> u8 {
        let mut d: Deque<u64> = Deque::<u64>::new();
        d.push_back(1);
        d.push_back(2);
        d.push_front(0);
        let f: Option<u64> = d.pop_front();
        match f { Option::Some { value } => { if value != 0 { d.drop(); return 1; } }, Option::None => { d.drop(); return 1; } }
        let bk: Option<u64> = d.pop_back();
        match bk { Option::Some { value } => { if value != 2 { d.drop(); return 1; } }, Option::None => { d.drop(); return 1; } }
        let ok: bool = d.len() == 1;
        d.drop();
        if ok { return 0; }
        return 1;
    }
}
EOF

covmod env <<'EOF'
mod main {
    import std.env.{get, set, unset};
    fn main() with(Std) -> u8 {
        let k: String = "CONCRETE_COV_ENV";
        let v: String = "roundtrip";
        set(&k, &v);
        let got: Option<String> = get(&k);
        unset(&k);
        let gone: Option<String> = get(&k);
        k.drop();
        let mut ok: bool = false;
        match got {
            Option::Some { value } => { ok = string_eq(&value, &v); value.drop(); },
            Option::None => { },
        }
        v.drop();
        match gone {
            Option::Some { value } => { value.drop(); return 1; },
            Option::None => { },
        }
        if ok { return 0; }
        return 1;
    }
}
EOF

covmod fmt <<'EOF'
mod main {
    import std.fmt.{format_int, format_hex, push_hex};
    fn main() with(Std) -> u8 {
        let a: String = format_int(0 - 42);
        let wa: String = "-42";
        let ok_a: bool = string_eq(&a, &wa);
        a.drop(); wa.drop();
        if !ok_a { return 1; }
        let h: String = format_hex(255);
        let wh: String = "0xff";
        let ok_h: bool = string_eq(&h, &wh);
        h.drop(); wh.drop();
        if !ok_h { return 1; }
        let mut s: String = String::new();
        push_hex(&mut s, 10, 2);
        let ws: String = "0a";
        let ok_s: bool = string_eq(&s, &ws);
        s.drop(); ws.drop();
        if ok_s { return 0; }
        return 1;
    }
}
EOF

covmod fs "$TMP/fs_scratch.bin" <<'EOF'
mod main {
    import std.fs.{write_file, read_file, file_exists};
    import std.bytes.{Bytes};
    import std.args.{get};
    fn main() with(Std) -> u8 {
        let path: String = get(1);
        let mut data: Bytes = Bytes::new();
        data.push(42); data.push(0); data.push(255);
        match write_file(&path, &data) {
            Result::Err { error } => { discard(error); data.drop(); path.drop(); return 1; },
            Result::Ok { value } => { if value != 3 { data.drop(); path.drop(); return 1; } },
        }
        if !file_exists(&path) { data.drop(); path.drop(); return 1; }
        match read_file(&path) {
            Result::Err { error } => { discard(error); data.drop(); path.drop(); return 1; },
            Result::Ok { value } => {
                let same: bool = value.eq(&data);
                value.drop(); data.drop(); path.drop();
                if same { return 0; }
                return 1;
            },
        }
    }
}
EOF

covmod hash <<'EOF'
mod main {
    import std.hash.{fnv1a_string, hash_u64, eq_string};
    fn main() with(Std) -> u8 {
        let e: String = "";
        if fnv1a_string(&e) != 14695981039346656037 { e.drop(); return 1; }  // FNV-1a offset basis
        e.drop();
        let a: String = "same";
        let b: String = "same";
        let ok: bool = eq_string(&a, &b) && fnv1a_string(&a) == fnv1a_string(&b);
        a.drop(); b.drop();
        if !ok { return 1; }
        let k: u64 = 7;
        if hash_u64(&k) != hash_u64(&k) { return 1; }
        return 0;
    }
}
EOF

covmod heap <<'EOF'
mod main {
    import std.heap.{BinaryHeap};
    fn less(a: &u64, b: &u64) -> bool { return *a < *b; }
    fn main() with(Std) -> u8 {
        let mut h: BinaryHeap<u64> = BinaryHeap::<u64>::new(less);
        h.push(5);
        if h.len() != 1 { h.drop(); return 1; }
        let top: Option<u64> = h.pop();
        match top { Option::Some { value } => { if value != 5 { h.drop(); return 1; } }, Option::None => { h.drop(); return 1; } }
        let ok: bool = h.is_empty();
        h.drop();
        if ok { return 0; }
        return 1;
    }
}
EOF

covmod hex <<'EOF'
mod main {
    import std.hex.{encode_bytes, decode};
    import std.vec.{Vec};
    fn main() with(Std) -> u8 {
        let mut v: Vec<u8> = Vec::<u8>::new();
        v.push(10); v.push(255);
        let s: String = encode_bytes(&v);
        let want: String = "0aff";
        let ok: bool = string_eq(&s, &want);
        want.drop();
        if !ok { s.drop(); v.drop(); return 1; }
        let back: Option<Vec<u8>> = decode(&s);
        s.drop();
        match back {
            Option::None => { v.drop(); return 1; },
            Option::Some { value } => {
                let same: bool = value.len() == 2 && value.get_unchecked(0) == 10
                              && value.get_unchecked(1) == 255;
                value.drop(); v.drop();
                if same { return 0; }
                return 1;
            },
        }
    }
}
EOF

covmod io <<'EOF'
mod main {
    import std.io.{println};
    fn main() with(Std) -> u8 {
        let s: String = "io-ok";
        println(&s);
        s.drop();
        return 0;
    }
}
EOF

covmod map <<'EOF'
mod main {
    import std.map.{HashMap};
    import std.hash.{hash_u64, eq_u64};
    fn main() with(Std) -> u8 {
        let mut m: HashMap<u64, u64> = HashMap::<u64, u64>::new(hash_u64, eq_u64);
        let o1: Option<u64> = m.insert(1, 10);
        discard(o1.is_none());
        let o2: Option<u64> = m.insert(2, 20);
        discard(o2.is_none());
        let k2: u64 = 2;
        let g: Option<u64> = m.get(&k2);
        match g { Option::Some { value } => { if value != 20 { m.drop(); return 1; } }, Option::None => { m.drop(); return 1; } }
        let k1: u64 = 1;
        let r: Option<u64> = m.remove(&k1);
        match r { Option::Some { value } => { if value != 10 { m.drop(); return 1; } }, Option::None => { m.drop(); return 1; } }
        let ok: bool = m.len() == 1;
        m.drop();
        if ok { return 0; }
        return 1;
    }
}
EOF

covmod math <<'EOF'
mod main {
    import std.math.{max, min, abs, clamp};
    fn main() with(Std) -> u8 {
        if max::<u64>(3, 9) != 9 { return 1; }
        if min::<u64>(3, 9) != 3 { return 1; }
        if abs(0 - 5) != 5 { return 1; }
        if clamp::<u64>(12, 0, 10) != 10 { return 1; }
        return 0;
    }
}
EOF

covmod mem <<'EOF'
mod main {
    import std.mem.{sizeof, alignof};
    fn main() with(Std) -> u8 {
        if sizeof::<u64>() != 8 { return 1; }
        if sizeof::<u8>() != 1 { return 1; }
        if alignof::<u32>() != 4 { return 1; }
        return 0;
    }
}
EOF

covmod net <<'EOF'
mod main {
    import std.net.{TcpListener, TcpStream, NetError};
    fn main() with(Std) -> u8 {
        // bind an OS-assigned ephemeral port (0) on loopback: must succeed
        let addr: String = "127.0.0.1";
        match TcpListener::bind(&addr, 0) {
            Result::Err { error } => { discard(error); addr.drop(); return 1; },
            Result::Ok { value } => { value.close(); },
        }
        // connect to a port nothing listens on: must be a domain Err, not a trap
        match TcpStream::connect(&addr, 1) {
            Result::Err { error } => { discard(error); addr.drop(); return 0; },
            Result::Ok { value } => { value.close(); addr.drop(); return 1; },
        }
    }
}
EOF

covmod numeric <<'EOF'
mod main {
    import std.numeric.{NonZeroU32};
    fn main() with(Std) -> u8 {
        let z: Option<NonZeroU32> = NonZeroU32::try_new(0);
        if z.is_some() { discard(z.is_some()); return 1; }
        let n: Option<NonZeroU32> = NonZeroU32::try_new(5);
        if n.is_none() { return 1; }
        return 0;
    }
}
EOF

covmod option <<'EOF'
mod main {
    fn main() with(Std) -> u8 {
        let a: Option<u64> = Option::<u64>::Some { value: 7 };
        if !a.is_some() { return 1; }
        if a.unwrap_or(0) != 7 { return 1; }
        let b: Option<u64> = Option::<u64>::None;
        if b.unwrap_or(9) != 9 { return 1; }
        return 0;
    }
}
EOF

covmod ordered_map <<'EOF'
mod main {
    import std.ordered_map.{OrderedMap};
    fn cmp(a: &u64, b: &u64) -> i32 {
        if *a < *b { return 0 - 1; }
        if *a > *b { return 1; }
        return 0;
    }
    fn main() with(Std) -> u8 {
        let mut m: OrderedMap<u64, u64> = OrderedMap::<u64, u64>::new(cmp);
        let o1: Option<u64> = m.insert(2, 20);
        discard(o1.is_none());
        let o2: Option<u64> = m.insert(1, 10);
        discard(o2.is_none());
        let k1: u64 = 1;
        let g: Option<u64> = m.get(&k1);
        match g { Option::Some { value } => { if value != 10 { m.drop(); return 1; } }, Option::None => { m.drop(); return 1; } }
        let ok: bool = m.len() == 2;
        m.drop();
        if ok { return 0; }
        return 1;
    }
}
EOF

covmod ordered_set <<'EOF'
mod main {
    import std.ordered_set.{OrderedSet};
    fn cmp(a: &u64, b: &u64) -> i32 {
        if *a < *b { return 0 - 1; }
        if *a > *b { return 1; }
        return 0;
    }
    fn main() with(Std) -> u8 {
        let mut s: OrderedSet<u64> = OrderedSet::<u64>::new(cmp);
        discard(s.insert(3));
        discard(s.insert(1));
        let k3: u64 = 3;
        let k2: u64 = 2;
        if !s.contains(&k3) { s.drop(); return 1; }
        if s.contains(&k2) { s.drop(); return 1; }
        let ok: bool = s.len() == 2;
        s.drop();
        if ok { return 0; }
        return 1;
    }
}
EOF

covmod parse <<'EOF'
mod main {
    import std.parse.{parse_uint, parse_hex, parse_bool};
    fn main() with(Std) -> u8 {
        let a: String = "42";
        let ua: Option<u64> = parse_uint(&a);
        a.drop();
        match ua { Option::Some { value } => { if value != 42 { return 1; } }, Option::None => { return 1; } }
        let h: String = "ff";
        let uh: Option<u64> = parse_hex(&h);
        h.drop();
        match uh { Option::Some { value } => { if value != 255 { return 1; } }, Option::None => { return 1; } }
        let bad: String = "12x";
        let ub: Option<u64> = parse_uint(&bad);
        bad.drop();
        if ub.is_some() { return 1; }
        let t: String = "true";
        let bt: Option<bool> = parse_bool(&t);
        t.drop();
        match bt { Option::Some { value } => { if !value { return 1; } }, Option::None => { return 1; } }
        return 0;
    }
}
EOF

covmod path <<'EOF'
mod main {
    import std.path.{PathBuf};
    fn main() with(Std) -> u8 {
        let root: String = "/a";
        let mut p: PathBuf = PathBuf::from_string(&root);
        root.drop();
        let seg: String = "b";
        p.push(&seg);
        seg.drop();
        match p.to_string() {
            Option::None => { p.drop(); return 1; },
            Option::Some { value } => {
                let want: String = "/a/b";
                let same: bool = string_eq(&value, &want);
                value.drop(); want.drop(); p.drop();
                if same { return 0; }
                return 1;
            },
        }
    }
}
EOF

covmod process <<'EOF'
mod main {
    import std.process.{process_getpid, sig_term};
    fn main() with(Std) -> u8 {
        if sig_term() != 15 { return 1; }
        if process_getpid() <= 0 { return 1; }
        return 0;
    }
}
EOF

covmod ptr <<'EOF'
mod main {
    import std.ptr.{offset};
    import std.alloc.{grow, dealloc};
    trusted fn roundtrip() with(Alloc) -> u8 {
        let base: *mut u64 = grow::<u64>(0 as *mut u64, 3) as *mut u64;
        let two: *mut u64 = offset::<u64>(base, 2);
        *two = 99;
        let ok: bool = *(offset::<u64>(base, 2)) == 99;
        dealloc::<u64>(base);
        if ok { return 0; }
        return 1;
    }
    fn main() with(Std) -> u8 { return roundtrip(); }
}
EOF

covmod rand <<'EOF'
mod main {
    import std.rand.{seed, random_range};
    fn main() with(Std) -> u8 {
        seed(42);
        let a: i32 = random_range(5, 9);
        if a < 5 { return 1; }
        if a > 9 { return 1; }
        // same libc seed -> same first draw (behavioral determinism)
        seed(42);
        let b: i32 = random_range(5, 9);
        if a != b { return 1; }
        return 0;
    }
}
EOF

covmod result <<'EOF'
mod main {
    fn main() with(Std) -> u8 {
        let r: Result<u64, u64> = Result::<u64, u64>::Ok { value: 3 };
        if !r.is_ok() { return 1; }
        if r.unwrap_or(0) != 3 { return 1; }
        let e: Result<u64, u64> = Result::<u64, u64>::Err { error: 8 };
        if e.unwrap_or(5) != 5 { return 1; }
        return 0;
    }
}
EOF

covmod set <<'EOF'
mod main {
    import std.set.{HashSet};
    import std.hash.{hash_u64, eq_u64};
    fn main() with(Std) -> u8 {
        let mut s: HashSet<u64> = HashSet::<u64>::new(hash_u64, eq_u64);
        discard(s.insert(4));
        discard(s.insert(6));
        let k4: u64 = 4;
        let k5: u64 = 5;
        if !s.contains(&k4) { s.drop(); return 1; }
        if s.contains(&k5) { s.drop(); return 1; }
        if !s.remove(&k4) { s.drop(); return 1; }
        let ok: bool = s.len() == 1;
        s.drop();
        if ok { return 0; }
        return 1;
    }
}
EOF

covmod sha256 <<'EOF'
mod main {
    import std.sha256.{hash_string};
    import std.vec.{Vec};
    fn main() with(Std) -> u8 {
        let s: String = "abc";
        let words: Vec<u32> = hash_string(&s);
        s.drop();
        // SHA-256("abc") = ba7816bf 8f01cfea ...
        let ok: bool = words.len() == 8 && words.get_unchecked(0) == 3128432319
                    && words.get_unchecked(1) == 2399260650;
        words.drop();
        if ok { return 0; }
        return 1;
    }
}
EOF

covmod string <<'EOF'
mod main {
    fn main() with(Std) -> u8 {
        let mut s: String = String::new();
        s.push_char('h');
        s.push_char('i');
        let want: String = "hi";
        if !s.eq(&want) { s.drop(); want.drop(); return 1; }
        let pre: String = "h";
        let ok: bool = s.starts_with(&pre) && s.len() == 2;
        s.drop(); want.drop(); pre.drop();
        if ok { return 0; }
        return 1;
    }
}
EOF

covmod test <<'EOF'
mod main {
    import std.test.{expect_some, expect_none};
    fn main() with(Std) -> u8 {
        let s: Option<u64> = Option::<u64>::Some { value: 3 };
        if !expect_some::<u64>(&s) { return 1; }
        let n: Option<u64> = Option::<u64>::None;
        if !expect_none::<u64>(&n) { return 1; }
        if expect_some::<u64>(&n) { return 1; }
        return 0;
    }
}
EOF

covmod text <<'EOF'
mod main {
    import std.text.{Text};
    fn main() with(Std) -> u8 {
        let s: String = "héllo";
        let t: Text = Text::from_string(&s);
        let ok_len: bool = t.len() == s.len();
        let t2: Text = Text::from_string(&s);
        let ok_eq: bool = t.eq(&t2);
        s.drop();
        if ok_len && ok_eq { return 0; }
        return 1;
    }
}
EOF

covmod time <<'EOF'
mod main {
    import std.time.{Duration, sleep, unix_timestamp};
    fn main() with(Std) -> u8 {
        let before: i64 = unix_timestamp();
        if before < 1700000000 { return 1; }   // sane wall clock (post-2023)
        let d: Duration = Duration::from_millis(1);
        sleep(&d);
        let after: i64 = unix_timestamp();
        if after < before { return 1; }        // time does not run backwards
        return 0;
    }
}
EOF

covmod vec <<'EOF'
mod main {
    import std.vec.{Vec};
    fn main() with(Std) -> u8 {
        let mut v: Vec<u64> = Vec::<u64>::new();
        v.push(1); v.push(2); v.push(3);
        if v.len() != 3 { v.drop(); return 1; }
        let g: Option<u64> = v.get(1);
        match g { Option::Some { value } => { if value != 2 { v.drop(); return 1; } }, Option::None => { v.drop(); return 1; } }
        let p: Option<u64> = v.pop();
        match p { Option::Some { value } => { if value != 3 { v.drop(); return 1; } }, Option::None => { v.drop(); return 1; } }
        let ok: bool = v.len() == 2;
        v.drop();
        if ok { return 0; }
        return 1;
    }
}
EOF

echo "=== bug 046: collection extraction is Copy-bounded (two-owners defect) ==="
# keys()/values()/elements() copied LINEAR payloads out while the map kept
# ownership — checker-accepted double-free. Pin the rejection and the
# Copy-typed positives.
mkdir -p "$TMP/b046/src"
printf '[package]\nname = "b046"\nversion = "0.1.0"\n' > "$TMP/b046/Concrete.toml"
cat > "$TMP/b046/src/main.con" <<'EOF'
mod main {
    import std.map.{HashMap};
    import std.hash.{hash_u64, eq_u64};
    import std.vec.{Vec};
    fn main() with(Std) -> u8 {
        let mut m: HashMap<u64, String> = HashMap::<u64, String>::new(hash_u64, eq_u64);
        let s: String = "x";
        let o: Option<String> = m.insert(1, s);
        match o { Option::Some { value } => { value.drop(); }, Option::None => { }, }
        let vs: Vec<String> = m.values();
        vs.drop(); m.drop();
        return 0;
    }
}
EOF
if ( cd "$TMP/b046" && "$C" build ) > "$TMP/b046.log" 2>&1; then
  no "values() over linear payloads COMPILED (two-owners defect is back)"
else
  grep -q "E0241" "$TMP/b046.log" \
    && ok "values() over linear payloads rejected (E0241 Copy bound)" \
    || no "rejected but not with E0241: $(grep -m1 error "$TMP/b046.log")"
fi
cat > "$TMP/b046/src/main.con" <<'EOF'
mod main {
    import std.map.{HashMap};
    import std.set.{HashSet};
    import std.hash.{hash_u64, eq_u64};
    import std.vec.{Vec};
    fn main() with(Std) -> u8 {
        let mut m: HashMap<u64, u64> = HashMap::<u64, u64>::new(hash_u64, eq_u64);
        discard(m.insert(1, 10).is_none());
        discard(m.insert(2, 20).is_none());
        let ks: Vec<u64> = m.keys();
        let vs: Vec<u64> = m.values();
        let count_ok: bool = ks.len() == 2 && vs.len() == 2;
        ks.drop(); vs.drop(); m.drop();
        if !count_ok { return 1; }
        let mut st: HashSet<u64> = HashSet::<u64>::new(hash_u64, eq_u64);
        discard(st.insert(7));
        let es: Vec<u64> = st.elements();
        let e_ok: bool = es.len() == 1 && es.get_unchecked(0) == 7;
        es.drop(); st.drop();
        if e_ok { return 0; }
        return 1;
    }
}
EOF
if ( cd "$TMP/b046" && "$C" build ) > "$TMP/b046.log" 2>&1 && "$TMP/b046/b046"; then
  ok "Copy-typed keys()/values()/elements() still work (positive)"
else
  no "Copy-typed extraction broke (rc or compile)"
fi

echo "=== dependencies are CHECKED in project mode (defect-queue item 3) ==="
# The H12-era filter excluded dep modules from front-end Check; a
# wrong-typed std edit compiled fine in project mode. Pin the closure: a
# local dependency with a type error must FAIL the consumer's build.
mkdir -p "$TMP/badlib/src" "$TMP/depcheck/src"
printf '[package]\nname = "badlib"\nversion = "0.1.0"\n' > "$TMP/badlib/Concrete.toml"
cat > "$TMP/badlib/src/lib.con" <<'EOF'
mod badlib {
    fn wants_ref(x: &String) -> u64 {
        return x.len();
    }
    pub fn broken() with(Alloc) -> u64 {
        let s: String = "x";
        let n: u64 = wants_ref(s);   // owned where &String expected: E0220
        s.drop();
        return n;
    }
}
EOF
printf '[package]\nname = "depcheck"\nversion = "0.1.0"\n\n[dependencies]\nbadlib = { path = "%s" }\n' "$TMP/badlib" > "$TMP/depcheck/Concrete.toml"
cat > "$TMP/depcheck/src/main.con" <<'EOF'
mod main {
    fn main() with(Std) -> u8 { return 0; }
}
EOF
if ( cd "$TMP/depcheck" && "$C" build ) > "$TMP/depcheck.log" 2>&1; then
  no "type-broken dependency compiled (dep modules skipped Check again?)"
else
  grep -q "E0220" "$TMP/depcheck.log" \
    && ok "type-broken dependency fails the consumer build with E0220" \
    || no "dep build failed but not with the expected E0220: $(grep -m1 error "$TMP/depcheck.log")"
fi

echo "=== fail-closed inventory (derived from std/src) ==="
derived=$(ls std/src/*.con | xargs -n1 basename | sed 's/\.con$//' | sort)
accounted=$(printf '%s %s' "$COVERED" "$EXEMPT" | tr ' ' '\n' | sed '/^$/d' | sort)
missing=$(comm -23 <(printf '%s\n' "$derived") <(printf '%s\n' "$accounted"))
stale=$(comm -13 <(printf '%s\n' "$derived") <(printf '%s\n' "$accounted"))
if [ -z "$missing" ] && [ -z "$stale" ]; then
  ok "every std module is covered or explicitly exempt ($(printf '%s\n' "$derived" | wc -l | tr -d ' ') modules)"
else
  [ -n "$missing" ] && no "std modules with NO compiled coverage (add a covmod fixture or an EXEMPT reason): $(echo $missing)"
  [ -n "$stale" ] && no "gate lists modules that no longer exist in std/src: $(echo $stale)"
fi

echo
echo "STD-COMPILED-COVERAGE: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
