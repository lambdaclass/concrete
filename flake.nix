{
  description = "Concrete development environment";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  };

  outputs = { self, nixpkgs }:
    let
      systems = [
        "x86_64-linux"
        "aarch64-linux"
        "x86_64-darwin"
        "aarch64-darwin"
      ];
      forAllSystems = f:
        nixpkgs.lib.genAttrs systems (system:
          f {
            pkgs = import nixpkgs { inherit system; };
          });
    in {
      devShells = forAllSystems ({ pkgs }:
        let
          # LEAN IS THE OFFICIAL v4.34.1 RELEASE ARCHIVE, not nixpkgs' lean4 (R-0208). The checker identity a
          # receipt binds is the executable and libraries that actually load; nixpkgs builds Lean itself
          # (reporting its version tag as the githash), so the devshell and CI's elan ran DIFFERENT checkers
          # under one name. Pinning the archive elan installs makes them the same build. Hashes are the
          # sha256 GitHub publishes for each release asset. macOS binaries are left byte-identical
          # (dontFixup); Linux needs autoPatchelf to run under Nix, so its bytes — and identity — differ
          # from elan's there, which CI (elan) does not use.
          leanRelease =
            let
              sys = pkgs.stdenv.hostPlatform.system;
              asset = {
                "aarch64-darwin" = { name = "darwin_aarch64"; sha256 = "65f22a4f047738ec742667b3247e836a86ebf06eabc12655c3dee00637e37866"; };
                "x86_64-darwin"  = { name = "darwin";         sha256 = "c2fee70250444e46c1106a27bddfef948d1e563eef431eaac05bb946b2f73d3a"; };
                "x86_64-linux"   = { name = "linux";          sha256 = "47bf4bbd78f70c2e9670598ab7124d92b6efb7330ff33e5fbb4030f6fd72e4e4"; };
                "aarch64-linux"  = { name = "linux_aarch64";  sha256 = "fdb974c2cdb4627e090d5d4007b913e09d13c4868720fb5594e22808b3de9e37"; };
              }.${sys};
            in pkgs.stdenv.mkDerivation {
              pname = "lean4-release";
              version = "4.34.1";
              src = pkgs.fetchurl {
                url = "https://github.com/leanprover/lean4/releases/download/v4.34.1/lean-4.34.1-${asset.name}.tar.zst";
                sha256 = asset.sha256;
              };
              nativeBuildInputs = [ pkgs.zstd ]
                ++ pkgs.lib.optionals pkgs.stdenv.isLinux [ pkgs.autoPatchelfHook ];
              buildInputs = pkgs.lib.optionals pkgs.stdenv.isLinux [ pkgs.stdenv.cc.cc.lib pkgs.zlib ];
              autoPatchelfIgnoreMissingDeps = true;
              dontConfigure = true;
              dontBuild = true;
              dontStrip = true;
              dontFixup = pkgs.stdenv.isDarwin;
              unpackPhase = "tar --zstd -xf $src";
              installPhase = "mkdir -p $out && cp -R lean-4.34.1-${asset.name}/. $out/";
            };
          # pyyaml so scripts/tests/check_workflow_yaml.sh can actually run: it needs pyyaml or ruby
          # and FAILS (rather than skips) without either. The only non-stdlib module any gate imports.
          python = pkgs.python3.withPackages (ps: [ ps.pyyaml ]);

          # THE GATE USERLAND: the programs scripts/tests (and the scripts/ci, scripts/gen,
          # scripts/land helpers they call) run, and nothing else. Gates run in three places, the
          # dev shell, Linux CI runners and the macOS CI runner, and used to take these tools from
          # whatever host they landed on: GNU on Linux, BSD on macOS, where `timeout` does not exist
          # and `sed -i`, `stat`, `date` and `readlink` take other flags. CI now runs every gate step
          # inside `nix develop .#gates`, so all three see these exact builds.
          #
          # NO LEAN HERE, and no lake. CI takes Lean from elan, and the checker identity a receipt
          # records is the Lean that actually loads (R-0208); a shell that put the nix-built Lean
          # first on PATH would change it. The C/LLVM/Rust toolchains, z3, valgrind, zola, curl and
          # gh also stay the host's: CI provisions those separately today, and moving them is a
          # different change.
          gatesUserland = with pkgs; [
            bash
            coreutils          # timeout, stat, date, readlink, sha256sum, mktemp, ...
            findutils
            diffutils          # diff, cmp
            gnused
            gnugrep
            gawk
            gnutar             # check_tar_list_differential.sh compares against `tar -tf`
            gzip
            xz
            zstd
            bzip2
            which
            file
            gnumake
            git
            jq
            xxd                # check_hexdump_differential.sh skips its format legs without it
            # perl for `shasum`: the checker identity hashes the Lean images with it
            # (Concrete/Proof/Replay.lean, sha256Files), and without it every receipt is withheld
            # as "Checker: UNIDENTIFIED". The shell used to inherit it from the host, and Arch
            # keeps it in /usr/bin/core_perl, which only login shells put on PATH, so 17
            # receipt/replay/checker-identity gates failed on the Arch gate host (2026-10-23).
            # The digest it prints does not depend on which perl computes it.
            perl
            python
          ] ++ lib.optionals stdenv.isLinux [
            procps             # ps, pgrep, pkill (macOS keeps its own; nixpkgs' procps is Linux-only)
            util-linux         # flock, hexdump, column, script
          ];

          # One owner for native link search policy and its complete pinned closure.
          nativeCC = pkgs.llvmPackages_21.clang;
          nativeLD = nativeCC.bintools;
          realLD = nativeLD.bintools;
          rt = "${nativeCC}/resource-root/lib/linux";
          cpu = pkgs.stdenv.hostPlatform.parsed.cpu.name;
          libcLib = "${pkgs.lib.getLib nativeCC.libc}/lib";
          nativeSysroot = if pkgs.stdenv.isDarwin then pkgs.apple-sdk
            else pkgs.lib.getDev nativeCC.libc;
          # Closure metadata needs no stdenv. In particular stdenvNoCC on Darwin
          # introduces an uncached locale -> bmake build just to enumerate paths.
          nativeClosure = builtins.derivation {
            name = "slice0-link-closure";
            system = pkgs.stdenv.hostPlatform.system;
            builder = "${pkgs.bash}/bin/bash";
            args = [ "-euc" ''
              source "$NIX_ATTRS_SH_FILE"
              out="''${outputs[out]}"
              ${pkgs.coreutils}/bin/mkdir "$out"
              ${pkgs.jq}/bin/jq -r '.closure[].path' "$NIX_ATTRS_JSON_FILE" > "$out/store-paths"
            '' ];
            __structuredAttrs = true;
            exportReferencesGraph.closure = [ nativeCC nativeLD realLD nativeSysroot
              pkgs.llvmPackages_21.llvm pkgs.llvmPackages_21.compiler-rt ]
              ++ pkgs.lib.optionals pkgs.stdenv.isLinux [ pkgs.llvmPackages_21.libunwind ];
          };
          nativeSettings = {
            CONCRETE_SLICE0_TARGET = {
              x86_64-linux = "x86_64-linux";
              aarch64-linux = "aarch64-linux";
              aarch64-darwin = "aarch64-macos";
              x86_64-darwin = "unsupported";
            }.${pkgs.stdenv.hostPlatform.system};
            CONCRETE_SLICE0_COMPILER_RT = "${pkgs.llvmPackages_21.compiler-rt}";
            CONCRETE_SLICE0_CLANG = "${nativeCC}/bin/clang";
            CONCRETE_SLICE0_LINK_CLOSURE = "${nativeClosure}/store-paths";
            CONCRETE_SLICE0_LINKER_BIN = "${nativeLD}/bin";
            CONCRETE_SLICE0_REAL_LD = "${realLD}/bin/ld";
            CONCRETE_SLICE0_LLVM = "${pkgs.llvmPackages_21.llvm}/bin";
            CONCRETE_SLICE0_SYSROOT = if pkgs.stdenv.isDarwin
              then "${nativeSysroot}/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk"
              else "${nativeSysroot}";
            CONCRETE_SLICE0_TOOL_PATH = pkgs.lib.makeBinPath [ nativeLD pkgs.coreutils pkgs.gnugrep pkgs.gnused ];
          };
          nativePolicy = builtins.derivation {
            name = "slice0-link-policy-v1";
            system = pkgs.stdenv.hostPlatform.system;
            builder = "${pkgs.bash}/bin/bash";
            args = [ "-euc" ''
              source "$NIX_ATTRS_SH_FILE"
              ${pkgs.jq}/bin/jq -r .text "$NIX_ATTRS_JSON_FILE" > "''${outputs[out]}"
            '' ];
            __structuredAttrs = true;
            text = builtins.toJSON {
            version = 1;
            settings = nativeSettings;
            # Startup objects and GCC/compiler runtimes are references of the
            # pinned wrapper, enumerated transitively by closureInfo.
            libc = "${nativeCC.libc}";
            compilerRuntime = "${pkgs.llvmPackages_21.compiler-rt}";
            environment = {
              PATH = nativeSettings.CONCRETE_SLICE0_TOOL_PATH;
              NIX_STORE = "/nix/store"; NIX_ENFORCE_PURITY = "0";
              NIX_BUILD_TOP = "/nonexistent"; TMPDIR = "@TMPDIR@";
              LC_ALL = "C"; TZ = "UTC"; SOURCE_DATE_EPOCH = "1"; ZERO_AR_DATE = "1";
            };
            compile = [ nativeSettings.CONCRETE_SLICE0_CLANG "--no-default-config"
              "-B${nativeLD}/bin" "--sysroot=${nativeSettings.CONCRETE_SLICE0_SYSROOT}"
              "-O0" "-g0" "-w" "-ffile-prefix-map=@CWD@=." "-fdebug-prefix-map=@CWD@=."
              "-fdebug-compilation-dir=." ]
              ++ (if pkgs.stdenv.isDarwin then [ "-mmacosx-version-min=15.0" ]
                  else [ "-rtlib=compiler-rt" "-unwindlib=none" ]);
            # These are real-linker templates, not compiler-driver flags. The only
            # substitutions are a local output basename and explicit local objects.
            # Grammar: binutils 2.44 ld/ld.texi; Apple ld64-954.16 doc/man/man1/ld-classic.1.
            link = if pkgs.stdenv.isDarwin then [
              nativeSettings.CONCRETE_SLICE0_REAL_LD "-dynamic" "-arch" "arm64"
              "-platform_version" "macos" "15.0" nativeSysroot.version "-syslibroot"
              nativeSettings.CONCRETE_SLICE0_SYSROOT "-no_uuid" "-w" "-Z" "-t"
              "-o" "@OUTPUT@" "@OBJECTS@" "-L${nativeSettings.CONCRETE_SLICE0_SYSROOT}/usr/lib" "-lSystem"
            ] else [
              nativeSettings.CONCRETE_SLICE0_REAL_LD "--sysroot=${nativeSettings.CONCRETE_SLICE0_SYSROOT}"
              "-z" "relro" "--hash-style=gnu" "--eh-frame-hdr" "-nostdlib" "-pie" "--build-id=none" "-t"
              "-dynamic-linker" "${libcLib}/${builtins.baseNameOf pkgs.stdenv.cc.bintools.dynamicLinker}"
              "-o" "@OUTPUT@" "${libcLib}/Scrt1.o" "${libcLib}/crti.o"
              "${rt}/clang_rt.crtbegin-${cpu}.o" "@OBJECTS@"
              "-L${libcLib}" "-rpath" libcLib
              "${rt}/libclang_rt.builtins-${cpu}.a" "-lc"
              "${rt}/clang_rt.crtend-${cpu}.o" "${libcLib}/crtn.o"
            ];
            sanitize = [ "-fsanitize=address" "-fno-sanitize-recover=all" ];
            sanitizeLink = if pkgs.stdenv.isDarwin then [
              "${pkgs.llvmPackages_21.compiler-rt}/lib/darwin/libclang_rt.asan_osx_dynamic.dylib"
              "-rpath" "${pkgs.llvmPackages_21.compiler-rt}/lib/darwin"
            ] else [
              "--whole-archive" "${rt}/libclang_rt.asan_static-${cpu}.a"
              "${rt}/libclang_rt.asan-${cpu}.a" "--no-whole-archive"
              "--dynamic-list=${rt}/libclang_rt.asan-${cpu}.a.syms"
              "${pkgs.lib.getLib pkgs.llvmPackages_21.libunwind}/lib/libunwind.a"
              "--no-as-needed" "-lpthread" "-lrt" "-lm" "-ldl" "-lresolv"
            ];
            validation = "exact real-linker template and closed grammar; raw path and symlink-prefix closure containment; trace and recursive loader dependencies without host cache";
            darwin = "direct ld64, no implicit directories; deployment 15.0 and pinned SDK version; SDK-backed OS dylibs and /usr/lib/dyld ABI; explicit ASan runtime and RPATH";
            };
          };
          nativeEnv = nativeSettings // { CONCRETE_SLICE0_LINK_POLICY = nativePolicy; };

          # Every gate refuses to run without this (scripts/tests/lib/gate_env.sh). Each shell
          # below that is a superset of the gate userland sets it too.
          gatesMarker = { CONCRETE_GATES_SHELL = "1"; };

          base = with pkgs; [
            # clang BEFORE leanRelease: mkShell puts earlier packages first on PATH, and the Lean
            # release ships its own clang without C headers (stddef.h not found), so with Lean first
            # every C link step that uses PATH's clang (run_tests phase3_abi_interop, regress_103)
            # failed in this shell while passing on CI, whose elan puts no clang on PATH.
            clang
            leanRelease
          ] ++ gatesUserland;
        in {
          # Default shell stays light: the pre-existing toolchain only. The
          # second-kernel drivers (Rocq, Isabelle) are an opt-in spike and Isabelle
          # is a multi-GB closure, so they live in a separate shell — no contributor
          # pays for them unless they run `--report multi-kernel`.
          default = pkgs.mkShell ({
            packages = base;
            # Slice-0 native builds name their compiler explicitly (scripts/checks/lib/slice0_native.sh),
            # pinned here so the shell's general clang can move without changing their identity.
            # Compiler and link policy are supplied together by nativeEnv.
            # The artifact gates (scripts/checks/compiler/slice0_artifacts.sh) read the linked executables
            # with the same LLVM release's llvm-objdump and llvm-size, and measure a run under gdb
            # (Linux, pinned) or LLDB (macOS: Apple's /usr/bin/lldb, whose signed debugserver is
            # the only one allowed to attach; not pinned, its identity is recorded in the report).
            CONCRETE_SLICE0_LLVM = "${pkgs.llvmPackages_21.llvm}/bin";
          } // pkgs.lib.optionalAttrs pkgs.stdenv.isLinux {
            CONCRETE_SLICE0_GDB = "${pkgs.gdb}/bin/gdb";
          } // pkgs.lib.optionalAttrs pkgs.stdenv.isDarwin {
            CONCRETE_SLICE0_LLDB = "/usr/bin/lldb";
          } // nativeEnv // gatesMarker);

          # `nix develop .#gates` — what CI runs every gate step in: the gate userland, the marker,
          # and no compiler toolchain (mkShellNoCC, so not even stdenv's cc). Lean and the C
          # toolchain come from the runner, exactly as before.
          gates = pkgs.mkShellNoCC ({ packages = gatesUserland; } // gatesMarker);

          # `nix develop .#extcheck` — the gate userland plus cargo and rustc, for
          # scripts/proofs/external.sh, which builds the pinned nanoda checker (a Rust program) once
          # per revision. mkShell, not NoCC: rustc links through the C compiler. Lean still comes
          # from elan, as in the gates shell.
          extcheck = pkgs.mkShell ({ packages = gatesUserland ++ [ pkgs.cargo pkgs.rustc ]; } // gatesMarker);

          # `nix develop .#provers` — adds the independent second kernels.
        });
    };
}
