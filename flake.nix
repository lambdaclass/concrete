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
            openssh            # T1 owner SSH signature verification and private controls
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

          # Every gate refuses to run without this (scripts/tests/lib/gate_env.sh). Each shell
          # below that is a superset of the gate userland sets it too.
          gatesMarker = { CONCRETE_GATES_SHELL = "1"; };

          base = with pkgs; [
            # clang BEFORE leanRelease: mkShell puts earlier packages first on PATH, and the Lean
            # release ships its own clang without C headers (stddef.h not found), so with Lean first
            # every C link step that uses PATH's clang (run_tests phase3_abi_interop, regress_103)
            # failed in this shell while passing on CI, whose elan puts no clang on PATH.
            clang
            nix.nix-cli         # gate 9 evaluates lock-derived launch and identity policy
            leanRelease
          ] ++ gatesUserland ++ lib.optionals stdenv.isLinux [
            strace               # gate 9 inventories exec'd compilation-pipeline tools
          ];
          pipelinePackages = [
            pkgs.llvmPackages_21.clang
            pkgs.clang
            leanRelease
            pkgs.git
            pkgs.bash
            pkgs.coreutils
          ];
        in {
          # Default shell stays light: the pre-existing toolchain only. The
          # second-kernel drivers (Rocq, Isabelle) are an opt-in spike and Isabelle
          # is a multi-GB closure, so they live in a separate shell — no contributor
          # pays for them unless they run `--report multi-kernel`.
          default = pkgs.mkShell ({
            packages = base ++ pkgs.lib.optionals pkgs.stdenv.isLinux [ pkgs.systemd ];
            # Gate 9 evaluates this from the checked-in flake/lock, rather than trusting PATH
            # or caller-supplied store roots. Descendant wrappers live in these closures.
            passthru.trustedLaunchPath = pkgs.lib.makeBinPath (base ++ [ pkgs.git ] ++ pkgs.lib.optionals pkgs.stdenv.isLinux [ pkgs.systemd ]);
            passthru.trustedPipelineRoots = map builtins.toString pipelinePackages;
            passthru.trustedIdentity = {
              roots = map builtins.toString pipelinePackages;
              derivations = map (p: {
                path = builtins.toString p;
                drv = builtins.unsafeDiscardOutputDependency p.drvPath;
              }) pipelinePackages;
              toolRoots = {
                lean = [ (builtins.toString leanRelease) ];
                leanc = [ (builtins.toString leanRelease) ];
                lake = [ (builtins.toString leanRelease) ];
                git = [ (builtins.toString pkgs.git) ];
                ar = map builtins.toString [ pkgs.llvmPackages_21.clang pkgs.clang leanRelease ];
                clang = map builtins.toString [ pkgs.llvmPackages_21.clang pkgs.clang leanRelease ];
                clang-21 = map builtins.toString [ pkgs.llvmPackages_21.clang pkgs.clang ];
                ld = map builtins.toString [ pkgs.llvmPackages_21.clang pkgs.clang leanRelease ];
                ld64 = map builtins.toString [ pkgs.llvmPackages_21.clang pkgs.clang leanRelease ];
                "ld.lld" = map builtins.toString [ pkgs.llvmPackages_21.clang pkgs.clang leanRelease ];
                as = map builtins.toString [ pkgs.llvmPackages_21.clang pkgs.clang ];
                bash = map builtins.toString [ pkgs.bash pkgs.llvmPackages_21.clang pkgs.clang ];
                env = map builtins.toString [ pkgs.coreutils pkgs.llvmPackages_21.clang pkgs.clang ];
                uname = map builtins.toString [ pkgs.coreutils pkgs.llvmPackages_21.clang pkgs.clang ];
                mktemp = map builtins.toString [ pkgs.coreutils pkgs.llvmPackages_21.clang pkgs.clang ];
                realpath = map builtins.toString [ pkgs.coreutils pkgs.llvmPackages_21.clang pkgs.clang ];
                rm = map builtins.toString [ pkgs.coreutils pkgs.llvmPackages_21.clang pkgs.clang ];
              };
              # Role boundaries enumerate every allowed tool and root per role independently of the union closure.
              roleToolRoots = let
                compilerTools = cc: let
                  compilers = map builtins.toString [ cc cc.cc ];
                  bintools = map builtins.toString [ cc.bintools cc.bintools.bintools ];
                in {
                  clang = compilers;
                  clang-21 = compilers;
                  ar = bintools;
                  ld = bintools;
                  ld64 = bintools;
                  "ld.lld" = bintools;
                  as = bintools;
                };
                shellTools = cc: {
                  bash = map builtins.toString [ pkgs.bash cc ];
                  env = map builtins.toString [ pkgs.coreutils cc ];
                  uname = map builtins.toString [ pkgs.coreutils cc ];
                  mktemp = map builtins.toString [ pkgs.coreutils cc ];
                  realpath = map builtins.toString [ pkgs.coreutils cc ];
                  rm = map builtins.toString [ pkgs.coreutils cc ];
                };
                noCompilerTools = {
                  clang = [];
                  clang-21 = [];
                  ar = [];
                  ld = [];
                  ld64 = [];
                  "ld.lld" = [];
                  as = [];
                };
                noShellTools = {
                  bash = [];
                  env = [];
                  uname = [];
                  mktemp = [];
                  realpath = [];
                  rm = [];
                };
              in {
                native = (compilerTools pkgs.llvmPackages_21.clang)
                  // (shellTools pkgs.llvmPackages_21.clang)
                  // { lean = []; leanc = []; lake = []; git = []; };
                # leanc uses the shell C compiler for headers/libc; Lean's
                # bundled toolchain also serves Lean-generated code, never native IR.
                lean-build = (pkgs.lib.mapAttrs (_: roots: roots ++ [ (builtins.toString leanRelease) ])
                    (compilerTools pkgs.clang))
                  // (shellTools pkgs.clang)
                  // {
                    lean = [ (builtins.toString leanRelease) ];
                    leanc = [ (builtins.toString leanRelease) ];
                    lake = [ (builtins.toString leanRelease) ];
                    git = [ (builtins.toString pkgs.git) ];
                  };
                lean-emit = noCompilerTools // noShellTools // {
                  lean = [ (builtins.toString leanRelease) ];
                  leanc = [];
                  lake = [];
                  git = [];
                };
              };
              roleLocalTools = {
                native = [ "lcb" ];
                lean-build = [];
                lean-emit = [];
              };
              roleLinkers = {
                native = map builtins.toString [ pkgs.llvmPackages_21.clang.bintools
                  pkgs.llvmPackages_21.clang.bintools.bintools ];
                lean-build = map builtins.toString [ pkgs.clang.bintools
                  pkgs.clang.bintools.bintools leanRelease ];
                lean-emit = [];
              };
              roleCompilers = {
                native = [ "${pkgs.llvmPackages_21.clang}/bin/clang"
                  "${pkgs.llvmPackages_21.clang.cc}/bin/clang" ];
                lean-build = [ "${pkgs.clang}/bin/clang" "${pkgs.clang.cc}/bin/clang"
                  "${leanRelease}/bin/clang" ];
                lean-emit = [];
              };
              lean = builtins.toString leanRelease;
              git = "${pkgs.git}/bin/git";
              nativeClang = "${pkgs.llvmPackages_21.clang}/bin/clang";
            };
            passthru.trustedLeanRoot = builtins.toString leanRelease;
            passthru.trustedGit = "${pkgs.git}/bin/git";
            passthru.trustedNativeClang = "${pkgs.llvmPackages_21.clang}/bin/clang";
            # Slice-0 native builds name their compiler explicitly (scripts/checks/lib/slice0_native.sh),
            # pinned here so the shell's general clang can move without changing their identity.
            CONCRETE_SLICE0_CLANG = "${pkgs.llvmPackages_21.clang}/bin/clang";
            # The artifact gates (scripts/checks/compiler/slice0_artifacts.sh) read the linked executables
            # with the same LLVM release's llvm-objdump and llvm-size, and measure a run under gdb
            # (Linux, pinned) or LLDB (macOS: Apple's /usr/bin/lldb, whose signed debugserver is
            # the only one allowed to attach; not pinned, its identity is recorded in the report).
            CONCRETE_SLICE0_LLVM = "${pkgs.llvmPackages_21.llvm}/bin";
          } // pkgs.lib.optionalAttrs pkgs.stdenv.isLinux {
            CONCRETE_SLICE0_GDB = "${pkgs.gdb}/bin/gdb";
          } // pkgs.lib.optionalAttrs pkgs.stdenv.isDarwin {
            CONCRETE_SLICE0_LLDB = "/usr/bin/lldb";
          } // gatesMarker);

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
