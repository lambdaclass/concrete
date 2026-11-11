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
          # LEAN IS THE OFFICIAL v4.35.0 RELEASE ARCHIVE, not nixpkgs' lean4 (R-0208). The checker identity a
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
                "aarch64-darwin" = { name = "darwin_aarch64"; sha256 = "c0c6f34f35ba1dcc5c4a5fd106fefab9eb5e8469712624dd2c868d51b63177c2"; };
                "x86_64-darwin"  = { name = "darwin";         sha256 = "0b43be5c7ecfb36016566a41005ab62bdb04743e6bfd5837e45f796ef4750b4e"; };
                "x86_64-linux"   = { name = "linux";          sha256 = "7289597a44543246c9227b8cae501767e095ce17f6d448b95fc4a8dc97268f6d"; };
                "aarch64-linux"  = { name = "linux_aarch64";  sha256 = "c8f0f8a3920e839718b1343e6781fd80a3e9b1024fbcd7078eb30f54e806b028"; };
              }.${sys};
            in pkgs.stdenv.mkDerivation {
              pname = "lean4-release";
              version = "4.35.0";
              src = pkgs.fetchurl {
                url = "https://github.com/leanprover/lean4/releases/download/v4.35.0/lean-4.35.0-${asset.name}.tar.zst";
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
              installPhase = "mkdir -p $out && cp -R lean-4.35.0-${asset.name}/. $out/";
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
          # Default shell includes the pinned Lean toolchain. External-check builders
          # live in extcheck. The second-kernel drivers (Rocq, Isabelle) are an opt-in spike and Isabelle
          # is a multi-GB closure, so they live in a separate shell — no contributor
          # pays for them unless they run `--report multi-kernel`.
          default = pkgs.mkShell ({
            packages = base;
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

          # External builders add Rust and a C linker, but no Lean/Lake proxy shims.
          # Nix elan supplies cache identity via --version; pinned Lean tool builds invoke it
          # by absolute path with an identity-keyed isolated home.
          # The project's Lake and Lean remain those supplied by the caller.
          extcheck = pkgs.mkShell ({
            packages = gatesUserland ++ [ pkgs.cargo pkgs.rustc ];
            CONCRETE_EXT_ELAN = "${pkgs.elan}/bin/elan";
          } // gatesMarker);

          # `nix develop .#provers` — adds the independent second kernels.
        });
    };
}
