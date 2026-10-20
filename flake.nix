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
          base = with pkgs; [
            leanRelease
            clang
            llvmPackages.llvm  # provides lli for fast test execution
            z3                 # external SMT solver for the pre-existing solver_trusted path
            bash
            gnumake
            gnugrep
            coreutils
            # pyyaml so scripts/tests/check_workflow_yaml.sh can actually run: it
            # needs pyyaml or ruby and FAILS (rather than skips) without either, so
            # the gate was red in this shell for everyone.
            (python3.withPackages (ps: [ ps.pyyaml ]))
            rustc
            typst
            zola
            # RocksDB slice 1 (R-0285): the C library examples/rocksdb_kv binds, at the
            # revision flake.lock pins, found through pkg-config. check_rocksdb_slice1.sh
            # fails closed when pkg-config cannot find it; it never skips.
            rocksdb
            pkg-config
          ];
        in {
          # Default shell stays light: the pre-existing toolchain only. The
          # second-kernel drivers (Rocq, Isabelle) are an opt-in spike and Isabelle
          # is a multi-GB closure, so they live in a separate shell — no contributor
          # pays for them unless they run `--report multi-kernel`.
          default = pkgs.mkShell { packages = base; };

          # `nix develop .#provers` — adds the independent second kernels.
          provers = pkgs.mkShell {
            packages = base ++ (with pkgs; [
              coq                # independent CIC kernel (coqc/lia)
              coqPackages.stdlib # Rocq 9.0 stdlib (ZArith, Lia) — split out of coq-core
              isabelle           # independent HOL kernel (presburger) — foundational independence
              # Certificate-replay toolchain (see docs/verification/SMT_SOUNDNESS.md). Makes a
              # solver's PROOF checkable rather than its verdict merely corroborated.
              drat-trim          # DRAT/LRAT checker for bit-blasted (SAT) certificates
              # cvc5 is here because check_multi_kernel.sh CONSUMES it: it locks the
              # measured ceiling that SMT datatype reasoning is provable but not
              # Alethe-certifiable, which is the reason the non-arithmetic tier stays
              # kernel-proved. It was previously dropped as unused — correct then, wrong
              # now. Isabelle's own CVC5_SOLVER is internal to its settings and is NOT
              # exported to the shell, so relying on that left the check silently
              # skipping with no change in the assertion count.
              cvc5
            ]);
            # No veriT here on purpose (cvc5 IS listed above — see its comment).
            # Isabelle's nix package already bundles `contrib/verit-2021.06.2-rmx-3`,
            # registers it in etc/components, and sets ISABELLE_VERIT — verified by
            # running `smt (verit)` under plain `nix shell nixpkgs#isabelle`, where it
            # closes a linear goal oracle-free. (`VERIT_SOLVER` is empty there, which is
            # misleading: the component variable Isabelle actually reads is
            # ISABELLE_VERIT, visible only via `isabelle getenv`.)
            #
            # Isabelle's bundled CVC5_SOLVER is a DIFFERENT build from the cvc5 above
            # (measured 2026-07-31: contrib 1.2.0 vs shell 1.3.2). Anything asserting a
            # version-scoped fact about cvc5 must therefore report which one it ran;
            # check_multi_kernel.sh prints it for exactly this reason.
          };
        });
    };
}
