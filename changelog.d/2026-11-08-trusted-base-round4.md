- Gate 9 separates writer-produced candidate trust digests from owner authentication.
  T1 reports authorization not established until the owner chooses an authenticator
  and approves the initial canonical digest (R-0678 / R-0509). Historical citations
  are context; regenerating the report and JSON cannot establish authorization.
- Traced tools are checked under native compile/link, Lean build, or Lean emission
  roles with per-role tool and root enumerations in `flake.nix` (`lean-emit`
  forbids `leanc`, `lake`, `git` and local `lcb`; `native` forbids `lean`, `leanc`,
  `lake` and `git` and includes `cc.bintools.bintools`). Live and focused controls
  cover role substitutions against the evaluated `identity.json` and cross-component
  resolution inside the store, and `trustedDeclarationKinds` / `slice0_examples.lean`
  audit nested auxiliaries, `ci.value?` constant closures, and non-`partial`
  `while`/`repeat` definitions without depending on an `example`/`partial` prefilter.
