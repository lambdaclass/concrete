- Gate 9 now keeps the complete trusted base in `Concrete/Premises.lean`: trusted components (and the
  explicitly non-trusted, proved BIR→LCB adapter), their premises, removal paths and matched names.
  The golden and README render that registry, while new audits cover every Concrete declaration's
  axioms and trusted-code attributes. On Linux, the gate traces every manifest BIR build and rejects
  unlisted exec'd tools, with planted controls for an extern, an axiom, `objcopy` and removed clang.
  Future slice briefs must declare the trusted components they add or remove.
