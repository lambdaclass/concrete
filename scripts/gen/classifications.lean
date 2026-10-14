-- Generator for `Concrete/Proof/ClassificationTable.lean`. Do not hand-edit the OUTPUT.
--
-- Classification reads a theorem's ELABORATED TYPE, which needs the proof modules imported.
-- The compiler imports neither and cannot: `Examples` imports `Concrete`, so importing the
-- proofs from `Concrete` would be a cycle. The answer is computed here and checked in as data.
--
-- THE NAME SET IS THE UNION OF BOTH WAYS A PROOF CAN BE ATTACHED, and getting that wrong is
-- how the first version of this table looked complete while missing a third of the corpus:
--
--   * `Proof.provedFunctions` — the hardcoded table;
--   * `#[proof_by(...)]` / `#[ensures_proof(...)]` in SOURCE — spliced in below by
--     `scripts/gen/refresh_classifications.sh`, which greps them from examples/.
--
-- `main.verify_message` was `proved` with a REFUSING dependency root because only the first
-- set was enumerated. Its theorem classifies as `body` perfectly well; nobody had asked.
--
-- Regenerate: bash scripts/gen/refresh_classifications.sh
import Concrete
import Examples
open Lean Meta Concrete Concrete.Proof

/-- Source-linked proof names, spliced by the refresh script. -/
def sourceLinkedThms : List Name :=
  [ `Concrete.Proof.compute_checksum_correct,
    `Examples.ConstantTimeTag.Proofs.ct_compare_different_tag_correct,
    `Examples.ConstantTimeTag.Proofs.ct_compare_same_tag_correct,
    `Examples.CryptoVerify.Proofs.check_nonce_correct,
    `Examples.CryptoVerify.Proofs.compute_tag_correct,
    `Examples.CryptoVerify.Proofs.verify_message_composed_correct,
    `Examples.CryptoVerify.Proofs.verify_tag_correct,
    `Examples.DoesNotExist.fake_theorem,
    `Examples.ElfHeader.Proofs.check_class_correct,
    `Examples.ElfHeader.Proofs.check_data_correct,
    `Examples.ElfHeader.Proofs.check_magic_correct,
    `Examples.ElfHeader.Proofs.check_version_correct,
    `Examples.ElfHeader.Proofs.validate_header_correct,
    `Examples.FixedCapacity.Proofs.compute_tag_zero_correct,
    `Examples.FixedCapacity.Proofs.ring_new_correct,
    `Examples.FixedCapacity.Proofs.ring_push_then_contains_correct,
    `Examples.FixedCapacity.Proofs.ring_push_zero_correct,
    `Examples.HmacSha256.Proofs.block_to_words_at_refines_spec,
    `Examples.HmacSha256.Proofs.block_to_words_refines_spec,
    `Examples.HmacSha256.Proofs.ch_refines,
    `Examples.HmacSha256.Proofs.ch_selects_high,
    `Examples.HmacSha256.Proofs.hmac_sha256_refines_spec,
    `Examples.HmacSha256.Proofs.round_refines_list,
    `Examples.HmacSha256.Proofs.sha256_compress_at_refines_spec,
    `Examples.HmacSha256.Proofs.sha256_compress_refines_spec,
    `Examples.HmacSha256.Proofs.sha256_hash_refines_spec,
    `Examples.HmacSha256.Proofs.sha256_init_correct,
    `Examples.HmacSha256.Proofs.sha256_schedule_refines_spec,
    `Examples.HmacSha256.Proofs.state_to_bytes_refines_spec,
    `Examples.LoopInvariant.Proofs.count_up_loop_preserves,
    `Examples.ParseValidate.Proofs.parse_header_too_short,
    `Examples.ParseValidate.Proofs.validate_header_fields_success,
    `Examples.ParseValidate.Proofs.validate_version_correct,
    `Examples.ProofPatterns.Proofs.add_three_correct,
    `Examples.ProofPatterns.Proofs.add_three_not_yet_proved,
    `Examples.ProofPatterns.Proofs.combine_correct,
    `Examples.ProofPatterns.Proofs.copy2_copies_faithfully,
    `Examples.ProofPatterns.Proofs.dbl_correct,
    `Examples.ProofPatterns.Proofs.ghost_sum_correct,
    `Examples.ProofPatterns.Proofs.inc_correct,
    `Examples.ProofPatterns.Proofs.put_writes_index_1_frames_rest,
    `Examples.ProofPatterns.Proofs.scale_by_two_correct,
    `Examples.ProofPatterns.Proofs.sum4_totals_concrete
  ]

#eval show MetaM Unit from do
  let hardcoded := (Proof.provedFunctions.map (fun t => t.2.2)).map (·.toName)
  let names := (hardcoded ++ sourceLinkedThms).eraseDups
  let rows ← classifyAll names
  -- Rows whose edge is `unclassified` are OMITTED: `classifiedEdgeOf` already answers
  -- `unclassified` for an absent name, so listing them would state the default twice and grow
  -- the table with rows that say nothing.
  let useful := rows.filter (fun r => r.2.edge != DependencyEdge.unclassified)
  -- The theorem's own digest travels with its classification. Without it the row is a label
  -- floating free of what it classifies, and survives the theorem being reproved or replaced.
  let mut lits : List String := []
  for (n, ev) in useful do
    -- NO PLACEHOLDER. `.getD "?"` would emit a row whose digest slot is filled with something
    -- that is not a digest, and a consumer comparing digests would then accept it as one. A
    -- theorem whose artifact cannot be digested must ABORT generation: the table's value is that
    -- every row is checkable, and one uncheckable row makes the whole table's guarantee "most of
    -- these are verified", which is not a guarantee.
    let d ← match (← theoremArtifactDigest n) with
      | some d => pure d
      | none => throwError s!"cannot digest theorem artifact for {n} — refusing to emit a placeholder row"
    -- TABLE IDENTITIES AND DIGESTS travel with the classification. Without them the row says
    -- which THEOREM was classified but not which DEPENDENCIES the classification is about, so a
    -- `body` label cannot be checked against the edges it is used to type. Sorted by table name
    -- so discovery order cannot enter the row.
    let tbls := ev.tableDigests.mergeSort (fun a b => toString a.1 ≤ toString b.1)
    let mut tlits : List String := []
    for (tn, td?) in tbls do
      match td? with
      | some td => tlits := tlits ++ [s!"(\"{tn}\", \"{td}\")"]
      | none =>
        -- An unbound table cannot be emitted: the row would claim a dependency it could not
        -- describe, and a consumer comparing digests would have nothing to compare.
        throwError s!"table {tn} of {n} is unbound — refusing to emit a row that names a dependency it cannot bind"
    let tstr := "[" ++ String.intercalate ", " tlits ++ "]"
    let q := if ev.quantifiesOverTable then "true" else "false"
    -- THE SPECS THE THEOREM IS ABOUT. Without this a claim could name any accepted theorem in
    -- `#[proof_by(...)]` and the receipt would record it as evidence for a subject the theorem
    -- never mentions — the break an independent adversarial review found.
    let slits := ev.specs.map (fun sn => s!"\"{sn}\"")
    let sstr := "[" ++ String.intercalate ", " slits ++ "]"
    lits := lits ++ [s!"  (\"{n}\", \"{ev.edge.canonical}\", \"{d}\", {tstr}, {q}, {sstr})"]
  IO.println (String.intercalate ",\n" lits)
  -- OUT-OF-BUILD TABLE ENTRIES. The compiler resolves a table NAME to its value through a closed
  -- dispatch, which covers every table defined inside the compiler. It cannot cover tables defined
  -- under `proofs/`, because `Examples` imports `Concrete` and the reverse would be a cycle — so
  -- for those, and ONLY those, the entries cross as data.
  --
  -- Referenced DIRECTLY rather than looked up by name: this module imports `Examples`, so the value
  -- is in scope and no `evalExpr` is involved. The list is explicit for the same reason the
  -- compiler's dispatch is: a name-keyed lookup that guessed would bind the wrong table.
  IO.println "-- EXTERNAL"
  -- EVERY out-of-build table a theorem names, not just the one that was noticed first. `shaFns` was
  -- absent, so `tableContainsCallee` could not answer for it at all — and "cannot tell" was being
  -- folded into "attested" at the dependency-request boundary, which reports an UNCHECKED membership
  -- with the same word as a checked one. Emitting its entries makes the question answerable, which
  -- is strictly better than typing the ignorance.
  let externals : List (String × FnTable) :=
    [ ("Examples.ProofPatterns.Proofs.combineFns", Examples.ProofPatterns.Proofs.combineFns)
    , ("Examples.HmacSha256.Proofs.shaFns", Examples.HmacSha256.Proofs.shaFns) ]
  let mut elits : List String := []
  let mut slits : List String := []
  for (nm, t) in externals do
    let mut rows : List String := []
    for d in t.canonicalEntries.toList do
      match d.identity.id?, d.sourceBodyDigest with
      | some cid, some sbd => rows := rows ++ [s!"(\"{cid.defModule}\", \"{cid.declName}\", \"{sbd.value}\")"]
      -- An entry without identity or provenance is NOT emitted as a partial row: the compiler
      -- would then hold a membership list that under-reports, which reads as absence.
      | _, _ => throwError s!"external table {nm} has an entry lacking identity or provenance — refusing to emit a partial membership list"
    -- The digest is computed by the SAME function the compiler uses, over evidence built the same
    -- way. Two copies of the formula would be two answers to "is this the table the theorem bound".
    let ev ← match tableEntryEvidence t with
      | .ok e => pure e
      | .error w => throwError s!"external table {nm}: {w.explain}"
    let dg := entryTableDigest nm ev
    elits := elits ++ [s!"  (\"{nm}\", \"{dg}\", [" ++ String.intercalate ", " rows ++ "])"]
    -- THE SCOPED MEMBERSHIP OF AN OUT-OF-BUILD TABLE, crossing as data for the same reason its
    -- entries do. Once the evidence join keys on `DefinitionIdentity`, "does this table hold an
    -- attested entry with THIS definition" has to be answerable for every table a theorem names —
    -- and the compiler cannot hold `combineFns` or `shaFns`. Emitted from the table's own
    -- `attested` array, so it is the same binding the site made, not a re-derivation.
    -- EMITTED FROM `scopedEntryEvidence`, NOT FROM THE RAW `attested` ARRAY.
    --
    -- Serializing `t.attested` directly skipped every check that makes the scoped path trustworthy:
    -- that each attested model is actually IN the table, that its stored provenance agrees with the
    -- digest recomputed from its body, that no two entries attest the same definition, and that no
    -- generated reference failed validation. The compiler cannot re-derive any of that for an
    -- out-of-build table — it holds rows and no bodies — so whatever this emits is what it must
    -- believe. And because freshness regenerates through this same producer, a defect here would
    -- verify against itself forever.
    --
    -- `scopedEntryEvidence` is the function the IN-BUILD path uses for exactly these questions, so
    -- calling it here means both paths are validated by one implementation rather than one being
    -- validated and the other transcribed.
    let srowsRes ← match scopedEntryEvidence t with
      | .error w => throwError s!"external table {nm}: scoped entry evidence refuses ({w.explain}) — refusing to emit membership the compiler could not re-derive"
      | .ok scopedRows => pure scopedRows
    let mut srows : List String := []
    for r in srowsRes do
      let d := r.definition
      srows := srows ++ [s!"(\"{d.scope}\", \"{d.moduleIdentity}\", \"{d.declarationIdentity}\", \"{d.implementationIdentity}\", \"{r.sourceBodyDigest}\")"]
    slits := slits ++ [s!"  (\"{nm}\", [" ++ String.intercalate ", " srows ++ "])"]
  IO.println (String.intercalate ",\n" elits)
  IO.println "-- EXTERNAL-SCOPED"
  IO.println (String.intercalate ",\n" slits)
