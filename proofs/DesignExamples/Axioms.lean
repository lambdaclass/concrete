import DesignExamples.Linear
import DesignExamples.LinearSound
import DesignExamples.Borrow
import DesignExamples.Cleanup
import DesignExamples.Repr

/-!
# Axiom footprint of every design-example theorem

Each theorem's `#print axioms` output is pinned with `#guard_msgs`, so building this module
fails if any theorem gains `sorryAx`, a user axiom, or native-code trust
(`Lean.ofReduceBool`, `Lean.trustCompiler`, `._native.` axioms). The only axioms that appear
are Lean's standard `propext`, `Classical.choice` and `Quot.sound` — the kernel-only set the
plan's axiom policy (S12a, exit 2g) permits. A new theorem must be added here; the README
says how to regenerate this file.
-/

/-- info: 'DesignExamples.Linear.straight_accepted' depends on axioms: [propext] -/
#guard_msgs (whitespace := lax) in
#print axioms DesignExamples.Linear.straight_accepted

/-- info: 'DesignExamples.Linear.branches_accepted' depends on axioms: [propext] -/
#guard_msgs (whitespace := lax) in
#print axioms DesignExamples.Linear.branches_accepted

/-- info: 'DesignExamples.Linear.earlyReturn_accepted' depends on axioms: [propext] -/
#guard_msgs (whitespace := lax) in
#print axioms DesignExamples.Linear.earlyReturn_accepted

/-- info: 'DesignExamples.Linear.straight_safe' depends on axioms: [propext] -/
#guard_msgs (whitespace := lax) in
#print axioms DesignExamples.Linear.straight_safe

/-- info: 'DesignExamples.Linear.branches_safe' depends on axioms: [propext] -/
#guard_msgs (whitespace := lax) in
#print axioms DesignExamples.Linear.branches_safe

/-- info: 'DesignExamples.Linear.earlyReturn_safe' depends on axioms: [propext] -/
#guard_msgs (whitespace := lax) in
#print axioms DesignExamples.Linear.earlyReturn_safe

/-- info: 'DesignExamples.Linear.earlyReturn_paths' depends on axioms: [propext] -/
#guard_msgs (whitespace := lax) in
#print axioms DesignExamples.Linear.earlyReturn_paths

/-- info: 'DesignExamples.Linear.earlyReturn_erasure' depends on axioms: [propext] -/
#guard_msgs (whitespace := lax) in
#print axioms DesignExamples.Linear.earlyReturn_erasure

/-- info: 'DesignExamples.Linear.branches_erasure' depends on axioms: [propext] -/
#guard_msgs (whitespace := lax) in
#print axioms DesignExamples.Linear.branches_erasure

/-- info: 'DesignExamples.Linear.leakOnReturn_rejected' depends on axioms: [propext] -/
#guard_msgs (whitespace := lax) in
#print axioms DesignExamples.Linear.leakOnReturn_rejected

/-- info: 'DesignExamples.Linear.leakOnReturn_leaks' depends on axioms: [propext] -/
#guard_msgs (whitespace := lax) in
#print axioms DesignExamples.Linear.leakOnReturn_leaks

/-- info: 'DesignExamples.Linear.useAfterMove_rejected' depends on axioms: [propext] -/
#guard_msgs (whitespace := lax) in
#print axioms DesignExamples.Linear.useAfterMove_rejected

/-- info: 'DesignExamples.Linear.useAfterMove_double_free' depends on axioms: [propext] -/
#guard_msgs (whitespace := lax) in
#print axioms DesignExamples.Linear.useAfterMove_double_free

/-- info: 'DesignExamples.Linear.disagree_rejected' depends on axioms: [propext] -/
#guard_msgs (whitespace := lax) in
#print axioms DesignExamples.Linear.disagree_rejected

/-- info: 'DesignExamples.Linear.disagree_double_free' depends on axioms: [propext] -/
#guard_msgs (whitespace := lax) in
#print axioms DesignExamples.Linear.disagree_double_free

/-- info: 'DesignExamples.Linear.shadowLive_rejected' depends on axioms: [propext] -/
#guard_msgs (whitespace := lax) in
#print axioms DesignExamples.Linear.shadowLive_rejected

/-- info: 'DesignExamples.Linear.shadowLive_leaks' depends on axioms: [propext] -/
#guard_msgs (whitespace := lax) in
#print axioms DesignExamples.Linear.shadowLive_leaks

/-- info: 'DesignExamples.Linear.lookup_isSome_iff' depends on axioms: [propext, Quot.sound] -/
#guard_msgs (whitespace := lax) in
#print axioms DesignExamples.Linear.lookup_isSome_iff

/-- info: 'DesignExamples.Linear.mem_of_lookup' depends on axioms: [propext] -/
#guard_msgs (whitespace := lax) in
#print axioms DesignExamples.Linear.mem_of_lookup

/-- info: 'DesignExamples.Linear.erase_eq_filter' depends on axioms: [propext, Quot.sound] -/
#guard_msgs (whitespace := lax) in
#print axioms DesignExamples.Linear.erase_eq_filter

/-- info: 'DesignExamples.Linear.lookup_erase' depends on axioms: [propext] -/
#guard_msgs (whitespace := lax) in
#print axioms DesignExamples.Linear.lookup_erase

/-- info: 'DesignExamples.Linear.key_unique' depends on axioms: [propext, Quot.sound] -/
#guard_msgs (whitespace := lax) in
#print axioms DesignExamples.Linear.key_unique

/-- info: 'DesignExamples.Linear.res_unique' depends on axioms: [propext, Quot.sound] -/
#guard_msgs (whitespace := lax) in
#print axioms DesignExamples.Linear.res_unique

/-- info: 'DesignExamples.Linear.frees_alloc' depends on axioms: [propext] -/
#guard_msgs (whitespace := lax) in
#print axioms DesignExamples.Linear.frees_alloc

/-- info: 'DesignExamples.Linear.frees_free' depends on axioms: [propext] -/
#guard_msgs (whitespace := lax) in
#print axioms DesignExamples.Linear.frees_free

/-- info: 'DesignExamples.Linear.mem_fst_filter' depends on axioms: [propext, Quot.sound] -/
#guard_msgs (whitespace := lax) in
#print axioms DesignExamples.Linear.mem_fst_filter

/-- info: 'DesignExamples.Linear.mem_snd_filter' depends on axioms: [propext, Quot.sound] -/
#guard_msgs (whitespace := lax) in
#print axioms DesignExamples.Linear.mem_snd_filter

/-- info: 'DesignExamples.Linear.ite_t' depends on axioms: [propext] -/
#guard_msgs (whitespace := lax) in
#print axioms DesignExamples.Linear.ite_t

/-- info: 'DesignExamples.Linear.ite_f' depends on axioms: [propext] -/
#guard_msgs (whitespace := lax) in
#print axioms DesignExamples.Linear.ite_f

/-- info: 'DesignExamples.Linear.ite_iff' depends on axioms: [propext] -/
#guard_msgs (whitespace := lax) in
#print axioms DesignExamples.Linear.ite_iff

/-- info: 'DesignExamples.Linear.inv_acquire' depends on axioms: [propext, Quot.sound] -/
#guard_msgs (whitespace := lax) in
#print axioms DesignExamples.Linear.inv_acquire

/-- info: 'DesignExamples.Linear.inv_move' depends on axioms: [propext, Classical.choice, Quot.sound] -/
#guard_msgs (whitespace := lax) in
#print axioms DesignExamples.Linear.inv_move

/-- info: 'DesignExamples.Linear.inv_destroy' depends on axioms: [propext, Classical.choice, Quot.sound] -/
#guard_msgs (whitespace := lax) in
#print axioms DesignExamples.Linear.inv_destroy

/-- info: 'DesignExamples.Linear.inv_setEq' depends on axioms: [propext, Quot.sound] -/
#guard_msgs (whitespace := lax) in
#print axioms DesignExamples.Linear.inv_setEq

/-- info: 'DesignExamples.Linear.some_fall_inj' depends on axioms: [propext] -/
#guard_msgs (whitespace := lax) in
#print axioms DesignExamples.Linear.some_fall_inj

/-- info: 'DesignExamples.Linear.chk_nodup' depends on axioms: [propext, Quot.sound] -/
#guard_msgs (whitespace := lax) in
#print axioms DesignExamples.Linear.chk_nodup

/-- info: 'DesignExamples.Linear.good_change' depends on axioms: [propext, Quot.sound] -/
#guard_msgs (whitespace := lax) in
#print axioms DesignExamples.Linear.good_change

/-- info: 'DesignExamples.Linear.exec_sound' depends on axioms: [propext, Classical.choice, Quot.sound] -/
#guard_msgs (whitespace := lax) in
#print axioms DesignExamples.Linear.exec_sound

/-- info: 'DesignExamples.Linear.inv_noDoubleDestroy' depends on axioms: [propext, Quot.sound] -/
#guard_msgs (whitespace := lax) in
#print axioms DesignExamples.Linear.inv_noDoubleDestroy

/-- info: 'DesignExamples.Linear.inv_nil_noLeak' depends on axioms: [propext, Quot.sound] -/
#guard_msgs (whitespace := lax) in
#print axioms DesignExamples.Linear.inv_nil_noLeak

/-- info: 'DesignExamples.Linear.checkFn_sound' depends on axioms: [propext, Classical.choice, Quot.sound] -/
#guard_msgs (whitespace := lax) in
#print axioms DesignExamples.Linear.checkFn_sound

/-- info: 'DesignExamples.Linear.erasure' depends on axioms: [propext] -/
#guard_msgs (whitespace := lax) in
#print axioms DesignExamples.Linear.erasure

/-- info: 'DesignExamples.Linear.checkFn_erasure' depends on axioms: [propext, Classical.choice, Quot.sound] -/
#guard_msgs (whitespace := lax) in
#print axioms DesignExamples.Linear.checkFn_erasure

/-- info: 'DesignExamples.Borrow.reborrowEx_accepted' depends on axioms: [propext] -/
#guard_msgs (whitespace := lax) in
#print axioms DesignExamples.Borrow.reborrowEx_accepted

/-- info: 'DesignExamples.Borrow.reborrowEx_runs' does not depend on any axioms -/
#guard_msgs (whitespace := lax) in
#print axioms DesignExamples.Borrow.reborrowEx_runs

/-- info: 'DesignExamples.Borrow.sharedEx_accepted' depends on axioms: [propext] -/
#guard_msgs (whitespace := lax) in
#print axioms DesignExamples.Borrow.sharedEx_accepted

/-- info: 'DesignExamples.Borrow.sharedEx_runs' does not depend on any axioms -/
#guard_msgs (whitespace := lax) in
#print axioms DesignExamples.Borrow.sharedEx_runs

/-- info: 'DesignExamples.Borrow.escapeEx_rejected' depends on axioms: [propext] -/
#guard_msgs (whitespace := lax) in
#print axioms DesignExamples.Borrow.escapeEx_rejected

/-- info: 'DesignExamples.Borrow.escapeEx_dangles' does not depend on any axioms -/
#guard_msgs (whitespace := lax) in
#print axioms DesignExamples.Borrow.escapeEx_dangles

/-- info: 'DesignExamples.Borrow.escapeViaCallee_rejected' depends on axioms: [propext] -/
#guard_msgs (whitespace := lax) in
#print axioms DesignExamples.Borrow.escapeViaCallee_rejected

/-- info: 'DesignExamples.Borrow.releaseWhileBorrowed_rejected' depends on axioms: [propext] -/
#guard_msgs (whitespace := lax) in
#print axioms DesignExamples.Borrow.releaseWhileBorrowed_rejected

/-- info: 'DesignExamples.Borrow.releaseWhileBorrowed_dangles' does not depend on any axioms -/
#guard_msgs (whitespace := lax) in
#print axioms DesignExamples.Borrow.releaseWhileBorrowed_dangles

/-- info: 'DesignExamples.Borrow.parentDuringReborrow_rejected' depends on axioms: [propext] -/
#guard_msgs (whitespace := lax) in
#print axioms DesignExamples.Borrow.parentDuringReborrow_rejected

/-- info: 'DesignExamples.Borrow.parentDuringReborrow_stuck' does not depend on any axioms -/
#guard_msgs (whitespace := lax) in
#print axioms DesignExamples.Borrow.parentDuringReborrow_stuck

/-- info: 'DesignExamples.Borrow.twoExclusive_rejected' depends on axioms: [propext] -/
#guard_msgs (whitespace := lax) in
#print axioms DesignExamples.Borrow.twoExclusive_rejected

/-- info: 'DesignExamples.Borrow.twoExclusive_stuck' does not depend on any axioms -/
#guard_msgs (whitespace := lax) in
#print axioms DesignExamples.Borrow.twoExclusive_stuck

/-- info: 'DesignExamples.Borrow.ownerReadWhileExcl_rejected' depends on axioms: [propext] -/
#guard_msgs (whitespace := lax) in
#print axioms DesignExamples.Borrow.ownerReadWhileExcl_rejected

/-- info: 'DesignExamples.Borrow.ownerReadWhileExcl_stuck' does not depend on any axioms -/
#guard_msgs (whitespace := lax) in
#print axioms DesignExamples.Borrow.ownerReadWhileExcl_stuck

/-- info: 'DesignExamples.Borrow.writeThroughShared_rejected' depends on axioms: [propext] -/
#guard_msgs (whitespace := lax) in
#print axioms DesignExamples.Borrow.writeThroughShared_rejected

/-- info: 'DesignExamples.Borrow.writeThroughShared_stuck' does not depend on any axioms -/
#guard_msgs (whitespace := lax) in
#print axioms DesignExamples.Borrow.writeThroughShared_stuck

/-- info: 'DesignExamples.Borrow.writeParentDuringSharedReborrow_rejected' depends on axioms: [propext] -/
#guard_msgs (whitespace := lax) in
#print axioms DesignExamples.Borrow.writeParentDuringSharedReborrow_rejected

/-- info: 'DesignExamples.Borrow.writeParentDuringSharedReborrow_stuck' does not depend on any axioms -/
#guard_msgs (whitespace := lax) in
#print axioms DesignExamples.Borrow.writeParentDuringSharedReborrow_stuck

/-- info: 'DesignExamples.Cleanup.normalEx_order' depends on axioms: [propext] -/
#guard_msgs (whitespace := lax) in
#print axioms DesignExamples.Cleanup.normalEx_order

/-- info: 'DesignExamples.Cleanup.earlyReturnEx_paths' depends on axioms: [propext] -/
#guard_msgs (whitespace := lax) in
#print axioms DesignExamples.Cleanup.earlyReturnEx_paths

/-- info: 'DesignExamples.Cleanup.loopEx_paths' depends on axioms: [propext] -/
#guard_msgs (whitespace := lax) in
#print axioms DesignExamples.Cleanup.loopEx_paths

/-- info: 'DesignExamples.Cleanup.abortEx_no_cleanup' depends on axioms: [propext] -/
#guard_msgs (whitespace := lax) in
#print axioms DesignExamples.Cleanup.abortEx_no_cleanup

/-- info: 'DesignExamples.Cleanup.deferPrint_exit_time' depends on axioms: [propext] -/
#guard_msgs (whitespace := lax) in
#print axioms DesignExamples.Cleanup.deferPrint_exit_time

/-- info: 'DesignExamples.Cleanup.normalEx_lowered' depends on axioms: [propext] -/
#guard_msgs (whitespace := lax) in
#print axioms DesignExamples.Cleanup.normalEx_lowered

/-- info: 'DesignExamples.Cleanup.earlyReturnEx_lowered' depends on axioms: [propext] -/
#guard_msgs (whitespace := lax) in
#print axioms DesignExamples.Cleanup.earlyReturnEx_lowered

/-- info: 'DesignExamples.Cleanup.loopEx_lowered' depends on axioms: [propext] -/
#guard_msgs (whitespace := lax) in
#print axioms DesignExamples.Cleanup.loopEx_lowered

/-- info: 'DesignExamples.Cleanup.abortEx_lowered' depends on axioms: [propext] -/
#guard_msgs (whitespace := lax) in
#print axioms DesignExamples.Cleanup.abortEx_lowered

/-- info: 'DesignExamples.Cleanup.deferPrint_lowered' depends on axioms: [propext] -/
#guard_msgs (whitespace := lax) in
#print axioms DesignExamples.Cleanup.deferPrint_lowered

/-- info: 'DesignExamples.Repr.le_length' depends on axioms: [propext] -/
#guard_msgs (whitespace := lax) in
#print axioms DesignExamples.Repr.le_length

/-- info: 'DesignExamples.Repr.unle_le' depends on axioms: [propext, Classical.choice, Quot.sound] -/
#guard_msgs (whitespace := lax) in
#print axioms DesignExamples.Repr.unle_le

/-- info: 'DesignExamples.Repr.encode_length' depends on axioms: [propext] -/
#guard_msgs (whitespace := lax) in
#print axioms DesignExamples.Repr.encode_length

/-- info: 'DesignExamples.Repr.decodeInt_le_of_lt' depends on axioms: [propext, Classical.choice, Quot.sound] -/
#guard_msgs (whitespace := lax) in
#print axioms DesignExamples.Repr.decodeInt_le_of_lt

/-- info: 'DesignExamples.Repr.decode_encode' depends on axioms: [propext, Classical.choice, Quot.sound] -/
#guard_msgs (whitespace := lax) in
#print axioms DesignExamples.Repr.decode_encode

/-- info: 'DesignExamples.Repr.alignUp_agrees_with_Layout' does not depend on any axioms -/
#guard_msgs (whitespace := lax) in
#print axioms DesignExamples.Repr.alignUp_agrees_with_Layout

/-- info: 'DesignExamples.Repr.le_alignUp' depends on axioms: [propext] -/
#guard_msgs (whitespace := lax) in
#print axioms DesignExamples.Repr.le_alignUp

/-- info: 'DesignExamples.Repr.le_fieldsEnd' depends on axioms: [propext, Quot.sound] -/
#guard_msgs (whitespace := lax) in
#print axioms DesignExamples.Repr.le_fieldsEnd

/-- info: 'DesignExamples.Repr.encFields_length' depends on axioms: [propext, Quot.sound] -/
#guard_msgs (whitespace := lax) in
#print axioms DesignExamples.Repr.encFields_length

/-- info: 'DesignExamples.Repr.decFields_encFields' depends on axioms: [propext, Classical.choice, Quot.sound] -/
#guard_msgs (whitespace := lax) in
#print axioms DesignExamples.Repr.decFields_encFields

/-- info: 'DesignExamples.Repr.decodeStruct_encodeStruct' depends on axioms: [propext, Classical.choice, Quot.sound] -/
#guard_msgs (whitespace := lax) in
#print axioms DesignExamples.Repr.decodeStruct_encodeStruct

/-- info: 'DesignExamples.Repr.load_store' depends on axioms: [propext, Classical.choice, Quot.sound] -/
#guard_msgs (whitespace := lax) in
#print axioms DesignExamples.Repr.load_store

/-- info: 'DesignExamples.Repr.S_layout' does not depend on any axioms -/
#guard_msgs (whitespace := lax) in
#print axioms DesignExamples.Repr.S_layout

/-- info: 'DesignExamples.Repr.S_bytes' depends on axioms: [propext] -/
#guard_msgs (whitespace := lax) in
#print axioms DesignExamples.Repr.S_bytes

/-- info: 'DesignExamples.Repr.S_padding_unobservable' depends on axioms: [propext] -/
#guard_msgs (whitespace := lax) in
#print axioms DesignExamples.Repr.S_padding_unobservable

/-- info: 'DesignExamples.Repr.S_padding_undef' depends on axioms: [propext] -/
#guard_msgs (whitespace := lax) in
#print axioms DesignExamples.Repr.S_padding_undef

/-- info: 'DesignExamples.Repr.ptr_bytes_not_int' depends on axioms: [propext] -/
#guard_msgs (whitespace := lax) in
#print axioms DesignExamples.Repr.ptr_bytes_not_int

/-- info: 'DesignExamples.Repr.int_bytes_not_ptr' depends on axioms: [propext] -/
#guard_msgs (whitespace := lax) in
#print axioms DesignExamples.Repr.int_bytes_not_ptr

/-- info: 'DesignExamples.Repr.torn_pointer' depends on axioms: [propext] -/
#guard_msgs (whitespace := lax) in
#print axioms DesignExamples.Repr.torn_pointer

/-- info: 'DesignExamples.Repr.bool_invalid_byte' depends on axioms: [propext] -/
#guard_msgs (whitespace := lax) in
#print axioms DesignExamples.Repr.bool_invalid_byte

/-- info: 'DesignExamples.Repr.fresh_is_undef' depends on axioms: [propext] -/
#guard_msgs (whitespace := lax) in
#print axioms DesignExamples.Repr.fresh_is_undef

/-- info: 'DesignExamples.Repr.access_rules' depends on axioms: [propext] -/
#guard_msgs (whitespace := lax) in
#print axioms DesignExamples.Repr.access_rules
