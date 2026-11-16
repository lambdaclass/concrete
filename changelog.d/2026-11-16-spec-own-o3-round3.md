### F16 round 3: close ownership protocol evidence gaps

Strengthen downward lending with an output-child activation obligation: the new
loan targets the current frame, never its caller. Admit same-root parent and
ancestor loans during adoption in both modes; prove the child usable. Add a
return Law preserving every caller loan and restoring access through the caller
parent after callee children end. The existing owner-return Law remains the
direct-owner case, with its explicit no-surviving-loan premise.

Route nested entry through the uniformly substituted lifecycle callback and its
pending-argument reset. Remove the entry/adoption bypass constructors and restrict
quiet effects to unchanged stores; explicit actual owner-consumption and
reinitialization effects continue to admit changing non-lifecycle steps. Lifetime covers shared and mutable call loans:
both freeze owner writes; mutable loans also freeze owner reads. A two-step real
nested call witnesses entry and body execution. Same-root deferred lending reaches
normal enclosing completion with restored parent access.

Prove one finite-history nonrevival Law across LoanAction, passOwner, passDown,
enterDown, and callLife, sharing lifecycle retirement inversion with the existing
place/loan Law. An eventful positive history exercises every issuing interface.
Add controls for inherited caller activation, same-root/shared pending adoption,
nested-entry loan wiping, shared-loan removal, shared-write permission, and
recycled passDown/enterDown identities; retain the return and execution controls.
The O3 tag inventory is 15 Law, 13 Helper, 32 Instance, 40 Planted.

Correct the O1 header, mutant doc comments and control metadata. Non-empty storage
cleanup removes a moved linear local beside an active loan. Qualify the round-2
nonrevival claim and update the main O3 fragment to describe the strengthened Laws.
Authorities remain OD2/OD15/OD23, recovered D3 C5 Q1–Q2, and the approved Foundation
law boundary; no held owner decision is encoded.
