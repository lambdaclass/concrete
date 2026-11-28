import Concrete.Interfaces.Contracts
import ConcreteSpec.Proofs.Ident

namespace Concrete.Lex.Bridge
open ConcreteSpec.StageInterface Concrete.Interfaces
open ConcreteSpec.Core.SourcePathSpan

/-- The stage reports exactly the scanner's typed failure at the issuing file. -/
theorem lexStream_refuses (source : Source) (failure : LexRefusal) :
    lexStream source = .error failure ↔ LexRefuses source failure := by
  unfold lexStream
  split
  · rename_i he
    simp [LexRefuses, he, eq_comm]
  · rename_i file he
    simp only [LexRefuses, he]
    split
    · rename_i stream scan
      cases failure <;> simp [scan]
    · rename_i err scan
      have allowed := lexLossless_failure scan
      cases err <;> try contradiction
      rename_i cause
      cases cause <;> try contradiction
      cases failure <;> simp [scan, eq_comm]

private abbrev raw : RetainedPiece → RawPiece
  | .inl trivia => trivia
  | .inr token => ⟨token.decoded.span,token.spelling⟩

private def entryPieces (e : List RawPiece × RetainedToken) : List RetainedPiece :=
  e.1.map Sum.inl ++ [.inr e.2]

private def flatten (runs : Runs) : List RetainedPiece :=
  runs.entries.flatMap entryPieces ++ runs.trailing.map Sum.inl

private theorem flatten_prepend (p : RetainedPiece) (runs : Runs) :
    flatten (runs.prepend p) = p :: flatten runs := by
  cases runs with
  | mk es tr =>
    cases p with
    | inl p => cases es <;> rfl
    | inr t => rfl

private theorem flatten_group (xs : List RetainedPiece) : flatten (group xs) = xs := by
  induction xs with
  | nil => rfl
  | cons x xs ih => rw [show group (x::xs) = (group xs).prepend x by rfl,flatten_prepend,ih]

private theorem retainPiece_raw (stream : LosslessStream) (p : {x // x ∈ stream.pieces}) :
    raw (retainPiece stream p) =
      ⟨fileSpan p.val.span,stream.source.extract p.val.span.start p.val.span.stop⟩ := by
  rcases p with ⟨p,h⟩
  cases p <;> rfl

private theorem fold_congr {α β : Type} (xs : List α) (f g : β → α → Option β) (init : β)
    (h : ∀ x ∈ xs, ∀ b, f b x = g b x) : xs.foldlM f init = xs.foldlM g init := by
  induction xs generalizing init with
  | nil => rfl
  | cons x xs ih =>
    simp only [List.foldlM_cons,h x List.mem_cons_self init]
    cases hit : g init x with
    | none => rfl
    | some b => exact ih b (fun y hy => h y (List.mem_cons_of_mem _ hy))

private theorem indexed_fold {α β : Type} (xs : List α) (f : β → Nat → Option β)
    (g : β → α → Option β) (init : β)
    (h : ∀ i x, xs[i]? = some x → ∀ b, f b i = g b x) :
    (List.range xs.length).foldlM f init = xs.foldlM g init := by
  rw [List.range_eq_range', ← List.zipIdx_map_snd 0 xs, List.foldlM_map]
  conv => rhs; rw [← List.zipIdx_map_fst 0 xs, List.foldlM_map]
  exact fold_congr _ _ _ _ fun pair hp => h pair.2 pair.1 (List.mem_zipIdx_iff_getElem?.mp hp)

private theorem flatten_fold {β : Type} (runs : Runs) (step : β → RawPiece → Option β) (b : β) :
    (flatten runs).foldlM (fun s p => step s (raw p)) b =
    (do
      let next ← runs.entries.foldlM (fun s e => do
        let s ← e.1.foldlM step s
        step s (raw (.inr e.2))) b
      runs.trailing.foldlM step next) := by
  cases runs with
  | mk es tr =>
    induction es generalizing b with
    | nil => simp [flatten, List.foldlM_map]
    | cons e es ih =>
      simp only [flatten, List.flatMap_cons, List.append_assoc] at ih ⊢
      simp only [entryPieces, List.append_assoc, List.foldlM_append, List.foldlM_map,
        List.singleton_append, List.foldlM_cons, raw, bind_assoc]
      exact bind_congr fun x => bind_congr fun y => by simpa only [List.foldlM_append, List.foldlM_map, raw] using ih y

private abbrev appendPiece (entry : FileId) :
    ByteArray × ConcreteSpec.Core.Position → RawPiece → Option (ByteArray × ConcreteSpec.Core.Position) :=
    fun ((bytes,pos) : ByteArray × ConcreteSpec.Core.Position) (p : RawPiece) =>
      if p.span.file == entry && p.span.startOffset == pos.byte &&
          p.span.endOffset == pos.byte + p.value.size && p.value.size > 0 &&
          p.span.line == pos.line && p.span.col == pos.column then
        some (bytes ++ p.value,p.value.data.foldl (fun (pos : ConcreteSpec.Core.Position) (b : UInt8) => pos.advanceByte b.toNat) pos)
      else none
/-- The frozen range-indexed reconstruction folds the same flattened pieces as
the scanner, including the last run on an empty or trivia-only input. -/
private theorem reconstruct_fold (runs : Runs) (source : Source) :
    let step := appendPiece source.entryFile
    reconstruct source.entryFile (runs.stream source).tokens (runs.stream source).trivia =
      ((flatten runs).foldlM (fun s p => step s (raw p)) (ByteArray.empty,{})).map Prod.fst := by
  intro step
  rw [flatten_fold]
  unfold reconstruct Runs.stream
  simp only [List.size_toArray, List.length_map, List.length_append,
    List.length_singleton, bne_self_eq_false, Bool.false_eq_true, ↓reduceIte]
  generalize he : runs.entries = es
  let tokens := (es.map Prod.snd).toArray
  let trivia := ((es.map (List.toArray ∘ Prod.fst)) ++ [runs.trailing.toArray]).toArray
  let atIndex := fun (s : ByteArray × ConcreteSpec.Core.Position) (i : Nat) => do
    let before ← trivia[i]?
    let s ← before.foldlM step s
    let t ← tokens[i]?
    step s (raw (.inr t))
  have mainFold := indexed_fold es atIndex (fun s e => do
    let s ← e.1.foldlM step s
    step s (raw (.inr e.2))) (ByteArray.empty,{}) (by
      intro i e hi s
      have lt : i < es.length := by
        obtain ⟨lt,_⟩ := List.getElem?_eq_some_iff.mp hi; exact lt
      have bound : i < (es.map (List.toArray ∘ Prod.fst)).length := by simpa using lt
      simp [atIndex,tokens,trivia, List.getElem?_append_left bound, hi,
        ← Array.foldlM_toList])
  change (do
    let s ← (List.range es.length).foldlM atIndex (ByteArray.empty,{})
    let final ← trivia[es.length]?
    return (← final.foldlM step s).1) = _
  rw [mainFold]
  simp [trivia, ← Array.foldlM_toList]
  apply Option.bind_congr
  intro s _
  dsimp only [Function.comp_apply]
  cases runs.trailing.foldlM step s <;> rfl



open ConcreteSpec.Core


private def slicePiece (bytes : ByteArray) (p : Piece) : RawPiece :=
  ⟨fileSpan p.span, bytes.extract p.span.start p.span.stop⟩

private def SpanOk (file : ConcreteSpec.Core.FileId) (bytes : List UInt8) (s : FileSpan) : Prop :=
  let pos := ((bytes.take s.startOffset).map UInt8.toNat).foldl Position.advanceByte {}
  s.file = file ∧ s.startOffset ≤ s.endOffset ∧ s.endOffset ≤ bytes.length ∧
    (s.line,s.col) = (pos.line,pos.column)

private theorem stream_partition {id pos remaining scalars pieces}
    (h : PieceStream id pos remaining scalars pieces) (src acc : ByteArray)
    (whole : src.data.toList = acc.data.toList ++ remaining)
    (offset : pos.byte = acc.size)
    (cursor : pos = (acc.data.toList.map UInt8.toNat).foldl Position.advanceByte {}) :
    ((pieces.map (slicePiece src)).foldlM (appendPiece id) (acc,pos)).map Prod.fst = some src ∧
      ∀ p ∈ pieces, SpanOk id src.data.toList (fileSpan p.span) := by
  induction h generalizing acc with
  | nil =>
    have eqn : src = acc := ByteArray.ext (Array.toList_inj.mp (by simpa using whole))
    simp [eqn]
  | @cons pos chunk remaining spelling suffix piece rest hd meaning _ ih =>
    let bs := src.extract pos.byte (pos.byte + chunk.length)
    have full : src.data.toList = acc.data.toList ++ (chunk ++ remaining) := whole
    have accLen : acc.data.toList.length = acc.size := by rfl
    have off : pos.byte = acc.data.toList.length := offset.trans accLen.symm
    have contents : bs.data.toList = chunk := by
      simp only [bs,ByteArray.data_extract,Array.toList_extract,List.extract_eq_take_drop,full,off,
        Nat.add_sub_cancel_left,List.drop_left,List.take_left]
    have size : bs.size = chunk.length := by
      simpa using congrArg List.length contents
    have nonempty : 0 < bs.size := by
      rw [size]
      apply List.length_pos_iff.mpr
      have ne := meaning.spelling_nonempty
      cases hd with
      | nil => exact False.elim (ne rfl)
      | cons enc _ => intro empty; exact ConcreteSpec.Surface0.encodes_nonempty enc (List.append_eq_nil_iff.mp empty).1
    have span := meaning.span_eq
    have width := decoded_width hd pos
    have sliced : slicePiece src piece =
        ⟨⟨id,pos.byte,pos.byte + chunk.length,pos.line,pos.column⟩,bs⟩ := by
      simp [slicePiece,fileSpan,span,ConcreteSpec.Surface0.Position.span,width,bs]
    have move : bs.data.foldl (fun p (b : UInt8) => Position.advanceByte p b.toNat) pos =
          spelling.foldl ConcreteSpec.Surface0.Position.advance pos := by
      rw [← Array.foldl_toList,contents,ConcreteSpec.Surface0.advance_decoding hd,List.foldl_map]
    have donePiece : appendPiece id (acc,pos) (slicePiece src piece) =
        some (acc ++ bs,spelling.foldl ConcreteSpec.Surface0.Position.advance pos) := by
      simp [sliced,appendPiece,← size,nonempty]
      exact move
    have accBytes : (acc ++ bs).data.toList = acc.data.toList ++ chunk := by
      simp [ByteArray.data_append,contents]
    obtain ⟨replay,valid⟩ := ih (acc ++ bs) (by rw [full,accBytes,List.append_assoc])
      (by simp [width,offset,size]) (by
        rw [accBytes,List.map_append,List.foldl_append,← cursor,ConcreteSpec.Surface0.advance_decoding hd])
    constructor
    · rw [List.map_cons,List.foldlM_cons,donePiece]; exact replay
    · intro p member
      rcases List.mem_cons.mp member with rfl | later
      · simp only [SpanOk,fileSpan,span,ConcreteSpec.Surface0.Position.span,width,full,off,List.take_left,← cursor,List.length_append]
        exact ⟨trivial, by omega, by omega, trivial⟩
      · exact valid p later

private theorem retained_reconstruct (ref : LosslessStream) (source : Source)
    (id : ref.fileId = source.entryFile) :
    reconstruct source.entryFile (retain source ref).tokens (retain source ref).trivia = some ref.source := by
  unfold retain
  rw [reconstruct_fold,flatten_group]
  have replay := (stream_partition ref.meaning.choose_spec ref.source ByteArray.empty (by rfl)
    (by rfl) (by rfl)).1
  rw [← id, ← replay, ← List.foldlM_map]
  congr 2
  simp only [List.map_map]
  calc
    _ = ref.pieces.attach.map (fun p => slicePiece ref.source p.val) :=
      List.map_congr_left fun p _ => retainPiece_raw ref p
    _ = _ := List.attach_map_val


private abbrev view : RetainedPiece → FileSpan × Option ConcreteSpec.StageInterface.TokenValue
  | .inl r => (r.span,none)
  | .inr t => (t.decoded.span,some t.decoded.value)

private theorem grouped_views (r : Runs) (source : Source) :
    let stream := r.stream source
    (stream.tokens.toList.zipIdx.flatMap fun (t,i) =>
      (stream.trivia[i]?.getD #[]).toList.map (fun p => (p.span,none)) ++
        [(t.decoded.span,some t.decoded.value)]) ++
      (stream.trivia[stream.tokens.size]?.getD #[]).toList.map (fun p => (p.span,none)) =
        (flatten r).map view := by
  simp only [Runs.stream,List.toList_toArray,List.size_toArray,List.length_map,
    List.getElem?_toArray,List.getElem?_append_right (by simp : (r.entries.map (List.toArray ∘ Prod.fst)).length ≤ r.entries.length),
    Nat.sub_self,List.length_map,List.getElem?_cons_zero,Option.getD_some,List.zipIdx_map,
    List.flatMap_map,flatten,List.map_append,List.map_map]
  congr 1
  rw [List.map_flatMap]
  conv => rhs; rw [← List.zipIdx_map_fst 0 r.entries,List.flatMap_map]
  apply congrArg List.flatten
  apply List.map_congr_left
  intro pair member
  have indexed := List.mem_zipIdx_iff_getElem?.mp member
  have bound : pair.2 < (r.entries.map (List.toArray ∘ Prod.fst)).length := by
    simpa using (List.getElem?_eq_some_iff.mp indexed).1
  simp [entryPieces, List.getElem?_append_left bound,indexed,view,List.map_map]

private theorem mapM_members {α β} (xs : List α) (f : α → Option β)
    (g : {x // x ∈ xs} → β) (h : ∀ p, f p.val = some (g p)) :
    xs.mapM f = some (xs.attach.map g) := by
  induction xs with
  | nil => rfl
  | cons x xs ih =>
    simp only [List.mapM_cons,h ⟨x,List.mem_cons_self⟩,List.attach_cons,List.map_cons,List.map_map]
    rw [ih (fun p => g ⟨p.val,List.mem_cons_of_mem _ p.property⟩)
      (fun p => h ⟨p.val,List.mem_cons_of_mem _ p.property⟩)]
    rfl

private theorem token_mem (runs : Runs) (t : RetainedToken) (h : t ∈ runs.entries.map Prod.snd) :
    Sum.inr t ∈ flatten runs := by
  obtain ⟨e,he,rfl⟩ := List.mem_map.mp h
  exact List.mem_append_left _ (List.mem_flatMap.mpr ⟨e,he, by simp [entryPieces]⟩)

private theorem retained_wf {source : Source} {file : File} {ref : LosslessStream}
    (fileAt : source.project.files[source.entryFile.index]? = some file)
    (scanned : lexLossless ⟨file.source.toBytes.toArray⟩ source.entryFile = .ok ref) :
    (retain source ref).parserView.WF := by
  obtain ⟨bytes,id⟩ := lexLossless_origin scanned
  have spans := (stream_partition ref.meaning.choose_spec ref.source ByteArray.empty (by rfl)
    (by rfl) (by rfl)).2
  let r := group (ref.pieces.attach.map (retainPiece ref))
  have valid (t : RetainedToken) (ht : t ∈ r.entries.map Prod.snd) :
      SpanOk source.entryFile file.source.toBytes t.decoded.span := by
    have member := token_mem r t ht
    unfold r at member
    rw [flatten_group] at member
    obtain ⟨p,_,hp⟩ := List.mem_map.mp member
    have eqn := congrArg Located.span (retainPiece_raw ref p)
    rw [hp] at eqn
    have ok := spans p.val p.property
    rw [bytes,id] at ok
    change t.decoded.span = fileSpan p.val.span at eqn
    rw [eqn]
    simpa only [List.toList_toArray] using ok
  unfold ParserView.WF ParserView.invalidSpan ParserView.validateCoordinates
  simp only [retain, TokenStream.parserView, Runs.stream, List.getElem?_toArray, List.getElem?_map,
    fileAt, Option.map_some, Function.comp_apply, Bind.bind, Except.bind, Pure.pure, Except.pure,
    throw, throwThe, MonadExceptOf.throw]
  let positions := ((file.source.bytes.scanl Position.advanceByte {}).map fun p => (p.line,p.column)).toArray
  have hback : positions.back?.isSome = true := by
    simp [Array.back?, positions]
  split
  · rename_i span heq
    split at heq
    · split at heq
      · rename_i token hsome
        rw [Array.find?_eq_none.mpr (by
          intro tok member
          simp only [List.mem_toArray, List.mem_map] at member
          obtain ⟨t, ⟨e, he, rfl⟩, rfl⟩ := member
          have ⟨fileId, ordered, within, coordinates⟩ := valid e.2 (List.mem_map.mpr ⟨e, he, rfl⟩)
          have castBytes := congrArg SourceText.bytes (SourceText.ofBytes_toBytes file.source)
          change file.source.toBytes.map UInt8.toNat = file.source.bytes at castBytes
          simp only [fileId, fileAt, Option.map_some, Function.comp_apply]
          change ¬ (!(decide (e.2.decoded.span.startOffset ≤ e.2.decoded.span.endOffset) &&
              decide (e.2.decoded.span.endOffset < positions.size) &&
              positions[e.2.decoded.span.startOffset]? == some (e.2.decoded.span.line,e.2.decoded.span.col))) = true
          simp only [positions, List.size_toArray, List.length_map, List.length_scanl, List.getElem?_toArray,
            List.getElem?_map, List.getElem?_scanl, ← castBytes, List.length_map]
          have bound := Nat.le_trans ordered within
          simp [ordered, coordinates]
          simpa only [SourceText.length, SourceText.toBytes, List.length_map] using
            And.intro (Nat.lt_succ_of_le within) bound)] at hsome
        contradiction
      · contradiction
    · rename_i hnone
      have hnone' : positions.back? = none := by
        cases hb : positions.back? with
        | none => rfl
        | some pair => exact (hnone pair.1 pair.2 hb).elim
      simp [hnone'] at hback
  · rfl

/-- A successful project scan satisfies the registered contract, including exact
bytes, reference payloads, trivia order and every parser-view coordinate. -/
theorem lexStream_lexes {source : Source} {stream : TokenStream}
    (hs : lexStream source = .ok stream) : Lexes source stream := by
  unfold lexStream at hs
  split at hs
  · contradiction
  · rename_i file he
    have fileAt : source.project.files[source.entryFile.index]? = some file := by
      simpa [Project.fileAt,Project.files] using he
    split at hs
    · rename_i ref scanned
      cases Except.ok.inj hs
      obtain ⟨bytes,id⟩ := lexLossless_origin scanned
      refine ⟨rfl,retained_wf fileAt scanned,file,fileAt,ref.source,retained_reconstruct ref source id,
        by simp [bytes],ref,by simpa [bytes] using scanned,?_⟩
      let r := group (ref.pieces.attach.map (retainPiece ref))
      change ref.pieces.mapM _ = some (_ ++ _)
      calc
        _ = some ((flatten r).map view) := ?_
        _ = _ := congrArg some (grouped_views r source).symm
      unfold r
      rw [flatten_group,List.map_map]
      apply mapM_members
      rintro ⟨p,member⟩
      cases p with
      | whitespace | lineComment => rfl
      | token v s =>
        cases v with
        | parsed val =>
          cases val with
          | fixed k _ =>
            have plain := fixed_in_stream ref.meaning.choose_spec member
            change (if h : Terminal.HasPayload k = false then some (TokenValue.fixed k h) else none).map _ = _
            rw [dite_eq_left plain]; rfl
          | name | integer => rfl
        | stringLiteral | byteLiteral | label => rfl
    · rename_i err scanned
      have allowed := lexLossless_failure scanned
      cases err <;> try contradiction
      rename_i cause
      cases cause <;> contradiction

end Concrete.Lex.Bridge
