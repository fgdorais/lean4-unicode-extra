/-
Copyright © 2026 François G. Dorais. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/
module
public import UnicodeBasic
import UnicodeBasic.CharacterDatabase
import UnicodeBasic.Hangul
public import UnicodeExtra.Types

/-! # Table lookups

The functions in this module are not part of the public API and may change at
any time. Use the functions in `UnicodeExtra` instead.
-/

namespace Unicode

/-- Binary search -/
@[specialize]
private partial def find [LT α] [DecidableRel (α := α) (· < ·)] (c : α) (t : USize → α)
    (lo hi : USize) : USize :=
  let mid := (lo + hi) / 2
  if lo = mid then
    lo
  else if c < t mid then
    find c t lo mid
  else
    find c t mid hi

/-- Parse a range compressed data table -/
private def parseDataTable (s : String) (f : UInt32 → UInt32 → Array String.Slice → α) : Thunk <| Array (UInt32 × UInt32 × α) :=
  .mk fun _ => Id.run do
    let mut r := #[]
    let mut stream := UCDStream.ofString s
    for record in stream do
      let start := ofHexString! record[0]!
      let stop := if record[1]!.isEmpty then start else ofHexString! record[1]!
      let val := f start stop record[2:]
      r := r.push (start, stop, val)
    return r

/-- Parse a range compressed property table -/
private def parsePropTable (s : String) : Thunk <| Array (UInt32 × UInt32) :=
  .mk fun _ => Id.run do
    let mut r := #[]
    let mut stream := UCDStream.ofString s
    for record in stream do
      let start := ofHexString! record[0]!
      let stop := if record[1]!.isEmpty then start else ofHexString! record[1]!
      r := r.push (start, stop)
    return r

/-- Check if code point is extended pictographic using lookup table

  Unicode property: `Extended_Pictographic` -/
public def lookupExtendedPictographic (c : UInt32) : Bool :=
  let table := table.get
  if c < table[0]!.1 then false else
    match table[find c (fun i => table[i]!.1) 0 table.size.toUSize]! with
    | (_, v) => c ≤ v
where
  str : String := include_str "../data/Extended_Pictographic.txt"
  table : Thunk <| Array (UInt32 × UInt32) := parsePropTable str

/-- Get grapheme cluster break property using lookup table

  Unicode property: `Grapheme_Cluster_Break` -/
public def lookupGraphemeClusterBreak (c : UInt32) : GraphemeClusterBreak :=
  if Hangul.Syllable.base ≤ c && c ≤ Hangul.Syllable.last then
    -- Hangul syllables are not in the table
    if (c - Hangul.Syllable.base) % Hangul.sizeT.toUInt32 == 0 then .syllableLV else .syllableLVT
  else
    let table := table.get
    if c < table[0]!.1 then .other else
      match table[find c (fun i => table[i]!.1) 0 table.size.toUSize]! with
      | (_, v, gcb) => if c ≤ v then gcb else .other
where
  str : String := include_str "../data/Grapheme_Cluster_Break.txt"
  table : Thunk <| Array (UInt32 × UInt32 × GraphemeClusterBreak) :=
    parseDataTable str fun _ _ x => GraphemeClusterBreak.ofAbbrev! x[0]!

/-- Get Indic conjunct break property using lookup table

  Unicode property: `Indic_Conjunct_Break` -/
public def lookupIndicConjunctBreak (c : UInt32) : IndicConjunctBreak :=
  let table := table.get
  if c < table[0]!.1 then .none else
    match table[find c (fun i => table[i]!.1) 0 table.size.toUSize]! with
    | (_, v, incb) => if c ≤ v then incb else .none
where
  str : String := include_str "../data/Indic_Conjunct_Break.txt"
  table : Thunk <| Array (UInt32 × UInt32 × IndicConjunctBreak) :=
    parseDataTable str fun _ _ x => IndicConjunctBreak.ofAbbrev! x[0]!

/-- Check if code point is soft dotted using lookup table

  Unicode property: `Soft_Dotted` -/
public def lookupSoftDotted (c : UInt32) : Bool :=
  let table := table.get
  if c < table[0]!.1 then false else
    match table[find c (fun i => table[i]!.1) 0 table.size.toUSize]! with
    | (_, v) => c ≤ v
where
  str : String := include_str "../data/Soft_Dotted.txt"
  table : Thunk <| Array (UInt32 × UInt32) := parsePropTable str

/-- Get sentence break property using lookup table

  Unicode property: `Sentence_Break` -/
public def lookupSentenceBreak (c : UInt32) : SentenceBreak :=
  let table := table.get
  if c < table[0]!.1 then .other else
    match table[find c (fun i => table[i]!.1) 0 table.size.toUSize]! with
    | (_, v, sb) =>
      if c ≤ v then
        match sb with
        | some sb => sb
        | none =>
          -- the table does not tell `Upper` from `Lower`
          let ch := Char.ofNat c.toNat
          if isUppercase ch || GeneralCategory.isTitlecaseLetter ch then .upper else .lower
      else .other
where
  str : String := include_str "../data/Sentence_Break.txt"
  table : Thunk <| Array (UInt32 × UInt32 × Option SentenceBreak) :=
    parseDataTable str fun _ _ x => if x[0]! == "lc" then none else some (SentenceBreak.ofAbbrev! x[0]!)

/-- Get word break property using lookup table

  Unicode property: `Word_Break` -/
public def lookupWordBreak (c : UInt32) : WordBreak :=
  let table := table.get
  if c < table[0]!.1 then .other else
    match table[find c (fun i => table[i]!.1) 0 table.size.toUSize]! with
    | (_, v, wb) => if c ≤ v then wb else .other
where
  str : String := include_str "../data/Word_Break.txt"
  table : Thunk <| Array (UInt32 × UInt32 × WordBreak) :=
    parseDataTable str fun _ _ x => WordBreak.ofAbbrev! x[0]!

/-- Get NFC quick check property using lookup table

  Unicode property: `NFC_Quick_Check` -/
public def lookupNFCQuickCheck (c : UInt32) : QuickCheck :=
  let table := table.get
  if c < table[0]!.1 then .yes else
    match table[find c (fun i => table[i]!.1) 0 table.size.toUSize]! with
    | (_, v, qc) => if c ≤ v then qc else .yes
where
  str : String := include_str "../data/NFC_Quick_Check.txt"
  table : Thunk <| Array (UInt32 × UInt32 × QuickCheck) :=
    parseDataTable str fun _ _ x => QuickCheck.ofAbbrev! x[0]!

/-- Get NFKC quick check property using lookup table

  Unicode property: `NFKC_Quick_Check` -/
public def lookupNFKCQuickCheck (c : UInt32) : QuickCheck :=
  let table := table.get
  if c < table[0]!.1 then .yes else
    match table[find c (fun i => table[i]!.1) 0 table.size.toUSize]! with
    | (_, v, qc) => if c ≤ v then qc else .yes
where
  str : String := include_str "../data/NFKC_Quick_Check.txt"
  table : Thunk <| Array (UInt32 × UInt32 × QuickCheck) :=
    parseDataTable str fun _ _ x => QuickCheck.ofAbbrev! x[0]!

/-- Get the primary composite of two code points using lookup table

  Returns `0` if there is none. Hangul syllables are not in the table.

  Unicode properties:
    `Decomposition_Mapping`
    `Full_Composition_Exclusion` -/
public def lookupPrimaryComposite (a b : UInt32) : UInt32 :=
  let table := table.get
  let k := key a b
  if table.size = 0 || k < table[0]!.1 then 0 else
    match table[find k (fun i => table[i]!.1) 0 table.size.toUSize]! with
    | (k', c) => if k == k' then c else 0
where
  key (a b : UInt32) : UInt64 := a.toUInt64 <<< 21 ||| b.toUInt64
  str : String := include_str "../data/Primary_Composite.txt"
  /-- Primary composites, keyed by the two code points of their canonical decompositions -/
  table : Thunk <| Array (UInt64 × UInt32) := .mk fun _ => Id.run do
    let mut r := #[]
    for (c₀, c₁) in (parsePropTable str).get do
      for c in [c₀.toNat:c₁.toNat+1] do
        match getDecompositionMapping? (Char.ofNat c) with
        | some ⟨none, [a, b]⟩ => r := r.push (key a.val b.val, c.toUInt32)
        | _ => continue
    return r.qsort fun x y => x.1 < y.1

end Unicode
