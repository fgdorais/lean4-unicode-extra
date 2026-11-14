/-
Copyright © 2026 François G. Dorais. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/
module

public import UnicodeBasic
import UnicodeExtra.Grapheme.Table

/-!
# Grapheme Clusters

Extended grapheme cluster boundaries, following Unicode Standard Annex #29, *Unicode Text
Segmentation*. A grapheme cluster is what a user thinks of as a single character: a base letter
with its combining marks, a Hangul syllable, an emoji sequence or a flag.
-/

namespace Unicode

/-- Grapheme cluster break property value

  Unicode property: `Grapheme_Cluster_Break` -/
public inductive GraphemeClusterBreak where
  /-- Any other character (`XX`) -/
  | other
  /-- U+000D CARRIAGE RETURN (`CR`) -/
  | cr
  /-- U+000A LINE FEED (`LF`) -/
  | lf
  /-- Control and format characters, separators and unassigned default ignorables (`CN`) -/
  | control
  /-- Grapheme extenders and emoji modifiers (`EX`) -/
  | extend
  /-- U+200D ZERO WIDTH JOINER (`ZWJ`) -/
  | zwj
  /-- Regional indicator symbols, used in pairs for flags (`RI`) -/
  | regionalIndicator
  /-- Prepended concatenation marks and similar characters (`PP`) -/
  | prepend
  /-- Spacing marks that extend a grapheme cluster (`SM`) -/
  | spacingMark
  /-- Hangul leading consonant jamo (`L`) -/
  | l
  /-- Hangul vowel jamo (`V`) -/
  | v
  /-- Hangul trailing consonant jamo (`T`) -/
  | t
  /-- Hangul LV syllable (`LV`) -/
  | lv
  /-- Hangul LVT syllable (`LVT`) -/
  | lvt
  deriving Inhabited, DecidableEq, Repr

/-- Indic conjunct break property value

  Unicode property: `Indic_Conjunct_Break` -/
public inductive IndicConjunctBreak where
  /-- Not part of an Indic conjunct (`None`) -/
  | none
  /-- Virama that links consonants into a conjunct (`Linker`) -/
  | linker
  /-- Consonant that can take part in a conjunct (`Consonant`) -/
  | consonant
  /-- Combining mark or joiner that can occur inside a conjunct (`Extend`) -/
  | extend
  deriving Inhabited, DecidableEq, Repr

namespace Grapheme

/-- Packed grapheme break data of a character

  Internal use only. Bits 0–3 are the `Grapheme_Cluster_Break` code, bit 4 is
  `Extended_Pictographic`, and bits 5–6 are the `Indic_Conjunct_Break` code. -/
public def lookup (c : Char) : UInt32 :=
  if c.val < 0x80 then
    -- ASCII: only CR, LF and controls are special
    if c.val == 0x0D then 1
    else if c.val == 0x0A then 2
    else if c.val < 0x20 || c.val == 0x7F then 3
    else 0
  else if 0xAC00 ≤ c.val && c.val ≤ 0xD7A3 then
    -- Hangul syllables: LV when there is no trailing consonant, LVT otherwise
    if (c.val - 0xAC00) % 28 == 0 then 12 else 13
  else
    -- last entry whose start is at most `c`
    loop 0 table.size
where
  loop (lo hi : Nat) : UInt32 :=
    if h : lo + 1 < hi then
      let mid := (lo + hi) / 2
      if (table[mid]?.getD 0 >>> 8) ≤ c.val then loop mid hi else loop lo mid
    else
      (table[lo]?.getD 0) &&& 0xFF
  termination_by hi - lo

end Grapheme

/-- Get the grapheme cluster break property of a character

  Unicode property: `Grapheme_Cluster_Break` -/
public def getGraphemeClusterBreak (c : Char) : GraphemeClusterBreak :=
  match Grapheme.lookup c &&& 0xF with
  | 1 => .cr
  | 2 => .lf
  | 3 => .control
  | 4 => .extend
  | 5 => .zwj
  | 6 => .regionalIndicator
  | 7 => .prepend
  | 8 => .spacingMark
  | 9 => .l
  | 10 => .v
  | 11 => .t
  | 12 => .lv
  | 13 => .lvt
  | _ => .other

/-- Check if a character is extended pictographic

  These are the characters used in emoji sequences, together with reserved code points set aside
  for future emoji.

  Unicode property: `Extended_Pictographic` -/
public def isExtendedPictographic (c : Char) : Bool :=
  Grapheme.lookup c &&& 0x10 != 0

/-- Get the Indic conjunct break property of a character

  Unicode property: `Indic_Conjunct_Break` -/
public def getIndicConjunctBreak (c : Char) : IndicConjunctBreak :=
  match (Grapheme.lookup c >>> 5) &&& 0x3 with
  | 1 => .linker
  | 2 => .consonant
  | 3 => .extend
  | _ => .none

namespace Grapheme

/-- State of the grapheme cluster boundary scan

  Internal use only. -/
public structure State where
  /-- Packed data of the previous character -/
  prev : UInt32 := 0
  /-- An odd number of regional indicators ends at the previous character -/
  riOdd : Bool := false
  /-- Progress through `ExtPict Extend* ZWJ`: 0 none, 1 after `ExtPict Extend*`, 2 after `ZWJ` -/
  pict : UInt8 := 0
  /-- The previous characters are a conjunct linker followed by zero or more conjunct extenders -/
  conjunct : Bool := false
  deriving Inhabited

/-- Check for a boundary between the previous character and a character with packed data `cur`

  Internal use only. -/
public def State.isBreak (st : State) (cur : UInt32) : Bool :=
  let p := st.prev &&& 0xF
  let q := cur &&& 0xF
  -- GB3: CR × LF
  if p == 1 && q == 2 then false
  -- GB4, GB5: break after and before controls
  else if p ≤ 3 && p != 0 then true
  else if q ≤ 3 && q != 0 then true
  -- GB6: L × (L | V | LV | LVT)
  else if p == 9 && (q == 9 || q == 10 || q == 12 || q == 13) then false
  -- GB7: (LV | V) × (V | T)
  else if (p == 12 || p == 10) && (q == 10 || q == 11) then false
  -- GB8: (LVT | T) × T
  else if (p == 13 || p == 11) && q == 11 then false
  -- GB9, GB9a: × (Extend | ZWJ | SpacingMark)
  else if q == 4 || q == 5 || q == 8 then false
  -- GB9b: Prepend ×
  else if p == 7 then false
  -- GB9c: ConjunctLinker ConjunctExtender* × LinkingConsonant
  else if st.conjunct && (cur >>> 5) &&& 0x3 == 2 then false
  -- GB11: ExtPict Extend* ZWJ × ExtPict
  else if st.pict == 2 && cur &&& 0x10 != 0 then false
  -- GB12, GB13: pairs of regional indicators
  else if p == 6 && q == 6 && st.riOdd then false
  -- GB999
  else true

/-- Update the scan state with a character with packed data `cur`

  Internal use only. -/
public def State.push (st : State) (cur : UInt32) : State :=
  let q := cur &&& 0xF
  let incb := (cur >>> 5) &&& 0x3
  { prev := cur
    riOdd := q == 6 && !(st.prev &&& 0xF == 6 && st.riOdd)
    pict :=
      if cur &&& 0x10 != 0 then 1
      else if st.pict == 1 && q == 4 then 1
      else if st.pict == 1 && q == 5 then 2
      else 0
    conjunct := incb == 1 || (st.conjunct && incb == 3) }

end Grapheme

/-- Split off the first extended grapheme cluster of a string

  Returns the first grapheme cluster and the rest of the string, or `none` if the string is
  empty. The start of the string must be a grapheme cluster boundary, as it is for any string
  or for the rest returned by a previous call.

  Specification: extended grapheme clusters, Unicode Standard Annex #29, *Unicode Text
  Segmentation*. -/
public def splitGraphemeCluster? (s : String.Slice) : Option (String.Slice × String.Slice) :=
  if h : s.startPos = s.endPos then none else
    let st : Grapheme.State := ({} : Grapheme.State).push (Grapheme.lookup (s.startPos.get h))
    some (loop st (s.startPos.next h))
where
  loop (st : Grapheme.State) (p : s.Pos) : String.Slice × String.Slice :=
    if h : p = s.endPos then (s, "".toSlice) else
      let cur := Grapheme.lookup (p.get h)
      if st.isBreak cur then (s.sliceTo p, s.sliceFrom p) else loop (st.push cur) (p.next h)
  termination_by p.remainingBytes
  decreasing_by grind [String.Slice.Pos.lt_iff_remainingBytes_lt, String.Slice.Pos.lt_next]

/-- Extended grapheme clusters of a string

  Specification: extended grapheme clusters, Unicode Standard Annex #29, *Unicode Text
  Segmentation*. -/
public def graphemeClusters (s : String.Slice) : Array String.Slice := Id.run do
  let mut out := #[]
  let mut st : Grapheme.State := {}
  let mut start := s.startPos
  for ⟨p, h⟩ in s.positions do
    let cur := Grapheme.lookup (p.get h)
    if p ≠ s.startPos && st.isBreak cur then
      out := out.push (s.slice! start p)
      start := p
    st := st.push cur
  if start ≠ s.endPos then
    out := out.push (s.sliceFrom start)
  return out

end Unicode
