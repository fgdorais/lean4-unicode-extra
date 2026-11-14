/-
Copyright © 2026 François G. Dorais. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/
module

public import UnicodeBasic
public import UnicodeExtra.Types
import UnicodeExtra.TableLookup

/-!
# Grapheme Clusters

Extended grapheme cluster boundaries, following Unicode Standard Annex #29, *Unicode Text
Segmentation*. A grapheme cluster is what a user thinks of as a single character: a base letter
with its combining marks, a Hangul syllable, an emoji sequence or a flag.
-/

namespace Unicode

/-- Get the grapheme cluster break property of a character

  Unicode property: `Grapheme_Cluster_Break` -/
public def getGraphemeClusterBreak (c : Char) : GraphemeClusterBreak :=
  -- ASCII shortcut
  if c.val < 0x80 then
    if c.val == 0x0D then .cr
    else if c.val == 0x0A then .lf
    else if c.val < 0x20 || c.val == 0x7F then .control
    else .other
  else
    lookupGraphemeClusterBreak c.val

/-- Check if a character is extended pictographic

  These are the characters used in emoji sequences, together with reserved code points set aside
  for future emoji.

  Unicode property: `Extended_Pictographic` -/
public def isExtendedPictographic (c : Char) : Bool :=
  -- ASCII shortcut
  if c.val < 0x80 then false else lookupExtendedPictographic c.val

/-- Get the Indic conjunct break property of a character

  Unicode property: `Indic_Conjunct_Break` -/
public def getIndicConjunctBreak (c : Char) : IndicConjunctBreak :=
  -- ASCII shortcut
  if c.val < 0x80 then .none else lookupIndicConjunctBreak c.val

namespace Grapheme

/-- Grapheme break properties of a character

  Internal use only. -/
public structure Props where
  /-- Grapheme cluster break property -/
  gcb : GraphemeClusterBreak
  /-- Extended pictographic property -/
  pict : Bool
  /-- Indic conjunct break property -/
  incb : IndicConjunctBreak
  deriving Inhabited

/-- Grapheme break properties of a character

  Internal use only. -/
public def Props.of (c : Char) : Props :=
  let gcb := getGraphemeClusterBreak c
  -- `util/MakeTables` checks that extended pictographic characters all have the grapheme
  -- cluster break property `other`, and Indic conjunct characters `other`, `extend` or `zwj`
  match gcb with
  | .other => { gcb, pict := isExtendedPictographic c, incb := getIndicConjunctBreak c }
  | .extend | .zwj => { gcb, pict := false, incb := getIndicConjunctBreak c }
  | _ => { gcb, pict := false, incb := .none }

/-- State of the grapheme cluster boundary scan

  Internal use only. -/
public structure State where
  /-- Grapheme cluster break property of the previous character -/
  prev : GraphemeClusterBreak := .other
  /-- An odd number of regional indicators ends at the previous character -/
  riOdd : Bool := false
  /-- Progress through `ExtPict Extend* ZWJ`: 0 none, 1 after `ExtPict Extend*`, 2 after `ZWJ` -/
  pict : UInt8 := 0
  /-- The previous characters are a conjunct linker followed by zero or more conjunct extenders -/
  conjunct : Bool := false
  deriving Inhabited

/-- Check for a boundary between the previous character and a character with properties `cur`

  Internal use only. -/
public def State.isBreak (st : State) (cur : Props) : Bool :=
  match st.prev, cur.gcb with
  -- GB3: CR × LF
  | .cr, .lf => false
  -- GB4, GB5: break after and before controls
  | .cr, _ | .lf, _ | .control, _ => true
  | _, .cr | _, .lf | _, .control => true
  -- GB6: L × (L | V | LV | LVT)
  | .jamoL, .jamoL | .jamoL, .jamoV | .jamoL, .syllableLV | .jamoL, .syllableLVT => false
  -- GB7: (LV | V) × (V | T)
  | .syllableLV, .jamoV | .syllableLV, .jamoT | .jamoV, .jamoV | .jamoV, .jamoT => false
  -- GB8: (LVT | T) × T
  | .syllableLVT, .jamoT | .jamoT, .jamoT => false
  -- GB9, GB9a: × (Extend | ZWJ | SpacingMark)
  | _, .extend | _, .zwj | _, .spacingMark => false
  -- GB9b: Prepend ×
  | .prepend, _ => false
  | p, q =>
    -- GB9c: ConjunctLinker ConjunctExtender* × LinkingConsonant
    if st.conjunct && cur.incb == .consonant then false
    -- GB11: ExtPict Extend* ZWJ × ExtPict
    else if st.pict == 2 && cur.pict then false
    -- GB12, GB13: pairs of regional indicators
    else if p == .regionalIndicator && q == .regionalIndicator && st.riOdd then false
    -- GB999
    else true

/-- Update the scan state with a character with properties `cur`

  Internal use only. -/
public def State.push (st : State) (cur : Props) : State :=
  { prev := cur.gcb
    riOdd := cur.gcb == .regionalIndicator && !(st.prev == .regionalIndicator && st.riOdd)
    pict :=
      if cur.pict then 1
      else if st.pict == 1 && cur.gcb == .extend then 1
      else if st.pict == 1 && cur.gcb == .zwj then 2
      else 0
    conjunct := cur.incb == .linker || (st.conjunct && cur.incb == .extend) }

end Grapheme

/-- Split off the first extended grapheme cluster of a string

  Returns the first grapheme cluster and the rest of the string, or `none` if the string is
  empty. The start of the string must be a grapheme cluster boundary, as it is for any string
  or for the rest returned by a previous call.

  Specification: extended grapheme clusters, Unicode Standard Annex #29, *Unicode Text
  Segmentation*. -/
public def splitGraphemeCluster? (s : String.Slice) : Option (String.Slice × String.Slice) :=
  if h : s.startPos = s.endPos then none else
    let st : Grapheme.State := ({} : Grapheme.State).push (Grapheme.Props.of (s.startPos.get h))
    some (loop st (s.startPos.next h))
where
  loop (st : Grapheme.State) (p : s.Pos) : String.Slice × String.Slice :=
    if h : p = s.endPos then (s, "".toSlice) else
      let cur := Grapheme.Props.of (p.get h)
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
    let cur := Grapheme.Props.of (p.get h)
    if p ≠ s.startPos && st.isBreak cur then
      out := out.push (s.slice! start p)
      start := p
    st := st.push cur
  if start ≠ s.endPos then
    out := out.push (s.sliceFrom start)
  return out

end Unicode
