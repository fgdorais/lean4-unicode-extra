/-
Copyright © 2026 François G. Dorais. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/
module

public import UnicodeBasic
public import UnicodeExtra.Types
import UnicodeExtra.Grapheme
import UnicodeExtra.TableLookup

/-!
# Word Boundaries

Default word boundaries, following Unicode Standard Annex #29, *Unicode Text Segmentation*. The
segments between word boundaries are words, numbers, runs of white space, and single
punctuation marks or symbols.
-/

namespace Unicode

/-- Get the word break property of a character

  Unicode property: `Word_Break` -/
public def getWordBreak (c : Char) : WordBreak := lookupWordBreak c.val

namespace Word

/-- Word break properties of a character -/
private structure Props where
  /-- Word break property -/
  wb : WordBreak
  /-- Extended pictographic property -/
  pict : Bool
  deriving Inhabited

/-- `Extend`, `Format` or `ZWJ`, which attach to the preceding character -/
private def isIgnorable : WordBreak → Bool
  | .extend | .format | .zwj => true
  | _ => false

/-- `CR`, `LF` or `Newline` -/
private def isNewline : WordBreak → Bool
  | .cr | .lf | .newline => true
  | _ => false

/-- `ALetter` or `Hebrew_Letter` -/
private def isAHLetter : WordBreak → Bool
  | .aLetter | .hebrewLetter => true
  | _ => false

/-- `MidNumLet` or `Single_Quote` -/
private def isMidNumLetQ : WordBreak → Bool
  | .midNumLet | .singleQuote => true
  | _ => false

/-- Word break properties of the characters of a string and their positions -/
private def scan (s : String.Slice) : Array s.Pos × Array Props := Id.run do
  let mut pos := #[]
  let mut data : Array Props := #[]
  for ⟨p, h⟩ in s.positions do
    let c := p.get h
    let wb := getWordBreak c
    pos := pos.push p
    -- only WB3c uses `pict`, right after a ZWJ
    let afterZWJ := data.back?.any (·.wb == .zwj)
    data := data.push { wb, pict := afterZWJ && isExtendedPictographic c }
  return (pos, data)

/-- Check for word boundaries before each character after the first

  Returns an array whose entry `i` is true if there is a word boundary before character `i`.
  Entry `0` is always true. -/
private def boundaries (data : Array Props) : Array Bool := Id.run do
  let n := data.size
  let wb (i : Nat) : WordBreak := (data[i]?.map (·.wb)).getD .other
  -- `eff[i]`: the character that character `i` attaches to under WB4
  let mut eff : Array Nat := Array.mkEmpty n
  for i in [0:n] do
    if i > 0 && isIgnorable (wb i) && !isNewline (wb (i - 1)) then
      eff := eff.push (eff[i - 1]?.getD i)
    else
      eff := eff.push i
  -- `next[i]`: the first character at or after `i` that is not ignorable, or `n`
  let mut next : Array Nat := .replicate (n + 1) n
  for j in [0:n] do
    let i := n - 1 - j
    next := next.set! i (if isIgnorable (wb i) then next[i + 1]! else i)
  -- `ri[i]`: number of consecutive regional indicators ending at the character `eff[i]`
  let mut ri : Array Nat := Array.mkEmpty n
  for i in [0:n] do
    let e := eff[i]!
    if e < i then
      ri := ri.push ri[e]!
    else if wb i == .regionalIndicator then
      ri := ri.push (if i > 0 && wb eff[i - 1]! == .regionalIndicator then ri[eff[i - 1]!]! + 1 else 1)
    else
      ri := ri.push 0
  let mut out := Array.mkEmpty n
  for i in [0:n] do
    if i == 0 then out := out.push true; continue
    let p := wb (i - 1)
    let q := wb i
    let brk :=
      -- WB3: CR × LF
      if p == .cr && q == .lf then false
      -- WB3a, WB3b: break after and before newlines
      else if isNewline p || isNewline q then true
      -- WB3c: ZWJ × ExtPict
      else if p == .zwj && (data[i]?.map (·.pict)).getD false then false
      -- WB3d: WSegSpace × WSegSpace
      else if p == .wSegSpace && q == .wSegSpace then false
      -- WB4: X (Extend | Format | ZWJ)* → X
      else if isIgnorable q then false
      else
        -- the characters around the boundary, ignoring characters that attach to others
        let a := eff[i - 1]!
        let pa := wb a
        let pb := if a > 0 then wb eff[a - 1]! else .other
        let qn := wb next[i + 1]!
        -- WB5: AHLetter × AHLetter
        if isAHLetter pa && isAHLetter q then false
        -- WB6: AHLetter × (MidLetter | MidNumLetQ) AHLetter
        else if isAHLetter pa && (q == .midLetter || isMidNumLetQ q) && isAHLetter qn then false
        -- WB7: AHLetter (MidLetter | MidNumLetQ) × AHLetter
        else if isAHLetter pb && (pa == .midLetter || isMidNumLetQ pa) && isAHLetter q then false
        -- WB7a: Hebrew_Letter × Single_Quote
        else if pa == .hebrewLetter && q == .singleQuote then false
        -- WB7b: Hebrew_Letter × Double_Quote Hebrew_Letter
        else if pa == .hebrewLetter && q == .doubleQuote && qn == .hebrewLetter then false
        -- WB7c: Hebrew_Letter Double_Quote × Hebrew_Letter
        else if pb == .hebrewLetter && pa == .doubleQuote && q == .hebrewLetter then false
        -- WB8, WB9, WB10: (AHLetter | Numeric) × (AHLetter | Numeric)
        else if (pa == .numeric || isAHLetter pa) && (q == .numeric || isAHLetter q) then false
        -- WB11: Numeric (MidNum | MidNumLetQ) × Numeric
        else if pb == .numeric && (pa == .midNum || isMidNumLetQ pa) && q == .numeric then false
        -- WB12: Numeric × (MidNum | MidNumLetQ) Numeric
        else if pa == .numeric && (q == .midNum || isMidNumLetQ q) && qn == .numeric then false
        -- WB13: Katakana × Katakana
        else if pa == .katakana && q == .katakana then false
        -- WB13a: (AHLetter | Numeric | Katakana | ExtendNumLet) × ExtendNumLet
        else if (isAHLetter pa || pa == .numeric || pa == .katakana || pa == .extendNumLet) &&
            q == .extendNumLet then false
        -- WB13b: ExtendNumLet × (AHLetter | Numeric | Katakana)
        else if pa == .extendNumLet && (isAHLetter q || q == .numeric || q == .katakana) then false
        -- WB15, WB16: pairs of regional indicators
        else if pa == .regionalIndicator && q == .regionalIndicator && ri[a]! % 2 == 1 then false
        -- WB999
        else true
    out := out.push brk
  return out

end Word

/-- Split a string at its word boundaries

  Returns the segments between consecutive word boundaries. These are words and numbers, but
  also runs of spaces and single punctuation marks, so the segments always join back to the
  whole string.

  Specification: default word boundaries, Unicode Standard Annex #29, *Unicode Text
  Segmentation*. -/
public def wordSegments (s : String.Slice) : Array String.Slice := Id.run do
  let (pos, data) := Word.scan s
  let brk := Word.boundaries data
  let mut out := #[]
  let mut start := s.startPos
  for i in [1:pos.size] do
    if brk[i]! then
      let p := pos[i]!
      out := out.push (s.slice! start p)
      start := p
  if start ≠ s.endPos then
    out := out.push (s.sliceFrom start)
  return out

end Unicode
