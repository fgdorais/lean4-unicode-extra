/-
Copyright © 2026 François G. Dorais. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/
module

public import UnicodeBasic
public import UnicodeExtra.Types
import UnicodeExtra.TableLookup

/-!
# Sentence Boundaries

Default sentence boundaries, following Unicode Standard Annex #29, *Unicode Text Segmentation*.
-/

namespace Unicode

/-- Get the sentence break property of a character

  Unicode property: `Sentence_Break` -/
public def getSentenceBreak (c : Char) : SentenceBreak := lookupSentenceBreak c.val

namespace Sentence

/-- `Extend` or `Format`, which attach to the preceding character -/
private def isIgnorable : SentenceBreak → Bool
  | .extend | .format => true
  | _ => false

/-- `Sep`, `CR` or `LF` -/
private def isParaSep : SentenceBreak → Bool
  | .sep | .cr | .lf => true
  | _ => false

/-- `STerm` or `ATerm` -/
private def isSATerm : SentenceBreak → Bool
  | .sTerm | .aTerm => true
  | _ => false

/-- Index of the last entry before `i` that is not `x`, if any -/
private def skipBack (codes : Array SentenceBreak) (i : Nat) (x : SentenceBreak) : Option Nat :=
  Id.run do
    let mut j := i
    while j > 0 do
      if codes[j - 1]! != x then return some (j - 1)
      j := j - 1
    return none

/-- Check for sentence boundaries before each character after the first

  Returns an array whose entry `i` is true if there is a sentence boundary before character
  `i`. Entry `0` is always true. -/
private def boundaries (data : Array SentenceBreak) : Array Bool := Id.run do
  let n := data.size
  -- SB5: characters that attach to the preceding character are left out of `codes`
  let mut base : Array Nat := Array.mkEmpty n
  let mut codes : Array SentenceBreak := Array.mkEmpty n
  for i in [0:n] do
    let b := data[i]!
    if i > 0 && isIgnorable b && !isParaSep data[i - 1]! then continue
    base := base.push i
    codes := codes.push b
  let m := codes.size
  -- the index of the `SATerm` when the entries before `k` end with `SATerm Close*`, or with
  -- `SATerm Close* Sp*` if `withSp` is true
  let satermBefore (k : Nat) (withSp : Bool) : Option Nat := do
    let j ← if withSp then (skipBack codes k .sp).map (· + 1) else some k
    let t ← skipBack codes j .close
    if isSATerm codes[t]! then some t else none
  let mut out : Array Bool := .replicate n false
  if n > 0 then out := out.set! 0 true
  for k in [1:m] do
    let p := codes[k - 1]!
    let q := codes[k]!
    let brk :=
      -- SB3: CR × LF
      if p == .cr && q == .lf then false
      -- SB4: ParaSep ÷
      else if isParaSep p then true
      -- SB6: ATerm × Numeric
      else if p == .aTerm && q == .numeric then false
      -- SB7: (Upper | Lower) ATerm × Upper
      else if k ≥ 2 && (codes[k - 2]! == .upper || codes[k - 2]! == .lower) && p == .aTerm &&
          q == .upper then false
      else
        let t := satermBefore k true
        -- SB8: ATerm Close* Sp* × [^ OLetter Upper Lower ParaSep SATerm]* Lower
        if t.any (codes[·]! == .aTerm) &&
            (Id.run do
              for j in [k:m] do
                let c := codes[j]!
                if c == .lower then return true
                if c == .oLetter || c == .upper || isParaSep c || isSATerm c then return false
              return false) then false
        -- SB8a: SATerm Close* Sp* × (SContinue | SATerm)
        else if t.isSome && (q == .sContinue || isSATerm q) then false
        -- SB9: SATerm Close* × (Close | Sp | ParaSep)
        else if (satermBefore k false).isSome && (q == .close || q == .sp || isParaSep q) then false
        -- SB10: SATerm Close* Sp* × (Sp | ParaSep)
        else if t.isSome && (q == .sp || isParaSep q) then false
        -- SB11: SATerm Close* Sp* ParaSep? ÷
        else if t.isSome then true
        -- SB998: Any × Any
        else false
    out := out.set! base[k]! brk
  return out

end Sentence

/-- Split a string at its sentence boundaries

  Returns the segments between consecutive sentence boundaries. Each segment keeps the spaces
  and line breaks that follow its sentence, so the segments always join back to the whole
  string.

  Specification: default sentence boundaries, Unicode Standard Annex #29, *Unicode Text
  Segmentation*. -/
public def sentenceSegments (s : String.Slice) : Array String.Slice := Id.run do
  let mut pos := #[]
  let mut data := #[]
  for ⟨p, h⟩ in s.positions do
    pos := pos.push p
    data := data.push (getSentenceBreak (p.get h))
  let brk := Sentence.boundaries data
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
