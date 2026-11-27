/-
Copyright © 2026 François G. Dorais. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/
module

public import UnicodeBasic
public import UnicodeExtra.Types
import UnicodeExtra.TableLookup

/-!
# Normalization

The Unicode normalization forms NFD, NFC, NFKD and NFKC, following Unicode Standard Annex #15,
*Unicode Normalization Forms*, and the "Normalization Forms" section of the Unicode Standard's core
specification. Two strings are canonically equivalent when they have the same NFD (or NFC), and
compatibility equivalent when they have the same NFKD (or NFKC).
-/

namespace Unicode

/-- Check whether the form uses compatibility decompositions -/
private def NormalizationForm.isCompat : NormalizationForm → Bool
  | .nfkd | .nfkc => true
  | _ => false

/-- Check whether the form composes after decomposing -/
private def NormalizationForm.isComposed : NormalizationForm → Bool
  | .nfc | .nfkc => true
  | _ => false

namespace Normalization

/-- Quick check property of a character for a normalization form -/
private def quickCheckChar : NormalizationForm → Char → QuickCheck
  | .nfd, c =>
    match getDecompositionMapping? c with
    | some ⟨none, _⟩ => .no
    | _ => .yes
  | .nfkd, c => if (getDecompositionMapping? c).isSome then .no else .yes
  | .nfc, c => lookupNFCQuickCheck c.val
  | .nfkc, c => lookupNFKCQuickCheck c.val

/-- Quick check of a string, with a position before which the string is known to be normalized

  The position is the end of the string when the result is `.yes`. Otherwise, it is the last
  starter with quick check `Y` before the first character that does not have quick check `Y`, or
  the start of the string if there is no such starter. Normalizing the string from that position
  on leaves the part before it unchanged. -/
private def scan (form : NormalizationForm) (s : String.Slice) : QuickCheck × s.Pos :=
  loop s.startPos 0 .yes s.startPos
where
  /-- `last` is the combining class of the previous character, `res` the result so far, and
    `safe` the last starter with quick check `Y` before the first character without -/
  loop (p : s.Pos) (last : Nat) (res : QuickCheck) (safe : s.Pos) : QuickCheck × s.Pos :=
    if h : p = s.endPos then (res, if res == .yes then s.endPos else safe) else
      let c := p.get h
      let cc := getCanonicalCombiningClass c
      if cc != 0 && last > cc then (.no, safe) else
        match quickCheckChar form c with
        | .no => (.no, safe)
        | .maybe => loop (p.next h) cc .maybe safe
        | .yes => loop (p.next h) cc res (if cc == 0 && res == .yes then p else safe)
  termination_by p.remainingBytes
  decreasing_by all_goals grind [String.Slice.Pos.lt_iff_remainingBytes_lt, String.Slice.Pos.lt_next]

/-- Append a character and its canonical combining class to `buf`, moving it before any
  preceding nonstarters with a higher combining class (canonical ordering) -/
private def pushOrdered (buf : Array (Char × Nat)) (c : Char) : Array (Char × Nat) :=
  let cc := getCanonicalCombiningClass c
  let n := buf.size
  if cc == 0 then buf.push (c, 0) else loop (buf.push (c, cc)) n
where
  /-- Move the nonstarter at `i` before any preceding nonstarters of higher class -/
  loop (buf : Array (Char × Nat)) : Nat → Array (Char × Nat)
    | 0 => buf
    | i + 1 =>
      if buf[i]!.2 > buf[i+1]!.2 then loop (buf.swapIfInBounds i (i+1)) i else buf

/-- Append the full canonical (or compatibility) decomposition of a character to `buf`, in
  canonical order -/
private def pushDecomposition (compat : Bool) (buf : Array (Char × Nat)) (c : Char) :
    Array (Char × Nat) :=
  -- Decomposition mappings nest at most 4 deep, as `util/MakeTables` checks.
  loop 4 buf c
where
  loop : Nat → Array (Char × Nat) → Char → Array (Char × Nat)
    | n + 1, buf, c =>
      match getDecompositionMapping? c with
      | some ⟨tag, m⟩ => if compat || tag.isNone then m.foldl (loop n) buf else pushOrdered buf c
      | none => pushOrdered buf c
    | 0, buf, c => pushOrdered buf c

/-- Primary composite of two characters, including Hangul syllables -/
private def composePair (a b : Char) : Option Char :=
  -- Only characters with `NFC_Quick_Check=M` combine with a preceding character, as
  -- `util/MakeTables` checks.
  if lookupNFCQuickCheck b.val != .maybe then none else
  let a := a.val
  let b := b.val
  -- Hangul syllables compose by the algorithm in the "Conjoining Jamo Behavior" section of the
  -- core specification: L + V → LV and LV + T → LVT.
  if 0x1100 ≤ a && a < 0x1113 && 0x1161 ≤ b && b < 0x1176 then
    some <| Char.ofNat (0xAC00 + ((a - 0x1100) * 21 + (b - 0x1161)) * 28).toNat
  else if 0xAC00 ≤ a && a ≤ 0xD7A3 && (a - 0xAC00) % 28 == 0 && 0x11A7 < b && b < 0x11C3 then
    some <| Char.ofNat (a + (b - 0x11A7)).toNat
  else
    let c := lookupPrimaryComposite a b
    if c == 0 then none else some (Char.ofNat c.toNat)

/-- Canonical composition of a string in canonical order -/
private def compose (buf : Array (Char × Nat)) : String :=
  (loop 0 #[] none 0).foldl String.push ""
where
  /-- `starter` is the position of the last starter in `out`, and `last` the combining class of
    the last character of `out` -/
  loop (i : Nat) (out : Array Char) (starter : Option Nat) (last : Nat) : Array Char :=
    if h : i < buf.size then
      let (c, cc) := buf[i]
      let composed := match starter with
        | some j =>
          -- `c` is not blocked from the starter if it follows the starter immediately, or if
          -- every character in between has a lower combining class, which is then at least `1`
          if out.size == j + 1 || last < cc then
            (composePair out[j]! c).map (j, ·)
          else none
        | none => none
      match composed with
      | some (j, d) => loop (i + 1) (out.set! j d) starter last
      | none =>
        let starter := if cc == 0 then some out.size else starter
        loop (i + 1) (out.push c) starter cc
    else out

/-- Normalize a whole string -/
private def normalizeAll (form : NormalizationForm) (s : String.Slice) : String :=
  let buf := s.chars.fold (pushDecomposition form.isCompat) #[]
  if form.isComposed then compose buf else buf.foldl (fun s (c, _) => s.push c) ""

end Normalization

open Normalization

/-- Quick check whether a string is in a normalization form

  Returns `.yes` if the string is in the normalization form, `.no` if it is not, and `.maybe` if
  the quick check cannot tell. `.maybe` only occurs for `.nfc` and `.nfkc`.

  Specification: quick check, Unicode Standard Annex #15, *Unicode Normalization Forms*. -/
public def quickCheck (form : NormalizationForm) (s : String.Slice) : QuickCheck :=
  (scan form s).1

/-- Check whether a string is in a normalization form

  Specification: Unicode Standard Annex #15, *Unicode Normalization Forms*. -/
public def isNormalized (form : NormalizationForm) (s : String.Slice) : Bool :=
  match scan form s with
  | (.yes, _) => true
  | (.no, _) => false
  | (.maybe, p) => normalizeAll form (s.sliceFrom p) == (s.sliceFrom p).toString

/-- Normalize a string to a normalization form

  Specification: Unicode Standard Annex #15, *Unicode Normalization Forms*. -/
public def normalize (form : NormalizationForm) (s : String.Slice) : String :=
  match scan form s with
  | (.yes, _) => s.toString
  | (_, p) => (s.sliceTo p).toString ++ normalizeAll form (s.sliceFrom p)

/-- Canonical decomposition of a string (`NFD`)

  Specification: Unicode Standard Annex #15, *Unicode Normalization Forms*. -/
public abbrev toNFD (s : String.Slice) : String := normalize .nfd s

/-- Canonical composition of a string (`NFC`)

  Specification: Unicode Standard Annex #15, *Unicode Normalization Forms*. -/
public abbrev toNFC (s : String.Slice) : String := normalize .nfc s

/-- Compatibility decomposition of a string (`NFKD`)

  Specification: Unicode Standard Annex #15, *Unicode Normalization Forms*. -/
public abbrev toNFKD (s : String.Slice) : String := normalize .nfkd s

/-- Compatibility composition of a string (`NFKC`)

  Specification: Unicode Standard Annex #15, *Unicode Normalization Forms*. -/
public abbrev toNFKC (s : String.Slice) : String := normalize .nfkc s

end Unicode
