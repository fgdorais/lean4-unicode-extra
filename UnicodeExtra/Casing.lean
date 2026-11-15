/-
Copyright © 2026 François G. Dorais. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/
module

public import UnicodeBasic
import UnicodeExtra.TableLookup

/-!
# Case Mapping of Strings

Full case mapping and case folding of whole strings, following the default case algorithms in the
"Default Case Algorithms" section of the Unicode Standard's core specification. Unlike the
per-character mappings in `UnicodeBasic`, these apply the conditional mappings of
`SpecialCasing.txt`: the language-independent final sigma rule, and the Lithuanian, Turkish and
Azeri tailorings.
-/

namespace Unicode

/-- Language for language-sensitive case mappings

  These are the languages with tailored mappings in `SpecialCasing.txt` and `CaseFolding.txt`.
  Every other language uses the default mappings. -/
public inductive CasingLanguage where
  /-- Default case mappings, used for every language without a tailoring -/
  | default
  /-- Lithuanian (`lt`) -/
  | lithuanian
  /-- Turkish (`tr`) -/
  | turkish
  /-- Azeri (`az`) -/
  | azeri
  deriving Inhabited, DecidableEq, Repr

/-- Casing language for a BCP 47 language tag

  Only the primary language subtag counts, ignoring case, and `'-'` and `'_'` are treated
  equivalently. For example, `"tr"`, `"TR-cy"` and `"az_Latn"` give tailored languages, while
  `"en-US"` gives `.default`. -/
public def CasingLanguage.ofLanguageTag (tag : String.Slice) : CasingLanguage :=
  let primary := (tag.split fun c => c == '-' || c == '_').toList.headD "".toSlice
  match primary.toString.map Char.toLower with
  | "lt" => .lithuanian
  | "tr" => .turkish
  | "az" => .azeri
  | _ => .default

/-- Check whether the language uses the Turkic case mappings (Turkish or Azeri) -/
public def CasingLanguage.isTurkic : CasingLanguage → Bool
  | .turkish | .azeri => true
  | _ => false

/-- Check if a character has the `Soft_Dotted` property -/
private def isSoftDotted (c : Char) : Bool := lookupSoftDotted c.val

/-- Check if a character has canonical combining class 0 or 230 (Above) -/
private def isStarterOrAbove (c : Char) : Bool :=
  match getCanonicalCombiningClass c with
  | 0 | 230 => true
  | _ => false

/-- Casing context `Final_Sigma` for the character at `p` -/
private def isFinalSigma (s : String.Slice) (p : s.Pos) (h : p ≠ s.endPos) : Bool :=
  before && !after
where
  -- Some characters are both cased and case-ignorable, such as U+0345 COMBINING GREEK
  -- YPOGEGRAMMENI and modifier letters such as U+02B0. The Standard makes the `*` in the
  -- Final_Sigma regular expressions possessive, with no backup, so these characters are always
  -- skipped as case-ignorable and never count as the cased character. ICU does the same.
  /-- Preceded by a cased character and then zero or more case-ignorable characters. -/
  before := Id.run do
    for c in (s.sliceTo p).revChars do
      if !isCaseIgnorable c then return isCased c
    return false
  /-- Followed by zero or more case-ignorable characters and then a cased character. -/
  after := Id.run do
    for c in (s.sliceFrom (p.next h)).chars do
      if !isCaseIgnorable c then return isCased c
    return false

/-- Casing context `After_Soft_Dotted` for the character at `p` -/
private def isAfterSoftDotted (s : String.Slice) (p : s.Pos) : Bool := Id.run do
  for c in (s.sliceTo p).revChars do
    if isSoftDotted c then return true
    if isStarterOrAbove c then return false
  return false

/-- Casing context `More_Above` for the character at `p` -/
private def isMoreAbove (s : String.Slice) (p : s.Pos) (h : p ≠ s.endPos) : Bool := Id.run do
  for c in (s.sliceFrom (p.next h)).chars do
    match getCanonicalCombiningClass c with
    | 230 => return true
    | 0 => return false
    | _ => continue
  return false

/-- Casing context `Before_Dot` for the character at `p` -/
private def isBeforeDot (s : String.Slice) (p : s.Pos) (h : p ≠ s.endPos) : Bool := Id.run do
  for c in (s.sliceFrom (p.next h)).chars do
    if c == '\u0307' then return true
    if isStarterOrAbove c then return false
  return false

/-- Casing context `After_I` for the character at `p` -/
private def isAfterI (s : String.Slice) (p : s.Pos) : Bool := Id.run do
  for c in (s.sliceTo p).revChars do
    if c == 'I' then return true
    if isStarterOrAbove c then return false
  return false

/-- Append the full lowercase mapping of a character, without conditions -/
@[inline] private def pushLower (out : String) (c : Char) : String :=
  withLowercasing c fun c l => l.foldl String.push (out.push c)

/-- Append the full uppercase mapping of a character, without conditions -/
@[inline] private def pushUpper (out : String) (c : Char) : String :=
  withUppercasing c fun c l => l.foldl String.push (out.push c)

/-- Append the full case folding of a character, without conditions -/
@[inline] private def pushCaseFolding (out : String) (c : Char) : String :=
  withCaseFolding c fun c l => l.foldl String.push (out.push c)

/-- Full lowercase mapping of a string

  Maps each character to its full lowercase mapping, applying the conditional mappings of
  `SpecialCasing.txt` for the given language. For every language, a capital sigma maps to
  U+03C2 GREEK SMALL LETTER FINAL SIGMA in the `Final_Sigma` context: it follows a cased
  character, possibly with case-ignorable characters in between, and no cased character follows,
  again possibly after case-ignorable characters. For example, `"ΟΔΟΣ"` maps to `"οδος"` with a
  final sigma, but `"3Σ"` maps to `"3σ"`.

  This is `toLowercase(X)` in the Default Case Algorithms of the Unicode Standard.

  Unicode properties: `Lowercase_Mapping`, `Cased`, `Case_Ignorable`, `Soft_Dotted`,
  `Canonical_Combining_Class` -/
public def toLowercase (s : String.Slice) (lang : CasingLanguage := .default) : String := Id.run do
  let mut out := ""
  for ⟨p, h⟩ in s.positions do
    let c := p.get h
    out := match lang, c with
      | _, '\u03A3' => out.push (if isFinalSigma s p h then '\u03C2' else '\u03C3')
      | .lithuanian, 'I' | .lithuanian, 'J' | .lithuanian, '\u012E' =>
        if isMoreAbove s p h then (pushLower out c).push '\u0307' else pushLower out c
      | .lithuanian, '\u00CC' => out ++ "i\u0307\u0300"
      | .lithuanian, '\u00CD' => out ++ "i\u0307\u0301"
      | .lithuanian, '\u0128' => out ++ "i\u0307\u0303"
      | .turkish, '\u0130' | .azeri, '\u0130' => out.push 'i'
      | .turkish, '\u0307' | .azeri, '\u0307' => if isAfterI s p then out else out.push c
      | .turkish, 'I' | .azeri, 'I' => out.push (if isBeforeDot s p h then 'i' else '\u0131')
      | _, _ => pushLower out c
  return out

/-- Full uppercase mapping of a string

  Maps each character to its full uppercase mapping, applying the conditional mappings of
  `SpecialCasing.txt` for the given language. For example, `"straße"` maps to `"STRASSE"`, and
  with `.turkish`, `"i"` maps to `"İ"`.

  This is `toUppercase(X)` in the Default Case Algorithms of the Unicode Standard.

  Unicode properties: `Uppercase_Mapping`, `Soft_Dotted`, `Canonical_Combining_Class` -/
public def toUppercase (s : String.Slice) (lang : CasingLanguage := .default) : String := Id.run do
  let mut out := ""
  for ⟨p, h⟩ in s.positions do
    let c := p.get h
    out := match lang, c with
      | .lithuanian, '\u0307' => if isAfterSoftDotted s p then out else out.push c
      | .turkish, 'i' | .azeri, 'i' => out.push '\u0130'
      | _, _ => pushUpper out c
  return out

-- TODO: `toTitlecase(X)` from the Default Case Algorithms. It titlecases the first cased
-- character after each word boundary and lowercases the rest of the word, so it needs the word
-- boundaries of UAX #29, which UnicodeExtra does not have yet.

/-- Full case folding of a string

  Maps each character to its full case folding. With `.turkish` or `.azeri`, uses the Turkic
  foldings of `CaseFolding.txt` (status `T`) for U+0049 LATIN CAPITAL LETTER I and U+0130 LATIN
  CAPITAL LETTER I WITH DOT ABOVE. Case folding does not depend on context.

  This is `toCasefold(X)` in the Default Case Algorithms of the Unicode Standard.

  Unicode property: `Case_Folding` -/
public def toCasefold (s : String.Slice) (lang : CasingLanguage := .default) : String := Id.run do
  let mut out := ""
  for c in s.chars do
    out := match lang, c with
      | .turkish, 'I' | .azeri, 'I' => out.push '\u0131'
      | .turkish, '\u0130' | .azeri, '\u0130' => out.push 'i'
      | _, _ => pushCaseFolding out c
  return out

end Unicode
