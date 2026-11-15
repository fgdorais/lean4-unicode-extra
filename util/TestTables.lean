/-
Copyright © 2026 François G. Dorais. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/
import UnicodeExtra
import UnicodeExtra.TableLookup

/-! Check the character properties of UnicodeExtra against the Unicode Character Database, for
every code point. The UCD files come from the UnicodeData package. Run `lake exe TestTables` in
this directory. -/

open Unicode

/-- Directory of the UCD files in the UnicodeData package -/
def ucdDir : System.FilePath := ".lake"/"packages"/"UnicodeData"/"data"/"ucd"

/-- Value of every code point from a UCD file, with `default` where the file has no value -/
def readValues (path : System.FilePath) (default : String) (f : Array String.Slice → Option String) :
    IO (Array String) := do
  let mut v : Array String := .replicate 0x110000 default
  for row in ← UCDStream.ofFile (ucdDir/path) do
    let some x := f row | continue
    let r := row[0]!
    let (c₀, c₁) := match r.split ".." |>.toList with
      | [a, b] => (ofHexString! a, ofHexString! b)
      | _ => (ofHexString! r, ofHexString! r)
    for c in [c₀.toNat:c₁.toNat+1] do
      v := v.set! c x
  return v

/-- Short value aliases of a property, from `PropertyValueAliases.txt` -/
def readValueAliases (prop : String) : IO (Std.HashMap String String) := do
  let mut m := {}
  for row in ← UCDStream.ofFile (ucdDir/"PropertyValueAliases.txt") do
    if row[0]! == prop then
      m := m.insert row[2]!.toString row[1]!.toString
  return m

/-- Compare a property with its expected value at every code point except surrogates -/
def check (name : String) (expected : Array String) (actual : Char → String) : IO Nat := do
  let mut failures := 0
  for c in [0:0x110000] do
    if 0xD800 ≤ c && c ≤ 0xDFFF then continue
    let got := actual (Char.ofNat c)
    if got != expected[c]! then
      failures := failures + 1
      if failures ≤ 10 then
        IO.println s!"FAIL {name} U+{toHexStringRaw c.toUInt32}: got {got}, expected {expected[c]!}"
  IO.println s!"{name}: {if failures == 0 then "ok" else s!"{failures} failures"}"
  return failures

def main : IO UInt32 := do
  let mut failures := 0
  let gcb ← readValueAliases "GCB"
  let t ← readValues ("auxiliary"/"GraphemeBreakProperty.txt") "XX" fun row =>
    gcb.get? row[1]!.toString
  failures := failures + (← check "Grapheme_Cluster_Break" t fun c => (getGraphemeClusterBreak c).toAbbrev)
  let t ← readValues ("emoji"/"emoji-data.txt") "N" fun row =>
    if row[1]! == "Extended_Pictographic" then some "Y" else none
  failures := failures + (← check "Extended_Pictographic" t fun c =>
    if isExtendedPictographic c then "Y" else "N")
  let t ← readValues "DerivedCoreProperties.txt" "None" fun row =>
    if row[1]! == "InCB" then some row[2]!.toString else none
  failures := failures + (← check "Indic_Conjunct_Break" t fun c => (getIndicConjunctBreak c).toAbbrev)
  let t ← readValues "PropList.txt" "N" fun row =>
    if row[1]! == "Soft_Dotted" then some "Y" else none
  failures := failures + (← check "Soft_Dotted" t fun c =>
    if lookupSoftDotted c.val then "Y" else "N")
  let sb ← readValueAliases "SB"
  let t ← readValues ("auxiliary"/"SentenceBreakProperty.txt") "XX" fun row =>
    sb.get? row[1]!.toString
  failures := failures + (← check "Sentence_Break" t fun c => (getSentenceBreak c).toAbbrev)
  let wb ← readValueAliases "WB"
  let t ← readValues ("auxiliary"/"WordBreakProperty.txt") "XX" fun row =>
    wb.get? row[1]!.toString
  failures := failures + (← check "Word_Break" t fun c => (getWordBreak c).toAbbrev)
  return if failures == 0 then 0 else 1
