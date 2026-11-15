/-
Copyright © 2026 François G. Dorais. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/
import UnicodeBasic

/-! Generate the data tables of UnicodeExtra in `../data` from the Unicode Character Database.

The UCD files come from the UnicodeData package. Run `lake exe MakeTables` in this directory, with
table names as arguments to generate only those tables. -/

open Unicode

/-- Directory of the UCD files in the UnicodeData package -/
def ucdDir : System.FilePath := ".lake"/"packages"/"UnicodeData"/"data"/"ucd"

/-- Read the code point ranges of a UCD file, with the value `f` of each record, if any -/
def readRanges (path : System.FilePath) (f : Array String.Slice → Option α) :
    IO (Array (UInt32 × UInt32 × α)) := do
  let mut out := #[]
  let stream ← UCDStream.ofFile (ucdDir/path)
  for row in stream do
    let some v := f row | continue
    let r := row[0]!
    let (c₀, c₁) := match r.split ".." |>.toList with
      | [a, b] => (ofHexString! a, ofHexString! b)
      | _ => (ofHexString! r, ofHexString! r)
    out := out.push (c₀, c₁, v)
  return out

/-- Sort ranges and merge adjacent ranges with equal values -/
def compressData [BEq α] (arr : Array (UInt32 × UInt32 × α)) : Array (UInt32 × UInt32 × α) :=
  let arr := arr.qsort fun a b => a.1 < b.1
  if h : arr.size > 0 then Id.run do
    let mut res := #[]
    let mut top := arr[0]
    for a in arr[1:] do
      if a.2.2 == top.2.2 && a.1 == top.2.1 + 1 then
        top := (top.1, a.2.1, top.2.2)
      else
        res := res.push top
        top := a
    return res.push top
  else #[]

/-- Sort ranges and merge adjacent ranges -/
def compressProp (arr : Array (UInt32 × UInt32)) : Array (UInt32 × UInt32) :=
  (compressData (arr.map fun (c₀, c₁) => (c₀, c₁, ()))).map fun (c₀, c₁, _) => (c₀, c₁)

/-- Short value alias of a long property value name, as in `PropertyValueAliases.txt` -/
def readValueAliases (prop : String) : IO (Std.HashMap String String) := do
  let mut m := {}
  for row in ← UCDStream.ofFile (ucdDir/"PropertyValueAliases.txt") do
    if row[0]! == prop then
      m := m.insert row[2]!.toString row[1]!.toString
  return m

/-- Ranges of a property value file such as `auxiliary/WordBreakProperty.txt`, with short value
  aliases -/
def readEnumProp (path : System.FilePath) (prop : String) :
    IO (Array (UInt32 × UInt32 × String)) := do
  let alias ← readValueAliases prop
  readRanges path fun row => alias.get? row[1]!.toString

/-- Ranges of a binary property of a UCD file -/
def readBinaryProp (path : System.FilePath) (prop : String) : IO (Array (UInt32 × UInt32)) := do
  let t ← readRanges path fun row => if row[1]! == prop then some () else none
  return t.map fun (c₀, c₁, _) => (c₀, c₁)

def mkExtendedPictographic : IO (Array (UInt32 × UInt32)) :=
  compressProp <$> readBinaryProp ("emoji"/"emoji-data.txt") "Extended_Pictographic"

/-- Hangul LV and LVT syllables are left out: they are determined by the syllable index. -/
def mkGraphemeClusterBreak : IO (Array (UInt32 × UInt32 × String)) := do
  let t ← readEnumProp ("auxiliary"/"GraphemeBreakProperty.txt") "GCB"
  return compressData <| t.filter fun (_, _, v) => v != "LV" && v != "LVT"

def mkIndicConjunctBreak : IO (Array (UInt32 × UInt32 × String)) := do
  let t ← readRanges "DerivedCoreProperties.txt" fun row =>
    if row[1]! == "InCB" then some row[2]!.toString else none
  return compressData t

def mkSoftDotted : IO (Array (UInt32 × UInt32)) :=
  compressProp <$> readBinaryProp "PropList.txt" "Soft_Dotted"

/-- `Upper` and `Lower` are both stored as `lc`: the library tells them apart with UnicodeBasic's
  `isUppercase` and `isTitlecaseLetter`, which this checks. -/
def mkSentenceBreak : IO (Array (UInt32 × UInt32 × String)) := do
  let t ← readEnumProp ("auxiliary"/"SentenceBreakProperty.txt") "SB"
  for (c₀, c₁, v) in t do
    if v == "UP" || v == "LO" then
      for c in [c₀.toNat:c₁.toNat+1] do
        let ch := Char.ofNat c
        if (v == "UP") != (isUppercase ch || GeneralCategory.isTitlecaseLetter ch) then
          throw <| .userError s!"Sentence_Break={v} of U+{toHexStringRaw c.toUInt32} \
            disagrees with isUppercase and isTitlecaseLetter"
  return compressData <| t.map fun (c₀, c₁, v) =>
    (c₀, c₁, if v == "UP" || v == "LO" then "lc" else v)

def mkWordBreak : IO (Array (UInt32 × UInt32 × String)) :=
  compressData <$> readEnumProp ("auxiliary"/"WordBreakProperty.txt") "WB"

/-- Write a range table: `start;stop;value`, with an empty `stop` for a single code point -/
def writeData (name : String) (table : Array (UInt32 × UInt32 × String)) : IO Unit := do
  IO.println s!"Generating table {name}"
  IO.FS.withFile (".."/"data"/(name ++ ".txt")) .write fun file => do
    for (c₀, c₁, v) in table do
      if c₀ == c₁ then
        file.putStrLn <| toHexStringRaw c₀ ++ ";;" ++ v
      else
        file.putStrLn <| toHexStringRaw c₀ ++ ";" ++ toHexStringRaw c₁ ++ ";" ++ v
  IO.println s!"Size: {table.size}"

/-- Write a range table of a binary property: `start;stop`, with an empty `stop` for a single
  code point -/
def writeProp (name : String) (table : Array (UInt32 × UInt32)) : IO Unit := do
  IO.println s!"Generating table {name}"
  IO.FS.withFile (".."/"data"/(name ++ ".txt")) .write fun file => do
    for (c₀, c₁) in table do
      if c₀ == c₁ then
        file.putStrLn <| toHexStringRaw c₀ ++ ";"
      else
        file.putStrLn <| toHexStringRaw c₀ ++ ";" ++ toHexStringRaw c₁
  IO.println s!"Size: {table.size}"

/-- Check the assumptions that `Grapheme.Props.of` makes about the grapheme break properties -/
def checkGraphemeProps : IO Unit := do
  let gcb ← mkGraphemeClusterBreak
  let gcbOf (c : UInt32) : String :=
    match gcb.find? fun (c₀, c₁, _) => c₀ ≤ c && c ≤ c₁ with
    | some (_, _, v) => v
    | none => "XX"
  for (c₀, c₁) in ← mkExtendedPictographic do
    for c in [c₀.toNat:c₁.toNat+1] do
      if gcbOf c.toUInt32 != "XX" then
        throw <| .userError s!"Extended_Pictographic U+{toHexStringRaw c.toUInt32} is not GCB=XX"
  for (c₀, c₁, _) in ← mkIndicConjunctBreak do
    for c in [c₀.toNat:c₁.toNat+1] do
      unless ["XX", "EX", "ZWJ"].contains (gcbOf c.toUInt32) do
        throw <| .userError s!"Indic_Conjunct_Break U+{toHexStringRaw c.toUInt32} is not GCB=XX, EX or ZWJ"

def main (args : List String) : IO UInt32 := do
  checkGraphemeProps
  let args := if args != [] then args else [
    "Extended_Pictographic",
    "Grapheme_Cluster_Break",
    "Indic_Conjunct_Break",
    "Sentence_Break",
    "Soft_Dotted",
    "Word_Break"]
  IO.FS.createDirAll (".."/"data")
  for arg in args do
    match arg with
    | "Extended_Pictographic" => writeProp arg (← mkExtendedPictographic)
    | "Grapheme_Cluster_Break" => writeData arg (← mkGraphemeClusterBreak)
    | "Indic_Conjunct_Break" => writeData arg (← mkIndicConjunctBreak)
    | "Sentence_Break" => writeData arg (← mkSentenceBreak)
    | "Soft_Dotted" => writeProp arg (← mkSoftDotted)
    | "Word_Break" => writeData arg (← mkWordBreak)
    | _ =>
      IO.eprintln s!"Unknown table {arg}"
      return 1
  return 0
