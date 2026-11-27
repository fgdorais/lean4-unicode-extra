/-
Copyright © 2026 François G. Dorais. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/
import UnicodeExtra

/-! Conformance tests for normalization, using `NormalizationTest.txt` of the Unicode Character
Database from the UnicodeData package. Run `lake exe TestNormalization` in this directory. -/

open Unicode

/-- Directory of the UCD files in the UnicodeData package -/
def ucdDir : System.FilePath := ".lake"/"packages"/"UnicodeData"/"data"/"ucd"

def showStr (s : String) : String := " ".intercalate (s.toList.map fun c => toHexStringRaw c.val)

def forms : List (String × NormalizationForm) :=
  [("NFD", .nfd), ("NFC", .nfc), ("NFKD", .nfkd), ("NFKC", .nfkc)]

/-- Check that `normalize`, `isNormalized` and `quickCheck` agree with the expected normal form
  of a string, returning a description of each failure -/
def checkForm (name : String) (form : NormalizationForm) (s expected : String) : List String :=
  let got := normalize form s.toSlice
  let norm := expected == s
  let qc := quickCheck form s.toSlice
  (if got != expected then [s!"to{name}({showStr s}) = {showStr got}, expected {showStr expected}"] else []) ++
  (if isNormalized form s.toSlice != norm then [s!"isNormalized {name} ({showStr s}) ≠ {norm}"] else []) ++
  (if (qc == .yes && !norm) || (qc == .no && norm) then
    [s!"quickCheck {name} ({showStr s}) = {qc.toAbbrev}, but normalized is {norm}"] else [])

def main : IO UInt32 := do
  let mut failures := 0
  let mut count := 0
  let mut part1 : Std.HashSet UInt32 := {}
  let mut part := ""
  for line in ← IO.FS.lines (ucdDir/"NormalizationTest.txt") do
    if line.startsWith "@" then
      part := (line.splitOn " ").headD ""
      continue
    let test := (line.splitOn "#").headD ""
    let cols := test.splitOn ";"
    if cols.length < 5 then continue
    let c := (cols.take 5).toArray.map fun col =>
      (col.trimAscii.toString.splitOn " ").foldl (fun s x => s.push (Char.ofNat (ofHexString! x.toSlice).toNat)) ""
    if part == "@Part1" then part1 := part1.insert c[0]!.front.val
    count := count + 1
    -- NFC: c2 == toNFC(c1) == toNFC(c2) == toNFC(c3), c4 == toNFC(c4) == toNFC(c5)
    -- NFD: c3 == toNFD(c1) == toNFD(c2) == toNFD(c3), c5 == toNFD(c4) == toNFD(c5)
    -- NFKC: c4 == toNFKC(c1) == ... == toNFKC(c5)
    -- NFKD: c5 == toNFKD(c1) == ... == toNFKD(c5)
    let expected : List (String × NormalizationForm × Nat × Nat) :=
      [("NFC", .nfc, 0, 1), ("NFC", .nfc, 1, 1), ("NFC", .nfc, 2, 1), ("NFC", .nfc, 3, 3), ("NFC", .nfc, 4, 3),
       ("NFD", .nfd, 0, 2), ("NFD", .nfd, 1, 2), ("NFD", .nfd, 2, 2), ("NFD", .nfd, 3, 4), ("NFD", .nfd, 4, 4)] ++
      ([0, 1, 2, 3, 4].map fun i => ("NFKC", .nfkc, i, 3)) ++
      ([0, 1, 2, 3, 4].map fun i => ("NFKD", .nfkd, i, 4))
    let errs := expected.flatMap fun (name, form, i, j) => checkForm name form c[i]! c[j]!
    if !errs.isEmpty then
      failures := failures + 1
      if failures ≤ 20 then
        IO.println s!"FAIL {test.trimAscii}"
        for e in errs do IO.println s!"  {e}"
  IO.println s!"NormalizationTest.txt: {count - failures}/{count} passed"
  -- Every code point not in part 1 is unchanged by every normalization form
  let mut other := 0
  for c in [0:0x110000] do
    if 0xD800 ≤ c && c ≤ 0xDFFF || part1.contains c.toUInt32 then continue
    let s := String.singleton (Char.ofNat c)
    let errs := forms.flatMap fun (name, form) => checkForm name form s s
    if !errs.isEmpty then
      other := other + 1
      if other ≤ 20 then
        for e in errs do IO.println s!"FAIL {e}"
  IO.println s!"Code points not in part 1: {if other == 0 then "ok" else s!"{other} failures"}"
  return if failures + other == 0 then 0 else 1
