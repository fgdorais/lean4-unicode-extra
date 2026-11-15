/-
Copyright © 2026 François G. Dorais. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/
import UnicodeExtra

/-! Conformance tests for text segmentation, using the test files of the Unicode Character
Database from the UnicodeData package. Run `lake exe TestSegmentation` in this directory. -/

open Unicode

/-- Directory of the UCD files in the UnicodeData package -/
def ucdDir : System.FilePath := ".lake"/"packages"/"UnicodeData"/"data"/"ucd"

/-- Parse a test line such as `÷ 0020 × 0308 ÷` into its expected segments -/
def parseTestLine (line : String) : List String := Id.run do
  let mut segs : Array String := #[]
  let mut cur := ""
  for tok in line.splitOn " " do
    if tok == "÷" then
      if !cur.isEmpty then segs := segs.push cur
      cur := ""
    else if tok == "×" || tok.isEmpty then
      continue
    else
      cur := cur.push (Char.ofNat (ofHexString! tok.toSlice).toNat)
  return segs.toList

/-- Split a string with `splitGraphemeCluster?` until it is empty -/
def splitAll (s : String) : List String := Id.run do
  let mut out := #[]
  let mut rest := s.toSlice
  for _ in [0:s.utf8ByteSize] do
    match splitGraphemeCluster? rest with
    | some (c, r) =>
      out := out.push c.toString
      rest := r
    | none => break
  return out.toList

def showSegs (l : List String) : String :=
  " ÷ ".intercalate <| l.map fun s => " ".intercalate (s.toList.map fun c => toHexStringRaw c.val)

def testGrapheme : IO Nat := do
  let mut failures := 0
  let mut count := 0
  for line in ← IO.FS.lines (ucdDir/"auxiliary"/"GraphemeBreakTest.txt") do
    let (test, cmt) := match line.splitOn "#" with
      | t :: c => (t.trimAscii.toString, "#".intercalate c)
      | [] => ("", "")
    if test.isEmpty then continue
    count := count + 1
    let expected := parseTestLine test
    let s := String.join expected
    let got := (graphemeClusters s.toSlice).toList.map (·.toString)
    let split := splitAll s
    if got != expected || split != expected then
      failures := failures + 1
      IO.println s!"FAIL {test}"
      IO.println s!"  got   {showSegs got}"
      if split != got then IO.println s!"  split {showSegs split}"
      IO.println s!"  {cmt.trimAscii}"
  IO.println s!"GraphemeBreakTest.txt: {count - failures}/{count} passed"
  return failures

def main : IO UInt32 := do
  let failures ← testGrapheme
  return if failures == 0 then 0 else 1
