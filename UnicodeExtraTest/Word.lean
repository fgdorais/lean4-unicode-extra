/-
Copyright © 2026 François G. Dorais. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/
import UnicodeExtra.Word

open Unicode

#guard (wordSegments "Hello, world!".toSlice).map (·.toString) == #["Hello", ",", " ", "world", "!"]
#guard (wordSegments "can't stop".toSlice).map (·.toString) == #["can't", " ", "stop"]
#guard (wordSegments "3.14 and 1,000".toSlice).map (·.toString) == #["3.14", " ", "and", " ", "1,000"]
#guard (wordSegments "snake_case  x".toSlice).map (·.toString) == #["snake_case", "  ", "x"]
#guard (wordSegments "e.g.".toSlice).map (·.toString) == #["e.g", "."]
#guard wordSegments "".toSlice == #[]

#guard getWordBreak 'a' == .aLetter
#guard getWordBreak '7' == .numeric
#guard getWordBreak '\'' == .singleQuote
#guard getWordBreak '_' == .extendNumLet
