/-
Copyright © 2026 François G. Dorais. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/
import UnicodeExtra.Sentence

open Unicode

#guard (sentenceSegments "This is one. This is two? Yes!".toSlice).map (·.toString) ==
  #["This is one. ", "This is two? ", "Yes!"]
-- a full stop followed by a lowercase word does not end the sentence
#guard (sentenceSegments "Mr. smith is here. Ok.".toSlice).map (·.toString) ==
  #["Mr. smith is here. ", "Ok."]
-- closing punctuation stays with its sentence
#guard (sentenceSegments "He said \"Stop.\" Then left.".toSlice).map (·.toString) ==
  #["He said \"Stop.\" ", "Then left."]
-- decimal numbers and abbreviations such as "U.S."
#guard (sentenceSegments "It cost 3.50 in the U.S. today.".toSlice).map (·.toString) ==
  #["It cost 3.50 in the U.S. today."]
#guard (sentenceSegments "One\nTwo".toSlice).map (·.toString) == #["One\n", "Two"]
#guard sentenceSegments "".toSlice == #[]

#guard getSentenceBreak 'A' == .upper
#guard getSentenceBreak 'a' == .lower
#guard getSentenceBreak '.' == .aTerm
#guard getSentenceBreak '?' == .sTerm
