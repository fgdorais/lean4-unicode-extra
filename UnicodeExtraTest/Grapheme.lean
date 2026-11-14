/-
Copyright © 2026 François G. Dorais. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/
import UnicodeExtra.Grapheme

open Unicode

-- simple letters
#guard (graphemeClusters "abc".toSlice).map (·.toString) == #["a", "b", "c"]
-- CR LF stays together
#guard (graphemeClusters "\u000D\u000Aa".toSlice).map (·.toString) == #["\u000D\u000A", "a"]
-- combining mark
#guard (graphemeClusters "e\u0301x".toSlice).map (·.toString) == #["e\u0301", "x"]
-- Hangul syllable from jamo
#guard (graphemeClusters "\u1100\u1161\u11A8\uAC00".toSlice).map (·.toString) == #["\u1100\u1161\u11A8", "\uAC00"]
-- flags: pairs of regional indicators
#guard (graphemeClusters "🇫🇷🇩🇪🇫".toSlice).map (·.toString) == #["🇫🇷", "🇩🇪", "🇫"]
-- emoji ZWJ sequence
#guard (graphemeClusters "👨\u200D👩\u200D👧".toSlice).map (·.toString) == #["👨\u200D👩\u200D👧"]
-- emoji with skin tone modifier
#guard (graphemeClusters "👍🏽!".toSlice).map (·.toString) == #["👍🏽", "!"]
-- Devanagari conjunct
#guard (graphemeClusters "\u0915\u094D\u0937".toSlice).map (·.toString) == #["\u0915\u094D\u0937"]

#guard graphemeClusters "".toSlice == #[]
#guard splitGraphemeCluster? "".toSlice == none
#guard (splitGraphemeCluster? "e\u0301x".toSlice).map (fun (a, b) => (a.toString, b.toString)) == some ("e\u0301", "x")

#guard getGraphemeClusterBreak 'a' == .other
#guard getGraphemeClusterBreak '\uAC00' == .syllableLV
#guard getGraphemeClusterBreak '\uAC01' == .syllableLVT
#guard isExtendedPictographic '😀'
#guard getIndicConjunctBreak '\u094D' == .linker
