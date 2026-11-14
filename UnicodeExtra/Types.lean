/-
Copyright © 2026 François G. Dorais. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/
module

/-! # Property value types -/

namespace Unicode

/-- Grapheme cluster break property value

  Unicode property: `Grapheme_Cluster_Break` -/
public inductive GraphemeClusterBreak where
  /-- Any other character (`XX`) -/
  | other
  /-- U+000D CARRIAGE RETURN (`CR`) -/
  | cr
  /-- U+000A LINE FEED (`LF`) -/
  | lf
  /-- Control and format characters, separators and unassigned default ignorables (`CN`) -/
  | control
  /-- Grapheme extenders and emoji modifiers (`EX`) -/
  | extend
  /-- U+200D ZERO WIDTH JOINER (`ZWJ`) -/
  | zwj
  /-- Regional indicator symbols, used in pairs for flags (`RI`) -/
  | regionalIndicator
  /-- Prepended concatenation marks and similar characters (`PP`) -/
  | prepend
  /-- Spacing marks that extend a grapheme cluster (`SM`) -/
  | spacingMark
  /-- Hangul leading consonant jamo (`L`) -/
  | jamoL
  /-- Hangul vowel jamo (`V`) -/
  | jamoV
  /-- Hangul trailing consonant jamo (`T`) -/
  | jamoT
  /-- Hangul LV syllable (`LV`) -/
  | syllableLV
  /-- Hangul LVT syllable (`LVT`) -/
  | syllableLVT
  deriving Inhabited, DecidableEq, Repr

/-- Indic conjunct break property value

  Unicode property: `Indic_Conjunct_Break` -/
public inductive IndicConjunctBreak where
  /-- Not part of an Indic conjunct (`None`) -/
  | none
  /-- Virama that links consonants into a conjunct (`Linker`) -/
  | linker
  /-- Consonant that can take part in a conjunct (`Consonant`) -/
  | consonant
  /-- Combining mark or joiner that can occur inside a conjunct (`Extend`) -/
  | extend
  deriving Inhabited, DecidableEq, Repr

/-- Get the short alias of a grapheme cluster break value -/
public def GraphemeClusterBreak.toAbbrev : GraphemeClusterBreak → String
  | .other => "XX"
  | .cr => "CR"
  | .lf => "LF"
  | .control => "CN"
  | .extend => "EX"
  | .zwj => "ZWJ"
  | .regionalIndicator => "RI"
  | .prepend => "PP"
  | .spacingMark => "SM"
  | .jamoL => "L"
  | .jamoV => "V"
  | .jamoT => "T"
  | .syllableLV => "LV"
  | .syllableLVT => "LVT"

/-- Get a grapheme cluster break value from its short alias -/
public def GraphemeClusterBreak.ofAbbrev? (abbr : String.Slice) : Option GraphemeClusterBreak :=
  match abbr.toString with
  | "XX" => some .other
  | "CR" => some .cr
  | "LF" => some .lf
  | "CN" => some .control
  | "EX" => some .extend
  | "ZWJ" => some .zwj
  | "RI" => some .regionalIndicator
  | "PP" => some .prepend
  | "SM" => some .spacingMark
  | "L" => some .jamoL
  | "V" => some .jamoV
  | "T" => some .jamoT
  | "LV" => some .syllableLV
  | "LVT" => some .syllableLVT
  | _ => none

@[inherit_doc GraphemeClusterBreak.ofAbbrev?]
public def GraphemeClusterBreak.ofAbbrev! (abbr : String.Slice) : GraphemeClusterBreak :=
  match ofAbbrev? abbr with
  | some x => x
  | Option.none => panic! "invalid grapheme cluster break alias"

/-- Get the short alias of a Indic conjunct break value -/
public def IndicConjunctBreak.toAbbrev : IndicConjunctBreak → String
  | .none => "None"
  | .linker => "Linker"
  | .consonant => "Consonant"
  | .extend => "Extend"

/-- Get a Indic conjunct break value from its short alias -/
public def IndicConjunctBreak.ofAbbrev? (abbr : String.Slice) : Option IndicConjunctBreak :=
  match abbr.toString with
  | "None" => some .none
  | "Linker" => some .linker
  | "Consonant" => some .consonant
  | "Extend" => some .extend
  | _ => none

@[inherit_doc IndicConjunctBreak.ofAbbrev?]
public def IndicConjunctBreak.ofAbbrev! (abbr : String.Slice) : IndicConjunctBreak :=
  match ofAbbrev? abbr with
  | some x => x
  | Option.none => panic! "invalid Indic conjunct break alias"

/-- Word break property value

  Unicode property: `Word_Break` -/
public inductive WordBreak where
  /-- Any other character (`XX`) -/
  | other
  /-- U+000D CARRIAGE RETURN (`CR`) -/
  | cr
  /-- U+000A LINE FEED (`LF`) -/
  | lf
  /-- Other line and paragraph separators (`NL`) -/
  | newline
  /-- Combining marks and other extenders (`Extend`) -/
  | extend
  /-- U+200D ZERO WIDTH JOINER (`ZWJ`) -/
  | zwj
  /-- Regional indicator symbols (`RI`) -/
  | regionalIndicator
  /-- Format characters (`FO`) -/
  | format
  /-- Katakana (`KA`) -/
  | katakana
  /-- Hebrew letters (`HL`) -/
  | hebrewLetter
  /-- Other letters (`LE`) -/
  | aLetter
  /-- U+0027 APOSTROPHE (`SQ`) -/
  | singleQuote
  /-- U+0022 QUOTATION MARK (`DQ`) -/
  | doubleQuote
  /-- Punctuation inside both words and numbers, such as `.` (`MB`) -/
  | midNumLet
  /-- Punctuation inside words, such as `:` (`ML`) -/
  | midLetter
  /-- Punctuation inside numbers, such as `,` (`MN`) -/
  | midNum
  /-- Digits (`NU`) -/
  | numeric
  /-- Connector punctuation, such as `_` (`EX`) -/
  | extendNumLet
  /-- Spaces that separate words (`WSegSpace`) -/
  | wSegSpace
  deriving Inhabited, DecidableEq, Repr

/-- Get the short alias of a word break value -/
public def WordBreak.toAbbrev : WordBreak → String
  | .other => "XX"
  | .cr => "CR"
  | .lf => "LF"
  | .newline => "NL"
  | .extend => "Extend"
  | .zwj => "ZWJ"
  | .regionalIndicator => "RI"
  | .format => "FO"
  | .katakana => "KA"
  | .hebrewLetter => "HL"
  | .aLetter => "LE"
  | .singleQuote => "SQ"
  | .doubleQuote => "DQ"
  | .midNumLet => "MB"
  | .midLetter => "ML"
  | .midNum => "MN"
  | .numeric => "NU"
  | .extendNumLet => "EX"
  | .wSegSpace => "WSegSpace"

/-- Get a word break value from its short alias -/
public def WordBreak.ofAbbrev? (abbr : String.Slice) : Option WordBreak :=
  match abbr.toString with
  | "XX" => some .other
  | "CR" => some .cr
  | "LF" => some .lf
  | "NL" => some .newline
  | "Extend" => some .extend
  | "ZWJ" => some .zwj
  | "RI" => some .regionalIndicator
  | "FO" => some .format
  | "KA" => some .katakana
  | "HL" => some .hebrewLetter
  | "LE" => some .aLetter
  | "SQ" => some .singleQuote
  | "DQ" => some .doubleQuote
  | "MB" => some .midNumLet
  | "ML" => some .midLetter
  | "MN" => some .midNum
  | "NU" => some .numeric
  | "EX" => some .extendNumLet
  | "WSegSpace" => some .wSegSpace
  | _ => none

@[inherit_doc WordBreak.ofAbbrev?]
public def WordBreak.ofAbbrev! (abbr : String.Slice) : WordBreak :=
  match ofAbbrev? abbr with
  | some x => x
  | Option.none => panic! "invalid word break alias"

/-- Sentence break property value

  Unicode property: `Sentence_Break` -/
public inductive SentenceBreak where
  /-- Any other character (`XX`) -/
  | other
  /-- U+000D CARRIAGE RETURN (`CR`) -/
  | cr
  /-- U+000A LINE FEED (`LF`) -/
  | lf
  /-- Other line and paragraph separators (`SE`) -/
  | sep
  /-- Combining marks and other extenders (`EX`) -/
  | extend
  /-- Format characters (`FO`) -/
  | format
  /-- Spaces (`SP`) -/
  | sp
  /-- Lowercase letters (`LO`) -/
  | lower
  /-- Uppercase and titlecase letters (`UP`) -/
  | upper
  /-- Other letters (`LE`) -/
  | oLetter
  /-- Digits (`NU`) -/
  | numeric
  /-- Full stops, which may also end abbreviations (`AT`) -/
  | aTerm
  /-- Other sentence terminators, such as `!` and `?` (`ST`) -/
  | sTerm
  /-- Closing punctuation and quotation marks (`CL`) -/
  | close
  /-- Punctuation that may continue a sentence, such as `,` (`SC`) -/
  | sContinue
  deriving Inhabited, DecidableEq, Repr

/-- Get the short alias of a sentence break value -/
public def SentenceBreak.toAbbrev : SentenceBreak → String
  | .other => "XX"
  | .cr => "CR"
  | .lf => "LF"
  | .sep => "SE"
  | .extend => "EX"
  | .format => "FO"
  | .sp => "SP"
  | .lower => "LO"
  | .upper => "UP"
  | .oLetter => "LE"
  | .numeric => "NU"
  | .aTerm => "AT"
  | .sTerm => "ST"
  | .close => "CL"
  | .sContinue => "SC"

/-- Get a sentence break value from its short alias -/
public def SentenceBreak.ofAbbrev? (abbr : String.Slice) : Option SentenceBreak :=
  match abbr.toString with
  | "XX" => some .other
  | "CR" => some .cr
  | "LF" => some .lf
  | "SE" => some .sep
  | "EX" => some .extend
  | "FO" => some .format
  | "SP" => some .sp
  | "LO" => some .lower
  | "UP" => some .upper
  | "LE" => some .oLetter
  | "NU" => some .numeric
  | "AT" => some .aTerm
  | "ST" => some .sTerm
  | "CL" => some .close
  | "SC" => some .sContinue
  | _ => none

@[inherit_doc SentenceBreak.ofAbbrev?]
public def SentenceBreak.ofAbbrev! (abbr : String.Slice) : SentenceBreak :=
  match ofAbbrev? abbr with
  | some x => x
  | Option.none => panic! "invalid sentence break alias"

end Unicode
