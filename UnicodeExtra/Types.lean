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

end Unicode
