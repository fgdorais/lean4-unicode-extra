/-
Copyright © 2026 François G. Dorais. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/
import UnicodeExtra.Casing

open Unicode

/-! Default mappings -/

#guard toLowercase "HELLO, World!".toSlice == "hello, world!"
#guard toUppercase "stra\u00DFe".toSlice == "STRASSE"
#guard toCasefold "Stra\u00DFe".toSlice == "strasse"
#guard toLowercase "\u0130".toSlice == "i\u0307"
#guard toUppercase "\u1FB3".toSlice == "\u0391\u0399"
#guard toLowercase "".toSlice == ""

/-! Final sigma -/

#guard toLowercase "\u039F\u0394\u039F\u03A3".toSlice == "\u03BF\u03B4\u03BF\u03C2"
#guard toLowercase "\u039F\u03A3 \u039F\u03A3".toSlice == "\u03BF\u03C2 \u03BF\u03C2"
#guard toLowercase "\u03A3".toSlice == "\u03C3"
#guard toLowercase "3\u03A3".toSlice == "3\u03C3"
#guard toLowercase "THE 3\u03A3 RULE".toSlice == "the 3\u03C3 rule"
#guard toLowercase "\u0391\u03A3\u0391".toSlice == "\u03B1\u03C3\u03B1"
-- case-ignorable characters between the sigma and the cased characters
#guard toLowercase "\u0391.\u03A3".toSlice == "\u03B1.\u03C2"
#guard toLowercase "\u0391\u03A3'\u0391".toSlice == "\u03B1\u03C3'\u03B1"
#guard toLowercase "\u0391\u03A3\u00AD".toSlice == "\u03B1\u03C2\u00AD"
-- U+0345 COMBINING GREEK YPOGEGRAMMENI and U+02B0 MODIFIER LETTER SMALL H are both cased and
-- case-ignorable; they are skipped as case-ignorable, never counted as cased
#guard toLowercase "\u0345\u03A3".toSlice == "\u0345\u03C3"
#guard toLowercase "\u0391\u03A3\u0345".toSlice == "\u03B1\u03C2\u0345"
#guard toLowercase "\u0391\u03A3\u0345\u0391".toSlice == "\u03B1\u03C3\u0345\u03B1"
#guard toLowercase "\u0391\u02B0\u03A3".toSlice == "\u03B1\u02B0\u03C2"
#guard toLowercase "\u02B0\u03A3".toSlice == "\u02B0\u03C3"

/-! Lithuanian -/

#guard toLowercase "I\u0300".toSlice .lithuanian == "i\u0307\u0300"
#guard toLowercase "I".toSlice .lithuanian == "i"
#guard toLowercase "J\u0328\u0301".toSlice .lithuanian == "j\u0307\u0328\u0301"
#guard toLowercase "\u00CC".toSlice .lithuanian == "i\u0307\u0300"
#guard toLowercase "\u00CC".toSlice == "\u00EC"
#guard toUppercase "i\u0307".toSlice .lithuanian == "I"
#guard toUppercase "i\u0328\u0307".toSlice .lithuanian == "I\u0328"
#guard toUppercase "a\u0307".toSlice .lithuanian == "A\u0307"
#guard toUppercase "i\u0307".toSlice == "I\u0307"

/-! Turkish and Azeri -/

#guard toLowercase "I".toSlice .turkish == "\u0131"
#guard toLowercase "\u0130".toSlice .turkish == "i"
#guard toLowercase "I\u0307".toSlice .turkish == "i"
#guard toLowercase "I\u0316\u0307".toSlice .azeri == "i\u0316"
#guard toLowercase "I\u0301\u0307".toSlice .turkish == "\u0131\u0301\u0307"
#guard toUppercase "i".toSlice .turkish == "\u0130"
#guard toUppercase "\u0131".toSlice .azeri == "I"
#guard toCasefold "I\u0130".toSlice .turkish == "\u0131i"
#guard toCasefold "I\u0130".toSlice == "ii\u0307"

/-! Language tags -/

#guard CasingLanguage.ofLanguageTag "lt".toSlice == .lithuanian
#guard CasingLanguage.ofLanguageTag "TR-cy".toSlice == .turkish
#guard CasingLanguage.ofLanguageTag "az_Latn".toSlice == .azeri
#guard CasingLanguage.ofLanguageTag "en-US".toSlice == .default
#guard CasingLanguage.ofLanguageTag "".toSlice == .default
#guard CasingLanguage.ofLanguageTag "trk".toSlice == .default
