/-
Copyright © 2026 François G. Dorais. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/
import UnicodeExtra.Normalization

open Unicode

#guard toNFD "é".toSlice == "e\u0301"
#guard toNFC "e\u0301".toSlice == "é"
#guard toNFKD "ﬁ".toSlice == "fi"
#guard toNFKC "２⁵".toSlice == "25"
#guard toNFC "\u1112\u1161\u11AB".toSlice == "한"
#guard toNFD "한".toSlice == "\u1112\u1161\u11AB"
#guard toNFC "a\u0323\u0302".toSlice == "ậ"
#guard toNFC "a\u0302\u0323".toSlice == "ậ"
#guard toNFD "q\u0307\u0323".toSlice == "q\u0323\u0307"
#guard toNFC "\u212B".toSlice == "Å"
#guard toNFC "".toSlice == ""

#guard isNormalized .nfc "Hello, world!".toSlice
#guard !isNormalized .nfd "é".toSlice
#guard !isNormalized .nfc "e\u0301".toSlice
#guard isNormalized .nfc "\u0301".toSlice

#guard quickCheck .nfc "abc".toSlice == .yes
#guard quickCheck .nfc "e\u0301".toSlice == .maybe
#guard quickCheck .nfc "\u212B".toSlice == .no
#guard quickCheck .nfd "a\u0302\u0323".toSlice == .no
