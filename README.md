# Lean 4 / Unicode Extra

Unicode algorithms for Lean 4 that are too large or too specialized to live in
[UnicodeBasic](https://github.com/fgdorais/lean4-unicode-basic).

## The Lean4 / Unicode family

This library is one of a small family of Lean 4 Unicode libraries. Each one has a narrow job, so
you only depend on what you need.

| Library | What it is for |
| --- | --- |
| [UnicodeBasic](https://github.com/fgdorais/lean4-unicode-basic) | The foundation. Fast lookups of commonly used character properties such as general category, case mappings, numeric values, scripts and names, from compact precompiled tables. Start here. |
| [UnicodeData](https://github.com/fgdorais/lean4-unicode-data) | Direct access to the Unicode Character Database files themselves, such as `UnicodeData.txt`, `PropList.txt`, `CaseFolding.txt` and `SpecialCasing.txt`. Heavier than UnicodeBasic, but complete, and handy for properties UnicodeBasic does not cover or for checking it against the source. |
| [UnicodeFormat](https://github.com/fgdorais/lean4-unicode-format) | Encoding and decoding: UTF-8, UTF-16 and UTF-32, the byte-level encoding schemes with byte order marks, strict and lossy decoding, and streaming iterators. |
| [UnicodeLemmas](https://github.com/fgdorais/lean4-unicode-lemmas) | Proofs about UnicodeBasic properties, such as the fact that every cased character is alphabetic. |
| **UnicodeExtra** (this library) | Unicode algorithms built on top of UnicodeBasic. |

## What this library is for

UnicodeBasic answers questions about single characters: what is the general category of this
character, what is its lowercase form, which script does it belong to? Many real tasks need more
than that. They work on whole strings and follow the multi-step algorithms specified in the Unicode
Standard and its annexes, often with extra data tables and many special cases.

That kind of code would make UnicodeBasic slower to build and harder to keep lightweight, so it
lives here instead. Typical candidates are algorithms such as full case mapping and case-insensitive
comparison, normalization, text segmentation into grapheme clusters, words and sentences, and line
breaking. UnicodeExtra depends on UnicodeBasic and reuses its character properties rather than
duplicating them.

The library is new and still taking shape. If there is a Unicode algorithm you would like to see
here, please open an issue!

## Installation

Add the following dependency to your project's `lakefile.toml`:

```toml
[[require]]
name = "UnicodeExtra"
git = "https://github.com/fgdorais/lean4-unicode-extra.git"
rev = "main" # or any specific revision
```

Or this dependency to your project's `lakefile.lean`:

```lean4
require UnicodeExtra from git
  "https://github.com/fgdorais/lean4-unicode-extra.git" @ "main"
```

## Documentation

You can generate documentation locally using `lake build UnicodeExtra:docs` in the `docs` directory.

-----

* The `Lean 4 / Unicode Extra` library is copyright © 2026 François G. Dorais. The library is released under the [Apache 2.0 license](http://www.apache.org/licenses/LICENSE-2.0). See the file LICENSE for additional details.
