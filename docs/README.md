# Lean 4 / Unicode Extra

Unicode algorithms for Lean 4 that are too large or too specialized for
[UnicodeBasic](https://github.com/fgdorais/lean4-unicode-basic), built on top of it.

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
