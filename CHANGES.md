# Changelog

## Unreleased

- Updated to KDL v2.0:
- - Added Line Tabulation U+000B to whitespace characters
- - Removed `\/` from escape sequences
- - Added whitespace escape sequence: all whitespace (including multiple
    newlines) is removed after `\`
- - Identifiers cannot start with `r#` anymore
- `interpret` now raises `Invalid_annotation` instead of `Failure`.
- `i8`, `i16`, etc. wrappers over `interpret` are removed.
- Dropped support for OCaml < 4.10.0

## 0.1.0 (2022-10-01)

Initial release.
