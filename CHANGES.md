# Changelog

## UNRELEASED

- Added lexer-level compatibility for old KDLv1 documents.
- Added `Kdl.{to_string_compact,node_to_string,node_to_string_compact}`.
- Simplified the semantics of `Kdl.Num.equal`.
- Fixed a token's `pos_bol` being zero.
- Fixed `equal_node` not taking children into account.
- Fixed `pp_node_compact` outputting invalid KDL.
- Fixed a bug in lexer's buffer refilling.
- Sexp conversion now produces `number-int` and `number-float` for numbers.

## 0.2.0 (2024-12-27)

- Updated to KDL v2.0 (many breaking changes, see the KDL changelog).
- Reworked numbers: added the `Kdl.Num` module, including
  `Kdl.Num.to_{string,float,int,int32,int64,nativeint}` and other functions.
- `Kdl.L`: Added `(.@!())` and `(.@!())<-` indexing operators as raising
  versions of `(.@())` and `(.@()<-)`, added `KDL.L.first_arg`.
- `interpret` now raises `Invalid_annotation` instead of `Failure`.
- `i8`, `i16`, etc. wrappers over `interpret` have been removed.
- Added `of_chunk_gen{,exn}`. Removed the `show`, `show_error`,
  `from_string{,exn}` aliases. Renamed `from_channel{,exn}` to
  `of_channel{,exn}`.
- Added `pp_error`, `pp_typed_value`.
- Dropped support for OCaml < 4.14.0.

## 0.1.0 (2022-10-01)

Initial experimental release.
