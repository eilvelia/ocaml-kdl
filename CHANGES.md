# Changelog

## Unreleased

- Updated to KDL v2.0 (many breaking changes, see the KDL changelog).
- Reworked numbers: added the `Kdl.Num` module, including
  `Kdl.Num.to_{string,float,int,int32,int64,nativeint}` and other functions.
- `Kdl.L`: Added `(.@!())` and `(.@!())<-` indexing operators as raising
  versions of `(.@())` and `(.@()<-)`.
- `interpret` now raises `Invalid_annotation` instead of `Failure`.
- `i8`, `i16`, etc. wrappers over `interpret` are removed.
- Dropped support for OCaml < 4.10.0.

## 0.1.0 (2022-10-01)

Initial experimental release.
