# ocaml-kdl &nbsp; [![CI][ci-badge]][ci-page]

[ci-badge]: https://github.com/Bannerets/ocaml-kdl/actions/workflows/ci.yml/badge.svg
[ci-page]: https://github.com/Bannerets/ocaml-kdl/actions/workflows/ci.yml

An OCaml implementation of the [KDL][] 1.0 document language.

[KDL]: https://github.com/kdl-org/kdl

```console
$ opam install kdl
```

## Features

- [x] Parsing
- [x] Pretty-printing
- [x] Lenses
- [ ] KDL Query Language (KQL)
- [ ] KDL Schema Language
- [ ] Preserve formatting when editing
<!-- - [ ] ppx_deriving_kdl -->

## Usage

Example:

```ocaml
# let kdl = Kdl.from_string {|
    contents {
      section "First section" {
        paragraph "This is the first paragraph"
        paragraph "This is the second paragraph"
      }
    }
  |}
val kdl : (Kdl.t, Kdl.error) result = Ok
 [{Kdl.name = "contents"; annot = None; args = []; props = [];
   children =
    [{Kdl.name = "section"; annot = None;
      args = [(None, `String "First section")]; props = [];
      children =
       [{Kdl.name = "paragraph"; annot = None;
         args = [(None, `String "This is the first paragraph")]; props = [];
         children = []};
        {Kdl.name = "paragraph"; annot = None;
         args = [(None, `String "This is the second paragraph")]; props = [];
         children = []}]}]}]
```

Convert to a sexp-expression (uses the `sexplib0` library):

```ocaml
# Kdl.sexp_of_t kdl |> Sexplib0.Sexp.to_string_hum |> print_endline
(contents
 (children
  (section (string "First section")
   (children (paragraph (string "This is the first paragraph"))
    (paragraph (string "This is the second paragraph"))))))
```

Pretty-print the KDL document back:

```ocaml
# Kdl.to_string kdl |> print_endline
contents {
  section "First section" {
    paragraph "This is the first paragraph"
    paragraph "This is the second paragraph"
  }
}
```

Similar to [to.ml][], ocaml-kdl provides lenses. The lenses can be composed
using `|--` or `//`:

[to.ml]: https://github.com/ocaml-toml/To.ml

```ocaml
# let open Kdl.L in
  kdl.@(top // child "section" // child_nth 1 // arg 0 // string_value)
- : string option = Some "This is the second paragraph"
```

Additionaly, type-annotated values can be "interpreted":

```ocaml
# let parsed = Kdl.from_string_exn "- (u8)220"
# Kdl.interpret Kdl.L.(get_exn parsed @@ node "-" // arg 0)
- : [> Kdl.typed_value ] = `U8 220
```

For the full list of available functions, see the [mli][] file.

[mli]: src/kdl.mli
