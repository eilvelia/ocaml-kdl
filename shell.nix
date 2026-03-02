{ pkgs ? import <nixpkgs> { } }:
pkgs.mkShell {
  packages = (with pkgs; [
    re2c
  ]) ++ (with pkgs.ocamlPackages; [
    ocaml
    dune_3

    menhir
    menhirLib
    ocaml-lsp
    odoc
    ppx_expect
    sexplib0
    utop
    zarith
  ]);
}
