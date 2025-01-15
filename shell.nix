{ pkgs ? import <nixpkgs> { } }:
pkgs.mkShell {
  packages = with pkgs; [
    ocaml
    dune_3
    ocamlPackages.menhir
    ocamlPackages.menhirLib
    ocamlPackages.ocaml-lsp
    ocamlPackages.odoc
    ocamlPackages.ppx_expect
    ocamlPackages.sexplib0
    ocamlPackages.utop
    ocamlPackages.zarith

    # Use an unreleased version of re2c
    (re2c.overrideAttrs {
      src = fetchFromGitHub {
        owner = "skvadrik";
        repo = "re2c";
        rev = "4fedb068c14c6a7aca023080ab90881af6688ce0";
        hash = "sha256-NgwSUqnCbSNuYJcWvgILhEcf+N0jA4DUNs6QJ4/ORZ0=";
      };
    })
  ];
}
