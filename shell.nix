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
        rev = "0e97d4917bcbdb8645cca2288c010b19c959278f";
        hash = "sha256-q7Y68MfUrQZDM1bKleqgOD/P3R7WJY6jjXwFLBiX0V8=";
      };
    })
  ];
}
