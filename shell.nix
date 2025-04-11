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

    (re2c.overrideAttrs rec {
      version = "4.1";
      src = fetchFromGitHub {
        owner = "skvadrik";
        repo = "re2c";
        tag = version;
        hash = "sha256-xB4oH0QS0VKTK2we+wdylS8VBijpp6tv7YV7fIX1s4A=";
      };
    })
  ];
}
