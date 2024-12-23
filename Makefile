.PHONY: default
default: build

.PHONY: generate-re2c
generate-re2c:
	re2ocaml -Wsentinel-in-midrule --no-generation-date -i src/re2c_lexer.ml.re2 -o src/re2c_lexer.ml
	sed -i "s/'\\\\f'/'\\\\x0C'/g" src/re2c_lexer.ml
	sed -i "s/'\\\\a'/'\\\\x07'/g" src/re2c_lexer.ml
	sed -i "s/'\\\\v'/'\\\\x0B'/g" src/re2c_lexer.ml

.PHONY: build
build:
	dune build @install

.PHONY: test
test:
	dune runtest

.PHONY: clean
clean:
	dune clean

.PHONY: lock
lock:
	opam lock ./kdl.opam
