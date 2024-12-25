.PHONY: default
default: build

.PHONY: generate-re2c
generate-re2c:
	re2ocaml -W --no-generation-date -i src/lexer.ml.re -o src/lexer.ml
	sed -i "s/'\\\\f'/'\\\\x0C'/g" src/lexer.ml
	sed -i "s/'\\\\a'/'\\\\x07'/g" src/lexer.ml
	sed -i "s/'\\\\v'/'\\\\x0B'/g" src/lexer.ml

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
