.PHONY: dune-build
dune-build:
	dune build @install

# Requires re2c >= 4.1 to be installed
src/lexer.ml: src/lexer.ml.re
	re2ocaml -W -Wno-nondeterministic-tags --no-generation-date \
		--conditions -i $< -o $@

# Note: build should be used for development, not for packaging
.PHONY: build
build: src/lexer.ml dune-build

.PHONY: test
test:
	dune runtest

.PHONY: clean
clean:
	dune clean

kdl.opam.locked: kdl.opam
	opam lock ./kdl.opam
