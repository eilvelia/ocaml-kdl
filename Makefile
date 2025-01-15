.PHONY: dune-build
dune-build:
	dune build @install

# Requires re2c >= 4.0.2-4fedb068c14c6a7aca023080ab90881af6688ce0 to be installed
src/lexer.ml: src/lexer.ml.re
	re2ocaml -W -Wno-nondeterministic-tags --no-generation-date \
		--conditions -i $< -o $@
	sed -i "s/'\\\\f'/'\\\\x0C'/g" src/lexer.ml
	sed -i "s/'\\\\a'/'\\\\x07'/g" src/lexer.ml
	sed -i "s/'\\\\v'/'\\\\x0B'/g" src/lexer.ml

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
