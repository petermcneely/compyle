all: clean tests check

# clean all artifacts
clean:
	rm -rf ./_build
	rm -f test.native
	rm -rf compyle.native
	rm -rf lib/parser.mli
	rm -rf lib/parser.ml
	rm -rf lib/parser.output

# clean and build compyle.
build: clean
	dune clean
	dune build

# build and run compyle. Save the output to ./example.out
run: build
	dune exec compyle -- -s example.cmpy > example.out

# shortcut for running ocamlyacc on the parser. Helpful when
# debugging and writing the parser.
yacc: clean
	ocamlyacc -v lib/parser.mly

# run tests
tests:
	dune test

# check the successful test percentage. Used in CI.
check:
	./check_test_percentage.sh
