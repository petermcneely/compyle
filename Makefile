all: clean tests check

# clean all artifacts
clean:
	rm -rf ./_build
	rm -f test.native
	rm -rf compyle.native
	rm -rf parser.mli
	rm -rf parser.ml
	rm -rf parser.output


# clean and build compyle.
build: clean
	ocamlbuild compyle.native

# build and run compyle. Save the output to ./example.out
run: build
	./compyle.native > ./example.out

# shortcut for running ocamlyacc on the parser. Helpful when
# debugging and writing the parser.
yacc: clean
	ocamlyacc -v parser.mly

# run tests
tests:
	ocamlbuild test.native
	./test.native

# check the successful test percentage. Used in CI.
check:
	./check_test_percentage.sh
