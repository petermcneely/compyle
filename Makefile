all: clean tests check

clean:
	rm -rf ./_build
	rm -f test.native
	rm -rf compyle.native
	rm -rf parser.mli
	rm -rf parser.ml
	rm -rf parser.output

build: clean
	ocamlbuild compyle.native

run: clean build
	./compyle.native > ./example.out

yacc: clean
	ocamlyacc -v parser.mly

tests:
	ocamlbuild test.native
	./test.native

check:
	./check_test_percentage.sh
