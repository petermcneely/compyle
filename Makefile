all: clean tests check

clean:
	rm -rf ./_build
	rm -f test.native

tests:
	ocamlbuild test.native
	./test.native

check:
	./check_test_percentage.sh