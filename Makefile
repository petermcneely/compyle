all: clean tests check

clean:
	rm -f test_percentage.txt
	rm -f test.native

tests:
	ocamlbuild test.native
	./test.native

check:
	./check_test_percentage.sh