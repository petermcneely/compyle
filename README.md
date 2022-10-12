# Compyle
Compiled Python for COMS W4115 Fall 2022

### Build the scanner
```
ocamlbuild test.native
```

### Run the scanner
```
./test.native
```

Recall that Ctrl+D sends the EOF command to std in.

If you want to pass the contents of a file to the scanner, you can do `cat myfile.cmpy | ./test.native`

## Project flow
create a feature branch off of main.
commit your work to your feature branch.
push the branch up to github.
create a pull request (PR) into main.
