# Compyle
Compiled Python for COMS W4115 Fall 2022

|TA|Office Hours|Location|
|---|---|---|
|Weicheng Zhao (wz2578)|Friday 9:30 - 11:30 AM ET|[Zoom](https://columbiauniversity.zoom.us/j/93494146149)|

## Dune Dependency
This project depends on dune. To install, run `opam install dune`.

## LLVM Dependency
This project depends on llvm version 14. To install, run the following:
```
> wget -O - https://apt.llvm.org/llvm.sh > llvm.sh
> sed 's/CURRENT_LLVM_STABLE=15/CURRENT_LLVM_STABLE=14/' llvm.sh > llvm-14.sh
> chmod +x llvm-14.sh
> ./llvm-14.sh
> opam install llvm
```

## Makefile
Run `make` to clean, build, test, and check the successful test percentage. 
Review the Makefile for specific steps in this process.

To run arbitrary ComPyle code, save your program to `example.cmpy` and run `make run`. You can view the output in the `example.out` file.

## Tests
Run `make tests` to execute the tests within this repository. The tests can be found under the test/ directory.

## CI
.github/workflows/test-runner.yml builds and runs tests on each merge to `main` and each pull request. This will ensure we are pushing
safe code.

Notice that the ocaml dependency is dockerized. You can view the image at [petermcneely/docker-ocaml](https://hub.docker.com/repository/docker/petermcneely/docker-ocaml/general). Its source code lives at [this github repository](https://github.com/petermcneely/docker-ocaml).

## pre-commit hook
If you want to run tests automatically before committing your code, you can add the make command to a pre-commit hook. To do so, run the following from this working directory:
1. `touch ./.git/hooks/pre-commit`
2. `vi ./.git/hooks/pre-commit`
3. copy over the following:
```
#!/bin/sh

set -e

make
```
4. save and quit

The makefile will now run on each commit :tada:

## Project Contribution Workflow
We use the [Feature Branch Workflow](https://www.atlassian.com/git/tutorials/comparing-workflows/feature-branch-workflow) in this repository.
In short summary, this is how to contribute:
1. Create your feature branch off of `main` (`git checkout -b my-feature-branch`)
2. Commit your work to your feature branch (`git add . && git commit -m "implemented this feature"`).
3. Push the branch up to github (`git push -u origin my-feature-branch`).
4. From the GitHub UI, create a pull request (PR) of `my-feature-branch` into `main`.
5. Wait for an approval
6. On approval, merge your branch into `main`.

## Performance
A naive benchmark test illustrates how much faster ComPyle is when compared to Python.

For the following Python script:
```
n: int = 0
while n < 1000000000:
    n += 1
print(n)
```

And the following ComPyle script:
```
def main() -> int:
	n: int = 0
	while n < 1000000000:
		n += 1
	print(n)
	return 0
```

We ran `time` on both executions.

Running `time python example.py` resulted in
|type|time|
|---|---|
|real|0m50.304s|
|user|0m50.293s|
|sys|0m0.012s|

Running `dune exec compyle -- -l example.cmpy > example.out && time lli example.out` resulted in 
|type|time|
|---|---|
|real|0m1.551s|
|user|0m1.541s|
|sys|0m0.011s|