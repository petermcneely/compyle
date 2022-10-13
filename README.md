# Compyle
Compiled Python for COMS W4115 Fall 2022

TA: Weicheng Zhao (wz2578)
Office Hours: Friday 9:30 - 11:30 AM ET
Location: [Zoom](https://columbiauniversity.zoom.us/j/93494146149)

### Makefile
Run `make` to clean, build, test, and check the successful test percentage. 
Review the Makefile for specific steps in this process.

### Tests
test.ml holds the tests for compyle's parser. Perhaps more tests beyond parsing will live there -- perhaps we will break them out to different files.

### CI
.github/workflows/test-runner.yml builds and runs tests on each merge to `main` and each pull request. This will ensure we are pushing
safe code.

Notice that the ocaml dependency is dockerized. You can view the image at [petermcneely/docker-ocaml](https://hub.docker.com/repository/docker/petermcneely/docker-ocaml/general). Its source code lives at [this github repository](https://github.com/petermcneely/docker-ocaml).

## Project Contribution Workflow
We use the [Feature Branch Workflow](https://www.atlassian.com/git/tutorials/comparing-workflows/feature-branch-workflow) in this repository.
In short summary, this is how to contribute:
1. Create your feature branch off of `main` (`git checkout -b my-feature-branch`)
2. Commit your work to your feature branch (`git add . && git commit -m "implemented this feature"`).
3. Push the branch up to github (`git push -u origin my-feature-branch`).
4. From the GitHub UI, create a pull request (PR) of `my-feature-branch` into `main`.
5. Wait for an approval
6. On approval, merge your branch into `main`.
