# Compyle
Compiled Python for COMS W4115 Fall 2022

TA: Weicheng Zhao (wz2578)
Office Hours: Friday 9:30 - 11:30 AM ET
Location: [Zoom](https://columbiauniversity.zoom.us/j/93494146149)

### Build the scanner
```
ocamlbuild test.native
```

### Run the scanner
```
./test.native
```

Recall that `Ctrl+D` sends the EOF command to std in.

If you want to pass the contents of a file to the scanner, you can do `cat myfile.cmpy | ./test.native`

## Project Contribution Workflow
We use the [Feature Branch Workflow](https://www.atlassian.com/git/tutorials/comparing-workflows/feature-branch-workflow) in this repository.
In short summary, this is how to contribute:
1. Create your feature branch off of `main` (`git checkout -b my-feature-branch`)
2. Commit your work to your feature branch (`git add . && git commit -m "implemented this feature"`).
3. Push the branch up to github (`git push -u origin my-feature-branch`).
4. From the GitHub UI, create a pull request (PR) of `my-feature-branch` into `main`.
5. Wait for an approval
6. On approval, merge your branch into `main`.
