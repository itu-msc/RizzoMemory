# Rizzo

This repository has an OCaml frontend implementation and a C backend implementation of the Rizzo language.

## Run, Test, and Build

Using the `package.json` scripts is the easiest for running builds and tests.
The environment has `opam` but `dune` needs to be activated or run using `opam exec -- dune ...`.
`dune build` has an empty response if the build is successful.
Run the tests using `opam exec -- dune runtest` or if you use the test runner directly:
```log
Usage: test_runner.exe test [--help] [OPTION]… [NAME_REGEX] [TESTCASES]
test_runner.exe: TESTCASES argument: must be a comma-separated list of integers / integer ranges
```
