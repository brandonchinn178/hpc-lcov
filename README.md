# hpc-lcov

![CircleCI](https://img.shields.io/circleci/build/github/LeapYear/hpc-lcov)
![Hackage](https://img.shields.io/hackage/v/hpc-lcov)

Convert HPC output into `lcov.info` files that can be uploaded to coverage
services, like [Codecov](https://codecov.io).

## Quickstart

### Stack

1. Run `stack build hpc-lcov`
1. Run your test(s) with coverage, e.g. `stack test --coverage`
1. Run `stack exec hpc-lcov`
1. Upload the generated `lcov.info` file to your coverage service

### Cabal

TODO

## FAQs

### How do I convert coverage for an executable?

1. Build the executable with coverage enabled (e.g. `stack build --coverage`)
1. Run the executable
1. This should generate a `.tix` file in the current directory
1. (Optional) Merge the `.tix` file for the executable with the `.tix` file for
   the test suites.

   ```bash
   # should list the .tix file for the executable and your test suites
   $ find . -name '*.tix'

   # merge with hpc
   $ stack exec -- hpc combine my-exe.tix my-test-suite.tix --union > combined.tix
   ```
1. Run `stack exec -- hpc-lcov --file TIX_FILE`, with the path of the `.tix`
   file
