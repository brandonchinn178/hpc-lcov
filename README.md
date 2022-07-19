# hpc-lcov

[![codecov](https://codecov.io/gh/LeapYear/hpc-lcov/branch/master/graph/badge.svg?token=8TErU2ntw9)](https://codecov.io/gh/LeapYear/hpc-lcov)
![CircleCI](https://img.shields.io/circleci/build/github/LeapYear/hpc-lcov)
![Hackage](https://img.shields.io/hackage/v/hpc-lcov)

Convert HPC output into `lcov.info` files that can be uploaded to coverage
services, like [Codecov](https://codecov.io).

## Quickstart

### Stack

1. Run `stack install hpc-lcov`
1. Run your test(s) with coverage, e.g. `stack test --coverage`
1. Run `hpc-lcov`
1. Upload the generated `lcov.info` file to your coverage service

### Cabal

Coming soon! (https://github.com/LeapYear/hpc-lcov/issues/3)

## FAQs

### How do I convert coverage for an executable?

Note: If you have both tests and executables, HPC will write module information to the same file. Because of this, you'll have to load the coverage for each separately, with a `stack clean` in between.

1. Build a single executable with coverage enabled (e.g. `stack build :my-exe --coverage`)
1. Run the executable
1. This should generate a `.tix` file in the current directory
1. Run the following, specifying the package that builds the executable:
    ```bash
    stack exec -- hpc-lcov --file my-exe.tix --main-package my-package
    ```

### How do I merge coverage files?

1. Install LCOV (e.g. `brew install lcov`)
1. Run

    ```bash
    lcov -a lcov1.info -a lcov2.info ... > lcov.info
    ```

## Resources

* [LCOV format description](http://ltp.sourceforge.net/coverage/lcov/geninfo.1.php)
* [More info on HPC](https://wiki.haskell.org/Haskell_program_coverage)
