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
1. Run the following, specifying the package that builds the executable:
    ```bash
    stack exec -- hpc-lcov --file my-exe.tix --main-package my-package
    ```
1. (Optional) If you have both test and executable coverage, you'll need to
   install LCOV (e.g. `brew install lcov`) and then run

   ```bash
   lcov -a lcov1.info -a lcov2.info ... > lcov.info
   ```

Note: if your test and executable modules are named the same (e.g.
`exe/Main.hs` and `test/Main.hs`), you'll need to run and get the coverage for
each separately, then merge it. e.g.

```bash
stack build --coverage
stack exec my-exe
stack exec -- hpc-lcov --file my-exe.tix -o my-exe-lcov.info

stack clean
stack test --coverage
stack exec -- hpc-lcov -o my-test-lcov.info

lcov -a my-exe-lcov.info -a my-test-lcov.info > lcov.info
```
