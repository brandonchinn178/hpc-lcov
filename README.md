# hpc-codecov

![CircleCI](https://img.shields.io/circleci/build/github/LeapYear/hpc-codecov)
![Hackage](https://img.shields.io/hackage/v/hpc-codecov)

Convert HPC output into JSON files that can be uploaded to [Codecov](https://codecov.io).

## Quickstart

### Stack

1. Run `stack build hpc-codecov`
1. Run your test(s) with coverage, e.g. `stack test --coverage`
1. Run `stack exec hpc-codecov`
1. Upload the JSON file with your desired method, e.g. using the
   [Codecov Bash uploader](https://docs.codecov.io/docs/about-the-codecov-bash-uploader)

### Cabal

TODO

## Comparison to other libraries

I tried using [`codecov-haskell`](http://hackage.haskell.org/package/codecov-haskell),
but I couldn't get it to build. I also wanted a tool that only did the conversion
because I was using the really handy Codecov Circle CI orb.

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
1. Run `stack exec -- hpc-codecov --file TIX_FILE`, with the path of the `.tix`
   file
