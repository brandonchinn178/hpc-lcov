# hpc-codecov

![CircleCI](https://img.shields.io/circleci/build/github/LeapYear/hpc-codecov)
![Hackage](https://img.shields.io/hackage/v/hpc-codecov)

Convert HPC output into JSON files that can be uploaded to [Codecov](https://codecov.io).

## Quickstart

### Stack

1. Run `stack build hpc-codecov`
1. Run your test with coverage, e.g. `stack test my-project:my-test-suite --coverage`
1. Run `stack exec -- hpc-codecov my-project:my-test-suite -o my-test-suite-codecov.json`
1. Upload the JSON file with your desired method, e.g. using the
   [Codecov Bash uploader](https://docs.codecov.io/docs/about-the-codecov-bash-uploader)

### Cabal

TODO

## Comparison to other libraries

I tried using [`codecov-haskell`](http://hackage.haskell.org/package/codecov-haskell),
but I couldn't get it to build. I also wanted a tool that only did the conversion
because I was using the really handy Codecov Circle CI orb.
