# Hyperion: Haskell-based systems benchmarking

[![Build Status](https://travis-ci.org/tweag/hyperion.svg?branch=master)](https://travis-ci.org/tweag/hyperion)

Hyperion is a DSL for writing benchmarks to measure and analyze
software performance. It is a lab for future [Criterion][criterion]
features.

## Getting started

### Build

You can build the [`micro benchmark example`](examples/micro-benchmarks.hs)
using stack:

``` shell
$ stack build
$ stack exec hyperion-micro-benchmark-example
```

### Example usage

The Hyperion DSL is a backwards compatible extension
to [Criterion][criterion]'s DSL (except for the rarely used `env`
combinator, which has a safer type). Here is an example:

``` haskell
benchmarks :: [Benchmark]
benchmarks =
    [ bench "id" (nf id ())
    , series [0,5..20] $ \n ->
        bgroup "pure-functions"
          [ bench "fact" (nf fact n)
          , bench "fib" (nf fib n)
          ]
    , series [1..4] $ \n ->
        series [1..n] $ \k ->
          bench "n choose k" $ nf (uncurry choose) (n, k)
    ]

main :: IO ()
main = defaultMain "hyperion-example-micro-benchmarks" benchmarks
```

By default Hyperion runs your benchmarks and pretty prints the
results. There are several command-line options that you can pass to
the executable, like printing the results to a JSON file or including
individual raw measurements. To see the full set of options run the
executable with `--help`:

``` shell
$ stack exec hyperion-micro-benchmark-example -- --help
Usage: hyperion-micro-benchmark-example ([--pretty] | [-j|--json PATH] |
                                        [-f|--flat PATH]) ([-l|--list] | [--run]
                                        | [--no-analyze]) [--raw]
                                        [--arg KEY:VAL] [NAME...]

Available options:
  -h,--help                Show this help text
  --pretty                 Pretty prints the measurements on stdout.
  -j,--json PATH           Where to write the json benchmarks output. Can be a
                           file name, a directory name or '-' for stdout.
  -f,--flat PATH           Where to write the json benchmarks output. Can be a
                           file name, a directory name or '-' for stdout.
  --version                Display version information
  -l,--list                List benchmark names
  --run                    Run benchmarks and analyze them (default)
  --no-analyze             Only run the benchmarks
  --raw                    Include raw measurement data in report.
  --arg KEY:VAL            Extra metadata to include in the report, in the
                           format key:value.
```
