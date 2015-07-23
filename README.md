# ghc-prof-flamegraph

[![Build Status](https://travis-ci.org/fpco/ghc-prof-flamegraph.svg)](https://travis-ci.org/fpco/ghc-prof-flamegraph)

This is a small tool to convert GHC time profiling reports into a format
understandable by the
[FlameGraph](https://github.com/brendangregg/FlameGraph) tool.

## Install

    cabal install

## Usage

First convert a `.prof` file into the flame graph format using
`ghc-prof-flamegraph`:

    $ cat ~/src/packdeps/packdeps.prof | ghc-prof-flamegraph > packdeps.prof.folded

Then you can use the file to produce an svg image, using the
[`flamegraph.pl`](https://github.com/brendangregg/FlameGraph) script:

    $ cat packdeps.prof.folded | ~/src/FlameGraph/flamegraph.pl > packdeps.prof.svg

You can also generate a flamegraph using the allocation measurements,
using the `--alloc` flag.
