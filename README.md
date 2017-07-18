# ghc-prof-flamegraph

[![Build Status](https://travis-ci.org/fpco/ghc-prof-flamegraph.svg)](https://travis-ci.org/fpco/ghc-prof-flamegraph)

This is a small tool to convert GHC time profiling reports into a format
understandable by the
[FlameGraph](https://github.com/brendangregg/FlameGraph) tool.

## Install

    cabal install

## Usage

First convert a `.prof` file into the flame graph svg:

    $ cat ~/src/packdeps/packdeps.prof | ghc-prof-flamegraph > packdeps.prof.svg

Or, alternatively, just pass the `.prof` file as an argument. The tool will
then create corresponing `.svg` file:

    $ ghc-prof-flamegraph ~/src/packdeps/packdeps.prof
    Output written to ~/src/packdeps/packdeps.svg

The previous command will produce `~/src/packdeps/packdeps.svg` file.

You can customize the behavior of the underlying `flamegraph.pl` by passing
options via `–framegraph-option`. For example, you can customize the title:

    $ ghc-prof-flamegraph ~/src/packdeps/packdeps.prof '--flamegraph-option=--title=Package dependencies'
    Output written to ~/src/packdeps/packdeps.svg

You can also generate a flamegraph using the allocation measurements,
using the `--alloc` flag.
