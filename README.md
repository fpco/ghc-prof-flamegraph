# ghc-prof-events

This is a small tool to convert GHC time profiling reports into a format
understandable by the
[FlameGraph](https://github.com/brendangregg/FlameGraph) tool.

## Install

    cabal install

## Usage

First convert a `.prof` file into the flame graph format using
`ghc-prof-flamegraph`:

    $ cat ~/src/packdeps/packdeps.prof | ghc-prof-flamegraph > folded-packdeps

Then you can use the file to produce an svg image, using the
[`flamegraph.pl`](https://github.com/brendangregg/FlameGraph) script:

    $ cat folded-packdeps | ~/src/FlameGraph/flamegraph.pl > flame-packdeps.svg 
