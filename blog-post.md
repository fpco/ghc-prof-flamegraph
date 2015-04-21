# Flame graphs for GHC time profiles

GHC comes with nice
[profiling facilities](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/profiling.html)
that are very useful to find out where CPU time is being spent in your
program. With the right flags turned on, GHC's RTS dumps a time profile in a `.prof` file your program exits, providing textual summary and detailed views of the program's runtime, broken down by cost centre.

However, in large programs these `.prof` files can become quite hard to
make sense of.  Visualizing profiling data is a common problem, and one
neat solution is to use
[flame graphs](http://www.brendangregg.com/flamegraphs.html) to get a
high-level view of where time is spent, and why it is spent there.
That's why we wrote `ghc-prof-flamegraph`, a new utility useful for turning textual `.prof` reports into a pretty picture.

![Flame graph for a run of `binary-tree.hs`.](/examples/binary-trees.svg)

## Installation

To install, clone the reposistory and then issue `cabal install`:

    $ git clone https://github.com/fpco/ghc-prof-flamegraph.git
    $ cd ghc-prof-flamegraph
    $ cabal install

We'll also need the
[FlameGraph](https://github.com/brendangregg/FlameGraph) scripts to
produce SVG files.  I will assume that the `flamegraph.pl` script is in
the `$PATH`, but it can also be called from some other location.

## Usage

(Example taken from from
<http://jaspervdj.be/posts/2014-02-25-profiteur-ghc-prof-visualiser.html>)

Using [an example](/examples/binary-trees.hs) from the
[Haskell wiki](https://wiki.haskell.org/Shootout/Binary_trees), we first
compile it using profiling options:

    $ ghc --make -auto-all -prof -rtsopts binary-trees.hs
    [1 of 1] Compiling Main             ( binary-trees.hs, binary-trees.o )
    Linking binary-trees ...

Then we run it enabling time profiling:

    $ ./binary-trees 15 +RTS -p -RTS
    stretch tree of depth 16	 check: -1
    65536	 trees of depth 4	 check: -65536
    16384	 trees of depth 6	 check: -16384
    4096	 trees of depth 8	 check: -4096
    1024	 trees of depth 10	 check: -1024
    256	 trees of depth 12	 check: -256
    64	 trees of depth 14	 check: -64
    long lived tree of depth 15	 check: -1

Which will generates `binary-trees.prof`.  Now we can use
`ghc-prof-flamegraph` to convert it into a format understandable by
`flamegraph.pl`:

    $ cat binary-trees.prof | ghc-prof-flamegraph > binary-trees.folded

and finaly use `flamegraph.pl` to convert it to an interactive SVG
image:

    $ cat binary-trees.folded | flamegraph.pl > binary-trees.svg

The result is shown at the beginning of the post.

We have included two othe flame graphs resulting from running larger
applications in the `examples` directory: one resulting from querying
[packdeps](https://github.com/snoyberg/packdeps) for reverse
dependencies, and one resulting from asking
[hoogle](https://github.com/snoyberg/packdeps) to generate the database.

![Flamegraph for `packdeps`](/examples/packdeps.svg)
