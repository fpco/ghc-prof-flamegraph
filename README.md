# ghc-prof-flamegraph

How to use:

```
$ cat ~/src/packdeps/packdeps.prof | runhaskell ghc-prof-flamegraph.hs > folded-packdeps
$ cat folded-packdeps | ~/src/FlameGraph/flamegraph.pl > flame-packdeps.svg 
```
