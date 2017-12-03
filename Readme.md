circuit-synthesis
=================
This repo contains three different arithmetic circuit compilers, which are used to compile low
multiplicative degree arithmetic circuits for use in [5Gen-C](https://eprint.iacr.org/2017/826). 
These tools can also be used to generate circuits for garbled circuits.
Anyone who needs to muck about with arithmetic circuits in Haskell might find these tools useful as
well.

* `circuit-synthesis`: a Haskell DSL for constructing and optimizing arithmetic circuits.
* `scripts/c2v`: a shell script that transforms Cryptol into arithmetic circuits using `yosys`.
* `scripts/c2a`: a shell script that transforms Cryptol into arithmetic circuits using `abc`.

circuit-synthesis
-----------------

`circuit-synthesis` is our name for the Haskell DSL in this repo. 
`cxs` is the name of it in executable form (see [app/Main.hs](app/Main.hs)).
It is very good at producing low multiplicative degree circuits.
It has optimizers for reducing the degree, and tools for building, translating, composing circuits.
The main tool is the [Builder monad](src/Circuit/Builder.hs).
See [src/Examples](src/Examples) for example circuits.

Building: `build.sh` compiles a binary named `cxs` in the current directory. 
This requires a recent version of `cabal` and `ghc`.
`cxs` is good for getting information about large circuits, producing the circuits from `Examples`,
optimizing existing cirucits, and so on.

[scripts/c2v](scripts/c2v)
---------------------------------------
`c2v` and `c2a` are very similar programs, which have slightly different pipelines.
Both generate arithmetic circuits from [Cryptol](https://cryptol.net/) programs.
`c2v` goes through `yosys` which allows it to tell how expensive different gates are.
This is generally useful, since it allows flexibility in what kind of optimization we wish to do.

Dependencies:
* [saw-script](https://github.com/GaloisInc/saw-script)
* [yosys](http://www.clifford.at/yosys/)
* [abc](http://people.eecs.berkeley.edu/~alanmi/abc/)

Usage: give `c2v` a Cryptol file, which function to turn into a circuit, and it will print
an arithmetic circuit to `stdout`. For example, to create a circuit for one round of AES:
```
    % ./scripts/c2v cryptol/AES.cry aes1r > aes1r.c2v.acirc
```

`c2v` can use liberty files to produce circuits with minimal numbers of AND gates, which
is useful for garbled circuits.
See `ctv -h` for details.

[scripts/c2a](scripts/c2a)
---------------------------------------
`c2a` is the cousin of `c2v`. It is generally worse, but sometimes surprisingly better, which is why
we keep it.

Dependencies:
* [saw-script](https://github.com/GaloisInc/saw-script)
* [abc](http://people.eecs.berkeley.edu/~alanmi/abc/)

Usage:
```
    % ./scripts/c2a cryptol/AES.cry aes1r > aes1r.c2a.acirc
```

Disclaimer
==========
This is research code. I'd love to document it someday (maybe today). 
So expect mysterious stuff, especially in the examples. 
They're not really good teaching examples, 
They are from us trying to get low multiplicative degree PRFs for 5Gen-C.

That said, I'm pretty excited about how useful this library has been for people besides me.
If you need help, or have constructive suggestions to improve it, I'd be happy to talk. 

License
=======
MIT
