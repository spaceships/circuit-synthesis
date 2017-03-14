Introduction
============

This document describes version 0.1 of the RAMPARTS intermediate representation.

The goal is for us to agree upon a language that RAMPARTS and other encryption
libraries can use to communicate with each other, while still permitting
optimizations to happen on both sides.  

We do not regard this representation as set in stone. We have a working
version of what is specified by this document and typechecking works with only
the inputs being annotated.[1]

We're happy to refine this to something that will help the Palisade team so
please let us know your thoughts. And of course, if you see any
mistakes/ambiguities, please let us know. Our implementation is in Haskell
and I had to do some translation from that form into EBNF, so it's possible I
messed up. :)

[1]: Currently we've assumed that arithmetic gates always produce the 'biggest'
our of all their inputs. So "+ 1 2" where 1 is an Integer and 2 is a vector of
Integers results in a Vector. We should sit down and work out the specifics of
which coercions are allowed and which aren't.

Specification Details
======================

What follows is the specification of the grammar for the intermediate
representation. Note that we use the EBNF form throughout.


Top-level
=========

A circuit is represented at the top level by the following grammar:

```
circ = version, { preamble }, ( lines )+, outputs, eof;
```

where `version` is a simple `Major.Minor` version designation.

```
version = integer, ".", integer, eol;
```

This will helps us keep track of what we mean about 'the format' as it evolves.

`eof` is the standard 'end of file' designation (similarly, we use `eol` to
mean 'end of line').

The rest of the sections will go through the other parts of the top-level
format one by one.

Preamble
========

The idea behind the `preambe` section is to allow for special directives. They
will have the form of:

```
preamble = ":", directive, eol;
```

`directive` will be extensible, with the supported directives for PALISADE. For
example we might want to specify minimum bit-widths for Integers.

Currently we are only using `directive` as follows:

```
directive = "test", (spaces, binary-num)+, eol;
```

in order to specify binary test values for the circuit.

Of course, there is not always a need for directives to the backend, which is
why this section is optional in the grammar.

Lines
=====

`lines` are where we specify the gates, their inputes, and their outputs. A
useful shorthand production is that for references:

```
ref = ( digit )+;
```

Now `line`s themselves:

```
lines = ref, spaces, gate, eol;
```

`gate`s are just the set of supported gates. They don't all have the same
format because some will special requirements

```
gate = arith-gate, spaces, ref, spaces, ref, { spaces, ref }, eol
     | "const", spaces, ( digit )+, eol
     | "input", spaces, ( digit )+, spaces, "@", spaces, type, eol
     | special-gate, eol
     ;
```

Arithmetic gates are what you would expect:

```
arith-gate = "+" | "-" | "*";
```

Constants just define an integer constant.

Inputs define the inputs to the circuit and require some form of index (i.e.
where is the input coming from) and a type.

Types are a new thing:

```
type = "[", basetype, "]"
     | basetype
     ;
```

```
basetype = "Integer" | "Rational";
```

In other words, inputs can be integers, rationals, or vectors of integers and
rationals.

Aside: If it's useful we can re-write the first alternative of `type` to be
recursive, which would allow for 'vectors of vectors of...'. Our implementation
supports this, but I wasn't sure if it was a useful concept to have on
palisade's side.


`special-gate`s are a catch-all for any fancy gates that might be introduced.
Currently we have the following:

```
special-gate = "linreg", spaces, ref, spaces, ref
             | "get-numer", spaces, ref
             | "get-denom", spaces, ref
             ;
```

`linreg` is the linear-regression gate, and `get-numer` and `get-denom` get the
numerator and denominator from a rational.

Outputs
=======

Lastly, we have a section defining the outputs of a circuit:

```
outputs = ":outputs", ( spaces, ref, [type] )+, eol;
```

There must be at least one output, specified by a reference, and each output may
be annotated with a type. For typechecking to work on these circuits you only
need types for each input and to know the types for every special gate (the
latter is known to the compiler and doesn't have to be in the IR). However,
having the types of outputs might be useful if you don't want to propagate the
type information but still need to know the types of the outputs.

Where do we go from here?
=========================

We have a few ideas of syntactic extensions that might be nice, but the main
features from our talk at UMD are here: Vectorized gates, type annotations, and
constants. Let's work on integrating this into our systems and refine this
specification as we build up our capabilities.
