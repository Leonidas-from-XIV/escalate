Escalate
========

Pure OCaml data compression.

[![Build Status](https://travis-ci.org/Leonidas-from-XIV/escalate.svg?branch=master)](https://travis-ci.org/Leonidas-from-XIV/escalate)

Rationale
---------

`zlib` is a de-facto key library of the internet, as the DEFLATE compression
and its container formats ZLIB and GZIP are ubiquitous. This library tries to
supply a type-safe, memory safe alternative to `zlib`.


Format support
--------------

Parsing deflated files is currently in the works.

| Inflate support | partly |
|-----------------|--------|
| Uncompressed    | yes    |
| Fixed Huffman   | yes    |
| Dynamic Huffman | no     |

Compressing is not yet implemented.

| Deflate support | no     |
|-----------------|--------|
| Uncompressed    | no     |
| Fixed Huffman   | no     |
| Dynamic Huffman | no     |

DEFLATE streams can be part of a number of containers. The most common ones are
planned to be implemented.

| Container support | partly |
|-------------------|--------|
| ZLIB              | yes    |
| GZIP              | no     |
| ZIP               | no     |


Installation
------------

You want to try it out? Cool, here are the the build instructions. At best you
get your OCaml via [OPAM](https://opam.ocaml.org/). Eventually, once this
library matures, it will also be available from OPAM directly!

If you want to build the code from the Git repository, you should follow my
footsteps and use these commands:

```sh
opam install oasis bitstring ounit
oasis setup
ocaml setup.ml -configure
ocaml setup.ml -build
```

This will get you the compiled binaries.


License
-------

The license of `escalate` is the same as of `zlib`, the so-called zlib license,
for convenience reasons. If you can use `zlib` in your program, you can use
`escalate`.
