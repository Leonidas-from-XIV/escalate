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


License
-------

The license of `escalate` is the same as of `zlib`, the so-called zlib license,
for convenience reasons. If you can use `zlib` in your program, you can use
`escalate`.
