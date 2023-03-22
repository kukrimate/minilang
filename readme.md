# minilang

This is a small, dynamically typed scripting language I have designed and implemented as a challange in less than 6 days in December 2022.

There are some example programs in the `examples` directory, although it is not meant for pratical use.

It combines Python style semantics with a C-like curly braced syntax.


The language has strings, arbitrary precision integers, classes, and lexically scoped functions.

The implementation (written in Rust) has the following architecture:

* Lexer and parser generated using lalrpop
* Compiler generating P-code in memory
* Custom stack based virtual machine
* Mark and sweep garbage collector for memory management

## Copyright
All non-trivial files in this repository are distributed under version 2.0 (only) of the GNU GPL, and
> Copyright (C) Mate Kukri, 2022-2023.
