# llStar

llStar is an automatic prover for programs written in bitcode. Bitcode
is the intermediate representation of the LLVM compiler
framework. llStar attempts to prove individual functions correct with
respect to memory safety and to their specifications. In doing so, it
automatically discovers loop invariants. Specifications are given in
the formalism of separation logic by the user.

llStar is distributed under a 3-clause BSD licences (see LICENSE). It
is available from

  https://bitbucket.org/jvillard/llstar/


## Requirements and How to Get Them

llStar requires OCaml, LLVM 3.3, and an SMT solver. The requirements
are detailed below, in reverse order of exoticism. llStar has been
developped under Linux, but other environments might be viable.

The executive summary is this: install z3, LLVM 3.3 with ocaml
development libraries, and OCaml.

### SMT solver

An SMT solver is used to solve the arithmetic constraints found during
the proof process. By default, llStar assumes that z3 is installed and
accessible in your `$PATH`. You can get z3 here:

  http://z3.codeplex.com/

Alternatively, you can specify which SMT solver to use by setting the
following environment variables.

    # wherever the SMT solver is on your system
    export JSTAR_SMT_PATH=$HOME/local/bin/z3
    # these options are correct for z3
    export JSTAR_SMT_ARGUMENTS="-in -smt2"

See the coreStar documentation for more information about using a
different SMT solver. llStar has been tested using z3 versions 4.2 and
4.3.

### LLVM

LLVM is an open source compiler framework. Its intermediate
representation is bitcode, which is the language that llStar takes as
input. llStar currently builds and run against LLVM version 3.3. LLVM
is *not* included in the llStar distribution. The preferred way to get
it is from your software management, or from

  http://llvm.org/releases/download.html#3.3

You will need the following, all included in the standard LLVM
distributions:

- LLVM dynamic libraries and OCaml bindings to build and run llStar

  On Debian, for instance, they are provided by the
  `libllvm-3.3-ocaml-dev` package.

- The Clang compiler, available from the same location, to compile
  languages from the C family to bitcode.

- The DragonEgg plugin for gcc (and gcc) to compile some of the
  examples written in other languages (eg Fortran) and included in the
  llStar distribution to bitcode.

You may want to set up the "LFLAGS" and "CFLAGS" variables in the
Makefile of llStar to adjust where the LLVM OCaml libraries are on your
system. By default, they are set as such:

    LFLAGS=-cc,g++,-I,/usr/lib/ocaml/llvm-3.3,-cclib,-lffi
    CFLAGS=-I,/usr/lib/ocaml/llvm-3.3

### OCaml

OCaml is a functional language. You can get it from

  http://caml.inria.fr/download.en.html

llStar has been tested against OCaml 3.12.1.


## Build Instructions

Simply go to the llStar directory and type make. This should produce
the llStar binary at bin/llstar. Let me know if this does not work for
you! (make sure you have the LLVM OCaml bindings)


## Usage

llStar can be invoked from the command line as follows, where
`bitcode_module` is the .bc file you want to analyse.

    ./bin/llstar [options] bitcode_module

llStar will try and locate logic files associated to the module
automatically, based on the module's name. See the output of
`./bin/llstar -help` for more options.

An easy way to start is to look at the Makefiles of the examples (in
`examples/*`) included in the distribution.

You can run all the tests from the top level directory by doing

    make test

>> Happy Proving!
