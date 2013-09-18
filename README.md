# LStar

LStar is an automatic prover for programs written in bitcode. Bitcode
is the intermediate representation of the LLVM compiler
framework. LStar attempts to prove individual functions correct with
respect to memory safety and to their specifications. In doing so, it
automatically discovers loop invariants. Specifications are given in
the formalism of separation logic by the user.

LStar is distributed under a 3-clause BSD licences (see LICENSE). It
is available from

  http://www0.cs.ucl.ac.uk/staff/J.Villard/lstar/

and development versions are hosted by bitbucket at

  https://bitbucket.org/jvillard/lstar/


## Requirements and How to Get Them

LStar requires OCaml, LLVM 3.2, and an SMT solver. The requirements
are detailed below, in reverse order of exoticism. LStar has been
developped under Linux, but other environments might be viable.

The executive summary is this: install z3, LLVM, and OCaml.

### SMT solver

An SMT solver is used to solve the arithmetic constraints found during
the proof process. By default, LStar assumes that z3 is installed and
accessible in your `$PATH`. You can get z3 here:

  http://z3.codeplex.com/

Alternatively, you can specify which SMT solver to use by setting the
following environment variables.

    # wherever the SMT solver is on your system
    export JSTAR_SMT_PATH=$HOME/local/bin/z3
    # these options are correct for z3
    export JSTAR_SMT_ARGUMENTS="-in -smt2"

See the coreStar documentation for more information about using a
different SMT solver. LStar has been tested using z3 version 4.2.

### LLVM

LLVM is an open source compiler framework that uses bitcode as its
intermediate representation, which, incidentally, is the language that
LStar takes as input. LStar 1.0 builds and run against LLVM version
3.2. LLVM is *not* included in the LStar distribution. The preferred
way to get it is from your software management, or from

  http://llvm.org/releases/download.html#3.2

You will need the following, all included in the standard LLVM
distributions:

- LLVM dynamic libraries and OCaml bindings to build and run LStar

- LLVM building tools to compile bitcode files into the equivalent
  binary form that LStar takes as input

- Optionally, the Clang compiler, available from the same location,
  to compile C (and other languages) files to bitcode

You may want to set up the "LFLAGS" and "CFLAGS" variables in the
Makefile of LStar to adjust where the LLVM OCaml libraries are on your
system. By default, they are set as such:

    LFLAGS=-cc,g++,-I,/usr/lib/ocaml/llvm-3.2,-cclib,-lffi
    CFLAGS=-I,/usr/lib/ocaml/llvm-3.2

### OCaml

OCaml is a functional language. You can get it from

  http://caml.inria.fr/download.en.html

LStar has been tested against OCaml 3.12.1. I currently do not know
which minimal version is required to compile or run LStar.


## Build Instructions

Simply go to the LStar directory and type make. This should produce
the LStar binary at bin/lstar. Let me know if this does not work for
you!


## Usage

LStar can be invoked from the command line as follows, where
`bitcode_module` is the .bc file you want to analyse.

    ./bin/lstar [options] bitcode_module

The main options you may want to specify are as follows (see the
output of `./bin/lstar -help` for more options):

- `-s <spec_file_name>` a file that contains specifications for each
  function in the bitcode module above. Specs are given as "f:
  {pre}{post}" where "f" is the name of the function, "pre" its
  intended precondition, and "post" its intended postcondition. See
  the examples included in the distribution for examples of the syntax
  (examples/*/*.spec).

- `-l <logic_file_name>` a file containing what logical rules LStar
  should use to manipulate symbolic states. A first guess can be to
  use the default file provided in the distribution, and located at
  "logic/llvm.logic".

- `-a <abstraction_file_name>` a file containing abstraction rules
  that LStar uses to approximate the symbolic state when trying to
  find loop invariants. See the "abs/" directory for examples
  (currently, only singly-linked lists).

An easy way to start is to look at the Makefiles in the examples
included in the distribution.


## Examples Included in the Distribution

A few examples are shipped with LStar, under "examples/". Each example
comes in its own subdirectory, that contains the spec, logic,
abstraction, and source files, with the addition of a Makefile that
runs LStar with the appropriate options. See each example and its
Makefile for details.

For instance, to run LStar on the Smaug example, that traverses a
singly-linked list, simply type

    cd examples/smaug
    make

Other examples include Norbert, a program that allocates a struct node
and fills in its fields with the values passed as argument, and
"structs", that performs complex field manipulations and pointer
arithmetic. All the examples are successfully checked by LStar.

You can run all the tests from the top level directory by doing

    make test

>> Happy Proving!
