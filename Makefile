# configurations

MAINS=llstar
LFLAGS=-lflags -cc,g++,-cclib,-lffi
CFLAGS=
OCAMLBUILD=ocamlbuild -use-ocamlfind -cflag -annot -yaccflag -v $(LFLAGS) $(CFLAGS)
CPLN=corestar/scripts/_build/cpln.byte

SHELL=/bin/bash

build: native

bin:
	mkdir bin

native byte d.byte p.native: bin
	@$(MAKE) -C corestar/scripts byte
	$(OCAMLBUILD) $(addsuffix .$@,$(MAINS))
	mkdir -p bin
	for f in $(MAINS); do $(CPLN) $$f.$@ bin/$$f; rm $$f.$@; done

all: build

clean:
	ocamlbuild -clean
	rm -f lib/*.a lib/*.cmxa lib/*.cmxs bin/* *.subdirs

mrproper: clean
	$(MAKE) -C examples clean

test: build
	$(MAKE) -C examples


.PHONY: build clean mrproper test native byte

-include .install.mk

#vim:noet:
