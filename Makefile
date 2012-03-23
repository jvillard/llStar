# configurations

ifndef CORESTAR_HOME
	CORESTAR_HOME=$(CURDIR)/../jstar/corestar
endif
export CORESTAR_HOME

SRC_DIRS=src corestar_src
MAINS=hopstar
LIBS=llvm llvm_executionengine llvm_bitreader llvm_target dynlink str unix
LFLAGS=-cc,g++,-I,$(HOME)/local/lib/ocaml,-cclib,-lffi#,-cclib,-L/home/jvillard/local/lib
CFLAGS=-I,$(HOME)/local/lib/ocaml
OB_FLAGS=-cflags -dtypes -lflags $(LFLAGS) -cflags $(CFLAGS)

# section with stuff that shouldn't change often

SHELL=/bin/bash
SRC_SUBDIRS=$(addsuffix .subdirs,$(SRC_DIRS))
OCAMLBUILD=ocamlbuild $(OB_FLAGS) `cat $(SRC_SUBDIRS)` $(addprefix -lib ,$(LIBS))

build: native

native byte: $(SRC_SUBDIRS)
	$(OCAMLBUILD) $(addsuffix .$@,$(MAINS))
	for f in $(MAINS); do ln -sf ../`readlink $$f.$@` bin/$$f; rm $$f.$@; done

all: build

clean:
	ocamlbuild -clean
	rm -f lib/*.a lib/*.cmxa lib/*.cmxs bin/* *.subdirs
	rm -rf corestar_src # DEV

%.subdirs: %
	ls -F $*/ | grep / | sed "s./.." | sed "s.^.-I $*/." > $*.subdirs

corestar_src:
	ln -sf "$(CORESTAR_HOME)/src" corestar_src

.PHONY: build clean doc test

-include .install.mk

#vim:noet:
