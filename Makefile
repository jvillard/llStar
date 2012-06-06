# configurations

ifndef CORESTAR_HOME
	CORESTAR_HOME=$(CURDIR)/../jstar/corestar
endif
export CORESTAR_HOME

SRC_DIR=src
CORESTAR_DIR=corestar_src
MAINS=lstar
LIBS=llvm llvm_executionengine llvm_bitreader llvm_target dynlink str unix
LFLAGS=-cc,g++,-I,/usr/lib/ocaml/llvm-3.0,-cclib,-lffi
CFLAGS=-I,/usr/lib/ocaml/llvm-3.0
OB_FLAGS=-cflags -dtypes -lflags $(LFLAGS) -cflags $(CFLAGS)

# section with stuff that shouldn't change often

SHELL=/bin/bash
CORESTAR_SUBDIRS=$(addsuffix .subdirs,$(CORESTAR_DIR))
OCAMLBUILD=ocamlbuild $(OB_FLAGS) -I $(SRC_DIR) `cat $(CORESTAR_SUBDIRS)` $(addprefix -lib ,$(LIBS))

build: native

native byte: $(CORESTAR_SUBDIRS)
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

test:
	cd examples && ./run.sh && cd -

.PHONY: build clean test

-include .install.mk

#vim:noet:
