# configurations

ifndef CORESTAR_HOME
	CORESTAR_HOME=$(CURDIR)/../jstar/corestar
endif
export CORESTAR_HOME

SRC_DIR=src
CORESTAR_DIR=corestar_src
MAINS=llstar
LIBS=llvm llvm_bitreader llvm_bitwriter llvm_ipo llvm_scalar_opts llvm_target dynlink str unix
LFLAGS=-cc,g++,-I,/usr/local/lib/ocaml,-cclib,-lffi,-g
CFLAGS=-I,/usr/local/lib/ocaml,-g
OB_FLAGS=-cflags -dtypes -lflags $(LFLAGS) -cflags $(CFLAGS)

# section with stuff that shouldn't change often

SHELL=/bin/bash
CORESTAR_SUBDIRS=$(addsuffix .subdirs,$(CORESTAR_DIR))
OCAMLBUILD=ocamlbuild $(OB_FLAGS) -I $(SRC_DIR) `cat $(CORESTAR_SUBDIRS)` $(addprefix -lib ,$(LIBS))

build: native

bin:
	mkdir bin

native byte d.byte p.native: $(CORESTAR_SUBDIRS) bin
	$(OCAMLBUILD) $(addsuffix .$@,$(MAINS))
	for f in $(MAINS); do ln -sf ../`readlink $$f.$@` bin/$$f; rm $$f.$@; done

all: build

clean:
	ocamlbuild -clean
	rm -f lib/*.a lib/*.cmxa lib/*.cmxs bin/* *.subdirs

mrproper: clean
	$(MAKE) -C examples clean

%.subdirs: %
	ls -F $*/ | grep / | sed "s./.." | sed "s.^.-I $*/." > $*.subdirs

test: build
	$(MAKE) -C examples


.PHONY: build clean mrproper test native byte

-include .install.mk

#vim:noet:
