LLSRC=$(wildcard *.ll)
CSRC=$(wildcard *.c)
F95SRC=$(wildcard *.f95)
TESTS=$(LLSRC:=.test) $(CSRC:=.test) $(F95SRC:=.test)
ABDUCTS=$(LLSRC:=.abduct) $(CSRC:=.abduct) $(F95SRC:=.abduct)

LLVM_AS=llvm-as
CC=clang -O2
LLSTAR=../bin/llstar -v -v -v

WEBEXAMPLES=skip.c.O0.ll

all: test

%.ll.bc: %.ll
	$(LLVM_AS) -f -o $@ $?

%.c.bc: %.c
	$(CC) -emit-llvm -g -c -o $@ $?

%.c.O0.ll: %.c
	$(CC) -emit-llvm -O0 -S -o $@ $?

%.f95.bc: %.f95
	gfortran-4.6 $? -S -g -o $@.ll -O1 -fplugin=/home/jvillard/softs/dragonegg-3.4.src/dragonegg.so -fplugin-arg-dragonegg-emit-ir
	$(LLVM_AS) -f -o $@ $@.ll

%.f95.O1.ll: %.f95
	gfortran-4.6 $? -S -o $@ -O1 -fplugin=/home/jvillard/softs/dragonegg-3.4.src/dragonegg.so -fplugin-arg-dragonegg-emit-ir

$(TESTS): %.test: %.bc
	@rm -rf $*.out && mkdir $*.out
	@echo "$(LLSTAR) $?"
	@cd $*.out;\
	  OCAMLRUNPARAM=b \
	    ../$(LLSTAR) ../$? > stdout 2> stderr \
	  || echo "  $* gives exit code $$?"

$(ABDUCTS): %.abduct: %.bc
	@rm -rf $*.out && mkdir $*.out
	@echo "$(LLSTAR) $?"
	@cd $*.out;\
	  OCAMLRUNPARAM=b \
	    ../$(LLSTAR) -abduct ../$? > stdout 2> stderr \
	  || echo "  $* gives exit code $$?"

test: $(TESTS)

webtests: $(WEBEXAMPLES)

clean:
	rm -rf *.out

.PHONY: all test $(TESTS) clean
