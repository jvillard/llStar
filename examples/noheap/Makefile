LLSRC=$(wildcard *.ll)
CSRC=$(wildcard *.c)
TESTS=$(LLSRC:=.test) $(CSRC:=.test)

LLVM_AS=llvm-as-3.4
CC=clang -O0
LLSTAR=../../bin/llstar -v -v -v

WEBEXAMPLES=skip.c.O0.ll

all: test

%.ll.bc: %.ll
	$(LLVM_AS) -f -o $@ $?

%.c.bc: %.c
	clang -emit-llvm -g -O0 -c -o $@ $?

%.c.O0.ll: %.c
	clang -emit-llvm -O0 -S -o $@ $?

$(TESTS): %.test: %.bc
	@rm -rf $*.out && mkdir $*.out
	@echo "$(LLSTAR) $?"
	@cd $*.out;\
	  OCAMLRUNPARAM=pb \
	    ../$(LLSTAR) ../$? > stdout 2> stderr \
	  || echo "  $* gives exit code $$?"

test: $(TESTS)

webtests: $(WEBEXAMPLES)

clean:
	rm -rf *.out

.PHONY: all test $(TESTS) clean
