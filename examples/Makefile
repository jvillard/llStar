LLSRC=$(wildcard *.ll)
CSRC=$(wildcard *.c)
TESTS=$(LLSRC:=.test) $(CSRC:=.test)
ABDUCTS=$(LLSRC:=.abduct) $(CSRC:=.abduct)

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
