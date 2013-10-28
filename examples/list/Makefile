LLVM_AS=llvm-as-3.3

LSTAR_OPTIONS=-a ../../rules/lseg.abs
SRC=smaug.ll list_append.c list_dispose.c list_reverse.c list_traverse.c
WEBEXAMPLES=list_append.c.O0.ll list_append.c.O2.ll list_dispose.c.O0.ll \
            list_dispose.c.O2.ll list_reverse.c.O0.ll list_reverse.c.O2.ll \
            list_traverse.c.O0.ll list_traverse.c.O2.ll
BITCODE=$(SRC:=.bc)
TESTS=$(SRC:=.test)

%.ll.bc: %.ll
	$(LLVM_AS) -f -o $@ $?

%.c.bc: %.c
	clang -emit-llvm -g -O0 -c -o $@ $?

%.c.O0.ll: %.c
	clang -emit-llvm -O0 -S -o $@ $?

%.c.O2.ll: %.c
	clang -emit-llvm -O2 -S -o $@ $?

%.test: %.bc
	../../bin/llstar $(LSTAR_OPTIONS) $?

test: $(TESTS)

webtests: $(WEBEXAMPLES)

# warning: will remove all .bc files, better not have one as a source!
clean:
	rm -rf _llstar $(BITCODE) $(WEBEXAMPLES)

.PHONY: test clean
