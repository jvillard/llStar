LLVM_AS=llvm-as-3.3

LSTAR_OPTIONS=
SRC=field_access.ll nested.ll corner_structs.ll norbert.ll pointer_arith.c
WEBEXAMPLES=pointer_arith.c.O0.ll pointer_arith.c.O2.ll
BITCODE=$(SRC:=.bc)
TESTS=$(SRC:=.test)

%.c.bc: %.c
	clang -emit-llvm -O0 -c -o $@ $?

%.ll.bc: %.ll
	$(LLVM_AS) -f -o $@ $?

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

.PHONY: test
