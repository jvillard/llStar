LLVM_AS=llvm-as-3.4

LSTAR_OPTIONS=-runopts false #-log smt
SRC=ir_test.ll floats.ll phi.ll
WEBEXAMPLES=
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

.PHONY: test
