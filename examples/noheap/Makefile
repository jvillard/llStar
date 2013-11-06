LLVM_AS=llvm-as-3.3

LSTAR_OPTIONS=-runopts false
SRC=skip.c 1664.c
WEBEXAMPLES=skip.c.O0.ll
BITCODE=$(SRC:=.bc)
TESTS=$(SRC:=.test)

%.ll.bc: %.ll
	$(LLVM_AS) -f -o $@ $?

%.c.bc: %.c
	clang -emit-llvm -g -O0 -c -o $@ $?

%.c.O0.ll: %.c
	clang -emit-llvm -O0 -S -o $@ $?

%.test: %.bc
	../../bin/llstar $(LSTAR_OPTIONS) $?

test: $(TESTS)

webtests: $(WEBEXAMPLES)

# warning: will remove all .bc files, better not have one as a source!
clean:
	rm -rf _llstar $(BITCODE) $(WEBEXAMPLES)

.PHONY: test
