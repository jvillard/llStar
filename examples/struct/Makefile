LLVM_AS=llvm-as-3.3

LSTAR_OPTIONS=
SRC=field_access.ll nested.ll corner_structs.ll norbert.ll pointer_arith.c
BITCODE=$(SRC:=.bc)
TEST=$(BITCODE:.bc=.test)

%.c.bc: %.c
	clang -emit-llvm -O0 -c -o $@ $?

%.ll.bc: %.ll
	$(LLVM_AS) -f -o $@ $?

%.test: %.bc
	../../bin/llstar $(LSTAR_OPTIONS) $?

test: $(TEST)

# warning: will remove all .bc files, better not have one as a source!
clean:
	rm -rf _llstar $(BITCODE)

.PHONY: test
