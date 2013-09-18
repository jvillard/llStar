LLVM_AS=llvm-as-3.2

LSTAR_OPTIONS=
SRC=structs.ll slow_mofo.c norbert.ll
BITCODE=$(SRC:=.bc)

%.ll.bc: %.ll
	$(LLVM_AS) -f -o $@ $?

%.c.bc: %.c
	clang -emit-llvm -O0 -g -c -o $@ $?

%.test: %.bc
	../../bin/lstar $(LSTAR_OPTIONS) $?

test: $(BITCODE)
	for i in $(BITCODE); do ../../bin/lstar $(LSTAR_OPTIONS) $$i; done

# warning: will remove all .bc files, better not have one as a source!
clean:
	rm -rf _lstar $(BITCODE)

.PHONY: test
