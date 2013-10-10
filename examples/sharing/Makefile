LLVM_AS=llvm-as-3.1

LSTAR_OPTIONS=
SRC=mark.c
BITCODE=$(SRC:=.bc)

%.ll.bc: %.ll
	$(LLVM_AS) -f -o $@ $?

%.c.bc: %.c
	clang -emit-llvm -g -O0 -c -o $@ $?

%.test: %.bc
	../../bin/lstar $(LSTAR_OPTIONS) $?

test: $(BITCODE)
	for i in $(BITCODE); do ../../bin/lstar $(LSTAR_OPTIONS) $$i; done

# warning: will remove all .bc files, better not have one as a source!
clean:
	rm -rf _lstar $(BITCODE)

.PHONY: test
