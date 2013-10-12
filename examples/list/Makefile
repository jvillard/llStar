LLVM_AS=llvm-as-3.2

LSTAR_OPTIONS=-a ../../rules/lseg.abs -autolists
SRC=smaug.ll list_append.c list_dispose.c list_reverse.c list_traverse.c
BITCODE=$(SRC:=.bc)

%.ll.bc: %.ll
	$(LLVM_AS) -f -o $@ $?

%.c.bc: %.c
	clang -emit-llvm -g -O0 -c -o $@ $?

%.test: %.bc
	../../bin/llstar $(LSTAR_OPTIONS) $?

test: $(BITCODE)
	for i in $(BITCODE); do ../../bin/llstar $(LSTAR_OPTIONS) $$i; done

# warning: will remove all .bc files, better not have one as a source!
clean:
	rm -rf _llstar $(BITCODE)

.PHONY: test clean
