PROG_NAME=structs

LLVM_AS=llvm-as-3.1
LLVM_DIS=llvm-dis-3.1

LSTAR_OPTIONS=
SRC_FILE=$(PROG_NAME).bc

%.bc: %.c
	clang -emit-llvm -g -O0 -c -o $@ $?

%.ll: %.bc
	$(LLVM_DIS) $?

test: $(SRC_FILE)
	../../bin/lstar $(LSTAR_OPTIONS) $(SRC_FILE)

clean:
	rm -rf _lstar $(SRC_FILE)

.PHONY: test
