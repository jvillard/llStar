PROG_NAME=structs

LLVM_AS=llvm-as-3.1

LSTAR_OPTIONS=
SRC_FILE=$(PROG_NAME).bc

%.bc: %.c
	clang -emit-llvm -O0 -c -o $@ $?

test: $(SRC_FILE)
	../../bin/lstar $(LSTAR_OPTIONS) $(SRC_FILE)

clean:
	rm -rf _lstar $(SRC_FILE)

.PHONY: test
