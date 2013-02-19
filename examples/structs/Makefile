PROG_NAME=structs

LLVM_AS=llvm-as-3.1

LSTAR_OPTIONS=
SRC_FILE=$(PROG_NAME).bc
SPEC_FILE=$(PROG_NAME).spec
LOGIC_FILE=$(PROG_NAME).logic
ABS_FILE=/dev/null
SMT_FILE=/dev/null

%.bc: %.c
	clang -emit-llvm -c -o $@ $?

test: $(SRC_FILE)
	../../bin/lstar -l $(LOGIC_FILE) -s $(SPEC_FILE) -a $(ABS_FILE) \
	-b $(SMT_FILE) $(LSTAR_OPTIONS) $(SRC_FILE)

clean:
	rm -rf _lstar $(SRC_FILE)

.PHONY: test
