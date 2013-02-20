PROG_NAME=norbert

LLVM_AS=llvm-as-3.1

LSTAR_OPTIONS=
SRC_FILE=$(PROG_NAME).bc

%.bc: %.ll
	$(LLVM_AS) -f -o $@ $?

test: $(SRC_FILE)
	../../bin/lstar $(LSTAR_OPTIONS) $(SRC_FILE)

clean:
	rm -rf _lstar $(SRC_FILE)

.PHONY: test
