PROG_NAME=smaug

LLVM_AS=llvm-as-3.1

LSTAR_OPTIONS=-lists -abduct -abduct_file ../../logic/abduct.logic
SPEC_FILE=/dev/null
#LSTAR_OPTIONS=-lists -d psc
SRC_FILE=$(PROG_NAME).bc
ABS_FILE=../../abs/lseg.abs

%.bc: %.ll
	$(LLVM_AS) -f -o $@ $?

test: $(SRC_FILE)
	../../bin/lstar -s $(SPEC_FILE) -a $(ABS_FILE) $(LSTAR_OPTIONS) $(SRC_FILE)

clean:
	rm -rf _lstar $(SRC_FILE)

.PHONY: test
