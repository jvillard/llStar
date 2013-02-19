PROG_NAME=smaug

LLVM_AS=llvm-as-3.1

LSTAR_OPTIONS=-lists -d psc -abduct -abduct_file ../../logic/abduct.logic
SPEC_FILE=/dev/null
#LSTAR_OPTIONS=-lists -d psc
#SPEC_FILE=$(PROG_NAME).spec
SRC_FILE=$(PROG_NAME).bc
LOGIC_FILE=$(PROG_NAME).logic
ABS_FILE=../../abs/lseg.abs
SMT_FILE=/dev/null

%.bc: %.ll
	$(LLVM_AS) -f -o $@ $?

test: $(SRC_FILE)
	../../bin/lstar -l $(LOGIC_FILE) -s $(SPEC_FILE) -a $(ABS_FILE) \
	-b $(SMT_FILE) $(LSTAR_OPTIONS) $(SRC_FILE)

clean:
	rm -rf _lstar $(SRC_FILE)

.PHONY: test
