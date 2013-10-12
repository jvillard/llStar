LLVM_AS=llvm-as-3.2

LSTAR_OPTIONS=-a ../../rules/lseg.abs
SRC=list.cpp list_dispose.f95 list_iter.cpp
BITCODE=$(SRC:=.bc)

%.ll.bc: %.ll
	$(LLVM_AS) -f -o $@ $?

%.cpp.bc: %.cpp
	clang++ -emit-llvm -O0 -c -o $@ $?

%.f95.bc: %.f95
	gfortran-4.6 $? -S -o $@.ll -O1 -fplugin=/home/jvillard/softs/dragonegg-3.2.src/dragonegg.so -fplugin-arg-dragonegg-emit-ir
	$(LLVM_AS) -f -o $@ $@.ll

%.test: %.bc
	../../bin/llstar $(LSTAR_OPTIONS) $?

test: $(BITCODE)
	for i in $(BITCODE); do ../../bin/llstar $(LSTAR_OPTIONS) $$i; done

# warning: will remove all .bc files, better not have one as a source!
clean:
	rm -rf _llstar $(BITCODE)

.PHONY: test clean
