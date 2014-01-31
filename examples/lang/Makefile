LLVM_AS=llvm-as-3.4

LSTAR_OPTIONS=-a ../../rules/lseg.abs
SRC=list.cpp list_dispose.f95 list_iter.cpp
WEBEXAMPLES=list.cpp.O0.ll list_dispose.f95.O1.ll
BITCODE=$(SRC:=.bc)
TESTS=$(SRC:=.test)

%.ll.bc: %.ll
	$(LLVM_AS) -f -o $@ $?

%.cpp.bc: %.cpp
	clang++ -emit-llvm -O0 -c -o $@ $?

%.f95.bc: %.f95
	gfortran-4.6 $? -S -g -o $@.ll -O1 -fplugin=/home/jvillard/softs/dragonegg-3.4.src/dragonegg.so -fplugin-arg-dragonegg-emit-ir
	$(LLVM_AS) -f -o $@ $@.ll

%.f95.O1.ll: %.f95
	gfortran-4.6 $? -S -o $@ -O1 -fplugin=/home/jvillard/softs/dragonegg-3.4.src/dragonegg.so -fplugin-arg-dragonegg-emit-ir

%.cpp.O0.ll: %.cpp
	clang++ -emit-llvm -O2 -S -o $@ $?

%.test: %.bc
	../../bin/llstar $(LSTAR_OPTIONS) $?

test: $(TESTS)

webtests: $(WEBEXAMPLES)

# warning: will remove all .bc files, better not have one as a source!
clean:
	rm -rf _llstar $(BITCODE) $(WEBEXAMPLES)

.PHONY: test clean
