SUBDIRS=noheap struct list ir array sharing lang

all_tests:
	for i in $(SUBDIRS); do $(MAKE) -C $$i test; done

clean:
	for i in $(SUBDIRS); do $(MAKE) -C $$i clean; done
