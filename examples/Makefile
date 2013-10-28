SUBDIRS=noheap struct list ir array sharing lang

webtests:
	for i in $(SUBDIRS); do $(MAKE) -C $$i webtests; done

all_tests:
	for i in $(SUBDIRS); do $(MAKE) -C $$i test; done

clean:
	for i in $(SUBDIRS); do $(MAKE) -C $$i clean; done
