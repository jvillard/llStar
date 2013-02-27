SUBDIRS=norbert smaug 1664 structs

all_tests:
	for i in $(SUBDIRS); do $(MAKE) -C $$i test; done

clean:
	for i in $(SUBDIRS); do $(MAKE) -C $$i clean; done
