SUBDIRS=norbert smaug 1664 structs

all_tests:
	for i in $(SUBDIRS); do cd $$i; make test; cd -; done
