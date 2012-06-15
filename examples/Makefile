SUBDIRS=norbert smaug

all_tests:
	for i in $(SUBDIRS); do cd $$i; make test; cd -; done
