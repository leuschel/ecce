include ../../../SETTINGS
include ../../../SHARED


all: normal objects

normal:
	$(SETLOCALCIAO) $(CIAOC) -x server
	$(SETLOCALCIAO) $(CIAOC) -x client

objects:
	$(SETLOCALCIAO) $(CIAOC) -x object_server
	$(SETLOCALCIAO) $(CIAOC) -x object_client

run_normal:
	./server &
	sleep 2
	./client localhost

run_objects:
	./object_server &
	sleep 2
	./object_client localhost


clean:
	/bin/rm -f *.po *.itf 

realclean distclean: clean
	/bin/rm -rf *~ tmpciao* *.asr client server conc example_server object_server object_client
