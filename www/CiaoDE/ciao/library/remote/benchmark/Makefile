include ../../../SETTINGS
include ../../../SHARED


all:
	$(SETLOCALCIAO) $(CIAOC) -x server
	$(SETLOCALCIAO) $(CIAOC) -x client 
	$(SETLOCALCIAO) $(CIAOC) -x client_constr

run:
	./server &
	sleep 2
	./client localhost

runconst:
	./server &
	sleep 2
	./client_constr localhost


clean:
	/bin/rm -f *.po *.itf client server client_constr

realclean distclean: clean
	/bin/rm -rf *~ tmpciao*
