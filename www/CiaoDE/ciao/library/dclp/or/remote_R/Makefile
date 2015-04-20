build: 

	ciaoc -x manager
	ciaoc -x agency
	ciaoc -x -a actmods/webbased_publish binder

run:
	binder &
	sleep 2
	agency &
	sleep 2
	manager &

clean:
	/bin/rm -f *.po *.itf *.dep *~ *.addr \
	tmpciao* binder agency manager

distclean: clean
