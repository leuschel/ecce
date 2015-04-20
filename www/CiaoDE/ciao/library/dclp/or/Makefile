build:
	cd remote; make build
	cd remote_R; make build

nameserver:
	ciaoc -s -a 'actmods/webserver_publish' nameserver/nameserver
	nameserver/nameserver &

clean:
	cd examples; make clean
	cd nameserver; make clean
	cd remote; make clean
	cd remote_R; make clean
	/bin/rm -f *.po *.itf *.dep *~ *.addr \
	tmpciao*

distclean: clean
