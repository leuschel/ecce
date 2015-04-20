
build: 
	ciaoc -a actmods/webbased_publish agent1
	agent1 &
	ciaoc -a actmods/webbased_publish agent2
	agent2 &
	ciaoc test

distclean:
	/bin/rm -f *.po *.itf *.dep *~ *.addr \
	tmpciao* agent1 agent2 test

clean:	distclean
	/bin/rm -rf traces/agent1 traces/agent2
