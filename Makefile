ECCE=bin/ecce_cli
test: $(ECCE)
	$(ECCE) ecce_examples/mymember.pl -pe "main(T)" -dot ecce_out.dot
$(ECCE): ecce_source/*.pl ecce_source/bimtools/*.pl
	make -C ecce_source/