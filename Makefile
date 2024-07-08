ECCE=bin/ecce_cli
test: $(ECCE)
	$(ECCE) ecce_examples/mymember.pl -pe "main(T)" -dot ecce_out.dot
power: $(ECCE)
	$(ECCE) ecce_examples/power.pl -pe "power(Base,3,T)" -dot ecce_out.dot
	dot -Tpdf <ecce_out.dot >ecce_out.pdf
	open ecce_out.pdf
power2: $(ECCE)
	$(ECCE) ecce_examples/power.pl -pe "power(2,Exp,T)" -dot ecce_out.dot
	dot -Tpdf <ecce_out.dot >ecce_out.pdf
	open ecce_out.pdf
$(ECCE): ecce_source/*.pl ecce_source/bimtools/*.pl
	make -C ecce_source/