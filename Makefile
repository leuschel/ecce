ECCE=bin/ecce_cli
RUL=bin/rul_cli
test: $(ECCE) $(RUL)
	$(ECCE) ecce_examples/mymember.pl -pe "main(T)" -dot ecce_out.dot
	$(RUL)  -t 5000 ecce_examples/mymember.pl "main(T)"
	$(RUL) -b -t 5000 ecce_examples/mymember.pl
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
$(RUL): rul_source/*.pl
	sicstus -l rul_source/newreg_cli.pl --goal "save_program('rul_source/rul.sav'),halt."
	spld --static --output $(RUL) --resources=./rul_source/rul.sav=/rul.sav