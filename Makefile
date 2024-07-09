ECCE=bin/ecce_cli
RUL=bin/rul_cli
CONVEX=bin/convex_cli
test: $(ECCE) $(RUL) $(CONVEX)
	$(ECCE) ecce_examples/mymember.pl -pe "main(T)" -dot ecce_out.dot
	$(RUL)  -t 5000 ecce_examples/mymember.pl "main(T)"
	$(RUL) -b -t 5000 ecce_examples/mymember.pl
	$(CONVEX) ecce_examples/mymember.pl
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
$(CONVEX): convex_hull/*.pl
	sicstus -l convex_hull/cli_convex_analyser.pl --goal "save_program('convex_hull/cli_convex_analyser.sav'),halt."
	spld --static --output $(CONVEX) --resources=./convex_hull/cli_convex_analyser.sav=/cli_convex_analyser.sav