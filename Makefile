ECCE=ecce_source/ecce_cli
test: $(ECCE)
	$(ECCE) ecce_examples/mymember.pl -pe "main(T)" -dot ~/Desktop/out.dot
$(ECCE):
	make -C ecce_source/