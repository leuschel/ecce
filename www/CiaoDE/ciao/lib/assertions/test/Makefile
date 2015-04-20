
SYSTEM=/home/clip/Systems/ciao
CIAOC=$(SYSTEM)/ciaoc/ciaoc
FILEINFO=$(SYSTEM)/etc/fileinfo
LIBS=$(SYSTEM)/lib $(SYSTEM)/library
# MAIN=main
MAIN=arith
# MAIN=/home/clip/Systems/ciao/lib/aggregates.pl
# MAIN=/home/clip/Systems/ciao/lib/engine/arithmetic.pl
# MAIN=/home/clip/Systems/ciaopp-0.6/examples/non-failure/reverse.pl
# MAIN=basic_props_test.pl
# MAIN=basic_props.pl

asr:
	$(FILEINFO) -asr $(MAIN).asr

allassertions:
	$(FILEINFO) -a $(MAIN).pl $(LIBS)

assertions:
	$(FILEINFO) -m -a $(MAIN).pl $(LIBS)

verbose:
	$(FILEINFO) -v -m -a $(MAIN).pl $(LIBS)

errors:
	$(FILEINFO) -m -e $(MAIN).pl $(LIBS)

allcode:
	$(FILEINFO) -c $(MAIN).pl $(LIBS)

code:
	$(FILEINFO) -m -c $(MAIN).pl $(LIBS)

compile:
	$(CIAOC) $(FILEINFO)

clean:
	'rm' -f *.itf *.po *.asr

distclean: clean
