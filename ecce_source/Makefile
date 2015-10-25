SICSTUS=sicstus
SPLD=spld
ECCECLI=../bin/ecce_cli

all: ecce_sicstus.sav $(ECCECLI)

ecce:  *.pl */*.pl
	ciaoc ecce_ciao_entry.pl 

mecce:  *.pl */*.pl
	ciaoc -o ../mecce ecce_ciao_entry.pl	

exe:  *.pl */*.pl
	ciaoc -S -o ../www/downloads/ecce ecce_ciao_entry.pl

install: ecce
	cp ecce_ciao_entry ~/bin/mecce

clean:
	find . -name "*.po"  -print -exec rm {} \;
	find . -name "*.itf" -print -exec rm {} \;
	find . -name "*.ast" -print -exec rm {} \;
	-rm ecce_ciao_entry

ecce_sicstus.sav: *.pl */*.pl
	@echo "Creating sav file..."
	${SICSTUS} -l ecce_cli.pl --goal "save_program('ecce_sicstus.sav'),halt."
	@echo "...done!"
	@echo 

$(ECCECLI): ecce_sicstus.sav
	@echo "Creating Sicstus executable..."
	${SPLD} --static --output $(ECCECLI) --resources=./ecce_sicstus.sav=/ecce_sicstus.sav
	@echo "...done!"
	@echo 	