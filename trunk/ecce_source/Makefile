all: sav sicsexe

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

sav:
	@echo "Creating sav file..."
	sicstus -l ecce_sicstus.pl --goal "save_program('ecce_sicstus.sav'),halt."
	@echo "...done!"
	@echo 

sicsexe:
	@echo "Creating Sicstus executable..."
	spld --static --output ecce --resources=./ecce_sicstus.sav=/ecce_sicstus.sav
	@echo "...done!"
	@echo 	