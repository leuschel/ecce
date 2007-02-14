all: sav convex

sav: *.pl
	@echo "Creating sav file..."
	sicstus -l cli_convex_analyser.pl --goal "save_program('cli_convex_analyser.sav'),halt."
	@echo "...done!"
	@echo 

convex: sav
	@echo "Creating Sicstus executable..."
	spld --static --output convex --resources=./cli_convex_analyser.sav=/cli_convex_analyser.sav
	@echo "...done!"
	@echo 	