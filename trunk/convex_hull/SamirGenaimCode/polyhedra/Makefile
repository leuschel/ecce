all: sav polyhedra

sav: *.pl
	@echo "Creating sav file..."
	sicstus -l cli_polyhedra.pl --goal "save_program('cli_polyhedra.sav'),halt."
	@echo "...done!"
	@echo 

polyhedra: sav
	@echo "Creating Sicstus executable..."
	spld --static --output polyhedra --resources=./cli_polyhedra.sav=/cli_polyhedra.sav
	@echo "...done!"
	@echo 	