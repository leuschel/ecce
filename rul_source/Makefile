
all: sav exe

sav:
	@echo "Creating sav file..."
	sicstus -l newreg_cli.pl --goal "save_program('rul.sav'),halt."
	@echo "...done!"
	@echo 

exe:
	@echo "Creating executable..."
	spld --static --output rul.exe --resources=./rul.sav=/rul.sav
	@echo "...done!"
	@echo 	
	