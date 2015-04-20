CIAOHOME = /home/clip/Systems/ciao
SICSTUS = sicstus3
CIAO = ciao

all: ciaolips sicstuslips

ciaolips:
	echo "use_module(benchmarking).\n \
	lips(ciao).\n halt.\n" | $(CIAO)

sicstuslips:
	echo "prolog_flag(compiling,_,compactcode), \
	assert(library_directory( \
	  '$(CIAOHOME)/compatibility/$(SICSTUS)')),\
	assert(ciao_library_directory('$(CIAOHOME)/lib')), \
	assert(compat_library_directory('$(CIAOHOME)/compatibility')), \
	compile(user:library(user)), \
	compile(library(builtin)), \
	compile(benchmarking), \
	lips(sicstus), halt." | $(SICSTUS)
