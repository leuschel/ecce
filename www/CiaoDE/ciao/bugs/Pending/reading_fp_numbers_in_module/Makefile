CIAO_VERSION=1.11

show_bug: 
	$(MAKE) test
	$(MAKE) clean
	$(MAKE) execute

test: clean compile execute

compile:
	ciaoc-$(CIAO_VERSION) main_module

execute:
	echo "use_module(np). g(X). n" | ciaosh-$(CIAO_VERSION)

clean:
	rm -f *.po *.itf listados
