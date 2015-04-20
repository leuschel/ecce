include ../../../SETTINGS

all:
	-javadoc  -sourcepath ../ -d ./ -version -author -overview ../CiaoJava/package.html CiaoJava
	-chmod --quiet $(DATAMODE) *.html *.css
	-chmod --quiet $(EXECMODE) CiaoJava
	-chmod --quiet $(DATAMODE) CiaoJava/*.html

clean distclean:
	rm -f -r *.html
	rm -f package-list
	rm -f *.css
	rm -f -r CiaoJava
