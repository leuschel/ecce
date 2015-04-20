CIAOLIBDIR=/home/clip/Systems/ciao/bin/LINUXi86
CIAOLIB=${CIAOLIBDIR}/libciao.a

HEADERS=${CIAOLIBDIR}/ciao_prolog.h

all: test engine

test: test.o test.po
	gcc -o test test.o ${CIAOLIB} -ldl -lm -lpthread

test.o: test.c ${HEADERS}
	gcc -O3 -c -Wall test.c -I${CIAOLIBDIR}

test.po: test.pl
	ciaoc -c test.pl

engine: engine.o 
	gcc -o engine engine.o ${CIAOLIB} -ldl -lm -lpthread

engine.o: engine.c ${HEADERS}
	gcc -O3 -c -Wall engine.c -I${CIAOLIBDIR}

clean distclean:
	-rm -f *~ *.o *.po engine test
