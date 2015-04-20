#------------------------------------------------------------
# Makefile for database interface : client-server version
#------------------------------------------------------------

# For solaris:
#  REQLIBS=-lsocket

REQLIBS=

all : client

#------------------------------------------------------------
# client files

client : client.o
	gcc -o client $(REQLIBS) -lnsl client.o

client.o : client.c
	gcc -c client.c

#------------------------------------------------------------

clean :
	-rm -f *.o
	-rm -f client

distclean : clean

#------------------------------------------------------------
