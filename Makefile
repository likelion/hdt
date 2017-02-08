# skeleton Makefile for cross compiling windows for SWI
# alot of the work is done by a prolog script that
# uses includes and R dlls in my disk space and includes those ala
# buildenv.sh (on the fly)
#

HDTHOME=/home/janw/3rdparty/hdt-cpp/hdt-lib
SOBJ=	$(PACKSODIR)/hdt4pl.$(SOEXT)
CFLAGS+=-I$(HDTHOME)/include -g
LIBS=	-L$(HDTHOME) -lhdt
OBJ=	c/hdt4pl.o
LD=g++

all:	$(SOBJ)

$(SOBJ): $(OBJ)
	mkdir -p $(PACKSODIR)
	$(LD) $(ARCH) $(LDSOFLAGS) -o $@ $< $(LIBS) $(SWISOLIB)

c/hdt4pl.o: c/hdt4pl.cpp
	$(CC) $(ARCH) $(CFLAGS) -c -o $@ c/hdt4pl.cpp

check::
install::
clean:
	rm -f $(OBJ)
distclean: clean
	rm -f $(SOBJ)
