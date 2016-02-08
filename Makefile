# skeleton Makefile for cross compiling windows for SWI
# alot of the work is done by a prolog script that
# uses includes and R dlls in my disk space and includes those ala
# buildenv.sh (on the fly)
#
SOBJ=	$(PACKSODIR)/hdt4pl.$(SOEXT)
CFLAGS+=-I/home/janw/3rdparty/hdt-cpp/hdt-lib/include -g
LIBS=	-L/home/janw/3rdparty/hdt-cpp/hdt-lib -lhdt
LD=g++

all:	$(SOBJ)

$(SOBJ): hdt4pl.o
	mkdir -p $(PACKSODIR)
	$(LD) $(ARCH) $(LDSOFLAGS) -o $@ $< $(LIBS) $(SWISOLIB)

hdt4pl.o:
	$(CC) $(ARCH) $(CFLAGS) -c -o $@ c/hdt4pl.cpp

check::
install::
clean:
	rm -f hdt4pl.o
distclean: clean
	rm -f $(SOBJ)
