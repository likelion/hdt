# Build HDT library for SWI-Prolog

HDTHOME=hdt-cpp/hdt-lib
SOBJ=	$(PACKSODIR)/hdt4pl.$(SOEXT)
CFLAGS+=-I$(HDTHOME)/include -g
LIBS=	-L$(HDTHOME) -lhdt
OBJ=	c/hdt4pl.o
LD=g++

all:	$(SOBJ)

$(SOBJ): $(OBJ) $(HDTHOME)/libhdt.a
	mkdir -p $(PACKSODIR)
	$(LD) $(ARCH) $(LDSOFLAGS) -o $@ $< $(LIBS) $(SWISOLIB)

c/hdt4pl.o: c/hdt4pl.cpp
	$(CC) $(ARCH) $(CFLAGS) -c -o $@ c/hdt4pl.cpp

$(HDTHOME):
	git submodule update --init

$(HDTHOME)/libhdt.a:
	sed -i 's/^FLAGS=-O3/FLAGS=-fPIC -O3/' $(HDTHOME)/Makefile
	$(MAKE) -C $(HDTHOME) all

check::
install::
clean:
	rm -f $(OBJ)
	(cd $(HDTHOME) && git reset --hard)
	$(MAKE) -C $(HDTHOME) clean

distclean: clean
	rm -f $(SOBJ)
