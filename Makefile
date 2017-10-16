# Build HDT library for SWI-Prolog

HDTHOME=hdt-cpp
DLIBS=/.libs
SOBJ=	$(PACKSODIR)/hdt4pl.$(SOEXT)
CFLAGS+=-I$(HDTHOME)/libhdt/include -g -Wall
LIBDIR=$(HDTHOME)/libhdt$(DLIBS)
LIBS=	-L$(LIBDIR) -lhdt
OBJ=	c/hdt4pl.o
LD=g++

all:	$(SOBJ)

$(SOBJ): $(OBJ)
	mkdir -p $(PACKSODIR)
	$(LD) $(ARCH) $(LDSOFLAGS) -o $@ $< $(LIBS) $(SWISOLIB) -lserd-0

c/hdt4pl.o: c/hdt4pl.cpp $(LIBDIR)/libhdt.a
	$(CC) $(ARCH) $(CFLAGS) -c -o $@ c/hdt4pl.cpp

$(LIBDIR)/libhdt.a: $(HDTHOME)/Makefile
	$(MAKE) -C $(HDTHOME)

$(HDTHOME)/Makefile.am:
	git submodule update --init

$(HDTHOME)/Makefile.in: $(HDTHOME)/Makefile.am
	(cd $(HDTHOME) && ./autogen.sh)

$(HDTHOME)/Makefile: $(HDTHOME)/Makefile.in
	(cd $(HDTHOME) && ./configure --with-pic --disable-shared)

check::
install::
clean:
	[ ! -f $(HDTHOME)/Makefile ] || $(MAKE) -C $(HDTHOME) clean

distclean:
	[ ! -f $(HDTHOME)/Makefile ] || $(MAKE) -C $(HDTHOME) distclean
	rm -f $(SOBJ)
