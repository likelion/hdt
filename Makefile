# Build HDT library for SWI-Prolog

HDTHOME=hdt-cpp
DLIBS=/.libs
SOBJ=	$(PACKSODIR)/hdt4pl.$(SOEXT)
CFLAGS+=-I$(HDTHOME)/libhdt/include -Iserd/serd -g -Wall
LIBDIR=$(HDTHOME)/libhdt$(DLIBS)
LIBHDT=$(LIBDIR)/libhdt.a
LIBSERD=serd/build/libserd-0.a
LIBS=	-L$(LIBDIR) -lhdt $(LIBSERD)
SERDHOME=$(shell pwd)/serd
OBJ=	c/hdt4pl.o
LD=g++

all:	$(SOBJ)

$(SOBJ): $(OBJ)
	mkdir -p $(PACKSODIR)
	$(LD) $(ARCH) $(LDSOFLAGS) -o $@ $< $(LIBS) $(SWISOLIB)

c/hdt4pl.o: c/hdt4pl.cpp $(LIBHDT) $(LIBSERD)
	$(CC) $(ARCH) $(CFLAGS) -c -o $@ c/hdt4pl.cpp

$(LIBHDT): $(HDTHOME)/Makefile
	$(MAKE) -j 2 -C $(HDTHOME)

$(HDTHOME)/Makefile.am:
	git submodule update --init $(HDTHOME)

$(HDTHOME)/Makefile.in: $(HDTHOME)/Makefile.am
	(cd $(HDTHOME) && ./autogen.sh)

$(HDTHOME)/Makefile: $(HDTHOME)/Makefile.in
	echo $(SERDHOME)
	(cd $(HDTHOME) && SERD_LIBS="-L$(SERDHOME)/build -lserd-0" SERD_CFLAGS=-I$(SERDHOME) \
	 ./configure --with-pic --disable-shared)

$(LIBSERD): serd/build/serd_config.h
	(cd serd && ./waf build)

serd/build/serd_config.h:
	git submodule update --init serd
	(cd serd && ./waf configure --no-shared --static)

check::
install::
clean:
	[ ! -f $(HDTHOME)/Makefile ] || $(MAKE) -C $(HDTHOME) clean

distclean:
	[ ! -f $(HDTHOME)/Makefile ] || $(MAKE) -C $(HDTHOME) distclean
	rm -f $(SOBJ)
