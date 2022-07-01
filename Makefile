# Build HDT library for SWI-Prolog

SOBJ=	$(PACKSODIR)/hdt4pl.$(SOEXT)
LD=g++

all:	$(SOBJ)

c/hdt4pl.o: c/hdt4pl.cpp
	$(CC) $(ARCH) $(CFLAGS) -c -o $@ c/hdt4pl.cpp

$(SOBJ): c/hdt4pl.o
	mkdir -p $(PACKSODIR)
	$(LD) $(ARCH) $(LDSOFLAGS) -o $@ $< $(SWISOLIB) -lserd-0 -lhdt

check::
install::
clean:
	rm -f c/hdt4pl.o
distclean: clean
	rm -f $(SOBJ)
