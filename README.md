# Header Dictionary Triples (HDT) for SWI-Prolog

This repository provides a [Prolog API](http://www.swipl-prolog.org/)
to the [Header Dictionary Triples (HDT)](http://www.rdfhdt.org/)
storage format for RDF data.  This repository uses [the C++
implementation of HDT](https://github.com/rdfhdt/hdt-cpp.git).

HDT provides a complementary apprach to SWI-Prolog's memory based RDF
store.  It allows large amounts of static background knowledge to be
accessed without enlarging the memory footprint.

This repository is organised as [a SWI-Prolog
_pack_](http://www.swi-prolog.org/pack/list).  To install it, perform
the steps below.  Installation and usage is tested on
[Ubuntu](https://www.ubuntu.com/) and
[Fedora](https://getfedora.org/).  This should work on most Unix-like
machines.  Installation on
[Windows](https://www.microsoft.com/en-us/windows/) requires more
creativity.



## Installation

1. Install a recent version of Serd:

```bash
curl -s http://download.drobilla.net/serd-0.26.0.tar.bz2 | tar -xj && \
  cd serd-0.26.0 && \
  ./waf configure && \
  ./waf && \
  sudo ./waf install;
```

2. Install Raptor2.

   On Fedora: `sudo dnf install raptor2-devel`

   On Ubuntu: `sudo apt-get install libraptor2-dev`

3. After the prerequisites are installed, the HDT library can be
   installed from within Prolog using the following command:

```bash
?- pack_install(hdt).
```



## Usage

If the installation went well, you can load the HDT library with the
following command:

```bash
?- [library(hdt)].
```



## Status

Usable, but still experimental.


# Roles

  - bnode
    Terms that are blank nodes.

  - iri
    Terms that are IRIs.

  - literal
    Terms that are literals.

  - name
    Terms that are IRIs or literals.

  - node
    Terms that appear in the subject or object position.

  - object
    Terms that appear in the object position.

  - predicate
    Terms that appear in the predicate position.

  - shared
    Terms that appear in the subject and object position.

  - sink
    Terms that only appear in the object position.

  - source
    Terms that only appear in the subject position.

  - subject
    Terms that appear in the subject position.

  - term
    Terms that appear somewhere.
