# Header Dictionary Triples (HDT) for SWI-Prolog

This  repository  is  work  in  progress   to  provide  access  to  [HDT
files](http://www.rdfhdt.org/)  from  SWI-Prolog  based    on  the  [C++
library](https://github.com/rdfhdt/hdt-cpp.git) for these files.

HDT files form a natural addition to SWI-Prolog's memory based RDF store
to access large amounts of static background knowledge without enlarging
the memory footprint.

This repository is organised as a SWI-Prolog _pack_.  To install it,
perform the steps below.  Installation and usage is tested on Ubuntu
and Fedora.  This should work on most Unix-like machines.
Installation on Windows requires more creativity though.

## Installation

1. Install dependencies

   On Mac: `brew install serd hdt`

   On Ubuntu:

   ```
   apt-get install libserd-0-0
   git clone https://github.com/rdfhdt/hdt-cpp
   cd hdt-cpp
   ./autogen.sh
   ./configure
   make
   make install
   ```

2. Install Raptor2.

   On Mac: `brew install raptor`

   On Ubuntu: `sudo apt-get install libraptor2-dev`

3. After the prerequisites are installed, the HDT library can be
   installed from within Prolog using the following command:

```bash
?- pack_install('https://github.com/likelion/hdt.git').
```

## Usage

If the installation went well, you can load the HDT library with the following command:

```bash
?- [library(hdt)].
```

## Status

Usable, but still experimental.
