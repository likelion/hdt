# Header Dictionary Triples (HDT) for SWI-Prolog

This repository is work in progress to provide access to [HDT
files](http://www.rdfhdt.org/) from SWI-Prolog based on the [C++
library](https:github.com/rdfhdt/hdt-cpp.git) for these files.

HDT files form a natural addition to SWI-Prolog's memory based
RDF store to access large amounts of static background knowledge
without enlarging the memory footprint.

This repository is organised as a SWI-Prolog _pack_.  To install
it, perform the steps below.  Tested on Ubuntu Linux.  Should work
on most Unix-like machines.  Installation on Windows requires more
creativity.

  - Install the HDT library
    - clone https:github.com/rdfhdt/hdt-cpp.git
    - Edit Makefile to suit your needs.  You can keep the
      dependencies to the minimum.  You do need to add
      `-fPIC` to `FLAGS`
    - Compile the liberary using `make`
  - Clone this repository in your SWI-Prolog _pack_ directory,
    typically `~/lib/swipl/pack` and install it
    - Clone using
      ```
      git clone https://github.com/JanWielemaker/hdt4swipl hdt
      ```
    - Edit Makefile, variable `HDTHOME` to point to the location
      of your built HDT library.
    - Start SWI-Prolog and run

      ```
      ?- pack_rebuild(hdt).
      ```

If all worked out, you can now use `?- use_module(library(hdt)).`
to load the library. Please example the comments there to understand the
interface.

## Status

This is just a proof of concept. Handling literals will be based on the,
also work-in-progress, new rdf11 library. The current interface does not
support creating HDT files yet.

Ultimately, this should become a SWI-Prolog pack that can be installed
using `?- pack_install(hdt).`
