# Header Dictionary Triples (HDT) for SWI-Prolog

This  repository  is  work  in  progress   to  provide  access  to  [HDT
files](http://www.rdfhdt.org/)  from  SWI-Prolog  based    on  the  [C++
library](https://github.com/rdfhdt/hdt-cpp.git) for these files.

HDT files form a natural addition to SWI-Prolog's memory based RDF store
to access large amounts of static background knowledge without enlarging
the memory footprint.

This repository is organised as  a   SWI-Prolog  _pack_.  To install it,
perform the steps below. Tested on  Ubuntu   Linux.  Should work on most
Unix-like machines. Installation on Windows requires more creativity.

  1. Clone this repository in your SWI-Prolog _pack_ directory,
     typically `~/lib/swipl/pack` and install it:

    a. Clone this pack:

      ```bash
      $ git clone https://github.com/JanWielemaker/hdt4swipl hdt
      ```

    b. Install the prerequisite libraries.  On Ubuntu:

      ```
      sudo apt-get install libserd-dev libraptor-dev
      ```

    c. Start SWI-Prolog and run

      ```prolog
      ?- pack_rebuild(hdt).
      ```

If all worked out, you  can   now  use `?- use_module(library(hdt)).` to
load the library. Please example the   comments  there to understand the
interface.

## Status

Usable, but still experimental.
