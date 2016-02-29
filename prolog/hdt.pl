/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2016, VU University Amsterdam
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

:- module(hdt,
	  [ hdt_open/2,			% -HDT, +Path
	    hdt_open/3,			% -HDT, +Path, +Options
	    hdt_close/1,		% +HDT
	    hdt_search/4,		% +HDT, ?S,?P,?O

	    hdt_subject/2,		% +HDT, -Subject
	    hdt_predicate/2,		% +HDT, -Predicate
	    hdt_shared/2,		% +HDT, -Shared
	    hdt_object/2,		% +HDT, -Object

	    hdt_property/2,		% +HTD, -Property

	    hdt_string_id/4,		% +HDT, +Role, ?String, ?Id

	    op(110, xfx, @),		% must be above .
	    op(650, xfx, ^^)		% must be above :
	  ]).

:- use_foreign_library(foreign(hdt4pl)).

/** <module> Access HDT (Header Dictionary Triples) files
*/

%%	hdt_open(-HDT, +File) is det.
%%	hdt_open(-HDT, +File, +Options) is det.
%
%	Open an existing HDT file and unify HDT with a handle to it. The
%	handle is an opaque symbol  that   is  subject to (atom) garbage
%	collection.  Options:
%
%	  - access(+Access)
%	  How the file is accessed. On of `map` (map the file
%	  into memory, default) or `load` (load the content of the
%	  file).
%	  - indexed(+Boolean)
%	  Whether an index is created. Default is `true`. Such an index
%	  is needed for partially instantiated calls to hdt_search/4.
%	  The index is maintained in a file with extension `.index`
%	  in the same directory as the HDT file.  An index is not needed
%	  if you only want to extract _all_ triples.

hdt_open(HDT, File) :-
	hdt_open(HDT, File, []).

%%	hdt_subject(+HDT, -Subject) is nondet.

hdt_subject(HDT, Subject) :-
	hdt_column_(HDT, subject, Var),
	Var = Subject.

hdt_predicate(HDT, Subject) :-
	hdt_column_(HDT, predicate, Var),
	Var = Subject.

hdt_shared(HDT, Subject) :-
	hdt_column_(HDT, shared, Var),
	Var = Subject.

hdt_object(HDT, Subject) :-
	hdt_object_(HDT, Var),
	Var = Subject.

hdt_property(HDT, Property) :-
	hdt_property(Property),
	hdt_property_(HDT, Property).

hdt_property(mapping(_)).
hdt_property(max_id(_)).
hdt_property(max_object_id(_)).
hdt_property(max_predicate_id(_)).
hdt_property(max_subject_id(_)).
hdt_property(objects(_)).
hdt_property(predicates(_)).
hdt_property(shared(_)).
hdt_property(subjects(_)).
hdt_property(elements(_)).


		 /*******************************
		 *	    IDENTIFIERS		*
		 *******************************/

%%	hdt_string_id(+HDT, +Role, ?String:atom, ?Id:integer) is semidet.
%
%	True if String is mapped to Id in   the given role. Fails if the
%	requested String or Id is not known for the given role in HDT.
%
%	@arg Role is one of `subject`, `predicate` or `object`


		 /*******************************
		 *	      MESSAGES		*
		 *******************************/

:- multifile prolog:error_message//1.

prolog:error_message(hdt_error(Message)) -->
	[ 'HDT: ~w'-[Message] ].
