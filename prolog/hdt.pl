/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2017, VU University Amsterdam
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

:- module(hdt, [
	% HDT FILES
	    hdt_create/2,		% +RDFFile, +HDTFile
	    hdt_create/3,		% +RDFFile, +HDTFile, +Options

	    hdt_open/2,			% +File, -HDT
	    hdt_open/3,			% +File, -HDT, +Options
	    hdt_close/1,		% +HDT

	% TERM ↔ ID
	    hdt_term_id/4,		% +HDT, +Role, ?Term, ?ID

	% TRIPLES
	    hdt/4,			% +HDT, ?S,?P,?O
	    hdt_id/4,			% +HDT, ?SID,?PID,?OID

            hdt_count/5,		% +HDT, ?S,?P,?O, ?Count
	    hdt_count_id/5,		% +HDT, ?SID,?PID,?OID, ?Count

	   %hdt_rnd/4,			% +HDT, ?S,?P,?O
	   %hdt_rnd_id/4,		% +HDT, ?SID,?PID,?OID

	% TERMS
	    hdt_term/3,			% +HDT, +Role, ?Term
	   %hdt_term_id/3,		% +HDT, +Role, ?ID

	   %hdt_term_count/3,		% +HDT, +Role, ?Count

	   %hdt_term_rnd/3,		% +HDT, +Role, -Term
	   %hdt_term_rnd_id/3,		% +HDT, +Role, -ID

	% TERMS BY PREFIX
	   %hdt_term/4,			% +HDT, +Role, +Prefix, ?Term
	   %hdt_term_id/4,		% +HDT, +Role, +Prefix, ?ID

	   %hdt_term_count/4,		% +HDT, +Role, +Prefix, ?Count

	   %hdt_term_rnd/4,		% +HDT, +Role, +Prefix, -Term
	   %hdt_term_rnd_id/4,		% +HDT, +Role, +Prefix, -ID

	% OTHERS
	    hdt_header/4,		% +HDT, ?S,?P,?O
	    hdt_property/2,		% +HTD, -Property
	    op(110, xfx, @),		% must be above .
	    op(650, xfx, ^^)		% must be above :
	  ]).
:- use_module(library(semweb/rdf11)).
:- use_module(library(sgml)).
:- use_module(library(lists)).

:- use_foreign_library(foreign(hdt4pl)).

/** <module> Access HDT (Header Dictionary Triples) files
*/

:- rdf_meta
	hdt_term_id(+, +, t, ?),
	hdt(+, r, r, o),
	hdt_count(+, r, r, o, -),
	%hdt_rnd(+, r, r, o),
	hdt_term(+, +, t),
	hdt_term(+, +, +, r),
	hdt_header(+, r, r, o).



		 /*******************************
		 *	 FILE OPERATIONS	*
		 *******************************/

%%	hdt_create(+RDFFile, +HDTFile)
%%	hdt_create(+RDFFile, +HDTFile, +Options)
%
%	Create an HDT file from an uncompressed N-Triples file.
%	Options:
%
%	  * base_uri(+URI)
%	  URI is used for generating the header properties (see
%	  http_header/4.

hdt_create(RDFFile, HDTFile) :-
	hdt_create(RDFFile, HDTFile, []).


hdt_create(RDFFile, HDTFile, Options) :-
	hdt_create_from_file(HDTFile, RDFFile, Options).


%%	hdt_open(+File, -HDT) is det.
%%	hdt_open(+File, -HDT, +Options) is det.
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

hdt_open(File, HDT) :-
	hdt_open(HDT, File, []).


%%	hdt_close(+HDT) is det.
%
%	Close an HDT that was previously opened with hdt_open/[2,3].



		 /*******************************
		 *	     TERM ↔ ID		*
		 *******************************/

%%	hdt_term_id(+HDT, +Role, +Term, +ID) is semidet.
%%	hdt_term_id(+HDT, +Role, +Term, -ID) is det.
%%	hdt_term_id(+HDT, +Role, -Term, +ID) is det.
%
%	@arg Role is one of `subject`, `predicate` or `object`

hdt_term_id(HDT, subject, Term, ID) :-
	hdt_subject_id(HDT, Term, ID).
hdt_term_id(HDT, predicate, Term, ID) :-
	hdt_predicate_id(HDT, Term, ID).
hdt_term_id(HDT, object, Term, ID) :-
	hdt_object_id(HDT, Term, ID).



		 /*******************************
		 *	      TRIPLES		*
		 *******************************/

%%	hdt(+HDT, ?S,?P,?O) is nondet.
%
%	True if 〈S,P,O〉 is a translated triple in HDT.

hdt(HDT, S,P,O) :-
	pre_object(HDT, O, OHDT),
	hdt_search(HDT, content, S, P, OHDT),
	post_object(O, OHDT).


%!	hdt_id(+HDT, ?SID,?PID,?OID) is nondet.
%
%	True if 〈SID,SIP,SIO〉 is a triple in HDT.

hdt_id(HDT, SID,PID,OID) :-
	hdt_search_id(HDT, SID,PID,OID).


%%	hdt_count(HDT, ?S,?P,?O, +Count:nonneg) is semidet.
%%	hdt_count(HDT, ?S,?P,?O, -Count:nonneg) is det.
%
%	True if Count is the number of matches of the Triple Pattern
%	〈S,P,O〉 on the graph stored in HDT.

hdt_count(HDT, S,P,O, Count) :-
	Triple   = t(S,P,O),
	TripleID = t(SID,PID,OID),
	hdt_pre_triple(HDT, Triple, TripleID),
	hdt_search_cost_id(HDT, SID,PID,OID, Count), !.
hdt_count(_, _,_,_, 0).


%!	hdt_count_id(+HDT, ?SID,?PID,?OID, +Count:nonneg) is semidet.
%!	hdt_count_id(+HDT, ?SID,?PID,?OID, -Count:nonneg) is det.

hdt_count_id(HDT, SID,PID,OID, Count) :-
	hdt_search_cost_id(HDT, SID,PID,OID, Count), !.
hdt_count_id(_, _,_,_, 0).



		 /******************
		 *	TERMS	   *
		 ******************/

%%	hdt_term(+HDT, +Role, +Term) is semidet.
%%	hdt_term(+HDT, +Role, -Term) is nondet.
%
%	@arg Term is either of the following term types:
%	- bnode
%	- iri
%	- literal
%	- name
%	or either of the following term positions:
%	- node
%	- object
%	- predicate
%	- shared
%	- subject

% name
hdt_term(HDT, name, Name) :-
	hdt_term(HDT, iri, Name).
hdt_term(HDT, name, Name) :-
	hdt_term(HDT, literal, Name).
% node
hdt_term(HDT, node, Node) :-
	(   var(Node)
	->  (   hdt_column_(HDT, shared, Var),
	        Var = Node
	    ;   hdt_column_(HDT, subject, Var),
	        Var = Node
	    ;   hdt_object_(HDT, Var),
	        post_object(Node, Var)
	    )
	;   hdt_search(HDT, Node,_,_)
	->  true
	;   hdt_search(HDT, _,_,Node)
	->  true
	).
% object
hdt_term(HDT, object, O) :-
	(   var(O)
	->  (   hdt_column_(HDT, shared, Var),
	        Var = O
	    ;	hdt_object_(HDT, Var),
		post_object(O, Var)
	    )
	;   hdt(HDT, _,_,O)
	->  true
	).
% predicate
hdt_term(HDT, predicate, P) :-
	(   var(P)
	->  hdt_column_(HDT, predicate, Var),
	    Var = P
	;   hdt(HDT, _,P,_)
	->  true
	).
% shared
hdt_term(HDT, shared, Shared) :-
	(   var(Shared)
	->  hdt_column_(HDT, shared, Var),
	    Var = Shared
	;   rdf_is_subject(Shared),
	    hdt(HDT, Shared,_,_),
	    hdt(HDT, _,_,Shared)
	->  true
	).
% subject
hdt_term(HDT, subject, S) :-
	(   var(S)
	->  (   hdt_column_(HDT, shared, Var)
	    ;	hdt_column_(HDT, subject, Var)
	    ),
	    Var = S
	;   hdt_search(HDT, S, _, _)
	->  true
	).


%%	hdt_term(+HDT, +Role, +Prefix, +Term) is semidet.
%%	hdt_term(+HDT, +Role, +Prefix, -Term) is nondet.

%%	hdt_term_count(+HDT, +Role, -Count:nonneg) is nondet.

hdt_term_count(HDT, node, Count) :-
	maplist(hdt_term_count(HDT), [object,shared,subject], Counts),
	sum_list(Counts, Count).
hdt_term_count(HDT, object, Count) :-
	hdt_header(HDT, _, '<http://rdfs.org/ns/void#distinctObjects>', Count^^_).
hdt_term_count(HDT, predicate, Count) :-
	hdt_header(HDT, _, '<http://rdfs.org/ns/void#properties>', Count^^_).
hdt_term_count(HDT, shared, Count) :-
	hdt_header(HDT, _, '<http://purl.org/HDT/hdt#dictionarynumSharedSubjectObject>', Count^^_).
hdt_term_count(HDT, subject, Count) :-
	hdt_header(HDT, _, '<http://rdfs.org/ns/void#distinctSubjects>', Count^^_).


%%	hdt_term_count(+HDT, +Role, +Prefix:atom, -Count:nonneg) is nondet.

%%	hdt_term_id(+HDT, +Role, -ID) is nondet.

%%	hdt_term_rnd(+HDT, +Role, -Term) is nondet.

%%	hdt_term_rnd(+HDT, +Role, +Prefix, -Term) is nondet.

%%	hdt_term_rnd_id(+HDT, +Role, -ID) is nondet.

%%	hdt_term_rnd_id(+HDT, +Role, +Prefix, -ID) is nondet.



		 /*******************************
		 *	      OTHERS		*
		 *******************************/

%%	hdt_header(+HDT, ?S,?P,?O) is nondet.
%
%	True if 〈S,P,O〉 is a triple in the header of HDT.

hdt_header(HDT, S, P, O) :-
	hdt_search(HDT, header, S, P, O0),
	header_object(O0, O).

header_object(O0, O) :-
	string(O0), !,
	header_untyped_object(O0, O).
header_object(O, O).

header_untyped_object(O0, O) :-
	catch(xsd_number_string(N, O0),
	      error(syntax_error(xsd_number), _),
	      fail), !,
	(   integer(N)
	->  rdf_equal(O, N^^xsd:integer)
	;   rdf_equal(O, N^^xsd:float)
	).
header_untyped_object(O0, O) :-
	catch(xsd_time_string(Term, Type, O0),
	      error(_,_), fail), !,
	O = Term^^Type.
header_untyped_object(S, O) :-
	rdf_equal(O, S^^xsd:string).


%%	hdt_property(+HDT, +Property) is semidet.
%%	hdt_property(+HDT, ?Property) is nondet.
%
%	True of Property is a property of HTD.  Defined properties are
%
%	  - mapping(-Mapping)
%	  - max_id(-ID))
%	  - max_object_id(-ID))
%	  - max_predicate_id(-ID))
%	  - max_subject_id(-ID))
%	  - objects(-Count))
%	  - predicates(-Count))
%	  - shared(-Count))
%	  - subjects(-Count))
%	  - elements(-Count))

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
		 *	     HELPERS		*
		 *******************************/

%%	hdt_search(+HDT, ?S, ?P, ?O)
%
%	True if <S,P,O> is a triple in HDT.

hdt_search(HDT, S, P, O) :-
	pre_object(HDT, O, OHDT),
	hdt_search(HDT, content, S, P, OHDT),
post_object(O, OHDT).


%%	hdt_suggestions(+HDT, +Base, +Role, +MaxResults, -Results:list) is det.
%
%	True when Results is a  list  of   suggestions  for  Base in the
%	triple role Role. Some experimentation   suggests  it performs a
%	prefix match on the internal string representation. This implies
%	that literals are only found if the   first character of Base is
%	`"`.
%
%	@arg Base is a string or atom
%	@arg Role is one of `subject`, `predicate` or `object`


%%	pre_object(+HDT, ?O, -OHDT) is det.
%%	post_object(?O, +OHDT) is det.
%
%	Pre/post object processing. The  HDT   library  itself is purely
%	string based.

pre_object(_HDT, O, OHDT) :-
	atom(O), \+ boolean(O), !,
	OHDT = O.
pre_object(_HDT, O, OHDT) :-
	ground(O), !,
	rdf_lexical_form(O, Lexical),
	canonical_string(Lexical, OHDT).
pre_object(HDT, O, OHDT) :-
	nonvar(O),
	O = String@Lang,
	ground(String),
	atomics_to_string(["\"", String, "\"@"], Prefix),
	hdt_suggestions(HDT, Prefix, object, 1000, List),
	length(List, Found),
	Found < 1000, !,		% we got them all
	member(_@Lang, List),
	canonical_string(String@Lang, OHDT).
pre_object(_, _, _).

canonical_string(Lexical^^Type, HDT) :-
	atomics_to_string(["\"", Lexical, "\"^^<", Type, ">"], HDT).
canonical_string(Lexical@Lang, HDT) :-
	atomics_to_string(["\"", Lexical, "\"@", Lang], HDT).

boolean(false).
boolean(true).

%!	post_object(?PrologObj, ?HDTObjectString) is semidet.

post_object(O, _HDT) :-
	ground(O), !.
post_object(O, IRI) :-
	atom(IRI), !,
	O = IRI.
post_object(O, HDT) :-
	rdf_canonical_literal(HDT, O).


%%	hdt_subject_id(+HDT, ?S, ?Id) is semidet.
%%	hdt_predicate_id(+HDT, ?P, ?Id) is semidet.
%%	hdt_object_id(+HDT, ?O, ?Id) is semidet.
%
%	True if String is mapped to Id in   the given role. Fails if the
%	requested String or Id is not known for the given role in HDT.

hdt_subject_id(HDT, String, Id) :-
	hdt_string_id(HDT, subject, String, Id).
hdt_predicate_id(HDT, String, Id) :-
	hdt_string_id(HDT, predicate, String, Id).
hdt_object_id(HDT, O, Id) :-
	pre_object(HDT, O, String),
	hdt_string_id(HDT, object, String, Id),
	post_object(O, String).

%%	hdt_pre_triple(+HDT,  ?TripleIn, -TripleID) is det.
%%	hdt_post_triple(+HDT, ?TripleIn, +TripleID) is det.
%
%	Perform term->id and  id->term  translation   for  triples.  The
%	predicate hdt_search/4 could be defined as:
%
%	  ==
%	  hdt_search(HDT, S, P, O) :-
%	      Triple   = t(S,P,O),
%	      TripleID = t(SID,PID,OID),
%	      hdt_pre_triple(HDT, Triple, TripleID),
%	      hdt_search_id(HDT,SID,PID,OID),
%	      hdt_post_triple(HDT, Triple, TripleID).
%	  ==
%
%	@see hdt_search_id/4.

hdt_pre_triple(HDT, t(S0,P0,O0), t(S,P,O)) :-
	pre_iri_id(HDT, subject, S0, S),
	pre_iri_id(HDT, predicate, P0, P),
	(   ground(O0)
	->  pre_object(HDT, O0, String),
	    hdt_string_id(HDT, object, String, O)
	;   true
	).

hdt_post_triple(HDT, t(S0,P0,O0), t(S,P,O)) :-
	post_iri_id(HDT, subject, S0, S),
	post_iri_id(HDT, predicate, P0, P),
	(   ground(O0)
	->  true
	;   hdt_string_id(HDT, object, String, O),
	    post_object(O0, String)
	).

pre_iri_id(_, _, In, _) :-
	var(In), !.
pre_iri_id(HDT, Role, In, Id) :-
	hdt_string_id(HDT, Role, In, Id).

post_iri_id(_, _, S0, _) :-
	atom(S0), !.
post_iri_id(HDT, Role, In, Id) :-
	hdt_string_id(HDT, Role, In, Id).



		 /*******************************
		 *	      MESSAGES		*
		 *******************************/

:- multifile prolog:error_message//1.

prolog:error_message(hdt_error(Message)) -->
	[ 'HDT: ~w'-[Message] ].
