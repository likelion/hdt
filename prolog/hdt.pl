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

:- module(
   hdt,
   [
   % HDT FILES
     hdt_create/2,      % +RdfFile, +HdtFile
     hdt_create/3,      % +RdfFile, +HdtFile, +Options
     hdt_graph/2,       % ?Hdt, ?G
     hdt_open/2,        % +File, -Hdt
     hdt_open/3,        % +File, -Hdt, +Options
     hdt_close/1,       % +Hdt

   % TERM ↔ ID
     hdt_term_id/4,     % +Hdt, +Role, ?Term, ?Id

   % TRIPLES
     hdt/4,             % +Hdt, ?S,?P,?O
     hdt_id/4,          % +Hdt, ?SId, ?PId, ?OId
     hdt_count/5,       % +Hdt, ?S, ?P, ?O, ?Count
     hdt_count_id/5,    % +Hdt, ?SId, ?PId, ?OId, ?Count
     hdt_rnd/4,         % +Hdt, ?S, ?P, ?O
     hdt_rnd_id/4,      % +Hdt, ?SId, ?PId, ?OId

   % TERMS
     hdt_term/3,        % +Hdt, +Role, ?Term
     %hdt_term_id/3,     % +Hdt, +Role, ?Id
     %hdt_term_count/3,  % +Hdt, +Role, ?Count
     %hdt_term_rnd/3,    % +Hdt, +Role, -Term
     %hdt_term_rnd_id/3, % +Hdt, +Role, -Id

   % PREFIX SEARCH
     %hdt_term/4,        % +Hdt, +Role, +Prefix, ?Term
     %hdt_term_id/4,     % +Hdt, +Role, +Prefix, ?Id
     %hdt_term_count/4,  % +Hdt, +Role, +Prefix, ?Count
     %hdt_term_rnd/4,    % +Hdt, +Role, +Prefix, -Term
     %hdt_term_rnd_id/4, % +Hdt, +Role, +Prefix, -Id

   % OTHERS
     hdt_header/4,      % +Hdt, ?S,?P,?O
     hdt_property/2,    % +HTD, -Property
     op(110, xfx, @),   % must be above `.'
     op(650, xfx, ^^)   % must be above `:'
   ]
).

/** <module> Access HDT (Header Dictionary Triples) files

@author Jan Wielemaker
@author Wouter Beek
@version 2017-09-18
*/

:- use_module(library(error)).
:- use_module(library(lists)).
:- use_module(library(option)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(sgml)).

:- use_foreign_library(foreign(hdt4pl)).

:- dynamic
    hdt_graph_/2.

:- rdf_meta
   hdt(+, r, r, o),
   hdt_close_graph(r),
   hdt_count(+, r, r, o, -),
   hdt_graph(?, r),
   hdt_header(+, r, r, o),
   hdt_rnd(+, r, r, o),
   hdt_term(+, +, t),
   hdt_term(+, +, +, r),
   hdt_term_id(+, +, t, ?).





% FILE OPERATIONS %

%! hdt_close(+Hdt) is det.
%
% Close an HDT that was previously opened with hdt_open/[2,3].

hdt_close(hdt(Hdt)) :- !,
  hdt_close_(Hdt).
hdt_close(G) :- !,
  with_mutex(hdt_graph_, (
    hdt_graph_(Hdt, G),
    hdt_close(hdt(Hdt)),
    retractall(hdt_graph_(_,G))
  )).



%! hdt_create(+RdfFile:atom, +HdtFile:atom) is det.
%! hdt_create(+RdfFile:atom, +HdtFile:atom, +Options:list(compound)) is det.
%
% Create an HDT file from an uncompressed N-Triples file.
%
% The following Options are supported:
%
%    * base_uri(+URI:atom)
%    URI is used for generating the header properties (see
%    http_header/4.

hdt_create(RdfFile, HdtFile) :-
  hdt_create(RdfFile, HdtFile, []).


hdt_create(RdfFile, HdtFile, Options) :-
  hdt_create_(HdtFile, RdfFile, Options).



%! hdt_graph(+Hdt:blob, +G:atom) is semidet.
%! hdt_graph(+Hdt:blob, -G:atom) is semidet.
%! hdt_graph(-Hdt:blob, +G:atom) is semidet.
%! hdt_graph(-Hdt:blob, -G:atom) is nondet.

hdt_graph(Hdt, G) :-
  with_mutex(hdt_graph_, (
    hdt_graph_(Hdt, G)
  )).



%! hdt_open(+File:atom, -Hdt:blob) is det.
%! hdt_open(+File:atom, -Hdt:blob, +Options:list(compound)) is det.
%
% Open an existing HDT file and unify Hdt with a handle to it.  The
% handle is an opaque symbol that is subject to (atom) garbage
% collection.
%
% The following Options are supported:
%
%   * access(+oneof([load,map]))
%
%   How the file is accessed.  On of `map` (map the file into memory,
%   default) or `load` (load the content of the file).
%
%   * graph(+atom)
%
%   An alias by which one can refer to the opaque Hdt handle.  This
%   alias acts as a name for the graph, or set of triples, that is
%   contained in the HDT file.
%
%   * indexed(+boolean)
%
%   Whether or not an index file is created.  Default is `true`.
%
%   An index is needed for partially instantiated calls to hdt/4, but
%   not for retrieving all statements (Triple Pattern 〈?,?,?〉).
%
%   The index is maintained in a file with extension `.index` in the
%   same directory as the HDT file.

hdt_open(File, Hdt) :-
  hdt_open(Hdt, File, []).


hdt_open(File, Hdt, Options1) :-
  (   select_option(graph(G), Options1, Options2)
  ->  hdt_register_graph_(Hdt, G)
  ;   Options2 = Options1
  ),
  hdt_open_(File, Hdt, Options2).





% TERM ↔ ID %

%! hdt_term_id(+Hdt, +Role, +Term, +Id) is semidet.
%! hdt_term_id(+Hdt, +Role, +Term, -Id) is det.
%! hdt_term_id(+Hdt, +Role, -Term, +Id) is det.
%
% @arg Role is one of `subject`, `predicate` or `object`.

hdt_term_id(Hdt0, Role, Term, Id) :- !,
  hdt_blob(Hdt0, Hdt),
  (   Role == object
  ->  pre_object(Hdt, Term, Var),
      hdt_term_id_(Hdt, object, Var, Id),
      post_object(Term, Var)
  ;   hdt_term_id_(Hdt, Role, Term, Id)
  ).





% TRIPLES %

%! hdt(+Hdt, ?S, ?P, ?O) is nondet.
%
% True if 〈S,P,O〉 is an RDF triple in HDT.

hdt(Hdt0, S, P, O) :-
  hdt_blob(Hdt0, Hdt),
  pre_object(Hdt, O, O0),
  hdt_(Hdt, content, S, P, O0),
  post_object(O, O0).



%! hdt_id(+Hdt, ?SId:nonneg, ?PId:nonneg, ?OId:nonneg) is nondet.
%
% True if 〈SId,SIP,SIO〉 is an integer triple in Hdt.

hdt_id(Hdt0, SId, PId, OId) :-
  hdt_blob(Hdt0, Hdt),
  hdt_id_(Hdt, SId, PId, OId).



%! hdt_count(+Hdt, ?S, ?P, ?O, +Count:nonneg) is semidet.
%! hdt_count(+Hdt, ?S, ?P, ?O, -Count:nonneg) is det.
%
% True if Count is the number of matches of the Triple Pattern〈S,P,O〉
% on the graph stored in HDT.

hdt_count(Hdt0, S, P, O, Count) :-
  hdt_blob(Hdt0, Hdt),
  Triple = t(S,P,O),
  TripleId = t(SId,PId,OId),
  hdt_pre_triple(Hdt, Triple, TripleId),
  hdt_count_id_(Hdt, SId, PId, OId, Count), !.
hdt_count(_, _,_,_, 0).



%! hdt_count_id(+Hdt, ?SId, ?PId, ?OId, +Count:nonneg) is semidet.
%! hdt_count_id(+Hdt, ?SId, ?PId, ?OId, -Count:nonneg) is det.

hdt_count_id(Hdt0, SId, PId, OId, Count) :-
  hdt_blob(Hdt0, Hdt),
  hdt_count_id_(Hdt, SId, PId, OId, Count), !.
hdt_count_id(_, _,_,_, 0).



%! hdt_rnd(+Hdt, ?S, ?P, ?O) is nondet.

hdt_rnd(Hdt0, S, P, O) :-
  hdt_blob(Hdt0, Hdt),
  Triple = t(S,P,O),
  TripleId = t(SId,PId,OId),
  hdt_pre_triple(Hdt, Triple, TripleId),
  hdt_rnd_id(Hdt, SId, PId, OId),
  hdt_post_triple(Hdt, Triple, TripleId).



%! hdt_rnd_id(+Hdt, ?SId, ?PId, ?OId) is nondet.

hdt_rnd_id(Hdt0, SId, PId, OId) :-
  hdt_blob(Hdt0, Hdt),
  hdt_rnd_id_(Hdt, SId, PId, OId).





% TERMS %

%! hdt_term(+Hdt, +Role, +Term) is semidet.
%! hdt_term(+Hdt, +Role, -Term) is nondet.
%
%  @arg Role is either of the following term types:
%  - bnode
%  - iri
%  - literal
%  - name
%  or either of the following term positions:
%  - node
%  - object
%  - predicate
%  - shared
%  - subject

% name
hdt_term(Hdt, name, Name) :-
  hdt_term(Hdt, iri, Name).
hdt_term(Hdt, name, Name) :-
  hdt_term(Hdt, literal, Name).
% node
hdt_term(Hdt, node, Node) :-
  (   var(Node)
  ->  (   hdt_term_(Hdt, shared, Var),
          Var = Node
      ;   hdt_term_(Hdt, subject, Var),
          Var = Node
      ;   hdt_object_(Hdt, Var),
          post_object(Node, Var)
      )
  ;   hdt_(Hdt, content, Node, _, _)
  ->  true
  ;   pre_object(Hdt, Node, Var),
      hdt_(Hdt, content, _, _, Var)
  ->  true
  ).
% object
hdt_term(Hdt, object, O) :-
  (   var(O)
  ->  (   hdt_term_(Hdt, shared, Var),
          Var = O
      ;   hdt_object_(Hdt, Var),
          post_object(O, Var)
      )
  ;   pre_object(Hdt, O, Var),
      hdt_(Hdt, content, _, _, Var)
  ->  true
  ).
% predicate
hdt_term(Hdt, predicate, P) :-
  (   var(P)
  ->  hdt_term_(Hdt, predicate, Var),
      Var = P
  ;   hdt_(Hdt, content, _, P, _)
  ->  true
  ).
% shared
hdt_term(Hdt, shared, Shared) :-
  (   var(Shared)
  ->  hdt_term_(Hdt, shared, Var),
      Var = Shared
  ;   rdf_is_subject(Shared),
      hdt_(Hdt, content, Shared, _, _),
      hdt_(Hdt, content, _, _, Shared)
  ->  true
  ).
% subject
hdt_term(Hdt, subject, S) :-
  (   var(S)
  ->  (   hdt_term_(Hdt, shared, Var)
      ;   hdt_term_(Hdt, subject, Var)
      ),
      Var = S
  ;   hdt_(Hdt, content, S, _, _)
  ->  true
  ).



%! hdt_term_count(+Hdt, +Role, -Count:nonneg) is nondet.

hdt_term_count(Hdt, node, Count) :-
  maplist(hdt_term_count(Hdt), [object,shared,subject], Counts),
  sum_list(Counts, Count).
hdt_term_count(Hdt, object, Count) :-
  hdt_header(Hdt, _, '<http://rdfs.org/ns/void#distinctObjects>', Count^^_).
hdt_term_count(Hdt, predicate, Count) :-
  hdt_header(Hdt, _, '<http://rdfs.org/ns/void#properties>', Count^^_).
hdt_term_count(Hdt, shared, Count) :-
  hdt_header(Hdt, _, '<http://purl.org/HDT/hdt#dictionarynumSharedSubjectObject>', Count^^_).
hdt_term_count(Hdt, subject, Count) :-
  hdt_header(Hdt, _, '<http://rdfs.org/ns/void#distinctSubjects>', Count^^_).



%! hdt_term_count(+Hdt, +Role, +Prefix:atom, -Count:nonneg) is nondet.



%! hdt_term_id(+Hdt, +Role, -Id) is nondet.



%! hdt_term_rnd(+Hdt, +Role, -Term) is nondet.



%! hdt_term_rnd(+Hdt, +Role, +Prefix, -Term) is nondet.



%! hdt_term_rnd_id(+Hdt, +Role, -Id) is nondet.



%! hdt_term_rnd_id(+Hdt, +Role, +Prefix, -Id) is nondet.





% PREFIX SEARCH %

%! hdt_prefix(+Hdt, +Role, +Prefix, +Term) is semidet.
%! hdt_prefix(+Hdt, +Role, +Prefix, -Term) is nondet.

hdt_prefix(Hdt0, Role, Prefix, Term) :-
  hdt_blob(Hdt0, Hdt),
  hdt_prefix_(Hdt, Role, Prefix, Term).





% OTHERS %

%! hdt_header(+Hdt, ?S, ?P, ?O) is nondet.
%
% True if 〈S,P,O〉 is a triple in the header of Hdt.

hdt_header(Hdt0, S, P, O) :-
  hdt_blob(Hdt0, Hdt),
  hdt_(Hdt, header, S, P, O0),
  header_object(O0, O).

header_object(O0, O) :-
  string(O0), !,
  header_untyped_object(O0, O).
header_object(O, O).

header_untyped_object(O0, O) :-
  catch(
    xsd_number_string(N, O0),
    error(syntax_error(xsd_number), _),
    fail
  ), !,
  (   integer(N)
  ->  rdf_equal(O, N^^xsd:integer)
  ;   rdf_equal(O, N^^xsd:float)
  ).
header_untyped_object(O0, O) :-
  catch(
    xsd_time_string(Term, Type, O0),
    error(_,_),
    fail
  ), !,
  O = Term^^Type.
header_untyped_object(S, O) :-
  rdf_equal(O, S^^xsd:string).



%! hdt_property(+Hdt, +Property:compound) is semidet.
%! hdt_property(+Hdt, ?Property:compound) is nondet.
%
%  True of Property is a property of HTD.  Defined properties are
%
%    * elements(-Count))
%    * mapping(-Mapping)
%    * max_id(-Id))
%    * max_object_id(-Id))
%    * max_predicate_id(-Id))
%    * max_subject_id(-Id))
%    * objects(-Count))
%    * predicates(-Count))
%    * shared(-Count))
%    * subjects(-Count))

hdt_property(Hdt0, Property) :-
  hdt_blob(Hdt0, Hdt),
  hdt_property(Property),
  hdt_property_(Hdt, Property).

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





% HELPERS %

%! hdt_blob(+Hdt0, -Hdt:blob) is det.

hdt_blob(hdt(Hdt), Hdt) :- !.
hdt_blob(graph(G), Hdt) :- !,
  hdt_graph(Hdt, G).
hdt_blob(Hdt0, Hdt) :-
  (atom(Hdt0) ->  hdt_graph(Hdt, Hdt0) ; Hdt = Hdt0).
  


%! hdt_prefix(+Hdt, +Role:oneof([subject,predicate,object]), +Prefix:atom,
%!            -Term) is det.
%
% True when Results is a list of suggestions for Base in the triple
% role Role. Some experimentation suggests it performs a prefix match
% on the internal string representation. This implies that literals
% are only found if the first character of Base is `"`.
%
% @arg Role is one of `subject`, `predicate` or `object`.
%
% @arg Prefix is a string or atom.



%! hdt_register_graph(+Hdt:blob, +G:atom) is det.

hdt_register_graph_(Hdt, G) :-
  must_be(atom, G),
  with_mutex(hdt_graph_, (
    (   hdt_graph_(Hdt, _)
    ->  existence_error(hdt, Hdt)
    ;   hdt_graph_(_, G)
    ->  existence_error(graph, G)
    ;   assert(hdt_graph_(Hdt, G))
    )
  )).



%! post_object(?O, +Var) is det.
%
% Pre/post object processing. The HDT library itself is purely string
% based.

post_object(O, _Hdt) :-
  ground(O), !.
post_object(O, IRI) :-
  atom(IRI), !,
  O = IRI.
post_object(O, Hdt) :-
  rdf_canonical_literal(Hdt, O).



%! pre_object(+Hdt:blob, ?O, -Var) is det.

pre_object(_, O, OHdt) :-
  atom(O), \+ boolean(O), !,
  OHdt = O.
pre_object(_, O, OHdt) :-
  ground(O), !,
  rdf_lexical_form(O, Lexical),
  canonical_string(Lexical, OHdt).
pre_object(Hdt, O, OHdt) :-
  nonvar(O),
  O = Lex@Lang,
  ground(Lex),
  atomics_to_string(["\"", Lex, "\"@"], Prefix),
  hdt_suggestions(Hdt, Prefix, object, 1000, List),
  length(List, Found),
  Found < 1000, !,    % we got them all
  member(_@Lang, List),
  canonical_string(Lex@Lang, OHdt).
pre_object(_, _, _).

canonical_string(Lexical^^Type, Hdt) :-
  atomics_to_string(["\"", Lexical, "\"^^<", Type, ">"], Hdt).
canonical_string(Lexical@Lang, Hdt) :-
  atomics_to_string(["\"", Lexical, "\"@", Lang], Hdt).

boolean(false).
boolean(true).



%! hdt_post_triple(+Hdt:blob, ?Triple:compound, +TripleId:compound) is det.
%
% Perform term->id and id->term translation for triples.  The
% predicate hdt_search/4 could be defined as:
%
%    ==
%    hdt_search(Hdt, S, P, O) :-
%        Triple   = t(S,P,O),
%        TripleId = t(SId,PId,OId),
%        hdt_pre_triple(Hdt, Triple, TripleId),
%        hdt_search_id(Hdt,SId,PId,OId),
%        hdt_post_triple(Hdt, Triple, TripleId).
%    ==
%
%  @see hdt_search_id/4.

hdt_post_triple(Hdt, t(S,P,O), t(SId,PId,OId)) :-
  post_iri_id(Hdt, subject, S, SId),
  post_iri_id(Hdt, predicate, P, PId),
  (   ground(O)
  ->  true
  ;   hdt_term_id_(Hdt, object, Var, OId),
      post_object(O, Var)
  ).

post_iri_id(_, _, S, _) :-
  atom(S), !.
post_iri_id(Hdt, Role, Term, Id) :-
  hdt_term_id_(Hdt, Role, Term, Id).



%! hdt_pre_triple(+Hdt:blob,  ?Triple:compound, -TripleId:compound) is det.

hdt_pre_triple(Hdt, t(S,P,O), t(SId,PId,OId)) :-
  pre_iri_id(Hdt, subject, S, SId),
  pre_iri_id(Hdt, predicate, P, PId),
  (   ground(O)
  ->  pre_object(Hdt, O, Var),
      hdt_term_id_(Hdt, object, Var, OId)
  ;   true
  ).

pre_iri_id(_, _, Term, _) :-
  var(Term), !.
pre_iri_id(Hdt, Role, Term, Id) :-
  hdt_term_id_(Hdt, Role, Term, Id).





% MESSAGES %

:- multifile
    prolog:error_message//1.

prolog:error_message(hdt_error(Message)) -->
  [ "HDT: ~w"-[Message] ].
