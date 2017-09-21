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
     hdt_create/1,      % +RdfFile
     hdt_create/2,      % +RdfFile, +Options
     hdt_graph/2,       % ?Hdt, ?G
     hdt_open/1,        % +HdtFile
     hdt_open/2,        % +HdtFile, +Options
     hdt_close/1,       % +G

   % TERM ↔ ID
     hdt_dict/3,        % +Role, ?Term, ?Id
     hdt_dict/4,        % +Role, ?Term, ?Id, ?G

   % TRIPLES
     hdt/3,             % ?S, ?P, ?O
     hdt/4,             % ?S, ?P, ?O, ?G
     hdt_count/4,       % ?S, ?P, ?O, ?Count
     hdt_count/5,       % ?S, ?P, ?O, ?Count, ?G
     hdt_count_id/4,    % ?SId, ?PId, ?OId, ?Count
     hdt_count_id/5,    % ?SId, ?PId, ?OId, ?Count, ?G
     hdt_id/3,          % ?SId, ?PId, ?OId
     hdt_id/4,          % ?SId, ?PId, ?OId, ?G
     hdt_rnd/3,         % ?S, ?P, ?O
     hdt_rnd/4,         % ?S, ?P, ?O, ?G
     hdt_rnd_id/3,      % ?SId, ?PId, ?OId
     hdt_rnd_id/4,      % ?SId, ?PId, ?OId, ?G

   % TERMS
     hdt_term/2,        % +Role, ?Term
     hdt_term/3,        % +Role, ?Term, ?G
     hdt_term_count/2,  % +Role, ?Count
     hdt_term_count/3,  % +Role, ?Count, ?G
     hdt_term_id/2,     % +Role, ?Id
     hdt_term_id/3,     % +Role, ?Id, ?G
     hdt_term_rnd/2,    % +Role, -Term
     hdt_term_rnd/3,    % +Role, -Term, ?G
     hdt_term_rnd_id/2, % +Role, -Id
     hdt_term_rnd_id/3, % +Role, -Id, ?G

   % PREFIX SEARCH
     hdt_prefix/3,      % +Role, +Prefix, ?Term
     hdt_prefix/4,      % +Role, +Prefix, ?Term, ?G
     hdt_prefix_id/3,   % +Role, +Prefix, ?Id
     hdt_prefix_id/4,   % +Role, +Prefix, ?Id, ?G

   % OTHERS
     hdt_header/3,      % ?S, ?P, ?O
     hdt_header/4,      % ?S, ?P, ?O, +G
     hdt_property/2,    % +Hdt, -Property
     op(110, xfx, @),   % must be above `.'
     op(650, xfx, ^^)   % must be above `:'
   ]
).

/** <module> Access HDT (Header Dictionary Triples) files

@author Jan Wielemaker
@author Wouter Beek
@version 2017/09
*/

:- use_module(library(dcg/basics)).
:- use_module(library(error)).
:- use_module(library(filesex)).
:- use_module(library(lists)).
:- use_module(library(option)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(sgml)).
:- use_module(library(uri)).

:- use_foreign_library(foreign(hdt4pl)).

:- dynamic
    hdt_graph_/2.

:- rdf_meta
   hdt(r, r, o),
   hdt(r, r, o, r),
   hdt_close(r),
   hdt_count(r, r, o, -),
   hdt_count(r, r, o, -, r),
   hdt_count_id(?, ?, ?, -, r),
   hdt_dict(+, r, ?),
   hdt_dict(+, r, ?, r),
   hdt_graph(?, r),
   hdt_header(r, r, o),
   hdt_header(r, r, o, r),
   hdt_id(?, ?, ?, r),
   hdt_rnd(r, r, o),
   hdt_rnd(r, r, o, r),
   hdt_rnd_id(?, ?, ?, r),
   hdt_term(+, t),
   hdt_term(+, t, r),
   hdt_prefix(+, +, t),
   hdt_prefix(+, +, t, r).





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



%! hdt_create(+RdfFile:atom) is det.
%! hdt_create(+RdfFile:atom, +Options:list(compound)) is det.
%
% Create an HDT file from an uncompressed N-Triples file.
%
% The following Options are supported:
%
%    * base_uri(+URI:atom)
%
%    URI is used for generating the header properties (see
%    http_header/4.
%
%    * hdt_file(?atom)
%
%    Either set the name of the HDT file, or retrieve the
%    automatically created name of the HDT file.

hdt_create(RdfFile) :-
  hdt_create(RdfFile, []).


hdt_create(RdfFile, Options1) :-
  (   select_option(hdt_file(HdtFile), Options1, Options2)
  ->  true
  ;   directory_file_path(Dir, RdfLocal, RdfFile),
      atomic_list_concat([Base|_], ., RdfLocal),
      file_name_extension(Base, hdt, HdtLocal),
      directory_file_path(Dir, HdtLocal, HdtFile),
      Options2 = Options1
  ),
  hdt_create_(HdtFile, RdfFile, Options2).



%! hdt_graph(+G:atom) is semidet.
%! hdt_graph(-G:atom) is nondet.

hdt_graph(G) :-
  hdt_graph(_, G).


%! hdt_graph(+Hdt:blob, +G:atom) is semidet.
%! hdt_graph(+Hdt:blob, -G:atom) is semidet.
%! hdt_graph(-Hdt:blob, +G:atom) is semidet.
%! hdt_graph(-Hdt:blob, -G:atom) is nondet.
%
% @throws existence_error
% @throws instantiation_error

hdt_graph(Hdt, G) :-
  with_mutex(hdt_graph_, hdt_graph_(Hdt, G)), !.
hdt_graph(_, G) :-
  ground(G), !,
  existence_error(hdt_graph, G).
hdt_graph(Hdt, G) :-
  ground(G), !,
  existence_error(hdt_blob, Hdt).
hdt_graph(_, _) :-
  instantiation_error(_).



%! hdt_open(+HdtFile:atom) is det.
%! hdt_open(+HdtFile:atom, +Options:list(compound)) is det.
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
%   By default this is the URI denoting HdtFile.
%
%   * handle(-blob)
%
%   Return a direct handle to the opened HDT file.
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

hdt_open(HdtFile) :-
  hdt_open(HdtFile, []).


hdt_open(HdtFile, Options) :-
  ignore(option(graph(G), Options)),
  (var(G) -> uri_file_name(G, HdtFile) ; true),
  hdt_open_(HdtFile, Hdt, Options),
  register_graph_(Hdt, G),
  ignore(option(handle(Hdt), Options)).





% DICTIONARY TRANSLATIONS: TERM ↔ ID %

%! hdt_dict(+Role, +Term, +Id) is semidet.
%! hdt_dict(+Role, +Term, -Id) is det.
%! hdt_dict(+Role, -Term, +Id) is det.

hdt_dict(Role, Term, Id) :-
  hdt_dict(Role, Term, Id, _).


%! hdt_dict(+Role, +Term, +Id, ?G) is semidet.
%! hdt_dict(+Role, +Term, -Id, ?G) is det.
%! hdt_dict(+Role, -Term, +Id, ?G) is det.
%
% @arg Role is one of `subject`, `predicate` or `object`.

hdt_dict(Role, Term, Id, Hdt0) :- !,
  hdt_blob(Hdt0, Hdt),
  (   Role == object
  ->  pre_object(Hdt, Term, Atom),
      hdt_dict_(Hdt, object, Atom, Id),
      post_object(Term, Atom)
  ;   hdt_dict_(Hdt, Role, Term, Id)
  ).





% TRIPLES %

%! hdt(?S, ?P, ?O) is nondet.

hdt(S, P, O) :-
  hdt(S, P, O, _).


%! hdt(?S, ?P, ?O, ?G) is nondet.
%
% True if 〈S,P,O〉 is an RDF triple in HDT.

hdt(S, P, O, Hdt0) :-
  hdt_blob(Hdt0, Hdt),
  %(var(S) -> true ; \+ rdf_is_literal(S)),
  pre_object(Hdt, O, Atom),
  hdt_(Hdt, content, S, P, Atom),
  post_object(O, Atom).



%! hdt_count(?S, ?P, ?O, +Count:nonneg) is semidet.
%! hdt_count(?S, ?P, ?O, -Count:nonneg) is det.

hdt_count(S, P, O, Count) :-
  hdt_count(S, P, O, Count, _).


%! hdt_count(?S, ?P, ?O, +Count:nonneg, ?G) is semidet.
%! hdt_count(?S, ?P, ?O, -Count:nonneg, ?G) is det.
%
% True if Count is the number of matches of the Triple Pattern〈S,P,O〉
% on the graph stored in HDT.

hdt_count(S, P, O, Count, Hdt0) :-
  hdt_blob(Hdt0, Hdt),
  pre_object(Hdt, O, Atom),
  hdt_count_(Hdt, S, P, Atom, Count).



%! hdt_count_id(?SId, ?PId, ?OId, +Count:nonneg) is semidet.
%! hdt_count_id(?SId, ?PId, ?OId, -Count:nonneg) is det.

hdt_count_id(SId, PId, OId, Count) :-
  hdt_count_id(SId, PId, OId, Count, _).


%! hdt_count_id(?SId, ?PId, ?OId, +Count:nonneg, ?G) is semidet.
%! hdt_count_id(?SId, ?PId, ?OId, -Count:nonneg, ?G) is det.

hdt_count_id(SId, PId, OId, Count, Hdt0) :-
  hdt_blob(Hdt0, Hdt),
  hdt_count_id_(Hdt, SId, PId, OId, Count), !.
hdt_count_id(_, _, _, 0, _).



%! hdt_id(?SId:nonneg, ?PId:nonneg, ?OId:nonneg) is nondet.

hdt_id(SId, PId, OId) :-
  hdt_id(SId, PId, OId, _).


%! hdt_id(?SId:nonneg, ?PId:nonneg, ?OId:nonneg, ?G) is nondet.
%
% True if 〈SId,SIP,SIO〉 is an integer triple in Hdt.

hdt_id(SId, PId, OId, Hdt0) :-
  hdt_blob(Hdt0, Hdt),
  hdt_id_(Hdt, SId, PId, OId).



%! hdt_rnd(?S, ?P, ?O) is nondet.

hdt_rnd(S, P, O) :-
  hdt_rnd(S, P, O, _).


%! hdt_rnd(?S, ?P, ?O, ?G) is nondet.

hdt_rnd(S, P, O, Hdt0) :-
  hdt_blob(Hdt0, Hdt),
  pre_object(Hdt, O, Atom),
  hdt_rnd_(Hdt, S, P, Atom),
  post_object(O, Atom).



%! hdt_rnd_id(?SId, ?PId, ?OId) is nondet.

hdt_rnd_id(SId, PId, OId) :-
  hdt_rnd_id(SId, PId, OId, _).


%! hdt_rnd_id(?SId, ?PId, ?OId, G) is nondet.

hdt_rnd_id(SId, PId, OId, Hdt0) :-
  hdt_blob(Hdt0, Hdt),
  hdt_rnd_id_(Hdt, SId, PId, OId).





% TERMS %

%! hdt_term(+Role, +Term) is semidet.
%! hdt_term(+Role, -Term) is nondet.

hdt_term(Role, Name) :-
  hdt_term(Role, Name, _).


%! hdt_term(+Role, +Term, ?G) is semidet.
%! hdt_term(+Role, -Term, ?G) is nondet.
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

hdt_term(Role, Term, Hdt0) :-
  hdt_blob(Hdt0, Hdt),
  hdt_term_blob(Hdt, Role, Term).

% name
hdt_term_blob(Hdt, name, Name) :-
  hdt_term_blob(Hdt, iri, Name).
hdt_term_blob(Hdt, name, Name) :-
  hdt_term_blob(Hdt, literal, Name).
% node
hdt_term_blob(Hdt, node, Node) :-
  (   var(Node)
  ->  (   hdt_term_(Hdt, shared, Node)
      ;   hdt_term_(Hdt, subject, Node)
      ;   pre_object(Hdt, Node, Atom),
          hdt_term_(Hdt, object, Atom),
          post_object(Node, Atom)
      )
  ;   hdt_(Hdt, content, Node, _, _)
  ->  true
  ;   pre_object(Hdt, Node, Atom),
      hdt_(Hdt, content, _, _, Atom),
      post_object(Node, Atom)
  ->  true
  ).
% object
hdt_term_blob(Hdt, object, O) :-
  (   var(O)
  ->  (   hdt_term_(Hdt, shared, O)
      ;   pre_object(Hdt, O, Atom),
          hdt_term_(Hdt, object, Atom),
          post_object(O, Atom)
      )
  ;   pre_object(Hdt, O, Atom),
      hdt_(Hdt, content, _, _, Atom),
      post_object(O, Atom)
  ->  true
  ).
% predicate
hdt_term_blob(Hdt, predicate, P) :-
  (   var(P)
  ->  hdt_term_(Hdt, predicate, Atom),
      Atom = P
  ;   hdt_(Hdt, content, _, P, _)
  ->  true
  ).
% shared
hdt_term_blob(Hdt, shared, Shared) :-
  (   var(Shared)
  ->  hdt_term_(Hdt, shared, Shared)
  ;   hdt_(Hdt, content, Shared, _, _),
      hdt_(Hdt, content, _, _, Shared)
  ->  true
  ).
% subject
hdt_term_blob(Hdt, subject, S) :-
  (   var(S)
  ->  (   hdt_term_(Hdt, shared, S)
      ;   hdt_term_(Hdt, subject, S)
      )
  ;   hdt_(Hdt, content, S, _, _)
  ->  true
  ).



%! hdt_term_count(+Role, -Count:nonneg) is nondet.
%! hdt_term_count(-Role, -Count:nonneg) is multi.

hdt_term_count(node, Count) :-
  hdt_term_count(node, Count, _).


%! hdt_term_count(+Role, -Count:nonneg, ?G) is nondet.
%! hdt_term_count(-Role, -Count:nonneg, ?G) is multi.

hdt_term_count(Role, Count, Hdt0) :-
  hdt_blob(Hdt0, Hdt),
  hdt_term_count_blob(Hdt, Role, Count).

hdt_term_count_blob(Hdt, term, Count) :-
  maplist(hdt_term_count_blob(Hdt), [predicate,node], Counts),
  sum_list(Counts, Count).
hdt_term_count_blob(Hdt, node, Count) :-
  maplist(hdt_term_count_blob(Hdt), [object,shared,subject], Counts),
  sum_list(Counts, Count).
hdt_term_count_blob(Hdt, object, Count) :-
  hdt_header_(_, '<http://rdfs.org/ns/void#distinctObjects>', Count0, Hdt),
  Count0 = Count^^_.
hdt_term_count_blob(Hdt, predicate, Count) :-
  hdt_header_(_, '<http://rdfs.org/ns/void#properties>', Count0, Hdt),
  Count0 = Count^^_.
hdt_term_count_blob(Hdt, shared, Count) :-
  hdt_header_(_, '<http://purl.org/HDT/hdt#dictionarynumSharedSubjectObject>', Count0, Hdt),
  Count0 = Count^^_.
hdt_term_count_blob(Hdt, subject, Count) :-
  hdt_header_(_, '<http://rdfs.org/ns/void#distinctSubjects>', Count0, Hdt),
  Count0 = Count^^_.



%! hdt_term_id(+Role, -Id) is nondet.

hdt_term_id(Role, Id) :-
  hdt_term_id(Role, Id, _).


%! hdt_term_id(+Role, -Id, ?G) is nondet.

hdt_term_id(Role1, Id, Hdt) :-
  hdt_term(Role1, Term, Hdt),
  narrow_role(Role1, Role2),
  hdt_dict(Role2, Term, Id, Hdt).

narrow_role(node, object).
narrow_role(node, subject).
narrow_role(object, object).
narrow_role(predicate, predicate).
narrow_role(shared, object).
narrow_role(shared, subject).
narrow_role(subject, subject).



%! hdt_term_rnd(+Role, -Term) is nondet.

hdt_term_rnd(Role, Term) :-
  hdt_term_rnd(Role, Term, _).


%! hdt_term_rnd(+Role, -Term, ?G) is nondet.

hdt_term_rnd(Role, Term, Hdt0) :-
  hdt_blob(Hdt0, Hdt),
  hdt_term_rnd_(Hdt, Role, Term).



%! hdt_term_rnd_id(+Role, -Id) is nondet.

hdt_term_rnd_id(Role, Id) :-
  hdt_term_rnd_id(Role, Id, _).


%! hdt_term_rnd_id(+Role, -Id, ?G) is nondet.

hdt_term_rnd_id(Role, Id, Hdt0) :-
  hdt_blob(Hdt0, Hdt),
  hdt_term_rnd_id_(Hdt, Role, Id).



% PREFIX SEARCH %

%! hdt_prefix(+Role, +Prefix, +Term) is semidet.
%! hdt_prefix(+Role, +Prefix, -Term) is nondet.

hdt_prefix(Role, Prefix, Term) :-
  hdt_prefix(Role, Prefix, Term, _).


%! hdt_prefix(+Role, +Prefix, +Term, ?G) is semidet.
%! hdt_prefix(+Role, +Prefix, -Term, ?G) is nondet.

hdt_prefix(Role, Prefix, Term, Hdt0) :-
  hdt_blob(Hdt0, Hdt),
  hdt_prefix_(Hdt, Role, Prefix, Term).



%! hdt_prefix_id(+Role, +Prefix:atom, ?Id:nonneg) is nondet.

hdt_prefix_id(Role, Prefix, Id) :-
  hdt_prefix_id(Role, Prefix, Id, _).


%! hdt_prefix_id(+Role, +Prefix:atom, ?Id:nonneg, ?G) is nondet.

hdt_prefix_id(Role, Prefix, Id, Hdt0) :-
  hdt_blob(Hdt0, Hdt),
  hdt_prefix_id_(Hdt, Role, Prefix, Id).





% OTHERS %

%! hdt_header(?S, ?P, ?O) is nondet.
%! hdt_header(?S, ?P, ?O, ?G) is nondet.
%
% True if 〈S,P,O〉 is a triple in the header of Hdt.

hdt_header(S, P, O) :-
  hdt_header(S, P, O, _).


hdt_header(S, P, O, Hdt0) :-
  hdt_blob(Hdt0, Hdt),
  hdt_header_(S, P, O, Hdt).

hdt_header_(S, P, O, Hdt) :-
  pre_object(Hdt, O, Atom),
  hdt_(Hdt, header, S, P, Atom),
  header_object(Atom, O).

header_object(Atom1, O) :-
  atom_concat('"', Atom2, Atom1),
  atom_concat(Atom3, '"', Atom2),
  header_untyped_object(Atom3, O).
header_object(O, O).

header_untyped_object(Atom, O) :-
  catch(
    xsd_number_string(N, Atom),
    error(syntax_error(xsd_number), _),
    fail
  ), !,
  (   integer(N)
  ->  rdf_equal(O, N^^xsd:integer)
  ;   rdf_equal(O, N^^xsd:float)
  ).
header_untyped_object(Atom, O) :-
  catch(
    xsd_time_string(Term, Type, Atom),
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

hdt_property(elements(_)).
hdt_property(mapping(_)).
hdt_property(max_id(_)).
hdt_property(max_object_id(_)).
hdt_property(max_predicate_id(_)).
hdt_property(max_subject_id(_)).
hdt_property(objects(_)).
hdt_property(predicates(_)).
hdt_property(shared(_)).
hdt_property(subjects(_)).





% HELPERS %

%! hdt_blob(+Various, -Hdt:blob) is det.
%! hdt_blob(-G:atom, -Hdt:blob) is nondet.
%
% Allows opened HDT files to be denoted by either (1) the opaque
% handle, or (2) the graph name (supporting RDF prefix expansion).

hdt_blob(G, Hdt) :-
  (atom(G) ; var(G)), !,
  hdt_graph(Hdt, G).
hdt_blob(Hdt, Hdt).
  


%! iri_id(+Hdt:blob, +Role:atom, ?Iri:atom, ?Id:positive_integer)

iri_id(_, _, Iri, _) :-
  atom(Iri), !.
iri_id(Hdt, Role, Iri, Id) :-
  hdt_dict_(Hdt, Role, Iri, Id).



%! post_object(?O, +Atom) is det.

post_object(O, Atom1) :-
  atom_concat('"', Atom2, Atom1), !,
  atom_codes(Atom2, Codes),
  phrase(post_literal(O), Codes).
post_object(NonLiteral, NonLiteral).

post_literal(Lit) -->
  string(Codes1),
  "\"", !,
  {string_codes(Lex, Codes1)},
  (   "^"
  ->  "^<",
      string(Codes2),
      ">",
      {
        atom_codes(D, Codes2),
        rdf11:post_object(Lit, literal(type(D,Lex)))
      }
  ;   "@"
  ->  remainder(Codes2),
      {
        atom_codes(LTag, Codes2),
        rdf11:post_object(Lit, literal(lang(LTag,Lex)))
      }
  ).



%! pre_object(+Hdt:blob, ?O, -Atom) is det.
%
% This helper predicate implements the feature that literals can be
% entered partially.  Specifically, it is possible to only supply
% their lexical form, and match their language tag or datatype IRI.

pre_object(_, Var, _) :-
  var(Var), !.
pre_object(Hdt, Lex@LTag, Atom) :- !,
  must_be(string, Lex),
  (   var(LTag)
  ->  atomic_list_concat(['"',Lex,'"@'], Prefix),
      hdt_prefix_(Hdt, object, Prefix, O),
      pre_object(Hdt, O, Atom)
  ;   atomic_list_concat(['"',Lex,'"@',LTag], Atom)
  ).
pre_object(Hdt, Val^^D, Atom) :- !,
  must_be(ground, Val),
  rdf11:rdf_lexical_form(Val^^D, Lex^^D),
  (   var(D)
  ->  atomic_list_concat(['"',Lex,'"^^<'], Prefix),
      hdt_prefix_(Hdt, object, Prefix, O),
      pre_object(Hdt, O, Atom)
  ;   atomic_list_concat(['"',Lex,'"^^<',D,>], Atom)
  ).
pre_object(_, NonLiteral, NonLiteral) :-
  must_be(atom, NonLiteral).



%! register_graph(+Hdt:blob, +G:atom) is det.

register_graph_(Hdt, G) :-
  must_be(atom, G),
  with_mutex(hdt_graph_, (
    (   hdt_graph_(Hdt, _)
    ->  existence_error(hdt, Hdt)
    ;   hdt_graph_(_, G)
    ->  existence_error(graph, G)
    ;   assert(hdt_graph_(Hdt, G))
    )
  )).



% MESSAGES %

:- multifile
    prolog:error_message//1.

prolog:error_message(hdt_error(Message)) -->
  [ "HDT: ~w"-[Message] ].
