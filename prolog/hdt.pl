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
     hdt_dict/2,        % ?Term, ?Id
     hdt_dict/3,        % ?Term, ?Id, ?G

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
     hdt_term/2,        % ?Role, ?Term
     hdt_term/3,        % ?Role, ?Term, ?G
     hdt_term_count/2,  % ?Role, ?Count
     hdt_term_count/3,  % ?Role, ?Count, ?G
     hdt_term_id/1,     % ?Id
     hdt_term_id/2,     % ?Id, ?G
     hdt_term_rnd/2,    % +Role, -Term
     hdt_term_rnd/3,    % +Role, -Term, ?G
     hdt_term_rnd_id/1, % -Id
     hdt_term_rnd_id/2, % -Id, ?G

   % PREFIX SEARCH
     hdt_prefix/3,      % ?Role, +Prefix, ?Term
     hdt_prefix/4,      % ?Role, +Prefix, ?Term, ?G
     hdt_prefix_id/2,   % +Prefix, ?Id
     hdt_prefix_id/3,   % +Prefix, ?Id, ?G

   % OTHERS
     hdt_header/3,      % ?S, ?P, ?O
     hdt_header/4,      % ?S, ?P, ?O, ?G
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

:- multifile
    error:has_type/2.

error:has_type(hdt_graph, G) :-
  error:has_type(atom, G).
error:has_type(hdt_graph, G) :-
  error:has_type(blob, G).

error:has_type(hdt_id, id(Role,N)) :-
  error:has_type(hdt_role, Role),
  error:has_type(positive_integer, N).

error:has_type(hdt_role, Role) :-
  error:has_type(oneof([predicate,shared,sink,source]), Role).

error:has_type(rdf_bnode, BNode) :-
  error:has_type(atom, BNode).

error:has_type(rdf_iri, Iri) :-
  error:has_type(atom, Iri).

error:has_type(rdf_literal, _^^D) :-
  error:has_type(rdf_iri, D).
error:has_type(rdf_literal, Lex@D) :-
  error:has_type(string, Lex),
  error:has_type(rdf_iri, D).

error:has_type(rdf_object, O) :-
  error:has_type(rdf_term, O).

error:has_type(rdf_predicate, P) :-
  error:has_type(rdf_iri, P).

error:has_type(rdf_role, Role) :-
  error:has_type(hdt_role, Role).
error:has_type(rdf_role, Role) :-
  error:has_type(oneof([bnode,iri,literal,name,node,object,subject]), Role).

error:has_type(rdf_subject, S) :-
  error:has_type(rdf_bnode, S).
error:has_type(rdf_subject, S) :-
  error:has_type(iri, S).

error:has_type(rdf_term, Term) :-
  error:has_type(rdf_bnode, Term).
error:has_type(rdf_term, Term) :-
  error:has_type(rdf_iri, Term).
error:has_type(rdf_term, Term) :-
  error:has_type(rdf_literal, Term).

:- rdf_meta
   hdt(r, r, o),
   hdt(r, r, o, r),
   hdt_close(r),
   hdt_count(r, r, o, -),
   hdt_count(r, r, o, -, r),
   hdt_count_id(?, ?, ?, -, r),
   hdt_dict(r, ?),
   hdt_dict(r, ?, r),
   hdt_graph(?, r),
   hdt_header(r, r, o),
   hdt_header(r, r, o, r),
   hdt_id(?, ?, ?, r),
   hdt_rnd(r, r, o),
   hdt_rnd(r, r, o, r),
   hdt_rnd_id(?, ?, ?, r),
   hdt_term(+, t),
   hdt_term(+, t, r),
   hdt_term_count(+, ?, r),
   hdt_term_id(?, r),
   hdt_term_rnd(+, -, r),
   hdt_term_rnd_id(-, r),
   hdt_prefix(+, +, t),
   hdt_prefix(+, +, t, r),
   hdt_prefix_id(?, ?, r).





% FILE OPERATIONS %

%! hdt_close(+G:hdt_graph) is det.
%
% Close an HDT that was previously opened with hdt_open/[2,3].

hdt_close(G) :-
  with_mutex(hdt_graph_, (
    ((atom(G) ; var(G)) -> hdt_graph_(Hdt, G) ; Hdt = G),
    hdt_close_(Hdt),
    retractall(hdt_graph_(Hdt,_))
  )).



%! hdt_create(+RdfFile:atom) is det.
%! hdt_create(+RdfFile:atom, +Options:list(compound)) is det.
%
% Create an HDT file from an uncompressed N-Triples file.
%
% The following Options are supported:
%
%    * base_uri(+atom)
%
%    The base URI that is used for generating HDT header properties.
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

hdt_graph(Hdt, G) :-
  hdt_graph_(Hdt, G).



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
%   * graph(?atom)
%
%   An alias by which one can refer to the opaque Hdt handle.  This
%   alias acts as a name for the graph, or set of triples, that is
%   contained in the HDT file.
%
%   By default this is the URI version of the HdtFile.
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

%! hdt_dict(+Term:rdf_term, +Id:hdt_id) is semidet.
%! hdt_dict(+Term:rdf_term, -Id:hdt_id) is det.
%! hdt_dict(-Term:rdf_term, +Id:hdt_id) is det.

hdt_dict(Term, Id) :-
  hdt_dict(Term, Id, _).


%! hdt_dict(+Term:rdf_term, +Id:hdt_id, ?G:hdt_graph) is semidet.
%! hdt_dict(+Term:rdf_term, -Id:hdt_id, ?G:hdt_graph) is det.
%! hdt_dict(-Term:rdf_term, +Id:hdt_id, ?G:hdt_graph) is det.

hdt_dict(Term, id(Role,Id), G) :- !,
  hdt_blob(G, Hdt),
  (   Role == object
  ->  pre_object(Hdt, Term, Atom),
      hdt_dict_(Hdt, Role, Atom, Id),
      post_object(Term, Atom)
  ;   hdt_dict_(Hdt, Role, Term, Id)
  ).





% TRIPLES %

%! hdt(?S:rdf_subject, ?P:rdf_predicate, ?O:rdf_object) is nondet.

hdt(S, P, O) :-
  hdt(S, P, O, _).


%! hdt(?S:rdf_subject, ?P:rdf_predicate, ?O:rdf_object, ?G:hdt_graph) is nondet.
%
% True if 〈S,P,O〉 is an RDF triple in HDT.

hdt(S, P, O, G) :-
  hdt_blob(G, Hdt),
  pre_object(Hdt, O, Atom),
  hdt_(Hdt, content, S, P, Atom),
  post_object(O, Atom).



%! hdt_count(?S:rdf_subject, ?P:rdf_predicate, ?O:rdf_object,
%!           +Count:nonneg) is semidet.
%! hdt_count(?S:rdf_subject, ?P:rdf_predicate, ?O:rdf_object,
%!           -Count:nonneg) is det.

hdt_count(S, P, O, Count) :-
  hdt_count(S, P, O, Count, _).


%! hdt_count(?S:rdf_subjecr, ?P:rdf_predicate, ?O:rdf_object, +Count:nonneg,
%!           ?G:hdt_graph) is semidet.
%! hdt_count(?S:rdf_subject, ?P:rdf_predicate, ?O:rdf_object, -Count:nonneg,
%!           ?G:hdt_graph) is det.
%
% True if Count is the number of matches of the Triple Pattern〈S,P,O〉
% on the graph stored in HDT.

hdt_count(S, P, O, Count, G) :-
  hdt_blob(G, Hdt),
  pre_object(Hdt, O, Atom),
  hdt_count_(Hdt, S, P, Atom, Count).



%! hdt_count_id(?SId:hdt_id, ?PId:hdt_id, ?OId:hdt_id,
%!              +Count:nonneg) is semidet.
%! hdt_count_id(?SId:hdt_id, ?PId:hdt_id, ?OId:hdt_id, -Count:nonneg) is det.

hdt_count_id(SId, PId, OId, Count) :-
  hdt_count_id(SId, PId, OId, Count, _).


%! hdt_count_id(?SId:hdt_id, ?PId:hdt_id, ?OId:hdt_id, +Count:nonneg,
%!              ?G:hdt_graph) is semidet.
%! hdt_count_id(?SId:hdt_id, ?PId:hdt_id, ?OId:hdt_id, -Count:nonneg,
%!              ?G:hdt_graph) is det.

hdt_count_id(SId, PId, OId, Count, G) :-
  hdt_blob(G, Hdt),
  hdt_count_id_(Hdt, SId, PId, OId, Count).



%! hdt_id(?SId:hdt_id, ?PId:hdt_id, ?OId:hdt_id) is nondet.

hdt_id(SId, PId, OId) :-
  hdt_id(SId, PId, OId, _).


%! hdt_id(?SId:hdt_id, ?PId:hdt_id, ?OId:hdt_id, ?G:hdt_graph) is nondet.
%
% True if 〈SId,SIP,SIO〉 is an integer triple in Hdt.

hdt_id(SId, PId, OId, G) :-
  hdt_blob(G, Hdt),
  hdt_id_(Hdt, SId, PId, OId).



%! hdt_rnd(?S:rdf_subject, ?P:rdf_predicate, ?O:rdf_object) is nondet.

hdt_rnd(S, P, O) :-
  hdt_rnd(S, P, O, _).


%! hdt_rnd(?S:rdf_subject, ?P:rdf_predicate, ?O:rdf_object,
%!         ?G:hdt_graph) is nondet.

hdt_rnd(S, P, O, G) :-
  hdt_blob(G, Hdt),
  pre_object(Hdt, O, Atom),
  hdt_rnd_(Hdt, S, P, Atom),
  post_object(O, Atom).



%! hdt_rnd_id(?SId:hdt_id, ?PId:hdt_id, ?OId:hdt_id) is nondet.

hdt_rnd_id(SId, PId, OId) :-
  hdt_rnd_id(SId, PId, OId, _).


%! hdt_rnd_id(?SId:hdt_id, ?PId:hdt_id, ?OId:hdt_id, G:hdt_graph) is nondet.

hdt_rnd_id(SId, PId, OId, G) :-
  hdt_blob(G, Hdt),
  hdt_rnd_id_(Hdt, SId, PId, OId).





% TERMS %

%! hdt_term(+Role:rdf_role, +Term:rdf_term) is semidet.
%! hdt_term(+Role:rdf_role, -Term:rdf_term) is nondet.
%! hdt_term(-Role:rdf_role, +Term:rdf_term) is nondet.
%! hdt_term(-Role:rdf_role, -Term:rdf_term) is nondet.

hdt_term(Role, Term) :-
  hdt_term(Role, Term, _).


%! hdt_term(+Role:rdf_role, +Term:rdf_term, ?G:hdt_graph) is semidet.
%! hdt_term(+Role:rdf_role, -Term:rdf_term, ?G:hdt_graph) is nondet.
%! hdt_term(-Role:rdf_role, +Term:rdf_term, ?G:hdt_graph) is nondet.
%! hdt_term(-Role:rdf_role, -Term:rdf_term, ?G:hdt_graph) is nondet.
%
%  @arg Role is either of the following term types:
%
%    * bnode
%
%      Terms that are blank nodes.
%
%    * iri
%
%      Terms that are IRIs.
%
%    * literal
%
%      Terms that are literals.
%
%    * name
%
%      Terms that are IRIs or literals.
%
%    * node
%
%      Terms that appear in the subject or object position.
%
%    * object
%
%      Terms that appear in the object position.
%
%    * predicate
%
%      Terms that appear in the predicate position.
%
%    * shared
%
%      Terms that appear in the subject and object position.
%
%    * sink
%
%      Terms that only appear in the object position.
%
%    * source
%
%      Terms that only appear in the subject position.
%
%    * subject
%
%      Terms that appear in the subject position.
%
%    * term
%
%      Terms that appear somewhere.

hdt_term(Role, Term, G) :-
  hdt_blob(G, Hdt),
  hdt_term_blob(Hdt, Role, Term).

% TBD: bnode
% TBD: iri
% TBD: literal
% name
hdt_term_blob(Hdt, name, Name) :-
  hdt_term_blob(Hdt, iri, Name).
hdt_term_blob(Hdt, name, Name) :-
  hdt_term_blob(Hdt, literal, Name).
% node
hdt_term_blob(Hdt, node, Node) :-
  hdt_term_blob(Hdt, shared, Node).
hdt_term_blob(Hdt, node, Node) :-
  hdt_term_blob(Hdt, source, Node).
hdt_term_blob(Hdt, node, Node) :-
  hdt_term_blob(Hdt, sink, Node).
% object
hdt_term_blob(Hdt, object, O) :-
  hdt_term_blob(Hdt, shared, O).
hdt_term_blob(Hdt, object, O) :-
  hdt_term_blob(Hdt, sink, O).
% predicate
hdt_term_blob(Hdt, predicate, P) :-
  (   var(P)
  ->  hdt_term_(Hdt, predicate, P)
  ;   hdt_(Hdt, content, _, P, _)
  ).
% shared
hdt_term_blob(Hdt, shared, Shared) :-
  (   var(Shared)
  ->  hdt_term_(Hdt, shared, Atom)
  ;   % O: Determine this based on ID offset.
      pre_object(Hdt, Shared, Atom),
      hdt_(Hdt, content, Atom, _, _),
      hdt_(Hdt, content, _, _, Atom)
  ),
  post_object(Shared, Atom).
% sink
hdt_term_blob(Hdt, sink, Sink) :-
  (   var(Sink)
  ->  hdt_term_(Hdt, sink, Atom)
  ;   % O: Determine this based on ID offset.
      pre_object(Hdt, Sink, Atom),
      hdt_(Hdt, content, _, _, Atom),
      \+ hdt_(Hdt, content, Atom, _, _)
  ),
  post_object(Sink, Atom).
% source
hdt_term_blob(Hdt, source, Source) :-
  (   var(Source)
  ->  hdt_term_(Hdt, source, Atom)
  ;   % O: Determine this based on ID offset.
      pre_object(Hdt, Source, Atom),
      hdt_(Hdt, content, Atom, _, _),
      \+ hdt_(Hdt, content, _, _, Atom)
  ),
  post_object(Source, Atom).
% subject
hdt_term_blob(Hdt, subject, S) :-
  hdt_term_blob(Hdt, source, S).
hdt_term_blob(Hdt, subject, S) :-
  hdt_term_blob(Hdt, shared, S).
% term
hdt_term_blob(Hdt, term, Term) :-
  hdt_term_blob(Hdt, node, Term).
hdt_term_blob(Hdt, term, Term) :-
  hdt_term_blob(Hdt, predicate, Term),
  \+ hdt_term_blob(Hdt, node, Term).



%! hdt_term_count(+Role:rdf_role, +Count:nonneg) is semidet.
%! hdt_term_count(+Role:rdf_role, -Count:nonneg) is det.
%! hdt_term_count(-Role:rdf_role, +Count:nonneg) is nondet.
%! hdt_term_count(-Role:rdf_role, -Count:nonneg) is multi.

hdt_term_count(Role, Count) :-
  hdt_term_count(Role, Count, _).


%! hdt_term_count(+Role:rdf_role, +Count:nonneg, ?G:hdt_graph) is semidet.
%! hdt_term_count(+Role:rdf_role, -Count:nonneg, ?G:hdt_graph) is nondet.
%! hdt_term_count(-Role:rdf_role, +Count:nonneg, ?G:hdt_graph) is nondet.
%! hdt_term_count(-Role:rdf_role, -Count:nonneg, ?G:hdt_graph) is multi.

hdt_term_count(Role, Count, G) :-
  hdt_blob(G, Hdt),
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



%! hdt_term_id(-Id:hdt_id) is nondet.

hdt_term_id(Id) :-
  hdt_term_id(Id, _).


%! hdt_term_id(-Id:hdt_id, ?G:hdt_graph) is nondet.

hdt_term_id(id(Role,Id), Hdt) :-
  % O: reimplement using S,P,O, and SO offsets
  hdt_term(Role, Term, Hdt),
  hdt_dict(Term, id(Role,Id), Hdt).



%! hdt_term_rnd(+Role:hdt_role, -Term:rdf_term) is nondet.

hdt_term_rnd(Role, Term) :-
  hdt_term_rnd(Role, Term, _).


%! hdt_term_rnd(+Role:hdt_role, -Term:rdf_term, ?G:hdt_graph) is nondet.

hdt_term_rnd(Role, Term, G) :-
  hdt_blob(G, Hdt),
  hdt_term_rnd_(Hdt, Role, Term).



%! hdt_term_rnd_id(-Id:hdt_id) is nondet.

hdt_term_rnd_id(Id) :-
  hdt_term_rnd_id(Id, _).


%! hdt_term_rnd_id(-Id:hdt_id, ?G:hdt_graph) is nondet.

hdt_term_rnd_id(id(Role,Id), G) :-
  hdt_blob(G, Hdt),
  hdt_term_rnd_id_(Hdt, Role, Id).



% PREFIX SEARCH %

%! hdt_prefix(+Role:hdt_role, +Prefix:atom, +Term:rdf_term) is semidet.
%! hdt_prefix(+Role:hdt_role, +Prefix:atom, -Term:rdf_term) is nondet.
%! hdt_prefix(-Role:hdt_role, +Prefix:atom, +Term:rdf_term) is nondet.
%! hdt_prefix(-Role:hdt_role, +Prefix:atom, -Term:rdf_term) is nondet.

hdt_prefix(Role, Prefix, Term) :-
  hdt_prefix(Role, Prefix, Term, _).


%! hdt_prefix(+Role:hdt_role, +Prefix:atom, +Term:rdf_term,
%!            ?G:hdt_graph) is semidet.
%! hdt_prefix(+Role:hdt_role, +Prefix:atom, -Term:rdf_term,
%!            ?G:hdt_graph) is nondet.
%! hdt_prefix(-Role:hdt_role, +Prefix:atom, +Term:rdf_term,
%!            ?G:hdt_graph) is nondet.
%! hdt_prefix(-Role:hdt_role, +Prefix:atom, -Term:rdf_term,
%!            ?G:hdt_graph) is nondet.

hdt_prefix(Role, Prefix, Term, G) :-
  hdt_blob(G, Hdt),
  hdt_prefix_(Hdt, Role, Prefix, Term).



%! hdt_prefix_id(+Prefix:atom, ?Id:hdt_id) is nondet.

hdt_prefix_id(Prefix, Id) :-
  hdt_prefix_id(Prefix, Id, _).


%! hdt_prefix_id(+Prefix:atom, ?Id:hdt_id, ?G:hdt_graph) is nondet.

hdt_prefix_id(Prefix, id(Role,Id), G) :-
  hdt_blob(G, Hdt),
  hdt_prefix_id_(Hdt, Role, Prefix, Id).





% OTHERS %

%! hdt_header(?S:rdf_subject, ?P:rdf_predicate, ?O:rdf_object) is nondet.
%! hdt_header(?S:rdf_subject, ?P:rdf_predicate, ?O:rdf_object,
%!            ?G:hdt_graph) is nondet.
%
% True if 〈S,P,O〉 is a triple in the header of Hdt.

hdt_header(S, P, O) :-
  hdt_header(S, P, O, _).


hdt_header(S, P, O, G) :-
  hdt_blob(G, Hdt),
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



%! hdt_property(+Property:compound, ?G:hdt_graph) is semidet.
%! hdt_property(?Property:compound, ?G:hdt_graph) is nondet.
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

hdt_property(Property, G) :-
  hdt_blob(G, Hdt),
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

%! hdt_blob(+G:hdt_graph, -Hdt:blob) is det.
%! hdt_blob(-G:atom, -Hdt:blob) is nondet.
%
% Allows opened HDT files to be denoted by either (1) the opaque
% handle, or (2) the graph name (supporting RDF prefix expansion).

hdt_blob(G, Hdt) :-
  (atom(G) ; var(G)), !,
  hdt_graph(Hdt, G).
hdt_blob(Hdt, Hdt).
  


%! iri_id(+Hdt:blob, ?Iri:atom, ?Id:hdt_id)

iri_id(_, _, id(_,Iri)) :-
  atom(Iri), !.
iri_id(Hdt, Iri, id(Role,Id)) :-
  hdt_dict_(Hdt, Role, Iri, Id).



%! post_object(?O:rdf_object, +Atom:atom) is det.

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
  ;   {rdf_global_object(Lex^^xsd:string, Lit)}
  ).



%! pre_object(+Hdt:blob, ?O:rdf_object, -Atom:atom) is det.
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
      hdt_prefix_(Hdt, sink, Prefix, O),
      pre_object(Hdt, O, Atom)
  ;   atomic_list_concat(['"',Lex,'"@',LTag], Atom)
  ).
pre_object(Hdt, Val^^D, Atom) :- !,
  must_be(ground, Val),
  rdf11:rdf_lexical_form(Val^^D, Lex^^D),
  (   var(D)
  ->  atomic_list_concat(['"',Lex,'"^^<'], Prefix),
      hdt_prefix_(Hdt, sink, Prefix, O),
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
