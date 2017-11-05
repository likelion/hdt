:- module(
  hdt,
  [
  % RDF/HDT
    hdt_close/1,            % +Hdt
    hdt_create_from_file/2, % +HdtFile, ?RdfFile
    hdt_create_from_file/3, % +HdtFile, ?RdfFile, +Options
    hdt_open/2,             % +Hdt, -HdtFile
    hdt_open/3,             % +Hdt, -HdtFile, +Options
    hdt_property/2,         % +Hdt, ?Property
  % TERMS
    hdt_term/3,             % +Hdt, +Role, ?Term
    hdt_term/4,             % +Hdt, +Role, -LeafRole, ?Term
    hdt_term_count/3,       % +Hdt, +Role, ?Count
    hdt_term_id/4,          % +Hdt, +Role, ?Term, ?IdTerm
    hdt_term_prefix/4,      % +Hdt, +Role, +Prefix, ?Term
    hdt_term_prefix/5,      % +Hdt, +Role, +Prefix, -LeafRole, ?Term
    hdt_term_random/3,      % +Hdt, +Role, -Term
    hdt_term_random/4,      % +Hdt, +Role, -LeafRole, -Term
  % TRIPLES
    hdt/4,                  % +Hdt, ?S, ?P, ?O
    hdt_triple/4,           % +Hdt, ?S, ?P, ?O
    hdt_triple_count/5,     % +Hdt, ?S, ?P, ?O, ?Count
    hdt_triple_id/3,        % +Hdt, ?TermTriple, ?IdTriple
    hdt_triple_lexical/4,   % +Hdt, ?S, ?P, ?OLex
    hdt_triple_random/4,    % +Hdt, ?S, ?P, ?O
  % OPERATORS
    op(110, xfx, @),        % must be above `.'
    op(650, xfx, ^^)        % must be above `:'
  ]
).

/** <module> HDT

@author Jan Wielemaker
@author Wouter Beek
@version 2017/09-2017/11
*/

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(dcg/basics)).
:- use_module(library(dif)).
:- use_module(library(filesex)).
:- use_module(library(lists)).
:- use_module(library(option)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(uri)).

:- use_foreign_library(foreign(hdt4pl)).

:- meta_predicate
    closure0(2, ?, ?),
    closure0(3, ?, ?, +).

:- rdf_meta
   atom_literal_(+, o),
   hdt_term(+, +, o),
   hdt_term(+, +, -, o),
   hdt_term_id(+, o, ?),
   hdt_term_prefix(+, +, +, o),
   hdt_term_prefix(+, +, +, -, o),
   hdt(+, r, r, o),
   hdt_triple(+, r, r, o),
   hdt_triple_count(+, r, r, o, ?),
   hdt_triple_id(+, t, ?),
   hdt_triple_lexical(+, r, r, o),
   hdt_triple_random(+, r, r, o),
   literal_codes(+, o).





%! hdt_close(+Hdt:blob) is det.
%
% Closes the HDT file by its opaque handle (`Hdt`).

hdt_close(Hdt) :-
  hdt_close_(Hdt).



%! hdt_create_from_file(?HdtFile:atom, +RdfFile) is det.
%! hdt_create_from_file(?HdtFile:atom, +RdfFile, +Options:list(compound)) is det.
%
% Create an HDT file from an uncompressed N-Triples file.
%
% The following Options are supported:
%
%    * base_uri(+atom)
%
%    The base URI that is used for generating HDT header properties.

hdt_create_from_file(HdtFile, RdfFile) :-
  hdt_create_from_file(HdtFile, RdfFile, []).


hdt_create_from_file(HdtFile, RdfFile, Options1) :-
  % Determine the serialization format of the RDF file.
  ignore(option(format(Format), Options1)),
  (   ground(Format)
  ->  Options2 = Options1
  ;   once((
        atomic_list_concat(Comps, ., RdfFile),
        member(Ext, Comps),
        extension_format(Ext, Format)
      )),
      merge_options(Options1, [format(Format)], Options2)
  ),

  % The HDT output file.
  (   var(HdtFile)
  ->  directory_file_path(Dir, RdfLocal, RdfFile),
      atomic_list_concat(Segments1, ., RdfLocal),
      % SWI does not auto-detect that this is deterministic :(
      once(append(Segments2, [_], Segments1)),
      atomic_list_concat(Segments2, ., Base),
      file_name_extension(Base, hdt, HdtLocal),
      directory_file_path(Dir, HdtLocal, HdtFile)
  ;   true
  ),

  % Set the base URI.
  ignore(option(base_uri(BaseUri), Options1)),
  (ground(BaseUri) -> true ; uri_file_name(BaseUri, HdtFile)),

  merge_options([base_uri(BaseUri)], Options2, Options3),
  hdt_create_from_file_(HdtFile, RdfFile, Options3).

extension_format(n3, n3).
extension_format(nq, nquads).
extension_format(nt, ntriples).
extension_format(ttl, turtle).



%! hdt_open(-Hdt:blob, +HdtFile:atom) is det.
%! hdt_open(-Hdt:blob, +HdtFile:atom, +Options:list(compound)) is det.
%
% Opens an existing HDT file `HdtFile', and unifies `Hdt' with a
% direct handle to it.
%
% @arg HdtFile An atom denoting a local HDT file.
%
% @arg Hdt An opaque symbol that acts as a handle to an opened HDT
%      files.  This symbol is subject to (atom) garbage collection.
%
% @arg Options The following Options are supported:
%
%      * access(+oneof([load,map]))
%
%      How the file is accessed.  On of `map` (map the file into
%      memory, default) or `load` (load the content of the file).
%
%      * indexed(+boolean)
%
%      Whether or not an index file is created.  Default is `true`.
%
%      An index is needed for partially instantiated calls to hdt/4,
%      but not for retrieving all statements (Triple Pattern 〈?,?,?〉).
%
%      The index is maintained in a file with extension `.index` in
%      the same directory as the HDT file.

hdt_open(Hdt, HdtFile) :-
  hdt_open(Hdt, HdtFile, []).


hdt_open(Hdt, HdtFile, Options) :-
  hdt_open_(Hdt, HdtFile, Options).



%! hdt_property(+Hdt:blob, +Property:compound) is semidet.
%! hdt_property(+Hdt:blob, -Property:compound) is nondet.
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

hdt_property(Hdt, Property) :-
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





% TERMS %

%! hdt_term(+Hdt:blob, +Role:atom, +Term:rdf_term) is semidet.
%! hdt_term(+Hdt:blob, +Role:atom, -Term:rdf_term) is nondet.
%! hdt_term(+Hdt:blob, +Role:atom, -LeafRole:atom, +Term:rdf_term) is semidet.
%! hdt_term(+Hdt:blob, +Role:atom, -LeafRole:atom, -Term:rdf_term) is nondet.

hdt_term(Hdt, Role, Term) :-
  hdt_term(Hdt, Role, _, Term).


% predicate
hdt_term(Hdt, predicate, predicate, Term) :-
  (   var(Term)
  ->  hdt_term_(Hdt, predicate, Term)
  ;   hdt_triple_(Hdt, content, _, Term, _)
  ).
% shared
hdt_term(Hdt, shared, shared, Term) :-
  (   var(Term)
  ->  hdt_term_(Hdt, shared, OTerm)
  ;   pre_term(Hdt, Term, OTerm),
      hdt_triple_(Hdt, content, OTerm, _, _),
      hdt_triple_(Hdt, content, _, _, OTerm)
  ),
  post_term(OTerm, Term).
% sink
hdt_term(Hdt, sink, sink, Term) :-
  (   var(Term)
  ->  hdt_term_(Hdt, sink, OTerm)
  ;   pre_term(Hdt, Term, OTerm),
      hdt_triple_(Hdt, content, _, _, OTerm),
      \+ hdt_triple_(Hdt, content, OTerm, _, _)
  ),
  post_term(OTerm, Term).
% source
hdt_term(Hdt, source, source, Term) :-
  (   var(Term)
  ->  hdt_term_(Hdt, source, OTerm)
  ;   pre_term(Hdt, Term, OTerm),
      hdt_triple_(Hdt, content, OTerm, _, _),
      \+ hdt_triple_(Hdt, content, _, _, OTerm)
  ),
  post_term(OTerm, Term).
% others: node, object, subject, term
hdt_term(Hdt, Role, LeafRole, Term) :-
  role_subrole(Role, SubRole),
  hdt_term(Hdt, SubRole, LeafRole, Term).



%! hdt_term_count(+Hdt:blob, +Role:atom, +N:nonneg) is semidet.
%! hdt_term_count(+Hdt:blob, +Role:atom, -N:nonneg) is det.

% object, predicate, shared, subject
hdt_term_count(Hdt, Role, N) :-
  header_role_property(Role, P), !,
  once(hdt_triple_(Hdt, header, _, P, Atom1)),
  atom_concat('"', Atom2, Atom1),
  atom_concat(Lex, '"', Atom2),
  atom_number(Lex, N).
% sink
hdt_term_count(Hdt, sink, N) :- !,
  maplist(hdt_term_count(Hdt), [shared,subject], [N1,N2]),
  N is N2 - N1.
% source
hdt_term_count(Hdt, source, N) :- !,
  maplist(hdt_term_count(Hdt), [object,shared], [N1,N2]),
  N is N1 - N2.
% others: node, object, subject, term
hdt_term_count(Hdt, Role, N) :- !,
  aggregate_all(set(LeafRole), role_leafrole(Role, LeafRole), LeafRoles),
  maplist(hdt_term_count(Hdt), LeafRoles, Ns),
  sum_list(Ns, N).

header_role_property(object, '<http://rdfs.org/ns/void#distinctObjects>').
header_role_property(predicate, '<http://rdfs.org/ns/void#properties>').
header_role_property(shared, '<http://purl.org/HDT/hdt#dictionarynumSharedSubjectObject>').
header_role_property(subject, '<http://rdfs.org/ns/void#distinctSubjects>').



%! hdt_term_prefix(+Hdt:blob, ?Role:atom, +Prefix:atom, ?Term:rdf_term) is nondet.
%! hdt_term_prefix(+Hdt:blob, ?Role:atom, +Prefix:atom, -LeafRole:atom, ?Term:rdf_term) is nondet.

hdt_term_prefix(Hdt, Role, Prefix, Term) :-
  hdt_term_prefix(Hdt, Role, Prefix, _, Term).


hdt_term_prefix(Hdt, Role, Prefix, LeafRole, Term) :-
  role_leafrole(Role, LeafRole),
  hdt_term_prefix_(Hdt, LeafRole, Prefix, Atom),
  (atom_prefix(Atom, Prefix) -> true ; !, fail),
  post_term(Atom, Term).



%! hdt_term_random(+Hdt:blob, +Role:atom, -Term:rdf_term) is nondet.
%! hdt_term_random(+Hdt:blob, +Role:atom, -LeafRole:atom, -Term:rdf_term) is nondet.

hdt_term_random(Hdt, Role, Term) :-
  hdt_term_random(Hdt, Role, _, Term).


hdt_term_random(Hdt, Role, LeafRole, Term) :-
  aggregate_all(set(LeafRole), role_leafrole(Role, LeafRole), LeafRoles),
  maplist(hdt_term_count(Hdt), LeafRoles, Counts),
  sum_list(Counts, Count),
  random_between(1, Count, Index),
  index_role(Index, Counts, LeafRoles, LeafRole),
  Rnd is random_float,
  hdt_term_random_(Hdt, LeafRole, Rnd, Atom),
  post_term(Atom, Term).

index_role(N, [Count|_], [Role|_], Role) :-
  N =< Count, !.
index_role(N1, [H|T1], [_|T2], Role) :-
  N2 is N1 - H,
  index_role(N2, T1, T2, Role).



%! hdt_term_id(+Hdt:blob, +Role:atom, ?Term:rdf_term, ?Id:compound) is det.

hdt_term_id(Hdt, Role, Term, Id) :-
  pre_term(Hdt, Term, Atom),
  hdt_term_id_(Hdt, Role, Atom, Id),
  post_term(Atom, Term).





% TRIPLES %

%! hdt(+Hdt:blob, ?S:rdf_nonliteral, ?P:rdf_iri, ?O:rdf_term) is nondet.
%
% @see Wrapper around hdt_triple/4.

hdt(Hdt, S, P, O) :-
  hdt_triple(Hdt, S, P, O).



%! hdt_triple(+Hdt:blob, ?S:rdf_nonliteral, ?P:rdf_iri, ?O:rdf_term) is nondet.
%
% True if 〈S,P,O〉 is a triple in Hdt.

hdt_triple(Hdt, S, P, O) :-
  pre_term(Hdt, O, OHdt),
  hdt_triple_(Hdt, content, S, P, OHdt),
  post_term(OHdt, O).



%! hdt_triple_count(+Hdt:blob, ?S:rdf_nonliteral, ?P:rdf_iri, ?O:rdf_term, -Count:nonneg) is det.

hdt_triple_count(Hdt, S, P, O, Count) :-
  pre_term(Hdt, O, OHdt),
  hdt_count_(Hdt, S, P, OHdt, Count), !.
hdt_triple_count(_, _, _, _, 0).



%! hdt_triple_id(+Hdt:blob, +TermTriple:rdf_term, -IdTriple:compound) is det.
%! hdt_triple_id(+Hdt:blob, -TermTriple:rdf_term, +IdTriple:compound) is det.

hdt_triple_id(
  Hdt,
  rdf(S,P,O),
  rdf(id(subject,SId),id(predicate,PId),id(object,OId))
) :-
  hdt_term_id(Hdt, subject, S, SId),
  hdt_term_id(Hdt, predicate, P, PId),
  hdt_term_id(Hdt, object, O, OId).



%! hdt_triple_lexical(+Hdt:blob, ?S, ?P, ?OLex) is nondet.
%
% True if 〈S,P,O〉 is a triple in Hdt, where object O is an
% uninterpreted Prolog compound term of the form
% `literal(lang(LTag:atom,Lex:atom))` or
% `literal(type(D:atom,Lex:atom))`.

hdt_triple_lexical(Hdt, S, P, OLex) :-
  pre_term_lexical(OLex, OHdt),
  hdt_triple_(Hdt, content, S, P, OHdt),
  post_term_lexical(OHdt, OLex).

% non-literal
pre_term_lexical(OLex, OHdt) :-
  atom(OLex), \+ boolean(OLex), !,
  OHdt = OLex.
% typed literal
pre_term_lexical(Lex^^D, OHdt) :-
  ground(Lex^^D), !,
  atomics_to_string(["\"",Lex,"\"^^<",D,">"], OHdt).
% language-tagged string
pre_term_lexical(Lex@LTag, OHdt) :-
  ground(Lex@LTag), !,
  atomics_to_string(["\"",Lex,"\"@",LTag], OHdt).
% not an RDF term (e.g., variable)
pre_term_lexical(_, _).

post_term_lexical(_, OLex) :-
  ground(OLex), !.
post_term_lexical(OHdt, OLex) :-
  OLex = OHdt.



%! hdt_triple_random(+Hdt:blob, ?S, ?P, ?O) is semidet.

hdt_triple_random(Hdt, S, P, O) :-
  pre_term(Hdt, O, OHdt),
  Rnd is random_float,
  hdt_triple_random_(Hdt, Rnd, S, P, OHdt),
  post_term(OHdt, O).





% GENERICS %

%! closure0(:Goal_2, +X, -Y) is nondet.
%
% Reflexive/transitive closure.

closure0(Goal_2, X, Y):-
  closure0(Goal_2, X, Y, [X]).

closure0(_, X, X, _).
closure0(Goal_2, X, Z, Hist):-
  call(Goal_2, X, Y),
  maplist(dif(Y), Hist),
  closure0(Goal_2, Y, Z, [Y|Hist]).



%! leafrole(+Role:atom) is semidet.
%! leafrole(-Role:atom) is multi.

leafrole(Role) :-
  role_subrole(_, Role),
  \+ role_subrole(Role, _).



%! post_term(+OHdt:compound, -Term:rdf_term) is det.

% no new binding
post_term(_, Term) :-
  ground(Term), !.
% non-literal
post_term(OHdt, O) :-
  atom(OHdt), !,
  O = OHdt.
% literal
post_term(OHdt, O) :-
  rdf_canonical_literal(OHdt, O).



%! pre_term(+Hdt:blob, ?O:rdf_term, -Atom:atom) is det.
%
% This helper predicate implements the feature that literals can be
% entered partially.  Specifically, it is possible to only supply
% their lexical form, and match their language tag or datatype IRI.
%
% From RDF terms to HDT terms:
%   - non-literal → non-literal
%   - literal → hdt-atom (e.g.,
%     `"\"1\"^^<http://www.w3.org/2001/XMLSchema#integer>"`
%   - var → var

% non-literal
pre_term(_, O1, O2) :-
  atom(O1), \+ boolean(O1), !,
  O2 = O1.
% typed literal
pre_term(Hdt, Val^^D, OHdt) :-
  ground(Val), !,
  rdf_lexical_form(Val^^D, Lex^^D),
  (   var(D)
  ->  % Match a typed literal whose datatype IRI is unbound.
      atomics_to_string(["\"",Lex,"\"^^<"], Prefix),
      hdt_term_prefix_(Hdt, sink, Prefix, OHdt)
  ;   atomics_to_string(["\"",Lex,"\"^^<",D,">"], OHdt)
  ).
% language-tagged string
pre_term(Hdt, Lex@LTag, OHdt) :-
  ground(Lex), !,
  (   var(LTag)
  ->  % Match a language-tagged string whose language tag is unbound.
      atomics_to_string(["\"",Lex,"\"@"], Prefix),
      hdt_term_prefix_(Hdt, sink, Prefix, O),
      pre_term(Hdt, O, OHdt)
  ;   atomics_to_string(["\"",Lex,"\"@",LTag], OHdt)
  ).
% not an RDF term (e.g., variable)
pre_term(_, _, _).

boolean(false).
boolean(true).



%! role_leafrole(+Role:atom, -LeafRole:atom) is nondet.

role_leafrole(Role, SubRole) :-
  closure0(role_subrole, Role, SubRole),
  leafrole(SubRole).



%! role_subrole(+Role:atom, +SubRole:atom) is semidet.
%! role_subrole(+Role:atom, -SubRole:atom) is nondet.

role_subrole(node, shared).
role_subrole(node, source).
role_subrole(node, sink).
role_subrole(object, shared).
role_subrole(object, sink).
role_subrole(subject, shared).
role_subrole(subject, source).
role_subrole(term, predicate).
role_subrole(term, node).
