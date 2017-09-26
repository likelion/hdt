:- module(
  hdt_term,
  [
    hdt_close/1,        % +Hdt
    hdt_create/2,       % +RdfFile, -HdtFile
    hdt_deinit/1,       % +Hdt
    hdt_graph/2,        % ?Hdt, ?G
    hdt_init/3,         % +HdtFile, -Hdt, ?G
    hdt_open/2,         % +HdtFile, -Hdt
    hdt_term/3,         % +Hdt, +Role, ?Term
    hdt_term_count/3,   % +Hdt, +Role, ?Count
    hdt_term_prefix/4,  % +Hdt, +Role, +Prefix, ?Term
    hdt_term_random/3,  % +Hdt, +Role, -Term
    hdt_triple/4,       % +Hdt, ?S, ?P, ?O
    hdt_triple_count/5, % +Hdt, ?S, ?P, ?O, ?Count
    hdt_triple_random/4 % +Hdt, ?S, ?P, ?O
  ]
).
:- reexport(library(semweb/rdf11)).

/** <module> HDT by term

@author Wouter Beek
@author Jan Wielemaker
@version 2017/09
*/

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(dcg/dcg_ext)).
:- use_module(library(debug)).
:- use_module(library(error)).
:- use_module(library(filesex)).
:- use_module(library(lists)).
:- use_module(library(ordsets)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(semweb/rdf_prefix), []).
:- use_module(library(semweb/rdf_print)).
:- use_module(library(sgml)).
:- use_module(library(uri)).

:- use_foreign_library(foreign(hdt4pl)).

:- at_halt(forall(hdt_graph(Hdt, _), hdt_close(Hdt))).

:- dynamic
    hdt_graph/2.

:- rdf_meta
   hdt_term(+, +, r),
   hdt_term_prefix(+, +, +, r),
   hdt_triple(+, r, r, o),
   hdt_triple_count(+, r, r, o, ?),
   hdt_triple_random(+, r, r, o).





%! hdt_close(+Hdt:blob) is det.

hdt_close(Hdt) :-
  hdt_close_(Hdt).



%! hdt_create(+RdfFile:atom, ?HdtFile:atom) is det.

hdt_create(RdfFile, HdtFile) :-
  (   var(HdtFile)
  ->  directory_file_path(Dir, RdfLocal, RdfFile),
      atomic_list_concat(Segments1, ., RdfLocal),
      % NOTE: does not auto-detect that this is deterministic :(
      once(append(Segments2, [_], Segments1)),
      atomic_list_concat(Segments2, ., Base),
      file_name_extension(Base, hdt, HdtLocal),
      directory_file_path(Dir, HdtLocal, HdtFile)
  ;   true
  ),
  hdt_create_(HdtFile, RdfFile, []).



%! hdt_deinit(+Hdt:blob) is det.

hdt_deinit(Hdt) :-
  with_mutex(hdt_term, (
    hdt_graph(Hdt, G),
    retractall(hdt_graph(Hdt, G)),
    hdt_close(Hdt)
  )).



%! hdt_init(+HdtFile:atom, -Hdt:blob, ?G:atom) is det.

hdt_init(HdtFile, Hdt, G) :-
  (var(G) -> uri_file_name(G, HdtFile) ; true),
  hdt_open(HdtFile, Hdt),
  with_mutex(hdt_term, (
    (   hdt_graph(Hdt, _)
    ->  existence_error(hdt, Hdt)
    ;   hdt_graph(_, G)
    ->  existence_error(graph, G)
    ;   assert(hdt_graph(Hdt, G))
    )
  )).



%! hdt_open(+HdtFile:atom, -Hdt:blob) is det.

hdt_open(HdtFile, Hdt) :-
  hdt_open_(HdtFile, Hdt, []).



%! hdt_term(+Hdt:blob, +Role, +Term) is semidet.
%! hdt_term(+Hdt:blob, +Role, -Term) is nondet.

% predicate
hdt_term(Hdt, predicate, Term) :-
  (   var(Term)
  ->  hdt_term_(Hdt, predicate, Term)
  ;   hdt_(Hdt, content, _, Term, _)
  ).
% shared
hdt_term(Hdt, shared, Term) :-
  (   var(Term)
  ->  hdt_term_(Hdt, shared, Atom)
  ;   pre_term(Hdt, Term, Atom),
      hdt_(Hdt, content, Atom, _, _),
      hdt_(Hdt, content, _, _, Atom)
  ),
  post_term(Term, Atom).
% sink
hdt_term(Hdt, sink, Term) :-
  (   var(Term)
  ->  hdt_term_(Hdt, sink, Atom)
  ;   pre_term(Hdt, Term, Atom),
      hdt_(Hdt, content, _, _, Atom),
      \+ hdt_(Hdt, content, Atom, _, _)
  ),
  post_term(Term, Atom).
% source
hdt_term(Hdt, source, Term) :-
  (   var(Term)
  ->  hdt_term_(Hdt, source, Atom)
  ;   pre_term(Hdt, Term, Atom),
      hdt_(Hdt, content, Atom, _, _),
      \+ hdt_(Hdt, content, _, _, Atom)
  ),
  post_term(Term, Atom).
% node, object, subject
hdt_term(Hdt, Role1, Term) :-
  map_role(Role1, Role2),
  hdt_term(Hdt, Role2, Term).



%! hdt_term_count(+Hdt:blob, +Role, ?Count:nonneg) is det.

% object
hdt_term_count(Hdt, object, Count) :- !,
  once(header(Hdt, _, '<http://rdfs.org/ns/void#distinctObjects>', Count0)),
  Count0 = Count^^_.
% predicate
hdt_term_count(Hdt, predicate, Count) :- !,
  once(header(Hdt, _, '<http://rdfs.org/ns/void#properties>', Count0)),
  Count0 = Count^^_.
% shared
hdt_term_count(Hdt, shared, Count) :- !,
  once(header(Hdt, _, '<http://purl.org/HDT/hdt#dictionarynumSharedSubjectObject>', Count0)),
  Count0 = Count^^_.
% sink
hdt_term_count(Hdt, sink, Count) :- !,
  maplist(hdt_term_count(Hdt), [shared,subject], [Count1,Count2]),
  Count is Count2 - Count1.
% source
hdt_term_count(Hdt, source, Count) :- !,
  maplist(hdt_term_count(Hdt), [object,shared], [Count1,Count2]),
  Count is Count1 - Count2.
% subject
hdt_term_count(Hdt, subject, Count) :- !,
  once(header(Hdt, _, '<http://rdfs.org/ns/void#distinctSubjects>', Count0)),
  Count0 = Count^^_.



%! hdt_term_prefix(+Hdt:blob, +Role, +Prefix:atom, ?Term) is nondet.

hdt_term_prefix(Hdt, Role1, Prefix, Term) :-
  map_role(Role1, Role2),
  hdt_prefix_(Hdt, Role2, Prefix, Atom),
  post_term(Term, Atom).



%! hdt_term_random(+Hdt:blob, +Role, -Term) is nondet.

hdt_term_random(Hdt, Role1, Term) :-
  aggregate_all(set(Role2), map_role(Role1, Role2), Roles2),
  maplist(hdt_term_count(Hdt), Roles2, Counts),
  sum_list(Counts, Count),
  random_between(1, Count, Index),
  index_role(Index, Counts, Roles2, Role2),
  hdt_term_rnd_(Hdt, Role2, Atom),
  post_term(Term, Atom).

index_role(N, [Count|_], [Role|_], Role) :-
  N =< Count, !.
index_role(N1, [H|T1], [_|T2], Role) :-
  N2 is N1 - H,
  index_role(N2, T1, T2, Role).



%! hdt_triple(+Hdt:blob, ?S, ?P, ?O) is nondet.
%
% True if 〈SId,SIP,SIO〉 is an integer triple in Hdt.

hdt_triple(Hdt, S, P, O) :-
  pre_term(Hdt, O, OAtom),
  hdt_(Hdt, content, S, P, OAtom),
  post_term(O, OAtom),
  (   debugging(hdt_term)
  ->  dcg_debug(hdt_term, ("TP ",rdf_dcg_triple(S,P,O)))
  ;   true
  ).



%! hdt_triple_count(+Hdt:blob, ?S, ?P, ?O, -Count:nonneg) is det.

hdt_triple_count(Hdt, S, P, O, Count) :-
  pre_term(Hdt, O, OAtom),
  hdt_count_(Hdt, S, P, OAtom, Count), !.
hdt_triple_count(_, _, _, _, 0).



%! hdt_triple_random(+Hdt:blob, ?S, ?P, ?O) is semidet.

hdt_triple_random(Hdt, S, P, O) :-
  pre_term(Hdt, O, OAtom),
  hdt_rnd_(Hdt, S, P, OAtom),
  post_term(O, OAtom),
  (   debugging(hdt_term)
  ->  dcg_debug(hdt_term, ("random ",rdf_dcg_triple(S,P,O)))
  ;   true
  ).





% HELPERS %

%! header(+Hdt:blob, ?S, ?P, ?O) is nondet.

header(Hdt, S, P, O) :-
  pre_term(Hdt, O, Atom),
  hdt_(Hdt, header, S, P, Atom),
  header_object(Atom, O).

header_object(Atom1, O) :-
  atom_concat('"', Atom2, Atom1), !,
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



%! map_role(+Role1, -Role2) is nondet.

map_role(node,    shared).
map_role(node,    source).
map_role(node,    sink  ).
map_role(object,  shared).
map_role(object,  sink  ).
map_role(subject, shared).
map_role(subject, source).



%! pre_term(+Hdt:blob, ?O:rdf_term, -Atom:atom) is det.
%
% This helper predicate implements the feature that literals can be
% entered partially.  Specifically, it is possible to only supply
% their lexical form, and match their language tag or datatype IRI.

pre_term(_, Var, _) :-
  var(Var), !.
pre_term(Hdt, literal(lang(LTag,Lex)), Atom) :- !,
  (   var(LTag)
  ->  atomic_list_concat(['"',Lex,'"@'], Prefix),
      hdt_prefix_(Hdt, sink, Prefix, O),
      pre_term(Hdt, O, Atom)
  ;   atomic_list_concat(['"',Lex,'"@',LTag], Atom)
  ).
pre_term(Hdt, literal(type(D,Lex)), Atom) :- !,
  (   var(D)
  ->  atomic_list_concat(['"',Lex,'"^^<'], Prefix),
      hdt_prefix_(Hdt, sink, Prefix, O),
      pre_term(Hdt, O, Atom)
  ;   atomic_list_concat(['"',Lex,'"^^<',D,>], Atom)
  ).
pre_term(_, literal(Lex), Atom) :- !,
  atomic_list_concat(['"',Lex,'"'], Atom).
pre_term(_, NonLiteral, NonLiteral).



%! post_term(?O:rdf_term, +Atom:atom) is det.

post_term(O, Atom1) :-
  atom_concat('"', Atom2, Atom1), !,
  atom_codes(Atom2, Codes),
  phrase(post_literal(O), Codes).
post_term(NonLiteral, NonLiteral).

post_literal(Literal) -->
  string(Codes1),
  "\"", !,
  {atom_codes(Lex, Codes1)},
  (   "^"
  ->  "^<",
      string(Codes2),
      ">",
      {
        atom_codes(D, Codes2),
        Literal = literal(type(D,Lex))
      }
  ;   "@"
  ->  remainder(Codes2),
      {
        atom_codes(LTag, Codes2),
        Literal = literal(lang(LTag,Lex))
      }
  ;   {Literal = literal(Lex)}
  ).
