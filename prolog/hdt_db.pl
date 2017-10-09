:- module(
  hdt_term,
  [
    hdt_term/3,            % +Hdt, +Role, ?Term
    hdt_term/4,            % +Hdt, +Role, -LeafRole, ?Term
    hdt_term_count/3,      % +Hdt, +Role, ?Count
    hdt_term_prefix/4,     % +Hdt, +Role, +Prefix, ?Term
    hdt_term_prefix/5,     % +Hdt, +Role, +Prefix, -LeafRole, ?Term
    hdt_term_random/3,     % +Hdt, +Role, -Term
    hdt_term_random/4,     % +Hdt, +Role, -LeafRole, -Term
    hdt_term_translate/4,  % +Hdt, +Role, ?Term, ?Id
    hdt_triple/4,          % +Hdt, ?S, ?P, ?O
    hdt_triple_count/5,    % +Hdt, ?S, ?P, ?O, ?Count
    hdt_triple_random/4,   % +Hdt, ?S, ?P, ?O
    hdt_triple_translate/3 % +Hdt, ?TermTriple, ?IdTriple
  ]
).
:- reexport(library(hdt_generic), [
     hdt_atom_to_term/2,
     hdt_close/1,
     hdt_create/2,
     hdt_create/3,
     hdt_deinit/1,
     hdt_graph/1,
     hdt_graph/2,
     hdt_init/2,
     hdt_open/2,
     hdt_term_count/3
   ]).
:- reexport(library(semweb/rdf11)).

/** <module> HDT by term

@author Wouter Beek
@author Jan Wielemaker
@version 2017/09-2017/10
*/

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(dcg/dcg_ext)).
:- use_module(library(debug)).
:- use_module(library(hdt_generic)).
:- use_module(library(lists)).
:- use_module(library(semweb/rdf_api)).
:- use_module(library(semweb/rdf_print)).

:- rdf_meta
   hdt_term(+, +, r),
   hdt_term(+, +, -, r),
   hdt_term_prefix(+, +, +, r),
   hdt_term_prefix(+, +, +, -, r),
   hdt_term_translate(+, +, t, ?),
   hdt_triple(+, r, r, o),
   hdt_triple_count(+, r, r, o, ?),
   hdt_triple_random(+, r, r, o),
   hdt_triple_translate(+, t, ?).





%! hdt_term(+Hdt:blob, +Role:atom, +Term:compound) is semidet.
%! hdt_term(+Hdt:blob, +Role:atom, -Term:compound) is nondet.
%! hdt_term(+Hdt:blob, +Role:atom, -LeafRole:atom, +Term:compound) is semidet.
%! hdt_term(+Hdt:blob, +Role:atom, -LeafRole:atom, -Term:compound) is nondet.

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
  ->  hdt_term_(Hdt, shared, Atom)
  ;   pre_term(Hdt, Term, Atom),
      hdt_triple_(Hdt, content, Atom, _, _),
      hdt_triple_(Hdt, content, _, _, Atom)
  ),
  hdt_atom_to_term(Atom, Term).
% sink
hdt_term(Hdt, sink, sink, Term) :-
  (   var(Term)
  ->  hdt_term_(Hdt, sink, Atom)
  ;   pre_term(Hdt, Term, Atom),
      hdt_triple_(Hdt, content, _, _, Atom),
      \+ hdt_triple_(Hdt, content, Atom, _, _)
  ),
  hdt_atom_to_term(Atom, Term).
% source
hdt_term(Hdt, source, source, Term) :-
  (   var(Term)
  ->  hdt_term_(Hdt, source, Atom)
  ;   pre_term(Hdt, Term, Atom),
      hdt_triple_(Hdt, content, Atom, _, _),
      \+ hdt_triple_(Hdt, content, _, _, Atom)
  ),
  hdt_atom_to_term(Atom, Term).
% others: node, object, subject, term
hdt_term(Hdt, Role, LeafRole, Term) :-
  role_subrole(Role, SubRole),
  hdt_term(Hdt, SubRole, LeafRole, Term).



%! hdt_term_prefix(+Hdt:blob, +Role:atom, +Prefix:atom, ?Term:compound) is nondet.
%! hdt_term_prefix(+Hdt:blob, +Role:atom, +Prefix:atom, -LeafRole:atom,
%!                 ?Term:compound) is nondet.

hdt_term_prefix(Hdt, Role, Prefix, Term) :-
  hdt_term_prefix(Hdt, Role, Prefix, _, Term).


hdt_term_prefix(Hdt, Role, Prefix, LeafRole, Term) :-
  role_leafrole(Role, LeafRole),
  hdt_term_prefix_(Hdt, LeafRole, Prefix, Atom),
  hdt_atom_to_term(Atom, Term).



%! hdt_term_random(+Hdt:blob, +Role:atom, -Term:compound) is nondet.
%! hdt_term_random(+Hdt:blob, +Role:atom, -LeafRole:atom, -Term:compound) is nondet.

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
  hdt_atom_to_term(Atom, Term).

index_role(N, [Count|_], [Role|_], Role) :-
  N =< Count, !.
index_role(N1, [H|T1], [_|T2], Role) :-
  N2 is N1 - H,
  index_role(N2, T1, T2, Role).



%! hdt_term_translate(+Hdt:blob, +Role:atom, ?Term:rdf_term, ?Id:compound) is det.

hdt_term_translate(Hdt, Role, Term, Id) :-
  pre_term(Hdt, Term, Atom),
  hdt_term_translate_(Hdt, Role, Atom, Id),
  hdt_atom_to_term(Atom, Term).



%! hdt_triple(+Hdt:blob, ?S, ?P, ?O) is nondet.
%
% True if 〈SId,SIP,SIO〉 is an integer triple in Hdt.

hdt_triple(Hdt, S, P, O) :-
  pre_term(Hdt, O, OAtom),
  hdt_triple_(Hdt, content, S, P, OAtom),
  hdt_atom_to_term(OAtom, O),
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
  Rnd is random_float,
  hdt_triple_random_(Hdt, Rnd, S, P, OAtom),
  hdt_atom_to_term(OAtom, O),
  (   debugging(hdt_term)
  ->  dcg_debug(hdt_term, ("random ",rdf_dcg_triple(S,P,O)))
  ;   true
  ).



%! hdt_triple_translate(+Hdt:blob, +TermTriple:compound, -IdTriple:compound) is det.
%! hdt_triple_translate(+Hdt:blob, -TermTriple:compound, +IdTriple:compound) is det.

hdt_triple_translate(
  Hdt,
  rdf(S,P,O),
  rdf(id(subject,SId),id(predicate,PId),id(object,OId))
) :-
  maplist(
    hdt_term_translate(Hdt),
    [subject,predicate,object],
    [S,P,O],
    [SId,PId,OId]
  ).





% HELPERS %

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
      hdt_term_prefix_(Hdt, sink, Prefix, O),
      pre_term(Hdt, O, Atom)
  ;   atomic_list_concat(['"',Lex,'"@',LTag], Atom)
  ).
pre_term(Hdt, literal(type(D,Lex)), Atom) :- !,
  (   var(D)
  ->  atomic_list_concat(['"',Lex,'"^^<'], Prefix),
      hdt_term_prefix_(Hdt, sink, Prefix, O),
      pre_term(Hdt, O, Atom)
  ;   atomic_list_concat(['"',Lex,'"^^<',D,>], Atom)
  ).
pre_term(_, literal(Lex), Atom) :- !,
  atomic_list_concat(['"',Lex,'"'], Atom).
pre_term(_, NonLiteral, NonLiteral).
