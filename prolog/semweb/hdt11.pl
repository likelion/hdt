:- module(
  hdt11,
  [
    hdt_atom_to_term/2,     % +Atom, -Term
    hdt_term/3,             % +Hdt, +Role, ?Term
    hdt_term/4,             % +Hdt, +Role, -LeafRole, ?Term
    hdt_term_count/3,       % +Hdt, +Role, ?Count
    hdt_term_prefix/4,      % +Hdt, +Role, +Prefix, ?Term
    hdt_term_prefix/5,      % +Hdt, +Role, +Prefix, -LeafRole, ?Term
    hdt_term_random/3,      % +Hdt, +Role, -Term
    hdt_term_random/4,      % +Hdt, +Role, -LeafRole, -Term
    hdt_term_translate/4,   % +Hdt, +Role, ?Term, ?Id
    hdt_triple/4,           % +Hdt, ?S, ?P, ?O
    hdt_triple_count/5,     % +Hdt, ?S, ?P, ?O, ?Count
    hdt_triple_random/4,    % +Hdt, ?S, ?P, ?O
    hdt_triple_translate/3, % +Hdt, ?TermTriple, ?IdTriple
    op(110, xfx, @),        % must be above `.'
    op(650, xfx, ^^)        % must be above `:'
  ]
).
:- reexport(library(semweb/hdt_generic), [
     hdt/1,
     hdt_close/1,
     hdt_create/1,
     hdt_create/2,
     hdt_create/3,
     hdt_deinit/0,
     hdt_deinit/1,
     hdt_graph/1,
     hdt_graph/2,
     hdt_init/1,
     hdt_init/2,
     hdt_open/2,
     hdt_term_count/3
   ]).
:- reexport(library(semweb/rdf11)).

/** <module> HDT API consistent with library RDF11

@author Jan Wielemaker
@author Wouter Beek
@version 2017/09-2017/10
*/

:- use_module(library(dcg/basics)).
:- use_module(library(error)).
:- use_module(library(filesex)).
:- use_module(library(lists)).
:- use_module(library(option)).
:- use_module(library(semweb/hdt_generic)).
:- use_module(library(semweb/rdf_api)).
:- use_module(library(sgml)).
:- use_module(library(uri)).

:- rdf_meta
   hdt_term(+, +, o),
   hdt_term(+, +, -, o),
   hdt_term_prefix(+, +, +, o),
   hdt_term_prefix(+, +, +, -, o),
   hdt_term_translate(+, o, ?),
   hdt_triple(+, r, r, o),
   hdt_triple_count(+, r, r, o, ?),
   hdt_triple_random(+, r, r, o),
   hdt_triple_translate(+, t, ?),
   literal_codes(+, o).





%! hdt_atom_to_term(+Atom:atom, -Term:rdf_term) is det.

hdt_atom_to_term(Atom, Literal) :-
  atom_codes(Atom, Codes),
  phrase(hdt_generic:hdt_literal1(Literal0), Codes), !,
  literal_codes(Literal0, Literal).
hdt_atom_to_term(NonLiteral, NonLiteral).

literal_codes(literal(lang(LTag0,Lex0)), String@LTag) :- !,
  atom_codes(LTag, LTag0),
  string_codes(String, Lex0).
literal_codes(literal(type(D0,Lex0)), Value^^D) :- !,
  maplist(atom_codes, [D,Lex], [D0,Lex0]),
  rdf_lexical_value(D, Lex, Value).
literal_codes(literal(Lex0), String^^xsd:string) :-
  string_codes(String, Lex0).



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



%! hdt_term_prefix(+Hdt:blob, ?Role:atom, +Prefix:atom, ?Term:rdf_term) is nondet.
%! hdt_term_prefix(+Hdt:blob, ?Role:atom, +Prefix:atom, -LeafRole:atom, ?Term:rdf_term) is nondet.

hdt_term_prefix(Hdt, Role, Prefix, Term) :-
  hdt_term_prefix(Hdt, Role, Prefix, _, Term).


hdt_term_prefix(Hdt, Role, Prefix, LeafRole, Term) :-
  role_leafrole(Role, LeafRole),
  hdt_term_prefix_(Hdt, LeafRole, Prefix, Atom),
  (atom_prefix(Atom, Prefix) -> true ; !, fail),
  hdt_atom_to_term(Atom, Term).



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



%! hdt_triple(+Hdt:blob, ?S:rdf_nonliteral, ?P:rdf_iri, ?O:rdf_term) is nondet.
%
% True if 〈SId,SIP,SIO〉 is an integer triple in Hdt.

hdt_triple(Hdt, S, P, O) :-
  pre_term(Hdt, O, OAtom),
  hdt_triple_(Hdt, content, S, P, OAtom),
  hdt_atom_to_term(OAtom, O).



%! hdt_triple_count(+Hdt:blob, ?S:rdf_nonliteral, ?P:rdf_iri, ?O:rdf_term, -Count:nonneg) is det.

hdt_triple_count(Hdt, S, P, O, Count) :-
  pre_term(Hdt, O, OAtom),
  hdt_count_(Hdt, S, P, OAtom, Count), !.
hdt_triple_count(_, _, _, _, 0).



%! hdt_triple_random(+Hdt:blob, ?S, ?P, ?O) is semidet.

hdt_triple_random(Hdt, S, P, O) :-
  pre_term(Hdt, O, OAtom),
  Rnd is random_float,
  hdt_triple_random_(Hdt, Rnd, S, P, OAtom),
  hdt_atom_to_term(OAtom, O).



%! hdt_triple_translate(+Hdt:blob, +TermTriple:rdf_term, -IdTriple:compound) is det.
%! hdt_triple_translate(+Hdt:blob, -TermTriple:rdf_term, +IdTriple:compound) is det.

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





% OTHERS %

%! pre_term(+Hdt:blob, ?O:rdf_term, -Atom:atom) is det.
%
% This helper predicate implements the feature that literals can be
% entered partially.  Specifically, it is possible to only supply
% their lexical form, and match their language tag or datatype IRI.

pre_term(_, Var, _) :-
  var(Var), !.
pre_term(Hdt, Lex@LTag, Atom) :- !,
  must_be(string, Lex),
  (   var(LTag)
  ->  atomic_list_concat(['"',Lex,'"@'], Prefix),
      hdt_term_prefix_(Hdt, sink, Prefix, O),
      pre_term(Hdt, O, Atom)
  ;   atomic_list_concat(['"',Lex,'"@',LTag], Atom)
  ).
pre_term(Hdt, Val^^D, Atom) :- !,
  must_be(ground, Val),
  rdf_lexical_form(Val^^D, Lex^^D),
  (   var(D)
  ->  atomic_list_concat(['"',Lex,'"^^<'], Prefix),
      hdt_term_prefix_(Hdt, sink, Prefix, O),
      pre_term(Hdt, O, Atom)
  ;   atomic_list_concat(['"',Lex,'"^^<',D,>], Atom)
  ).
pre_term(_, NonLiteral, NonLiteral).



%! post_term(?O:rdf_term, +Atom:atom) is det.

post_term(O, Atom1) :-
  atom_concat('"', Atom2, Atom1), !,
  atom_codes(Atom2, Codes),
  phrase(post_literal(O), Codes).
post_term(NonLiteral, NonLiteral).

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
