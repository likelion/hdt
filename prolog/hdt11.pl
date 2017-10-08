:- module(
  hdt11,
  [
    hdt_property/2,         % +Hdt, -Property
    hdt_term/3,             % +Hdt, ?Role, ?Term
    hdt_term_count/3,       % +Hdt, +Role, ?Count
    hdt_term_prefix/4,      % +Hdt, ?Role, +Prefix, ?Term
    hdt_term_random/3,      % +Hdt, +Role, -Term
    hdt_term_translate/3,   % +Hdt, ?Term, ?Id
    hdt_triple/4,           % +Hdt, ?S, ?P, ?O
    hdt_triple_count/5,     % +Hdt, ?S, ?P, ?O, ?Count
    hdt_triple_random/4,    % +Hdt, ?S, ?P, ?O
    hdt_triple_translate/3, % +Hdt, ?TermTriple, ?IdTriple
    op(110, xfx, @),        % must be above `.'
    op(650, xfx, ^^)        % must be above `:'
  ]
).
:- reexport(library(hdt_generic), [
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
:- use_module(library(semweb/rdf11)).
:- use_module(library(sgml)).
:- use_module(library(uri)).

:- use_foreign_library(foreign(hdt4pl)).

:- rdf_meta
   hdt_term(+, ?, o),
   hdt_term_prefix(+, ?, +, o),
   hdt_term_random(+, -, o),
   hdt_term_translate(+, t, ?),
   hdt_triple(+, r, r, o),
   hdt_triple_count(+, r, r, o, -),
   hdt_triple_random(+, r, r, o),
   hdt_triple_translate(+, t, ?).





%! hdt_term(+Hdt:blob, ?Role:atom, ?Term:rdf_term) is nondet.
%
%  @arg Role is either of the following atoms:
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

% node
hdt_term(Hdt, node, Node) :-
  hdt_term(Hdt, shared, Node).
hdt_term(Hdt, node, Node) :-
  hdt_term(Hdt, sink, Node).
hdt_term(Hdt, node, Node) :-
  hdt_term(Hdt, source, Node).
% object
hdt_term(Hdt, object, O) :-
  hdt_term(Hdt, shared, O).
hdt_term(Hdt, object, O) :-
  hdt_term(Hdt, sink, O).
% predicate
hdt_term(Hdt, predicate, P) :-
  (   var(P)
  ->  hdt_term_(Hdt, predicate, P)
  ;   hdt_(Hdt, content, _, P, _)
  ).
% shared
hdt_term(Hdt, shared, Shared) :-
  (   var(Shared)
  ->  hdt_term_(Hdt, shared, Atom)
  ;   % O: Determine this based on ID offset.
      pre_term(Hdt, Shared, Atom),
      hdt_(Hdt, content, Atom, _, _),
      hdt_(Hdt, content, _, _, Atom)
  ),
  post_term(Shared, Atom).
% sink
hdt_term(Hdt, sink, Sink) :-
  (   var(Sink)
  ->  hdt_term_(Hdt, sink, Atom)
  ;   % O: Determine this based on ID offset.
      pre_term(Hdt, Sink, Atom),
      hdt_(Hdt, content, _, _, Atom),
      \+ hdt_(Hdt, content, Atom, _, _)
  ),
  post_term(Sink, Atom).
% source
hdt_term(Hdt, source, Source) :-
  (   var(Source)
  ->  hdt_term_(Hdt, source, Atom)
  ;   % O: Determine this based on ID offset.
      pre_term(Hdt, Source, Atom),
      hdt_(Hdt, content, Atom, _, _),
      \+ hdt_(Hdt, content, _, _, Atom)
  ),
  post_term(Source, Atom).
% subject
hdt_term(Hdt, subject, S) :-
  hdt_term(Hdt, source, S).
hdt_term(Hdt, subject, S) :-
  hdt_term(Hdt, shared, S).
% term
hdt_term(Hdt, term, Term) :-
  hdt_term(Hdt, node, Term).
hdt_term(Hdt, term, Term) :-
  hdt_term(Hdt, predicate, Term),
  \+ hdt_term(Hdt, node, Term).



%! hdt_term_prefix(+Hdt:blob, ?Role:atom, +Prefix:atom,
%!                 ?Term:rdf_term) is nondet.

hdt_term_prefix(Hdt, Role, Prefix, Term) :-
  pre_term(Hdt, Term, Atom),
  hdt_term_prefix_(Hdt, Role, Prefix, Atom),
  post_term(Term, Atom).



%! hdt_term_random(+Hdt:blob, +Role:atom, -Term:rdf_term) is nondet.

hdt_term_random(Hdt, node, Term) :- !,
  maplist(hdt_term_count, [shared,sink,source], [N1,N2,N3]),
  sum_list([N1,N2,N3], N),
  random_between(1, N, Rnd),
  (Rnd =< N1 -> Role = shared ; Rnd =< N2 -> Role = sink ; Role = source),
  hdt_term_random(Hdt, Role, Term).
hdt_term_random(Hdt, Role, Term) :-
  Rnd is random_float,
  hdt_term_random_(Hdt, Role, Rnd, Term).



%! hdt_term_translate(+Hdt:blob, +Term:rdf_term, +Id:compound) is semidet.
%! hdt_term_translate(+Hdt:blob, +Term:rdf_term, -Id:compound) is det.
%! hdt_term_translate(+Hdt:blob, -Term:rdf_term, +Id:compound) is det.

hdt_term_translate(Hdt, Term, id(Role,Id)) :-
  pre_term(Hdt, Term, Atom),
  hdt_term_translate_(Hdt, Role, Atom, Id),
  post_term(Term, Atom).



%! hdt_triple(+Hdt:blob, ?S:rdf_nonliteral, ?P:rdf_iri, ?O:rdf_term) is nondet.
%
% True if 〈S,P,O〉 is an RDF triple in HDT.

hdt_triple_triple(Hdt, S, P, O) :-
  pre_term(Hdt, O, Atom),
  hdt_triple_(Hdt, content, S, P, Atom),
  post_term(O, Atom).



%! hdt_triple_count(+Hdt:blob, ?S:rdf_nonliteral, ?P:rdf_iri, ?O:rdf_term,
%!                  -Count:nonneg) is det.
%
% True if Count is the number of matches of the Triple Pattern〈S,P,O〉
% on the graph stored in HDT.

hdt_triple_count(Hdt, S, P, O, Count) :-
  (var(S) -> true ; rdf_is_subject(S)), !,
  pre_term(Hdt, O, Atom),
  hdt_triple_count_(Hdt, S, P, Atom, Count).
hdt_triple_count(_, _, _, _, 0).



%! hdt_triple_random(+Hdt:blob, ?S:rdf_nonliteral, ?P:rdf_iri,
%!                   ?O:rdf_term) is semidet.

hdt_triple_random(Hdt, S, P, O) :-
  pre_term(Hdt, O, Atom),
  Rnd is random_float,
  hdt_triple_random_(Hdt, Rnd, S, P, Atom),
  post_term(O, Atom).



%! hdt_triple_translate(+Hdt:blob, +TermTriple:compound,
%!                      -IdTriple:compound) is det.
%! hdt_triple_translate(+Hdt:blob, -TermTriple:compound,
%!                      +IdTriple:compound) is det.

hdt_triple_translate(Hdt, rdf(S,P,O), rdf(SId,PId,OId)) :-
  maplist(hdt_term_translate(Hdt), [S,P,O], [SId,PId,OId]).





% OTHERS %

%! hdt_property(+Hdt:blob, +Property:compound) is semidet.
%! hdt_property(+Hdt:blob, ?Property:compound) is nondet.
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
      hdt_prefix_(Hdt, sink, Prefix, O),
      pre_term(Hdt, O, Atom)
  ;   atomic_list_concat(['"',Lex,'"@',LTag], Atom)
  ).
pre_term(Hdt, Val^^D, Atom) :- !,
  must_be(ground, Val),
  rdf_lexical_form(Val^^D, Lex^^D),
  (   var(D)
  ->  atomic_list_concat(['"',Lex,'"^^<'], Prefix),
      hdt_prefix_(Hdt, sink, Prefix, O),
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
