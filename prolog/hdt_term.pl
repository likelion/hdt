:- module(
  hdt_term,
  [
    hdt_close/1,        % +Hdt
    hdt_create/2,       % +RdfFile, -HdtFile
    hdt_open/2,         % +HdtFile, -Hdt
    hdt_term/3,         % +Hdt, +QTerm, -Term
    hdt_term_count/3,   % +Hdt, +Role, -Count
    hdt_term_prefix/4,  % +Hdt, +Roles, +Prefix, -Term
    hdt_term_random/3,  % +Hdt, +Roles, -Term
    hdt_triple/3,       % +Hdt, +QTriple, -Triple
    hdt_triple_count/3, % +Hdt, +QTriple, ?Count
    hdt_triple_random/3 % +Hdt, +QTriple, -Triple
  ]
).
:- reexport(library(semweb/rdf11)).

/** <module> HDT by term

A QTriple is a compound term of the form rdf(S,P,O), where S, P and O
are QTerms.

A QTerm is a compound term of the form term(Roles,Term0), where Roles
is an ordset and Term0 is an rdf11 term.

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

:- use_foreign_library(foreign(hdt4pl)).





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



%! hdt_open(+HdtFile:atom, -Hdt:blob) is det.

hdt_open(HdtFile, Hdt) :-
  hdt_open_(HdtFile, Hdt, []).



%! hdt_term(+Hdt:blob, +QTerm, -Term) is nondet.

hdt_term(Hdt, term(Roles,Term0), term([Role2],Term0)) :-
  member(Role1, Roles),
  map_role(Role1, Role2),
  hdt_term_role(Hdt, Role2, Term0).

% predicate
hdt_term_role(Hdt, predicate, Term) :-
  (   var(Term)
  ->  hdt_term_(Hdt, predicate, Term)
  ;   hdt_(Hdt, content, _, Term, _)
  ).
% shared
hdt_term_role(Hdt, shared, Term) :-
  (   var(Term)
  ->  hdt_term_(Hdt, shared, Atom)
  ;   pre_term(Hdt, Term, Atom),
      hdt_(Hdt, content, Atom, _, _),
      hdt_(Hdt, content, _, _, Atom)
  ),
  post_term(Term, Atom).
% sink
hdt_term_role(Hdt, sink, Term) :-
  (   var(Term)
  ->  hdt_term_(Hdt, sink, Atom)
  ;   pre_term(Hdt, Term, Atom),
      hdt_(Hdt, content, _, _, Atom),
      \+ hdt_(Hdt, content, Atom, _, _)
  ),
  post_term(Term, Atom).
% source
hdt_term_role(Hdt, source, Term) :-
  (   var(Term)
  ->  hdt_term_(Hdt, source, Atom)
  ;   pre_term(Hdt, Term, Atom),
      hdt_(Hdt, content, Atom, _, _),
      \+ hdt_(Hdt, content, _, _, Atom)
  ),
  post_term(Term, Atom).



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



%! hdt_term_prefix(+Hdt:blob, +Roles:ordset, +Prefix:atom, ?Term) is nondet.

hdt_term_prefix(Hdt, Roles, Prefix, Term) :-
  member(Role1, Roles),
  map_role(Role1, Role2),
  hdt_prefix_(Hdt, Role2, Prefix, Atom),
  post_term(Term, Atom).



%! hdt_term_random(+Hdt:blob, +Roles, -Term) is nondet.

hdt_term_random(Hdt, Roles1, term([Role2],Term0)) :-
  aggregate_all(
    set(Role2),
    (
      member(Role1, Roles1),
      map_role(Role1, Role2)
    ),
    Roles2
  ),
  maplist(hdt_term_count(Hdt), Roles2, Counts),
  sum_list(Counts, Count),
  random_between(1, Count, Index),
  index_role(Index, Counts, Roles2, Role2),
  hdt_term_rnd_(Hdt, Role2, Atom),
  post_term(Term0, Atom).

index_role(N, [Count|_], [Role|_], Role) :-
  N =< Count, !.
index_role(N1, [H|T1], [_|T2], Role) :-
  N2 is N1 - H,
  index_role(N2, T1, T2, Role).



%! hdt_triple(+Hdt:blob, +QTriple:compound, -Triple:compound) is nondet.
%
% True if 〈SId,SIP,SIO〉 is an integer triple in Hdt.

hdt_triple(
  Hdt,
  rdf(term(SRoles1,STerm0),term(PRoles1,PTerm0),term(ORoles1,OTerm0)),
  rdf(term(SRoles2,STerm0),term(PRoles2,PTerm0),term(ORoles2,OTerm0))
) :-
  add_subject_role(SRoles1, SRoles2),
  add_predicate_role(PRoles1, PRoles2),
  add_object_role(ORoles1, ORoles2),
  maplist(pre_term(Hdt), [STerm0,PTerm0,OTerm0], [SAtom,PAtom,OAtom]),
  hdt_(Hdt, content, SAtom, PAtom, OAtom),
  maplist(post_term, [STerm0,PTerm0,OTerm0], [SAtom,PAtom,OAtom]),
  (   debugging(hdt_atom)
  ->  dcg_debug(hdt_atom, ("TP ",rdf_dcg_triple(STerm0,PTerm0,OTerm0)))
  ;   true
  ).



%! hdt_triple_count(+Hdt:blob, +QTriple:compound, +Count:nonneg) is semidet.

hdt_triple_count(
  Hdt,
  rdf(term(_,STerm0),term(_,PTerm0),term(_,OTerm0)),
  Count
) :-
  maplist(pre_term(Hdt), [STerm0,PTerm0,OTerm0], [SAtom,PAtom,OAtom]),
  hdt_count_(Hdt, SAtom, PAtom, OAtom, Count), !.
hdt_triple_count(_, _, _, _, 0).



%! hdt_triple_random(+Hdt:blob, +QTriple:compound,
%!                   -Triple:compound) is semidet.

hdt_triple_random(
  Hdt,
  rdf(term(SRoles1,STerm0),term(PRoles1,PTerm0),term(ORoles1,OTerm0)),
  rdf(term(SRoles2,STerm0),term(PRoles2,PTerm0),term(ORoles2,OTerm0))
) :-
  add_subject_role(SRoles1, SRoles2),
  add_predicate_role(PRoles1, PRoles2),
  add_object_role(ORoles1, ORoles2),
  maplist(pre_term(Hdt), [STerm0,PTerm0,OTerm0], [SAtom,PAtom,OAtom]),
  hdt_rnd_(Hdt, SAtom, PAtom, OAtom),
  maplist(post_term, [STerm0,PTerm0,OTerm0], [SAtom,PAtom,OAtom]),
  (   debugging(hdt_atom)
  ->  dcg_debug(hdt_atom, ("random ",rdf_dcg_triple(STerm0,PTerm0,OTerm0)))
  ;   true
  ).





% HELPERS %

add_object_role(X, []) :- var(X), !.
add_object_role(Roles1, Roles3) :-
  ord_del_element(Roles1, subject, Roles2), !,
  ord_add_element(Roles2, shared, Roles3).
add_object_role(Roles1, Roles2) :-
  ord_add_element(Roles1, object, Roles2).

add_predicate_role(X, []) :- var(X), !.
add_predicate_role(Roles1, Roles2) :-
  ord_add_element(Roles1, predicate, Roles2).

add_subject_role(X, []) :- var(X), !.
add_subject_role(Roles1, Roles3) :-
  ord_del_element(Roles1, object, Roles2), !,
  ord_add_element(Roles2, shared, Roles3).
add_subject_role(Roles1, Roles2) :-
  ord_add_element(Roles1, subject, Roles2).



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

map_role(predicate, predicate).
map_role(object,    shared   ).
map_role(object,    sink     ).
map_role(sink,      sink     ).
map_role(subject,   shared   ).
map_role(subject,   source   ).
map_role(source,    source   ).
map_role(shared,    shared   ).



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
