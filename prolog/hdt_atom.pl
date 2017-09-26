:- module(
  hdt_atom,
  [
    hdt_close/1,           % +Hdt
    hdt_create/2,          % +RdfFile, -HdtFile
    hdt_open/2,            % +HdtFile, -Hdt
    hdt_term/3,            % +Hdt, +Role, ?Term
    hdt_term_count/3,      % +Hdt, ?Role, ?Count
    hdt_term_prefix/3,     % +Hdt, +Prefix, ?Term
    hdt_term_random/3,     % +Hdt, +Role, -Term
    hdt_term_translate/3,  % +Hdt, ?RdfTerm, ?HdtTerm
    hdt_triple/4,          % +Hdt, ?S, ?P, ?O
    hdt_triple_count/5,    % +Hdt, ?S, ?P, ?O, ?Count
    hdt_triple_random/4,   % +Hdt, ?S, ?P, ?O
    hdt_triple_translate/3 % +Hdt, ?RdfTriple, ?HdtTriple
  ]
).
:- reexport(library(semweb/rdf11)).

/** <module> HDT by atom

@author Wouter Beek
@author Jan Wielemaker
@version 2017/09
*/

:- use_module(library(apply)).
:- use_module(library(dcg/dcg_ext)).
:- use_module(library(debug)).
:- use_module(library(error)).
:- use_module(library(filesex)).
:- use_module(library(lists)).
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



%! hdt_term(+Hdt:blob, +Role, ?Term) is nondet.

% node
hdt_term(Hdt, node, Term) :-
  (   hdt_term(Hdt, shared, Term)
  ;   hdt_term(Hdt, sink, Term)
  ;   hdt_term(Hdt, source, Term)
  ).
% object
hdt_term(Hdt, object, Term) :-
  (   hdt_term(Hdt, shared, Term)
  ;   hdt_term(Hdt, sink, Term)
  ).
% predicate
hdt_term(Hdt, predicate, atom(predicate,Atom)) :-
  (   var(Atom)
  ->  hdt_term_(Hdt, predicate, Atom)
  ;   hdt_(Hdt, content, _, Atom, _)
  ).
% shared
hdt_term(Hdt, shared, atom(shared,Atom)) :-
  (   var(Atom)
  ->  hdt_term_(Hdt, shared, Atom)
  ;   hdt_(Hdt, content, Atom, _, _),
      hdt_(Hdt, content, _, _, Atom)
  ).
% sink
hdt_term(Hdt, sink, atom(sink,Atom)) :-
  (   var(Atom)
  ->  hdt_term_(Hdt, sink, Atom)
  ;   hdt_(Hdt, content, _, _, Atom),
      \+ hdt_(Hdt, content, Atom, _, _)
  ).
% source
hdt_term(Hdt, source, atom(source,Atom)) :-
  (   var(Atom)
  ->  hdt_term_(Hdt, source, Atom)
  ;   hdt_(Hdt, content, Atom, _, _),
      \+ hdt_(Hdt, content, _, _, Atom)
  ).
% subject
hdt_term(Hdt, subject, Term) :-
  (   hdt_term(Hdt, source, Term)
  ;   hdt_term(Hdt, shared, Term)
  ).



%! hdt_term_count(+Hdt:blob, +Role, ?Count:nonneg) is det.

% node
hdt_term_count(Hdt, node, Count) :- !,
  maplist(hdt_term_count(Hdt), [shared,sink,source], Counts),
  sum_list(Counts, Count).
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
% term
hdt_term_count(Hdt, term, Count) :-
  maplist(hdt_term_count(Hdt), [predicate,node], Counts),
  sum_list(Counts, Count).



%! hdt_term_prefix(+Hdt:blob, +Prefix:atom, ?Term) is nondet.

hdt_term_prefix(Hdt, Prefix, atom(Role,Atom)) :-
  hdt_prefix_(Hdt, Role, Prefix, Atom).



%! hdt_term_random(+Hdt:blob, +Role, -Term) is nondet.

hdt_term_random(Hdt, node, atom(Role,Atom)) :- !,
  maplist(hdt_term_count(Hdt), [shared,sink,source], [N1,N2,N3]),
  sum_list([N1,N2,N3], N),
  random_between(1, N, Rnd),
  (Rnd =< N1 -> Role = shared ; Rnd =< N2 -> Role = sink ; Role = source),
  hdt_term_rnd_(Hdt, Role, Atom).
% object, predicate, subject
hdt_term_random(Hdt, Role, atom(Role,Atom)) :-
  hdt_term_rnd_(Hdt, Role, Atom).



%! hdt_term_translate(+Hdt:blob, ?RdfTerm, ?HdtAtom) is det.

hdt_term_translate(Hdt, term(Role,Term), id(Role,Atom)) :-
  pre_term(Hdt, Term, Atom),
  post_term(Term, Atom).



%! hdt_triple(+Hdt:blob, ?S, ?P, ?O) is nondet.
%
% True if 〈SId,SIP,SIO〉 is an integer triple in Hdt.

hdt_triple(Hdt, atom(SRole,SAtom), atom(predicate,PAtom), atom(ORole,OAtom)) :-
  pre_triple(SRole, ORole),
  hdt_(Hdt, content, SAtom, PAtom, OAtom),
  (   debugging(hdt_atom)
  ->  maplist(hdt_term_translate(Hdt), [S,P,O],
              [atom(SRole,SAtom),atom(predicate,PAtom),atom(ORole,OAtom)]),
      dcg_debug(hdt_atom, ("TP ",rdf_dcg_triple(S,P,O)))
  ;   true
  ).



%! hdt_triple_count(+Hdt:blob, ?S, ?P, ?O, +Count:nonneg) is semidet.

hdt_triple_count(Hdt, atom(SRole,SAtom), atom(predicate,PAtom),
                 atom(ORole,OAtom), Count) :-
  pre_triple(SRole, ORole),
  hdt_count_(Hdt, SAtom, PAtom, OAtom, Count), !.
hdt_triple_count(_, _, _, _, 0).



%! hdt_triple_random(+Hdt:blob, ?S, ?P, ?O) is semidet.

hdt_triple_random(Hdt, atom(SRole,SAtom), atom(predicate,PAtom),
                  atom(ORole,OAtom)) :-
  pre_triple(SRole, ORole),
  hdt_rnd_(Hdt, SAtom, PAtom, OAtom),
  (   debugging(hdt_atom)
  ->  maplist(hdt_term_translate(Hdt), [S,P,O],
              [atom(SRole,SAtom),atom(predicate,PAtom),atom(ORole,OAtom)]),
      dcg_debug(hdt_atom, ("random ",rdf_dcg_triple(S,P,O)))
  ;   true
  ).



%! hdt_triple_translate(+Hdt:blob, ?Triple:compound,
%!                      ?TripleAtom:compound) is det.

hdt_triple_translate(Hdt, rdf(S,P,O), rdf(SAtom,PAtom,OAtom)) :-
  maplist(hdt_term_translate(Hdt), [S,P,O], [SAtom,PAtom,OAtom]).





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



%! pre_triple(?SRole, ?ORole) is semidet.

pre_triple(SRole, ORole) :-
  (var(SRole) -> true ; memberchk(SRole, [shared,source,subject])),
  (var(ORole) -> true ; memberchk(ORole, [object,shared,sink])).



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
