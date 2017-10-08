:- module(
  hdt_id,
  [
    hdt_term_id/3,         % +Hdt, +Role, ?Id
    hdt_term_id_count/3,   % +Hdt, ?Role, ?Count
    hdt_term_id_prefix/3,  % +Hdt, +Prefix, ?Id
    hdt_term_id_random/3,  % +Hdt, +Role, -Id
    hdt_triple_id/4,       % +Hdt, ?SId, ?PId, ?OId
    hdt_triple_id_count/5, % +Hdt, ?SId, ?PId, ?OId, ?Count
    hdt_triple_id_random/4 % +Hdt, ?SId, ?PId, ?OId
  ]
).
:- reexport(library(hdt_generic)).

/** <module> HDT by ID

@author Wouter Beek
@author Jan Wielemaker
@version 2017/09
*/

:- use_module(library(apply)).
:- use_module(library(dcg/dcg_ext)).
:- use_module(library(debug)).
:- use_module(library(error)).
:- use_module(library(lists)).
:- use_module(library(semweb/rdf_prefix), []).
:- use_module(library(semweb/rdf_print)).
:- use_module(library(sgml)).





%! hdt_term_id(+Hdt:blob, +Role:atom, ?Id:compound) is nondet.

% sink
hdt_term_id(Hdt, sink, id(sink,Id)) :- !,
  maplist(hdt_term_id_count(Hdt), [shared,object], [Offset,Max]),
  Min is Offset + 1,
  between(Min, Max, Id).
% source
hdt_term_id(Hdt, source, id(source,Id)) :- !,
  maplist(hdt_term_id_count(Hdt), [shared,subject], [Offset,Max]),
  Min is Offset + 1,
  between(Min, Max, Id).
% object, predicate, shared, subject
hdt_term_id(Hdt, Role, id(Role,Id)) :-
  hdt_term_id_count(Hdt, Role, N),
  between(1, N, Id).
% others: node, term
hdt_term_id(Hdt, Role1, Id) :- !,
  subrole(Role1, Role2),
  member(Role, [shared,sink,source]),
  hdt_term_id(Hdt, Role, Id).



%! hdt_term_id_prefix(+Hdt:blob, +Prefix:atom, ?Id:compound) is nondet.

hdt_term_id_prefix(Hdt, Prefix, id(Role,Id)) :-
  hdt_prefix_id_(Hdt, Role, Prefix, Id).



%! hdt_term_id_random(+Hdt:blob, +Role:atom, -Id:compound) is nondet.

hdt_term_id_random(Hdt, node, id(Role,Id)) :- !,
  maplist(hdt_term_id_count(Hdt), [shared,sink,source], [N1,N2,N3]),
  sum_list([N1,N2,N3], N),
  random_between(1, N, Rnd),
  (Rnd =< N1 -> Role = shared ; Rnd =< N2 -> Role = sink ; Role = source),
  hdt_term_id_rnd_id_(Hdt, Role, Id).
% object, predicate, subject
hdt_term_id_random(Hdt, Role, id(Role,Id)) :-
  hdt_term_id_rnd_id_(Hdt, Role, Id).



%! hdt_triple_id(+Hdt:blob, ?SId:compound, ?PId:compound,
%!               ?OId:compound) is nondet.
%
% True if 〈SId,PId,OId〉 is an integer triple in Hdt.

hdt_triple_id(Hdt, id(SRole,SId), id(predicate,PId), id(ORole,OId)) :-
  pre_triple_id(SRole, ORole),
  hdt_id_(Hdt, SId, PId, OId),
  post_triple_id(Hdt, id(SRole,SId), id(ORole,OId)),
  (   debugging(hdt_id)
  ->  maplist(hdt_term_translate(Hdt), [S,P,O],
              [id(SRole,SId),id(predicate,PId),id(ORole,OId)]),
      dcg_debug(hdt_id, ("TP ",rdf_dcg_triple(S,P,O)))
  ;   true
  ).



%! hdt_triple_id_count(+Hdt:blob, ?SId:compound, ?PId:compound, ?OId:compound,
%!                     -Count:nonneg) is det.

hdt_triple_id_count(Hdt, id(SRole,SId), id(predicate,PId), id(ORole,OId), Count) :-
  pre_triple_id(SRole, ORole),
  hdt_count_id_(Hdt, SId, PId, OId, Count), !.
hdt_triple_id_count(_, _, _, _, 0).



%! hdt_triple_id_random(+Hdt:blob, ?SId:compound, ?PId:compound,
%!                      ?OId:compound) is nondet.

hdt_triple_id_random(Hdt, id(SRole,SId), id(predicate,PId), id(ORole,OId)) :-
  pre_triple_id(SRole, ORole),
  hdt_rnd_id_(Hdt, SId, PId, OId),
  post_triple_id(Hdt, id(SRole,SId), id(ORole,OId)),
  (   debugging(hdt_id)
  ->  maplist(hdt_term_translate(Hdt), [S,P,O],
              [id(SRole,SId),id(predicate,PId),id(ORole,OId)]),
      dcg_debug(hdt_id, ("random ",rdf_dcg_triple(S,P,O)))
  ;   true
  ).





% HELPERS %

%! pre_triple_id(?SRole, ?ORole) is semidet.

pre_triple_id(SRole, ORole) :-
  (var(SRole) -> SRole = subject ; memberchk(SRole, [shared,source,subject])),
  (var(ORole) -> ORole = object ; memberchk(ORole, [object,shared,sink])).



%! post_triple_id(+Hdt:blob, ?SId, ?OId) is semidet.

post_triple_id(Hdt, id(SRole,SId), id(ORole,OId)) :-
  hdt_term_count(Hdt, shared, Max),
  (SId > Max -> SRole = source ; SRole = shared),
  (OId > Max -> ORole = sink ; ORole = shared).
