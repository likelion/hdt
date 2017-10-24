:- module(
  closure,
  [
    closure/3, % :Goal_2, ?X, ?Y
    closure0/3 % :Goal_2, ?X, ?Y
  ]
).

/** <module> Reflexive/transitive closures

I learned about these predicates from Ulrich Neumerkel on
StackOverflow.

@author Wouter Beek
@version 2017/10
*/

:- use_module(library(apply)).
:- use_module(library(dif)).

:- meta_predicate
    closure(2, ?, ?),
    closure0(2, ?, ?),
    closure0(3, ?, ?, +).





%! closure(:Goal_2, +X, -Y) is nondet.
%
% Transitive closure.

closure(Goal_2, X, Z) :-
  call(Goal_2, X, Y),
  closure0(Goal_2, Y, Z, [X,Y]).



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
