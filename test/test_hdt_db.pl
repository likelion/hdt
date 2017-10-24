:- module(test_hdt_db, []).
:- reexport(library(semweb/hdt_db)).

/** <module> Tests for the HDT library consistent with rdf_db

@author Wouter Beek
@author Jan Wielemaker
@version 2017/09-2017/10
*/

:- use_module(library(apply)).
:- use_module(library(plunit)).

:- begin_tests(hdt, [cleanup(delete_hdts)]).

test(hdt_create) :-
  expand_file_name('test-*.nt', RdfFiles),
  maplist(hdt_create, RdfFiles, _).

test(count, [cleanup(hdt_close(Hdt)),
             nondet,
             setup(hdt_open('test-1.hdt', Hdt)),
             true(Count =:= 1)]) :-
  hdt_triple_count(Hdt, '_:x', _, _, Count).

test(node, [cleanup(hdt_close(Hdt)),
            nondet,
            setup(hdt_open('test-1.hdt', Hdt)),
            set(Node = ['_:x',literal(type('y:y','x:x'))])]) :-
  hdt_term(Hdt, node, Node).

test(object, [cleanup(hdt_close(Hdt)),
              nondet,
              setup(hdt_open('test-1.hdt', Hdt)),
              set(O = [literal(type('y:y','x:x'))])]) :-
  hdt_term(Hdt, object, O).

test(predicate, [cleanup(hdt_close(Hdt)),
                 nondet,
                 setup(hdt_open('test-1.hdt', Hdt)),
                 set(P = ['x:x'])]) :-
  hdt_term(Hdt, predicate, P).

test(shared, [cleanup(hdt_close(Hdt)),
              nondet,
              setup(hdt_open('test-1.hdt', Hdt)),
              set(Shared = [])]) :-
  hdt_term(Hdt, shared, Shared).

test(subject, [cleanup(hdt_close(Hdt)),
               nondet,
               setup(hdt_open('test-1.hdt', Hdt)),
               set(S = ['_:x'])]) :-
  hdt_term(Hdt, subject, S)).

test(hdt, [cleanup(hdt_close(Hdt)),
           nondet,
           setup(hdt_open('test-1.hdt', Hdt)),
           set(Triple == [rdf('_:x','x:x',literal(type('y:y','x:x')))])]) :-
  hdt_triple(Hdt, S, P, O),
  Triple = rdf(S,P,O).

:- end_tests(hdt).

delete_hdts :-
  expand_file_name('test-*.hdt*', HdtFiles),
  maplist(delete_file, HdtFiles).
