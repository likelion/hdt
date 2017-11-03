:- module(test_hdt, []).
:- reexport(library(hdt)).

/** <module> Tests for the HDT library

@author Wouter Beek
@author Jan Wielemaker
@version 2017/09-2017/11
*/

:- use_module(library(apply)).
:- use_module(library(plunit)).

:- begin_tests(hdt, [cleanup(delete_hdts)]).

test(hdt_create) :-
  expand_file_name('test-*.{nt,ttl}', RdfFiles),
  maplist(hdt_create_from_file, HdtFiles, RdfFiles),
  maplist(exists_file, HdtFiles).

test(count, [cleanup(hdt_close(Hdt)),
             nondet,
             setup(hdt_open(Hdt, 'test-1.hdt')),
             true(Count =:= 1)]) :-
  hdt_triple_count(Hdt, '_:x', _, _, Count).

test(node, [cleanup(hdt_close(Hdt)),
            nondet,
            setup(hdt_open(Hdt, 'test-1.hdt')),
            set(Term = ['_:x',"x:x"^^'y:y'])]) :-
  hdt_term(Hdt, node, Term).

test(object, [cleanup(hdt_close(Hdt)),
              nondet,
              setup(hdt_open(Hdt, 'test-1.hdt')),
              set(Term = ["x:x"^^'y:y'])]) :-
  hdt_term(Hdt, object, Term).

test(predicate, [cleanup(hdt_close(Hdt)),
                 nondet,
                 setup(hdt_open(Hdt, 'test-1.hdt')),
                 set(Term = ['x:x'])]) :-
  hdt_term(Hdt, predicate, Term).

test(shared, [cleanup(hdt_close(Hdt)),
              nondet,
              setup(hdt_open(Hdt, 'test-1.hdt')),
              set(Term = [])]) :-
  hdt_term(Hdt, shared, Term).

test(subject, [cleanup(hdt_close(Hdt)),
               nondet,
               setup(hdt_open(Hdt, 'test-1.hdt')),
               set(Term = ['_:x'])]) :-
  hdt_term(Hdt, subject, Term).

test(triple, [cleanup(hdt_close(Hdt)),
              nondet,
              setup(hdt_open(Hdt, 'test-1.hdt')),
              set(Triple == [rdf('_:x','x:x',"x:x"^^'y:y')])]) :-
  hdt_triple(Hdt, S, P, O),
  Triple = rdf(S,P,O).

test(triple_uninterpreted, [cleanup(hdt_close(Hdt)),
                            nondet,
                            setup(hdt_open(Hdt, 'test-1.hdt')),
                            set(Triple == [rdf('_:x','x:x',"x:x"^^'y:y')])]) :-
  hdt:hdt_triple_uninterpreted(Hdt, S, P, O),
  Triple = rdf(S,P,O).

:- end_tests(hdt).

delete_hdts :-
  expand_file_name('test-*.hdt*', HdtFiles),
  maplist(delete_file, HdtFiles).
