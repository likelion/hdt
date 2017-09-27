:- module(test_hdt_term, []).

:- reexport(library(hdt_term)).

:- use_module(library(apply)).
:- use_module(library(plunit)).

:- begin_tests(hdt, [cleanup(delete_hdts)]).

test(hdt_create) :-
  expand_file_name('test-*.{nt,ttl}', RdfFiles),
  maplist(hdt_create, RdfFiles, HdtFiles),
  maplist(exists_file, HdtFiles).

test(count, [cleanup(hdt_close(Hdt)),
             nondet,
             setup(hdt_open('test-1.hdt', Hdt)),
             true(Count =:= 1)]) :-
  hdt_triple_count(Hdt, '_:x', _, _, Count).

test(node, [cleanup(hdt_close(Hdt)),
            nondet,
            setup(hdt_open('test-1.hdt', Hdt)),
            set(Term = ['_:x',literal(type('y:y','x:x'))])]) :-
  hdt_term(Hdt, node, Term).

test(object, [cleanup(hdt_close(Hdt)),
              nondet,
              setup(hdt_open('test-1.hdt', Hdt)),
              set(Term = [literal(type('y:y','x:x'))])]) :-
  hdt_term(Hdt, object, Term).

test(predicate, [cleanup(hdt_close(Hdt)),
                 nondet,
                 setup(hdt_open('test-1.hdt', Hdt)),
                 set(Term = ['x:x'])]) :-
  hdt_term(Hdt, predicate, Term).

test(shared, [cleanup(hdt_close(Hdt)),
              nondet,
              setup(hdt_open('test-1.hdt', Hdt)),
              set(Term = [])]) :-
  hdt_term(Hdt, shared, Term).

test(subject, [cleanup(hdt_close(Hdt)),
               nondet,
               setup(hdt_open('test-1.hdt', Hdt)),
               set(Term = ['_:x'])]) :-
  hdt_term(Hdt, subject, Term).

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
