:- module(test_hdt, []).

:- use_module(library(apply)).
:- use_module(library(hdt)).
:- use_module(library(plunit)).
:- use_module(library(semweb/rdf11)).

:- begin_tests(hdt, [cleanup(delete_hdts)]).

test(hdt_create) :-
  expand_file_name('test-*.nt', RdfFiles),
  maplist(hdt_create, RdfFiles).

test(count, [cleanup(hdt_close(G)),
             nondet,
             setup(hdt_open('test-1.hdt', [graph(G)])),
             true(Count =:= 1)]) :-
  hdt_count('_:x', _, _, Count, G).

test(node, [cleanup(hdt_close(G)),
            nondet,
            setup(hdt_open('test-1.hdt', [graph(G)])),
            all(Term = ['_:x',"x:x"^^'y:y'])]) :-
  hdt_term(node, Term, G).

test(object, [cleanup(hdt_close(G)),
              nondet,
              setup(hdt_open('test-1.hdt', [graph(G)])),
              all(Term = ["x:x"^^'y:y'])]) :-
  hdt_term(object, Term, G).

test(predicate, [cleanup(hdt_close(G)),
                 nondet,
                 setup(hdt_open('test-1.hdt', [graph(G)])),
                 all(Term = ['x:x'])]) :-
  hdt_term(predicate, Term, G).

test(shared, [cleanup(hdt_close(G)),
              nondet,
              setup(hdt_open('test-1.hdt', [graph(G)])),
              all(Term = [])]) :-
  hdt_term(shared, Term, G).

test(subject, [cleanup(hdt_close(G)),
               nondet,
               setup(hdt_open('test-1.hdt', [graph(G)])),
               all(Term = ['_:x'])]) :-
  hdt_term(subject, Term, G).

test(hdt, [cleanup(hdt_close(G)),
           nondet,
           setup(hdt_open('test-1.hdt', [graph(G)])),
           all(Triple == [rdf('_:x','x:x',"x:x"^^'y:y')])]) :-
  hdt(S, P, O, G),
  Triple = rdf(S,P,O).

:- end_tests(hdt).



% HELPERS %

delete_hdts :-
  expand_file_name('test-*.hdt*', HdtFiles),
  maplist(delete_file, HdtFiles).
