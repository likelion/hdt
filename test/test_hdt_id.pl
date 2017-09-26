:- module(test_hdt, []).

:- reexport(library(hdt_id)).

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
  hdt_term_translate(Hdt, term(subject,'_:x'), S),
  hdt_triple_count(Hdt, S, _, _, Count).

test(node, [cleanup(hdt_close(Hdt)),
            nondet,
            setup(hdt_open('test-1.hdt', Hdt)),
            set(Term0 = ['_:x',"x:x"^^'y:y'])]) :-
  hdt_term(Hdt, node, Term),
  hdt_term_result(Hdt, Term, Term0).

test(object, [cleanup(hdt_close(Hdt)),
              nondet,
              setup(hdt_open('test-1.hdt', Hdt)),
              set(Term0 = ["x:x"^^'y:y'])]) :-
  hdt_term(Hdt, object, Term),
  hdt_term_result(Hdt, Term, Term0).

test(predicate, [cleanup(hdt_close(Hdt)),
                 nondet,
                 setup(hdt_open('test-1.hdt', Hdt)),
                 set(Term0 = ['x:x'])]) :-
  hdt_term(Hdt, predicate, Term),
  hdt_term_result(Hdt, Term, Term0).

test(shared, [cleanup(hdt_close(Hdt)),
              nondet,
              setup(hdt_open('test-1.hdt', Hdt)),
              set(Term0 = [])]) :-
  hdt_term(Hdt, shared, Term),
  hdt_term_result(Hdt, Term, Term0).

test(subject, [cleanup(hdt_close(Hdt)),
               nondet,
               setup(hdt_open('test-1.hdt', Hdt)),
               set(Term0 = ['_:x'])]) :-
  hdt_term(Hdt, subject, Term),
  hdt_term_result(Hdt, Term, Term0).

test(hdt, [cleanup(hdt_close(Hdt)),
           nondet,
           setup(hdt_open('test-1.hdt', Hdt)),
           set(Triple0 == [rdf('_:x','x:x',"x:x"^^'y:y')])]) :-
  hdt_triple(Hdt, S, P, O),
  hdt_triple_result(Hdt, rdf(S,P,O), Triple0).

:- end_tests(hdt).

delete_hdts :-
  expand_file_name('test-*.hdt*', HdtFiles),
  maplist(delete_file, HdtFiles).

hdt_term_result(Hdt, Term, Term0) :-
  hdt_term_translate(Hdt, term(_,Term0), Term).

hdt_triple_result(Hdt, rdf(S,P,O), rdf(S0,P0,O0)) :-
  maplist(hdt_term_result(Hdt), [S,P,O], [S0,P0,O0]).
