:- module(test_hdt_term, []).

:- reexport(library(hdt_term)).

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
  hdt_triple_count(Hdt, rdf(term([subject],'_:x'),_,_), Count).

test(node, [cleanup(hdt_close(Hdt)),
            nondet,
            setup(hdt_open('test-1.hdt', Hdt)),
            set(Term0 = ['_:x',"x:x"^^'y:y'])]) :-
  hdt_term(Hdt, term([shared,sink,source],_), term(_,Term0)).

test(object, [cleanup(hdt_close(Hdt)),
              nondet,
              setup(hdt_open('test-1.hdt', Hdt)),
              set(Term0 = ["x:x"^^'y:y'])]) :-
  hdt_term(Hdt, term([object],_), term(_,Term0)).

test(predicate, [cleanup(hdt_close(Hdt)),
                 nondet,
                 setup(hdt_open('test-1.hdt', Hdt)),
                 set(Term0 = ['x:x'])]) :-
  hdt_term(Hdt, term([predicate],_), term(_,Term0)).

test(shared, [cleanup(hdt_close(Hdt)),
              nondet,
              setup(hdt_open('test-1.hdt', Hdt)),
              set(Term0 = [])]) :-
  hdt_term(Hdt, term([shared],_), term(_,Term0)).

test(subject, [cleanup(hdt_close(Hdt)),
               nondet,
               setup(hdt_open('test-1.hdt', Hdt)),
               set(Term0 = ['_:x'])]) :-
  hdt_term(Hdt, term([subject],_), term(_,Term0)).

test(hdt, [cleanup(hdt_close(Hdt)),
           nondet,
           setup(hdt_open('test-1.hdt', Hdt)),
           set(Triple0 == [rdf('_:x','x:x',"x:x"^^'y:y')])]) :-
  hdt_triple(Hdt, _, rdf(term(_,S0),term(_,P0),term(_,O0))),
  Triple0 = rdf(S0,P0,O0).

:- end_tests(hdt).

delete_hdts :-
  expand_file_name('test-*.hdt*', HdtFiles),
  maplist(delete_file, HdtFiles).
