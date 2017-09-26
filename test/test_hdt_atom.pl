:- module(test_hdt, []).

:- reexport(library(hdt_atom)).

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
  hdt_triple_count(Hdt, atom(subject,'_:x'), _, _, Count).

test(node, [cleanup(hdt_close(Hdt)),
            nondet,
            setup(hdt_open('test-1.hdt', Hdt)),
            set(Atom = ['_:x','"x:x"^^<y:y>'])]) :-
  hdt_term(Hdt, node, atom(_,Atom)).

test(object, [cleanup(hdt_close(Hdt)),
              nondet,
              setup(hdt_open('test-1.hdt', Hdt)),
              set(Atom = ['"x:x"^^<y:y>'])]) :-
  hdt_term(Hdt, object, atom(_,Atom)).

test(predicate, [cleanup(hdt_close(Hdt)),
                 nondet,
                 setup(hdt_open('test-1.hdt', Hdt)),
                 set(Atom = ['x:x'])]) :-
  hdt_term(Hdt, predicate, atom(_,Atom)).

test(shared, [cleanup(hdt_close(Hdt)),
              nondet,
              setup(hdt_open('test-1.hdt', Hdt)),
              set(Atom = [])]) :-
  hdt_term(Hdt, shared, atom(_,Atom)).

test(subject, [cleanup(hdt_close(Hdt)),
               nondet,
               setup(hdt_open('test-1.hdt', Hdt)),
               set(Atom = ['_:x'])]) :-
  hdt_term(Hdt, subject, atom(_,Atom)).

test(hdt, [cleanup(hdt_close(Hdt)),
           nondet,
           setup(hdt_open('test-1.hdt', Hdt)),
           set(Triple == [rdf('_:x','x:x','"x:x"^^<y:y>')])]) :-
  hdt_triple(Hdt, atom(_,SAtom), atom(_,PAtom), atom(_,OAtom)),
  Triple = rdf(SAtom,PAtom,OAtom).

:- end_tests(hdt).

delete_hdts :-
  expand_file_name('test-*.hdt*', HdtFiles),
  maplist(delete_file, HdtFiles).
