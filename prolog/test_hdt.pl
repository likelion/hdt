:- module(test_hdt, []).

:- use_module(library(apply)).
:- use_module(library(hdt)).
:- use_module(library(plunit)).



:- begin_tests(hdt, [cleanup(clear_hdts)]).

test(hdt_create) :-
  expand_file_name('test*.nt', Files),
  maplist(hdt_create, Files).

test(count, [cleanup(hdt_close(HDT)),
             nondet,
             setup(hdt_open('test1.hdt', HDT)),
             true(Count =:= 1)]) :-
  hdt_count(HDT, '_:x', _, _, Count).

test(node, [cleanup(hdt_close(HDT)),
            nondet,
            setup(hdt_open('test1.hdt', HDT)),
            all(Term = ['_:x',"x:x"^^'y:y'])]) :-
  hdt_term(HDT, node, Term).

test(object, [cleanup(hdt_close(HDT)),
              nondet,
              setup(hdt_open('test1.hdt', HDT)),
              all(Term = ["x:x"^^'y:y'])]) :-
  hdt_term(HDT, object, Term).

test(predicate, [cleanup(hdt_close(HDT)),
                 nondet,
                 setup(hdt_open('test1.hdt', HDT)),
                 all(Term = ['x:x'])]) :-
  hdt_term(HDT, predicate, Term).

test(shared, [cleanup(hdt_close(HDT)),
              nondet,
              setup(hdt_open('test1.hdt', HDT)),
              all(Term = [])]) :-
  hdt_term(HDT, shared, Term).

test(subject, [cleanup(hdt_close(HDT)),
               nondet,
               setup(hdt_open('test1.hdt', HDT)),
               all(Term = ['_:x'])]) :-
  hdt_term(HDT, subject, Term).

test(hdt, [cleanup(hdt_close(HDT)),
           nondet,
           setup(hdt_open('test1.hdt', HDT)),
           all(Triple == [rdf('_:x','x:x',"x:x"^^'y:y')])]) :-
  hdt(HDT, S, P, O),
  Triple = rdf(S,P,O).

:- end_tests(hdt).



% HELPERS %

clear_hdts :-
  expand_file_name('test*.hdt*', Files),
  maplist(delete_file, Files).


hdt_create(RDFFile) :-
  file_name_extension(Base, nt, RDFFile),
  file_name_extension(Base, hdt, HDTFile),
  hdt_create(RDFFile, HDTFile).
