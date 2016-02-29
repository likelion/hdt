:- module(hdt,
	  [ hdt_open/2,			% -HDT, +Path
	    hdt_open/3,			% -HDT, +Path, +Options
	    hdt_close/1,		% +HDT
	    hdt_search/4,		% +HDT, ?S,?P,?O

	    hdt_subject/2,		% +HDT, -Subject
	    hdt_predicate/2,		% +HDT, -Predicate
	    hdt_shared/2,		% +HDT, -Shared
	    hdt_object/2,		% +HDT, -Object

	    hdt_property/2,		% +HTD, -Property

	    op(110, xfx, @),		% must be above .
	    op(650, xfx, ^^)		% must be above :
	  ]).

:- use_foreign_library(foreign(hdt4pl)).

hdt_open(HDT, File) :-
	hdt_open(HDT, File, []).

hdt_subject(HDT, Subject) :-
	hdt_column_(HDT, subject, Var),
	Var = Subject.

hdt_predicate(HDT, Subject) :-
	hdt_column_(HDT, predicate, Var),
	Var = Subject.

hdt_shared(HDT, Subject) :-
	hdt_column_(HDT, shared, Var),
	Var = Subject.

hdt_object(HDT, Subject) :-
	hdt_object_(HDT, Var),
	Var = Subject.

hdt_property(HDT, Property) :-
	hdt_property(Property),
	hdt_property_(HDT, Property).

hdt_property(mapping(_)).
hdt_property(max_id(_)).
hdt_property(max_object_id(_)).
hdt_property(max_predicate_id(_)).
hdt_property(max_subject_id(_)).
hdt_property(objects(_)).
hdt_property(predicates(_)).
hdt_property(shared(_)).
hdt_property(subjects(_)).
hdt_property(elements(_)).


		 /*******************************
		 *	      MESSAGES		*
		 *******************************/

:- multifile prolog:error_message//1.

prolog:error_message(hdt_error(Message)) -->
	[ 'HDT: ~w'-[Message] ].
