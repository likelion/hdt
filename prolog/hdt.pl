:- module(hdt,
	  [ hdt_open/2,			% -HDT, +Path
	    hdt_close/1,		% +HDT
	    hdt_search/4,		% +HDT, ?S,?P,?O

	    hdt_property/2,		% +HTD, -Property

	    op(110, xfx, @),		% must be above .
	    op(650, xfx, ^^)		% must be above :
	  ]).

:- use_foreign_library(foreign(hdt4pl)).

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
