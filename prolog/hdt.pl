:- module(hdt,
	  [ hdt_open/2,			% -HDT, +Path
	    hdt_close/1,		% +HDT
	    hdt_search/4,		% +HDT, ?S,?P,?O

	    op(110, xfx, @),		% must be above .
	    op(650, xfx, ^^)		% must be above :
	  ]).

:- use_foreign_library(foreign(hdt4pl)).

