static int
unify_object(term_t t, const char *s)
{ if ( s[0] == '"' )
  { const char *e = s+strlen(s)-1;

    for(;; e--)
    { while( e>s && *e != '"' )
	e--;
      if ( e > s )
      { if ( e[1] == '\0' )		/* No type nor lang??  In header ... */
	{ term_t av = PL_new_term_refs(2);
	  int rc;

	  s++;
	  rc = PL_unify_chars(t, PL_STRING|REP_UTF8, e-s, s);
	  return rc;
	} else if ( strncmp(e+1, "^^<", 3) == 0 )
	{ term_t av = PL_new_term_refs(2);
	  int rc;

	  s++;
	  rc = PL_unify_chars(av+0, PL_STRING|REP_UTF8, e-s, s);
	  e += 4;
	  rc = rc && PL_unify_chars(av+1, PL_ATOM|REP_UTF8, strlen(e)-1, e);
	  rc = rc && PL_cons_functor_v(av, FUNCTOR_rdftype2, av);
	  rc = rc && PL_unify(t, av);
	  return rc;
	} else if ( strncmp(e+1, "@", 1) == 0 )
	{ term_t av = PL_new_term_refs(2);
	  int rc;

	  s++;
	  rc = PL_unify_chars(av+0, PL_STRING|REP_UTF8, e-s, s);
	  e += 2;
	  rc = rc && PL_unify_chars(av+1, PL_ATOM|REP_UTF8, (size_t)-1, e);
	  rc = rc && PL_cons_functor_v(av, FUNCTOR_rdflang2, av);
	  rc = rc && PL_unify(t, av);
	  return rc;
	}
      } else
      { assert(0);
	return FALSE;
      }
    }
  }

  return PL_unify_chars(t, PL_ATOM|REP_UTF8, (size_t)-1, s);
}
