#define PL_ARITY_AS_SIZE 1
#include <SWI-Stream.h>
#include <SWI-cpp.h>
#include <iostream>
#include <HDTManager.hpp>
#include <assert.h>

using namespace std;
using namespace hdt;

static void deleteHDT(HDT *hdt);

extern "C" {

static atom_t ATOM_mapping;
static atom_t ATOM_max_id;
static atom_t ATOM_max_object_id;
static atom_t ATOM_max_predicate_id;
static atom_t ATOM_max_subject_id;
static atom_t ATOM_objects;
static atom_t ATOM_predicates;
static atom_t ATOM_shared;
static atom_t ATOM_subjects;
static atom_t ATOM_elements;
static atom_t ATOM_subject;
static atom_t ATOM_predicate;
static atom_t ATOM_object;

static functor_t FUNCTOR_rdftype2;
static functor_t FUNCTOR_rdflang2;

typedef struct hdt_wrapper
{ atom_t	symbol;			/* Associated symbol */
  HDT	       *hdt;
} hdt_wrapper;


static void
acquire_hdt(atom_t symbol)
{ hdt_wrapper *symb = (hdt_wrapper*)PL_blob_data(symbol, NULL, NULL);
  symb->symbol = symbol;
}


static int
release_hdt(atom_t symbol)
{ hdt_wrapper *symb = (hdt_wrapper*)PL_blob_data(symbol, NULL, NULL);

  if ( symb->hdt )
  { deleteHDT(symb->hdt);
    symb->hdt = NULL;
  }
  PL_free(symb);

  return TRUE;
}

static int
compare_hdts(atom_t a, atom_t b)
{ hdt_wrapper *ara = (hdt_wrapper*)PL_blob_data(a, NULL, NULL);
  hdt_wrapper *arb = (hdt_wrapper*)PL_blob_data(b, NULL, NULL);

  return ( ara > arb ?  1 :
	   ara < arb ? -1 : 0
	 );
}


static int
write_hdt(IOSTREAM *s, atom_t symbol, int flags)
{ hdt_wrapper *symb = (hdt_wrapper*)PL_blob_data(symbol, NULL, NULL);

  Sfprintf(s, "<hdt>(%p)", symb);

  return TRUE;
}

static PL_blob_t hdt_blob =
{ PL_BLOB_MAGIC,
  PL_BLOB_NOCOPY,
  (char*)"hdt",
  release_hdt,
  compare_hdts,
  write_hdt,
  acquire_hdt
};


static int
get_hdt(term_t t, hdt_wrapper **symb_ptr)
{ PL_blob_t *type;
  void *data;

  if ( PL_get_blob(t, &data, NULL, &type) && type == &hdt_blob)
  { hdt_wrapper *symb = (hdt_wrapper*)data;

    if ( !symb->hdt )
      return PL_existence_error("hdt", t);
    *symb_ptr = symb;

    return TRUE;
  }

  return PL_type_error("hdt", t);
}


#define MKATOM(a) ATOM_ ## a = PL_new_atom(#a)

install_t
install_hdt4pl(void)
{ MKATOM(mapping);
  MKATOM(max_id);
  MKATOM(max_object_id);
  MKATOM(max_predicate_id);
  MKATOM(max_subject_id);
  MKATOM(objects);
  MKATOM(predicates);
  MKATOM(shared);
  MKATOM(subjects);
  MKATOM(elements);
  MKATOM(subject);
  MKATOM(predicate);
  MKATOM(object);

  FUNCTOR_rdftype2 = PL_new_functor(PL_new_atom("^^"), 2);
  FUNCTOR_rdflang2 = PL_new_functor(PL_new_atom("@"), 2);
}

}/* end extern "C" */

static void
deleteHDT(HDT *hdt)
{ delete hdt;
}



		 /*******************************
		 *	     PREDICATES		*
		 *******************************/


PREDICATE(hdt_open, 2)
{ HDT *hdt = HDTManager::mapHDT(A2);
  hdt_wrapper *symb = (hdt_wrapper*)PL_malloc(sizeof(*symb));

  memset(symb, 0, sizeof(*symb));
  symb->hdt = hdt;

  return PL_unify_blob(A1, symb, sizeof(*symb), &hdt_blob);
}


PREDICATE(hdt_close, 1)
{ hdt_wrapper *symb;

  if ( !get_hdt(A1, &symb) )
    return FALSE;

  deleteHDT(symb->hdt);				/* FIXME: Thread safety */
  symb->hdt = NULL;

  return TRUE;
}


#define S_S 0x01
#define S_P 0x02
#define S_O 0x04

typedef struct
{ unsigned flags;
  IteratorTripleString *it;
} search_it;


static int
get_search_string(term_t t, char **s, unsigned flag, unsigned *flagp)
{ if ( PL_is_variable(t) )
  { *s = (char*)"";
    *flagp |= flag;
    return TRUE;
  } else
  { size_t len;

    return PL_get_nchars(t, &len, s, CVT_ATOM|CVT_STRING|CVT_EXCEPTION|REP_UTF8);
  }
}

static int
unify_string(term_t t, const char *s)
{ return PL_unify_chars(t, PL_ATOM|REP_UTF8, (size_t)-1, s);
}


static int
unify_object(term_t t, const char *s)
{ if ( s[0] == '"' )
  { const char *e = s+strlen(s)-1;

    for(;;)
    { while( e>s && *e != '"' )
	e--;
      if ( e > s )
      { if ( strncmp(e+1, "^^<", 3) == 0 )
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



PREDICATE_NONDET(hdt_search, 4)
{ hdt_wrapper *symb;
  search_it ctx_buf = {0};
  search_it *ctx;
  int rc;

  switch(PL_foreign_control(handle))
  { case PL_FIRST_CALL:
    { char *s, *p, *o;

      ctx = &ctx_buf;
      if ( !get_hdt(A1, &symb) )
	return FALSE;
      if ( !get_search_string(A2, &s, S_S, &ctx->flags) ||
	   !get_search_string(A3, &p, S_O, &ctx->flags) ||
	   !get_search_string(A4, &o, S_P, &ctx->flags) )
	return FALSE;
      ctx->it = symb->hdt->search(s,p,o);
      goto next;
    }
    case PL_REDO:
      ctx = (search_it*)PL_foreign_context_address(handle);
    next:
    { if ( ctx->it->hasNext() )
      { TripleString *triple = ctx->it->next();

	if ( (!(ctx->flags&S_S) || unify_string(A2, triple->getSubject().c_str())) &&
	     (!(ctx->flags&S_P) || unify_string(A3, triple->getPredicate().c_str())) &&
	     (!(ctx->flags&S_O) || unify_object(A4, triple->getObject().c_str())) )
	{ if ( ctx == &ctx_buf )
	  { ctx = (search_it*)PL_malloc(sizeof(*ctx));
	    *ctx = ctx_buf;
	  }
	  PL_retry_address(ctx);
	}
	rc = FALSE;
	goto cleanup;
      }
    }
    case PL_PRUNED:
      ctx = (search_it*)PL_foreign_context_address(handle);
      rc = TRUE;
    cleanup:
      if ( ctx->it )
	delete ctx->it;
      if ( ctx != &ctx_buf )
	PL_free(ctx);
      return rc;
  }

  return FALSE;
}


		 /*******************************
		 *      DICTIONARY ACCESS	*
		 *******************************/

PREDICATE(hdt_property_, 2)
{ hdt_wrapper *symb;
  atom_t name; size_t arity;

  if ( !get_hdt(A1, &symb) )
    return FALSE;

  if ( PL_get_name_arity(A2, &name, &arity) )
  { PlTerm a = A2[1];
    Dictionary *dict = symb->hdt->getDictionary();

    if ( name == ATOM_mapping )
      return (a = (long)dict->getMapping());
    else if ( name == ATOM_max_id )
      return (a = (long)dict->getMaxID());
    else if ( name == ATOM_max_object_id )
      return (a = (long)dict->getMaxObjectID());
    else if ( name == ATOM_max_predicate_id )
      return (a = (long)dict->getMaxPredicateID());
    else if ( name == ATOM_max_subject_id )
      return (a = (long)dict->getMaxSubjectID());
    else if ( name == ATOM_objects )
      return (a = (long)dict->getNobjects());
    else if ( name == ATOM_predicates )
      return (a = (long)dict->getNpredicates());
    else if ( name == ATOM_shared )
      return (a = (long)dict->getNshared());
    else if ( name == ATOM_subjects )
      return (a = (long)dict->getNsubjects());
    else if ( name == ATOM_elements )
      return (a = (long)dict->getNumberOfElements());
    else
      return PL_domain_error("hdt_property", A2);
  }

  return PL_type_error("compound", A2);
}


PREDICATE_NONDET(hdt_column_, 3)
{ IteratorUCharString *it;

  switch(PL_foreign_control(handle))
  { case PL_FIRST_CALL:
    { hdt_wrapper *symb;
      atom_t a;

      if ( !get_hdt(A1, &symb) ||
	   !PL_get_atom_ex(A2, &a) )
	return FALSE;

      Dictionary *dict = symb->hdt->getDictionary();
      if ( a == ATOM_subject )
	it = dict->getSubjects();
      else if ( a == ATOM_predicate )
	it = dict->getPredicates();
      else if ( a == ATOM_shared )
	it = dict->getShared();
      else if ( a == ATOM_object )
	it = dict->getObjects();
      else
	return PL_domain_error("hdt_column", A2);

      goto next;
    }
    case PL_REDO:
      it = (IteratorUCharString*)PL_foreign_context_address(handle);
    next:
      if ( it->hasNext() )
      { unsigned char *s = it->next();
	int rc;

	rc = PL_unify_chars(A3, PL_ATOM|REP_UTF8, (size_t)-1, (const char*)s);
	it->freeStr(s);
	if ( rc )
	  PL_retry_address((void*)it);
      }
      delete it;
      return FALSE;
    case PL_PRUNED:
      it = (IteratorUCharString*)PL_foreign_context_address(handle);
      delete it;
      return TRUE;
  }
}


PREDICATE_NONDET(hdt_object_, 2)
{ IteratorUCharString *it;
  uintptr_t mask = 0;

  switch(PL_foreign_control(handle))
  { case PL_FIRST_CALL:
    { hdt_wrapper *symb;
      atom_t a;

      if ( !get_hdt(A1, &symb) )
	return FALSE;

      it = symb->hdt->getDictionary()->getObjects();
      goto next;
    }
    case PL_REDO:
      it = (IteratorUCharString*)PL_foreign_context_address(handle);
    next:
      if ( it->hasNext() )
      { unsigned char *s = it->next();
	int rc;

	rc = unify_object(A2, (const char*)s);
	it->freeStr(s);
	if ( rc )
	  PL_retry_address((void*)it);
      }
      delete it;
      return FALSE;
    case PL_PRUNED:
      it = (IteratorUCharString*)PL_foreign_context_address(handle);
      delete it;
      return TRUE;
  }
}
