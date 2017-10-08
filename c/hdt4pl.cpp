/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2017, VU University Amsterdam
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

#define PL_ARITY_AS_SIZE 1
#include <SWI-Stream.h>
#include <SWI-cpp.h>
#include <iostream>
#include <HDTManager.hpp>
#include <assert.h>
#include <cmath>

using namespace std;
using namespace hdt;

static void deleteHDT(HDT *hdt);
static int get_dict_section(term_t t, DictionarySection *section);
static int get_triple_role(term_t t, TripleComponentRole *role);
static int unify_string(term_t t, const char *s);

#define CATCH_HDT \
	catch (char *e)				\
	{ return hdt_error(e);			\
	} catch (const char *e)			\
	{ return hdt_error(e);			\
	} catch (exception& e)		\
	{ return hdt_error(e.what());		\
	}

#define CVT_TEXT ( CVT_ATOM|CVT_STRING|CVT_EXCEPTION|REP_UTF8 )

extern "C" {

#define URL_xsd		  "http://www.w3.org/2001/XMLSchema#"
#define URL_xsdString     URL_xsd "string"
#define URL_xsdDouble     URL_xsd "double"

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
static atom_t ATOM_access;
static atom_t ATOM_indexed;
static atom_t ATOM_map;
static atom_t ATOM_load;
static atom_t ATOM_header;
static atom_t ATOM_content;
static atom_t ATOM_base_uri;
static atom_t ATOM_source;
static atom_t ATOM_sink;

static atom_t ATOM_format;
static atom_t ATOM_nquads;
static atom_t ATOM_ntriples;
static atom_t ATOM_turtle;
static atom_t ATOM_n3;

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
  MKATOM(access);
  MKATOM(indexed);
  MKATOM(map);
  MKATOM(load);
  MKATOM(content);
  MKATOM(header);
  MKATOM(base_uri);
  MKATOM(source);
  MKATOM(sink);
  MKATOM(format);
  MKATOM(nquads);
  MKATOM(ntriples);
  MKATOM(turtle);
  MKATOM(n3);

  FUNCTOR_rdftype2 = PL_new_functor(PL_new_atom("^^"), 2);
  FUNCTOR_rdflang2 = PL_new_functor(PL_new_atom("@"), 2);
}

}/* end extern "C" */

static void
deleteHDT(HDT *hdt)
{ delete hdt;
}

static int
hdt_error(const char *e)
{ PlCompound f("hdt_error", PlTermv(e));

  return PL_raise_exception(PlCompound("error", PlTermv(f, PlTerm())));
}


		 /*******************************
		 *	     PREDICATES		*
		 *******************************/

// hdt_open(+File, -Hdt, +Options)
PREDICATE(hdt_open_, 3)
{ HDT *hdt;
  atom_t access = ATOM_map;
  int indexed = TRUE;
  PlTail options(A3);
  PlTerm opt;
  char *name;
  while(options.next(opt))
  { atom_t name;
    size_t arity;
    if ( PL_get_name_arity(opt, &name, &arity) && arity == 1 )
    { PlTerm ov = opt[1];
      if ( name == ATOM_access )
      { if ( !PL_get_atom_ex(ov, &access) )
	  return FALSE;
      } else if ( name == ATOM_indexed )
      { if ( !PL_get_bool_ex(ov, &indexed) )
	  return FALSE;
      }
    } else
      return PL_type_error("option", opt);
  }
  if ( !PL_get_file_name(A1, &name, PL_FILE_EXIST) )
    return FALSE;
  try
  { if ( access == ATOM_map )
    { if ( indexed )
	hdt = HDTManager::mapIndexedHDT(name);
      else
	hdt = HDTManager::mapHDT(A1);
    } else if ( access == ATOM_load )
    { if ( indexed )
	hdt = HDTManager::loadIndexedHDT(name);
      else
	hdt = HDTManager::loadHDT(A1);
    } else
    { PlTerm ex;

      PL_put_atom(ex, access);
      return PL_domain_error("hdt_access", ex);
    }
  } CATCH_HDT;
  hdt_wrapper *symb = (hdt_wrapper*)PL_malloc(sizeof(*symb));
  memset(symb, 0, sizeof(*symb));
  symb->hdt = hdt;
  return PL_unify_blob(A2, symb, sizeof(*symb), &hdt_blob);
}


// hdt_close(+Hdt)
PREDICATE(hdt_close_, 1)
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
get_hdt_string(term_t t, char **s, unsigned flag, unsigned *flagp)
{ if ( PL_is_variable(t) )
  { *s = (char*)"";
    *flagp |= flag;
    return TRUE;
  } else
  { size_t len;

    return PL_get_nchars(t, &len, s, CVT_TEXT);
  }
}

// hdt_triple_(+HDT, +Where, ?S, ?P, ?O)
PREDICATE_NONDET(hdt_triple_, 5)
{ hdt_wrapper *symb;
  search_it ctx_buf = {0};
  search_it *ctx;
  int rc;

  switch(PL_foreign_control(handle))
  { case PL_FIRST_CALL:
    { char *s, *p, *o;
      atom_t where;
      ctx = &ctx_buf;
      if ( !get_hdt(A1, &symb) ||
           !PL_get_atom_ex(A2, &where) ||
	   !get_hdt_string(A3, &s, S_S, &ctx->flags) ||
	   !get_hdt_string(A4, &p, S_P, &ctx->flags) ||
	   !get_hdt_string(A5, &o, S_O, &ctx->flags) )
	return FALSE;
      try
      { if ( where == ATOM_content )
	  ctx->it = symb->hdt->search(s,p,o);
	else if ( where == ATOM_header )
	  ctx->it = symb->hdt->getHeader()->search(s,p,o);
	else
	  return PL_domain_error("hdt_where", A2);
      } CATCH_HDT;
      goto next;
    }
    case PL_REDO:
      ctx = (search_it*)PL_foreign_context_address(handle);
    next:
    { if ( ctx->it->hasNext() )
      { TripleString *t = ctx->it->next();
	if ( (!(ctx->flags&S_S) || unify_string(A3, t->getSubject().c_str())) &&
	     (!(ctx->flags&S_P) || unify_string(A4, t->getPredicate().c_str())) &&
	     (!(ctx->flags&S_O) || unify_string(A5, t->getObject().c_str())) )
	{ if ( ctx == &ctx_buf )
	  { ctx = (search_it*)PL_malloc(sizeof(*ctx));
	    *ctx = ctx_buf;
	  }
	  PL_retry_address(ctx);
	}
      }
      rc = FALSE;
      goto cleanup;
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

static int unify_string(term_t t, const char *s)
{
  return PL_unify_chars(t, PL_ATOM|REP_UTF8, (size_t)-1, s);
}


// hdt_term_prefix_(+HDT, +Role, +Prefix, -Term)
PREDICATE_NONDET(hdt_term_prefix_, 4)
{ IteratorUCharString *it;
  switch(PL_foreign_control(handle))
  { case PL_FIRST_CALL:
    { hdt_wrapper *symb;
      TripleComponentRole role;
      char *prefix;
      size_t len;
      if ( !get_hdt(A1, &symb) ||
           !get_triple_role(A2, &role) ||
           !PL_get_nchars(A3, &len, &prefix, CVT_TEXT) )
        return FALSE;
      try
      { it = symb->hdt->getDictionary()->getSuggestions(prefix, role);
      } CATCH_HDT;
      goto next;
    }
    case PL_REDO:
      it = (IteratorUCharString*)PL_foreign_context_address(handle);
    next:
      if ( it->hasNext() )
      { unsigned char *s = it->next();
	int rc;
	rc = PL_unify_chars(A4, PL_ATOM|REP_UTF8, (size_t)-1, (const char*)s);
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
  return FALSE;
}


// hdt_term_prefix_id_(+Hdt, +Role, +Prefix, -Id)
PREDICATE_NONDET(hdt_term_prefix_id_, 4)
{ IteratorUInt *it;
  switch(PL_foreign_control(handle)) {
  case PL_FIRST_CALL:
    {
      hdt_wrapper *symb;
      TripleComponentRole role;
      char *prefix;
      size_t len;
      if ( !get_hdt(A1, &symb) ||
           !get_triple_role(A2, &role) ||
           !PL_get_nchars(A3, &len, &prefix, CVT_TEXT) )
        return FALSE;
      try {
        it = symb->hdt->getDictionary()->getIDSuggestions(prefix, role);
      } CATCH_HDT;
      goto next;
    }
  case PL_REDO:
    {
      it = (IteratorUInt*)PL_foreign_context_address(handle);
    }
  next:
    {
      if ( it->hasNext() ) {
        unsigned int id = it->next();
        int rc = PL_unify_integer(A4, id);
        if ( rc )
          PL_retry_address((void*) it);
      }
    }
    delete it;
    return FALSE;
  case PL_PRUNED:
    {
      it = (IteratorUInt*) PL_foreign_context_address(handle);
      delete it;
      return TRUE;
    }
  }
  return FALSE;
}


		 /*******************************
		 *      DICTIONARY ACCESS	*
		 *******************************/

// hdt_property_(+Hdt, ?Property)
PREDICATE(hdt_property_, 2)
{ hdt_wrapper *symb;
  atom_t name; size_t arity;

  if ( !get_hdt(A1, &symb) )
    return FALSE;

  if ( PL_get_name_arity(A2, &name, &arity) )
  { PlTerm a = A2[1];

    try
    { Dictionary *dict = symb->hdt->getDictionary();

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
    } CATCH_HDT;
  }

  return PL_type_error("compound", A2);
}


// hdt_term_(+Hdt, +Role, -Term)
PREDICATE_NONDET(hdt_term_, 3)
{ IteratorUCharString *it;
  switch(PL_foreign_control(handle))
  { case PL_FIRST_CALL:
    { hdt_wrapper *symb;
      DictionarySection section;
      if ( !get_hdt(A1, &symb) ||
	   !get_dict_section(A2, &section) )
	return FALSE;
      try
      { Dictionary *dict = symb->hdt->getDictionary();
	if ( section == NOT_SHARED_SUBJECT )
	  it = dict->getSubjects();
	else if ( section == NOT_SHARED_PREDICATE )
	  it = dict->getPredicates();
	else if ( section == SHARED_SUBJECT )
	  it = dict->getShared();
	else if ( section == NOT_SHARED_OBJECT )
	  it = dict->getObjects();
	else
	  return PL_domain_error("hdt_term", A2);
      } CATCH_HDT;
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
  return FALSE;
}


// hdt_term_random_(+Hdt, +Role, +Rnd, -Term)
PREDICATE(hdt_term_random_, 4)
{
  hdt_wrapper *symb;
  DictionarySection section;
  double rnd;
  if ( !get_hdt(A1, &symb) ||
       !get_dict_section(A2, &section) ||
       !PL_get_float(A3, &rnd) )
    return FALSE;
  try {
    Dictionary *dict = symb->hdt->getDictionary();
    unsigned int min, max;
    TripleComponentRole role;
    if ( section == NOT_SHARED_OBJECT ) {
      min = dict->getNshared() + 1;
      max = dict->getMaxObjectID();
      role = OBJECT;
    } else if ( section == NOT_SHARED_SUBJECT ) {
      min = dict->getNshared() + 1;
      max = dict->getMaxSubjectID();
      role = SUBJECT;
    } else if ( section == NOT_SHARED_PREDICATE ) {
      min = 1;
      max = dict->getNpredicates();
      role = PREDICATE;
    } else if ( section == SHARED_SUBJECT ) {
      min = 1;
      max = dict->getNshared();
      // We can pick either subject or object here.
      role = SUBJECT;
    }
    size_t index = floor(rnd * (max - min) + min);
    Sprintf("%d from %d..%d\n", index, min, max);
    string str {dict->idToString((size_t) index, role)};
    if ( !str.empty() ) {
      return PL_unify_chars(A3, PL_ATOM|REP_UTF8, (size_t)-1, str.c_str());
    }
  } CATCH_HDT;
  return FALSE;
}


// hdt_term_random_id_(+Hdt, +Role, +Rnd, -Id)
PREDICATE(hdt_term_random_id_, 4)
{ hdt_wrapper *symb;
  DictionarySection section;
  double rnd;
  if ( !get_hdt(A1, &symb) ||
       !get_dict_section(A2, &section) ||
       !PL_get_float(A3, &rnd) ) {
    return FALSE;
  }
  try {
    Dictionary *dict = symb->hdt->getDictionary();
    unsigned int min, max;
    if ( section == NOT_SHARED_OBJECT ) {
      min = dict->getNshared() + 1;
      max = dict->getMaxObjectID();
    } else if ( section == NOT_SHARED_SUBJECT ) {
      min = dict->getNshared() + 1;
      max = dict->getMaxSubjectID();
    } else if ( section == NOT_SHARED_PREDICATE ) {
      min = 1;
      max = dict->getNpredicates();
    } else if ( section == SHARED_SUBJECT ) {
      min = 1;
      max = dict->getNshared();
    }
    unsigned int index = floor(rnd * (max - min) + min);
    Sprintf("%d from 0..%d\n", index, max);
    return PL_unify_integer(A3, index);
  } CATCH_HDT;
  return FALSE;
}


static int get_serialization_format(term_t t, RDFNotation *format)
{
  atom_t name;
  if ( !PL_get_atom_ex(t, &name) ) {
    return FALSE;
  }
  if ( name == ATOM_ntriples ) {
    *format = NTRIPLES;
  } else if ( name == ATOM_nquads ) {
    *format = NQUAD;
  } else if ( name == ATOM_turtle ) {
    *format = TURTLE;
  } else {
    return PL_domain_error("rdf_format", t);
  }
  return TRUE;
}


static int get_triple_role(term_t t, TripleComponentRole *role)
{
  atom_t name;
  if ( !PL_get_atom_ex(t, &name) ) {
    return FALSE;
  }
  if ( name == ATOM_predicate ) {
    *role = PREDICATE;
  } else if ( name == ATOM_shared ) {
    // choose either SUBJECT or OBJECT
    *role = SUBJECT;
  } else if ( name == ATOM_sink ) {
    *role = OBJECT;
  } else if ( name == ATOM_source ) {
    *role = SUBJECT;
  } else if ( name == ATOM_subject ) {
    *role = SUBJECT;
  } else if ( name == ATOM_object ) {
    *role = OBJECT;
  } else {
    return PL_domain_error("triple_role", t);
  }
  return TRUE;
}


static int get_dict_section(term_t t, DictionarySection *section)
{
  atom_t name;
  if ( !PL_get_atom_ex(t, &name) ) {
    return FALSE;
  }
  if ( name == ATOM_predicate ) {
    *section = NOT_SHARED_PREDICATE;
  } else if ( name == ATOM_shared ) {
    // We can pick either `SHARED_SUBJECT' or `SHARED_OBJECT' here.
    *section = SHARED_SUBJECT;
  } else if ( name == ATOM_sink ) {
    *section = NOT_SHARED_OBJECT;
  } else if ( name == ATOM_source ) {
    *section = NOT_SHARED_SUBJECT;
  } else {
    return PL_domain_error("dict_section", t);
  }
  return TRUE;
}


// hdt_term_translate_(+HDT, +Role, ?String, ?Id)
PREDICATE(hdt_term_translate_, 4)
{ hdt_wrapper *symb;
  TripleComponentRole role;
  size_t len;
  char *s;
  if ( !get_hdt(A1, &symb) || !get_triple_role(A2, &role) )
    return FALSE;
  unsigned int id;
  try {
    Dictionary *dict = symb->hdt->getDictionary();
    if ( !PL_is_variable(A3) ) {
      if ( PL_get_nchars(A3, &len, &s, CVT_TEXT) ) {
        string str(s);
        id = dict->stringToId(str, role);
        if ( id )
          return (A4 = (long) id); // signed/unsigned mismatch
      }
    } else {
      string str = dict->idToString((size_t)(long)A4, role);
      if ( !str.empty() )
        return PL_unify_chars(A3, PL_ATOM|REP_UTF8, (size_t)-1, str.c_str());
    }
  } CATCH_HDT;
  return FALSE;
}


typedef struct
{ unsigned flags;
  IteratorTripleID *it;
} searchid_it;


static int
get_hdt_id(term_t t, size_t *id, unsigned flag, unsigned *flagp)
{ if ( PL_is_variable(t) )
  { *id = 0;
    *flagp |= flag;
    return TRUE;
  } else
  { size_t i;

    if ( PL_get_size_ex(t, &i) )
    { *id = i;
      return TRUE;
    }
  }

  return FALSE;
}



// hdt_triple_id_(+HDT, ?SId, ?PId, ?OId)
PREDICATE_NONDET(hdt_triple_id_, 4)
{ hdt_wrapper *symb;
  searchid_it ctx_buf = {0};
  searchid_it *ctx;
  int rc;

  switch(PL_foreign_control(handle))
  { case PL_FIRST_CALL:
    { size_t s, p, o;

      ctx = &ctx_buf;
      if ( !get_hdt(A1, &symb) ||
	   !get_hdt_id(A2, &s, S_S, &ctx->flags) ||
	   !get_hdt_id(A3, &p, S_P, &ctx->flags) ||
	   !get_hdt_id(A4, &o, S_O, &ctx->flags) )
	return FALSE;

      try
      { TripleID t(s,p,o);
	ctx->it = symb->hdt->getTriples()->search(t);
      } CATCH_HDT;

      goto next;
    }
    case PL_REDO:
      ctx = (searchid_it*)PL_foreign_context_address(handle);
    next:
    { if ( ctx->it->hasNext() )
      { TripleID *t = ctx->it->next();

	if ( (!(ctx->flags&S_S) || PL_unify_integer(A2, t->getSubject())) &&
	     (!(ctx->flags&S_P) || PL_unify_integer(A3, t->getPredicate())) &&
	     (!(ctx->flags&S_O) || PL_unify_integer(A4, t->getObject())) )
	{ if ( ctx == &ctx_buf )
	  { ctx = (searchid_it*)PL_malloc(sizeof(*ctx));
	    *ctx = ctx_buf;
	  }
	  PL_retry_address(ctx);
	}
      }
      rc = FALSE;
      goto cleanup;
    }
    case PL_PRUNED:
      ctx = (searchid_it*)PL_foreign_context_address(handle);
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


// hdt_count_(+HDT, ?S, ?P, ?O, -Count)
PREDICATE(hdt_count_, 5)
{ hdt_wrapper *symb;
  unsigned int flags = 0;
  char *s, *p, *o;
  if ( !get_hdt(A1, &symb) ||
       !get_hdt_string(A2, &s, S_S, &flags) ||
       !get_hdt_string(A3, &p, S_P, &flags) ||
       !get_hdt_string(A4, &o, S_O, &flags) )
    return FALSE;
  try {
    IteratorTripleString *it = symb->hdt->search(s,p,o);
    long numResults = it->estimatedNumResults();
    delete it;
    return (A5 = numResults);
  } CATCH_HDT;
}


// hdt_count_id_(+HDT, ?SId, ?PId, ?OId, -Count)
PREDICATE(hdt_count_id_, 5)
{ hdt_wrapper *symb;
  unsigned int flags = 0;
  size_t s, p, o;
  if ( !get_hdt(A1, &symb) ||
       !get_hdt_id(A2, &s, S_S, &flags) ||
       !get_hdt_id(A3, &p, S_P, &flags) ||
       !get_hdt_id(A4, &o, S_O, &flags) )
    return FALSE;
  try {
    TripleID t(s,p,o);
    IteratorTripleID *it = symb->hdt->getTriples()->search(t);
    long numResults = it->estimatedNumResults();
    delete it;
    return (A5 = numResults);
  } CATCH_HDT;
}


// hdt_triple_random_(+HDT, +Rnd, ?S, ?P, ?O)
PREDICATE(hdt_triple_random_, 5)
{
  hdt_wrapper *symb;
  unsigned int flags = 0;
  char *s, *p, *o;
  double rnd;
  if ( !get_hdt(A1, &symb) ||
       !PL_get_float(A2, &rnd) ||
       !get_hdt_string(A3, &s, S_S, &flags) ||
       !get_hdt_string(A4, &p, S_P, &flags) ||
       !get_hdt_string(A5, &o, S_O, &flags) )
    return FALSE;
  try {
    IteratorTripleString *it {symb->hdt->search(s,p,o)};
    size_t count {it->estimatedNumResults()};
    if (count == 0)
      return FALSE;
    size_t index {floor(rnd * (count - 1) + 1)};
    Sprintf("%d from 0..%d\n", index, count-1);
    it->skip(index);
    if (it->hasNext()) {
      TripleString *t {it->next()};
      bool rc =
        ( (!(flags&S_S) || unify_string(A3, t->getSubject().c_str())) &&
          (!(flags&S_P) || unify_string(A4, t->getPredicate().c_str())) &&
          (!(flags&S_O) || unify_string(A5, t->getObject().c_str())) );
      delete it;
      return rc;
    }
  } CATCH_HDT;
  return FALSE;
}


// hdt_triple_random_id_(+HDT, +Rnd, ?SId, ?PId, ?OId)
PREDICATE(hdt_triple_random_id_, 5)
{
  hdt_wrapper *symb;
  unsigned int flags = 0;
  size_t s, p, o;
  double rnd;
  if ( !get_hdt(A1, &symb) ||
       !PL_get_float(A2, &rnd) ||
       !get_hdt_id(A3, &s, S_S, &flags) ||
       !get_hdt_id(A4, &p, S_P, &flags) ||
       !get_hdt_id(A5, &o, S_O, &flags) )
    return FALSE;
  try {
    TripleID pattern(s, p, o);
    IteratorTripleID *it {symb->hdt->getTriples()->search(pattern)};
    size_t count {it->estimatedNumResults()};
    if (count == 0)
      return FALSE;
    size_t index {floor(rnd * (count - 1) + 1)};
    Sprintf("%d from 0..%d\n", index, count-1);
    it->skip(index);
    if (it->hasNext()) {
      TripleID *t {it->next()};
      bool rc =
        ( (!(flags&S_S) || PL_unify_integer(A3, t->getSubject())) &&
          (!(flags&S_P) || PL_unify_integer(A4, t->getPredicate())) &&
          (!(flags&S_O) || PL_unify_integer(A5, t->getObject())) );
      delete it;
      return rc;
    }
  } CATCH_HDT;
  return FALSE;
}



		 /*******************************
		 *	      GENERATE		*
		 *******************************/

/**
 * hdt_create_(+HDTFile, +RDFFile, +Options)
 *
 * @tbd Fill HDTSpecification
 * @tbd Allow additional header triples
 */

PREDICATE(hdt_create_, 3)
{ char *hdt_file, *rdf_file;
  HDTSpecification spec;
  char *base_uri = (char*)"http://example.org/base";
  RDFNotation format = NTRIPLES;

  if ( !PL_get_file_name(A1, &hdt_file, PL_FILE_OSPATH) ||
       !PL_get_file_name(A2, &rdf_file, PL_FILE_OSPATH|PL_FILE_READ) )
    return FALSE;

  PlTail options(A3);
  PlTerm opt;
  while(options.next(opt))
  { atom_t name;
    size_t arity;

    if ( PL_get_name_arity(opt, &name, &arity) && arity == 1 )
    { PlTerm ov = opt[1];

      if ( name == ATOM_base_uri ) {
        size_t len;
	if ( !PL_get_nchars(ov, &len, &base_uri, CVT_TEXT) )
	  return FALSE;
      } else if ( name == ATOM_format ) {
        if ( !get_serialization_format(ov, &format) ) {
          return FALSE;
        }
      }
    } else
      return PL_type_error("option", opt);
  }

  try
  { HDT *hdt = HDTManager::generateHDT(rdf_file, base_uri, format, spec);

    //Header *header = hdt->getHeader();
    //header->insert("myResource1", "property", "value");

    hdt->saveToHDT(hdt_file);

    delete hdt;
  } CATCH_HDT

  return TRUE;
}
