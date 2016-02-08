#include <SWI-Stream.h>
#include <SWI-cpp.h>
#include <iostream>
#include <HDTManager.hpp>

using namespace std;
using namespace hdt;

static void deleteHDT(HDT *hdt);

extern "C" {

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


PREDICATE(hdt_destroy, 1)
{ hdt_wrapper *symb;

  if ( !get_hdt(A1, &symb) )
    return FALSE;

  deleteHDT(symb->hdt);
  symb->hdt = NULL;

  return TRUE;
}


PREDICATE(hdt_search, 4)
{ hdt_wrapper *symb;

  if ( !get_hdt(A1, &symb) )
    return FALSE;

  return TRUE;
}
