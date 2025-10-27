/* Implementation of GC handles
   Copyright (C) 2025 Free Software Foundation, Inc.

This file is part of GNU Emacs.

GNU Emacs is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or (at
your option) any later version.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>. */

/* GC-handles can be used to retain Lisp_Objects.  A GC-handle keeps
   its Lisp_Object alive until the GC-handle is freed.  */

#include "gc-handles.h"

struct gc_handle_struct
{
  size_t index;
  size_t refcount;
};

static Lisp_Object global_handle_table;

static void
shrink_handle_table (void)
{
  Lisp_Object new_table = CALLN (Fmake_hash_table, QCtest, Qeq);
  DOHASH (XHASH_TABLE (global_handle_table), k, v)
  {
    gc_handle gch = xmint_pointer (v);
    Fputhash (k, v, new_table);
    gch->index = hash_find (XHASH_TABLE (new_table), k);
  }
  global_handle_table = new_table;
}

gc_handle
gc_handle_for (Lisp_Object obj)
{
  Lisp_Object handle;
  handle = Fgethash (obj, global_handle_table, Qnil);
  if (NILP (handle))
    {
      gc_handle gch = xmalloc (sizeof (*gch));
      gch->refcount = 1;
      handle = make_mint_ptr (gch);
      Fputhash (obj, handle, global_handle_table);
      gch->index = hash_find (XHASH_TABLE (global_handle_table), obj);
      return gch;
    }
  else
    {
      gc_handle gch = xmint_pointer (handle);
      gch->refcount++;
      return gch;
    }
}

Lisp_Object
gc_handle_value (gc_handle gch)
{
  return HASH_KEY (XHASH_TABLE (global_handle_table), gch->index);
}

void
free_gc_handle (gc_handle h)
{
  h->refcount--;
  if (h->refcount == 0)
    {
      Fremhash (gc_handle_value (h), global_handle_table);
      xfree (h);
      /* FIXME: better (but still safe) heuristic needed.  */
      if (XFIXNUM (Fhash_table_count (global_handle_table))
	  < XFIXNUM (Fhash_table_size (global_handle_table)) / 4
	      - 128)
	shrink_handle_table ();
    }
}

gc_handle
gc_handle_for_pvec (struct vectorlike_header *h)
{
  Lisp_Object obj = make_lisp_ptr (h, Lisp_Vectorlike);
  return gc_handle_for (obj);
}

DEFUN ("gc-handles-info", Fgc_handles_info, Sgc_handles_info, 0, 0, 0,
       doc: /* Return information about registered GC handles.
Return value is an list with the following entries:
- ("objects" OBJECTS) -- the number of distinct objects which have GC handles
- ("refs" REFS) -- the number of handles which were allocated
- ("refcounts" ((OBJ . COUNT) ...)) -- per object reference counts.  */)
(void)
{
  size_t nobj = 0;
  size_t nref = 0;
  Lisp_Object refcounts = Qnil;
  DOHASH (XHASH_TABLE (global_handle_table), k, v)
  {
    gc_handle gch = xmint_pointer (v);
    nobj += 1;
    nref += gch->refcount;
    Lisp_Object obj = k;
    if (XTYPE (k) == Lisp_Vectorlike
	&& PSEUDOVECTOR_TYPE (XVECTOR (k)) == PVEC_OTHER)
      obj = build_string ("#<PVEC_OTHER>");
    refcounts
      = Fcons (Fcons (obj, make_uint (gch->refcount)), refcounts);
  }
  return list (list (build_string ("objects"), make_uint (nobj)),
	       list (build_string ("refs"), make_uint (nref)),
	       list (build_string ("refcounts"), refcounts));
}

void
syms_of_gc_handles (void)
{
  defsubr (&Sgc_handles_info);
  global_handle_table = CALLN (Fmake_hash_table, QCtest, Qeq);
  staticpro (&global_handle_table);
}
