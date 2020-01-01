/* Header file for the portable dumper.

Copyright (C) 2016, 2018-2020 Free Software Foundation, Inc.

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
along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.  */

#ifndef EMACS_PDUMPER_H
#define EMACS_PDUMPER_H

#include "lisp.h"

INLINE_HEADER_BEGIN

enum { PDUMPER_NO_OBJECT = -1 };

/* Indicate in source code that we're deliberately relying on pdumper
   not preserving the given value.  Compiles to nothing --- for humans
   only.  */
#define PDUMPER_IGNORE(thing) ((void) &(thing))

/* The portable dumper automatically preserves the Lisp heap and any C
   variables to which the Lisp heap points.  It doesn't know anything
   about other C variables.  The functions below allow code from other
   parts of Emacs to tell the portable dumper about other bits of
   information to preserve in dump files.

   These memory-records are themselves preserved in the dump, so call
   the functions below only on the !initialized init path, just
   like staticpro.

   There are no special functions to preserve a global Lisp_Object.
   You should just staticpro these.  */

/* Remember the value of THING in dump files.  THING must not
   contain any pointers or Lisp_Object variables: these values are not
   valid across dump and load.  */
#define PDUMPER_REMEMBER_SCALAR(thing)                  \
  pdumper_remember_scalar (&(thing), sizeof (thing))

extern void pdumper_remember_scalar_impl (void *data, ptrdiff_t nbytes);

INLINE void
pdumper_remember_scalar (void *data, ptrdiff_t nbytes)
{
#ifdef HAVE_PDUMPER
  pdumper_remember_scalar_impl (data, nbytes);
#else
  (void) data;
  (void) nbytes;
#endif
}

extern void pdumper_remember_lv_ptr_raw_impl (void *ptr, enum Lisp_Type type);

/* Remember the pointer at *PTR.  *PTR must be null or point to a Lisp
   object.  TYPE is the rough type of Lisp object to which *PTR
   points.  */
INLINE void
pdumper_remember_lv_ptr_raw (void *ptr, enum Lisp_Type type)
{
#ifdef HAVE_PDUMPER
  pdumper_remember_lv_ptr_raw_impl (ptr, type);
#else
  (void) ptr;
  (void) type;
#endif
}

typedef void (*pdumper_hook)(void);
extern void pdumper_do_now_and_after_load_impl (pdumper_hook hook);

INLINE void
pdumper_do_now_and_after_load (pdumper_hook hook)
{
#ifdef HAVE_PDUMPER
  pdumper_do_now_and_after_load_impl (hook);
#else
  hook ();
#endif
}

/* Macros useful in pdumper callback functions.  Assign a value if
   we're loading a dump and the value needs to be reset to its
   original value, and if we're initializing for the first time,
   assert that the value has the expected original value.  */

#define PDUMPER_RESET(variable, value)         \
  do {                                         \
    if (dumped_with_pdumper_p ())              \
      (variable) = (value);                    \
    else                                       \
      eassert ((variable) == (value));         \
  } while (0)

#define PDUMPER_RESET_LV(variable, value)         \
  do {                                            \
    if (dumped_with_pdumper_p ())                 \
      (variable) = (value);                       \
    else                                          \
      eassert (EQ ((variable), (value)));         \
  } while (0)

/* Actually load a dump.  */

enum pdumper_load_result
  {
    PDUMPER_LOAD_SUCCESS,
    PDUMPER_NOT_LOADED /* Not returned: useful for callers */,
    PDUMPER_LOAD_FILE_NOT_FOUND,
    PDUMPER_LOAD_BAD_FILE_TYPE,
    PDUMPER_LOAD_FAILED_DUMP,
    PDUMPER_LOAD_OOM,
    PDUMPER_LOAD_VERSION_MISMATCH,
    PDUMPER_LOAD_ERROR /* Must be last, as errno may be added.  */
  };

int pdumper_load (const char *dump_filename);

struct pdumper_loaded_dump
{
  uintptr_t start;
  uintptr_t end;
};

extern struct pdumper_loaded_dump dump_public;

/* Return whether the OBJ points somewhere into the loaded dump image.
   Works even when we have no dump loaded --- in this case, it just
   returns false.  */
INLINE _GL_ATTRIBUTE_CONST bool
pdumper_object_p (const void *obj)
{
#ifdef HAVE_PDUMPER
  uintptr_t obj_addr = (uintptr_t) obj;
  return dump_public.start <= obj_addr && obj_addr < dump_public.end;
#else
  (void) obj;
  return false;
#endif
}

extern bool pdumper_cold_object_p_impl (const void *obj);

/* Return whether the OBJ is in the cold section of the dump.
   Only bool-vectors and floats should end up there.
   pdumper_object_p() and pdumper_object_p_precise() must have
   returned true for OBJ before calling this function.  */
INLINE _GL_ATTRIBUTE_CONST bool
pdumper_cold_object_p (const void *obj)
{
#ifdef HAVE_PDUMPER
  return pdumper_cold_object_p_impl (obj);
#else
  (void) obj;
  return false;
#endif
}


extern int pdumper_find_object_type_impl (const void *obj);

/* Return the type of the dumped object that starts at OBJ.  It is a
   programming error to call this routine for an OBJ for which
   pdumper_object_p would return false.  */
INLINE _GL_ATTRIBUTE_CONST int
pdumper_find_object_type (const void *obj)
{
#ifdef HAVE_PDUMPER
  return pdumper_find_object_type_impl (obj);
#else
  (void) obj;
  emacs_abort ();
#endif
}

/* Return true if TYPE is that of a Lisp object.
   PDUMPER_NO_OBJECT is invalid.  */
INLINE bool
pdumper_valid_object_type_p (int type)
{
  return 0 <= type;
}

/* Return whether OBJ points exactly to the start of some object in
   the loaded dump image.  It is a programming error to call this
   routine for an OBJ for which pdumper_object_p would return
   false.  */
INLINE _GL_ATTRIBUTE_CONST bool
pdumper_object_p_precise (const void *obj)
{
#ifdef HAVE_PDUMPER
  return pdumper_valid_object_type_p (pdumper_find_object_type (obj));
#else
  (void) obj;
  emacs_abort ();
#endif
}

extern bool pdumper_marked_p_impl (const void *obj);

/* Return whether OBJ is marked according to the portable dumper.
   It is an error to call this routine for an OBJ for which
   pdumper_object_p_precise would return false.  */
INLINE bool
pdumper_marked_p (const void *obj)
{
#ifdef HAVE_PDUMPER
  return pdumper_marked_p_impl (obj);
#else
  (void) obj;
  emacs_abort ();
#endif
}

extern void pdumper_set_marked_impl (const void *obj);

/* Set the pdumper mark bit for OBJ.  It is a programming error to
   call this function with an OBJ for which pdumper_object_p_precise
   would return false.  */
INLINE void
pdumper_set_marked (const void *obj)
{
#ifdef HAVE_PDUMPER
  pdumper_set_marked_impl (obj);
#else
  (void) obj;
  emacs_abort ();
#endif
}

extern void pdumper_clear_marks_impl (void);

/* Clear all the mark bits for pdumper objects.  */
INLINE void
pdumper_clear_marks (void)
{
#ifdef HAVE_PDUMPER
  pdumper_clear_marks_impl ();
#endif
}

/* Record the Emacs startup directory, relative to which the pdump
   file was loaded.  */
extern void pdumper_record_wd (const char *);

void syms_of_pdumper (void);

INLINE_HEADER_END
#endif
