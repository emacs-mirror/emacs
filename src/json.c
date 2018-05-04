/* JSON parsing and serialization.

Copyright (C) 2017-2018 Free Software Foundation, Inc.

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

#include <config.h>

#include <errno.h>
#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>

#include <jansson.h>

#include "lisp.h"
#include "buffer.h"
#include "coding.h"

#define JSON_HAS_ERROR_CODE (JANSSON_VERSION_HEX >= 0x020B00)

#ifdef WINDOWSNT
# include <windows.h>
# include "w32.h"

DEF_DLL_FN (void, json_set_alloc_funcs,
	    (json_malloc_t malloc_fn, json_free_t free_fn));
DEF_DLL_FN (void, json_delete, (json_t *json));
DEF_DLL_FN (json_t *, json_array, (void));
DEF_DLL_FN (int, json_array_append_new, (json_t *array, json_t *value));
DEF_DLL_FN (size_t, json_array_size, (const json_t *array));
DEF_DLL_FN (json_t *, json_object, (void));
DEF_DLL_FN (int, json_object_set_new,
	    (json_t *object, const char *key, json_t *value));
DEF_DLL_FN (json_t *, json_null, (void));
DEF_DLL_FN (json_t *, json_true, (void));
DEF_DLL_FN (json_t *, json_false, (void));
DEF_DLL_FN (json_t *, json_integer, (json_int_t value));
DEF_DLL_FN (json_t *, json_real, (double value));
DEF_DLL_FN (json_t *, json_stringn, (const char *value, size_t len));
DEF_DLL_FN (char *, json_dumps, (const json_t *json, size_t flags));
DEF_DLL_FN (int, json_dump_callback,
	    (const json_t *json, json_dump_callback_t callback, void *data,
	     size_t flags));
DEF_DLL_FN (json_int_t, json_integer_value, (const json_t *integer));
DEF_DLL_FN (double, json_real_value, (const json_t *real));
DEF_DLL_FN (const char *, json_string_value, (const json_t *string));
DEF_DLL_FN (size_t, json_string_length, (const json_t *string));
DEF_DLL_FN (json_t *, json_array_get, (const json_t *array, size_t index));
DEF_DLL_FN (json_t *, json_object_get, (const json_t *object, const char *key));
DEF_DLL_FN (size_t, json_object_size, (const json_t *object));
DEF_DLL_FN (const char *, json_object_iter_key, (void *iter));
DEF_DLL_FN (void *, json_object_iter, (json_t *object));
DEF_DLL_FN (json_t *, json_object_iter_value, (void *iter));
DEF_DLL_FN (void *, json_object_key_to_iter, (const char *key));
DEF_DLL_FN (void *, json_object_iter_next, (json_t *object, void *iter));
DEF_DLL_FN (json_t *, json_loads,
	    (const char *input, size_t flags, json_error_t *error));
DEF_DLL_FN (json_t *, json_load_callback,
	    (json_load_callback_t callback, void *data, size_t flags,
	     json_error_t *error));

/* This is called by json_decref, which is an inline function.  */
void json_delete(json_t *json)
{
  fn_json_delete (json);
}

static bool json_initialized;

static bool
init_json_functions (void)
{
  HMODULE library = w32_delayed_load (Qjson);

  if (!library)
    return false;

  LOAD_DLL_FN (library, json_set_alloc_funcs);
  LOAD_DLL_FN (library, json_delete);
  LOAD_DLL_FN (library, json_array);
  LOAD_DLL_FN (library, json_array_append_new);
  LOAD_DLL_FN (library, json_array_size);
  LOAD_DLL_FN (library, json_object);
  LOAD_DLL_FN (library, json_object_set_new);
  LOAD_DLL_FN (library, json_null);
  LOAD_DLL_FN (library, json_true);
  LOAD_DLL_FN (library, json_false);
  LOAD_DLL_FN (library, json_integer);
  LOAD_DLL_FN (library, json_real);
  LOAD_DLL_FN (library, json_stringn);
  LOAD_DLL_FN (library, json_dumps);
  LOAD_DLL_FN (library, json_dump_callback);
  LOAD_DLL_FN (library, json_integer_value);
  LOAD_DLL_FN (library, json_real_value);
  LOAD_DLL_FN (library, json_string_value);
  LOAD_DLL_FN (library, json_string_length);
  LOAD_DLL_FN (library, json_array_get);
  LOAD_DLL_FN (library, json_object_get);
  LOAD_DLL_FN (library, json_object_size);
  LOAD_DLL_FN (library, json_object_iter_key);
  LOAD_DLL_FN (library, json_object_iter);
  LOAD_DLL_FN (library, json_object_iter_value);
  LOAD_DLL_FN (library, json_object_key_to_iter);
  LOAD_DLL_FN (library, json_object_iter_next);
  LOAD_DLL_FN (library, json_loads);
  LOAD_DLL_FN (library, json_load_callback);

  init_json ();

  return true;
}

#define json_set_alloc_funcs fn_json_set_alloc_funcs
#define json_array fn_json_array
#define json_array_append_new fn_json_array_append_new
#define json_array_size fn_json_array_size
#define json_object fn_json_object
#define json_object_set_new fn_json_object_set_new
#define json_null fn_json_null
#define json_true fn_json_true
#define json_false fn_json_false
#define json_integer fn_json_integer
#define json_real fn_json_real
#define json_stringn fn_json_stringn
#define json_dumps fn_json_dumps
#define json_dump_callback fn_json_dump_callback
#define json_integer_value fn_json_integer_value
#define json_real_value fn_json_real_value
#define json_string_value fn_json_string_value
#define json_string_length fn_json_string_length
#define json_array_get fn_json_array_get
#define json_object_get fn_json_object_get
#define json_object_size fn_json_object_size
#define json_object_iter_key fn_json_object_iter_key
#define json_object_iter fn_json_object_iter
#define json_object_iter_value fn_json_object_iter_value
#define json_object_key_to_iter fn_json_object_key_to_iter
#define json_object_iter_next fn_json_object_iter_next
#define json_loads fn_json_loads
#define json_load_callback fn_json_load_callback

#endif	/* WINDOWSNT */

/* We install a custom allocator so that we can avoid objects larger
   than PTRDIFF_MAX.  Such objects wouldn't play well with the rest of
   Emacs's codebase, which generally uses ptrdiff_t for sizes and
   indices.  The other functions in this file also generally assume
   that size_t values never exceed PTRDIFF_MAX.  */

static void *
json_malloc (size_t size)
{
  if (size > PTRDIFF_MAX)
    {
      errno = ENOMEM;
      return NULL;
    }
  return malloc (size);
}

static void
json_free (void *ptr)
{
  free (ptr);
}

void
init_json (void)
{
  json_set_alloc_funcs (json_malloc, json_free);
}

#if !JSON_HAS_ERROR_CODE

/* Return whether STRING starts with PREFIX.  */

static bool
json_has_prefix (const char *string, const char *prefix)
{
  size_t string_len = strlen (string);
  size_t prefix_len = strlen (prefix);
  return string_len >= prefix_len && memcmp (string, prefix, prefix_len) == 0;
}

/* Return whether STRING ends with SUFFIX.  */

static bool
json_has_suffix (const char *string, const char *suffix)
{
  size_t string_len = strlen (string);
  size_t suffix_len = strlen (suffix);
  return string_len >= suffix_len
    && memcmp (string + string_len - suffix_len, suffix, suffix_len) == 0;
}

#endif

/* Create a multibyte Lisp string from the UTF-8 string in
   [DATA, DATA + SIZE).  If the range [DATA, DATA + SIZE) does not
   contain a valid UTF-8 string, an unspecified string is returned.
   Note that all callers below either pass only value UTF-8 strings or
   use this function for formatting error messages; in the latter case
   correctness isn't critical.  */

static Lisp_Object
json_make_string (const char *data, ptrdiff_t size)
{
  return code_convert_string (make_specified_string (data, -1, size, false),
                              Qutf_8_unix, Qt, false, true, true);
}

/* Create a multibyte Lisp string from the null-terminated UTF-8
   string beginning at DATA.  If the string is not a valid UTF-8
   string, an unspecified string is returned.  Note that all callers
   below either pass only value UTF-8 strings or use this function for
   formatting error messages; in the latter case correctness isn't
   critical.  */

static Lisp_Object
json_build_string (const char *data)
{
  return json_make_string (data, strlen (data));
}

/* Return a unibyte string containing the sequence of UTF-8 encoding
   units of the UTF-8 representation of STRING.  If STRING does not
   represent a sequence of Unicode scalar values, return a string with
   unspecified contents.  */

static Lisp_Object
json_encode (Lisp_Object string)
{
  /* FIXME: Raise an error if STRING is not a scalar value
     sequence.  */
  return code_convert_string (string, Qutf_8_unix, Qt, true, true, true);
}

static _Noreturn void
json_out_of_memory (void)
{
  xsignal0 (Qjson_out_of_memory);
}

/* Signal a Lisp error corresponding to the JSON ERROR.  */

static _Noreturn void
json_parse_error (const json_error_t *error)
{
  Lisp_Object symbol;
#if JSON_HAS_ERROR_CODE
  switch (json_error_code (error))
    {
    case json_error_premature_end_of_input:
      symbol = Qjson_end_of_file;
      break;
    case json_error_end_of_input_expected:
      symbol = Qjson_trailing_content;
      break;
    default:
      symbol = Qjson_parse_error;
      break;
    }
#else
  if (json_has_suffix (error->text, "expected near end of file"))
    symbol = Qjson_end_of_file;
  else if (json_has_prefix (error->text, "end of file expected"))
    symbol = Qjson_trailing_content;
  else
    symbol = Qjson_parse_error;
#endif
  xsignal (symbol,
           list5 (json_build_string (error->text),
                  json_build_string (error->source), make_natnum (error->line),
                  make_natnum (error->column), make_natnum (error->position)));
}

static void
json_release_object (void *object)
{
  json_decref (object);
}

/* Signal an error if OBJECT is not a string, or if OBJECT contains
   embedded null characters.  */

static void
check_string_without_embedded_nulls (Lisp_Object object)
{
  CHECK_STRING (object);
  CHECK_TYPE (memchr (SDATA (object), '\0', SBYTES (object)) == NULL,
              Qstring_without_embedded_nulls_p, object);
}

/* Signal an error of type `json-out-of-memory' if OBJECT is
   NULL.  */

static json_t *
json_check (json_t *object)
{
  if (object == NULL)
    json_out_of_memory ();
  return object;
}

/* If STRING is not a valid UTF-8 string, signal an error of type
   `wrong-type-argument'.  STRING must be a unibyte string.  */

static void
json_check_utf8 (Lisp_Object string)
{
  CHECK_TYPE (utf8_string_p (string), Qutf_8_string_p, string);
}

static json_t *lisp_to_json (Lisp_Object);

/* Convert a Lisp object to a toplevel JSON object (array or object).
   This returns Lisp_Object so we can use unbind_to.  The return value
   is always nil.  */

static _GL_ARG_NONNULL ((2)) Lisp_Object
lisp_to_json_toplevel_1 (Lisp_Object lisp, json_t **json)
{
  if (VECTORP (lisp))
    {
      ptrdiff_t size = ASIZE (lisp);
      *json = json_check (json_array ());
      ptrdiff_t count = SPECPDL_INDEX ();
      record_unwind_protect_ptr (json_release_object, json);
      for (ptrdiff_t i = 0; i < size; ++i)
        {
          int status
            = json_array_append_new (*json, lisp_to_json (AREF (lisp, i)));
          if (status == -1)
            json_out_of_memory ();
        }
      eassert (json_array_size (*json) == size);
      clear_unwind_protect (count);
      return unbind_to (count, Qnil);
    }
  else if (HASH_TABLE_P (lisp))
    {
      struct Lisp_Hash_Table *h = XHASH_TABLE (lisp);
      *json = json_check (json_object ());
      ptrdiff_t count = SPECPDL_INDEX ();
      record_unwind_protect_ptr (json_release_object, *json);
      for (ptrdiff_t i = 0; i < HASH_TABLE_SIZE (h); ++i)
        if (!NILP (HASH_HASH (h, i)))
          {
            Lisp_Object key = json_encode (HASH_KEY (h, i));
            /* We can't specify the length, so the string must be
               null-terminated.  */
            check_string_without_embedded_nulls (key);
            const char *key_str = SSDATA (key);
            /* Reject duplicate keys.  These are possible if the hash
               table test is not `equal'.  */
            if (json_object_get (*json, key_str) != NULL)
              wrong_type_argument (Qjson_value_p, lisp);
            int status = json_object_set_new (*json, key_str,
                                              lisp_to_json (HASH_VALUE (h, i)));
            if (status == -1)
              {
                /* A failure can be caused either by an invalid key or
                   by low memory.  */
                json_check_utf8 (key);
                json_out_of_memory ();
              }
          }
      clear_unwind_protect (count);
      return unbind_to (count, Qnil);
    }
  else if (NILP (lisp))
    {
      *json = json_check (json_object ());
      return Qnil;
    }
  else if (CONSP (lisp))
    {
      Lisp_Object tail = lisp;
      *json = json_check (json_object ());
      ptrdiff_t count = SPECPDL_INDEX ();
      record_unwind_protect_ptr (json_release_object, *json);
      FOR_EACH_TAIL (tail)
        {
          Lisp_Object pair = XCAR (tail);
          CHECK_CONS (pair);
          Lisp_Object key_symbol = XCAR (pair);
          Lisp_Object value = XCDR (pair);
          CHECK_SYMBOL (key_symbol);
          Lisp_Object key = SYMBOL_NAME (key_symbol);
          /* We can't specify the length, so the string must be
             null-terminated.  */
          check_string_without_embedded_nulls (key);
          const char *key_str = SSDATA (key);
          /* Only add element if key is not already present.  */
          if (json_object_get (*json, key_str) == NULL)
            {
              int status
                = json_object_set_new (*json, key_str, lisp_to_json (value));
              if (status == -1)
                json_out_of_memory ();
            }
        }
      CHECK_LIST_END (tail, lisp);
      clear_unwind_protect (count);
      return unbind_to (count, Qnil);
    }
  wrong_type_argument (Qjson_value_p, lisp);
}

/* Convert LISP to a toplevel JSON object (array or object).  Signal
   an error of type `wrong-type-argument' if LISP is not a vector,
   hashtable, or alist.  */

static json_t *
lisp_to_json_toplevel (Lisp_Object lisp)
{
  if (++lisp_eval_depth > max_lisp_eval_depth)
    xsignal0 (Qjson_object_too_deep);
  json_t *json;
  lisp_to_json_toplevel_1 (lisp, &json);
  --lisp_eval_depth;
  return json;
}

/* Convert LISP to any JSON object.  Signal an error of type
   `wrong-type-argument' if the type of LISP can't be converted to a
   JSON object.  */

static json_t *
lisp_to_json (Lisp_Object lisp)
{
  if (EQ (lisp, QCnull))
    return json_check (json_null ());
  else if (EQ (lisp, QCfalse))
    return json_check (json_false ());
  else if (EQ (lisp, Qt))
    return json_check (json_true ());
  else if (INTEGERP (lisp))
    {
      CHECK_TYPE_RANGED_INTEGER (json_int_t, lisp);
      return json_check (json_integer (XINT (lisp)));
    }
  else if (FLOATP (lisp))
    return json_check (json_real (XFLOAT_DATA (lisp)));
  else if (STRINGP (lisp))
    {
      Lisp_Object encoded = json_encode (lisp);
      json_t *json = json_stringn (SSDATA (encoded), SBYTES (encoded));
      if (json == NULL)
        {
          /* A failure can be caused either by an invalid string or by
             low memory.  */
          json_check_utf8 (encoded);
          json_out_of_memory ();
        }
      return json;
    }

  /* LISP now must be a vector, hashtable, or alist.  */
  return lisp_to_json_toplevel (lisp);
}

DEFUN ("json-serialize", Fjson_serialize, Sjson_serialize, 1, 1, NULL,
       doc: /* Return the JSON representation of OBJECT as a string.
OBJECT must be a vector, hashtable, or alist, and its elements can
recursively contain `:null', `:false', t, numbers, strings, or other
vectors hashtables, and alist.  `:null', `:false', and t will be
converted to JSON null, false, and true values, respectively.  Vectors
will be converted to JSON arrays, and hashtables and alists to JSON
objects.  Hashtable keys must be strings without embedded null
characters and must be unique within each object.  Alist keys must be
symbols; if a key is duplicate, the first instance is used.  */)
  (Lisp_Object object)
{
  ptrdiff_t count = SPECPDL_INDEX ();

#ifdef WINDOWSNT
  if (!json_initialized)
    {
      Lisp_Object status;
      json_initialized = init_json_functions ();
      status = json_initialized ? Qt : Qnil;
      Vlibrary_cache = Fcons (Fcons (Qjson, status), Vlibrary_cache);
    }
  if (!json_initialized)
    {
      message1 ("jansson library not found");
      return Qnil;
    }
#endif

  json_t *json = lisp_to_json_toplevel (object);
  record_unwind_protect_ptr (json_release_object, json);

  /* If desired, we might want to add the following flags:
     JSON_DECODE_ANY, JSON_ALLOW_NUL.  */
  char *string = json_dumps (json, JSON_COMPACT);
  if (string == NULL)
    json_out_of_memory ();
  record_unwind_protect_ptr (free, string);

  return unbind_to (count, json_build_string (string));
}

struct json_buffer_and_size
{
  const char *buffer;
  ptrdiff_t size;
};

static Lisp_Object
json_insert (void *data)
{
  struct json_buffer_and_size *buffer_and_size = data;
  /* FIXME: This should be possible without creating an intermediate
     string object.  */
  Lisp_Object string
    = json_make_string (buffer_and_size->buffer, buffer_and_size->size);
  insert1 (string);
  return Qnil;
}

struct json_insert_data
{
  /* nil if json_insert succeeded, otherwise the symbol
     Qcatch_all_memory_full or a cons (ERROR-SYMBOL . ERROR-DATA).  */
  Lisp_Object error;
};

/* Callback for json_dump_callback that inserts the UTF-8 string in
   [BUFFER, BUFFER + SIZE) into the current buffer.
   If [BUFFER, BUFFER + SIZE) does not contain a valid UTF-8 string,
   an unspecified string is inserted into the buffer.  DATA must point
   to a structure of type json_insert_data.  This function may not
   exit nonlocally.  It catches all nonlocal exits and stores them in
   data->error for reraising.  */

static int
json_insert_callback (const char *buffer, size_t size, void *data)
{
  struct json_insert_data *d = data;
  struct json_buffer_and_size buffer_and_size
    = {.buffer = buffer, .size = size};
  d->error = internal_catch_all (json_insert, &buffer_and_size, Fidentity);
  return NILP (d->error) ? 0 : -1;
}

DEFUN ("json-insert", Fjson_insert, Sjson_insert, 1, 1, NULL,
       doc: /* Insert the JSON representation of OBJECT before point.
This is the same as (insert (json-serialize OBJECT)), but potentially
faster.  See the function `json-serialize' for allowed values of
OBJECT.  */)
  (Lisp_Object object)
{
  ptrdiff_t count = SPECPDL_INDEX ();

#ifdef WINDOWSNT
  if (!json_initialized)
    {
      Lisp_Object status;
      json_initialized = init_json_functions ();
      status = json_initialized ? Qt : Qnil;
      Vlibrary_cache = Fcons (Fcons (Qjson, status), Vlibrary_cache);
    }
  if (!json_initialized)
    {
      message1 ("jansson library not found");
      return Qnil;
    }
#endif

  json_t *json = lisp_to_json (object);
  record_unwind_protect_ptr (json_release_object, json);

  struct json_insert_data data;
  /* If desired, we might want to add the following flags:
     JSON_DECODE_ANY, JSON_ALLOW_NUL.  */
  int status
    = json_dump_callback (json, json_insert_callback, &data, JSON_COMPACT);
  if (status == -1)
    {
      if (CONSP (data.error))
        xsignal (XCAR (data.error), XCDR (data.error));
      else
        json_out_of_memory ();
    }

  return unbind_to (count, Qnil);
}

enum json_object_type {
  json_object_hashtable,
  json_object_alist,
};

/* Convert a JSON object to a Lisp object.  */

static _GL_ARG_NONNULL ((1)) Lisp_Object
json_to_lisp (json_t *json, enum json_object_type object_type)
{
  switch (json_typeof (json))
    {
    case JSON_NULL:
      return QCnull;
    case JSON_FALSE:
      return QCfalse;
    case JSON_TRUE:
      return Qt;
    case JSON_INTEGER:
      /* Return an integer if possible, a floating-point number
         otherwise.  This loses precision for integers with large
         magnitude; however, such integers tend to be nonportable
         anyway because many JSON implementations use only 64-bit
         floating-point numbers with 53 mantissa bits.  See
         https://tools.ietf.org/html/rfc7159#section-6 for some
         discussion.  */
      return make_fixnum_or_float (json_integer_value (json));
    case JSON_REAL:
      return make_float (json_real_value (json));
    case JSON_STRING:
      return json_make_string (json_string_value (json),
                               json_string_length (json));
    case JSON_ARRAY:
      {
        if (++lisp_eval_depth > max_lisp_eval_depth)
          xsignal0 (Qjson_object_too_deep);
        size_t size = json_array_size (json);
        if (FIXNUM_OVERFLOW_P (size))
          xsignal0 (Qoverflow_error);
        Lisp_Object result = Fmake_vector (make_natnum (size), Qunbound);
        for (ptrdiff_t i = 0; i < size; ++i)
          ASET (result, i,
                json_to_lisp (json_array_get (json, i), object_type));
        --lisp_eval_depth;
        return result;
      }
    case JSON_OBJECT:
      {
        if (++lisp_eval_depth > max_lisp_eval_depth)
          xsignal0 (Qjson_object_too_deep);
        Lisp_Object result;
        switch (object_type)
          {
          case json_object_hashtable:
            {
              size_t size = json_object_size (json);
              if (FIXNUM_OVERFLOW_P (size))
                xsignal0 (Qoverflow_error);
              result = CALLN (Fmake_hash_table, QCtest, Qequal, QCsize,
                              make_natnum (size));
              struct Lisp_Hash_Table *h = XHASH_TABLE (result);
              const char *key_str;
              json_t *value;
              json_object_foreach (json, key_str, value)
                {
                  Lisp_Object key = json_build_string (key_str);
                  EMACS_UINT hash;
                  ptrdiff_t i = hash_lookup (h, key, &hash);
                  /* Keys in JSON objects are unique, so the key can't
                     be present yet.  */
                  eassert (i < 0);
                  hash_put (h, key, json_to_lisp (value, object_type), hash);
                }
              break;
            }
          case json_object_alist:
            {
              result = Qnil;
              const char *key_str;
              json_t *value;
              json_object_foreach (json, key_str, value)
                {
                  Lisp_Object key = Fintern (json_build_string (key_str), Qnil);
                  result
                    = Fcons (Fcons (key, json_to_lisp (value, object_type)),
                             result);
                }
              result = Fnreverse (result);
              break;
            }
          default:
            /* Can't get here.  */
            emacs_abort ();
          }
        --lisp_eval_depth;
        return result;
      }
    }
  /* Can't get here.  */
  emacs_abort ();
}

static enum json_object_type
json_parse_object_type (ptrdiff_t nargs, Lisp_Object *args)
{
  switch (nargs)
    {
    case 0:
      return json_object_hashtable;
    case 2:
      {
        Lisp_Object key = args[0];
        Lisp_Object value = args[1];
        if (!EQ (key, QCobject_type))
          wrong_choice (list1 (QCobject_type), key);
        if (EQ (value, Qhash_table))
          return json_object_hashtable;
        else if (EQ (value, Qalist))
          return json_object_alist;
        else
          wrong_choice (list2 (Qhash_table, Qalist), value);
      }
    default:
      wrong_type_argument (Qplistp, Flist (nargs, args));
    }
}

DEFUN ("json-parse-string", Fjson_parse_string, Sjson_parse_string, 1, MANY,
       NULL,
       doc: /* Parse the JSON STRING into a Lisp object.
This is essentially the reverse operation of `json-serialize', which
see.  The returned object will be a vector, hashtable, or alist.  Its
elements will be `:null', `:false', t, numbers, strings, or further
vectors, hashtables, and alists.  If there are duplicate keys in an
object, all but the last one are ignored.  If STRING doesn't contain a
valid JSON object, an error of type `json-parse-error' is signaled.
The keyword argument `:object-type' specifies which Lisp type is used
to represent objects; it can be `hash-table' or `alist'.
usage: (json-parse-string STRING &key (OBJECT-TYPE \\='hash-table))  */)
  (ptrdiff_t nargs, Lisp_Object *args)
{
  ptrdiff_t count = SPECPDL_INDEX ();

#ifdef WINDOWSNT
  if (!json_initialized)
    {
      Lisp_Object status;
      json_initialized = init_json_functions ();
      status = json_initialized ? Qt : Qnil;
      Vlibrary_cache = Fcons (Fcons (Qjson, status), Vlibrary_cache);
    }
  if (!json_initialized)
    {
      message1 ("jansson library not found");
      return Qnil;
    }
#endif

  Lisp_Object string = args[0];
  Lisp_Object encoded = json_encode (string);
  check_string_without_embedded_nulls (encoded);
  enum json_object_type object_type
    = json_parse_object_type (nargs - 1, args + 1);

  json_error_t error;
  json_t *object = json_loads (SSDATA (encoded), 0, &error);
  if (object == NULL)
    json_parse_error (&error);

  /* Avoid leaking the object in case of further errors.  */
  if (object != NULL)
    record_unwind_protect_ptr (json_release_object, object);

  return unbind_to (count, json_to_lisp (object, object_type));
}

struct json_read_buffer_data
{
  /* Byte position of position to read the next chunk from.  */
  ptrdiff_t point;
};

/* Callback for json_load_callback that reads from the current buffer.
   DATA must point to a structure of type json_read_buffer_data.
   data->point must point to the byte position to read from; after
   reading, data->point is advanced accordingly.  The buffer point
   itself is ignored.  This function may not exit nonlocally.  */

static size_t
json_read_buffer_callback (void *buffer, size_t buflen, void *data)
{
  struct json_read_buffer_data *d = data;

  /* First, parse from point to the gap or the end of the accessible
     portion, whatever is closer.  */
  ptrdiff_t point = d->point;
  ptrdiff_t end = BUFFER_CEILING_OF (point) + 1;
  ptrdiff_t count = end - point;
  if (buflen < count)
    count = buflen;
  memcpy (buffer, BYTE_POS_ADDR (point), count);
  d->point += count;
  return count;
}

DEFUN ("json-parse-buffer", Fjson_parse_buffer, Sjson_parse_buffer,
       0, MANY, NULL,
       doc: /* Read JSON object from current buffer starting at point.
This is similar to `json-parse-string', which see.  Move point after
the end of the object if parsing was successful.  On error, point is
not moved.
usage: (json-parse-buffer &key (OBJECT-TYPE \\='hash-table))  */)
  (ptrdiff_t nargs, Lisp_Object *args)
{
  ptrdiff_t count = SPECPDL_INDEX ();

#ifdef WINDOWSNT
  if (!json_initialized)
    {
      Lisp_Object status;
      json_initialized = init_json_functions ();
      status = json_initialized ? Qt : Qnil;
      Vlibrary_cache = Fcons (Fcons (Qjson, status), Vlibrary_cache);
    }
  if (!json_initialized)
    {
      message1 ("jansson library not found");
      return Qnil;
    }
#endif

  enum json_object_type object_type = json_parse_object_type (nargs, args);

  ptrdiff_t point = PT_BYTE;
  struct json_read_buffer_data data = {.point = point};
  json_error_t error;
  json_t *object = json_load_callback (json_read_buffer_callback, &data,
                                       JSON_DISABLE_EOF_CHECK, &error);

  if (object == NULL)
    json_parse_error (&error);

  /* Avoid leaking the object in case of further errors.  */
  record_unwind_protect_ptr (json_release_object, object);

  /* Convert and then move point only if everything succeeded.  */
  Lisp_Object lisp = json_to_lisp (object, object_type);

  /* Adjust point by how much we just read.  */
  point += error.position;
  SET_PT_BOTH (BYTE_TO_CHAR (point), point);

  return unbind_to (count, lisp);
}

/* Simplified version of 'define-error' that works with pure
   objects.  */

static void
define_error (Lisp_Object name, const char *message, Lisp_Object parent)
{
  eassert (SYMBOLP (name));
  eassert (SYMBOLP (parent));
  Lisp_Object parent_conditions = Fget (parent, Qerror_conditions);
  eassert (CONSP (parent_conditions));
  eassert (!NILP (Fmemq (parent, parent_conditions)));
  eassert (NILP (Fmemq (name, parent_conditions)));
  Fput (name, Qerror_conditions, pure_cons (name, parent_conditions));
  Fput (name, Qerror_message, build_pure_c_string (message));
}

void
syms_of_json (void)
{
  DEFSYM (QCnull, ":null");
  DEFSYM (QCfalse, ":false");

  DEFSYM (Qstring_without_embedded_nulls_p, "string-without-embedded-nulls-p");
  DEFSYM (Qjson_value_p, "json-value-p");
  DEFSYM (Qutf_8_string_p, "utf-8-string-p");

  DEFSYM (Qjson_error, "json-error");
  DEFSYM (Qjson_out_of_memory, "json-out-of-memory");
  DEFSYM (Qjson_parse_error, "json-parse-error");
  DEFSYM (Qjson_end_of_file, "json-end-of-file");
  DEFSYM (Qjson_trailing_content, "json-trailing-content");
  DEFSYM (Qjson_object_too_deep, "json-object-too-deep");
  define_error (Qjson_error, "generic JSON error", Qerror);
  define_error (Qjson_out_of_memory,
                "not enough memory for creating JSON object", Qjson_error);
  define_error (Qjson_parse_error, "could not parse JSON stream",
                Qjson_error);
  define_error (Qjson_end_of_file, "end of JSON stream", Qjson_parse_error);
  define_error (Qjson_trailing_content, "trailing content after JSON stream",
                Qjson_parse_error);
  define_error (Qjson_object_too_deep,
                "object cyclic or Lisp evaluation too deep", Qjson_error);

  DEFSYM (Qpure, "pure");
  DEFSYM (Qside_effect_free, "side-effect-free");

  DEFSYM (Qjson_serialize, "json-serialize");
  DEFSYM (Qjson_parse_string, "json-parse-string");
  Fput (Qjson_serialize, Qpure, Qt);
  Fput (Qjson_serialize, Qside_effect_free, Qt);
  Fput (Qjson_parse_string, Qpure, Qt);
  Fput (Qjson_parse_string, Qside_effect_free, Qt);

  DEFSYM (QCobject_type, ":object-type");
  DEFSYM (Qalist, "alist");

  defsubr (&Sjson_serialize);
  defsubr (&Sjson_insert);
  defsubr (&Sjson_parse_string);
  defsubr (&Sjson_parse_buffer);
}
