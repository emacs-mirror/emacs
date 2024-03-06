/* JSON parsing and serialization.

Copyright (C) 2017-2024 Free Software Foundation, Inc.

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
#include <math.h>

#include <jansson.h>

#include "lisp.h"
#include "buffer.h"
#include "coding.h"

#define JSON_HAS_ERROR_CODE (JANSSON_VERSION_HEX >= 0x020B00)

#ifdef WINDOWSNT
# include <windows.h>
# include "w32common.h"
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
   that size_t values never exceed PTRDIFF_MAX.

   In addition, we need to use a custom allocator because on
   MS-Windows we replace malloc/free with our own functions, see
   w32heap.c, so we must force the library to use our allocator, or
   else we won't be able to free storage allocated by the library.  */

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
  return strncmp (string, prefix, strlen (prefix)) == 0;
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

/* Note that all callers of make_string_from_utf8 and build_string_from_utf8
   below either pass only value UTF-8 strings or use the functionf for
   formatting error messages; in the latter case correctness isn't
   critical.  */

/* Return a unibyte string containing the sequence of UTF-8 encoding
   units of the UTF-8 representation of STRING.  If STRING does not
   represent a sequence of Unicode scalar values, return a string with
   unspecified contents.  */

static Lisp_Object
json_encode (Lisp_Object string)
{
  /* FIXME: Raise an error if STRING is not a scalar value
     sequence.  */
  return encode_string_utf_8 (string, Qnil, false, Qt, Qt);
}

static AVOID
json_out_of_memory (void)
{
  xsignal0 (Qjson_out_of_memory);
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

enum json_object_type {
  json_object_hashtable,
  json_object_alist,
  json_object_plist
};

enum json_array_type {
  json_array_array,
  json_array_list
};

struct json_configuration {
  enum json_object_type object_type;
  enum json_array_type array_type;
  Lisp_Object null_object;
  Lisp_Object false_object;
};

static json_t *lisp_to_json (Lisp_Object,
                             const struct json_configuration *conf);

/* Convert a Lisp object to a nonscalar JSON object (array or object).  */

static json_t *
lisp_to_json_nonscalar_1 (Lisp_Object lisp,
                          const struct json_configuration *conf)
{
  json_t *json;
  specpdl_ref count;

  if (VECTORP (lisp))
    {
      ptrdiff_t size = ASIZE (lisp);
      json = json_check (json_array ());
      count = SPECPDL_INDEX ();
      record_unwind_protect_ptr (json_release_object, json);
      for (ptrdiff_t i = 0; i < size; ++i)
        {
          int status
            = json_array_append_new (json, lisp_to_json (AREF (lisp, i),
                                                         conf));
          if (status == -1)
            json_out_of_memory ();
        }
      eassert (json_array_size (json) == size);
    }
  else if (HASH_TABLE_P (lisp))
    {
      struct Lisp_Hash_Table *h = XHASH_TABLE (lisp);
      json = json_check (json_object ());
      count = SPECPDL_INDEX ();
      record_unwind_protect_ptr (json_release_object, json);
      DOHASH (h, key, v)
        {
	  CHECK_STRING (key);
	  Lisp_Object ekey = json_encode (key);
	  /* We can't specify the length, so the string must be
	     null-terminated.  */
	  check_string_without_embedded_nulls (ekey);
	  const char *key_str = SSDATA (ekey);
	  /* Reject duplicate keys.  These are possible if the hash
	     table test is not `equal'.  */
	  if (json_object_get (json, key_str) != NULL)
	    wrong_type_argument (Qjson_value_p, lisp);
	  int status
	    = json_object_set_new (json, key_str,
				   lisp_to_json (v, conf));
	  if (status == -1)
	    {
	      /* A failure can be caused either by an invalid key or
		 by low memory.  */
	      json_check_utf8 (ekey);
	      json_out_of_memory ();
	    }
	}
    }
  else if (NILP (lisp))
    return json_check (json_object ());
  else if (CONSP (lisp))
    {
      Lisp_Object tail = lisp;
      json = json_check (json_object ());
      count = SPECPDL_INDEX ();
      record_unwind_protect_ptr (json_release_object, json);
      bool is_plist = !CONSP (XCAR (tail));
      FOR_EACH_TAIL (tail)
        {
          const char *key_str;
          Lisp_Object value;
          Lisp_Object key_symbol;
          if (is_plist)
            {
              key_symbol = XCAR (tail);
              tail = XCDR (tail);
              CHECK_CONS (tail);
              value = XCAR (tail);
            }
          else
            {
              Lisp_Object pair = XCAR (tail);
              CHECK_CONS (pair);
              key_symbol = XCAR (pair);
              value = XCDR (pair);
            }
          CHECK_SYMBOL (key_symbol);
          Lisp_Object key = SYMBOL_NAME (key_symbol);
          /* We can't specify the length, so the string must be
             null-terminated.  */
          check_string_without_embedded_nulls (key);
          key_str = SSDATA (key);
          /* In plists, ensure leading ":" in keys is stripped.  It
             will be reconstructed later in `json_to_lisp'.*/
          if (is_plist && ':' == key_str[0] && key_str[1])
            {
              key_str = &key_str[1];
            }
          /* Only add element if key is not already present.  */
          if (json_object_get (json, key_str) == NULL)
            {
              int status
                = json_object_set_new (json, key_str, lisp_to_json (value,
                                                                    conf));
              if (status == -1)
                json_out_of_memory ();
            }
        }
      CHECK_LIST_END (tail, lisp);
    }
  else
    wrong_type_argument (Qjson_value_p, lisp);

  clear_unwind_protect (count);
  unbind_to (count, Qnil);
  return json;
}

/* Convert LISP to a nonscalar JSON object (array or object).  Signal
   an error of type `wrong-type-argument' if LISP is not a vector,
   hashtable, alist, or plist.  */

static json_t *
lisp_to_json_nonscalar (Lisp_Object lisp,
                        const struct json_configuration *conf)
{
  if (++lisp_eval_depth > max_lisp_eval_depth)
    xsignal0 (Qjson_object_too_deep);
  json_t *json = lisp_to_json_nonscalar_1 (lisp, conf);
  --lisp_eval_depth;
  return json;
}

/* Convert LISP to any JSON object.  Signal an error of type
   `wrong-type-argument' if the type of LISP can't be converted to a
   JSON object.  */

static json_t *
lisp_to_json (Lisp_Object lisp, const struct json_configuration *conf)
{
  if (EQ (lisp, conf->null_object))
    return json_check (json_null ());
  else if (EQ (lisp, conf->false_object))
    return json_check (json_false ());
  else if (EQ (lisp, Qt))
    return json_check (json_true ());
  else if (INTEGERP (lisp))
    {
      intmax_t low = TYPE_MINIMUM (json_int_t);
      intmax_t high = TYPE_MAXIMUM (json_int_t);
      intmax_t value = check_integer_range (lisp, low, high);
      return json_check (json_integer (value));
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

  /* LISP now must be a vector, hashtable, alist, or plist.  */
  return lisp_to_json_nonscalar (lisp, conf);
}

static void
json_parse_args (ptrdiff_t nargs,
                 Lisp_Object *args,
                 struct json_configuration *conf,
                 bool parse_object_types)
{
  if ((nargs % 2) != 0)
    wrong_type_argument (Qplistp, Flist (nargs, args));

  /* Start from the back so keyword values appearing
     first take precedence. */
  for (ptrdiff_t i = nargs; i > 0; i -= 2) {
    Lisp_Object key = args[i - 2];
    Lisp_Object value = args[i - 1];
    if (parse_object_types && EQ (key, QCobject_type))
      {
        if (EQ (value, Qhash_table))
          conf->object_type = json_object_hashtable;
        else if (EQ (value, Qalist))
          conf->object_type = json_object_alist;
        else if (EQ (value, Qplist))
          conf->object_type = json_object_plist;
        else
          wrong_choice (list3 (Qhash_table, Qalist, Qplist), value);
      }
    else if (parse_object_types && EQ (key, QCarray_type))
      {
        if (EQ (value, Qarray))
          conf->array_type = json_array_array;
        else if (EQ (value, Qlist))
          conf->array_type = json_array_list;
        else
          wrong_choice (list2 (Qarray, Qlist), value);
      }
    else if (EQ (key, QCnull_object))
      conf->null_object = value;
    else if (EQ (key, QCfalse_object))
      conf->false_object = value;
    else if (parse_object_types)
      wrong_choice (list4 (QCobject_type,
                           QCarray_type,
                           QCnull_object,
                           QCfalse_object),
                    value);
    else
      wrong_choice (list2 (QCnull_object,
                           QCfalse_object),
                    value);
  }
}

static bool
json_available_p (void)
{
#ifdef WINDOWSNT
  if (!json_initialized)
    {
      Lisp_Object status;
      json_initialized = init_json_functions ();
      status = json_initialized ? Qt : Qnil;
      Vlibrary_cache = Fcons (Fcons (Qjson, status), Vlibrary_cache);
    }
  return json_initialized;
#else  /* !WINDOWSNT */
  return true;
#endif
}

#ifdef WINDOWSNT
static void
ensure_json_available (void)
{
  if (!json_available_p ())
    Fsignal (Qjson_unavailable,
	     list1 (build_unibyte_string ("jansson library not found")));
}
#endif

DEFUN ("json--available-p", Fjson__available_p, Sjson__available_p, 0, 0, NULL,
       doc: /* Return non-nil if libjansson is available (internal use only).  */)
  (void)
{
  return json_available_p () ? Qt : Qnil;
}

DEFUN ("json-serialize", Fjson_serialize, Sjson_serialize, 1, MANY,
       NULL,
       doc: /* Return the JSON representation of OBJECT as a string.

OBJECT must be t, a number, string, vector, hashtable, alist, plist,
or the Lisp equivalents to the JSON null and false values, and its
elements must recursively consist of the same kinds of values.  t will
be converted to the JSON true value.  Vectors will be converted to
JSON arrays, whereas hashtables, alists and plists are converted to
JSON objects.  Hashtable keys must be strings without embedded null
characters and must be unique within each object.  Alist and plist
keys must be symbols; if a key is duplicate, the first instance is
used.

The Lisp equivalents to the JSON null and false values are
configurable in the arguments ARGS, a list of keyword/argument pairs:

The keyword argument `:null-object' specifies which object to use
to represent a JSON null value.  It defaults to `:null'.

The keyword argument `:false-object' specifies which object to use to
represent a JSON false value.  It defaults to `:false'.

In you specify the same value for `:null-object' and `:false-object',
a potentially ambiguous situation, the JSON output will not contain
any JSON false values.
usage: (json-serialize OBJECT &rest ARGS)  */)
     (ptrdiff_t nargs, Lisp_Object *args)
{
  specpdl_ref count = SPECPDL_INDEX ();

#ifdef WINDOWSNT
  ensure_json_available ();
#endif

  struct json_configuration conf =
    {json_object_hashtable, json_array_array, QCnull, QCfalse};
  json_parse_args (nargs - 1, args + 1, &conf, false);

  json_t *json = lisp_to_json (args[0], &conf);
  record_unwind_protect_ptr (json_release_object, json);

  char *string = json_dumps (json, JSON_COMPACT | JSON_ENCODE_ANY);
  if (string == NULL)
    json_out_of_memory ();
  record_unwind_protect_ptr (json_free, string);

  return unbind_to (count, build_string_from_utf8 (string));
}

struct json_buffer_and_size
{
  const char *buffer;
  ptrdiff_t size;
  /* This tracks how many bytes were inserted by the callback since
     json_dump_callback was called.  */
  ptrdiff_t inserted_bytes;
};

static Lisp_Object
json_insert (void *data)
{
  struct json_buffer_and_size *buffer_and_size = data;
  ptrdiff_t len = buffer_and_size->size;
  ptrdiff_t inserted_bytes = buffer_and_size->inserted_bytes;
  ptrdiff_t gap_size = GAP_SIZE - inserted_bytes;

  /* Enlarge the gap if necessary.  */
  if (gap_size < len)
    make_gap (len - gap_size);

  /* Copy this chunk of data into the gap.  */
  memcpy ((char *) BEG_ADDR + PT_BYTE - BEG_BYTE + inserted_bytes,
	  buffer_and_size->buffer, len);
  buffer_and_size->inserted_bytes += len;
  return Qnil;
}

static Lisp_Object
json_handle_nonlocal_exit (enum nonlocal_exit type, Lisp_Object data)
{
  switch (type)
    {
    case NONLOCAL_EXIT_SIGNAL:
      return data;
    case NONLOCAL_EXIT_THROW:
      return Fcons (Qno_catch, data);
    default:
      eassume (false);
    }
}

struct json_insert_data
{
  /* This tracks how many bytes were inserted by the callback since
     json_dump_callback was called.  */
  ptrdiff_t inserted_bytes;
  /* nil if json_insert succeeded, otherwise the symbol
     Qcatch_all_memory_full or a cons (ERROR-SYMBOL . ERROR-DATA).  */
  Lisp_Object error;
};

/* Callback for json_dump_callback that inserts a JSON representation
   as a unibyte string into the gap.  DATA must point to a structure
   of type json_insert_data.  This function may not exit nonlocally.
   It catches all nonlocal exits and stores them in data->error for
   reraising.  */

static int
json_insert_callback (const char *buffer, size_t size, void *data)
{
  struct json_insert_data *d = data;
  struct json_buffer_and_size buffer_and_size
    = {.buffer = buffer, .size = size, .inserted_bytes = d->inserted_bytes};
  d->error = internal_catch_all (json_insert, &buffer_and_size,
                                 json_handle_nonlocal_exit);
  d->inserted_bytes = buffer_and_size.inserted_bytes;
  return NILP (d->error) ? 0 : -1;
}

DEFUN ("json-insert", Fjson_insert, Sjson_insert, 1, MANY,
       NULL,
       doc: /* Insert the JSON representation of OBJECT before point.
This is the same as (insert (json-serialize OBJECT)), but potentially
faster.  See the function `json-serialize' for allowed values of
OBJECT.
usage: (json-insert OBJECT &rest ARGS)  */)
     (ptrdiff_t nargs, Lisp_Object *args)
{
  specpdl_ref count = SPECPDL_INDEX ();

#ifdef WINDOWSNT
  ensure_json_available ();
#endif

  struct json_configuration conf =
    {json_object_hashtable, json_array_array, QCnull, QCfalse};
  json_parse_args (nargs - 1, args + 1, &conf, false);

  json_t *json = lisp_to_json (args[0], &conf);
  record_unwind_protect_ptr (json_release_object, json);

  prepare_to_modify_buffer (PT, PT, NULL);
  move_gap_both (PT, PT_BYTE);
  struct json_insert_data data;
  data.inserted_bytes = 0;
  /* Could have used json_dumpb, but that became available only in
     Jansson 2.10, whereas we want to support 2.7 and upward.  */
  int status = json_dump_callback (json, json_insert_callback, &data,
                                   JSON_COMPACT | JSON_ENCODE_ANY);
  if (status == -1)
    {
      if (CONSP (data.error))
        xsignal (XCAR (data.error), XCDR (data.error));
      else
        json_out_of_memory ();
    }

  ptrdiff_t inserted = 0;
  ptrdiff_t inserted_bytes = data.inserted_bytes;
  if (inserted_bytes > 0)
    {
      /* If required, decode the stuff we've read into the gap.  */
      struct coding_system coding;
      /* JSON strings are UTF-8 encoded strings.  If for some reason
	 the text returned by the Jansson library includes invalid
	 byte sequences, they will be represented by raw bytes in the
	 buffer text.  */
      setup_coding_system (Qutf_8_unix, &coding);
      coding.dst_multibyte =
	!NILP (BVAR (current_buffer, enable_multibyte_characters));
      if (CODING_MAY_REQUIRE_DECODING (&coding))
	{
          /* Now we have all the new bytes at the beginning of the gap,
             but `decode_coding_gap` needs them at the end of the gap, so
             we need to move them.  */
          memmove (GAP_END_ADDR - inserted_bytes, GPT_ADDR, inserted_bytes);
	  decode_coding_gap (&coding, inserted_bytes);
	  inserted = coding.produced_char;
	}
      else
	{
          /* Make the inserted text part of the buffer, as unibyte text.  */
          eassert (NILP (BVAR (current_buffer, enable_multibyte_characters)));
          insert_from_gap_1 (inserted_bytes, inserted_bytes, false);

	  /* The target buffer is unibyte, so we don't need to decode.  */
	  invalidate_buffer_caches (current_buffer,
				    PT, PT + inserted_bytes);
	  adjust_after_insert (PT, PT_BYTE,
			       PT + inserted_bytes,
			       PT_BYTE + inserted_bytes,
			       inserted_bytes);
	  inserted = inserted_bytes;
	}
    }

  /* Call after-change hooks.  */
  signal_after_change (PT, 0, inserted);
  if (inserted > 0)
    {
      update_compositions (PT, PT, CHECK_BORDER);
      /* Move point to after the inserted text.  */
      SET_PT_BOTH (PT + inserted, PT_BYTE + inserted_bytes);
    }

  return unbind_to (count, Qnil);
}

#define JSON_PARSER_INTERNAL_OBJECT_WORKSPACE_SIZE 64
#define JSON_PARSER_INTERNAL_BYTE_WORKSPACE_SIZE 512

struct json_parser
{
  /* Because of a possible gap in the input (an emacs buffer can have
     a gap), the input is described by [input_begin;input_end) and
     [secondary_input_begin;secondary_input_end).  If the input is
     continuous, then secondary_input_begin and secondary_input_end
     should be NULL */
  const unsigned char *input_current;
  const unsigned char *input_begin;
  const unsigned char *input_end;

  const unsigned char *secondary_input_begin;
  const unsigned char *secondary_input_end;

  ptrdiff_t current_line;
  ptrdiff_t current_column;
  ptrdiff_t point_of_current_line;

  /* The parser has a maximum allowed depth.  available_depth
     decreases at each object/array begin.  If reaches zero, then an
     error is generated */
  int available_depth;

  struct json_configuration conf;

  size_t additional_bytes_count;

  /* Lisp_Objects are collected in this area during object/array
     parsing.  To avoid allocations, initially
     internal_object_workspace is used.  If it runs out of space then
     we switch to allocated space.  Important note: with this design,
     GC must not run during JSON parsing, otherwise Lisp_Objects in
     the workspace may get incorrectly collected. */
  Lisp_Object internal_object_workspace
  [JSON_PARSER_INTERNAL_OBJECT_WORKSPACE_SIZE];
  Lisp_Object *object_workspace;
  size_t object_workspace_size;
  size_t object_workspace_current;

  /* String and number parsing uses this workspace.  The idea behind
     internal_byte_workspace is the same as the idea behind
     internal_object_workspace */
  unsigned char
  internal_byte_workspace[JSON_PARSER_INTERNAL_BYTE_WORKSPACE_SIZE];
  unsigned char *byte_workspace;
  unsigned char *byte_workspace_end;
  unsigned char *byte_workspace_current;
};

static AVOID
json_signal_error (struct json_parser *parser, Lisp_Object error)
{
  xsignal3 (error, INT_TO_INTEGER (parser->current_line),
            INT_TO_INTEGER (parser->current_column),
            INT_TO_INTEGER (parser->point_of_current_line
                            + parser->current_column));
}

static void
json_parser_init (struct json_parser *parser,
		  struct json_configuration conf,
		  const unsigned char *input,
		  const unsigned char *input_end,
		  const unsigned char *secondary_input,
		  const unsigned char *secondary_input_end)
{
  if (secondary_input >= secondary_input_end)
    {
      secondary_input = NULL;
      secondary_input_end = NULL;
    }

  if (input < input_end)
    {
      parser->input_begin = input;
      parser->input_end = input_end;

      parser->secondary_input_begin = secondary_input;
      parser->secondary_input_end = secondary_input_end;
    }
  else
    {
      parser->input_begin = secondary_input;
      parser->input_end = secondary_input_end;

      parser->secondary_input_begin = NULL;
      parser->secondary_input_end = NULL;
    }

  parser->input_current = parser->input_begin;

  parser->current_line = 1;
  parser->current_column = 0;
  parser->point_of_current_line = 0;
  parser->available_depth = 10000;
  parser->conf = conf;

  parser->additional_bytes_count = 0;

  parser->object_workspace = parser->internal_object_workspace;
  parser->object_workspace_size
    = JSON_PARSER_INTERNAL_OBJECT_WORKSPACE_SIZE;
  parser->object_workspace_current = 0;

  parser->byte_workspace = parser->internal_byte_workspace;
  parser->byte_workspace_end
          = (parser->byte_workspace
             + JSON_PARSER_INTERNAL_BYTE_WORKSPACE_SIZE);
}

static void
json_parser_done (void *parser)
{
  struct json_parser *p = (struct json_parser *) parser;
  if (p->object_workspace != p->internal_object_workspace)
    xfree (p->object_workspace);
  if (p->byte_workspace != p->internal_byte_workspace)
    xfree (p->byte_workspace);
}

/* Makes sure that the object_workspace has 'size' available space for
   Lisp_Objects */
NO_INLINE static void
json_make_object_workspace_for_slow_path (struct json_parser *parser,
					  size_t size)
{
  size_t needed_workspace_size
    = (parser->object_workspace_current + size);
  size_t new_workspace_size = parser->object_workspace_size;
  while (new_workspace_size < needed_workspace_size)
    {
      if (ckd_mul (&new_workspace_size, new_workspace_size, 2))
	{
	  json_signal_error (parser, Qjson_out_of_memory);
	}
    }

  Lisp_Object *new_workspace_ptr;
  if (parser->object_workspace_size
      == JSON_PARSER_INTERNAL_OBJECT_WORKSPACE_SIZE)
    {
      new_workspace_ptr
	= xnmalloc (new_workspace_size, sizeof (Lisp_Object));
      memcpy (new_workspace_ptr, parser->object_workspace,
	      (sizeof (Lisp_Object)
	       * parser->object_workspace_current));
    }
  else
    {
      new_workspace_ptr
	= xnrealloc (parser->object_workspace, new_workspace_size,
		     sizeof (Lisp_Object));
    }

  parser->object_workspace = new_workspace_ptr;
  parser->object_workspace_size = new_workspace_size;
}

INLINE void
json_make_object_workspace_for (struct json_parser *parser,
				size_t size)
{
  if (parser->object_workspace_size - parser->object_workspace_current
      < size)
    {
      json_make_object_workspace_for_slow_path (parser, size);
    }
}

static void
json_byte_workspace_reset (struct json_parser *parser)
{
  parser->byte_workspace_current = parser->byte_workspace;
}

/* Puts 'value' into the byte_workspace.  If there is no space
   available, it allocates space */
NO_INLINE static void
json_byte_workspace_put_slow_path (struct json_parser *parser,
				   unsigned char value)
{
  size_t new_workspace_size
    = parser->byte_workspace_end - parser->byte_workspace;
  if (ckd_mul (&new_workspace_size, new_workspace_size, 2))
    {
      json_signal_error (parser, Qjson_out_of_memory);
    }

  size_t offset
    = parser->byte_workspace_current - parser->byte_workspace;

  if (parser->byte_workspace == parser->internal_byte_workspace)
    {
      parser->byte_workspace = xmalloc (new_workspace_size);
      memcpy (parser->byte_workspace, parser->internal_byte_workspace,
	      offset);
    }
  else
    {
      parser->byte_workspace
	= xrealloc (parser->byte_workspace, new_workspace_size);
    }
  parser->byte_workspace_end
    = parser->byte_workspace + new_workspace_size;
  parser->byte_workspace_current = parser->byte_workspace + offset;
  *parser->byte_workspace_current++ = value;
}

INLINE void
json_byte_workspace_put (struct json_parser *parser,
			 unsigned char value)
{
  if (parser->byte_workspace_current < parser->byte_workspace_end)
    {
      *parser->byte_workspace_current++ = value;
    }
  else
    {
      json_byte_workspace_put_slow_path (parser, value);
    }
}

static bool
json_input_at_eof (struct json_parser *parser)
{
  if (parser->input_current < parser->input_end)
    return false;
  return parser->secondary_input_end == NULL;
}

/* If there is a secondary buffer, this switches to it */
static int
json_input_switch_to_secondary (struct json_parser *parser)
{
  if (parser->secondary_input_begin < parser->secondary_input_end)
    {
      parser->additional_bytes_count
	= parser->input_end - parser->input_begin;
      parser->input_begin = parser->secondary_input_begin;
      parser->input_end = parser->secondary_input_end;
      parser->input_current = parser->secondary_input_begin;
      parser->secondary_input_begin = NULL;
      parser->secondary_input_end = NULL;
      return 0;
    }
  else
    return -1;
}

/* Reads a byte from the JSON input stream */
NO_INLINE static unsigned char
json_input_get_slow_path (struct json_parser *parser)
{
  if (json_input_switch_to_secondary (parser) < 0)
    json_signal_error (parser, Qjson_end_of_file);
  return *parser->input_current++;
}

static unsigned char
json_input_get (struct json_parser *parser)
{
  if (parser->input_current < parser->input_end)
    return *parser->input_current++;
  return json_input_get_slow_path (parser);
}

/* Reads a byte from the JSON input stream, if the stream is not at
 * eof.  At eof, returns -1 */
static int
json_input_get_if_possible (struct json_parser *parser)
{
  if (parser->input_current >= parser->input_end
      && json_input_switch_to_secondary (parser) < 0)
    return -1;
  return *parser->input_current++;
}

/* Puts back the last read input byte.  Only one byte can be put back,
   because otherwise this code would need to handle switching from
   the secondary buffer to the initial */
static void
json_input_put_back (struct json_parser *parser)
{
  parser->input_current--;
}

static bool
json_skip_whitespace_internal (struct json_parser *parser, int c)
{
  parser->current_column++;
  if (c == 0x20 || c == 0x09 || c == 0x0d)
    return false;
  else if (c == 0x0a)
    {
      parser->current_line++;
      parser->point_of_current_line += parser->current_column;
      parser->current_column = 0;
      return false;
    }
  else
    return true;
}

/* Skips JSON whitespace, and returns with the first non-whitespace
 * character */
static int
json_skip_whitespace (struct json_parser *parser)
{
  for (;;)
    {
      int c = json_input_get (parser);
      if (json_skip_whitespace_internal (parser, c))
	return c;
    }
}

/* Skips JSON whitespace, and returns with the first non-whitespace
 * character, if possible.  If there is no non-whitespace character
 * (because we reached the end), it returns -1 */
static int
json_skip_whitespace_if_possible (struct json_parser *parser)
{
  for (;;)
    {
      int c = json_input_get_if_possible (parser);
      if (c < 0)
	return c;
      if (json_skip_whitespace_internal (parser, c))
	return c;
    }
}

static int
json_hex_value (int c)
{
  if (c >= '0' && c <= '9')
    return c - '0';
  else if (c >= 'A' && c <= 'F')
    return c - 'A' + 10;
  else if (c >= 'a' && c <= 'f')
    return c - 'a' + 10;
  else
    return -1;
}

/* Parses the CCCC part of the unicode escape sequence \uCCCC */
static int
json_parse_unicode (struct json_parser *parser)
{
  unsigned char v[4];
  for (int i = 0; i < 4; i++)
    {
      int c = json_hex_value (json_input_get (parser));
      parser->current_column++;
      if (c < 0)
	json_signal_error (parser, Qjson_escape_sequence_error);
      v[i] = c;
    }

  return v[0] << 12 | v[1] << 8 | v[2] << 4 | v[3];
}

/* Parses an utf-8 code-point encoding (except the first byte), and
   returns the numeric value of the code-point (without considering
   the first byte) */
static int
json_handle_utf8_tail_bytes (struct json_parser *parser, int n)
{
  int v = 0;
  for (int i = 0; i < n; i++)
    {
      int c = json_input_get (parser);
      json_byte_workspace_put (parser, c);
      if ((c & 0xc0) != 0x80)
	json_signal_error (parser, Qjson_utf8_decode_error);
      v = (v << 6) | (c & 0x3f);
    }
  return v;
}

/* Reads a JSON string, and puts the result into the byte workspace */
static void
json_parse_string (struct json_parser *parser)
{
  /* a single_uninteresting byte can be simply copied from the input
     to output, it doesn't need any extra care.  This means all the
     characters between [0x20;0x7f], except the double quote and
     the backslash */
  static const char is_single_uninteresting[256] = {
    /*      0  1  2  3  4  5  6  7  8  9  a  b  c  d  e  f */
    /* 0 */ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    /* 1 */ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    /* 2 */ 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
    /* 3 */ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
    /* 4 */ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
    /* 5 */ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1,
    /* 6 */ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
    /* 7 */ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
    /* 8 */ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    /* 9 */ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    /* a */ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    /* b */ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    /* c */ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    /* d */ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    /* e */ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    /* f */ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  };

  for (;;)
    {
      /* This if is only here for a possible speedup.  If there are 4
	 bytes available, and all of them are single_uninteresting,
	 then we can just copy these 4 bytes to output */
      if (parser->input_end - parser->input_current >= 4)
	{
	  int c0 = parser->input_current[0];
	  int c1 = parser->input_current[1];
	  int c2 = parser->input_current[2];
	  int c3 = parser->input_current[3];
	  bool v0 = is_single_uninteresting[c0];
	  bool v1 = is_single_uninteresting[c1];
	  bool v2 = is_single_uninteresting[c2];
	  bool v3 = is_single_uninteresting[c3];
	  if (v0 && v1 && v2 && v3)
	    {
	      json_byte_workspace_put (parser, c0);
	      json_byte_workspace_put (parser, c1);
	      json_byte_workspace_put (parser, c2);
	      json_byte_workspace_put (parser, c3);
	      parser->input_current += 4;
	      parser->current_column += 4;
	      continue;
	    }
	}

      int c = json_input_get (parser);
      parser->current_column++;
      if (is_single_uninteresting[c])
	{
	  json_byte_workspace_put (parser, c);
	  continue;
	}

      if (c == '"')
	return;
      else if (c & 0x80)
	{
	  /* Handle utf-8 encoding */
	  json_byte_workspace_put (parser, c);
	  if (c < 0xc0)
	    json_signal_error (parser, Qjson_utf8_decode_error);
	  else if (c < 0xe0)
	    {
	      int n = ((c & 0x1f) << 6
		       | json_handle_utf8_tail_bytes (parser, 1));
	      if (n < 0x80)
		json_signal_error (parser, Qjson_utf8_decode_error);
	    }
	  else if (c < 0xf0)
	    {
	      int n = ((c & 0xf) << 12
		       | json_handle_utf8_tail_bytes (parser, 2));
	      if (n < 0x800 || (n >= 0xd800 && n < 0xe000))
		json_signal_error (parser, Qjson_utf8_decode_error);
	    }
	  else if (c < 0xf8)
	    {
	      int n = ((c & 0x7) << 18
		       | json_handle_utf8_tail_bytes (parser, 3));
	      if (n < 0x10000 || n > 0x10ffff)
		json_signal_error (parser, Qjson_utf8_decode_error);
	    }
	  else
	    json_signal_error (parser, Qjson_utf8_decode_error);
	}
      else if (c == '\\')
	{
	  /* Handle escape sequences */
	  c = json_input_get (parser);
	  parser->current_column++;
	  if (c == '"')
	    json_byte_workspace_put (parser, '"');
	  else if (c == '\\')
	    json_byte_workspace_put (parser, '\\');
	  else if (c == '/')
	    json_byte_workspace_put (parser, '/');
	  else if (c == 'b')
	    json_byte_workspace_put (parser, '\b');
	  else if (c == 'f')
	    json_byte_workspace_put (parser, '\f');
	  else if (c == 'n')
	    json_byte_workspace_put (parser, '\n');
	  else if (c == 'r')
	    json_byte_workspace_put (parser, '\r');
	  else if (c == 't')
	    json_byte_workspace_put (parser, '\t');
	  else if (c == 'u')
	    {
	      int num = json_parse_unicode (parser);
	      /* is the first half of the surrogate pair */
	      if (num >= 0xd800 && num < 0xdc00)
		{
		  parser->current_column++;
		  if (json_input_get (parser) != '\\')
		    json_signal_error (parser,
				       Qjson_invalid_surrogate_error);
		  parser->current_column++;
		  if (json_input_get (parser) != 'u')
		    json_signal_error (parser,
				       Qjson_invalid_surrogate_error);
		  int num2 = json_parse_unicode (parser);
		  if (num2 < 0xdc00 || num2 >= 0xe000)
		    json_signal_error (parser,
				       Qjson_invalid_surrogate_error);
		  num = (0x10000
			 + ((num - 0xd800) << 10 | (num2 - 0xdc00)));
		}
	      else if (num >= 0xdc00 && num < 0xe000)
		/* is the second half of the surrogate pair without
		   the first half */
		json_signal_error (parser,
				   Qjson_invalid_surrogate_error);

	      /* utf-8 encode the code-point */
	      if (num < 0x80)
		json_byte_workspace_put (parser, num);
	      else if (num < 0x800)
		{
		  json_byte_workspace_put (parser, 0xc0 | num >> 6);
		  json_byte_workspace_put (parser,
					   0x80 | (num & 0x3f));
		}
	      else if (num < 0x10000)
		{
		  json_byte_workspace_put (parser, 0xe0 | num >> 12);
		  json_byte_workspace_put (parser,
					   (0x80
					    | ((num >> 6) & 0x3f)));
		  json_byte_workspace_put (parser,
					   0x80 | (num & 0x3f));
		}
	      else
		{
		  json_byte_workspace_put (parser, 0xf0 | num >> 18);
		  json_byte_workspace_put (parser,
					   (0x80
					    | ((num >> 12) & 0x3f)));
		  json_byte_workspace_put (parser,
					   (0x80
					    | ((num >> 6) & 0x3f)));
		  json_byte_workspace_put (parser,
					   0x80 | (num & 0x3f));
		}
	    }
	  else
	    json_signal_error (parser, Qjson_escape_sequence_error);
	}
      else
	json_signal_error (parser, Qjson_parse_error);
    }
}

/* If there was no integer overflow during parsing the integer, this
   puts 'value' to the output. Otherwise this calls string_to_number
   to parse integer on the byte workspace.  This could just always
   call string_to_number, but for performance reasons, during parsing
   the code tries to calculate the value, so in most cases, we can
   save call of string_to_number */
static Lisp_Object
json_create_integer (struct json_parser *parser,
		     bool integer_overflow, bool negative,
		     EMACS_UINT value)
{
  if (!integer_overflow)
    {
      if (negative)
	{
	  uintmax_t v = value;
	  if (v <= (uintmax_t) INTMAX_MAX + 1)
	    return INT_TO_INTEGER ((intmax_t) -v);
	}
      else
	return INT_TO_INTEGER (value);
    }

  json_byte_workspace_put (parser, 0);
  ptrdiff_t len;
  Lisp_Object result
    = string_to_number ((const char *) parser->byte_workspace, 10,
			&len);
  if (len
      != parser->byte_workspace_current - parser->byte_workspace - 1)
    json_signal_error (parser, Qjson_error);
  return result;
}

/* Parses a float using the byte workspace */
static Lisp_Object
json_create_float (struct json_parser *parser)
{
  json_byte_workspace_put (parser, 0);
  errno = 0;
  char *e;
  double value = strtod ((const char *) parser->byte_workspace, &e);
  bool out_of_range
    = (errno != 0 && (value == HUGE_VAL || value == -HUGE_VAL));
  if (out_of_range)
    json_signal_error (parser, Qjson_number_out_of_range);
  else if ((const unsigned char *) e
	   != parser->byte_workspace_current - 1)
    json_signal_error (parser, Qjson_error);
  else
    return make_float (value);
}

/* Parses a number.  The first character is the input parameter 'c'.
 */
static Lisp_Object
json_parse_number (struct json_parser *parser, int c)
{
  json_byte_workspace_reset (parser);
  json_byte_workspace_put (parser, c);

  bool negative = false;
  if (c == '-')
    {
      negative = true;
      c = json_input_get (parser);
      json_byte_workspace_put (parser, c);
      parser->current_column++;
    }
  if (c < '0' || c > '9')
    json_signal_error (parser, Qjson_parse_error);

  /* The idea is that during finding the last character of the
     number, the for loop below also tries to calculate the value.  If
     the parsed number is an integer which fits into unsigned long,
     then the parser can use the value of 'integer' right away,
     instead of having to re-parse the byte workspace later.
     Ideally, this integer should have the same size as a CPU general
     purpose register. */
  EMACS_UINT integer = c - '0';
  bool integer_overflow = false;

  if (integer == 0)
    {
      if (json_input_at_eof (parser))
	return INT_TO_INTEGER (0);
      c = json_input_get (parser);
    }
  else
    {
      for (;;)
	{
	  if (json_input_at_eof (parser))
	    return json_create_integer (parser, integer_overflow,
					negative, integer);
	  c = json_input_get (parser);
	  if (c < '0' || c > '9')
	    break;
	  json_byte_workspace_put (parser, c);
	  parser->current_column++;

	  integer_overflow |= ckd_mul (&integer, integer, 10);
	  integer_overflow |= ckd_add (&integer, integer, c - '0');
	}
    }

  bool is_float = false;
  if (c == '.')
    {
      json_byte_workspace_put (parser, c);
      parser->current_column++;

      is_float = true;
      c = json_input_get (parser);
      json_byte_workspace_put (parser, c);
      parser->current_column++;
      if (c < '0' || c > '9')
	json_signal_error (parser, Qjson_parse_error);
      for (;;)
	{
	  if (json_input_at_eof (parser))
	    return json_create_float (parser);
	  c = json_input_get (parser);
	  if (c < '0' || c > '9')
	    break;
	  json_byte_workspace_put (parser, c);
	  parser->current_column++;
	}
    }
  if (c == 'e' || c == 'E')
    {
      json_byte_workspace_put (parser, c);
      parser->current_column++;

      is_float = true;
      c = json_input_get (parser);
      json_byte_workspace_put (parser, c);
      parser->current_column++;
      if (c == '-' || c == '+')
	{
	  c = json_input_get (parser);
	  json_byte_workspace_put (parser, c);
	  parser->current_column++;
	}
      if (c < '0' || c > '9')
	json_signal_error (parser, Qjson_parse_error);
      for (;;)
	{
	  if (json_input_at_eof (parser))
	    return json_create_float (parser);
	  c = json_input_get (parser);
	  if (c < '0' || c > '9')
	    break;
	  json_byte_workspace_put (parser, c);
	  parser->current_column++;
	}
    }

  /* 'c' contains a character which is not part of the number,
     so it is need to be put back */
  json_input_put_back (parser);

  if (is_float)
    return json_create_float (parser);
  else
    return json_create_integer (parser, integer_overflow, negative,
				integer);
}

static Lisp_Object json_parse_value (struct json_parser *parser,
				     int c);

/* Parses a JSON array. */
static Lisp_Object
json_parse_array (struct json_parser *parser)
{
  int c = json_skip_whitespace (parser);

  const size_t first = parser->object_workspace_current;
  Lisp_Object result = Qnil;

  if (c != ']')
    {
      parser->available_depth--;
      if (parser->available_depth < 0)
	json_signal_error (parser, Qjson_object_too_deep);

      size_t number_of_elements = 0;
      Lisp_Object *cdr = &result;
      /* This loop collects the array elements in the object workspace
       */
      for (;;)
	{
	  Lisp_Object element = json_parse_value (parser, c);
	  switch (parser->conf.array_type)
	    {
	    case json_array_array:
	      json_make_object_workspace_for (parser, 1);
	      parser->object_workspace[parser->object_workspace_current]
		= element;
	      parser->object_workspace_current++;
	      break;
	    case json_array_list:
	      {
		Lisp_Object nc = Fcons (element, Qnil);
		*cdr = nc;
		cdr = xcdr_addr (nc);
		break;
	      }
	    default:
	      emacs_abort ();
	    }

	  c = json_skip_whitespace (parser);

	  number_of_elements++;
	  if (c == ']')
	    {
	      parser->available_depth++;
	      break;
	    }

	  if (c != ',')
	    json_signal_error (parser, Qjson_parse_error);

	  c = json_skip_whitespace (parser);
	}
    }

  switch (parser->conf.array_type)
    {
    case json_array_array:
      {
	size_t number_of_elements
	  = parser->object_workspace_current - first;
	result = make_vector (number_of_elements, Qnil);
	for (size_t i = 0; i < number_of_elements; i++)
	  {
	    rarely_quit (i);
	    ASET (result, i, parser->object_workspace[first + i]);
	  }
	parser->object_workspace_current = first;
	break;
      }
    case json_array_list:
      break;
    default:
      emacs_abort ();
    }

  return result;
}

/* Parses the ": value" part of a JSON object member. */
static Lisp_Object
json_parse_object_member_value (struct json_parser *parser)
{
  int c = json_skip_whitespace (parser);
  if (c != ':')
    json_signal_error (parser, Qjson_parse_error);

  c = json_skip_whitespace (parser);

  return json_parse_value (parser, c);
}

/* Parses a JSON object. */
static Lisp_Object
json_parse_object (struct json_parser *parser)
{
  int c = json_skip_whitespace (parser);

  const size_t first = parser->object_workspace_current;
  Lisp_Object result = Qnil;

  if (c != '}')
    {
      parser->available_depth--;
      if (parser->available_depth < 0)
	json_signal_error (parser, Qjson_object_too_deep);

      Lisp_Object *cdr = &result;

      /* This loop collects the object members (key/value pairs) in
       * the object workspace */
      for (;;)
	{
	  if (c != '"')
	    json_signal_error (parser, Qjson_parse_error);

	  json_byte_workspace_reset (parser);
	  switch (parser->conf.object_type)
	    {
	    case json_object_hashtable:
	      {
		json_parse_string (parser);
		Lisp_Object key
		  = make_string_from_utf8 ((char *)
                                           parser->byte_workspace,
					   (parser->byte_workspace_current
					    - parser->byte_workspace));
		Lisp_Object value
		  = json_parse_object_member_value (parser);
		json_make_object_workspace_for (parser, 2);
		parser->object_workspace[parser->object_workspace_current]
		  = key;
		parser->object_workspace_current++;
		parser->object_workspace[parser->object_workspace_current]
		  = value;
		parser->object_workspace_current++;
		break;
	      }
	    case json_object_alist:
	      {
		json_parse_string (parser);
		Lisp_Object key
		  = Fintern (make_string_from_utf8 (
                                                    (char *) parser->byte_workspace,
                                                    (parser->byte_workspace_current
                                                     - parser->byte_workspace)),
			     Qnil);
		Lisp_Object value
		  = json_parse_object_member_value (parser);
		Lisp_Object nc = Fcons (Fcons (key, value), Qnil);
		*cdr = nc;
		cdr = xcdr_addr (nc);
		break;
	      }
	    case json_object_plist:
	      {
		json_byte_workspace_put (parser, ':');
		json_parse_string (parser);
		Lisp_Object key
		  = intern_1 ((char *) parser->byte_workspace,
			      (parser->byte_workspace_current
			       - parser->byte_workspace));
		Lisp_Object value
		  = json_parse_object_member_value (parser);
		Lisp_Object nc = Fcons (key, Qnil);
		*cdr = nc;
		cdr = xcdr_addr (nc);

		nc = Fcons (value, Qnil);
		*cdr = nc;
		cdr = xcdr_addr (nc);
		break;
	      }
	    default:
	      emacs_abort ();
	    }

	  c = json_skip_whitespace (parser);

	  if (c == '}')
	    {
	      parser->available_depth++;
	      break;
	    }

	  if (c != ',')
	    json_signal_error (parser, Qjson_parse_error);

	  c = json_skip_whitespace (parser);
	}
    }

  switch (parser->conf.object_type)
    {
    case json_object_hashtable:
      {
	result
	  = CALLN (Fmake_hash_table, QCtest, Qequal, QCsize,
		   make_fixed_natnum (
                                      (parser->object_workspace_current - first) / 2));
	struct Lisp_Hash_Table *h = XHASH_TABLE (result);
	for (size_t i = first; i < parser->object_workspace_current;
	     i += 2)
	  {
	    hash_hash_t hash;
	    Lisp_Object key = parser->object_workspace[i];
	    Lisp_Object value = parser->object_workspace[i + 1];
	    ptrdiff_t i = hash_lookup_get_hash (h, key, &hash);
	    if (i < 0)
	      hash_put (h, key, value, hash);
	    else
	      set_hash_value_slot (h, i, value);
	  }
	parser->object_workspace_current = first;
	break;
      }
    case json_object_alist:
    case json_object_plist:
      break;
    default:
      emacs_abort ();
    }

  return result;
}

/* Token-char is not a JSON terminology.  When parsing
   null/false/true, this function tells the character set that is need
   to be considered as part of a token.  For example, if the input is
   "truesomething", then the parser shouldn't consider it as "true",
   and an additional later "something" token. An additional example:
   if the input is "truetrue", then calling (json-parse-buffer) twice
   shouldn't produce two successful calls which return t, but a
   parsing error */
static bool
json_is_token_char (int c)
{
  return ((c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z')
	  || (c >= '0' && c <= '9') || (c == '-'));
}

/* This is the entry point to the value parser, this parses a JSON
 * value */
Lisp_Object
json_parse_value (struct json_parser *parser, int c)
{
  if (c == '{')
    return json_parse_object (parser);
  else if (c == '[')
    return json_parse_array (parser);
  else if (c == '"')
    {
      json_byte_workspace_reset (parser);
      json_parse_string (parser);
      Lisp_Object result
	= make_string_from_utf8 ((const char *)
                                 parser->byte_workspace,
				 (parser->byte_workspace_current
				  - parser->byte_workspace));
      return result;
    }
  else if ((c >= '0' && c <= '9') || (c == '-'))
    return json_parse_number (parser, c);
  else
    {
      int c2 = json_input_get (parser);
      int c3 = json_input_get (parser);
      int c4 = json_input_get (parser);
      int c5 = json_input_get_if_possible (parser);

      if (c == 't' && c2 == 'r' && c3 == 'u' && c4 == 'e'
	  && (c5 < 0 || !json_is_token_char (c5)))
	{
	  if (c5 >= 0)
	    json_input_put_back (parser);
	  parser->current_column += 3;
	  return Qt;
	}
      if (c == 'n' && c2 == 'u' && c3 == 'l' && c4 == 'l'
	  && (c5 < 0 || !json_is_token_char (c5)))
	{
	  if (c5 >= 0)
	    json_input_put_back (parser);
	  parser->current_column += 3;
	  return parser->conf.null_object;
	}
      if (c == 'f' && c2 == 'a' && c3 == 'l' && c4 == 's'
	  && c5 == 'e')
	{
	  int c6 = json_input_get_if_possible (parser);
	  if (c6 < 0 || !json_is_token_char (c6))
	    {
	      if (c6 >= 0)
		json_input_put_back (parser);
	      parser->current_column += 4;
	      return parser->conf.false_object;
	    }
	}

      json_signal_error (parser, Qjson_parse_error);
    }
}

enum ParseEndBehavior
  {
    PARSEENDBEHAVIOR_CheckForGarbage,
    PARSEENDBEHAVIOR_MovePoint
  };

static Lisp_Object
json_parse (struct json_parser *parser,
	    enum ParseEndBehavior parse_end_behavior)
{
  int c = json_skip_whitespace (parser);

  Lisp_Object result = json_parse_value (parser, c);

  switch (parse_end_behavior)
    {
    case PARSEENDBEHAVIOR_CheckForGarbage:
      c = json_skip_whitespace_if_possible (parser);
      if (c >= 0)
	json_signal_error (parser, Qjson_trailing_content);
      break;
    case PARSEENDBEHAVIOR_MovePoint:
      {
	ptrdiff_t byte
	  = (PT_BYTE + parser->input_current - parser->input_begin
	     + parser->additional_bytes_count);
	ptrdiff_t position;
	if (NILP (BVAR (current_buffer, enable_multibyte_characters)))
	  position = byte;
	else
	  position
	    = PT + parser->point_of_current_line + parser->current_column;

	SET_PT_BOTH (position, byte);
	break;
      }
    }

  return result;
}

DEFUN ("json-parse-string", Fjson_parse_string, Sjson_parse_string, 1, MANY,
       NULL,
       doc: /* Parse the JSON STRING into a Lisp object.
This is essentially the reverse operation of `json-serialize', which
see.  The returned object will be the JSON null value, the JSON false
value, t, a number, a string, a vector, a list, a hashtable, an alist,
or a plist.  Its elements will be further objects of these types.  If
there are duplicate keys in an object, all but the last one are
ignored.  If STRING doesn't contain a valid JSON object, this function
signals an error of type `json-parse-error'.

The arguments ARGS are a list of keyword/argument pairs:

The keyword argument `:object-type' specifies which Lisp type is used
to represent objects; it can be `hash-table', `alist' or `plist'.  It
defaults to `hash-table'.  If an object has members with the same
key, `hash-table' keeps only the last value of such keys, while
`alist' and `plist' keep all the members.

The keyword argument `:array-type' specifies which Lisp type is used
to represent arrays; it can be `array' (the default) or `list'.

The keyword argument `:null-object' specifies which object to use
to represent a JSON null value.  It defaults to `:null'.

The keyword argument `:false-object' specifies which object to use to
represent a JSON false value.  It defaults to `:false'.
usage: (json-parse-string STRING &rest ARGS) */)
(ptrdiff_t nargs, Lisp_Object *args)
{
  specpdl_ref count = SPECPDL_INDEX ();

  Lisp_Object string = args[0];
  CHECK_STRING (string);
  Lisp_Object encoded = json_encode (string);
  struct json_configuration conf
    = { json_object_hashtable, json_array_array, QCnull, QCfalse };
  json_parse_args (nargs - 1, args + 1, &conf, true);

  struct json_parser p;
  const unsigned char *begin
    = (const unsigned char *) SSDATA (encoded);
  json_parser_init (&p, conf, begin, begin + SBYTES (encoded), NULL,
		    NULL);
  record_unwind_protect_ptr (json_parser_done, &p);

  return unbind_to (count,
		    json_parse (&p,
				PARSEENDBEHAVIOR_CheckForGarbage));
}

DEFUN ("json-parse-buffer", Fjson_parse_buffer, Sjson_parse_buffer,
       0, MANY, NULL,
       doc: /* Read JSON object from current buffer starting at point.
Move point after the end of the object if parsing was successful.
On error, don't move point.

The returned object will be a vector, list, hashtable, alist, or
plist.  Its elements will be the JSON null value, the JSON false
value, t, numbers, strings, or further vectors, lists, hashtables,
alists, or plists.  If there are duplicate keys in an object, all
but the last one are ignored.

If the current buffer doesn't contain a valid JSON object, the
function signals an error of type `json-parse-error'.

The arguments ARGS are a list of keyword/argument pairs:

The keyword argument `:object-type' specifies which Lisp type is used
to represent objects; it can be `hash-table', `alist' or `plist'.  It
defaults to `hash-table'.  If an object has members with the same
key, `hash-table' keeps only the last value of such keys, while
`alist' and `plist' keep all the members.

The keyword argument `:array-type' specifies which Lisp type is used
to represent arrays; it can be `array' (the default) or `list'.

The keyword argument `:null-object' specifies which object to use
to represent a JSON null value.  It defaults to `:null'.

The keyword argument `:false-object' specifies which object to use to
represent a JSON false value.  It defaults to `:false'.
usage: (json-parse-buffer &rest args) */)
(ptrdiff_t nargs, Lisp_Object *args)
{
  specpdl_ref count = SPECPDL_INDEX ();

  struct json_configuration conf
    = { json_object_hashtable, json_array_array, QCnull, QCfalse };
  json_parse_args (nargs, args, &conf, true);

  struct json_parser p;
  unsigned char *begin = PT_ADDR;
  unsigned char *end = GPT_ADDR;
  unsigned char *secondary_begin = NULL;
  unsigned char *secondary_end = NULL;
  if (GPT_ADDR < Z_ADDR)
    {
      secondary_begin = GAP_END_ADDR;
      if (secondary_begin < PT_ADDR)
	secondary_begin = PT_ADDR;
      secondary_end = Z_ADDR;
    }

  json_parser_init (&p, conf, begin, end, secondary_begin,
		    secondary_end);
  record_unwind_protect_ptr (json_parser_done, &p);

  return unbind_to (count,
		    json_parse (&p, PARSEENDBEHAVIOR_MovePoint));
}

void
syms_of_json (void)
{
  DEFSYM (QCnull, ":null");
  DEFSYM (QCfalse, ":false");

  DEFSYM (Qstring_without_embedded_nulls_p, "string-without-embedded-nulls-p");
  DEFSYM (Qjson_value_p, "json-value-p");

  DEFSYM (Qjson_error, "json-error");
  DEFSYM (Qjson_out_of_memory, "json-out-of-memory");
  DEFSYM (Qjson_parse_error, "json-parse-error");
  DEFSYM (Qjson_end_of_file, "json-end-of-file");
  DEFSYM (Qjson_trailing_content, "json-trailing-content");
  DEFSYM (Qjson_object_too_deep, "json-object-too-deep");
  DEFSYM (Qjson_utf8_decode_error, "json-utf8-decode-error")
  DEFSYM (Qjson_invalid_surrogate_error, "json-invalid-surrogate-error")
  DEFSYM (Qjson_number_out_of_range, "json-number-out-of-range-error")
  DEFSYM (Qjson_escape_sequence_error, "json-escape-sequence-error")
  DEFSYM (Qjson_unavailable, "json-unavailable");
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
  define_error (Qjson_utf8_decode_error,
                "invalid utf-8 encoding", Qjson_error);
  define_error (Qjson_invalid_surrogate_error,
                "invalid surrogate pair", Qjson_error);
  define_error (Qjson_number_out_of_range,
                "number out of range", Qjson_error);
  define_error (Qjson_escape_sequence_error,
                "invalid escape sequence", Qjson_parse_error);

  DEFSYM (Qpure, "pure");
  DEFSYM (Qside_effect_free, "side-effect-free");

  DEFSYM (Qjson_serialize, "json-serialize");
  DEFSYM (Qjson_parse_string, "json-parse-string");
  Fput (Qjson_serialize, Qpure, Qt);
  Fput (Qjson_serialize, Qside_effect_free, Qt);
  Fput (Qjson_parse_string, Qpure, Qt);
  Fput (Qjson_parse_string, Qside_effect_free, Qt);

  DEFSYM (QCobject_type, ":object-type");
  DEFSYM (QCarray_type, ":array-type");
  DEFSYM (QCnull_object, ":null-object");
  DEFSYM (QCfalse_object, ":false-object");
  DEFSYM (Qalist, "alist");
  DEFSYM (Qplist, "plist");
  DEFSYM (Qarray, "array");

  defsubr (&Sjson__available_p);
  defsubr (&Sjson_serialize);
  defsubr (&Sjson_insert);
  defsubr (&Sjson_parse_string);
  defsubr (&Sjson_parse_buffer);
}
