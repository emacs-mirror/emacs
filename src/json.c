/* JSON parsing and serialization.

Copyright (C) 2017-2026 Free Software Foundation, Inc.

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

#include "lisp.h"
#include "buffer.h"
#include "coding.h"

enum json_object_type
  {
    json_object_hashtable,
    json_object_alist,
    json_object_plist,
  };

enum json_array_type
  {
    json_array_array,
    json_array_list,
  };

struct json_configuration
{
  enum json_object_type object_type;
  enum json_array_type array_type;
  Lisp_Object null_object;
  Lisp_Object false_object;
};

static void
json_parse_args (ptrdiff_t nargs, Lisp_Object *args,
		 struct json_configuration *conf,
		 bool parse_object_types)
{
  if ((nargs % 2) != 0)
    wrong_type_argument (Qplistp, Flist (nargs, args));

  /* Start from the back so keyword values appearing first take
     precedence.  */
  for (ptrdiff_t i = nargs; i > 0; i -= 2)
    {
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

/* JSON encoding context.  */
typedef struct
{
  char *buf;
  ptrdiff_t size;	      /* number of bytes in buf */
  ptrdiff_t capacity;	      /* allocated size of buf */
  ptrdiff_t chars_delta;      /* size - {number of characters in buf} */

  int maxdepth;
  struct symset_tbl *ss_table;	/* table used by containing object */
  struct json_configuration conf;
} json_out_t;

/* Set of symbols.  */
typedef struct
{
  ptrdiff_t count;		/* symbols in table */
  int bits;			/* log2(table size) */
  struct symset_tbl *table;	/* heap-allocated table */
} symset_t;

struct symset_tbl
{
  /* Table used by the containing object if any, so that we can free all
     tables if an error occurs.  */
  struct symset_tbl *up;
  /* Table of symbols (2**bits elements), Qunbound where unused.  */
  Lisp_Object entries[];
};

static inline ptrdiff_t
symset_size (int bits)
{
  return (ptrdiff_t) 1 << bits;
}

static struct symset_tbl *
make_symset_table (int bits, struct symset_tbl *up)
{
  int maxbits = min (SIZE_WIDTH - 2 - (word_size < 8 ? 2 : 3), 32);
  if (bits > maxbits)
    memory_full (PTRDIFF_MAX);	/* Will never happen in practice.  */
  struct symset_tbl *st = xmalloc (sizeof *st + (sizeof *st->entries << bits));
  st->up = up;
  ptrdiff_t size = symset_size (bits);
  for (ptrdiff_t i = 0; i < size; i++)
    st->entries[i] = Qunbound;
  return st;
}

/* Create a new symset to use for a new object.  */
static symset_t
push_symset (json_out_t *jo)
{
  int bits = 4;
  struct symset_tbl *tbl = make_symset_table (bits, jo->ss_table);
  jo->ss_table = tbl;
  return (symset_t){ .count = 0, .bits = bits, .table = tbl };
}

/* Destroy the current symset.  */
static void
pop_symset (json_out_t *jo, symset_t *ss)
{
  jo->ss_table = ss->table->up;
  xfree (ss->table);
}

/* Remove all heap-allocated symset tables, in case an error occurred.  */
static void
cleanup_symset_tables (struct symset_tbl *st)
{
  while (st)
    {
      struct symset_tbl *up = st->up;
      xfree (st);
      st = up;
    }
}

static inline uint32_t
symset_hash (Lisp_Object sym, int bits)
{
  return knuth_hash (reduce_emacs_uint_to_hash_hash (XHASH (sym)), bits);
}

/* Enlarge the table used by a symset.  */
static NO_INLINE void
symset_expand (symset_t *ss)
{
  struct symset_tbl *old_table = ss->table;
  int oldbits = ss->bits;
  ptrdiff_t oldsize = symset_size (oldbits);
  int bits = oldbits + 1;
  ss->bits = bits;
  ss->table = make_symset_table (bits, old_table->up);
  /* Move all entries from the old table to the new one.  */
  ptrdiff_t mask = symset_size (bits) - 1;
  struct symset_tbl *tbl = ss->table;
  for (ptrdiff_t i = 0; i < oldsize; i++)
    {
      Lisp_Object sym = old_table->entries[i];
      if (!BASE_EQ (sym, Qunbound))
	{
	  ptrdiff_t j = symset_hash (sym, bits);
	  while (!BASE_EQ (tbl->entries[j], Qunbound))
	    j = (j + 1) & mask;
	  tbl->entries[j] = sym;
	}
    }
  xfree (old_table);
}

/* If sym is in ss, return false; otherwise add it and return true.
   Comparison is done by strict identity.  */
static inline bool
symset_add (json_out_t *jo, symset_t *ss, Lisp_Object sym)
{
  /* Make sure we don't fill more than half of the table.  */
  if (ss->count >= (symset_size (ss->bits) >> 1))
    {
      symset_expand (ss);
      jo->ss_table = ss->table;
    }

  struct symset_tbl *tbl = ss->table;
  ptrdiff_t mask = symset_size (ss->bits) - 1;
  for (ptrdiff_t i = symset_hash (sym, ss->bits); ; i = (i + 1) & mask)
    {
      Lisp_Object s = tbl->entries[i];
      if (BASE_EQ (s, sym))
	return false;		/* Previous occurrence found.  */
      if (BASE_EQ (s, Qunbound))
	{
	  /* Not in set, add it.  */
	  tbl->entries[i] = sym;
	  ss->count++;
	  return true;
	}
    }
}

static NO_INLINE void
json_out_grow_buf (json_out_t *jo, ptrdiff_t bytes)
{
  ptrdiff_t need = jo->size + bytes;
  ptrdiff_t new_size = max (jo->capacity, 512);
  while (new_size < need)
    new_size <<= 1;
  jo->buf = xrealloc (jo->buf, new_size);
  jo->capacity = new_size;
}

static void
cleanup_json_out (void *arg)
{
  json_out_t *jo = arg;
  xfree (jo->buf);
  jo->buf = NULL;
  cleanup_symset_tables (jo->ss_table);
}

/* Make room for `bytes` more bytes in buffer.  */
static void
json_make_room (json_out_t *jo, ptrdiff_t bytes)
{
  if (bytes > jo->capacity - jo->size)
    json_out_grow_buf (jo, bytes);
}

#define JSON_OUT_STR(jo, str) (json_out_str (jo, str, sizeof (str) - 1))

/* Add `bytes` bytes from `str` to the buffer.  */
static void
json_out_str (json_out_t *jo, const char *str, size_t bytes)
{
  json_make_room (jo, bytes);
  memcpy (jo->buf + jo->size, str, bytes);
  jo->size += bytes;
}

static void
json_out_byte (json_out_t *jo, unsigned char c)
{
  json_make_room (jo, 1);
  jo->buf[jo->size++] = c;
}

static void
json_out_fixnum (json_out_t *jo, EMACS_INT x)
{
  char buf[INT_BUFSIZE_BOUND (EMACS_INT)];
  char *end = buf + sizeof buf;
  char *p = fixnum_to_string (x, buf, end);
  json_out_str (jo, p, end - p);
}

static AVOID
string_not_unicode (Lisp_Object obj)
{
  /* FIXME: this is just for compatibility with existing tests, it's not
     a very descriptive error.  */
  wrong_type_argument (Qjson_value_p, obj);
}

static const unsigned char json_plain_char[256] = {
  /* 32 chars/line: 1 for printable ASCII + DEL except " and \, 0 elsewhere */
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, /* 00-1f */
  1,1,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1, /* 20-3f */
  1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,1,1,1, /* 40-5f */
  1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1, /* 60-7f */
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, /* 80-9f */
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, /* a0-bf */
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, /* c0-df */
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, /* e0-ff */
};

static void
json_out_string (json_out_t *jo, Lisp_Object str, int skip)
{
  /* FIXME: this code is slow, make faster! */

  static const char hexchar[16] ATTRIBUTE_NONSTRING = "0123456789ABCDEF";
  ptrdiff_t len = SBYTES (str);
  json_make_room (jo, len + 2);
  json_out_byte (jo, '"');
  unsigned char *p = SDATA (str);
  unsigned char *end = p + len;
  p += skip;
  while (p < end)
    {
      unsigned char c = *p;
      if (json_plain_char[c])
	{
	  json_out_byte (jo, c);
	  p++;
	}
      else if (c > 0x7f)
	{
	  if (STRING_MULTIBYTE (str))
	    {
	      int n;
	      if (c <= 0xc1)
		string_not_unicode (str);
	      if (c <= 0xdf)
		n = 2;
	      else if (c <= 0xef)
		{
		  int v = (((c & 0x0f) << 12)
			   + ((p[1] & 0x3f) << 6) + (p[2] & 0x3f));
		  if (char_surrogate_p (v))
		    string_not_unicode (str);
		  n = 3;
		}
	      else if (c <= 0xf7)
		{
		  int v = (((c & 0x07) << 18)
			   + ((p[1] & 0x3f) << 12)
			   + ((p[2] & 0x3f) << 6)
			   + (p[3] & 0x3f));
		  if (v > MAX_UNICODE_CHAR)
		    string_not_unicode (str);
		  n = 4;
		}
	      else
		string_not_unicode (str);
	      json_out_str (jo, (const char *)p, n);
	      jo->chars_delta += n - 1;
	      p += n;
	    }
	  else
	    string_not_unicode (str);
	}
      else
	{
	  json_out_byte (jo, '\\');
	  switch (c)
	    {
	    case '"':
	    case '\\': json_out_byte (jo, c); break;
	    case '\b': json_out_byte (jo, 'b'); break;
	    case '\t': json_out_byte (jo, 't'); break;
	    case '\n': json_out_byte (jo, 'n'); break;
	    case '\f': json_out_byte (jo, 'f'); break;
	    case '\r': json_out_byte (jo, 'r'); break;
	    default:
	      {
		char hex[5] = { 'u', '0', '0',
				hexchar[c >> 4], hexchar[c & 0xf] };
		json_out_str (jo, hex, 5);
		break;
	      }
	    }
	  p++;
	}
    }
  json_out_byte (jo, '"');
}

static void
json_out_nest (json_out_t *jo)
{
  --jo->maxdepth;
  if (jo->maxdepth < 0)
    error ("Maximum JSON serialization depth exceeded");
}

static void
json_out_unnest (json_out_t *jo)
{
  ++jo->maxdepth;
}

static void json_out_something (json_out_t *jo, Lisp_Object obj);

static void
json_out_object_cons (json_out_t *jo, Lisp_Object obj)
{
  json_out_nest (jo);
  symset_t ss = push_symset (jo);
  json_out_byte (jo, '{');
  bool is_alist = CONSP (XCAR (obj));
  bool first = true;
  Lisp_Object tail = obj;
  FOR_EACH_TAIL (tail)
    {
      Lisp_Object key;
      Lisp_Object value;
      if (is_alist)
	{
	  Lisp_Object pair = XCAR (tail);
	  CHECK_CONS (pair);
	  key = XCAR (pair);
	  value = XCDR (pair);
	}
      else
	{
	  key = XCAR (tail);
	  tail = XCDR (tail);
	  CHECK_CONS (tail);
	  value = XCAR (tail);
	}
      key = maybe_remove_pos_from_symbol (key);
      CHECK_TYPE (BARE_SYMBOL_P (key), Qsymbolp, key);

      if (symset_add (jo, &ss, key))
	{
	  if (!first)
	    json_out_byte (jo, ',');
	  first = false;

	  Lisp_Object key_str = SYMBOL_NAME (key);
	  const char *str = SSDATA (key_str);
	  /* Skip leading ':' in plist keys.  */
	  int skip = !is_alist && str[0] == ':' && str[1] ? 1 : 0;
	  json_out_string (jo, key_str, skip);
	  json_out_byte (jo, ':');
	  json_out_something (jo, value);
	}
    }
  CHECK_LIST_END (tail, obj);
  json_out_byte (jo, '}');
  pop_symset (jo, &ss);
  json_out_unnest (jo);
}

static void
json_out_object_hash (json_out_t *jo, Lisp_Object obj)
{
  json_out_nest (jo);
  json_out_byte (jo, '{');
  struct Lisp_Hash_Table *h = XHASH_TABLE (obj);
  bool first = true;
  DOHASH (h, k, v)
    {
      if (!first)
	json_out_byte (jo, ',');
      first = false;
      CHECK_STRING (k);
      /* It's the user's responsibility to ensure that hash keys are
	 unique; we don't check for it.  */
      json_out_string (jo, k, 0);
      json_out_byte (jo, ':');
      json_out_something (jo, v);
    }
  json_out_byte (jo, '}');
  json_out_unnest (jo);

}

static void
json_out_array (json_out_t *jo, Lisp_Object obj)
{
  json_out_nest (jo);
  json_out_byte (jo, '[');
  ptrdiff_t n = ASIZE (obj);
  for (ptrdiff_t i = 0; i < n; i++)
    {
      if (i > 0)
	json_out_byte (jo, ',');
      json_out_something (jo, AREF (obj, i));
    }
  json_out_byte (jo, ']');
  json_out_unnest (jo);
}

static void
json_out_float (json_out_t *jo, Lisp_Object f)
{
  double x = XFLOAT_DATA (f);
  if (!isfinite (x))
    signal_error ("JSON does not allow Inf or NaN", f);
  /* As luck has it, float_to_string emits correct JSON float syntax for
     all numbers (because Vfloat_output_format is Qnil).  */
  json_make_room (jo, FLOAT_TO_STRING_BUFSIZE);
  int n = float_to_string (jo->buf + jo->size, x);
  jo->size += n;
}

static void
json_out_bignum (json_out_t *jo, Lisp_Object x)
{
  int base = 10;
  ptrdiff_t size = bignum_bufsize (x, base);
  json_make_room (jo, size);
  int n = bignum_to_c_string (jo->buf + jo->size, size, x, base);
  jo->size += n;
}

static void
json_out_something (json_out_t *jo, Lisp_Object obj)
{
  if (EQ (obj, jo->conf.null_object))
    JSON_OUT_STR (jo, "null");
  else if (EQ (obj, jo->conf.false_object))
    JSON_OUT_STR (jo, "false");
  else if (EQ (obj, Qt))
    JSON_OUT_STR (jo, "true");
  else if (NILP (obj))
    JSON_OUT_STR (jo, "{}");
  else if (FIXNUMP (obj))
    json_out_fixnum (jo, XFIXNUM (obj));
  else if (STRINGP (obj))
    json_out_string (jo, obj, 0);
  else if (CONSP (obj))
    json_out_object_cons (jo, obj);
  else if (FLOATP (obj))
    json_out_float (jo, obj);
  else if (HASH_TABLE_P (obj))
    json_out_object_hash (jo, obj);
  else if (VECTORP (obj))
    json_out_array (jo, obj);
  else if (BIGNUMP (obj))
    json_out_bignum (jo, obj);
  else
    wrong_type_argument (Qjson_value_p, obj);
}

static void
json_serialize (json_out_t *jo, Lisp_Object object,
		ptrdiff_t nargs, Lisp_Object *args)
{
  jo->maxdepth = 50;
  jo->size = 0;
  jo->capacity = 0;
  jo->chars_delta = 0;
  jo->buf = NULL;
  jo->ss_table = NULL;
  jo->conf.object_type = json_object_hashtable;
  jo->conf.array_type = json_array_array;
  jo->conf.null_object = QCnull;
  jo->conf.false_object = QCfalse;

  json_parse_args (nargs, args, &jo->conf, false);
  record_unwind_protect_ptr (cleanup_json_out, jo);

  /* Make float conversion independent of float-output-format.  */
  if (!NILP (Vfloat_output_format))
    specbind (Qfloat_output_format, Qnil);

  json_out_something (jo, object);
}

DEFUN ("json-serialize", Fjson_serialize, Sjson_serialize, 1, MANY,
       NULL,
       doc: /* Return the JSON representation of OBJECT as a unibyte string.

OBJECT is translated as follows:

`t'        -- the JSON `true' value.
number     -- a JSON number.
string     -- a JSON string.
vector     -- a JSON array.
hash-table -- a JSON object.  Keys must be strings.
alist      -- a JSON object.  Keys must be symbols.
plist      -- a JSON object.  Keys must be symbols.
              A leading colon in plist key names is elided.

For duplicate object keys, the first value is used.

The Lisp equivalents to the JSON null and false values are
configurable in the arguments ARGS, a list of keyword/argument pairs:

:null-object OBJ -- use OBJ to represent a JSON null value.
  It defaults to `:null'.

:false-object OBJ -- use OBJ to represent a JSON false value.
  It defaults to `:false'.

In you specify the same value for `:null-object' and `:false-object',
a potentially ambiguous situation, the JSON output will not contain
any JSON false values.
usage: (json-serialize OBJECT &rest ARGS)  */)
  (ptrdiff_t nargs, Lisp_Object *args)
{
  specpdl_ref count = SPECPDL_INDEX ();
  json_out_t jo;
  json_serialize (&jo, args[0], nargs - 1, args + 1);
  return unbind_to (count, make_unibyte_string (jo.buf, jo.size));
}

DEFUN ("json-insert", Fjson_insert, Sjson_insert, 1, MANY,
       NULL,
       doc: /* Insert the JSON representation of OBJECT before point.
This is the same as (insert (json-serialize OBJECT ...)), but potentially
faster, and with the difference that Unicode characters are inserted as
themselves into multibyte buffers, and as UTF-8 byte sequences into
unibyte buffers.
See the function `json-serialize' for allowed values of OBJECT and ARGS.
usage: (json-insert OBJECT &rest ARGS)  */)
  (ptrdiff_t nargs, Lisp_Object *args)
{
  specpdl_ref count = SPECPDL_INDEX ();
  json_out_t jo;
  json_serialize (&jo, args[0], nargs - 1, args + 1);

  prepare_to_modify_buffer (PT, PT, NULL);
  move_gap_both (PT, PT_BYTE);
  if (GAP_SIZE < jo.size)
    make_gap (jo.size - GAP_SIZE);
  memcpy (GPT_ADDR, jo.buf, jo.size);

  /* No need to keep allocation beyond this point.  */
  unbind_to (count, Qnil);

  bool ub_buffer = NILP (BVAR (current_buffer, enable_multibyte_characters));
  ptrdiff_t inserted_bytes = jo.size;
  ptrdiff_t inserted = ub_buffer ? jo.size : jo.size - jo.chars_delta;
  eassert (inserted > 0);

  insert_from_gap_1 (inserted, inserted_bytes, false);
  invalidate_buffer_caches (current_buffer, PT, PT + inserted);
  adjust_after_insert (PT, PT_BYTE, PT + inserted, PT_BYTE + inserted_bytes,
		       inserted);

  /* Call after-change hooks.  */
  signal_after_change (PT, 0, inserted);

  update_compositions (PT, PT, CHECK_BORDER);
  /* Move point to after the inserted text.  */
  SET_PT_BOTH (PT + inserted, PT_BYTE + inserted_bytes);

  return Qnil;
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

  Lisp_Object obj;
  ptrdiff_t (*byte_to_pos) (Lisp_Object obj, ptrdiff_t byte);
  ptrdiff_t (*byte_to_line) (Lisp_Object obj, ptrdiff_t byte);
};

static AVOID
json_signal_error (struct json_parser *p, Lisp_Object error)
{
  ptrdiff_t byte = (p->input_current - p->input_begin
		    + p->additional_bytes_count);
  ptrdiff_t pos = p->byte_to_pos (p->obj, byte);
  ptrdiff_t line = p->byte_to_line (p->obj, byte) + 1;
  /* The line number here is deprecated and provided for compatibility only.
     It is scheduled for removal in Emacs 32.  */
  xsignal3 (error, INT_TO_INTEGER (line), Qnil, INT_TO_INTEGER (pos));
}

static void
json_parser_init (struct json_parser *parser,
		  struct json_configuration conf,
		  const unsigned char *input,
		  const unsigned char *input_end,
		  const unsigned char *secondary_input,
		  const unsigned char *secondary_input_end,
		  ptrdiff_t (*byte_to_pos) (Lisp_Object, ptrdiff_t),
		  ptrdiff_t (*byte_to_line) (Lisp_Object, ptrdiff_t),
		  Lisp_Object obj)
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

  parser->available_depth = 10000;
  parser->conf = conf;

  parser->additional_bytes_count = 0;

  parser->object_workspace = parser->internal_object_workspace;
  parser->object_workspace_size
    = JSON_PARSER_INTERNAL_OBJECT_WORKSPACE_SIZE;
  parser->object_workspace_current = 0;

  parser->byte_workspace = parser->internal_byte_workspace;
  parser->byte_workspace_end = (parser->byte_workspace
				+ JSON_PARSER_INTERNAL_BYTE_WORKSPACE_SIZE);
  parser->byte_to_pos = byte_to_pos;
  parser->byte_to_line = byte_to_line;
  parser->obj = obj;
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
is_json_whitespace (int c)
{
  return c == 0x20 || c == 0x09 || c == 0x0d || c == 0x0a;
}

/* Skips JSON whitespace, and returns with the first non-whitespace
 * character */
static int
json_skip_whitespace (struct json_parser *parser)
{
  for (;;)
    {
      int c = json_input_get (parser);
      if (!is_json_whitespace (c))
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
      if (!is_json_whitespace (c) || c < 0)
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
      if (c < 0)
	json_signal_error (parser, Qjson_escape_sequence_error);
      v[i] = c;
    }

  return v[0] << 12 | v[1] << 8 | v[2] << 4 | v[3];
}

static AVOID
utf8_error (struct json_parser *parser)
{
  json_signal_error (parser, Qjson_utf8_decode_error);
}

/* Parse a string literal.  Optionally prepend a ':'.
   Return the string or an interned symbol.  */
static Lisp_Object
json_parse_string (struct json_parser *parser, bool intern, bool leading_colon)
{
  json_byte_workspace_reset (parser);
  if (leading_colon)
    json_byte_workspace_put (parser, ':');
  ptrdiff_t chars_delta = 0;	/* nbytes - nchars */
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
	  bool v0 = json_plain_char[c0];
	  bool v1 = json_plain_char[c1];
	  bool v2 = json_plain_char[c2];
	  bool v3 = json_plain_char[c3];
	  if (v0 && v1 && v2 && v3)
	    {
	      json_byte_workspace_put (parser, c0);
	      json_byte_workspace_put (parser, c1);
	      json_byte_workspace_put (parser, c2);
	      json_byte_workspace_put (parser, c3);
	      parser->input_current += 4;
	      continue;
	    }
	}

      int c = json_input_get (parser);
      if (json_plain_char[c])
	{
	  json_byte_workspace_put (parser, c);
	  continue;
	}

      if (c == '"')
	{
	  ptrdiff_t nbytes
	    = parser->byte_workspace_current - parser->byte_workspace;
	  ptrdiff_t nchars = nbytes - chars_delta;
	  const char *str = (const char *) parser->byte_workspace;
	  return (intern
		  ? intern_c_multibyte (str, nchars, nbytes)
		  : make_multibyte_string (str, nchars, nbytes));
	}

      if (c & 0x80)
	{
	  /* Parse UTF-8, strictly.  This is the correct thing to do
	     whether the input is a unibyte or multibyte string.  */
	  json_byte_workspace_put (parser, c);
	  unsigned char c1 = json_input_get (parser);
	  if ((c1 & 0xc0) != 0x80)
	    utf8_error (parser);
	  json_byte_workspace_put (parser, c1);
	  if (c <= 0xc1)
	    utf8_error (parser);
	  else if (c <= 0xdf)
	    chars_delta += 1;
	  else if (c <= 0xef)
	    {
	      unsigned char c2 = json_input_get (parser);
	      if ((c2 & 0xc0) != 0x80)
		utf8_error (parser);
	      int v = ((c & 0x0f) << 12) + ((c1 & 0x3f) << 6) + (c2 & 0x3f);
	      if (v < 0x800 || (v >= 0xd800 && v <= 0xdfff))
		utf8_error (parser);
	      json_byte_workspace_put (parser, c2);
	      chars_delta += 2;
	    }
	  else if (c <= 0xf7)
	    {
	      unsigned char c2 = json_input_get (parser);
	      unsigned char c3 = json_input_get (parser);
	      if ((c2 & 0xc0) != 0x80 || (c3 & 0xc0) != 0x80)
		utf8_error (parser);
	      int v = (((c & 0x07) << 18) + ((c1 & 0x3f) << 12)
		       + ((c2 & 0x3f) << 6) + (c3 & 0x3f));
	      if (v < 0x10000 || v > 0x10ffff)
		utf8_error (parser);
	      json_byte_workspace_put (parser, c2);
	      json_byte_workspace_put (parser, c3);
	      chars_delta += 3;
	    }
	  else
	    utf8_error (parser);
	}
      else if (c == '\\')
	{
	  /* Handle escape sequences */
	  c = json_input_get (parser);
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
		  if (json_input_get (parser) != '\\')
		    json_signal_error (parser,
				       Qjson_invalid_surrogate_error);
		  if (json_input_get (parser) != 'u')
		    json_signal_error (parser,
				       Qjson_invalid_surrogate_error);
		  int num2 = json_parse_unicode (parser);
		  if (num2 < 0xdc00 || num2 >= 0xe000)
		    json_signal_error (parser,
				       Qjson_invalid_surrogate_error);
		  num = (0x10000 + ((num - 0xd800) << 10 | (num2 - 0xdc00)));
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
		  chars_delta += 1;
		}
	      else if (num < 0x10000)
		{
		  json_byte_workspace_put (parser, 0xe0 | num >> 12);
		  json_byte_workspace_put (parser,
					   (0x80
					    | ((num >> 6) & 0x3f)));
		  json_byte_workspace_put (parser,
					   0x80 | (num & 0x3f));
		  chars_delta += 2;
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
		  chars_delta += 3;
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
    = string_to_number ((const char *) parser->byte_workspace, 10, &len);
  if (len != parser->byte_workspace_current - parser->byte_workspace - 1)
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
  bool out_of_range = (errno != 0 && (value == HUGE_VAL || value == -HUGE_VAL));
  if (out_of_range)
    json_signal_error (parser, Qjson_number_out_of_range);
  else if ((const unsigned char *) e != parser->byte_workspace_current - 1)
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

	  integer_overflow |= ckd_mul (&integer, integer, 10);
	  integer_overflow |= ckd_add (&integer, integer, c - '0');
	}
    }

  bool is_float = false;
  if (c == '.')
    {
      json_byte_workspace_put (parser, c);

      is_float = true;
      c = json_input_get (parser);
      json_byte_workspace_put (parser, c);
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
	}
    }
  if (c == 'e' || c == 'E')
    {
      json_byte_workspace_put (parser, c);

      is_float = true;
      c = json_input_get (parser);
      json_byte_workspace_put (parser, c);
      if (c == '-' || c == '+')
	{
	  c = json_input_get (parser);
	  json_byte_workspace_put (parser, c);
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

	  switch (parser->conf.object_type)
	    {
	    case json_object_hashtable:
	      {
		Lisp_Object key = json_parse_string (parser, false, false);
		Lisp_Object value = json_parse_object_member_value (parser);
		json_make_object_workspace_for (parser, 2);
		parser->object_workspace[parser->object_workspace_current] = key;
		parser->object_workspace_current++;
		parser->object_workspace[parser->object_workspace_current] = value;
		parser->object_workspace_current++;
		break;
	      }
	    case json_object_alist:
	      {
		Lisp_Object key = json_parse_string (parser, true, false);
		Lisp_Object value = json_parse_object_member_value (parser);
		Lisp_Object nc = Fcons (Fcons (key, value), Qnil);
		*cdr = nc;
		cdr = xcdr_addr (nc);
		break;
	      }
	    case json_object_plist:
	      {
		Lisp_Object key = json_parse_string (parser, true, true);
		Lisp_Object value = json_parse_object_member_value (parser);
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
	EMACS_INT value = (parser->object_workspace_current - first) / 2;
	result = make_hash_table (&hashtest_equal, value, Weak_None);
	struct Lisp_Hash_Table *h = XHASH_TABLE (result);
	for (size_t i = first; i < parser->object_workspace_current; i += 2)
	  {
	    hash_hash_t hash;
	    Lisp_Object key = parser->object_workspace[i];
	    Lisp_Object value = parser->object_workspace[i + 1];
	    ptrdiff_t i = hash_find_get_hash (h, key, &hash);
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

static Lisp_Object
json_parse_value (struct json_parser *parser, int c)
{
  switch (c)
    {
    case '{':
      return json_parse_object (parser);
    case '[':
      return json_parse_array (parser);
    case '"':
      return json_parse_string (parser, false, false);
    case '0': case '1': case '2': case '3': case '4':
    case '5': case '6': case '7': case '8': case '9':
    case '-':
      return json_parse_number (parser, c);
    case 't':
      if (json_input_get_if_possible (parser) == 'r'
	  && json_input_get_if_possible (parser) == 'u'
	  && json_input_get_if_possible (parser) == 'e')
	{
	  int c2 = json_input_get_if_possible (parser);
	  if (!json_is_token_char (c2))
	    {
	      if (c2 >= 0)
		json_input_put_back (parser);
	      return Qt;
	    }
	}
      break;
    case 'f':
      if (json_input_get_if_possible (parser) == 'a'
	  && json_input_get_if_possible (parser) == 'l'
	  && json_input_get_if_possible (parser) == 's'
	  && json_input_get_if_possible (parser) == 'e')
	{
	  int c2 = json_input_get_if_possible (parser);
	  if (!json_is_token_char (c2))
	    {
	      if (c2 >= 0)
		json_input_put_back (parser);
	      return parser->conf.false_object;
	    }
	}
      break;
    case 'n':
      if (json_input_get_if_possible (parser) == 'u'
	  && json_input_get_if_possible (parser) == 'l'
	  && json_input_get_if_possible (parser) == 'l')
	{
	  int c2 = json_input_get_if_possible (parser);
	  if (!json_is_token_char (c2))
	    {
	      if (c2 >= 0)
		json_input_put_back (parser);
	      return parser->conf.null_object;
	    }
	}
      break;
    }

  json_signal_error (parser, Qjson_parse_error);
}

static Lisp_Object
json_parse (struct json_parser *parser)
{
  return json_parse_value (parser, json_skip_whitespace (parser));
}

/* Count number of characters in the NBYTES bytes at S.  */
static ptrdiff_t
count_chars (const unsigned char *s, ptrdiff_t nbytes)
{
  ptrdiff_t nchars = 0;
  for (ptrdiff_t i = 0; i < nbytes; i++)
    nchars += (s[i] & 0xc0) != 0x80;
  return nchars;
}

/* Count number of newlines in the NBYTES bytes at S.  */
static ptrdiff_t
count_newlines (const unsigned char *s, ptrdiff_t nbytes)
{
  ptrdiff_t nls = 0;
  for (ptrdiff_t i = 0; i < nbytes; i++)
    nls += (s[i] == '\n');
  return nls;
}

static ptrdiff_t
string_byte_to_pos (Lisp_Object obj, ptrdiff_t byte)
{
  eassert (STRINGP (obj));
  eassert (byte <= SBYTES (obj));
  return STRING_MULTIBYTE (obj) ? count_chars (SDATA (obj), byte) : byte;
}

static ptrdiff_t
string_byte_to_line (Lisp_Object obj, ptrdiff_t byte)
{
  eassert (STRINGP (obj));
  eassert (byte <= SBYTES (obj));
  return count_newlines (SDATA (obj), byte);
}

DEFUN ("json-parse-string", Fjson_parse_string, Sjson_parse_string, 1, MANY,
       NULL,
       doc: /* Parse the JSON STRING into a Lisp value.
This is essentially the reverse operation of `json-serialize', which
see.  The returned value will be the JSON null value, the JSON false
value, t, a number, a string, a vector, a list, a hash-table, an alist,
or a plist.  Its elements will be further values of these types.
If STRING doesn't contain a valid JSON value, this function
signals an error of type `json-parse-error'.

The arguments ARGS are a list of keyword/argument pairs:

:object-type TYPE -- use TYPE to represent JSON objects.
  TYPE can be `hash-table' (the default), `alist' or `plist'.
  If an object has members with the same key, `hash-table' keeps only
  the last value of such keys, while `alist' and `plist' keep all the
  members.

:array-type TYPE -- use TYPE to represent JSON arrays.
  TYPE can be `array' (the default) or `list'.

:null-object OBJ -- use OBJ to represent a JSON null value.
  It defaults to `:null'.

:false-object OBJ -- use OBJ to represent a JSON false value.
  It defaults to `:false'.
usage: (json-parse-string STRING &rest ARGS) */)
(ptrdiff_t nargs, Lisp_Object *args)
{
  specpdl_ref count = SPECPDL_INDEX ();

  Lisp_Object string = args[0];
  CHECK_STRING (string);
  struct json_configuration conf
    = { json_object_hashtable, json_array_array, QCnull, QCfalse };
  json_parse_args (nargs - 1, args + 1, &conf, true);

  struct json_parser p;
  const unsigned char *begin = SDATA (string);
  json_parser_init (&p, conf, begin, begin + SBYTES (string), NULL, NULL,
		    string_byte_to_pos, string_byte_to_line, string);
  record_unwind_protect_ptr (json_parser_done, &p);
  Lisp_Object result = json_parse (&p);

  if (json_skip_whitespace_if_possible (&p) >= 0)
    json_signal_error (&p, Qjson_trailing_content);

  return unbind_to (count, result);
}

static ptrdiff_t
buffer_byte_to_pos (Lisp_Object obj, ptrdiff_t byte)
{
  /* The position from the start of the parse (for compatibility).  */
  return BYTE_TO_CHAR (PT_BYTE + byte) - PT;
}

static ptrdiff_t
buffer_byte_to_line (Lisp_Object obj, ptrdiff_t byte)
{
  /* Line from start of the parse (for compatibility). */
  ptrdiff_t to_gap = GPT_BYTE - PT_BYTE;
  return (to_gap > 0 && to_gap < byte
	  ? (count_newlines (PT_ADDR, to_gap)
	     + count_newlines (GAP_END_ADDR, byte - to_gap))
	  : count_newlines (PT_ADDR, byte));
}

DEFUN ("json-parse-buffer", Fjson_parse_buffer, Sjson_parse_buffer,
       0, MANY, NULL,
       doc: /* Read a JSON value from current buffer starting at point.
Move point after the end of the value if parsing was successful.
On error, don't move point.

The returned value will be a vector, list, hashtable, alist, or
plist.  Its elements will be the JSON null value, the JSON false
value, t, numbers, strings, or further vectors, lists, hashtables,
alists, or plists.

If the current buffer doesn't contain a valid JSON value, the
function signals an error of type `json-parse-error'.

The arguments ARGS are a list of keyword/argument pairs:

:object-type TYPE -- use TYPE to represent JSON objects.
  TYPE can be `hash-table' (the default), `alist' or `plist'.
  If an object has members with the same key, `hash-table' keeps only
  the last value of such keys, while `alist' and `plist' keep all the
  members.

:array-type TYPE -- use TYPE to represent JSON arrays.
  TYPE can be `array' (the default) or `list'.

:null-object OBJ -- use OBJ to represent a JSON null value.
  It defaults to `:null'.

:false-object OBJ -- use OBJ to represent a JSON false value.
  It defaults to `:false'.
usage: (json-parse-buffer &rest args) */)
(ptrdiff_t nargs, Lisp_Object *args)
{
  specpdl_ref count = SPECPDL_INDEX ();

  struct json_configuration conf
    = { json_object_hashtable, json_array_array, QCnull, QCfalse };
  json_parse_args (nargs, args, &conf, true);

  struct json_parser p;
  unsigned char *begin = PT_ADDR;
  unsigned char *end = (GPT == ZV) ? GPT_ADDR : ZV_ADDR;
  unsigned char *secondary_begin = NULL;
  unsigned char *secondary_end = NULL;
  if (PT == ZV)
    begin = end = NULL;
  else if (GPT > PT && GPT < ZV && GAP_SIZE > 0)
    {
      end = GPT_ADDR;
      secondary_begin = GAP_END_ADDR;
      secondary_end = ZV_ADDR;
    }

  json_parser_init (&p, conf, begin, end, secondary_begin, secondary_end,
		    buffer_byte_to_pos, buffer_byte_to_line, Qnil);
  record_unwind_protect_ptr (json_parser_done, &p);
  Lisp_Object result = json_parse (&p);

  ptrdiff_t byte = (PT_BYTE + p.input_current - p.input_begin
		    + p.additional_bytes_count);
  ptrdiff_t position = (NILP (BVAR (current_buffer,
				    enable_multibyte_characters))
			? byte
			: BYTE_TO_CHAR (byte));
  SET_PT_BOTH (position, byte);

  return unbind_to (count, result);
}

void
syms_of_json (void)
{
  DEFSYM (QCnull, ":null");
  DEFSYM (QCfalse, ":false");

  DEFSYM (Qjson_value_p, "json-value-p");

  DEFSYM (Qjson_error, "json-error");
  DEFSYM (Qjson_out_of_memory, "json-out-of-memory");
  DEFSYM (Qjson_parse_error, "json-parse-error");
  DEFSYM (Qjson_end_of_file, "json-end-of-file");
  DEFSYM (Qjson_trailing_content, "json-trailing-content");
  DEFSYM (Qjson_object_too_deep, "json-object-too-deep");
  DEFSYM (Qjson_utf8_decode_error, "json-utf8-decode-error");
  DEFSYM (Qjson_invalid_surrogate_error, "json-invalid-surrogate-error");
  DEFSYM (Qjson_number_out_of_range, "json-number-out-of-range-error");
  DEFSYM (Qjson_escape_sequence_error, "json-escape-sequence-error");
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

  DEFSYM (QCobject_type, ":object-type");
  DEFSYM (QCarray_type, ":array-type");
  DEFSYM (QCnull_object, ":null-object");
  DEFSYM (QCfalse_object, ":false-object");
  DEFSYM (Qalist, "alist");
  DEFSYM (Qplist, "plist");
  DEFSYM (Qarray, "array");

  defsubr (&Sjson_serialize);
  defsubr (&Sjson_insert);
  defsubr (&Sjson_parse_string);
  defsubr (&Sjson_parse_buffer);
}
