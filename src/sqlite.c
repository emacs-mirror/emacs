/* Support for accessing SQLite databases.

Copyright (C) 2021-2026 Free Software Foundation, Inc.

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
along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

This file is based on the emacs-sqlite3 package written by Syohei
YOSHIDA <syohex@gmail.com>, which can be found at:

   https://github.com/syohex/emacs-sqlite3  */

#include <config.h>

#include <c-strcase.h>
#include "lisp.h"
#include "coding.h"

#ifdef HAVE_SQLITE3

#include <sqlite3.h>

/* Support for loading SQLite extensions requires the ability to
   enable and disable loading of extensions (by default this is
   disabled, and we want to keep it that way).  The required macro is
   available since SQLite 3.13.  */
# if defined HAVE_SQLITE3_LOAD_EXTENSION && \
     defined SQLITE_DBCONFIG_ENABLE_LOAD_EXTENSION
#  define HAVE_LOAD_EXTENSION 1
# else
#  define HAVE_LOAD_EXTENSION 0
# endif

#ifdef WINDOWSNT

# include <windows.h>
# include "w32common.h"
# include "w32.h"

DEF_DLL_FN (SQLITE_API int, sqlite3_finalize, (sqlite3_stmt*));
DEF_DLL_FN (SQLITE_API int, sqlite3_close, (sqlite3*));
DEF_DLL_FN (SQLITE_API int, sqlite3_open_v2,
	    (const char*, sqlite3**, int, const char*));
DEF_DLL_FN (SQLITE_API int, sqlite3_reset, (sqlite3_stmt*));
DEF_DLL_FN (SQLITE_API int, sqlite3_bind_text,
	    (sqlite3_stmt*, int, const char*, int, void(*)(void*)));
DEF_DLL_FN (SQLITE_API int, sqlite3_bind_blob,
	    (sqlite3_stmt*, int, const char*, int, void(*)(void*)));
DEF_DLL_FN (SQLITE_API int, sqlite3_bind_int64,
	    (sqlite3_stmt*, int, sqlite3_int64));
DEF_DLL_FN (SQLITE_API int, sqlite3_bind_double, (sqlite3_stmt*, int, double));
DEF_DLL_FN (SQLITE_API int, sqlite3_bind_null, (sqlite3_stmt*, int));
DEF_DLL_FN (SQLITE_API int, sqlite3_bind_int, (sqlite3_stmt*, int, int));
DEF_DLL_FN (SQLITE_API int, sqlite3_extended_errcode, (sqlite3*));
DEF_DLL_FN (SQLITE_API const char*, sqlite3_errmsg, (sqlite3*));
#if SQLITE_VERSION_NUMBER >= 3007015
DEF_DLL_FN (SQLITE_API const char*, sqlite3_errstr, (int));
#endif
DEF_DLL_FN (SQLITE_API const char*, sqlite3_libversion, (void));
DEF_DLL_FN (SQLITE_API int, sqlite3_step, (sqlite3_stmt*));
DEF_DLL_FN (SQLITE_API int, sqlite3_changes, (sqlite3*));
DEF_DLL_FN (SQLITE_API int, sqlite3_column_count, (sqlite3_stmt*));
DEF_DLL_FN (SQLITE_API int, sqlite3_column_type, (sqlite3_stmt*, int));
DEF_DLL_FN (SQLITE_API sqlite3_int64, sqlite3_column_int64,
	    (sqlite3_stmt*, int));
DEF_DLL_FN (SQLITE_API double, sqlite3_column_double, (sqlite3_stmt*, int));
DEF_DLL_FN (SQLITE_API const void*, sqlite3_column_blob,
	    (sqlite3_stmt*, int));
DEF_DLL_FN (SQLITE_API int, sqlite3_column_bytes, (sqlite3_stmt*, int));
DEF_DLL_FN (SQLITE_API const unsigned char*, sqlite3_column_text,
	    (sqlite3_stmt*, int));
DEF_DLL_FN (SQLITE_API const char*, sqlite3_column_name, (sqlite3_stmt*, int));
DEF_DLL_FN (SQLITE_API int, sqlite3_exec,
	    (sqlite3*, const char*, int (*callback)(void*,int,char**,char**),
	     void*, char**));
DEF_DLL_FN (SQLITE_API int, sqlite3_prepare_v2,
	    (sqlite3*, const char*, int, sqlite3_stmt**, const char**));

# if HAVE_LOAD_EXTENSION
DEF_DLL_FN (SQLITE_API int, sqlite3_load_extension,
	    (sqlite3*, const char*, const char*, char**));
#  undef sqlite3_load_extension
#  define sqlite3_load_extension fn_sqlite3_load_extension
DEF_DLL_FN (SQLITE_API int, sqlite3_db_config, (sqlite3*, int, ...));
#  undef sqlite3_db_config
#  define sqlite3_db_config fn_sqlite3_db_config
# endif

# undef sqlite3_finalize
# undef sqlite3_close
# undef sqlite3_open_v2
# undef sqlite3_reset
# undef sqlite3_bind_text
# undef sqlite3_bind_blob
# undef sqlite3_bind_int64
# undef sqlite3_bind_double
# undef sqlite3_bind_null
# undef sqlite3_bind_int
# undef sqlite3_extended_errcode
# undef sqlite3_errmsg
# if SQLITE_VERSION_NUMBER >= 3007015
#  undef sqlite3_errstr
# endif
# undef sqlite3_libversion
# undef sqlite3_step
# undef sqlite3_changes
# undef sqlite3_column_count
# undef sqlite3_column_type
# undef sqlite3_column_int64
# undef sqlite3_column_double
# undef sqlite3_column_blob
# undef sqlite3_column_bytes
# undef sqlite3_column_text
# undef sqlite3_column_name
# undef sqlite3_exec
# undef sqlite3_prepare_v2

# define sqlite3_finalize fn_sqlite3_finalize
# define sqlite3_close fn_sqlite3_close
# define sqlite3_open_v2 fn_sqlite3_open_v2
# define sqlite3_reset fn_sqlite3_reset
# define sqlite3_bind_text fn_sqlite3_bind_text
# define sqlite3_bind_blob fn_sqlite3_bind_blob
# define sqlite3_bind_int64 fn_sqlite3_bind_int64
# define sqlite3_bind_double fn_sqlite3_bind_double
# define sqlite3_bind_null fn_sqlite3_bind_null
# define sqlite3_bind_int fn_sqlite3_bind_int
# define sqlite3_extended_errcode fn_sqlite3_extended_errcode
# define sqlite3_errmsg fn_sqlite3_errmsg
# if SQLITE_VERSION_NUMBER >= 3007015
#  define sqlite3_errstr fn_sqlite3_errstr
# endif
# define sqlite3_libversion fn_sqlite3_libversion
# define sqlite3_step fn_sqlite3_step
# define sqlite3_changes fn_sqlite3_changes
# define sqlite3_column_count fn_sqlite3_column_count
# define sqlite3_column_type fn_sqlite3_column_type
# define sqlite3_column_int64 fn_sqlite3_column_int64
# define sqlite3_column_double fn_sqlite3_column_double
# define sqlite3_column_blob fn_sqlite3_column_blob
# define sqlite3_column_bytes fn_sqlite3_column_bytes
# define sqlite3_column_text fn_sqlite3_column_text
# define sqlite3_column_name fn_sqlite3_column_name
# define sqlite3_exec fn_sqlite3_exec
# define sqlite3_prepare_v2 fn_sqlite3_prepare_v2

static bool
load_dll_functions (HMODULE library)
{
  LOAD_DLL_FN (library, sqlite3_finalize);
  LOAD_DLL_FN (library, sqlite3_close);
  LOAD_DLL_FN (library, sqlite3_open_v2);
  LOAD_DLL_FN (library, sqlite3_reset);
  LOAD_DLL_FN (library, sqlite3_bind_text);
  LOAD_DLL_FN (library, sqlite3_bind_blob);
  LOAD_DLL_FN (library, sqlite3_bind_int64);
  LOAD_DLL_FN (library, sqlite3_bind_double);
  LOAD_DLL_FN (library, sqlite3_bind_null);
  LOAD_DLL_FN (library, sqlite3_bind_int);
  LOAD_DLL_FN (library, sqlite3_extended_errcode);
  LOAD_DLL_FN (library, sqlite3_errmsg);
#if SQLITE_VERSION_NUMBER >= 3007015
  LOAD_DLL_FN (library, sqlite3_errstr);
#endif
  LOAD_DLL_FN (library, sqlite3_libversion);
  LOAD_DLL_FN (library, sqlite3_step);
  LOAD_DLL_FN (library, sqlite3_changes);
  LOAD_DLL_FN (library, sqlite3_column_count);
  LOAD_DLL_FN (library, sqlite3_column_type);
  LOAD_DLL_FN (library, sqlite3_column_int64);
  LOAD_DLL_FN (library, sqlite3_column_double);
  LOAD_DLL_FN (library, sqlite3_column_blob);
  LOAD_DLL_FN (library, sqlite3_column_bytes);
  LOAD_DLL_FN (library, sqlite3_column_text);
  LOAD_DLL_FN (library, sqlite3_column_name);
  LOAD_DLL_FN (library, sqlite3_exec);
# if HAVE_LOAD_EXTENSION
  LOAD_DLL_FN (library, sqlite3_load_extension);
  LOAD_DLL_FN (library, sqlite3_db_config);
# endif
  LOAD_DLL_FN (library, sqlite3_prepare_v2);
  return true;
}
#endif /* WINDOWSNT */

static bool
init_sqlite_functions (void)
{
#ifdef WINDOWSNT
  static bool sqlite3_initialized;

  if (!sqlite3_initialized)
    {
      HMODULE library = w32_delayed_load (Qsqlite3);

      if (!library)
	message1 ("sqlite3 library was not found");
      else if (load_dll_functions (library))
	{
	  sqlite3_initialized = true;
	  Vlibrary_cache = Fcons (Fcons (Qsqlite3, Qt), Vlibrary_cache);
	}
      else
	{
	  message1 ("sqlite3 library was found, but could not be loaded successfully");
	  Vlibrary_cache = Fcons (Fcons (Qsqlite3, Qnil), Vlibrary_cache);
	}
    }
  return sqlite3_initialized;
#else  /* !WINDOWSNT */
  return true;
#endif	/* !WINDOWSNT */
}


static void
sqlite_free (void *arg)
{
  struct Lisp_Sqlite *ptr = (struct Lisp_Sqlite *)arg;
  if (ptr->is_statement)
    sqlite3_finalize (ptr->stmt);
  else if (ptr->db)
    sqlite3_close (ptr->db);
  xfree (ptr->name);
  xfree (ptr);
}

static Lisp_Object
encode_string (Lisp_Object string)
{
  if (STRING_MULTIBYTE (string))
    return encode_string_utf_8 (string, Qnil, 0, Qt, Qt);
  else
    return string;
}

static Lisp_Object
make_sqlite (bool is_statement, void *db, void *stmt, char *name)
{
  struct Lisp_Sqlite *ptr
    = ALLOCATE_PLAIN_PSEUDOVECTOR (struct Lisp_Sqlite, PVEC_SQLITE);
  ptr->is_statement = is_statement;
  ptr->finalizer = sqlite_free;
  ptr->db = db;
  ptr->name = name;
  ptr->stmt = stmt;
  ptr->eof = false;
  return make_lisp_ptr (ptr, Lisp_Vectorlike);
}

static void
check_sqlite (Lisp_Object db, bool is_statement)
{
  init_sqlite_functions ();
  CHECK_SQLITE (db);
  if (is_statement && !XSQLITE (db)->is_statement)
    xsignal1 (Qsqlite_error, build_string ("Invalid set object"));
  else if (!is_statement && XSQLITE (db)->is_statement)
    xsignal1 (Qsqlite_error, build_string ("Invalid database object"));
  if (!is_statement && !XSQLITE (db)->db)
    xsignal1 (Qsqlite_error, build_string ("Database closed"));
  else if (is_statement && !XSQLITE (db)->db)
    xsignal1 (Qsqlite_error, build_string ("Statement closed"));
}

static int db_count = 0;

DEFUN ("sqlite-open", Fsqlite_open, Ssqlite_open, 0, 3, 0,
       doc: /* Open FILE as an sqlite database.
If FILE is nil or omitted, an in-memory database will be opened instead.
If READONLY is non-nil or omitted, open the database in read-only mode,
otherwise open it in read-write mode.
By default, file:// URIs are automatically recognized, unless
DISABLE-URI is non-nil.  */)
  (Lisp_Object file, Lisp_Object readonly, Lisp_Object disable_uri)
{
  Lisp_Object name;
  int flags;

  if (!NILP (readonly))
    flags = SQLITE_OPEN_READONLY;
  else
    flags = (SQLITE_OPEN_CREATE | SQLITE_OPEN_READWRITE);
#ifdef SQLITE_OPEN_FULLMUTEX
  flags |= SQLITE_OPEN_FULLMUTEX;
#endif
#ifdef SQLITE_OPEN_URI
  if (NILP (disable_uri))
    flags |= SQLITE_OPEN_URI;
#endif

  if (!init_sqlite_functions ())
    xsignal1 (Qsqlite_error, build_string ("sqlite support is not available"));

  if (!NILP (file))
    name = ENCODE_FILE (Fexpand_file_name (file, Qnil));
  else
    {
#ifdef SQLITE_OPEN_MEMORY
      /* In-memory database.  These have to have different names to
	 refer to different databases.  */
      AUTO_STRING (memory_fmt, ":memory:%d");
      name = CALLN (Fformat, memory_fmt, make_int (++db_count));
      flags |= SQLITE_OPEN_MEMORY;
#else
      xsignal1 (Qsqlite_error, build_string ("sqlite in-memory is not available"));
#endif
    }

  sqlite3 *sdb;
  if (sqlite3_open_v2 (SSDATA (name), &sdb, flags, NULL) != SQLITE_OK)
    return Qnil;

  return make_sqlite (false, sdb, NULL, xstrdup (SSDATA (name)));
}

DEFUN ("sqlite-close", Fsqlite_close, Ssqlite_close, 1, 1, 0,
       doc: /* Close the sqlite database DB.  */)
  (Lisp_Object db)
{
  check_sqlite (db, false);
  sqlite3_close (XSQLITE (db)->db);
  XSQLITE (db)->db = NULL;
  return Qt;
}

/* Bind values in a statement like
   "insert into foo values (?, ?, ?)".  */
static const char *
bind_values (sqlite3 *db, sqlite3_stmt *stmt, Lisp_Object values)
{
  sqlite3_reset (stmt);
  int len;
  if (VECTORP (values))
    len = ASIZE (values);
  else
    len = list_length (values);

  for (int i = 0; i < len; ++i)
    {
      int ret = SQLITE_MISMATCH;
      Lisp_Object value;
      if (VECTORP (values))
	value = AREF (values, i);
      else
	{
	  value = XCAR (values);
	  values = XCDR (values);
	}
      if (STRINGP (value))
	{
	  Lisp_Object encoded;
	  bool blob = false;

	  if (SBYTES (value) == 0)
	    encoded = value;
	  else
	    {
	      Lisp_Object coding_system =
		Fget_text_property (make_fixnum (0), Qcoding_system, value);
	      if (NILP (coding_system))
		/* Default to utf-8.  */
		encoded = encode_string (value);
	      else if (EQ (coding_system, Qbinary))
		blob = true;
	      else
		encoded = Fencode_coding_string (value, coding_system,
						 Qnil, Qnil);
	    }

	  if (blob)
	    {
	      if (SBYTES (value) != SCHARS (value))
		xsignal1 (Qsqlite_error, build_string ("BLOB values must be unibyte"));
	    ret = sqlite3_bind_blob (stmt, i + 1,
				       SSDATA (value), SBYTES (value),
				       NULL);
	    }
	    else
	      ret = sqlite3_bind_text (stmt, i + 1,
				       SSDATA (encoded), SBYTES (encoded),
				       NULL);
	}
      else if (FIXNUMP (value))
	ret = sqlite3_bind_int64 (stmt, i + 1, XFIXNUM (value));
      else if (BIGNUMP (value))
	ret = sqlite3_bind_int64 (stmt, i + 1, bignum_to_intmax (value));
      else if (FLOATP (value))
	ret = sqlite3_bind_double (stmt, i + 1, XFLOAT_DATA (value));
      else if (NILP (value))
	ret = sqlite3_bind_null (stmt, i + 1);
      else if (EQ (value, Qt))
	ret = sqlite3_bind_int (stmt, i + 1, 1);
      else if (EQ (value, Qfalse))
	ret = sqlite3_bind_int (stmt, i + 1, 0);
      else
	return "invalid argument";

      if (ret != SQLITE_OK)
	return sqlite3_errmsg (db);
    }

  return NULL;
}

static Lisp_Object
row_to_value (sqlite3_stmt *stmt)
{
  int len = sqlite3_column_count (stmt);
  Lisp_Object values = Qnil;

  for (int i = len - 1; i >= 0; i--)
    {
      Lisp_Object v = Qnil;

      switch (sqlite3_column_type (stmt, i))
	{
	case SQLITE_INTEGER:
	  v = make_int (sqlite3_column_int64 (stmt, i));
	  break;

	case SQLITE_FLOAT:
	  v = make_float (sqlite3_column_double (stmt, i));
	  break;

	case SQLITE_BLOB:
	  v = make_unibyte_string (sqlite3_column_blob (stmt, i),
				   sqlite3_column_bytes (stmt, i));
	  break;

	case SQLITE_NULL:
	  v = Qnil;
	  break;

	case SQLITE_TEXT:
	  v =
	    code_convert_string_norecord
	    (make_unibyte_string ((const char *)sqlite3_column_text (stmt, i),
				  sqlite3_column_bytes (stmt, i)),
	     Qutf_8, false);
	  break;
	}

      values = Fcons (v, values);
    }

  return values;
}

static Lisp_Object
sqlite_prepare_errdata (int code, sqlite3 *sdb)
{
  Lisp_Object errcode = make_fixnum (code);
  const char *errmsg = sqlite3_errmsg (sdb);
  Lisp_Object lerrmsg = errmsg ? build_string (errmsg) : Qnil;
  Lisp_Object errstr, ext_errcode;

#if SQLITE_VERSION_NUMBER >= 3007015
  errstr = build_string (sqlite3_errstr (code));
#else
  /* The internet says this is identical to sqlite3_errstr (code).  */
  errstr = lerrmsg;
#endif

  /* More details about what went wrong.  */
#if SQLITE_VERSION_NUMBER >= 3006005
  ext_errcode = make_fixnum (sqlite3_extended_errcode (sdb));
#else
  /* What value to use here?  */
  ext_errcode = make_fixnum (0);
#endif

  return list4 (errstr, lerrmsg, errcode, ext_errcode);
}

DEFUN ("sqlite-execute", Fsqlite_execute, Ssqlite_execute, 2, 3, 0,
       doc: /* Execute a non-select SQL statement.
If VALUES is non-nil, it should be a vector or a list of values
to bind when executing a statement like

   insert into foo values (?, ?, ...)

Value is the number of affected rows.  */)
  (Lisp_Object db, Lisp_Object query, Lisp_Object values)
{
  check_sqlite (db, false);
  CHECK_STRING (query);
  if (!(NILP (values) || CONSP (values) || VECTORP (values)))
    xsignal1 (Qsqlite_error, build_string ("VALUES must be a list or a vector"));

  sqlite3 *sdb = XSQLITE (db)->db;
  Lisp_Object errmsg = Qnil,
    encoded = encode_string (query);
  sqlite3_stmt *stmt = NULL;

  /* We only execute the first statement -- if there's several
     (separated by a semicolon), the subsequent statements won't be
     done.  */
  int ret = sqlite3_prepare_v2 (sdb, SSDATA (encoded), -1, &stmt, NULL);
  if (ret != SQLITE_OK)
    {
      if (stmt != NULL)
	{
	  sqlite3_finalize (stmt);
	  sqlite3_reset (stmt);
	}

      errmsg = sqlite_prepare_errdata (ret, sdb);
      goto exit;
    }

  /* Bind ? values.  */
  if (!NILP (values))
    {
      const char *err = bind_values (sdb, stmt, values);
      if (err != NULL)
	{
	  errmsg = build_string (err);
	  goto exit;
	}
    }

  ret = sqlite3_step (stmt);

  if (ret == SQLITE_ROW)
    {
      Lisp_Object data = Qnil;
      do
	data = Fcons (row_to_value (stmt), data);
      while (sqlite3_step (stmt) == SQLITE_ROW);

      sqlite3_finalize (stmt);
      return Fnreverse (data);
    }
  else if (ret == SQLITE_OK || ret == SQLITE_DONE)
    {
      Lisp_Object rows = make_fixnum (sqlite3_changes (sdb));
      sqlite3_finalize (stmt);
      return rows;
    }
  else
    errmsg = build_string (sqlite3_errmsg (sdb));

 exit:
  sqlite3_finalize (stmt);
  xsignal1 (ret == SQLITE_LOCKED || ret == SQLITE_BUSY?
	    Qsqlite_locked_error: Qsqlite_error,
	    errmsg);
}

static Lisp_Object
column_names (sqlite3_stmt *stmt)
{
  Lisp_Object columns = Qnil;
  int count = sqlite3_column_count (stmt);
  for (int i = 0; i < count; ++i)
    columns = Fcons (build_string (sqlite3_column_name (stmt, i)), columns);

  return Fnreverse (columns);
}

DEFUN ("sqlite-select", Fsqlite_select, Ssqlite_select, 2, 4, 0,
       doc: /* Select data from the database DB that matches QUERY.
If VALUES is non-nil, it should be a list or a vector specifying the
values that will be interpolated into a parameterized statement.

By default, the return value is a list, whose contents depend on
the value of the optional argument RETURN-TYPE.

If RETURN-TYPE is nil or omitted, the function returns a list of rows
matching QUERY.  If RETURN-TYPE is `full', the function returns a
list whose first element is the list of column names, and the rest
of the elements are the rows matching QUERY.  If RETURN-TYPE is `set',
the function returns a set object that can be queried with functions
like `sqlite-next' etc., in order to get the data.  */)
  (Lisp_Object db, Lisp_Object query, Lisp_Object values,
   Lisp_Object return_type)
{
  check_sqlite (db, false);
  CHECK_STRING (query);

  if (!(NILP (values) || CONSP (values) || VECTORP (values)))
    xsignal1 (Qsqlite_error, build_string ("VALUES must be a list or a vector"));

  sqlite3 *sdb = XSQLITE (db)->db;
  Lisp_Object retval = Qnil, errmsg = Qnil,
    encoded = encode_string (query);

  sqlite3_stmt *stmt = NULL;
  int ret = sqlite3_prepare_v2 (sdb, SSDATA (encoded), SBYTES (encoded),
				&stmt, NULL);
  if (ret != SQLITE_OK)
    {
      if (stmt)
	sqlite3_finalize (stmt);
      errmsg = sqlite_prepare_errdata (ret, sdb);
      goto exit;
    }

  /* Query with parameters.  */
  if (!NILP (values))
    {
      const char *err = bind_values (sdb, stmt, values);
      if (err != NULL)
	{
	  sqlite3_finalize (stmt);
	  errmsg = build_string (err);
	  goto exit;
	}
    }

  /* Return a handle to get the data.  */
  if (EQ (return_type, Qset))
    {
      retval = make_sqlite (true, sdb, stmt, XSQLITE (db)->name);
      goto exit;
    }

  /* Return the data directly.  */
  Lisp_Object data = Qnil;
  while (sqlite3_step (stmt) == SQLITE_ROW)
    data = Fcons (row_to_value (stmt), data);

  if (EQ (return_type, Qfull))
    retval = Fcons (column_names (stmt), Fnreverse (data));
  else
    retval = Fnreverse (data);
  sqlite3_finalize (stmt);

 exit:
  if (! NILP (errmsg))
    xsignal1 (Qsqlite_error, errmsg);

  return retval;
}

static Lisp_Object
sqlite_exec (sqlite3 *sdb, const char *query)
{
  int ret = sqlite3_exec (sdb, query, NULL, NULL, NULL);
  if (ret != SQLITE_OK)
    return Qnil;

  return Qt;
}

DEFUN ("sqlite-execute-batch", Fsqlite_execute_batch, Ssqlite_execute_batch, 2, 2, 0,
       doc: /* Execute multiple SQL STATEMENTS in DB.
STATEMENTS is a string containing 0 or more SQL statements.  */)
  (Lisp_Object db, Lisp_Object statements)
{
  check_sqlite (db, false);
  CHECK_STRING (statements);
  Lisp_Object encoded = encode_string (statements);
  return sqlite_exec (XSQLITE (db)->db, SSDATA (encoded));
}

DEFUN ("sqlite-transaction", Fsqlite_transaction, Ssqlite_transaction, 1, 1, 0,
       doc: /* Start a transaction in DB.  */)
  (Lisp_Object db)
{
  check_sqlite (db, false);
  return sqlite_exec (XSQLITE (db)->db, "begin");
}

DEFUN ("sqlite-commit", Fsqlite_commit, Ssqlite_commit, 1, 1, 0,
       doc: /* Commit a transaction in DB.  */)
  (Lisp_Object db)
{
  check_sqlite (db, false);
  return sqlite_exec (XSQLITE (db)->db, "commit");
}

DEFUN ("sqlite-rollback", Fsqlite_rollback, Ssqlite_rollback, 1, 1, 0,
       doc: /* Roll back a transaction in DB.  */)
  (Lisp_Object db)
{
  check_sqlite (db, false);
  return sqlite_exec (XSQLITE (db)->db, "rollback");
}

DEFUN ("sqlite-pragma", Fsqlite_pragma, Ssqlite_pragma, 2, 2, 0,
       doc: /* Execute PRAGMA in DB.  */)
  (Lisp_Object db, Lisp_Object pragma)
{
  check_sqlite (db, false);
  CHECK_STRING (pragma);

  return sqlite_exec (XSQLITE (db)->db,
		      SSDATA (concat2 (build_string ("PRAGMA "), pragma)));
}

#if HAVE_LOAD_EXTENSION
DEFUN ("sqlite-load-extension", Fsqlite_load_extension,
       Ssqlite_load_extension, 2, 2, 0,
       doc: /* Load an SQlite MODULE into DB.
MODULE should be the name of an SQlite module's file, a
shared library in the system-dependent format and having a
system-dependent file-name extension.

Only modules on Emacs's list of allowed modules can be loaded.  */)
  (Lisp_Object db, Lisp_Object module)
{
  check_sqlite (db, false);
  CHECK_STRING (module);

  /* Add names of useful and free modules here.  */
  const char *allowlist[] = {
    "base64",
    "cksumvfs",
    "compress",
    "csv",
    "csvtable",
    "fts3",
    "icu",
    "pcre",
    "percentile",
    "regexp",
    "rot13",
    "rtree",
    "sha1",
    "uuid",
    "vec0",
    "vector0",
    "vfslog",
    "vss0",
    "zipfile",
    NULL
  };
  char *name = SSDATA (Ffile_name_nondirectory (module));
  /* Possibly skip past a common prefix (libsqlite3_mod_ is used by
     Debian, see https://packages.debian.org/source/sid/sqliteodbc).  */
  const char *prefix = "libsqlite3_mod_";
  if (!strncmp (name, prefix, strlen (prefix)))
    name += strlen (prefix);

  bool do_allow = false;
  for (const char **allow = allowlist; *allow; allow++)
    {
      ptrdiff_t allow_len = strlen (*allow);
      if (allow_len < strlen (name)
	  && !strncmp (*allow, name, allow_len)
	  && (!strcmp (name + allow_len, ".so")
	      ||!strcmp (name + allow_len, ".dylib")
	      || !strcasecmp (name + allow_len, ".dll")))
	{
	  do_allow = true;
	  break;
	}
    }

  if (!do_allow)
    xsignal1 (Qsqlite_error, build_string ("Module name not on allowlist"));

  /* Expand all Lisp data explicitly, so as to avoid signaling an
     error while extension loading is enabled -- we don't want to
     "leak" this outside this function.  */
  sqlite3 *sdb = XSQLITE (db)->db;
  char *ext_fn = SSDATA (ENCODE_FILE (Fexpand_file_name (module, Qnil)));
  /* Temporarily enable loading extensions via the C API.  */
  int result = sqlite3_db_config (sdb, SQLITE_DBCONFIG_ENABLE_LOAD_EXTENSION, 1,
				  NULL);
  if (result == SQLITE_OK)
    {
      result = sqlite3_load_extension (sdb, ext_fn, NULL, NULL);
      /* Disable loading extensions via C API.  */
      sqlite3_db_config (sdb, SQLITE_DBCONFIG_ENABLE_LOAD_EXTENSION, 0, NULL);
      if (result == SQLITE_OK)
	return Qt;
    }
  return Qnil;
}
#endif /* HAVE_LOAD_EXTENSION */

DEFUN ("sqlite-next", Fsqlite_next, Ssqlite_next, 1, 1, 0,
       doc: /* Return the next result set from SET.
Return nil when the statement has finished executing successfully.  */)
  (Lisp_Object set)
{
  check_sqlite (set, true);

  if (XSQLITE (set)->eof)
    return Qnil;

  int ret = sqlite3_step (XSQLITE (set)->stmt);
  if (ret != SQLITE_ROW && ret != SQLITE_OK && ret != SQLITE_DONE)
    xsignal1 (Qsqlite_error, build_string (sqlite3_errmsg (XSQLITE (set)->db)));

  if (ret == SQLITE_DONE)
    {
      XSQLITE (set)->eof = true;
      return Qnil;
    }

  return row_to_value (XSQLITE (set)->stmt);
}

DEFUN ("sqlite-columns", Fsqlite_columns, Ssqlite_columns, 1, 1, 0,
       doc: /* Return the column names of SET.  */)
  (Lisp_Object set)
{
  check_sqlite (set, true);
  return column_names (XSQLITE (set)->stmt);
}

DEFUN ("sqlite-more-p", Fsqlite_more_p, Ssqlite_more_p, 1, 1, 0,
       doc: /* Say whether there are any further results in SET.  */)
  (Lisp_Object set)
{
  check_sqlite (set, true);

  if (XSQLITE (set)->eof)
    return Qnil;
  else
    return Qt;
}

DEFUN ("sqlite-finalize", Fsqlite_finalize, Ssqlite_finalize, 1, 1, 0,
       doc: /* Mark this SET as being finished.
This will free the resources held by SET.  */)
  (Lisp_Object set)
{
  check_sqlite (set, true);
  sqlite3_finalize (XSQLITE (set)->stmt);
  XSQLITE (set)->db = NULL;
  return Qt;
}

DEFUN ("sqlite-version", Fsqlite_version, Ssqlite_version, 0, 0, 0,
       doc: /* Return the version string of the SQLite library.
Signal an error if SQLite support is not available.  */)
  (void)
{
  if (!init_sqlite_functions ())
    error ("sqlite support is not available");
  return build_string (sqlite3_libversion ());
}

#endif /* HAVE_SQLITE3 */

DEFUN ("sqlitep", Fsqlitep, Ssqlitep, 1, 1, 0,
       doc: /* Say whether OBJECT is an SQlite object.  */)
  (Lisp_Object object)
{
#ifdef HAVE_SQLITE3
  return SQLITE (object)? Qt: Qnil;
#else
  return Qnil;
#endif
}

DEFUN ("sqlite-available-p", Fsqlite_available_p, Ssqlite_available_p, 0, 0, 0,
       doc: /* Return t if sqlite3 support is available in this instance of Emacs.*/)
  (void)
{
#ifdef HAVE_SQLITE3
# ifdef WINDOWSNT
  Lisp_Object found = Fassq (Qsqlite3, Vlibrary_cache);
  if (CONSP (found))
    return XCDR (found);
  else
    return init_sqlite_functions () ? Qt : Qnil;
# else
  return Qt;
#endif
#else
  return Qnil;
#endif
}

void
syms_of_sqlite (void)
{
#ifdef HAVE_SQLITE3
  defsubr (&Ssqlite_open);
  defsubr (&Ssqlite_close);
  defsubr (&Ssqlite_execute);
  defsubr (&Ssqlite_select);
  defsubr (&Ssqlite_execute_batch);
  defsubr (&Ssqlite_transaction);
  defsubr (&Ssqlite_commit);
  defsubr (&Ssqlite_rollback);
  defsubr (&Ssqlite_pragma);
#if HAVE_LOAD_EXTENSION
  defsubr (&Ssqlite_load_extension);
#endif
  defsubr (&Ssqlite_next);
  defsubr (&Ssqlite_columns);
  defsubr (&Ssqlite_more_p);
  defsubr (&Ssqlite_finalize);
  defsubr (&Ssqlite_version);
  DEFSYM (Qset, "set");
  DEFSYM (Qfull, "full");
#endif
  defsubr (&Ssqlitep);
  defsubr (&Ssqlite_available_p);

  DEFSYM (Qsqlite_error, "sqlite-error");
  Fput (Qsqlite_error, Qerror_conditions,
	list2 (Qsqlite_error, Qerror));
  Fput (Qsqlite_error, Qerror_message,
	build_string ("Database error"));

  DEFSYM (Qsqlite_locked_error, "sqlite-locked-error");
  Fput (Qsqlite_locked_error, Qerror_conditions,
	list3 (Qsqlite_locked_error, Qsqlite_error, Qerror));
  Fput (Qsqlite_locked_error, Qerror_message,
	build_string ("Database locked"));

  DEFSYM (Qsqlitep, "sqlitep");
  DEFSYM (Qfalse, "false");
  DEFSYM (Qsqlite, "sqlite");
  DEFSYM (Qsqlite3, "sqlite3");
  DEFSYM (Qbinary, "binary");
  DEFSYM (Qcoding_system, "coding-system");
}
