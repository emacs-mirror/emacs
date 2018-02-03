/* File IO for GNU Emacs.

Copyright (C) 1985-1988, 1993-2018 Free Software Foundation, Inc.

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
#include <limits.h>
#include <fcntl.h>
#include "sysstdio.h"
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>

#ifdef DARWIN_OS
#include <sys/attr.h>
#endif

#ifdef HAVE_PWD_H
#include <pwd.h>
#endif

#include <errno.h>

#ifdef HAVE_LIBSELINUX
#include <selinux/selinux.h>
#include <selinux/context.h>
#endif

#if USE_ACL && defined HAVE_ACL_SET_FILE
#include <sys/acl.h>
#endif

#include <c-ctype.h>

#include "lisp.h"
#include "composite.h"
#include "character.h"
#include "buffer.h"
#include "coding.h"
#include "window.h"
#include "blockinput.h"
#include "region-cache.h"
#include "frame.h"

#ifdef HAVE_LINUX_FS_H
# include <sys/ioctl.h>
# include <linux/fs.h>
#endif

#ifdef WINDOWSNT
#define NOMINMAX 1
#include <windows.h>
/* The redundant #ifdef is to avoid compiler warning about unused macro.  */
#ifdef NOMINMAX
#undef NOMINMAX
#endif
#include <sys/file.h>
#include "w32.h"
#endif /* not WINDOWSNT */

#ifdef MSDOS
#include "msdos.h"
#include <sys/param.h>
#endif

#ifdef DOS_NT
/* On Windows, drive letters must be alphabetic - on DOS, the Netware
   redirector allows the six letters between 'Z' and 'a' as well.  */
#ifdef MSDOS
#define IS_DRIVE(x) ((x) >= 'A' && (x) <= 'z')
#endif
#ifdef WINDOWSNT
#define IS_DRIVE(x) c_isalpha (x)
#endif
/* Need to lower-case the drive letter, or else expanded
   filenames will sometimes compare unequal, because
   `expand-file-name' doesn't always down-case the drive letter.  */
#define DRIVE_LETTER(x) c_tolower (x)
#endif

#include "systime.h"
#include <acl.h>
#include <allocator.h>
#include <careadlinkat.h>
#include <fsusage.h>
#include <stat-time.h>
#include <tempname.h>

#include <binary-io.h>

#ifdef HPUX
#include <netio.h>
#endif

#include "commands.h"

/* True during writing of auto-save files.  */
static bool auto_saving;

/* Emacs's real umask.  */
static mode_t realmask;

/* Nonzero umask during creation of auto-save directories.  */
static mode_t auto_saving_dir_umask;

/* Set by auto_save_1 to mode of original file so Fwrite_region will create
   a new file with the same mode as the original.  */
static mode_t auto_save_mode_bits;

/* Set by auto_save_1 if an error occurred during the last auto-save.  */
static bool auto_save_error_occurred;

/* If VALID_TIMESTAMP_FILE_SYSTEM, then TIMESTAMP_FILE_SYSTEM is the device
   number of a file system where time stamps were observed to work.  */
static bool valid_timestamp_file_system;
static dev_t timestamp_file_system;

/* Each time an annotation function changes the buffer, the new buffer
   is added here.  */
static Lisp_Object Vwrite_region_annotation_buffers;

static bool a_write (int, Lisp_Object, ptrdiff_t, ptrdiff_t,
		     Lisp_Object *, struct coding_system *);
static bool e_write (int, Lisp_Object, ptrdiff_t, ptrdiff_t,
		     struct coding_system *);


/* Return true if FILENAME exists.  */

static bool
check_existing (const char *filename)
{
  return faccessat (AT_FDCWD, filename, F_OK, AT_EACCESS) == 0;
}

/* Return true if file FILENAME exists and can be executed.  */

static bool
check_executable (char *filename)
{
  return faccessat (AT_FDCWD, filename, X_OK, AT_EACCESS) == 0;
}

/* Return true if file FILENAME exists and can be accessed
   according to AMODE, which should include W_OK.
   On failure, return false and set errno.  */

static bool
check_writable (const char *filename, int amode)
{
#ifdef MSDOS
  /* FIXME: an faccessat implementation should be added to the
     DOS/Windows ports and this #ifdef branch should be removed.  */
  struct stat st;
  if (stat (filename, &st) < 0)
    return 0;
  errno = EPERM;
  return (st.st_mode & S_IWRITE || S_ISDIR (st.st_mode));
#else /* not MSDOS */
  bool res = faccessat (AT_FDCWD, filename, amode, AT_EACCESS) == 0;
#ifdef CYGWIN
  /* faccessat may have returned failure because Cygwin couldn't
     determine the file's UID or GID; if so, we return success.  */
  if (!res)
    {
      int faccessat_errno = errno;
      struct stat st;
      if (stat (filename, &st) < 0)
        return 0;
      res = (st.st_uid == -1 || st.st_gid == -1);
      errno = faccessat_errno;
    }
#endif /* CYGWIN */
  return res;
#endif /* not MSDOS */
}

/* Signal a file-access failure.  STRING describes the failure,
   NAME the file involved, and ERRORNO the errno value.

   If NAME is neither null nor a pair, package it up as a singleton
   list before reporting it; this saves report_file_errno's caller the
   trouble of preserving errno before calling list1.  */

void
report_file_errno (char const *string, Lisp_Object name, int errorno)
{
  Lisp_Object data = CONSP (name) || NILP (name) ? name : list1 (name);
  char *str = emacs_strerror (errorno);
  AUTO_STRING (unibyte_str, str);
  Lisp_Object errstring
    = code_convert_string_norecord (unibyte_str, Vlocale_coding_system, 0);
  Lisp_Object errdata = Fcons (errstring, data);

  if (errorno == EEXIST)
    xsignal (Qfile_already_exists, errdata);
  else
    xsignal (errorno == ENOENT ? Qfile_missing : Qfile_error,
	     Fcons (build_string (string), errdata));
}

/* Signal a file-access failure that set errno.  STRING describes the
   failure, NAME the file involved.  When invoking this function, take
   care to not use arguments such as build_string ("foo") that involve
   side effects that may set errno.  */

void
report_file_error (char const *string, Lisp_Object name)
{
  report_file_errno (string, name, errno);
}

/* Like report_file_error, but reports a file-notify-error instead.  */

void
report_file_notify_error (const char *string, Lisp_Object name)
{
  char *str = emacs_strerror (errno);
  AUTO_STRING (unibyte_str, str);
  Lisp_Object errstring
    = code_convert_string_norecord (unibyte_str, Vlocale_coding_system, 0);
  Lisp_Object data = CONSP (name) || NILP (name) ? name : list1 (name);
  Lisp_Object errdata = Fcons (errstring, data);

  xsignal (Qfile_notify_error, Fcons (build_string (string), errdata));
}

void
close_file_unwind (int fd)
{
  emacs_close (fd);
}

void
fclose_unwind (void *arg)
{
  FILE *stream = arg;
  fclose (stream);
}

/* Restore point, having saved it as a marker.  */

void
restore_point_unwind (Lisp_Object location)
{
  Fgoto_char (location);
  unchain_marker (XMARKER (location));
}


DEFUN ("find-file-name-handler", Ffind_file_name_handler,
       Sfind_file_name_handler, 2, 2, 0,
       doc: /* Return FILENAME's handler function for OPERATION, if it has one.
Otherwise, return nil.
A file name is handled if one of the regular expressions in
`file-name-handler-alist' matches it.

If OPERATION equals `inhibit-file-name-operation', then ignore
any handlers that are members of `inhibit-file-name-handlers',
but still do run any other handlers.  This lets handlers
use the standard functions without calling themselves recursively.  */)
  (Lisp_Object filename, Lisp_Object operation)
{
  /* This function must not munge the match data.  */
  Lisp_Object chain, inhibited_handlers, result;
  ptrdiff_t pos = -1;

  result = Qnil;
  CHECK_STRING (filename);

  if (EQ (operation, Vinhibit_file_name_operation))
    inhibited_handlers = Vinhibit_file_name_handlers;
  else
    inhibited_handlers = Qnil;

  for (chain = Vfile_name_handler_alist; CONSP (chain);
       chain = XCDR (chain))
    {
      Lisp_Object elt;
      elt = XCAR (chain);
      if (CONSP (elt))
	{
	  Lisp_Object string = XCAR (elt);
	  ptrdiff_t match_pos;
	  Lisp_Object handler = XCDR (elt);
	  Lisp_Object operations = Qnil;

	  if (SYMBOLP (handler))
	    operations = Fget (handler, Qoperations);

	  if (STRINGP (string)
	      && (match_pos = fast_string_match (string, filename)) > pos
	      && (NILP (operations) || ! NILP (Fmemq (operation, operations))))
	    {
	      Lisp_Object tem;

	      handler = XCDR (elt);
	      tem = Fmemq (handler, inhibited_handlers);
	      if (NILP (tem))
		{
		  result = handler;
		  pos = match_pos;
		}
	    }
	}

      maybe_quit ();
    }
  return result;
}

DEFUN ("file-name-directory", Ffile_name_directory, Sfile_name_directory,
       1, 1, 0,
       doc: /* Return the directory component in file name FILENAME.
Return nil if FILENAME does not include a directory.
Otherwise return a directory name.
Given a Unix syntax file name, returns a string ending in slash.  */)
  (Lisp_Object filename)
{
  Lisp_Object handler;

  CHECK_STRING (filename);

  /* If the file name has special constructs in it,
     call the corresponding file handler.  */
  handler = Ffind_file_name_handler (filename, Qfile_name_directory);
  if (!NILP (handler))
    {
      Lisp_Object handled_name = call2 (handler, Qfile_name_directory,
					filename);
      return STRINGP (handled_name) ? handled_name : Qnil;
    }

  char *beg = SSDATA (filename);
  char const *p = beg + SBYTES (filename);

  while (p != beg && !IS_DIRECTORY_SEP (p[-1])
#ifdef DOS_NT
	 /* only recognize drive specifier at the beginning */
	 && !(p[-1] == ':'
	      /* handle the "/:d:foo" and "/:foo" cases correctly  */
	      && ((p == beg + 2 && !IS_DIRECTORY_SEP (*beg))
		  || (p == beg + 4 && IS_DIRECTORY_SEP (*beg))))
#endif
	 ) p--;

  if (p == beg)
    return Qnil;
#ifdef DOS_NT
  /* Expansion of "c:" to drive and default directory.  */
  Lisp_Object tem_fn;
  USE_SAFE_ALLOCA;
  SAFE_ALLOCA_STRING (beg, filename);
  p = beg + (p - SSDATA (filename));

  if (p[-1] == ':')
    {
      /* MAXPATHLEN+1 is guaranteed to be enough space for getdefdir.  */
      char *res = alloca (MAXPATHLEN + 1);
      char *r = res;

      if (p == beg + 4 && IS_DIRECTORY_SEP (*beg) && beg[1] == ':')
	{
	  memcpy (res, beg, 2);
	  beg += 2;
	  r += 2;
	}

      if (getdefdir (c_toupper (*beg) - 'A' + 1, r))
	{
	  size_t l = strlen (res);

	  if (l > 3 || !IS_DIRECTORY_SEP (res[l - 1]))
	    strcat (res, "/");
	  beg = res;
	  p = beg + strlen (beg);
	  dostounix_filename (beg);
	  tem_fn = make_specified_string (beg, -1, p - beg,
					  STRING_MULTIBYTE (filename));
	}
      else
	tem_fn = make_specified_string (beg - 2, -1, p - beg + 2,
					STRING_MULTIBYTE (filename));
    }
  else if (STRING_MULTIBYTE (filename))
    {
      tem_fn = make_specified_string (beg, -1, p - beg, 1);
      dostounix_filename (SSDATA (tem_fn));
#ifdef WINDOWSNT
      if (!NILP (Vw32_downcase_file_names))
	tem_fn = Fdowncase (tem_fn);
#endif
    }
  else
    {
      dostounix_filename (beg);
      tem_fn = make_specified_string (beg, -1, p - beg, 0);
    }
  SAFE_FREE ();
  return tem_fn;
#else  /* DOS_NT */
  return make_specified_string (beg, -1, p - beg, STRING_MULTIBYTE (filename));
#endif	/* DOS_NT */
}

DEFUN ("file-name-nondirectory", Ffile_name_nondirectory,
       Sfile_name_nondirectory, 1, 1, 0,
       doc: /* Return file name FILENAME sans its directory.
For example, in a Unix-syntax file name,
this is everything after the last slash,
or the entire name if it contains no slash.  */)
  (Lisp_Object filename)
{
  register const char *beg, *p, *end;
  Lisp_Object handler;

  CHECK_STRING (filename);

  /* If the file name has special constructs in it,
     call the corresponding file handler.  */
  handler = Ffind_file_name_handler (filename, Qfile_name_nondirectory);
  if (!NILP (handler))
    {
      Lisp_Object handled_name = call2 (handler, Qfile_name_nondirectory,
					filename);
      if (STRINGP (handled_name))
	return handled_name;
      error ("Invalid handler in `file-name-handler-alist'");
    }

  beg = SSDATA (filename);
  end = p = beg + SBYTES (filename);

  while (p != beg && !IS_DIRECTORY_SEP (p[-1])
#ifdef DOS_NT
	 /* only recognize drive specifier at beginning */
	 && !(p[-1] == ':'
	      /* handle the "/:d:foo" case correctly  */
	      && (p == beg + 2 || (p == beg + 4 && IS_DIRECTORY_SEP (*beg))))
#endif
	 )
    p--;

  return make_specified_string (p, -1, end - p, STRING_MULTIBYTE (filename));
}

DEFUN ("unhandled-file-name-directory", Funhandled_file_name_directory,
       Sunhandled_file_name_directory, 1, 1, 0,
       doc: /* Return a directly usable directory name somehow associated with FILENAME.
A `directly usable' directory name is one that may be used without the
intervention of any file handler.
If FILENAME is a directly usable file itself, return
\(file-name-as-directory FILENAME).
If FILENAME refers to a file which is not accessible from a local process,
then this should return nil.
The `call-process' and `start-process' functions use this function to
get a current directory to run processes in.  */)
  (Lisp_Object filename)
{
  Lisp_Object handler;

  /* If the file name has special constructs in it,
     call the corresponding file handler.  */
  handler = Ffind_file_name_handler (filename, Qunhandled_file_name_directory);
  if (!NILP (handler))
    {
      Lisp_Object handled_name = call2 (handler, Qunhandled_file_name_directory,
					filename);
      return STRINGP (handled_name) ? handled_name : Qnil;
    }

  return Ffile_name_as_directory (filename);
}

/* Maximum number of bytes that DST will be longer than SRC
   in file_name_as_directory.  This occurs when SRCLEN == 0.  */
enum { file_name_as_directory_slop = 2 };

/* Convert from file name SRC of length SRCLEN to directory name in
   DST.  MULTIBYTE non-zero means the file name in SRC is a multibyte
   string.  On UNIX, just make sure there is a terminating /.  Return
   the length of DST in bytes.  */

static ptrdiff_t
file_name_as_directory (char *dst, const char *src, ptrdiff_t srclen,
			bool multibyte)
{
  if (srclen == 0)
    {
      dst[0] = '.';
      dst[1] = '/';
      dst[2] = '\0';
      return 2;
    }

  memcpy (dst, src, srclen);
  if (!IS_DIRECTORY_SEP (dst[srclen - 1]))
    dst[srclen++] = DIRECTORY_SEP;
  dst[srclen] = 0;
#ifdef DOS_NT
  dostounix_filename (dst);
#endif
  return srclen;
}

DEFUN ("file-name-as-directory", Ffile_name_as_directory,
       Sfile_name_as_directory, 1, 1, 0,
       doc: /* Return a string representing the file name FILE interpreted as a directory.
This operation exists because a directory is also a file, but its name as
a directory is different from its name as a file.
The result can be used as the value of `default-directory'
or passed as second argument to `expand-file-name'.
For a Unix-syntax file name, just appends a slash unless a trailing slash
is already present.  */)
  (Lisp_Object file)
{
  char *buf;
  ptrdiff_t length;
  Lisp_Object handler, val;
  USE_SAFE_ALLOCA;

  CHECK_STRING (file);

  /* If the file name has special constructs in it,
     call the corresponding file handler.  */
  handler = Ffind_file_name_handler (file, Qfile_name_as_directory);
  if (!NILP (handler))
    {
      Lisp_Object handled_name = call2 (handler, Qfile_name_as_directory,
					file);
      if (STRINGP (handled_name))
	return handled_name;
      error ("Invalid handler in `file-name-handler-alist'");
    }

#ifdef WINDOWSNT
  if (!NILP (Vw32_downcase_file_names))
    file = Fdowncase (file);
#endif
  buf = SAFE_ALLOCA (SBYTES (file) + file_name_as_directory_slop + 1);
  length = file_name_as_directory (buf, SSDATA (file), SBYTES (file),
				   STRING_MULTIBYTE (file));
  val = make_specified_string (buf, -1, length, STRING_MULTIBYTE (file));
  SAFE_FREE ();
  return val;
}

/* Convert from directory name SRC of length SRCLEN to file name in
   DST.  MULTIBYTE non-zero means the file name in SRC is a multibyte
   string.  On UNIX, just make sure there isn't a terminating /.
   Return the length of DST in bytes.  */

static ptrdiff_t
directory_file_name (char *dst, char *src, ptrdiff_t srclen, bool multibyte)
{
  /* In Unix-like systems, just remove any final slashes.  However, if
     they are all slashes, leave "/" and "//" alone, and treat "///"
     and longer as if they were "/".  */
  if (! (srclen == 2 && IS_DIRECTORY_SEP (src[0])))
    while (srclen > 1
#ifdef DOS_NT
	   && !(srclen > 2 && IS_DEVICE_SEP (src[srclen - 2]))
#endif
	   && IS_DIRECTORY_SEP (src[srclen - 1]))
      srclen--;

  memcpy (dst, src, srclen);
  dst[srclen] = 0;
#ifdef DOS_NT
  dostounix_filename (dst);
#endif
  return srclen;
}

DEFUN ("directory-name-p", Fdirectory_name_p, Sdirectory_name_p, 1, 1, 0,
       doc: /* Return non-nil if NAME ends with a directory separator character.  */)
  (Lisp_Object name)
{
  CHECK_STRING (name);
  ptrdiff_t namelen = SBYTES (name);
  unsigned char c = namelen ? SREF (name, namelen - 1) : 0;
  return IS_DIRECTORY_SEP (c) ? Qt : Qnil;
}

/* Return the expansion of NEWNAME, except that if NEWNAME is a
   directory name then return the expansion of FILE's basename under
   NEWNAME.  This resembles how 'cp FILE NEWNAME' works, except that
   it requires NEWNAME to be a directory name (typically, by ending in
   "/").  */

static Lisp_Object
expand_cp_target (Lisp_Object file, Lisp_Object newname)
{
  return (!NILP (Fdirectory_name_p (newname))
	  ? Fexpand_file_name (Ffile_name_nondirectory (file), newname)
	  : Fexpand_file_name (newname, Qnil));
}

DEFUN ("directory-file-name", Fdirectory_file_name, Sdirectory_file_name,
       1, 1, 0,
       doc: /* Returns the file name of the directory named DIRECTORY.
This is the name of the file that holds the data for the directory DIRECTORY.
This operation exists because a directory is also a file, but its name as
a directory is different from its name as a file.
In Unix-syntax, this function just removes the final slash.  */)
  (Lisp_Object directory)
{
  char *buf;
  ptrdiff_t length;
  Lisp_Object handler, val;
  USE_SAFE_ALLOCA;

  CHECK_STRING (directory);

  /* If the file name has special constructs in it,
     call the corresponding file handler.  */
  handler = Ffind_file_name_handler (directory, Qdirectory_file_name);
  if (!NILP (handler))
    {
      Lisp_Object handled_name = call2 (handler, Qdirectory_file_name,
					directory);
      if (STRINGP (handled_name))
	return handled_name;
      error ("Invalid handler in `file-name-handler-alist'");
    }

#ifdef WINDOWSNT
  if (!NILP (Vw32_downcase_file_names))
    directory = Fdowncase (directory);
#endif
  buf = SAFE_ALLOCA (SBYTES (directory) + 1);
  length = directory_file_name (buf, SSDATA (directory), SBYTES (directory),
				STRING_MULTIBYTE (directory));
  val = make_specified_string (buf, -1, length, STRING_MULTIBYTE (directory));
  SAFE_FREE ();
  return val;
}

DEFUN ("make-temp-file-internal", Fmake_temp_file_internal,
       Smake_temp_file_internal, 4, 4, 0,
       doc: /* Generate a new file whose name starts with PREFIX, a string.
Return the name of the generated file.  If DIR-FLAG is zero, do not
create the file, just its name.  Otherwise, if DIR-FLAG is non-nil,
create an empty directory.  The file name should end in SUFFIX.
Do not expand PREFIX; a non-absolute PREFIX is relative to the Emacs
working directory.  If TEXT is a string, insert it into the newly
created file.

Signal an error if the file could not be created.

This function does not grok magic file names.  */)
  (Lisp_Object prefix, Lisp_Object dir_flag, Lisp_Object suffix,
   Lisp_Object text)
{
  CHECK_STRING (prefix);
  CHECK_STRING (suffix);
  Lisp_Object encoded_prefix = ENCODE_FILE (prefix);
  Lisp_Object encoded_suffix = ENCODE_FILE (suffix);
  ptrdiff_t prefix_len = SBYTES (encoded_prefix);
  ptrdiff_t suffix_len = SBYTES (encoded_suffix);
  if (INT_MAX < suffix_len)
    args_out_of_range (prefix, suffix);
  int nX = 6;
  Lisp_Object val = make_uninit_string (prefix_len + nX + suffix_len);
  char *data = SSDATA (val);
  memcpy (data, SSDATA (encoded_prefix), prefix_len);
  memset (data + prefix_len, 'X', nX);
  memcpy (data + prefix_len + nX, SSDATA (encoded_suffix), suffix_len);
  int kind = (NILP (dir_flag) ? GT_FILE
	      : EQ (dir_flag, make_number (0)) ? GT_NOCREATE
	      : GT_DIR);
  int fd = gen_tempname (data, suffix_len, O_BINARY | O_CLOEXEC, kind);
  bool failed = fd < 0;
  if (!failed)
    {
      ptrdiff_t count = SPECPDL_INDEX ();
      record_unwind_protect_int (close_file_unwind, fd);
      val = DECODE_FILE (val);
      if (STRINGP (text) && SBYTES (text) != 0)
	write_region (text, Qnil, val, Qnil, Qnil, Qnil, Qnil, fd);
      failed = NILP (dir_flag) && emacs_close (fd) != 0;
      /* Discard the unwind protect.  */
      specpdl_ptr = specpdl + count;
    }
  if (failed)
    {
      static char const kind_message[][32] =
	{
	  [GT_FILE] = "Creating file with prefix",
	  [GT_DIR] = "Creating directory with prefix",
	  [GT_NOCREATE] = "Creating file name with prefix"
	};
      report_file_error (kind_message[kind], prefix);
    }
  return val;
}


DEFUN ("make-temp-name", Fmake_temp_name, Smake_temp_name, 1, 1, 0,
       doc: /* Generate temporary file name (string) starting with PREFIX (a string).

This function tries to choose a name that has no existing file.
For this to work, PREFIX should be an absolute file name, and PREFIX
and the returned string should both be non-magic.

There is a race condition between calling `make-temp-name' and
later creating the file, which opens all kinds of security holes.
For that reason, you should normally use `make-temp-file' instead.  */)
  (Lisp_Object prefix)
{
  return Fmake_temp_file_internal (prefix, make_number (0),
				   empty_unibyte_string, Qnil);
}

DEFUN ("expand-file-name", Fexpand_file_name, Sexpand_file_name, 1, 2, 0,
       doc: /* Convert filename NAME to absolute, and canonicalize it.
Second arg DEFAULT-DIRECTORY is directory to start with if NAME is relative
\(does not start with slash or tilde); both the directory name and
a directory's file name are accepted.  If DEFAULT-DIRECTORY is nil or
missing, the current buffer's value of `default-directory' is used.
NAME should be a string that is a valid file name for the underlying
filesystem.
File name components that are `.' are removed, and
so are file name components followed by `..', along with the `..' itself;
note that these simplifications are done without checking the resulting
file names in the file system.
Multiple consecutive slashes are collapsed into a single slash,
except at the beginning of the file name when they are significant (e.g.,
UNC file names on MS-Windows.)
An initial `~/' expands to your home directory.
An initial `~USER/' expands to USER's home directory.
See also the function `substitute-in-file-name'.

For technical reasons, this function can return correct but
non-intuitive results for the root directory; for instance,
\(expand-file-name ".." "/") returns "/..".  For this reason, use
\(directory-file-name (file-name-directory dirname)) to traverse a
filesystem tree, not (expand-file-name ".." dirname).  Note: make
sure DIRNAME in this example doesn't end in a slash, unless it's
the root directory.  */)
  (Lisp_Object name, Lisp_Object default_directory)
{
  /* These point to SDATA and need to be careful with string-relocation
     during GC (via DECODE_FILE).  */
  char *nm;
  char *nmlim;
  const char *newdir;
  const char *newdirlim;
  /* This should only point to alloca'd data.  */
  char *target;

  ptrdiff_t tlen;
  struct passwd *pw;
#ifdef DOS_NT
  int drive = 0;
  bool collapse_newdir = true;
  bool is_escaped = 0;
#endif /* DOS_NT */
  ptrdiff_t length, nbytes;
  Lisp_Object handler, result, handled_name;
  bool multibyte;
  Lisp_Object hdir;
  USE_SAFE_ALLOCA;

  CHECK_STRING (name);

  /* If the file name has special constructs in it,
     call the corresponding file handler.  */
  handler = Ffind_file_name_handler (name, Qexpand_file_name);
  if (!NILP (handler))
    {
      handled_name = call3 (handler, Qexpand_file_name,
			    name, default_directory);
      if (STRINGP (handled_name))
	return handled_name;
      error ("Invalid handler in `file-name-handler-alist'");
    }


  /* Use the buffer's default-directory if DEFAULT_DIRECTORY is omitted.  */
  if (NILP (default_directory))
    default_directory = BVAR (current_buffer, directory);
  if (! STRINGP (default_directory))
    {
#ifdef DOS_NT
      /* "/" is not considered a root directory on DOS_NT, so using "/"
	 here causes an infinite recursion in, e.g., the following:

            (let (default-directory)
	      (expand-file-name "a"))

	 To avoid this, we set default_directory to the root of the
	 current drive.  */
      default_directory = build_string (emacs_root_dir ());
#else
      default_directory = build_string ("/");
#endif
    }

  if (!NILP (default_directory))
    {
      handler = Ffind_file_name_handler (default_directory, Qexpand_file_name);
      if (!NILP (handler))
	{
	  handled_name = call3 (handler, Qexpand_file_name,
				name, default_directory);
	  if (STRINGP (handled_name))
	    return handled_name;
	  error ("Invalid handler in `file-name-handler-alist'");
	}
    }

  {
    char *o = SSDATA (default_directory);

    /* Make sure DEFAULT_DIRECTORY is properly expanded.
       It would be better to do this down below where we actually use
       default_directory.  Unfortunately, calling Fexpand_file_name recursively
       could invoke GC, and the strings might be relocated.  This would
       be annoying because we have pointers into strings lying around
       that would need adjusting, and people would add new pointers to
       the code and forget to adjust them, resulting in intermittent bugs.
       Putting this call here avoids all that crud.

       The EQ test avoids infinite recursion.  */
    if (! NILP (default_directory) && !EQ (default_directory, name)
	/* Save time in some common cases - as long as default_directory
	   is not relative, it can be canonicalized with name below (if it
	   is needed at all) without requiring it to be expanded now.  */
#ifdef DOS_NT
	/* Detect MSDOS file names with drive specifiers.  */
	&& ! (IS_DRIVE (o[0]) && IS_DEVICE_SEP (o[1])
	      && IS_DIRECTORY_SEP (o[2]))
	/* Detect escaped file names without drive spec after "/:".
	   These should not be recursively expanded, to avoid
	   including the default directory twice in the expanded
	   result.  */
	&& ! (o[0] == '/' && o[1] == ':')
#ifdef WINDOWSNT
	/* Detect Windows file names in UNC format.  */
	&& ! (IS_DIRECTORY_SEP (o[0]) && IS_DIRECTORY_SEP (o[1]))
#endif
#else /* not DOS_NT */
      /* Detect Unix absolute file names (/... alone is not absolute on
	 DOS or Windows).  */
	&& ! (IS_DIRECTORY_SEP (o[0]))
#endif /* not DOS_NT */
	)
      {
	default_directory = Fexpand_file_name (default_directory, Qnil);
      }
  }
  multibyte = STRING_MULTIBYTE (name);
  if (multibyte != STRING_MULTIBYTE (default_directory))
    {
      if (multibyte)
	{
	  unsigned char *p = SDATA (name);

	  while (*p && ASCII_CHAR_P (*p))
	    p++;
	  if (*p == '\0')
	    {
	      /* NAME is a pure ASCII string, and DEFAULT_DIRECTORY is
		 unibyte.  Do not convert DEFAULT_DIRECTORY to
		 multibyte; instead, convert NAME to a unibyte string,
		 so that the result of this function is also a unibyte
		 string.  This is needed during bootstrapping and
		 dumping, when Emacs cannot decode file names, because
		 the locale environment is not set up.  */
	      name = make_unibyte_string (SSDATA (name), SBYTES (name));
	      multibyte = 0;
	    }
	  else
	    default_directory = string_to_multibyte (default_directory);
	}
      else
	{
	  name = string_to_multibyte (name);
	  multibyte = 1;
	}
    }

#ifdef WINDOWSNT
  if (!NILP (Vw32_downcase_file_names))
    default_directory = Fdowncase (default_directory);
#endif

  /* Make a local copy of NAME to protect it from GC in DECODE_FILE below.  */
  SAFE_ALLOCA_STRING (nm, name);
  nmlim = nm + SBYTES (name);

#ifdef DOS_NT
  /* Note if special escape prefix is present, but remove for now.  */
  if (nm[0] == '/' && nm[1] == ':')
    {
      is_escaped = 1;
      nm += 2;
    }

  /* Find and remove drive specifier if present; this makes nm absolute
     even if the rest of the name appears to be relative.  Only look for
     drive specifier at the beginning.  */
  if (IS_DRIVE (nm[0]) && IS_DEVICE_SEP (nm[1]))
    {
      drive = (unsigned char) nm[0];
      nm += 2;
    }

#ifdef WINDOWSNT
  /* If we see "c://somedir", we want to strip the first slash after the
     colon when stripping the drive letter.  Otherwise, this expands to
     "//somedir".  */
  if (drive && IS_DIRECTORY_SEP (nm[0]) && IS_DIRECTORY_SEP (nm[1]))
    nm++;

  /* Discard any previous drive specifier if nm is now in UNC format.  */
  if (IS_DIRECTORY_SEP (nm[0]) && IS_DIRECTORY_SEP (nm[1])
      && !IS_DIRECTORY_SEP (nm[2]))
    drive = 0;
#endif /* WINDOWSNT */
#endif /* DOS_NT */

  /* If nm is absolute, look for `/./' or `/../' or `//''sequences; if
     none are found, we can probably return right away.  We will avoid
     allocating a new string if name is already fully expanded.  */
  if (
      IS_DIRECTORY_SEP (nm[0])
#ifdef MSDOS
      && drive && !is_escaped
#endif
#ifdef WINDOWSNT
      && (drive || IS_DIRECTORY_SEP (nm[1])) && !is_escaped
#endif
      )
    {
      /* If it turns out that the filename we want to return is just a
	 suffix of FILENAME, we don't need to go through and edit
	 things; we just need to construct a new string using data
	 starting at the middle of FILENAME.  If we set LOSE, that
	 means we've discovered that we can't do that cool trick.  */
      bool lose = 0;
      char *p = nm;

      while (*p)
	{
	  /* Since we know the name is absolute, we can assume that each
	     element starts with a "/".  */

	  /* "." and ".." are hairy.  */
	  if (IS_DIRECTORY_SEP (p[0])
	      && p[1] == '.'
	      && (IS_DIRECTORY_SEP (p[2])
		  || p[2] == 0
		  || (p[2] == '.' && (IS_DIRECTORY_SEP (p[3])
				      || p[3] == 0))))
	    lose = 1;
	  /* Replace multiple slashes with a single one, except
	     leave leading "//" alone.  */
	  else if (IS_DIRECTORY_SEP (p[0])
		   && IS_DIRECTORY_SEP (p[1])
		   && (p != nm || IS_DIRECTORY_SEP (p[2])))
	    lose = 1;
	  p++;
	}
      if (!lose)
	{
#ifdef DOS_NT
	  /* Make sure directories are all separated with /, but
	     avoid allocation of a new string when not required. */
	  dostounix_filename (nm);
#ifdef WINDOWSNT
	  if (IS_DIRECTORY_SEP (nm[1]))
	    {
	      if (strcmp (nm, SSDATA (name)) != 0)
		name = make_specified_string (nm, -1, nmlim - nm, multibyte);
	    }
	  else
#endif
	  /* Drive must be set, so this is okay.  */
	  if (strcmp (nm - 2, SSDATA (name)) != 0)
	    {
	      name = make_specified_string (nm, -1, p - nm, multibyte);
	      char temp[] = { DRIVE_LETTER (drive), ':', 0 };
	      AUTO_STRING_WITH_LEN (drive_prefix, temp, 2);
	      name = concat2 (drive_prefix, name);
	    }
#ifdef WINDOWSNT
	  if (!NILP (Vw32_downcase_file_names))
	    name = Fdowncase (name);
#endif
#else /* not DOS_NT */
	  if (strcmp (nm, SSDATA (name)) != 0)
	    name = make_specified_string (nm, -1, nmlim - nm, multibyte);
#endif /* not DOS_NT */
	  SAFE_FREE ();
	  return name;
	}
    }

  /* At this point, nm might or might not be an absolute file name.  We
     need to expand ~ or ~user if present, otherwise prefix nm with
     default_directory if nm is not absolute, and finally collapse /./
     and /foo/../ sequences.

     We set newdir to be the appropriate prefix if one is needed:
       - the relevant user directory if nm starts with ~ or ~user
       - the specified drive's working dir (DOS/NT only) if nm does not
         start with /
       - the value of default_directory.

     Note that these prefixes are not guaranteed to be absolute (except
     for the working dir of a drive).  Therefore, to ensure we always
     return an absolute name, if the final prefix is not absolute we
     append it to the current working directory.  */

  newdir = newdirlim = 0;

  if (nm[0] == '~'		/* prefix ~ */
#ifdef DOS_NT
    && !is_escaped		/* don't expand ~ in escaped file names */
#endif
      )
    {
      if (IS_DIRECTORY_SEP (nm[1])
	  || nm[1] == 0)	/* ~ by itself */
	{
	  Lisp_Object tem;

	  if (!(newdir = egetenv ("HOME")))
	    newdir = newdirlim = "";
	  nm++;
#ifdef WINDOWSNT
	  if (newdir[0])
	    {
	      char newdir_utf8[MAX_UTF8_PATH];

	      filename_from_ansi (newdir, newdir_utf8);
	      tem = make_unibyte_string (newdir_utf8, strlen (newdir_utf8));
	      newdir = SSDATA (tem);
	    }
	  else
#endif
	    tem = build_string (newdir);
	  newdirlim = newdir + SBYTES (tem);
	  /* `egetenv' may return a unibyte string, which will bite us
	     if we expect the directory to be multibyte.  */
	  if (multibyte && !STRING_MULTIBYTE (tem))
	    {
	      hdir = DECODE_FILE (tem);
	      newdir = SSDATA (hdir);
	      newdirlim = newdir + SBYTES (hdir);
	    }
#ifdef DOS_NT
	  collapse_newdir = false;
#endif
	}
      else			/* ~user/filename */
	{
	  char *o, *p;
	  for (p = nm; *p && !IS_DIRECTORY_SEP (*p); p++)
	    continue;
	  o = SAFE_ALLOCA (p - nm + 1);
	  memcpy (o, nm, p - nm);
	  o[p - nm] = 0;

	  block_input ();
	  pw = getpwnam (o + 1);
	  unblock_input ();
	  if (pw)
	    {
	      Lisp_Object tem;

	      newdir = pw->pw_dir;
	      /* `getpwnam' may return a unibyte string, which will
		 bite us when we expect the directory to be multibyte.  */
	      tem = make_unibyte_string (newdir, strlen (newdir));
	      newdirlim = newdir + SBYTES (tem);
	      if (multibyte && !STRING_MULTIBYTE (tem))
		{
		  hdir = DECODE_FILE (tem);
		  newdir = SSDATA (hdir);
		  newdirlim = newdir + SBYTES (hdir);
		}
	      nm = p;
#ifdef DOS_NT
	      collapse_newdir = false;
#endif
	    }

	  /* If we don't find a user of that name, leave the name
	     unchanged; don't move nm forward to p.  */
	}
    }

#ifdef DOS_NT
  /* On DOS and Windows, nm is absolute if a drive name was specified;
     use the drive's current directory as the prefix if needed.  */
  if (!newdir && drive)
    {
      /* Get default directory if needed to make nm absolute.  */
      char *adir = NULL;
      if (!IS_DIRECTORY_SEP (nm[0]))
	{
	  adir = alloca (MAXPATHLEN + 1);
	  if (!getdefdir (c_toupper (drive) - 'A' + 1, adir))
	    adir = NULL;
	  else if (multibyte)
	    {
	      Lisp_Object tem = build_string (adir);

	      tem = DECODE_FILE (tem);
	      newdirlim = adir + SBYTES (tem);
	      memcpy (adir, SSDATA (tem), SBYTES (tem) + 1);
	    }
	  else
	    newdirlim = adir + strlen (adir);
	}
      if (!adir)
	{
	  /* Either nm starts with /, or drive isn't mounted.  */
	  adir = alloca (4);
	  adir[0] = DRIVE_LETTER (drive);
	  adir[1] = ':';
	  adir[2] = '/';
	  adir[3] = 0;
	  newdirlim = adir + 3;
	}
      newdir = adir;
    }
#endif /* DOS_NT */

  /* Finally, if no prefix has been specified and nm is not absolute,
     then it must be expanded relative to default_directory.  */

  if (1
#ifndef DOS_NT
      /* /... alone is not absolute on DOS and Windows.  */
      && !IS_DIRECTORY_SEP (nm[0])
#endif
#ifdef WINDOWSNT
      && !(IS_DIRECTORY_SEP (nm[0]) && IS_DIRECTORY_SEP (nm[1])
	   && !IS_DIRECTORY_SEP (nm[2]))
#endif
      && !newdir)
    {
      newdir = SSDATA (default_directory);
      newdirlim = newdir + SBYTES (default_directory);
#ifdef DOS_NT
      /* Note if special escape prefix is present, but remove for now.  */
      if (newdir[0] == '/' && newdir[1] == ':')
	{
	  is_escaped = 1;
	  newdir += 2;
	}
#endif
    }

#ifdef DOS_NT
  if (newdir)
    {
      /* First ensure newdir is an absolute name.  */
      if (
	  /* Detect MSDOS file names with drive specifiers.  */
	  ! (IS_DRIVE (newdir[0])
	     && IS_DEVICE_SEP (newdir[1]) && IS_DIRECTORY_SEP (newdir[2]))
#ifdef WINDOWSNT
	  /* Detect Windows file names in UNC format.  */
	  && ! (IS_DIRECTORY_SEP (newdir[0]) && IS_DIRECTORY_SEP (newdir[1])
		&& !IS_DIRECTORY_SEP (newdir[2]))
#endif
	  )
	{
	  /* Effectively, let newdir be (expand-file-name newdir cwd).
	     Because of the admonition against calling expand-file-name
	     when we have pointers into lisp strings, we accomplish this
	     indirectly by prepending newdir to nm if necessary, and using
	     cwd (or the wd of newdir's drive) as the new newdir.  */
	  char *adir;
#ifdef WINDOWSNT
	  const int adir_size = MAX_UTF8_PATH;
#else
	  const int adir_size = MAXPATHLEN + 1;
#endif

	  if (IS_DRIVE (newdir[0]) && IS_DEVICE_SEP (newdir[1]))
	    {
	      drive = (unsigned char) newdir[0];
	      newdir += 2;
	    }
	  if (!IS_DIRECTORY_SEP (nm[0]))
	    {
	      ptrdiff_t nmlen = nmlim - nm;
	      ptrdiff_t newdirlen = newdirlim - newdir;
	      char *tmp = alloca (newdirlen + file_name_as_directory_slop
				  + nmlen + 1);
	      ptrdiff_t dlen = file_name_as_directory (tmp, newdir, newdirlen,
						       multibyte);
	      memcpy (tmp + dlen, nm, nmlen + 1);
	      nm = tmp;
	      nmlim = nm + dlen + nmlen;
	    }
	  adir = alloca (adir_size);
	  if (drive)
	    {
	      if (!getdefdir (c_toupper (drive) - 'A' + 1, adir))
		strcpy (adir, "/");
	    }
	  else
	    getcwd (adir, adir_size);
	  if (multibyte)
	    {
	      Lisp_Object tem = build_string (adir);

	      tem = DECODE_FILE (tem);
	      newdirlim = adir + SBYTES (tem);
	      memcpy (adir, SSDATA (tem), SBYTES (tem) + 1);
	    }
	  else
	    newdirlim = adir + strlen (adir);
	  newdir = adir;
	}

      /* Strip off drive name from prefix, if present.  */
      if (IS_DRIVE (newdir[0]) && IS_DEVICE_SEP (newdir[1]))
	{
	  drive = newdir[0];
	  newdir += 2;
	}

      /* Keep only a prefix from newdir if nm starts with slash
         (//server/share for UNC, nothing otherwise).  */
      if (IS_DIRECTORY_SEP (nm[0]) && collapse_newdir)
	{
#ifdef WINDOWSNT
	  if (IS_DIRECTORY_SEP (newdir[0]) && IS_DIRECTORY_SEP (newdir[1])
	      && !IS_DIRECTORY_SEP (newdir[2]))
	    {
	      char *adir = strcpy (alloca (newdirlim - newdir + 1), newdir);
	      char *p = adir + 2;
	      while (*p && !IS_DIRECTORY_SEP (*p)) p++;
	      p++;
	      while (*p && !IS_DIRECTORY_SEP (*p)) p++;
	      *p = 0;
	      newdir = adir;
	      newdirlim = newdir + strlen (adir);
	    }
	  else
#endif
	    newdir = newdirlim = "";
	}
    }
#endif /* DOS_NT */

  /* Ignore any slash at the end of newdir, unless newdir is
     just "/" or "//".  */
  length = newdirlim - newdir;
  while (length > 1 && IS_DIRECTORY_SEP (newdir[length - 1])
	 && ! (length == 2 && IS_DIRECTORY_SEP (newdir[0])))
    length--;

  /* Now concatenate the directory and name to new space in the stack frame.  */
  tlen = length + file_name_as_directory_slop + (nmlim - nm) + 1;
  eassert (tlen > file_name_as_directory_slop + 1);
#ifdef DOS_NT
  /* Reserve space for drive specifier and escape prefix, since either
     or both may need to be inserted.  (The Microsoft x86 compiler
     produces incorrect code if the following two lines are combined.)  */
  target = alloca (tlen + 4);
  target += 4;
#else  /* not DOS_NT */
  target = SAFE_ALLOCA (tlen);
#endif /* not DOS_NT */
  *target = 0;
  nbytes = 0;

  if (newdir)
    {
      if (nm[0] == 0 || IS_DIRECTORY_SEP (nm[0]))
	{
#ifdef DOS_NT
	  /* If newdir is effectively "C:/", then the drive letter will have
	     been stripped and newdir will be "/".  Concatenating with an
	     absolute directory in nm produces "//", which will then be
	     incorrectly treated as a network share.  Ignore newdir in
	     this case (keeping the drive letter).  */
	  if (!(drive && nm[0] && IS_DIRECTORY_SEP (newdir[0])
		&& newdir[1] == '\0'))
#endif
	    {
	      memcpy (target, newdir, length);
	      target[length] = 0;
	      nbytes = length;
	    }
	}
      else
	nbytes = file_name_as_directory (target, newdir, length, multibyte);
    }

  memcpy (target + nbytes, nm, nmlim - nm + 1);

  /* Now canonicalize by removing `//', `/.' and `/foo/..' if they
     appear.  */
  {
    char *p = target;
    char *o = target;

    while (*p)
      {
	if (!IS_DIRECTORY_SEP (*p))
	  {
	    *o++ = *p++;
	  }
	else if (p[1] == '.'
		 && (IS_DIRECTORY_SEP (p[2])
		     || p[2] == 0))
	  {
	    /* If "/." is the entire filename, keep the "/".  Otherwise,
	       just delete the whole "/.".  */
	    if (o == target && p[2] == '\0')
	      *o++ = *p;
	    p += 2;
	  }
	else if (p[1] == '.' && p[2] == '.'
		 /* `/../' is the "superroot" on certain file systems.
		    Turned off on DOS_NT systems because they have no
		    "superroot" and because this causes us to produce
		    file names like "d:/../foo" which fail file-related
		    functions of the underlying OS.  (To reproduce, try a
		    long series of "../../" in default_directory, longer
		    than the number of levels from the root.)  */
#ifndef DOS_NT
		 && o != target
#endif
		 && (IS_DIRECTORY_SEP (p[3]) || p[3] == 0))
	  {
#ifdef WINDOWSNT
	    char *prev_o = o;
#endif
	    while (o != target && (--o, !IS_DIRECTORY_SEP (*o)))
	      continue;
#ifdef WINDOWSNT
	    /* Don't go below server level in UNC filenames.  */
	    if (o == target + 1 && IS_DIRECTORY_SEP (*o)
		&& IS_DIRECTORY_SEP (*target))
	      o = prev_o;
	    else
#endif
	    /* Keep initial / only if this is the whole name.  */
	    if (o == target && IS_ANY_SEP (*o) && p[3] == 0)
	      ++o;
	    p += 3;
	  }
	else if (IS_DIRECTORY_SEP (p[1])
		 && (p != target || IS_DIRECTORY_SEP (p[2])))
	  /* Collapse multiple "/", except leave leading "//" alone.  */
	  p++;
	else
	  {
	    *o++ = *p++;
	  }
      }

#ifdef DOS_NT
    /* At last, set drive name.  */
#ifdef WINDOWSNT
    /* Except for network file name.  */
    if (!(IS_DIRECTORY_SEP (target[0]) && IS_DIRECTORY_SEP (target[1])))
#endif /* WINDOWSNT */
      {
	if (!drive) emacs_abort ();
	target -= 2;
	target[0] = DRIVE_LETTER (drive);
	target[1] = ':';
      }
    /* Reinsert the escape prefix if required.  */
    if (is_escaped)
      {
	target -= 2;
	target[0] = '/';
	target[1] = ':';
      }
    result = make_specified_string (target, -1, o - target, multibyte);
    dostounix_filename (SSDATA (result));
#ifdef WINDOWSNT
    if (!NILP (Vw32_downcase_file_names))
      result = Fdowncase (result);
#endif
#else  /* !DOS_NT */
    result = make_specified_string (target, -1, o - target, multibyte);
#endif /* !DOS_NT */
  }

  /* Again look to see if the file name has special constructs in it
     and perhaps call the corresponding file handler.  This is needed
     for filenames such as "/foo/../user@host:/bar/../baz".  Expanding
     the ".." component gives us "/user@host:/bar/../baz" which needs
     to be expanded again.  */
  handler = Ffind_file_name_handler (result, Qexpand_file_name);
  if (!NILP (handler))
    {
      handled_name = call3 (handler, Qexpand_file_name,
			    result, default_directory);
      if (! STRINGP (handled_name))
	error ("Invalid handler in `file-name-handler-alist'");
      result = handled_name;
    }

  SAFE_FREE ();
  return result;
}

#if 0
/* PLEASE DO NOT DELETE THIS COMMENTED-OUT VERSION!
   This is the old version of expand-file-name, before it was thoroughly
   rewritten for Emacs 10.31.  We leave this version here commented-out,
   because the code is very complex and likely to have subtle bugs.  If
   bugs _are_ found, it might be of interest to look at the old code and
   see what did it do in the relevant situation.

   Don't remove this code: it's true that it will be accessible
   from the repository, but a few years from deletion, people will
   forget it is there.  */

/* Changed this DEFUN to a DEAFUN, so as not to confuse `make-docfile'.  */
DEAFUN ("expand-file-name", Fexpand_file_name, Sexpand_file_name, 1, 2, 0,
  "Convert FILENAME to absolute, and canonicalize it.\n\
Second arg DEFAULT is directory to start with if FILENAME is relative\n\
\(does not start with slash); if DEFAULT is nil or missing,\n\
the current buffer's value of default-directory is used.\n\
Filenames containing `.' or `..' as components are simplified;\n\
initial `~/' expands to your home directory.\n\
See also the function `substitute-in-file-name'.")
     (name, defalt)
     Lisp_Object name, defalt;
{
  unsigned char *nm;

  register unsigned char *newdir, *p, *o;
  ptrdiff_t tlen;
  unsigned char *target;
  struct passwd *pw;

  CHECK_STRING (name);
  nm = SDATA (name);

  /* If nm is absolute, flush ...// and detect /./ and /../.
     If no /./ or /../ we can return right away.  */
  if (nm[0] == '/')
    {
      bool lose = 0;
      p = nm;
      while (*p)
	{
	  if (p[0] == '/' && p[1] == '/')
	    nm = p + 1;
	  if (p[0] == '/' && p[1] == '~')
	    nm = p + 1, lose = 1;
	  if (p[0] == '/' && p[1] == '.'
	      && (p[2] == '/' || p[2] == 0
		  || (p[2] == '.' && (p[3] == '/' || p[3] == 0))))
	    lose = 1;
	  p++;
	}
      if (!lose)
	{
	  if (nm == SDATA (name))
	    return name;
	  return build_string (nm);
	}
    }

  /* Now determine directory to start with and put it in NEWDIR.  */

  newdir = 0;

  if (nm[0] == '~')             /* prefix ~ */
    if (nm[1] == '/' || nm[1] == 0)/* ~/filename */
      {
	if (!(newdir = (unsigned char *) egetenv ("HOME")))
	  newdir = (unsigned char *) "";
	nm++;
      }
    else  /* ~user/filename */
      {
	/* Get past ~ to user.  */
	unsigned char *user = nm + 1;
	/* Find end of name.  */
	unsigned char *ptr = (unsigned char *) strchr (user, '/');
	ptrdiff_t len = ptr ? ptr - user : strlen (user);
	/* Copy the user name into temp storage.  */
	o = alloca (len + 1);
	memcpy (o, user, len);
	o[len] = 0;

	/* Look up the user name.  */
	block_input ();
	pw = (struct passwd *) getpwnam (o + 1);
	unblock_input ();
	if (!pw)
	  error ("\"%s\" isn't a registered user", o + 1);

	newdir = (unsigned char *) pw->pw_dir;

	/* Discard the user name from NM.  */
	nm += len;
      }

  if (nm[0] != '/' && !newdir)
    {
      if (NILP (defalt))
	defalt = current_buffer->directory;
      CHECK_STRING (defalt);
      newdir = SDATA (defalt);
    }

  /* Now concatenate the directory and name to new space in the stack frame.  */

  tlen = (newdir ? strlen (newdir) + 1 : 0) + strlen (nm) + 1;
  target = alloca (tlen);
  *target = 0;

  if (newdir)
    {
      if (nm[0] == 0 || nm[0] == '/')
	strcpy (target, newdir);
      else
      file_name_as_directory (target, newdir);
    }

  strcat (target, nm);

  /* Now canonicalize by removing /. and /foo/.. if they appear.  */

  p = target;
  o = target;

  while (*p)
    {
      if (*p != '/')
	{
	  *o++ = *p++;
	}
      else if (!strncmp (p, "//", 2)
	       )
	{
	  o = target;
	  p++;
	}
      else if (p[0] == '/' && p[1] == '.'
	       && (p[2] == '/' || p[2] == 0))
	p += 2;
      else if (!strncmp (p, "/..", 3)
	       /* `/../' is the "superroot" on certain file systems.  */
	       && o != target
	       && (p[3] == '/' || p[3] == 0))
	{
	  while (o != target && *--o != '/')
	    ;
	  if (o == target && *o == '/')
	    ++o;
	  p += 3;
	}
      else
	{
	  *o++ = *p++;
	}
    }

  return make_string (target, o - target);
}
#endif

/* If /~ or // appears, discard everything through first slash.  */
static bool
file_name_absolute_p (const char *filename)
{
  return
    (IS_DIRECTORY_SEP (*filename) || *filename == '~'
#ifdef DOS_NT
     || (IS_DRIVE (*filename) && IS_DEVICE_SEP (filename[1])
	 && IS_DIRECTORY_SEP (filename[2]))
#endif
     );
}

static char *
search_embedded_absfilename (char *nm, char *endp)
{
  char *p, *s;

  for (p = nm + 1; p < endp; p++)
    {
      if (IS_DIRECTORY_SEP (p[-1])
	  && file_name_absolute_p (p)
#if defined (WINDOWSNT) || defined (CYGWIN)
	  /* // at start of file name is meaningful in Apollo,
	     WindowsNT and Cygwin systems.  */
	  && !(IS_DIRECTORY_SEP (p[0]) && p - 1 == nm)
#endif /* not (WINDOWSNT || CYGWIN) */
	  )
	{
	  for (s = p; *s && !IS_DIRECTORY_SEP (*s); s++);
	  if (p[0] == '~' && s > p + 1)	/* We've got "/~something/".  */
	    {
	      USE_SAFE_ALLOCA;
	      char *o = SAFE_ALLOCA (s - p + 1);
	      struct passwd *pw;
	      memcpy (o, p, s - p);
	      o [s - p] = 0;

	      /* If we have ~user and `user' exists, discard
		 everything up to ~.  But if `user' does not exist, leave
		 ~user alone, it might be a literal file name.  */
	      block_input ();
	      pw = getpwnam (o + 1);
	      unblock_input ();
	      SAFE_FREE ();
	      if (pw)
		return p;
	    }
	  else
	    return p;
	}
    }
  return NULL;
}

DEFUN ("substitute-in-file-name", Fsubstitute_in_file_name,
       Ssubstitute_in_file_name, 1, 1, 0,
       doc: /* Substitute environment variables referred to in FILENAME.
`$FOO' where FOO is an environment variable name means to substitute
the value of that variable.  The variable name should be terminated
with a character not a letter, digit or underscore; otherwise, enclose
the entire variable name in braces.

If `/~' appears, all of FILENAME through that `/' is discarded.
If `//' appears, everything up to and including the first of
those `/' is discarded.  */)
  (Lisp_Object filename)
{
  char *nm, *p, *x, *endp;
  bool substituted = false;
  bool multibyte;
  char *xnm;
  Lisp_Object handler;

  CHECK_STRING (filename);

  multibyte = STRING_MULTIBYTE (filename);

  /* If the file name has special constructs in it,
     call the corresponding file handler.  */
  handler = Ffind_file_name_handler (filename, Qsubstitute_in_file_name);
  if (!NILP (handler))
    {
      Lisp_Object handled_name = call2 (handler, Qsubstitute_in_file_name,
					filename);
      if (STRINGP (handled_name))
	return handled_name;
      error ("Invalid handler in `file-name-handler-alist'");
    }

  /* Always work on a copy of the string, in case GC happens during
     decode of environment variables, causing the original Lisp_String
     data to be relocated.  */
  USE_SAFE_ALLOCA;
  SAFE_ALLOCA_STRING (nm, filename);

#ifdef DOS_NT
  dostounix_filename (nm);
  substituted = (memcmp (nm, SDATA (filename), SBYTES (filename)) != 0);
#endif
  endp = nm + SBYTES (filename);

  /* If /~ or // appears, discard everything through first slash.  */
  p = search_embedded_absfilename (nm, endp);
  if (p)
    /* Start over with the new string, so we check the file-name-handler
       again.  Important with filenames like "/home/foo//:/hello///there"
       which would substitute to "/:/hello///there" rather than "/there".  */
    {
      Lisp_Object result
	= (Fsubstitute_in_file_name
	   (make_specified_string (p, -1, endp - p, multibyte)));
      SAFE_FREE ();
      return result;
    }

  /* See if any variables are substituted into the string.  */

  if (!NILP (Ffboundp (Qsubstitute_env_in_file_name)))
    {
      Lisp_Object name
	= (!substituted ? filename
	   : make_specified_string (nm, -1, endp - nm, multibyte));
      Lisp_Object tmp = call1 (Qsubstitute_env_in_file_name, name);
      CHECK_STRING (tmp);
      if (!EQ (tmp, name))
	substituted = true;
      filename = tmp;
    }

  if (!substituted)
    {
#ifdef WINDOWSNT
      if (!NILP (Vw32_downcase_file_names))
	filename = Fdowncase (filename);
#endif
      SAFE_FREE ();
      return filename;
    }

  xnm = SSDATA (filename);
  x = xnm + SBYTES (filename);

  /* If /~ or // appears, discard everything through first slash.  */
  while ((p = search_embedded_absfilename (xnm, x)) != NULL)
    /* This time we do not start over because we've already expanded envvars
       and replaced $$ with $.  Maybe we should start over as well, but we'd
       need to quote some $ to $$ first.  */
    xnm = p;

#ifdef WINDOWSNT
  if (!NILP (Vw32_downcase_file_names))
    {
      Lisp_Object xname = make_specified_string (xnm, -1, x - xnm, multibyte);

      filename = Fdowncase (xname);
    }
  else
#endif
  if (xnm != SSDATA (filename))
    filename = make_specified_string (xnm, -1, x - xnm, multibyte);
  SAFE_FREE ();
  return filename;
}

/* A slightly faster and more convenient way to get
   (directory-file-name (expand-file-name FOO)).  */

Lisp_Object
expand_and_dir_to_file (Lisp_Object filename)
{
  Lisp_Object absname = Fexpand_file_name (filename, Qnil);

  /* Remove final slash, if any (unless this is the root dir).
     stat behaves differently depending!  */
  if (SCHARS (absname) > 1
      && IS_DIRECTORY_SEP (SREF (absname, SBYTES (absname) - 1))
      && !IS_DEVICE_SEP (SREF (absname, SBYTES (absname) - 2)))
    /* We cannot take shortcuts; they might be wrong for magic file names.  */
    absname = Fdirectory_file_name (absname);
  return absname;
}

/* Signal an error if the file ABSNAME already exists.
   If KNOWN_TO_EXIST, the file is known to exist.
   QUERYSTRING is a name for the action that is being considered
   to alter the file.
   If INTERACTIVE, ask the user whether to proceed,
   and bypass the error if the user says to go ahead.
   If QUICK, ask for y or n, not yes or no.  */

static void
barf_or_query_if_file_exists (Lisp_Object absname, bool known_to_exist,
			      const char *querystring, bool interactive,
			      bool quick)
{
  Lisp_Object tem, encoded_filename;
  struct stat statbuf;

  encoded_filename = ENCODE_FILE (absname);

  if (! known_to_exist && lstat (SSDATA (encoded_filename), &statbuf) == 0)
    {
      if (S_ISDIR (statbuf.st_mode))
	xsignal2 (Qfile_error,
		  build_string ("File is a directory"), absname);
      known_to_exist = true;
    }

  if (known_to_exist)
    {
      if (! interactive)
	xsignal2 (Qfile_already_exists,
		  build_string ("File already exists"), absname);
      AUTO_STRING (format, "File %s already exists; %s anyway? ");
      tem = CALLN (Fformat, format, absname, build_string (querystring));
      if (quick)
	tem = call1 (intern ("y-or-n-p"), tem);
      else
	tem = do_yes_or_no_p (tem);
      if (NILP (tem))
	xsignal2 (Qfile_already_exists,
		  build_string ("File already exists"), absname);
    }
}

#ifndef WINDOWSNT
/* Copy data to DEST from SOURCE if possible.  Return true if OK.  */
static bool
clone_file (int dest, int source)
{
#ifdef FICLONE
  return ioctl (dest, FICLONE, source) == 0;
#endif
  return false;
}
#endif

DEFUN ("copy-file", Fcopy_file, Scopy_file, 2, 6,
       "fCopy file: \nGCopy %s to file: \np\nP",
       doc: /* Copy FILE to NEWNAME.  Both args must be strings.
If NEWNAME is a directory name, copy FILE to a like-named file under
NEWNAME.  For NEWNAME to be recognized as a directory name, it should
end in a slash.

This function always sets the file modes of the output file to match
the input file.

The optional third argument OK-IF-ALREADY-EXISTS specifies what to do
if file NEWNAME already exists.  If OK-IF-ALREADY-EXISTS is nil,
signal a `file-already-exists' error without overwriting.  If
OK-IF-ALREADY-EXISTS is an integer, request confirmation from the user
about overwriting; this is what happens in interactive use with M-x.
Any other value for OK-IF-ALREADY-EXISTS means to overwrite the
existing file.

Fourth arg KEEP-TIME non-nil means give the output file the same
last-modified time as the old one.  (This works on only some systems.)

A prefix arg makes KEEP-TIME non-nil.

If PRESERVE-UID-GID is non-nil, try to transfer the uid and gid of
FILE to NEWNAME.

If PRESERVE-PERMISSIONS is non-nil, copy permissions of FILE to NEWNAME;
this includes the file modes, along with ACL entries and SELinux
context if present.  Otherwise, if NEWNAME is created its file
permission bits are those of FILE, masked by the default file
permissions.  */)
  (Lisp_Object file, Lisp_Object newname, Lisp_Object ok_if_already_exists,
   Lisp_Object keep_time, Lisp_Object preserve_uid_gid,
   Lisp_Object preserve_permissions)
{
  Lisp_Object handler;
  ptrdiff_t count = SPECPDL_INDEX ();
  Lisp_Object encoded_file, encoded_newname;
#if HAVE_LIBSELINUX
  security_context_t con;
  int conlength = 0;
#endif
#ifdef WINDOWSNT
  int result;
#else
  bool already_exists = false;
  mode_t new_mask;
  int ifd, ofd;
  struct stat st;
#endif

  file = Fexpand_file_name (file, Qnil);
  newname = expand_cp_target (file, newname);

  /* If the input file name has special constructs in it,
     call the corresponding file handler.  */
  handler = Ffind_file_name_handler (file, Qcopy_file);
  /* Likewise for output file name.  */
  if (NILP (handler))
    handler = Ffind_file_name_handler (newname, Qcopy_file);
  if (!NILP (handler))
    return call7 (handler, Qcopy_file, file, newname,
		  ok_if_already_exists, keep_time, preserve_uid_gid,
		  preserve_permissions);

  encoded_file = ENCODE_FILE (file);
  encoded_newname = ENCODE_FILE (newname);

#ifdef WINDOWSNT
  if (NILP (ok_if_already_exists)
      || INTEGERP (ok_if_already_exists))
    barf_or_query_if_file_exists (newname, false, "copy to it",
				  INTEGERP (ok_if_already_exists), false);

  result = w32_copy_file (SSDATA (encoded_file), SSDATA (encoded_newname),
			  !NILP (keep_time), !NILP (preserve_uid_gid),
			  !NILP (preserve_permissions));
  switch (result)
    {
    case -1:
      report_file_error ("Copying file", list2 (file, newname));
    case -2:
      report_file_error ("Copying permissions from", file);
    case -3:
      xsignal2 (Qfile_date_error,
		build_string ("Resetting file times"), newname);
    case -4:
      report_file_error ("Copying permissions to", newname);
    }
#else /* not WINDOWSNT */
  ifd = emacs_open (SSDATA (encoded_file), O_RDONLY, 0);

  if (ifd < 0)
    report_file_error ("Opening input file", file);

  record_unwind_protect_int (close_file_unwind, ifd);

  if (fstat (ifd, &st) != 0)
    report_file_error ("Input file status", file);

  if (!NILP (preserve_permissions))
    {
#if HAVE_LIBSELINUX
      if (is_selinux_enabled ())
	{
	  conlength = fgetfilecon (ifd, &con);
	  if (conlength == -1)
	    report_file_error ("Doing fgetfilecon", file);
	}
#endif
    }

  /* We can copy only regular files.  */
  if (!S_ISREG (st.st_mode))
    report_file_errno ("Non-regular file", file,
		       S_ISDIR (st.st_mode) ? EISDIR : EINVAL);

#ifndef MSDOS
  new_mask = st.st_mode & (!NILP (preserve_uid_gid) ? 0700 : 0777);
#else
  new_mask = S_IREAD | S_IWRITE;
#endif

  ofd = emacs_open (SSDATA (encoded_newname), O_WRONLY | O_CREAT | O_EXCL,
		    new_mask);
  if (ofd < 0 && errno == EEXIST)
    {
      if (NILP (ok_if_already_exists) || INTEGERP (ok_if_already_exists))
	barf_or_query_if_file_exists (newname, true, "copy to it",
				      INTEGERP (ok_if_already_exists), false);
      already_exists = true;
      ofd = emacs_open (SSDATA (encoded_newname), O_WRONLY, 0);
    }
  if (ofd < 0)
    report_file_error ("Opening output file", newname);

  record_unwind_protect_int (close_file_unwind, ofd);

  off_t oldsize = 0, newsize;

  if (already_exists)
    {
      struct stat out_st;
      if (fstat (ofd, &out_st) != 0)
	report_file_error ("Output file status", newname);
      if (st.st_dev == out_st.st_dev && st.st_ino == out_st.st_ino)
	report_file_errno ("Input and output files are the same",
			   list2 (file, newname), 0);
      if (S_ISREG (out_st.st_mode))
	oldsize = out_st.st_size;
    }

  maybe_quit ();

  if (clone_file (ofd, ifd))
    newsize = st.st_size;
  else
    {
      char buf[MAX_ALLOCA];
      ptrdiff_t n;
      for (newsize = 0; 0 < (n = emacs_read_quit (ifd, buf, sizeof buf));
	   newsize += n)
	if (emacs_write_quit (ofd, buf, n) != n)
	  report_file_error ("Write error", newname);
      if (n < 0)
	report_file_error ("Read error", file);
    }

  /* Truncate any existing output file after writing the data.  This
     is more likely to work than truncation before writing, if the
     file system is out of space or the user is over disk quota.  */
  if (newsize < oldsize && ftruncate (ofd, newsize) != 0)
    report_file_error ("Truncating output file", newname);

#ifndef MSDOS
  /* Preserve the original file permissions, and if requested, also its
     owner and group.  */
  {
    mode_t preserved_permissions = st.st_mode & 07777;
    mode_t default_permissions = st.st_mode & 0777 & ~realmask;
    if (!NILP (preserve_uid_gid))
      {
	/* Attempt to change owner and group.  If that doesn't work
	   attempt to change just the group, as that is sometimes allowed.
	   Adjust the mode mask to eliminate setuid or setgid bits
	   or group permissions bits that are inappropriate if the
	   owner or group are wrong.  */
	if (fchown (ofd, st.st_uid, st.st_gid) != 0)
	  {
	    if (fchown (ofd, -1, st.st_gid) == 0)
	      preserved_permissions &= ~04000;
	    else
	      {
		preserved_permissions &= ~06000;

		/* Copy the other bits to the group bits, since the
		   group is wrong.  */
		preserved_permissions &= ~070;
		preserved_permissions |= (preserved_permissions & 7) << 3;
		default_permissions &= ~070;
		default_permissions |= (default_permissions & 7) << 3;
	      }
	  }
      }

    switch (!NILP (preserve_permissions)
	    ? qcopy_acl (SSDATA (encoded_file), ifd,
			 SSDATA (encoded_newname), ofd,
			 preserved_permissions)
	    : (already_exists
	       || (new_mask & ~realmask) == default_permissions)
	    ? 0
	    : fchmod (ofd, default_permissions))
      {
      case -2: report_file_error ("Copying permissions from", file);
      case -1: report_file_error ("Copying permissions to", newname);
      }
  }
#endif	/* not MSDOS */

#if HAVE_LIBSELINUX
  if (conlength > 0)
    {
      /* Set the modified context back to the file.  */
      bool fail = fsetfilecon (ofd, con) != 0;
      /* See https://debbugs.gnu.org/11245 for ENOTSUP.  */
      if (fail && errno != ENOTSUP)
	report_file_error ("Doing fsetfilecon", newname);

      freecon (con);
    }
#endif

  if (!NILP (keep_time))
    {
      struct timespec atime = get_stat_atime (&st);
      struct timespec mtime = get_stat_mtime (&st);
      if (set_file_times (ofd, SSDATA (encoded_newname), atime, mtime) != 0)
	xsignal2 (Qfile_date_error,
		  build_string ("Cannot set file date"), newname);
    }

  if (emacs_close (ofd) < 0)
    report_file_error ("Write error", newname);

  emacs_close (ifd);

#ifdef MSDOS
  /* In DJGPP v2.0 and later, fstat usually returns true file mode bits,
     and if it can't, it tells so.  Otherwise, under MSDOS we usually
     get only the READ bit, which will make the copied file read-only,
     so it's better not to chmod at all.  */
  if ((_djstat_flags & _STFAIL_WRITEBIT) == 0)
    chmod (SDATA (encoded_newname), st.st_mode & 07777);
#endif /* MSDOS */
#endif /* not WINDOWSNT */

  /* Discard the unwind protects.  */
  specpdl_ptr = specpdl + count;

  return Qnil;
}

DEFUN ("make-directory-internal", Fmake_directory_internal,
       Smake_directory_internal, 1, 1, 0,
       doc: /* Create a new directory named DIRECTORY.  */)
  (Lisp_Object directory)
{
  const char *dir;
  Lisp_Object handler;
  Lisp_Object encoded_dir;

  CHECK_STRING (directory);
  directory = Fexpand_file_name (directory, Qnil);

  handler = Ffind_file_name_handler (directory, Qmake_directory_internal);
  if (!NILP (handler))
    return call2 (handler, Qmake_directory_internal, directory);

  encoded_dir = ENCODE_FILE (directory);

  dir = SSDATA (encoded_dir);

  if (mkdir (dir, 0777 & ~auto_saving_dir_umask) != 0)
    report_file_error ("Creating directory", directory);

  return Qnil;
}

DEFUN ("delete-directory-internal", Fdelete_directory_internal,
       Sdelete_directory_internal, 1, 1, 0,
       doc: /* Delete the directory named DIRECTORY.  Does not follow symlinks.  */)
  (Lisp_Object directory)
{
  const char *dir;
  Lisp_Object encoded_dir;

  CHECK_STRING (directory);
  directory = Fdirectory_file_name (Fexpand_file_name (directory, Qnil));
  encoded_dir = ENCODE_FILE (directory);
  dir = SSDATA (encoded_dir);

  if (rmdir (dir) != 0)
    report_file_error ("Removing directory", directory);

  return Qnil;
}

DEFUN ("delete-file", Fdelete_file, Sdelete_file, 1, 2,
       "(list (read-file-name \
                (if (and delete-by-moving-to-trash (null current-prefix-arg)) \
                    \"Move file to trash: \" \"Delete file: \") \
                nil default-directory (confirm-nonexistent-file-or-buffer)) \
              (null current-prefix-arg))",
       doc: /* Delete file named FILENAME.  If it is a symlink, remove the symlink.
If file has multiple names, it continues to exist with the other names.
TRASH non-nil means to trash the file instead of deleting, provided
`delete-by-moving-to-trash' is non-nil.

When called interactively, TRASH is t if no prefix argument is given.
With a prefix argument, TRASH is nil.  */)
  (Lisp_Object filename, Lisp_Object trash)
{
  Lisp_Object handler;
  Lisp_Object encoded_file;

  if (!NILP (Ffile_directory_p (filename))
      && NILP (Ffile_symlink_p (filename)))
    xsignal2 (Qfile_error,
	      build_string ("Removing old name: is a directory"),
	      filename);
  filename = Fexpand_file_name (filename, Qnil);

  handler = Ffind_file_name_handler (filename, Qdelete_file);
  if (!NILP (handler))
    return call3 (handler, Qdelete_file, filename, trash);

  if (delete_by_moving_to_trash && !NILP (trash))
    return call1 (Qmove_file_to_trash, filename);

  encoded_file = ENCODE_FILE (filename);

  if (unlink (SSDATA (encoded_file)) != 0 && errno != ENOENT)
    report_file_error ("Removing old name", filename);
  return Qnil;
}

static Lisp_Object
internal_delete_file_1 (Lisp_Object ignore)
{
  return Qt;
}

/* Delete file FILENAME, returning true if successful.
   This ignores `delete-by-moving-to-trash'.  */

bool
internal_delete_file (Lisp_Object filename)
{
  Lisp_Object tem;

  tem = internal_condition_case_2 (Fdelete_file, filename, Qnil,
				   Qt, internal_delete_file_1);
  return NILP (tem);
}

/* Filesystems are case-sensitive on all supported systems except
   MS-Windows, MS-DOS, Cygwin, and Mac OS X.  They are always
   case-insensitive on the first two, but they may or may not be
   case-insensitive on Cygwin and OS X.  The following function
   attempts to provide a runtime test on those two systems.  If the
   test is not conclusive, we assume case-insensitivity on Cygwin and
   case-sensitivity on Mac OS X.

   FIXME: Mounted filesystems on Posix hosts, like Samba shares or
   NFS-mounted Windows volumes, might be case-insensitive.  Can we
   detect this?  */

static bool
file_name_case_insensitive_p (const char *filename)
{
  /* Use pathconf with _PC_CASE_INSENSITIVE or _PC_CASE_SENSITIVE if
     those flags are available.  As of this writing (2017-05-20),
     Cygwin is the only platform known to support the former (starting
     with Cygwin-2.6.1), and macOS is the only platform known to
     support the latter.  */

#ifdef _PC_CASE_INSENSITIVE
  int res = pathconf (filename, _PC_CASE_INSENSITIVE);
  if (res >= 0)
    return res > 0;
#elif defined _PC_CASE_SENSITIVE
  int res = pathconf (filename, _PC_CASE_SENSITIVE);
  if (res >= 0)
    return res == 0;
#endif

#if defined CYGWIN || defined DOS_NT
  return true;
#else
  return false;
#endif
}

DEFUN ("file-name-case-insensitive-p", Ffile_name_case_insensitive_p,
       Sfile_name_case_insensitive_p, 1, 1, 0,
       doc: /* Return t if file FILENAME is on a case-insensitive filesystem.
The arg must be a string.  */)
  (Lisp_Object filename)
{
  Lisp_Object handler;

  CHECK_STRING (filename);
  filename = Fexpand_file_name (filename, Qnil);

  /* If the file name has special constructs in it,
     call the corresponding file handler.  */
  handler = Ffind_file_name_handler (filename, Qfile_name_case_insensitive_p);
  if (!NILP (handler))
    return call2 (handler, Qfile_name_case_insensitive_p, filename);

  filename = ENCODE_FILE (filename);
  return file_name_case_insensitive_p (SSDATA (filename)) ? Qt : Qnil;
}

DEFUN ("rename-file", Frename_file, Srename_file, 2, 3,
       "fRename file: \nGRename %s to file: \np",
       doc: /* Rename FILE as NEWNAME.  Both args must be strings.
If file has names other than FILE, it continues to have those names.
If NEWNAME is a directory name, rename FILE to a like-named file under
NEWNAME.  For NEWNAME to be recognized as a directory name, it should
end in a slash.

Signal a `file-already-exists' error if a file NEWNAME already exists
unless optional third argument OK-IF-ALREADY-EXISTS is non-nil.
An integer third arg means request confirmation if NEWNAME already exists.
This is what happens in interactive use with M-x.  */)
  (Lisp_Object file, Lisp_Object newname, Lisp_Object ok_if_already_exists)
{
  Lisp_Object handler;
  Lisp_Object encoded_file, encoded_newname;

  file = Fexpand_file_name (file, Qnil);

  /* If the filesystem is case-insensitive and the file names are
     identical but for case, treat it as a change-case request, and do
     not worry whether NEWNAME exists or whether it is a directory, as
     it is already another name for FILE.  */
  bool case_only_rename = false;
#if defined CYGWIN || defined DOS_NT
  if (!NILP (Ffile_name_case_insensitive_p (file)))
    {
      newname = Fexpand_file_name (newname, Qnil);
      case_only_rename = !NILP (Fstring_equal (Fdowncase (file),
					       Fdowncase (newname)));
    }
#endif

  if (!case_only_rename)
    newname = expand_cp_target (Fdirectory_file_name (file), newname);

  /* If the file name has special constructs in it,
     call the corresponding file handler.  */
  handler = Ffind_file_name_handler (file, Qrename_file);
  if (NILP (handler))
    handler = Ffind_file_name_handler (newname, Qrename_file);
  if (!NILP (handler))
    return call4 (handler, Qrename_file,
		  file, newname, ok_if_already_exists);

  encoded_file = ENCODE_FILE (file);
  encoded_newname = ENCODE_FILE (newname);

  bool plain_rename = (case_only_rename
		       || (!NILP (ok_if_already_exists)
			   && !INTEGERP (ok_if_already_exists)));
  int rename_errno UNINIT;
  if (!plain_rename)
    {
      if (renameat_noreplace (AT_FDCWD, SSDATA (encoded_file),
			      AT_FDCWD, SSDATA (encoded_newname))
	  == 0)
	return Qnil;

      rename_errno = errno;
      switch (rename_errno)
	{
	case EEXIST: case EINVAL: case ENOSYS:
#if ENOSYS != ENOTSUP
	case ENOTSUP:
#endif
	  barf_or_query_if_file_exists (newname, rename_errno == EEXIST,
					"rename to it",
					INTEGERP (ok_if_already_exists),
					false);
	  plain_rename = true;
	  break;
	}
    }

  if (plain_rename)
    {
      if (rename (SSDATA (encoded_file), SSDATA (encoded_newname)) == 0)
	return Qnil;
      rename_errno = errno;
      /* Don't prompt again.  */
      ok_if_already_exists = Qt;
    }
  else if (!NILP (ok_if_already_exists))
    ok_if_already_exists = Qt;

  if (rename_errno != EXDEV)
    report_file_errno ("Renaming", list2 (file, newname), rename_errno);

  struct stat file_st;
  bool dirp = !NILP (Fdirectory_name_p (file));
  if (!dirp)
    {
      if (lstat (SSDATA (encoded_file), &file_st) != 0)
	report_file_error ("Renaming", list2 (file, newname));
      dirp = S_ISDIR (file_st.st_mode) != 0;
    }
  if (dirp)
    call4 (Qcopy_directory, file, newname, Qt, Qnil);
  else
    {
      Lisp_Object symlink_target
	= (S_ISLNK (file_st.st_mode)
	   ? emacs_readlinkat (AT_FDCWD, SSDATA (encoded_file))
	   : Qnil);
      if (!NILP (symlink_target))
	Fmake_symbolic_link (symlink_target, newname, ok_if_already_exists);
      else
	Fcopy_file (file, newname, ok_if_already_exists, Qt, Qt, Qt);
    }

  ptrdiff_t count = SPECPDL_INDEX ();
  specbind (Qdelete_by_moving_to_trash, Qnil);
  if (dirp)
    call2 (Qdelete_directory, file, Qt);
  else
    Fdelete_file (file, Qnil);
  return unbind_to (count, Qnil);
}

DEFUN ("add-name-to-file", Fadd_name_to_file, Sadd_name_to_file, 2, 3,
       "fAdd name to file: \nGName to add to %s: \np",
       doc: /* Give FILE additional name NEWNAME.  Both args must be strings.
If NEWNAME is a directory name, give FILE a like-named new name under
NEWNAME.

Signal a `file-already-exists' error if a file NEWNAME already exists
unless optional third argument OK-IF-ALREADY-EXISTS is non-nil.
An integer third arg means request confirmation if NEWNAME already exists.
This is what happens in interactive use with M-x.  */)
  (Lisp_Object file, Lisp_Object newname, Lisp_Object ok_if_already_exists)
{
  Lisp_Object handler;
  Lisp_Object encoded_file, encoded_newname;

  file = Fexpand_file_name (file, Qnil);
  newname = expand_cp_target (file, newname);

  /* If the file name has special constructs in it,
     call the corresponding file handler.  */
  handler = Ffind_file_name_handler (file, Qadd_name_to_file);
  if (!NILP (handler))
    return call4 (handler, Qadd_name_to_file, file,
		  newname, ok_if_already_exists);

  /* If the new name has special constructs in it,
     call the corresponding file handler.  */
  handler = Ffind_file_name_handler (newname, Qadd_name_to_file);
  if (!NILP (handler))
    return call4 (handler, Qadd_name_to_file, file,
		  newname, ok_if_already_exists);

  encoded_file = ENCODE_FILE (file);
  encoded_newname = ENCODE_FILE (newname);

  if (link (SSDATA (encoded_file), SSDATA (encoded_newname)) == 0)
    return Qnil;

  if (errno == EEXIST)
    {
      if (NILP (ok_if_already_exists)
	  || INTEGERP (ok_if_already_exists))
	barf_or_query_if_file_exists (newname, true, "make it a new name",
				      INTEGERP (ok_if_already_exists), false);
      unlink (SSDATA (newname));
      if (link (SSDATA (encoded_file), SSDATA (encoded_newname)) == 0)
	return Qnil;
    }

  report_file_error ("Adding new name", list2 (file, newname));
}

DEFUN ("make-symbolic-link", Fmake_symbolic_link, Smake_symbolic_link, 2, 3,
       "FMake symbolic link to file: \nGMake symbolic link to file %s: \np",
       doc: /* Make a symbolic link to TARGET, named NEWNAME.
If NEWNAME is a directory name, make a like-named symbolic link under
NEWNAME.

Signal a `file-already-exists' error if a file NEWNAME already exists
unless optional third argument OK-IF-ALREADY-EXISTS is non-nil.
An integer third arg means request confirmation if NEWNAME already
exists, and expand leading "~" or strip leading "/:" in TARGET.
This happens for interactive use with M-x.  */)
  (Lisp_Object target, Lisp_Object linkname, Lisp_Object ok_if_already_exists)
{
  Lisp_Object handler;
  Lisp_Object encoded_target, encoded_linkname;

  CHECK_STRING (target);
  if (INTEGERP (ok_if_already_exists))
    {
      if (SREF (target, 0) == '~')
	target = Fexpand_file_name (target, Qnil);
      else if (SREF (target, 0) == '/' && SREF (target, 1) == ':')
	target = Fsubstring_no_properties (target, make_number (2), Qnil);
    }
  linkname = expand_cp_target (target, linkname);

  /* If the new link name has special constructs in it,
     call the corresponding file handler.  */
  handler = Ffind_file_name_handler (linkname, Qmake_symbolic_link);
  if (!NILP (handler))
    return call4 (handler, Qmake_symbolic_link, target,
		  linkname, ok_if_already_exists);

  encoded_target = ENCODE_FILE (target);
  encoded_linkname = ENCODE_FILE (linkname);

  if (symlink (SSDATA (encoded_target), SSDATA (encoded_linkname)) == 0)
    return Qnil;

  if (errno == ENOSYS)
    xsignal1 (Qfile_error,
	      build_string ("Symbolic links are not supported"));

  if (errno == EEXIST)
    {
      if (NILP (ok_if_already_exists)
	  || INTEGERP (ok_if_already_exists))
	barf_or_query_if_file_exists (linkname, true, "make it a link",
				      INTEGERP (ok_if_already_exists), false);
      unlink (SSDATA (encoded_linkname));
      if (symlink (SSDATA (encoded_target), SSDATA (encoded_linkname)) == 0)
	return Qnil;
    }

  report_file_error ("Making symbolic link", list2 (target, linkname));
}


DEFUN ("file-name-absolute-p", Ffile_name_absolute_p, Sfile_name_absolute_p,
       1, 1, 0,
       doc: /* Return t if FILENAME is an absolute file name or starts with `~'.
On Unix, absolute file names start with `/'.  */)
  (Lisp_Object filename)
{
  CHECK_STRING (filename);
  return file_name_absolute_p (SSDATA (filename)) ? Qt : Qnil;
}

DEFUN ("file-exists-p", Ffile_exists_p, Sfile_exists_p, 1, 1, 0,
       doc: /* Return t if file FILENAME exists (whether or not you can read it.)
See also `file-readable-p' and `file-attributes'.
This returns nil for a symlink to a nonexistent file.
Use `file-symlink-p' to test for such links.  */)
  (Lisp_Object filename)
{
  Lisp_Object absname;
  Lisp_Object handler;

  CHECK_STRING (filename);
  absname = Fexpand_file_name (filename, Qnil);

  /* If the file name has special constructs in it,
     call the corresponding file handler.  */
  handler = Ffind_file_name_handler (absname, Qfile_exists_p);
  if (!NILP (handler))
    {
      Lisp_Object result = call2 (handler, Qfile_exists_p, absname);
      errno = 0;
      return result;
    }

  absname = ENCODE_FILE (absname);

  return check_existing (SSDATA (absname)) ? Qt : Qnil;
}

DEFUN ("file-executable-p", Ffile_executable_p, Sfile_executable_p, 1, 1, 0,
       doc: /* Return t if FILENAME can be executed by you.
For a directory, this means you can access files in that directory.
\(It is generally better to use `file-accessible-directory-p' for that
purpose, though.)  */)
  (Lisp_Object filename)
{
  Lisp_Object absname;
  Lisp_Object handler;

  CHECK_STRING (filename);
  absname = Fexpand_file_name (filename, Qnil);

  /* If the file name has special constructs in it,
     call the corresponding file handler.  */
  handler = Ffind_file_name_handler (absname, Qfile_executable_p);
  if (!NILP (handler))
    return call2 (handler, Qfile_executable_p, absname);

  absname = ENCODE_FILE (absname);

  return (check_executable (SSDATA (absname)) ? Qt : Qnil);
}

DEFUN ("file-readable-p", Ffile_readable_p, Sfile_readable_p, 1, 1, 0,
       doc: /* Return t if file FILENAME exists and you can read it.
See also `file-exists-p' and `file-attributes'.  */)
  (Lisp_Object filename)
{
  Lisp_Object absname;
  Lisp_Object handler;

  CHECK_STRING (filename);
  absname = Fexpand_file_name (filename, Qnil);

  /* If the file name has special constructs in it,
     call the corresponding file handler.  */
  handler = Ffind_file_name_handler (absname, Qfile_readable_p);
  if (!NILP (handler))
    return call2 (handler, Qfile_readable_p, absname);

  absname = ENCODE_FILE (absname);
  return (faccessat (AT_FDCWD, SSDATA (absname), R_OK, AT_EACCESS) == 0
	  ? Qt : Qnil);
}

DEFUN ("file-writable-p", Ffile_writable_p, Sfile_writable_p, 1, 1, 0,
       doc: /* Return t if file FILENAME can be written or created by you.  */)
  (Lisp_Object filename)
{
  Lisp_Object absname, dir, encoded;
  Lisp_Object handler;

  CHECK_STRING (filename);
  absname = Fexpand_file_name (filename, Qnil);

  /* If the file name has special constructs in it,
     call the corresponding file handler.  */
  handler = Ffind_file_name_handler (absname, Qfile_writable_p);
  if (!NILP (handler))
    return call2 (handler, Qfile_writable_p, absname);

  encoded = ENCODE_FILE (absname);
  if (check_writable (SSDATA (encoded), W_OK))
    return Qt;
  if (errno != ENOENT)
    return Qnil;

  dir = Ffile_name_directory (absname);
  eassert (!NILP (dir));
#ifdef MSDOS
  dir = Fdirectory_file_name (dir);
#endif /* MSDOS */

  dir = ENCODE_FILE (dir);
#ifdef WINDOWSNT
  /* The read-only attribute of the parent directory doesn't affect
     whether a file or directory can be created within it.  Some day we
     should check ACLs though, which do affect this.  */
  return file_directory_p (SSDATA (dir)) ? Qt : Qnil;
#else
  return check_writable (SSDATA (dir), W_OK | X_OK) ? Qt : Qnil;
#endif
}

DEFUN ("access-file", Faccess_file, Saccess_file, 2, 2, 0,
       doc: /* Access file FILENAME, and get an error if that does not work.
The second argument STRING is prepended to the error message.
If there is no error, returns nil.  */)
  (Lisp_Object filename, Lisp_Object string)
{
  Lisp_Object handler, encoded_filename, absname;

  CHECK_STRING (filename);
  absname = Fexpand_file_name (filename, Qnil);

  CHECK_STRING (string);

  /* If the file name has special constructs in it,
     call the corresponding file handler.  */
  handler = Ffind_file_name_handler (absname, Qaccess_file);
  if (!NILP (handler))
    return call3 (handler, Qaccess_file, absname, string);

  encoded_filename = ENCODE_FILE (absname);

  if (faccessat (AT_FDCWD, SSDATA (encoded_filename), R_OK, AT_EACCESS) != 0)
    report_file_error (SSDATA (string), filename);

  return Qnil;
}

/* Relative to directory FD, return the symbolic link value of FILENAME.
   On failure, return nil.  */
Lisp_Object
emacs_readlinkat (int fd, char const *filename)
{
  static struct allocator const emacs_norealloc_allocator =
    { xmalloc, NULL, xfree, memory_full };
  Lisp_Object val;
  char readlink_buf[1024];
  char *buf = careadlinkat (fd, filename, readlink_buf, sizeof readlink_buf,
			    &emacs_norealloc_allocator, readlinkat);
  if (!buf)
    return Qnil;

  val = build_unibyte_string (buf);
  if (buf != readlink_buf)
    xfree (buf);
  val = DECODE_FILE (val);
  return val;
}

DEFUN ("file-symlink-p", Ffile_symlink_p, Sfile_symlink_p, 1, 1, 0,
       doc: /* Return non-nil if file FILENAME is the name of a symbolic link.
The value is the link target, as a string.
Otherwise it returns nil.

This function does not check whether the link target exists.  */)
  (Lisp_Object filename)
{
  Lisp_Object handler;

  CHECK_STRING (filename);
  filename = Fexpand_file_name (filename, Qnil);

  /* If the file name has special constructs in it,
     call the corresponding file handler.  */
  handler = Ffind_file_name_handler (filename, Qfile_symlink_p);
  if (!NILP (handler))
    return call2 (handler, Qfile_symlink_p, filename);

  filename = ENCODE_FILE (filename);

  return emacs_readlinkat (AT_FDCWD, SSDATA (filename));
}

DEFUN ("file-directory-p", Ffile_directory_p, Sfile_directory_p, 1, 1, 0,
       doc: /* Return t if FILENAME names an existing directory.
Symbolic links to directories count as directories.
See `file-symlink-p' to distinguish symlinks.  */)
  (Lisp_Object filename)
{
  Lisp_Object absname = expand_and_dir_to_file (filename);

  /* If the file name has special constructs in it,
     call the corresponding file handler.  */
  Lisp_Object handler = Ffind_file_name_handler (absname, Qfile_directory_p);
  if (!NILP (handler))
    return call2 (handler, Qfile_directory_p, absname);

  absname = ENCODE_FILE (absname);

  return file_directory_p (SSDATA (absname)) ? Qt : Qnil;
}

/* Return true if FILE is a directory or a symlink to a directory.  */
bool
file_directory_p (char const *file)
{
#ifdef WINDOWSNT
  /* This is cheaper than 'stat'.  */
  return faccessat (AT_FDCWD, file, D_OK, AT_EACCESS) == 0;
#else
  struct stat st;
  return stat (file, &st) == 0 && S_ISDIR (st.st_mode);
#endif
}

DEFUN ("file-accessible-directory-p", Ffile_accessible_directory_p,
       Sfile_accessible_directory_p, 1, 1, 0,
       doc: /* Return t if FILENAME names a directory you can open.
For the value to be t, FILENAME must specify the name of a directory
as a file, and the directory must allow you to open files in it.  In
order to use a directory as a buffer's current directory, this
predicate must return true.  A directory name spec may be given
instead; then the value is t if the directory so specified exists and
really is a readable and searchable directory.  */)
  (Lisp_Object filename)
{
  Lisp_Object absname;
  Lisp_Object handler;

  CHECK_STRING (filename);
  absname = Fexpand_file_name (filename, Qnil);

  /* If the file name has special constructs in it,
     call the corresponding file handler.  */
  handler = Ffind_file_name_handler (absname, Qfile_accessible_directory_p);
  if (!NILP (handler))
    {
      Lisp_Object r = call2 (handler, Qfile_accessible_directory_p, absname);

      /* Set errno in case the handler failed.  EACCES might be a lie
	 (e.g., the directory might not exist, or be a regular file),
	 but at least it does TRT in the "usual" case of an existing
	 directory that is not accessible by the current user, and
	 avoids reporting "Success" for a failed operation.  Perhaps
	 someday we can fix this in a better way, by improving
	 file-accessible-directory-p's API; see Bug#25419.  */
      if (!EQ (r, Qt))
	errno = EACCES;

      return r;
    }

  absname = ENCODE_FILE (absname);
  return file_accessible_directory_p (absname) ? Qt : Qnil;
}

/* If FILE is a searchable directory or a symlink to a
   searchable directory, return true.  Otherwise return
   false and set errno to an error number.  */
bool
file_accessible_directory_p (Lisp_Object file)
{
#ifdef DOS_NT
# ifdef WINDOWSNT
  /* We need a special-purpose test because (a) NTFS security data is
     not reflected in Posix-style mode bits, and (b) the trick with
     accessing "DIR/.", used below on Posix hosts, doesn't work on
     Windows, because "DIR/." is normalized to just "DIR" before
     hitting the disk.  */
  return (SBYTES (file) == 0
	  || w32_accessible_directory_p (SSDATA (file), SBYTES (file)));
# else	/* MSDOS */
  return file_directory_p (SSDATA (file));
# endif	 /* MSDOS */
#else	 /* !DOS_NT */
  /* On POSIXish platforms, use just one system call; this avoids a
     race and is typically faster.  */
  const char *data = SSDATA (file);
  ptrdiff_t len = SBYTES (file);
  char const *dir;
  bool ok;
  int saved_errno;
  USE_SAFE_ALLOCA;

  /* Normally a file "FOO" is an accessible directory if "FOO/." exists.
     There are three exceptions: "", "/", and "//".  Leave "" alone,
     as it's invalid.  Append only "." to the other two exceptions as
     "/" and "//" are distinct on some platforms, whereas "/", "///",
     "////", etc. are all equivalent.  */
  if (! len)
    dir = data;
  else
    {
      /* Just check for trailing '/' when deciding whether to append '/'.
	 That's simpler than testing the two special cases "/" and "//",
	 and it's a safe optimization here.  */
      char *buf = SAFE_ALLOCA (len + 3);
      memcpy (buf, data, len);
      strcpy (buf + len, &"/."[data[len - 1] == '/']);
      dir = buf;
    }

  ok = check_existing (dir);
  saved_errno = errno;
  SAFE_FREE ();
  errno = saved_errno;
  return ok;
#endif	/* !DOS_NT */
}

DEFUN ("file-regular-p", Ffile_regular_p, Sfile_regular_p, 1, 1, 0,
       doc: /* Return t if FILENAME names a regular file.
This is the sort of file that holds an ordinary stream of data bytes.
Symbolic links to regular files count as regular files.
See `file-symlink-p' to distinguish symlinks.  */)
  (Lisp_Object filename)
{
  struct stat st;
  Lisp_Object absname = expand_and_dir_to_file (filename);

  /* If the file name has special constructs in it,
     call the corresponding file handler.  */
  Lisp_Object handler = Ffind_file_name_handler (absname, Qfile_regular_p);
  if (!NILP (handler))
    return call2 (handler, Qfile_regular_p, absname);

  absname = ENCODE_FILE (absname);

#ifdef WINDOWSNT
  {
    int result;
    Lisp_Object tem = Vw32_get_true_file_attributes;

    /* Tell stat to use expensive method to get accurate info.  */
    Vw32_get_true_file_attributes = Qt;
    result = stat (SSDATA (absname), &st);
    Vw32_get_true_file_attributes = tem;

    if (result < 0)
      return Qnil;
    return S_ISREG (st.st_mode) ? Qt : Qnil;
  }
#else
  if (stat (SSDATA (absname), &st) < 0)
    return Qnil;
  return S_ISREG (st.st_mode) ? Qt : Qnil;
#endif
}

DEFUN ("file-selinux-context", Ffile_selinux_context,
       Sfile_selinux_context, 1, 1, 0,
       doc: /* Return SELinux context of file named FILENAME.
The return value is a list (USER ROLE TYPE RANGE), where the list
elements are strings naming the user, role, type, and range of the
file's SELinux security context.

Return (nil nil nil nil) if the file is nonexistent or inaccessible,
or if SELinux is disabled, or if Emacs lacks SELinux support.  */)
  (Lisp_Object filename)
{
  Lisp_Object user = Qnil, role = Qnil, type = Qnil, range = Qnil;
  Lisp_Object absname = expand_and_dir_to_file (filename);

  /* If the file name has special constructs in it,
     call the corresponding file handler.  */
  Lisp_Object handler = Ffind_file_name_handler (absname,
						 Qfile_selinux_context);
  if (!NILP (handler))
    return call2 (handler, Qfile_selinux_context, absname);

  absname = ENCODE_FILE (absname);

#if HAVE_LIBSELINUX
  if (is_selinux_enabled ())
    {
      security_context_t con;
      int conlength = lgetfilecon (SSDATA (absname), &con);
      if (conlength > 0)
	{
	  context_t context = context_new (con);
	  if (context_user_get (context))
	    user = build_string (context_user_get (context));
	  if (context_role_get (context))
	    role = build_string (context_role_get (context));
	  if (context_type_get (context))
	    type = build_string (context_type_get (context));
	  if (context_range_get (context))
	    range = build_string (context_range_get (context));
	  context_free (context);
	  freecon (con);
	}
    }
#endif

  return list4 (user, role, type, range);
}

DEFUN ("set-file-selinux-context", Fset_file_selinux_context,
       Sset_file_selinux_context, 2, 2, 0,
       doc: /* Set SELinux context of file named FILENAME to CONTEXT.
CONTEXT should be a list (USER ROLE TYPE RANGE), where the list
elements are strings naming the components of a SELinux context.

Value is t if setting of SELinux context was successful, nil otherwise.

This function does nothing and returns nil if SELinux is disabled,
or if Emacs was not compiled with SELinux support.  */)
  (Lisp_Object filename, Lisp_Object context)
{
  Lisp_Object absname;
  Lisp_Object handler;
#if HAVE_LIBSELINUX
  Lisp_Object encoded_absname;
  Lisp_Object user = CAR_SAFE (context);
  Lisp_Object role = CAR_SAFE (CDR_SAFE (context));
  Lisp_Object type = CAR_SAFE (CDR_SAFE (CDR_SAFE (context)));
  Lisp_Object range = CAR_SAFE (CDR_SAFE (CDR_SAFE (CDR_SAFE (context))));
  security_context_t con;
  bool fail;
  int conlength;
  context_t parsed_con;
#endif

  absname = Fexpand_file_name (filename, BVAR (current_buffer, directory));

  /* If the file name has special constructs in it,
     call the corresponding file handler.  */
  handler = Ffind_file_name_handler (absname, Qset_file_selinux_context);
  if (!NILP (handler))
    return call3 (handler, Qset_file_selinux_context, absname, context);

#if HAVE_LIBSELINUX
  if (is_selinux_enabled ())
    {
      /* Get current file context. */
      encoded_absname = ENCODE_FILE (absname);
      conlength = lgetfilecon (SSDATA (encoded_absname), &con);
      if (conlength > 0)
	{
	  parsed_con = context_new (con);
	  /* Change the parts defined in the parameter.*/
	  if (STRINGP (user))
	    {
	      if (context_user_set (parsed_con, SSDATA (user)))
		error ("Doing context_user_set");
	    }
	  if (STRINGP (role))
	    {
	      if (context_role_set (parsed_con, SSDATA (role)))
		error ("Doing context_role_set");
	    }
	  if (STRINGP (type))
	    {
	      if (context_type_set (parsed_con, SSDATA (type)))
		error ("Doing context_type_set");
	    }
	  if (STRINGP (range))
	    {
	      if (context_range_set (parsed_con, SSDATA (range)))
		error ("Doing context_range_set");
	    }

	  /* Set the modified context back to the file.  */
	  fail = (lsetfilecon (SSDATA (encoded_absname),
			       context_str (parsed_con))
		  != 0);
          /* See https://debbugs.gnu.org/11245 for ENOTSUP.  */
	  if (fail && errno != ENOTSUP)
	    report_file_error ("Doing lsetfilecon", absname);

	  context_free (parsed_con);
	  freecon (con);
	  return fail ? Qnil : Qt;
	}
      else
	report_file_error ("Doing lgetfilecon", absname);
    }
#endif

  return Qnil;
}

DEFUN ("file-acl", Ffile_acl, Sfile_acl, 1, 1, 0,
       doc: /* Return ACL entries of file named FILENAME.
The entries are returned in a format suitable for use in `set-file-acl'
but is otherwise undocumented and subject to change.
Return nil if file does not exist or is not accessible, or if Emacs
was unable to determine the ACL entries.  */)
  (Lisp_Object filename)
{
  Lisp_Object acl_string = Qnil;

#if USE_ACL
  Lisp_Object absname = expand_and_dir_to_file (filename);

  /* If the file name has special constructs in it,
     call the corresponding file handler.  */
  Lisp_Object handler = Ffind_file_name_handler (absname, Qfile_acl);
  if (!NILP (handler))
    return call2 (handler, Qfile_acl, absname);

# ifdef HAVE_ACL_SET_FILE
  absname = ENCODE_FILE (absname);

#  ifndef HAVE_ACL_TYPE_EXTENDED
  acl_type_t ACL_TYPE_EXTENDED = ACL_TYPE_ACCESS;
#  endif
  acl_t acl = acl_get_file (SSDATA (absname), ACL_TYPE_EXTENDED);
  if (acl == NULL)
    return Qnil;

  char *str = acl_to_text (acl, NULL);
  if (str == NULL)
    {
      acl_free (acl);
      return Qnil;
    }

  acl_string = build_string (str);
  acl_free (str);
  acl_free (acl);
# endif
#endif

  return acl_string;
}

DEFUN ("set-file-acl", Fset_file_acl, Sset_file_acl,
       2, 2, 0,
       doc: /* Set ACL of file named FILENAME to ACL-STRING.
ACL-STRING should contain the textual representation of the ACL
entries in a format suitable for the platform.

Value is t if setting of ACL was successful, nil otherwise.

Setting ACL for local files requires Emacs to be built with ACL
support.  */)
  (Lisp_Object filename, Lisp_Object acl_string)
{
#if USE_ACL
  Lisp_Object absname;
  Lisp_Object handler;
# ifdef HAVE_ACL_SET_FILE
  Lisp_Object encoded_absname;
  acl_t acl;
  bool fail;
# endif

  absname = Fexpand_file_name (filename, BVAR (current_buffer, directory));

  /* If the file name has special constructs in it,
     call the corresponding file handler.  */
  handler = Ffind_file_name_handler (absname, Qset_file_acl);
  if (!NILP (handler))
    return call3 (handler, Qset_file_acl, absname, acl_string);

# ifdef HAVE_ACL_SET_FILE
  if (STRINGP (acl_string))
    {
      acl = acl_from_text (SSDATA (acl_string));
      if (acl == NULL)
	{
	  if (acl_errno_valid (errno))
	    report_file_error ("Converting ACL", absname);
	  return Qnil;
	}

      encoded_absname = ENCODE_FILE (absname);

      fail = (acl_set_file (SSDATA (encoded_absname), ACL_TYPE_ACCESS,
			    acl)
	      != 0);
      if (fail && acl_errno_valid (errno))
	report_file_error ("Setting ACL", absname);

      acl_free (acl);
      return fail ? Qnil : Qt;
    }
# endif
#endif

  return Qnil;
}

DEFUN ("file-modes", Ffile_modes, Sfile_modes, 1, 1, 0,
       doc: /* Return mode bits of file named FILENAME, as an integer.
Return nil, if file does not exist or is not accessible.  */)
  (Lisp_Object filename)
{
  struct stat st;
  Lisp_Object absname = expand_and_dir_to_file (filename);

  /* If the file name has special constructs in it,
     call the corresponding file handler.  */
  Lisp_Object handler = Ffind_file_name_handler (absname, Qfile_modes);
  if (!NILP (handler))
    return call2 (handler, Qfile_modes, absname);

  absname = ENCODE_FILE (absname);

  if (stat (SSDATA (absname), &st) < 0)
    return Qnil;

  return make_number (st.st_mode & 07777);
}

DEFUN ("set-file-modes", Fset_file_modes, Sset_file_modes, 2, 2,
       "(let ((file (read-file-name \"File: \")))			\
	  (list file (read-file-modes nil file)))",
       doc: /* Set mode bits of file named FILENAME to MODE (an integer).
Only the 12 low bits of MODE are used.

Interactively, mode bits are read by `read-file-modes', which accepts
symbolic notation, like the `chmod' command from GNU Coreutils.  */)
  (Lisp_Object filename, Lisp_Object mode)
{
  Lisp_Object absname, encoded_absname;
  Lisp_Object handler;

  absname = Fexpand_file_name (filename, BVAR (current_buffer, directory));
  CHECK_NUMBER (mode);

  /* If the file name has special constructs in it,
     call the corresponding file handler.  */
  handler = Ffind_file_name_handler (absname, Qset_file_modes);
  if (!NILP (handler))
    return call3 (handler, Qset_file_modes, absname, mode);

  encoded_absname = ENCODE_FILE (absname);

  if (chmod (SSDATA (encoded_absname), XINT (mode) & 07777) < 0)
    report_file_error ("Doing chmod", absname);

  return Qnil;
}

DEFUN ("set-default-file-modes", Fset_default_file_modes, Sset_default_file_modes, 1, 1, 0,
       doc: /* Set the file permission bits for newly created files.
The argument MODE should be an integer; only the low 9 bits are used.
On Posix hosts, this setting is inherited by subprocesses.

This function works by setting the Emacs's file mode creation mask.
Each bit that is set in the mask means that the corresponding bit
in the permissions of newly created files will be disabled.

Note that when `write-region' creates a file, it resets the
execute bit, even if the mask set by this function allows that bit
by having the corresponding bit in the mask reset.  */)
  (Lisp_Object mode)
{
  mode_t oldrealmask, oldumask, newumask;
  CHECK_NUMBER (mode);
  oldrealmask = realmask;
  newumask = ~ XINT (mode) & 0777;

  block_input ();
  realmask = newumask;
  oldumask = umask (newumask);
  unblock_input ();

  eassert (oldumask == oldrealmask);
  return Qnil;
}

DEFUN ("default-file-modes", Fdefault_file_modes, Sdefault_file_modes, 0, 0, 0,
       doc: /* Return the default file protection for created files.
The value is an integer.  */)
  (void)
{
  Lisp_Object value;
  XSETINT (value, (~ realmask) & 0777);
  return value;
}


DEFUN ("set-file-times", Fset_file_times, Sset_file_times, 1, 2, 0,
       doc: /* Set times of file FILENAME to TIMESTAMP.
Set both access and modification times.
Return t on success, else nil.
Use the current time if TIMESTAMP is nil.  TIMESTAMP is in the format of
`current-time'. */)
  (Lisp_Object filename, Lisp_Object timestamp)
{
  Lisp_Object absname, encoded_absname;
  Lisp_Object handler;
  struct timespec t = lisp_time_argument (timestamp);

  absname = Fexpand_file_name (filename, BVAR (current_buffer, directory));

  /* If the file name has special constructs in it,
     call the corresponding file handler.  */
  handler = Ffind_file_name_handler (absname, Qset_file_times);
  if (!NILP (handler))
    return call3 (handler, Qset_file_times, absname, timestamp);

  encoded_absname = ENCODE_FILE (absname);

  {
    if (set_file_times (-1, SSDATA (encoded_absname), t, t) != 0)
      {
#ifdef MSDOS
        /* Setting times on a directory always fails.  */
        if (file_directory_p (SSDATA (encoded_absname)))
          return Qnil;
#endif
        report_file_error ("Setting file times", absname);
      }
  }

  return Qt;
}

#ifdef HAVE_SYNC
DEFUN ("unix-sync", Funix_sync, Sunix_sync, 0, 0, "",
       doc: /* Tell Unix to finish all pending disk updates.  */)
  (void)
{
  sync ();
  return Qnil;
}

#endif /* HAVE_SYNC */

DEFUN ("file-newer-than-file-p", Ffile_newer_than_file_p, Sfile_newer_than_file_p, 2, 2, 0,
       doc: /* Return t if file FILE1 is newer than file FILE2.
If FILE1 does not exist, the answer is nil;
otherwise, if FILE2 does not exist, the answer is t.  */)
  (Lisp_Object file1, Lisp_Object file2)
{
  struct stat st1, st2;

  CHECK_STRING (file1);
  CHECK_STRING (file2);

  Lisp_Object absname1 = expand_and_dir_to_file (file1);
  Lisp_Object absname2 = expand_and_dir_to_file (file2);

  /* If the file name has special constructs in it,
     call the corresponding file handler.  */
  Lisp_Object handler = Ffind_file_name_handler (absname1,
						 Qfile_newer_than_file_p);
  if (NILP (handler))
    handler = Ffind_file_name_handler (absname2, Qfile_newer_than_file_p);
  if (!NILP (handler))
    return call3 (handler, Qfile_newer_than_file_p, absname1, absname2);

  absname1 = ENCODE_FILE (absname1);
  absname2 = ENCODE_FILE (absname2);

  if (stat (SSDATA (absname1), &st1) < 0)
    return Qnil;

  if (stat (SSDATA (absname2), &st2) < 0)
    return Qt;

  return (timespec_cmp (get_stat_mtime (&st2), get_stat_mtime (&st1)) < 0
	  ? Qt : Qnil);
}

enum { READ_BUF_SIZE = MAX_ALLOCA };

/* This function is called after Lisp functions to decide a coding
   system are called, or when they cause an error.  Before they are
   called, the current buffer is set unibyte and it contains only a
   newly inserted text (thus the buffer was empty before the
   insertion).

   The functions may set markers, overlays, text properties, or even
   alter the buffer contents, change the current buffer.

   Here, we reset all those changes by:
	o set back the current buffer.
	o move all markers and overlays to BEG.
	o remove all text properties.
	o set back the buffer multibyteness.  */

static void
decide_coding_unwind (Lisp_Object unwind_data)
{
  Lisp_Object multibyte, undo_list, buffer;

  multibyte = XCAR (unwind_data);
  unwind_data = XCDR (unwind_data);
  undo_list = XCAR (unwind_data);
  buffer = XCDR (unwind_data);

  set_buffer_internal (XBUFFER (buffer));
  adjust_markers_for_delete (BEG, BEG_BYTE, Z, Z_BYTE);
  adjust_overlays_for_delete (BEG, Z - BEG);
  set_buffer_intervals (current_buffer, NULL);
  TEMP_SET_PT_BOTH (BEG, BEG_BYTE);

  /* Now we are safe to change the buffer's multibyteness directly.  */
  bset_enable_multibyte_characters (current_buffer, multibyte);
  bset_undo_list (current_buffer, undo_list);
}

/* Read from a non-regular file.  STATE is a Lisp_Save_Value
   object where slot 0 is the file descriptor, slot 1 specifies
   an offset to put the read bytes, and slot 2 is the maximum
   amount of bytes to read.  Value is the number of bytes read.  */

static Lisp_Object
read_non_regular (Lisp_Object state)
{
  int nbytes = emacs_read_quit (XSAVE_INTEGER (state, 0),
				((char *) BEG_ADDR + PT_BYTE - BEG_BYTE
				 + XSAVE_INTEGER (state, 1)),
				XSAVE_INTEGER (state, 2));
  /* Fast recycle this object for the likely next call.  */
  free_misc (state);
  return make_number (nbytes);
}


/* Condition-case handler used when reading from non-regular files
   in insert-file-contents.  */

static Lisp_Object
read_non_regular_quit (Lisp_Object ignore)
{
  return Qnil;
}

/* Return the file offset that VAL represents, checking for type
   errors and overflow.  */
static off_t
file_offset (Lisp_Object val)
{
  if (RANGED_INTEGERP (0, val, TYPE_MAXIMUM (off_t)))
    return XINT (val);

  if (FLOATP (val))
    {
      double v = XFLOAT_DATA (val);
      if (0 <= v && v < 1.0 + TYPE_MAXIMUM (off_t))
	{
	  off_t o = v;
	  if (o == v)
	    return o;
	}
    }

  wrong_type_argument (intern ("file-offset"), val);
}

/* Return a special time value indicating the error number ERRNUM.  */
static struct timespec
time_error_value (int errnum)
{
  int ns = (errnum == ENOENT || errnum == EACCES || errnum == ENOTDIR
	    ? NONEXISTENT_MODTIME_NSECS
	    : UNKNOWN_MODTIME_NSECS);
  return make_timespec (0, ns);
}

static Lisp_Object
get_window_points_and_markers (void)
{
  Lisp_Object pt_marker = Fpoint_marker ();
  Lisp_Object windows
    = call3 (Qget_buffer_window_list, Fcurrent_buffer (), Qnil, Qt);
  Lisp_Object window_markers = windows;
  /* Window markers (and point) are handled specially: rather than move to
     just before or just after the modified text, we try to keep the
     markers at the same distance (bug#19161).
     In general, this is wrong, but for window-markers, this should be harmless
     and is convenient for the end user when most of the file is unmodified,
     except for a few minor details near the beginning and near the end.  */
  for (; CONSP (windows); windows = XCDR (windows))
    if (WINDOWP (XCAR (windows)))
      {
	Lisp_Object window_marker = XWINDOW (XCAR (windows))->pointm;
	XSETCAR (windows,
		 Fcons (window_marker, Fmarker_position (window_marker)));
      }
  return Fcons (Fcons (pt_marker, Fpoint ()), window_markers);
}

static void
restore_window_points (Lisp_Object window_markers, ptrdiff_t inserted,
		       ptrdiff_t same_at_start, ptrdiff_t same_at_end)
{
  for (; CONSP (window_markers); window_markers = XCDR (window_markers))
    if (CONSP (XCAR (window_markers)))
      {
	Lisp_Object car = XCAR (window_markers);
	Lisp_Object marker = XCAR (car);
	Lisp_Object oldpos = XCDR (car);
	if (MARKERP (marker) && INTEGERP (oldpos)
	    && XINT (oldpos) > same_at_start
	    && XINT (oldpos) < same_at_end)
	  {
	    ptrdiff_t oldsize = same_at_end - same_at_start;
	    ptrdiff_t newsize = inserted;
	    double growth = newsize / (double)oldsize;
	    ptrdiff_t newpos
	      = same_at_start + growth * (XINT (oldpos) - same_at_start);
	    Fset_marker (marker, make_number (newpos), Qnil);
	  }
      }
}

/* Make sure the gap is at Z_BYTE.  This is required to treat buffer
   text as a linear C char array.  */
static void
maybe_move_gap (struct buffer *b)
{
  if (BUF_GPT_BYTE (b) != BUF_Z_BYTE (b))
    {
      struct buffer *cb = current_buffer;

      set_buffer_internal (b);
      move_gap_both (Z, Z_BYTE);
      set_buffer_internal (cb);
    }
}

/* FIXME: insert-file-contents should be split with the top-level moved to
   Elisp and only the core kept in C.  */

DEFUN ("insert-file-contents", Finsert_file_contents, Sinsert_file_contents,
       1, 5, 0,
       doc: /* Insert contents of file FILENAME after point.
Returns list of absolute file name and number of characters inserted.
If second argument VISIT is non-nil, the buffer's visited filename and
last save file modtime are set, and it is marked unmodified.  If
visiting and the file does not exist, visiting is completed before the
error is signaled.

The optional third and fourth arguments BEG and END specify what portion
of the file to insert.  These arguments count bytes in the file, not
characters in the buffer.  If VISIT is non-nil, BEG and END must be nil.

If optional fifth argument REPLACE is non-nil, replace the current
buffer contents (in the accessible portion) with the file contents.
This is better than simply deleting and inserting the whole thing
because (1) it preserves some marker positions and (2) it puts less data
in the undo list.  When REPLACE is non-nil, the second return value is
the number of characters that replace previous buffer contents.

This function does code conversion according to the value of
`coding-system-for-read' or `file-coding-system-alist', and sets the
variable `last-coding-system-used' to the coding system actually used.

In addition, this function decodes the inserted text from known formats
by calling `format-decode', which see.  */)
  (Lisp_Object filename, Lisp_Object visit, Lisp_Object beg, Lisp_Object end, Lisp_Object replace)
{
  struct stat st;
  struct timespec mtime;
  int fd;
  ptrdiff_t inserted = 0;
  ptrdiff_t how_much;
  off_t beg_offset, end_offset;
  int unprocessed;
  ptrdiff_t count = SPECPDL_INDEX ();
  Lisp_Object handler, val, insval, orig_filename, old_undo;
  Lisp_Object p;
  ptrdiff_t total = 0;
  bool not_regular = 0;
  int save_errno = 0;
  char read_buf[READ_BUF_SIZE];
  struct coding_system coding;
  bool replace_handled = false;
  bool set_coding_system = false;
  Lisp_Object coding_system;
  bool read_quit = false;
  /* If the undo log only contains the insertion, there's no point
     keeping it.  It's typically when we first fill a file-buffer.  */
  bool empty_undo_list_p
    = (!NILP (visit) && NILP (BVAR (current_buffer, undo_list))
       && BEG == Z);
  Lisp_Object old_Vdeactivate_mark = Vdeactivate_mark;
  bool we_locked_file = false;
  ptrdiff_t fd_index;
  Lisp_Object window_markers = Qnil;
  /* same_at_start and same_at_end count bytes, because file access counts
     bytes and BEG and END count bytes.  */
  ptrdiff_t same_at_start = BEGV_BYTE;
  ptrdiff_t same_at_end = ZV_BYTE;
  /* SAME_AT_END_CHARPOS counts characters, because
     restore_window_points needs the old character count.  */
  ptrdiff_t same_at_end_charpos = ZV;

  if (current_buffer->base_buffer && ! NILP (visit))
    error ("Cannot do file visiting in an indirect buffer");

  if (!NILP (BVAR (current_buffer, read_only)))
    Fbarf_if_buffer_read_only (Qnil);

  val = Qnil;
  p = Qnil;
  orig_filename = Qnil;
  old_undo = Qnil;

  CHECK_STRING (filename);
  filename = Fexpand_file_name (filename, Qnil);

  /* The value Qnil means that the coding system is not yet
     decided.  */
  coding_system = Qnil;

  /* If the file name has special constructs in it,
     call the corresponding file handler.  */
  handler = Ffind_file_name_handler (filename, Qinsert_file_contents);
  if (!NILP (handler))
    {
      val = call6 (handler, Qinsert_file_contents, filename,
		   visit, beg, end, replace);
      if (CONSP (val) && CONSP (XCDR (val))
	  && RANGED_INTEGERP (0, XCAR (XCDR (val)), ZV - PT))
	inserted = XINT (XCAR (XCDR (val)));
      goto handled;
    }

  orig_filename = filename;
  filename = ENCODE_FILE (filename);

  fd = emacs_open (SSDATA (filename), O_RDONLY, 0);
  if (fd < 0)
    {
      save_errno = errno;
      if (NILP (visit))
	report_file_error ("Opening input file", orig_filename);
      mtime = time_error_value (save_errno);
      st.st_size = -1;
      if (!NILP (Vcoding_system_for_read))
	{
	  /* Don't let invalid values into buffer-file-coding-system.  */
	  CHECK_CODING_SYSTEM (Vcoding_system_for_read);
	  Fset (Qbuffer_file_coding_system, Vcoding_system_for_read);
	}
      goto notfound;
    }

  fd_index = SPECPDL_INDEX ();
  record_unwind_protect_int (close_file_unwind, fd);

  /* Replacement should preserve point as it preserves markers.  */
  if (!NILP (replace))
    {
      window_markers = get_window_points_and_markers ();
      record_unwind_protect (restore_point_unwind,
			     XCAR (XCAR (window_markers)));
    }

  if (fstat (fd, &st) != 0)
    report_file_error ("Input file status", orig_filename);
  mtime = get_stat_mtime (&st);

  /* This code will need to be changed in order to work on named
     pipes, and it's probably just not worth it.  So we should at
     least signal an error.  */
  if (!S_ISREG (st.st_mode))
    {
      not_regular = 1;

      if (! NILP (visit))
	goto notfound;

      if (! NILP (replace) || ! NILP (beg) || ! NILP (end))
	xsignal2 (Qfile_error,
		  build_string ("not a regular file"), orig_filename);
    }

  if (!NILP (visit))
    {
      if (!NILP (beg) || !NILP (end))
	error ("Attempt to visit less than an entire file");
      if (BEG < Z && NILP (replace))
	error ("Cannot do file visiting in a non-empty buffer");
    }

  if (!NILP (beg))
    beg_offset = file_offset (beg);
  else
    beg_offset = 0;

  if (!NILP (end))
    end_offset = file_offset (end);
  else
    {
      if (not_regular)
	end_offset = TYPE_MAXIMUM (off_t);
      else
	{
	  end_offset = st.st_size;

	  /* A negative size can happen on a platform that allows file
	     sizes greater than the maximum off_t value.  */
	  if (end_offset < 0)
	    buffer_overflow ();

	  /* The file size returned from stat may be zero, but data
	     may be readable nonetheless, for example when this is a
	     file in the /proc filesystem.  */
	  if (end_offset == 0)
	    end_offset = READ_BUF_SIZE;
	}
    }

  /* Check now whether the buffer will become too large,
     in the likely case where the file's length is not changing.
     This saves a lot of needless work before a buffer overflow.  */
  if (! not_regular)
    {
      /* The likely offset where we will stop reading.  We could read
	 more (or less), if the file grows (or shrinks) as we read it.  */
      off_t likely_end = min (end_offset, st.st_size);

      if (beg_offset < likely_end)
	{
	  ptrdiff_t buf_bytes
	    = Z_BYTE - (!NILP (replace) ? ZV_BYTE - BEGV_BYTE  : 0);
	  ptrdiff_t buf_growth_max = BUF_BYTES_MAX - buf_bytes;
	  off_t likely_growth = likely_end - beg_offset;
	  if (buf_growth_max < likely_growth)
	    buffer_overflow ();
	}
    }

  /* Prevent redisplay optimizations.  */
  current_buffer->clip_changed = true;

  if (EQ (Vcoding_system_for_read, Qauto_save_coding))
    {
      coding_system = coding_inherit_eol_type (Qutf_8_emacs, Qunix);
      setup_coding_system (coding_system, &coding);
      /* Ensure we set Vlast_coding_system_used.  */
      set_coding_system = true;
    }
  else if (BEG < Z)
    {
      /* Decide the coding system to use for reading the file now
         because we can't use an optimized method for handling
         `coding:' tag if the current buffer is not empty.  */
      if (!NILP (Vcoding_system_for_read))
	coding_system = Vcoding_system_for_read;
      else
	{
	  /* Don't try looking inside a file for a coding system
	     specification if it is not seekable.  */
	  if (! not_regular && ! NILP (Vset_auto_coding_function))
	    {
	      /* Find a coding system specified in the heading two
		 lines or in the tailing several lines of the file.
		 We assume that the 1K-byte and 3K-byte for heading
		 and tailing respectively are sufficient for this
		 purpose.  */
	      int nread;

	      if (st.st_size <= (1024 * 4))
		nread = emacs_read_quit (fd, read_buf, 1024 * 4);
	      else
		{
		  nread = emacs_read_quit (fd, read_buf, 1024);
		  if (nread == 1024)
		    {
		      int ntail;
		      if (lseek (fd, - (1024 * 3), SEEK_END) < 0)
			report_file_error ("Setting file position",
					   orig_filename);
		      ntail = emacs_read_quit (fd, read_buf + nread, 1024 * 3);
		      nread = ntail < 0 ? ntail : nread + ntail;
		    }
		}

	      if (nread < 0)
		report_file_error ("Read error", orig_filename);
	      else if (nread > 0)
		{
		  AUTO_STRING (name, " *code-converting-work*");
		  struct buffer *prev = current_buffer;
		  Lisp_Object workbuf;
		  struct buffer *buf;

		  record_unwind_current_buffer ();

		  workbuf = Fget_buffer_create (name);
		  buf = XBUFFER (workbuf);

		  delete_all_overlays (buf);
		  bset_directory (buf, BVAR (current_buffer, directory));
		  bset_read_only (buf, Qnil);
		  bset_filename (buf, Qnil);
		  bset_undo_list (buf, Qt);
		  eassert (buf->overlays_before == NULL);
		  eassert (buf->overlays_after == NULL);

		  set_buffer_internal (buf);
		  Ferase_buffer ();
		  bset_enable_multibyte_characters (buf, Qnil);

		  insert_1_both ((char *) read_buf, nread, nread, 0, 0, 0);
		  TEMP_SET_PT_BOTH (BEG, BEG_BYTE);
		  coding_system = call2 (Vset_auto_coding_function,
					 filename, make_number (nread));
		  set_buffer_internal (prev);

		  /* Discard the unwind protect for recovering the
                     current buffer.  */
		  specpdl_ptr--;

		  /* Rewind the file for the actual read done later.  */
		  if (lseek (fd, 0, SEEK_SET) < 0)
		    report_file_error ("Setting file position", orig_filename);
		}
	    }

	  if (NILP (coding_system))
	    {
	      /* If we have not yet decided a coding system, check
                 file-coding-system-alist.  */
	      coding_system = CALLN (Ffind_operation_coding_system,
				     Qinsert_file_contents, orig_filename,
				     visit, beg, end, replace);
	      if (CONSP (coding_system))
		coding_system = XCAR (coding_system);
	    }
	}

      if (NILP (coding_system))
	coding_system = Qundecided;
      else
	CHECK_CODING_SYSTEM (coding_system);

      if (NILP (BVAR (current_buffer, enable_multibyte_characters)))
	/* We must suppress all character code conversion except for
	   end-of-line conversion.  */
	coding_system = raw_text_coding_system (coding_system);

      setup_coding_system (coding_system, &coding);
      /* Ensure we set Vlast_coding_system_used.  */
      set_coding_system = true;
    }

  /* If requested, replace the accessible part of the buffer
     with the file contents.  Avoid replacing text at the
     beginning or end of the buffer that matches the file contents;
     that preserves markers pointing to the unchanged parts.

     Here we implement this feature in an optimized way
     for the case where code conversion is NOT needed.
     The following if-statement handles the case of conversion
     in a less optimal way.

     If the code conversion is "automatic" then we try using this
     method and hope for the best.
     But if we discover the need for conversion, we give up on this method
     and let the following if-statement handle the replace job.  */
  if (!NILP (replace)
      && BEGV < ZV
      && (NILP (coding_system)
	  || ! CODING_REQUIRE_DECODING (&coding)))
    {
      ptrdiff_t overlap;
      /* There is still a possibility we will find the need to do code
	 conversion.  If that happens, set this variable to
	 give up on handling REPLACE in the optimized way.  */
      bool giveup_match_end = false;

      if (beg_offset != 0)
	{
	  if (lseek (fd, beg_offset, SEEK_SET) < 0)
	    report_file_error ("Setting file position", orig_filename);
	}

      /* Count how many chars at the start of the file
	 match the text at the beginning of the buffer.  */
      while (true)
	{
	  int nread = emacs_read_quit (fd, read_buf, sizeof read_buf);
	  if (nread < 0)
	    report_file_error ("Read error", orig_filename);
	  else if (nread == 0)
	    break;

	  if (CODING_REQUIRE_DETECTION (&coding))
	    {
	      coding_system = detect_coding_system ((unsigned char *) read_buf,
						    nread, nread, 1, 0,
						    coding_system);
	      setup_coding_system (coding_system, &coding);
	    }

	  if (CODING_REQUIRE_DECODING (&coding))
	    /* We found that the file should be decoded somehow.
               Let's give up here.  */
	    {
	      giveup_match_end = true;
	      break;
	    }

	  int bufpos = 0;
	  while (bufpos < nread && same_at_start < ZV_BYTE
		 && FETCH_BYTE (same_at_start) == read_buf[bufpos])
	    same_at_start++, bufpos++;
	  /* If we found a discrepancy, stop the scan.
	     Otherwise loop around and scan the next bufferful.  */
	  if (bufpos != nread)
	    break;
	}
      /* If the file matches the buffer completely,
	 there's no need to replace anything.  */
      if (same_at_start - BEGV_BYTE == end_offset - beg_offset)
	{
	  emacs_close (fd);
	  clear_unwind_protect (fd_index);

	  /* Truncate the buffer to the size of the file.  */
	  del_range_1 (same_at_start, same_at_end, 0, 0);
	  goto handled;
	}

      /* Count how many chars at the end of the file
	 match the text at the end of the buffer.  But, if we have
	 already found that decoding is necessary, don't waste time.  */
      while (!giveup_match_end)
	{
	  int total_read, nread, bufpos, trial;
	  off_t curpos;

	  /* At what file position are we now scanning?  */
	  curpos = end_offset - (ZV_BYTE - same_at_end);
	  /* If the entire file matches the buffer tail, stop the scan.  */
	  if (curpos == 0)
	    break;
	  /* How much can we scan in the next step?  */
	  trial = min (curpos, sizeof read_buf);
	  if (lseek (fd, curpos - trial, SEEK_SET) < 0)
	    report_file_error ("Setting file position", orig_filename);

	  total_read = nread = 0;
	  while (total_read < trial)
	    {
	      nread = emacs_read_quit (fd, read_buf + total_read,
				       trial - total_read);
	      if (nread < 0)
		report_file_error ("Read error", orig_filename);
	      else if (nread == 0)
		break;
	      total_read += nread;
	    }

	  /* Scan this bufferful from the end, comparing with
	     the Emacs buffer.  */
	  bufpos = total_read;

	  /* Compare with same_at_start to avoid counting some buffer text
	     as matching both at the file's beginning and at the end.  */
	  while (bufpos > 0 && same_at_end > same_at_start
		 && FETCH_BYTE (same_at_end - 1) == read_buf[bufpos - 1])
	    same_at_end--, bufpos--;

	  /* If we found a discrepancy, stop the scan.
	     Otherwise loop around and scan the preceding bufferful.  */
	  if (bufpos != 0)
	    {
	      /* If this discrepancy is because of code conversion,
		 we cannot use this method; giveup and try the other.  */
	      if (same_at_end > same_at_start
		  && FETCH_BYTE (same_at_end - 1) >= 0200
		  && ! NILP (BVAR (current_buffer, enable_multibyte_characters))
		  && (CODING_MAY_REQUIRE_DECODING (&coding)))
		giveup_match_end = true;
	      break;
	    }

	  if (nread == 0)
	    break;
	}

      if (! giveup_match_end)
	{
	  ptrdiff_t temp;
          ptrdiff_t this_count = SPECPDL_INDEX ();

	  /* We win!  We can handle REPLACE the optimized way.  */

	  /* Extend the start of non-matching text area to multibyte
             character boundary.  */
	  if (! NILP (BVAR (current_buffer, enable_multibyte_characters)))
	    while (same_at_start > BEGV_BYTE
		   && ! CHAR_HEAD_P (FETCH_BYTE (same_at_start)))
	      same_at_start--;

	  /* Extend the end of non-matching text area to multibyte
             character boundary.  */
	  if (! NILP (BVAR (current_buffer, enable_multibyte_characters)))
	    while (same_at_end < ZV_BYTE
		   && ! CHAR_HEAD_P (FETCH_BYTE (same_at_end)))
	      same_at_end++;

	  /* Don't try to reuse the same piece of text twice.  */
	  overlap = (same_at_start - BEGV_BYTE
		     - (same_at_end
			+ (! NILP (end) ? end_offset : st.st_size) - ZV_BYTE));
	  if (overlap > 0)
	    same_at_end += overlap;
	  same_at_end_charpos = BYTE_TO_CHAR (same_at_end);

	  /* Arrange to read only the nonmatching middle part of the file.  */
	  beg_offset += same_at_start - BEGV_BYTE;
	  end_offset -= ZV_BYTE - same_at_end;

          /* This binding is to avoid ask-user-about-supersession-threat
	     being called in insert_from_buffer or del_range_bytes (via
	     prepare_to_modify_buffer).
             AFAICT we could avoid ask-user-about-supersession-threat by setting
             current_buffer->modtime earlier, but we could still end up calling
             ask-user-about-supersession-threat if the file is modified while
             we read it, so we bind buffer-file-name instead.  */
          specbind (intern ("buffer-file-name"), Qnil);
	  del_range_byte (same_at_start, same_at_end);
	  /* Insert from the file at the proper position.  */
	  temp = BYTE_TO_CHAR (same_at_start);
	  SET_PT_BOTH (temp, same_at_start);
          unbind_to (this_count, Qnil);

	  /* If display currently starts at beginning of line,
	     keep it that way.  */
	  if (XBUFFER (XWINDOW (selected_window)->contents) == current_buffer)
	    XWINDOW (selected_window)->start_at_line_beg = !NILP (Fbolp ());

	  replace_handled = true;
	}
    }

  /* If requested, replace the accessible part of the buffer
     with the file contents.  Avoid replacing text at the
     beginning or end of the buffer that matches the file contents;
     that preserves markers pointing to the unchanged parts.

     Here we implement this feature for the case where code conversion
     is needed, in a simple way that needs a lot of memory.
     The preceding if-statement handles the case of no conversion
     in a more optimized way.  */
  if (!NILP (replace) && ! replace_handled && BEGV < ZV)
    {
      ptrdiff_t same_at_start_charpos;
      ptrdiff_t inserted_chars;
      ptrdiff_t overlap;
      ptrdiff_t bufpos;
      unsigned char *decoded;
      ptrdiff_t temp;
      ptrdiff_t this = 0;
      ptrdiff_t this_count = SPECPDL_INDEX ();
      bool multibyte
	= ! NILP (BVAR (current_buffer, enable_multibyte_characters));
      Lisp_Object conversion_buffer;

      conversion_buffer = code_conversion_save (1, multibyte);

      /* First read the whole file, performing code conversion into
	 CONVERSION_BUFFER.  */

      if (lseek (fd, beg_offset, SEEK_SET) < 0)
	report_file_error ("Setting file position", orig_filename);

      inserted = 0;		/* Bytes put into CONVERSION_BUFFER so far.  */
      unprocessed = 0;		/* Bytes not processed in previous loop.  */

      while (true)
	{
	  /* Read at most READ_BUF_SIZE bytes at a time, to allow
	     quitting while reading a huge file.  */

	  this = emacs_read_quit (fd, read_buf + unprocessed,
				  READ_BUF_SIZE - unprocessed);
	  if (this <= 0)
	    break;

	  BUF_TEMP_SET_PT (XBUFFER (conversion_buffer),
			   BUF_Z (XBUFFER (conversion_buffer)));
	  decode_coding_c_string (&coding, (unsigned char *) read_buf,
				  unprocessed + this, conversion_buffer);
	  unprocessed = coding.carryover_bytes;
	  if (coding.carryover_bytes > 0)
	    memcpy (read_buf, coding.carryover, unprocessed);
	}

      if (this < 0)
	report_file_error ("Read error", orig_filename);
      emacs_close (fd);
      clear_unwind_protect (fd_index);

      if (unprocessed > 0)
	{
	  coding.mode |= CODING_MODE_LAST_BLOCK;
	  decode_coding_c_string (&coding, (unsigned char *) read_buf,
				  unprocessed, conversion_buffer);
	  coding.mode &= ~CODING_MODE_LAST_BLOCK;
	}

      coding_system = CODING_ID_NAME (coding.id);
      set_coding_system = true;
      maybe_move_gap (XBUFFER (conversion_buffer));
      decoded = BUF_BEG_ADDR (XBUFFER (conversion_buffer));
      inserted = (BUF_Z_BYTE (XBUFFER (conversion_buffer))
		  - BUF_BEG_BYTE (XBUFFER (conversion_buffer)));

      /* Compare the beginning of the converted string with the buffer
	 text.  */

      bufpos = 0;
      while (bufpos < inserted && same_at_start < same_at_end
	     && FETCH_BYTE (same_at_start) == decoded[bufpos])
	same_at_start++, bufpos++;

      /* If the file matches the head of buffer completely,
	 there's no need to replace anything.  */

      if (bufpos == inserted)
	{
	  /* Truncate the buffer to the size of the file.  */
	  if (same_at_start != same_at_end)
	    {
              /* See previous specbind for the reason behind this.  */
              specbind (intern ("buffer-file-name"), Qnil);
	      del_range_byte (same_at_start, same_at_end);
	    }
	  inserted = 0;

	  unbind_to (this_count, Qnil);
	  goto handled;
	}

      /* Extend the start of non-matching text area to the previous
	 multibyte character boundary.  */
      if (! NILP (BVAR (current_buffer, enable_multibyte_characters)))
	while (same_at_start > BEGV_BYTE
	       && ! CHAR_HEAD_P (FETCH_BYTE (same_at_start)))
	  same_at_start--;

      /* Scan this bufferful from the end, comparing with
	 the Emacs buffer.  */
      bufpos = inserted;

      /* Compare with same_at_start to avoid counting some buffer text
	 as matching both at the file's beginning and at the end.  */
      while (bufpos > 0 && same_at_end > same_at_start
	     && FETCH_BYTE (same_at_end - 1) == decoded[bufpos - 1])
	same_at_end--, bufpos--;

      /* Extend the end of non-matching text area to the next
	 multibyte character boundary.  */
      if (! NILP (BVAR (current_buffer, enable_multibyte_characters)))
	while (same_at_end < ZV_BYTE
	       && ! CHAR_HEAD_P (FETCH_BYTE (same_at_end)))
	  same_at_end++;

      /* Don't try to reuse the same piece of text twice.  */
      overlap = same_at_start - BEGV_BYTE - (same_at_end + inserted - ZV_BYTE);
      if (overlap > 0)
	same_at_end += overlap;
      same_at_end_charpos = BYTE_TO_CHAR (same_at_end);

      /* If display currently starts at beginning of line,
	 keep it that way.  */
      if (XBUFFER (XWINDOW (selected_window)->contents) == current_buffer)
	XWINDOW (selected_window)->start_at_line_beg = !NILP (Fbolp ());

      /* Replace the chars that we need to replace,
	 and update INSERTED to equal the number of bytes
	 we are taking from the decoded string.  */
      inserted -= (ZV_BYTE - same_at_end) + (same_at_start - BEGV_BYTE);

      /* See previous specbind for the reason behind this.  */
      specbind (intern ("buffer-file-name"), Qnil);
      if (same_at_end != same_at_start)
	{
	  del_range_byte (same_at_start, same_at_end);
	  temp = GPT;
	  eassert (same_at_start == GPT_BYTE);
	  same_at_start = GPT_BYTE;
	}
      else
	{
	  temp = same_at_end_charpos;
	}
      /* Insert from the file at the proper position.  */
      SET_PT_BOTH (temp, same_at_start);
      same_at_start_charpos
	= buf_bytepos_to_charpos (XBUFFER (conversion_buffer),
				  same_at_start - BEGV_BYTE
				  + BUF_BEG_BYTE (XBUFFER (conversion_buffer)));
      eassert (same_at_start_charpos == temp - (BEGV - BEG));
      inserted_chars
	= (buf_bytepos_to_charpos (XBUFFER (conversion_buffer),
				   same_at_start + inserted - BEGV_BYTE
				  + BUF_BEG_BYTE (XBUFFER (conversion_buffer)))
	   - same_at_start_charpos);
      insert_from_buffer (XBUFFER (conversion_buffer),
			  same_at_start_charpos, inserted_chars, 0);
      /* Set `inserted' to the number of inserted characters.  */
      inserted = PT - temp;
      /* Set point before the inserted characters.  */
      SET_PT_BOTH (temp, same_at_start);

      unbind_to (this_count, Qnil);

      goto handled;
    }

  if (! not_regular)
    total = end_offset - beg_offset;
  else
    /* For a special file, all we can do is guess.  */
    total = READ_BUF_SIZE;

  if (NILP (visit) && total > 0)
    {
      if (!NILP (BVAR (current_buffer, file_truename))
	  /* Make binding buffer-file-name to nil effective.  */
	  && !NILP (BVAR (current_buffer, filename))
	  && SAVE_MODIFF >= MODIFF)
	we_locked_file = true;
      prepare_to_modify_buffer (PT, PT, NULL);
    }

  move_gap_both (PT, PT_BYTE);
  if (GAP_SIZE < total)
    make_gap (total - GAP_SIZE);

  if (beg_offset != 0 || !NILP (replace))
    {
      if (lseek (fd, beg_offset, SEEK_SET) < 0)
	report_file_error ("Setting file position", orig_filename);
    }

  /* In the following loop, HOW_MUCH contains the total bytes read so
     far for a regular file, and not changed for a special file.  But,
     before exiting the loop, it is set to a negative value if I/O
     error occurs.  */
  how_much = 0;

  /* Total bytes inserted.  */
  inserted = 0;

  /* Here, we don't do code conversion in the loop.  It is done by
     decode_coding_gap after all data are read into the buffer.  */
  {
    ptrdiff_t gap_size = GAP_SIZE;

    while (how_much < total)
      {
	/* `try' is reserved in some compilers (Microsoft C).  */
	ptrdiff_t trytry = min (total - how_much, READ_BUF_SIZE);
	ptrdiff_t this;

	if (not_regular)
	  {
	    Lisp_Object nbytes;

	    /* Maybe make more room.  */
	    if (gap_size < trytry)
	      {
		make_gap (trytry - gap_size);
		gap_size = GAP_SIZE - inserted;
	      }

	    /* Read from the file, capturing `quit'.  When an
	       error occurs, end the loop, and arrange for a quit
	       to be signaled after decoding the text we read.  */
	    nbytes = internal_condition_case_1
	      (read_non_regular,
	       make_save_int_int_int (fd, inserted, trytry),
	       Qerror, read_non_regular_quit);

	    if (NILP (nbytes))
	      {
		read_quit = true;
		break;
	      }

	    this = XINT (nbytes);
	  }
	else
	  {
	    /* Allow quitting out of the actual I/O.  We don't make text
	       part of the buffer until all the reading is done, so a C-g
	       here doesn't do any harm.  */
	    this = emacs_read_quit (fd,
				    ((char *) BEG_ADDR + PT_BYTE - BEG_BYTE
				     + inserted),
				    trytry);
	  }

	if (this <= 0)
	  {
	    how_much = this;
	    break;
	  }

	gap_size -= this;

	/* For a regular file, where TOTAL is the real size,
	   count HOW_MUCH to compare with it.
	   For a special file, where TOTAL is just a buffer size,
	   so don't bother counting in HOW_MUCH.
	   (INSERTED is where we count the number of characters inserted.)  */
	if (! not_regular)
	  how_much += this;
	inserted += this;
      }
  }

  /* Now we have either read all the file data into the gap,
     or stop reading on I/O error or quit.  If nothing was
     read, undo marking the buffer modified.  */

  if (inserted == 0)
    {
      if (we_locked_file)
	unlock_file (BVAR (current_buffer, file_truename));
      Vdeactivate_mark = old_Vdeactivate_mark;
    }
  else
    Fset (Qdeactivate_mark, Qt);

  emacs_close (fd);
  clear_unwind_protect (fd_index);

  if (how_much < 0)
    report_file_error ("Read error", orig_filename);

  /* Make the text read part of the buffer.  */
  GAP_SIZE -= inserted;
  GPT      += inserted;
  GPT_BYTE += inserted;
  ZV       += inserted;
  ZV_BYTE  += inserted;
  Z        += inserted;
  Z_BYTE   += inserted;

  if (GAP_SIZE > 0)
    /* Put an anchor to ensure multi-byte form ends at gap.  */
    *GPT_ADDR = 0;

 notfound:

  if (NILP (coding_system))
    {
      /* The coding system is not yet decided.  Decide it by an
	 optimized method for handling `coding:' tag.

	 Note that we can get here only if the buffer was empty
	 before the insertion.  */

      if (!NILP (Vcoding_system_for_read))
	coding_system = Vcoding_system_for_read;
      else
	{
	  /* Since we are sure that the current buffer was empty
	     before the insertion, we can toggle
	     enable-multibyte-characters directly here without taking
	     care of marker adjustment.  By this way, we can run Lisp
	     program safely before decoding the inserted text.  */
	  Lisp_Object unwind_data;
	  ptrdiff_t count1 = SPECPDL_INDEX ();

	  unwind_data = Fcons (BVAR (current_buffer, enable_multibyte_characters),
			       Fcons (BVAR (current_buffer, undo_list),
				      Fcurrent_buffer ()));
	  bset_enable_multibyte_characters (current_buffer, Qnil);
	  bset_undo_list (current_buffer, Qt);
	  record_unwind_protect (decide_coding_unwind, unwind_data);

	  if (inserted > 0 && ! NILP (Vset_auto_coding_function))
	    {
	      coding_system = call2 (Vset_auto_coding_function,
				     filename, make_number (inserted));
	    }

	  if (NILP (coding_system))
	    {
	      /* If the coding system is not yet decided, check
		 file-coding-system-alist.  */
	      coding_system = CALLN (Ffind_operation_coding_system,
				     Qinsert_file_contents, orig_filename,
				     visit, beg, end, Qnil);
	      if (CONSP (coding_system))
		coding_system = XCAR (coding_system);
	    }
	  unbind_to (count1, Qnil);
	  inserted = Z_BYTE - BEG_BYTE;
	}

      if (NILP (coding_system))
	coding_system = Qundecided;
      else
	CHECK_CODING_SYSTEM (coding_system);

      if (NILP (BVAR (current_buffer, enable_multibyte_characters)))
	/* We must suppress all character code conversion except for
	   end-of-line conversion.  */
	coding_system = raw_text_coding_system (coding_system);
      setup_coding_system (coding_system, &coding);
      /* Ensure we set Vlast_coding_system_used.  */
      set_coding_system = true;
    }

  if (!NILP (visit))
    {
      /* When we visit a file by raw-text, we change the buffer to
	 unibyte.  */
      if (CODING_FOR_UNIBYTE (&coding)
	  /* Can't do this if part of the buffer might be preserved.  */
	  && NILP (replace))
	{
	  /* Visiting a file with these coding system makes the buffer
	     unibyte.  */
	  if (inserted > 0)
	    bset_enable_multibyte_characters (current_buffer, Qnil);
	  else
	    Fset_buffer_multibyte (Qnil);
	}
    }

  coding.dst_multibyte = ! NILP (BVAR (current_buffer, enable_multibyte_characters));
  if (CODING_MAY_REQUIRE_DECODING (&coding)
      && (inserted > 0 || CODING_REQUIRE_FLUSHING (&coding)))
    {
      move_gap_both (PT, PT_BYTE);
      GAP_SIZE += inserted;
      ZV_BYTE -= inserted;
      Z_BYTE -= inserted;
      ZV -= inserted;
      Z -= inserted;
      decode_coding_gap (&coding, inserted, inserted);
      inserted = coding.produced_char;
      coding_system = CODING_ID_NAME (coding.id);
    }
  else if (inserted > 0)
    {
      invalidate_buffer_caches (current_buffer, PT, PT + inserted);
      adjust_after_insert (PT, PT_BYTE, PT + inserted, PT_BYTE + inserted,
			   inserted);
    }

  /* Call after-change hooks for the inserted text, aside from the case
     of normal visiting (not with REPLACE), which is done in a new buffer
     "before" the buffer is changed.  */
  if (inserted > 0 && total > 0
      && (NILP (visit) || !NILP (replace)))
    {
      signal_after_change (PT, 0, inserted);
      update_compositions (PT, PT, CHECK_BORDER);
    }

  /* Now INSERTED is measured in characters.  */

 handled:

  if (inserted > 0)
    restore_window_points (window_markers, inserted,
			   BYTE_TO_CHAR (same_at_start),
			   same_at_end_charpos);

  if (!NILP (visit))
    {
      if (empty_undo_list_p)
	bset_undo_list (current_buffer, Qnil);

      if (NILP (handler))
	{
	  current_buffer->modtime = mtime;
	  current_buffer->modtime_size = st.st_size;
	  bset_filename (current_buffer, orig_filename);
	}

      SAVE_MODIFF = MODIFF;
      BUF_AUTOSAVE_MODIFF (current_buffer) = MODIFF;
      XSETFASTINT (BVAR (current_buffer, save_length), Z - BEG);
      if (NILP (handler))
	{
	  if (!NILP (BVAR (current_buffer, file_truename)))
	    unlock_file (BVAR (current_buffer, file_truename));
	  unlock_file (filename);
	}
      if (not_regular)
	xsignal2 (Qfile_error,
		  build_string ("not a regular file"), orig_filename);
    }

  if (set_coding_system)
    Vlast_coding_system_used = coding_system;

  if (! NILP (Ffboundp (Qafter_insert_file_set_coding)))
    {
      insval = call2 (Qafter_insert_file_set_coding, make_number (inserted),
		      visit);
      if (! NILP (insval))
	{
	  if (! RANGED_INTEGERP (0, insval, ZV - PT))
	    wrong_type_argument (intern ("inserted-chars"), insval);
	  inserted = XFASTINT (insval);
	}
    }

  /* Decode file format.  */
  if (inserted > 0)
    {
      /* Don't run point motion or modification hooks when decoding.  */
      ptrdiff_t count1 = SPECPDL_INDEX ();
      ptrdiff_t old_inserted = inserted;
      specbind (Qinhibit_point_motion_hooks, Qt);
      specbind (Qinhibit_modification_hooks, Qt);

      /* Save old undo list and don't record undo for decoding.  */
      old_undo = BVAR (current_buffer, undo_list);
      bset_undo_list (current_buffer, Qt);

      if (NILP (replace))
	{
	  insval = call3 (Qformat_decode,
			  Qnil, make_number (inserted), visit);
	  if (! RANGED_INTEGERP (0, insval, ZV - PT))
	    wrong_type_argument (intern ("inserted-chars"), insval);
	  inserted = XFASTINT (insval);
	}
      else
	{
	  /* If REPLACE is non-nil and we succeeded in not replacing the
	     beginning or end of the buffer text with the file's contents,
	     call format-decode with `point' positioned at the beginning
	     of the buffer and `inserted' equaling the number of
	     characters in the buffer.  Otherwise, format-decode might
	     fail to correctly analyze the beginning or end of the buffer.
	     Hence we temporarily save `point' and `inserted' here and
	     restore `point' iff format-decode did not insert or delete
	     any text.  Otherwise we leave `point' at point-min.  */
	  ptrdiff_t opoint = PT;
	  ptrdiff_t opoint_byte = PT_BYTE;
	  ptrdiff_t oinserted = ZV - BEGV;
	  EMACS_INT ochars_modiff = CHARS_MODIFF;

	  TEMP_SET_PT_BOTH (BEGV, BEGV_BYTE);
	  insval = call3 (Qformat_decode,
			  Qnil, make_number (oinserted), visit);
	  if (! RANGED_INTEGERP (0, insval, ZV - PT))
	    wrong_type_argument (intern ("inserted-chars"), insval);
	  if (ochars_modiff == CHARS_MODIFF)
	    /* format_decode didn't modify buffer's characters => move
	       point back to position before inserted text and leave
	       value of inserted alone.  */
	    SET_PT_BOTH (opoint, opoint_byte);
	  else
	    /* format_decode modified buffer's characters => consider
	       entire buffer changed and leave point at point-min.  */
	    inserted = XFASTINT (insval);
	}

      /* For consistency with format-decode call these now iff inserted > 0
	 (martin 2007-06-28).  */
      p = Vafter_insert_file_functions;
      while (CONSP (p))
	{
	  if (NILP (replace))
	    {
	      insval = call1 (XCAR (p), make_number (inserted));
	      if (!NILP (insval))
		{
		  if (! RANGED_INTEGERP (0, insval, ZV - PT))
		    wrong_type_argument (intern ("inserted-chars"), insval);
		  inserted = XFASTINT (insval);
		}
	    }
	  else
	    {
	      /* For the rationale of this see the comment on
		 format-decode above.  */
	      ptrdiff_t opoint = PT;
	      ptrdiff_t opoint_byte = PT_BYTE;
	      ptrdiff_t oinserted = ZV - BEGV;
	      EMACS_INT ochars_modiff = CHARS_MODIFF;

	      TEMP_SET_PT_BOTH (BEGV, BEGV_BYTE);
	      insval = call1 (XCAR (p), make_number (oinserted));
	      if (!NILP (insval))
		{
		  if (! RANGED_INTEGERP (0, insval, ZV - PT))
		    wrong_type_argument (intern ("inserted-chars"), insval);
		  if (ochars_modiff == CHARS_MODIFF)
		    /* after_insert_file_functions didn't modify
		       buffer's characters => move point back to
		       position before inserted text and leave value of
		       inserted alone.  */
		    SET_PT_BOTH (opoint, opoint_byte);
		  else
		    /* after_insert_file_functions did modify buffer's
	               characters => consider entire buffer changed and
	               leave point at point-min.  */
		    inserted = XFASTINT (insval);
		}
	    }

	  maybe_quit ();
	  p = XCDR (p);
	}

      if (!empty_undo_list_p)
	{
	  bset_undo_list (current_buffer, old_undo);
	  if (CONSP (old_undo) && inserted != old_inserted)
	    {
	      /* Adjust the last undo record for the size change during
		 the format conversion.  */
	      Lisp_Object tem = XCAR (old_undo);
	      if (CONSP (tem) && INTEGERP (XCAR (tem))
		  && INTEGERP (XCDR (tem))
		  && XFASTINT (XCDR (tem)) == PT + old_inserted)
		XSETCDR (tem, make_number (PT + inserted));
	    }
	}
      else
	/* If undo_list was Qt before, keep it that way.
	   Otherwise start with an empty undo_list.  */
	bset_undo_list (current_buffer, EQ (old_undo, Qt) ? Qt : Qnil);

      unbind_to (count1, Qnil);
    }

  if (!NILP (visit)
      && current_buffer->modtime.tv_nsec == NONEXISTENT_MODTIME_NSECS)
    {
      /* If visiting nonexistent file, return nil.  */
      report_file_errno ("Opening input file", orig_filename, save_errno);
    }

  /* We made a lot of deletions and insertions above, so invalidate
     the newline cache for the entire region of the inserted
     characters.  */
  if (current_buffer->base_buffer && current_buffer->base_buffer->newline_cache)
    invalidate_region_cache (current_buffer->base_buffer,
                             current_buffer->base_buffer->newline_cache,
                             PT - BEG, Z - PT - inserted);
  else if (current_buffer->newline_cache)
    invalidate_region_cache (current_buffer,
                             current_buffer->newline_cache,
                             PT - BEG, Z - PT - inserted);

  if (read_quit)
    quit ();

  /* Retval needs to be dealt with in all cases consistently.  */
  if (NILP (val))
    val = list2 (orig_filename, make_number (inserted));

  return unbind_to (count, val);
}

static Lisp_Object build_annotations (Lisp_Object, Lisp_Object);

static void
build_annotations_unwind (Lisp_Object arg)
{
  Vwrite_region_annotation_buffers = arg;
}

/* Decide the coding-system to encode the data with.  */

static Lisp_Object
choose_write_coding_system (Lisp_Object start, Lisp_Object end, Lisp_Object filename,
			    Lisp_Object append, Lisp_Object visit, Lisp_Object lockname,
			    struct coding_system *coding)
{
  Lisp_Object val;
  Lisp_Object eol_parent = Qnil;

  if (auto_saving
      && NILP (Fstring_equal (BVAR (current_buffer, filename),
			      BVAR (current_buffer, auto_save_file_name))))
    {
      val = Qutf_8_emacs;
      eol_parent = Qunix;
    }
  else if (!NILP (Vcoding_system_for_write))
    {
      val = Vcoding_system_for_write;
      if (coding_system_require_warning
	  && !NILP (Ffboundp (Vselect_safe_coding_system_function)))
	/* Confirm that VAL can surely encode the current region.  */
	val = call5 (Vselect_safe_coding_system_function,
		     start, end, list2 (Qt, val),
		     Qnil, filename);
    }
  else
    {
      /* If the variable `buffer-file-coding-system' is set locally,
	 it means that the file was read with some kind of code
	 conversion or the variable is explicitly set by users.  We
	 had better write it out with the same coding system even if
	 `enable-multibyte-characters' is nil.

	 If it is not set locally, we anyway have to convert EOL
	 format if the default value of `buffer-file-coding-system'
	 tells that it is not Unix-like (LF only) format.  */
      bool using_default_coding = 0;
      bool force_raw_text = 0;

      val = BVAR (current_buffer, buffer_file_coding_system);
      if (NILP (val)
	  || NILP (Flocal_variable_p (Qbuffer_file_coding_system, Qnil)))
	{
	  val = Qnil;
	  if (NILP (BVAR (current_buffer, enable_multibyte_characters)))
	    force_raw_text = 1;
	}

      if (NILP (val))
	{
	  /* Check file-coding-system-alist.  */
	  Lisp_Object coding_systems
	    = CALLN (Ffind_operation_coding_system, Qwrite_region, start, end,
		     filename, append, visit, lockname);
	  if (CONSP (coding_systems) && !NILP (XCDR (coding_systems)))
	    val = XCDR (coding_systems);
	}

      if (NILP (val))
	{
	  /* If we still have not decided a coding system, use the
	     current buffer's value of buffer-file-coding-system.  */
	  val = BVAR (current_buffer, buffer_file_coding_system);
	  using_default_coding = 1;
	}

      if (! NILP (val) && ! force_raw_text)
	{
	  Lisp_Object spec, attrs;

	  CHECK_CODING_SYSTEM (val);
	  CHECK_CODING_SYSTEM_GET_SPEC (val, spec);
	  attrs = AREF (spec, 0);
	  if (EQ (CODING_ATTR_TYPE (attrs), Qraw_text))
	    force_raw_text = 1;
	}

      if (!force_raw_text
	  && !NILP (Ffboundp (Vselect_safe_coding_system_function)))
	{
	  /* Confirm that VAL can surely encode the current region.  */
	  val = call5 (Vselect_safe_coding_system_function,
		       start, end, val, Qnil, filename);
	  /* As the function specified by select-safe-coding-system-function
	     is out of our control, make sure we are not fed by bogus
	     values.  */
	  if (!NILP (val))
	    CHECK_CODING_SYSTEM (val);
	}

      /* If the decided coding-system doesn't specify end-of-line
	 format, we use that of `buffer-file-coding-system'.  */
      if (! using_default_coding)
	{
	  Lisp_Object dflt = BVAR (&buffer_defaults, buffer_file_coding_system);

	  if (! NILP (dflt))
	    val = coding_inherit_eol_type (val, dflt);
	}

      /* If we decide not to encode text, use `raw-text' or one of its
	 subsidiaries.  */
      if (force_raw_text)
	val = raw_text_coding_system (val);
    }

  val = coding_inherit_eol_type (val, eol_parent);
  setup_coding_system (val, coding);

  if (!STRINGP (start) && !NILP (BVAR (current_buffer, selective_display)))
    coding->mode |= CODING_MODE_SELECTIVE_DISPLAY;
  return val;
}

DEFUN ("write-region", Fwrite_region, Swrite_region, 3, 7,
       "r\nFWrite region to file: \ni\ni\ni\np",
       doc: /* Write current region into specified file.
When called from a program, requires three arguments:
START, END and FILENAME.  START and END are normally buffer positions
specifying the part of the buffer to write.
If START is nil, that means to use the entire buffer contents; END is
ignored.
If START is a string, then output that string to the file
instead of any buffer contents; END is ignored.

Optional fourth argument APPEND if non-nil means
  append to existing file contents (if any).  If it is a number,
  seek to that offset in the file before writing.
Optional fifth argument VISIT, if t or a string, means
  set the last-save-file-modtime of buffer to this file's modtime
  and mark buffer not modified.
If VISIT is a string, it is a second file name;
  the output goes to FILENAME, but the buffer is marked as visiting VISIT.
  VISIT is also the file name to lock and unlock for clash detection.
If VISIT is neither t nor nil nor a string, or if Emacs is in batch mode,
  do not display the \"Wrote file\" message.
The optional sixth arg LOCKNAME, if non-nil, specifies the name to
  use for locking and unlocking, overriding FILENAME and VISIT.
The optional seventh arg MUSTBENEW, if non-nil, insists on a check
  for an existing file with the same name.  If MUSTBENEW is `excl',
  that means to get an error if the file already exists; never overwrite.
  If MUSTBENEW is neither nil nor `excl', that means ask for
  confirmation before overwriting, but do go ahead and overwrite the file
  if the user confirms.

This does code conversion according to the value of
`coding-system-for-write', `buffer-file-coding-system', or
`file-coding-system-alist', and sets the variable
`last-coding-system-used' to the coding system actually used.

This calls `write-region-annotate-functions' at the start, and
`write-region-post-annotation-function' at the end.  */)
  (Lisp_Object start, Lisp_Object end, Lisp_Object filename, Lisp_Object append,
   Lisp_Object visit, Lisp_Object lockname, Lisp_Object mustbenew)
{
  return write_region (start, end, filename, append, visit, lockname, mustbenew,
		       -1);
}

/* Like Fwrite_region, except that if DESC is nonnegative, it is a file
   descriptor for FILENAME, so do not open or close FILENAME.  */

Lisp_Object
write_region (Lisp_Object start, Lisp_Object end, Lisp_Object filename,
	      Lisp_Object append, Lisp_Object visit, Lisp_Object lockname,
	      Lisp_Object mustbenew, int desc)
{
  int open_flags;
  int mode;
  off_t offset UNINIT;
  bool open_and_close_file = desc < 0;
  bool ok;
  int save_errno = 0;
  const char *fn;
  struct stat st;
  struct timespec modtime;
  ptrdiff_t count = SPECPDL_INDEX ();
  ptrdiff_t count1 UNINIT;
  Lisp_Object handler;
  Lisp_Object visit_file;
  Lisp_Object annotations;
  Lisp_Object encoded_filename;
  bool visiting = (EQ (visit, Qt) || STRINGP (visit));
  bool quietly = !NILP (visit);
  bool file_locked = 0;
  struct buffer *given_buffer;
  struct coding_system coding;

  if (current_buffer->base_buffer && visiting)
    error ("Cannot do file visiting in an indirect buffer");

  if (!NILP (start) && !STRINGP (start))
    validate_region (&start, &end);

  visit_file = Qnil;

  filename = Fexpand_file_name (filename, Qnil);

  if (!NILP (mustbenew) && !EQ (mustbenew, Qexcl))
    barf_or_query_if_file_exists (filename, false, "overwrite", true, true);

  if (STRINGP (visit))
    visit_file = Fexpand_file_name (visit, Qnil);
  else
    visit_file = filename;

  if (NILP (lockname))
    lockname = visit_file;

  annotations = Qnil;

  /* If the file name has special constructs in it,
     call the corresponding file handler.  */
  handler = Ffind_file_name_handler (filename, Qwrite_region);
  /* If FILENAME has no handler, see if VISIT has one.  */
  if (NILP (handler) && STRINGP (visit))
    handler = Ffind_file_name_handler (visit, Qwrite_region);

  if (!NILP (handler))
    {
      Lisp_Object val;
      val = call8 (handler, Qwrite_region, start, end,
		   filename, append, visit, lockname, mustbenew);

      if (visiting)
	{
	  SAVE_MODIFF = MODIFF;
	  XSETFASTINT (BVAR (current_buffer, save_length), Z - BEG);
	  bset_filename (current_buffer, visit_file);
	}

      return val;
    }

  record_unwind_protect (save_restriction_restore, save_restriction_save ());

  /* Special kludge to simplify auto-saving.  */
  if (NILP (start))
    {
      /* Do it later, so write-region-annotate-function can work differently
	 if we save "the buffer" vs "a region".
	 This is useful in tar-mode.  --Stef
      XSETFASTINT (start, BEG);
      XSETFASTINT (end, Z); */
      Fwiden ();
    }

  record_unwind_protect (build_annotations_unwind,
			 Vwrite_region_annotation_buffers);
  Vwrite_region_annotation_buffers = list1 (Fcurrent_buffer ());

  given_buffer = current_buffer;

  if (!STRINGP (start))
    {
      annotations = build_annotations (start, end);

      if (current_buffer != given_buffer)
	{
	  XSETFASTINT (start, BEGV);
	  XSETFASTINT (end, ZV);
	}
    }

  if (NILP (start))
    {
      XSETFASTINT (start, BEGV);
      XSETFASTINT (end, ZV);
    }

  /* Decide the coding-system to encode the data with.
     We used to make this choice before calling build_annotations, but that
     leads to problems when a write-annotate-function takes care of
     unsavable chars (as was the case with X-Symbol).  */
  Vlast_coding_system_used
    = choose_write_coding_system (start, end, filename,
                                 append, visit, lockname, &coding);

  if (open_and_close_file && !auto_saving)
    {
      lock_file (lockname);
      file_locked = 1;
    }

  encoded_filename = ENCODE_FILE (filename);
  fn = SSDATA (encoded_filename);
  open_flags = O_WRONLY | O_CREAT;
  open_flags |= EQ (mustbenew, Qexcl) ? O_EXCL : !NILP (append) ? 0 : O_TRUNC;
  if (NUMBERP (append))
    offset = file_offset (append);
  else if (!NILP (append))
    open_flags |= O_APPEND;
#ifdef DOS_NT
  mode = S_IREAD | S_IWRITE;
#else
  mode = auto_saving ? auto_save_mode_bits : 0666;
#endif

  if (open_and_close_file)
    {
      desc = emacs_open (fn, open_flags, mode);
      if (desc < 0)
	{
	  int open_errno = errno;
	  if (file_locked)
	    unlock_file (lockname);
	  report_file_errno ("Opening output file", filename, open_errno);
	}

      count1 = SPECPDL_INDEX ();
      record_unwind_protect_int (close_file_unwind, desc);
    }

  if (NUMBERP (append))
    {
      off_t ret = lseek (desc, offset, SEEK_SET);
      if (ret < 0)
	{
	  int lseek_errno = errno;
	  if (file_locked)
	    unlock_file (lockname);
	  report_file_errno ("Lseek error", filename, lseek_errno);
	}
    }

  if (STRINGP (start))
    ok = a_write (desc, start, 0, SCHARS (start), &annotations, &coding);
  else if (XINT (start) != XINT (end))
    ok = a_write (desc, Qnil, XINT (start), XINT (end) - XINT (start),
		  &annotations, &coding);
  else
    {
      /* If file was empty, still need to write the annotations.  */
      coding.mode |= CODING_MODE_LAST_BLOCK;
      ok = a_write (desc, Qnil, XINT (end), 0, &annotations, &coding);
    }
  save_errno = errno;

  if (ok && CODING_REQUIRE_FLUSHING (&coding)
      && !(coding.mode & CODING_MODE_LAST_BLOCK))
    {
      /* We have to flush out a data. */
      coding.mode |= CODING_MODE_LAST_BLOCK;
      ok = e_write (desc, Qnil, 1, 1, &coding);
      save_errno = errno;
    }

  /* fsync is not crucial for temporary files.  Nor for auto-save
     files, since they might lose some work anyway.  */
  if (open_and_close_file && !auto_saving && !write_region_inhibit_fsync)
    {
      /* Transfer data and metadata to disk, retrying if interrupted.
	 fsync can report a write failure here, e.g., due to disk full
	 under NFS.  But ignore EINVAL, which means fsync is not
	 supported on this file.  */
      while (fsync (desc) != 0)
	if (errno != EINTR)
	  {
	    if (errno != EINVAL)
	      ok = 0, save_errno = errno;
	    break;
	  }
    }

  modtime = invalid_timespec ();
  if (visiting)
    {
      if (fstat (desc, &st) == 0)
	modtime = get_stat_mtime (&st);
      else
	ok = 0, save_errno = errno;
    }

  if (open_and_close_file)
    {
      /* NFS can report a write failure now.  */
      if (emacs_close (desc) < 0)
	ok = 0, save_errno = errno;

      /* Discard the unwind protect for close_file_unwind.  */
      specpdl_ptr = specpdl + count1;
    }

  /* Some file systems have a bug where st_mtime is not updated
     properly after a write.  For example, CIFS might not see the
     st_mtime change until after the file is opened again.

     Attempt to detect this file system bug, and update MODTIME to the
     newer st_mtime if the bug appears to be present.  This introduces
     a race condition, so to avoid most instances of the race condition
     on non-buggy file systems, skip this check if the most recently
     encountered non-buggy file system was the current file system.

     A race condition can occur if some other process modifies the
     file between the fstat above and the fstat below, but the race is
     unlikely and a similar race between the last write and the fstat
     above cannot possibly be closed anyway.  */

  if (timespec_valid_p (modtime)
      && ! (valid_timestamp_file_system && st.st_dev == timestamp_file_system))
    {
      int desc1 = emacs_open (fn, O_WRONLY, 0);
      if (desc1 >= 0)
	{
	  struct stat st1;
	  if (fstat (desc1, &st1) == 0
	      && st.st_dev == st1.st_dev && st.st_ino == st1.st_ino)
	    {
	      /* Use the heuristic if it appears to be valid.  With neither
		 O_EXCL nor O_TRUNC, if Emacs happened to write nothing to the
		 file, the time stamp won't change.  Also, some non-POSIX
		 systems don't update an empty file's time stamp when
		 truncating it.  Finally, file systems with 100 ns or worse
		 resolution sometimes seem to have bugs: on a system with ns
		 resolution, checking ns % 100 incorrectly avoids the heuristic
		 1% of the time, but the problem should be temporary as we will
		 try again on the next time stamp.  */
	      bool use_heuristic
		= ((open_flags & (O_EXCL | O_TRUNC)) != 0
		   && st.st_size != 0
		   && modtime.tv_nsec % 100 != 0);

	      struct timespec modtime1 = get_stat_mtime (&st1);
	      if (use_heuristic
		  && timespec_cmp (modtime, modtime1) == 0
		  && st.st_size == st1.st_size)
		{
		  timestamp_file_system = st.st_dev;
		  valid_timestamp_file_system = 1;
		}
	      else
		{
		  st.st_size = st1.st_size;
		  modtime = modtime1;
		}
	    }
	  emacs_close (desc1);
	}
    }

  /* Call write-region-post-annotation-function. */
  while (CONSP (Vwrite_region_annotation_buffers))
    {
      Lisp_Object buf = XCAR (Vwrite_region_annotation_buffers);
      if (!NILP (Fbuffer_live_p (buf)))
  	{
  	  Fset_buffer (buf);
  	  if (FUNCTIONP (Vwrite_region_post_annotation_function))
  	    call0 (Vwrite_region_post_annotation_function);
  	}
      Vwrite_region_annotation_buffers
  	= XCDR (Vwrite_region_annotation_buffers);
    }

  unbind_to (count, Qnil);

  if (file_locked)
    unlock_file (lockname);

  /* Do this before reporting IO error
     to avoid a "file has changed on disk" warning on
     next attempt to save.  */
  if (timespec_valid_p (modtime))
    {
      current_buffer->modtime = modtime;
      current_buffer->modtime_size = st.st_size;
    }

  if (! ok)
    report_file_errno ("Write error", filename, save_errno);

  bool auto_saving_into_visited_file =
    auto_saving
    && ! NILP (Fstring_equal (BVAR (current_buffer, filename),
			      BVAR (current_buffer, auto_save_file_name)));
  if (visiting)
    {
      SAVE_MODIFF = MODIFF;
      XSETFASTINT (BVAR (current_buffer, save_length), Z - BEG);
      bset_filename (current_buffer, visit_file);
      update_mode_lines = 14;
      if (auto_saving_into_visited_file)
	unlock_file (lockname);
    }
  else if (quietly)
    {
      if (auto_saving_into_visited_file)
	{
	  SAVE_MODIFF = MODIFF;
	  unlock_file (lockname);
	}

      return Qnil;
    }

  if (!auto_saving && !noninteractive)
    message_with_string ((NUMBERP (append)
			  ? "Updated %s"
			  : ! NILP (append)
			  ? "Added to %s"
			  : "Wrote %s"),
			 visit_file, 1);

  return Qnil;
}

DEFUN ("car-less-than-car", Fcar_less_than_car, Scar_less_than_car, 2, 2, 0,
       doc: /* Return t if (car A) is numerically less than (car B).  */)
  (Lisp_Object a, Lisp_Object b)
{
  return arithcompare (Fcar (a), Fcar (b), ARITH_LESS);
}

/* Build the complete list of annotations appropriate for writing out
   the text between START and END, by calling all the functions in
   write-region-annotate-functions and merging the lists they return.
   If one of these functions switches to a different buffer, we assume
   that buffer contains altered text.  Therefore, the caller must
   make sure to restore the current buffer in all cases,
   as save-excursion would do.  */

static Lisp_Object
build_annotations (Lisp_Object start, Lisp_Object end)
{
  Lisp_Object annotations;
  Lisp_Object p, res;
  Lisp_Object original_buffer;
  int i;
  bool used_global = false;

  XSETBUFFER (original_buffer, current_buffer);

  annotations = Qnil;
  p = Vwrite_region_annotate_functions;
  while (CONSP (p))
    {
      struct buffer *given_buffer = current_buffer;
      if (EQ (Qt, XCAR (p)) && !used_global)
	{ /* Use the global value of the hook.  */
	  used_global = true;
	  p = CALLN (Fappend,
		     Fdefault_value (Qwrite_region_annotate_functions),
		     XCDR (p));
	  continue;
	}
      Vwrite_region_annotations_so_far = annotations;
      res = call2 (XCAR (p), start, end);
      /* If the function makes a different buffer current,
	 assume that means this buffer contains altered text to be output.
	 Reset START and END from the buffer bounds
	 and discard all previous annotations because they should have
	 been dealt with by this function.  */
      if (current_buffer != given_buffer)
	{
	  Vwrite_region_annotation_buffers
	    = Fcons (Fcurrent_buffer (),
		     Vwrite_region_annotation_buffers);
	  XSETFASTINT (start, BEGV);
	  XSETFASTINT (end, ZV);
	  annotations = Qnil;
	}
      Flength (res);   /* Check basic validity of return value */
      annotations = merge (annotations, res, Qcar_less_than_car);
      p = XCDR (p);
    }

  /* Now do the same for annotation functions implied by the file-format */
  if (auto_saving && (!EQ (BVAR (current_buffer, auto_save_file_format), Qt)))
    p = BVAR (current_buffer, auto_save_file_format);
  else
    p = BVAR (current_buffer, file_format);
  for (i = 0; CONSP (p); p = XCDR (p), ++i)
    {
      struct buffer *given_buffer = current_buffer;

      Vwrite_region_annotations_so_far = annotations;

      /* Value is either a list of annotations or nil if the function
         has written annotations to a temporary buffer, which is now
         current.  */
      res = call5 (Qformat_annotate_function, XCAR (p), start, end,
		   original_buffer, make_number (i));
      if (current_buffer != given_buffer)
	{
	  XSETFASTINT (start, BEGV);
	  XSETFASTINT (end, ZV);
	  annotations = Qnil;
	}

      if (CONSP (res))
	annotations = merge (annotations, res, Qcar_less_than_car);
    }

  return annotations;
}


/* Write to descriptor DESC the NCHARS chars starting at POS of STRING.
   If STRING is nil, POS is the character position in the current buffer.
   Intersperse with them the annotations from *ANNOT
   which fall within the range of POS to POS + NCHARS,
   each at its appropriate position.

   We modify *ANNOT by discarding elements as we use them up.

   Return true if successful.  */

static bool
a_write (int desc, Lisp_Object string, ptrdiff_t pos,
	 ptrdiff_t nchars, Lisp_Object *annot,
	 struct coding_system *coding)
{
  Lisp_Object tem;
  ptrdiff_t nextpos;
  ptrdiff_t lastpos = pos + nchars;

  while (NILP (*annot) || CONSP (*annot))
    {
      tem = Fcar_safe (Fcar (*annot));
      nextpos = pos - 1;
      if (INTEGERP (tem))
	nextpos = XFASTINT (tem);

      /* If there are no more annotations in this range,
	 output the rest of the range all at once.  */
      if (! (nextpos >= pos && nextpos <= lastpos))
	return e_write (desc, string, pos, lastpos, coding);

      /* Output buffer text up to the next annotation's position.  */
      if (nextpos > pos)
	{
	  if (!e_write (desc, string, pos, nextpos, coding))
	    return 0;
	  pos = nextpos;
	}
      /* Output the annotation.  */
      tem = Fcdr (Fcar (*annot));
      if (STRINGP (tem))
	{
	  if (!e_write (desc, tem, 0, SCHARS (tem), coding))
	    return 0;
	}
      *annot = Fcdr (*annot);
    }
  return 1;
}

/* Maximum number of characters that the next
   function encodes per one loop iteration.  */

enum { E_WRITE_MAX = 8 * 1024 * 1024 };

/* Write text in the range START and END into descriptor DESC,
   encoding them with coding system CODING.  If STRING is nil, START
   and END are character positions of the current buffer, else they
   are indexes to the string STRING.  Return true if successful.  */

static bool
e_write (int desc, Lisp_Object string, ptrdiff_t start, ptrdiff_t end,
	 struct coding_system *coding)
{
  if (STRINGP (string))
    {
      start = 0;
      end = SCHARS (string);
    }

  /* We used to have a code for handling selective display here.  But,
     now it is handled within encode_coding.  */

  while (start < end)
    {
      if (STRINGP (string))
	{
	  coding->src_multibyte = SCHARS (string) < SBYTES (string);
	  if (CODING_REQUIRE_ENCODING (coding))
	    {
	      ptrdiff_t nchars = min (end - start, E_WRITE_MAX);

	      /* Avoid creating huge Lisp string in encode_coding_object.  */
	      if (nchars == E_WRITE_MAX)
		coding->raw_destination = 1;

	      encode_coding_object
		(coding, string, start, string_char_to_byte (string, start),
		 start + nchars, string_char_to_byte (string, start + nchars),
		 Qt);
	    }
	  else
	    {
	      coding->dst_object = string;
	      coding->consumed_char = SCHARS (string);
	      coding->produced = SBYTES (string);
	    }
	}
      else
	{
	  ptrdiff_t start_byte = CHAR_TO_BYTE (start);
	  ptrdiff_t end_byte = CHAR_TO_BYTE (end);

	  coding->src_multibyte = (end - start) < (end_byte - start_byte);
	  if (CODING_REQUIRE_ENCODING (coding))
	    {
	      ptrdiff_t nchars = min (end - start, E_WRITE_MAX);

	      /* Likewise.  */
	      if (nchars == E_WRITE_MAX)
		coding->raw_destination = 1;

	      encode_coding_object
		(coding, Fcurrent_buffer (), start, start_byte,
		 start + nchars, CHAR_TO_BYTE (start + nchars), Qt);
	    }
	  else
	    {
	      coding->dst_object = Qnil;
	      coding->dst_pos_byte = start_byte;
	      if (start >= GPT || end <= GPT)
		{
		  coding->consumed_char = end - start;
		  coding->produced = end_byte - start_byte;
		}
	      else
		{
		  coding->consumed_char = GPT - start;
		  coding->produced = GPT_BYTE - start_byte;
		}
	    }
	}

      if (coding->produced > 0)
	{
	  char *buf = (coding->raw_destination ? (char *) coding->destination
		       : (STRINGP (coding->dst_object)
			  ? SSDATA (coding->dst_object)
			  : (char *) BYTE_POS_ADDR (coding->dst_pos_byte)));
	  coding->produced -= emacs_write_quit (desc, buf, coding->produced);

	  if (coding->raw_destination)
	    {
	      /* We're responsible for freeing this, see
		 encode_coding_object to check why.  */
	      xfree (coding->destination);
	      coding->raw_destination = 0;
	    }
	  if (coding->produced)
	    return 0;
	}
      start += coding->consumed_char;
    }

  return 1;
}

DEFUN ("verify-visited-file-modtime", Fverify_visited_file_modtime,
       Sverify_visited_file_modtime, 0, 1, 0,
       doc: /* Return t if last mod time of BUF's visited file matches what BUF records.
This means that the file has not been changed since it was visited or saved.
If BUF is omitted or nil, it defaults to the current buffer.
See Info node `(elisp)Modification Time' for more details.  */)
  (Lisp_Object buf)
{
  struct buffer *b = decode_buffer (buf);
  struct stat st;
  Lisp_Object handler;
  Lisp_Object filename;
  struct timespec mtime;

  if (!STRINGP (BVAR (b, filename))) return Qt;
  if (b->modtime.tv_nsec == UNKNOWN_MODTIME_NSECS) return Qt;

  /* If the file name has special constructs in it,
     call the corresponding file handler.  */
  handler = Ffind_file_name_handler (BVAR (b, filename),
				     Qverify_visited_file_modtime);
  if (!NILP (handler))
    return call2 (handler, Qverify_visited_file_modtime, buf);

  filename = ENCODE_FILE (BVAR (b, filename));

  mtime = (stat (SSDATA (filename), &st) == 0
	   ? get_stat_mtime (&st)
	   : time_error_value (errno));
  if (timespec_cmp (mtime, b->modtime) == 0
      && (b->modtime_size < 0
	  || st.st_size == b->modtime_size))
    return Qt;
  return Qnil;
}

DEFUN ("visited-file-modtime", Fvisited_file_modtime,
       Svisited_file_modtime, 0, 0, 0,
       doc: /* Return the current buffer's recorded visited file modification time.
The value is a list of the form (HIGH LOW USEC PSEC), like the time values that
`file-attributes' returns.  If the current buffer has no recorded file
modification time, this function returns 0.  If the visited file
doesn't exist, return -1.
See Info node `(elisp)Modification Time' for more details.  */)
  (void)
{
  int ns = current_buffer->modtime.tv_nsec;
  if (ns < 0)
    return make_number (UNKNOWN_MODTIME_NSECS - ns);
  return make_lisp_time (current_buffer->modtime);
}

DEFUN ("set-visited-file-modtime", Fset_visited_file_modtime,
       Sset_visited_file_modtime, 0, 1, 0,
       doc: /* Update buffer's recorded modification time from the visited file's time.
Useful if the buffer was not read from the file normally
or if the file itself has been changed for some known benign reason.
An argument specifies the modification time value to use
\(instead of that of the visited file), in the form of a list
\(HIGH LOW USEC PSEC) or an integer flag as returned by
`visited-file-modtime'.  */)
  (Lisp_Object time_flag)
{
  if (!NILP (time_flag))
    {
      struct timespec mtime;
      if (INTEGERP (time_flag))
	{
	  CHECK_RANGED_INTEGER (time_flag, -1, 0);
	  mtime = make_timespec (0, UNKNOWN_MODTIME_NSECS - XINT (time_flag));
	}
      else
	mtime = lisp_time_argument (time_flag);

      current_buffer->modtime = mtime;
      current_buffer->modtime_size = -1;
    }
  else
    {
      register Lisp_Object filename;
      struct stat st;
      Lisp_Object handler;

      filename = Fexpand_file_name (BVAR (current_buffer, filename), Qnil);

      /* If the file name has special constructs in it,
	 call the corresponding file handler.  */
      handler = Ffind_file_name_handler (filename, Qset_visited_file_modtime);
      if (!NILP (handler))
	/* The handler can find the file name the same way we did.  */
	return call2 (handler, Qset_visited_file_modtime, Qnil);

      filename = ENCODE_FILE (filename);

      if (stat (SSDATA (filename), &st) >= 0)
        {
	  current_buffer->modtime = get_stat_mtime (&st);
          current_buffer->modtime_size = st.st_size;
        }
    }

  return Qnil;
}

static Lisp_Object
auto_save_error (Lisp_Object error_val)
{
  auto_save_error_occurred = 1;

  ring_bell (XFRAME (selected_frame));

  AUTO_STRING (format, "Auto-saving %s: %s");
  Lisp_Object msg = CALLN (Fformat, format, BVAR (current_buffer, name),
			   Ferror_message_string (error_val));
  call3 (intern ("display-warning"),
         intern ("auto-save"), msg, intern ("error"));

  return Qnil;
}

static Lisp_Object
auto_save_1 (void)
{
  struct stat st;
  Lisp_Object modes;

  auto_save_mode_bits = 0666;

  /* Get visited file's mode to become the auto save file's mode.  */
  if (! NILP (BVAR (current_buffer, filename)))
    {
      if (stat (SSDATA (BVAR (current_buffer, filename)), &st) >= 0)
	/* But make sure we can overwrite it later!  */
	auto_save_mode_bits = (st.st_mode | 0600) & 0777;
      else if (modes = Ffile_modes (BVAR (current_buffer, filename)),
	       INTEGERP (modes))
	/* Remote files don't cooperate with stat.  */
	auto_save_mode_bits = (XINT (modes) | 0600) & 0777;
    }

  return
    Fwrite_region (Qnil, Qnil, BVAR (current_buffer, auto_save_file_name), Qnil,
		   NILP (Vauto_save_visited_file_name) ? Qlambda : Qt,
		   Qnil, Qnil);
}

struct auto_save_unwind
{
  FILE *stream;
  bool auto_raise;
};

static void
do_auto_save_unwind (void *arg)
{
  struct auto_save_unwind *p = arg;
  FILE *stream = p->stream;
  minibuffer_auto_raise = p->auto_raise;
  auto_saving = 0;
  if (stream != NULL)
    {
      block_input ();
      fclose (stream);
      unblock_input ();
    }
}

static Lisp_Object
do_auto_save_make_dir (Lisp_Object dir)
{
  Lisp_Object result;

  auto_saving_dir_umask = 077;
  result = call2 (Qmake_directory, dir, Qt);
  auto_saving_dir_umask = 0;
  return result;
}

static Lisp_Object
do_auto_save_eh (Lisp_Object ignore)
{
  auto_saving_dir_umask = 0;
  return Qnil;
}

DEFUN ("do-auto-save", Fdo_auto_save, Sdo_auto_save, 0, 2, "",
       doc: /* Auto-save all buffers that need it.
This is all buffers that have auto-saving enabled
and are changed since last auto-saved.
Auto-saving writes the buffer into a file
so that your editing is not lost if the system crashes.
This file is not the file you visited; that changes only when you save.
Normally, run the normal hook `auto-save-hook' before saving.

A non-nil NO-MESSAGE argument means do not print any message if successful.
A non-nil CURRENT-ONLY argument means save only current buffer.  */)
  (Lisp_Object no_message, Lisp_Object current_only)
{
  struct buffer *old = current_buffer, *b;
  Lisp_Object tail, buf, hook;
  bool auto_saved = 0;
  int do_handled_files;
  Lisp_Object oquit;
  FILE *stream = NULL;
  ptrdiff_t count = SPECPDL_INDEX ();
  bool orig_minibuffer_auto_raise = minibuffer_auto_raise;
  bool old_message_p = 0;
  struct auto_save_unwind auto_save_unwind;

  if (max_specpdl_size < specpdl_size + 40)
    max_specpdl_size = specpdl_size + 40;

  if (minibuf_level)
    no_message = Qt;

  if (NILP (no_message))
    {
      old_message_p = push_message ();
      record_unwind_protect_void (pop_message_unwind);
    }

  /* Ordinarily don't quit within this function,
     but don't make it impossible to quit (in case we get hung in I/O).  */
  oquit = Vquit_flag;
  Vquit_flag = Qnil;

  hook = intern ("auto-save-hook");
  safe_run_hooks (hook);

  if (STRINGP (Vauto_save_list_file_name))
    {
      Lisp_Object listfile;

      listfile = Fexpand_file_name (Vauto_save_list_file_name, Qnil);

      /* Don't try to create the directory when shutting down Emacs,
         because creating the directory might signal an error, and
         that would leave Emacs in a strange state.  */
      if (!NILP (Vrun_hooks))
	{
	  Lisp_Object dir;
	  dir = Ffile_name_directory (listfile);
	  if (NILP (Ffile_directory_p (dir)))
	    internal_condition_case_1 (do_auto_save_make_dir,
				       dir, Qt,
				       do_auto_save_eh);
	}

      stream = emacs_fopen (SSDATA (listfile), "w");
    }

  auto_save_unwind.stream = stream;
  auto_save_unwind.auto_raise = minibuffer_auto_raise;
  record_unwind_protect_ptr (do_auto_save_unwind, &auto_save_unwind);
  minibuffer_auto_raise = 0;
  auto_saving = 1;
  auto_save_error_occurred = 0;

  /* On first pass, save all files that don't have handlers.
     On second pass, save all files that do have handlers.

     If Emacs is crashing, the handlers may tweak what is causing
     Emacs to crash in the first place, and it would be a shame if
     Emacs failed to autosave perfectly ordinary files because it
     couldn't handle some ange-ftp'd file.  */

  for (do_handled_files = 0; do_handled_files < 2; do_handled_files++)
    FOR_EACH_LIVE_BUFFER (tail, buf)
      {
	b = XBUFFER (buf);

	/* Record all the buffers that have auto save mode
	   in the special file that lists them.  For each of these buffers,
	   Record visited name (if any) and auto save name.  */
	if (STRINGP (BVAR (b, auto_save_file_name))
	    && stream != NULL && do_handled_files == 0)
	  {
	    block_input ();
	    if (!NILP (BVAR (b, filename)))
	      fwrite_unlocked (SDATA (BVAR (b, filename)), 1,
			       SBYTES (BVAR (b, filename)), stream);
	    putc_unlocked ('\n', stream);
	    fwrite_unlocked (SDATA (BVAR (b, auto_save_file_name)), 1,
			     SBYTES (BVAR (b, auto_save_file_name)), stream);
	    putc_unlocked ('\n', stream);
	    unblock_input ();
	  }

	if (!NILP (current_only)
	    && b != current_buffer)
	  continue;

	/* Don't auto-save indirect buffers.
	   The base buffer takes care of it.  */
	if (b->base_buffer)
	  continue;

	/* Check for auto save enabled
	   and file changed since last auto save
	   and file changed since last real save.  */
	if (STRINGP (BVAR (b, auto_save_file_name))
	    && BUF_SAVE_MODIFF (b) < BUF_MODIFF (b)
	    && BUF_AUTOSAVE_MODIFF (b) < BUF_MODIFF (b)
	    /* -1 means we've turned off autosaving for a while--see below.  */
	    && XINT (BVAR (b, save_length)) >= 0
	    && (do_handled_files
		|| NILP (Ffind_file_name_handler (BVAR (b, auto_save_file_name),
						  Qwrite_region))))
	  {
	    struct timespec before_time = current_timespec ();
	    struct timespec after_time;

	    /* If we had a failure, don't try again for 20 minutes.  */
	    if (b->auto_save_failure_time > 0
		&& before_time.tv_sec - b->auto_save_failure_time < 1200)
	      continue;

	    set_buffer_internal (b);
	    if (NILP (Vauto_save_include_big_deletions)
		&& (XFASTINT (BVAR (b, save_length)) * 10
		    > (BUF_Z (b) - BUF_BEG (b)) * 13)
		/* A short file is likely to change a large fraction;
		   spare the user annoying messages.  */
		&& XFASTINT (BVAR (b, save_length)) > 5000
		/* These messages are frequent and annoying for `*mail*'.  */
		&& !EQ (BVAR (b, filename), Qnil)
		&& NILP (no_message))
	      {
		/* It has shrunk too much; turn off auto-saving here.  */
		minibuffer_auto_raise = orig_minibuffer_auto_raise;
		message_with_string ("Buffer %s has shrunk a lot; auto save disabled in that buffer until next real save",
				     BVAR (b, name), 1);
		minibuffer_auto_raise = 0;
		/* Turn off auto-saving until there's a real save,
		   and prevent any more warnings.  */
		XSETINT (BVAR (b, save_length), -1);
		Fsleep_for (make_number (1), Qnil);
		continue;
	      }
	    if (!auto_saved && NILP (no_message))
	      message1 ("Auto-saving...");
	    internal_condition_case (auto_save_1, Qt, auto_save_error);
	    auto_saved = 1;
	    BUF_AUTOSAVE_MODIFF (b) = BUF_MODIFF (b);
	    XSETFASTINT (BVAR (current_buffer, save_length), Z - BEG);
	    set_buffer_internal (old);

	    after_time = current_timespec ();

	    /* If auto-save took more than 60 seconds,
	       assume it was an NFS failure that got a timeout.  */
	    if (after_time.tv_sec - before_time.tv_sec > 60)
	      b->auto_save_failure_time = after_time.tv_sec;
	  }
      }

  /* Prevent another auto save till enough input events come in.  */
  record_auto_save ();

  if (auto_saved && NILP (no_message))
    {
      if (old_message_p)
	{
	  /* If we are going to restore an old message,
	     give time to read ours.  */
	  sit_for (make_number (1), 0, 0);
	  restore_message ();
	}
      else if (!auto_save_error_occurred)
	/* Don't overwrite the error message if an error occurred.
	   If we displayed a message and then restored a state
	   with no message, leave a "done" message on the screen.  */
	message1 ("Auto-saving...done");
    }

  Vquit_flag = oquit;

  /* This restores the message-stack status.  */
  unbind_to (count, Qnil);
  return Qnil;
}

DEFUN ("set-buffer-auto-saved", Fset_buffer_auto_saved,
       Sset_buffer_auto_saved, 0, 0, 0,
       doc: /* Mark current buffer as auto-saved with its current text.
No auto-save file will be written until the buffer changes again.  */)
  (void)
{
  /* FIXME: This should not be called in indirect buffers, since
     they're not autosaved.  */
  BUF_AUTOSAVE_MODIFF (current_buffer) = MODIFF;
  XSETFASTINT (BVAR (current_buffer, save_length), Z - BEG);
  current_buffer->auto_save_failure_time = 0;
  return Qnil;
}

DEFUN ("clear-buffer-auto-save-failure", Fclear_buffer_auto_save_failure,
       Sclear_buffer_auto_save_failure, 0, 0, 0,
       doc: /* Clear any record of a recent auto-save failure in the current buffer.  */)
  (void)
{
  current_buffer->auto_save_failure_time = 0;
  return Qnil;
}

DEFUN ("recent-auto-save-p", Frecent_auto_save_p, Srecent_auto_save_p,
       0, 0, 0,
       doc: /* Return t if current buffer has been auto-saved recently.
More precisely, if it has been auto-saved since last read from or saved
in the visited file.  If the buffer has no visited file,
then any auto-save counts as "recent".  */)
  (void)
{
  /* FIXME: maybe we should return nil for indirect buffers since
     they're never autosaved.  */
  return (SAVE_MODIFF < BUF_AUTOSAVE_MODIFF (current_buffer) ? Qt : Qnil);
}

/* Reading and completing file names.  */

DEFUN ("next-read-file-uses-dialog-p", Fnext_read_file_uses_dialog_p,
       Snext_read_file_uses_dialog_p, 0, 0, 0,
       doc: /* Return t if a call to `read-file-name' will use a dialog.
The return value is only relevant for a call to `read-file-name' that happens
before any other event (mouse or keypress) is handled.  */)
  (void)
{
#if (defined USE_GTK || defined USE_MOTIF \
     || defined HAVE_NS || defined HAVE_NTGUI)
  if ((NILP (last_nonmenu_event) || CONSP (last_nonmenu_event))
      && use_dialog_box
      && use_file_dialog
      && window_system_available (SELECTED_FRAME ()))
    return Qt;
#endif
  return Qnil;
}


DEFUN ("set-binary-mode", Fset_binary_mode, Sset_binary_mode, 2, 2, 0,
       doc: /* Switch STREAM to binary I/O mode or text I/O mode.
STREAM can be one of the symbols `stdin', `stdout', or `stderr'.
If MODE is non-nil, switch STREAM to binary mode, otherwise switch
it to text mode.

As a side effect, this function flushes any pending STREAM's data.

Value is the previous value of STREAM's I/O mode, nil for text mode,
non-nil for binary mode.

On MS-Windows and MS-DOS, binary mode is needed to read or write
arbitrary binary data, and for disabling translation between CR-LF
pairs and a single newline character.  Examples include generation
of text files with Unix-style end-of-line format using `princ' in
batch mode, with standard output redirected to a file.

On Posix systems, this function always returns non-nil, and has no
effect except for flushing STREAM's data.  */)
  (Lisp_Object stream, Lisp_Object mode)
{
  FILE *fp = NULL;
  int binmode;

  CHECK_SYMBOL (stream);
  if (EQ (stream, Qstdin))
    fp = stdin;
  else if (EQ (stream, Qstdout))
    fp = stdout;
  else if (EQ (stream, Qstderr))
    fp = stderr;
  else
    xsignal2 (Qerror, build_string ("unsupported stream"), stream);

  binmode = NILP (mode) ? O_TEXT : O_BINARY;
  if (fp != stdin)
    fflush_unlocked (fp);

  return (set_binary_mode (fileno (fp), binmode) == O_BINARY) ? Qt : Qnil;
}

#ifndef DOS_NT

/* Yield a Lisp float as close as possible to BLOCKSIZE * BLOCKS, with
   the result negated if NEGATE.  */
static Lisp_Object
blocks_to_bytes (uintmax_t blocksize, uintmax_t blocks, bool negate)
{
  /* On typical platforms the following code is accurate to 53 bits,
     which is close enough.  BLOCKSIZE is invariably a power of 2, so
     converting it to double does not lose information.  */
  double bs = blocksize;
  return make_float (negate ? -bs * -blocks : bs * blocks);
}

DEFUN ("file-system-info", Ffile_system_info, Sfile_system_info, 1, 1, 0,
       doc: /* Return storage information about the file system FILENAME is on.
Value is a list of numbers (TOTAL FREE AVAIL), where TOTAL is the total
storage of the file system, FREE is the free storage, and AVAIL is the
storage available to a non-superuser.  All 3 numbers are in bytes.
If the underlying system call fails, value is nil.  */)
  (Lisp_Object filename)
{
  Lisp_Object encoded = ENCODE_FILE (Fexpand_file_name (filename, Qnil));

  /* If the file name has special constructs in it,
     call the corresponding file handler.  */
  Lisp_Object handler = Ffind_file_name_handler (encoded, Qfile_system_info);
  if (!NILP (handler))
    {
      Lisp_Object result = call2 (handler, Qfile_system_info, encoded);
      if (CONSP (result) || NILP (result))
	return result;
      error ("Invalid handler in `file-name-handler-alist'");
    }

  struct fs_usage u;
  if (get_fs_usage (SSDATA (encoded), NULL, &u) != 0)
    return Qnil;
  return list3 (blocks_to_bytes (u.fsu_blocksize, u.fsu_blocks, false),
		blocks_to_bytes (u.fsu_blocksize, u.fsu_bfree, false),
		blocks_to_bytes (u.fsu_blocksize, u.fsu_bavail,
				 u.fsu_bavail_top_bit_set));
}

#endif /* !DOS_NT */

void
init_fileio (void)
{
  realmask = umask (0);
  umask (realmask);

  valid_timestamp_file_system = 0;

  /* fsync can be a significant performance hit.  Often it doesn't
     suffice to make the file-save operation survive a crash.  For
     batch scripts, which are typically part of larger shell commands
     that don't fsync other files, its effect on performance can be
     significant so its utility is particularly questionable.
     Hence, for now by default fsync is used only when interactive.

     For more on why fsync often fails to work on today's hardware, see:
     Zheng M et al. Understanding the robustness of SSDs under power fault.
     11th USENIX Conf. on File and Storage Technologies, 2013 (FAST '13), 271-84
     http://www.usenix.org/system/files/conference/fast13/fast13-final80.pdf

     For more on why fsync does not suffice even if it works properly, see:
     Roche X. Necessary step(s) to synchronize filename operations on disk.
     Austin Group Defect 672, 2013-03-19
     http://austingroupbugs.net/view.php?id=672  */
  write_region_inhibit_fsync = noninteractive;
}

void
syms_of_fileio (void)
{
  /* Property name of a file name handler,
     which gives a list of operations it handles.  */
  DEFSYM (Qoperations, "operations");

  DEFSYM (Qexpand_file_name, "expand-file-name");
  DEFSYM (Qsubstitute_in_file_name, "substitute-in-file-name");
  DEFSYM (Qdirectory_file_name, "directory-file-name");
  DEFSYM (Qfile_name_directory, "file-name-directory");
  DEFSYM (Qfile_name_nondirectory, "file-name-nondirectory");
  DEFSYM (Qunhandled_file_name_directory, "unhandled-file-name-directory");
  DEFSYM (Qfile_name_as_directory, "file-name-as-directory");
  DEFSYM (Qcopy_file, "copy-file");
  DEFSYM (Qmake_directory_internal, "make-directory-internal");
  DEFSYM (Qmake_directory, "make-directory");
  DEFSYM (Qdelete_file, "delete-file");
  DEFSYM (Qfile_name_case_insensitive_p, "file-name-case-insensitive-p");
  DEFSYM (Qrename_file, "rename-file");
  DEFSYM (Qadd_name_to_file, "add-name-to-file");
  DEFSYM (Qmake_symbolic_link, "make-symbolic-link");
  DEFSYM (Qfile_exists_p, "file-exists-p");
  DEFSYM (Qfile_executable_p, "file-executable-p");
  DEFSYM (Qfile_readable_p, "file-readable-p");
  DEFSYM (Qfile_writable_p, "file-writable-p");
  DEFSYM (Qfile_symlink_p, "file-symlink-p");
  DEFSYM (Qaccess_file, "access-file");
  DEFSYM (Qfile_directory_p, "file-directory-p");
  DEFSYM (Qfile_regular_p, "file-regular-p");
  DEFSYM (Qfile_accessible_directory_p, "file-accessible-directory-p");
  DEFSYM (Qfile_modes, "file-modes");
  DEFSYM (Qset_file_modes, "set-file-modes");
  DEFSYM (Qset_file_times, "set-file-times");
  DEFSYM (Qfile_selinux_context, "file-selinux-context");
  DEFSYM (Qset_file_selinux_context, "set-file-selinux-context");
  DEFSYM (Qfile_acl, "file-acl");
  DEFSYM (Qset_file_acl, "set-file-acl");
  DEFSYM (Qfile_newer_than_file_p, "file-newer-than-file-p");
  DEFSYM (Qinsert_file_contents, "insert-file-contents");
  DEFSYM (Qwrite_region, "write-region");
  DEFSYM (Qverify_visited_file_modtime, "verify-visited-file-modtime");
  DEFSYM (Qset_visited_file_modtime, "set-visited-file-modtime");
  DEFSYM (Qfile_system_info, "file-system-info");

  /* The symbol bound to coding-system-for-read when
     insert-file-contents is called for recovering a file.  This is not
     an actual coding system name, but just an indicator to tell
     insert-file-contents to use `emacs-mule' with a special flag for
     auto saving and recovering a file.  */
  DEFSYM (Qauto_save_coding, "auto-save-coding");

  DEFSYM (Qfile_name_history, "file-name-history");
  Fset (Qfile_name_history, Qnil);

  DEFSYM (Qfile_error, "file-error");
  DEFSYM (Qfile_already_exists, "file-already-exists");
  DEFSYM (Qfile_date_error, "file-date-error");
  DEFSYM (Qfile_missing, "file-missing");
  DEFSYM (Qfile_notify_error, "file-notify-error");
  DEFSYM (Qexcl, "excl");

  DEFVAR_LISP ("file-name-coding-system", Vfile_name_coding_system,
	       doc: /* Coding system for encoding file names.
If it is nil, `default-file-name-coding-system' (which see) is used.

On MS-Windows, the value of this variable is largely ignored if
`w32-unicode-filenames' (which see) is non-nil.  Emacs on Windows
behaves as if file names were encoded in `utf-8'.  */);
  Vfile_name_coding_system = Qnil;

  DEFVAR_LISP ("default-file-name-coding-system",
	       Vdefault_file_name_coding_system,
	       doc: /* Default coding system for encoding file names.
This variable is used only when `file-name-coding-system' is nil.

This variable is set/changed by the command `set-language-environment'.
User should not set this variable manually,
instead use `file-name-coding-system' to get a constant encoding
of file names regardless of the current language environment.

On MS-Windows, the value of this variable is largely ignored if
`w32-unicode-filenames' (which see) is non-nil.  Emacs on Windows
behaves as if file names were encoded in `utf-8'.  */);
  Vdefault_file_name_coding_system = Qnil;

  /* Lisp functions for translating file formats.  */
  DEFSYM (Qformat_decode, "format-decode");
  DEFSYM (Qformat_annotate_function, "format-annotate-function");

  /* Lisp function for setting buffer-file-coding-system and the
     multibyteness of the current buffer after inserting a file.  */
  DEFSYM (Qafter_insert_file_set_coding, "after-insert-file-set-coding");

  DEFSYM (Qcar_less_than_car, "car-less-than-car");

  Fput (Qfile_error, Qerror_conditions,
	Fpurecopy (list2 (Qfile_error, Qerror)));
  Fput (Qfile_error, Qerror_message,
	build_pure_c_string ("File error"));

  Fput (Qfile_already_exists, Qerror_conditions,
	Fpurecopy (list3 (Qfile_already_exists, Qfile_error, Qerror)));
  Fput (Qfile_already_exists, Qerror_message,
	build_pure_c_string ("File already exists"));

  Fput (Qfile_date_error, Qerror_conditions,
	Fpurecopy (list3 (Qfile_date_error, Qfile_error, Qerror)));
  Fput (Qfile_date_error, Qerror_message,
	build_pure_c_string ("Cannot set file date"));

  Fput (Qfile_missing, Qerror_conditions,
	Fpurecopy (list3 (Qfile_missing, Qfile_error, Qerror)));
  Fput (Qfile_missing, Qerror_message,
	build_pure_c_string ("File is missing"));

  Fput (Qfile_notify_error, Qerror_conditions,
	Fpurecopy (list3 (Qfile_notify_error, Qfile_error, Qerror)));
  Fput (Qfile_notify_error, Qerror_message,
	build_pure_c_string ("File notification error"));

  DEFVAR_LISP ("file-name-handler-alist", Vfile_name_handler_alist,
	       doc: /* Alist of elements (REGEXP . HANDLER) for file names handled specially.
If a file name matches REGEXP, all I/O on that file is done by calling
HANDLER.  If a file name matches more than one handler, the handler
whose match starts last in the file name gets precedence.  The
function `find-file-name-handler' checks this list for a handler for
its argument.

HANDLER should be a function.  The first argument given to it is the
name of the I/O primitive to be handled; the remaining arguments are
the arguments that were passed to that primitive.  For example, if you
do (file-exists-p FILENAME) and FILENAME is handled by HANDLER, then
HANDLER is called like this:

  (funcall HANDLER \\='file-exists-p FILENAME)

Note that HANDLER must be able to handle all I/O primitives; if it has
nothing special to do for a primitive, it should reinvoke the
primitive to handle the operation \"the usual way\".
See Info node `(elisp)Magic File Names' for more details.  */);
  Vfile_name_handler_alist = Qnil;

  DEFVAR_LISP ("set-auto-coding-function",
	       Vset_auto_coding_function,
	       doc: /* If non-nil, a function to call to decide a coding system of file.
Two arguments are passed to this function: the file name
and the length of a file contents following the point.
This function should return a coding system to decode the file contents.
It should check the file name against `auto-coding-alist'.
If no coding system is decided, it should check a coding system
specified in the heading lines with the format:
	-*- ... coding: CODING-SYSTEM; ... -*-
or local variable spec of the tailing lines with `coding:' tag.  */);
  Vset_auto_coding_function = Qnil;

  DEFVAR_LISP ("after-insert-file-functions", Vafter_insert_file_functions,
	       doc: /* A list of functions to be called at the end of `insert-file-contents'.
Each is passed one argument, the number of characters inserted,
with point at the start of the inserted text.  Each function
should leave point the same, and return the new character count.
If `insert-file-contents' is intercepted by a handler from
`file-name-handler-alist', that handler is responsible for calling the
functions in `after-insert-file-functions' if appropriate.  */);
  Vafter_insert_file_functions = Qnil;

  DEFVAR_LISP ("write-region-annotate-functions", Vwrite_region_annotate_functions,
	       doc: /* A list of functions to be called at the start of `write-region'.
Each is passed two arguments, START and END as for `write-region'.
These are usually two numbers but not always; see the documentation
for `write-region'.  The function should return a list of pairs
of the form (POSITION . STRING), consisting of strings to be effectively
inserted at the specified positions of the file being written (1 means to
insert before the first byte written).  The POSITIONs must be sorted into
increasing order.

If there are several annotation functions, the lists returned by these
functions are merged destructively.  As each annotation function runs,
the variable `write-region-annotations-so-far' contains a list of all
annotations returned by previous annotation functions.

An annotation function can return with a different buffer current.
Doing so removes the annotations returned by previous functions, and
resets START and END to `point-min' and `point-max' of the new buffer.

After `write-region' completes, Emacs calls the function stored in
`write-region-post-annotation-function', once for each buffer that was
current when building the annotations (i.e., at least once), with that
buffer current.  */);
  Vwrite_region_annotate_functions = Qnil;
  DEFSYM (Qwrite_region_annotate_functions, "write-region-annotate-functions");

  DEFVAR_LISP ("write-region-post-annotation-function",
	       Vwrite_region_post_annotation_function,
	       doc: /* Function to call after `write-region' completes.
The function is called with no arguments.  If one or more of the
annotation functions in `write-region-annotate-functions' changed the
current buffer, the function stored in this variable is called for
each of those additional buffers as well, in addition to the original
buffer.  The relevant buffer is current during each function call.  */);
  Vwrite_region_post_annotation_function = Qnil;
  staticpro (&Vwrite_region_annotation_buffers);

  DEFVAR_LISP ("write-region-annotations-so-far",
	       Vwrite_region_annotations_so_far,
	       doc: /* When an annotation function is called, this holds the previous annotations.
These are the annotations made by other annotation functions
that were already called.  See also `write-region-annotate-functions'.  */);
  Vwrite_region_annotations_so_far = Qnil;

  DEFVAR_LISP ("inhibit-file-name-handlers", Vinhibit_file_name_handlers,
	       doc: /* A list of file name handlers that temporarily should not be used.
This applies only to the operation `inhibit-file-name-operation'.  */);
  Vinhibit_file_name_handlers = Qnil;

  DEFVAR_LISP ("inhibit-file-name-operation", Vinhibit_file_name_operation,
	       doc: /* The operation for which `inhibit-file-name-handlers' is applicable.  */);
  Vinhibit_file_name_operation = Qnil;

  DEFVAR_LISP ("auto-save-list-file-name", Vauto_save_list_file_name,
	       doc: /* File name in which to write a list of all auto save file names.
This variable is initialized automatically from `auto-save-list-file-prefix'
shortly after Emacs reads your init file, if you have not yet given it
a non-nil value.  */);
  Vauto_save_list_file_name = Qnil;

  DEFVAR_LISP ("auto-save-visited-file-name", Vauto_save_visited_file_name,
	       doc: /* Non-nil says auto-save a buffer in the file it is visiting, when practical.
Normally auto-save files are written under other names.  */);
  Vauto_save_visited_file_name = Qnil;

  DEFVAR_LISP ("auto-save-include-big-deletions", Vauto_save_include_big_deletions,
	       doc: /* If non-nil, auto-save even if a large part of the text is deleted.
If nil, deleting a substantial portion of the text disables auto-save
in the buffer; this is the default behavior, because the auto-save
file is usually more useful if it contains the deleted text.  */);
  Vauto_save_include_big_deletions = Qnil;

  DEFVAR_BOOL ("write-region-inhibit-fsync", write_region_inhibit_fsync,
	       doc: /* Non-nil means don't call fsync in `write-region'.
This variable affects calls to `write-region' as well as save commands.
Setting this to nil may avoid data loss if the system loses power or
the operating system crashes.  By default, it is non-nil in batch mode.  */);
  write_region_inhibit_fsync = 0; /* See also `init_fileio' above.  */

  DEFVAR_BOOL ("delete-by-moving-to-trash", delete_by_moving_to_trash,
               doc: /* Specifies whether to use the system's trash can.
When non-nil, certain file deletion commands use the function
`move-file-to-trash' instead of deleting files outright.
This includes interactive calls to `delete-file' and
`delete-directory' and the Dired deletion commands.  */);
  delete_by_moving_to_trash = 0;
  DEFSYM (Qdelete_by_moving_to_trash, "delete-by-moving-to-trash");

  /* Lisp function for moving files to trash.  */
  DEFSYM (Qmove_file_to_trash, "move-file-to-trash");

  /* Lisp function for recursively copying directories.  */
  DEFSYM (Qcopy_directory, "copy-directory");

  /* Lisp function for recursively deleting directories.  */
  DEFSYM (Qdelete_directory, "delete-directory");

  DEFSYM (Qsubstitute_env_in_file_name, "substitute-env-in-file-name");
  DEFSYM (Qget_buffer_window_list, "get-buffer-window-list");

  DEFSYM (Qstdin, "stdin");
  DEFSYM (Qstdout, "stdout");
  DEFSYM (Qstderr, "stderr");

  defsubr (&Sfind_file_name_handler);
  defsubr (&Sfile_name_directory);
  defsubr (&Sfile_name_nondirectory);
  defsubr (&Sunhandled_file_name_directory);
  defsubr (&Sfile_name_as_directory);
  defsubr (&Sdirectory_name_p);
  defsubr (&Sdirectory_file_name);
  defsubr (&Smake_temp_file_internal);
  defsubr (&Smake_temp_name);
  defsubr (&Sexpand_file_name);
  defsubr (&Ssubstitute_in_file_name);
  defsubr (&Scopy_file);
  defsubr (&Smake_directory_internal);
  defsubr (&Sdelete_directory_internal);
  defsubr (&Sdelete_file);
  defsubr (&Sfile_name_case_insensitive_p);
  defsubr (&Srename_file);
  defsubr (&Sadd_name_to_file);
  defsubr (&Smake_symbolic_link);
  defsubr (&Sfile_name_absolute_p);
  defsubr (&Sfile_exists_p);
  defsubr (&Sfile_executable_p);
  defsubr (&Sfile_readable_p);
  defsubr (&Sfile_writable_p);
  defsubr (&Saccess_file);
  defsubr (&Sfile_symlink_p);
  defsubr (&Sfile_directory_p);
  defsubr (&Sfile_accessible_directory_p);
  defsubr (&Sfile_regular_p);
  defsubr (&Sfile_modes);
  defsubr (&Sset_file_modes);
  defsubr (&Sset_file_times);
  defsubr (&Sfile_selinux_context);
  defsubr (&Sfile_acl);
  defsubr (&Sset_file_acl);
  defsubr (&Sset_file_selinux_context);
  defsubr (&Sset_default_file_modes);
  defsubr (&Sdefault_file_modes);
  defsubr (&Sfile_newer_than_file_p);
  defsubr (&Sinsert_file_contents);
  defsubr (&Swrite_region);
  defsubr (&Scar_less_than_car);
  defsubr (&Sverify_visited_file_modtime);
  defsubr (&Svisited_file_modtime);
  defsubr (&Sset_visited_file_modtime);
  defsubr (&Sdo_auto_save);
  defsubr (&Sset_buffer_auto_saved);
  defsubr (&Sclear_buffer_auto_save_failure);
  defsubr (&Srecent_auto_save_p);

  defsubr (&Snext_read_file_uses_dialog_p);

  defsubr (&Sset_binary_mode);

#ifndef DOS_NT
  defsubr (&Sfile_system_info);
#endif

#ifdef HAVE_SYNC
  defsubr (&Sunix_sync);
#endif
}
