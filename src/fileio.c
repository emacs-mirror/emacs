/* File IO for GNU Emacs.
   Copyright (C) 1985,86,87,88,93,94,95,96,97,98,99,2000, 2001
     Free Software Foundation, Inc.

This file is part of GNU Emacs.

GNU Emacs is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs; see the file COPYING.  If not, write to
the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

#define _GNU_SOURCE		/* for euidaccess */

#include <config.h>

#if defined (USG5) || defined (BSD_SYSTEM) || defined (GNU_LINUX)
#include <fcntl.h>
#endif

#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#if !defined (S_ISLNK) && defined (S_IFLNK)
#  define S_ISLNK(m) (((m) & S_IFMT) == S_IFLNK)
#endif

#if !defined (S_ISFIFO) && defined (S_IFIFO)
#  define S_ISFIFO(m) (((m) & S_IFMT) == S_IFIFO)
#endif

#if !defined (S_ISREG) && defined (S_IFREG)
#  define S_ISREG(m) (((m) & S_IFMT) == S_IFREG)
#endif

#ifdef VMS
#include "vms-pwd.h"
#else
#include <pwd.h>
#endif

#include <ctype.h>

#ifdef VMS
#include "vmsdir.h"
#include <perror.h>
#include <stddef.h>
#include <string.h>
#endif

#include <errno.h>

#ifndef vax11c
#ifndef USE_CRT_DLL
extern int errno;
#endif
#endif

#ifdef APOLLO
#include <sys/time.h>
#endif

#ifndef USG
#ifndef VMS
#ifndef BSD4_1
#ifndef WINDOWSNT
#define HAVE_FSYNC
#endif
#endif
#endif
#endif

#include "lisp.h"
#include "intervals.h"
#include "buffer.h"
#include "charset.h"
#include "coding.h"
#include "window.h"

#ifdef WINDOWSNT
#define NOMINMAX 1
#include <windows.h>
#include <stdlib.h>
#include <fcntl.h>
#endif /* not WINDOWSNT */

#ifdef MSDOS
#include "msdos.h"
#include <sys/param.h>
#if __DJGPP__ >= 2
#include <fcntl.h>
#include <string.h>
#endif
#endif

#ifdef DOS_NT
#define CORRECT_DIR_SEPS(s) \
  do { if ('/' == DIRECTORY_SEP) dostounix_filename (s); \
       else unixtodos_filename (s); \
  } while (0)
/* On Windows, drive letters must be alphabetic - on DOS, the Netware
   redirector allows the six letters between 'Z' and 'a' as well. */
#ifdef MSDOS
#define IS_DRIVE(x) ((x) >= 'A' && (x) <= 'z')
#endif
#ifdef WINDOWSNT
#define IS_DRIVE(x) isalpha (x)
#endif
/* Need to lower-case the drive letter, or else expanded
   filenames will sometimes compare inequal, because
   `expand-file-name' doesn't always down-case the drive letter.  */
#define DRIVE_LETTER(x) (tolower (x))
#endif

#ifdef VMS
#include <file.h>
#include <rmsdef.h>
#include <fab.h>
#include <nam.h>
#endif

#include "systime.h"

#ifdef HPUX
#include <netio.h>
#ifndef HPUX8
#ifndef HPUX9
#include <errnet.h>
#endif
#endif
#endif

#include "commands.h"
extern int use_dialog_box;

#ifndef O_WRONLY
#define O_WRONLY 1
#endif

#ifndef O_RDONLY
#define O_RDONLY 0
#endif

#ifndef S_ISLNK
#  define lstat stat
#endif

/* Nonzero during writing of auto-save files */
int auto_saving;

/* Set by auto_save_1 to mode of original file so Fwrite_region will create
   a new file with the same mode as the original */
int auto_save_mode_bits;

/* Coding system for file names, or nil if none.  */
Lisp_Object Vfile_name_coding_system;

/* Coding system for file names used only when
   Vfile_name_coding_system is nil.  */
Lisp_Object Vdefault_file_name_coding_system;

/* Alist of elements (REGEXP . HANDLER) for file names
   whose I/O is done with a special handler.  */
Lisp_Object Vfile_name_handler_alist;

/* Format for auto-save files */
Lisp_Object Vauto_save_file_format;

/* Lisp functions for translating file formats */
Lisp_Object Qformat_decode, Qformat_annotate_function;

/* Function to be called to decide a coding system of a reading file.  */
Lisp_Object Vset_auto_coding_function;

/* Functions to be called to process text properties in inserted file.  */
Lisp_Object Vafter_insert_file_functions;

/* Functions to be called to create text property annotations for file.  */
Lisp_Object Vwrite_region_annotate_functions;

/* During build_annotations, each time an annotation function is called,
   this holds the annotations made by the previous functions.  */
Lisp_Object Vwrite_region_annotations_so_far;

/* File name in which we write a list of all our auto save files.  */
Lisp_Object Vauto_save_list_file_name;

/* Function to call to read a file name.  */
Lisp_Object Vread_file_name_function; 

/* Current predicate used by read_file_name_internal.  */
Lisp_Object Vread_file_name_predicate;

/* Nonzero means, when reading a filename in the minibuffer,
 start out by inserting the default directory into the minibuffer. */
int insert_default_directory;

/* On VMS, nonzero means write new files with record format stmlf.
   Zero means use var format.  */
int vms_stmlf_recfm;

/* On NT, specifies the directory separator character, used (eg.) when
   expanding file names.  This can be bound to / or \. */
Lisp_Object Vdirectory_sep_char;

extern Lisp_Object Vuser_login_name;

#ifdef WINDOWSNT
extern Lisp_Object Vw32_get_true_file_attributes;
#endif

extern int minibuf_level;

extern int minibuffer_auto_raise;

/* These variables describe handlers that have "already" had a chance
   to handle the current operation.

   Vinhibit_file_name_handlers is a list of file name handlers.
   Vinhibit_file_name_operation is the operation being handled.
   If we try to handle that operation, we ignore those handlers.  */

static Lisp_Object Vinhibit_file_name_handlers;
static Lisp_Object Vinhibit_file_name_operation;

Lisp_Object Qfile_error, Qfile_already_exists, Qfile_date_error;
Lisp_Object Qexcl;
Lisp_Object Qfile_name_history;

Lisp_Object Qcar_less_than_car;

static int a_write P_ ((int, Lisp_Object, int, int,
			Lisp_Object *, struct coding_system *));
static int e_write P_ ((int, Lisp_Object, int, int, struct coding_system *));


void
report_file_error (string, data)
     char *string;
     Lisp_Object data;
{
  Lisp_Object errstring;
  int errorno = errno;

  synchronize_system_messages_locale ();
  errstring = code_convert_string_norecord (build_string (strerror (errorno)),
					    Vlocale_coding_system, 0);

  while (1)
    switch (errorno)
      {
      case EEXIST:
	Fsignal (Qfile_already_exists, Fcons (errstring, data));
	break;
      default:
	/* System error messages are capitalized.  Downcase the initial
	   unless it is followed by a slash.  */
	if (XSTRING (errstring)->data[1] != '/')
	  XSTRING (errstring)->data[0] = DOWNCASE (XSTRING (errstring)->data[0]);

	Fsignal (Qfile_error,
		 Fcons (build_string (string), Fcons (errstring, data)));
      }
}

Lisp_Object
close_file_unwind (fd)
     Lisp_Object fd;
{
  emacs_close (XFASTINT (fd));
  return Qnil;
}

/* Restore point, having saved it as a marker.  */

static Lisp_Object
restore_point_unwind (location)
     Lisp_Object location;
{
  Fgoto_char (location);
  Fset_marker (location, Qnil, Qnil);
  return Qnil;
}

Lisp_Object Qexpand_file_name;
Lisp_Object Qsubstitute_in_file_name;
Lisp_Object Qdirectory_file_name;
Lisp_Object Qfile_name_directory;
Lisp_Object Qfile_name_nondirectory;
Lisp_Object Qunhandled_file_name_directory;
Lisp_Object Qfile_name_as_directory;
Lisp_Object Qcopy_file;
Lisp_Object Qmake_directory_internal;
Lisp_Object Qmake_directory;
Lisp_Object Qdelete_directory;
Lisp_Object Qdelete_file;
Lisp_Object Qrename_file;
Lisp_Object Qadd_name_to_file;
Lisp_Object Qmake_symbolic_link;
Lisp_Object Qfile_exists_p;
Lisp_Object Qfile_executable_p;
Lisp_Object Qfile_readable_p;
Lisp_Object Qfile_writable_p;
Lisp_Object Qfile_symlink_p;
Lisp_Object Qaccess_file;
Lisp_Object Qfile_directory_p;
Lisp_Object Qfile_regular_p;
Lisp_Object Qfile_accessible_directory_p;
Lisp_Object Qfile_modes;
Lisp_Object Qset_file_modes;
Lisp_Object Qfile_newer_than_file_p;
Lisp_Object Qinsert_file_contents;
Lisp_Object Qwrite_region;
Lisp_Object Qverify_visited_file_modtime;
Lisp_Object Qset_visited_file_modtime;

DEFUN ("find-file-name-handler", Ffind_file_name_handler, Sfind_file_name_handler, 2, 2, 0,
       doc: /* Return FILENAME's handler function for OPERATION, if it has one.
Otherwise, return nil.
A file name is handled if one of the regular expressions in
`file-name-handler-alist' matches it.

If OPERATION equals `inhibit-file-name-operation', then we ignore
any handlers that are members of `inhibit-file-name-handlers',
but we still do run any other handlers.  This lets handlers
use the standard functions without calling themselves recursively.  */)
     (filename, operation)
     Lisp_Object filename, operation;
{
  /* This function must not munge the match data.  */
  Lisp_Object chain, inhibited_handlers, result;
  int pos = -1;

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
	  Lisp_Object string;
	  int match_pos;
	  string = XCAR (elt);
	  if (STRINGP (string)
	      && (match_pos = fast_string_match (string, filename)) > pos)
	    {
	      Lisp_Object handler, tem;

	      handler = XCDR (elt);
	      tem = Fmemq (handler, inhibited_handlers);
	      if (NILP (tem))
		{
		  result = handler;
		  pos = match_pos;
		}
	    }
	}

      QUIT;
    }
  return result;
}

DEFUN ("file-name-directory", Ffile_name_directory, Sfile_name_directory,
       1, 1, 0,
       doc: /* Return the directory component in file name FILENAME.
Return nil if FILENAME does not include a directory.
Otherwise return a directory spec.
Given a Unix syntax file name, returns a string ending in slash;
on VMS, perhaps instead a string ending in `:', `]' or `>'.  */)
     (filename)
     Lisp_Object filename;
{
  register unsigned char *beg;
  register unsigned char *p;
  Lisp_Object handler;

  CHECK_STRING (filename);

  /* If the file name has special constructs in it,
     call the corresponding file handler.  */
  handler = Ffind_file_name_handler (filename, Qfile_name_directory);
  if (!NILP (handler))
    return call2 (handler, Qfile_name_directory, filename);

#ifdef FILE_SYSTEM_CASE
  filename = FILE_SYSTEM_CASE (filename);
#endif
  beg = XSTRING (filename)->data;
#ifdef DOS_NT
  beg = strcpy (alloca (strlen (beg) + 1), beg);
#endif
  p = beg + STRING_BYTES (XSTRING (filename));

  while (p != beg && !IS_DIRECTORY_SEP (p[-1])
#ifdef VMS
	 && p[-1] != ':' && p[-1] != ']' && p[-1] != '>'
#endif /* VMS */
#ifdef DOS_NT
	 /* only recognise drive specifier at the beginning */
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
  if (p[-1] == ':')
    {
      /* MAXPATHLEN+1 is guaranteed to be enough space for getdefdir.  */
      unsigned char *res = alloca (MAXPATHLEN + 1);
      unsigned char *r = res;

      if (p == beg + 4 && IS_DIRECTORY_SEP (*beg) && beg[1] == ':')
	{
	  strncpy (res, beg, 2);
	  beg += 2;
	  r += 2;
	}

      if (getdefdir (toupper (*beg) - 'A' + 1, r))
	{
	  if (!IS_DIRECTORY_SEP (res[strlen (res) - 1]))
	    strcat (res, "/");
	  beg = res;
	  p = beg + strlen (beg);
	}
    }
  CORRECT_DIR_SEPS (beg);
#endif /* DOS_NT */

  if (STRING_MULTIBYTE (filename))
    return make_string (beg, p - beg);
  return make_unibyte_string (beg, p - beg);
}

DEFUN ("file-name-nondirectory", Ffile_name_nondirectory,
       Sfile_name_nondirectory, 1, 1, 0,
       doc: /* Return file name FILENAME sans its directory.
For example, in a Unix-syntax file name,
this is everything after the last slash,
or the entire name if it contains no slash.  */)
     (filename)
     Lisp_Object filename;
{
  register unsigned char *beg, *p, *end;
  Lisp_Object handler;

  CHECK_STRING (filename);

  /* If the file name has special constructs in it,
     call the corresponding file handler.  */
  handler = Ffind_file_name_handler (filename, Qfile_name_nondirectory);
  if (!NILP (handler))
    return call2 (handler, Qfile_name_nondirectory, filename);

  beg = XSTRING (filename)->data;
  end = p = beg + STRING_BYTES (XSTRING (filename));

  while (p != beg && !IS_DIRECTORY_SEP (p[-1])
#ifdef VMS
	 && p[-1] != ':' && p[-1] != ']' && p[-1] != '>'
#endif /* VMS */
#ifdef DOS_NT
	 /* only recognise drive specifier at beginning */
	 && !(p[-1] == ':'
	      /* handle the "/:d:foo" case correctly  */
	      && (p == beg + 2 || (p == beg + 4 && IS_DIRECTORY_SEP (*beg))))
#endif
	 )
    p--;

  if (STRING_MULTIBYTE (filename))
    return make_string (p, end - p);
  return make_unibyte_string (p, end - p);
}

DEFUN ("unhandled-file-name-directory", Funhandled_file_name_directory,
       Sunhandled_file_name_directory, 1, 1, 0,
       doc: /* Return a directly usable directory name somehow associated with FILENAME.
A `directly usable' directory name is one that may be used without the
intervention of any file handler.
If FILENAME is a directly usable file itself, return
\(file-name-directory FILENAME).
The `call-process' and `start-process' functions use this function to
get a current directory to run processes in.  */)
     (filename)
     Lisp_Object filename;
{
  Lisp_Object handler;

  /* If the file name has special constructs in it,
     call the corresponding file handler.  */
  handler = Ffind_file_name_handler (filename, Qunhandled_file_name_directory);
  if (!NILP (handler))
    return call2 (handler, Qunhandled_file_name_directory, filename);

  return Ffile_name_directory (filename);
}


char *
file_name_as_directory (out, in)
     char *out, *in;
{
  int size = strlen (in) - 1;

  strcpy (out, in);

  if (size < 0)
    {
      out[0] = '.';
      out[1] = '/';
      out[2] = 0;
      return out;
    }

#ifdef VMS
  /* Is it already a directory string? */
  if (in[size] == ':' || in[size] == ']' || in[size] == '>')
    return out;
  /* Is it a VMS directory file name?  If so, hack VMS syntax.  */
  else if (! index (in, '/')
	   && ((size > 3 && ! strcmp (&in[size - 3], ".DIR"))
	       || (size > 3 && ! strcmp (&in[size - 3], ".dir"))
	       || (size > 5 && (! strncmp (&in[size - 5], ".DIR", 4)
				|| ! strncmp (&in[size - 5], ".dir", 4))
		   && (in[size - 1] == '.' || in[size - 1] == ';')
		   && in[size] == '1')))
    {
      register char *p, *dot;
      char brack;

      /* x.dir -> [.x]
	 dir:x.dir --> dir:[x]
	 dir:[x]y.dir --> dir:[x.y] */
      p = in + size;
      while (p != in && *p != ':' && *p != '>' && *p != ']') p--;
      if (p != in)
	{
	  strncpy (out, in, p - in);
	  out[p - in] = '\0';
	  if (*p == ':')
	    {
	      brack = ']';
	      strcat (out, ":[");
	    }
	  else
	    {
	      brack = *p;
	      strcat (out, ".");
	    }
	  p++;
	}
      else
	{
	  brack = ']';
	  strcpy (out, "[.");
	}
      dot = index (p, '.');
      if (dot)
	{
	  /* blindly remove any extension */
	  size = strlen (out) + (dot - p);
	  strncat (out, p, dot - p);
	}
      else
	{
	  strcat (out, p);
	  size = strlen (out);
	}
      out[size++] = brack;
      out[size] = '\0';
    }
#else /* not VMS */
  /* For Unix syntax, Append a slash if necessary */
  if (!IS_DIRECTORY_SEP (out[size]))
    {
      out[size + 1] = DIRECTORY_SEP;
      out[size + 2] = '\0';
    }
#ifdef DOS_NT
  CORRECT_DIR_SEPS (out);
#endif
#endif /* not VMS */
  return out;
}

DEFUN ("file-name-as-directory", Ffile_name_as_directory,
       Sfile_name_as_directory, 1, 1, 0,
       doc: /* Return a string representing file FILENAME interpreted as a directory.
This operation exists because a directory is also a file, but its name as
a directory is different from its name as a file.
The result can be used as the value of `default-directory'
or passed as second argument to `expand-file-name'.
For a Unix-syntax file name, just appends a slash.
On VMS, converts \"[X]FOO.DIR\" to \"[X.FOO]\", etc.  */)
     (file)
     Lisp_Object file;
{
  char *buf;
  Lisp_Object handler;

  CHECK_STRING (file);
  if (NILP (file))
    return Qnil;

  /* If the file name has special constructs in it,
     call the corresponding file handler.  */
  handler = Ffind_file_name_handler (file, Qfile_name_as_directory);
  if (!NILP (handler))
    return call2 (handler, Qfile_name_as_directory, file);

  buf = (char *) alloca (STRING_BYTES (XSTRING (file)) + 10);
  return build_string (file_name_as_directory (buf, XSTRING (file)->data));
}

/*
 * Convert from directory name to filename.
 * On VMS:
 *       xyzzy:[mukesh.emacs] => xyzzy:[mukesh]emacs.dir.1
 *       xyzzy:[mukesh] => xyzzy:[000000]mukesh.dir.1
 * On UNIX, it's simple: just make sure there isn't a terminating /

 * Value is nonzero if the string output is different from the input.
 */

int
directory_file_name (src, dst)
     char *src, *dst;
{
  long slen;
#ifdef VMS
  long rlen;
  char * ptr, * rptr;
  char bracket;
  struct FAB fab = cc$rms_fab;
  struct NAM nam = cc$rms_nam;
  char esa[NAM$C_MAXRSS];
#endif /* VMS */

  slen = strlen (src);
#ifdef VMS
  if (! index (src, '/')
      && (src[slen - 1] == ']'
	  || src[slen - 1] == ':'
	  || src[slen - 1] == '>'))
    {
      /* VMS style - convert [x.y.z] to [x.y]z, [x] to [000000]x */
      fab.fab$l_fna = src;
      fab.fab$b_fns = slen;
      fab.fab$l_nam = &nam;
      fab.fab$l_fop = FAB$M_NAM;

      nam.nam$l_esa = esa;
      nam.nam$b_ess = sizeof esa;
      nam.nam$b_nop |= NAM$M_SYNCHK;

      /* We call SYS$PARSE to handle such things as [--] for us. */
      if (SYS$PARSE (&fab, 0, 0) == RMS$_NORMAL)
	{
	  slen = nam.nam$b_esl;
	  if (esa[slen - 1] == ';' && esa[slen - 2] == '.')
	    slen -= 2;
	  esa[slen] = '\0';
	  src = esa;
	}
      if (src[slen - 1] != ']' && src[slen - 1] != '>')
	{
	  /* what about when we have logical_name:???? */
	  if (src[slen - 1] == ':')
	    {                   /* Xlate logical name and see what we get */
	      ptr = strcpy (dst, src); /* upper case for getenv */
	      while (*ptr)
		{
		  if ('a' <= *ptr && *ptr <= 'z')
		    *ptr -= 040;
		  ptr++;
		}
	      dst[slen - 1] = 0;        /* remove colon */
	      if (!(src = egetenv (dst)))
		return 0;
	      /* should we jump to the beginning of this procedure?
		 Good points: allows us to use logical names that xlate
		 to Unix names,
		 Bad points: can be a problem if we just translated to a device
		 name...
		 For now, I'll punt and always expect VMS names, and hope for
		 the best! */
	      slen = strlen (src);
	      if (src[slen - 1] != ']' && src[slen - 1] != '>')
		{ /* no recursion here! */
		  strcpy (dst, src);
		  return 0;
		}
	    }
	  else
	    {           /* not a directory spec */
	      strcpy (dst, src);
	      return 0;
	    }
	}
      bracket = src[slen - 1];

      /* If bracket is ']' or '>', bracket - 2 is the corresponding
	 opening bracket.  */
      ptr = index (src, bracket - 2);
      if (ptr == 0)
	{ /* no opening bracket */
	  strcpy (dst, src);
	  return 0;
	}
      if (!(rptr = rindex (src, '.')))
	rptr = ptr;
      slen = rptr - src;
      strncpy (dst, src, slen);
      dst[slen] = '\0';
      if (*rptr == '.')
	{
	  dst[slen++] = bracket;
	  dst[slen] = '\0';
	}
      else
	{
	  /* If we have the top-level of a rooted directory (i.e. xx:[000000]),
	     then translate the device and recurse. */
	  if (dst[slen - 1] == ':'
	      && dst[slen - 2] != ':'   /* skip decnet nodes */
	      && strcmp (src + slen, "[000000]") == 0)
	    {
	      dst[slen - 1] = '\0';
	      if ((ptr = egetenv (dst))
		  && (rlen = strlen (ptr) - 1) > 0
		  && (ptr[rlen] == ']' || ptr[rlen] == '>')
		  && ptr[rlen - 1] == '.')
		{
		  char * buf = (char *) alloca (strlen (ptr) + 1);
		  strcpy (buf, ptr);
		  buf[rlen - 1] = ']';
		  buf[rlen] = '\0';
		  return directory_file_name (buf, dst);
		}
	      else
		dst[slen - 1] = ':';
	    }
	  strcat (dst, "[000000]");
	  slen += 8;
	}
      rptr++;
      rlen = strlen (rptr) - 1;
      strncat (dst, rptr, rlen);
      dst[slen + rlen] = '\0';
      strcat (dst, ".DIR.1");
      return 1;
    }
#endif /* VMS */
  /* Process as Unix format: just remove any final slash.
     But leave "/" unchanged; do not change it to "".  */
  strcpy (dst, src);
#ifdef APOLLO
  /* Handle // as root for apollo's.  */
  if ((slen > 2 && dst[slen - 1] == '/')
      || (slen > 1 && dst[0] != '/' && dst[slen - 1] == '/'))
    dst[slen - 1] = 0;
#else
  if (slen > 1
      && IS_DIRECTORY_SEP (dst[slen - 1])
#ifdef DOS_NT
      && !IS_ANY_SEP (dst[slen - 2])
#endif
      )
    dst[slen - 1] = 0;
#endif
#ifdef DOS_NT
  CORRECT_DIR_SEPS (dst);
#endif
  return 1;
}

DEFUN ("directory-file-name", Fdirectory_file_name, Sdirectory_file_name,
       1, 1, 0,
       doc: /* Returns the file name of the directory named DIRECTORY.
This is the name of the file that holds the data for the directory DIRECTORY.
This operation exists because a directory is also a file, but its name as
a directory is different from its name as a file.
In Unix-syntax, this function just removes the final slash.
On VMS, given a VMS-syntax directory name such as \"[X.Y]\",
it returns a file name such as \"[X]Y.DIR.1\".  */)
     (directory)
     Lisp_Object directory;
{
  char *buf;
  Lisp_Object handler;

  CHECK_STRING (directory);

  if (NILP (directory))
    return Qnil;

  /* If the file name has special constructs in it,
     call the corresponding file handler.  */
  handler = Ffind_file_name_handler (directory, Qdirectory_file_name);
  if (!NILP (handler))
    return call2 (handler, Qdirectory_file_name, directory);

#ifdef VMS
  /* 20 extra chars is insufficient for VMS, since we might perform a
     logical name translation. an equivalence string can be up to 255
     chars long, so grab that much extra space...  - sss */
  buf = (char *) alloca (STRING_BYTES (XSTRING (directory)) + 20 + 255);
#else
  buf = (char *) alloca (STRING_BYTES (XSTRING (directory)) + 20);
#endif
  directory_file_name (XSTRING (directory)->data, buf);
  return build_string (buf);
}

static char make_temp_name_tbl[64] =
{
  'A','B','C','D','E','F','G','H',
  'I','J','K','L','M','N','O','P',
  'Q','R','S','T','U','V','W','X',
  'Y','Z','a','b','c','d','e','f',
  'g','h','i','j','k','l','m','n',
  'o','p','q','r','s','t','u','v',
  'w','x','y','z','0','1','2','3',
  '4','5','6','7','8','9','-','_'
};

static unsigned make_temp_name_count, make_temp_name_count_initialized_p;

/* Value is a temporary file name starting with PREFIX, a string.
   
   The Emacs process number forms part of the result, so there is
   no danger of generating a name being used by another process.
   In addition, this function makes an attempt to choose a name
   which has no existing file.  To make this work, PREFIX should be
   an absolute file name.
   
   BASE64_P non-zero means add the pid as 3 characters in base64
   encoding.  In this case, 6 characters will be added to PREFIX to
   form the file name.  Otherwise, if Emacs is running on a system
   with long file names, add the pid as a decimal number.

   This function signals an error if no unique file name could be
   generated.  */

Lisp_Object
make_temp_name (prefix, base64_p)
     Lisp_Object prefix;
     int base64_p;
{
  Lisp_Object val;
  int len;
  int pid;
  unsigned char *p, *data;
  char pidbuf[20];
  int pidlen;
     
  CHECK_STRING (prefix);

  /* VAL is created by adding 6 characters to PREFIX.  The first
     three are the PID of this process, in base 64, and the second
     three are incremented if the file already exists.  This ensures
     262144 unique file names per PID per PREFIX.  */

  pid = (int) getpid ();

  if (base64_p)
    {
      pidbuf[0] = make_temp_name_tbl[pid & 63], pid >>= 6;
      pidbuf[1] = make_temp_name_tbl[pid & 63], pid >>= 6;
      pidbuf[2] = make_temp_name_tbl[pid & 63], pid >>= 6;
      pidlen = 3;
    }
  else
    {
#ifdef HAVE_LONG_FILE_NAMES
      sprintf (pidbuf, "%d", pid);
      pidlen = strlen (pidbuf);
#else
      pidbuf[0] = make_temp_name_tbl[pid & 63], pid >>= 6;
      pidbuf[1] = make_temp_name_tbl[pid & 63], pid >>= 6;
      pidbuf[2] = make_temp_name_tbl[pid & 63], pid >>= 6;
      pidlen = 3;
#endif
    }
  
  len = XSTRING (prefix)->size;
  val = make_uninit_string (len + 3 + pidlen);
  data = XSTRING (val)->data;
  bcopy(XSTRING (prefix)->data, data, len);
  p = data + len;

  bcopy (pidbuf, p, pidlen);
  p += pidlen;

  /* Here we try to minimize useless stat'ing when this function is
     invoked many times successively with the same PREFIX.  We achieve
     this by initializing count to a random value, and incrementing it
     afterwards.

     We don't want make-temp-name to be called while dumping,
     because then make_temp_name_count_initialized_p would get set
     and then make_temp_name_count would not be set when Emacs starts.  */

  if (!make_temp_name_count_initialized_p)
    {
      make_temp_name_count = (unsigned) time (NULL);
      make_temp_name_count_initialized_p = 1;
    }

  while (1)
    {
      struct stat ignored;
      unsigned num = make_temp_name_count;

      p[0] = make_temp_name_tbl[num & 63], num >>= 6;
      p[1] = make_temp_name_tbl[num & 63], num >>= 6;
      p[2] = make_temp_name_tbl[num & 63], num >>= 6;

      /* Poor man's congruential RN generator.  Replace with
         ++make_temp_name_count for debugging.  */
      make_temp_name_count += 25229;
      make_temp_name_count %= 225307;

      if (stat (data, &ignored) < 0)
	{
	  /* We want to return only if errno is ENOENT.  */
	  if (errno == ENOENT)
	    return val;
	  else
	    /* The error here is dubious, but there is little else we
	       can do.  The alternatives are to return nil, which is
	       as bad as (and in many cases worse than) throwing the
	       error, or to ignore the error, which will likely result
	       in looping through 225307 stat's, which is not only
	       dog-slow, but also useless since it will fallback to
	       the errow below, anyway.  */
	    report_file_error ("Cannot create temporary name for prefix",
			       Fcons (prefix, Qnil));
	  /* not reached */
	}
    }

  error ("Cannot create temporary name for prefix `%s'",
	 XSTRING (prefix)->data);
  return Qnil;
}


DEFUN ("make-temp-name", Fmake_temp_name, Smake_temp_name, 1, 1, 0,
       doc: /* Generate temporary file name (string) starting with PREFIX (a string).
The Emacs process number forms part of the result,
so there is no danger of generating a name being used by another process.

In addition, this function makes an attempt to choose a name
which has no existing file.  To make this work,
PREFIX should be an absolute file name.

There is a race condition between calling `make-temp-name' and creating the
file which opens all kinds of security holes.  For that reason, you should
probably use `make-temp-file' instead, except in three circumstances:

* If you are creating the file in the user's home directory.
* If you are creating a directory rather than an ordinary file.
* If you are taking special precautions as `make-temp-file' does.  */)
     (prefix)
     Lisp_Object prefix;
{
  return make_temp_name (prefix, 0);
}



DEFUN ("expand-file-name", Fexpand_file_name, Sexpand_file_name, 1, 2, 0,
       doc: /* Convert filename NAME to absolute, and canonicalize it.
Second arg DEFAULT-DIRECTORY is directory to start with if NAME is relative
 (does not start with slash); if DEFAULT-DIRECTORY is nil or missing,
the current buffer's value of default-directory is used.
File name components that are `.' are removed, and
so are file name components followed by `..', along with the `..' itself;
note that these simplifications are done without checking the resulting
file names in the file system.
An initial `~/' expands to your home directory.
An initial `~USER/' expands to USER's home directory.
See also the function `substitute-in-file-name'.  */)
     (name, default_directory)
     Lisp_Object name, default_directory;
{
  unsigned char *nm;

  register unsigned char *newdir, *p, *o;
  int tlen;
  unsigned char *target;
  struct passwd *pw;
#ifdef VMS
  unsigned char * colon = 0;
  unsigned char * close = 0;
  unsigned char * slash = 0;
  unsigned char * brack = 0;
  int lbrack = 0, rbrack = 0;
  int dots = 0;
#endif /* VMS */
#ifdef DOS_NT
  int drive = 0;
  int collapse_newdir = 1;
  int is_escaped = 0;
#endif /* DOS_NT */
  int length;
  Lisp_Object handler;

  CHECK_STRING (name);

  /* If the file name has special constructs in it,
     call the corresponding file handler.  */
  handler = Ffind_file_name_handler (name, Qexpand_file_name);
  if (!NILP (handler))
    return call3 (handler, Qexpand_file_name, name, default_directory);

  /* Use the buffer's default-directory if DEFAULT_DIRECTORY is omitted.  */
  if (NILP (default_directory))
    default_directory = current_buffer->directory;
  if (! STRINGP (default_directory))
    {
#ifdef DOS_NT
      /* "/" is not considered a root directory on DOS_NT, so using "/"
	 here causes an infinite recursion in, e.g., the following:

            (let (default-directory)
	      (expand-file-name "a"))

	 To avoid this, we set default_directory to the root of the
	 current drive.  */
      extern char *emacs_root_dir (void);

      default_directory = build_string (emacs_root_dir ());
#else
      default_directory = build_string ("/");
#endif
    }

  if (!NILP (default_directory))
    {
      handler = Ffind_file_name_handler (default_directory, Qexpand_file_name);
      if (!NILP (handler))
	return call3 (handler, Qexpand_file_name, name, default_directory);
    }

  o = XSTRING (default_directory)->data;

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
      && ! (IS_DRIVE (o[0]) && IS_DEVICE_SEP (o[1]) && IS_DIRECTORY_SEP (o[2]))
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
      struct gcpro gcpro1;

      GCPRO1 (name);
      default_directory = Fexpand_file_name (default_directory, Qnil);
      UNGCPRO;
    }

#ifdef VMS
  /* Filenames on VMS are always upper case.  */
  name = Fupcase (name);
#endif
#ifdef FILE_SYSTEM_CASE
  name = FILE_SYSTEM_CASE (name);
#endif

  nm = XSTRING (name)->data;

#ifdef DOS_NT
  /* We will force directory separators to be either all \ or /, so make
     a local copy to modify, even if there ends up being no change. */
  nm = strcpy (alloca (strlen (nm) + 1), nm);

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
      drive = nm[0];
      nm += 2;
    }

#ifdef WINDOWSNT
  /* If we see "c://somedir", we want to strip the first slash after the
     colon when stripping the drive letter.  Otherwise, this expands to
     "//somedir".  */
  if (drive && IS_DIRECTORY_SEP (nm[0]) && IS_DIRECTORY_SEP (nm[1]))
    nm++;
#endif /* WINDOWSNT */
#endif /* DOS_NT */

#ifdef WINDOWSNT
  /* Discard any previous drive specifier if nm is now in UNC format. */
  if (IS_DIRECTORY_SEP (nm[0]) && IS_DIRECTORY_SEP (nm[1]))
    {
      drive = 0;
    }
#endif

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
#ifdef VMS
      || index (nm, ':')
#endif /* VMS */
      )
    {
      /* If it turns out that the filename we want to return is just a
	 suffix of FILENAME, we don't need to go through and edit
	 things; we just need to construct a new string using data
	 starting at the middle of FILENAME.  If we set lose to a
	 non-zero value, that means we've discovered that we can't do
	 that cool trick.  */
      int lose = 0;

      p = nm;
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
	  /* We want to replace multiple `/' in a row with a single
	     slash.  */
	  else if (p > nm
		   && IS_DIRECTORY_SEP (p[0])
		   && IS_DIRECTORY_SEP (p[1]))
	    lose = 1;
	  
#ifdef VMS
	  if (p[0] == '\\')
	    lose = 1;
	  if (p[0] == '/') {
	    /* if dev:[dir]/, move nm to / */
	    if (!slash && p > nm && (brack || colon)) {
	      nm = (brack ? brack + 1 : colon + 1);
	      lbrack = rbrack = 0;
	      brack = 0;
	      colon = 0;
	    }
	    slash = p;
	  }
	  if (p[0] == '-')
#ifndef VMS4_4
	    /* VMS pre V4.4,convert '-'s in filenames. */
	    if (lbrack == rbrack)
	      {
		if (dots < 2)   /* this is to allow negative version numbers */
		  p[0] = '_';
	      }
	    else
#endif /* VMS4_4 */
	      if (lbrack > rbrack &&
		  ((p[-1] == '.' || p[-1] == '[' || p[-1] == '<') &&
		   (p[1] == '.' || p[1] == ']' || p[1] == '>')))
		lose = 1;
#ifndef VMS4_4
	      else
		p[0] = '_';
#endif /* VMS4_4 */
	  /* count open brackets, reset close bracket pointer */
	  if (p[0] == '[' || p[0] == '<')
	    lbrack++, brack = 0;
	  /* count close brackets, set close bracket pointer */
	  if (p[0] == ']' || p[0] == '>')
	    rbrack++, brack = p;
	  /* detect ][ or >< */
	  if ((p[0] == ']' || p[0] == '>') && (p[1] == '[' || p[1] == '<'))
	    lose = 1;
	  if ((p[0] == ':' || p[0] == ']' || p[0] == '>') && p[1] == '~')
	    nm = p + 1, lose = 1;
	  if (p[0] == ':' && (colon || slash))
	    /* if dev1:[dir]dev2:, move nm to dev2: */
	    if (brack)
	      {
		nm = brack + 1;
		brack = 0;
	      }
	    /* if /name/dev:, move nm to dev: */
	    else if (slash)
	      nm = slash + 1;
	    /* if node::dev:, move colon following dev */
	    else if (colon && colon[-1] == ':')
	      colon = p;
	    /* if dev1:dev2:, move nm to dev2: */
	    else if (colon && colon[-1] != ':')
	      {
		nm = colon + 1;
		colon = 0;
	      }
	  if (p[0] == ':' && !colon)
	    {
	      if (p[1] == ':')
		p++;
	      colon = p;
	    }
	  if (lbrack == rbrack)
	    if (p[0] == ';')
	      dots = 2;
	    else if (p[0] == '.')
	      dots++;
#endif /* VMS */
	  p++;
	}
      if (!lose)
	{
#ifdef VMS
	  if (index (nm, '/'))
	    return build_string (sys_translate_unix (nm));
#endif /* VMS */
#ifdef DOS_NT
	  /* Make sure directories are all separated with / or \ as
	     desired, but avoid allocation of a new string when not
	     required. */
	  CORRECT_DIR_SEPS (nm);
#ifdef WINDOWSNT
	  if (IS_DIRECTORY_SEP (nm[1]))
	    {
	      if (strcmp (nm, XSTRING (name)->data) != 0)
		name = build_string (nm);
	    }
	  else
#endif
	  /* drive must be set, so this is okay */
	  if (strcmp (nm - 2, XSTRING (name)->data) != 0)
	    {
	      name = make_string (nm - 2, p - nm + 2);
	      XSTRING (name)->data[0] = DRIVE_LETTER (drive);
	      XSTRING (name)->data[1] = ':';
	    }
	  return name;
#else /* not DOS_NT */
	  if (nm == XSTRING (name)->data)
	    return name;
	  return build_string (nm);
#endif /* not DOS_NT */
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

  newdir = 0;

  if (nm[0] == '~')		/* prefix ~ */
    {
      if (IS_DIRECTORY_SEP (nm[1])
#ifdef VMS
	  || nm[1] == ':'
#endif /* VMS */
	  || nm[1] == 0)	/* ~ by itself */
	{
	  if (!(newdir = (unsigned char *) egetenv ("HOME")))
	    newdir = (unsigned char *) "";
	  nm++;
#ifdef DOS_NT
	  collapse_newdir = 0;
#endif
#ifdef VMS
	  nm++;			/* Don't leave the slash in nm.  */
#endif /* VMS */
	}
      else			/* ~user/filename */
	{
	  for (p = nm; *p && (!IS_DIRECTORY_SEP (*p)
#ifdef VMS
			      && *p != ':'
#endif /* VMS */
			      ); p++);
	  o = (unsigned char *) alloca (p - nm + 1);
	  bcopy ((char *) nm, o, p - nm);
	  o [p - nm] = 0;

	  pw = (struct passwd *) getpwnam (o + 1);
	  if (pw)
	    {
	      newdir = (unsigned char *) pw -> pw_dir;
#ifdef VMS
	      nm = p + 1;	/* skip the terminator */
#else
	      nm = p;
#ifdef DOS_NT
	      collapse_newdir = 0;
#endif
#endif /* VMS */
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
      /* Get default directory if needed to make nm absolute. */
      if (!IS_DIRECTORY_SEP (nm[0]))
	{
	  newdir = alloca (MAXPATHLEN + 1);
	  if (!getdefdir (toupper (drive) - 'A' + 1, newdir))
	    newdir = NULL;
	}
      if (!newdir)
	{
	  /* Either nm starts with /, or drive isn't mounted. */
	  newdir = alloca (4);
	  newdir[0] = DRIVE_LETTER (drive);
	  newdir[1] = ':';
	  newdir[2] = '/';
	  newdir[3] = 0;
	}
    }
#endif /* DOS_NT */

  /* Finally, if no prefix has been specified and nm is not absolute,
     then it must be expanded relative to default_directory. */

  if (1
#ifndef DOS_NT
      /* /... alone is not absolute on DOS and Windows. */
      && !IS_DIRECTORY_SEP (nm[0])
#endif
#ifdef WINDOWSNT
      && !(IS_DIRECTORY_SEP (nm[0]) && IS_DIRECTORY_SEP (nm[1]))
#endif
#ifdef VMS
      && !index (nm, ':')
#endif
      && !newdir)
    {
      newdir = XSTRING (default_directory)->data;
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
      /* First ensure newdir is an absolute name. */
      if (
	  /* Detect MSDOS file names with drive specifiers.  */
	  ! (IS_DRIVE (newdir[0])
	     && IS_DEVICE_SEP (newdir[1]) && IS_DIRECTORY_SEP (newdir[2]))
#ifdef WINDOWSNT
	  /* Detect Windows file names in UNC format.  */
	  && ! (IS_DIRECTORY_SEP (newdir[0]) && IS_DIRECTORY_SEP (newdir[1]))
#endif
	  )
	{
	  /* Effectively, let newdir be (expand-file-name newdir cwd).
	     Because of the admonition against calling expand-file-name
	     when we have pointers into lisp strings, we accomplish this
	     indirectly by prepending newdir to nm if necessary, and using
	     cwd (or the wd of newdir's drive) as the new newdir. */

	  if (IS_DRIVE (newdir[0]) && newdir[1] == ':')
	    {
	      drive = newdir[0];
	      newdir += 2;
	    }
	  if (!IS_DIRECTORY_SEP (nm[0]))
	    {
	      char * tmp = alloca (strlen (newdir) + strlen (nm) + 2);
	      file_name_as_directory (tmp, newdir);
	      strcat (tmp, nm);
	      nm = tmp;
	    }
	  newdir = alloca (MAXPATHLEN + 1);
	  if (drive)
	    {
	      if (!getdefdir (toupper (drive) - 'A' + 1, newdir))
		newdir = "/";
	    }
	  else
	    getwd (newdir);
	}

      /* Strip off drive name from prefix, if present. */
      if (IS_DRIVE (newdir[0]) && newdir[1] == ':')
	{
	  drive = newdir[0];
	  newdir += 2;
	}

      /* Keep only a prefix from newdir if nm starts with slash
         (//server/share for UNC, nothing otherwise).  */
      if (IS_DIRECTORY_SEP (nm[0]) && collapse_newdir)
	{
#ifdef WINDOWSNT
	  if (IS_DIRECTORY_SEP (newdir[0]) && IS_DIRECTORY_SEP (newdir[1]))
	    {
	      newdir = strcpy (alloca (strlen (newdir) + 1), newdir);
	      p = newdir + 2;
	      while (*p && !IS_DIRECTORY_SEP (*p)) p++;
	      p++;
	      while (*p && !IS_DIRECTORY_SEP (*p)) p++;
	      *p = 0;
	    }
	  else
#endif
	    newdir = "";
	}
    }
#endif /* DOS_NT */

  if (newdir)
    {
      /* Get rid of any slash at the end of newdir, unless newdir is
	 just / or // (an incomplete UNC name).  */
      length = strlen (newdir);
      if (length > 1 && IS_DIRECTORY_SEP (newdir[length - 1])
#ifdef WINDOWSNT
	  && !(length == 2 && IS_DIRECTORY_SEP (newdir[0]))
#endif
	  )
	{
	  unsigned char *temp = (unsigned char *) alloca (length);
	  bcopy (newdir, temp, length - 1);
	  temp[length - 1] = 0;
	  newdir = temp;
	}
      tlen = length + 1;
    }
  else
    tlen = 0;

  /* Now concatenate the directory and name to new space in the stack frame */
  tlen += strlen (nm) + 1;
#ifdef DOS_NT
  /* Reserve space for drive specifier and escape prefix, since either
     or both may need to be inserted.  (The Microsoft x86 compiler
     produces incorrect code if the following two lines are combined.)  */
  target = (unsigned char *) alloca (tlen + 4);
  target += 4;
#else  /* not DOS_NT */
  target = (unsigned char *) alloca (tlen);
#endif /* not DOS_NT */
  *target = 0;

  if (newdir)
    {
#ifndef VMS
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
	    strcpy (target, newdir);
	}
      else
#endif
	file_name_as_directory (target, newdir);
    }

  strcat (target, nm);
#ifdef VMS
  if (index (target, '/'))
    strcpy (target, sys_translate_unix (target));
#endif /* VMS */

  /* ASSERT (IS_DIRECTORY_SEP (target[0])) if not VMS */

  /* Now canonicalize by removing `//', `/.' and `/foo/..' if they
     appear.  */

  p = target;
  o = target;

  while (*p)
    {
#ifdef VMS
      if (*p != ']' && *p != '>' && *p != '-')
	{
	  if (*p == '\\')
	    p++;
	  *o++ = *p++;
	}
      else if ((p[0] == ']' || p[0] == '>') && p[0] == p[1] + 2)
	/* brackets are offset from each other by 2 */
	{
	  p += 2;
	  if (*p != '.' && *p != '-' && o[-1] != '.')
	    /* convert [foo][bar] to [bar] */
	    while (o[-1] != '[' && o[-1] != '<')
	      o--;
	  else if (*p == '-' && *o != '.')
	    *--p = '.';
	}
      else if (p[0] == '-' && o[-1] == '.' &&
	       (p[1] == '.' || p[1] == ']' || p[1] == '>'))
	/* flush .foo.- ; leave - if stopped by '[' or '<' */
	{
	  do
	    o--;
	  while (o[-1] != '.' && o[-1] != '[' && o[-1] != '<');
	  if (p[1] == '.')      /* foo.-.bar ==> bar.  */
	    p += 2;
	  else if (o[-1] == '.') /* '.foo.-]' ==> ']' */
	    p++, o--;
	  /* else [foo.-] ==> [-] */
	}
      else
	{
#ifndef VMS4_4
	  if (*p == '-' &&
	      o[-1] != '[' && o[-1] != '<' && o[-1] != '.' &&
	      p[1] != ']' && p[1] != '>' && p[1] != '.')
	    *p = '_';
#endif /* VMS4_4 */
	  *o++ = *p++;
	}
#else /* not VMS */
      if (!IS_DIRECTORY_SEP (*p))
	{
	  *o++ = *p++;
	}
      else if (IS_DIRECTORY_SEP (p[0])
	       && p[1] == '.'
	       && (IS_DIRECTORY_SEP (p[2])
		   || p[2] == 0))
	{
	  /* If "/." is the entire filename, keep the "/".  Otherwise,
	     just delete the whole "/.".  */
	  if (o == target && p[2] == '\0')
	    *o++ = *p;
	  p += 2;
	}
      else if (IS_DIRECTORY_SEP (p[0]) && p[1] == '.' && p[2] == '.'
	       /* `/../' is the "superroot" on certain file systems.  */
	       && o != target
	       && (IS_DIRECTORY_SEP (p[3]) || p[3] == 0))
	{
	  while (o != target && (--o) && !IS_DIRECTORY_SEP (*o))
	    ;
	  /* Keep initial / only if this is the whole name.  */
	  if (o == target && IS_ANY_SEP (*o) && p[3] == 0)
	    ++o;
	  p += 3;
	}
      else if (p > target
	       && IS_DIRECTORY_SEP (p[0]) && IS_DIRECTORY_SEP (p[1]))
	{
	  /* Collapse multiple `/' in a row.  */
	  *o++ = *p++;
	  while (IS_DIRECTORY_SEP (*p))
	    ++p;
	}
      else
	{
	  *o++ = *p++;
	}
#endif /* not VMS */
    }

#ifdef DOS_NT
  /* At last, set drive name. */
#ifdef WINDOWSNT
  /* Except for network file name.  */
  if (!(IS_DIRECTORY_SEP (target[0]) && IS_DIRECTORY_SEP (target[1])))
#endif /* WINDOWSNT */
    {
      if (!drive) abort ();
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
  CORRECT_DIR_SEPS (target);
#endif /* DOS_NT */

  return make_string (target, o - target);
}

#if 0
/* PLEASE DO NOT DELETE THIS COMMENTED-OUT VERSION!
   This is the old version of expand-file-name, before it was thoroughly
   rewritten for Emacs 10.31.  We leave this version here commented-out,
   because the code is very complex and likely to have subtle bugs.  If
   bugs _are_ found, it might be of interest to look at the old code and
   see what did it do in the relevant situation.

   Don't remove this code: it's true that it will be accessible via CVS,
   but a few years from deletion, people will forget it is there.  */

/* Changed this DEFUN to a DEAFUN, so as not to confuse `make-docfile'.  */
DEAFUN ("expand-file-name", Fexpand_file_name, Sexpand_file_name, 1, 2, 0,
  "Convert FILENAME to absolute, and canonicalize it.\n\
Second arg DEFAULT is directory to start with if FILENAME is relative\n\
 (does not start with slash); if DEFAULT is nil or missing,\n\
the current buffer's value of default-directory is used.\n\
Filenames containing `.' or `..' as components are simplified;\n\
initial `~/' expands to your home directory.\n\
See also the function `substitute-in-file-name'.")
     (name, defalt)
     Lisp_Object name, defalt;
{
  unsigned char *nm;

  register unsigned char *newdir, *p, *o;
  int tlen;
  unsigned char *target;
  struct passwd *pw;
  int lose;
#ifdef VMS
  unsigned char * colon = 0;
  unsigned char * close = 0;
  unsigned char * slash = 0;
  unsigned char * brack = 0;
  int lbrack = 0, rbrack = 0;
  int dots = 0;
#endif /* VMS */

  CHECK_STRING (name);

#ifdef VMS
  /* Filenames on VMS are always upper case.  */
  name = Fupcase (name);
#endif

  nm = XSTRING (name)->data;

  /* If nm is absolute, flush ...// and detect /./ and /../.
     If no /./ or /../ we can return right away.  */
  if (
      nm[0] == '/'
#ifdef VMS
      || index (nm, ':')
#endif /* VMS */
      )
    {
      p = nm;
      lose = 0;
      while (*p)
	{
	  if (p[0] == '/' && p[1] == '/'
#ifdef APOLLO
	      /* // at start of filename is meaningful on Apollo system.  */
	      && nm != p
#endif /* APOLLO */
	      )
	    nm = p + 1;
	  if (p[0] == '/' && p[1] == '~')
	    nm = p + 1, lose = 1;
	  if (p[0] == '/' && p[1] == '.'
	      && (p[2] == '/' || p[2] == 0
		  || (p[2] == '.' && (p[3] == '/' || p[3] == 0))))
	    lose = 1;
#ifdef VMS
	  if (p[0] == '\\')
	    lose = 1;
	  if (p[0] == '/') {
	    /* if dev:[dir]/, move nm to / */
	    if (!slash && p > nm && (brack || colon)) {
	      nm = (brack ? brack + 1 : colon + 1);
	      lbrack = rbrack = 0;
	      brack = 0;
	      colon = 0;
	    }
	    slash = p;
	  }
	  if (p[0] == '-')
#ifndef VMS4_4
	    /* VMS pre V4.4,convert '-'s in filenames. */
	    if (lbrack == rbrack)
	      {
		if (dots < 2)   /* this is to allow negative version numbers */
		  p[0] = '_';
	      }
	    else
#endif /* VMS4_4 */
	      if (lbrack > rbrack &&
		  ((p[-1] == '.' || p[-1] == '[' || p[-1] == '<') &&
		   (p[1] == '.' || p[1] == ']' || p[1] == '>')))
		lose = 1;
#ifndef VMS4_4
	      else
		p[0] = '_';
#endif /* VMS4_4 */
	  /* count open brackets, reset close bracket pointer */
	  if (p[0] == '[' || p[0] == '<')
	    lbrack++, brack = 0;
	  /* count close brackets, set close bracket pointer */
	  if (p[0] == ']' || p[0] == '>')
	    rbrack++, brack = p;
	  /* detect ][ or >< */
	  if ((p[0] == ']' || p[0] == '>') && (p[1] == '[' || p[1] == '<'))
	    lose = 1;
	  if ((p[0] == ':' || p[0] == ']' || p[0] == '>') && p[1] == '~')
	    nm = p + 1, lose = 1;
	  if (p[0] == ':' && (colon || slash))
	    /* if dev1:[dir]dev2:, move nm to dev2: */
	    if (brack)
	      {
		nm = brack + 1;
		brack = 0;
	      }
	    /* If /name/dev:, move nm to dev: */
	    else if (slash)
	      nm = slash + 1;
	    /* If node::dev:, move colon following dev */
	    else if (colon && colon[-1] == ':')
	      colon = p;
	    /* If dev1:dev2:, move nm to dev2: */
	    else if (colon && colon[-1] != ':')
	      {
		nm = colon + 1;
		colon = 0;
	      }
	  if (p[0] == ':' && !colon)
	    {
	      if (p[1] == ':')
		p++;
	      colon = p;
	    }
	  if (lbrack == rbrack)
	    if (p[0] == ';')
	      dots = 2;
	    else if (p[0] == '.')
	      dots++;
#endif /* VMS */
	  p++;
	}
      if (!lose)
	{
#ifdef VMS
	  if (index (nm, '/'))
	    return build_string (sys_translate_unix (nm));
#endif /* VMS */
	  if (nm == XSTRING (name)->data)
	    return name;
	  return build_string (nm);
	}
    }

  /* Now determine directory to start with and put it in NEWDIR */

  newdir = 0;

  if (nm[0] == '~')             /* prefix ~ */
    if (nm[1] == '/'
#ifdef VMS
	|| nm[1] == ':'
#endif /* VMS */
	|| nm[1] == 0)/* ~/filename */
      {
	if (!(newdir = (unsigned char *) egetenv ("HOME")))
	  newdir = (unsigned char *) "";
	nm++;
#ifdef VMS
	nm++;                   /* Don't leave the slash in nm.  */
#endif /* VMS */
      }
    else  /* ~user/filename */
      {
	/* Get past ~ to user */
	unsigned char *user = nm + 1;
	/* Find end of name. */
	unsigned char *ptr = (unsigned char *) index (user, '/');
	int len = ptr ? ptr - user : strlen (user);
#ifdef VMS
	unsigned char *ptr1 = index (user, ':');
	if (ptr1 != 0 && ptr1 - user < len)
	  len = ptr1 - user;
#endif /* VMS */
	/* Copy the user name into temp storage. */
	o = (unsigned char *) alloca (len + 1);
	bcopy ((char *) user, o, len);
	o[len] = 0;

	/* Look up the user name. */
	pw = (struct passwd *) getpwnam (o + 1);
	if (!pw)
	  error ("\"%s\" isn't a registered user", o + 1);

	newdir = (unsigned char *) pw->pw_dir;

	/* Discard the user name from NM.  */
	nm += len;
      }

  if (nm[0] != '/'
#ifdef VMS
      && !index (nm, ':')
#endif /* not VMS */
      && !newdir)
    {
      if (NILP (defalt))
	defalt = current_buffer->directory;
      CHECK_STRING (defalt);
      newdir = XSTRING (defalt)->data;
    }

  /* Now concatenate the directory and name to new space in the stack frame */

  tlen = (newdir ? strlen (newdir) + 1 : 0) + strlen (nm) + 1;
  target = (unsigned char *) alloca (tlen);
  *target = 0;

  if (newdir)
    {
#ifndef VMS
      if (nm[0] == 0 || nm[0] == '/')
	strcpy (target, newdir);
      else
#endif
      file_name_as_directory (target, newdir);
    }

  strcat (target, nm);
#ifdef VMS
  if (index (target, '/'))
    strcpy (target, sys_translate_unix (target));
#endif /* VMS */

  /* Now canonicalize by removing /. and /foo/.. if they appear */

  p = target;
  o = target;

  while (*p)
    {
#ifdef VMS
      if (*p != ']' && *p != '>' && *p != '-')
	{
	  if (*p == '\\')
	    p++;
	  *o++ = *p++;
	}
      else if ((p[0] == ']' || p[0] == '>') && p[0] == p[1] + 2)
	/* brackets are offset from each other by 2 */
	{
	  p += 2;
	  if (*p != '.' && *p != '-' && o[-1] != '.')
	    /* convert [foo][bar] to [bar] */
	    while (o[-1] != '[' && o[-1] != '<')
	      o--;
	  else if (*p == '-' && *o != '.')
	    *--p = '.';
	}
      else if (p[0] == '-' && o[-1] == '.' &&
	       (p[1] == '.' || p[1] == ']' || p[1] == '>'))
	/* flush .foo.- ; leave - if stopped by '[' or '<' */
	{
	  do
	    o--;
	  while (o[-1] != '.' && o[-1] != '[' && o[-1] != '<');
	  if (p[1] == '.')      /* foo.-.bar ==> bar.  */
	    p += 2;
	  else if (o[-1] == '.') /* '.foo.-]' ==> ']' */
	    p++, o--;
	  /* else [foo.-] ==> [-] */
	}
      else
	{
#ifndef VMS4_4
	  if (*p == '-' &&
	      o[-1] != '[' && o[-1] != '<' && o[-1] != '.' &&
	      p[1] != ']' && p[1] != '>' && p[1] != '.')
	    *p = '_';
#endif /* VMS4_4 */
	  *o++ = *p++;
	}
#else /* not VMS */
      if (*p != '/')
	{
	  *o++ = *p++;
	}
      else if (!strncmp (p, "//", 2)
#ifdef APOLLO
	       /* // at start of filename is meaningful in Apollo system.  */
	       && o != target
#endif /* APOLLO */
	       )
	{
	  o = target;
	  p++;
	}
      else if (p[0] == '/' && p[1] == '.' &&
	       (p[2] == '/' || p[2] == 0))
	p += 2;
      else if (!strncmp (p, "/..", 3)
	       /* `/../' is the "superroot" on certain file systems.  */
	       && o != target
	       && (p[3] == '/' || p[3] == 0))
	{
	  while (o != target && *--o != '/')
	    ;
#ifdef APOLLO
	  if (o == target + 1 && o[-1] == '/' && o[0] == '/')
	    ++o;
	  else
#endif /* APOLLO */
	  if (o == target && *o == '/')
	    ++o;
	  p += 3;
	}
      else
	{
	  *o++ = *p++;
	}
#endif /* not VMS */
    }

  return make_string (target, o - target);
}
#endif

DEFUN ("substitute-in-file-name", Fsubstitute_in_file_name,
       Ssubstitute_in_file_name, 1, 1, 0,
       doc: /* Substitute environment variables referred to in FILENAME.
`$FOO' where FOO is an environment variable name means to substitute
the value of that variable.  The variable name should be terminated
with a character not a letter, digit or underscore; otherwise, enclose
the entire variable name in braces.
If `/~' appears, all of FILENAME through that `/' is discarded.

On VMS, `$' substitution is not done; this function does little and only
duplicates what `expand-file-name' does.  */)
     (filename)
     Lisp_Object filename;
{
  unsigned char *nm;

  register unsigned char *s, *p, *o, *x, *endp;
  unsigned char *target = NULL;
  int total = 0;
  int substituted = 0;
  unsigned char *xnm;
  struct passwd *pw;
  Lisp_Object handler;

  CHECK_STRING (filename);

  /* If the file name has special constructs in it,
     call the corresponding file handler.  */
  handler = Ffind_file_name_handler (filename, Qsubstitute_in_file_name);
  if (!NILP (handler))
    return call2 (handler, Qsubstitute_in_file_name, filename);

  nm = XSTRING (filename)->data;
#ifdef DOS_NT
  nm = strcpy (alloca (strlen (nm) + 1), nm);
  CORRECT_DIR_SEPS (nm);
  substituted = (strcmp (nm, XSTRING (filename)->data) != 0);
#endif
  endp = nm + STRING_BYTES (XSTRING (filename));

  /* If /~ or // appears, discard everything through first slash.  */

  for (p = nm; p != endp; p++)
    {
      if ((p[0] == '~'
#if defined (APOLLO) || defined (WINDOWSNT)
	   /* // at start of file name is meaningful in Apollo and
	      WindowsNT systems.  */
	   || (IS_DIRECTORY_SEP (p[0]) && p - 1 != nm)
#else /* not (APOLLO || WINDOWSNT) */
	   || IS_DIRECTORY_SEP (p[0])
#endif /* not (APOLLO || WINDOWSNT) */
	   )
	  && p != nm
	  && (0
#ifdef VMS
	      || p[-1] == ':' || p[-1] == ']' || p[-1] == '>'
#endif /* VMS */
	      || IS_DIRECTORY_SEP (p[-1])))
	{
	  for (s = p; *s && (!IS_DIRECTORY_SEP (*s)
#ifdef VMS
			      && *s != ':'
#endif /* VMS */
			      ); s++);
	  if (p[0] == '~' && s > p + 1)	/* we've got "/~something/" */
	    {
	      o = (unsigned char *) alloca (s - p + 1);
	      bcopy ((char *) p, o, s - p);
	      o [s - p] = 0;

	      pw = (struct passwd *) getpwnam (o + 1);
	    }
	  /* If we have ~/ or ~user and `user' exists, discard
	     everything up to ~.  But if `user' does not exist, leave
	     ~user alone, it might be a literal file name.  */
	  if (IS_DIRECTORY_SEP (p[0]) || s == p + 1 || pw)
	    {
	      nm = p;
	      substituted = 1;
	    }
	}
#ifdef DOS_NT
      /* see comment in expand-file-name about drive specifiers */
      else if (IS_DRIVE (p[0]) && p[1] == ':'
	       && p > nm && IS_DIRECTORY_SEP (p[-1]))
	{
	  nm = p;
	  substituted = 1;
	}
#endif /* DOS_NT */
    }

#ifdef VMS
  return build_string (nm);
#else

  /* See if any variables are substituted into the string
     and find the total length of their values in `total' */

  for (p = nm; p != endp;)
    if (*p != '$')
      p++;
    else
      {
	p++;
	if (p == endp)
	  goto badsubst;
	else if (*p == '$')
	  {
	    /* "$$" means a single "$" */
	    p++;
	    total -= 1;
	    substituted = 1;
	    continue;
	  }
	else if (*p == '{')
	  {
	    o = ++p;
	    while (p != endp && *p != '}') p++;
	    if (*p != '}') goto missingclose;
	    s = p;
	  }
	else
	  {
	    o = p;
	    while (p != endp && (isalnum (*p) || *p == '_')) p++;
	    s = p;
	  }

	/* Copy out the variable name */
	target = (unsigned char *) alloca (s - o + 1);
	strncpy (target, o, s - o);
	target[s - o] = 0;
#ifdef DOS_NT
	strupr (target); /* $home == $HOME etc.  */
#endif /* DOS_NT */

	/* Get variable value */
	o = (unsigned char *) egetenv (target);
	if (o)
	  {
	    total += strlen (o);
	    substituted = 1;
	  }
	else if (*p == '}')
	  goto badvar;
      }

  if (!substituted)
    return filename;

  /* If substitution required, recopy the string and do it */
  /* Make space in stack frame for the new copy */
  xnm = (unsigned char *) alloca (STRING_BYTES (XSTRING (filename)) + total + 1);
  x = xnm;

  /* Copy the rest of the name through, replacing $ constructs with values */
  for (p = nm; *p;)
    if (*p != '$')
      *x++ = *p++;
    else
      {
	p++;
	if (p == endp)
	  goto badsubst;
	else if (*p == '$')
	  {
	    *x++ = *p++;
	    continue;
	  }
	else if (*p == '{')
	  {
	    o = ++p;
	    while (p != endp && *p != '}') p++;
	    if (*p != '}') goto missingclose;
	    s = p++;
	  }
	else
	  {
	    o = p;
	    while (p != endp && (isalnum (*p) || *p == '_')) p++;
	    s = p;
	  }

	/* Copy out the variable name */
	target = (unsigned char *) alloca (s - o + 1);
	strncpy (target, o, s - o);
	target[s - o] = 0;
#ifdef DOS_NT
	strupr (target); /* $home == $HOME etc.  */
#endif /* DOS_NT */

	/* Get variable value */
	o = (unsigned char *) egetenv (target);
	if (!o)
	  {
	    *x++ = '$';
	    strcpy (x, target); x+= strlen (target);
	  }
	else if (STRING_MULTIBYTE (filename))
	  {
	    /* If the original string is multibyte,
	       convert what we substitute into multibyte.  */
	    while (*o)
	      {
		int c = unibyte_char_to_multibyte (*o++);
		x += CHAR_STRING (c, x);
	      }
	  }
	else
	  {
	    strcpy (x, o);
	    x += strlen (o);
	  }
      }

  *x = 0;

  /* If /~ or // appears, discard everything through first slash.  */

  for (p = xnm; p != x; p++)
    if ((p[0] == '~'
#if defined (APOLLO) || defined (WINDOWSNT)
	 || (IS_DIRECTORY_SEP (p[0]) && p - 1 != xnm)
#else /* not (APOLLO || WINDOWSNT) */
	 || IS_DIRECTORY_SEP (p[0])
#endif /* not (APOLLO || WINDOWSNT) */
	 )
	&& p != xnm && IS_DIRECTORY_SEP (p[-1]))
      xnm = p;
#ifdef DOS_NT
    else if (IS_DRIVE (p[0]) && p[1] == ':'
	     && p > xnm && IS_DIRECTORY_SEP (p[-1]))
      xnm = p;
#endif

  if (STRING_MULTIBYTE (filename))
    return make_string (xnm, x - xnm);
  return make_unibyte_string (xnm, x - xnm);

 badsubst:
  error ("Bad format environment-variable substitution");
 missingclose:
  error ("Missing \"}\" in environment-variable substitution");
 badvar:
  error ("Substituting nonexistent environment variable \"%s\"", target);

  /* NOTREACHED */
#endif /* not VMS */
  return Qnil;
}

/* A slightly faster and more convenient way to get
   (directory-file-name (expand-file-name FOO)).  */

Lisp_Object
expand_and_dir_to_file (filename, defdir)
     Lisp_Object filename, defdir;
{
  register Lisp_Object absname;

  absname = Fexpand_file_name (filename, defdir);
#ifdef VMS
  {
    register int c = XSTRING (absname)->data[STRING_BYTES (XSTRING (absname)) - 1];
    if (c == ':' || c == ']' || c == '>')
      absname = Fdirectory_file_name (absname);
  }
#else
  /* Remove final slash, if any (unless this is the root dir).
     stat behaves differently depending!  */
  if (XSTRING (absname)->size > 1
      && IS_DIRECTORY_SEP (XSTRING (absname)->data[STRING_BYTES (XSTRING (absname)) - 1])
      && !IS_DEVICE_SEP (XSTRING (absname)->data[STRING_BYTES (XSTRING (absname))-2]))
    /* We cannot take shortcuts; they might be wrong for magic file names.  */
    absname = Fdirectory_file_name (absname);
#endif
  return absname;
}

/* Signal an error if the file ABSNAME already exists.
   If INTERACTIVE is nonzero, ask the user whether to proceed,
   and bypass the error if the user says to go ahead.
   QUERYSTRING is a name for the action that is being considered
   to alter the file.

   *STATPTR is used to store the stat information if the file exists.
   If the file does not exist, STATPTR->st_mode is set to 0.
   If STATPTR is null, we don't store into it.

   If QUICK is nonzero, we ask for y or n, not yes or no.  */

void
barf_or_query_if_file_exists (absname, querystring, interactive, statptr, quick)
     Lisp_Object absname;
     unsigned char *querystring;
     int interactive;
     struct stat *statptr;
     int quick;
{
  register Lisp_Object tem, encoded_filename;
  struct stat statbuf;
  struct gcpro gcpro1;

  encoded_filename = ENCODE_FILE (absname);

  /* stat is a good way to tell whether the file exists,
     regardless of what access permissions it has.  */
  if (stat (XSTRING (encoded_filename)->data, &statbuf) >= 0)
    {
      if (! interactive)
	Fsignal (Qfile_already_exists,
		 Fcons (build_string ("File already exists"),
			Fcons (absname, Qnil)));
      GCPRO1 (absname);
      tem = format1 ("File %s already exists; %s anyway? ",
		     XSTRING (absname)->data, querystring);
      if (quick)
	tem = Fy_or_n_p (tem);
      else
	tem = do_yes_or_no_p (tem);
      UNGCPRO;
      if (NILP (tem))
	Fsignal (Qfile_already_exists,
		 Fcons (build_string ("File already exists"),
			Fcons (absname, Qnil)));
      if (statptr)
	*statptr = statbuf;
    }
  else
    {
      if (statptr)
	statptr->st_mode = 0;
    }
  return;
}

DEFUN ("copy-file", Fcopy_file, Scopy_file, 2, 4,
       "fCopy file: \nFCopy %s to file: \np\nP",
       doc: /* Copy FILE to NEWNAME.  Both args must be strings.
If NEWNAME names a directory, copy FILE there.
Signals a `file-already-exists' error if file NEWNAME already exists,
unless a third argument OK-IF-ALREADY-EXISTS is supplied and non-nil.
A number as third arg means request confirmation if NEWNAME already exists.
This is what happens in interactive use with M-x.
Fourth arg KEEP-TIME non-nil means give the new file the same
last-modified time as the old one.  (This works on only some systems.)
A prefix arg makes KEEP-TIME non-nil.  */)
     (file, newname, ok_if_already_exists, keep_time)
     Lisp_Object file, newname, ok_if_already_exists, keep_time;
{
  int ifd, ofd, n;
  char buf[16 * 1024];
  struct stat st, out_st;
  Lisp_Object handler;
  struct gcpro gcpro1, gcpro2, gcpro3, gcpro4;
  int count = specpdl_ptr - specpdl;
  int input_file_statable_p;
  Lisp_Object encoded_file, encoded_newname;

  encoded_file = encoded_newname = Qnil;
  GCPRO4 (file, newname, encoded_file, encoded_newname);
  CHECK_STRING (file);
  CHECK_STRING (newname);

  if (!NILP (Ffile_directory_p (newname)))
    newname = Fexpand_file_name (file, newname);
  else
    newname = Fexpand_file_name (newname, Qnil);

  file = Fexpand_file_name (file, Qnil);

  /* If the input file name has special constructs in it,
     call the corresponding file handler.  */
  handler = Ffind_file_name_handler (file, Qcopy_file);
  /* Likewise for output file name.  */
  if (NILP (handler))
    handler = Ffind_file_name_handler (newname, Qcopy_file);
  if (!NILP (handler))
    RETURN_UNGCPRO (call5 (handler, Qcopy_file, file, newname,
			   ok_if_already_exists, keep_time));

  encoded_file = ENCODE_FILE (file);
  encoded_newname = ENCODE_FILE (newname);

  if (NILP (ok_if_already_exists)
      || INTEGERP (ok_if_already_exists))
    barf_or_query_if_file_exists (encoded_newname, "copy to it",
				  INTEGERP (ok_if_already_exists), &out_st, 0);
  else if (stat (XSTRING (encoded_newname)->data, &out_st) < 0)
    out_st.st_mode = 0;

#ifdef WINDOWSNT
  if (!CopyFile (XSTRING (encoded_file)->data,
		 XSTRING (encoded_newname)->data, 
		 FALSE))
    report_file_error ("Copying file", Fcons (file, Fcons (newname, Qnil)));
  else if (NILP (keep_time))
    {
      EMACS_TIME now;
      DWORD attributes;
      char * filename;

      EMACS_GET_TIME (now);
      filename = XSTRING (encoded_newname)->data;

      /* Ensure file is writable while its modified time is set.  */
      attributes = GetFileAttributes (filename);
      SetFileAttributes (filename, attributes & ~FILE_ATTRIBUTE_READONLY);
      if (set_file_times (filename, now, now))
	{
	  /* Restore original attributes.  */
	  SetFileAttributes (filename, attributes);
	  Fsignal (Qfile_date_error,
		   Fcons (build_string ("Cannot set file date"),
			  Fcons (newname, Qnil)));
	}
      /* Restore original attributes.  */
      SetFileAttributes (filename, attributes);
    }
#else /* not WINDOWSNT */
  ifd = emacs_open (XSTRING (encoded_file)->data, O_RDONLY, 0);
  if (ifd < 0)
    report_file_error ("Opening input file", Fcons (file, Qnil));

  record_unwind_protect (close_file_unwind, make_number (ifd));

  /* We can only copy regular files and symbolic links.  Other files are not
     copyable by us. */
  input_file_statable_p = (fstat (ifd, &st) >= 0);

#if !defined (DOS_NT) || __DJGPP__ > 1
  if (out_st.st_mode != 0
      && st.st_dev == out_st.st_dev && st.st_ino == out_st.st_ino)
    {
      errno = 0;
      report_file_error ("Input and output files are the same",
			 Fcons (file, Fcons (newname, Qnil)));
    }
#endif

#if defined (S_ISREG) && defined (S_ISLNK)
  if (input_file_statable_p)
    {
      if (!(S_ISREG (st.st_mode)) && !(S_ISLNK (st.st_mode)))
	{
#if defined (EISDIR)
	  /* Get a better looking error message. */
	  errno = EISDIR;
#endif /* EISDIR */
	  report_file_error ("Non-regular file", Fcons (file, Qnil));
	}
    }
#endif /* S_ISREG && S_ISLNK */

#ifdef VMS
  /* Create the copy file with the same record format as the input file */
  ofd = sys_creat (XSTRING (encoded_newname)->data, 0666, ifd);
#else
#ifdef MSDOS
  /* System's default file type was set to binary by _fmode in emacs.c.  */
  ofd = creat (XSTRING (encoded_newname)->data, S_IREAD | S_IWRITE);
#else /* not MSDOS */
  ofd = creat (XSTRING (encoded_newname)->data, 0666);
#endif /* not MSDOS */
#endif /* VMS */
  if (ofd < 0)
    report_file_error ("Opening output file", Fcons (newname, Qnil));

  record_unwind_protect (close_file_unwind, make_number (ofd));

  immediate_quit = 1;
  QUIT;
  while ((n = emacs_read (ifd, buf, sizeof buf)) > 0)
    if (emacs_write (ofd, buf, n) != n)
      report_file_error ("I/O error", Fcons (newname, Qnil));
  immediate_quit = 0;

  /* Closing the output clobbers the file times on some systems.  */
  if (emacs_close (ofd) < 0)
    report_file_error ("I/O error", Fcons (newname, Qnil));

  if (input_file_statable_p)
    {
      if (!NILP (keep_time))
	{
	  EMACS_TIME atime, mtime;
	  EMACS_SET_SECS_USECS (atime, st.st_atime, 0);
	  EMACS_SET_SECS_USECS (mtime, st.st_mtime, 0);
	  if (set_file_times (XSTRING (encoded_newname)->data,
			      atime, mtime))
	    Fsignal (Qfile_date_error,
		     Fcons (build_string ("Cannot set file date"),
			    Fcons (newname, Qnil)));
	}
#ifndef MSDOS
      chmod (XSTRING (encoded_newname)->data, st.st_mode & 07777);
#else /* MSDOS */
#if defined (__DJGPP__) && __DJGPP__ > 1
      /* In DJGPP v2.0 and later, fstat usually returns true file mode bits,
         and if it can't, it tells so.  Otherwise, under MSDOS we usually
         get only the READ bit, which will make the copied file read-only,
         so it's better not to chmod at all.  */
      if ((_djstat_flags & _STFAIL_WRITEBIT) == 0)
	chmod (XSTRING (encoded_newname)->data, st.st_mode & 07777);
#endif /* DJGPP version 2 or newer */
#endif /* MSDOS */
    }

  emacs_close (ifd);
#endif /* WINDOWSNT */

  /* Discard the unwind protects.  */
  specpdl_ptr = specpdl + count;

  UNGCPRO;
  return Qnil;
}

DEFUN ("make-directory-internal", Fmake_directory_internal,
       Smake_directory_internal, 1, 1, 0,
       doc: /* Create a new directory named DIRECTORY.  */)
     (directory)
     Lisp_Object directory;
{
  unsigned char *dir;
  Lisp_Object handler;
  Lisp_Object encoded_dir;

  CHECK_STRING (directory);
  directory = Fexpand_file_name (directory, Qnil);

  handler = Ffind_file_name_handler (directory, Qmake_directory_internal);
  if (!NILP (handler))
    return call2 (handler, Qmake_directory_internal, directory);

  encoded_dir = ENCODE_FILE (directory);

  dir = XSTRING (encoded_dir)->data;

#ifdef WINDOWSNT
  if (mkdir (dir) != 0)
#else
  if (mkdir (dir, 0777) != 0)
#endif
    report_file_error ("Creating directory", Flist (1, &directory));

  return Qnil;
}

DEFUN ("delete-directory", Fdelete_directory, Sdelete_directory, 1, 1, "FDelete directory: ",
       doc: /* Delete the directory named DIRECTORY.  */)
     (directory)
     Lisp_Object directory;
{
  unsigned char *dir;
  Lisp_Object handler;
  Lisp_Object encoded_dir;

  CHECK_STRING (directory);
  directory = Fdirectory_file_name (Fexpand_file_name (directory, Qnil));

  handler = Ffind_file_name_handler (directory, Qdelete_directory);
  if (!NILP (handler))
    return call2 (handler, Qdelete_directory, directory);

  encoded_dir = ENCODE_FILE (directory);

  dir = XSTRING (encoded_dir)->data;

  if (rmdir (dir) != 0)
    report_file_error ("Removing directory", Flist (1, &directory));

  return Qnil;
}

DEFUN ("delete-file", Fdelete_file, Sdelete_file, 1, 1, "fDelete file: ",
       doc: /* Delete file named FILENAME.
If file has multiple names, it continues to exist with the other names.  */)
     (filename)
     Lisp_Object filename;
{
  Lisp_Object handler;
  Lisp_Object encoded_file;

  CHECK_STRING (filename);
  filename = Fexpand_file_name (filename, Qnil);

  handler = Ffind_file_name_handler (filename, Qdelete_file);
  if (!NILP (handler))
    return call2 (handler, Qdelete_file, filename);

  encoded_file = ENCODE_FILE (filename);

  if (0 > unlink (XSTRING (encoded_file)->data))
    report_file_error ("Removing old name", Flist (1, &filename));
  return Qnil;
}

static Lisp_Object
internal_delete_file_1 (ignore)
     Lisp_Object ignore;
{
  return Qt;
}

/* Delete file FILENAME, returning 1 if successful and 0 if failed.  */

int
internal_delete_file (filename)
     Lisp_Object filename;
{
  return NILP (internal_condition_case_1 (Fdelete_file, filename,
					  Qt, internal_delete_file_1));
}

DEFUN ("rename-file", Frename_file, Srename_file, 2, 3,
       "fRename file: \nFRename %s to file: \np",
       doc: /* Rename FILE as NEWNAME.  Both args strings.
If file has names other than FILE, it continues to have those names.
Signals a `file-already-exists' error if a file NEWNAME already exists
unless optional third argument OK-IF-ALREADY-EXISTS is non-nil.
A number as third arg means request confirmation if NEWNAME already exists.
This is what happens in interactive use with M-x.  */)
     (file, newname, ok_if_already_exists)
     Lisp_Object file, newname, ok_if_already_exists;
{
#ifdef NO_ARG_ARRAY
  Lisp_Object args[2];
#endif
  Lisp_Object handler;
  struct gcpro gcpro1, gcpro2, gcpro3, gcpro4;
  Lisp_Object encoded_file, encoded_newname;

  encoded_file = encoded_newname = Qnil;
  GCPRO4 (file, newname, encoded_file, encoded_newname);
  CHECK_STRING (file);
  CHECK_STRING (newname);
  file = Fexpand_file_name (file, Qnil);
  newname = Fexpand_file_name (newname, Qnil);

  /* If the file name has special constructs in it,
     call the corresponding file handler.  */
  handler = Ffind_file_name_handler (file, Qrename_file);
  if (NILP (handler))
    handler = Ffind_file_name_handler (newname, Qrename_file);
  if (!NILP (handler))
    RETURN_UNGCPRO (call4 (handler, Qrename_file,
			   file, newname, ok_if_already_exists));

  encoded_file = ENCODE_FILE (file);
  encoded_newname = ENCODE_FILE (newname);

#ifdef DOS_NT
  /* If the file names are identical but for the case, don't ask for
     confirmation: they simply want to change the letter-case of the
     file name.  */
  if (NILP (Fstring_equal (Fdowncase (file), Fdowncase (newname))))
#endif
  if (NILP (ok_if_already_exists)
      || INTEGERP (ok_if_already_exists))
    barf_or_query_if_file_exists (encoded_newname, "rename to it",
				  INTEGERP (ok_if_already_exists), 0, 0);
#ifndef BSD4_1
  if (0 > rename (XSTRING (encoded_file)->data, XSTRING (encoded_newname)->data))
#else
  if (0 > link (XSTRING (encoded_file)->data, XSTRING (encoded_newname)->data)
      || 0 > unlink (XSTRING (encoded_file)->data))
#endif
    {
      if (errno == EXDEV)
	{
	  Fcopy_file (file, newname,
		      /* We have already prompted if it was an integer,
			 so don't have copy-file prompt again.  */
		      NILP (ok_if_already_exists) ? Qnil : Qt, Qt);
	  Fdelete_file (file);
	}
      else
#ifdef NO_ARG_ARRAY
	{
	  args[0] = file;
	  args[1] = newname;
	  report_file_error ("Renaming", Flist (2, args));
	}
#else
	report_file_error ("Renaming", Flist (2, &file));
#endif
    }
  UNGCPRO;
  return Qnil;
}

DEFUN ("add-name-to-file", Fadd_name_to_file, Sadd_name_to_file, 2, 3,
       "fAdd name to file: \nFName to add to %s: \np",
       doc: /* Give FILE additional name NEWNAME.  Both args strings.
Signals a `file-already-exists' error if a file NEWNAME already exists
unless optional third argument OK-IF-ALREADY-EXISTS is non-nil.
A number as third arg means request confirmation if NEWNAME already exists.
This is what happens in interactive use with M-x.  */)
     (file, newname, ok_if_already_exists)
     Lisp_Object file, newname, ok_if_already_exists;
{
#ifdef NO_ARG_ARRAY
  Lisp_Object args[2];
#endif
  Lisp_Object handler;
  Lisp_Object encoded_file, encoded_newname;
  struct gcpro gcpro1, gcpro2, gcpro3, gcpro4;

  GCPRO4 (file, newname, encoded_file, encoded_newname);
  encoded_file = encoded_newname = Qnil;
  CHECK_STRING (file);
  CHECK_STRING (newname);
  file = Fexpand_file_name (file, Qnil);
  newname = Fexpand_file_name (newname, Qnil);

  /* If the file name has special constructs in it,
     call the corresponding file handler.  */
  handler = Ffind_file_name_handler (file, Qadd_name_to_file);
  if (!NILP (handler))
    RETURN_UNGCPRO (call4 (handler, Qadd_name_to_file, file,
			   newname, ok_if_already_exists));

  /* If the new name has special constructs in it,
     call the corresponding file handler.  */
  handler = Ffind_file_name_handler (newname, Qadd_name_to_file);
  if (!NILP (handler))
    RETURN_UNGCPRO (call4 (handler, Qadd_name_to_file, file,
			   newname, ok_if_already_exists));

  encoded_file = ENCODE_FILE (file);
  encoded_newname = ENCODE_FILE (newname);

  if (NILP (ok_if_already_exists)
      || INTEGERP (ok_if_already_exists))
    barf_or_query_if_file_exists (encoded_newname, "make it a new name",
				  INTEGERP (ok_if_already_exists), 0, 0);

  unlink (XSTRING (newname)->data);
  if (0 > link (XSTRING (encoded_file)->data, XSTRING (encoded_newname)->data))
    {
#ifdef NO_ARG_ARRAY
      args[0] = file;
      args[1] = newname;
      report_file_error ("Adding new name", Flist (2, args));
#else
      report_file_error ("Adding new name", Flist (2, &file));
#endif
    }

  UNGCPRO;
  return Qnil;
}

#ifdef S_IFLNK
DEFUN ("make-symbolic-link", Fmake_symbolic_link, Smake_symbolic_link, 2, 3,
       "FMake symbolic link to file: \nFMake symbolic link to file %s: \np",
       doc: /* Make a symbolic link to FILENAME, named LINKNAME.  Both args strings.
Signals a `file-already-exists' error if a file LINKNAME already exists
unless optional third argument OK-IF-ALREADY-EXISTS is non-nil.
A number as third arg means request confirmation if LINKNAME already exists.
This happens for interactive use with M-x.  */)
     (filename, linkname, ok_if_already_exists)
     Lisp_Object filename, linkname, ok_if_already_exists;
{
#ifdef NO_ARG_ARRAY
  Lisp_Object args[2];
#endif
  Lisp_Object handler;
  Lisp_Object encoded_filename, encoded_linkname;
  struct gcpro gcpro1, gcpro2, gcpro3, gcpro4;

  GCPRO4 (filename, linkname, encoded_filename, encoded_linkname);
  encoded_filename = encoded_linkname = Qnil;
  CHECK_STRING (filename);
  CHECK_STRING (linkname);
  /* If the link target has a ~, we must expand it to get
     a truly valid file name.  Otherwise, do not expand;
     we want to permit links to relative file names.  */
  if (XSTRING (filename)->data[0] == '~')
    filename = Fexpand_file_name (filename, Qnil);
  linkname = Fexpand_file_name (linkname, Qnil);

  /* If the file name has special constructs in it,
     call the corresponding file handler.  */
  handler = Ffind_file_name_handler (filename, Qmake_symbolic_link);
  if (!NILP (handler))
    RETURN_UNGCPRO (call4 (handler, Qmake_symbolic_link, filename,
			   linkname, ok_if_already_exists));

  /* If the new link name has special constructs in it,
     call the corresponding file handler.  */
  handler = Ffind_file_name_handler (linkname, Qmake_symbolic_link);
  if (!NILP (handler))
    RETURN_UNGCPRO (call4 (handler, Qmake_symbolic_link, filename,
			   linkname, ok_if_already_exists));

  encoded_filename = ENCODE_FILE (filename);
  encoded_linkname = ENCODE_FILE (linkname);

  if (NILP (ok_if_already_exists)
      || INTEGERP (ok_if_already_exists))
    barf_or_query_if_file_exists (encoded_linkname, "make it a link",
				  INTEGERP (ok_if_already_exists), 0, 0);
  if (0 > symlink (XSTRING (encoded_filename)->data,
		   XSTRING (encoded_linkname)->data))
    {
      /* If we didn't complain already, silently delete existing file.  */
      if (errno == EEXIST)
	{
	  unlink (XSTRING (encoded_linkname)->data);
	  if (0 <= symlink (XSTRING (encoded_filename)->data,
			    XSTRING (encoded_linkname)->data))
	    {
	      UNGCPRO;
	      return Qnil;
	    }
	}

#ifdef NO_ARG_ARRAY
      args[0] = filename;
      args[1] = linkname;
      report_file_error ("Making symbolic link", Flist (2, args));
#else
      report_file_error ("Making symbolic link", Flist (2, &filename));
#endif
    }
  UNGCPRO;
  return Qnil;
}
#endif /* S_IFLNK */

#ifdef VMS

DEFUN ("define-logical-name", Fdefine_logical_name, Sdefine_logical_name,
       2, 2, "sDefine logical name: \nsDefine logical name %s as: ",
       doc: /* Define the job-wide logical name NAME to have the value STRING.
If STRING is nil or a null string, the logical name NAME is deleted.  */)
     (name, string)
     Lisp_Object name;
     Lisp_Object string;
{
  CHECK_STRING (name);
  if (NILP (string))
    delete_logical_name (XSTRING (name)->data);
  else
    {
      CHECK_STRING (string);

      if (XSTRING (string)->size == 0)
	delete_logical_name (XSTRING (name)->data);
      else
	define_logical_name (XSTRING (name)->data, XSTRING (string)->data);
    }

  return string;
}
#endif /* VMS */

#ifdef HPUX_NET

DEFUN ("sysnetunam", Fsysnetunam, Ssysnetunam, 2, 2, 0,
       doc: /* Open a network connection to PATH using LOGIN as the login string.  */)
     (path, login)
     Lisp_Object path, login;
{
  int netresult;

  CHECK_STRING (path);
  CHECK_STRING (login);

  netresult = netunam (XSTRING (path)->data, XSTRING (login)->data);

  if (netresult == -1)
    return Qnil;
  else
    return Qt;
}
#endif /* HPUX_NET */

DEFUN ("file-name-absolute-p", Ffile_name_absolute_p, Sfile_name_absolute_p,
       1, 1, 0,
       doc: /* Return t if file FILENAME specifies an absolute file name.
On Unix, this is a name starting with a `/' or a `~'.  */)
     (filename)
     Lisp_Object filename;
{
  unsigned char *ptr;

  CHECK_STRING (filename);
  ptr = XSTRING (filename)->data;
  if (IS_DIRECTORY_SEP (*ptr) || *ptr == '~'
#ifdef VMS
/* ??? This criterion is probably wrong for '<'.  */
      || index (ptr, ':') || index (ptr, '<')
      || (*ptr == '[' && (ptr[1] != '-' || (ptr[2] != '.' && ptr[2] != ']'))
	  && ptr[1] != '.')
#endif /* VMS */
#ifdef DOS_NT
      || (IS_DRIVE (*ptr) && ptr[1] == ':' && IS_DIRECTORY_SEP (ptr[2]))
#endif
      )
    return Qt;
  else
    return Qnil;
}

/* Return nonzero if file FILENAME exists and can be executed.  */

static int
check_executable (filename)
     char *filename;
{
#ifdef DOS_NT
  int len = strlen (filename);
  char *suffix;
  struct stat st;
  if (stat (filename, &st) < 0)
    return 0;
#if defined (WINDOWSNT) || (defined (MSDOS) && __DJGPP__ > 1)
  return ((st.st_mode & S_IEXEC) != 0);
#else
  return (S_ISREG (st.st_mode)
	  && len >= 5
	  && (stricmp ((suffix = filename + len-4), ".com") == 0
	      || stricmp (suffix, ".exe") == 0
	      || stricmp (suffix, ".bat") == 0)
	  || (st.st_mode & S_IFMT) == S_IFDIR);
#endif /* not WINDOWSNT */
#else /* not DOS_NT */
#ifdef HAVE_EUIDACCESS
  return (euidaccess (filename, 1) >= 0);
#else
  /* Access isn't quite right because it uses the real uid
     and we really want to test with the effective uid.
     But Unix doesn't give us a right way to do it.  */
  return (access (filename, 1) >= 0);
#endif
#endif /* not DOS_NT */
}

/* Return nonzero if file FILENAME exists and can be written.  */

static int
check_writable (filename)
     char *filename;
{
#ifdef MSDOS
  struct stat st;
  if (stat (filename, &st) < 0)
    return 0;
  return (st.st_mode & S_IWRITE || (st.st_mode & S_IFMT) == S_IFDIR);
#else /* not MSDOS */
#ifdef HAVE_EUIDACCESS
  return (euidaccess (filename, 2) >= 0);
#else
  /* Access isn't quite right because it uses the real uid
     and we really want to test with the effective uid.
     But Unix doesn't give us a right way to do it.
     Opening with O_WRONLY could work for an ordinary file,
     but would lose for directories.  */
  return (access (filename, 2) >= 0);
#endif
#endif /* not MSDOS */
}

DEFUN ("file-exists-p", Ffile_exists_p, Sfile_exists_p, 1, 1, 0,
       doc: /* Return t if file FILENAME exists.  (This does not mean you can read it.)
See also `file-readable-p' and `file-attributes'.  */)
     (filename)
     Lisp_Object filename;
{
  Lisp_Object absname;
  Lisp_Object handler;
  struct stat statbuf;

  CHECK_STRING (filename);
  absname = Fexpand_file_name (filename, Qnil);

  /* If the file name has special constructs in it,
     call the corresponding file handler.  */
  handler = Ffind_file_name_handler (absname, Qfile_exists_p);
  if (!NILP (handler))
    return call2 (handler, Qfile_exists_p, absname);

  absname = ENCODE_FILE (absname);

  return (stat (XSTRING (absname)->data, &statbuf) >= 0) ? Qt : Qnil;
}

DEFUN ("file-executable-p", Ffile_executable_p, Sfile_executable_p, 1, 1, 0,
       doc: /* Return t if FILENAME can be executed by you.
For a directory, this means you can access files in that directory.  */)
     (filename)
     Lisp_Object filename;
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

  return (check_executable (XSTRING (absname)->data) ? Qt : Qnil);
}

DEFUN ("file-readable-p", Ffile_readable_p, Sfile_readable_p, 1, 1, 0,
       doc: /* Return t if file FILENAME exists and you can read it.
See also `file-exists-p' and `file-attributes'.  */)
     (filename)
     Lisp_Object filename;
{
  Lisp_Object absname;
  Lisp_Object handler;
  int desc;
  int flags;
  struct stat statbuf;

  CHECK_STRING (filename);
  absname = Fexpand_file_name (filename, Qnil);

  /* If the file name has special constructs in it,
     call the corresponding file handler.  */
  handler = Ffind_file_name_handler (absname, Qfile_readable_p);
  if (!NILP (handler))
    return call2 (handler, Qfile_readable_p, absname);

  absname = ENCODE_FILE (absname);

#if defined(DOS_NT) || defined(macintosh)
  /* Under MS-DOS, Windows, and Macintosh, open does not work for
     directories.  */
  if (access (XSTRING (absname)->data, 0) == 0)
    return Qt;
  return Qnil;
#else /* not DOS_NT and not macintosh */
  flags = O_RDONLY;
#if defined (S_ISFIFO) && defined (O_NONBLOCK)
  /* Opening a fifo without O_NONBLOCK can wait.
     We don't want to wait.  But we don't want to mess wth O_NONBLOCK
     except in the case of a fifo, on a system which handles it.  */
  desc = stat (XSTRING (absname)->data, &statbuf);
  if (desc < 0)
    return Qnil;
  if (S_ISFIFO (statbuf.st_mode))
    flags |= O_NONBLOCK;
#endif
  desc = emacs_open (XSTRING (absname)->data, flags, 0);
  if (desc < 0)
    return Qnil;
  emacs_close (desc);
  return Qt;
#endif /* not DOS_NT and not macintosh */
}

/* Having this before file-symlink-p mysteriously caused it to be forgotten
   on the RT/PC.  */
DEFUN ("file-writable-p", Ffile_writable_p, Sfile_writable_p, 1, 1, 0,
       doc: /* Return t if file FILENAME can be written or created by you.  */)
     (filename)
     Lisp_Object filename;
{
  Lisp_Object absname, dir, encoded;
  Lisp_Object handler;
  struct stat statbuf;

  CHECK_STRING (filename);
  absname = Fexpand_file_name (filename, Qnil);

  /* If the file name has special constructs in it,
     call the corresponding file handler.  */
  handler = Ffind_file_name_handler (absname, Qfile_writable_p);
  if (!NILP (handler))
    return call2 (handler, Qfile_writable_p, absname);

  encoded = ENCODE_FILE (absname);
  if (stat (XSTRING (encoded)->data, &statbuf) >= 0)
    return (check_writable (XSTRING (encoded)->data)
	    ? Qt : Qnil);

  dir = Ffile_name_directory (absname);
#ifdef VMS
  if (!NILP (dir))
    dir = Fdirectory_file_name (dir);
#endif /* VMS */
#ifdef MSDOS
  if (!NILP (dir))
    dir = Fdirectory_file_name (dir);
#endif /* MSDOS */

  dir = ENCODE_FILE (dir);
#ifdef WINDOWSNT
  /* The read-only attribute of the parent directory doesn't affect
     whether a file or directory can be created within it.  Some day we
     should check ACLs though, which do affect this.  */
  if (stat (XSTRING (dir)->data, &statbuf) < 0)
    return Qnil;
  return (statbuf.st_mode & S_IFMT) == S_IFDIR ? Qt : Qnil;
#else
  return (check_writable (!NILP (dir) ? (char *) XSTRING (dir)->data : "")
	  ? Qt : Qnil);
#endif
}

DEFUN ("access-file", Faccess_file, Saccess_file, 2, 2, 0,
       doc: /* Access file FILENAME, and get an error if that does not work.
The second argument STRING is used in the error message.
If there is no error, we return nil.  */)
     (filename, string)
     Lisp_Object filename, string;
{
  Lisp_Object handler, encoded_filename, absname;
  int fd;

  CHECK_STRING (filename);
  absname = Fexpand_file_name (filename, Qnil);

  CHECK_STRING (string);

  /* If the file name has special constructs in it,
     call the corresponding file handler.  */
  handler = Ffind_file_name_handler (absname, Qaccess_file);
  if (!NILP (handler))
    return call3 (handler, Qaccess_file, absname, string);

  encoded_filename = ENCODE_FILE (absname);

  fd = emacs_open (XSTRING (encoded_filename)->data, O_RDONLY, 0);
  if (fd < 0)
    report_file_error (XSTRING (string)->data, Fcons (filename, Qnil));
  emacs_close (fd);

  return Qnil;
}

DEFUN ("file-symlink-p", Ffile_symlink_p, Sfile_symlink_p, 1, 1, 0,
       doc: /* Return non-nil if file FILENAME is the name of a symbolic link.
The value is the name of the file to which it is linked.
Otherwise returns nil.  */)
     (filename)
     Lisp_Object filename;
{
#ifdef S_IFLNK
  char *buf;
  int bufsize;
  int valsize;
  Lisp_Object val;
  Lisp_Object handler;

  CHECK_STRING (filename);
  filename = Fexpand_file_name (filename, Qnil);

  /* If the file name has special constructs in it,
     call the corresponding file handler.  */
  handler = Ffind_file_name_handler (filename, Qfile_symlink_p);
  if (!NILP (handler))
    return call2 (handler, Qfile_symlink_p, filename);

  filename = ENCODE_FILE (filename);

  bufsize = 50;
  buf = NULL;
  do
    {
      bufsize *= 2;
      buf = (char *) xrealloc (buf, bufsize);
      bzero (buf, bufsize);
      
      errno = 0;
      valsize = readlink (XSTRING (filename)->data, buf, bufsize);
      if (valsize == -1)
	{
#ifdef ERANGE
	  /* HP-UX reports ERANGE if buffer is too small.  */
	  if (errno == ERANGE)
	    valsize = bufsize;
	  else
#endif
	    {
	      xfree (buf);
	      return Qnil;
	    }
	}
    }
  while (valsize >= bufsize);
  
  val = make_string (buf, valsize);
  if (buf[0] == '/' && index (buf, ':'))
    val = concat2 (build_string ("/:"), val);
  xfree (buf);
  val = DECODE_FILE (val);
  return val;
#else /* not S_IFLNK */
  return Qnil;
#endif /* not S_IFLNK */
}

DEFUN ("file-directory-p", Ffile_directory_p, Sfile_directory_p, 1, 1, 0,
       doc: /* Return t if FILENAME names an existing directory.
Symbolic links to directories count as directories.
See `file-symlink-p' to distinguish symlinks.  */)
     (filename)
     Lisp_Object filename;
{
  register Lisp_Object absname;
  struct stat st;
  Lisp_Object handler;

  absname = expand_and_dir_to_file (filename, current_buffer->directory);

  /* If the file name has special constructs in it,
     call the corresponding file handler.  */
  handler = Ffind_file_name_handler (absname, Qfile_directory_p);
  if (!NILP (handler))
    return call2 (handler, Qfile_directory_p, absname);

  absname = ENCODE_FILE (absname);

  if (stat (XSTRING (absname)->data, &st) < 0)
    return Qnil;
  return (st.st_mode & S_IFMT) == S_IFDIR ? Qt : Qnil;
}

DEFUN ("file-accessible-directory-p", Ffile_accessible_directory_p, Sfile_accessible_directory_p, 1, 1, 0,
       doc: /* Return t if file FILENAME names a directory you can open.
For the value to be t, FILENAME must specify the name of a directory as a file,
and the directory must allow you to open files in it.  In order to use a
directory as a buffer's current directory, this predicate must return true.
A directory name spec may be given instead; then the value is t
if the directory so specified exists and really is a readable and
searchable directory.  */)
     (filename)
     Lisp_Object filename;
{
  Lisp_Object handler;
  int tem;
  struct gcpro gcpro1;

  /* If the file name has special constructs in it,
     call the corresponding file handler.  */
  handler = Ffind_file_name_handler (filename, Qfile_accessible_directory_p);
  if (!NILP (handler))
    return call2 (handler, Qfile_accessible_directory_p, filename);

  /* It's an unlikely combination, but yes we really do need to gcpro:
     Suppose that file-accessible-directory-p has no handler, but
     file-directory-p does have a handler; this handler causes a GC which
     relocates the string in `filename'; and finally file-directory-p
     returns non-nil.  Then we would end up passing a garbaged string
     to file-executable-p.  */
  GCPRO1 (filename);
  tem = (NILP (Ffile_directory_p (filename))
	 || NILP (Ffile_executable_p (filename)));
  UNGCPRO;
  return tem ? Qnil : Qt;
}

DEFUN ("file-regular-p", Ffile_regular_p, Sfile_regular_p, 1, 1, 0,
       doc: /* Return t if file FILENAME is the name of a regular file.
This is the sort of file that holds an ordinary stream of data bytes.  */)
     (filename)
     Lisp_Object filename;
{
  register Lisp_Object absname;
  struct stat st;
  Lisp_Object handler;

  absname = expand_and_dir_to_file (filename, current_buffer->directory);

  /* If the file name has special constructs in it,
     call the corresponding file handler.  */
  handler = Ffind_file_name_handler (absname, Qfile_regular_p);
  if (!NILP (handler))
    return call2 (handler, Qfile_regular_p, absname);

  absname = ENCODE_FILE (absname);

#ifdef WINDOWSNT
  {
    int result;
    Lisp_Object tem = Vw32_get_true_file_attributes;

    /* Tell stat to use expensive method to get accurate info.  */
    Vw32_get_true_file_attributes = Qt;
    result = stat (XSTRING (absname)->data, &st);
    Vw32_get_true_file_attributes = tem;

    if (result < 0)
      return Qnil;
    return (st.st_mode & S_IFMT) == S_IFREG ? Qt : Qnil;
  }
#else
  if (stat (XSTRING (absname)->data, &st) < 0)
    return Qnil;
  return (st.st_mode & S_IFMT) == S_IFREG ? Qt : Qnil;
#endif
}

DEFUN ("file-modes", Ffile_modes, Sfile_modes, 1, 1, 0,
       doc: /* Return mode bits of file named FILENAME, as an integer.  */)
     (filename)
     Lisp_Object filename;
{
  Lisp_Object absname;
  struct stat st;
  Lisp_Object handler;

  absname = expand_and_dir_to_file (filename, current_buffer->directory);

  /* If the file name has special constructs in it,
     call the corresponding file handler.  */
  handler = Ffind_file_name_handler (absname, Qfile_modes);
  if (!NILP (handler))
    return call2 (handler, Qfile_modes, absname);

  absname = ENCODE_FILE (absname);

  if (stat (XSTRING (absname)->data, &st) < 0)
    return Qnil;
#if defined (MSDOS) && __DJGPP__ < 2
  if (check_executable (XSTRING (absname)->data))
    st.st_mode |= S_IEXEC;
#endif /* MSDOS && __DJGPP__ < 2 */

  return make_number (st.st_mode & 07777);
}

DEFUN ("set-file-modes", Fset_file_modes, Sset_file_modes, 2, 2, 0,
       doc: /* Set mode bits of file named FILENAME to MODE (an integer).
Only the 12 low bits of MODE are used.  */)
  (filename, mode)
     Lisp_Object filename, mode;
{
  Lisp_Object absname, encoded_absname;
  Lisp_Object handler;

  absname = Fexpand_file_name (filename, current_buffer->directory);
  CHECK_NUMBER (mode);

  /* If the file name has special constructs in it,
     call the corresponding file handler.  */
  handler = Ffind_file_name_handler (absname, Qset_file_modes);
  if (!NILP (handler))
    return call3 (handler, Qset_file_modes, absname, mode);

  encoded_absname = ENCODE_FILE (absname);

  if (chmod (XSTRING (encoded_absname)->data, XINT (mode)) < 0)
    report_file_error ("Doing chmod", Fcons (absname, Qnil));

  return Qnil;
}

DEFUN ("set-default-file-modes", Fset_default_file_modes, Sset_default_file_modes, 1, 1, 0,
       doc: /* Set the file permission bits for newly created files.
The argument MODE should be an integer; only the low 9 bits are used.
This setting is inherited by subprocesses.  */)
     (mode)
     Lisp_Object mode;
{
  CHECK_NUMBER (mode);

  umask ((~ XINT (mode)) & 0777);

  return Qnil;
}

DEFUN ("default-file-modes", Fdefault_file_modes, Sdefault_file_modes, 0, 0, 0,
       doc: /* Return the default file protection for created files.
The value is an integer.  */)
     ()
{
  int realmask;
  Lisp_Object value;

  realmask = umask (0);
  umask (realmask);

  XSETINT (value, (~ realmask) & 0777);
  return value;
}


#ifdef __NetBSD__
#define unix 42
#endif

#ifdef unix
DEFUN ("unix-sync", Funix_sync, Sunix_sync, 0, 0, "",
       doc: /* Tell Unix to finish all pending disk updates.  */)
     ()
{
  sync ();
  return Qnil;
}

#endif /* unix */

DEFUN ("file-newer-than-file-p", Ffile_newer_than_file_p, Sfile_newer_than_file_p, 2, 2, 0,
       doc: /* Return t if file FILE1 is newer than file FILE2.
If FILE1 does not exist, the answer is nil;
otherwise, if FILE2 does not exist, the answer is t.  */)
     (file1, file2)
     Lisp_Object file1, file2;
{
  Lisp_Object absname1, absname2;
  struct stat st;
  int mtime1;
  Lisp_Object handler;
  struct gcpro gcpro1, gcpro2;

  CHECK_STRING (file1);
  CHECK_STRING (file2);

  absname1 = Qnil;
  GCPRO2 (absname1, file2);
  absname1 = expand_and_dir_to_file (file1, current_buffer->directory);
  absname2 = expand_and_dir_to_file (file2, current_buffer->directory);
  UNGCPRO;

  /* If the file name has special constructs in it,
     call the corresponding file handler.  */
  handler = Ffind_file_name_handler (absname1, Qfile_newer_than_file_p);
  if (NILP (handler))
    handler = Ffind_file_name_handler (absname2, Qfile_newer_than_file_p);
  if (!NILP (handler))
    return call3 (handler, Qfile_newer_than_file_p, absname1, absname2);

  GCPRO2 (absname1, absname2);
  absname1 = ENCODE_FILE (absname1);
  absname2 = ENCODE_FILE (absname2);
  UNGCPRO;

  if (stat (XSTRING (absname1)->data, &st) < 0)
    return Qnil;

  mtime1 = st.st_mtime;

  if (stat (XSTRING (absname2)->data, &st) < 0)
    return Qt;

  return (mtime1 > st.st_mtime) ? Qt : Qnil;
}

#ifdef DOS_NT
Lisp_Object Qfind_buffer_file_type;
#endif /* DOS_NT */

#ifndef READ_BUF_SIZE
#define READ_BUF_SIZE (64 << 10)
#endif

extern void adjust_markers_for_delete P_ ((int, int, int, int));

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

static Lisp_Object
decide_coding_unwind (unwind_data)
     Lisp_Object unwind_data;
{
  Lisp_Object multibyte, undo_list, buffer;

  multibyte = XCAR (unwind_data);
  unwind_data = XCDR (unwind_data);
  undo_list = XCAR (unwind_data);
  buffer = XCDR (unwind_data);

  if (current_buffer != XBUFFER (buffer))
    set_buffer_internal (XBUFFER (buffer));
  adjust_markers_for_delete (BEG, BEG_BYTE, Z, Z_BYTE);
  adjust_overlays_for_delete (BEG, Z - BEG);
  BUF_INTERVALS (current_buffer) = 0;
  TEMP_SET_PT_BOTH (BEG, BEG_BYTE);

  /* Now we are safe to change the buffer's multibyteness directly.  */
  current_buffer->enable_multibyte_characters = multibyte;
  current_buffer->undo_list = undo_list;

  return Qnil;
}


/* Used to pass values from insert-file-contents to read_non_regular.  */

static int non_regular_fd;
static int non_regular_inserted;
static int non_regular_nbytes;


/* Read from a non-regular file.
   Read non_regular_trytry bytes max from non_regular_fd.
   Non_regular_inserted specifies where to put the read bytes.
   Value is the number of bytes read.  */

static Lisp_Object
read_non_regular ()
{
  int nbytes;
  
  immediate_quit = 1;
  QUIT;
  nbytes = emacs_read (non_regular_fd,
		       BEG_ADDR + PT_BYTE - BEG_BYTE + non_regular_inserted,
		       non_regular_nbytes);
  immediate_quit = 0;
  return make_number (nbytes);
}


/* Condition-case handler used when reading from non-regular files
   in insert-file-contents.  */

static Lisp_Object
read_non_regular_quit ()
{
  return Qnil;
}


DEFUN ("insert-file-contents", Finsert_file_contents, Sinsert_file_contents,
       1, 5, 0,
       doc: /* Insert contents of file FILENAME after point.
Returns list of absolute file name and number of bytes inserted.
If second argument VISIT is non-nil, the buffer's visited filename
and last save file modtime are set, and it is marked unmodified.
If visiting and the file does not exist, visiting is completed
before the error is signaled.
The optional third and fourth arguments BEG and END
specify what portion of the file to insert.
These arguments count bytes in the file, not characters in the buffer.
If VISIT is non-nil, BEG and END must be nil.

If optional fifth argument REPLACE is non-nil,
it means replace the current buffer contents (in the accessible portion)
with the file contents.  This is better than simply deleting and inserting
the whole thing because (1) it preserves some marker positions
and (2) it puts less data in the undo list.
When REPLACE is non-nil, the value is the number of characters actually read,
which is often less than the number of characters to be read.

This does code conversion according to the value of
`coding-system-for-read' or `file-coding-system-alist',
and sets the variable `last-coding-system-used' to the coding system
actually used.  */)
     (filename, visit, beg, end, replace)
     Lisp_Object filename, visit, beg, end, replace;
{
  struct stat st;
  register int fd;
  int inserted = 0;
  register int how_much;
  register int unprocessed;
  int count = BINDING_STACK_SIZE ();
  struct gcpro gcpro1, gcpro2, gcpro3, gcpro4;
  Lisp_Object handler, val, insval, orig_filename;
  Lisp_Object p;
  int total = 0;
  int not_regular = 0;
  unsigned char read_buf[READ_BUF_SIZE];
  struct coding_system coding;
  unsigned char buffer[1 << 14];
  int replace_handled = 0;
  int set_coding_system = 0;
  int coding_system_decided = 0;
  int read_quit = 0;

  if (current_buffer->base_buffer && ! NILP (visit))
    error ("Cannot do file visiting in an indirect buffer");

  if (!NILP (current_buffer->read_only))
    Fbarf_if_buffer_read_only ();

  val = Qnil;
  p = Qnil;
  orig_filename = Qnil;

  GCPRO4 (filename, val, p, orig_filename);

  CHECK_STRING (filename);
  filename = Fexpand_file_name (filename, Qnil);

  /* If the file name has special constructs in it,
     call the corresponding file handler.  */
  handler = Ffind_file_name_handler (filename, Qinsert_file_contents);
  if (!NILP (handler))
    {
      val = call6 (handler, Qinsert_file_contents, filename,
		   visit, beg, end, replace);
      if (CONSP (val) && CONSP (XCDR (val)))
	inserted = XINT (XCAR (XCDR (val)));
      goto handled;
    }

  orig_filename = filename;
  filename = ENCODE_FILE (filename);

  fd = -1;

#ifdef WINDOWSNT
  {
    Lisp_Object tem = Vw32_get_true_file_attributes;

    /* Tell stat to use expensive method to get accurate info.  */
    Vw32_get_true_file_attributes = Qt;
    total = stat (XSTRING (filename)->data, &st);
    Vw32_get_true_file_attributes = tem;
  }
  if (total < 0)
#else
#ifndef APOLLO
  if (stat (XSTRING (filename)->data, &st) < 0)
#else
  if ((fd = emacs_open (XSTRING (filename)->data, O_RDONLY, 0)) < 0
      || fstat (fd, &st) < 0)
#endif /* not APOLLO */
#endif /* WINDOWSNT */
    {
      if (fd >= 0) emacs_close (fd);
    badopen:
      if (NILP (visit))
	report_file_error ("Opening input file", Fcons (orig_filename, Qnil));
      st.st_mtime = -1;
      how_much = 0;
      if (!NILP (Vcoding_system_for_read))
	Fset (Qbuffer_file_coding_system, Vcoding_system_for_read);
      goto notfound;
    }

#ifdef S_IFREG
  /* This code will need to be changed in order to work on named
     pipes, and it's probably just not worth it.  So we should at
     least signal an error.  */
  if (!S_ISREG (st.st_mode))
    {
      not_regular = 1;

      if (! NILP (visit))
	goto notfound;

      if (! NILP (replace) || ! NILP (beg) || ! NILP (end))
	Fsignal (Qfile_error,
		 Fcons (build_string ("not a regular file"),
			Fcons (orig_filename, Qnil)));
    }
#endif

  if (fd < 0)
    if ((fd = emacs_open (XSTRING (filename)->data, O_RDONLY, 0)) < 0)
      goto badopen;

  /* Replacement should preserve point as it preserves markers.  */
  if (!NILP (replace))
    record_unwind_protect (restore_point_unwind, Fpoint_marker ());

  record_unwind_protect (close_file_unwind, make_number (fd));

  /* Supposedly happens on VMS.  */
  if (! not_regular && st.st_size < 0)
    error ("File size is negative");

  /* Prevent redisplay optimizations.  */
  current_buffer->clip_changed = 1;

  if (!NILP (visit))
    {
      if (!NILP (beg) || !NILP (end))
	error ("Attempt to visit less than an entire file");
      if (BEG < Z && NILP (replace))
	error ("Cannot do file visiting in a non-empty buffer");
    }

  if (!NILP (beg))
    CHECK_NUMBER (beg);
  else
    XSETFASTINT (beg, 0);

  if (!NILP (end))
    CHECK_NUMBER (end);
  else
    {
      if (! not_regular)
	{
	  XSETINT (end, st.st_size);

	  /* Arithmetic overflow can occur if an Emacs integer cannot
	     represent the file size, or if the calculations below
	     overflow.  The calculations below double the file size
	     twice, so check that it can be multiplied by 4 safely.  */
	  if (XINT (end) != st.st_size
	      || ((int) st.st_size * 4) / 4 != st.st_size)
	    error ("Maximum buffer size exceeded");

	  /* The file size returned from stat may be zero, but data
	     may be readable nonetheless, for example when this is a
	     file in the /proc filesystem.  */
	  if (st.st_size == 0)
	    XSETINT (end, READ_BUF_SIZE);
	}
    }

  if (BEG < Z)
    {
      /* Decide the coding system to use for reading the file now
         because we can't use an optimized method for handling
         `coding:' tag if the current buffer is not empty.  */
      Lisp_Object val;
      val = Qnil;

      if (!NILP (Vcoding_system_for_read))
	val = Vcoding_system_for_read;
      else if (! NILP (replace))
	/* In REPLACE mode, we can use the same coding system
	   that was used to visit the file.  */
	val = current_buffer->buffer_file_coding_system;
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
		nread = emacs_read (fd, read_buf, 1024 * 4);
	      else
		{
		  nread = emacs_read (fd, read_buf, 1024);
		  if (nread >= 0)
		    {
		      if (lseek (fd, st.st_size - (1024 * 3), 0) < 0)
			report_file_error ("Setting file position",
					   Fcons (orig_filename, Qnil));
		      nread += emacs_read (fd, read_buf + nread, 1024 * 3);
		    }
		}

	      if (nread < 0)
		error ("IO error reading %s: %s",
		       XSTRING (orig_filename)->data, emacs_strerror (errno));
	      else if (nread > 0)
		{
		  struct buffer *prev = current_buffer;
		  Lisp_Object buffer;
		  struct buffer *buf;

		  record_unwind_protect (Fset_buffer, Fcurrent_buffer ());

		  buffer = Fget_buffer_create (build_string (" *code-converting-work*"));
		  buf = XBUFFER (buffer);

		  buf->directory = current_buffer->directory;
		  buf->read_only = Qnil;
		  buf->filename = Qnil;
		  buf->undo_list = Qt;
		  buf->overlays_before = Qnil;
		  buf->overlays_after = Qnil;
		  
		  set_buffer_internal (buf);
		  Ferase_buffer ();
		  buf->enable_multibyte_characters = Qnil;

		  insert_1_both (read_buf, nread, nread, 0, 0, 0);
		  TEMP_SET_PT_BOTH (BEG, BEG_BYTE);
		  val = call2 (Vset_auto_coding_function,
			       filename, make_number (nread));
		  set_buffer_internal (prev);
		  
		  /* Discard the unwind protect for recovering the
                     current buffer.  */
		  specpdl_ptr--;

		  /* Rewind the file for the actual read done later.  */
		  if (lseek (fd, 0, 0) < 0)
		    report_file_error ("Setting file position",
				       Fcons (orig_filename, Qnil));
		}
	    }

	  if (NILP (val))
	    {
	      /* If we have not yet decided a coding system, check
                 file-coding-system-alist.  */
	      Lisp_Object args[6], coding_systems;

	      args[0] = Qinsert_file_contents, args[1] = orig_filename;
	      args[2] = visit, args[3] = beg, args[4] = end, args[5] = replace;
	      coding_systems = Ffind_operation_coding_system (6, args);
	      if (CONSP (coding_systems))
		val = XCAR (coding_systems);
	    }
	}

      setup_coding_system (Fcheck_coding_system (val), &coding);
      /* Ensure we set Vlast_coding_system_used.  */
      set_coding_system = 1;

      if (NILP (current_buffer->enable_multibyte_characters)
	  && ! NILP (val))
	/* We must suppress all character code conversion except for
	   end-of-line conversion.  */
	setup_raw_text_coding_system (&coding);

      coding.src_multibyte = 0;
      coding.dst_multibyte
	= !NILP (current_buffer->enable_multibyte_characters);
      coding_system_decided = 1;
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
      && !(coding.common_flags & CODING_REQUIRE_DECODING_MASK))
    {
      /* same_at_start and same_at_end count bytes,
	 because file access counts bytes
	 and BEG and END count bytes.  */
      int same_at_start = BEGV_BYTE;
      int same_at_end = ZV_BYTE;
      int overlap;
      /* There is still a possibility we will find the need to do code
	 conversion.  If that happens, we set this variable to 1 to
	 give up on handling REPLACE in the optimized way.  */
      int giveup_match_end = 0;

      if (XINT (beg) != 0)
	{
	  if (lseek (fd, XINT (beg), 0) < 0)
	    report_file_error ("Setting file position",
			       Fcons (orig_filename, Qnil));
	}

      immediate_quit = 1;
      QUIT;
      /* Count how many chars at the start of the file
	 match the text at the beginning of the buffer.  */
      while (1)
	{
	  int nread, bufpos;

	  nread = emacs_read (fd, buffer, sizeof buffer);
	  if (nread < 0)
	    error ("IO error reading %s: %s",
		   XSTRING (orig_filename)->data, emacs_strerror (errno));
	  else if (nread == 0)
	    break;

	  if (coding.type == coding_type_undecided)
	    detect_coding (&coding, buffer, nread);
	  if (coding.common_flags & CODING_REQUIRE_DECODING_MASK)
	    /* We found that the file should be decoded somehow.
               Let's give up here.  */
	    {
	      giveup_match_end = 1;
	      break;
	    }

	  if (coding.eol_type == CODING_EOL_UNDECIDED)
	    detect_eol (&coding, buffer, nread);
	  if (coding.eol_type != CODING_EOL_UNDECIDED
	      && coding.eol_type != CODING_EOL_LF)
	    /* We found that the format of eol should be decoded.
               Let's give up here.  */
	    {
	      giveup_match_end = 1;
	      break;
	    }

	  bufpos = 0;
	  while (bufpos < nread && same_at_start < ZV_BYTE
		 && FETCH_BYTE (same_at_start) == buffer[bufpos])
	    same_at_start++, bufpos++;
	  /* If we found a discrepancy, stop the scan.
	     Otherwise loop around and scan the next bufferful.  */
	  if (bufpos != nread)
	    break;
	}
      immediate_quit = 0;
      /* If the file matches the buffer completely,
	 there's no need to replace anything.  */
      if (same_at_start - BEGV_BYTE == XINT (end))
	{
	  emacs_close (fd);
	  specpdl_ptr--;
	  /* Truncate the buffer to the size of the file.  */
	  del_range_1 (same_at_start, same_at_end, 0, 0);
	  goto handled;
	}
      immediate_quit = 1;
      QUIT;
      /* Count how many chars at the end of the file
	 match the text at the end of the buffer.  But, if we have
	 already found that decoding is necessary, don't waste time.  */
      while (!giveup_match_end)
	{
	  int total_read, nread, bufpos, curpos, trial;

	  /* At what file position are we now scanning?  */
	  curpos = XINT (end) - (ZV_BYTE - same_at_end);
	  /* If the entire file matches the buffer tail, stop the scan.  */
	  if (curpos == 0)
	    break;
	  /* How much can we scan in the next step?  */
	  trial = min (curpos, sizeof buffer);
	  if (lseek (fd, curpos - trial, 0) < 0)
	    report_file_error ("Setting file position",
			       Fcons (orig_filename, Qnil));

	  total_read = nread = 0;
	  while (total_read < trial)
	    {
	      nread = emacs_read (fd, buffer + total_read, trial - total_read);
	      if (nread < 0)
		error ("IO error reading %s: %s",
		       XSTRING (orig_filename)->data, emacs_strerror (errno));
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
		 && FETCH_BYTE (same_at_end - 1) == buffer[bufpos - 1])
	    same_at_end--, bufpos--;

	  /* If we found a discrepancy, stop the scan.
	     Otherwise loop around and scan the preceding bufferful.  */
	  if (bufpos != 0)
	    {
	      /* If this discrepancy is because of code conversion,
		 we cannot use this method; giveup and try the other.  */
	      if (same_at_end > same_at_start
		  && FETCH_BYTE (same_at_end - 1) >= 0200
		  && ! NILP (current_buffer->enable_multibyte_characters)
		  && (CODING_MAY_REQUIRE_DECODING (&coding)))
		giveup_match_end = 1;
	      break;
	    }

	  if (nread == 0)
	    break;
	}
      immediate_quit = 0;

      if (! giveup_match_end)
	{
	  int temp;

	  /* We win!  We can handle REPLACE the optimized way.  */

	  /* Extend the start of non-matching text area to multibyte
             character boundary.  */
	  if (! NILP (current_buffer->enable_multibyte_characters))
	    while (same_at_start > BEGV_BYTE
		   && ! CHAR_HEAD_P (FETCH_BYTE (same_at_start)))
	      same_at_start--;

	  /* Extend the end of non-matching text area to multibyte
             character boundary.  */
	  if (! NILP (current_buffer->enable_multibyte_characters))
	    while (same_at_end < ZV_BYTE
		   && ! CHAR_HEAD_P (FETCH_BYTE (same_at_end)))
	      same_at_end++;

	  /* Don't try to reuse the same piece of text twice.  */
	  overlap = (same_at_start - BEGV_BYTE
		     - (same_at_end + st.st_size - ZV));
	  if (overlap > 0)
	    same_at_end += overlap;

	  /* Arrange to read only the nonmatching middle part of the file.  */
	  XSETFASTINT (beg, XINT (beg) + (same_at_start - BEGV_BYTE));
	  XSETFASTINT (end, XINT (end) - (ZV_BYTE - same_at_end));

	  del_range_byte (same_at_start, same_at_end, 0);
	  /* Insert from the file at the proper position.  */
	  temp = BYTE_TO_CHAR (same_at_start);
	  SET_PT_BOTH (temp, same_at_start);

	  /* If display currently starts at beginning of line,
	     keep it that way.  */
	  if (XBUFFER (XWINDOW (selected_window)->buffer) == current_buffer)
	    XWINDOW (selected_window)->start_at_line_beg = Fbolp ();

	  replace_handled = 1;
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
      int same_at_start = BEGV_BYTE;
      int same_at_end = ZV_BYTE;
      int overlap;
      int bufpos;
      /* Make sure that the gap is large enough.  */
      int bufsize = 2 * st.st_size;
      unsigned char *conversion_buffer = (unsigned char *) xmalloc (bufsize);
      int temp;

      /* First read the whole file, performing code conversion into
	 CONVERSION_BUFFER.  */

      if (lseek (fd, XINT (beg), 0) < 0)
	{
	  xfree (conversion_buffer);
	  report_file_error ("Setting file position",
			     Fcons (orig_filename, Qnil));
	}

      total = st.st_size;	/* Total bytes in the file.  */
      how_much = 0;		/* Bytes read from file so far.  */
      inserted = 0;		/* Bytes put into CONVERSION_BUFFER so far.  */
      unprocessed = 0;		/* Bytes not processed in previous loop.  */

      while (how_much < total)
	{
	  /* try is reserved in some compilers (Microsoft C) */
	  int trytry = min (total - how_much, READ_BUF_SIZE - unprocessed);
	  unsigned char *destination = read_buf + unprocessed;
	  int this;

	  /* Allow quitting out of the actual I/O.  */
	  immediate_quit = 1;
	  QUIT;
	  this = emacs_read (fd, destination, trytry);
	  immediate_quit = 0;

	  if (this < 0 || this + unprocessed == 0)
	    {
	      how_much = this;
	      break;
	    }

	  how_much += this;

	  if (CODING_MAY_REQUIRE_DECODING (&coding))
	    {
	      int require, result;

	      this += unprocessed;

	      /* If we are using more space than estimated,
		 make CONVERSION_BUFFER bigger.  */
	      require = decoding_buffer_size (&coding, this);
	      if (inserted + require + 2 * (total - how_much) > bufsize)
		{
		  bufsize = inserted + require + 2 * (total - how_much);
		  conversion_buffer = (unsigned char *) xrealloc (conversion_buffer, bufsize);
		}

	      /* Convert this batch with results in CONVERSION_BUFFER.  */
	      if (how_much >= total)  /* This is the last block.  */
		coding.mode |= CODING_MODE_LAST_BLOCK;
	      if (coding.composing != COMPOSITION_DISABLED)
		coding_allocate_composition_data (&coding, BEGV);
	      result = decode_coding (&coding, read_buf,
				      conversion_buffer + inserted,
				      this, bufsize - inserted);

	      /* Save for next iteration whatever we didn't convert.  */
	      unprocessed = this - coding.consumed;
	      bcopy (read_buf + coding.consumed, read_buf, unprocessed);
	      if (!NILP (current_buffer->enable_multibyte_characters))
		this = coding.produced;
	      else
		this = str_as_unibyte (conversion_buffer + inserted,
				       coding.produced);
	    }

	  inserted += this;
	}

      /* At this point, INSERTED is how many characters (i.e. bytes)
	 are present in CONVERSION_BUFFER.
	 HOW_MUCH should equal TOTAL,
	 or should be <= 0 if we couldn't read the file.  */

      if (how_much < 0)
	{
	  xfree (conversion_buffer);

	  if (how_much == -1)
	    error ("IO error reading %s: %s",
		   XSTRING (orig_filename)->data, emacs_strerror (errno));
	  else if (how_much == -2)
	    error ("maximum buffer size exceeded");
	}

      /* Compare the beginning of the converted file
	 with the buffer text.  */

      bufpos = 0;
      while (bufpos < inserted && same_at_start < same_at_end
	     && FETCH_BYTE (same_at_start) == conversion_buffer[bufpos])
	same_at_start++, bufpos++;

      /* If the file matches the buffer completely,
	 there's no need to replace anything.  */

      if (bufpos == inserted)
	{
	  xfree (conversion_buffer);
	  emacs_close (fd);
	  specpdl_ptr--;
	  /* Truncate the buffer to the size of the file.  */
	  del_range_byte (same_at_start, same_at_end, 0);
	  inserted = 0;
	  goto handled;
	}

      /* Extend the start of non-matching text area to multibyte
	 character boundary.  */
      if (! NILP (current_buffer->enable_multibyte_characters))
	while (same_at_start > BEGV_BYTE
	       && ! CHAR_HEAD_P (FETCH_BYTE (same_at_start)))
	  same_at_start--;

      /* Scan this bufferful from the end, comparing with
	 the Emacs buffer.  */
      bufpos = inserted;

      /* Compare with same_at_start to avoid counting some buffer text
	 as matching both at the file's beginning and at the end.  */
      while (bufpos > 0 && same_at_end > same_at_start
	     && FETCH_BYTE (same_at_end - 1) == conversion_buffer[bufpos - 1])
	same_at_end--, bufpos--;

      /* Extend the end of non-matching text area to multibyte
	 character boundary.  */
      if (! NILP (current_buffer->enable_multibyte_characters))
	while (same_at_end < ZV_BYTE
	       && ! CHAR_HEAD_P (FETCH_BYTE (same_at_end)))
	  same_at_end++;

      /* Don't try to reuse the same piece of text twice.  */
      overlap = same_at_start - BEGV_BYTE - (same_at_end + inserted - ZV_BYTE);
      if (overlap > 0)
	same_at_end += overlap;

      /* If display currently starts at beginning of line,
	 keep it that way.  */
      if (XBUFFER (XWINDOW (selected_window)->buffer) == current_buffer)
	XWINDOW (selected_window)->start_at_line_beg = Fbolp ();

      /* Replace the chars that we need to replace,
	 and update INSERTED to equal the number of bytes
	 we are taking from the file.  */
      inserted -= (Z_BYTE - same_at_end) + (same_at_start - BEG_BYTE);

      if (same_at_end != same_at_start)
	{
	  del_range_byte (same_at_start, same_at_end, 0);
	  temp = GPT;
	  same_at_start = GPT_BYTE;
	}
      else
	{
	  temp = BYTE_TO_CHAR (same_at_start);
	}
      /* Insert from the file at the proper position.  */
      SET_PT_BOTH (temp, same_at_start);
      insert_1 (conversion_buffer + same_at_start - BEG_BYTE, inserted,
		0, 0, 0);
      if (coding.cmp_data && coding.cmp_data->used)
	coding_restore_composition (&coding, Fcurrent_buffer ());
      coding_free_composition_data (&coding);
  
      /* Set `inserted' to the number of inserted characters.  */
      inserted = PT - temp;

      xfree (conversion_buffer);
      emacs_close (fd);
      specpdl_ptr--;

      goto handled;
    }

  if (! not_regular)
    {
      register Lisp_Object temp;

      total = XINT (end) - XINT (beg);

      /* Make sure point-max won't overflow after this insertion.  */
      XSETINT (temp, total);
      if (total != XINT (temp))
	error ("Maximum buffer size exceeded");
    }
  else
    /* For a special file, all we can do is guess.  */
    total = READ_BUF_SIZE;

  if (NILP (visit) && total > 0)
    prepare_to_modify_buffer (PT, PT, NULL);

  move_gap (PT);
  if (GAP_SIZE < total)
    make_gap (total - GAP_SIZE);

  if (XINT (beg) != 0 || !NILP (replace))
    {
      if (lseek (fd, XINT (beg), 0) < 0)
	report_file_error ("Setting file position",
			   Fcons (orig_filename, Qnil));
    }

  /* In the following loop, HOW_MUCH contains the total bytes read so
     far for a regular file, and not changed for a special file.  But,
     before exiting the loop, it is set to a negative value if I/O
     error occurs.  */
  how_much = 0;
  
  /* Total bytes inserted.  */
  inserted = 0;
  
  /* Here, we don't do code conversion in the loop.  It is done by
     code_convert_region after all data are read into the buffer.  */
  {
    int gap_size = GAP_SIZE;
    
    while (how_much < total)
      {
	/* try is reserved in some compilers (Microsoft C) */
	int trytry = min (total - how_much, READ_BUF_SIZE);
	int this;

	if (not_regular)
	  {
	    Lisp_Object val;

	    /* Maybe make more room.  */
	    if (gap_size < trytry)
	      {
		make_gap (total - gap_size);
		gap_size = GAP_SIZE;
	      }

	    /* Read from the file, capturing `quit'.  When an
	       error occurs, end the loop, and arrange for a quit
	       to be signaled after decoding the text we read.  */
	    non_regular_fd = fd;
	    non_regular_inserted = inserted;
	    non_regular_nbytes = trytry;
	    val = internal_condition_case_1 (read_non_regular, Qnil, Qerror,
					     read_non_regular_quit);
	    if (NILP (val))
	      {
		read_quit = 1;
		break;
	      }

	    this = XINT (val);
	  }
	else
	  {
	    /* Allow quitting out of the actual I/O.  We don't make text
	       part of the buffer until all the reading is done, so a C-g
	       here doesn't do any harm.  */
	    immediate_quit = 1;
	    QUIT;
	    this = emacs_read (fd, BEG_ADDR + PT_BYTE - BEG_BYTE + inserted, trytry);
	    immediate_quit = 0;
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

  emacs_close (fd);

  /* Discard the unwind protect for closing the file.  */
  specpdl_ptr--;

  if (how_much < 0)
    error ("IO error reading %s: %s",
	   XSTRING (orig_filename)->data, emacs_strerror (errno));

 notfound:

  if (! coding_system_decided)
    {
      /* The coding system is not yet decided.  Decide it by an
	 optimized method for handling `coding:' tag.

	 Note that we can get here only if the buffer was empty
	 before the insertion.  */
      Lisp_Object val;
      val = Qnil;

      if (!NILP (Vcoding_system_for_read))
	val = Vcoding_system_for_read;
      else
	{
	  /* Since we are sure that the current buffer was empty
	     before the insertion, we can toggle
	     enable-multibyte-characters directly here without taking
	     care of marker adjustment and byte combining problem.  By
	     this way, we can run Lisp program safely before decoding
	     the inserted text.  */
	  Lisp_Object unwind_data;
	      int count = specpdl_ptr - specpdl;

	  unwind_data = Fcons (current_buffer->enable_multibyte_characters,
			       Fcons (current_buffer->undo_list,
				      Fcurrent_buffer ()));
	      current_buffer->enable_multibyte_characters = Qnil;
	  current_buffer->undo_list = Qt;
	  record_unwind_protect (decide_coding_unwind, unwind_data);

	  if (inserted > 0 && ! NILP (Vset_auto_coding_function))
	    {
	      val = call2 (Vset_auto_coding_function,
			   filename, make_number (inserted));
	    }

	  if (NILP (val))
	    {
	      /* If the coding system is not yet decided, check
		 file-coding-system-alist.  */
	      Lisp_Object args[6], coding_systems;

	      args[0] = Qinsert_file_contents, args[1] = orig_filename;
	      args[2] = visit, args[3] = beg, args[4] = end, args[5] = Qnil;
	      coding_systems = Ffind_operation_coding_system (6, args);
	      if (CONSP (coding_systems))
		val = XCAR (coding_systems);
	    }

	  unbind_to (count, Qnil);
	  inserted = Z_BYTE - BEG_BYTE;
	}

      /* The following kludgy code is to avoid some compiler bug.
	 We can't simply do
	 setup_coding_system (val, &coding);
	 on some system.  */
      {
	struct coding_system temp_coding;
	setup_coding_system (val, &temp_coding);
	bcopy (&temp_coding, &coding, sizeof coding);
      }
      /* Ensure we set Vlast_coding_system_used.  */
      set_coding_system = 1;

      if (NILP (current_buffer->enable_multibyte_characters)
	  && ! NILP (val))
	/* We must suppress all character code conversion except for
	   end-of-line conversion.  */
	setup_raw_text_coding_system (&coding);
      coding.src_multibyte = 0;
      coding.dst_multibyte
	= !NILP (current_buffer->enable_multibyte_characters);
    }

  if (!NILP (visit)
      /* Can't do this if part of the buffer might be preserved.  */
      && NILP (replace)
      && (coding.type == coding_type_no_conversion
	  || coding.type == coding_type_raw_text))
    {
      /* Visiting a file with these coding system makes the buffer
         unibyte. */
      current_buffer->enable_multibyte_characters = Qnil;
      coding.dst_multibyte = 0;
    }

  if (inserted > 0 || coding.type == coding_type_ccl)
    {
      if (CODING_MAY_REQUIRE_DECODING (&coding))
	{
	  code_convert_region (PT, PT_BYTE, PT + inserted, PT_BYTE + inserted,
			       &coding, 0, 0);
	  inserted = coding.produced_char;
	}
      else
	adjust_after_insert (PT, PT_BYTE, PT + inserted, PT_BYTE + inserted,
 			     inserted);
    }

#ifdef DOS_NT
  /* Use the conversion type to determine buffer-file-type
     (find-buffer-file-type is now used to help determine the
     conversion).  */
  if ((coding.eol_type == CODING_EOL_UNDECIDED 
       || coding.eol_type == CODING_EOL_LF)
      && ! CODING_REQUIRE_DECODING (&coding))
    current_buffer->buffer_file_type = Qt;
  else
    current_buffer->buffer_file_type = Qnil;
#endif

 handled:

  if (!NILP (visit))
    {
      if (!EQ (current_buffer->undo_list, Qt))
	current_buffer->undo_list = Qnil;
#ifdef APOLLO
      stat (XSTRING (filename)->data, &st);
#endif

      if (NILP (handler))
	{
	  current_buffer->modtime = st.st_mtime;
	  current_buffer->filename = orig_filename;
	}

      SAVE_MODIFF = MODIFF;
      current_buffer->auto_save_modified = MODIFF;
      XSETFASTINT (current_buffer->save_length, Z - BEG);
#ifdef CLASH_DETECTION
      if (NILP (handler))
	{
	  if (!NILP (current_buffer->file_truename))
	    unlock_file (current_buffer->file_truename);
	  unlock_file (filename);
	}
#endif /* CLASH_DETECTION */
      if (not_regular)
	Fsignal (Qfile_error,
		 Fcons (build_string ("not a regular file"),
			Fcons (orig_filename, Qnil)));
    }

  /* Decode file format */
  if (inserted > 0)
    {
      int empty_undo_list_p = 0;
      
      /* If we're anyway going to discard undo information, don't
	 record it in the first place.  The buffer's undo list at this
	 point is either nil or t when visiting a file.  */
      if (!NILP (visit))
	{
	  empty_undo_list_p = NILP (current_buffer->undo_list);
	  current_buffer->undo_list = Qt;
	}
	  
      insval = call3 (Qformat_decode,
		      Qnil, make_number (inserted), visit);
      CHECK_NUMBER (insval);
      inserted = XFASTINT (insval);
      
      if (!NILP (visit))
	current_buffer->undo_list = empty_undo_list_p ? Qnil : Qt;
    }

  if (set_coding_system)
    Vlast_coding_system_used = coding.symbol;

  /* Call after-change hooks for the inserted text, aside from the case
     of normal visiting (not with REPLACE), which is done in a new buffer
     "before" the buffer is changed.  */
  if (inserted > 0 && total > 0
      && (NILP (visit) || !NILP (replace)))
    {
      signal_after_change (PT, 0, inserted);
      update_compositions (PT, PT, CHECK_BORDER);
    }

  p = Vafter_insert_file_functions;
  while (CONSP (p))
    {
      insval = call1 (XCAR (p), make_number (inserted));
      if (!NILP (insval))
	{
	  CHECK_NUMBER (insval);
	  inserted = XFASTINT (insval);
	}
      QUIT;
      p = XCDR (p);
    }

  if (!NILP (visit)
      && current_buffer->modtime == -1)
    {
      /* If visiting nonexistent file, return nil.  */
      report_file_error ("Opening input file", Fcons (orig_filename, Qnil));
    }

  if (read_quit)
    Fsignal (Qquit, Qnil);

  /* ??? Retval needs to be dealt with in all cases consistently.  */
  if (NILP (val))
    val = Fcons (orig_filename,
		 Fcons (make_number (inserted),
			Qnil));

  RETURN_UNGCPRO (unbind_to (count, val));
}

static Lisp_Object build_annotations P_ ((Lisp_Object, Lisp_Object));
static Lisp_Object build_annotations_2 P_ ((Lisp_Object, Lisp_Object,
					    Lisp_Object, Lisp_Object));

/* If build_annotations switched buffers, switch back to BUF.
   Kill the temporary buffer that was selected in the meantime.

   Since this kill only the last temporary buffer, some buffers remain
   not killed if build_annotations switched buffers more than once.
   -- K.Handa */

static Lisp_Object
build_annotations_unwind (buf)
     Lisp_Object buf;
{
  Lisp_Object tembuf;

  if (XBUFFER (buf) == current_buffer)
    return Qnil;
  tembuf = Fcurrent_buffer ();
  Fset_buffer (buf);
  Fkill_buffer (tembuf);
  return Qnil;
}

/* Decide the coding-system to encode the data with.  */

void
choose_write_coding_system (start, end, filename,
			    append, visit, lockname, coding)
     Lisp_Object start, end, filename, append, visit, lockname;
     struct coding_system *coding;
{
  Lisp_Object val;

  if (auto_saving)
    val = Qnil;
  else if (!NILP (Vcoding_system_for_write))
    val = Vcoding_system_for_write;
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
      int using_default_coding = 0;
      int force_raw_text = 0;

      val = current_buffer->buffer_file_coding_system;
      if (NILP (val)
	  || NILP (Flocal_variable_p (Qbuffer_file_coding_system, Qnil)))
	{
	  val = Qnil;
	  if (NILP (current_buffer->enable_multibyte_characters))
	    force_raw_text = 1;
	}
	
      if (NILP (val))
	{
	  /* Check file-coding-system-alist.  */
	  Lisp_Object args[7], coding_systems;

	  args[0] = Qwrite_region; args[1] = start; args[2] = end;
	  args[3] = filename; args[4] = append; args[5] = visit;
	  args[6] = lockname;
	  coding_systems = Ffind_operation_coding_system (7, args);
	  if (CONSP (coding_systems) && !NILP (XCDR (coding_systems)))
	    val = XCDR (coding_systems);
	}

      if (NILP (val)
	  && !NILP (current_buffer->buffer_file_coding_system))
	{
	  /* If we still have not decided a coding system, use the
	     default value of buffer-file-coding-system.  */
	  val = current_buffer->buffer_file_coding_system;
	  using_default_coding = 1;
	}
	    
      if (!force_raw_text
	  && !NILP (Ffboundp (Vselect_safe_coding_system_function)))
	/* Confirm that VAL can surely encode the current region.  */
	val = call3 (Vselect_safe_coding_system_function, start, end, val);

      setup_coding_system (Fcheck_coding_system (val), coding);
      if (coding->eol_type == CODING_EOL_UNDECIDED
	  && !using_default_coding)
	{
	  if (! EQ (default_buffer_file_coding.symbol,
		    buffer_defaults.buffer_file_coding_system))
	    setup_coding_system (buffer_defaults.buffer_file_coding_system,
				 &default_buffer_file_coding);
	  if (default_buffer_file_coding.eol_type != CODING_EOL_UNDECIDED)
	    {
	      Lisp_Object subsidiaries;

	      coding->eol_type = default_buffer_file_coding.eol_type;
	      subsidiaries = Fget (coding->symbol, Qeol_type);
	      if (VECTORP (subsidiaries)
		  && XVECTOR (subsidiaries)->size == 3)
		coding->symbol
		  = XVECTOR (subsidiaries)->contents[coding->eol_type];
	    }
	}

      if (force_raw_text)
	setup_raw_text_coding_system (coding);
      goto done_setup_coding;
    }

  setup_coding_system (Fcheck_coding_system (val), coding);

 done_setup_coding:
  if (!STRINGP (start) && !NILP (current_buffer->selective_display))
    coding->mode |= CODING_MODE_SELECTIVE_DISPLAY;
}

DEFUN ("write-region", Fwrite_region, Swrite_region, 3, 7,
       "r\nFWrite region to file: \ni\ni\ni\np",
       doc: /* Write current region into specified file.
When called from a program, requires three arguments:
START, END and FILENAME.  START and END are normally buffer positions
specifying the part of the buffer to write.
If START is nil, that means to use the entire buffer contents.
If START is a string, then output that string to the file
instead of any buffer contents; END is ignored.

Optional fourth argument APPEND if non-nil means
  append to existing file contents (if any).  If it is an integer,
  seek to that offset in the file before writing.
Optional fifth argument VISIT if t means
  set the last-save-file-modtime of buffer to this file's modtime
  and mark buffer not modified.
If VISIT is a string, it is a second file name;
  the output goes to FILENAME, but the buffer is marked as visiting VISIT.
  VISIT is also the file name to lock and unlock for clash detection.
If VISIT is neither t nor nil nor a string,
  that means do not print the \"Wrote file\" message.
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
`last-coding-system-used' to the coding system actually used.  */)
     (start, end, filename, append, visit, lockname, mustbenew)
     Lisp_Object start, end, filename, append, visit, lockname, mustbenew;
{
  register int desc;
  int failure;
  int save_errno = 0;
  unsigned char *fn;
  struct stat st;
  int tem;
  int count = specpdl_ptr - specpdl;
  int count1;
#ifdef VMS
  unsigned char *fname = 0;     /* If non-0, original filename (must rename) */
#endif /* VMS */
  Lisp_Object handler;
  Lisp_Object visit_file;
  Lisp_Object annotations;
  Lisp_Object encoded_filename;
  int visiting = (EQ (visit, Qt) || STRINGP (visit));
  int quietly = !NILP (visit);
  struct gcpro gcpro1, gcpro2, gcpro3, gcpro4, gcpro5;
  struct buffer *given_buffer;
#ifdef DOS_NT
  int buffer_file_type = O_BINARY;
#endif /* DOS_NT */
  struct coding_system coding;

  if (current_buffer->base_buffer && visiting)
    error ("Cannot do file visiting in an indirect buffer");

  if (!NILP (start) && !STRINGP (start))
    validate_region (&start, &end);

  GCPRO5 (start, filename, visit, visit_file, lockname);

  filename = Fexpand_file_name (filename, Qnil);

  if (!NILP (mustbenew) && !EQ (mustbenew, Qexcl))
    barf_or_query_if_file_exists (filename, "overwrite", 1, 0, 1);

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
      val = call6 (handler, Qwrite_region, start, end,
		   filename, append, visit);

      if (visiting)
	{
	  SAVE_MODIFF = MODIFF;
	  XSETFASTINT (current_buffer->save_length, Z - BEG);
	  current_buffer->filename = visit_file;
	}
      UNGCPRO;
      return val;
    }

  /* Special kludge to simplify auto-saving.  */
  if (NILP (start))
    {
      XSETFASTINT (start, BEG);
      XSETFASTINT (end, Z);
    }

  record_unwind_protect (build_annotations_unwind, Fcurrent_buffer ());
  count1 = specpdl_ptr - specpdl;

  given_buffer = current_buffer;
  annotations = build_annotations (start, end);
  if (current_buffer != given_buffer)
    {
      XSETFASTINT (start, BEGV);
      XSETFASTINT (end, ZV);
    }

  UNGCPRO;

  GCPRO5 (start, filename, annotations, visit_file, lockname);

  /* Decide the coding-system to encode the data with.
     We used to make this choice before calling build_annotations, but that
     leads to problems when a write-annotate-function takes care of
     unsavable chars (as was the case with X-Symbol).  */
  choose_write_coding_system (start, end, filename,
			      append, visit, lockname, &coding);
  Vlast_coding_system_used = coding.symbol;

  given_buffer = current_buffer;
  annotations = build_annotations_2 (start, end,
				     coding.pre_write_conversion, annotations);
  if (current_buffer != given_buffer)
    {
      XSETFASTINT (start, BEGV);
      XSETFASTINT (end, ZV);
    }

#ifdef CLASH_DETECTION
  if (!auto_saving)
    {
#if 0  /* This causes trouble for GNUS.  */
      /* If we've locked this file for some other buffer,
	 query before proceeding.  */
      if (!visiting && EQ (Ffile_locked_p (lockname), Qt))
	call2 (intern ("ask-user-about-lock"), filename, Vuser_login_name);
#endif

      lock_file (lockname);
    }
#endif /* CLASH_DETECTION */

  encoded_filename = ENCODE_FILE (filename);

  fn = XSTRING (encoded_filename)->data;
  desc = -1;
  if (!NILP (append))
#ifdef DOS_NT
    desc = emacs_open (fn, O_WRONLY | buffer_file_type, 0);
#else  /* not DOS_NT */
    desc = emacs_open (fn, O_WRONLY, 0);
#endif /* not DOS_NT */

  if (desc < 0 && (NILP (append) || errno == ENOENT))
#ifdef VMS
    if (auto_saving)    /* Overwrite any previous version of autosave file */
      {
	vms_truncate (fn);      /* if fn exists, truncate to zero length */
	desc = emacs_open (fn, O_RDWR, 0);
	if (desc < 0)
	  desc = creat_copy_attrs (STRINGP (current_buffer->filename)
				   ? XSTRING (current_buffer->filename)->data : 0,
				   fn);
      }
    else                /* Write to temporary name and rename if no errors */
      {
	Lisp_Object temp_name;
	temp_name = Ffile_name_directory (filename);

	if (!NILP (temp_name))
	  {
	    temp_name = Fmake_temp_name (concat2 (temp_name,
						  build_string ("$$SAVE$$")));
	    fname = XSTRING (filename)->data;
	    fn = XSTRING (temp_name)->data;
	    desc = creat_copy_attrs (fname, fn);
	    if (desc < 0)
	      {
		/* If we can't open the temporary file, try creating a new
		   version of the original file.  VMS "creat" creates a
		   new version rather than truncating an existing file. */
		fn = fname;
		fname = 0;
		desc = creat (fn, 0666);
#if 0 /* This can clobber an existing file and fail to replace it,
	 if the user runs out of space.  */
		if (desc < 0)
		  {
		    /* We can't make a new version;
		       try to truncate and rewrite existing version if any.  */
		    vms_truncate (fn);
		    desc = emacs_open (fn, O_RDWR, 0);
		  }
#endif
	      }
	  }
	else
	  desc = creat (fn, 0666);
      }
#else /* not VMS */
#ifdef DOS_NT
  desc = emacs_open (fn,
		     O_WRONLY | O_CREAT | buffer_file_type
		     | (EQ (mustbenew, Qexcl) ? O_EXCL : O_TRUNC),
		     S_IREAD | S_IWRITE);
#else  /* not DOS_NT */
  desc = emacs_open (fn, O_WRONLY | O_TRUNC | O_CREAT
		     | (EQ (mustbenew, Qexcl) ? O_EXCL : 0),
		     auto_saving ? auto_save_mode_bits : 0666);
#endif /* not DOS_NT */
#endif /* not VMS */

  if (desc < 0)
    {
#ifdef CLASH_DETECTION
      save_errno = errno;
      if (!auto_saving) unlock_file (lockname);
      errno = save_errno;
#endif /* CLASH_DETECTION */
      UNGCPRO;
      report_file_error ("Opening output file", Fcons (filename, Qnil));
    }

  record_unwind_protect (close_file_unwind, make_number (desc));

  if (!NILP (append) && !NILP (Ffile_regular_p (filename)))
    {
      long ret;
      
      if (NUMBERP (append))
	ret = lseek (desc, XINT (append), 1);
      else
	ret = lseek (desc, 0, 2);
      if (ret < 0)
	{
#ifdef CLASH_DETECTION
	  if (!auto_saving) unlock_file (lockname);
#endif /* CLASH_DETECTION */
	  UNGCPRO;
	  report_file_error ("Lseek error", Fcons (filename, Qnil));
	}
    }
  
  UNGCPRO;

#ifdef VMS
/*
 * Kludge Warning: The VMS C RTL likes to insert carriage returns
 * if we do writes that don't end with a carriage return. Furthermore
 * it cannot handle writes of more then 16K. The modified
 * version of "sys_write" in SYSDEP.C (see comment there) copes with
 * this EXCEPT for the last record (iff it doesn't end with a carriage
 * return). This implies that if your buffer doesn't end with a carriage
 * return, you get one free... tough. However it also means that if
 * we make two calls to sys_write (a la the following code) you can
 * get one at the gap as well. The easiest way to fix this (honest)
 * is to move the gap to the next newline (or the end of the buffer).
 * Thus this change.
 *
 * Yech!
 */
  if (GPT > BEG && GPT_ADDR[-1] != '\n')
    move_gap (find_next_newline (GPT, 1));
#else
  /* Whether VMS or not, we must move the gap to the next of newline
     when we must put designation sequences at beginning of line.  */
  if (INTEGERP (start)
      && coding.type == coding_type_iso2022
      && coding.flags & CODING_FLAG_ISO_DESIGNATE_AT_BOL
      && GPT > BEG && GPT_ADDR[-1] != '\n')
    {
      int opoint = PT, opoint_byte = PT_BYTE;
      scan_newline (PT, PT_BYTE, ZV, ZV_BYTE, 1, 0);
      move_gap_both (PT, PT_BYTE);
      SET_PT_BOTH (opoint, opoint_byte);
    }
#endif

  failure = 0;
  immediate_quit = 1;

  if (STRINGP (start))
    {
      failure = 0 > a_write (desc, start, 0, XSTRING (start)->size,
			     &annotations, &coding);
      save_errno = errno;
    }
  else if (XINT (start) != XINT (end))
    {
      tem = CHAR_TO_BYTE (XINT (start));

      if (XINT (start) < GPT)
	{
	  failure = 0 > a_write (desc, Qnil, XINT (start),
				 min (GPT, XINT (end)) - XINT (start),
				 &annotations, &coding);
	  save_errno = errno;
	}

      if (XINT (end) > GPT && !failure)
	{
	  tem = max (XINT (start), GPT);
	  failure = 0 > a_write (desc, Qnil, tem , XINT (end) - tem,
				 &annotations, &coding);
	  save_errno = errno;
	}
    }
  else
    {
      /* If file was empty, still need to write the annotations */
      coding.mode |= CODING_MODE_LAST_BLOCK;
      failure = 0 > a_write (desc, Qnil, XINT (end), 0, &annotations, &coding);
      save_errno = errno;
    }

  if (CODING_REQUIRE_FLUSHING (&coding)
      && !(coding.mode & CODING_MODE_LAST_BLOCK)
      && ! failure)
    {
      /* We have to flush out a data. */
      coding.mode |= CODING_MODE_LAST_BLOCK;
      failure = 0 > e_write (desc, Qnil, 0, 0, &coding);
      save_errno = errno;
    }

  immediate_quit = 0;

#ifdef HAVE_FSYNC
  /* Note fsync appears to change the modtime on BSD4.2 (both vax and sun).
     Disk full in NFS may be reported here.  */
  /* mib says that closing the file will try to write as fast as NFS can do
     it, and that means the fsync here is not crucial for autosave files.  */
  if (!auto_saving && fsync (desc) < 0)
    {
      /* If fsync fails with EINTR, don't treat that as serious.  */
      if (errno != EINTR)
	failure = 1, save_errno = errno;
    }
#endif

  /* Spurious "file has changed on disk" warnings have been
     observed on Suns as well.
     It seems that `close' can change the modtime, under nfs.

     (This has supposedly been fixed in Sunos 4,
     but who knows about all the other machines with NFS?)  */
#if 0

  /* On VMS and APOLLO, must do the stat after the close
     since closing changes the modtime.  */
#ifndef VMS
#ifndef APOLLO
  /* Recall that #if defined does not work on VMS.  */
#define FOO
  fstat (desc, &st);
#endif
#endif
#endif

  /* NFS can report a write failure now.  */
  if (emacs_close (desc) < 0)
    failure = 1, save_errno = errno;

#ifdef VMS
  /* If we wrote to a temporary name and had no errors, rename to real name. */
  if (fname)
    {
      if (!failure)
	failure = (rename (fn, fname) != 0), save_errno = errno;
      fn = fname;
    }
#endif /* VMS */

#ifndef FOO
  stat (fn, &st);
#endif
  /* Discard the unwind protect for close_file_unwind.  */
  specpdl_ptr = specpdl + count1;
  /* Restore the original current buffer.  */
  visit_file = unbind_to (count, visit_file);

#ifdef CLASH_DETECTION
  if (!auto_saving)
    unlock_file (lockname);
#endif /* CLASH_DETECTION */

  /* Do this before reporting IO error
     to avoid a "file has changed on disk" warning on
     next attempt to save.  */
  if (visiting)
    current_buffer->modtime = st.st_mtime;

  if (failure)
    error ("IO error writing %s: %s", XSTRING (filename)->data,
	   emacs_strerror (save_errno));

  if (visiting)
    {
      SAVE_MODIFF = MODIFF;
      XSETFASTINT (current_buffer->save_length, Z - BEG);
      current_buffer->filename = visit_file;
      update_mode_lines++;
    }
  else if (quietly)
    return Qnil;

  if (!auto_saving)
    message_with_string ("Wrote %s", visit_file, 1);

  return Qnil;
}

Lisp_Object merge ();

DEFUN ("car-less-than-car", Fcar_less_than_car, Scar_less_than_car, 2, 2, 0,
       doc: /* Return t if (car A) is numerically less than (car B).  */)
     (a, b)
     Lisp_Object a, b;
{
  return Flss (Fcar (a), Fcar (b));
}

/* Build the complete list of annotations appropriate for writing out
   the text between START and END, by calling all the functions in
   write-region-annotate-functions and merging the lists they return.
   If one of these functions switches to a different buffer, we assume
   that buffer contains altered text.  Therefore, the caller must
   make sure to restore the current buffer in all cases,
   as save-excursion would do.  */

static Lisp_Object
build_annotations (start, end)
     Lisp_Object start, end;
{
  Lisp_Object annotations;
  Lisp_Object p, res;
  struct gcpro gcpro1, gcpro2;
  Lisp_Object original_buffer;
  int i;

  XSETBUFFER (original_buffer, current_buffer);

  annotations = Qnil;
  p = Vwrite_region_annotate_functions;
  GCPRO2 (annotations, p);
  while (CONSP (p))
    {
      struct buffer *given_buffer = current_buffer;
      Vwrite_region_annotations_so_far = annotations;
      res = call2 (XCAR (p), start, end);
      /* If the function makes a different buffer current,
	 assume that means this buffer contains altered text to be output.
	 Reset START and END from the buffer bounds
	 and discard all previous annotations because they should have
	 been dealt with by this function.  */
      if (current_buffer != given_buffer)
	{
	  XSETFASTINT (start, BEGV);
	  XSETFASTINT (end, ZV);
	  annotations = Qnil;
	}
      Flength (res);   /* Check basic validity of return value */
      annotations = merge (annotations, res, Qcar_less_than_car);
      p = XCDR (p);
    }

  /* Now do the same for annotation functions implied by the file-format */
  if (auto_saving && (!EQ (Vauto_save_file_format, Qt)))
    p = Vauto_save_file_format;
  else
    p = current_buffer->file_format;
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

  UNGCPRO;
  return annotations;
}

static Lisp_Object
build_annotations_2 (start, end, pre_write_conversion, annotations)
     Lisp_Object start, end, pre_write_conversion, annotations;
{
  struct gcpro gcpro1;
  Lisp_Object res;

  GCPRO1 (annotations);
  /* At last, do the same for the function PRE_WRITE_CONVERSION
     implied by the current coding-system.  */
  if (!NILP (pre_write_conversion))
    {
      struct buffer *given_buffer = current_buffer;
      Vwrite_region_annotations_so_far = annotations;
      res = call2 (pre_write_conversion, start, end);
      Flength (res);
      annotations = (current_buffer != given_buffer
		     ? res
		     : merge (annotations, res, Qcar_less_than_car));
    }

  UNGCPRO;
  return annotations;
}

/* Write to descriptor DESC the NCHARS chars starting at POS of STRING.
   If STRING is nil, POS is the character position in the current buffer.
   Intersperse with them the annotations from *ANNOT
   which fall within the range of POS to POS + NCHARS,
   each at its appropriate position.

   We modify *ANNOT by discarding elements as we use them up.

   The return value is negative in case of system call failure.  */

static int
a_write (desc, string, pos, nchars, annot, coding)
     int desc;
     Lisp_Object string;
     register int nchars;
     int pos;
     Lisp_Object *annot;
     struct coding_system *coding;
{
  Lisp_Object tem;
  int nextpos;
  int lastpos = pos + nchars;

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
	  if (0 > e_write (desc, string, pos, nextpos, coding))
	    return -1;
	  pos = nextpos;
	}
      /* Output the annotation.  */
      tem = Fcdr (Fcar (*annot));
      if (STRINGP (tem))
	{
	  if (0 > e_write (desc, tem, 0, XSTRING (tem)->size, coding))
	    return -1;
	}
      *annot = Fcdr (*annot);
    }
  return 0;
}

#ifndef WRITE_BUF_SIZE
#define WRITE_BUF_SIZE (16 * 1024)
#endif

/* Write text in the range START and END into descriptor DESC,
   encoding them with coding system CODING.  If STRING is nil, START
   and END are character positions of the current buffer, else they
   are indexes to the string STRING.  */

static int
e_write (desc, string, start, end, coding)
     int desc;
     Lisp_Object string;
     int start, end;
     struct coding_system *coding;
{
  register char *addr;
  register int nbytes;
  char buf[WRITE_BUF_SIZE];
  int return_val = 0;

  if (start >= end)
    coding->composing = COMPOSITION_DISABLED;
  if (coding->composing != COMPOSITION_DISABLED)
    coding_save_composition (coding, start, end, string);

  if (STRINGP (string))
    {
      addr = XSTRING (string)->data;
      nbytes = STRING_BYTES (XSTRING (string));
      coding->src_multibyte = STRING_MULTIBYTE (string);
    }
  else if (start < end)
    {
      /* It is assured that the gap is not in the range START and END-1.  */
      addr = CHAR_POS_ADDR (start);
      nbytes = CHAR_TO_BYTE (end) - CHAR_TO_BYTE (start);
      coding->src_multibyte
	= !NILP (current_buffer->enable_multibyte_characters);
    }
  else
    {
      addr = "";
      nbytes = 0;
      coding->src_multibyte = 1;
    }

  /* We used to have a code for handling selective display here.  But,
     now it is handled within encode_coding.  */
  while (1)
    {
      int result;

      result = encode_coding (coding, addr, buf, nbytes, WRITE_BUF_SIZE);
      if (coding->produced > 0)
	{
	  coding->produced -= emacs_write (desc, buf, coding->produced);
	  if (coding->produced)
	    {
	      return_val = -1;
	      break;
	    }
	}
      nbytes -= coding->consumed;
      addr += coding->consumed;
      if (result == CODING_FINISH_INSUFFICIENT_SRC
	  && nbytes > 0)
	{
	  /* The source text ends by an incomplete multibyte form.
             There's no way other than write it out as is.  */
	  nbytes -= emacs_write (desc, addr, nbytes);
	  if (nbytes)
	    {
	      return_val = -1;
	      break;
	    }
	}
      if (nbytes <= 0)
	break;
      start += coding->consumed_char;
      if (coding->cmp_data)
	coding_adjust_composition_offset (coding, start);
    }

  if (coding->cmp_data)
    coding_free_composition_data (coding);

  return return_val;
}

DEFUN ("verify-visited-file-modtime", Fverify_visited_file_modtime,
       Sverify_visited_file_modtime, 1, 1, 0,
       doc: /* Return t if last mod time of BUF's visited file matches what BUF records.
This means that the file has not been changed since it was visited or saved.  */)
     (buf)
     Lisp_Object buf;
{
  struct buffer *b;
  struct stat st;
  Lisp_Object handler;
  Lisp_Object filename;

  CHECK_BUFFER (buf);
  b = XBUFFER (buf);

  if (!STRINGP (b->filename)) return Qt;
  if (b->modtime == 0) return Qt;

  /* If the file name has special constructs in it,
     call the corresponding file handler.  */
  handler = Ffind_file_name_handler (b->filename,
				     Qverify_visited_file_modtime);
  if (!NILP (handler))
    return call2 (handler, Qverify_visited_file_modtime, buf);

  filename = ENCODE_FILE (b->filename);

  if (stat (XSTRING (filename)->data, &st) < 0)
    {
      /* If the file doesn't exist now and didn't exist before,
	 we say that it isn't modified, provided the error is a tame one.  */
      if (errno == ENOENT || errno == EACCES || errno == ENOTDIR)
	st.st_mtime = -1;
      else
	st.st_mtime = 0;
    }
  if (st.st_mtime == b->modtime
      /* If both are positive, accept them if they are off by one second.  */
      || (st.st_mtime > 0 && b->modtime > 0
	  && (st.st_mtime == b->modtime + 1
	      || st.st_mtime == b->modtime - 1)))
    return Qt;
  return Qnil;
}

DEFUN ("clear-visited-file-modtime", Fclear_visited_file_modtime,
       Sclear_visited_file_modtime, 0, 0, 0,
       doc: /* Clear out records of last mod time of visited file.
Next attempt to save will certainly not complain of a discrepancy.  */)
     ()
{
  current_buffer->modtime = 0;
  return Qnil;
}

DEFUN ("visited-file-modtime", Fvisited_file_modtime,
       Svisited_file_modtime, 0, 0, 0,
       doc: /* Return the current buffer's recorded visited file modification time.
The value is a list of the form (HIGH . LOW), like the time values
that `file-attributes' returns.  */)
     ()
{
  return long_to_cons ((unsigned long) current_buffer->modtime);
}

DEFUN ("set-visited-file-modtime", Fset_visited_file_modtime,
       Sset_visited_file_modtime, 0, 1, 0,
       doc: /* Update buffer's recorded modification time from the visited file's time.
Useful if the buffer was not read from the file normally
or if the file itself has been changed for some known benign reason.
An argument specifies the modification time value to use
\(instead of that of the visited file), in the form of a list
\(HIGH . LOW) or (HIGH LOW).  */)
     (time_list)
     Lisp_Object time_list;
{
  if (!NILP (time_list))
    current_buffer->modtime = cons_to_long (time_list);
  else
    {
      register Lisp_Object filename;
      struct stat st;
      Lisp_Object handler;

      filename = Fexpand_file_name (current_buffer->filename, Qnil);

      /* If the file name has special constructs in it,
	 call the corresponding file handler.  */
      handler = Ffind_file_name_handler (filename, Qset_visited_file_modtime);
      if (!NILP (handler))
	/* The handler can find the file name the same way we did.  */
	return call2 (handler, Qset_visited_file_modtime, Qnil);

      filename = ENCODE_FILE (filename);

      if (stat (XSTRING (filename)->data, &st) >= 0)
	current_buffer->modtime = st.st_mtime;
    }

  return Qnil;
}

Lisp_Object
auto_save_error (error)
     Lisp_Object error;
{
  Lisp_Object args[3], msg;
  int i, nbytes;
  struct gcpro gcpro1;
  
  ring_bell ();
  
  args[0] = build_string ("Auto-saving %s: %s");
  args[1] = current_buffer->name;
  args[2] = Ferror_message_string (error);
  msg = Fformat (3, args);
  GCPRO1 (msg);
  nbytes = STRING_BYTES (XSTRING (msg));

  for (i = 0; i < 3; ++i)
    {
      if (i == 0)
	message2 (XSTRING (msg)->data, nbytes, STRING_MULTIBYTE (msg));
      else
	message2_nolog (XSTRING (msg)->data, nbytes, STRING_MULTIBYTE (msg));
      Fsleep_for (make_number (1), Qnil);
    }

  UNGCPRO;
  return Qnil;
}

Lisp_Object
auto_save_1 ()
{
  struct stat st;

  /* Get visited file's mode to become the auto save file's mode.  */
  if (! NILP (current_buffer->filename)
      && stat (XSTRING (current_buffer->filename)->data, &st) >= 0)
    /* But make sure we can overwrite it later!  */
    auto_save_mode_bits = st.st_mode | 0600;
  else
    auto_save_mode_bits = 0666;

  return
    Fwrite_region (Qnil, Qnil,
		   current_buffer->auto_save_file_name,
		   Qnil, Qlambda, Qnil, Qnil);
}

static Lisp_Object
do_auto_save_unwind (stream)  /* used as unwind-protect function */
     Lisp_Object stream;
{
  auto_saving = 0;
  if (!NILP (stream))
    fclose ((FILE *) (XFASTINT (XCAR (stream)) << 16
		      | XFASTINT (XCDR (stream))));
  pop_message ();
  return Qnil;
}

static Lisp_Object
do_auto_save_unwind_1 (value)  /* used as unwind-protect function */
     Lisp_Object value;
{
  minibuffer_auto_raise = XINT (value);
  return Qnil;
}

DEFUN ("do-auto-save", Fdo_auto_save, Sdo_auto_save, 0, 2, "",
       doc: /* Auto-save all buffers that need it.
This is all buffers that have auto-saving enabled
and are changed since last auto-saved.
Auto-saving writes the buffer into a file
so that your editing is not lost if the system crashes.
This file is not the file you visited; that changes only when you save.
Normally we run the normal hook `auto-save-hook' before saving.

A non-nil NO-MESSAGE argument means do not print any message if successful.
A non-nil CURRENT-ONLY argument means save only current buffer.  */)
     (no_message, current_only)
     Lisp_Object no_message, current_only;
{
  struct buffer *old = current_buffer, *b;
  Lisp_Object tail, buf;
  int auto_saved = 0;
  int do_handled_files;
  Lisp_Object oquit;
  FILE *stream;
  Lisp_Object lispstream;
  int count = specpdl_ptr - specpdl;
  int orig_minibuffer_auto_raise = minibuffer_auto_raise;
  int message_p = 0;

  if (max_specpdl_size < specpdl_size + 40)
    max_specpdl_size = specpdl_size + 40;

  if (minibuf_level)
    no_message = Qt;

  if (NILP (no_message));
    message_p = push_message ();
  
  /* Ordinarily don't quit within this function,
     but don't make it impossible to quit (in case we get hung in I/O).  */
  oquit = Vquit_flag;
  Vquit_flag = Qnil;

  /* No GCPRO needed, because (when it matters) all Lisp_Object variables
     point to non-strings reached from Vbuffer_alist.  */

  if (!NILP (Vrun_hooks))
    call1 (Vrun_hooks, intern ("auto-save-hook"));

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
	    call2 (Qmake_directory, dir, Qt);
	}
      
      stream = fopen (XSTRING (listfile)->data, "w");
      if (stream != NULL)
	{
	  /* Arrange to close that file whether or not we get an error.
	     Also reset auto_saving to 0.  */
	  lispstream = Fcons (Qnil, Qnil);
	  XSETCARFASTINT (lispstream, (EMACS_UINT)stream >> 16);
	  XSETCDRFASTINT (lispstream, (EMACS_UINT)stream & 0xffff);
	}
      else
	lispstream = Qnil;
    }
  else
    {
      stream = NULL;
      lispstream = Qnil;
    }

  record_unwind_protect (do_auto_save_unwind, lispstream);
  record_unwind_protect (do_auto_save_unwind_1,
			 make_number (minibuffer_auto_raise));
  minibuffer_auto_raise = 0;
  auto_saving = 1;

  /* First, save all files which don't have handlers.  If Emacs is
     crashing, the handlers may tweak what is causing Emacs to crash
     in the first place, and it would be a shame if Emacs failed to
     autosave perfectly ordinary files because it couldn't handle some
     ange-ftp'd file.  */
  for (do_handled_files = 0; do_handled_files < 2; do_handled_files++)
    for (tail = Vbuffer_alist; GC_CONSP (tail); tail = XCDR (tail))
      {
	buf = XCDR (XCAR (tail));
	b = XBUFFER (buf);

	/* Record all the buffers that have auto save mode
	   in the special file that lists them.  For each of these buffers,
	   Record visited name (if any) and auto save name.  */
	if (STRINGP (b->auto_save_file_name)
	    && stream != NULL && do_handled_files == 0)
	  {
	    if (!NILP (b->filename))
	      {
		fwrite (XSTRING (b->filename)->data, 1,
			STRING_BYTES (XSTRING (b->filename)), stream);
	      }
	    putc ('\n', stream);
	    fwrite (XSTRING (b->auto_save_file_name)->data, 1,
		    STRING_BYTES (XSTRING (b->auto_save_file_name)), stream);
	    putc ('\n', stream);
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
	if (STRINGP (b->auto_save_file_name)
	    && BUF_SAVE_MODIFF (b) < BUF_MODIFF (b)
	    && b->auto_save_modified < BUF_MODIFF (b)
	    /* -1 means we've turned off autosaving for a while--see below.  */
	    && XINT (b->save_length) >= 0
	    && (do_handled_files
		|| NILP (Ffind_file_name_handler (b->auto_save_file_name,
						  Qwrite_region))))
	  {
	    EMACS_TIME before_time, after_time;

	    EMACS_GET_TIME (before_time);

	    /* If we had a failure, don't try again for 20 minutes.  */
	    if (b->auto_save_failure_time >= 0
		&& EMACS_SECS (before_time) - b->auto_save_failure_time < 1200)
	      continue;

	    if ((XFASTINT (b->save_length) * 10
		 > (BUF_Z (b) - BUF_BEG (b)) * 13)
		/* A short file is likely to change a large fraction;
		   spare the user annoying messages.  */
		&& XFASTINT (b->save_length) > 5000
		/* These messages are frequent and annoying for `*mail*'.  */
		&& !EQ (b->filename, Qnil)
		&& NILP (no_message))
	      {
		/* It has shrunk too much; turn off auto-saving here.  */
		minibuffer_auto_raise = orig_minibuffer_auto_raise;
		message_with_string ("Buffer %s has shrunk a lot; auto save disabled in that buffer until next real save",
				     b->name, 1);
		minibuffer_auto_raise = 0;
		/* Turn off auto-saving until there's a real save,
		   and prevent any more warnings.  */
		XSETINT (b->save_length, -1);
		Fsleep_for (make_number (1), Qnil);
		continue;
	      }
	    set_buffer_internal (b);
	    if (!auto_saved && NILP (no_message))
	      message1 ("Auto-saving...");
	    internal_condition_case (auto_save_1, Qt, auto_save_error);
	    auto_saved++;
	    b->auto_save_modified = BUF_MODIFF (b);
	    XSETFASTINT (current_buffer->save_length, Z - BEG);
	    set_buffer_internal (old);

	    EMACS_GET_TIME (after_time);

	    /* If auto-save took more than 60 seconds,
	       assume it was an NFS failure that got a timeout.  */
	    if (EMACS_SECS (after_time) - EMACS_SECS (before_time) > 60)
	      b->auto_save_failure_time = EMACS_SECS (after_time);
	  }
      }

  /* Prevent another auto save till enough input events come in.  */
  record_auto_save ();

  if (auto_saved && NILP (no_message))
    {
      if (message_p)
	{
	  sit_for (1, 0, 0, 0, 0);
	  restore_message ();
	}
      else
	message1 ("Auto-saving...done");
    }

  Vquit_flag = oquit;

  unbind_to (count, Qnil);
  return Qnil;
}

DEFUN ("set-buffer-auto-saved", Fset_buffer_auto_saved,
       Sset_buffer_auto_saved, 0, 0, 0,
       doc: /* Mark current buffer as auto-saved with its current text.
No auto-save file will be written until the buffer changes again.  */)
     ()
{
  current_buffer->auto_save_modified = MODIFF;
  XSETFASTINT (current_buffer->save_length, Z - BEG);
  current_buffer->auto_save_failure_time = -1;
  return Qnil;
}

DEFUN ("clear-buffer-auto-save-failure", Fclear_buffer_auto_save_failure,
       Sclear_buffer_auto_save_failure, 0, 0, 0,
       doc: /* Clear any record of a recent auto-save failure in the current buffer.  */)
     ()
{
  current_buffer->auto_save_failure_time = -1;
  return Qnil;
}

DEFUN ("recent-auto-save-p", Frecent_auto_save_p, Srecent_auto_save_p,
       0, 0, 0,
       doc: /* Return t if buffer has been auto-saved since last read in or saved.  */)
     ()
{
  return (SAVE_MODIFF < current_buffer->auto_save_modified) ? Qt : Qnil;
}

/* Reading and completing file names */
extern Lisp_Object Ffile_name_completion (), Ffile_name_all_completions ();

/* In the string VAL, change each $ to $$ and return the result.  */

static Lisp_Object
double_dollars (val)
     Lisp_Object val;
{
  register unsigned char *old, *new;
  register int n;
  int osize, count;

  osize = STRING_BYTES (XSTRING (val));

  /* Count the number of $ characters.  */
  for (n = osize, count = 0, old = XSTRING (val)->data; n > 0; n--)
    if (*old++ == '$') count++;
  if (count > 0)
    {
      old = XSTRING (val)->data;
      val = make_uninit_multibyte_string (XSTRING (val)->size + count,
					  osize + count);
      new = XSTRING (val)->data;
      for (n = osize; n > 0; n--)
	if (*old != '$')
	  *new++ = *old++;
	else
	  {
	    *new++ = '$';
	    *new++ = '$';
	    old++;
	  }
    }
  return val;
}

static Lisp_Object
read_file_name_cleanup (arg)
     Lisp_Object arg;
{
  current_buffer->directory = arg;
}

DEFUN ("read-file-name-internal", Fread_file_name_internal, Sread_file_name_internal,
       3, 3, 0,
       doc: /* Internal subroutine for read-file-name.  Do not call this.  */)
     (string, dir, action)
     Lisp_Object string, dir, action;
  /* action is nil for complete, t for return list of completions,
     lambda for verify final value */
{
  Lisp_Object name, specdir, realdir, val, orig_string;
  int changed;
  struct gcpro gcpro1, gcpro2, gcpro3, gcpro4, gcpro5;

  CHECK_STRING (string);

  realdir = dir;
  name = string;
  orig_string = Qnil;
  specdir = Qnil;
  changed = 0;
  /* No need to protect ACTION--we only compare it with t and nil.  */
  GCPRO5 (string, realdir, name, specdir, orig_string);

  if (XSTRING (string)->size == 0)
    {
      if (EQ (action, Qlambda))
	{
	  UNGCPRO;
	  return Qnil;
	}
    }
  else
    {
      orig_string = string;
      string = Fsubstitute_in_file_name (string);
      changed = NILP (Fstring_equal (string, orig_string));
      name = Ffile_name_nondirectory (string);
      val = Ffile_name_directory (string);
      if (! NILP (val))
	realdir = Fexpand_file_name (val, realdir);
    }

  if (NILP (action))
    {
      specdir = Ffile_name_directory (string);
      val = Ffile_name_completion (name, realdir);
      UNGCPRO;
      if (!STRINGP (val))
	{
	  if (changed)
	    return double_dollars (string);
	  return val;
	}

      if (!NILP (specdir))
	val = concat2 (specdir, val);
#ifndef VMS
      return double_dollars (val);
#else /* not VMS */
      return val;
#endif /* not VMS */
    }
  UNGCPRO;

  if (EQ (action, Qt))
    {
      Lisp_Object all = Ffile_name_all_completions (name, realdir);
      Lisp_Object comp;
      int count;

      if (NILP (Vread_file_name_predicate)
	  || EQ (Vread_file_name_predicate, Qfile_exists_p))
	return all;

#ifndef VMS
      if (EQ (Vread_file_name_predicate, Qfile_directory_p))
	{
	  /* Brute-force speed up for directory checking: 
	     Discard strings which don't end in a slash.  */
	  for (comp = Qnil; CONSP (all); all = XCDR (all))
	    {
	      Lisp_Object tem = XCAR (all);
	      int len;
	      if (STRINGP (tem) &&
		  (len = XSTRING (tem)->size, len > 0) &&
		  IS_DIRECTORY_SEP (XSTRING (tem)->data[len-1]))
		comp = Fcons (tem, comp);
	    }
	}
      else
#endif
	{
	  /* Must do it the hard (and slow) way.  */
	  GCPRO3 (all, comp, specdir);
	  count = specpdl_ptr - specpdl;
	  record_unwind_protect (read_file_name_cleanup, current_buffer->directory);
	  current_buffer->directory = realdir;
	  for (comp = Qnil; CONSP (all); all = XCDR (all))
	    if (!NILP (call1 (Vread_file_name_predicate, XCAR (all))))
	      comp = Fcons (XCAR (all), comp);
	  unbind_to (count, Qnil);
	  UNGCPRO;
	}
      return Fnreverse (comp);
    }

  /* Only other case actually used is ACTION = lambda */
#ifdef VMS
  /* Supposedly this helps commands such as `cd' that read directory names,
     but can someone explain how it helps them? -- RMS */
  if (XSTRING (name)->size == 0)
    return Qt;
#endif /* VMS */
  if (!NILP (Vread_file_name_predicate))
    return call1 (Vread_file_name_predicate, string);
  return Ffile_exists_p (string);
}

DEFUN ("read-file-name", Fread_file_name, Sread_file_name, 1, 6, 0,
       doc: /* Read file name, prompting with PROMPT and completing in directory DIR.
Value is not expanded---you must call `expand-file-name' yourself.
Default name to DEFAULT-FILENAME if user enters a null string.
 (If DEFAULT-FILENAME is omitted, the visited file name is used,
  except that if INITIAL is specified, that combined with DIR is used.)
Fourth arg MUSTMATCH non-nil means require existing file's name.
 Non-nil and non-t means also require confirmation after completion.
Fifth arg INITIAL specifies text to start with.
If optional sixth arg PREDICATE is non-nil, possible completions and the 
resulting file name must satisfy (funcall PREDICATE NAME).
DIR defaults to current buffer's directory default.

If this command was invoked with the mouse, use a file dialog box if
`use-dialog-box' is non-nil, and the window system or X toolkit in use
provides a file dialog box.  */)
     (prompt, dir, default_filename, mustmatch, initial, predicate)
     Lisp_Object prompt, dir, default_filename, mustmatch, initial, predicate;
{
  Lisp_Object val, insdef, tem;
  struct gcpro gcpro1, gcpro2;
  register char *homedir;
  int replace_in_history = 0;
  int add_to_history = 0;
  int count;

  if (NILP (dir))
    dir = current_buffer->directory;
  if (NILP (default_filename))
    {
      if (! NILP (initial))
	default_filename = Fexpand_file_name (initial, dir);
      else
	default_filename = current_buffer->filename;
    }

  /* If dir starts with user's homedir, change that to ~. */
  homedir = (char *) egetenv ("HOME");
#ifdef DOS_NT
  /* homedir can be NULL in temacs, since Vprocess_environment is not
     yet set up.  We shouldn't crash in that case.  */
  if (homedir != 0)
    {
      homedir = strcpy (alloca (strlen (homedir) + 1), homedir);
      CORRECT_DIR_SEPS (homedir);
    }
#endif
  if (homedir != 0
      && STRINGP (dir)
      && !strncmp (homedir, XSTRING (dir)->data, strlen (homedir))
      && IS_DIRECTORY_SEP (XSTRING (dir)->data[strlen (homedir)]))
    {
      dir = make_string (XSTRING (dir)->data + strlen (homedir) - 1,
			 STRING_BYTES (XSTRING (dir)) - strlen (homedir) + 1);
      XSTRING (dir)->data[0] = '~';
    }
  /* Likewise for default_filename.  */
  if (homedir != 0
      && STRINGP (default_filename)
      && !strncmp (homedir, XSTRING (default_filename)->data, strlen (homedir))
      && IS_DIRECTORY_SEP (XSTRING (default_filename)->data[strlen (homedir)]))
    {
      default_filename
	= make_string (XSTRING (default_filename)->data + strlen (homedir) - 1,
		       STRING_BYTES (XSTRING (default_filename)) - strlen (homedir) + 1);
      XSTRING (default_filename)->data[0] = '~';
    }
  if (!NILP (default_filename))
    {
      CHECK_STRING (default_filename);
      default_filename = double_dollars (default_filename);
    }

  if (insert_default_directory && STRINGP (dir))
    {
      insdef = dir;
      if (!NILP (initial))
	{
	  Lisp_Object args[2], pos;

	  args[0] = insdef;
	  args[1] = initial;
	  insdef = Fconcat (2, args);
	  pos = make_number (XSTRING (double_dollars (dir))->size);
	  insdef = Fcons (double_dollars (insdef), pos);
	}
      else
	insdef = double_dollars (insdef);
    }
  else if (STRINGP (initial))
    insdef = Fcons (double_dollars (initial), make_number (0));
  else
    insdef = Qnil;

  if (!NILP (Vread_file_name_function))
    {
      Lisp_Object args[7];

      GCPRO2 (insdef, default_filename);
      args[0] = Vread_file_name_function;
      args[1] = prompt;
      args[2] = dir;
      args[3] = default_filename;
      args[4] = mustmatch;
      args[5] = initial;
      args[6] = predicate;
      RETURN_UNGCPRO (Ffuncall (7, args));
    }

  count = specpdl_ptr - specpdl;
#ifdef VMS
  specbind (intern ("completion-ignore-case"), Qt);
#endif

  specbind (intern ("minibuffer-completing-file-name"), Qt);
  specbind (intern ("read-file-name-predicate"), 
	    (NILP (predicate) ? Qfile_exists_p : predicate));

  GCPRO2 (insdef, default_filename);
  
#if defined (USE_MOTIF) || defined (HAVE_NTGUI)
  if ((NILP (last_nonmenu_event) || CONSP (last_nonmenu_event))
      && use_dialog_box
      && have_menus_p ())
    {
      /* If DIR contains a file name, split it.  */
      Lisp_Object file;
      file = Ffile_name_nondirectory (dir);
      if (XSTRING (file)->size && NILP (default_filename))
	{
	  default_filename = file;
	  dir = Ffile_name_directory (dir);
	}
      if (!NILP(default_filename))
        default_filename = Fexpand_file_name (default_filename, dir);
      val = Fx_file_dialog (prompt, dir, default_filename, mustmatch);
      add_to_history = 1;
    }
  else
#endif
    val = Fcompleting_read (prompt, intern ("read-file-name-internal"),
			    dir, mustmatch, insdef,
			    Qfile_name_history, default_filename, Qnil);

  tem = Fsymbol_value (Qfile_name_history);
  if (CONSP (tem) && EQ (XCAR (tem), val))
    replace_in_history = 1;

  /* If Fcompleting_read returned the inserted default string itself
     (rather than a new string with the same contents),
     it has to mean that the user typed RET with the minibuffer empty.
     In that case, we really want to return ""
     so that commands such as set-visited-file-name can distinguish.  */
  if (EQ (val, default_filename))
    {
      /* In this case, Fcompleting_read has not added an element
	 to the history.  Maybe we should.  */
      if (! replace_in_history)
	add_to_history = 1;

      val = build_string ("");
    }

  unbind_to (count, Qnil);
  UNGCPRO;
  if (NILP (val))
    error ("No file name specified");

  tem = Fstring_equal (val, CONSP (insdef) ? XCAR (insdef) : insdef);

  if (!NILP (tem) && !NILP (default_filename))
    val = default_filename;
  else if (XSTRING (val)->size == 0 && NILP (insdef))
    {
      if (!NILP (default_filename))
	val = default_filename;
      else
	error ("No default file name");
    }
  val = Fsubstitute_in_file_name (val);

  if (replace_in_history)
    /* Replace what Fcompleting_read added to the history
       with what we will actually return.  */
    XSETCAR (Fsymbol_value (Qfile_name_history), double_dollars (val));
  else if (add_to_history)
    {
      /* Add the value to the history--but not if it matches
	 the last value already there.  */
      Lisp_Object val1 = double_dollars (val);
      tem = Fsymbol_value (Qfile_name_history);
      if (! CONSP (tem) || NILP (Fequal (XCAR (tem), val1)))
	Fset (Qfile_name_history,
	      Fcons (val1, tem));
    }
    
  return val;
}


void
init_fileio_once ()
{
  /* Must be set before any path manipulation is performed.  */
  XSETFASTINT (Vdirectory_sep_char, '/');
}


void
syms_of_fileio ()
{
  Qexpand_file_name = intern ("expand-file-name");
  Qsubstitute_in_file_name = intern ("substitute-in-file-name");
  Qdirectory_file_name = intern ("directory-file-name");
  Qfile_name_directory = intern ("file-name-directory");
  Qfile_name_nondirectory = intern ("file-name-nondirectory");
  Qunhandled_file_name_directory = intern ("unhandled-file-name-directory");
  Qfile_name_as_directory = intern ("file-name-as-directory");
  Qcopy_file = intern ("copy-file");
  Qmake_directory_internal = intern ("make-directory-internal");
  Qmake_directory = intern ("make-directory");
  Qdelete_directory = intern ("delete-directory");
  Qdelete_file = intern ("delete-file");
  Qrename_file = intern ("rename-file");
  Qadd_name_to_file = intern ("add-name-to-file");
  Qmake_symbolic_link = intern ("make-symbolic-link");
  Qfile_exists_p = intern ("file-exists-p");
  Qfile_executable_p = intern ("file-executable-p");
  Qfile_readable_p = intern ("file-readable-p");
  Qfile_writable_p = intern ("file-writable-p");
  Qfile_symlink_p = intern ("file-symlink-p");
  Qaccess_file = intern ("access-file");
  Qfile_directory_p = intern ("file-directory-p");
  Qfile_regular_p = intern ("file-regular-p");
  Qfile_accessible_directory_p = intern ("file-accessible-directory-p");
  Qfile_modes = intern ("file-modes");
  Qset_file_modes = intern ("set-file-modes");
  Qfile_newer_than_file_p = intern ("file-newer-than-file-p");
  Qinsert_file_contents = intern ("insert-file-contents");
  Qwrite_region = intern ("write-region");
  Qverify_visited_file_modtime = intern ("verify-visited-file-modtime");
  Qset_visited_file_modtime = intern ("set-visited-file-modtime");

  staticpro (&Qexpand_file_name);
  staticpro (&Qsubstitute_in_file_name);
  staticpro (&Qdirectory_file_name);
  staticpro (&Qfile_name_directory);
  staticpro (&Qfile_name_nondirectory);
  staticpro (&Qunhandled_file_name_directory);
  staticpro (&Qfile_name_as_directory);
  staticpro (&Qcopy_file);
  staticpro (&Qmake_directory_internal);
  staticpro (&Qmake_directory);
  staticpro (&Qdelete_directory);
  staticpro (&Qdelete_file);
  staticpro (&Qrename_file);
  staticpro (&Qadd_name_to_file);
  staticpro (&Qmake_symbolic_link);
  staticpro (&Qfile_exists_p);
  staticpro (&Qfile_executable_p);
  staticpro (&Qfile_readable_p);
  staticpro (&Qfile_writable_p);
  staticpro (&Qaccess_file);
  staticpro (&Qfile_symlink_p);
  staticpro (&Qfile_directory_p);
  staticpro (&Qfile_regular_p);
  staticpro (&Qfile_accessible_directory_p);
  staticpro (&Qfile_modes);
  staticpro (&Qset_file_modes);
  staticpro (&Qfile_newer_than_file_p);
  staticpro (&Qinsert_file_contents);
  staticpro (&Qwrite_region);
  staticpro (&Qverify_visited_file_modtime);
  staticpro (&Qset_visited_file_modtime);

  Qfile_name_history = intern ("file-name-history");
  Fset (Qfile_name_history, Qnil);
  staticpro (&Qfile_name_history);

  Qfile_error = intern ("file-error");
  staticpro (&Qfile_error);
  Qfile_already_exists = intern ("file-already-exists");
  staticpro (&Qfile_already_exists);
  Qfile_date_error = intern ("file-date-error");
  staticpro (&Qfile_date_error);
  Qexcl = intern ("excl");
  staticpro (&Qexcl);

#ifdef DOS_NT
  Qfind_buffer_file_type = intern ("find-buffer-file-type");
  staticpro (&Qfind_buffer_file_type);
#endif /* DOS_NT */

  DEFVAR_LISP ("file-name-coding-system", &Vfile_name_coding_system,
	       doc: /* *Coding system for encoding file names.
If it is nil, `default-file-name-coding-system' (which see) is used.  */);
  Vfile_name_coding_system = Qnil;

  DEFVAR_LISP ("default-file-name-coding-system",
	       &Vdefault_file_name_coding_system,
	       doc: /* Default coding system for encoding file names.
This variable is used only when `file-name-coding-system' is nil.

This variable is set/changed by the command `set-language-environment'.
User should not set this variable manually,
instead use `file-name-coding-system' to get a constant encoding
of file names regardless of the current language environment.  */);
  Vdefault_file_name_coding_system = Qnil;

  DEFVAR_LISP ("auto-save-file-format", &Vauto_save_file_format,
    doc: /* *Format in which to write auto-save files.
Should be a list of symbols naming formats that are defined in `format-alist'.
If it is t, which is the default, auto-save files are written in the
same format as a regular save would use.  */);
  Vauto_save_file_format = Qt;

  Qformat_decode = intern ("format-decode");
  staticpro (&Qformat_decode);
  Qformat_annotate_function = intern ("format-annotate-function");
  staticpro (&Qformat_annotate_function);
	
  Qcar_less_than_car = intern ("car-less-than-car");
  staticpro (&Qcar_less_than_car);

  Fput (Qfile_error, Qerror_conditions,
	Fcons (Qfile_error, Fcons (Qerror, Qnil)));
  Fput (Qfile_error, Qerror_message,
	build_string ("File error"));

  Fput (Qfile_already_exists, Qerror_conditions,
	Fcons (Qfile_already_exists,
	       Fcons (Qfile_error, Fcons (Qerror, Qnil))));
  Fput (Qfile_already_exists, Qerror_message,
	build_string ("File already exists"));

  Fput (Qfile_date_error, Qerror_conditions,
	Fcons (Qfile_date_error,
	       Fcons (Qfile_error, Fcons (Qerror, Qnil))));
  Fput (Qfile_date_error, Qerror_message,
	build_string ("Cannot set file date"));

  DEFVAR_LISP ("read-file-name-function", &Vread_file_name_function,
	       doc: /* If this is non-nil, `read-file-name' does its work by calling this function.  */);
  Vread_file_name_function = Qnil;

  DEFVAR_LISP ("read-file-name-predicate", &Vread_file_name_predicate,
	       doc: /* Current predicate used by `read-file-name-internal'.  */);
  Vread_file_name_predicate = Qnil;

  DEFVAR_BOOL ("insert-default-directory", &insert_default_directory,
	       doc: /* *Non-nil means when reading a filename start with default dir in minibuffer.  */);
  insert_default_directory = 1;

  DEFVAR_BOOL ("vms-stmlf-recfm", &vms_stmlf_recfm,
	       doc: /* *Non-nil means write new files with record format `stmlf'.
nil means use format `var'.  This variable is meaningful only on VMS.  */);
  vms_stmlf_recfm = 0;

  DEFVAR_LISP ("directory-sep-char", &Vdirectory_sep_char,
	       doc: /* Directory separator character for built-in functions that return file names.
The value should be either ?/ or ?\\ (any other value is treated as ?\\).
This variable affects the built-in functions only on Windows,
on other platforms, it is initialized so that Lisp code can find out
what the normal separator is.

WARNING: This variable is deprecated and will be removed in the near
future.  DO NOT USE IT.  */);

  DEFVAR_LISP ("file-name-handler-alist", &Vfile_name_handler_alist,
	       doc: /* *Alist of elements (REGEXP . HANDLER) for file names handled specially.
If a file name matches REGEXP, then all I/O on that file is done by calling
HANDLER.

The first argument given to HANDLER is the name of the I/O primitive
to be handled; the remaining arguments are the arguments that were
passed to that primitive.  For example, if you do
    (file-exists-p FILENAME)
and FILENAME is handled by HANDLER, then HANDLER is called like this:
    (funcall HANDLER 'file-exists-p FILENAME)
The function `find-file-name-handler' checks this list for a handler
for its argument.  */);
  Vfile_name_handler_alist = Qnil;

  DEFVAR_LISP ("set-auto-coding-function",
	       &Vset_auto_coding_function,
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

  DEFVAR_LISP ("after-insert-file-functions", &Vafter_insert_file_functions,
	       doc: /* A list of functions to be called at the end of `insert-file-contents'.
Each is passed one argument, the number of bytes inserted.  It should return
the new byte count, and leave point the same.  If `insert-file-contents' is
intercepted by a handler from `file-name-handler-alist', that handler is
responsible for calling the after-insert-file-functions if appropriate.  */);
  Vafter_insert_file_functions = Qnil;

  DEFVAR_LISP ("write-region-annotate-functions", &Vwrite_region_annotate_functions,
	       doc: /* A list of functions to be called at the start of `write-region'.
Each is passed two arguments, START and END as for `write-region'.
These are usually two numbers but not always; see the documentation
for `write-region'.  The function should return a list of pairs
of the form (POSITION . STRING), consisting of strings to be effectively
inserted at the specified positions of the file being written (1 means to
insert before the first byte written).  The POSITIONs must be sorted into
increasing order.  If there are several functions in the list, the several
lists are merged destructively.  Alternatively, the function can return
with a different buffer current and value nil.*/);
  Vwrite_region_annotate_functions = Qnil;

  DEFVAR_LISP ("write-region-annotations-so-far",
	       &Vwrite_region_annotations_so_far,
	       doc: /* When an annotation function is called, this holds the previous annotations.
These are the annotations made by other annotation functions
that were already called.  See also `write-region-annotate-functions'.  */);
  Vwrite_region_annotations_so_far = Qnil;

  DEFVAR_LISP ("inhibit-file-name-handlers", &Vinhibit_file_name_handlers,
	       doc: /* A list of file name handlers that temporarily should not be used.
This applies only to the operation `inhibit-file-name-operation'.  */);
  Vinhibit_file_name_handlers = Qnil;

  DEFVAR_LISP ("inhibit-file-name-operation", &Vinhibit_file_name_operation,
	       doc: /* The operation for which `inhibit-file-name-handlers' is applicable.  */);
  Vinhibit_file_name_operation = Qnil;

  DEFVAR_LISP ("auto-save-list-file-name", &Vauto_save_list_file_name,
	       doc: /* File name in which we write a list of all auto save file names.
This variable is initialized automatically from `auto-save-list-file-prefix'
shortly after Emacs reads your `.emacs' file, if you have not yet given it
a non-nil value.  */);
  Vauto_save_list_file_name = Qnil;

  defsubr (&Sfind_file_name_handler);
  defsubr (&Sfile_name_directory);
  defsubr (&Sfile_name_nondirectory);
  defsubr (&Sunhandled_file_name_directory);
  defsubr (&Sfile_name_as_directory);
  defsubr (&Sdirectory_file_name);
  defsubr (&Smake_temp_name);
  defsubr (&Sexpand_file_name);
  defsubr (&Ssubstitute_in_file_name);
  defsubr (&Scopy_file);
  defsubr (&Smake_directory_internal);
  defsubr (&Sdelete_directory);
  defsubr (&Sdelete_file);
  defsubr (&Srename_file);
  defsubr (&Sadd_name_to_file);
#ifdef S_IFLNK
  defsubr (&Smake_symbolic_link);
#endif /* S_IFLNK */
#ifdef VMS
  defsubr (&Sdefine_logical_name);
#endif /* VMS */
#ifdef HPUX_NET
  defsubr (&Ssysnetunam);
#endif /* HPUX_NET */
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
  defsubr (&Sset_default_file_modes);
  defsubr (&Sdefault_file_modes);
  defsubr (&Sfile_newer_than_file_p);
  defsubr (&Sinsert_file_contents);
  defsubr (&Swrite_region);
  defsubr (&Scar_less_than_car);
  defsubr (&Sverify_visited_file_modtime);
  defsubr (&Sclear_visited_file_modtime);
  defsubr (&Svisited_file_modtime);
  defsubr (&Sset_visited_file_modtime);
  defsubr (&Sdo_auto_save);
  defsubr (&Sset_buffer_auto_saved);
  defsubr (&Sclear_buffer_auto_save_failure);
  defsubr (&Srecent_auto_save_p);

  defsubr (&Sread_file_name_internal);
  defsubr (&Sread_file_name);

#ifdef unix
  defsubr (&Sunix_sync);
#endif
}

