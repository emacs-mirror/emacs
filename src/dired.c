/* Lisp functions for making directory listings.
   Copyright (C) 1985, 1986, 1993, 1994, 1999, 2000, 2001, 2002, 2003,
                 2004, 2005, 2006, 2007, 2008 Free Software Foundation, Inc.

This file is part of GNU Emacs.

GNU Emacs is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.  */


#include <config.h>

#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>

#ifdef HAVE_PWD_H
#include <pwd.h>
#endif
#ifndef VMS
#include <grp.h>
#endif

#include <errno.h>

#ifdef VMS
#include <string.h>
#include <rms.h>
#include <rmsdef.h>
#endif

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

/* The d_nameln member of a struct dirent includes the '\0' character
   on some systems, but not on others.  What's worse, you can't tell
   at compile-time which one it will be, since it really depends on
   the sort of system providing the filesystem you're reading from,
   not the system you are running on.  Paul Eggert
   <eggert@bi.twinsun.com> says this occurs when Emacs is running on a
   SunOS 4.1.2 host, reading a directory that is remote-mounted from a
   Solaris 2.1 host and is in a native Solaris 2.1 filesystem.

   Since applying strlen to the name always works, we'll just do that.  */
#define NAMLEN(p) strlen (p->d_name)

#ifdef SYSV_SYSTEM_DIR

#include <dirent.h>
#define DIRENTRY struct dirent

#else /* not SYSV_SYSTEM_DIR */

#ifdef NONSYSTEM_DIR_LIBRARY
#include "ndir.h"
#else /* not NONSYSTEM_DIR_LIBRARY */
#ifdef MSDOS
#include <dirent.h>
#else
#include <sys/dir.h>
#endif
#endif /* not NONSYSTEM_DIR_LIBRARY */

#include <sys/stat.h>

#ifndef MSDOS
#define DIRENTRY struct direct

extern DIR *opendir ();
extern struct direct *readdir ();

#endif /* not MSDOS */
#endif /* not SYSV_SYSTEM_DIR */

/* Some versions of Cygwin don't have d_ino in `struct dirent'.  */
#if defined(MSDOS) || defined(__CYGWIN__)
#define DIRENTRY_NONEMPTY(p) ((p)->d_name[0] != 0)
#else
#define DIRENTRY_NONEMPTY(p) ((p)->d_ino)
#endif

#include "lisp.h"
#include "systime.h"
#include "buffer.h"
#include "commands.h"
#include "character.h"
#include "charset.h"
#include "coding.h"
#include "regex.h"
#include "blockinput.h"

/* Returns a search buffer, with a fastmap allocated and ready to go.  */
extern struct re_pattern_buffer *compile_pattern ();

/* From filemode.c.  Can't go in Lisp.h because of `stat'.  */
extern void filemodestring P_ ((struct stat *, char *));

/* if system does not have symbolic links, it does not have lstat.
   In that case, use ordinary stat instead.  */

#ifndef S_IFLNK
#define lstat stat
#endif

extern int completion_ignore_case;
extern Lisp_Object Qcompletion_ignore_case;
extern Lisp_Object Vcompletion_regexp_list;

Lisp_Object Vcompletion_ignored_extensions;
Lisp_Object Qdirectory_files;
Lisp_Object Qdirectory_files_and_attributes;
Lisp_Object Qfile_name_completion;
Lisp_Object Qfile_name_all_completions;
Lisp_Object Qfile_attributes;
Lisp_Object Qfile_attributes_lessp;

static int scmp P_ ((unsigned char *, unsigned char *, int));


Lisp_Object
directory_files_internal_unwind (dh)
     Lisp_Object dh;
{
  DIR *d = (DIR *) XSAVE_VALUE (dh)->pointer;
  BLOCK_INPUT;
  closedir (d);
  UNBLOCK_INPUT;
  return Qnil;
}

/* Function shared by Fdirectory_files and Fdirectory_files_and_attributes.
   When ATTRS is zero, return a list of directory filenames; when
   non-zero, return a list of directory filenames and their attributes.
   In the latter case, ID_FORMAT is passed to Ffile_attributes.  */

Lisp_Object
directory_files_internal (directory, full, match, nosort, attrs, id_format)
     Lisp_Object directory, full, match, nosort;
     int attrs;
     Lisp_Object id_format;
{
  DIR *d;
  int directory_nbytes;
  Lisp_Object list, dirfilename, encoded_directory;
  struct re_pattern_buffer *bufp = NULL;
  int needsep = 0;
  int count = SPECPDL_INDEX ();
  struct gcpro gcpro1, gcpro2, gcpro3, gcpro4, gcpro5;
  DIRENTRY *dp;

  /* Because of file name handlers, these functions might call
     Ffuncall, and cause a GC.  */
  list = encoded_directory = dirfilename = Qnil;
  GCPRO5 (match, directory, list, dirfilename, encoded_directory);
  dirfilename = Fdirectory_file_name (directory);

  if (!NILP (match))
    {
      CHECK_STRING (match);

      /* MATCH might be a flawed regular expression.  Rather than
	 catching and signaling our own errors, we just call
	 compile_pattern to do the work for us.  */
      /* Pass 1 for the MULTIBYTE arg
	 because we do make multibyte strings if the contents warrant.  */
#ifdef VMS
      bufp = compile_pattern (match, 0,
			      buffer_defaults.downcase_table, 0, 1);
#else  /* !VMS */
# ifdef WINDOWSNT
      /* Windows users want case-insensitive wildcards.  */
      bufp = compile_pattern (match, 0,
			      buffer_defaults.case_canon_table, 0, 1);
# else	/* !WINDOWSNT */
      bufp = compile_pattern (match, 0, Qnil, 0, 1);
# endif	 /* !WINDOWSNT */
#endif	 /* !VMS */
    }

  /* Note: ENCODE_FILE and DECODE_FILE can GC because they can run
     run_pre_post_conversion_on_str which calls Lisp directly and
     indirectly.  */
  if (STRING_MULTIBYTE (dirfilename))
    dirfilename = ENCODE_FILE (dirfilename);
  encoded_directory = (STRING_MULTIBYTE (directory)
		       ? ENCODE_FILE (directory) : directory);

  /* Now *bufp is the compiled form of MATCH; don't call anything
     which might compile a new regexp until we're done with the loop!  */

  BLOCK_INPUT;
  d = opendir (SDATA (dirfilename));
  UNBLOCK_INPUT;
  if (d == NULL)
    report_file_error ("Opening directory", Fcons (directory, Qnil));

  /* Unfortunately, we can now invoke expand-file-name and
     file-attributes on filenames, both of which can throw, so we must
     do a proper unwind-protect.  */
  record_unwind_protect (directory_files_internal_unwind,
			 make_save_value (d, 0));

  directory_nbytes = SBYTES (directory);
  re_match_object = Qt;

  /* Decide whether we need to add a directory separator.  */
#ifndef VMS
  if (directory_nbytes == 0
      || !IS_ANY_SEP (SREF (directory, directory_nbytes - 1)))
    needsep = 1;
#endif /* not VMS */

  /* Loop reading blocks until EOF or error.  */
  for (;;)
    {
      errno = 0;
      dp = readdir (d);

      if (dp == NULL && (0
#ifdef EAGAIN
			 || errno == EAGAIN
#endif
#ifdef EINTR
			 || errno == EINTR
#endif
			 ))
	{ QUIT; continue; }

      if (dp == NULL)
	break;

      if (DIRENTRY_NONEMPTY (dp))
	{
	  int len;
	  int wanted = 0;
	  Lisp_Object name, finalname;
	  struct gcpro gcpro1, gcpro2;

	  len = NAMLEN (dp);
	  name = finalname = make_unibyte_string (dp->d_name, len);
	  GCPRO2 (finalname, name);

	  /* Note: DECODE_FILE can GC; it should protect its argument,
	     though.  */
	  name = DECODE_FILE (name);
	  len = SBYTES (name);

	  /* Now that we have unwind_protect in place, we might as well
             allow matching to be interrupted.  */
	  immediate_quit = 1;
	  QUIT;

	  if (NILP (match)
	      || (0 <= re_search (bufp, SDATA (name), len, 0, len, 0)))
	    wanted = 1;

	  immediate_quit = 0;

	  if (wanted)
	    {
	      if (!NILP (full))
		{
		  Lisp_Object fullname;
		  int nbytes = len + directory_nbytes + needsep;
		  int nchars;

		  fullname = make_uninit_multibyte_string (nbytes, nbytes);
		  bcopy (SDATA (directory), SDATA (fullname),
			 directory_nbytes);

		  if (needsep)
		    SSET (fullname, directory_nbytes, DIRECTORY_SEP);

		  bcopy (SDATA (name),
			 SDATA (fullname) + directory_nbytes + needsep,
			 len);

		  nchars = chars_in_text (SDATA (fullname), nbytes);

		  /* Some bug somewhere.  */
		  if (nchars > nbytes)
		    abort ();

		  STRING_SET_CHARS (fullname, nchars);
		  if (nchars == nbytes)
		    STRING_SET_UNIBYTE (fullname);

		  finalname = fullname;
		}
	      else
		finalname = name;

	      if (attrs)
		{
		  /* Construct an expanded filename for the directory entry.
		     Use the decoded names for input to Ffile_attributes.  */
		  Lisp_Object decoded_fullname, fileattrs;
		  struct gcpro gcpro1, gcpro2;

		  decoded_fullname = fileattrs = Qnil;
		  GCPRO2 (decoded_fullname, fileattrs);

		  /* Both Fexpand_file_name and Ffile_attributes can GC.  */
		  decoded_fullname = Fexpand_file_name (name, directory);
		  fileattrs = Ffile_attributes (decoded_fullname, id_format);

		  list = Fcons (Fcons (finalname, fileattrs), list);
		  UNGCPRO;
		}
	      else
		list = Fcons (finalname, list);
	    }

	  UNGCPRO;
	}
    }

  BLOCK_INPUT;
  closedir (d);
  UNBLOCK_INPUT;

  /* Discard the unwind protect.  */
  specpdl_ptr = specpdl + count;

  if (NILP (nosort))
    list = Fsort (Fnreverse (list),
		  attrs ? Qfile_attributes_lessp : Qstring_lessp);

  RETURN_UNGCPRO (list);
}


DEFUN ("directory-files", Fdirectory_files, Sdirectory_files, 1, 4, 0,
       doc: /* Return a list of names of files in DIRECTORY.
There are three optional arguments:
If FULL is non-nil, return absolute file names.  Otherwise return names
 that are relative to the specified directory.
If MATCH is non-nil, mention only file names that match the regexp MATCH.
If NOSORT is non-nil, the list is not sorted--its order is unpredictable.
 NOSORT is useful if you plan to sort the result yourself.  */)
     (directory, full, match, nosort)
     Lisp_Object directory, full, match, nosort;
{
  Lisp_Object handler;
  directory = Fexpand_file_name (directory, Qnil);

  /* If the file name has special constructs in it,
     call the corresponding file handler.  */
  handler = Ffind_file_name_handler (directory, Qdirectory_files);
  if (!NILP (handler))
    return call5 (handler, Qdirectory_files, directory,
                  full, match, nosort);

  return directory_files_internal (directory, full, match, nosort, 0, Qnil);
}

DEFUN ("directory-files-and-attributes", Fdirectory_files_and_attributes,
       Sdirectory_files_and_attributes, 1, 5, 0,
       doc: /* Return a list of names of files and their attributes in DIRECTORY.
There are four optional arguments:
If FULL is non-nil, return absolute file names.  Otherwise return names
 that are relative to the specified directory.
If MATCH is non-nil, mention only file names that match the regexp MATCH.
If NOSORT is non-nil, the list is not sorted--its order is unpredictable.
 NOSORT is useful if you plan to sort the result yourself.
ID-FORMAT specifies the preferred format of attributes uid and gid, see
`file-attributes' for further documentation.
On MS-Windows, performance depends on `w32-get-true-file-attributes',
which see.  */)
     (directory, full, match, nosort, id_format)
     Lisp_Object directory, full, match, nosort, id_format;
{
  Lisp_Object handler;
  directory = Fexpand_file_name (directory, Qnil);

  /* If the file name has special constructs in it,
     call the corresponding file handler.  */
  handler = Ffind_file_name_handler (directory, Qdirectory_files_and_attributes);
  if (!NILP (handler))
    return call6 (handler, Qdirectory_files_and_attributes,
                  directory, full, match, nosort, id_format);

  return directory_files_internal (directory, full, match, nosort, 1, id_format);
}


Lisp_Object file_name_completion ();

DEFUN ("file-name-completion", Ffile_name_completion, Sfile_name_completion,
       2, 3, 0,
       doc: /* Complete file name FILE in directory DIRECTORY.
Returns the longest string
common to all file names in DIRECTORY that start with FILE.
If there is only one and FILE matches it exactly, returns t.
Returns nil if DIRECTORY contains no name starting with FILE.

If PREDICATE is non-nil, call PREDICATE with each possible
completion (in absolute form) and ignore it if PREDICATE returns nil.

This function ignores some of the possible completions as
determined by the variable `completion-ignored-extensions', which see.  */)
     (file, directory, predicate)
     Lisp_Object file, directory, predicate;
{
  Lisp_Object handler;

  /* If the directory name has special constructs in it,
     call the corresponding file handler.  */
  handler = Ffind_file_name_handler (directory, Qfile_name_completion);
  if (!NILP (handler))
    return call4 (handler, Qfile_name_completion, file, directory, predicate);

  /* If the file name has special constructs in it,
     call the corresponding file handler.  */
  handler = Ffind_file_name_handler (file, Qfile_name_completion);
  if (!NILP (handler))
    return call4 (handler, Qfile_name_completion, file, directory, predicate);

  return file_name_completion (file, directory, 0, 0, predicate);
}

DEFUN ("file-name-all-completions", Ffile_name_all_completions,
       Sfile_name_all_completions, 2, 2, 0,
       doc: /* Return a list of all completions of file name FILE in directory DIRECTORY.
These are all file names in directory DIRECTORY which begin with FILE.  */)
     (file, directory)
     Lisp_Object file, directory;
{
  Lisp_Object handler;

  /* If the directory name has special constructs in it,
     call the corresponding file handler.  */
  handler = Ffind_file_name_handler (directory, Qfile_name_all_completions);
  if (!NILP (handler))
    return call3 (handler, Qfile_name_all_completions, file, directory);

  /* If the file name has special constructs in it,
     call the corresponding file handler.  */
  handler = Ffind_file_name_handler (file, Qfile_name_all_completions);
  if (!NILP (handler))
    return call3 (handler, Qfile_name_all_completions, file, directory);

  return file_name_completion (file, directory, 1, 0, Qnil);
}

static int file_name_completion_stat ();
Lisp_Object Qdefault_directory;

Lisp_Object
file_name_completion (file, dirname, all_flag, ver_flag, predicate)
     Lisp_Object file, dirname;
     int all_flag, ver_flag;
     Lisp_Object predicate;
{
  DIR *d;
  int bestmatchsize = 0;
  int matchcount = 0;
  /* If ALL_FLAG is 1, BESTMATCH is the list of all matches, decoded.
     If ALL_FLAG is 0, BESTMATCH is either nil
     or the best match so far, not decoded.  */
  Lisp_Object bestmatch, tem, elt, name;
  Lisp_Object encoded_file;
  Lisp_Object encoded_dir;
  struct stat st;
  int directoryp;
  /* If includeall is zero, exclude files in completion-ignored-extensions as
     well as "." and "..".  Until shown otherwise, assume we can't exclude
     anything.  */
  int includeall = 1;
  int count = SPECPDL_INDEX ();
  struct gcpro gcpro1, gcpro2, gcpro3, gcpro4, gcpro5;

  elt = Qnil;

#ifdef VMS
  extern DIRENTRY * readdirver ();

  DIRENTRY *((* readfunc) ());

  /* Filename completion on VMS ignores case, since VMS filesys does.  */
  specbind (Qcompletion_ignore_case, Qt);

  readfunc = readdir;
  if (ver_flag)
    readfunc = readdirver;
  file = Fupcase (file);
#else  /* not VMS */
  CHECK_STRING (file);
#endif /* not VMS */

#ifdef FILE_SYSTEM_CASE
  file = FILE_SYSTEM_CASE (file);
#endif
  bestmatch = Qnil;
  encoded_file = encoded_dir = Qnil;
  GCPRO5 (file, dirname, bestmatch, encoded_file, encoded_dir);
  dirname = Fexpand_file_name (dirname, Qnil);
  specbind (Qdefault_directory, dirname);

  /* Do completion on the encoded file name
     because the other names in the directory are (we presume)
     encoded likewise.  We decode the completed string at the end.  */
  /* Actually, this is not quite true any more: we do most of the completion
     work with decoded file names, but we still do some filtering based
     on the encoded file name.  */
  encoded_file = STRING_MULTIBYTE (file) ? ENCODE_FILE (file) : file;

  encoded_dir = ENCODE_FILE (dirname);

  BLOCK_INPUT;
  d = opendir (SDATA (Fdirectory_file_name (encoded_dir)));
  UNBLOCK_INPUT;
  if (!d)
    report_file_error ("Opening directory", Fcons (dirname, Qnil));

  record_unwind_protect (directory_files_internal_unwind,
			 make_save_value (d, 0));

  /* Loop reading blocks */
  /* (att3b compiler bug requires do a null comparison this way) */
  while (1)
    {
      DIRENTRY *dp;
      int len;
      int canexclude = 0;

#ifdef VMS
      dp = (*readfunc) (d);
#else
      errno = 0;
      dp = readdir (d);
      if (dp == NULL && (0
# ifdef EAGAIN
			 || errno == EAGAIN
# endif
# ifdef EINTR
			 || errno == EINTR
# endif
			 ))
	{ QUIT; continue; }
#endif

      if (!dp) break;

      len = NAMLEN (dp);

      QUIT;
      if (! DIRENTRY_NONEMPTY (dp)
	  || len < SCHARS (encoded_file)
	  || 0 <= scmp (dp->d_name, SDATA (encoded_file),
			SCHARS (encoded_file)))
	continue;

      if (file_name_completion_stat (encoded_dir, dp, &st) < 0)
	continue;

      directoryp = ((st.st_mode & S_IFMT) == S_IFDIR);
      tem = Qnil;
      /* If all_flag is set, always include all.
	 It would not actually be helpful to the user to ignore any possible
	 completions when making a list of them.  */
      if (!all_flag)
	{
	  int skip;
	  if (directoryp)
	    {
#ifndef TRIVIAL_DIRECTORY_ENTRY
#define TRIVIAL_DIRECTORY_ENTRY(n) (!strcmp (n, ".") || !strcmp (n, ".."))
#endif
	      /* "." and ".." are never interesting as completions, and are
		 actually in the way in a directory with only one file.  */
	      if (TRIVIAL_DIRECTORY_ENTRY (dp->d_name))
		canexclude = 1;
	      else if (len > SCHARS (encoded_file))
		/* Ignore directories if they match an element of
		   completion-ignored-extensions which ends in a slash.  */
		for (tem = Vcompletion_ignored_extensions;
		     CONSP (tem); tem = XCDR (tem))
		  {
		    int elt_len;
		    unsigned char *p1;

		    elt = XCAR (tem);
		    if (!STRINGP (elt))
		      continue;
		    /* Need to encode ELT, since scmp compares unibyte
		       strings only.  */
		    elt = ENCODE_FILE (elt);
		    elt_len = SCHARS (elt) - 1; /* -1 for trailing / */
		    if (elt_len <= 0)
		      continue;
		    p1 = SDATA (elt);
		    if (p1[elt_len] != '/')
		      continue;
		    skip = len - elt_len;
		    if (skip < 0)
		      continue;

		    if (0 <= scmp (dp->d_name + skip, p1, elt_len))
		      continue;
		    break;
		  }
	    }
	  else
	    {
	      /* Compare extensions-to-be-ignored against end of this file name */
	      /* if name is not an exact match against specified string */
	      if (len > SCHARS (encoded_file))
		/* and exit this for loop if a match is found */
		for (tem = Vcompletion_ignored_extensions;
		     CONSP (tem); tem = XCDR (tem))
		  {
		    elt = XCAR (tem);
		    if (!STRINGP (elt)) continue;
		    /* Need to encode ELT, since scmp compares unibyte
		       strings only.  */
		    elt = ENCODE_FILE (elt);
		    skip = len - SCHARS (elt);
		    if (skip < 0) continue;

		    if (0 <= scmp (dp->d_name + skip,
				   SDATA (elt),
				   SCHARS (elt)))
		      continue;
		    break;
		  }
	    }

	  /* If an ignored-extensions match was found,
	     don't process this name as a completion.  */
	  if (CONSP (tem))
	    canexclude = 1;

	  if (!includeall && canexclude)
	    /* We're not including all files and this file can be excluded.  */
	    continue;

	  if (includeall && !canexclude)
	    { /* If we have one non-excludable file, we want to exclude the
		 excudable files.  */
	      includeall = 0;
	      /* Throw away any previous excludable match found.  */
	      bestmatch = Qnil;
	      bestmatchsize = 0;
	      matchcount = 0;
	    }
	}
      /* FIXME: If we move this `decode' earlier we can eliminate
	 the repeated ENCODE_FILE on Vcompletion_ignored_extensions.  */
      name = make_unibyte_string (dp->d_name, len);
      name = DECODE_FILE (name);

      {
	Lisp_Object regexps;
	Lisp_Object zero;
	XSETFASTINT (zero, 0);

	/* Ignore this element if it fails to match all the regexps.  */
	for (regexps = Vcompletion_regexp_list; CONSP (regexps);
	     regexps = XCDR (regexps))
	  if (fast_string_match (XCAR (regexps), name) < 0)
	    break;
	if (CONSP (regexps))
	  continue;
      }

      /* This is a possible completion */
      if (directoryp)
	/* This completion is a directory; make it end with '/'.  */
	name = Ffile_name_as_directory (name);

      /* Test the predicate, if any.  */
      if (!NILP (predicate))
	{
	  Lisp_Object val;
	  struct gcpro gcpro1;

	  GCPRO1 (name);
	  val = call1 (predicate, name);
	  UNGCPRO;

	  if (NILP (val))
	    continue;
	}

      /* Suitably record this match.  */

      matchcount++;

      if (all_flag)
	bestmatch = Fcons (name, bestmatch);
      else if (NILP (bestmatch))
	{
	  bestmatch = name;
	  bestmatchsize = SCHARS (name);
	}
      else
	{
	  Lisp_Object zero = make_number (0);
	  /* FIXME: This is a copy of the code in Ftry_completion.  */
	  int compare = min (bestmatchsize, SCHARS (name));
	  Lisp_Object tem
	    = Fcompare_strings (bestmatch, zero,
				make_number (compare),
				name, zero,
				make_number (compare),
				completion_ignore_case ? Qt : Qnil);
	  int matchsize
	    = (EQ (tem, Qt)     ? compare
	       : XINT (tem) < 0 ? - XINT (tem) - 1
	       :                  XINT (tem) - 1);

	  if (completion_ignore_case)
	    {
	      /* If this is an exact match except for case,
		 use it as the best match rather than one that is not
		 an exact match.  This way, we get the case pattern
		 of the actual match.  */
	      /* This tests that the current file is an exact match
		 but BESTMATCH is not (it is too long).  */
	      if ((matchsize == SCHARS (name)
		   && matchsize + !!directoryp
		   < SCHARS (bestmatch))
		  ||
		  /* If there is no exact match ignoring case,
		     prefer a match that does not change the case
		     of the input.  */
		  /* If there is more than one exact match aside from
		     case, and one of them is exact including case,
		     prefer that one.  */
		  /* This == checks that, of current file and BESTMATCH,
		     either both or neither are exact.  */
		  (((matchsize == SCHARS (name))
		    ==
		    (matchsize + !!directoryp == SCHARS (bestmatch)))
		   && (tem = Fcompare_strings (name, zero,
					       make_number (SCHARS (file)),
					       file, zero,
					       Qnil,
					       Qnil),
		       EQ (Qt, tem))
		   && (tem = Fcompare_strings (bestmatch, zero,
					       make_number (SCHARS (file)),
					       file, zero,
					       Qnil,
					       Qnil),
		       ! EQ (Qt, tem))))
		bestmatch = name;
	    }
	  bestmatchsize = matchsize;
	}
    }

  UNGCPRO;
  /* This closes the directory.  */
  bestmatch = unbind_to (count, bestmatch);

  if (all_flag || NILP (bestmatch))
    return bestmatch;
  if (matchcount == 1 && bestmatchsize == SCHARS (file))
    return Qt;
  bestmatch = Fsubstring (bestmatch, make_number (0),
			  make_number (bestmatchsize));
  return bestmatch;
}

/* Compare exactly LEN chars of strings at S1 and S2,
   ignoring case if appropriate.
   Return -1 if strings match,
   else number of chars that match at the beginning.  */

static int
scmp (s1, s2, len)
     register unsigned char *s1, *s2;
     int len;
{
  register int l = len;

  if (completion_ignore_case)
    {
      while (l && DOWNCASE (*s1++) == DOWNCASE (*s2++))
	l--;
    }
  else
    {
      while (l && *s1++ == *s2++)
	l--;
    }
  if (l == 0)
    return -1;
  else
    return len - l;
}

static int
file_name_completion_stat (dirname, dp, st_addr)
     Lisp_Object dirname;
     DIRENTRY *dp;
     struct stat *st_addr;
{
  int len = NAMLEN (dp);
  int pos = SCHARS (dirname);
  int value;
  char *fullname = (char *) alloca (len + pos + 2);

#ifdef MSDOS
#if __DJGPP__ > 1
  /* Some fields of struct stat are *very* expensive to compute on MS-DOS,
     but aren't required here.  Avoid computing the following fields:
     st_inode, st_size and st_nlink for directories, and the execute bits
     in st_mode for non-directory files with non-standard extensions.  */

  unsigned short save_djstat_flags = _djstat_flags;

  _djstat_flags = _STAT_INODE | _STAT_EXEC_MAGIC | _STAT_DIRSIZE;
#endif /* __DJGPP__ > 1 */
#endif /* MSDOS */

  bcopy (SDATA (dirname), fullname, pos);
#ifndef VMS
  if (!IS_DIRECTORY_SEP (fullname[pos - 1]))
    fullname[pos++] = DIRECTORY_SEP;
#endif

  bcopy (dp->d_name, fullname + pos, len);
  fullname[pos + len] = 0;

#ifdef S_IFLNK
  /* We want to return success if a link points to a nonexistent file,
     but we want to return the status for what the link points to,
     in case it is a directory.  */
  value = lstat (fullname, st_addr);
  stat (fullname, st_addr);
  return value;
#else
  value = stat (fullname, st_addr);
#ifdef MSDOS
#if __DJGPP__ > 1
  _djstat_flags = save_djstat_flags;
#endif /* __DJGPP__ > 1 */
#endif /* MSDOS */
  return value;
#endif /* S_IFLNK */
}

#ifdef VMS

DEFUN ("file-name-all-versions", Ffile_name_all_versions,
       Sfile_name_all_versions, 2, 2, 0,
       doc: /* Return a list of all versions of file name FILE in directory DIRECTORY.  */)
     (file, directory)
     Lisp_Object file, directory;
{
  return file_name_completion (file, directory, 1, 1, Qnil);
}

DEFUN ("file-version-limit", Ffile_version_limit, Sfile_version_limit, 1, 1, 0,
       doc: /* Return the maximum number of versions allowed for FILE.
Returns nil if the file cannot be opened or if there is no version limit.  */)
     (filename)
     Lisp_Object filename;
{
  Lisp_Object retval;
  struct FAB    fab;
  struct RAB    rab;
  struct XABFHC xabfhc;
  int status;

  filename = Fexpand_file_name (filename, Qnil);
  fab      = cc$rms_fab;
  xabfhc   = cc$rms_xabfhc;
  fab.fab$l_fna = SDATA (filename);
  fab.fab$b_fns = strlen (fab.fab$l_fna);
  fab.fab$l_xab = (char *) &xabfhc;
  status = sys$open (&fab, 0, 0);
  if (status != RMS$_NORMAL)	/* Probably non-existent file */
    return Qnil;
  sys$close (&fab, 0, 0);
  if (xabfhc.xab$w_verlimit == 32767)
    return Qnil;		/* No version limit */
  else
    return make_number (xabfhc.xab$w_verlimit);
}

#endif /* VMS */

Lisp_Object
make_time (time)
     time_t time;
{
  return Fcons (make_number (time >> 16),
		Fcons (make_number (time & 0177777), Qnil));
}

static char *
stat_uname (struct stat *st)
{
#ifdef WINDOWSNT
  return st->st_uname;
#else
  struct passwd *pw = (struct passwd *) getpwuid (st->st_uid);

  if (pw)
    return pw->pw_name;
  else
    return NULL;
#endif
}

static char *
stat_gname (struct stat *st)
{
#ifdef WINDOWSNT
  return st->st_gname;
#else
  struct group *gr = (struct group *) getgrgid (st->st_gid);

  if (gr)
    return gr->gr_name;
  else
    return NULL;
#endif
}

DEFUN ("file-attributes", Ffile_attributes, Sfile_attributes, 1, 2, 0,
       doc: /* Return a list of attributes of file FILENAME.
Value is nil if specified file cannot be opened.

ID-FORMAT specifies the preferred format of attributes uid and gid (see
below) - valid values are 'string and 'integer. The latter is the default,
but we plan to change that, so you should specify a non-nil value for
ID-FORMAT if you use the returned uid or gid.

Elements of the attribute list are:
 0. t for directory, string (name linked to) for symbolic link, or nil.
 1. Number of links to file.
 2. File uid as a string or an integer.  If a string value cannot be
  looked up, the integer value is returned.
 3. File gid, likewise.
 4. Last access time, as a list of two integers.
  First integer has high-order 16 bits of time, second has low 16 bits.
 5. Last modification time, likewise.
 6. Last status change time, likewise.
 7. Size in bytes.
  This is a floating point number if the size is too large for an integer.
 8. File modes, as a string of ten letters or dashes as in ls -l.
 9. t if file's gid would change if file were deleted and recreated.
10. inode number.  If inode number is larger than the Emacs integer,
  but still fits into a 32-bit number, this is a cons cell containing two
  integers: first the high part, then the low 16 bits.  If the inode number
  is wider than 32 bits, this is a cons cell containing three integers:
  first the high 24 bits, then middle 24 bits, and finally the low 16 bits.
11. Device number.  If it is larger than the Emacs integer, this is
  a cons cell, similar to the inode number.

On MS-Windows, performance depends on `w32-get-true-file-attributes',
which see.  */)
     (filename, id_format)
     Lisp_Object filename, id_format;
{
  Lisp_Object values[12];
  Lisp_Object encoded;
  struct stat s;
#if defined (BSD4_2) || defined (BSD4_3)
  Lisp_Object dirname;
  struct stat sdir;
#endif
  char modes[10];
  Lisp_Object handler;
  struct gcpro gcpro1;
  EMACS_INT ino, uid, gid;
  char *uname, *gname;

  filename = Fexpand_file_name (filename, Qnil);

  /* If the file name has special constructs in it,
     call the corresponding file handler.  */
  handler = Ffind_file_name_handler (filename, Qfile_attributes);
  if (!NILP (handler))
    { /* Only pass the extra arg if it is used to help backward compatibility
	 with old file handlers which do not implement the new arg.  --Stef  */
      if (NILP (id_format))
	return call2 (handler, Qfile_attributes, filename);
      else
	return call3 (handler, Qfile_attributes, filename, id_format);
    }

  GCPRO1 (filename);
  encoded = ENCODE_FILE (filename);
  UNGCPRO;

  if (lstat (SDATA (encoded), &s) < 0)
    return Qnil;

  switch (s.st_mode & S_IFMT)
    {
    default:
      values[0] = Qnil; break;
    case S_IFDIR:
      values[0] = Qt; break;
#ifdef S_IFLNK
    case S_IFLNK:
      values[0] = Ffile_symlink_p (filename); break;
#endif
    }
  values[1] = make_number (s.st_nlink);
  uid = s.st_uid;
  gid = s.st_gid;
  if (NILP (id_format) || EQ (id_format, Qinteger))
    {
      values[2] = make_fixnum_or_float (uid);
      values[3] = make_fixnum_or_float (gid);
    }
  else
    {
      BLOCK_INPUT;
      uname = stat_uname (&s);
      values[2] = (uname ? build_string (uname)
		   : make_fixnum_or_float (uid));
      gname = stat_gname (&s);
      values[3] = (gname ? build_string (gname)
		   : make_fixnum_or_float (gid));
      UNBLOCK_INPUT;
    }
  values[4] = make_time (s.st_atime);
  values[5] = make_time (s.st_mtime);
  values[6] = make_time (s.st_ctime);
  values[7] = make_number (s.st_size);
  /* If the size is out of range for an integer, return a float.  */
  if (XINT (values[7]) != s.st_size)
    values[7] = make_float ((double)s.st_size);
  /* If the size is negative, and its type is long, convert it back to
     positive.  */
  if (s.st_size < 0 && sizeof (s.st_size) == sizeof (long))
    values[7] = make_float ((double) ((unsigned long) s.st_size));

  filemodestring (&s, modes);
  values[8] = make_string (modes, 10);
#if defined (BSD4_2) || defined (BSD4_3) /* file gid will be dir gid */
  dirname = Ffile_name_directory (filename);
  if (! NILP (dirname))
    encoded = ENCODE_FILE (dirname);
  if (! NILP (dirname) && stat (SDATA (encoded), &sdir) == 0)
    values[9] = (sdir.st_gid != s.st_gid) ? Qt : Qnil;
  else					/* if we can't tell, assume worst */
    values[9] = Qt;
#else					/* file gid will be egid */
  values[9] = (s.st_gid != getegid ()) ? Qt : Qnil;
#endif	/* BSD4_2 (or BSD4_3) */
  /* Shut up GCC warnings in FIXNUM_OVERFLOW_P below.  */
  if (sizeof (s.st_ino) > sizeof (ino))
    ino = (EMACS_INT)(s.st_ino & 0xffffffff);
  else
    ino = s.st_ino;
  if (!FIXNUM_OVERFLOW_P (ino)
      && (sizeof (s.st_ino) <= sizeof (ino) || (s.st_ino & ~INTMASK) == 0))
    /* Keep the most common cases as integers.  */
    values[10] = make_number (ino);
  else if (sizeof (s.st_ino) <= sizeof (ino)
	   || ((s.st_ino >> 16) & ~INTMASK) == 0)
    /* To allow inode numbers larger than VALBITS, separate the bottom
       16 bits.  */
    values[10] = Fcons (make_number ((EMACS_INT)(s.st_ino >> 16)),
			make_number ((EMACS_INT)(s.st_ino & 0xffff)));
  else
    {
      /* To allow inode numbers beyond 32 bits, separate into 2 24-bit
	 high parts and a 16-bit bottom part.  */
      EMACS_INT high_ino = s.st_ino >> 32;
      EMACS_INT low_ino  = s.st_ino & 0xffffffff;

      values[10] = Fcons (make_number (high_ino >> 8),
			  Fcons (make_number (((high_ino & 0xff) << 16)
					      + (low_ino >> 16)),
				 make_number (low_ino & 0xffff)));
    }

  /* Likewise for device, but don't let it become negative.  We used
     to use FIXNUM_OVERFLOW_P here, but that won't catch large
     positive numbers such as 0xFFEEDDCC.  */
  if ((EMACS_INT)s.st_dev < 0
      || (EMACS_INT)s.st_dev > MOST_POSITIVE_FIXNUM)
    values[11] = Fcons (make_number (s.st_dev >> 16),
			make_number (s.st_dev & 0xffff));
  else
    values[11] = make_number (s.st_dev);

  return Flist (sizeof(values) / sizeof(values[0]), values);
}

DEFUN ("file-attributes-lessp", Ffile_attributes_lessp, Sfile_attributes_lessp, 2, 2, 0,
       doc: /* Return t if first arg file attributes list is less than second.
Comparison is in lexicographic order and case is significant.  */)
     (f1, f2)
     Lisp_Object f1, f2;
{
  return Fstring_lessp (Fcar (f1), Fcar (f2));
}

void
syms_of_dired ()
{
  Qdirectory_files = intern ("directory-files");
  Qdirectory_files_and_attributes = intern ("directory-files-and-attributes");
  Qfile_name_completion = intern ("file-name-completion");
  Qfile_name_all_completions = intern ("file-name-all-completions");
  Qfile_attributes = intern ("file-attributes");
  Qfile_attributes_lessp = intern ("file-attributes-lessp");
  Qdefault_directory = intern ("default-directory");

  staticpro (&Qdirectory_files);
  staticpro (&Qdirectory_files_and_attributes);
  staticpro (&Qfile_name_completion);
  staticpro (&Qfile_name_all_completions);
  staticpro (&Qfile_attributes);
  staticpro (&Qfile_attributes_lessp);
  staticpro (&Qdefault_directory);

  defsubr (&Sdirectory_files);
  defsubr (&Sdirectory_files_and_attributes);
  defsubr (&Sfile_name_completion);
#ifdef VMS
  defsubr (&Sfile_name_all_versions);
  defsubr (&Sfile_version_limit);
#endif /* VMS */
  defsubr (&Sfile_name_all_completions);
  defsubr (&Sfile_attributes);
  defsubr (&Sfile_attributes_lessp);

  DEFVAR_LISP ("completion-ignored-extensions", &Vcompletion_ignored_extensions,
	       doc: /* Completion ignores file names ending in any string in this list.
It does not ignore them if all possible completions end in one of
these strings or when displaying a list of completions.
It ignores directory names if they match any string in this list which
ends in a slash.  */);
  Vcompletion_ignored_extensions = Qnil;
}

/* arch-tag: 1ac8deca-4d8f-4d41-ade9-089154d98c03
   (do not change this comment) */
