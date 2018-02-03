/* Lisp functions for making directory listings.
   Copyright (C) 1985-1986, 1993-1994, 1999-2018 Free Software
   Foundation, Inc.

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

#include <stdio.h>
#include <sys/stat.h>

#ifdef HAVE_PWD_H
#include <pwd.h>
#endif
#include <grp.h>

#include <errno.h>
#include <fcntl.h>
#include <unistd.h>

#include <dirent.h>
#include <filemode.h>
#include <stat-time.h>

#include "lisp.h"
#include "systime.h"
#include "buffer.h"
#include "coding.h"
#include "regex.h"

#ifdef MSDOS
#include "msdos.h"	/* for fstatat */
#endif

#ifdef WINDOWSNT
extern int is_slow_fs (const char *);
#endif

static ptrdiff_t scmp (const char *, const char *, ptrdiff_t);
static Lisp_Object file_attributes (int, char const *, Lisp_Object,
				    Lisp_Object, Lisp_Object);

/* Return the number of bytes in DP's name.  */
static ptrdiff_t
dirent_namelen (struct dirent *dp)
{
#ifdef _D_EXACT_NAMLEN
  return _D_EXACT_NAMLEN (dp);
#else
  return strlen (dp->d_name);
#endif
}

#ifndef HAVE_STRUCT_DIRENT_D_TYPE
enum { DT_UNKNOWN, DT_DIR, DT_LNK };
#endif

/* Return the file type of DP.  */
static int
dirent_type (struct dirent *dp)
{
#ifdef HAVE_STRUCT_DIRENT_D_TYPE
  return dp->d_type;
#else
  return DT_UNKNOWN;
#endif
}

static DIR *
open_directory (Lisp_Object dirname, int *fdp)
{
  char *name = SSDATA (dirname);
  DIR *d;
  int fd, opendir_errno;

#ifdef DOS_NT
  /* Directories cannot be opened.  The emulation assumes that any
     file descriptor other than AT_FDCWD corresponds to the most
     recently opened directory.  This hack is good enough for Emacs.  */
  fd = 0;
  d = opendir (name);
  opendir_errno = errno;
#else
  fd = emacs_open (name, O_RDONLY | O_DIRECTORY, 0);
  if (fd < 0)
    {
      opendir_errno = errno;
      d = 0;
    }
  else
    {
      d = fdopendir (fd);
      opendir_errno = errno;
      if (! d)
	emacs_close (fd);
    }
#endif

  if (!d)
    report_file_errno ("Opening directory", dirname, opendir_errno);
  *fdp = fd;
  return d;
}

#ifdef WINDOWSNT
static void
directory_files_internal_w32_unwind (Lisp_Object arg)
{
  Vw32_get_true_file_attributes = arg;
}
#endif

static void
directory_files_internal_unwind (void *d)
{
  closedir (d);
}

/* Return the next directory entry from DIR; DIR's name is DIRNAME.
   If there are no more directory entries, return a null pointer.
   Signal any unrecoverable errors.  */

static struct dirent *
read_dirent (DIR *dir, Lisp_Object dirname)
{
  while (true)
    {
      errno = 0;
      struct dirent *dp = readdir (dir);
      if (dp || errno == 0)
	return dp;
      if (! (errno == EAGAIN || errno == EINTR))
	{
#ifdef WINDOWSNT
	  /* The MS-Windows implementation of 'opendir' doesn't
	     actually open a directory until the first call to
	     'readdir'.  If 'readdir' fails to open the directory, it
	     sets errno to ENOENT or EACCES, see w32.c.  */
	  if (errno == ENOENT || errno == EACCES)
	    report_file_error ("Opening directory", dirname);
#endif
	  report_file_error ("Reading directory", dirname);
	}
      maybe_quit ();
    }
}

/* Function shared by Fdirectory_files and Fdirectory_files_and_attributes.
   If not ATTRS, return a list of directory filenames;
   if ATTRS, return a list of directory filenames and their attributes.
   In the latter case, pass ID_FORMAT to file_attributes.  */

Lisp_Object
directory_files_internal (Lisp_Object directory, Lisp_Object full,
			  Lisp_Object match, Lisp_Object nosort, bool attrs,
			  Lisp_Object id_format)
{
  ptrdiff_t directory_nbytes;
  Lisp_Object list, dirfilename, encoded_directory;
  struct re_pattern_buffer *bufp = NULL;
  bool needsep = 0;
  ptrdiff_t count = SPECPDL_INDEX ();
#ifdef WINDOWSNT
  Lisp_Object w32_save = Qnil;
#endif

  /* Don't let the compiler optimize away all copies of DIRECTORY,
     which would break GC; see Bug#16986.  */
  Lisp_Object volatile directory_volatile = directory;

  /* Because of file name handlers, these functions might call
     Ffuncall, and cause a GC.  */
  list = encoded_directory = dirfilename = Qnil;
  dirfilename = Fdirectory_file_name (directory);

  if (!NILP (match))
    {
      CHECK_STRING (match);

      /* MATCH might be a flawed regular expression.  Rather than
	 catching and signaling our own errors, we just call
	 compile_pattern to do the work for us.  */
      /* Pass 1 for the MULTIBYTE arg
	 because we do make multibyte strings if the contents warrant.  */
# ifdef WINDOWSNT
      /* Windows users want case-insensitive wildcards.  */
      bufp = compile_pattern (match, 0,
			      BVAR (&buffer_defaults, case_canon_table), 0, 1);
# else	/* !WINDOWSNT */
      bufp = compile_pattern (match, 0, Qnil, 0, 1);
# endif	 /* !WINDOWSNT */
    }

  /* Note: ENCODE_FILE and DECODE_FILE can GC because they can run
     run_pre_post_conversion_on_str which calls Lisp directly and
     indirectly.  */
  dirfilename = ENCODE_FILE (dirfilename);
  encoded_directory = ENCODE_FILE (directory);

  /* Now *bufp is the compiled form of MATCH; don't call anything
     which might compile a new regexp until we're done with the loop!  */

  int fd;
  DIR *d = open_directory (dirfilename, &fd);

  /* Unfortunately, we can now invoke expand-file-name and
     file-attributes on filenames, both of which can throw, so we must
     do a proper unwind-protect.  */
  record_unwind_protect_ptr (directory_files_internal_unwind, d);

#ifdef WINDOWSNT
  if (attrs)
    {
      /* Do this only once to avoid doing it (in w32.c:stat) for each
	 file in the directory, when we call file_attributes below.  */
      record_unwind_protect (directory_files_internal_w32_unwind,
			     Vw32_get_true_file_attributes);
      w32_save = Vw32_get_true_file_attributes;
      if (EQ (Vw32_get_true_file_attributes, Qlocal))
	{
	  /* w32.c:stat will notice these bindings and avoid calling
	     GetDriveType for each file.  */
	  if (is_slow_fs (SSDATA (dirfilename)))
	    Vw32_get_true_file_attributes = Qnil;
	  else
	    Vw32_get_true_file_attributes = Qt;
	}
    }
#endif

  directory_nbytes = SBYTES (directory);
  re_match_object = Qt;

  /* Decide whether we need to add a directory separator.  */
  if (directory_nbytes == 0
      || !IS_ANY_SEP (SREF (directory, directory_nbytes - 1)))
    needsep = 1;

  /* Loop reading directory entries.  */
  for (struct dirent *dp; (dp = read_dirent (d, directory)); )
    {
      ptrdiff_t len = dirent_namelen (dp);
      Lisp_Object name = make_unibyte_string (dp->d_name, len);
      Lisp_Object finalname = name;

      /* Note: DECODE_FILE can GC; it should protect its argument,
	 though.  */
      name = DECODE_FILE (name);
      len = SBYTES (name);

      /* Now that we have unwind_protect in place, we might as well
	 allow matching to be interrupted.  */
      maybe_quit ();

      bool wanted = (NILP (match)
		     || re_search (bufp, SSDATA (name), len, 0, len, 0) >= 0);

      if (wanted)
	{
	  if (!NILP (full))
	    {
	      Lisp_Object fullname;
	      ptrdiff_t nbytes = len + directory_nbytes + needsep;
	      ptrdiff_t nchars;

	      fullname = make_uninit_multibyte_string (nbytes, nbytes);
	      memcpy (SDATA (fullname), SDATA (directory),
		      directory_nbytes);

	      if (needsep)
		SSET (fullname, directory_nbytes, DIRECTORY_SEP);

	      memcpy (SDATA (fullname) + directory_nbytes + needsep,
		      SDATA (name), len);

	      nchars = multibyte_chars_in_text (SDATA (fullname), nbytes);

	      /* Some bug somewhere.  */
	      if (nchars > nbytes)
		emacs_abort ();

	      STRING_SET_CHARS (fullname, nchars);
	      if (nchars == nbytes)
		STRING_SET_UNIBYTE (fullname);

	      finalname = fullname;
	    }
	  else
	    finalname = name;

	  if (attrs)
	    {
	      Lisp_Object fileattrs
		= file_attributes (fd, dp->d_name, directory, name, id_format);
	      list = Fcons (Fcons (finalname, fileattrs), list);
	    }
	  else
	    list = Fcons (finalname, list);
	}
    }

  closedir (d);
#ifdef WINDOWSNT
  if (attrs)
    Vw32_get_true_file_attributes = w32_save;
#endif

  /* Discard the unwind protect.  */
  specpdl_ptr = specpdl + count;

  if (NILP (nosort))
    list = Fsort (Fnreverse (list),
		  attrs ? Qfile_attributes_lessp : Qstring_lessp);

  (void) directory_volatile;
  return list;
}


DEFUN ("directory-files", Fdirectory_files, Sdirectory_files, 1, 4, 0,
       doc: /* Return a list of names of files in DIRECTORY.
There are three optional arguments:
If FULL is non-nil, return absolute file names.  Otherwise return names
 that are relative to the specified directory.
If MATCH is non-nil, mention only file names that match the regexp MATCH.
If NOSORT is non-nil, the list is not sorted--its order is unpredictable.
 Otherwise, the list returned is sorted with `string-lessp'.
 NOSORT is useful if you plan to sort the result yourself.  */)
  (Lisp_Object directory, Lisp_Object full, Lisp_Object match, Lisp_Object nosort)
{
  Lisp_Object handler;
  directory = Fexpand_file_name (directory, Qnil);

  /* If the file name has special constructs in it,
     call the corresponding file handler.  */
  handler = Ffind_file_name_handler (directory, Qdirectory_files);
  if (!NILP (handler))
    return call5 (handler, Qdirectory_files, directory,
                  full, match, nosort);

  return directory_files_internal (directory, full, match, nosort, false, Qnil);
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
  (Lisp_Object directory, Lisp_Object full, Lisp_Object match, Lisp_Object nosort, Lisp_Object id_format)
{
  Lisp_Object handler;
  directory = Fexpand_file_name (directory, Qnil);

  /* If the file name has special constructs in it,
     call the corresponding file handler.  */
  handler = Ffind_file_name_handler (directory, Qdirectory_files_and_attributes);
  if (!NILP (handler))
    return call6 (handler, Qdirectory_files_and_attributes,
                  directory, full, match, nosort, id_format);

  return directory_files_internal (directory, full, match, nosort,
				   true, id_format);
}


static Lisp_Object file_name_completion (Lisp_Object, Lisp_Object, bool,
					 Lisp_Object);

DEFUN ("file-name-completion", Ffile_name_completion, Sfile_name_completion,
       2, 3, 0,
       doc: /* Complete file name FILE in directory DIRECTORY.
Returns the longest string
common to all file names in DIRECTORY that start with FILE.
If there is only one and FILE matches it exactly, returns t.
Returns nil if DIRECTORY contains no name starting with FILE.

If PREDICATE is non-nil, call PREDICATE with each possible
completion (in absolute form) and ignore it if PREDICATE returns nil.

This function ignores some of the possible completions as determined
by the variables `completion-regexp-list' and
`completion-ignored-extensions', which see.  `completion-regexp-list'
is matched against file and directory names relative to DIRECTORY.  */)
  (Lisp_Object file, Lisp_Object directory, Lisp_Object predicate)
{
  Lisp_Object handler;
  directory = Fexpand_file_name (directory, Qnil);

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

  return file_name_completion (file, directory, 0, predicate);
}

DEFUN ("file-name-all-completions", Ffile_name_all_completions,
       Sfile_name_all_completions, 2, 2, 0,
       doc: /* Return a list of all completions of file name FILE in directory DIRECTORY.
These are all file names in directory DIRECTORY which begin with FILE.

This function ignores some of the possible completions as determined
by `completion-regexp-list', which see.  `completion-regexp-list'
is matched against file and directory names relative to DIRECTORY.  */)
  (Lisp_Object file, Lisp_Object directory)
{
  Lisp_Object handler;
  directory = Fexpand_file_name (directory, Qnil);

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

  return file_name_completion (file, directory, 1, Qnil);
}

static bool file_name_completion_dirp (int, struct dirent *, ptrdiff_t);

static Lisp_Object
file_name_completion (Lisp_Object file, Lisp_Object dirname, bool all_flag,
		      Lisp_Object predicate)
{
  ptrdiff_t bestmatchsize = 0;
  int matchcount = 0;
  /* If ALL_FLAG is 1, BESTMATCH is the list of all matches, decoded.
     If ALL_FLAG is 0, BESTMATCH is either nil
     or the best match so far, not decoded.  */
  Lisp_Object bestmatch, tem, elt, name;
  Lisp_Object encoded_file;
  Lisp_Object encoded_dir;
  bool directoryp;
  /* If not INCLUDEALL, exclude files in completion-ignored-extensions as
     well as "." and "..".  Until shown otherwise, assume we can't exclude
     anything.  */
  bool includeall = 1;
  bool check_decoded = false;
  ptrdiff_t count = SPECPDL_INDEX ();

  elt = Qnil;

  CHECK_STRING (file);

  bestmatch = Qnil;
  encoded_file = encoded_dir = Qnil;
  specbind (Qdefault_directory, dirname);

  /* Do completion on the encoded file name
     because the other names in the directory are (we presume)
     encoded likewise.  We decode the completed string at the end.  */
  /* Actually, this is not quite true any more: we do most of the completion
     work with decoded file names, but we still do some filtering based
     on the encoded file name.  */
  encoded_file = ENCODE_FILE (file);
  encoded_dir = ENCODE_FILE (Fdirectory_file_name (dirname));

  Lisp_Object file_encoding = Vfile_name_coding_system;
  if (NILP (Vfile_name_coding_system))
    file_encoding = Vdefault_file_name_coding_system;
  /* If the file-name encoding decomposes characters, as we do for
     HFS+ filesystems, we need to make an additional comparison of
     decoded names in order to filter false positives, such as "a"
     falsely matching "a-ring".  */
  if (!NILP (file_encoding)
      && !NILP (Fplist_get (Fcoding_system_plist (file_encoding),
			    Qdecomposed_characters)))
    {
      check_decoded = true;
      if (STRING_MULTIBYTE (file))
	{
	  /* Recompute FILE to make sure any decomposed characters in
	     it are re-composed by the post-read-conversion.
	     Otherwise, any decomposed characters will be rejected by
	     the additional check below.  */
	  file = DECODE_FILE (encoded_file);
	}
    }
  int fd;
  DIR *d = open_directory (encoded_dir, &fd);
  record_unwind_protect_ptr (directory_files_internal_unwind, d);

  /* Loop reading directory entries.  */
  for (struct dirent *dp; (dp = read_dirent (d, dirname)); )
    {
      ptrdiff_t len = dirent_namelen (dp);
      bool canexclude = 0;

      maybe_quit ();
      if (len < SCHARS (encoded_file)
	  || (scmp (dp->d_name, SSDATA (encoded_file),
		    SCHARS (encoded_file))
	      >= 0))
	continue;

      switch (dirent_type (dp))
	{
	case DT_DIR:
	  directoryp = true;
	  break;

	case DT_LNK: case DT_UNKNOWN:
	  directoryp = file_name_completion_dirp (fd, dp, len);
	  break;

	default:
	  directoryp = false;
	  break;
	}

      tem = Qnil;
      /* If all_flag is set, always include all.
	 It would not actually be helpful to the user to ignore any possible
	 completions when making a list of them.  */
      if (!all_flag)
	{
	  ptrdiff_t skip;

#if 0 /* FIXME: The `scmp' call compares an encoded and a decoded string. */
	  /* If this entry matches the current bestmatch, the only
	     thing it can do is increase matchcount, so don't bother
	     investigating it any further.  */
	  if (!completion_ignore_case
	      /* The return result depends on whether it's the sole match.  */
	      && matchcount > 1
	      && !includeall /* This match may allow includeall to 0.  */
	      && len >= bestmatchsize
	      && 0 > scmp (dp->d_name, SSDATA (bestmatch), bestmatchsize))
	    continue;
#endif

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
		    ptrdiff_t elt_len;
		    char *p1;

		    elt = XCAR (tem);
		    if (!STRINGP (elt))
		      continue;
		    /* Need to encode ELT, since scmp compares unibyte
		       strings only.  */
		    elt = ENCODE_FILE (elt);
		    elt_len = SCHARS (elt) - 1; /* -1 for trailing / */
		    if (elt_len <= 0)
		      continue;
		    p1 = SSDATA (elt);
		    if (p1[elt_len] != '/')
		      continue;
		    skip = len - elt_len;
		    if (skip < 0)
		      continue;

		    if (scmp (dp->d_name + skip, p1, elt_len) >= 0)
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

		    if (scmp (dp->d_name + skip, SSDATA (elt), SCHARS (elt))
			>= 0)
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
		 excludable files.  */
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
	Lisp_Object regexps, table = (completion_ignore_case
				      ? Vascii_canon_table : Qnil);

	/* Ignore this element if it fails to match all the regexps.  */
	for (regexps = Vcompletion_regexp_list; CONSP (regexps);
	     regexps = XCDR (regexps))
	  if (fast_string_match_internal (XCAR (regexps), name, table) < 0)
	    break;

	if (CONSP (regexps))
	  continue;
      }

      /* This is a possible completion */
      if (directoryp)
	/* This completion is a directory; make it end with '/'.  */
	name = Ffile_name_as_directory (name);

      /* Test the predicate, if any.  */
      if (!NILP (predicate) && NILP (call1 (predicate, name)))
	continue;

      /* Reject entries where the encoded strings match, but the
         decoded don't.  For example, "a" should not match "a-ring" on
         file systems that store decomposed characters. */
      Lisp_Object zero = make_number (0);

      if (check_decoded && SCHARS (file) <= SCHARS (name))
	{
	  /* FIXME: This is a copy of the code below.  */
	  ptrdiff_t compare = SCHARS (file);
	  Lisp_Object cmp
	    = Fcompare_strings (name, zero, make_number (compare),
				file, zero, make_number (compare),
				completion_ignore_case ? Qt : Qnil);
	  if (!EQ (cmp, Qt))
	    continue;
	}

      /* Suitably record this match.  */

      matchcount += matchcount <= 1;

      if (all_flag)
	bestmatch = Fcons (name, bestmatch);
      else if (NILP (bestmatch))
	{
	  bestmatch = name;
	  bestmatchsize = SCHARS (name);
	}
      else
	{
	  /* FIXME: This is a copy of the code in Ftry_completion.  */
	  ptrdiff_t compare = min (bestmatchsize, SCHARS (name));
	  Lisp_Object cmp
	    = Fcompare_strings (bestmatch, zero, make_number (compare),
				name, zero, make_number (compare),
				completion_ignore_case ? Qt : Qnil);
	  ptrdiff_t matchsize = EQ (cmp, Qt) ? compare : eabs (XINT (cmp)) - 1;

	  if (completion_ignore_case)
	    {
	      /* If this is an exact match except for case,
		 use it as the best match rather than one that is not
		 an exact match.  This way, we get the case pattern
		 of the actual match.  */
	      /* This tests that the current file is an exact match
		 but BESTMATCH is not (it is too long).  */
	      if ((matchsize == SCHARS (name)
		   && matchsize + directoryp < SCHARS (bestmatch))
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
		    (matchsize + directoryp == SCHARS (bestmatch)))
		   && (cmp = Fcompare_strings (name, zero,
					       make_number (SCHARS (file)),
					       file, zero,
					       Qnil,
					       Qnil),
		       EQ (Qt, cmp))
		   && (cmp = Fcompare_strings (bestmatch, zero,
					       make_number (SCHARS (file)),
					       file, zero,
					       Qnil,
					       Qnil),
		       ! EQ (Qt, cmp))))
		bestmatch = name;
	    }
	  bestmatchsize = matchsize;

	  /* If the best completion so far is reduced to the string
	     we're trying to complete, then we already know there's no
	     other completion, so there's no point looking any further.  */
	  if (matchsize <= SCHARS (file)
	      && !includeall /* A future match may allow includeall to 0.  */
	      /* If completion-ignore-case is non-nil, don't
		 short-circuit because we want to find the best
		 possible match *including* case differences.  */
	      && (!completion_ignore_case || matchsize == 0)
	      /* The return value depends on whether it's the sole match.  */
	      && matchcount > 1)
	    break;

	}
    }

  /* This closes the directory.  */
  bestmatch = unbind_to (count, bestmatch);

  if (all_flag || NILP (bestmatch))
    return bestmatch;
  /* Return t if the supplied string is an exact match (counting case);
     it does not require any change to be made.  */
  if (matchcount == 1 && !NILP (Fequal (bestmatch, file)))
    return Qt;
  bestmatch = Fsubstring (bestmatch, make_number (0),
			  make_number (bestmatchsize));
  return bestmatch;
}

/* Compare exactly LEN chars of strings at S1 and S2,
   ignoring case if appropriate.
   Return -1 if strings match,
   else number of chars that match at the beginning.  */

static ptrdiff_t
scmp (const char *s1, const char *s2, ptrdiff_t len)
{
  register ptrdiff_t l = len;

  if (completion_ignore_case)
    {
      while (l
	     && (downcase ((unsigned char) *s1++)
		 == downcase ((unsigned char) *s2++)))
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

/* Return true if in the directory FD the directory entry DP, whose
   string length is LEN, is that of a subdirectory that can be searched.  */
static bool
file_name_completion_dirp (int fd, struct dirent *dp, ptrdiff_t len)
{
  USE_SAFE_ALLOCA;
  char *subdir_name = SAFE_ALLOCA (len + 2);
  memcpy (subdir_name, dp->d_name, len);
  strcpy (subdir_name + len, "/");
  bool dirp = faccessat (fd, subdir_name, F_OK, AT_EACCESS) == 0;
  SAFE_FREE ();
  return dirp;
}

static char *
stat_uname (struct stat *st)
{
#ifdef WINDOWSNT
  return st->st_uname;
#else
  struct passwd *pw = getpwuid (st->st_uid);

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
  struct group *gr = getgrgid (st->st_gid);

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
below) - valid values are `string' and `integer'.  The latter is the
default, but we plan to change that, so you should specify a non-nil value
for ID-FORMAT if you use the returned uid or gid.

To access the elements returned, the following access functions are
provided: `file-attribute-type', `file-attribute-link-number',
`file-attribute-user-id', `file-attribute-group-id',
`file-attribute-access-time', `file-attribute-modification-time',
`file-attribute-status-change-time', `file-attribute-size',
`file-attribute-modes', `file-attribute-inode-number', and
`file-attribute-device-number'.

Elements of the attribute list are:
 0. t for directory, string (name linked to) for symbolic link, or nil.
 1. Number of links to file.
 2. File uid as a string or a number.  If a string value cannot be
  looked up, a numeric value, either an integer or a float, is returned.
 3. File gid, likewise.
 4. Last access time, as a list of integers (HIGH LOW USEC PSEC) in the
  same style as (current-time).
  (See a note below about access time on FAT-based filesystems.)
 5. Last modification time, likewise.  This is the time of the last
  change to the file's contents.
 6. Last status change time, likewise.  This is the time of last change
  to the file's attributes: owner and group, access mode bits, etc.
 7. Size in bytes.
  This is a floating point number if the size is too large for an integer.
 8. File modes, as a string of ten letters or dashes as in ls -l.
 9. An unspecified value, present only for backward compatibility.
10. inode number.  If it is larger than what an Emacs integer can hold,
  this is of the form (HIGH . LOW): first the high bits, then the low 16 bits.
  If even HIGH is too large for an Emacs integer, this is instead of the form
  (HIGH MIDDLE . LOW): first the high bits, then the middle 24 bits,
  and finally the low 16 bits.
11. Filesystem device number.  If it is larger than what the Emacs
  integer can hold, this is a cons cell, similar to the inode number.

On most filesystems, the combination of the inode and the device
number uniquely identifies the file.

On MS-Windows, performance depends on `w32-get-true-file-attributes',
which see.

On some FAT-based filesystems, only the date of last access is recorded,
so last access time will always be midnight of that day.  */)
  (Lisp_Object filename, Lisp_Object id_format)
{
  Lisp_Object encoded;
  Lisp_Object handler;

  filename = internal_condition_case_2 (Fexpand_file_name, filename, Qnil,
					Qt, Fidentity);
  if (!STRINGP (filename))
    return Qnil;

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

  encoded = ENCODE_FILE (filename);
  return file_attributes (AT_FDCWD, SSDATA (encoded), Qnil, filename,
			  id_format);
}

static Lisp_Object
file_attributes (int fd, char const *name,
		 Lisp_Object dirname, Lisp_Object filename,
		 Lisp_Object id_format)
{
  ptrdiff_t count = SPECPDL_INDEX ();
  struct stat s;

  /* An array to hold the mode string generated by filemodestring,
     including its terminating space and null byte.  */
  char modes[sizeof "-rwxr-xr-x "];

  char *uname = NULL, *gname = NULL;

  int err = EINVAL;

#ifdef O_PATH
  int namefd = openat (fd, name, O_PATH | O_CLOEXEC | O_NOFOLLOW);
  if (namefd < 0)
    err = errno;
  else
    {
      record_unwind_protect_int (close_file_unwind, namefd);
      if (fstat (namefd, &s) != 0)
	{
	  err = errno;
	  /* The Linux kernel before version 3.6 does not support
	     fstat on O_PATH file descriptors.  Handle this error like
	     missing support for O_PATH.  */
	  if (err == EBADF)
	    err = EINVAL;
	}
      else
	{
	  err = 0;
	  fd = namefd;
	  name = "";
	}
    }
#endif

  if (err == EINVAL)
    {
#ifdef WINDOWSNT
      /* We usually don't request accurate owner and group info,
	 because it can be expensive on Windows to get that, and most
	 callers of 'lstat' don't need that.  But here we do want that
	 information to be accurate.  */
      w32_stat_get_owner_group = 1;
#endif
      if (fstatat (fd, name, &s, AT_SYMLINK_NOFOLLOW) == 0)
	err = 0;
#ifdef WINDOWSNT
      w32_stat_get_owner_group = 0;
#endif
    }

  if (err != 0)
    return unbind_to (count, Qnil);

  Lisp_Object file_type;
  if (S_ISLNK (s.st_mode))
    {
      /* On systems lacking O_PATH support there is a race if the
	 symlink is replaced between the call to fstatat and the call
	 to emacs_readlinkat.  Detect this race unless the replacement
	 is also a symlink.  */
      file_type = emacs_readlinkat (fd, name);
      if (NILP (file_type))
	return unbind_to (count, Qnil);
    }
  else
    file_type = S_ISDIR (s.st_mode) ? Qt : Qnil;

  unbind_to (count, Qnil);

  if (!(NILP (id_format) || EQ (id_format, Qinteger)))
    {
      uname = stat_uname (&s);
      gname = stat_gname (&s);
    }

  filemodestring (&s, modes);

  return CALLN (Flist,
		file_type,
		make_number (s.st_nlink),
		(uname
		 ? DECODE_SYSTEM (build_unibyte_string (uname))
		 : make_fixnum_or_float (s.st_uid)),
		(gname
		 ? DECODE_SYSTEM (build_unibyte_string (gname))
		 : make_fixnum_or_float (s.st_gid)),
		make_lisp_time (get_stat_atime (&s)),
		make_lisp_time (get_stat_mtime (&s)),
		make_lisp_time (get_stat_ctime (&s)),

		/* If the file size is a 4-byte type, assume that
		   files of sizes in the 2-4 GiB range wrap around to
		   negative values, as this is a common bug on older
		   32-bit platforms.  */
		make_fixnum_or_float (sizeof (s.st_size) == 4
				      ? s.st_size & 0xffffffffu
				      : s.st_size),

		make_string (modes, 10),
		Qt,
		INTEGER_TO_CONS (s.st_ino),
		INTEGER_TO_CONS (s.st_dev));
}

DEFUN ("file-attributes-lessp", Ffile_attributes_lessp, Sfile_attributes_lessp, 2, 2, 0,
       doc: /* Return t if first arg file attributes list is less than second.
Comparison is in lexicographic order and case is significant.  */)
  (Lisp_Object f1, Lisp_Object f2)
{
  return Fstring_lessp (Fcar (f1), Fcar (f2));
}


DEFUN ("system-users", Fsystem_users, Ssystem_users, 0, 0, 0,
       doc: /* Return a list of user names currently registered in the system.
If we don't know how to determine that on this platform, just
return a list with one element, taken from `user-real-login-name'.  */)
     (void)
{
  Lisp_Object users = Qnil;
#if defined HAVE_GETPWENT && defined HAVE_ENDPWENT
  struct passwd *pw;

  while ((pw = getpwent ()))
    users = Fcons (DECODE_SYSTEM (build_string (pw->pw_name)), users);

  endpwent ();
#endif
  if (EQ (users, Qnil))
    /* At least current user is always known. */
    users = list1 (Vuser_real_login_name);
  return users;
}

DEFUN ("system-groups", Fsystem_groups, Ssystem_groups, 0, 0, 0,
       doc: /* Return a list of user group names currently registered in the system.
The value may be nil if not supported on this platform.  */)
     (void)
{
  Lisp_Object groups = Qnil;
#if defined HAVE_GETGRENT && defined HAVE_ENDGRENT
  struct group *gr;

  while ((gr = getgrent ()))
    groups = Fcons (DECODE_SYSTEM (build_string (gr->gr_name)), groups);

  endgrent ();
#endif
  return groups;
}

void
syms_of_dired (void)
{
  DEFSYM (Qdirectory_files, "directory-files");
  DEFSYM (Qdirectory_files_and_attributes, "directory-files-and-attributes");
  DEFSYM (Qfile_name_completion, "file-name-completion");
  DEFSYM (Qfile_name_all_completions, "file-name-all-completions");
  DEFSYM (Qfile_attributes, "file-attributes");
  DEFSYM (Qfile_attributes_lessp, "file-attributes-lessp");
  DEFSYM (Qdefault_directory, "default-directory");
  DEFSYM (Qdecomposed_characters, "decomposed-characters");

  defsubr (&Sdirectory_files);
  defsubr (&Sdirectory_files_and_attributes);
  defsubr (&Sfile_name_completion);
  defsubr (&Sfile_name_all_completions);
  defsubr (&Sfile_attributes);
  defsubr (&Sfile_attributes_lessp);
  defsubr (&Ssystem_users);
  defsubr (&Ssystem_groups);

  DEFVAR_LISP ("completion-ignored-extensions", Vcompletion_ignored_extensions,
	       doc: /* Completion ignores file names ending in any string in this list.
It does not ignore them if all possible completions end in one of
these strings or when displaying a list of completions.
It ignores directory names if they match any string in this list which
ends in a slash.  */);
  Vcompletion_ignored_extensions = Qnil;
}
