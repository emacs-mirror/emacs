/* Lisp functions for making directory listings.
   Copyright (C) 1985, 1986, 1993, 1994, 1999, 2000, 2001
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


#include <config.h>

#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>

#include "systime.h"
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

#ifdef MSDOS
#define DIRENTRY_NONEMPTY(p) ((p)->d_name[0] != 0)
#else
#define DIRENTRY_NONEMPTY(p) ((p)->d_ino)
#endif

#include "lisp.h"
#include "buffer.h"
#include "commands.h"
#include "charset.h"
#include "coding.h"
#include "regex.h"

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
extern Lisp_Object Vcompletion_regexp_list;
extern Lisp_Object Vfile_name_coding_system, Vdefault_file_name_coding_system;

Lisp_Object Vcompletion_ignored_extensions;
Lisp_Object Qcompletion_ignore_case;
Lisp_Object Qdirectory_files;
Lisp_Object Qdirectory_files_and_attributes;
Lisp_Object Qfile_name_completion;
Lisp_Object Qfile_name_all_completions;
Lisp_Object Qfile_attributes;
Lisp_Object Qfile_attributes_lessp;


Lisp_Object
directory_files_internal_unwind (dh)
     Lisp_Object dh;
{
  DIR *d = (DIR *) ((XINT (XCAR (dh)) << 16) + XINT (XCDR (dh)));
  closedir (d);
  return Qnil;
}

/* Function shared by Fdirectory_files and Fdirectory_files_and_attributes.  
   When ATTRS is zero, return a list of directory filenames; when
   non-zero, return a list of directory filenames and their attributes.  */

Lisp_Object
directory_files_internal (directory, full, match, nosort, attrs)
     Lisp_Object directory, full, match, nosort;
     int attrs;
{
  DIR *d;
  int directory_nbytes;
  Lisp_Object list, dirfilename, encoded_directory;
  struct re_pattern_buffer *bufp = NULL;
  int needsep = 0;
  int count = specpdl_ptr - specpdl;
  struct gcpro gcpro1, gcpro2, gcpro3, gcpro4, gcpro5;
  DIRENTRY *dp;
  int retry_p;

  /* Because of file name handlers, these functions might call
     Ffuncall, and cause a GC.  */
  list = encoded_directory = dirfilename = Qnil;
  GCPRO5 (match, directory, list, dirfilename, encoded_directory);
  directory = Fexpand_file_name (directory, Qnil);
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
#else
      bufp = compile_pattern (match, 0, Qnil, 0, 1);
#endif
    }

  /* Note: ENCODE_FILE and DECODE_FILE can GC because they can run
     run_pre_post_conversion_on_str which calls Lisp directly and
     indirectly.  */
  dirfilename = ENCODE_FILE (dirfilename);
  encoded_directory = ENCODE_FILE (directory);

  /* Now *bufp is the compiled form of MATCH; don't call anything
     which might compile a new regexp until we're done with the loop!  */

  /* Do this opendir after anything which might signal an error; if
     an error is signaled while the directory stream is open, we
     have to make sure it gets closed, and setting up an
     unwind_protect to do so would be a pain.  */
 retry:
  
  d = opendir (XSTRING (dirfilename)->data);
  if (d == NULL)
    report_file_error ("Opening directory", Fcons (directory, Qnil));

  /* Unfortunately, we can now invoke expand-file-name and
     file-attributes on filenames, both of which can throw, so we must
     do a proper unwind-protect.  */
  record_unwind_protect (directory_files_internal_unwind,
			 Fcons (make_number (((unsigned long) d) >> 16),
				make_number (((unsigned long) d) & 0xffff)));

  directory_nbytes = STRING_BYTES (XSTRING (directory));
  re_match_object = Qt;

  /* Decide whether we need to add a directory separator.  */
#ifndef VMS
  if (directory_nbytes == 0
      || !IS_ANY_SEP (XSTRING (directory)->data[directory_nbytes - 1]))
    needsep = 1;
#endif /* not VMS */

  /* Loop reading blocks until EOF or error.  */
  for (;;)
    {
      errno = 0;
      dp = readdir (d);

#ifdef EAGAIN
      if (dp == NULL && errno == EAGAIN)
	continue;
#endif
      
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
	  
	  /* Note: ENCODE_FILE can GC; it should protect its argument,
	     though.  */
	  name = DECODE_FILE (name);
	  len = STRING_BYTES (XSTRING (name));

	  /* Now that we have unwind_protect in place, we might as well
             allow matching to be interrupted.  */
	  immediate_quit = 1;
	  QUIT;

	  if (NILP (match)
	      || (0 <= re_search (bufp, XSTRING (name)->data, len, 0, len, 0)))
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
		  bcopy (XSTRING (directory)->data, XSTRING (fullname)->data,
			 directory_nbytes);
		  
		  if (needsep)
		    XSTRING (fullname)->data[directory_nbytes] = DIRECTORY_SEP;
		  
		  bcopy (XSTRING (name)->data,
			 XSTRING (fullname)->data + directory_nbytes + needsep,
			 len);
		  
		  nchars = chars_in_text (XSTRING (fullname)->data, nbytes);

		  /* Some bug somewhere.  */
		  if (nchars > nbytes)
		    abort ();
		      
		  XSTRING (fullname)->size = nchars;
		  if (nchars == nbytes)
		    SET_STRING_BYTES (XSTRING (fullname), -1);
		  
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
		  fileattrs = Ffile_attributes (decoded_fullname);

		  list = Fcons (Fcons (finalname, fileattrs), list);
		  UNGCPRO;
		}
	      else
		list = Fcons (finalname, list);
	    }

	  UNGCPRO;
	}
    }

  retry_p = 0;
#ifdef EINTR
  retry_p |= errno == EINTR;
#endif

  closedir (d);

  /* Discard the unwind protect.  */
  specpdl_ptr = specpdl + count;

  if (retry_p)
    {
      list = Qnil;
      goto retry;
    }

  if (NILP (nosort))
    list = Fsort (Fnreverse (list),
		  attrs ? Qfile_attributes_lessp : Qstring_lessp);
  
  RETURN_UNGCPRO (list);
}


DEFUN ("directory-files", Fdirectory_files, Sdirectory_files, 1, 4, 0,
  "Return a list of names of files in DIRECTORY.\n\
There are three optional arguments:\n\
If FULL is non-nil, return absolute file names.  Otherwise return names\n\
 that are relative to the specified directory.\n\
If MATCH is non-nil, mention only file names that match the regexp MATCH.\n\
If NOSORT is non-nil, the list is not sorted--its order is unpredictable.\n\
 NOSORT is useful if you plan to sort the result yourself.")
  (directory, full, match, nosort)
     Lisp_Object directory, full, match, nosort;
{
  Lisp_Object handler;

  /* If the file name has special constructs in it,
     call the corresponding file handler.  */
  handler = Ffind_file_name_handler (directory, Qdirectory_files);
  if (!NILP (handler))
    {
      Lisp_Object args[6];

      args[0] = handler;
      args[1] = Qdirectory_files;
      args[2] = directory;
      args[3] = full;
      args[4] = match;
      args[5] = nosort;
      return Ffuncall (6, args);
    }

  return directory_files_internal (directory, full, match, nosort, 0);
}

DEFUN ("directory-files-and-attributes", Fdirectory_files_and_attributes, Sdirectory_files_and_attributes, 1, 4, 0,
  "Return a list of names of files and their attributes in DIRECTORY.\n\
There are three optional arguments:\n\
If FULL is non-nil, return absolute file names.  Otherwise return names\n\
 that are relative to the specified directory.\n\
If MATCH is non-nil, mention only file names that match the regexp MATCH.\n\
If NOSORT is non-nil, the list is not sorted--its order is unpredictable.\n\
 NOSORT is useful if you plan to sort the result yourself.")
  (directory, full, match, nosort)
     Lisp_Object directory, full, match, nosort;
{
  Lisp_Object handler;

  /* If the file name has special constructs in it,
     call the corresponding file handler.  */
  handler = Ffind_file_name_handler (directory, Qdirectory_files_and_attributes);
  if (!NILP (handler))
    {
      Lisp_Object args[6];

      args[0] = handler;
      args[1] = Qdirectory_files_and_attributes;
      args[2] = directory;
      args[3] = full;
      args[4] = match;
      args[5] = nosort;
      return Ffuncall (6, args);
    }

  return directory_files_internal (directory, full, match, nosort, 1);
}


Lisp_Object file_name_completion ();

DEFUN ("file-name-completion", Ffile_name_completion, Sfile_name_completion,
  2, 2, 0,
  "Complete file name FILE in directory DIRECTORY.\n\
Returns the longest string\n\
common to all file names in DIRECTORY that start with FILE.\n\
If there is only one and FILE matches it exactly, returns t.\n\
Returns nil if DIR contains no name starting with FILE.\n\
\n\
This function ignores some of the possible completions as\n\
determined by the variable `completion-ignored-extensions', which see.")
  (file, directory)
     Lisp_Object file, directory;
{
  Lisp_Object handler;

  /* If the directory name has special constructs in it,
     call the corresponding file handler.  */
  handler = Ffind_file_name_handler (directory, Qfile_name_completion);
  if (!NILP (handler))
    return call3 (handler, Qfile_name_completion, file, directory);

  /* If the file name has special constructs in it,
     call the corresponding file handler.  */
  handler = Ffind_file_name_handler (file, Qfile_name_completion);
  if (!NILP (handler))
    return call3 (handler, Qfile_name_completion, file, directory);

  return file_name_completion (file, directory, 0, 0);
}

DEFUN ("file-name-all-completions", Ffile_name_all_completions,
  Sfile_name_all_completions, 2, 2, 0,
  "Return a list of all completions of file name FILE in directory DIRECTORY.\n\
These are all file names in directory DIRECTORY which begin with FILE.")
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

  return file_name_completion (file, directory, 1, 0);
}

static int file_name_completion_stat ();

Lisp_Object
file_name_completion (file, dirname, all_flag, ver_flag)
     Lisp_Object file, dirname;
     int all_flag, ver_flag;
{
  DIR *d;
  int bestmatchsize = 0, skip;
  register int compare, matchsize;
  unsigned char *p1, *p2;
  int matchcount = 0;
  Lisp_Object bestmatch, tem, elt, name;
  Lisp_Object encoded_file;
  Lisp_Object encoded_dir;
  struct stat st;
  int directoryp;
  int passcount;
  int count = specpdl_ptr - specpdl;
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

  /* Do completion on the encoded file name
     because the other names in the directory are (we presume)
     encoded likewise.  We decode the completed string at the end.  */
  encoded_file = ENCODE_FILE (file);

  encoded_dir = ENCODE_FILE (dirname);

  /* With passcount = 0, ignore files that end in an ignored extension.
     If nothing found then try again with passcount = 1, don't ignore them.
     If looking for all completions, start with passcount = 1,
     so always take even the ignored ones.

     ** It would not actually be helpful to the user to ignore any possible
     completions when making a list of them.**  */

  for (passcount = !!all_flag; NILP (bestmatch) && passcount < 2; passcount++)
    {
      d = opendir (XSTRING (Fdirectory_file_name (encoded_dir))->data);
      if (!d)
	report_file_error ("Opening directory", Fcons (dirname, Qnil));

      /* Loop reading blocks */
      /* (att3b compiler bug requires do a null comparison this way) */
      while (1)
	{
	  DIRENTRY *dp;
	  int len;

#ifdef VMS
	  dp = (*readfunc) (d);
#else
	  dp = readdir (d);
#endif
	  if (!dp) break;

	  len = NAMLEN (dp);

	  if (!NILP (Vquit_flag) && NILP (Vinhibit_quit))
	    goto quit;
	  if (! DIRENTRY_NONEMPTY (dp)
	      || len < XSTRING (encoded_file)->size
	      || 0 <= scmp (dp->d_name, XSTRING (encoded_file)->data,
			    XSTRING (encoded_file)->size))
	    continue;

          if (file_name_completion_stat (encoded_dir, dp, &st) < 0)
            continue;

          directoryp = ((st.st_mode & S_IFMT) == S_IFDIR);
	  tem = Qnil;
          if (directoryp)
	    {
#ifndef TRIVIAL_DIRECTORY_ENTRY
#define TRIVIAL_DIRECTORY_ENTRY(n) (!strcmp (n, ".") || !strcmp (n, ".."))
#endif
	      /* "." and ".." are never interesting as completions, but are
		 actually in the way in a directory contains only one file.  */
	      if (!passcount && TRIVIAL_DIRECTORY_ENTRY (dp->d_name))
		continue;
	      if (!passcount && len > XSTRING (encoded_file)->size)
		/* Ignore directories if they match an element of
		   completion-ignored-extensions which ends in a slash.  */
		for (tem = Vcompletion_ignored_extensions;
		     CONSP (tem); tem = XCDR (tem))
		  {
		    int elt_len;

		    elt = XCAR (tem);
		    if (!STRINGP (elt))
		      continue;
		    elt_len = XSTRING (elt)->size - 1; /* -1 for trailing / */
		    if (elt_len <= 0)
		      continue;
		    p1 = XSTRING (elt)->data;
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
	      if (!passcount && len > XSTRING (encoded_file)->size)
		/* and exit this for loop if a match is found */
		for (tem = Vcompletion_ignored_extensions;
		     CONSP (tem); tem = XCDR (tem))
		  {
		    elt = XCAR (tem);
		    if (!STRINGP (elt)) continue;
		    skip = len - XSTRING (elt)->size;
		    if (skip < 0) continue;

		    if (0 <= scmp (dp->d_name + skip,
				   XSTRING (elt)->data,
				   XSTRING (elt)->size))
		      continue;
		    break;
		  }
	    }

	  /* If an ignored-extensions match was found,
	     don't process this name as a completion.  */
	  if (!passcount && CONSP (tem))
	    continue;

	  if (!passcount)
	    {
	      Lisp_Object regexps;
	      Lisp_Object zero;
	      XSETFASTINT (zero, 0);

	      /* Ignore this element if it fails to match all the regexps.  */
	      for (regexps = Vcompletion_regexp_list; CONSP (regexps);
		   regexps = XCDR (regexps))
		{
		  tem = Fstring_match (XCAR (regexps),
				       make_string (dp->d_name, len), zero);
		  if (NILP (tem))
		    break;
		}
	      if (CONSP (regexps))
		continue;
	    }

	  /* Update computation of how much all possible completions match */

	  matchcount++;

	  if (all_flag || NILP (bestmatch))
	    {
	      /* This is a possible completion */
	      if (directoryp)
		{
		  /* This completion is a directory; make it end with '/' */
		  name = Ffile_name_as_directory (make_string (dp->d_name, len));
		}
	      else
		name = make_string (dp->d_name, len);
	      if (all_flag)
		{
		  name = DECODE_FILE (name);
		  bestmatch = Fcons (name, bestmatch);
		}
	      else
		{
		  bestmatch = name;
		  bestmatchsize = XSTRING (name)->size;
		}
	    }
	  else
	    {
	      compare = min (bestmatchsize, len);
	      p1 = XSTRING (bestmatch)->data;
	      p2 = (unsigned char *) dp->d_name;
	      matchsize = scmp(p1, p2, compare);
	      if (matchsize < 0)
		matchsize = compare;
	      if (completion_ignore_case)
		{
		  /* If this is an exact match except for case,
		     use it as the best match rather than one that is not
		     an exact match.  This way, we get the case pattern
		     of the actual match.  */
		  /* This tests that the current file is an exact match
		     but BESTMATCH is not (it is too long).  */
		  if ((matchsize == len
		       && matchsize + !!directoryp 
			  < XSTRING (bestmatch)->size)
		      ||
		      /* If there is no exact match ignoring case,
			 prefer a match that does not change the case
			 of the input.  */
		      /* If there is more than one exact match aside from
			 case, and one of them is exact including case,
			 prefer that one.  */
		      /* This == checks that, of current file and BESTMATCH,
			 either both or neither are exact.  */
		      (((matchsize == len)
			==
			(matchsize + !!directoryp 
			 == XSTRING (bestmatch)->size))
		       && !bcmp (p2, XSTRING (encoded_file)->data, XSTRING (encoded_file)->size)
		       && bcmp (p1, XSTRING (encoded_file)->data, XSTRING (encoded_file)->size)))
		    {
		      bestmatch = make_string (dp->d_name, len);
		      if (directoryp)
			bestmatch = Ffile_name_as_directory (bestmatch);
		    }
		}

	      /* If this dirname all matches, see if implicit following
		 slash does too.  */
	      if (directoryp
		  && compare == matchsize
		  && bestmatchsize > matchsize
		  && IS_ANY_SEP (p1[matchsize]))
		matchsize++;
	      bestmatchsize = matchsize;
	    }
	}
      closedir (d);
    }

  UNGCPRO;
  bestmatch = unbind_to (count, bestmatch);

  if (all_flag || NILP (bestmatch))
    {
      if (STRINGP (bestmatch))
	bestmatch = DECODE_FILE (bestmatch);
      return bestmatch;
    }
  if (matchcount == 1 && bestmatchsize == XSTRING (file)->size)
    return Qt;
  bestmatch = Fsubstring (bestmatch, make_number (0),
			  make_number (bestmatchsize));
  /* Now that we got the right initial segment of BESTMATCH,
     decode it from the coding system in use.  */
  bestmatch = DECODE_FILE (bestmatch);
  return bestmatch;

 quit:
  if (d) closedir (d);
  Vquit_flag = Qnil;
  return Fsignal (Qquit, Qnil);
}

static int
file_name_completion_stat (dirname, dp, st_addr)
     Lisp_Object dirname;
     DIRENTRY *dp;
     struct stat *st_addr;
{
  int len = NAMLEN (dp);
  int pos = XSTRING (dirname)->size;
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

  bcopy (XSTRING (dirname)->data, fullname, pos);
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
  "Return a list of all versions of file name FILE in directory DIRECTORY.")
  (file, directory)
     Lisp_Object file, directory;
{
  return file_name_completion (file, directory, 1, 1);
}

DEFUN ("file-version-limit", Ffile_version_limit, Sfile_version_limit, 1, 1, 0,
  "Return the maximum number of versions allowed for FILE.\n\
Returns nil if the file cannot be opened or if there is no version limit.")
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
  fab.fab$l_fna = XSTRING (filename)->data;
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

DEFUN ("file-attributes", Ffile_attributes, Sfile_attributes, 1, 1, 0,
  "Return a list of attributes of file FILENAME.\n\
Value is nil if specified file cannot be opened.\n\
Otherwise, list elements are:\n\
 0. t for directory, string (name linked to) for symbolic link, or nil.\n\
 1. Number of links to file.\n\
 2. File uid.\n\
 3. File gid.\n\
 4. Last access time, as a list of two integers.\n\
  First integer has high-order 16 bits of time, second has low 16 bits.\n\
 5. Last modification time, likewise.\n\
 6. Last status change time, likewise.\n\
 7. Size in bytes.\n\
  This is a floating point number if the size is too large for an integer.\n\
 8. File modes, as a string of ten letters or dashes as in ls -l.\n\
 9. t iff file's gid would change if file were deleted and recreated.\n\
10. inode number.  If inode number is larger than the Emacs integer,\n\
  this is a cons cell containing two integers: first the high part,\n\
  then the low 16 bits.\n\
11. Device number.  If it is larger than the Emacs integer, this is\n\
  a cons cell, similar to the inode number.\n\
\n\
If file does not exist, returns nil.")
  (filename)
     Lisp_Object filename;
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

  filename = Fexpand_file_name (filename, Qnil);

  /* If the file name has special constructs in it,
     call the corresponding file handler.  */
  handler = Ffind_file_name_handler (filename, Qfile_attributes);
  if (!NILP (handler))
    return call2 (handler, Qfile_attributes, filename);

  encoded = ENCODE_FILE (filename);

  if (lstat (XSTRING (encoded)->data, &s) < 0)
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
  values[2] = make_number (s.st_uid);
  values[3] = make_number (s.st_gid);
  values[4] = make_time (s.st_atime);
  values[5] = make_time (s.st_mtime);
  values[6] = make_time (s.st_ctime);
  values[7] = make_number (s.st_size);
  /* If the size is out of range for an integer, return a float.  */
  if (XINT (values[7]) != s.st_size)
    values[7] = make_float ((double)s.st_size);
  filemodestring (&s, modes);
  values[8] = make_string (modes, 10);
#if defined (BSD4_2) || defined (BSD4_3) /* file gid will be dir gid */
  dirname = Ffile_name_directory (filename);
  if (! NILP (dirname))
    encoded = ENCODE_FILE (dirname);
  if (! NILP (dirname) && stat (XSTRING (encoded)->data, &sdir) == 0)
    values[9] = (sdir.st_gid != s.st_gid) ? Qt : Qnil;
  else					/* if we can't tell, assume worst */
    values[9] = Qt;
#else					/* file gid will be egid */
  values[9] = (s.st_gid != getegid ()) ? Qt : Qnil;
#endif	/* BSD4_2 (or BSD4_3) */
  /* Cast -1 to avoid warning if int is not as wide as VALBITS.  */
  if (FIXNUM_OVERFLOW_P (s.st_ino))
    /* To allow inode numbers larger than VALBITS, separate the bottom
       16 bits.  */
    values[10] = Fcons (make_number (s.st_ino >> 16),
			make_number (s.st_ino & 0xffff));
  else
    /* But keep the most common cases as integers.  */
    values[10] = make_number (s.st_ino);

  /* Likewise for device.  */
  if (FIXNUM_OVERFLOW_P (s.st_dev))
    values[11] = Fcons (make_number (s.st_dev >> 16),
			make_number (s.st_dev & 0xffff));
  else
    values[11] = make_number (s.st_dev);

  return Flist (sizeof(values) / sizeof(values[0]), values);
}

DEFUN ("file-attributes-lessp", Ffile_attributes_lessp, Sfile_attributes_lessp, 2, 2, 0,
  "Return t if first arg file attributes list is less than second.\n\
Comparison is in lexicographic order and case is significant.")
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

  staticpro (&Qdirectory_files);
  staticpro (&Qdirectory_files_and_attributes);
  staticpro (&Qfile_name_completion);
  staticpro (&Qfile_name_all_completions);
  staticpro (&Qfile_attributes);
  staticpro (&Qfile_attributes_lessp);

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

#ifdef VMS
  Qcompletion_ignore_case = intern ("completion-ignore-case");
  staticpro (&Qcompletion_ignore_case);
#endif /* VMS */

  DEFVAR_LISP ("completion-ignored-extensions", &Vcompletion_ignored_extensions,
    "*Completion ignores filenames ending in any string in this list.\n\
Directories are ignored if they match any string in this list which\n\
ends in a slash.\n\
This variable does not affect lists of possible completions,\n\
but does affect the commands that actually do completions.");
  Vcompletion_ignored_extensions = Qnil;
}
