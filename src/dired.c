/* Lisp functions for making directory listings.
   Copyright (C) 1985, 1986, 1993 Free Software Foundation, Inc.

This file is part of GNU Emacs.

GNU Emacs is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 1, or (at your option)
any later version.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs; see the file COPYING.  If not, write to
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */


#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>

#include <config.h>

#ifdef VMS
#include <string.h>
#include <rms.h>
#include <rmsdef.h>
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

#else

#ifdef NONSYSTEM_DIR_LIBRARY
#include "ndir.h"
#else /* not NONSYSTEM_DIR_LIBRARY */
#include <sys/dir.h>
#endif /* not NONSYSTEM_DIR_LIBRARY */

#define DIRENTRY struct direct

extern DIR *opendir ();
extern struct direct *readdir ();

#endif

#include "lisp.h"
#include "buffer.h"
#include "commands.h"

#include "regex.h"

/* A search buffer, with a fastmap allocated and ready to go.  */
extern struct re_pattern_buffer searchbuf;

#define min(a, b) ((a) < (b) ? (a) : (b))

/* if system does not have symbolic links, it does not have lstat.
   In that case, use ordinary stat instead.  */

#ifndef S_IFLNK
#define lstat stat
#endif

extern int completion_ignore_case;
extern Lisp_Object Ffind_file_name_handler ();

Lisp_Object Vcompletion_ignored_extensions;

Lisp_Object Qcompletion_ignore_case;

Lisp_Object Qdirectory_files;
Lisp_Object Qfile_name_completion;
Lisp_Object Qfile_name_all_completions;
Lisp_Object Qfile_attributes;

DEFUN ("directory-files", Fdirectory_files, Sdirectory_files, 1, 4, 0,
  "Return a list of names of files in DIRECTORY.\n\
There are three optional arguments:\n\
If FULL is non-nil, absolute pathnames of the files are returned.\n\
If MATCH is non-nil, only pathnames containing that regexp are returned.\n\
If NOSORT is non-nil, the list is not sorted--its order is unpredictable.\n\
 NOSORT is useful if you plan to sort the result yourself.")
  (dirname, full, match, nosort)
     Lisp_Object dirname, full, match, nosort;
{
  DIR *d;
  int length;
  Lisp_Object list, name, dirfilename;
  Lisp_Object handler;

  /* If the file name has special constructs in it,
     call the corresponding file handler.  */
  handler = Ffind_file_name_handler (dirname);
  if (!NILP (handler))
    {
      Lisp_Object args[6];

      args[0] = handler;
      args[1] = Qdirectory_files;
      args[2] = dirname;
      args[3] = full;
      args[4] = match;
      args[5] = nosort;
      return Ffuncall (6, args);
    }

  {
    struct gcpro gcpro1, gcpro2;

    /* Because of file name handlers, these functions might call
     Ffuncall, and cause a GC.  */
    GCPRO1 (match);
    dirname = Fexpand_file_name (dirname, Qnil);
    UNGCPRO;
    GCPRO2 (match, dirname);
    dirfilename = Fdirectory_file_name (dirname);
    UNGCPRO;
  }

  if (!NILP (match))
    {
      CHECK_STRING (match, 3);

      /* MATCH might be a flawed regular expression.  Rather than
	 catching and signalling our own errors, we just call
	 compile_pattern to do the work for us.  */
#ifdef VMS
      compile_pattern (match, &searchbuf, 0,
		       buffer_defaults.downcase_table->contents);
#else
      compile_pattern (match, &searchbuf, 0, 0);
#endif
    }

  /* Now searchbuf is the compiled form of MATCH; don't call anything
     which might compile a new regexp until we're done with the loop!  */

  /* Do this opendir after anything which might signal an error; if
     an error is signalled while the directory stream is open, we
     have to make sure it gets closed, and setting up an
     unwind_protect to do so would be a pain.  */
  d = opendir (XSTRING (dirfilename)->data);
  if (! d)
    report_file_error ("Opening directory", Fcons (dirname, Qnil));

  list = Qnil;
  length = XSTRING (dirname)->size;

  /* Loop reading blocks */
  while (1)
    {
      DIRENTRY *dp = readdir (d);
      int len;

      if (!dp) break;
      len = NAMLEN (dp);
      if (dp->d_ino)
	{
	  if (NILP (match)
	      || (0 <= re_search (&searchbuf, dp->d_name, len, 0, len, 0)))
	    {
	      if (!NILP (full))
		{
		  int index = XSTRING (dirname)->size;
		  int total = len + index;
#ifndef VMS
		  if (length == 0
		      || XSTRING (dirname)->data[length - 1] != '/')
		    total++;
#endif /* VMS */

		  name = make_uninit_string (total);
		  bcopy (XSTRING (dirname)->data, XSTRING (name)->data,
			 index);
#ifndef VMS
		  if (length == 0
		      || XSTRING (dirname)->data[length - 1] != '/')
		    XSTRING (name)->data[index++] = '/';
#endif /* VMS */
		  bcopy (dp->d_name, XSTRING (name)->data + index, len);
		}
	      else
		name = make_string (dp->d_name, len);
	      list = Fcons (name, list);
	    }
	}
    }
  closedir (d);
  if (!NILP (nosort))
    return list;
  return Fsort (Fnreverse (list), Qstring_lessp);
}

Lisp_Object file_name_completion ();

DEFUN ("file-name-completion", Ffile_name_completion, Sfile_name_completion,
  2, 2, 0,
  "Complete file name FILE in directory DIR.\n\
Returns the longest string\n\
common to all filenames in DIR that start with FILE.\n\
If there is only one and FILE matches it exactly, returns t.\n\
Returns nil if DIR contains no name starting with FILE.")
  (file, dirname)
     Lisp_Object file, dirname;
{
  Lisp_Object handler;
  /* Don't waste time trying to complete a null string.
     Besides, this case happens when user is being asked for
     a directory name and has supplied one ending in a /.
     We would not want to add anything in that case
     even if there are some unique characters in that directory.  */
  if (XTYPE (file) == Lisp_String && XSTRING (file)->size == 0)
    return file;

  /* If the file name has special constructs in it,
     call the corresponding file handler.  */
  handler = Ffind_file_name_handler (dirname);
  if (!NILP (handler))
    return call3 (handler, Qfile_name_completion, file, dirname);

  return file_name_completion (file, dirname, 0, 0);
}

DEFUN ("file-name-all-completions", Ffile_name_all_completions,
  Sfile_name_all_completions, 2, 2, 0,
  "Return a list of all completions of file name FILE in directory DIR.\n\
These are all file names in directory DIR which begin with FILE.")
  (file, dirname)
     Lisp_Object file, dirname;
{
  Lisp_Object handler;

  /* If the file name has special constructs in it,
     call the corresponding file handler.  */
  handler = Ffind_file_name_handler (dirname);
  if (!NILP (handler))
    return call3 (handler, Qfile_name_all_completions, file, dirname);

  return file_name_completion (file, dirname, 1, 0);
}

Lisp_Object
file_name_completion (file, dirname, all_flag, ver_flag)
     Lisp_Object file, dirname;
     int all_flag, ver_flag;
{
  DIR *d;
  DIRENTRY *dp;
  int bestmatchsize, skip;
  register int compare, matchsize;
  unsigned char *p1, *p2;
  int matchcount = 0;
  Lisp_Object bestmatch, tem, elt, name;
  struct stat st;
  int directoryp;
  int passcount;
  int count = specpdl_ptr - specpdl;
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
  CHECK_STRING (file, 0);
#endif /* not VMS */

  dirname = Fexpand_file_name (dirname, Qnil);
  bestmatch = Qnil;

  /* With passcount = 0, ignore files that end in an ignored extension.
     If nothing found then try again with passcount = 1, don't ignore them.
     If looking for all completions, start with passcount = 1,
     so always take even the ignored ones.

     ** It would not actually be helpful to the user to ignore any possible
     completions when making a list of them.**  */

  for (passcount = !!all_flag; NILP (bestmatch) && passcount < 2; passcount++)
    {
      if (!(d = opendir (XSTRING (Fdirectory_file_name (dirname))->data)))
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
	  if (!dp->d_ino
	      || len < XSTRING (file)->size
	      || 0 <= scmp (dp->d_name, XSTRING (file)->data,
			    XSTRING (file)->size))
	    continue;

          if (file_name_completion_stat (dirname, dp, &st) < 0)
            continue;

          directoryp = ((st.st_mode & S_IFMT) == S_IFDIR);
	  tem = Qnil;
          if (!directoryp)
            {
	      /* Compare extensions-to-be-ignored against end of this file name */
	      /* if name is not an exact match against specified string */
	      if (!passcount && len > XSTRING (file)->size)
		/* and exit this for loop if a match is found */
		for (tem = Vcompletion_ignored_extensions;
		     CONSP (tem); tem = XCONS (tem)->cdr)
		  {
		    elt = XCONS (tem)->car;
		    if (XTYPE (elt) != Lisp_String) continue;
		    skip = len - XSTRING (elt)->size;
		    if (skip < 0) continue;

		    if (0 <= scmp (dp->d_name + skip,
				   XSTRING (elt)->data,
				   XSTRING (elt)->size))
		      continue;
		    break;
		  }
	    }

	  /* Unless an ignored-extensions match was found,
             process this name as a completion */
	  if (passcount || !CONSP (tem))
	    {
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
		      if ((matchsize == len
			   && matchsize + !!directoryp 
			      < XSTRING (bestmatch)->size)
			  ||
			  /* If there is no exact match ignoring case,
			     prefer a match that does not change the case
			     of the input.  */
			  (((matchsize == len)
			    ==
			    (matchsize + !!directoryp 
			     == XSTRING (bestmatch)->size))
			   /* If there is more than one exact match aside from
			      case, and one of them is exact including case,
			      prefer that one.  */
			   && !bcmp (p2, XSTRING (file)->data, XSTRING (file)->size)
			   && bcmp (p1, XSTRING (file)->data, XSTRING (file)->size)))
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
		      && p1[matchsize] == '/')
		    matchsize++;
		  bestmatchsize = matchsize;
		}
	    }
	}
      closedir (d);
    }

  unbind_to (count, Qnil);

  if (all_flag || NILP (bestmatch))
    return bestmatch;
  if (matchcount == 1 && bestmatchsize == XSTRING (file)->size)
    return Qt;
  return Fsubstring (bestmatch, make_number (0), make_number (bestmatchsize));
 quit:
  if (d) closedir (d);
  Vquit_flag = Qnil;
  return Fsignal (Qquit, Qnil);
}

file_name_completion_stat (dirname, dp, st_addr)
     Lisp_Object dirname;
     DIRENTRY *dp;
     struct stat *st_addr;
{
  int len = NAMLEN (dp);
  int pos = XSTRING (dirname)->size;
  char *fullname = (char *) alloca (len + pos + 2);

  bcopy (XSTRING (dirname)->data, fullname, pos);
#ifndef VMS
  if (fullname[pos - 1] != '/')
    fullname[pos++] = '/';
#endif

  bcopy (dp->d_name, fullname + pos, len);
  fullname[pos + len] = 0;

  return stat (fullname, st_addr);
}

#ifdef VMS

DEFUN ("file-name-all-versions", Ffile_name_all_versions,
  Sfile_name_all_versions, 2, 2, 0,
  "Return a list of all versions of file name FILE in directory DIR.")
  (file, dirname)
     Lisp_Object file, dirname;
{
  return file_name_completion (file, dirname, 1, 1);
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
     int time;
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
 7. Size in bytes (-1, if number is out of range).\n\
 8. File modes, as a string of ten letters or dashes as in ls -l.\n\
 9. t iff file's gid would change if file were deleted and recreated.\n\
10. inode number.\n\
11. Device number.\n\
\n\
If file does not exist, returns nil.")
  (filename)
     Lisp_Object filename;
{
  Lisp_Object values[12];
  Lisp_Object dirname;
  struct stat s;
  struct stat sdir;
  char modes[10];
  Lisp_Object handler;

  filename = Fexpand_file_name (filename, Qnil);

  /* If the file name has special constructs in it,
     call the corresponding file handler.  */
  handler = Ffind_file_name_handler (filename);
  if (!NILP (handler))
    return call2 (handler, Qfile_attributes, filename);

  if (lstat (XSTRING (filename)->data, &s) < 0)
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
  /* If the size is out of range, give back -1.  */
  if (XINT (values[7]) != s.st_size)
    XSETINT (values[7], -1);
  filemodestring (&s, modes);
  values[8] = make_string (modes, 10);
#ifdef BSD4_3 /* Gross kludge to avoid lack of "#if defined(...)" in VMS */
#define BSD4_2 /* A new meaning to the term `backwards compatibility' */
#endif
#ifdef BSD4_2			/* file gid will be dir gid */
  dirname = Ffile_name_directory (filename);
  if (! NILP (dirname) && stat (XSTRING (dirname)->data, &sdir) == 0)
    values[9] = (sdir.st_gid != s.st_gid) ? Qt : Qnil;
  else					/* if we can't tell, assume worst */
    values[9] = Qt;
#else					/* file gid will be egid */
  values[9] = (s.st_gid != getegid ()) ? Qt : Qnil;
#endif	/* BSD4_2 (or BSD4_3) */
#ifdef BSD4_3
#undef BSD4_2 /* ok, you can look again without throwing up */
#endif
  values[10] = make_number (s.st_ino);
  values[11] = make_number (s.st_dev);
  return Flist (sizeof(values) / sizeof(values[0]), values);
}

syms_of_dired ()
{
  Qdirectory_files = intern ("directory-files");
  Qfile_name_completion = intern ("file-name-completion");
  Qfile_name_all_completions = intern ("file-name-all-completions");
  Qfile_attributes = intern ("file-attributes");

  defsubr (&Sdirectory_files);
  defsubr (&Sfile_name_completion);
#ifdef VMS
  defsubr (&Sfile_name_all_versions);
  defsubr (&Sfile_version_limit);
#endif /* VMS */
  defsubr (&Sfile_name_all_completions);
  defsubr (&Sfile_attributes);

#ifdef VMS
  Qcompletion_ignore_case = intern ("completion-ignore-case");
  staticpro (&Qcompletion_ignore_case);
#endif /* VMS */

  DEFVAR_LISP ("completion-ignored-extensions", &Vcompletion_ignored_extensions,
    "*Completion ignores filenames ending in any string in this list.\n\
This variable does not affect lists of possible completions,\n\
but does affect the commands that actually do completions.");
  Vcompletion_ignored_extensions = Qnil;
}
