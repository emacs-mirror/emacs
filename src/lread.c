/* Lisp parsing and input streams.
   Copyright (C) 1985, 1986, 1987, 1988, 1989, 
   1993, 1994 Free Software Foundation, Inc.

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
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */


#include <config.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/file.h>
#include <errno.h>
#include "lisp.h"

#ifndef standalone
#include "buffer.h"
#include <paths.h>
#include "commands.h"
#include "keyboard.h"
#include "termhooks.h"
#endif

#ifdef lint
#include <sys/inode.h>
#endif /* lint */

#ifndef X_OK
#define X_OK 01
#endif

#ifdef LISP_FLOAT_TYPE
#ifdef STDC_HEADERS
#include <stdlib.h>
#endif

#ifdef MSDOS
#include "msdos.h"
/* These are redefined (correctly, but differently) in values.h.  */
#undef INTBITS
#undef LONGBITS
#undef SHORTBITS
#endif

#include <math.h>
#endif /* LISP_FLOAT_TYPE */

#ifndef O_RDONLY
#define O_RDONLY 0
#endif

extern int errno;

Lisp_Object Qread_char, Qget_file_char, Qstandard_input, Qcurrent_load_list;
Lisp_Object Qvariable_documentation, Vvalues, Vstandard_input, Vafter_load_alist;
Lisp_Object Qascii_character, Qload, Qload_file_name;

extern Lisp_Object Qevent_symbol_element_mask;

/* non-zero if inside `load' */
int load_in_progress;

/* Search path for files to be loaded. */
Lisp_Object Vload_path;

/* This is the user-visible association list that maps features to
   lists of defs in their load files. */
Lisp_Object Vload_history;

/* This is used to build the load history. */
Lisp_Object Vcurrent_load_list;

/* Name of file actually being read by `load'.  */
Lisp_Object Vload_file_name;

/* List of descriptors now open for Fload.  */
static Lisp_Object load_descriptor_list;

/* File for get_file_char to read from.  Use by load */
static FILE *instream;

/* When nonzero, read conses in pure space */
static int read_pure;

/* For use within read-from-string (this reader is non-reentrant!!) */
static int read_from_string_index;
static int read_from_string_limit;

/* Handle unreading and rereading of characters.
   Write READCHAR to read a character,
   UNREAD(c) to unread c to be read again. */

#define READCHAR readchar (readcharfun)
#define UNREAD(c) unreadchar (readcharfun, c)

static int
readchar (readcharfun)
     Lisp_Object readcharfun;
{
  Lisp_Object tem;
  register struct buffer *inbuffer;
  register int c, mpos;

  if (BUFFERP (readcharfun))
    {
      inbuffer = XBUFFER (readcharfun);

      if (BUF_PT (inbuffer) >= BUF_ZV (inbuffer))
	return -1;
      c = *(unsigned char *) BUF_CHAR_ADDRESS (inbuffer, BUF_PT (inbuffer));
      SET_BUF_PT (inbuffer, BUF_PT (inbuffer) + 1);

      return c;
    }
  if (MARKERP (readcharfun))
    {
      inbuffer = XMARKER (readcharfun)->buffer;

      mpos = marker_position (readcharfun);

      if (mpos > BUF_ZV (inbuffer) - 1)
	return -1;
      c = *(unsigned char *) BUF_CHAR_ADDRESS (inbuffer, mpos);
      if (mpos != BUF_GPT (inbuffer))
	XMARKER (readcharfun)->bufpos++;
      else
	Fset_marker (readcharfun, make_number (mpos + 1),
		     Fmarker_buffer (readcharfun));
      return c;
    }
  if (EQ (readcharfun, Qget_file_char))
    {
      c = getc (instream);
#ifdef EINTR
      /* Interrupted reads have been observed while reading over the network */
      while (c == EOF && ferror (instream) && errno == EINTR)
	{
	  clearerr (instream);
	  c = getc (instream);
	}
#endif
      return c;
    }

  if (STRINGP (readcharfun))
    {
      register int c;
      /* This used to be return of a conditional expression,
	 but that truncated -1 to a char on VMS.  */
      if (read_from_string_index < read_from_string_limit)
	c = XSTRING (readcharfun)->data[read_from_string_index++];
      else
	c = -1;
      return c;
    }

  tem = call0 (readcharfun);

  if (NILP (tem))
    return -1;
  return XINT (tem);
}

/* Unread the character C in the way appropriate for the stream READCHARFUN.
   If the stream is a user function, call it with the char as argument.  */

static void
unreadchar (readcharfun, c)
     Lisp_Object readcharfun;
     int c;
{
  if (c == -1)
    /* Don't back up the pointer if we're unreading the end-of-input mark,
       since readchar didn't advance it when we read it.  */
    ;
  else if (BUFFERP (readcharfun))
    {
      if (XBUFFER (readcharfun) == current_buffer)
	SET_PT (point - 1);
      else
	SET_BUF_PT (XBUFFER (readcharfun), BUF_PT (XBUFFER (readcharfun)) - 1);
    }
  else if (MARKERP (readcharfun))
    XMARKER (readcharfun)->bufpos--;
  else if (STRINGP (readcharfun))
    read_from_string_index--;
  else if (EQ (readcharfun, Qget_file_char))
    ungetc (c, instream);
  else
    call1 (readcharfun, make_number (c));
}

static Lisp_Object read0 (), read1 (), read_list (), read_vector ();

/* get a character from the tty */

extern Lisp_Object read_char ();

/* Read input events until we get one that's acceptable for our purposes.

   If NO_SWITCH_FRAME is non-zero, switch-frame events are stashed
   until we get a character we like, and then stuffed into
   unread_switch_frame.

   If ASCII_REQUIRED is non-zero, we check function key events to see
   if the unmodified version of the symbol has a Qascii_character
   property, and use that character, if present.

   If ERROR_NONASCII is non-zero, we signal an error if the input we
   get isn't an ASCII character with modifiers.  If it's zero but
   ASCII_REQUIRED is non-zero, we just re-read until we get an ASCII
   character.  */
Lisp_Object
read_filtered_event (no_switch_frame, ascii_required, error_nonascii)
     int no_switch_frame, ascii_required, error_nonascii;
{
#ifdef standalone
  return make_number (getchar ());
#else
  register Lisp_Object val, delayed_switch_frame;

  delayed_switch_frame = Qnil;

  /* Read until we get an acceptable event.  */
 retry:
  val = read_char (0, 0, 0, Qnil, 0);

  if (BUFFERP (val))
    goto retry;

  /* switch-frame events are put off until after the next ASCII
     character.  This is better than signalling an error just because
     the last characters were typed to a separate minibuffer frame,
     for example.  Eventually, some code which can deal with
     switch-frame events will read it and process it.  */
  if (no_switch_frame
      && EVENT_HAS_PARAMETERS (val)
      && EQ (EVENT_HEAD (val), Qswitch_frame))
    {
      delayed_switch_frame = val;
      goto retry;
    }

  if (ascii_required)
    {
      /* Convert certain symbols to their ASCII equivalents.  */
      if (SYMBOLP (val))
	{
	  Lisp_Object tem, tem1, tem2;
	  tem = Fget (val, Qevent_symbol_element_mask);
	  if (!NILP (tem))
	    {
	      tem1 = Fget (Fcar (tem), Qascii_character);
	      /* Merge this symbol's modifier bits
		 with the ASCII equivalent of its basic code.  */
	      if (!NILP (tem1))
		XSETFASTINT (val, XINT (tem1) | XINT (Fcar (Fcdr (tem))));
	    }
	}
	  
      /* If we don't have a character now, deal with it appropriately.  */
      if (!INTEGERP (val))
	{
	  if (error_nonascii)
	    {
	      Vunread_command_events = Fcons (val, Qnil);
	      error ("Non-character input-event");
	    }
	  else
	    goto retry;
	}
    }

  if (! NILP (delayed_switch_frame))
    unread_switch_frame = delayed_switch_frame;

  return val;
#endif
}

DEFUN ("read-char", Fread_char, Sread_char, 0, 0, 0,
  "Read a character from the command input (keyboard or macro).\n\
It is returned as a number.\n\
If the user generates an event which is not a character (i.e. a mouse\n\
click or function key event), `read-char' signals an error.  As an\n\
exception, switch-frame events are put off until non-ASCII events can\n\
be read.\n\
If you want to read non-character events, or ignore them, call\n\
`read-event' or `read-char-exclusive' instead.")
  ()
{
  return read_filtered_event (1, 1, 1);
}

DEFUN ("read-event", Fread_event, Sread_event, 0, 0, 0,
  "Read an event object from the input stream.")
  ()
{
  return read_filtered_event (0, 0, 0);
}

DEFUN ("read-char-exclusive", Fread_char_exclusive, Sread_char_exclusive, 0, 0, 0,
  "Read a character from the command input (keyboard or macro).\n\
It is returned as a number.  Non character events are ignored.")
  ()
{
  return read_filtered_event (1, 1, 0);
}

DEFUN ("get-file-char", Fget_file_char, Sget_file_char, 0, 0, 0,
  "Don't use this yourself.")
  ()
{
  register Lisp_Object val;
  XSETINT (val, getc (instream));
  return val;
}

static void readevalloop ();
static Lisp_Object load_unwind ();
static Lisp_Object load_descriptor_unwind ();

DEFUN ("load", Fload, Sload, 1, 4, 0,
  "Execute a file of Lisp code named FILE.\n\
First try FILE with `.elc' appended, then try with `.el',\n\
 then try FILE unmodified.\n\
This function searches the directories in `load-path'.\n\
If optional second arg NOERROR is non-nil,\n\
 report no error if FILE doesn't exist.\n\
Print messages at start and end of loading unless\n\
 optional third arg NOMESSAGE is non-nil.\n\
If optional fourth arg NOSUFFIX is non-nil, don't try adding\n\
 suffixes `.elc' or `.el' to the specified name FILE.\n\
Return t if file exists.")
  (str, noerror, nomessage, nosuffix)
     Lisp_Object str, noerror, nomessage, nosuffix;
{
  register FILE *stream;
  register int fd = -1;
  register Lisp_Object lispstream;
  int count = specpdl_ptr - specpdl;
  Lisp_Object temp;
  struct gcpro gcpro1;
  Lisp_Object found;
  /* 1 means inhibit the message at the beginning.  */
  int nomessage1 = 0;
  Lisp_Object handler;
#ifdef DOS_NT
  char *dosmode = "rt";
#endif /* DOS_NT */

  CHECK_STRING (str, 0);

  /* If file name is magic, call the handler.  */
  handler = Ffind_file_name_handler (str, Qload);
  if (!NILP (handler))
    return call5 (handler, Qload, str, noerror, nomessage, nosuffix);

  /* Do this after the handler to avoid
     the need to gcpro noerror, nomessage and nosuffix.
     (Below here, we care only whether they are nil or not.)  */
  str = Fsubstitute_in_file_name (str);

  /* Avoid weird lossage with null string as arg,
     since it would try to load a directory as a Lisp file */
  if (XSTRING (str)->size > 0)
    {
      GCPRO1 (str);
      fd = openp (Vload_path, str, !NILP (nosuffix) ? "" : ".elc:.el:",
		  &found, 0);
      UNGCPRO;
    }

  if (fd < 0)
    {
      if (NILP (noerror))
	while (1)
	  Fsignal (Qfile_error, Fcons (build_string ("Cannot open load file"),
				       Fcons (str, Qnil)));
      else
	return Qnil;
    }

  if (!bcmp (&(XSTRING (found)->data[XSTRING (found)->size - 4]),
	     ".elc", 4))
    {
      struct stat s1, s2;
      int result;

#ifdef DOS_NT
      dosmode = "rb";
#endif /* DOS_NT */
      stat ((char *)XSTRING (found)->data, &s1);
      XSTRING (found)->data[XSTRING (found)->size - 1] = 0;
      result = stat ((char *)XSTRING (found)->data, &s2);
      if (result >= 0 && (unsigned) s1.st_mtime < (unsigned) s2.st_mtime)
	{
	  message ("Source file `%s' newer than byte-compiled file",
		   XSTRING (found)->data);
	  /* Don't immediately overwrite this message.  */
	  if (!noninteractive)
	    nomessage1 = 1;
	}
      XSTRING (found)->data[XSTRING (found)->size - 1] = 'c';
    }

#ifdef DOS_NT
  close (fd);
  stream = fopen ((char *) XSTRING (found)->data, dosmode);
#else  /* not DOS_NT */
  stream = fdopen (fd, "r");
#endif /* not DOS_NT */
  if (stream == 0)
    {
      close (fd);
      error ("Failure to create stdio stream for %s", XSTRING (str)->data);
    }

  if (NILP (nomessage) && !nomessage1)
    message ("Loading %s...", XSTRING (str)->data);

  GCPRO1 (str);
  lispstream = Fcons (Qnil, Qnil);
  XSETFASTINT (XCONS (lispstream)->car, (EMACS_UINT)stream >> 16);
  XSETFASTINT (XCONS (lispstream)->cdr, (EMACS_UINT)stream & 0xffff);
  record_unwind_protect (load_unwind, lispstream);
  record_unwind_protect (load_descriptor_unwind, load_descriptor_list);
  specbind (Qload_file_name, found);
  load_descriptor_list
    = Fcons (make_number (fileno (stream)), load_descriptor_list);
  load_in_progress++;
  readevalloop (Qget_file_char, stream, str, Feval, 0);
  unbind_to (count, Qnil);

  /* Run any load-hooks for this file.  */
  temp = Fassoc (str, Vafter_load_alist);
  if (!NILP (temp))
    Fprogn (Fcdr (temp));
  UNGCPRO;

  if (!noninteractive && NILP (nomessage))
    message ("Loading %s...done", XSTRING (str)->data);
  return Qt;
}

static Lisp_Object
load_unwind (stream)  /* used as unwind-protect function in load */
     Lisp_Object stream;
{
  fclose ((FILE *) (XFASTINT (XCONS (stream)->car) << 16
		    | XFASTINT (XCONS (stream)->cdr)));
  if (--load_in_progress < 0) load_in_progress = 0;
  return Qnil;
}

static Lisp_Object
load_descriptor_unwind (oldlist)
     Lisp_Object oldlist;
{
  load_descriptor_list = oldlist;
  return Qnil;
}

/* Close all descriptors in use for Floads.
   This is used when starting a subprocess.  */

void
close_load_descs ()
{
  Lisp_Object tail;
  for (tail = load_descriptor_list; !NILP (tail); tail = XCONS (tail)->cdr)
    close (XFASTINT (XCONS (tail)->car));
}

static int
complete_filename_p (pathname)
     Lisp_Object pathname;
{
  register unsigned char *s = XSTRING (pathname)->data;
  return (IS_DIRECTORY_SEP (s[0])
	  || (XSTRING (pathname)->size > 2
	      && IS_DEVICE_SEP (s[1]) && IS_DIRECTORY_SEP (s[2]))
#ifdef ALTOS
	  || *s == '@'
#endif
#ifdef VMS
	  || index (s, ':')
#endif /* VMS */
	  );
}

/* Search for a file whose name is STR, looking in directories
   in the Lisp list PATH, and trying suffixes from SUFFIX.
   SUFFIX is a string containing possible suffixes separated by colons.
   On success, returns a file descriptor.  On failure, returns -1.

   EXEC_ONLY nonzero means don't open the files,
   just look for one that is executable.  In this case,
   returns 1 on success.

   If STOREPTR is nonzero, it points to a slot where the name of
   the file actually found should be stored as a Lisp string.
   Nil is stored there on failure.  */

int
openp (path, str, suffix, storeptr, exec_only)
     Lisp_Object path, str;
     char *suffix;
     Lisp_Object *storeptr;
     int exec_only;
{
  register int fd;
  int fn_size = 100;
  char buf[100];
  register char *fn = buf;
  int absolute = 0;
  int want_size;
  register Lisp_Object filename;
  struct stat st;
  struct gcpro gcpro1;

  GCPRO1 (str);
  if (storeptr)
    *storeptr = Qnil;

  if (complete_filename_p (str))
    absolute = 1;

  for (; !NILP (path); path = Fcdr (path))
    {
      char *nsuffix;

      filename = Fexpand_file_name (str, Fcar (path));
      if (!complete_filename_p (filename))
	/* If there are non-absolute elts in PATH (eg ".") */
	/* Of course, this could conceivably lose if luser sets
	   default-directory to be something non-absolute... */
	{
	  filename = Fexpand_file_name (filename, current_buffer->directory);
	  if (!complete_filename_p (filename))
	    /* Give up on this path element! */
	    continue;
	}

      /* Calculate maximum size of any filename made from
	 this path element/specified file name and any possible suffix.  */
      want_size = strlen (suffix) + XSTRING (filename)->size + 1;
      if (fn_size < want_size)
	fn = (char *) alloca (fn_size = 100 + want_size);

      nsuffix = suffix;

      /* Loop over suffixes.  */
      while (1)
	{
	  char *esuffix = (char *) index (nsuffix, ':');
	  int lsuffix = esuffix ? esuffix - nsuffix : strlen (nsuffix);

	  /* Concatenate path element/specified name with the suffix.  */
	  strncpy (fn, XSTRING (filename)->data, XSTRING (filename)->size);
	  fn[XSTRING (filename)->size] = 0;
	  if (lsuffix != 0)  /* Bug happens on CCI if lsuffix is 0.  */
	    strncat (fn, nsuffix, lsuffix);

	  /* Ignore file if it's a directory.  */
	  if (stat (fn, &st) >= 0
	      && (st.st_mode & S_IFMT) != S_IFDIR)
	    {
	      /* Check that we can access or open it.  */
	      if (exec_only)
		fd = (access (fn, X_OK) == 0) ? 1 : -1;
	      else
		fd = open (fn, O_RDONLY, 0);

	      if (fd >= 0)
		{
		  /* We succeeded; return this descriptor and filename.  */
		  if (storeptr)
		    *storeptr = build_string (fn);
		  UNGCPRO;
		  return fd;
		}
	    }

	  /* Advance to next suffix.  */
	  if (esuffix == 0)
	    break;
	  nsuffix += lsuffix + 1;
	}
      if (absolute)
	break;
    }

  UNGCPRO;
  return -1;
}


/* Merge the list we've accumulated of globals from the current input source
   into the load_history variable.  The details depend on whether
   the source has an associated file name or not. */

static void
build_load_history (stream, source)
     FILE *stream;
     Lisp_Object source;
{
  register Lisp_Object tail, prev, newelt;
  register Lisp_Object tem, tem2;
  register int foundit, loading;

  /* Don't bother recording anything for preloaded files.  */
  if (!NILP (Vpurify_flag))
    return;

  loading = stream || !NARROWED;

  tail = Vload_history;
  prev = Qnil;
  foundit = 0;
  while (!NILP (tail))
    {
      tem = Fcar (tail);

      /* Find the feature's previous assoc list... */
      if (!NILP (Fequal (source, Fcar (tem))))
	{
	  foundit = 1;

	  /*  If we're loading, remove it. */
	  if (loading)
	    {	  
	      if (NILP (prev))
		Vload_history = Fcdr (tail);
	      else
		Fsetcdr (prev, Fcdr (tail));
	    }

	  /*  Otherwise, cons on new symbols that are not already members.  */
	  else
	    {
	      tem2 = Vcurrent_load_list;

	      while (CONSP (tem2))
		{
		  newelt = Fcar (tem2);

		  if (NILP (Fmemq (newelt, tem)))
		    Fsetcar (tail, Fcons (Fcar (tem),
					  Fcons (newelt, Fcdr (tem))));

		  tem2 = Fcdr (tem2);
		  QUIT;
		}
	    }
	}
      else
	prev = tail;
      tail = Fcdr (tail);
      QUIT;
    }

  /* If we're loading, cons the new assoc onto the front of load-history,
     the most-recently-loaded position.  Also do this if we didn't find
     an existing member for the current source.  */
  if (loading || !foundit)
    Vload_history = Fcons (Fnreverse (Vcurrent_load_list),
			   Vload_history);
}

Lisp_Object
unreadpure ()	/* Used as unwind-protect function in readevalloop */
{
  read_pure = 0;
  return Qnil;
}

static void
readevalloop (readcharfun, stream, sourcename, evalfun, printflag)
     Lisp_Object readcharfun;
     FILE *stream;
     Lisp_Object sourcename;
     Lisp_Object (*evalfun) ();
     int printflag;
{
  register int c;
  register Lisp_Object val;
  int count = specpdl_ptr - specpdl;
  struct gcpro gcpro1;
  struct buffer *b = 0;

  if (BUFFERP (readcharfun))
    b = XBUFFER (readcharfun);
  else if (MARKERP (readcharfun))
    b = XMARKER (readcharfun)->buffer;

  specbind (Qstandard_input, readcharfun);
  specbind (Qcurrent_load_list, Qnil);

  GCPRO1 (sourcename);

  LOADHIST_ATTACH (sourcename);

  while (1)
    {
      if (b != 0 && NILP (b->name))
	error ("Reading from killed buffer");

      instream = stream;
      c = READCHAR;
      if (c == ';')
	{
	  while ((c = READCHAR) != '\n' && c != -1);
	  continue;
	}
      if (c < 0) break;

      /* Ignore whitespace here, so we can detect eof.  */
      if (c == ' ' || c == '\t' || c == '\n' || c == '\f' || c == '\r')
	continue;

      if (!NILP (Vpurify_flag) && c == '(')
	{
	  int count1 = specpdl_ptr - specpdl;
	  record_unwind_protect (unreadpure, Qnil);
	  val = read_list (-1, readcharfun);
	  unbind_to (count1, Qnil);
	}
      else
	{
	  UNREAD (c);
	  val = read0 (readcharfun);
	}

      val = (*evalfun) (val);
      if (printflag)
	{
	  Vvalues = Fcons (val, Vvalues);
	  if (EQ (Vstandard_output, Qt))
	    Fprin1 (val, Qnil);
	  else
	    Fprint (val, Qnil);
	}
    }

  build_load_history (stream, sourcename);
  UNGCPRO;

  unbind_to (count, Qnil);
}

#ifndef standalone

DEFUN ("eval-buffer", Feval_buffer, Seval_buffer, 0, 2, "",
  "Execute the current buffer as Lisp code.\n\
Programs can pass two arguments, BUFFER and PRINTFLAG.\n\
BUFFER is the buffer to evaluate (nil means use current buffer).\n\
PRINTFLAG controls printing of output:\n\
nil means discard it; anything else is stream for print.\n\
\n\
If there is no error, point does not move.  If there is an error,\n\
point remains at the end of the last character read from the buffer.")
  (bufname, printflag)
     Lisp_Object bufname, printflag;
{
  int count = specpdl_ptr - specpdl;
  Lisp_Object tem, buf;

  if (NILP (bufname))
    buf = Fcurrent_buffer ();
  else
    buf = Fget_buffer (bufname);
  if (NILP (buf))
    error ("No such buffer.");

  if (NILP (printflag))
    tem = Qsymbolp;
  else
    tem = printflag;
  specbind (Qstandard_output, tem);
  record_unwind_protect (save_excursion_restore, save_excursion_save ());
  BUF_SET_PT (XBUFFER (buf), BUF_BEGV (XBUFFER (buf)));
  readevalloop (buf, 0, XBUFFER (buf)->filename, Feval, !NILP (printflag));
  unbind_to (count, Qnil);

  return Qnil;
}

#if 0
DEFUN ("eval-current-buffer", Feval_current_buffer, Seval_current_buffer, 0, 1, "",
  "Execute the current buffer as Lisp code.\n\
Programs can pass argument PRINTFLAG which controls printing of output:\n\
nil means discard it; anything else is stream for print.\n\
\n\
If there is no error, point does not move.  If there is an error,\n\
point remains at the end of the last character read from the buffer.")
  (printflag)
     Lisp_Object printflag;
{
  int count = specpdl_ptr - specpdl;
  Lisp_Object tem, cbuf;

  cbuf = Fcurrent_buffer ()

  if (NILP (printflag))
    tem = Qsymbolp;
  else
    tem = printflag;
  specbind (Qstandard_output, tem);
  record_unwind_protect (save_excursion_restore, save_excursion_save ());
  SET_PT (BEGV);
  readevalloop (cbuf, 0, XBUFFER (cbuf)->filename, Feval, !NILP (printflag));
  return unbind_to (count, Qnil);
}
#endif

DEFUN ("eval-region", Feval_region, Seval_region, 2, 3, "r",
  "Execute the region as Lisp code.\n\
When called from programs, expects two arguments,\n\
giving starting and ending indices in the current buffer\n\
of the text to be executed.\n\
Programs can pass third argument PRINTFLAG which controls output:\n\
nil means discard it; anything else is stream for printing it.\n\
\n\
If there is no error, point does not move.  If there is an error,\n\
point remains at the end of the last character read from the buffer.")
  (b, e, printflag)
     Lisp_Object b, e, printflag;
{
  int count = specpdl_ptr - specpdl;
  Lisp_Object tem, cbuf;

  cbuf = Fcurrent_buffer ();

  if (NILP (printflag))
    tem = Qsymbolp;
  else
    tem = printflag;
  specbind (Qstandard_output, tem);

  if (NILP (printflag))
    record_unwind_protect (save_excursion_restore, save_excursion_save ());
  record_unwind_protect (save_restriction_restore, save_restriction_save ());

  /* This both uses b and checks its type.  */
  Fgoto_char (b);
  Fnarrow_to_region (make_number (BEGV), e);
  readevalloop (cbuf, 0, XBUFFER (cbuf)->filename, Feval, !NILP (printflag));

  return unbind_to (count, Qnil);
}

#endif /* standalone */

DEFUN ("read", Fread, Sread, 0, 1, 0,
  "Read one Lisp expression as text from STREAM, return as Lisp object.\n\
If STREAM is nil, use the value of `standard-input' (which see).\n\
STREAM or the value of `standard-input' may be:\n\
 a buffer (read from point and advance it)\n\
 a marker (read from where it points and advance it)\n\
 a function (call it with no arguments for each character,\n\
     call it with a char as argument to push a char back)\n\
 a string (takes text from string, starting at the beginning)\n\
 t (read text line using minibuffer and use it).")
  (readcharfun)
     Lisp_Object readcharfun;
{
  extern Lisp_Object Fread_minibuffer ();

  if (NILP (readcharfun))
    readcharfun = Vstandard_input;
  if (EQ (readcharfun, Qt))
    readcharfun = Qread_char;

#ifndef standalone
  if (EQ (readcharfun, Qread_char))
    return Fread_minibuffer (build_string ("Lisp expression: "), Qnil);
#endif

  if (STRINGP (readcharfun))
    return Fcar (Fread_from_string (readcharfun, Qnil, Qnil));

  return read0 (readcharfun);
}

DEFUN ("read-from-string", Fread_from_string, Sread_from_string, 1, 3, 0,
  "Read one Lisp expression which is represented as text by STRING.\n\
Returns a cons: (OBJECT-READ . FINAL-STRING-INDEX).\n\
START and END optionally delimit a substring of STRING from which to read;\n\
 they default to 0 and (length STRING) respectively.")
  (string, start, end)
     Lisp_Object string, start, end;
{
  int startval, endval;
  Lisp_Object tem;

  CHECK_STRING (string,0);

  if (NILP (end))
    endval = XSTRING (string)->size;
  else
    { CHECK_NUMBER (end,2);
      endval = XINT (end);
      if (endval < 0 || endval > XSTRING (string)->size)
	args_out_of_range (string, end);
    }

  if (NILP (start))
    startval = 0;
  else
    { CHECK_NUMBER (start,1);
      startval = XINT (start);
      if (startval < 0 || startval > endval)
	args_out_of_range (string, start);
    }

  read_from_string_index = startval;
  read_from_string_limit = endval;

  tem = read0 (string);
  return Fcons (tem, make_number (read_from_string_index));
}

/* Use this for recursive reads, in contexts where internal tokens
   are not allowed. */
static Lisp_Object
read0 (readcharfun)
     Lisp_Object readcharfun;
{
  register Lisp_Object val;
  char c;

  val = read1 (readcharfun, &c);
  if (c)
    Fsignal (Qinvalid_read_syntax, Fcons (make_string (&c, 1), Qnil));

  return val;
}

static int read_buffer_size;
static char *read_buffer;

static int
read_escape (readcharfun)
     Lisp_Object readcharfun;
{
  register int c = READCHAR;
  switch (c)
    {
    case 'a':
      return '\007';
    case 'b':
      return '\b';
    case 'd':
      return 0177;
    case 'e':
      return 033;
    case 'f':
      return '\f';
    case 'n':
      return '\n';
    case 'r':
      return '\r';
    case 't':
      return '\t';
    case 'v':
      return '\v';
    case '\n':
      return -1;

    case 'M':
      c = READCHAR;
      if (c != '-')
	error ("Invalid escape character syntax");
      c = READCHAR;
      if (c == '\\')
	c = read_escape (readcharfun);
      return c | meta_modifier;

    case 'S':
      c = READCHAR;
      if (c != '-')
	error ("Invalid escape character syntax");
      c = READCHAR;
      if (c == '\\')
	c = read_escape (readcharfun);
      if ((c & 0xff) >= 'a' && (c & 0xff) <= 'z')
	return c - ('a' - 'A');
      return c | shift_modifier;

    case 'H':
      c = READCHAR;
      if (c != '-')
	error ("Invalid escape character syntax");
      c = READCHAR;
      if (c == '\\')
	c = read_escape (readcharfun);
      return c | hyper_modifier;

    case 'A':
      c = READCHAR;
      if (c != '-')
	error ("Invalid escape character syntax");
      c = READCHAR;
      if (c == '\\')
	c = read_escape (readcharfun);
      return c | alt_modifier;

    case 's':
      c = READCHAR;
      if (c != '-')
	error ("Invalid escape character syntax");
      c = READCHAR;
      if (c == '\\')
	c = read_escape (readcharfun);
      return c | super_modifier;

    case 'C':
      c = READCHAR;
      if (c != '-')
	error ("Invalid escape character syntax");
    case '^':
      c = READCHAR;
      if (c == '\\')
	c = read_escape (readcharfun);
      if ((c & 0177) == '?')
	return 0177 | c;
      /* ASCII control chars are made from letters (both cases),
	 as well as the non-letters within 0100...0137.  */
      else if ((c & 0137) >= 0101 && (c & 0137) <= 0132)
	return (c & (037 | ~0177));
      else if ((c & 0177) >= 0100 && (c & 0177) <= 0137)
	return (c & (037 | ~0177));
      else
	return c | ctrl_modifier;

    case '0':
    case '1':
    case '2':
    case '3':
    case '4':
    case '5':
    case '6':
    case '7':
      /* An octal escape, as in ANSI C.  */
      {
	register int i = c - '0';
	register int count = 0;
	while (++count < 3)
	  {
	    if ((c = READCHAR) >= '0' && c <= '7')
	      {
		i *= 8;
		i += c - '0';
	      }
	    else
	      {
		UNREAD (c);
		break;
	      }
	  }
	return i;
      }

    case 'x':
      /* A hex escape, as in ANSI C.  */
      {
	int i = 0;
	while (1)
	  {
	    c = READCHAR;
	    if (c >= '0' && c <= '9')
	      {
		i *= 16;
		i += c - '0';
	      }
	    else if ((c >= 'a' && c <= 'f')
		     || (c >= 'A' && c <= 'F'))
	      {
		i *= 16;
		if (c >= 'a' && c <= 'f')
		  i += c - 'a' + 10;
		else
		  i += c - 'A' + 10;
	      }
	    else
	      {
		UNREAD (c);
		break;
	      }
	  }
	return i;
      }

    default:
      return c;
    }
}

/* If the next token is ')' or ']' or '.', we store that character
   in *PCH and the return value is not interesting.  Else, we store
   zero in *PCH and we read and return one lisp object.  */
static Lisp_Object
read1 (readcharfun, pch)
     register Lisp_Object readcharfun;
     char *pch;
{
  register int c;
  *pch = 0;

 retry:

  c = READCHAR;
  if (c < 0) return Fsignal (Qend_of_file, Qnil);

  switch (c)
    {
    case '(':
      return read_list (0, readcharfun);

    case '[':
      return read_vector (readcharfun);

    case ')':
    case ']':
      {
	*pch = c;
	return Qnil;
      }

    case '#':
      c = READCHAR;
      if (c == '[')
	{
	  /* Accept compiled functions at read-time so that we don't have to
	     build them using function calls.  */
	  Lisp_Object tmp;
	  tmp = read_vector (readcharfun);
	  return Fmake_byte_code (XVECTOR (tmp)->size,
				  XVECTOR (tmp)->contents);
	}
#ifdef USE_TEXT_PROPERTIES
      if (c == '(')
	{
	  Lisp_Object tmp;
	  struct gcpro gcpro1;
	  char ch;

	  /* Read the string itself.  */
	  tmp = read1 (readcharfun, &ch);
	  if (ch != 0 || !STRINGP (tmp))
	    Fsignal (Qinvalid_read_syntax, Fcons (make_string ("#", 1), Qnil));
	  GCPRO1 (tmp);
	  /* Read the intervals and their properties.  */
	  while (1)
	    {
	      Lisp_Object beg, end, plist;

	      beg = read1 (readcharfun, &ch);
	      if (ch == ')')
		break;
	      if (ch == 0)
		end = read1 (readcharfun, &ch);
	      if (ch == 0)
		plist = read1 (readcharfun, &ch);
	      if (ch)
		Fsignal (Qinvalid_read_syntax,
			 Fcons (build_string ("invalid string property list"),
				Qnil));
	      Fset_text_properties (beg, end, plist, tmp);
	    }
	  UNGCPRO;
	  return tmp;
	}
#endif
      /* #@NUMBER is used to skip NUMBER following characters.
	 That's used in .elc files to skip over doc strings
	 and function definitions.  */
      if (c == '@')
	{
	  int i, nskip = 0;

	  /* Read a decimal integer.  */
	  while ((c = READCHAR) >= 0
		 && c >= '0' && c <= '9')
	    {
	      nskip *= 10;
	      nskip += c - '0';
	    }
	  if (c >= 0)
	    UNREAD (c);
	  
	  /* Skip that many characters.  */
	  for (i = 0; i < nskip && c >= 0; i++)
	    c = READCHAR;
	  goto retry;
	}
      if (c == '$')
	return Vload_file_name;

      UNREAD (c);
      Fsignal (Qinvalid_read_syntax, Fcons (make_string ("#", 1), Qnil));

    case ';':
      while ((c = READCHAR) >= 0 && c != '\n');
      goto retry;

    case '\'':
      {
	return Fcons (Qquote, Fcons (read0 (readcharfun), Qnil));
      }

    case '?':
      {
	register Lisp_Object val;

	c = READCHAR;
	if (c < 0) return Fsignal (Qend_of_file, Qnil);

	if (c == '\\')
	  XSETINT (val, read_escape (readcharfun));
	else
	  XSETINT (val, c);

	return val;
      }

    case '\"':
      {
	register char *p = read_buffer;
	register char *end = read_buffer + read_buffer_size;
	register int c;
	int cancel = 0;

	while ((c = READCHAR) >= 0
	       && c != '\"')
	  {
	    if (p == end)
	      {
		char *new = (char *) xrealloc (read_buffer, read_buffer_size *= 2);
		p += new - read_buffer;
		read_buffer += new - read_buffer;
		end = read_buffer + read_buffer_size;
	      }
	    if (c == '\\')
	      c = read_escape (readcharfun);
	    /* c is -1 if \ newline has just been seen */
	    if (c == -1)
	      {
		if (p == read_buffer)
		  cancel = 1;
	      }
	    else
	      {
		/* Allow `\C- ' and `\C-?'.  */
		if (c == (CHAR_CTL | ' '))
		  c = 0;
		else if (c == (CHAR_CTL | '?'))
		  c = 127;

		if (c & CHAR_META)
		  /* Move the meta bit to the right place for a string.  */
		  c = (c & ~CHAR_META) | 0x80;
		if (c & ~0xff)
		  error ("Invalid modifier in string");
		*p++ = c;
	      }
	  }
	if (c < 0) return Fsignal (Qend_of_file, Qnil);

	/* If purifying, and string starts with \ newline,
	   return zero instead.  This is for doc strings
	   that we are really going to find in etc/DOC.nn.nn  */
	if (!NILP (Vpurify_flag) && NILP (Vdoc_file_name) && cancel)
	  return make_number (0);

	if (read_pure)
	  return make_pure_string (read_buffer, p - read_buffer);
	else
	  return make_string (read_buffer, p - read_buffer);
      }

    case '.':
      {
#ifdef LISP_FLOAT_TYPE
	/* If a period is followed by a number, then we should read it
	   as a floating point number.  Otherwise, it denotes a dotted
	   pair.  */
	int next_char = READCHAR;
	UNREAD (next_char);

	if (! (next_char >= '0' && next_char <= '9'))
#endif
	  {
	    *pch = c;
	    return Qnil;
	  }

	/* Otherwise, we fall through!  Note that the atom-reading loop
	   below will now loop at least once, assuring that we will not
	   try to UNREAD two characters in a row.  */
      }
    default:
      if (c <= 040) goto retry;
      {
	register char *p = read_buffer;
	int quoted = 0;

	{
	  register char *end = read_buffer + read_buffer_size;

	  while (c > 040 && 
		 !(c == '\"' || c == '\'' || c == ';' || c == '?'
		   || c == '(' || c == ')'
#ifndef LISP_FLOAT_TYPE
		   /* If we have floating-point support, then we need
		      to allow <digits><dot><digits>.  */
		   || c =='.'
#endif /* not LISP_FLOAT_TYPE */
		   || c == '[' || c == ']' || c == '#'
		   ))
	    {
	      if (p == end)
		{
		  register char *new = (char *) xrealloc (read_buffer, read_buffer_size *= 2);
		  p += new - read_buffer;
		  read_buffer += new - read_buffer;
		  end = read_buffer + read_buffer_size;
		}
	      if (c == '\\')
		{
		  c = READCHAR;
		  quoted = 1;
		}
	      *p++ = c;
	      c = READCHAR;
	    }

	  if (p == end)
	    {
	      char *new = (char *) xrealloc (read_buffer, read_buffer_size *= 2);
	      p += new - read_buffer;
	      read_buffer += new - read_buffer;
/*	      end = read_buffer + read_buffer_size;  */
	    }
	  *p = 0;
	  if (c >= 0)
	    UNREAD (c);
	}

	if (!quoted)
	  {
	    register char *p1;
	    register Lisp_Object val;
	    p1 = read_buffer;
	    if (*p1 == '+' || *p1 == '-') p1++;
	    /* Is it an integer? */
	    if (p1 != p)
	      {
		while (p1 != p && (c = *p1) >= '0' && c <= '9') p1++;
#ifdef LISP_FLOAT_TYPE
		/* Integers can have trailing decimal points.  */
		if (p1 > read_buffer && p1 < p && *p1 == '.') p1++;
#endif
		if (p1 == p)
		  /* It is an integer. */
		  {
#ifdef LISP_FLOAT_TYPE
		    if (p1[-1] == '.')
		      p1[-1] = '\0';
#endif
		    XSETINT (val, atoi (read_buffer));
		    return val;
		  }
	      }
#ifdef LISP_FLOAT_TYPE
	    if (isfloat_string (read_buffer))
	      return make_float (atof (read_buffer));
#endif
	  }

	return intern (read_buffer);
      }
    }
}

#ifdef LISP_FLOAT_TYPE

#define LEAD_INT 1
#define DOT_CHAR 2
#define TRAIL_INT 4
#define E_CHAR 8
#define EXP_INT 16

int
isfloat_string (cp)
     register char *cp;
{
  register state;
  
  state = 0;
  if (*cp == '+' || *cp == '-')
    cp++;

  if (*cp >= '0' && *cp <= '9')
    {
      state |= LEAD_INT;
      while (*cp >= '0' && *cp <= '9')
	cp++;
    }
  if (*cp == '.')
    {
      state |= DOT_CHAR;
      cp++;
    }
  if (*cp >= '0' && *cp <= '9')
    {
      state |= TRAIL_INT;
      while (*cp >= '0' && *cp <= '9')
	cp++;
    }
  if (*cp == 'e')
    {
      state |= E_CHAR;
      cp++;
    }
  if ((*cp == '+') || (*cp == '-'))
    cp++;

  if (*cp >= '0' && *cp <= '9')
    {
      state |= EXP_INT;
      while (*cp >= '0' && *cp <= '9')
	cp++;
    }
  return (*cp == 0
	  && (state == (LEAD_INT|DOT_CHAR|TRAIL_INT)
	      || state == (DOT_CHAR|TRAIL_INT)
	      || state == (LEAD_INT|E_CHAR|EXP_INT)
	      || state == (LEAD_INT|DOT_CHAR|TRAIL_INT|E_CHAR|EXP_INT)
	      || state == (DOT_CHAR|TRAIL_INT|E_CHAR|EXP_INT)));
}
#endif /* LISP_FLOAT_TYPE */

static Lisp_Object
read_vector (readcharfun)
     Lisp_Object readcharfun;
{
  register int i;
  register int size;
  register Lisp_Object *ptr;
  register Lisp_Object tem, vector;
  register struct Lisp_Cons *otem;
  Lisp_Object len;

  tem = read_list (1, readcharfun);
  len = Flength (tem);
  vector = (read_pure ? make_pure_vector (XINT (len)) : Fmake_vector (len, Qnil));


  size = XVECTOR (vector)->size;
  ptr = XVECTOR (vector)->contents;
  for (i = 0; i < size; i++)
    {
      ptr[i] = read_pure ? Fpurecopy (Fcar (tem)) : Fcar (tem);
      otem = XCONS (tem);
      tem = Fcdr (tem);
      free_cons (otem);
    }
  return vector;
}
  
/* flag = 1 means check for ] to terminate rather than ) and .
   flag = -1 means check for starting with defun
    and make structure pure.  */

static Lisp_Object
read_list (flag, readcharfun)
     int flag;
     register Lisp_Object readcharfun;
{
  /* -1 means check next element for defun,
     0 means don't check,
     1 means already checked and found defun. */
  int defunflag = flag < 0 ? -1 : 0;
  Lisp_Object val, tail;
  register Lisp_Object elt, tem;
  struct gcpro gcpro1, gcpro2;
  int cancel = 0;

  val = Qnil;
  tail = Qnil;

  while (1)
    {
      char ch;
      GCPRO2 (val, tail);
      elt = read1 (readcharfun, &ch);
      UNGCPRO;

	/* If purifying, and the list starts with #$,
	   return 0 instead.  This is a doc string reference
	   and it will be replaced anyway by Snarf-documentation,
	   so don't waste pure space with it.  */
      if (EQ (elt, Vload_file_name)
	  && !NILP (Vpurify_flag) && NILP (Vdoc_file_name))
	cancel = 1;

      if (ch)
	{
	  if (flag > 0)
	    {
	      if (ch == ']')
		return val;
	      Fsignal (Qinvalid_read_syntax, Fcons (make_string (") or . in a vector", 18), Qnil));
	    }
	  if (ch == ')')
	    return val;
	  if (ch == '.')
	    {
	      GCPRO2 (val, tail);
	      if (!NILP (tail))
		XCONS (tail)->cdr = read0 (readcharfun);
	      else
		val = read0 (readcharfun);
	      read1 (readcharfun, &ch);
	      UNGCPRO;
	      if (ch == ')')
		return (cancel ? make_number (0) : val);
	      return Fsignal (Qinvalid_read_syntax, Fcons (make_string (". in wrong context", 18), Qnil));
	    }
	  return Fsignal (Qinvalid_read_syntax, Fcons (make_string ("] in a list", 11), Qnil));
	}
      tem = (read_pure && flag <= 0
	     ? pure_cons (elt, Qnil)
	     : Fcons (elt, Qnil));
      if (!NILP (tail))
	XCONS (tail)->cdr = tem;
      else
	val = tem;
      tail = tem;
      if (defunflag < 0)
	defunflag = EQ (elt, Qdefun);
      else if (defunflag > 0)
	read_pure = 1;
    }
}

Lisp_Object Vobarray;
Lisp_Object initial_obarray;

Lisp_Object
check_obarray (obarray)
     Lisp_Object obarray;
{
  while (!VECTORP (obarray) || XVECTOR (obarray)->size == 0)
    {
      /* If Vobarray is now invalid, force it to be valid.  */
      if (EQ (Vobarray, obarray)) Vobarray = initial_obarray;

      obarray = wrong_type_argument (Qvectorp, obarray);
    }
  return obarray;
}

static int hash_string ();
Lisp_Object oblookup ();

Lisp_Object
intern (str)
     char *str;
{
  Lisp_Object tem;
  int len = strlen (str);
  Lisp_Object obarray;

  obarray = Vobarray;
  if (!VECTORP (obarray) || XVECTOR (obarray)->size == 0)
    obarray = check_obarray (obarray);
  tem = oblookup (obarray, str, len);
  if (SYMBOLP (tem))
    return tem;
  return Fintern ((!NILP (Vpurify_flag)
		   ? make_pure_string (str, len)
		   : make_string (str, len)),
		  obarray);
}

DEFUN ("intern", Fintern, Sintern, 1, 2, 0,
  "Return the canonical symbol whose name is STRING.\n\
If there is none, one is created by this function and returned.\n\
A second optional argument specifies the obarray to use;\n\
it defaults to the value of `obarray'.")
  (str, obarray)
     Lisp_Object str, obarray;
{
  register Lisp_Object tem, sym, *ptr;

  if (NILP (obarray)) obarray = Vobarray;
  obarray = check_obarray (obarray);

  CHECK_STRING (str, 0);

  tem = oblookup (obarray, XSTRING (str)->data, XSTRING (str)->size);
  if (!INTEGERP (tem))
    return tem;

  if (!NILP (Vpurify_flag))
    str = Fpurecopy (str);
  sym = Fmake_symbol (str);

  ptr = &XVECTOR (obarray)->contents[XINT (tem)];
  if (SYMBOLP (*ptr))
    XSYMBOL (sym)->next = XSYMBOL (*ptr);
  else
    XSYMBOL (sym)->next = 0;
  *ptr = sym;
  return sym;
}

DEFUN ("intern-soft", Fintern_soft, Sintern_soft, 1, 2, 0,
  "Return the canonical symbol whose name is STRING, or nil if none exists.\n\
A second optional argument specifies the obarray to use;\n\
it defaults to the value of `obarray'.")
  (str, obarray)
     Lisp_Object str, obarray;
{
  register Lisp_Object tem;

  if (NILP (obarray)) obarray = Vobarray;
  obarray = check_obarray (obarray);

  CHECK_STRING (str, 0);

  tem = oblookup (obarray, XSTRING (str)->data, XSTRING (str)->size);
  if (!INTEGERP (tem))
    return tem;
  return Qnil;
}

Lisp_Object
oblookup (obarray, ptr, size)
     Lisp_Object obarray;
     register char *ptr;
     register int size;
{
  int hash;
  int obsize;
  register Lisp_Object tail;
  Lisp_Object bucket, tem;

  if (!VECTORP (obarray)
      || (obsize = XVECTOR (obarray)->size) == 0)
    {
      obarray = check_obarray (obarray);
      obsize = XVECTOR (obarray)->size;
    }
  /* Combining next two lines breaks VMS C 2.3.  */
  hash = hash_string (ptr, size);
  hash %= obsize;
  bucket = XVECTOR (obarray)->contents[hash];
  if (XFASTINT (bucket) == 0)
    ;
  else if (!SYMBOLP (bucket))
    error ("Bad data in guts of obarray"); /* Like CADR error message */
  else for (tail = bucket; ; XSETSYMBOL (tail, XSYMBOL (tail)->next))
      {
	if (XSYMBOL (tail)->name->size == size &&
	    !bcmp (XSYMBOL (tail)->name->data, ptr, size))
	  return tail;
	else if (XSYMBOL (tail)->next == 0)
	  break;
      }
  XSETINT (tem, hash);
  return tem;
}

static int
hash_string (ptr, len)
     unsigned char *ptr;
     int len;
{
  register unsigned char *p = ptr;
  register unsigned char *end = p + len;
  register unsigned char c;
  register int hash = 0;

  while (p != end)
    {
      c = *p++;
      if (c >= 0140) c -= 40;
      hash = ((hash<<3) + (hash>>28) + c);
    }
  return hash & 07777777777;
}

void
map_obarray (obarray, fn, arg)
     Lisp_Object obarray;
     int (*fn) ();
     Lisp_Object arg;
{
  register int i;
  register Lisp_Object tail;
  CHECK_VECTOR (obarray, 1);
  for (i = XVECTOR (obarray)->size - 1; i >= 0; i--)
    {
      tail = XVECTOR (obarray)->contents[i];
      if (XFASTINT (tail) != 0)
	while (1)
	  {
	    (*fn) (tail, arg);
	    if (XSYMBOL (tail)->next == 0)
	      break;
	    XSETSYMBOL (tail, XSYMBOL (tail)->next);
	  }
    }
}

mapatoms_1 (sym, function)
     Lisp_Object sym, function;
{
  call1 (function, sym);
}

DEFUN ("mapatoms", Fmapatoms, Smapatoms, 1, 2, 0,
  "Call FUNCTION on every symbol in OBARRAY.\n\
OBARRAY defaults to the value of `obarray'.")
  (function, obarray)
     Lisp_Object function, obarray;
{
  Lisp_Object tem;

  if (NILP (obarray)) obarray = Vobarray;
  obarray = check_obarray (obarray);

  map_obarray (obarray, mapatoms_1, function);
  return Qnil;
}

#define OBARRAY_SIZE 1511

void
init_obarray ()
{
  Lisp_Object oblength;
  int hash;
  Lisp_Object *tem;

  XSETFASTINT (oblength, OBARRAY_SIZE);

  Qnil = Fmake_symbol (make_pure_string ("nil", 3));
  Vobarray = Fmake_vector (oblength, make_number (0));
  initial_obarray = Vobarray;
  staticpro (&initial_obarray);
  /* Intern nil in the obarray */
  /* These locals are to kludge around a pyramid compiler bug. */
  hash = hash_string ("nil", 3);
  /* Separate statement here to avoid VAXC bug. */
  hash %= OBARRAY_SIZE;
  tem = &XVECTOR (Vobarray)->contents[hash];
  *tem = Qnil;

  Qunbound = Fmake_symbol (make_pure_string ("unbound", 7));
  XSYMBOL (Qnil)->function = Qunbound;
  XSYMBOL (Qunbound)->value = Qunbound;
  XSYMBOL (Qunbound)->function = Qunbound;

  Qt = intern ("t");
  XSYMBOL (Qnil)->value = Qnil;
  XSYMBOL (Qnil)->plist = Qnil;
  XSYMBOL (Qt)->value = Qt;

  /* Qt is correct even if CANNOT_DUMP.  loadup.el will set to nil at end.  */
  Vpurify_flag = Qt;

  Qvariable_documentation = intern ("variable-documentation");

  read_buffer_size = 100;
  read_buffer = (char *) malloc (read_buffer_size);
}

void
defsubr (sname)
     struct Lisp_Subr *sname;
{
  Lisp_Object sym;
  sym = intern (sname->symbol_name);
  XSETSUBR (XSYMBOL (sym)->function, sname);
}

#ifdef NOTDEF /* use fset in subr.el now */
void
defalias (sname, string)
     struct Lisp_Subr *sname;
     char *string;
{
  Lisp_Object sym;
  sym = intern (string);
  XSETSUBR (XSYMBOL (sym)->function, sname);
}
#endif /* NOTDEF */

/* Define an "integer variable"; a symbol whose value is forwarded
   to a C variable of type int.  Sample call: */
  /* DEFVARINT ("indent-tabs-mode", &indent_tabs_mode, "Documentation");  */
void
defvar_int (namestring, address)
     char *namestring;
     int *address;
{
  Lisp_Object sym, val;
  sym = intern (namestring);
  val = allocate_misc ();
  XMISC (val)->type = Lisp_Misc_Intfwd;
  XINTFWD (val)->intvar = address;
  XSYMBOL (sym)->value = val;
}

/* Similar but define a variable whose value is T if address contains 1,
   NIL if address contains 0 */
void
defvar_bool (namestring, address)
     char *namestring;
     int *address;
{
  Lisp_Object sym, val;
  sym = intern (namestring);
  val = allocate_misc ();
  XMISC (val)->type = Lisp_Misc_Boolfwd;
  XBOOLFWD (val)->boolvar = address;
  XSYMBOL (sym)->value = val;
}

/* Similar but define a variable whose value is the Lisp Object stored
   at address.  Two versions: with and without gc-marking of the C
   variable.  The nopro version is used when that variable will be
   gc-marked for some other reason, since marking the same slot twice
   can cause trouble with strings.  */
void
defvar_lisp_nopro (namestring, address)
     char *namestring;
     Lisp_Object *address;
{
  Lisp_Object sym, val;
  sym = intern (namestring);
  val = allocate_misc ();
  XMISC (val)->type = Lisp_Misc_Objfwd;
  XOBJFWD (val)->objvar = address;
  XSYMBOL (sym)->value = val;
}

void
defvar_lisp (namestring, address)
     char *namestring;
     Lisp_Object *address;
{
  defvar_lisp_nopro (namestring, address);
  staticpro (address);
}

#ifndef standalone

/* Similar but define a variable whose value is the Lisp Object stored in
   the current buffer.  address is the address of the slot in the buffer
   that is current now. */

void
defvar_per_buffer (namestring, address, type, doc)
     char *namestring;
     Lisp_Object *address;
     Lisp_Object type;
     char *doc;
{
  Lisp_Object sym, val;
  int offset;
  extern struct buffer buffer_local_symbols;

  sym = intern (namestring);
  val = allocate_misc ();
  offset = (char *)address - (char *)current_buffer;

  XMISC (val)->type = Lisp_Misc_Buffer_Objfwd;
  XBUFFER_OBJFWD (val)->offset = offset;
  XSYMBOL (sym)->value = val;
  *(Lisp_Object *)(offset + (char *)&buffer_local_symbols) = sym;
  *(Lisp_Object *)(offset + (char *)&buffer_local_types) = type;
  if (XINT (*(Lisp_Object *)(offset + (char *)&buffer_local_flags)) == 0)
    /* Did a DEFVAR_PER_BUFFER without initializing the corresponding
       slot of buffer_local_flags */
    abort ();
}

#endif /* standalone */

init_lread ()
{
  char *normal;

  /* Compute the default load-path.  */
#ifdef CANNOT_DUMP
  normal = PATH_LOADSEARCH;
  Vload_path = decode_env_path (0, normal);
#else
  if (NILP (Vpurify_flag))
    normal = PATH_LOADSEARCH;
  else
    normal = PATH_DUMPLOADSEARCH;

  /* In a dumped Emacs, we normally have to reset the value of
     Vload_path from PATH_LOADSEARCH, since the value that was dumped
     uses ../lisp, instead of the path of the installed elisp
     libraries.  However, if it appears that Vload_path was changed
     from the default before dumping, don't override that value.  */
  if (initialized)
    {
      Lisp_Object dump_path;

      dump_path = decode_env_path (0, PATH_DUMPLOADSEARCH);
      if (! NILP (Fequal (dump_path, Vload_path)))
	{
	  Vload_path = decode_env_path (0, normal);
	  if (!NILP (Vinstallation_directory))
	    {
	      /* Add to the path the lisp subdir of the
		 installation dir, if it exists.  */
	      Lisp_Object tem, tem1;
	      tem = Fexpand_file_name (build_string ("lisp"),
				       Vinstallation_directory);
	      tem1 = Ffile_exists_p (tem);
	      if (!NILP (tem1))
		{
		  if (NILP (Fmember (tem, Vload_path)))
		    Vload_path = nconc2 (Vload_path, Fcons (tem, Qnil));
		}
	      else
		/* That dir doesn't exist, so add the build-time
		   Lisp dirs instead.  */
		Vload_path = nconc2 (Vload_path, dump_path);
	    }
	}
    }
  else
    Vload_path = decode_env_path (0, normal);
#endif

#ifndef WINDOWSNT
  /* When Emacs is invoked over network shares on NT, PATH_LOADSEARCH is 
     almost never correct, thereby causing a warning to be printed out that 
     confuses users.  Since PATH_LOADSEARCH is always overriden by the
     EMACSLOADPATH environment variable below, disable the warning on NT.  */

  /* Warn if dirs in the *standard* path don't exist.  */
  {
    Lisp_Object path_tail;

    for (path_tail = Vload_path;
	 !NILP (path_tail);
	 path_tail = XCONS (path_tail)->cdr)
      {
	Lisp_Object dirfile;
	dirfile = Fcar (path_tail);
	if (STRINGP (dirfile))
	  {
	    dirfile = Fdirectory_file_name (dirfile);
	    if (access (XSTRING (dirfile)->data, 0) < 0)
	      fprintf (stderr,
		       "Warning: Lisp directory `%s' does not exist.\n",
		       XSTRING (Fcar (path_tail))->data);
	  }
      }
  }
#endif /* WINDOWSNT */

  /* If the EMACSLOADPATH environment variable is set, use its value.
     This doesn't apply if we're dumping.  */
  if (NILP (Vpurify_flag)
      && egetenv ("EMACSLOADPATH"))
    Vload_path = decode_env_path ("EMACSLOADPATH", normal);

  Vvalues = Qnil;

  load_in_progress = 0;

  load_descriptor_list = Qnil;
}

void
syms_of_lread ()
{
  defsubr (&Sread);
  defsubr (&Sread_from_string);
  defsubr (&Sintern);
  defsubr (&Sintern_soft);
  defsubr (&Sload);
  defsubr (&Seval_buffer);
  defsubr (&Seval_region);
  defsubr (&Sread_char);
  defsubr (&Sread_char_exclusive);
  defsubr (&Sread_event);
  defsubr (&Sget_file_char);
  defsubr (&Smapatoms);

  DEFVAR_LISP ("obarray", &Vobarray,
    "Symbol table for use by `intern' and `read'.\n\
It is a vector whose length ought to be prime for best results.\n\
The vector's contents don't make sense if examined from Lisp programs;\n\
to find all the symbols in an obarray, use `mapatoms'.");

  DEFVAR_LISP ("values", &Vvalues,
    "List of values of all expressions which were read, evaluated and printed.\n\
Order is reverse chronological.");

  DEFVAR_LISP ("standard-input", &Vstandard_input,
    "Stream for read to get input from.\n\
See documentation of `read' for possible values.");
  Vstandard_input = Qt;

  DEFVAR_LISP ("load-path", &Vload_path,
    "*List of directories to search for files to load.\n\
Each element is a string (directory name) or nil (try default directory).\n\
Initialized based on EMACSLOADPATH environment variable, if any,\n\
otherwise to default specified by file `paths.h' when Emacs was built.");

  DEFVAR_BOOL ("load-in-progress", &load_in_progress,
    "Non-nil iff inside of `load'.");

  DEFVAR_LISP ("after-load-alist", &Vafter_load_alist,
    "An alist of expressions to be evalled when particular files are loaded.\n\
Each element looks like (FILENAME FORMS...).\n\
When `load' is run and the file-name argument is FILENAME,\n\
the FORMS in the corresponding element are executed at the end of loading.\n\n\
FILENAME must match exactly!  Normally FILENAME is the name of a library,\n\
with no directory specified, since that is how `load' is normally called.\n\
An error in FORMS does not undo the load,\n\
but does prevent execution of the rest of the FORMS.");
  Vafter_load_alist = Qnil;

  DEFVAR_LISP ("load-history", &Vload_history,
    "Alist mapping source file names to symbols and features.\n\
Each alist element is a list that starts with a file name,\n\
except for one element (optional) that starts with nil and describes\n\
definitions evaluated from buffers not visiting files.\n\
The remaining elements of each list are symbols defined as functions\n\
or variables, and cons cells `(provide . FEATURE)' and `(require . FEATURE)'.");
  Vload_history = Qnil;

  DEFVAR_LISP ("load-file-name", &Vload_file_name,
    "Full name of file being loaded by `load'.");
  Vload_file_name = Qnil;

  DEFVAR_LISP ("current-load-list", &Vcurrent_load_list,
    "Used for internal purposes by `load'.");
  Vcurrent_load_list = Qnil;

  load_descriptor_list = Qnil;
  staticpro (&load_descriptor_list);

  Qcurrent_load_list = intern ("current-load-list");
  staticpro (&Qcurrent_load_list);

  Qstandard_input = intern ("standard-input");
  staticpro (&Qstandard_input);

  Qread_char = intern ("read-char");
  staticpro (&Qread_char);

  Qget_file_char = intern ("get-file-char");
  staticpro (&Qget_file_char);

  Qascii_character = intern ("ascii-character");
  staticpro (&Qascii_character);

  Qload = intern ("load");
  staticpro (&Qload);

  Qload_file_name = intern ("load-file-name");
  staticpro (&Qload_file_name);
}
