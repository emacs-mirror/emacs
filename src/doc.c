/* Record indices of function doc strings stored in a file.
   Copyright (C) 1985, 1986, 1993, 1994 Free Software Foundation, Inc.

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


#include <config.h>

#include <sys/types.h>
#include <sys/file.h>	/* Must be after sys/types.h for USG and BSD4_1*/

#ifdef USG5
#include <fcntl.h>
#endif

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#ifndef O_RDONLY
#define O_RDONLY 0
#endif

#include "lisp.h"
#include "buffer.h"
#include "keyboard.h"

Lisp_Object Vdoc_file_name;

extern char *index ();

extern Lisp_Object Voverriding_local_map;

/* For VMS versions with limited file name syntax,
   convert the name to something VMS will allow. */
static void
munge_doc_file_name (name)
     char *name;
{
#ifdef VMS
#ifndef VMS4_4
  /* For VMS versions with limited file name syntax,
     convert the name to something VMS will allow.  */
  p = name;
  while (*p)
    {
      if (*p == '-')
	*p = '_';
      p++;
    }
#endif /* not VMS4_4 */
#ifdef VMS4_4
  strcpy (name, sys_translate_unix (name));
#endif /* VMS4_4 */
#endif /* VMS */
}

/* Extract a doc string from a file.  FILEPOS says where to get it.
   If it is an integer, use that position in the standard DOC-... file.
   If it is (FILE . INTEGER), use FILE as the file name
   and INTEGER as the position in that file.  */

static Lisp_Object
get_doc_string (filepos)
     Lisp_Object filepos;
{
  char buf[512 * 32 + 1];
  char *buffer;
  int buffer_size;
  int free_it;
  char *from, *to;
  register int fd;
  register char *name;
  register char *p, *p1;
  int minsize;
  int position;
  Lisp_Object file, tem;

  if (INTEGERP (filepos))
    {
      file = Vdoc_file_name;
      position = XINT (filepos);
    }
  else if (CONSP (filepos))
    {
      file = XCONS (filepos)->car;
      position = XINT (XCONS (filepos)->cdr);
    }
  else
    return Qnil;

  if (!STRINGP (Vdoc_directory))
    return Qnil;

  if (!STRINGP (file))
    return Qnil;
    
  /* Put the file name in NAME as a C string.
     If it is relative, combine it with Vdoc_directory.  */

  tem = Ffile_name_absolute_p (file);
  if (NILP (tem))
    {
      minsize = XSTRING (Vdoc_directory)->size;
      /* sizeof ("../etc/") == 8 */
      if (minsize < 8)
	minsize = 8;
      name = (char *) alloca (minsize + XSTRING (file)->size + 8);
      strcpy (name, XSTRING (Vdoc_directory)->data);
      strcat (name, XSTRING (file)->data);
      munge_doc_file_name (name);
    }
  else
    {
      name = XSTRING (file)->data;
    }

  fd = open (name, O_RDONLY, 0);
  if (fd < 0)
    {
#ifndef CANNOT_DUMP
      if (!NILP (Vpurify_flag))
	{
	  /* Preparing to dump; DOC file is probably not installed.
	     So check in ../etc. */
	  strcpy (name, "../etc/");
	  strcat (name, XSTRING (file)->data);
	  munge_doc_file_name (name);

	  fd = open (name, O_RDONLY, 0);
	}
#endif

      if (fd < 0)
	error ("Cannot open doc string file \"%s\"", name);
    }

  if (0 > lseek (fd, position, 0))
    {
      close (fd);
      error ("Position %ld out of range in doc string file \"%s\"",
	     position, name);
    }

  /* Read the doc string into a buffer.
     Use the fixed buffer BUF if it is big enough;
     otherwise allocate one and set FREE_IT.
     We store the buffer in use in BUFFER and its size in BUFFER_SIZE.  */

  buffer = buf;
  buffer_size = sizeof buf;
  free_it = 0;
  p = buf;
  while (1)
    {
      int space_left = buffer_size - (p - buffer);
      int nread;

      /* Switch to a bigger buffer if we need one.  */
      if (space_left == 0)
	{
	  if (free_it)
	    {
	      int offset = p - buffer;
	      buffer = (char *) xrealloc (buffer,
					  buffer_size *= 2);
	      p = buffer + offset;
	    }
	  else
	    {
	      buffer = (char *) xmalloc (buffer_size *= 2);
	      bcopy (buf, buffer, p - buf);
	      p = buffer + (p - buf);
	    }
	  free_it = 1;
	  space_left = buffer_size - (p - buffer);
	}

      /* Don't read too too much at one go.  */
      if (space_left > 1024 * 8)
	space_left = 1024 * 8;
      nread = read (fd, p, space_left);
      if (nread < 0)
	{
	  close (fd);
	  error ("Read error on documentation file");
	}
      p[nread] = 0;
      if (!nread)
	break;
      p1 = index (p, '\037');
      if (p1)
	{
	  *p1 = 0;
	  p = p1;
	  break;
	}
      p += nread;
    }
  close (fd);

  /* Scan the text and perform quoting with ^A (char code 1).
     ^A^A becomes ^A, ^A0 becomes a null char, and ^A_ becomes a ^_.  */
  from = buffer;
  to = buffer;
  while (from != p)
    {
      if (*from == 1)
	{
	  int c;

	  from++;
	  c = *from++;
	  if (c == 1)
	    *to++ = c;
	  else if (c == '0')
	    *to++ = 0;
	  else if (c == '_')
	    *to++ = 037;
	  else
	    error ("Invalid data in documentation file -- ^A followed by code 0%o", c);
	}
      else
	*to++ = *from++;
    }

  tem = make_string (buffer, to - buffer);
  if (free_it)
    free (buffer);

  return tem;
}

/* Get a string from position FILEPOS and pass it through the Lisp reader.
   We use this for fetching the bytecode string and constants vector
   of a compiled function from the .elc file.  */

Lisp_Object
read_doc_string (filepos)
     Lisp_Object filepos;
{
  return Fread (get_doc_string (filepos));
}

DEFUN ("documentation", Fdocumentation, Sdocumentation, 1, 2, 0,
  "Return the documentation string of FUNCTION.\n\
Unless a non-nil second argument is given, the\n\
string is passed through `substitute-command-keys'.")
  (function, raw)
     Lisp_Object function, raw;
{
  Lisp_Object fun;
  Lisp_Object funcar;
  Lisp_Object tem, doc;

  fun = Findirect_function (function);

  if (SUBRP (fun))
    {
      if (XSUBR (fun)->doc == 0) return Qnil;
      if ((EMACS_INT) XSUBR (fun)->doc >= 0)
	doc = build_string (XSUBR (fun)->doc);
      else
	doc = get_doc_string (make_number (- (EMACS_INT) XSUBR (fun)->doc));
    }
  else if (COMPILEDP (fun))
    {
      if (XVECTOR (fun)->size <= COMPILED_DOC_STRING)
	return Qnil;
      tem = XVECTOR (fun)->contents[COMPILED_DOC_STRING];
      if (STRINGP (tem))
	doc = tem;
      else if (NATNUMP (tem) || CONSP (tem))
	doc = get_doc_string (tem);
      else
	return Qnil;
    }
  else if (STRINGP (fun) || VECTORP (fun))
    {
      return build_string ("Keyboard macro.");
    }
  else if (CONSP (fun))
    {
      funcar = Fcar (fun);
      if (!SYMBOLP (funcar))
	return Fsignal (Qinvalid_function, Fcons (fun, Qnil));
      else if (EQ (funcar, Qkeymap))
	return build_string ("Prefix command (definition is a keymap associating keystrokes with\n\
subcommands.)");
      else if (EQ (funcar, Qlambda)
	       || EQ (funcar, Qautoload))
	{
	  tem = Fcar (Fcdr (Fcdr (fun)));
	  if (STRINGP (tem))
	    doc = tem;
	  else if (NATNUMP (tem) || CONSP (tem))
	    doc = get_doc_string (tem);
	  else
	    return Qnil;
	}
      else if (EQ (funcar, Qmocklisp))
	return Qnil;
      else if (EQ (funcar, Qmacro))
	return Fdocumentation (Fcdr (fun), raw);
      else
	goto oops;
    }
  else
    {
    oops:
      Fsignal (Qinvalid_function, Fcons (fun, Qnil));
    }

  if (NILP (raw))
    {
      struct gcpro gcpro1;

      GCPRO1 (doc);
      doc = Fsubstitute_command_keys (doc);
      UNGCPRO;
    }
  return doc;
}

DEFUN ("documentation-property", Fdocumentation_property, Sdocumentation_property, 2, 3, 0,
  "Return the documentation string that is SYMBOL's PROP property.\n\
This is like `get', but it can refer to strings stored in the\n\
`etc/DOC' file; and if the value is a string, it is passed through\n\
`substitute-command-keys'.  A non-nil third argument avoids this\n\
translation.")
  (sym, prop, raw)
     Lisp_Object sym, prop, raw;
{
  register Lisp_Object tem;

  tem = Fget (sym, prop);
  if (INTEGERP (tem))
    tem = get_doc_string (XINT (tem) > 0 ? tem : make_number (- XINT (tem)));
  else if (CONSP (tem))
    tem = get_doc_string (tem);
  if (NILP (raw) && STRINGP (tem))
    return Fsubstitute_command_keys (tem);
  return tem;
}

/* Scanning the DOC files and placing docstring offsets into functions.  */

static void
store_function_docstring (fun, offset)
     Lisp_Object fun;
     int offset;
{
  fun = indirect_function (fun);

  /* The type determines where the docstring is stored.  */

  /* Lisp_Subrs have a slot for it.  */
  if (SUBRP (fun))
    XSUBR (fun)->doc = (char *) - offset;

  /* If it's a lisp form, stick it in the form.  */
  else if (CONSP (fun))
    {
      Lisp_Object tem;

      tem = XCONS (fun)->car;
      if (EQ (tem, Qlambda) || EQ (tem, Qautoload))
	{
	  tem = Fcdr (Fcdr (fun));
	  if (CONSP (tem) && INTEGERP (XCONS (tem)->car))
	    XSETFASTINT (XCONS (tem)->car, offset);
	}
      else if (EQ (tem, Qmacro))
	store_function_docstring (XCONS (fun)->cdr, offset);
    }

  /* Bytecode objects sometimes have slots for it.  */
  else if (COMPILEDP (fun))
    {
      /* This bytecode object must have a slot for the
	 docstring, since we've found a docstring for it.  */
      if (XVECTOR (fun)->size > COMPILED_DOC_STRING)
	XSETFASTINT (XVECTOR (fun)->contents[COMPILED_DOC_STRING], offset);
    }
}


DEFUN ("Snarf-documentation", Fsnarf_documentation, Ssnarf_documentation,
  1, 1, 0,
  "Used during Emacs initialization, before dumping runnable Emacs,\n\
to find pointers to doc strings stored in `etc/DOC...' and\n\
record them in function definitions.\n\
One arg, FILENAME, a string which does not include a directory.\n\
The file is found in `../etc' now; found in the `data-directory'\n\
when doc strings are referred to later in the dumped Emacs.")
  (filename)
     Lisp_Object filename;
{
  int fd;
  char buf[1024 + 1];
  register int filled;
  register int pos;
  register char *p, *end;
  Lisp_Object sym, fun, tem;
  char *name;
  extern char *index ();

#ifndef CANNOT_DUMP
  if (NILP (Vpurify_flag))
    error ("Snarf-documentation can only be called in an undumped Emacs");
#endif

  CHECK_STRING (filename, 0);

#ifndef CANNOT_DUMP
  name = (char *) alloca (XSTRING (filename)->size + 14);
  strcpy (name, "../etc/");
#else /* CANNOT_DUMP */
  CHECK_STRING (Vdoc_directory, 0);
  name = (char *) alloca (XSTRING (filename)->size +
			  XSTRING (Vdoc_directory)->size + 1);
  strcpy (name, XSTRING (Vdoc_directory)->data);
#endif /* CANNOT_DUMP */
  strcat (name, XSTRING (filename)->data); 	/*** Add this line ***/
#ifdef VMS
#ifndef VMS4_4
  /* For VMS versions with limited file name syntax,
     convert the name to something VMS will allow.  */
  p = name;
  while (*p)
    {
      if (*p == '-')
	*p = '_';
      p++;
    }
#endif /* not VMS4_4 */
#ifdef VMS4_4
  strcpy (name, sys_translate_unix (name));
#endif /* VMS4_4 */
#endif /* VMS */

  fd = open (name, O_RDONLY, 0);
  if (fd < 0)
    report_file_error ("Opening doc string file",
		       Fcons (build_string (name), Qnil));
  Vdoc_file_name = filename;
  filled = 0;
  pos = 0;
  while (1)
    {
      if (filled < 512)
	filled += read (fd, &buf[filled], sizeof buf - 1 - filled);
      if (!filled)
	break;

      buf[filled] = 0;
      p = buf;
      end = buf + (filled < 512 ? filled : filled - 128);
      while (p != end && *p != '\037') p++;
      /* p points to ^_Ffunctionname\n or ^_Vvarname\n.  */
      if (p != end)
	{
	  end = index (p, '\n');
	  sym = oblookup (Vobarray, p + 2, end - p - 2);
	  if (SYMBOLP (sym))
	    {
	      /* Attach a docstring to a variable?  */
	      if (p[1] == 'V')
		{
		  /* Install file-position as variable-documentation property
		     and make it negative for a user-variable
		     (doc starts with a `*').  */
		  Fput (sym, Qvariable_documentation,
			make_number ((pos + end + 1 - buf)
				     * (end[1] == '*' ? -1 : 1)));
		}

	      /* Attach a docstring to a function?  */
	      else if (p[1] == 'F')
		store_function_docstring (sym, pos + end + 1 - buf);

	      else
		error ("DOC file invalid at position %d", pos);
	    }
	}
      pos += end - buf;
      filled -= end - buf;
      bcopy (end, buf, filled);
    }
  close (fd);
  return Qnil;
}

DEFUN ("substitute-command-keys", Fsubstitute_command_keys,
  Ssubstitute_command_keys, 1, 1, 0,
  "Substitute key descriptions for command names in STRING.\n\
Return a new string which is STRING with substrings of the form \\=\\[COMMAND]\n\
replaced by either:  a keystroke sequence that will invoke COMMAND,\n\
or \"M-x COMMAND\" if COMMAND is not on any keys.\n\
Substrings of the form \\=\\{MAPVAR} are replaced by summaries\n\
\(made by describe-bindings) of the value of MAPVAR, taken as a keymap.\n\
Substrings of the form \\=\\<MAPVAR> specify to use the value of MAPVAR\n\
as the keymap for future \\=\\[COMMAND] substrings.\n\
\\=\\= quotes the following character and is discarded;\n\
thus, \\=\\=\\=\\= puts \\=\\= into the output, and \\=\\=\\=\\[ puts \\=\\[ into the output.")
  (str)
     Lisp_Object str;
{
  unsigned char *buf;
  int changed = 0;
  register unsigned char *strp;
  register unsigned char *bufp;
  int idx;
  int bsize;
  unsigned char *new;
  Lisp_Object tem;
  Lisp_Object keymap;
  unsigned char *start;
  int length;
  Lisp_Object name;
  struct gcpro gcpro1, gcpro2, gcpro3, gcpro4;

  if (NILP (str))
    return Qnil;

  CHECK_STRING (str, 0);
  tem = Qnil;
  keymap = Qnil;
  name = Qnil;
  GCPRO4 (str, tem, keymap, name);

  /* KEYMAP is either nil (which means search all the active keymaps)
     or a specified local map (which means search just that and the
     global map).  If non-nil, it might come from Voverriding_local_map,
     or from a \\<mapname> construct in STR itself..  */
  keymap = Voverriding_local_map;

  bsize = XSTRING (str)->size;
  bufp = buf = (unsigned char *) xmalloc (bsize);

  strp = (unsigned char *) XSTRING (str)->data;
  while (strp < (unsigned char *) XSTRING (str)->data + XSTRING (str)->size)
    {
      if (strp[0] == '\\' && strp[1] == '=')
	{
	  /* \= quotes the next character;
	     thus, to put in \[ without its special meaning, use \=\[.  */
	  changed = 1;
	  *bufp++ = strp[2];
	  strp += 3;
	}
      else if (strp[0] == '\\' && strp[1] == '[')
	{
	  Lisp_Object firstkey;

	  changed = 1;
	  strp += 2;		/* skip \[ */
	  start = strp;

	  while ((strp - (unsigned char *) XSTRING (str)->data
		  < XSTRING (str)->size)
		 && *strp != ']')
	    strp++;
	  length = strp - start;
	  strp++;		/* skip ] */

	  /* Save STRP in IDX.  */
	  idx = strp - (unsigned char *) XSTRING (str)->data;
	  tem = Fintern (make_string (start, length), Qnil);
	  tem = Fwhere_is_internal (tem, keymap, Qt, Qnil);

	  /* Disregard menu bar bindings; it is positively annoying to
	     mention them when there's no menu bar, and it isn't terribly
	     useful even when there is a menu bar.  */
	  if (!NILP (tem))
	    {
	      firstkey = Faref (tem, make_number (0));
	      if (EQ (firstkey, Qmenu_bar))
		tem = Qnil;
	    }

	  if (NILP (tem))	/* but not on any keys */
	    {
	      new = (unsigned char *) xrealloc (buf, bsize += 4);
	      bufp += new - buf;
	      buf = new;
	      bcopy ("M-x ", bufp, 4);
	      bufp += 4;
	      goto subst;
	    }
	  else
	    {			/* function is on a key */
	      tem = Fkey_description (tem);
	      goto subst_string;
	    }
	}
      /* \{foo} is replaced with a summary of the keymap (symbol-value foo).
	 \<foo> just sets the keymap used for \[cmd].  */
      else if (strp[0] == '\\' && (strp[1] == '{' || strp[1] == '<'))
	{
	  struct buffer *oldbuf;

	  changed = 1;
	  strp += 2;		/* skip \{ or \< */
	  start = strp;

	  while ((strp - (unsigned char *) XSTRING (str)->data
		  < XSTRING (str)->size)
		 && *strp != '}' && *strp != '>')
	    strp++;
	  length = strp - start;
	  strp++;			/* skip } or > */

	  /* Save STRP in IDX.  */
	  idx = strp - (unsigned char *) XSTRING (str)->data;

	  /* Get the value of the keymap in TEM, or nil if undefined.
	     Do this while still in the user's current buffer
	     in case it is a local variable.  */
	  name = Fintern (make_string (start, length), Qnil);
	  tem = Fboundp (name);
	  if (! NILP (tem))
	    {
	      tem = Fsymbol_value (name);
	      if (! NILP (tem))
		tem = get_keymap_1 (tem, 0, 1);
	    }

	  /* Now switch to a temp buffer.  */
	  oldbuf = current_buffer;
	  set_buffer_internal (XBUFFER (Vprin1_to_string_buffer));

	  if (NILP (tem))
	    {
	      name = Fsymbol_name (name);
	      insert_string ("\nUses keymap \"");
	      insert_from_string (name, 0, XSTRING (name)->size, 1);
	      insert_string ("\", which is not currently defined.\n");
	      if (start[-1] == '<') keymap = Qnil;
	    }
	  else if (start[-1] == '<')
	    keymap = tem;
	  else
	    describe_map_tree (tem, 1, Qnil, Qnil, 0, 1);
	  tem = Fbuffer_string ();
	  Ferase_buffer ();
	  set_buffer_internal (oldbuf);

	subst_string:
	  start = XSTRING (tem)->data;
	  length = XSTRING (tem)->size;
	subst:
	  new = (unsigned char *) xrealloc (buf, bsize += length);
	  bufp += new - buf;
	  buf = new;
	  bcopy (start, bufp, length);
	  bufp += length;
	  /* Check STR again in case gc relocated it.  */
	  strp = (unsigned char *) XSTRING (str)->data + idx;
	}
      else			/* just copy other chars */
	*bufp++ = *strp++;
    }

  if (changed)			/* don't bother if nothing substituted */
    tem = make_string (buf, bufp - buf);
  else
    tem = str;
  xfree (buf);
  RETURN_UNGCPRO (tem);
}

syms_of_doc ()
{
  DEFVAR_LISP ("internal-doc-file-name", &Vdoc_file_name,
    "Name of file containing documentation strings of built-in symbols.");
  Vdoc_file_name = Qnil;

  defsubr (&Sdocumentation);
  defsubr (&Sdocumentation_property);
  defsubr (&Ssnarf_documentation);
  defsubr (&Ssubstitute_command_keys);
}
