/* Record indices of function doc strings stored in a file. -*- coding: utf-8 -*-

Copyright (C) 1985-1986, 1993-1995, 1997-2020 Free Software Foundation,
Inc.

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
#include <sys/types.h>
#include <sys/file.h>	/* Must be after sys/types.h for USG.  */
#include <fcntl.h>
#include <unistd.h>

#include <c-ctype.h>

#include "lisp.h"
#include "character.h"
#include "coding.h"
#include "buffer.h"
#include "disptab.h"
#include "intervals.h"
#include "keymap.h"

/* Buffer used for reading from documentation file.  */
static char *get_doc_string_buffer;
static ptrdiff_t get_doc_string_buffer_size;

static unsigned char *read_bytecode_pointer;

static char const sibling_etc[] = "../etc/";

/* `readchar' in lread.c calls back here to fetch the next byte.
   If UNREADFLAG is 1, we unread a byte.  */

int
read_bytecode_char (bool unreadflag)
{
  if (unreadflag)
    {
      read_bytecode_pointer--;
      return 0;
    }
  return *read_bytecode_pointer++;
}

/* Extract a doc string from a file.  FILEPOS says where to get it.
   If it is an integer, use that position in the standard DOC file.
   If it is (FILE . INTEGER), use FILE as the file name
   and INTEGER as the position in that file.
   But if INTEGER is negative, make it positive.
   (A negative integer is used for user variables, so we can distinguish
   them without actually fetching the doc string.)

   If the location does not point to the beginning of a docstring
   (e.g. because the file has been modified and the location is stale),
   return nil.

   If UNIBYTE, always make a unibyte string.

   If DEFINITION, assume this is for reading
   a dynamic function definition; convert the bytestring
   and the constants vector with appropriate byte handling,
   and return a cons cell.  */

Lisp_Object
get_doc_string (Lisp_Object filepos, bool unibyte, bool definition)
{
  char *from, *to, *name, *p, *p1;
  int fd;
  int offset;
  EMACS_INT position;
  Lisp_Object file, tem, pos;
  ptrdiff_t count = SPECPDL_INDEX ();
  USE_SAFE_ALLOCA;

  if (FIXNUMP (filepos))
    {
      file = Vdoc_file_name;
      pos = filepos;
    }
  else if (CONSP (filepos))
    {
      file = XCAR (filepos);
      pos = XCDR (filepos);
    }
  else
    return Qnil;

  position = eabs (XFIXNUM (pos));

  if (!STRINGP (Vdoc_directory))
    return Qnil;

  if (!STRINGP (file))
    return Qnil;

  /* Put the file name in NAME as a C string.
     If it is relative, combine it with Vdoc_directory.  */

  tem = Ffile_name_absolute_p (file);
  file = ENCODE_FILE (file);
  Lisp_Object docdir
    = NILP (tem) ? ENCODE_FILE (Vdoc_directory) : empty_unibyte_string;
  ptrdiff_t docdir_sizemax = SBYTES (docdir) + 1;
  if (will_dump_p ())
    docdir_sizemax = max (docdir_sizemax, sizeof sibling_etc);
  name = SAFE_ALLOCA (docdir_sizemax + SBYTES (file));
  lispstpcpy (lispstpcpy (name, docdir), file);

  fd = emacs_open (name, O_RDONLY, 0);
  if (fd < 0)
    {
      if (will_dump_p ())
	{
	  /* Preparing to dump; DOC file is probably not installed.
	     So check in ../etc.  */
	  lispstpcpy (stpcpy (name, sibling_etc), file);

	  fd = emacs_open (name, O_RDONLY, 0);
	}
      if (fd < 0)
	{
	  if (errno != ENOENT && errno != ENOTDIR)
	    report_file_error ("Read error on documentation file", file);

	  SAFE_FREE ();
	  AUTO_STRING (cannot_open, "Cannot open doc string file \"");
	  AUTO_STRING (quote_nl, "\"\n");
	  return concat3 (cannot_open, file, quote_nl);
	}
    }
  record_unwind_protect_int (close_file_unwind, fd);

  /* Seek only to beginning of disk block.  */
  /* Make sure we read at least 1024 bytes before `position'
     so we can check the leading text for consistency.  */
  offset = min (position, max (1024, position % (8 * 1024)));
  if (TYPE_MAXIMUM (off_t) < position
      || lseek (fd, position - offset, 0) < 0)
    error ("Position %"pI"d out of range in doc string file \"%s\"",
	   position, name);

  /* Read the doc string into get_doc_string_buffer.
     P points beyond the data just read.  */

  p = get_doc_string_buffer;
  while (1)
    {
      ptrdiff_t space_left = (get_doc_string_buffer_size - 1
			      - (p - get_doc_string_buffer));
      int nread;

      /* Allocate or grow the buffer if we need to.  */
      if (space_left <= 0)
	{
	  ptrdiff_t in_buffer = p - get_doc_string_buffer;
	  get_doc_string_buffer
	    = xpalloc (get_doc_string_buffer, &get_doc_string_buffer_size,
		       16 * 1024, -1, 1);
	  p = get_doc_string_buffer + in_buffer;
	  space_left = (get_doc_string_buffer_size - 1
			- (p - get_doc_string_buffer));
	}

      /* Read a disk block at a time.
         If we read the same block last time, maybe skip this?  */
      if (space_left > 1024 * 8)
	space_left = 1024 * 8;
      nread = emacs_read_quit (fd, p, space_left);
      if (nread < 0)
	report_file_error ("Read error on documentation file", file);
      p[nread] = 0;
      if (!nread)
	break;
      if (p == get_doc_string_buffer)
	p1 = strchr (p + offset, '\037');
      else
	p1 = strchr (p, '\037');
      if (p1)
	{
	  *p1 = 0;
	  p = p1;
	  break;
	}
      p += nread;
    }
  SAFE_FREE_UNBIND_TO (count, Qnil);

  /* Sanity checking.  */
  if (CONSP (filepos))
    {
      int test = 1;
      /* A dynamic docstring should be either at the very beginning of a "#@
	 comment" or right after a dynamic docstring delimiter (in case we
	 pack several such docstrings within the same comment).  */
      if (get_doc_string_buffer[offset - test] != '\037')
	{
	  if (get_doc_string_buffer[offset - test++] != ' ')
	    return Qnil;
	  while (get_doc_string_buffer[offset - test] >= '0'
		 && get_doc_string_buffer[offset - test] <= '9')
	    test++;
	  if (get_doc_string_buffer[offset - test++] != '@'
	      || get_doc_string_buffer[offset - test] != '#')
	    return Qnil;
	}
    }
  else
    {
      int test = 1;
      if (get_doc_string_buffer[offset - test++] != '\n')
	return Qnil;
      while (get_doc_string_buffer[offset - test] > ' ')
	test++;
      if (get_doc_string_buffer[offset - test] != '\037')
	return Qnil;
    }

  /* Scan the text and perform quoting with ^A (char code 1).
     ^A^A becomes ^A, ^A0 becomes a NUL char, and ^A_ becomes a ^_.  */
  from = get_doc_string_buffer + offset;
  to = get_doc_string_buffer + offset;
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
	    {
	      unsigned char uc = c;
	      error ("\
Invalid data in documentation file -- %c followed by code %03o",
		     1, uc);
	    }
	}
      else
	*to++ = *from++;
    }

  /* If DEFINITION, read from this buffer
     the same way we would read bytes from a file.  */
  if (definition)
    {
      read_bytecode_pointer = (unsigned char *) get_doc_string_buffer + offset;
      return Fread (Qlambda);
    }

  if (unibyte)
    return make_unibyte_string (get_doc_string_buffer + offset,
				to - (get_doc_string_buffer + offset));
  else
    {
      /* The data determines whether the string is multibyte.  */
      ptrdiff_t nchars
	= multibyte_chars_in_text (((unsigned char *) get_doc_string_buffer
				    + offset),
				   to - (get_doc_string_buffer + offset));
      return make_string_from_bytes (get_doc_string_buffer + offset,
				     nchars,
				     to - (get_doc_string_buffer + offset));
    }
}

/* Get a string from position FILEPOS and pass it through the Lisp reader.
   We use this for fetching the bytecode string and constants vector
   of a compiled function from the .elc file.  */

Lisp_Object
read_doc_string (Lisp_Object filepos)
{
  return get_doc_string (filepos, 0, 1);
}

static bool
reread_doc_file (Lisp_Object file)
{
  if (NILP (file))
    Fsnarf_documentation (Vdoc_file_name);
  else
    save_match_data_load (file, Qt, Qt, Qt, Qnil);

  return 1;
}

DEFUN ("documentation", Fdocumentation, Sdocumentation, 1, 2, 0,
       doc: /* Return the documentation string of FUNCTION.
Unless a non-nil second argument RAW is given, the
string is passed through `substitute-command-keys'.  */)
  (Lisp_Object function, Lisp_Object raw)
{
  Lisp_Object fun;
  Lisp_Object funcar;
  Lisp_Object doc;
  bool try_reload = 1;

 documentation:

  doc = Qnil;

  if (SYMBOLP (function))
    {
      Lisp_Object tem = Fget (function, Qfunction_documentation);
      if (!NILP (tem))
	return Fdocumentation_property (function, Qfunction_documentation,
					raw);
    }

  fun = Findirect_function (function, Qnil);
  if (NILP (fun))
    xsignal1 (Qvoid_function, function);
  if (CONSP (fun) && EQ (XCAR (fun), Qmacro))
    fun = XCDR (fun);
  if (SUBRP (fun))
    doc = make_fixnum (XSUBR (fun)->doc);
#ifdef HAVE_MODULES
  else if (MODULE_FUNCTIONP (fun))
    doc = module_function_documentation (XMODULE_FUNCTION (fun));
#endif
  else if (COMPILEDP (fun))
    {
      if (PVSIZE (fun) <= COMPILED_DOC_STRING)
	return Qnil;
      else
	{
	  Lisp_Object tem = AREF (fun, COMPILED_DOC_STRING);
	  if (STRINGP (tem))
	    doc = tem;
	  else if (FIXNATP (tem) || CONSP (tem))
	    doc = tem;
	  else
	    return Qnil;
	}
    }
  else if (STRINGP (fun) || VECTORP (fun))
    {
      return build_string ("Keyboard macro.");
    }
  else if (CONSP (fun))
    {
      funcar = XCAR (fun);
      if (!SYMBOLP (funcar))
	xsignal1 (Qinvalid_function, fun);
      else if (EQ (funcar, Qkeymap))
	return build_string ("Prefix command (definition is a keymap associating keystrokes with commands).");
      else if (EQ (funcar, Qlambda)
	       || (EQ (funcar, Qclosure) && (fun = XCDR (fun), 1))
	       || EQ (funcar, Qautoload))
	{
	  Lisp_Object tem1 = Fcdr (Fcdr (fun));
	  Lisp_Object tem = Fcar (tem1);
	  if (STRINGP (tem))
	    doc = tem;
	  /* Handle a doc reference--but these never come last
	     in the function body, so reject them if they are last.  */
	  else if ((FIXNATP (tem) || (CONSP (tem) && FIXNUMP (XCDR (tem))))
		   && !NILP (XCDR (tem1)))
	    doc = tem;
	  else
	    return Qnil;
	}
      else
	goto oops;
    }
  else
    {
    oops:
      xsignal1 (Qinvalid_function, fun);
    }

  /* If DOC is 0, it's typically because of a dumped file missing
     from the DOC file (bug in src/Makefile.in).  */
  if (EQ (doc, make_fixnum (0)))
    doc = Qnil;
  if (FIXNUMP (doc) || CONSP (doc))
    {
      Lisp_Object tem;
      tem = get_doc_string (doc, 0, 0);
      if (NILP (tem) && try_reload)
	{
	  /* The file is newer, we need to reset the pointers.  */
	  try_reload = reread_doc_file (Fcar_safe (doc));
	  if (try_reload)
	    {
	      try_reload = 0;
	      goto documentation;
	    }
	}
      else
	doc = tem;
    }

  if (NILP (raw))
    doc = Fsubstitute_command_keys (doc);
  return doc;
}

DEFUN ("documentation-property", Fdocumentation_property,
       Sdocumentation_property, 2, 3, 0,
       doc: /* Return the documentation string that is SYMBOL's PROP property.
Third argument RAW omitted or nil means pass the result through
`substitute-command-keys' if it is a string.

This differs from `get' in that it can refer to strings stored in the
`etc/DOC' file; and that it evaluates documentation properties that
aren't strings.  */)
  (Lisp_Object symbol, Lisp_Object prop, Lisp_Object raw)
{
  bool try_reload = 1;
  Lisp_Object tem;

 documentation_property:

  tem = Fget (symbol, prop);

  /* If we don't have any documentation for this symbol (and we're asking for
     the variable documentation), try to see whether it's an indirect variable
     and get the documentation from there instead. */
  if (EQ (prop, Qvariable_documentation)
      && NILP (tem))
    {
      Lisp_Object indirect = Findirect_variable (symbol);
      if (!NILP (indirect))
	tem = Fget (indirect, prop);
    }

  if (EQ (tem, make_fixnum (0)))
    tem = Qnil;

  /* See if we want to look for the string in the DOC file. */
  if (FIXNUMP (tem) || (CONSP (tem) && FIXNUMP (XCDR (tem))))
    {
      Lisp_Object doc = tem;
      tem = get_doc_string (tem, 0, 0);
      if (NILP (tem) && try_reload)
	{
	  /* The file is newer, we need to reset the pointers.  */
	  try_reload = reread_doc_file (Fcar_safe (doc));
	  if (try_reload)
	    {
	      try_reload = 0;
	      goto documentation_property;
	    }
	}
    }
  else if (!STRINGP (tem))
    /* Feval protects its argument.  */
    tem = Feval (tem, Qnil);

  if (NILP (raw) && STRINGP (tem))
    tem = Fsubstitute_command_keys (tem);
  return tem;
}

/* Scanning the DOC files and placing docstring offsets into functions.  */

static void
store_function_docstring (Lisp_Object obj, EMACS_INT offset)
{
  /* Don't use indirect_function here, or defaliases will apply their
     docstrings to the base functions (Bug#2603).  */
  Lisp_Object fun = SYMBOLP (obj) ? XSYMBOL (obj)->u.s.function : obj;

  /* The type determines where the docstring is stored.  */

  /* If it's a lisp form, stick it in the form.  */
  if (CONSP (fun) && EQ (XCAR (fun), Qmacro))
    fun = XCDR (fun);
  if (CONSP (fun))
    {
      Lisp_Object tem;

      tem = XCAR (fun);
      if (EQ (tem, Qlambda) || EQ (tem, Qautoload)
	  || (EQ (tem, Qclosure) && (fun = XCDR (fun), 1)))
	{
	  tem = Fcdr (Fcdr (fun));
	  if (CONSP (tem) && FIXNUMP (XCAR (tem)))
	    /* FIXME: This modifies typically pure hash-cons'd data, so its
	       correctness is quite delicate.  */
	    XSETCAR (tem, make_fixnum (offset));
	}
    }

  /* Lisp_Subrs have a slot for it.  */
  else if (SUBRP (fun))
    XSUBR (fun)->doc = offset;

  /* Bytecode objects sometimes have slots for it.  */
  else if (COMPILEDP (fun))
    {
      /* This bytecode object must have a slot for the
	 docstring, since we've found a docstring for it.  */
      if (PVSIZE (fun) > COMPILED_DOC_STRING)
	ASET (fun, COMPILED_DOC_STRING, make_fixnum (offset));
      else
	{
	  AUTO_STRING (format, "No docstring slot for %s");
	  CALLN (Fmessage, format,
		 (SYMBOLP (obj)
		  ? SYMBOL_NAME (obj)
		  : build_string ("<anonymous>")));
	}
    }
}


DEFUN ("Snarf-documentation", Fsnarf_documentation, Ssnarf_documentation,
       1, 1, 0,
       doc: /* Used during Emacs initialization to scan the `etc/DOC...' file.
This searches the `etc/DOC...' file for doc strings and
records them in function and variable definitions.
The function takes one argument, FILENAME, a string;
it specifies the file name (without a directory) of the DOC file.
That file is found in `../etc' now; later, when the dumped Emacs is run,
the same file name is found in the `doc-directory'.  */)
  (Lisp_Object filename)
{
  int fd;
  char buf[1024 + 1];
  int filled;
  EMACS_INT pos;
  Lisp_Object sym;
  char *p, *name;
  ptrdiff_t count;
  char const *dirname;
  ptrdiff_t dirlen;
  /* Preloaded defcustoms using custom-initialize-delay are added to
     this list, but kept unbound.  See https://debbugs.gnu.org/11565  */
  Lisp_Object delayed_init =
    find_symbol_value (intern ("custom-delayed-init-variables"));

  if (EQ (delayed_init, Qunbound)) delayed_init = Qnil;

  CHECK_STRING (filename);

  if (will_dump_p ())
    {
      dirname = sibling_etc;
      dirlen = sizeof sibling_etc - 1;
    }
  else
    {
      CHECK_STRING (Vdoc_directory);
      dirname = SSDATA (Vdoc_directory);
      dirlen = SBYTES (Vdoc_directory);
    }

  count = SPECPDL_INDEX ();
  USE_SAFE_ALLOCA;
  name = SAFE_ALLOCA (dirlen + SBYTES (filename) + 1);
  lispstpcpy (stpcpy (name, dirname), filename); 	/*** Add this line ***/

  /* Vbuild_files is nil when temacs is run, and non-nil after that.  */
  if (NILP (Vbuild_files))
    {
      static char const *const buildobj[] =
	{
	  #include "buildobj.h"
	};
      int i = ARRAYELTS (buildobj);
      while (0 <= --i)
	Vbuild_files = Fcons (build_string (buildobj[i]), Vbuild_files);
      Vbuild_files = Fpurecopy (Vbuild_files);
    }

  fd = emacs_open (name, O_RDONLY, 0);
  if (fd < 0)
    {
      int open_errno = errno;
      report_file_errno ("Opening doc string file", build_string (name),
			 open_errno);
    }
  record_unwind_protect_int (close_file_unwind, fd);
  Vdoc_file_name = filename;
  filled = 0;
  pos = 0;
  while (true)
    {
      if (filled < 512)
	filled += emacs_read_quit (fd, &buf[filled], sizeof buf - 1 - filled);
      if (!filled)
	break;

      buf[filled] = 0;
      char *end = buf + (filled < 512 ? filled : filled - 128);
      p = memchr (buf, '\037', end - buf);
      /* p points to ^_Ffunctionname\n or ^_Vvarname\n or ^_Sfilename\n.  */
      if (p)
	{
	  end = strchr (p, '\n');

	  /* We used to skip files not in build_files, so that when a
	     function was defined several times in different files
	     (typically, once in xterm, once in w32term, ...), we only
	     paid attention to the relevant one.

	     But this meant the doc had to be kept and updated in
	     multiple files.  Nowadays we keep the doc only in eg xterm.
	     The (f)boundp checks below ensure we don't report
	     docs for eg w32-specific items on X.
	  */

	  sym = oblookup (Vobarray, p + 2,
			  multibyte_chars_in_text ((unsigned char *) p + 2,
						   end - p - 2),
			  end - p - 2);
          /* Ignore docs that start with SKIP.  These mark
             placeholders where the real doc is elsewhere.  */
	  if (SYMBOLP (sym))
	    {
	      /* Attach a docstring to a variable?  */
	      if (p[1] == 'V')
		{
		  /* Install file-position as variable-documentation property
		     and make it negative for a user-variable
		     (doc starts with a `*').  */
                  if ((!NILP (Fboundp (sym))
                      || !NILP (Fmemq (sym, delayed_init)))
                      && strncmp (end, "\nSKIP", 5))
                    Fput (sym, Qvariable_documentation,
                          make_fixnum ((pos + end + 1 - buf)
                                       * (end[1] == '*' ? -1 : 1)));
		}

	      /* Attach a docstring to a function?  */
	      else if (p[1] == 'F')
                {
                  if (!NILP (Ffboundp (sym)) && strncmp (end, "\nSKIP", 5))
                    store_function_docstring (sym, pos + end + 1 - buf);
                }
	      else if (p[1] == 'S')
		; /* Just a source file name boundary marker.  Ignore it.  */

	      else
		error ("DOC file invalid at position %"pI"d", pos);
	    }
	}
      pos += end - buf;
      filled -= end - buf;
      memmove (buf, end, filled);
    }

  return SAFE_FREE_UNBIND_TO (count, Qnil);
}

/* Return true if text quoting style should default to quote `like this'.  */
static bool
default_to_grave_quoting_style (void)
{
  if (!text_quoting_flag)
    return true;
  if (! DISP_TABLE_P (Vstandard_display_table))
    return false;
  Lisp_Object dv = DISP_CHAR_VECTOR (XCHAR_TABLE (Vstandard_display_table),
				     LEFT_SINGLE_QUOTATION_MARK);
  return (VECTORP (dv) && ASIZE (dv) == 1
	  && EQ (AREF (dv, 0), make_fixnum ('`')));
}

/* Return the current effective text quoting style.  */
enum text_quoting_style
text_quoting_style (void)
{
  if (NILP (Vtext_quoting_style)
      ? default_to_grave_quoting_style ()
      : EQ (Vtext_quoting_style, Qgrave))
    return GRAVE_QUOTING_STYLE;
  else if (EQ (Vtext_quoting_style, Qstraight))
    return STRAIGHT_QUOTING_STYLE;
  else
    return CURVE_QUOTING_STYLE;
}

DEFUN ("substitute-command-keys", Fsubstitute_command_keys,
       Ssubstitute_command_keys, 1, 1, 0,
       doc: /* Substitute key descriptions for command names in STRING.
Each substring of the form \\=\\[COMMAND] is replaced by either a
keystroke sequence that invokes COMMAND, or "M-x COMMAND" if COMMAND
is not on any keys.

Each substring of the form \\=\\{MAPVAR} is replaced by a summary of
the value of MAPVAR as a keymap.  This summary is similar to the one
produced by `describe-bindings'.  The summary ends in two newlines
\(used by the helper function `help-make-xrefs' to find the end of the
summary).

Each substring of the form \\=\\<MAPVAR> specifies the use of MAPVAR
as the keymap for future \\=\\[COMMAND] substrings.

Each grave accent \\=` is replaced by left quote, and each apostrophe \\='
is replaced by right quote.  Left and right quote characters are
specified by `text-quoting-style'.

\\=\\= quotes the following character and is discarded; thus, \\=\\=\\=\\= puts \\=\\=
into the output, \\=\\=\\=\\[ puts \\=\\[ into the output, and \\=\\=\\=` puts \\=` into the
output.

Return the original STRING if no substitutions are made.
Otherwise, return a new string (without any text properties).  */)
  (Lisp_Object string)
{
  char *buf;
  bool changed = false;
  bool nonquotes_changed = false;
  unsigned char *strp;
  char *bufp;
  ptrdiff_t idx;
  ptrdiff_t bsize;
  Lisp_Object tem;
  Lisp_Object keymap;
  unsigned char const *start;
  ptrdiff_t length, length_byte;
  Lisp_Object name;
  ptrdiff_t nchars;

  if (NILP (string))
    return Qnil;

  /* If STRING contains non-ASCII unibyte data, process its
     properly-encoded multibyte equivalent instead.  This simplifies
     the implementation and is OK since substitute-command-keys is
     intended for use only on text strings.  Keep STRING around, since
     it will be returned if no changes occur.  */
  Lisp_Object str = Fstring_make_multibyte (string);

  enum text_quoting_style quoting_style = text_quoting_style ();

  nchars = 0;

  /* KEYMAP is either nil (which means search all the active keymaps)
     or a specified local map (which means search just that and the
     global map).  If non-nil, it might come from Voverriding_local_map,
     or from a \\<mapname> construct in STRING itself..  */
  keymap = Voverriding_local_map;

  ptrdiff_t strbytes = SBYTES (str);
  bsize = strbytes;

  /* Fixed-size stack buffer.  */
  char sbuf[MAX_ALLOCA];

  /* Heap-allocated buffer, if any.  */
  char *abuf;

  /* Extra room for expansion due to replacing ‘\[]’ with ‘M-x ’.  */
  enum { EXTRA_ROOM = sizeof "M-x " - sizeof "\\[]" };

  ptrdiff_t count = SPECPDL_INDEX ();

  if (bsize <= sizeof sbuf - EXTRA_ROOM)
    {
      abuf = NULL;
      buf = sbuf;
      bsize = sizeof sbuf;
    }
  else
    {
      buf = abuf = xpalloc (NULL, &bsize, EXTRA_ROOM, STRING_BYTES_BOUND, 1);
      record_unwind_protect_ptr (xfree, abuf);
    }
  bufp = buf;

  strp = SDATA (str);
  while (strp < SDATA (str) + strbytes)
    {
      unsigned char *close_bracket;

      if (strp[0] == '\\' && strp[1] == '='
	  && strp + 2 < SDATA (str) + strbytes)
	{
	  /* \= quotes the next character;
	     thus, to put in \[ without its special meaning, use \=\[.  */
	  changed = nonquotes_changed = true;
	  strp += 2;
	  /* Fall through to copy one char.  */
	}
      else if (strp[0] == '\\' && strp[1] == '['
	       && (close_bracket
		   = memchr (strp + 2, ']',
			     SDATA (str) + strbytes - (strp + 2))))
	{
	  bool follow_remap = 1;

	  start = strp + 2;
	  length_byte = close_bracket - start;
	  idx = close_bracket + 1 - SDATA (str);

	  name = Fintern (make_string ((char *) start, length_byte), Qnil);

	do_remap:
	  tem = Fwhere_is_internal (name, keymap, Qt, Qnil, Qnil);

	  if (VECTORP (tem) && ASIZE (tem) > 1
	      && EQ (AREF (tem, 0), Qremap) && SYMBOLP (AREF (tem, 1))
	      && follow_remap)
	    {
	      name = AREF (tem, 1);
	      follow_remap = 0;
	      goto do_remap;
	    }

	  /* Fwhere_is_internal can GC, so take relocation of string
	     contents into account.  */
	  strp = SDATA (str) + idx;
	  start = strp - length_byte - 1;

	  if (NILP (tem))	/* but not on any keys */
	    {
	      memcpy (bufp, "M-x ", 4);
	      bufp += 4;
	      nchars += 4;
	      length = multibyte_chars_in_text (start, length_byte);
	      goto subst;
	    }
	  else
	    {			/* function is on a key */
	      tem = Fkey_description (tem, Qnil);
	      goto subst_string;
	    }
	}
      /* \{foo} is replaced with a summary of the keymap (symbol-value foo).
	 \<foo> just sets the keymap used for \[cmd].  */
      else if (strp[0] == '\\' && (strp[1] == '{' || strp[1] == '<')
	       && (close_bracket
		   = memchr (strp + 2, strp[1] == '{' ? '}' : '>',
			     SDATA (str) + strbytes - (strp + 2))))
	{
	 {
	  bool generate_summary = strp[1] == '{';
	  /* This is for computing the SHADOWS arg for describe_map_tree.  */
	  Lisp_Object active_maps = Fcurrent_active_maps (Qnil, Qnil);
	  ptrdiff_t count = SPECPDL_INDEX ();

	  start = strp + 2;
	  length_byte = close_bracket - start;
	  idx = close_bracket + 1 - SDATA (str);

	  /* Get the value of the keymap in TEM, or nil if undefined.
	     Do this while still in the user's current buffer
	     in case it is a local variable.  */
	  name = Fintern (make_string ((char *) start, length_byte), Qnil);
	  tem = Fboundp (name);
	  if (! NILP (tem))
	    {
	      tem = Fsymbol_value (name);
	      if (! NILP (tem))
		tem = get_keymap (tem, 0, 1);
	    }

	  /* Now switch to a temp buffer.  */
	  struct buffer *oldbuf = current_buffer;
	  set_buffer_internal (XBUFFER (Vprin1_to_string_buffer));
	  /* This is for an unusual case where some after-change
	     function uses 'format' or 'prin1' or something else that
	     will thrash Vprin1_to_string_buffer we are using.  */
	  specbind (Qinhibit_modification_hooks, Qt);

	  if (NILP (tem))
	    {
	      name = Fsymbol_name (name);
	      AUTO_STRING (msg_prefix, "\nUses keymap `");
	      insert1 (Fsubstitute_command_keys (msg_prefix));
	      insert_from_string (name, 0, 0,
				  SCHARS (name),
				  SBYTES (name), 1);
	      AUTO_STRING (msg_suffix, "', which is not currently defined.\n");
	      insert1 (Fsubstitute_command_keys (msg_suffix));
	      if (!generate_summary)
		keymap = Qnil;
	    }
	  else if (!generate_summary)
	    keymap = tem;
	  else
	    {
	      /* Get the list of active keymaps that precede this one.
		 If this one's not active, get nil.  */
	      Lisp_Object earlier_maps
		= Fcdr (Fmemq (tem, Freverse (active_maps)));
	      describe_map_tree (tem, 1, Fnreverse (earlier_maps),
				 Qnil, 0, 1, 0, 0, 1);
	    }
	  tem = Fbuffer_string ();
	  Ferase_buffer ();
	  set_buffer_internal (oldbuf);
	  unbind_to (count, Qnil);
	 }

	subst_string:
	  /* Convert non-ASCII unibyte data to properly-encoded multibyte,
	     for the same reason STRING was converted to STR.  */
	  tem = Fstring_make_multibyte (tem);
	  start = SDATA (tem);
	  length = SCHARS (tem);
	  length_byte = SBYTES (tem);
	subst:
	  nonquotes_changed = true;
	subst_quote:
	  changed = true;
	  {
	    ptrdiff_t offset = bufp - buf;
	    ptrdiff_t avail = bsize - offset;
	    ptrdiff_t need = strbytes - idx;
	    if (INT_ADD_WRAPV (need, length_byte + EXTRA_ROOM, &need))
	      string_overflow ();
	    if (avail < need)
	      {
		abuf = xpalloc (abuf, &bsize, need - avail,
				STRING_BYTES_BOUND, 1);
		if (buf == sbuf)
		  {
		    record_unwind_protect_ptr (xfree, abuf);
		    memcpy (abuf, sbuf, offset);
		  }
		else
		  set_unwind_protect_ptr (count, xfree, abuf);
		buf = abuf;
		bufp = buf + offset;
	      }
	    memcpy (bufp, start, length_byte);
	    bufp += length_byte;
	    nchars += length;

	    /* Some of the previous code can GC, so take relocation of
	       string contents into account.  */
	    strp = SDATA (str) + idx;

	    continue;
	  }
	}
      else if ((strp[0] == '`' || strp[0] == '\'')
	       && quoting_style == CURVE_QUOTING_STYLE)
	{
	  start = (unsigned char const *) (strp[0] == '`' ? uLSQM : uRSQM);
	  length = 1;
	  length_byte = sizeof uLSQM - 1;
	  idx = strp - SDATA (str) + 1;
	  goto subst_quote;
	}
      else if (strp[0] == '`' && quoting_style == STRAIGHT_QUOTING_STYLE)
	{
	  *bufp++ = '\'';
	  strp++;
	  nchars++;
	  changed = true;
	  continue;
	}

      /* Copy one char.  */
      do
	*bufp++ = *strp++;
      while (! CHAR_HEAD_P (*strp));
      nchars++;
    }

  if (changed)			/* don't bother if nothing substituted */
    {
      tem = make_string_from_bytes (buf, nchars, bufp - buf);
      if (!nonquotes_changed)
	{
	  /* Nothing has changed other than quoting, so copy the string’s
	     text properties.  FIXME: Text properties should survive other
	     changes too; see bug#17052.  */
	  INTERVAL interval_copy = copy_intervals (string_intervals (string),
						   0, SCHARS (string));
	  if (interval_copy)
	    {
	      set_interval_object (interval_copy, tem);
	      set_string_intervals (tem, interval_copy);
	    }
	}
    }
  else
    tem = string;
  return unbind_to (count, tem);
}

void
syms_of_doc (void)
{
  DEFSYM (Qfunction_documentation, "function-documentation");
  DEFSYM (Qgrave, "grave");
  DEFSYM (Qstraight, "straight");

  DEFVAR_LISP ("internal-doc-file-name", Vdoc_file_name,
	       doc: /* Name of file containing documentation strings of built-in symbols.  */);
  Vdoc_file_name = Qnil;

  DEFVAR_LISP ("build-files", Vbuild_files,
               doc: /* A list of files used to build this Emacs binary.  */);
  Vbuild_files = Qnil;

  DEFVAR_LISP ("text-quoting-style", Vtext_quoting_style,
               doc: /* Style to use for single quotes in help and messages.
Its value should be a symbol.  It works by substituting certain single
quotes for grave accent and apostrophe.  This is done in help output
\(but not for display of Info manuals) and in functions like `message'
and `format-message'.  It is not done in `format'.

`curve' means quote with curved single quotes ‘like this’.
`straight' means quote with straight apostrophes \\='like this\\='.
`grave' means quote with grave accent and apostrophe \\=`like this\\=';
i.e., do not alter quote marks.  The default value nil acts like
`curve' if curved single quotes are displayable, and like `grave'
otherwise.  */);
  Vtext_quoting_style = Qnil;

  DEFVAR_BOOL ("internal--text-quoting-flag", text_quoting_flag,
	       doc: /* If nil, a nil `text-quoting-style' is treated as `grave'.  */);
  /* Initialized by ‘main’.  */

  defsubr (&Sdocumentation);
  defsubr (&Sdocumentation_property);
  defsubr (&Ssnarf_documentation);
  defsubr (&Ssubstitute_command_keys);
}
