/* Lisp object printing and output streams.
   Copyright (C) 1985, 86, 88, 93, 94, 95, 97, 98, 1999, 2000, 2001
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
#include "lisp.h"
#include "buffer.h"
#include "charset.h"
#include "keyboard.h"
#include "frame.h"
#include "window.h"
#include "process.h"
#include "dispextern.h"
#include "termchar.h"
#include "intervals.h"

Lisp_Object Vstandard_output, Qstandard_output;

Lisp_Object Qtemp_buffer_setup_hook;

/* These are used to print like we read.  */
extern Lisp_Object Qbackquote, Qcomma, Qcomma_at, Qcomma_dot, Qfunction;

Lisp_Object Vfloat_output_format, Qfloat_output_format;

/* Work around a problem that happens because math.h on hpux 7
   defines two static variables--which, in Emacs, are not really static,
   because `static' is defined as nothing.  The problem is that they are
   defined both here and in lread.c.
   These macros prevent the name conflict.  */
#if defined (HPUX) && !defined (HPUX8)
#define _MAXLDBL print_maxldbl
#define _NMAXLDBL print_nmaxldbl
#endif

#include <math.h>

#if STDC_HEADERS
#include <float.h>
#endif

/* Default to values appropriate for IEEE floating point.  */
#ifndef FLT_RADIX
#define FLT_RADIX 2
#endif
#ifndef DBL_MANT_DIG
#define DBL_MANT_DIG 53
#endif
#ifndef DBL_DIG
#define DBL_DIG 15
#endif
#ifndef DBL_MIN
#define DBL_MIN 2.2250738585072014e-308
#endif

#ifdef DBL_MIN_REPLACEMENT
#undef DBL_MIN
#define DBL_MIN DBL_MIN_REPLACEMENT
#endif

/* Define DOUBLE_DIGITS_BOUND, an upper bound on the number of decimal digits
   needed to express a float without losing information.
   The general-case formula is valid for the usual case, IEEE floating point,
   but many compilers can't optimize the formula to an integer constant,
   so make a special case for it.  */
#if FLT_RADIX == 2 && DBL_MANT_DIG == 53
#define DOUBLE_DIGITS_BOUND 17 /* IEEE floating point */
#else
#define DOUBLE_DIGITS_BOUND ((int) ceil (log10 (pow (FLT_RADIX, DBL_MANT_DIG))))
#endif

/* Avoid actual stack overflow in print.  */
int print_depth;

/* Detect most circularities to print finite output.  */
#define PRINT_CIRCLE 200
Lisp_Object being_printed[PRINT_CIRCLE];

/* When printing into a buffer, first we put the text in this
   block, then insert it all at once.  */
char *print_buffer;

/* Size allocated in print_buffer.  */
int print_buffer_size;
/* Chars stored in print_buffer.  */
int print_buffer_pos;
/* Bytes stored in print_buffer.  */
int print_buffer_pos_byte;

/* Maximum length of list to print in full; noninteger means
   effectively infinity */

Lisp_Object Vprint_length;

/* Maximum depth of list to print in full; noninteger means
   effectively infinity.  */

Lisp_Object Vprint_level;

/* Nonzero means print newlines in strings as \n.  */

int print_escape_newlines;

/* Nonzero means to print single-byte non-ascii characters in strings as
   octal escapes.  */

int print_escape_nonascii;

/* Nonzero means to print multibyte characters in strings as hex escapes.  */

int print_escape_multibyte;

Lisp_Object Qprint_escape_newlines;
Lisp_Object Qprint_escape_multibyte, Qprint_escape_nonascii;

/* Nonzero means print (quote foo) forms as 'foo, etc.  */

int print_quoted;

/* Non-nil means print #: before uninterned symbols.  */

Lisp_Object Vprint_gensym;

/* Non-nil means print recursive structures using #n= and #n# syntax.  */

Lisp_Object Vprint_circle;

/* Non-nil means keep continuous number for #n= and #n# syntax
   between several print functions.  */

Lisp_Object Vprint_continuous_numbering;

/* Vprint_number_table is a vector like [OBJ1 STAT1 OBJ2 STAT2 ...],
   where OBJn are objects going to be printed, and STATn are their status,
   which may be different meanings during process.  See the comments of
   the functions print and print_preprocess for details.
   print_number_index keeps the last position the next object should be added,
   twice of which is the actual vector position in Vprint_number_table.  */
int print_number_index;
Lisp_Object Vprint_number_table;

/* PRINT_NUMBER_OBJECT returns the I'th object in Vprint_number_table TABLE.
   PRINT_NUMBER_STATUS returns the status of the I'th object in TABLE.
   See the comment of the variable Vprint_number_table.  */
#define PRINT_NUMBER_OBJECT(table,i) XVECTOR ((table))->contents[(i) * 2]
#define PRINT_NUMBER_STATUS(table,i) XVECTOR ((table))->contents[(i) * 2 + 1]

/* Nonzero means print newline to stdout before next minibuffer message.
   Defined in xdisp.c */

extern int noninteractive_need_newline;

extern int minibuffer_auto_raise;

#ifdef MAX_PRINT_CHARS
static int print_chars;
static int max_print;
#endif /* MAX_PRINT_CHARS */

void print_interval ();


/* Low level output routines for characters and strings */

/* Lisp functions to do output using a stream
   must have the stream in a variable called printcharfun
   and must start with PRINTPREPARE, end with PRINTFINISH,
   and use PRINTDECLARE to declare common variables.
   Use PRINTCHAR to output one character,
   or call strout to output a block of characters. */ 

#define PRINTDECLARE							\
   struct buffer *old = current_buffer;					\
   int old_point = -1, start_point = -1;				\
   int old_point_byte = -1, start_point_byte = -1;			\
   int specpdl_count = specpdl_ptr - specpdl;				\
   int free_print_buffer = 0;						\
   int multibyte = !NILP (current_buffer->enable_multibyte_characters);	\
   Lisp_Object original

#define PRINTPREPARE							\
   original = printcharfun;						\
   if (NILP (printcharfun)) printcharfun = Qt;				\
   if (BUFFERP (printcharfun))						\
     {									\
       if (XBUFFER (printcharfun) != current_buffer)			\
	 Fset_buffer (printcharfun);					\
       printcharfun = Qnil;						\
     }									\
   if (MARKERP (printcharfun))						\
     {									\
       if (!(XMARKER (original)->buffer))				\
         error ("Marker does not point anywhere");			\
       if (XMARKER (original)->buffer != current_buffer)		\
         set_buffer_internal (XMARKER (original)->buffer);		\
       old_point = PT;							\
       old_point_byte = PT_BYTE;					\
       SET_PT_BOTH (marker_position (printcharfun),			\
		    marker_byte_position (printcharfun));		\
       start_point = PT;						\
       start_point_byte = PT_BYTE;					\
       printcharfun = Qnil;						\
     }									\
   if (NILP (printcharfun))						\
     {									\
       Lisp_Object string;						\
       if (NILP (current_buffer->enable_multibyte_characters)		\
	   && ! print_escape_multibyte)					\
         specbind (Qprint_escape_multibyte, Qt);			\
       if (! NILP (current_buffer->enable_multibyte_characters)		\
	   && ! print_escape_nonascii)					\
         specbind (Qprint_escape_nonascii, Qt);				\
       if (print_buffer != 0)						\
	 {								\
	   string = make_string_from_bytes (print_buffer,		\
					    print_buffer_pos,		\
					    print_buffer_pos_byte);	\
	   record_unwind_protect (print_unwind, string);		\
	 }								\
       else								\
	 {								\
           print_buffer_size = 1000;					\
           print_buffer = (char *) xmalloc (print_buffer_size);		\
	   free_print_buffer = 1;					\
	 }								\
       print_buffer_pos = 0;						\
       print_buffer_pos_byte = 0;					\
     }									\
   if (EQ (printcharfun, Qt) && ! noninteractive)			\
     setup_echo_area_for_printing (multibyte);

#define PRINTFINISH							\
   if (NILP (printcharfun))						\
     {									\
       if (print_buffer_pos != print_buffer_pos_byte			\
	   && NILP (current_buffer->enable_multibyte_characters))	\
	 {								\
	   unsigned char *temp						\
	     = (unsigned char *) alloca (print_buffer_pos + 1);		\
	   copy_text (print_buffer, temp, print_buffer_pos_byte,	\
		      1, 0);						\
	   insert_1_both (temp, print_buffer_pos,			\
			  print_buffer_pos, 0, 1, 0);			\
	 }								\
       else								\
	 insert_1_both (print_buffer, print_buffer_pos,			\
			print_buffer_pos_byte, 0, 1, 0);		\
     }									\
   if (free_print_buffer)						\
     {									\
       xfree (print_buffer);						\
       print_buffer = 0;						\
     }									\
   unbind_to (specpdl_count, Qnil);					\
   if (MARKERP (original))						\
     set_marker_both (original, Qnil, PT, PT_BYTE);			\
   if (old_point >= 0)							\
     SET_PT_BOTH (old_point + (old_point >= start_point			\
			       ? PT - start_point : 0),			\
		  old_point_byte + (old_point_byte >= start_point_byte	\
			       ? PT_BYTE - start_point_byte : 0));	\
   if (old != current_buffer)						\
     set_buffer_internal (old);

#define PRINTCHAR(ch) printchar (ch, printcharfun)

/* This is used to restore the saved contents of print_buffer
   when there is a recursive call to print.  */

static Lisp_Object
print_unwind (saved_text)
     Lisp_Object saved_text;
{
  bcopy (XSTRING (saved_text)->data, print_buffer, XSTRING (saved_text)->size);
  return Qnil;
}


/* Print character CH using method FUN.  FUN nil means print to
   print_buffer.  FUN t means print to echo area or stdout if
   non-interactive.  If FUN is neither nil nor t, call FUN with CH as
   argument.  */

static void
printchar (ch, fun)
     unsigned int ch;
     Lisp_Object fun;
{
#ifdef MAX_PRINT_CHARS
  if (max_print)
    print_chars++;
#endif /* MAX_PRINT_CHARS */

  if (!NILP (fun) && !EQ (fun, Qt))
    call1 (fun, make_number (ch));
  else
    {
      unsigned char str[MAX_MULTIBYTE_LENGTH];
      int len = CHAR_STRING (ch, str);

      QUIT;
      
      if (NILP (fun))
	{
	  if (print_buffer_pos_byte + len >= print_buffer_size)
	    print_buffer = (char *) xrealloc (print_buffer,
					      print_buffer_size *= 2);
	  bcopy (str, print_buffer + print_buffer_pos_byte, len);
	  print_buffer_pos += 1;
	  print_buffer_pos_byte += len;
	}
      else if (noninteractive)
	{
	  fwrite (str, 1, len, stdout);
	  noninteractive_need_newline = 1;
	}
      else
	{
	  int multibyte_p
	    = !NILP (current_buffer->enable_multibyte_characters);
	  
	  setup_echo_area_for_printing (multibyte_p);
	  insert_char (ch);
	  message_dolog (str, len, 0, multibyte_p);
	}
    }
}


/* Output SIZE characters, SIZE_BYTE bytes from string PTR using
   method PRINTCHARFUN.  If SIZE < 0, use the string length of PTR for
   both SIZE and SIZE_BYTE.  PRINTCHARFUN nil means output to
   print_buffer.  PRINTCHARFUN t means output to the echo area or to
   stdout if non-interactive.  If neither nil nor t, call Lisp
   function PRINTCHARFUN for each character printed.  MULTIBYTE
   non-zero means PTR contains multibyte characters.  */

static void
strout (ptr, size, size_byte, printcharfun, multibyte)
     char *ptr;
     int size, size_byte;
     Lisp_Object printcharfun;
     int multibyte;
{
  if (size < 0)
    size_byte = size = strlen (ptr);

  if (NILP (printcharfun))
    {
      if (print_buffer_pos_byte + size_byte > print_buffer_size)
	{
	  print_buffer_size = print_buffer_size * 2 + size_byte;
	  print_buffer = (char *) xrealloc (print_buffer,
					    print_buffer_size);
	}
      bcopy (ptr, print_buffer + print_buffer_pos_byte, size_byte);
      print_buffer_pos += size;
      print_buffer_pos_byte += size_byte;

#ifdef MAX_PRINT_CHARS
      if (max_print)
        print_chars += size;
#endif /* MAX_PRINT_CHARS */
    }
  else if (noninteractive && EQ (printcharfun, Qt))
    {
      fwrite (ptr, 1, size_byte, stdout);
      noninteractive_need_newline = 1;
    }
  else if (EQ (printcharfun, Qt))
    {
      /* Output to echo area.  We're trying to avoid a little overhead
	 here, that's the reason we don't call printchar to do the
	 job.  */
      int i;
      int multibyte_p
	= !NILP (current_buffer->enable_multibyte_characters);
      
      setup_echo_area_for_printing (multibyte_p);
      message_dolog (ptr, size_byte, 0, multibyte_p);
      
      if (size == size_byte)
	{
	  for (i = 0; i < size; ++i)
	    insert_char ((unsigned char )*ptr++);
	}
      else
	{
	  int len;
	  for (i = 0; i < size_byte; i += len)
	    {
	      int ch = STRING_CHAR_AND_LENGTH (ptr + i, size_byte - i, len);
	      insert_char (ch);
	    }
	}
      
#ifdef MAX_PRINT_CHARS
      if (max_print)
        print_chars += size;
#endif /* MAX_PRINT_CHARS */
    }
  else
    {
      /* PRINTCHARFUN is a Lisp function.  */
      int i = 0;

      if (size == size_byte)
	{
	  while (i < size_byte)
	    {
	      int ch = ptr[i++];
	      PRINTCHAR (ch);
	    }
	}
      else
	{
	  while (i < size_byte)
	    {
	      /* Here, we must convert each multi-byte form to the
		 corresponding character code before handing it to
		 PRINTCHAR.  */
	      int len;
	      int ch = STRING_CHAR_AND_LENGTH (ptr + i, size_byte - i, len);
	      PRINTCHAR (ch);
	      i += len;
	    }
	}
    }
}

/* Print the contents of a string STRING using PRINTCHARFUN.
   It isn't safe to use strout in many cases,
   because printing one char can relocate.  */

static void
print_string (string, printcharfun)
     Lisp_Object string;
     Lisp_Object printcharfun;
{
  if (EQ (printcharfun, Qt) || NILP (printcharfun))
    {
      int chars;

      if (STRING_MULTIBYTE (string))
	chars = XSTRING (string)->size;
      else if (EQ (printcharfun, Qt)
	       ? ! NILP (buffer_defaults.enable_multibyte_characters)
	       : ! NILP (current_buffer->enable_multibyte_characters))
	{
	  /* If unibyte string STRING contains 8-bit codes, we must
	     convert STRING to a multibyte string containing the same
	     character codes.  */
	  Lisp_Object newstr;
	  int bytes;

	  chars = STRING_BYTES (XSTRING (string));
	  bytes = parse_str_to_multibyte (XSTRING (string)->data, chars);
	  if (chars < bytes)
	    {
	      newstr = make_uninit_multibyte_string (chars, bytes);
	      bcopy (XSTRING (string)->data, XSTRING (newstr)->data, chars);
	      str_to_multibyte (XSTRING (newstr)->data, bytes, chars);
	      string = newstr;
	    }
	}
      else
	chars = STRING_BYTES (XSTRING (string));

      /* strout is safe for output to a frame (echo area) or to print_buffer.  */
      strout (XSTRING (string)->data,
	      chars, STRING_BYTES (XSTRING (string)),
	      printcharfun, STRING_MULTIBYTE (string));
    }
  else
    {
      /* Otherwise, string may be relocated by printing one char.
	 So re-fetch the string address for each character.  */
      int i;
      int size = XSTRING (string)->size;
      int size_byte = STRING_BYTES (XSTRING (string));
      struct gcpro gcpro1;
      GCPRO1 (string);
      if (size == size_byte)
	for (i = 0; i < size; i++)
	  PRINTCHAR (XSTRING (string)->data[i]);
      else
	for (i = 0; i < size_byte; i++)
	  {
	    /* Here, we must convert each multi-byte form to the
	       corresponding character code before handing it to PRINTCHAR.  */
	    int len;
	    int ch = STRING_CHAR_AND_LENGTH (XSTRING (string)->data + i,
					     size_byte - i, len);
	    if (!CHAR_VALID_P (ch, 0))
	      {
		ch = XSTRING (string)->data[i];
		len = 1;
	      }
	    PRINTCHAR (ch);
	    i += len;
	  }
      UNGCPRO;
    }
}

DEFUN ("write-char", Fwrite_char, Swrite_char, 1, 2, 0,
       doc: /* Output character CHARACTER to stream PRINTCHARFUN.
PRINTCHARFUN defaults to the value of `standard-output' (which see).  */)
     (character, printcharfun)
     Lisp_Object character, printcharfun;
{
  PRINTDECLARE;

  if (NILP (printcharfun))
    printcharfun = Vstandard_output;
  CHECK_NUMBER (character);
  PRINTPREPARE;
  PRINTCHAR (XINT (character));
  PRINTFINISH;
  return character;
}

/* Used from outside of print.c to print a block of SIZE
   single-byte chars at DATA on the default output stream.
   Do not use this on the contents of a Lisp string.  */

void
write_string (data, size)
     char *data;
     int size;
{
  PRINTDECLARE;
  Lisp_Object printcharfun;

  printcharfun = Vstandard_output;

  PRINTPREPARE;
  strout (data, size, size, printcharfun, 0);
  PRINTFINISH;
}

/* Used from outside of print.c to print a block of SIZE
   single-byte chars at DATA on a specified stream PRINTCHARFUN.
   Do not use this on the contents of a Lisp string.  */

void
write_string_1 (data, size, printcharfun)
     char *data;
     int size;
     Lisp_Object printcharfun;
{
  PRINTDECLARE;

  PRINTPREPARE;
  strout (data, size, size, printcharfun, 0);
  PRINTFINISH;
}


void
temp_output_buffer_setup (bufname)
    char *bufname;
{
  int count = specpdl_ptr - specpdl;
  register struct buffer *old = current_buffer;
  register Lisp_Object buf;

  record_unwind_protect (set_buffer_if_live, Fcurrent_buffer ());

  Fset_buffer (Fget_buffer_create (build_string (bufname)));

  current_buffer->directory = old->directory;
  current_buffer->read_only = Qnil;
  current_buffer->filename = Qnil;
  current_buffer->undo_list = Qt;
  current_buffer->overlays_before = Qnil;
  current_buffer->overlays_after = Qnil;
  current_buffer->enable_multibyte_characters
    = buffer_defaults.enable_multibyte_characters;
  Ferase_buffer ();
  XSETBUFFER (buf, current_buffer);

  Frun_hooks (1, &Qtemp_buffer_setup_hook);

  unbind_to (count, Qnil);

  specbind (Qstandard_output, buf);
}

Lisp_Object
internal_with_output_to_temp_buffer (bufname, function, args)
     char *bufname;
     Lisp_Object (*function) P_ ((Lisp_Object));
     Lisp_Object args;
{
  int count = specpdl_ptr - specpdl;
  Lisp_Object buf, val;
  struct gcpro gcpro1;

  GCPRO1 (args);
  record_unwind_protect (Fset_buffer, Fcurrent_buffer ());
  temp_output_buffer_setup (bufname);
  buf = Vstandard_output;
  UNGCPRO;

  val = (*function) (args);

  GCPRO1 (val);
  temp_output_buffer_show (buf);
  UNGCPRO;

  return unbind_to (count, val);
}

DEFUN ("with-output-to-temp-buffer",
       Fwith_output_to_temp_buffer, Swith_output_to_temp_buffer,
       1, UNEVALLED, 0,
       doc: /* Bind `standard-output' to buffer BUFNAME, eval BODY, then show that buffer.
The buffer is cleared out initially, and marked as unmodified when done.
All output done by BODY is inserted in that buffer by default.
The buffer is displayed in another window, but not selected.
The value of the last form in BODY is returned.
If BODY does not finish normally, the buffer BUFNAME is not displayed.

The hook `temp-buffer-setup-hook' is run before BODY,
with the buffer BUFNAME temporarily current.
The hook `temp-buffer-show-hook' is run after the buffer is displayed,
with the buffer temporarily current, and the window that was used
to display it temporarily selected.

If variable `temp-buffer-show-function' is non-nil, call it at the end
to get the buffer displayed instead of just displaying the non-selected
buffer and calling the hook.  It gets one argument, the buffer to display.  

usage: (with-output-to-temp-buffer BUFFNAME BODY ...)  */)
     (args)
     Lisp_Object args;
{
  struct gcpro gcpro1;
  Lisp_Object name;
  int count = specpdl_ptr - specpdl;
  Lisp_Object buf, val;

  GCPRO1(args);
  name = Feval (Fcar (args));
  CHECK_STRING (name);
  temp_output_buffer_setup (XSTRING (name)->data);
  buf = Vstandard_output;
  UNGCPRO;

  val = Fprogn (XCDR (args));

  GCPRO1 (val);
  temp_output_buffer_show (buf);
  UNGCPRO;

  return unbind_to (count, val);
}


static void print ();
static void print_preprocess ();
static void print_preprocess_string ();
static void print_object ();

DEFUN ("terpri", Fterpri, Sterpri, 0, 1, 0,
       doc: /* Output a newline to stream PRINTCHARFUN.
If PRINTCHARFUN is omitted or nil, the value of `standard-output' is used.  */)
  (printcharfun)
     Lisp_Object printcharfun;
{
  PRINTDECLARE;

  if (NILP (printcharfun))
    printcharfun = Vstandard_output;
  PRINTPREPARE;
  PRINTCHAR ('\n');
  PRINTFINISH;
  return Qt;
}

DEFUN ("prin1", Fprin1, Sprin1, 1, 2, 0,
       doc: /* Output the printed representation of OBJECT, any Lisp object.
Quoting characters are printed when needed to make output that `read'
can handle, whenever this is possible.  For complex objects, the behavior
is controlled by `print-level' and `print-length', which see.

OBJECT is any of the Lisp data types: a number, a string, a symbol,
a list, a buffer, a window, a frame, etc.

A printed representation of an object is text which describes that object.

Optional argument PRINTCHARFUN is the output stream, which can be one
of these:

   - a buffer, in which case output is inserted into that buffer at point;
   - a marker, in which case output is inserted at marker's position;
   - a function, in which case that function is called once for each
     character of OBJECT's printed representation;
   - a symbol, in which case that symbol's function definition is called; or
   - t, in which case the output is displayed in the echo area.

If PRINTCHARFUN is omitted, the value of `standard-output' (which see)
is used instead.  */)
     (object, printcharfun)
     Lisp_Object object, printcharfun;
{
  PRINTDECLARE;

#ifdef MAX_PRINT_CHARS
  max_print = 0;
#endif /* MAX_PRINT_CHARS */
  if (NILP (printcharfun))
    printcharfun = Vstandard_output;
  PRINTPREPARE;
  print (object, printcharfun, 1);
  PRINTFINISH;
  return object;
}

/* a buffer which is used to hold output being built by prin1-to-string */
Lisp_Object Vprin1_to_string_buffer;

DEFUN ("prin1-to-string", Fprin1_to_string, Sprin1_to_string, 1, 2, 0,
       doc: /* Return a string containing the printed representation of OBJECT.
OBJECT can be any Lisp object.  This function outputs quoting characters
when necessary to make output that `read' can handle, whenever possible,
unless the optional second argument NOESCAPE is non-nil.

OBJECT is any of the Lisp data types: a number, a string, a symbol,
a list, a buffer, a window, a frame, etc.

A printed representation of an object is text which describes that object.  */)
     (object, noescape)
     Lisp_Object object, noescape;
{
  PRINTDECLARE;
  Lisp_Object printcharfun;
  struct gcpro gcpro1, gcpro2;
  Lisp_Object tem;

  /* Save and restore this--we are altering a buffer
     but we don't want to deactivate the mark just for that.
     No need for specbind, since errors deactivate the mark.  */
  tem = Vdeactivate_mark;
  GCPRO2 (object, tem);

  printcharfun = Vprin1_to_string_buffer;
  PRINTPREPARE;
  print (object, printcharfun, NILP (noescape));
  /* Make Vprin1_to_string_buffer be the default buffer after PRINTFINSH */
  PRINTFINISH;
  set_buffer_internal (XBUFFER (Vprin1_to_string_buffer));
  object = Fbuffer_string ();

  Ferase_buffer ();
  set_buffer_internal (old);

  Vdeactivate_mark = tem;
  UNGCPRO;

  return object;
}

DEFUN ("princ", Fprinc, Sprinc, 1, 2, 0,
       doc: /* Output the printed representation of OBJECT, any Lisp object.
No quoting characters are used; no delimiters are printed around
the contents of strings.

OBJECT is any of the Lisp data types: a number, a string, a symbol,
a list, a buffer, a window, a frame, etc.

A printed representation of an object is text which describes that object.

Optional argument PRINTCHARFUN is the output stream, which can be one
of these:

   - a buffer, in which case output is inserted into that buffer at point;
   - a marker, in which case output is inserted at marker's position;
   - a function, in which case that function is called once for each
     character of OBJECT's printed representation;
   - a symbol, in which case that symbol's function definition is called; or
   - t, in which case the output is displayed in the echo area.

If PRINTCHARFUN is omitted, the value of `standard-output' (which see)
is used instead.  */)
     (object, printcharfun)
     Lisp_Object object, printcharfun;
{
  PRINTDECLARE;

  if (NILP (printcharfun))
    printcharfun = Vstandard_output;
  PRINTPREPARE;
  print (object, printcharfun, 0);
  PRINTFINISH;
  return object;
}

DEFUN ("print", Fprint, Sprint, 1, 2, 0,
       doc: /* Output the printed representation of OBJECT, with newlines around it.
Quoting characters are printed when needed to make output that `read'
can handle, whenever this is possible.  For complex objects, the behavior
is controlled by `print-level' and `print-length', which see.

OBJECT is any of the Lisp data types: a number, a string, a symbol,
a list, a buffer, a window, a frame, etc.

A printed representation of an object is text which describes that object.

Optional argument PRINTCHARFUN is the output stream, which can be one
of these:

   - a buffer, in which case output is inserted into that buffer at point;
   - a marker, in which case output is inserted at marker's position;
   - a function, in which case that function is called once for each
     character of OBJECT's printed representation;
   - a symbol, in which case that symbol's function definition is called; or
   - t, in which case the output is displayed in the echo area.

If PRINTCHARFUN is omitted, the value of `standard-output' (which see)
is used instead.  */)
     (object, printcharfun)
     Lisp_Object object, printcharfun;
{
  PRINTDECLARE;
  struct gcpro gcpro1;

#ifdef MAX_PRINT_CHARS
  print_chars = 0;
  max_print = MAX_PRINT_CHARS;
#endif /* MAX_PRINT_CHARS */
  if (NILP (printcharfun))
    printcharfun = Vstandard_output;
  GCPRO1 (object);
  PRINTPREPARE;
  PRINTCHAR ('\n');
  print (object, printcharfun, 1);
  PRINTCHAR ('\n');
  PRINTFINISH;
#ifdef MAX_PRINT_CHARS
  max_print = 0;
  print_chars = 0;
#endif /* MAX_PRINT_CHARS */
  UNGCPRO;
  return object;
}

/* The subroutine object for external-debugging-output is kept here
   for the convenience of the debugger.  */
Lisp_Object Qexternal_debugging_output;

DEFUN ("external-debugging-output", Fexternal_debugging_output, Sexternal_debugging_output, 1, 1, 0,
       doc: /* Write CHARACTER to stderr.
You can call print while debugging emacs, and pass it this function
to make it write to the debugging output.  */)
     (character)
     Lisp_Object character;
{
  CHECK_NUMBER (character);
  putc (XINT (character), stderr);

#ifdef WINDOWSNT
  /* Send the output to a debugger (nothing happens if there isn't one).  */
  {
    char buf[2] = {(char) XINT (character), '\0'};
    OutputDebugString (buf);
  }
#endif

  return character;
}

/* This is the interface for debugging printing.  */

void
debug_print (arg)
     Lisp_Object arg;
{
  Fprin1 (arg, Qexternal_debugging_output);
  fprintf (stderr, "\r\n");
}

DEFUN ("error-message-string", Ferror_message_string, Serror_message_string,
       1, 1, 0,
       doc: /* Convert an error value (ERROR-SYMBOL . DATA) to an error message.  */)
     (obj)
     Lisp_Object obj;
{
  struct buffer *old = current_buffer;
  Lisp_Object value;
  struct gcpro gcpro1;

  /* If OBJ is (error STRING), just return STRING.
     That is not only faster, it also avoids the need to allocate
     space here when the error is due to memory full.  */
  if (CONSP (obj) && EQ (XCAR (obj), Qerror)
      && CONSP (XCDR (obj))
      && STRINGP (XCAR (XCDR (obj)))
      && NILP (XCDR (XCDR (obj))))
    return XCAR (XCDR (obj));

  print_error_message (obj, Vprin1_to_string_buffer);

  set_buffer_internal (XBUFFER (Vprin1_to_string_buffer));
  value = Fbuffer_string ();

  GCPRO1 (value);
  Ferase_buffer ();
  set_buffer_internal (old);
  UNGCPRO;

  return value;
}

/* Print an error message for the error DATA onto Lisp output stream
   STREAM (suitable for the print functions).  */

void
print_error_message (data, stream)
     Lisp_Object data, stream;
{
  Lisp_Object errname, errmsg, file_error, tail;
  struct gcpro gcpro1;
  int i;

  errname = Fcar (data);

  if (EQ (errname, Qerror))
    {
      data = Fcdr (data);
      if (!CONSP (data))
	data = Qnil;
      errmsg = Fcar (data);
      file_error = Qnil;
    }
  else
    {
      Lisp_Object error_conditions;
      errmsg = Fget (errname, Qerror_message);
      error_conditions = Fget (errname, Qerror_conditions);
      file_error = Fmemq (Qfile_error, error_conditions);
    }

  /* Print an error message including the data items.  */

  tail = Fcdr_safe (data);
  GCPRO1 (tail);

  /* If we know from where the error was signaled, show it in
     *Messages*.  */
  if (!NILP (Vsignaling_function) && SYMBOLP (Vsignaling_function))
    {
      char *name = XSTRING (SYMBOL_NAME (Vsignaling_function))->data;
      message_dolog (name, strlen (name), 0, 0);
      message_dolog (": ", 2, 0, 0);
      Vsignaling_function = Qnil;
    }

  /* For file-error, make error message by concatenating
     all the data items.  They are all strings.  */
  if (!NILP (file_error) && CONSP (tail))
    errmsg = XCAR (tail), tail = XCDR (tail);

  if (STRINGP (errmsg))
    Fprinc (errmsg, stream);
  else
    write_string_1 ("peculiar error", -1, stream);

  for (i = 0; CONSP (tail); tail = XCDR (tail), i++)
    {
      Lisp_Object obj;

      write_string_1 (i ? ", " : ": ", 2, stream);
      obj = XCAR (tail);
      if (!NILP (file_error) || EQ (errname, Qend_of_file))
	Fprinc (obj, stream);
      else
	Fprin1 (obj, stream);
    }
  
  UNGCPRO;
}



/*
 * The buffer should be at least as large as the max string size of the
 * largest float, printed in the biggest notation.  This is undoubtedly
 * 20d float_output_format, with the negative of the C-constant "HUGE"
 * from <math.h>.
 * 
 * On the vax the worst case is -1e38 in 20d format which takes 61 bytes.
 * 
 * I assume that IEEE-754 format numbers can take 329 bytes for the worst
 * case of -1e307 in 20d float_output_format. What is one to do (short of
 * re-writing _doprnt to be more sane)?
 * 			-wsr
 */

void
float_to_string (buf, data)
     unsigned char *buf;
     double data;
{
  unsigned char *cp;
  int width;
      
  /* Check for plus infinity in a way that won't lose
     if there is no plus infinity.  */
  if (data == data / 2 && data > 1.0)
    {
      strcpy (buf, "1.0e+INF");
      return;
    }
  /* Likewise for minus infinity.  */
  if (data == data / 2 && data < -1.0)
    {
      strcpy (buf, "-1.0e+INF");
      return;
    }
  /* Check for NaN in a way that won't fail if there are no NaNs.  */
  if (! (data * 0.0 >= 0.0))
    {
      /* Prepend "-" if the NaN's sign bit is negative.
	 The sign bit of a double is the bit that is 1 in -0.0.  */
      int i;
      union { double d; char c[sizeof (double)]; } u_data, u_minus_zero;
      u_data.d = data;
      u_minus_zero.d = - 0.0;
      for (i = 0; i < sizeof (double); i++)
	if (u_data.c[i] & u_minus_zero.c[i])
	  {
	    *buf++ = '-';
	    break;
	  }
      
      strcpy (buf, "0.0e+NaN");
      return;
    }

  if (NILP (Vfloat_output_format)
      || !STRINGP (Vfloat_output_format))
  lose:
    {
      /* Generate the fewest number of digits that represent the
	 floating point value without losing information.
	 The following method is simple but a bit slow.
	 For ideas about speeding things up, please see:

	 Guy L Steele Jr & Jon L White, How to print floating-point numbers
	 accurately.  SIGPLAN notices 25, 6 (June 1990), 112-126.

	 Robert G Burger & R Kent Dybvig, Printing floating point numbers
	 quickly and accurately, SIGPLAN notices 31, 5 (May 1996), 108-116.  */

      width = fabs (data) < DBL_MIN ? 1 : DBL_DIG;
      do
	sprintf (buf, "%.*g", width, data);
      while (width++ < DOUBLE_DIGITS_BOUND && atof (buf) != data);
    }
  else			/* oink oink */
    {
      /* Check that the spec we have is fully valid.
	 This means not only valid for printf,
	 but meant for floats, and reasonable.  */
      cp = XSTRING (Vfloat_output_format)->data;

      if (cp[0] != '%')
	goto lose;
      if (cp[1] != '.')
	goto lose;

      cp += 2;

      /* Check the width specification.  */
      width = -1;
      if ('0' <= *cp && *cp <= '9')
	{
	  width = 0;
	  do
	    width = (width * 10) + (*cp++ - '0');
	  while (*cp >= '0' && *cp <= '9');

	  /* A precision of zero is valid only for %f.  */
	  if (width > DBL_DIG
	      || (width == 0 && *cp != 'f'))
	    goto lose;
	}

      if (*cp != 'e' && *cp != 'f' && *cp != 'g')
	goto lose;

      if (cp[1] != 0)
	goto lose;

      sprintf (buf, XSTRING (Vfloat_output_format)->data, data);
    }

  /* Make sure there is a decimal point with digit after, or an
     exponent, so that the value is readable as a float.  But don't do
     this with "%.0f"; it's valid for that not to produce a decimal
     point.  Note that width can be 0 only for %.0f.  */
  if (width != 0)
    {
      for (cp = buf; *cp; cp++)
	if ((*cp < '0' || *cp > '9') && *cp != '-')
	  break;

      if (*cp == '.' && cp[1] == 0)
	{
	  cp[1] = '0';
	  cp[2] = 0;
	}

      if (*cp == 0)
	{
	  *cp++ = '.';
	  *cp++ = '0';
	  *cp++ = 0;
	}
    }
}


static void
print (obj, printcharfun, escapeflag)
     Lisp_Object obj;
     register Lisp_Object printcharfun;
     int escapeflag;
{
  print_depth = 0;

  /* Reset print_number_index and Vprint_number_table only when
     the variable Vprint_continuous_numbering is nil.  Otherwise,
     the values of these variables will be kept between several
     print functions.  */
  if (NILP (Vprint_continuous_numbering))
    {
      print_number_index = 0;
      Vprint_number_table = Qnil;
    }

  /* Construct Vprint_number_table for print-gensym and print-circle.  */
  if (!NILP (Vprint_gensym) || !NILP (Vprint_circle))
    {
      int i, start, index;
      /* Construct Vprint_number_table.  */
      start = index = print_number_index;
      print_preprocess (obj);
      /* Remove unnecessary objects, which appear only once in OBJ;
	 that is, whose status is Qnil.  */
      for (i = start; i < print_number_index; i++)
	if (!NILP (PRINT_NUMBER_STATUS (Vprint_number_table, i)))
	  {
	    PRINT_NUMBER_OBJECT (Vprint_number_table, index)
	      = PRINT_NUMBER_OBJECT (Vprint_number_table, i);
	    /* Reset the status field for the next print step.  Now this
	       field means whether the object has already been printed.  */
	    PRINT_NUMBER_STATUS (Vprint_number_table, index) = Qnil;
	    index++;
	  }
      print_number_index = index;
    }

  print_object (obj, printcharfun, escapeflag);
}

/* Construct Vprint_number_table according to the structure of OBJ.
   OBJ itself and all its elements will be added to Vprint_number_table
   recursively if it is a list, vector, compiled function, char-table,
   string (its text properties will be traced), or a symbol that has
   no obarray (this is for the print-gensym feature).
   The status fields of Vprint_number_table mean whether each object appears
   more than once in OBJ: Qnil at the first time, and Qt after that .  */
static void
print_preprocess (obj)
     Lisp_Object obj;
{
  int i, size;

 loop:
  if (STRINGP (obj) || CONSP (obj) || VECTORP (obj)
      || COMPILEDP (obj) || CHAR_TABLE_P (obj)
      || (! NILP (Vprint_gensym)
	  && SYMBOLP (obj)
	  && !SYMBOL_INTERNED_P (obj)))
    {
      /* In case print-circle is nil and print-gensym is t,
	 add OBJ to Vprint_number_table only when OBJ is a symbol.  */
      if (! NILP (Vprint_circle) || SYMBOLP (obj))
	{
	  for (i = 0; i < print_number_index; i++)
	    if (EQ (PRINT_NUMBER_OBJECT (Vprint_number_table, i), obj))
	      {
		/* OBJ appears more than once.  Let's remember that.  */
		PRINT_NUMBER_STATUS (Vprint_number_table, i) = Qt;
		return;
	      }

	  /* OBJ is not yet recorded.  Let's add to the table.  */
	  if (print_number_index == 0)
	    {
	      /* Initialize the table.  */
	      Vprint_number_table = Fmake_vector (make_number (40), Qnil);
	    }
	  else if (XVECTOR (Vprint_number_table)->size == print_number_index * 2)
	    {
	      /* Reallocate the table.  */
	      int i = print_number_index * 4;
	      Lisp_Object old_table = Vprint_number_table;
	      Vprint_number_table = Fmake_vector (make_number (i), Qnil);
	      for (i = 0; i < print_number_index; i++)
		{
		  PRINT_NUMBER_OBJECT (Vprint_number_table, i)
		    = PRINT_NUMBER_OBJECT (old_table, i);
		  PRINT_NUMBER_STATUS (Vprint_number_table, i)
		    = PRINT_NUMBER_STATUS (old_table, i);
		}
	    }
	  PRINT_NUMBER_OBJECT (Vprint_number_table, print_number_index) = obj;
	  /* If Vprint_continuous_numbering is non-nil and OBJ is a gensym,
	     always print the gensym with a number.  This is a special for
	     the lisp function byte-compile-output-docform.  */
	  if (!NILP (Vprint_continuous_numbering)
	      && SYMBOLP (obj)
	      && !SYMBOL_INTERNED_P (obj))
	    PRINT_NUMBER_STATUS (Vprint_number_table, print_number_index) = Qt;
	  print_number_index++;
	}

      switch (XGCTYPE (obj))
	{
	case Lisp_String:
	  /* A string may have text properties, which can be circular.  */
	  traverse_intervals_noorder (XSTRING (obj)->intervals,
				      print_preprocess_string, Qnil);
	  break;

	case Lisp_Cons:
	  print_preprocess (XCAR (obj));
	  obj = XCDR (obj);
	  goto loop;

	case Lisp_Vectorlike:
	  size = XVECTOR (obj)->size & PSEUDOVECTOR_SIZE_MASK;
	  for (i = 0; i < size; i++)
	    print_preprocess (XVECTOR (obj)->contents[i]);
	  break;

	default:
	  break;
	}
    }
}

static void
print_preprocess_string (interval, arg)
     INTERVAL interval;
     Lisp_Object arg;
{
  print_preprocess (interval->plist);
}

static void
print_object (obj, printcharfun, escapeflag)
     Lisp_Object obj;
     register Lisp_Object printcharfun;
     int escapeflag;
{
  char buf[30];

  QUIT;

  /* Detect circularities and truncate them.  */
  if (STRINGP (obj) || CONSP (obj) || VECTORP (obj)
      || COMPILEDP (obj) || CHAR_TABLE_P (obj)
      || (! NILP (Vprint_gensym)
	  && SYMBOLP (obj)
	  && !SYMBOL_INTERNED_P (obj)))
    {
      if (NILP (Vprint_circle) && NILP (Vprint_gensym))
	{
	  /* Simple but incomplete way.  */
	  int i;
	  for (i = 0; i < print_depth; i++)
	    if (EQ (obj, being_printed[i]))
	      {
		sprintf (buf, "#%d", i);
		strout (buf, -1, -1, printcharfun, 0);
		return;
	      }
	  being_printed[print_depth] = obj;
	}
      else
	{
	  /* With the print-circle feature.  */
	  int i;
	  for (i = 0; i < print_number_index; i++)
	    if (EQ (PRINT_NUMBER_OBJECT (Vprint_number_table, i), obj))
	      {
		if (NILP (PRINT_NUMBER_STATUS (Vprint_number_table, i)))
		  {
		    /* Add a prefix #n= if OBJ has not yet been printed;
		       that is, its status field is nil.  */
		    sprintf (buf, "#%d=", i + 1);
		    strout (buf, -1, -1, printcharfun, 0);
		    /* OBJ is going to be printed.  Set the status to t.  */
		    PRINT_NUMBER_STATUS (Vprint_number_table, i) = Qt;
		    break;
		  }
		else
		  {
		    /* Just print #n# if OBJ has already been printed.  */
		    sprintf (buf, "#%d#", i + 1);
		    strout (buf, -1, -1, printcharfun, 0);
		    return;
		  }
	      }
	}
    }

  print_depth++;

  if (print_depth > PRINT_CIRCLE)
    error ("Apparently circular structure being printed");
#ifdef MAX_PRINT_CHARS
  if (max_print && print_chars > max_print)
    {
      PRINTCHAR ('\n');
      print_chars = 0;
    }
#endif /* MAX_PRINT_CHARS */

  switch (XGCTYPE (obj))
    {
    case Lisp_Int:
      if (sizeof (int) == sizeof (EMACS_INT))
	sprintf (buf, "%d", XINT (obj));
      else if (sizeof (long) == sizeof (EMACS_INT))
	sprintf (buf, "%ld", (long) XINT (obj));
      else
	abort ();
      strout (buf, -1, -1, printcharfun, 0);
      break;

    case Lisp_Float:
      {
	char pigbuf[350];	/* see comments in float_to_string */

	float_to_string (pigbuf, XFLOAT_DATA (obj));
	strout (pigbuf, -1, -1, printcharfun, 0);
      }
      break;

    case Lisp_String:
      if (!escapeflag)
	print_string (obj, printcharfun);
      else
	{
	  register int i, i_byte;
	  struct gcpro gcpro1;
	  unsigned char *str;
	  int size_byte;
	  /* 1 means we must ensure that the next character we output
	     cannot be taken as part of a hex character escape.  */
	  int need_nonhex = 0;
	  int multibyte = STRING_MULTIBYTE (obj);

	  GCPRO1 (obj);

	  if (!NULL_INTERVAL_P (XSTRING (obj)->intervals))
	    {
	      PRINTCHAR ('#');
	      PRINTCHAR ('(');
	    }

	  PRINTCHAR ('\"');
	  str = XSTRING (obj)->data;
	  size_byte = STRING_BYTES (XSTRING (obj));

	  for (i = 0, i_byte = 0; i_byte < size_byte;)
	    {
	      /* Here, we must convert each multi-byte form to the
		 corresponding character code before handing it to PRINTCHAR.  */
	      int len;
	      int c;

	      if (multibyte)
		{
		  c = STRING_CHAR_AND_LENGTH (str + i_byte,
					      size_byte - i_byte, len);
		  if (CHAR_VALID_P (c, 0))
		    i_byte += len;
		  else
		    c = str[i_byte++];
		}
	      else
		c = str[i_byte++];

	      QUIT;

	      if (c == '\n' && print_escape_newlines)
		{
		  PRINTCHAR ('\\');
		  PRINTCHAR ('n');
		}
	      else if (c == '\f' && print_escape_newlines)
		{
		  PRINTCHAR ('\\');
		  PRINTCHAR ('f');
		}
	      else if (multibyte
		       && ! ASCII_BYTE_P (c)
		       && (SINGLE_BYTE_CHAR_P (c) || print_escape_multibyte))
		{
		  /* When multibyte is disabled,
		     print multibyte string chars using hex escapes.
		     For a char code that could be in a unibyte string,
		     when found in a multibyte string, always use a hex escape
		     so it reads back as multibyte.  */
		  unsigned char outbuf[50];
		  sprintf (outbuf, "\\x%x", c);
		  strout (outbuf, -1, -1, printcharfun, 0);
		  need_nonhex = 1;
		}
	      else if (! multibyte
		       && SINGLE_BYTE_CHAR_P (c) && ! ASCII_BYTE_P (c)
		       && print_escape_nonascii)
		{
		  /* When printing in a multibyte buffer
		     or when explicitly requested,
		     print single-byte non-ASCII string chars
		     using octal escapes.  */
		  unsigned char outbuf[5];
		  sprintf (outbuf, "\\%03o", c);
		  strout (outbuf, -1, -1, printcharfun, 0);
		}
	      else
		{
		  /* If we just had a hex escape, and this character
		     could be taken as part of it,
		     output `\ ' to prevent that.  */
		  if (need_nonhex)
		    {
		      need_nonhex = 0;
		      if ((c >= 'a' && c <= 'f')
			  || (c >= 'A' && c <= 'F')
			  || (c >= '0' && c <= '9'))
			strout ("\\ ", -1, -1, printcharfun, 0);
		    }

		  if (c == '\"' || c == '\\')
		    PRINTCHAR ('\\');
		  PRINTCHAR (c);
		}
	    }
	  PRINTCHAR ('\"');

	  if (!NULL_INTERVAL_P (XSTRING (obj)->intervals))
	    {
	      traverse_intervals (XSTRING (obj)->intervals,
				  0, print_interval, printcharfun);
	      PRINTCHAR (')');
	    }

	  UNGCPRO;
	}
      break;

    case Lisp_Symbol:
      {
	register int confusing;
	register unsigned char *p = XSTRING (SYMBOL_NAME (obj))->data;
	register unsigned char *end = p + STRING_BYTES (XSTRING (SYMBOL_NAME (obj)));
	register int c;
	int i, i_byte, size_byte;
	Lisp_Object name;

	name = SYMBOL_NAME (obj);

	if (p != end && (*p == '-' || *p == '+')) p++;
	if (p == end)
	  confusing = 0;
	/* If symbol name begins with a digit, and ends with a digit,
	   and contains nothing but digits and `e', it could be treated
	   as a number.  So set CONFUSING.

	   Symbols that contain periods could also be taken as numbers,
	   but periods are always escaped, so we don't have to worry
	   about them here.  */
	else if (*p >= '0' && *p <= '9'
		 && end[-1] >= '0' && end[-1] <= '9')
	  {
	    while (p != end && ((*p >= '0' && *p <= '9')
				/* Needed for \2e10.  */
				|| *p == 'e'))
	      p++;
	    confusing = (end == p);
	  }
	else
	  confusing = 0;

	if (! NILP (Vprint_gensym) && !SYMBOL_INTERNED_P (obj))
	  {
	    PRINTCHAR ('#');
	    PRINTCHAR (':');
	  }

	size_byte = STRING_BYTES (XSTRING (name));

	for (i = 0, i_byte = 0; i_byte < size_byte;)
	  {
	    /* Here, we must convert each multi-byte form to the
	       corresponding character code before handing it to PRINTCHAR.  */
	    FETCH_STRING_CHAR_ADVANCE (c, name, i, i_byte);
	    QUIT;

	    if (escapeflag)
	      {
		if (c == '\"' || c == '\\' || c == '\''
		    || c == ';' || c == '#' || c == '(' || c == ')'
		    || c == ',' || c =='.' || c == '`'
		    || c == '[' || c == ']' || c == '?' || c <= 040
		    || confusing)
		  PRINTCHAR ('\\'), confusing = 0;
	      }
	    PRINTCHAR (c);
	  }
      }
      break;

    case Lisp_Cons:
      /* If deeper than spec'd depth, print placeholder.  */
      if (INTEGERP (Vprint_level)
	  && print_depth > XINT (Vprint_level))
	strout ("...", -1, -1, printcharfun, 0);
      else if (print_quoted && CONSP (XCDR (obj)) && NILP (XCDR (XCDR (obj)))
	       && (EQ (XCAR (obj), Qquote)))
	{
	  PRINTCHAR ('\'');
	  print_object (XCAR (XCDR (obj)), printcharfun, escapeflag);
	}
      else if (print_quoted && CONSP (XCDR (obj)) && NILP (XCDR (XCDR (obj)))
	       && (EQ (XCAR (obj), Qfunction)))
	{
	  PRINTCHAR ('#');
	  PRINTCHAR ('\'');
	  print_object (XCAR (XCDR (obj)), printcharfun, escapeflag);
	}
      else if (print_quoted && CONSP (XCDR (obj)) && NILP (XCDR (XCDR (obj)))
	       && ((EQ (XCAR (obj), Qbackquote)
		    || EQ (XCAR (obj), Qcomma)
		    || EQ (XCAR (obj), Qcomma_at)
		    || EQ (XCAR (obj), Qcomma_dot))))
	{
	  print_object (XCAR (obj), printcharfun, 0);
	  print_object (XCAR (XCDR (obj)), printcharfun, escapeflag);
	}
      else
	{
	  PRINTCHAR ('(');
	  {
	    int print_length, i;
	    Lisp_Object halftail = obj;

	    /* Negative values of print-length are invalid in CL.
	       Treat them like nil, as CMUCL does.  */
	    if (NATNUMP (Vprint_length))
	      print_length = XFASTINT (Vprint_length);
	    else
	      print_length = 0;

	    i = 0;
	    while (CONSP (obj))
	      {
		/* Detect circular list.  */
		if (NILP (Vprint_circle))
		  {
		    /* Simple but imcomplete way.  */
		    if (i != 0 && EQ (obj, halftail))
		      {
			sprintf (buf, " . #%d", i / 2);
			strout (buf, -1, -1, printcharfun, 0);
			goto end_of_list;
		      }
		  }
		else
		  {
		    /* With the print-circle feature.  */
		    if (i != 0)
		      {
			int i;
			for (i = 0; i < print_number_index; i++)
			  if (EQ (PRINT_NUMBER_OBJECT (Vprint_number_table, i),
				  obj))
			    {
			      if (NILP (PRINT_NUMBER_STATUS (Vprint_number_table, i)))
				{
				  strout (" . ", 3, 3, printcharfun, 0);
				  print_object (obj, printcharfun, escapeflag);
				}
			      else
				{
				  sprintf (buf, " . #%d#", i + 1);
				  strout (buf, -1, -1, printcharfun, 0);
				}
			      goto end_of_list;
			    }
		      }
		  }
		
		if (i++)
		  PRINTCHAR (' ');
		
		if (print_length && i > print_length)
		  {
		    strout ("...", 3, 3, printcharfun, 0);
		    goto end_of_list;
		  }
		
		print_object (XCAR (obj), printcharfun, escapeflag);
		
		obj = XCDR (obj);
		if (!(i & 1))
		  halftail = XCDR (halftail);
	      }
	  }

	  /* OBJ non-nil here means it's the end of a dotted list.  */
	  if (!NILP (obj))
	    {
	      strout (" . ", 3, 3, printcharfun, 0);
	      print_object (obj, printcharfun, escapeflag);
	    }
	  
	end_of_list:
	  PRINTCHAR (')');
	}
      break;

    case Lisp_Vectorlike:
      if (PROCESSP (obj))
	{
	  if (escapeflag)
	    {
	      strout ("#<process ", -1, -1, printcharfun, 0);
	      print_string (XPROCESS (obj)->name, printcharfun);
	      PRINTCHAR ('>');
	    }
	  else
	    print_string (XPROCESS (obj)->name, printcharfun);
	}
      else if (BOOL_VECTOR_P (obj))
	{
	  register int i;
	  register unsigned char c;
	  struct gcpro gcpro1;
	  int size_in_chars
	    = (XBOOL_VECTOR (obj)->size + BITS_PER_CHAR - 1) / BITS_PER_CHAR;

	  GCPRO1 (obj);

	  PRINTCHAR ('#');
	  PRINTCHAR ('&');
	  sprintf (buf, "%d", XBOOL_VECTOR (obj)->size);
	  strout (buf, -1, -1, printcharfun, 0);
	  PRINTCHAR ('\"');

	  /* Don't print more characters than the specified maximum.
	     Negative values of print-length are invalid.  Treat them
	     like a print-length of nil.  */
	  if (NATNUMP (Vprint_length)
	      && XFASTINT (Vprint_length) < size_in_chars)
	    size_in_chars = XFASTINT (Vprint_length);

	  for (i = 0; i < size_in_chars; i++)
	    {
	      QUIT;
	      c = XBOOL_VECTOR (obj)->data[i];
	      if (c == '\n' && print_escape_newlines)
		{
		  PRINTCHAR ('\\');
		  PRINTCHAR ('n');
		}
	      else if (c == '\f' && print_escape_newlines)
		{
		  PRINTCHAR ('\\');
		  PRINTCHAR ('f');
		}
	      else
		{
		  if (c == '\"' || c == '\\')
		    PRINTCHAR ('\\');
		  PRINTCHAR (c);
		}
	    }
	  PRINTCHAR ('\"');

	  UNGCPRO;
	}
      else if (SUBRP (obj))
	{
	  strout ("#<subr ", -1, -1, printcharfun, 0);
	  strout (XSUBR (obj)->symbol_name, -1, -1, printcharfun, 0);
	  PRINTCHAR ('>');
	}
      else if (WINDOWP (obj))
	{
	  strout ("#<window ", -1, -1, printcharfun, 0);
	  sprintf (buf, "%d", XFASTINT (XWINDOW (obj)->sequence_number));
	  strout (buf, -1, -1, printcharfun, 0);
	  if (!NILP (XWINDOW (obj)->buffer))
	    {
	      strout (" on ", -1, -1, printcharfun, 0);
	      print_string (XBUFFER (XWINDOW (obj)->buffer)->name, printcharfun);
	    }
	  PRINTCHAR ('>');
	}
      else if (HASH_TABLE_P (obj))
	{
	  struct Lisp_Hash_Table *h = XHASH_TABLE (obj);
	  strout ("#<hash-table", -1, -1, printcharfun, 0);
	  if (SYMBOLP (h->test))
	    {
	      PRINTCHAR (' ');
	      PRINTCHAR ('\'');
	      strout (XSTRING (SYMBOL_NAME (h->test))->data, -1, -1, printcharfun, 0);
	      PRINTCHAR (' ');
	      strout (XSTRING (SYMBOL_NAME (h->weak))->data, -1, -1, printcharfun, 0);
	      PRINTCHAR (' ');
	      sprintf (buf, "%d/%d", XFASTINT (h->count),
		       XVECTOR (h->next)->size);
	      strout (buf, -1, -1, printcharfun, 0);
	    }
	  sprintf (buf, " 0x%lx", (unsigned long) h);
	  strout (buf, -1, -1, printcharfun, 0);
	  PRINTCHAR ('>');
	}
      else if (BUFFERP (obj))
	{
	  if (NILP (XBUFFER (obj)->name))
	    strout ("#<killed buffer>", -1, -1, printcharfun, 0);
	  else if (escapeflag)
	    {
	      strout ("#<buffer ", -1, -1, printcharfun, 0);
	      print_string (XBUFFER (obj)->name, printcharfun);
	      PRINTCHAR ('>');
	    }
	  else
	    print_string (XBUFFER (obj)->name, printcharfun);
	}
      else if (WINDOW_CONFIGURATIONP (obj))
	{
	  strout ("#<window-configuration>", -1, -1, printcharfun, 0);
	}
      else if (FRAMEP (obj))
	{
	  strout ((FRAME_LIVE_P (XFRAME (obj))
		   ? "#<frame " : "#<dead frame "),
		  -1, -1, printcharfun, 0);
	  print_string (XFRAME (obj)->name, printcharfun);
	  sprintf (buf, " 0x%lx", (unsigned long) (XFRAME (obj)));
	  strout (buf, -1, -1, printcharfun, 0);
	  PRINTCHAR ('>');
	}
      else
	{
	  int size = XVECTOR (obj)->size;
	  if (COMPILEDP (obj))
	    {
	      PRINTCHAR ('#');
	      size &= PSEUDOVECTOR_SIZE_MASK;
	    }
	  if (CHAR_TABLE_P (obj))
	    {
	      /* We print a char-table as if it were a vector,
		 lumping the parent and default slots in with the
		 character slots.  But we add #^ as a prefix.  */
	      PRINTCHAR ('#');
	      PRINTCHAR ('^');
	      if (SUB_CHAR_TABLE_P (obj))
		PRINTCHAR ('^');
	      size &= PSEUDOVECTOR_SIZE_MASK;
	    }
	  if (size & PSEUDOVECTOR_FLAG)
	    goto badtype;

	  PRINTCHAR ('[');
	  {
	    register int i;
	    register Lisp_Object tem;
	    int real_size = size;

	    /* Don't print more elements than the specified maximum.  */
	    if (NATNUMP (Vprint_length)
		&& XFASTINT (Vprint_length) < size)
	      size = XFASTINT (Vprint_length);

	    for (i = 0; i < size; i++)
	      {
		if (i) PRINTCHAR (' ');
		tem = XVECTOR (obj)->contents[i];
		print_object (tem, printcharfun, escapeflag);
	      }
	    if (size < real_size)
	      strout (" ...", 4, 4, printcharfun, 0);
	  }
	  PRINTCHAR (']');
	}
      break;

    case Lisp_Misc:
      switch (XMISCTYPE (obj))
	{
	case Lisp_Misc_Marker:
	  strout ("#<marker ", -1, -1, printcharfun, 0);
	  /* Do you think this is necessary?  */
	  if (XMARKER (obj)->insertion_type != 0)
	    strout ("(moves after insertion) ", -1, -1, printcharfun, 0);
	  if (!(XMARKER (obj)->buffer))
	    strout ("in no buffer", -1, -1, printcharfun, 0);
	  else
	    {
	      sprintf (buf, "at %d", marker_position (obj));
	      strout (buf, -1, -1, printcharfun, 0);
	      strout (" in ", -1, -1, printcharfun, 0);
	      print_string (XMARKER (obj)->buffer->name, printcharfun);
	    }
	  PRINTCHAR ('>');
	  break;

	case Lisp_Misc_Overlay:
	  strout ("#<overlay ", -1, -1, printcharfun, 0);
	  if (!(XMARKER (OVERLAY_START (obj))->buffer))
	    strout ("in no buffer", -1, -1, printcharfun, 0);
	  else
	    {
	      sprintf (buf, "from %d to %d in ",
		       marker_position (OVERLAY_START (obj)),
		       marker_position (OVERLAY_END   (obj)));
	      strout (buf, -1, -1, printcharfun, 0);
	      print_string (XMARKER (OVERLAY_START (obj))->buffer->name,
			    printcharfun);
	    }
	  PRINTCHAR ('>');
	  break;

      /* Remaining cases shouldn't happen in normal usage, but let's print
	 them anyway for the benefit of the debugger.  */
	case Lisp_Misc_Free:
	  strout ("#<misc free cell>", -1, -1, printcharfun, 0);
	  break;

	case Lisp_Misc_Intfwd:
	  sprintf (buf, "#<intfwd to %d>", *XINTFWD (obj)->intvar);
	  strout (buf, -1, -1, printcharfun, 0);
	  break;

	case Lisp_Misc_Boolfwd:
	  sprintf (buf, "#<boolfwd to %s>",
		   (*XBOOLFWD (obj)->boolvar ? "t" : "nil"));
	  strout (buf, -1, -1, printcharfun, 0);
	  break;

	case Lisp_Misc_Objfwd:
	  strout ("#<objfwd to ", -1, -1, printcharfun, 0);
	  print_object (*XOBJFWD (obj)->objvar, printcharfun, escapeflag);
	  PRINTCHAR ('>');
	  break;

	case Lisp_Misc_Buffer_Objfwd:
	  strout ("#<buffer_objfwd to ", -1, -1, printcharfun, 0);
	  print_object (PER_BUFFER_VALUE (current_buffer,
					  XBUFFER_OBJFWD (obj)->offset),
			printcharfun, escapeflag);
	  PRINTCHAR ('>');
	  break;

	case Lisp_Misc_Kboard_Objfwd:
	  strout ("#<kboard_objfwd to ", -1, -1, printcharfun, 0);
	  print_object (*(Lisp_Object *)((char *) current_kboard
					 + XKBOARD_OBJFWD (obj)->offset),
			printcharfun, escapeflag);
	  PRINTCHAR ('>');
	  break;

	case Lisp_Misc_Buffer_Local_Value:
	  strout ("#<buffer_local_value ", -1, -1, printcharfun, 0);
	  goto do_buffer_local;
	case Lisp_Misc_Some_Buffer_Local_Value:
	  strout ("#<some_buffer_local_value ", -1, -1, printcharfun, 0);
	do_buffer_local:
	  strout ("[realvalue] ", -1, -1, printcharfun, 0);
	  print_object (XBUFFER_LOCAL_VALUE (obj)->realvalue,
			printcharfun, escapeflag);
	  if (XBUFFER_LOCAL_VALUE (obj)->found_for_buffer)
	    strout ("[local in buffer] ", -1, -1, printcharfun, 0);
	  else
	    strout ("[buffer] ", -1, -1, printcharfun, 0);
	  print_object (XBUFFER_LOCAL_VALUE (obj)->buffer,
			printcharfun, escapeflag);
	  if (XBUFFER_LOCAL_VALUE (obj)->check_frame)
	    {
	      if (XBUFFER_LOCAL_VALUE (obj)->found_for_frame)
		strout ("[local in frame] ", -1, -1, printcharfun, 0);
	      else
		strout ("[frame] ", -1, -1, printcharfun, 0);
	      print_object (XBUFFER_LOCAL_VALUE (obj)->frame,
			    printcharfun, escapeflag);
	    }
	  strout ("[alist-elt] ", -1, -1, printcharfun, 0);
	  print_object (XCAR (XBUFFER_LOCAL_VALUE (obj)->cdr),
			printcharfun, escapeflag);
	  strout ("[default-value] ", -1, -1, printcharfun, 0);
	  print_object (XCDR (XBUFFER_LOCAL_VALUE (obj)->cdr),
			printcharfun, escapeflag);
	  PRINTCHAR ('>');
	  break;

	default:
	  goto badtype;
	}
      break;

    default:
    badtype:
      {
	/* We're in trouble if this happens!
	   Probably should just abort () */
	strout ("#<EMACS BUG: INVALID DATATYPE ", -1, -1, printcharfun, 0);
	if (MISCP (obj))
	  sprintf (buf, "(MISC 0x%04x)", (int) XMISCTYPE (obj));
	else if (VECTORLIKEP (obj))
	  sprintf (buf, "(PVEC 0x%08x)", (int) XVECTOR (obj)->size);
	else
	  sprintf (buf, "(0x%02x)", (int) XTYPE (obj));
	strout (buf, -1, -1, printcharfun, 0);
	strout (" Save your buffers immediately and please report this bug>",
		-1, -1, printcharfun, 0);
      }
    }

  print_depth--;
}


/* Print a description of INTERVAL using PRINTCHARFUN.
   This is part of printing a string that has text properties.  */

void
print_interval (interval, printcharfun)
     INTERVAL interval;
     Lisp_Object printcharfun;
{
  PRINTCHAR (' ');
  print_object (make_number (interval->position), printcharfun, 1);
  PRINTCHAR (' ');
  print_object (make_number (interval->position + LENGTH (interval)),
	 printcharfun, 1);
  PRINTCHAR (' ');
  print_object (interval->plist, printcharfun, 1);
}


void
syms_of_print ()
{
  Qtemp_buffer_setup_hook = intern ("temp-buffer-setup-hook");
  staticpro (&Qtemp_buffer_setup_hook);

  DEFVAR_LISP ("standard-output", &Vstandard_output,
	       doc: /* Output stream `print' uses by default for outputting a character.
This may be any function of one argument.
It may also be a buffer (output is inserted before point)
or a marker (output is inserted and the marker is advanced)
or the symbol t (output appears in the echo area).  */);
  Vstandard_output = Qt;
  Qstandard_output = intern ("standard-output");
  staticpro (&Qstandard_output);

  DEFVAR_LISP ("float-output-format", &Vfloat_output_format,
	       doc: /* The format descriptor string used to print floats.
This is a %-spec like those accepted by `printf' in C,
but with some restrictions.  It must start with the two characters `%.'.
After that comes an integer precision specification,
and then a letter which controls the format.
The letters allowed are `e', `f' and `g'.
Use `e' for exponential notation \"DIG.DIGITSeEXPT\"
Use `f' for decimal point notation \"DIGITS.DIGITS\".
Use `g' to choose the shorter of those two formats for the number at hand.
The precision in any of these cases is the number of digits following
the decimal point.  With `f', a precision of 0 means to omit the
decimal point.  0 is not allowed with `e' or `g'.

A value of nil means to use the shortest notation
that represents the number without losing information.  */);
  Vfloat_output_format = Qnil;
  Qfloat_output_format = intern ("float-output-format");
  staticpro (&Qfloat_output_format);

  DEFVAR_LISP ("print-length", &Vprint_length,
	       doc: /* Maximum length of list to print before abbreviating.
A value of nil means no limit.  See also `eval-expression-print-length'.  */);
  Vprint_length = Qnil;

  DEFVAR_LISP ("print-level", &Vprint_level,
	       doc: /* Maximum depth of list nesting to print before abbreviating.
A value of nil means no limit.  See also `eval-expression-print-level'.  */);
  Vprint_level = Qnil;

  DEFVAR_BOOL ("print-escape-newlines", &print_escape_newlines,
	       doc: /* Non-nil means print newlines in strings as `\\n'.
Also print formfeeds as `\\f'.  */);
  print_escape_newlines = 0;

  DEFVAR_BOOL ("print-escape-nonascii", &print_escape_nonascii,
	       doc: /* Non-nil means print unibyte non-ASCII chars in strings as \\OOO.
\(OOO is the octal representation of the character code.)
Only single-byte characters are affected, and only in `prin1'.  */);
  print_escape_nonascii = 0;

  DEFVAR_BOOL ("print-escape-multibyte", &print_escape_multibyte,
	       doc: /* Non-nil means print multibyte characters in strings as \\xXXXX.
\(XXXX is the hex representation of the character code.)
This affects only `prin1'.  */);
  print_escape_multibyte = 0;

  DEFVAR_BOOL ("print-quoted", &print_quoted,
	       doc: /* Non-nil means print quoted forms with reader syntax.
I.e., (quote foo) prints as 'foo, (function foo) as #'foo, and backquoted
forms print as in the new syntax.  */);
  print_quoted = 0;

  DEFVAR_LISP ("print-gensym", &Vprint_gensym,
	       doc: /* Non-nil means print uninterned symbols so they will read as uninterned.
I.e., the value of (make-symbol \"foobar\") prints as #:foobar.
When the uninterned symbol appears within a recursive data structure,
and the symbol appears more than once, in addition use the #N# and #N=
constructs as needed, so that multiple references to the same symbol are
shared once again when the text is read back.  */);
  Vprint_gensym = Qnil;

  DEFVAR_LISP ("print-circle", &Vprint_circle,
	       doc: /* *Non-nil means print recursive structures using #N= and #N# syntax.
If nil, printing proceeds recursively and may lead to
`max-lisp-eval-depth' being exceeded or an error may occur:
\"Apparently circular structure being printed.\"  Also see
`print-length' and `print-level'.
If non-nil, shared substructures anywhere in the structure are printed
with `#N=' before the first occurrence (in the order of the print
representation) and `#N#' in place of each subsequent occurrence,
where N is a positive decimal integer.  */);
  Vprint_circle = Qnil;

  DEFVAR_LISP ("print-continuous-numbering", &Vprint_continuous_numbering,
	       doc: /* *Non-nil means number continuously across print calls.
This affects the numbers printed for #N= labels and #M# references.
See also `print-circle', `print-gensym', and `print-number-table'.
This variable should not be set with `setq'; bind it with a `let' instead.  */);
  Vprint_continuous_numbering = Qnil;

  DEFVAR_LISP ("print-number-table", &Vprint_number_table,
	       doc: /* A vector used internally to produce `#N=' labels and `#N#' references.
The Lisp printer uses this vector to detect Lisp objects referenced more
than once.  When `print-continuous-numbering' is bound to t, you should
probably also bind `print-number-table' to nil.  This ensures that the
value of `print-number-table' can be garbage-collected once the printing
is done.  */);
  Vprint_number_table = Qnil;

  /* prin1_to_string_buffer initialized in init_buffer_once in buffer.c */
  staticpro (&Vprin1_to_string_buffer);

  defsubr (&Sprin1);
  defsubr (&Sprin1_to_string);
  defsubr (&Serror_message_string);
  defsubr (&Sprinc);
  defsubr (&Sprint);
  defsubr (&Sterpri);
  defsubr (&Swrite_char);
  defsubr (&Sexternal_debugging_output);

  Qexternal_debugging_output = intern ("external-debugging-output");
  staticpro (&Qexternal_debugging_output);

  Qprint_escape_newlines = intern ("print-escape-newlines");
  staticpro (&Qprint_escape_newlines);

  Qprint_escape_multibyte = intern ("print-escape-multibyte");
  staticpro (&Qprint_escape_multibyte);

  Qprint_escape_nonascii = intern ("print-escape-nonascii");
  staticpro (&Qprint_escape_nonascii);

  defsubr (&Swith_output_to_temp_buffer);
}
