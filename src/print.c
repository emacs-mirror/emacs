/* Lisp object printing and output streams.
   Copyright (C) 1985, 86, 88, 93, 94, 95 Free Software Foundation, Inc.

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
#undef NULL
#include "lisp.h"

#ifndef standalone
#include "buffer.h"
#include "frame.h"
#include "window.h"
#include "process.h"
#include "dispextern.h"
#include "termchar.h"
#endif /* not standalone */

#ifdef USE_TEXT_PROPERTIES
#include "intervals.h"
#endif

Lisp_Object Vstandard_output, Qstandard_output;

#ifdef LISP_FLOAT_TYPE
Lisp_Object Vfloat_output_format, Qfloat_output_format;
#endif /* LISP_FLOAT_TYPE */

/* Avoid actual stack overflow in print.  */
int print_depth;

/* Detect most circularities to print finite output.  */
#define PRINT_CIRCLE 200
Lisp_Object being_printed[PRINT_CIRCLE];

/* Maximum length of list to print in full; noninteger means
   effectively infinity */

Lisp_Object Vprint_length;

/* Maximum depth of list to print in full; noninteger means
   effectively infinity.  */

Lisp_Object Vprint_level;

/* Nonzero means print newlines in strings as \n.  */

int print_escape_newlines;

Lisp_Object Qprint_escape_newlines;

/* Nonzero means print newline to stdout before next minibuffer message.
   Defined in xdisp.c */

extern int noninteractive_need_newline;

#ifdef MAX_PRINT_CHARS
static int print_chars;
static int max_print;
#endif /* MAX_PRINT_CHARS */

void print_interval ();

#if 0
/* Convert between chars and GLYPHs */

int
glyphlen (glyphs)
     register GLYPH *glyphs;
{
  register int i = 0;

  while (glyphs[i])
    i++;
  return i;
}

void
str_to_glyph_cpy (str, glyphs)
     char *str;
     GLYPH *glyphs;
{
  register GLYPH *gp = glyphs;
  register char *cp = str;

  while (*cp)
    *gp++ = *cp++;
}

void
str_to_glyph_ncpy (str, glyphs, n)
     char *str;
     GLYPH *glyphs;
     register int n;
{
  register GLYPH *gp = glyphs;
  register char *cp = str;

  while (n-- > 0)
    *gp++ = *cp++;
}

void
glyph_to_str_cpy (glyphs, str)
     GLYPH *glyphs;
     char *str;
{
  register GLYPH *gp = glyphs;
  register char *cp = str;

  while (*gp)
    *str++ = *gp++ & 0377;
}
#endif

/* Low level output routines for characters and strings */

/* Lisp functions to do output using a stream
 must have the stream in a variable called printcharfun
 and must start with PRINTPREPARE and end with PRINTFINISH.
 Use PRINTCHAR to output one character,
 or call strout to output a block of characters.
 Also, each one must have the declarations
   struct buffer *old = current_buffer;
   int old_point = -1, start_point;
   Lisp_Object original;
*/ 

#define PRINTPREPARE						\
   original = printcharfun;					\
   if (NILP (printcharfun)) printcharfun = Qt;			\
   if (BUFFERP (printcharfun))					\
     { if (XBUFFER (printcharfun) != current_buffer)		\
	 Fset_buffer (printcharfun);				\
       printcharfun = Qnil;}					\
   if (MARKERP (printcharfun))					\
     { if (!(XMARKER (original)->buffer))			\
         error ("Marker does not point anywhere");		\
       if (XMARKER (original)->buffer != current_buffer)	\
         set_buffer_internal (XMARKER (original)->buffer);	\
       old_point = point;					\
       SET_PT (marker_position (printcharfun));			\
       start_point = point;					\
       printcharfun = Qnil;}

#define PRINTFINISH					\
   if (MARKERP (original))				\
     Fset_marker (original, make_number (point), Qnil);	\
   if (old_point >= 0)					\
     SET_PT (old_point + (old_point >= start_point	\
			  ? point - start_point : 0));	\
   if (old != current_buffer)				\
     set_buffer_internal (old)

#define PRINTCHAR(ch) printchar (ch, printcharfun)

/* Index of first unused element of FRAME_MESSAGE_BUF(mini_frame). */
static int printbufidx;

static void
printchar (ch, fun)
     unsigned char ch;
     Lisp_Object fun;
{
  Lisp_Object ch1;

#ifdef MAX_PRINT_CHARS
  if (max_print)
    print_chars++;
#endif /* MAX_PRINT_CHARS */
#ifndef standalone
  if (EQ (fun, Qnil))
    {
      QUIT;
      insert (&ch, 1);
      return;
    }

  if (EQ (fun, Qt))
    {
      FRAME_PTR mini_frame
	= XFRAME (WINDOW_FRAME (XWINDOW (minibuf_window)));

      if (noninteractive)
	{
	  putchar (ch);
	  noninteractive_need_newline = 1;
	  return;
	}

      if (echo_area_glyphs != FRAME_MESSAGE_BUF (mini_frame)
	  || !message_buf_print)
	{
	  message_log_maybe_newline ();
	  echo_area_glyphs = FRAME_MESSAGE_BUF (mini_frame);
	  printbufidx = 0;
	  echo_area_glyphs_length = 0;
	  message_buf_print = 1;
	}

      message_dolog (&ch, 1, 0);
      if (printbufidx < FRAME_WIDTH (mini_frame) - 1)
	FRAME_MESSAGE_BUF (mini_frame)[printbufidx++] = ch;
      FRAME_MESSAGE_BUF (mini_frame)[printbufidx] = 0;
      echo_area_glyphs_length = printbufidx;

      return;
    }
#endif /* not standalone */

  XSETFASTINT (ch1, ch);
  call1 (fun, ch1);
}

static void
strout (ptr, size, printcharfun)
     char *ptr;
     int size;
     Lisp_Object printcharfun;
{
  int i = 0;

  if (EQ (printcharfun, Qnil))
    {
      insert (ptr, size >= 0 ? size : strlen (ptr));
#ifdef MAX_PRINT_CHARS
      if (max_print)
        print_chars += size >= 0 ? size : strlen(ptr);
#endif /* MAX_PRINT_CHARS */
      return;
    }
  if (EQ (printcharfun, Qt))
    {
      FRAME_PTR mini_frame
	= XFRAME (WINDOW_FRAME (XWINDOW (minibuf_window)));

      i = size >= 0 ? size : strlen (ptr);
#ifdef MAX_PRINT_CHARS
      if (max_print)
        print_chars += i;
#endif /* MAX_PRINT_CHARS */

      if (noninteractive)
	{
	  fwrite (ptr, 1, i, stdout);
	  noninteractive_need_newline = 1;
	  return;
	}

      if (echo_area_glyphs != FRAME_MESSAGE_BUF (mini_frame)
	  || !message_buf_print)
	{
	  message_log_maybe_newline ();
	  echo_area_glyphs = FRAME_MESSAGE_BUF (mini_frame);
	  printbufidx = 0;
	  echo_area_glyphs_length = 0;
	  message_buf_print = 1;
	}

      message_dolog (ptr, i, 0);
      if (i > FRAME_WIDTH (mini_frame) - printbufidx - 1)
	i = FRAME_WIDTH (mini_frame) - printbufidx - 1;
      bcopy (ptr, &FRAME_MESSAGE_BUF (mini_frame) [printbufidx], i);
      printbufidx += i;
      echo_area_glyphs_length = printbufidx;
      FRAME_MESSAGE_BUF (mini_frame) [printbufidx] = 0;

      return;
    }

  if (size >= 0)
    while (i < size)
      PRINTCHAR (ptr[i++]);
  else
    while (ptr[i])
      PRINTCHAR (ptr[i++]);
}

/* Print the contents of a string STRING using PRINTCHARFUN.
   It isn't safe to use strout, because printing one char can relocate.  */

print_string (string, printcharfun)
     Lisp_Object string;
     Lisp_Object printcharfun;
{
  if (EQ (printcharfun, Qnil) || EQ (printcharfun, Qt))
    /* In predictable cases, strout is safe: output to buffer or frame.  */
    strout (XSTRING (string)->data, XSTRING (string)->size, printcharfun);
  else
    {
      /* Otherwise, fetch the string address for each character.  */
      int i;
      int size = XSTRING (string)->size;
      struct gcpro gcpro1;
      GCPRO1 (string);
      for (i = 0; i < size; i++)
	PRINTCHAR (XSTRING (string)->data[i]);
      UNGCPRO;
    }
}

DEFUN ("write-char", Fwrite_char, Swrite_char, 1, 2, 0,
  "Output character CHAR to stream PRINTCHARFUN.\n\
PRINTCHARFUN defaults to the value of `standard-output' (which see).")
  (ch, printcharfun)
     Lisp_Object ch, printcharfun;
{
  struct buffer *old = current_buffer;
  int old_point = -1;
  int start_point;
  Lisp_Object original;

  if (NILP (printcharfun))
    printcharfun = Vstandard_output;
  CHECK_NUMBER (ch, 0);
  PRINTPREPARE;
  PRINTCHAR (XINT (ch));
  PRINTFINISH;
  return ch;
}

/* Used from outside of print.c to print a block of SIZE chars at DATA
   on the default output stream.
   Do not use this on the contents of a Lisp string.  */

write_string (data, size)
     char *data;
     int size;
{
  struct buffer *old = current_buffer;
  Lisp_Object printcharfun;
  int old_point = -1;
  int start_point;
  Lisp_Object original;

  printcharfun = Vstandard_output;

  PRINTPREPARE;
  strout (data, size, printcharfun);
  PRINTFINISH;
}

/* Used from outside of print.c to print a block of SIZE chars at DATA
   on a specified stream PRINTCHARFUN.
   Do not use this on the contents of a Lisp string.  */

write_string_1 (data, size, printcharfun)
     char *data;
     int size;
     Lisp_Object printcharfun;
{
  struct buffer *old = current_buffer;
  int old_point = -1;
  int start_point;
  Lisp_Object original;

  PRINTPREPARE;
  strout (data, size, printcharfun);
  PRINTFINISH;
}


#ifndef standalone

void
temp_output_buffer_setup (bufname)
    char *bufname;
{
  register struct buffer *old = current_buffer;
  register Lisp_Object buf;

  Fset_buffer (Fget_buffer_create (build_string (bufname)));

  current_buffer->directory = old->directory;
  current_buffer->read_only = Qnil;
  Ferase_buffer ();

  XSETBUFFER (buf, current_buffer);
  specbind (Qstandard_output, buf);

  set_buffer_internal (old);
}

Lisp_Object
internal_with_output_to_temp_buffer (bufname, function, args)
     char *bufname;
     Lisp_Object (*function) ();
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

DEFUN ("with-output-to-temp-buffer", Fwith_output_to_temp_buffer, Swith_output_to_temp_buffer,
       1, UNEVALLED, 0,
  "Bind `standard-output' to buffer BUFNAME, eval BODY, then show that buffer.\n\
The buffer is cleared out initially, and marked as unmodified when done.\n\
All output done by BODY is inserted in that buffer by default.\n\
The buffer is displayed in another window, but not selected.\n\
The value of the last form in BODY is returned.\n\
If BODY does not finish normally, the buffer BUFNAME is not displayed.\n\n\
If variable `temp-buffer-show-function' is non-nil, call it at the end\n\
to get the buffer displayed.  It gets one argument, the buffer to display.")
  (args)
     Lisp_Object args;
{
  struct gcpro gcpro1;
  Lisp_Object name;
  int count = specpdl_ptr - specpdl;
  Lisp_Object buf, val;

  GCPRO1(args);
  name = Feval (Fcar (args));
  UNGCPRO;

  CHECK_STRING (name, 0);
  temp_output_buffer_setup (XSTRING (name)->data);
  buf = Vstandard_output;

  val = Fprogn (Fcdr (args));

  temp_output_buffer_show (buf);

  return unbind_to (count, val);
}
#endif /* not standalone */

static void print ();

DEFUN ("terpri", Fterpri, Sterpri, 0, 1, 0,
  "Output a newline to stream PRINTCHARFUN.\n\
If PRINTCHARFUN is omitted or nil, the value of `standard-output' is used.")
  (printcharfun)
     Lisp_Object printcharfun;
{
  struct buffer *old = current_buffer;
  int old_point = -1;
  int start_point;
  Lisp_Object original;

  if (NILP (printcharfun))
    printcharfun = Vstandard_output;
  PRINTPREPARE;
  PRINTCHAR ('\n');
  PRINTFINISH;
  return Qt;
}

DEFUN ("prin1", Fprin1, Sprin1, 1, 2, 0,
  "Output the printed representation of OBJECT, any Lisp object.\n\
Quoting characters are printed when needed to make output that `read'\n\
can handle, whenever this is possible.\n\
Output stream is PRINTCHARFUN, or value of `standard-output' (which see).")
  (obj, printcharfun)
     Lisp_Object obj, printcharfun;
{
  struct buffer *old = current_buffer;
  int old_point = -1;
  int start_point;
  Lisp_Object original;

#ifdef MAX_PRINT_CHARS
  max_print = 0;
#endif /* MAX_PRINT_CHARS */
  if (NILP (printcharfun))
    printcharfun = Vstandard_output;
  PRINTPREPARE;
  print_depth = 0;
  print (obj, printcharfun, 1);
  PRINTFINISH;
  return obj;
}

/* a buffer which is used to hold output being built by prin1-to-string */
Lisp_Object Vprin1_to_string_buffer;

DEFUN ("prin1-to-string", Fprin1_to_string, Sprin1_to_string, 1, 2, 0,
  "Return a string containing the printed representation of OBJECT,\n\
any Lisp object.  Quoting characters are used when needed to make output\n\
that `read' can handle, whenever this is possible, unless the optional\n\
second argument NOESCAPE is non-nil.")
  (obj, noescape)
     Lisp_Object obj, noescape;
{
  struct buffer *old = current_buffer;
  int old_point = -1;
  int start_point;
  Lisp_Object original, printcharfun;
  struct gcpro gcpro1;

  printcharfun = Vprin1_to_string_buffer;
  PRINTPREPARE;
  print_depth = 0;
  print (obj, printcharfun, NILP (noescape));
  /* Make Vprin1_to_string_buffer be the default buffer after PRINTFINSH */
  PRINTFINISH;
  set_buffer_internal (XBUFFER (Vprin1_to_string_buffer));
  obj = Fbuffer_string ();

  GCPRO1 (obj);
  Ferase_buffer ();
  set_buffer_internal (old);
  UNGCPRO;

  return obj;
}

DEFUN ("princ", Fprinc, Sprinc, 1, 2, 0,
  "Output the printed representation of OBJECT, any Lisp object.\n\
No quoting characters are used; no delimiters are printed around\n\
the contents of strings.\n\
Output stream is PRINTCHARFUN, or value of standard-output (which see).")
  (obj, printcharfun)
     Lisp_Object obj, printcharfun;
{
  struct buffer *old = current_buffer;
  int old_point = -1;
  int start_point;
  Lisp_Object original;

  if (NILP (printcharfun))
    printcharfun = Vstandard_output;
  PRINTPREPARE;
  print_depth = 0;
  print (obj, printcharfun, 0);
  PRINTFINISH;
  return obj;
}

DEFUN ("print", Fprint, Sprint, 1, 2, 0,
  "Output the printed representation of OBJECT, with newlines around it.\n\
Quoting characters are printed when needed to make output that `read'\n\
can handle, whenever this is possible.\n\
Output stream is PRINTCHARFUN, or value of `standard-output' (which see).")
  (obj, printcharfun)
     Lisp_Object obj, printcharfun;
{
  struct buffer *old = current_buffer;
  int old_point = -1;
  int start_point;
  Lisp_Object original;
  struct gcpro gcpro1;

#ifdef MAX_PRINT_CHARS
  print_chars = 0;
  max_print = MAX_PRINT_CHARS;
#endif /* MAX_PRINT_CHARS */
  if (NILP (printcharfun))
    printcharfun = Vstandard_output;
  GCPRO1 (obj);
  PRINTPREPARE;
  print_depth = 0;
  PRINTCHAR ('\n');
  print (obj, printcharfun, 1);
  PRINTCHAR ('\n');
  PRINTFINISH;
#ifdef MAX_PRINT_CHARS
  max_print = 0;
  print_chars = 0;
#endif /* MAX_PRINT_CHARS */
  UNGCPRO;
  return obj;
}

/* The subroutine object for external-debugging-output is kept here
   for the convenience of the debugger.  */
Lisp_Object Qexternal_debugging_output;

DEFUN ("external-debugging-output", Fexternal_debugging_output, Sexternal_debugging_output, 1, 1, 0,
  "Write CHARACTER to stderr.\n\
You can call print while debugging emacs, and pass it this function\n\
to make it write to the debugging output.\n")
  (character)
     Lisp_Object character;
{
  CHECK_NUMBER (character, 0);
  putc (XINT (character), stderr);
  
  return character;
}

/* This is the interface for debugging printing.  */

void
debug_print (arg)
     Lisp_Object arg;
{
  Fprin1 (arg, Qexternal_debugging_output);
}

#ifdef LISP_FLOAT_TYPE

/*
 * The buffer should be at least as large as the max string size of the
 * largest float, printed in the biggest notation.  This is undoubtably
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
      
  if (NILP (Vfloat_output_format)
      || !STRINGP (Vfloat_output_format))
  lose:
    {
      sprintf (buf, "%.17g", data);
      width = -1;
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
	for (width = 0; (*cp >= '0' && *cp <= '9'); cp++)
	  width = (width * 10) + (*cp - '0');

      if (*cp != 'e' && *cp != 'f' && *cp != 'g')
	goto lose;

      /* A precision of zero is valid for %f; everything else requires
	 at least one.  Width may be omitted anywhere.  */
      if (width != -1
	  && (width < (*cp != 'f')
	      || width > DBL_DIG))
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
#endif /* LISP_FLOAT_TYPE */

static void
print (obj, printcharfun, escapeflag)
     Lisp_Object obj;
     register Lisp_Object printcharfun;
     int escapeflag;
{
  char buf[30];

  QUIT;

#if 1  /* I'm not sure this is really worth doing.  */
  /* Detect circularities and truncate them.
     No need to offer any alternative--this is better than an error.  */
  if (CONSP (obj) || VECTORP (obj) || COMPILEDP (obj))
    {
      int i;
      for (i = 0; i < print_depth; i++)
	if (EQ (obj, being_printed[i]))
	  {
	    sprintf (buf, "#%d", i);
	    strout (buf, -1, printcharfun);
	    return;
	  }
    }
#endif

  being_printed[print_depth] = obj;
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
      sprintf (buf, "%d", XINT (obj));
      strout (buf, -1, printcharfun);
      break;

#ifdef LISP_FLOAT_TYPE
    case Lisp_Float:
      {
	char pigbuf[350];	/* see comments in float_to_string */

	float_to_string (pigbuf, XFLOAT(obj)->data);
	strout (pigbuf, -1, printcharfun);
      }
      break;
#endif

    case Lisp_String:
      if (!escapeflag)
	print_string (obj, printcharfun);
      else
	{
	  register int i;
	  register unsigned char c;
	  struct gcpro gcpro1;

	  GCPRO1 (obj);

#ifdef USE_TEXT_PROPERTIES
	  if (!NULL_INTERVAL_P (XSTRING (obj)->intervals))
	    {
	      PRINTCHAR ('#');
	      PRINTCHAR ('(');
	    }
#endif

	  PRINTCHAR ('\"');
	  for (i = 0; i < XSTRING (obj)->size; i++)
	    {
	      QUIT;
	      c = XSTRING (obj)->data[i];
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

#ifdef USE_TEXT_PROPERTIES
	  if (!NULL_INTERVAL_P (XSTRING (obj)->intervals))
	    {
	      traverse_intervals (XSTRING (obj)->intervals,
				  0, 0, print_interval, printcharfun);
	      PRINTCHAR (')');
	    }
#endif

	  UNGCPRO;
	}
      break;

    case Lisp_Symbol:
      {
	register int confusing;
	register unsigned char *p = XSYMBOL (obj)->name->data;
	register unsigned char *end = p + XSYMBOL (obj)->name->size;
	register unsigned char c;

	if (p != end && (*p == '-' || *p == '+')) p++;
	if (p == end)
	  confusing = 0;
	else
	  {
	    while (p != end && *p >= '0' && *p <= '9')
	      p++;
	    confusing = (end == p);
	  }

	p = XSYMBOL (obj)->name->data;
	while (p != end)
	  {
	    QUIT;
	    c = *p++;
	    if (escapeflag)
	      {
		if (c == '\"' || c == '\\' || c == '\'' || c == ';' || c == '#' ||
		    c == '(' || c == ')' || c == ',' || c =='.' || c == '`' ||
		    c == '[' || c == ']' || c == '?' || c <= 040 || confusing)
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
	strout ("...", -1, printcharfun);
      else
	{
	  PRINTCHAR ('(');
	  {
	    register int i = 0;
	    register int max = 0;

	    if (INTEGERP (Vprint_length))
	      max = XINT (Vprint_length);
	    /* Could recognize circularities in cdrs here,
	       but that would make printing of long lists quadratic.
	       It's not worth doing.  */
	    while (CONSP (obj))
	      {
		if (i++)
		  PRINTCHAR (' ');
		if (max && i > max)
		  {
		    strout ("...", 3, printcharfun);
		    break;
		  }
		print (Fcar (obj), printcharfun, escapeflag);
		obj = Fcdr (obj);
	      }
	  }
	  if (!NILP (obj) && !CONSP (obj))
	    {
	      strout (" . ", 3, printcharfun);
	      print (obj, printcharfun, escapeflag);
	    }
	  PRINTCHAR (')');
	}
      break;

    case Lisp_Vectorlike:
      if (PROCESSP (obj))
	{
	  if (escapeflag)
	    {
	      strout ("#<process ", -1, printcharfun);
	      print_string (XPROCESS (obj)->name, printcharfun);
	      PRINTCHAR ('>');
	    }
	  else
	    print_string (XPROCESS (obj)->name, printcharfun);
	}
      else if (SUBRP (obj))
	{
	  strout ("#<subr ", -1, printcharfun);
	  strout (XSUBR (obj)->symbol_name, -1, printcharfun);
	  PRINTCHAR ('>');
	}
#ifndef standalone
      else if (WINDOWP (obj))
	{
	  strout ("#<window ", -1, printcharfun);
	  sprintf (buf, "%d", XFASTINT (XWINDOW (obj)->sequence_number));
	  strout (buf, -1, printcharfun);
	  if (!NILP (XWINDOW (obj)->buffer))
	    {
	      strout (" on ", -1, printcharfun);
	      print_string (XBUFFER (XWINDOW (obj)->buffer)->name, printcharfun);
	    }
	  PRINTCHAR ('>');
	}
      else if (BUFFERP (obj))
	{
	  if (NILP (XBUFFER (obj)->name))
	    strout ("#<killed buffer>", -1, printcharfun);
	  else if (escapeflag)
	    {
	      strout ("#<buffer ", -1, printcharfun);
	      print_string (XBUFFER (obj)->name, printcharfun);
	      PRINTCHAR ('>');
	    }
	  else
	    print_string (XBUFFER (obj)->name, printcharfun);
	}
      else if (WINDOW_CONFIGURATIONP (obj))
	{
	  strout ("#<window-configuration>", -1, printcharfun);
	}
#ifdef MULTI_FRAME
      else if (FRAMEP (obj))
	{
	  strout ((FRAME_LIVE_P (XFRAME (obj))
		   ? "#<frame " : "#<dead frame "),
		  -1, printcharfun);
	  print_string (XFRAME (obj)->name, printcharfun);
	  sprintf (buf, " 0x%lx", (unsigned long) (XFRAME (obj)));
	  strout (buf, -1, printcharfun);
	  PRINTCHAR ('>');
	}
#endif
#endif /* not standalone */
      else
	{
	  int size = XVECTOR (obj)->size;
	  if (COMPILEDP (obj))
	    {
	      PRINTCHAR ('#');
	      size &= PSEUDOVECTOR_SIZE_MASK;
	    }
	  if (size & PSEUDOVECTOR_FLAG)
	    goto badtype;

	  PRINTCHAR ('[');
	  {
	    register int i;
	    register Lisp_Object tem;
	    for (i = 0; i < size; i++)
	      {
		if (i) PRINTCHAR (' ');
		tem = XVECTOR (obj)->contents[i];
		print (tem, printcharfun, escapeflag);
	      }
	  }
	  PRINTCHAR (']');
	}
      break;

#ifndef standalone
    case Lisp_Misc:
      switch (XMISCTYPE (obj))
	{
	case Lisp_Misc_Marker:
	  strout ("#<marker ", -1, printcharfun);
	  if (!(XMARKER (obj)->buffer))
	    strout ("in no buffer", -1, printcharfun);
	  else
	    {
	      sprintf (buf, "at %d", marker_position (obj));
	      strout (buf, -1, printcharfun);
	      strout (" in ", -1, printcharfun);
	      print_string (XMARKER (obj)->buffer->name, printcharfun);
	    }
	  PRINTCHAR ('>');
	  break;

	case Lisp_Misc_Overlay:
	  strout ("#<overlay ", -1, printcharfun);
	  if (!(XMARKER (OVERLAY_START (obj))->buffer))
	    strout ("in no buffer", -1, printcharfun);
	  else
	    {
	      sprintf (buf, "from %d to %d in ",
		       marker_position (OVERLAY_START (obj)),
		       marker_position (OVERLAY_END   (obj)));
	      strout (buf, -1, printcharfun);
	      print_string (XMARKER (OVERLAY_START (obj))->buffer->name,
			    printcharfun);
	    }
	  PRINTCHAR ('>');
	  break;

      /* Remaining cases shouldn't happen in normal usage, but let's print
	 them anyway for the benefit of the debugger.  */
	case Lisp_Misc_Free:
	  strout ("#<misc free cell>", -1, printcharfun);
	  break;

	case Lisp_Misc_Intfwd:
	  sprintf (buf, "#<intfwd to %d>", *XINTFWD (obj)->intvar);
	  strout (buf, -1, printcharfun);
	  break;

	case Lisp_Misc_Boolfwd:
	  sprintf (buf, "#<boolfwd to %s>",
		   (*XBOOLFWD (obj)->boolvar ? "t" : "nil"));
	  strout (buf, -1, printcharfun);
	  break;

	case Lisp_Misc_Objfwd:
	  strout (buf, "#<objfwd to ", -1, printcharfun);
	  print (*XOBJFWD (obj)->objvar, printcharfun, escapeflag);
	  PRINTCHAR ('>');
	  break;

	case Lisp_Misc_Buffer_Objfwd:
	  strout (buf, "#<buffer_objfwd to ", -1, printcharfun);
	  print (*(Lisp_Object *)((char *)current_buffer
				  + XBUFFER_OBJFWD (obj)->offset),
		 printcharfun, escapeflag);
	  PRINTCHAR ('>');
	  break;

	case Lisp_Misc_Kboard_Objfwd:
	  strout (buf, "#<kboard_objfwd to ", -1, printcharfun);
	  print (*(Lisp_Object *)((char *) current_kboard
				  + XKBOARD_OBJFWD (obj)->offset),
		 printcharfun, escapeflag);
	  PRINTCHAR ('>');
	  break;

	case Lisp_Misc_Buffer_Local_Value:
	  strout ("#<buffer_local_value ", -1, printcharfun);
	  goto do_buffer_local;
	case Lisp_Misc_Some_Buffer_Local_Value:
	  strout ("#<some_buffer_local_value ", -1, printcharfun);
	do_buffer_local:
	  strout ("[realvalue] ", -1, printcharfun);
	  print (XBUFFER_LOCAL_VALUE (obj)->car, printcharfun, escapeflag);
	  strout ("[buffer] ", -1, printcharfun);
	  print (XCONS (XBUFFER_LOCAL_VALUE (obj)->cdr)->car,
		 printcharfun, escapeflag);
	  strout ("[alist-elt] ", -1, printcharfun);
	  print (XCONS (XCONS (XBUFFER_LOCAL_VALUE (obj)->cdr)->cdr)->car,
		 printcharfun, escapeflag);
	  strout ("[default-value] ", -1, printcharfun);
	  print (XCONS (XCONS (XBUFFER_LOCAL_VALUE (obj)->cdr)->cdr)->cdr,
		 printcharfun, escapeflag);
	  PRINTCHAR ('>');
	  break;

	default:
	  goto badtype;
	}
      break;
#endif /* standalone */

    default:
    badtype:
      {
	/* We're in trouble if this happens!
	   Probably should just abort () */
	strout ("#<EMACS BUG: INVALID DATATYPE ", -1, printcharfun);
	if (MISCP (obj))
	  sprintf (buf, "(MISC 0x%04x)", (int) XMISCTYPE (obj));
	else if (VECTORLIKEP (obj))
	  sprintf (buf, "(PVEC 0x%08x)", (int) XVECTOR (obj)->size);
	else
	  sprintf (buf, "(0x%02x)", (int) XTYPE (obj));
	strout (buf, -1, printcharfun);
	strout (" Save your buffers immediately and please report this bug>",
		-1, printcharfun);
      }
    }

  print_depth--;
}

#ifdef USE_TEXT_PROPERTIES

/* Print a description of INTERVAL using PRINTCHARFUN.
   This is part of printing a string that has text properties.  */

void
print_interval (interval, printcharfun)
     INTERVAL interval;
     Lisp_Object printcharfun;
{
  PRINTCHAR (' ');
  print (make_number (interval->position), printcharfun, 1);
  PRINTCHAR (' ');
  print (make_number (interval->position + LENGTH (interval)),
	 printcharfun, 1);
  PRINTCHAR (' ');
  print (interval->plist, printcharfun, 1);
}

#endif /* USE_TEXT_PROPERTIES */

void
syms_of_print ()
{
  staticpro (&Qprint_escape_newlines);
  Qprint_escape_newlines = intern ("print-escape-newlines");

  DEFVAR_LISP ("standard-output", &Vstandard_output,
    "Output stream `print' uses by default for outputting a character.\n\
This may be any function of one argument.\n\
It may also be a buffer (output is inserted before point)\n\
or a marker (output is inserted and the marker is advanced)\n\
or the symbol t (output appears in the minibuffer line).");
  Vstandard_output = Qt;
  Qstandard_output = intern ("standard-output");
  staticpro (&Qstandard_output);

#ifdef LISP_FLOAT_TYPE
  DEFVAR_LISP ("float-output-format", &Vfloat_output_format,
    "The format descriptor string used to print floats.\n\
This is a %-spec like those accepted by `printf' in C,\n\
but with some restrictions.  It must start with the two characters `%.'.\n\
After that comes an integer precision specification,\n\
and then a letter which controls the format.\n\
The letters allowed are `e', `f' and `g'.\n\
Use `e' for exponential notation \"DIG.DIGITSeEXPT\"\n\
Use `f' for decimal point notation \"DIGITS.DIGITS\".\n\
Use `g' to choose the shorter of those two formats for the number at hand.\n\
The precision in any of these cases is the number of digits following\n\
the decimal point.  With `f', a precision of 0 means to omit the\n\
decimal point.  0 is not allowed with `e' or `g'.\n\n\
A value of nil means to use `%.17g'.");
  Vfloat_output_format = Qnil;
  Qfloat_output_format = intern ("float-output-format");
  staticpro (&Qfloat_output_format);
#endif /* LISP_FLOAT_TYPE */

  DEFVAR_LISP ("print-length", &Vprint_length,
    "Maximum length of list to print before abbreviating.\n\
A value of nil means no limit.");
  Vprint_length = Qnil;

  DEFVAR_LISP ("print-level", &Vprint_level,
    "Maximum depth of list nesting to print before abbreviating.\n\
A value of nil means no limit.");
  Vprint_level = Qnil;

  DEFVAR_BOOL ("print-escape-newlines", &print_escape_newlines,
    "Non-nil means print newlines in strings as backslash-n.\n\
Also print formfeeds as backslash-f.");
  print_escape_newlines = 0;

  /* prin1_to_string_buffer initialized in init_buffer_once in buffer.c */
  staticpro (&Vprin1_to_string_buffer);

  defsubr (&Sprin1);
  defsubr (&Sprin1_to_string);
  defsubr (&Sprinc);
  defsubr (&Sprint);
  defsubr (&Sterpri);
  defsubr (&Swrite_char);
  defsubr (&Sexternal_debugging_output);

  Qexternal_debugging_output = intern ("external-debugging-output");
  staticpro (&Qexternal_debugging_output);

#ifndef standalone
  defsubr (&Swith_output_to_temp_buffer);
#endif /* not standalone */
}
