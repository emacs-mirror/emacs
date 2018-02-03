/* Lisp object printing and output streams.

Copyright (C) 1985-1986, 1988, 1993-1995, 1997-2018 Free Software
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
#include "sysstdio.h"

#include "lisp.h"
#include "character.h"
#include "coding.h"
#include "buffer.h"
#include "charset.h"
#include "frame.h"
#include "process.h"
#include "disptab.h"
#include "intervals.h"
#include "blockinput.h"
#include "xwidget.h"
#include "dynlib.h"

#include <c-ctype.h>
#include <float.h>
#include <ftoastr.h>

#ifdef WINDOWSNT
# include <sys/socket.h> /* for F_DUPFD_CLOEXEC */
#endif

struct terminal;

/* Avoid actual stack overflow in print.  */
static ptrdiff_t print_depth;

/* Level of nesting inside outputting backquote in new style.  */
static ptrdiff_t new_backquote_output;

/* Detect most circularities to print finite output.  */
#define PRINT_CIRCLE 200
static Lisp_Object being_printed[PRINT_CIRCLE];

/* Last char printed to stdout by printchar.  */
static unsigned int printchar_stdout_last;

/* When printing into a buffer, first we put the text in this
   block, then insert it all at once.  */
static char *print_buffer;

/* Size allocated in print_buffer.  */
static ptrdiff_t print_buffer_size;
/* Chars stored in print_buffer.  */
static ptrdiff_t print_buffer_pos;
/* Bytes stored in print_buffer.  */
static ptrdiff_t print_buffer_pos_byte;

/* Vprint_number_table is a table, that keeps objects that are going to
   be printed, to allow use of #n= and #n# to express sharing.
   For any given object, the table can give the following values:
     t    the object will be printed only once.
     -N   the object will be printed several times and will take number N.
     N    the object has been printed so we can refer to it as #N#.
   print_number_index holds the largest N already used.
   N has to be striclty larger than 0 since we need to distinguish -N.  */
static ptrdiff_t print_number_index;
static void print_interval (INTERVAL interval, Lisp_Object printcharfun);

/* GDB resets this to zero on W32 to disable OutputDebugString calls.  */
bool print_output_debug_flag EXTERNALLY_VISIBLE = 1;


/* Low level output routines for characters and strings.  */

/* Lisp functions to do output using a stream
   must have the stream in a variable called printcharfun
   and must start with PRINTPREPARE, end with PRINTFINISH.
   Use printchar to output one character,
   or call strout to output a block of characters.  */

#define PRINTPREPARE							\
   struct buffer *old = current_buffer;					\
   ptrdiff_t old_point = -1, start_point = -1;				\
   ptrdiff_t old_point_byte = -1, start_point_byte = -1;		\
   ptrdiff_t specpdl_count = SPECPDL_INDEX ();				\
   bool free_print_buffer = 0;						\
   bool multibyte							\
     = !NILP (BVAR (current_buffer, enable_multibyte_characters));	\
   Lisp_Object original = printcharfun;					\
   if (NILP (printcharfun)) printcharfun = Qt;				\
   if (BUFFERP (printcharfun))						\
     {									\
       if (XBUFFER (printcharfun) != current_buffer)			\
	 Fset_buffer (printcharfun);					\
       printcharfun = Qnil;						\
     }									\
   if (MARKERP (printcharfun))						\
     {									\
       ptrdiff_t marker_pos;						\
       if (! XMARKER (printcharfun)->buffer)				\
         error ("Marker does not point anywhere");			\
       if (XMARKER (printcharfun)->buffer != current_buffer)		\
         set_buffer_internal (XMARKER (printcharfun)->buffer);		\
       marker_pos = marker_position (printcharfun);			\
       if (marker_pos < BEGV || marker_pos > ZV)			\
	 signal_error ("Marker is outside the accessible "		\
		       "part of the buffer", printcharfun);		\
       old_point = PT;							\
       old_point_byte = PT_BYTE;					\
       SET_PT_BOTH (marker_pos,						\
		    marker_byte_position (printcharfun));		\
       start_point = PT;						\
       start_point_byte = PT_BYTE;					\
       printcharfun = Qnil;						\
     }									\
   if (NILP (printcharfun))						\
     {									\
       Lisp_Object string;						\
       if (NILP (BVAR (current_buffer, enable_multibyte_characters))	\
	   && ! print_escape_multibyte)					\
         specbind (Qprint_escape_multibyte, Qt);			\
       if (! NILP (BVAR (current_buffer, enable_multibyte_characters))	\
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
	   int new_size = 1000;						\
	   print_buffer = xmalloc (new_size);				\
	   print_buffer_size = new_size;				\
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
	   && NILP (BVAR (current_buffer, enable_multibyte_characters)))\
	 {								\
	   USE_SAFE_ALLOCA;						\
	   unsigned char *temp = SAFE_ALLOCA (print_buffer_pos + 1);	\
	   copy_text ((unsigned char *) print_buffer, temp,		\
		      print_buffer_pos_byte, 1, 0);			\
	   insert_1_both ((char *) temp, print_buffer_pos,		\
			  print_buffer_pos, 0, 1, 0);			\
	   SAFE_FREE ();						\
	 }								\
       else								\
	 insert_1_both (print_buffer, print_buffer_pos,			\
			print_buffer_pos_byte, 0, 1, 0);		\
       signal_after_change (PT - print_buffer_pos, 0, print_buffer_pos);\
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
   set_buffer_internal (old);

/* This is used to restore the saved contents of print_buffer
   when there is a recursive call to print.  */

static void
print_unwind (Lisp_Object saved_text)
{
  memcpy (print_buffer, SDATA (saved_text), SCHARS (saved_text));
}

/* Print character CH to the stdio stream STREAM.  */

static void
printchar_to_stream (unsigned int ch, FILE *stream)
{
  Lisp_Object dv UNINIT;
  ptrdiff_t i = 0, n = 1;
  Lisp_Object coding_system = Vlocale_coding_system;
  bool encode_p = false;

  if (!NILP (Vcoding_system_for_write))
    coding_system = Vcoding_system_for_write;
  if (!NILP (coding_system))
    encode_p = true;

  if (CHAR_VALID_P (ch) && DISP_TABLE_P (Vstandard_display_table))
    {
      dv = DISP_CHAR_VECTOR (XCHAR_TABLE (Vstandard_display_table), ch);
      if (VECTORP (dv))
	{
	  n = ASIZE (dv);
	  goto next_char;
	}
    }

  while (true)
    {
      if (ASCII_CHAR_P (ch))
	{
	  putc_unlocked (ch, stream);
#ifdef WINDOWSNT
	  /* Send the output to a debugger (nothing happens if there
	     isn't one).  */
	  if (print_output_debug_flag && stream == stderr)
	    OutputDebugString ((char []) {ch, '\0'});
#endif
	}
      else
	{
	  unsigned char mbstr[MAX_MULTIBYTE_LENGTH];
	  int len = CHAR_STRING (ch, mbstr);
	  Lisp_Object encoded_ch =
	    make_multibyte_string ((char *) mbstr, 1, len);

	  if (encode_p)
	    encoded_ch = code_convert_string_norecord (encoded_ch,
						       coding_system, true);
	  fwrite_unlocked (SSDATA (encoded_ch), 1, SBYTES (encoded_ch), stream);
#ifdef WINDOWSNT
	  if (print_output_debug_flag && stream == stderr)
	    OutputDebugString (SSDATA (encoded_ch));
#endif
	}

      i++;

    next_char:
      for (; i < n; i++)
	if (CHARACTERP (AREF (dv, i)))
	  break;
      if (! (i < n))
	break;
      ch = XFASTINT (AREF (dv, i));
    }
}

/* Print character CH using method FUN.  FUN nil means print to
   print_buffer.  FUN t means print to echo area or stdout if
   non-interactive.  If FUN is neither nil nor t, call FUN with CH as
   argument.  */

static void
printchar (unsigned int ch, Lisp_Object fun)
{
  if (!NILP (fun) && !EQ (fun, Qt))
    call1 (fun, make_number (ch));
  else
    {
      unsigned char str[MAX_MULTIBYTE_LENGTH];
      int len = CHAR_STRING (ch, str);

      maybe_quit ();

      if (NILP (fun))
	{
	  ptrdiff_t incr = len - (print_buffer_size - print_buffer_pos_byte);
	  if (incr > 0)
	    print_buffer = xpalloc (print_buffer, &print_buffer_size,
				    incr, -1, 1);
	  memcpy (print_buffer + print_buffer_pos_byte, str, len);
	  print_buffer_pos += 1;
	  print_buffer_pos_byte += len;
	}
      else if (noninteractive)
	{
	  printchar_stdout_last = ch;
	  if (DISP_TABLE_P (Vstandard_display_table))
	    printchar_to_stream (ch, stdout);
	  else
	    fwrite_unlocked (str, 1, len, stdout);
	  noninteractive_need_newline = 1;
	}
      else
	{
	  bool multibyte_p
	    = !NILP (BVAR (current_buffer, enable_multibyte_characters));

	  setup_echo_area_for_printing (multibyte_p);
	  insert_char (ch);
	  message_dolog ((char *) str, len, 0, multibyte_p);
	}
    }
}


/* Output SIZE characters, SIZE_BYTE bytes from string PTR using
   method PRINTCHARFUN.  PRINTCHARFUN nil means output to
   print_buffer.  PRINTCHARFUN t means output to the echo area or to
   stdout if non-interactive.  If neither nil nor t, call Lisp
   function PRINTCHARFUN for each character printed.  MULTIBYTE
   non-zero means PTR contains multibyte characters.

   In the case where PRINTCHARFUN is nil, it is safe for PTR to point
   to data in a Lisp string.  Otherwise that is not safe.  */

static void
strout (const char *ptr, ptrdiff_t size, ptrdiff_t size_byte,
	Lisp_Object printcharfun)
{
  if (NILP (printcharfun))
    {
      ptrdiff_t incr = size_byte - (print_buffer_size - print_buffer_pos_byte);
      if (incr > 0)
	print_buffer = xpalloc (print_buffer, &print_buffer_size, incr, -1, 1);
      memcpy (print_buffer + print_buffer_pos_byte, ptr, size_byte);
      print_buffer_pos += size;
      print_buffer_pos_byte += size_byte;
    }
  else if (noninteractive && EQ (printcharfun, Qt))
    {
      if (DISP_TABLE_P (Vstandard_display_table))
	{
	  int len;
	  for (ptrdiff_t i = 0; i < size_byte; i += len)
	    {
	      int ch = STRING_CHAR_AND_LENGTH ((const unsigned char *) ptr + i,
					       len);
	      printchar_to_stream (ch, stdout);
	    }
	}
      else
	fwrite_unlocked (ptr, 1, size_byte, stdout);

      noninteractive_need_newline = 1;
    }
  else if (EQ (printcharfun, Qt))
    {
      /* Output to echo area.  We're trying to avoid a little overhead
	 here, that's the reason we don't call printchar to do the
	 job.  */
      int i;
      bool multibyte_p
	= !NILP (BVAR (current_buffer, enable_multibyte_characters));

      setup_echo_area_for_printing (multibyte_p);
      message_dolog (ptr, size_byte, 0, multibyte_p);

      if (size == size_byte)
	{
	  for (i = 0; i < size; ++i)
	    insert_char ((unsigned char) *ptr++);
	}
      else
	{
	  int len;
	  for (i = 0; i < size_byte; i += len)
	    {
	      int ch = STRING_CHAR_AND_LENGTH ((const unsigned char *) ptr + i,
					       len);
	      insert_char (ch);
	    }
	}
    }
  else
    {
      /* PRINTCHARFUN is a Lisp function.  */
      ptrdiff_t i = 0;

      if (size == size_byte)
	{
	  while (i < size_byte)
	    {
	      int ch = ptr[i++];
	      printchar (ch, printcharfun);
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
	      int ch = STRING_CHAR_AND_LENGTH ((const unsigned char *) ptr + i,
					       len);
	      printchar (ch, printcharfun);
	      i += len;
	    }
	}
    }
}

/* Print the contents of a string STRING using PRINTCHARFUN.
   It isn't safe to use strout in many cases,
   because printing one char can relocate.  */

static void
print_string (Lisp_Object string, Lisp_Object printcharfun)
{
  if (EQ (printcharfun, Qt) || NILP (printcharfun))
    {
      ptrdiff_t chars;

      if (print_escape_nonascii)
	string = string_escape_byte8 (string);

      if (STRING_MULTIBYTE (string))
	chars = SCHARS (string);
      else if (! print_escape_nonascii
	       && (EQ (printcharfun, Qt)
		   ? ! NILP (BVAR (&buffer_defaults, enable_multibyte_characters))
		   : ! NILP (BVAR (current_buffer, enable_multibyte_characters))))
	{
	  /* If unibyte string STRING contains 8-bit codes, we must
	     convert STRING to a multibyte string containing the same
	     character codes.  */
	  Lisp_Object newstr;
	  ptrdiff_t bytes;

	  chars = SBYTES (string);
	  bytes = count_size_as_multibyte (SDATA (string), chars);
	  if (chars < bytes)
	    {
	      newstr = make_uninit_multibyte_string (chars, bytes);
	      memcpy (SDATA (newstr), SDATA (string), chars);
	      str_to_multibyte (SDATA (newstr), bytes, chars);
	      string = newstr;
	    }
	}
      else
	chars = SBYTES (string);

      if (EQ (printcharfun, Qt))
	{
	  /* Output to echo area.  */
	  ptrdiff_t nbytes = SBYTES (string);

	  /* Copy the string contents so that relocation of STRING by
	     GC does not cause trouble.  */
	  USE_SAFE_ALLOCA;
	  char *buffer = SAFE_ALLOCA (nbytes);
	  memcpy (buffer, SDATA (string), nbytes);

	  strout (buffer, chars, nbytes, printcharfun);

	  SAFE_FREE ();
	}
      else
	/* No need to copy, since output to print_buffer can't GC.  */
	strout (SSDATA (string), chars, SBYTES (string), printcharfun);
    }
  else
    {
      /* Otherwise, string may be relocated by printing one char.
	 So re-fetch the string address for each character.  */
      ptrdiff_t i;
      ptrdiff_t size = SCHARS (string);
      ptrdiff_t size_byte = SBYTES (string);
      if (size == size_byte)
	for (i = 0; i < size; i++)
	  printchar (SREF (string, i), printcharfun);
      else
	for (i = 0; i < size_byte; )
	  {
	    /* Here, we must convert each multi-byte form to the
	       corresponding character code before handing it to PRINTCHAR.  */
	    int len;
	    int ch = STRING_CHAR_AND_LENGTH (SDATA (string) + i, len);
	    printchar (ch, printcharfun);
	    i += len;
	  }
    }
}

DEFUN ("write-char", Fwrite_char, Swrite_char, 1, 2, 0,
       doc: /* Output character CHARACTER to stream PRINTCHARFUN.
PRINTCHARFUN defaults to the value of `standard-output' (which see).  */)
  (Lisp_Object character, Lisp_Object printcharfun)
{
  if (NILP (printcharfun))
    printcharfun = Vstandard_output;
  CHECK_NUMBER (character);
  PRINTPREPARE;
  printchar (XINT (character), printcharfun);
  PRINTFINISH;
  return character;
}

/* Print the contents of a unibyte C string STRING using PRINTCHARFUN.
   The caller should arrange to put this inside PRINTPREPARE and PRINTFINISH.
   Do not use this on the contents of a Lisp string.  */

static void
print_c_string (char const *string, Lisp_Object printcharfun)
{
  ptrdiff_t len = strlen (string);
  strout (string, len, len, printcharfun);
}

/* Print unibyte C string at DATA on a specified stream PRINTCHARFUN.
   Do not use this on the contents of a Lisp string.  */

static void
write_string (const char *data, Lisp_Object printcharfun)
{
  PRINTPREPARE;
  print_c_string (data, printcharfun);
  PRINTFINISH;
}


void
temp_output_buffer_setup (const char *bufname)
{
  ptrdiff_t count = SPECPDL_INDEX ();
  register struct buffer *old = current_buffer;
  register Lisp_Object buf;

  record_unwind_current_buffer ();

  Fset_buffer (Fget_buffer_create (build_string (bufname)));

  Fkill_all_local_variables ();
  delete_all_overlays (current_buffer);
  bset_directory (current_buffer, BVAR (old, directory));
  bset_read_only (current_buffer, Qnil);
  bset_filename (current_buffer, Qnil);
  bset_undo_list (current_buffer, Qt);
  eassert (current_buffer->overlays_before == NULL);
  eassert (current_buffer->overlays_after == NULL);
  bset_enable_multibyte_characters
    (current_buffer, BVAR (&buffer_defaults, enable_multibyte_characters));
  specbind (Qinhibit_read_only, Qt);
  specbind (Qinhibit_modification_hooks, Qt);
  Ferase_buffer ();
  XSETBUFFER (buf, current_buffer);

  run_hook (Qtemp_buffer_setup_hook);

  unbind_to (count, Qnil);

  specbind (Qstandard_output, buf);
}

static void print (Lisp_Object, Lisp_Object, bool);
static void print_preprocess (Lisp_Object);
static void print_preprocess_string (INTERVAL, void *);
static void print_object (Lisp_Object, Lisp_Object, bool);

DEFUN ("terpri", Fterpri, Sterpri, 0, 2, 0,
       doc: /* Output a newline to stream PRINTCHARFUN.
If ENSURE is non-nil only output a newline if not already at the
beginning of a line.  Value is non-nil if a newline is printed.
If PRINTCHARFUN is omitted or nil, the value of `standard-output' is used.  */)
  (Lisp_Object printcharfun, Lisp_Object ensure)
{
  Lisp_Object val;

  if (NILP (printcharfun))
    printcharfun = Vstandard_output;
  PRINTPREPARE;

  if (NILP (ensure))
    val = Qt;
  /* Difficult to check if at line beginning so abort.  */
  else if (FUNCTIONP (printcharfun))
    signal_error ("Unsupported function argument", printcharfun);
  else if (noninteractive && !NILP (printcharfun))
    val = printchar_stdout_last == 10 ? Qnil : Qt;
  else
    val = NILP (Fbolp ()) ? Qt : Qnil;

  if (!NILP (val))
    printchar ('\n', printcharfun);
  PRINTFINISH;
  return val;
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
  (Lisp_Object object, Lisp_Object printcharfun)
{
  if (NILP (printcharfun))
    printcharfun = Vstandard_output;
  PRINTPREPARE;
  print (object, printcharfun, 1);
  PRINTFINISH;
  return object;
}

/* A buffer which is used to hold output being built by prin1-to-string.  */
Lisp_Object Vprin1_to_string_buffer;

DEFUN ("prin1-to-string", Fprin1_to_string, Sprin1_to_string, 1, 2, 0,
       doc: /* Return a string containing the printed representation of OBJECT.
OBJECT can be any Lisp object.  This function outputs quoting characters
when necessary to make output that `read' can handle, whenever possible,
unless the optional second argument NOESCAPE is non-nil.  For complex objects,
the behavior is controlled by `print-level' and `print-length', which see.

OBJECT is any of the Lisp data types: a number, a string, a symbol,
a list, a buffer, a window, a frame, etc.

A printed representation of an object is text which describes that object.  */)
  (Lisp_Object object, Lisp_Object noescape)
{
  ptrdiff_t count = SPECPDL_INDEX ();

  specbind (Qinhibit_modification_hooks, Qt);

  /* Save and restore this: we are altering a buffer
     but we don't want to deactivate the mark just for that.
     No need for specbind, since errors deactivate the mark.  */
  Lisp_Object save_deactivate_mark = Vdeactivate_mark;

  Lisp_Object printcharfun = Vprin1_to_string_buffer;
  PRINTPREPARE;
  print (object, printcharfun, NILP (noescape));
  /* Make Vprin1_to_string_buffer be the default buffer after PRINTFINISH */
  PRINTFINISH;

  struct buffer *previous = current_buffer;
  set_buffer_internal (XBUFFER (Vprin1_to_string_buffer));
  object = Fbuffer_string ();
  if (SBYTES (object) == SCHARS (object))
    STRING_SET_UNIBYTE (object);

  /* Note that this won't make prepare_to_modify_buffer call
     ask-user-about-supersession-threat because this buffer
     does not visit a file.  */
  Ferase_buffer ();
  set_buffer_internal (previous);

  Vdeactivate_mark = save_deactivate_mark;

  return unbind_to (count, object);
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
  (Lisp_Object object, Lisp_Object printcharfun)
{
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
  (Lisp_Object object, Lisp_Object printcharfun)
{
  if (NILP (printcharfun))
    printcharfun = Vstandard_output;
  PRINTPREPARE;
  printchar ('\n', printcharfun);
  print (object, printcharfun, 1);
  printchar ('\n', printcharfun);
  PRINTFINISH;
  return object;
}

DEFUN ("external-debugging-output", Fexternal_debugging_output, Sexternal_debugging_output, 1, 1, 0,
       doc: /* Write CHARACTER to stderr.
You can call `print' while debugging emacs, and pass it this function
to make it write to the debugging output.  */)
  (Lisp_Object character)
{
  CHECK_NUMBER (character);
  printchar_to_stream (XINT (character), stderr);
  return character;
}

/* This function is never called.  Its purpose is to prevent
   print_output_debug_flag from being optimized away.  */

extern void debug_output_compilation_hack (bool) EXTERNALLY_VISIBLE;
void
debug_output_compilation_hack (bool x)
{
  print_output_debug_flag = x;
}

DEFUN ("redirect-debugging-output", Fredirect_debugging_output, Sredirect_debugging_output,
       1, 2,
       "FDebug output file: \nP",
       doc: /* Redirect debugging output (stderr stream) to file FILE.
If FILE is nil, reset target to the initial stderr stream.
Optional arg APPEND non-nil (interactively, with prefix arg) means
append to existing target file.  */)
  (Lisp_Object file, Lisp_Object append)
{
  /* If equal to STDERR_FILENO, stderr has not been duplicated and is OK as-is.
     Otherwise, this is a close-on-exec duplicate of the original stderr. */
  static int stderr_dup = STDERR_FILENO;
  int fd = stderr_dup;

  if (! NILP (file))
    {
      file = Fexpand_file_name (file, Qnil);

      if (stderr_dup == STDERR_FILENO)
	{
	  int n = fcntl (STDERR_FILENO, F_DUPFD_CLOEXEC, STDERR_FILENO + 1);
	  if (n < 0)
	    report_file_error ("dup", file);
	  stderr_dup = n;
	}

      fd = emacs_open (SSDATA (ENCODE_FILE (file)),
		       (O_WRONLY | O_CREAT
			| (! NILP (append) ? O_APPEND : O_TRUNC)),
		       0666);
      if (fd < 0)
	report_file_error ("Cannot open debugging output stream", file);
    }

  fflush_unlocked (stderr);
  if (dup2 (fd, STDERR_FILENO) < 0)
    report_file_error ("dup2", file);
  if (fd != stderr_dup)
    emacs_close (fd);
  return Qnil;
}


/* This is the interface for debugging printing.  */

void
debug_print (Lisp_Object arg)
{
  Fprin1 (arg, Qexternal_debugging_output);
  fprintf (stderr, "\r\n");
}

void safe_debug_print (Lisp_Object) EXTERNALLY_VISIBLE;
void
safe_debug_print (Lisp_Object arg)
{
  int valid = valid_lisp_object_p (arg);

  if (valid > 0)
    debug_print (arg);
  else
    {
      EMACS_UINT n = XLI (arg);
      fprintf (stderr, "#<%s_LISP_OBJECT 0x%08"pI"x>\r\n",
	       !valid ? "INVALID" : "SOME",
	       n);
    }
}


DEFUN ("error-message-string", Ferror_message_string, Serror_message_string,
       1, 1, 0,
       doc: /* Convert an error value (ERROR-SYMBOL . DATA) to an error message.
See Info anchor `(elisp)Definition of signal' for some details on how this
error message is constructed.  */)
  (Lisp_Object obj)
{
  struct buffer *old = current_buffer;
  Lisp_Object value;

  /* If OBJ is (error STRING), just return STRING.
     That is not only faster, it also avoids the need to allocate
     space here when the error is due to memory full.  */
  if (CONSP (obj) && EQ (XCAR (obj), Qerror)
      && CONSP (XCDR (obj))
      && STRINGP (XCAR (XCDR (obj)))
      && NILP (XCDR (XCDR (obj))))
    return XCAR (XCDR (obj));

  print_error_message (obj, Vprin1_to_string_buffer, 0, Qnil);

  set_buffer_internal (XBUFFER (Vprin1_to_string_buffer));
  value = Fbuffer_string ();

  Ferase_buffer ();
  set_buffer_internal (old);

  return value;
}

/* Print an error message for the error DATA onto Lisp output stream
   STREAM (suitable for the print functions).
   CONTEXT is a C string describing the context of the error.
   CALLER is the Lisp function inside which the error was signaled.  */

void
print_error_message (Lisp_Object data, Lisp_Object stream, const char *context,
		     Lisp_Object caller)
{
  Lisp_Object errname, errmsg, file_error, tail;

  if (context != 0)
    write_string (context, stream);

  /* If we know from where the error was signaled, show it in
   *Messages*.  */
  if (!NILP (caller) && SYMBOLP (caller))
    {
      Lisp_Object cname = SYMBOL_NAME (caller);
      ptrdiff_t cnamelen = SBYTES (cname);
      USE_SAFE_ALLOCA;
      char *name = SAFE_ALLOCA (cnamelen);
      memcpy (name, SDATA (cname), cnamelen);
      message_dolog (name, cnamelen, 0, STRING_MULTIBYTE (cname));
      message_dolog (": ", 2, 0, 0);
      SAFE_FREE ();
    }

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
      Lisp_Object error_conditions = Fget (errname, Qerror_conditions);
      errmsg = Fsubstitute_command_keys (Fget (errname, Qerror_message));
      file_error = Fmemq (Qfile_error, error_conditions);
    }

  /* Print an error message including the data items.  */

  tail = Fcdr_safe (data);

  /* For file-error, make error message by concatenating
     all the data items.  They are all strings.  */
  if (!NILP (file_error) && CONSP (tail))
    errmsg = XCAR (tail), tail = XCDR (tail);

  {
    const char *sep = ": ";

    if (!STRINGP (errmsg))
      write_string ("peculiar error", stream);
    else if (SCHARS (errmsg))
      Fprinc (errmsg, stream);
    else
      sep = NULL;

    for (; CONSP (tail); tail = XCDR (tail), sep = ", ")
      {
	Lisp_Object obj;

	if (sep)
	  write_string (sep, stream);
	obj = XCAR (tail);
	if (!NILP (file_error)
	    || EQ (errname, Qend_of_file) || EQ (errname, Quser_error))
	  Fprinc (obj, stream);
	else
	  Fprin1 (obj, stream);
      }
  }
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
 * Given the above, the buffer must be least FLOAT_TO_STRING_BUFSIZE bytes.
 */

int
float_to_string (char *buf, double data)
{
  char *cp;
  int width;
  int len;

  /* Check for plus infinity in a way that won't lose
     if there is no plus infinity.  */
  if (data == data / 2 && data > 1.0)
    {
      static char const infinity_string[] = "1.0e+INF";
      strcpy (buf, infinity_string);
      return sizeof infinity_string - 1;
    }
  /* Likewise for minus infinity.  */
  if (data == data / 2 && data < -1.0)
    {
      static char const minus_infinity_string[] = "-1.0e+INF";
      strcpy (buf, minus_infinity_string);
      return sizeof minus_infinity_string - 1;
    }
  /* Check for NaN in a way that won't fail if there are no NaNs.  */
  if (! (data * 0.0 >= 0.0))
    {
      /* Prepend "-" if the NaN's sign bit is negative.
	 The sign bit of a double is the bit that is 1 in -0.0.  */
      static char const NaN_string[] = "0.0e+NaN";
      int i;
      union { double d; char c[sizeof (double)]; } u_data, u_minus_zero;
      bool negative = 0;
      u_data.d = data;
      u_minus_zero.d = - 0.0;
      for (i = 0; i < sizeof (double); i++)
	if (u_data.c[i] & u_minus_zero.c[i])
	  {
	    *buf = '-';
	    negative = 1;
	    break;
	  }

      strcpy (buf + negative, NaN_string);
      return negative + sizeof NaN_string - 1;
    }

  if (NILP (Vfloat_output_format)
      || !STRINGP (Vfloat_output_format))
  lose:
    {
      /* Generate the fewest number of digits that represent the
	 floating point value without losing information.  */
      len = dtoastr (buf, FLOAT_TO_STRING_BUFSIZE - 2, 0, 0, data);
      /* The decimal point must be printed, or the byte compiler can
	 get confused (Bug#8033). */
      width = 1;
    }
  else			/* oink oink */
    {
      /* Check that the spec we have is fully valid.
	 This means not only valid for printf,
	 but meant for floats, and reasonable.  */
      cp = SSDATA (Vfloat_output_format);

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
	    {
	      width = (width * 10) + (*cp++ - '0');
	      if (DBL_DIG < width)
		goto lose;
	    }
	  while (*cp >= '0' && *cp <= '9');

	  /* A precision of zero is valid only for %f.  */
	  if (width == 0 && *cp != 'f')
	    goto lose;
	}

      if (*cp != 'e' && *cp != 'f' && *cp != 'g')
	goto lose;

      if (cp[1] != 0)
	goto lose;

      len = sprintf (buf, SSDATA (Vfloat_output_format), data);
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
	  len++;
	}
      else if (*cp == 0)
	{
	  *cp++ = '.';
	  *cp++ = '0';
	  *cp++ = 0;
	  len += 2;
	}
    }

  return len;
}


static void
print (Lisp_Object obj, Lisp_Object printcharfun, bool escapeflag)
{
  new_backquote_output = 0;

  /* Reset print_number_index and Vprint_number_table only when
     the variable Vprint_continuous_numbering is nil.  Otherwise,
     the values of these variables will be kept between several
     print functions.  */
  if (NILP (Vprint_continuous_numbering)
      || NILP (Vprint_number_table))
    {
      print_number_index = 0;
      Vprint_number_table = Qnil;
    }

  /* Construct Vprint_number_table for print-gensym and print-circle.  */
  if (!NILP (Vprint_gensym) || !NILP (Vprint_circle))
    {
      /* Construct Vprint_number_table.
	 This increments print_number_index for the objects added.  */
      print_depth = 0;
      print_preprocess (obj);

      if (HASH_TABLE_P (Vprint_number_table))
	{ /* Remove unnecessary objects, which appear only once in OBJ;
	     that is, whose status is Qt.  */
	  struct Lisp_Hash_Table *h = XHASH_TABLE (Vprint_number_table);
	  ptrdiff_t i;

	  for (i = 0; i < HASH_TABLE_SIZE (h); ++i)
	    if (!NILP (HASH_HASH (h, i))
		&& EQ (HASH_VALUE (h, i), Qt))
	      Fremhash (HASH_KEY (h, i), Vprint_number_table);
	}
    }

  print_depth = 0;
  print_object (obj, printcharfun, escapeflag);
}

#define PRINT_CIRCLE_CANDIDATE_P(obj)			   \
  (STRINGP (obj) || CONSP (obj)				   \
   || (VECTORLIKEP (obj)				   \
       && (VECTORP (obj) || COMPILEDP (obj)		   \
	   || CHAR_TABLE_P (obj) || SUB_CHAR_TABLE_P (obj) \
	   || HASH_TABLE_P (obj) || FONTP (obj)		   \
	   || RECORDP (obj)))				   \
   || (! NILP (Vprint_gensym)				   \
       && SYMBOLP (obj)					   \
       && !SYMBOL_INTERNED_P (obj)))

/* Construct Vprint_number_table according to the structure of OBJ.
   OBJ itself and all its elements will be added to Vprint_number_table
   recursively if it is a list, vector, compiled function, char-table,
   string (its text properties will be traced), or a symbol that has
   no obarray (this is for the print-gensym feature).
   The status fields of Vprint_number_table mean whether each object appears
   more than once in OBJ: Qnil at the first time, and Qt after that.  */
static void
print_preprocess (Lisp_Object obj)
{
  int i;
  ptrdiff_t size;
  int loop_count = 0;
  Lisp_Object halftail;

  /* Avoid infinite recursion for circular nested structure
     in the case where Vprint_circle is nil.  */
  if (NILP (Vprint_circle))
    {
      /* Give up if we go so deep that print_object will get an error.  */
      /* See similar code in print_object.  */
      if (print_depth >= PRINT_CIRCLE)
	error ("Apparently circular structure being printed");

      for (i = 0; i < print_depth; i++)
	if (EQ (obj, being_printed[i]))
	  return;
      being_printed[print_depth] = obj;
    }

  print_depth++;
  halftail = obj;

 loop:
  if (PRINT_CIRCLE_CANDIDATE_P (obj))
    {
      if (!HASH_TABLE_P (Vprint_number_table))
	Vprint_number_table = CALLN (Fmake_hash_table, QCtest, Qeq);

      /* In case print-circle is nil and print-gensym is t,
	 add OBJ to Vprint_number_table only when OBJ is a symbol.  */
      if (! NILP (Vprint_circle) || SYMBOLP (obj))
	{
	  Lisp_Object num = Fgethash (obj, Vprint_number_table, Qnil);
	  if (!NILP (num)
	      /* If Vprint_continuous_numbering is non-nil and OBJ is a gensym,
		 always print the gensym with a number.  This is a special for
		 the lisp function byte-compile-output-docform.  */
	      || (!NILP (Vprint_continuous_numbering)
		  && SYMBOLP (obj)
		  && !SYMBOL_INTERNED_P (obj)))
	    { /* OBJ appears more than once.	Let's remember that.  */
	      if (!INTEGERP (num))
		{
		  print_number_index++;
		  /* Negative number indicates it hasn't been printed yet.  */
		  Fputhash (obj, make_number (- print_number_index),
			    Vprint_number_table);
		}
	      print_depth--;
	      return;
	    }
	  else
	    /* OBJ is not yet recorded.  Let's add to the table.  */
	    Fputhash (obj, Qt, Vprint_number_table);
	}

      switch (XTYPE (obj))
	{
	case Lisp_String:
	  /* A string may have text properties, which can be circular.  */
	  traverse_intervals_noorder (string_intervals (obj),
				      print_preprocess_string, NULL);
	  break;

	case Lisp_Cons:
	  /* Use HALFTAIL and LOOP_COUNT to detect circular lists,
	     just as in print_object.  */
	  if (loop_count && EQ (obj, halftail))
	    break;
	  print_preprocess (XCAR (obj));
	  obj = XCDR (obj);
	  loop_count++;
	  if (!(loop_count & 1))
	    halftail = XCDR (halftail);
	  goto loop;

	case Lisp_Vectorlike:
	  size = ASIZE (obj);
	  if (size & PSEUDOVECTOR_FLAG)
	    size &= PSEUDOVECTOR_SIZE_MASK;
	  for (i = (SUB_CHAR_TABLE_P (obj)
		    ? SUB_CHAR_TABLE_OFFSET : 0); i < size; i++)
	    print_preprocess (AREF (obj, i));
	  if (HASH_TABLE_P (obj))
	    { /* For hash tables, the key_and_value slot is past
		 `size' because it needs to be marked specially in case
		 the table is weak.  */
	      struct Lisp_Hash_Table *h = XHASH_TABLE (obj);
	      print_preprocess (h->key_and_value);
	    }
	  break;

	default:
	  break;
	}
    }
  print_depth--;
}

DEFUN ("print--preprocess", Fprint_preprocess, Sprint_preprocess, 1, 1, 0,
       doc: /* Extract sharing info from OBJECT needed to print it.
Fills `print-number-table'.  */)
  (Lisp_Object object)
{
  print_number_index = 0;
  print_preprocess (object);
  return Qnil;
}

static void
print_preprocess_string (INTERVAL interval, void *arg)
{
  print_preprocess (interval->plist);
}

static void print_check_string_charset_prop (INTERVAL interval, Lisp_Object string);

#define PRINT_STRING_NON_CHARSET_FOUND 1
#define PRINT_STRING_UNSAFE_CHARSET_FOUND 2

/* Bitwise or of the above macros.  */
static int print_check_string_result;

static void
print_check_string_charset_prop (INTERVAL interval, Lisp_Object string)
{
  Lisp_Object val;

  if (NILP (interval->plist)
      || (print_check_string_result == (PRINT_STRING_NON_CHARSET_FOUND
					| PRINT_STRING_UNSAFE_CHARSET_FOUND)))
    return;
  for (val = interval->plist; CONSP (val) && ! EQ (XCAR (val), Qcharset);
       val = XCDR (XCDR (val)));
  if (! CONSP (val))
    {
      print_check_string_result |= PRINT_STRING_NON_CHARSET_FOUND;
      return;
    }
  if (! (print_check_string_result & PRINT_STRING_NON_CHARSET_FOUND))
    {
      if (! EQ (val, interval->plist)
	  || CONSP (XCDR (XCDR (val))))
	print_check_string_result |= PRINT_STRING_NON_CHARSET_FOUND;
    }
  if (NILP (Vprint_charset_text_property)
      || ! (print_check_string_result & PRINT_STRING_UNSAFE_CHARSET_FOUND))
    {
      int i, c;
      ptrdiff_t charpos = interval->position;
      ptrdiff_t bytepos = string_char_to_byte (string, charpos);
      Lisp_Object charset;

      charset = XCAR (XCDR (val));
      for (i = 0; i < LENGTH (interval); i++)
	{
	  FETCH_STRING_CHAR_ADVANCE (c, string, charpos, bytepos);
	  if (! ASCII_CHAR_P (c)
	      && ! EQ (CHARSET_NAME (CHAR_CHARSET (c)), charset))
	    {
	      print_check_string_result |= PRINT_STRING_UNSAFE_CHARSET_FOUND;
	      break;
	    }
	}
    }
}

/* The value is (charset . nil).  */
static Lisp_Object print_prune_charset_plist;

static Lisp_Object
print_prune_string_charset (Lisp_Object string)
{
  print_check_string_result = 0;
  traverse_intervals (string_intervals (string), 0,
		      print_check_string_charset_prop, string);
  if (! (print_check_string_result & PRINT_STRING_UNSAFE_CHARSET_FOUND))
    {
      string = Fcopy_sequence (string);
      if (print_check_string_result & PRINT_STRING_NON_CHARSET_FOUND)
	{
	  if (NILP (print_prune_charset_plist))
	    print_prune_charset_plist = list1 (Qcharset);
	  Fremove_text_properties (make_number (0),
				   make_number (SCHARS (string)),
				   print_prune_charset_plist, string);
	}
      else
	Fset_text_properties (make_number (0), make_number (SCHARS (string)),
			      Qnil, string);
    }
  return string;
}

static bool
print_vectorlike (Lisp_Object obj, Lisp_Object printcharfun, bool escapeflag,
		  char *buf)
{
  switch (PSEUDOVECTOR_TYPE (XVECTOR (obj)))
    {
    case PVEC_PROCESS:
      if (escapeflag)
	{
	  print_c_string ("#<process ", printcharfun);
	  print_string (XPROCESS (obj)->name, printcharfun);
	  printchar ('>', printcharfun);
	}
      else
	print_string (XPROCESS (obj)->name, printcharfun);
      break;

    case PVEC_BOOL_VECTOR:
      {
	EMACS_INT size = bool_vector_size (obj);
	ptrdiff_t size_in_chars = bool_vector_bytes (size);
	ptrdiff_t real_size_in_chars = size_in_chars;

	int len = sprintf (buf, "#&%"pI"d\"", size);
	strout (buf, len, len, printcharfun);

	/* Don't print more characters than the specified maximum.
	   Negative values of print-length are invalid.  Treat them
	   like a print-length of nil.  */
	if (NATNUMP (Vprint_length)
	    && XFASTINT (Vprint_length) < size_in_chars)
	  size_in_chars = XFASTINT (Vprint_length);

	for (ptrdiff_t i = 0; i < size_in_chars; i++)
	  {
	    maybe_quit ();
	    unsigned char c = bool_vector_uchar_data (obj)[i];
	    if (c == '\n' && print_escape_newlines)
	      print_c_string ("\\n", printcharfun);
	    else if (c == '\f' && print_escape_newlines)
	      print_c_string ("\\f", printcharfun);
	    else if (c > '\177')
	      {
		/* Use octal escapes to avoid encoding issues.  */
		int len = sprintf (buf, "\\%o", c);
		strout (buf, len, len, printcharfun);
	      }
	    else
	      {
		if (c == '\"' || c == '\\')
		  printchar ('\\', printcharfun);
		printchar (c, printcharfun);
	      }
	  }

	if (size_in_chars < real_size_in_chars)
	  print_c_string (" ...", printcharfun);
	printchar ('\"', printcharfun);
      }
      break;

    case PVEC_SUBR:
      print_c_string ("#<subr ", printcharfun);
      print_c_string (XSUBR (obj)->symbol_name, printcharfun);
      printchar ('>', printcharfun);
      break;

    case PVEC_XWIDGET: case PVEC_XWIDGET_VIEW:
      print_c_string ("#<xwidget ", printcharfun);
      printchar ('>', printcharfun);
      break;

    case PVEC_WINDOW:
      {
	int len = sprintf (buf, "#<window %"pI"d",
			   XWINDOW (obj)->sequence_number);
	strout (buf, len, len, printcharfun);
	if (BUFFERP (XWINDOW (obj)->contents))
	  {
	    print_c_string (" on ", printcharfun);
	    print_string (BVAR (XBUFFER (XWINDOW (obj)->contents), name),
			  printcharfun);
	  }
	printchar ('>', printcharfun);
      }
      break;

    case PVEC_TERMINAL:
      {
	struct terminal *t = XTERMINAL (obj);
	int len = sprintf (buf, "#<terminal %d", t->id);
	strout (buf, len, len, printcharfun);
	if (t->name)
	  {
	    print_c_string (" on ", printcharfun);
	    print_c_string (t->name, printcharfun);
	  }
	printchar ('>', printcharfun);
      }
      break;

    case PVEC_HASH_TABLE:
      {
	struct Lisp_Hash_Table *h = XHASH_TABLE (obj);
	/* Implement a readable output, e.g.:
	  #s(hash-table size 2 test equal data (k1 v1 k2 v2)) */
	/* Always print the size.  */
	int len = sprintf (buf, "#s(hash-table size %"pD"d", ASIZE (h->next));
	strout (buf, len, len, printcharfun);

	if (!NILP (h->test.name))
	  {
	    print_c_string (" test ", printcharfun);
	    print_object (h->test.name, printcharfun, escapeflag);
	  }

	if (!NILP (h->weak))
	  {
	    print_c_string (" weakness ", printcharfun);
	    print_object (h->weak, printcharfun, escapeflag);
	  }

	print_c_string (" rehash-size ", printcharfun);
	print_object (Fhash_table_rehash_size (obj),
		      printcharfun, escapeflag);

	print_c_string (" rehash-threshold ", printcharfun);
	print_object (Fhash_table_rehash_threshold (obj),
		      printcharfun, escapeflag);

	if (h->pure)
	  {
	    print_c_string (" purecopy ", printcharfun);
	    print_object (h->pure ? Qt : Qnil, printcharfun, escapeflag);
	  }

	print_c_string (" data ", printcharfun);

	/* Print the data here as a plist. */
	ptrdiff_t real_size = HASH_TABLE_SIZE (h);
	ptrdiff_t size = real_size;

	/* Don't print more elements than the specified maximum.  */
	if (NATNUMP (Vprint_length) && XFASTINT (Vprint_length) < size)
	  size = XFASTINT (Vprint_length);

	printchar ('(', printcharfun);
	for (ptrdiff_t i = 0; i < size; i++)
	  if (!NILP (HASH_HASH (h, i)))
	    {
	      if (i) printchar (' ', printcharfun);
	      print_object (HASH_KEY (h, i), printcharfun, escapeflag);
	      printchar (' ', printcharfun);
	      print_object (HASH_VALUE (h, i), printcharfun, escapeflag);
	    }

	if (size < real_size)
	  print_c_string (" ...", printcharfun);

	print_c_string ("))", printcharfun);
      }
      break;

    case PVEC_BUFFER:
      if (!BUFFER_LIVE_P (XBUFFER (obj)))
	print_c_string ("#<killed buffer>", printcharfun);
      else if (escapeflag)
	{
	  print_c_string ("#<buffer ", printcharfun);
	  print_string (BVAR (XBUFFER (obj), name), printcharfun);
	  printchar ('>', printcharfun);
	}
      else
	print_string (BVAR (XBUFFER (obj), name), printcharfun);
      break;

    case PVEC_WINDOW_CONFIGURATION:
      print_c_string ("#<window-configuration>", printcharfun);
      break;

    case PVEC_FRAME:
      {
	void *ptr = XFRAME (obj);
	Lisp_Object frame_name = XFRAME (obj)->name;

	print_c_string ((FRAME_LIVE_P (XFRAME (obj))
			 ? "#<frame "
			 : "#<dead frame "),
			printcharfun);
	if (!STRINGP (frame_name))
	  {
	    /* A frame could be too young and have no name yet;
	       don't crash.  */
	    if (SYMBOLP (frame_name))
	      frame_name = Fsymbol_name (frame_name);
	    else	/* can't happen: name should be either nil or string */
	      frame_name = build_string ("*INVALID*FRAME*NAME*");
	  }
	print_string (frame_name, printcharfun);
	int len = sprintf (buf, " %p>", ptr);
	strout (buf, len, len, printcharfun);
      }
      break;

    case PVEC_FONT:
      {
	if (! FONT_OBJECT_P (obj))
	  {
	    if (FONT_SPEC_P (obj))
	      print_c_string ("#<font-spec", printcharfun);
	    else
	      print_c_string ("#<font-entity", printcharfun);
	    for (int i = 0; i < FONT_SPEC_MAX; i++)
	      {
		printchar (' ', printcharfun);
		if (i < FONT_WEIGHT_INDEX || i > FONT_WIDTH_INDEX)
		  print_object (AREF (obj, i), printcharfun, escapeflag);
		else
		  print_object (font_style_symbolic (obj, i, 0),
				printcharfun, escapeflag);
	      }
	  }
	else
	  {
	    print_c_string ("#<font-object ", printcharfun);
	    print_object (AREF (obj, FONT_NAME_INDEX), printcharfun,
			  escapeflag);
	  }
	printchar ('>', printcharfun);
      }
      break;

    case PVEC_THREAD:
      print_c_string ("#<thread ", printcharfun);
      if (STRINGP (XTHREAD (obj)->name))
	print_string (XTHREAD (obj)->name, printcharfun);
      else
	{
	  int len = sprintf (buf, "%p", XTHREAD (obj));
	  strout (buf, len, len, printcharfun);
	}
      printchar ('>', printcharfun);
      break;

    case PVEC_MUTEX:
      print_c_string ("#<mutex ", printcharfun);
      if (STRINGP (XMUTEX (obj)->name))
	print_string (XMUTEX (obj)->name, printcharfun);
      else
	{
	  int len = sprintf (buf, "%p", XMUTEX (obj));
	  strout (buf, len, len, printcharfun);
	}
      printchar ('>', printcharfun);
      break;

    case PVEC_CONDVAR:
      print_c_string ("#<condvar ", printcharfun);
      if (STRINGP (XCONDVAR (obj)->name))
	print_string (XCONDVAR (obj)->name, printcharfun);
      else
	{
	  int len = sprintf (buf, "%p", XCONDVAR (obj));
	  strout (buf, len, len, printcharfun);
	}
      printchar ('>', printcharfun);
      break;

    case PVEC_RECORD:
      {
	ptrdiff_t size = PVSIZE (obj);

	/* Don't print more elements than the specified maximum.  */
	ptrdiff_t n
	  = (NATNUMP (Vprint_length) && XFASTINT (Vprint_length) < size
	     ? XFASTINT (Vprint_length) : size);

	print_c_string ("#s(", printcharfun);
	for (ptrdiff_t i = 0; i < n; i ++)
	  {
	    if (i) printchar (' ', printcharfun);
	    print_object (AREF (obj, i), printcharfun, escapeflag);
	  }
	if (n < size)
	  print_c_string (" ...", printcharfun);
	printchar (')', printcharfun);
      }
      break;

    case PVEC_SUB_CHAR_TABLE:
    case PVEC_COMPILED:
    case PVEC_CHAR_TABLE:
    case PVEC_NORMAL_VECTOR:
      {
	ptrdiff_t size = ASIZE (obj);
	if (COMPILEDP (obj))
	  {
	    printchar ('#', printcharfun);
	    size &= PSEUDOVECTOR_SIZE_MASK;
	  }
	if (CHAR_TABLE_P (obj) || SUB_CHAR_TABLE_P (obj))
	  {
	    /* Print a char-table as if it were a vector,
	       lumping the parent and default slots in with the
	       character slots.  But add #^ as a prefix.  */

	    /* Make each lowest sub_char_table start a new line.
	       Otherwise we'll make a line extremely long, which
	       results in slow redisplay.  */
	    if (SUB_CHAR_TABLE_P (obj)
		&& XSUB_CHAR_TABLE (obj)->depth == 3)
	      printchar ('\n', printcharfun);
	    print_c_string ("#^", printcharfun);
	    if (SUB_CHAR_TABLE_P (obj))
	      printchar ('^', printcharfun);
	    size &= PSEUDOVECTOR_SIZE_MASK;
	  }
	if (size & PSEUDOVECTOR_FLAG)
	  return false;

	printchar ('[', printcharfun);

	int idx = SUB_CHAR_TABLE_P (obj) ? SUB_CHAR_TABLE_OFFSET : 0;
	Lisp_Object tem;
	ptrdiff_t real_size = size;

	/* For a sub char-table, print heading non-Lisp data first.  */
	if (SUB_CHAR_TABLE_P (obj))
	  {
	    int i = sprintf (buf, "%d %d", XSUB_CHAR_TABLE (obj)->depth,
			     XSUB_CHAR_TABLE (obj)->min_char);
	    strout (buf, i, i, printcharfun);
	  }

	/* Don't print more elements than the specified maximum.  */
	if (NATNUMP (Vprint_length)
	    && XFASTINT (Vprint_length) < size)
	  size = XFASTINT (Vprint_length);

	for (int i = idx; i < size; i++)
	  {
	    if (i) printchar (' ', printcharfun);
	    tem = AREF (obj, i);
	    print_object (tem, printcharfun, escapeflag);
	  }
	if (size < real_size)
	  print_c_string (" ...", printcharfun);
	printchar (']', printcharfun);
      }
      break;

#ifdef HAVE_MODULES
    case PVEC_MODULE_FUNCTION:
      {
	print_c_string ("#<module function ", printcharfun);
	void *ptr = XMODULE_FUNCTION (obj)->subr;
	const char *file = NULL;
	const char *symbol = NULL;
	dynlib_addr (ptr, &file, &symbol);

	if (symbol == NULL)
	  {
	    print_c_string ("at ", printcharfun);
	    enum { pointer_bufsize = sizeof ptr * 16 / CHAR_BIT + 2 + 1 };
	    char buffer[pointer_bufsize];
	    int needed = snprintf (buffer, sizeof buffer, "%p", ptr);
	    const char p0x[] = "0x";
	    eassert (needed <= sizeof buffer);
	    /* ANSI C doesn't guarantee that %p produces a string that
	       begins with a "0x".  */
	    if (c_strncasecmp (buffer, p0x, sizeof (p0x) - 1) != 0)
	      print_c_string (p0x, printcharfun);
	    print_c_string (buffer, printcharfun);
	  }
	else
	  print_c_string (symbol, printcharfun);

	if (file != NULL)
	  {
	    print_c_string (" from ", printcharfun);
	    print_c_string (file, printcharfun);
	  }

	printchar ('>', printcharfun);
      }
      break;
#endif

    default:
      emacs_abort ();
    }

  return true;
}

static void
print_object (Lisp_Object obj, Lisp_Object printcharfun, bool escapeflag)
{
  char buf[max (sizeof "from..to..in " + 2 * INT_STRLEN_BOUND (EMACS_INT),
		max (sizeof " . #" + INT_STRLEN_BOUND (printmax_t),
		     40))];
  current_thread->stack_top = buf;
  maybe_quit ();

  /* Detect circularities and truncate them.  */
  if (NILP (Vprint_circle))
    {
      /* Simple but incomplete way.  */
      int i;

      /* See similar code in print_preprocess.  */
      if (print_depth >= PRINT_CIRCLE)
	error ("Apparently circular structure being printed");

      for (i = 0; i < print_depth; i++)
	if (EQ (obj, being_printed[i]))
	  {
	    int len = sprintf (buf, "#%d", i);
	    strout (buf, len, len, printcharfun);
	    return;
	  }
      being_printed[print_depth] = obj;
    }
  else if (PRINT_CIRCLE_CANDIDATE_P (obj))
    {
      /* With the print-circle feature.  */
      Lisp_Object num = Fgethash (obj, Vprint_number_table, Qnil);
      if (INTEGERP (num))
	{
	  EMACS_INT n = XINT (num);
	  if (n < 0)
	    { /* Add a prefix #n= if OBJ has not yet been printed;
		 that is, its status field is nil.  */
	      int len = sprintf (buf, "#%"pI"d=", -n);
	      strout (buf, len, len, printcharfun);
	      /* OBJ is going to be printed.  Remember that fact.  */
	      Fputhash (obj, make_number (- n), Vprint_number_table);
	    }
	  else
	    {
	      /* Just print #n# if OBJ has already been printed.  */
	      int len = sprintf (buf, "#%"pI"d#", n);
	      strout (buf, len, len, printcharfun);
	      return;
	    }
	}
    }

  print_depth++;

  switch (XTYPE (obj))
    {
    case_Lisp_Int:
      {
	int len = sprintf (buf, "%"pI"d", XINT (obj));
	strout (buf, len, len, printcharfun);
      }
      break;

    case Lisp_Float:
      {
	char pigbuf[FLOAT_TO_STRING_BUFSIZE];
	int len = float_to_string (pigbuf, XFLOAT_DATA (obj));
	strout (pigbuf, len, len, printcharfun);
      }
      break;

    case Lisp_String:
      if (!escapeflag)
	print_string (obj, printcharfun);
      else
	{
	  ptrdiff_t i, i_byte;
	  ptrdiff_t size_byte;
	  /* True means we must ensure that the next character we output
	     cannot be taken as part of a hex character escape.  */
	  bool need_nonhex = false;
	  bool multibyte = STRING_MULTIBYTE (obj);

	  if (! EQ (Vprint_charset_text_property, Qt))
	    obj = print_prune_string_charset (obj);

	  if (string_intervals (obj))
	    print_c_string ("#(", printcharfun);

	  printchar ('\"', printcharfun);
	  size_byte = SBYTES (obj);

	  for (i = 0, i_byte = 0; i_byte < size_byte;)
	    {
	      /* Here, we must convert each multi-byte form to the
		 corresponding character code before handing it to printchar.  */
	      int c;

	      FETCH_STRING_CHAR_ADVANCE (c, obj, i, i_byte);

	      maybe_quit ();

	      if (multibyte
		  ? (CHAR_BYTE8_P (c) && (c = CHAR_TO_BYTE8 (c), true))
		  : (SINGLE_BYTE_CHAR_P (c) && ! ASCII_CHAR_P (c)
		     && print_escape_nonascii))
		{
		  /* When printing a raw 8-bit byte in a multibyte buffer, or
		     (when requested) a non-ASCII character in a unibyte buffer,
		     print single-byte non-ASCII string chars
		     using octal escapes.  */
		  char outbuf[5];
		  int len = sprintf (outbuf, "\\%03o", c + 0u);
		  strout (outbuf, len, len, printcharfun);
		  need_nonhex = false;
		}
	      else if (multibyte
		       && ! ASCII_CHAR_P (c) && print_escape_multibyte)
		{
		  /* When requested, print multibyte chars using hex escapes.  */
		  char outbuf[sizeof "\\x" + INT_STRLEN_BOUND (c)];
		  int len = sprintf (outbuf, "\\x%04x", c + 0u);
		  strout (outbuf, len, len, printcharfun);
		  need_nonhex = true;
		}
	      else
		{
                  bool still_need_nonhex = false;
		  /* If we just had a hex escape, and this character
		     could be taken as part of it,
		     output `\ ' to prevent that.  */
                  if (c_isxdigit (c))
                    {
                      if (need_nonhex)
                        print_c_string ("\\ ", printcharfun);
                      printchar (c, printcharfun);
                    }
                  else if (c == '\n' && print_escape_newlines
                           ? (c = 'n', true)
                           : c == '\f' && print_escape_newlines
                           ? (c = 'f', true)
                           : c == '\0' && print_escape_control_characters
                           ? (c = '0', still_need_nonhex = true)
                           : c == '\"' || c == '\\')
                    {
                      printchar ('\\', printcharfun);
                      printchar (c, printcharfun);
                    }
                  else if (print_escape_control_characters && c_iscntrl (c))
                    {
                      char outbuf[1 + 3 + 1];
                      int len = sprintf (outbuf, "\\%03o", c + 0u);
                      strout (outbuf, len, len, printcharfun);
                    }
                  else
                    printchar (c, printcharfun);
		  need_nonhex = still_need_nonhex;
		}
	    }
	  printchar ('\"', printcharfun);

	  if (string_intervals (obj))
	    {
	      traverse_intervals (string_intervals (obj),
				  0, print_interval, printcharfun);
	      printchar (')', printcharfun);
	    }
	}
      break;

    case Lisp_Symbol:
      {
	bool confusing;
	unsigned char *p = SDATA (SYMBOL_NAME (obj));
	unsigned char *end = p + SBYTES (SYMBOL_NAME (obj));
	int c;
	ptrdiff_t i, i_byte;
	ptrdiff_t size_byte;
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
				|| *p == 'e' || *p == 'E'))
	      p++;
	    confusing = (end == p);
	  }
	else
	  confusing = 0;

	size_byte = SBYTES (name);

	if (! NILP (Vprint_gensym)
            && !SYMBOL_INTERNED_IN_INITIAL_OBARRAY_P (obj))
	  print_c_string ("#:", printcharfun);
	else if (size_byte == 0)
	  {
	    print_c_string ("##", printcharfun);
	    break;
	  }

	for (i = 0, i_byte = 0; i_byte < size_byte;)
	  {
	    /* Here, we must convert each multi-byte form to the
	       corresponding character code before handing it to PRINTCHAR.  */
	    FETCH_STRING_CHAR_ADVANCE (c, name, i, i_byte);
	    maybe_quit ();

	    if (escapeflag)
	      {
		if (c == '\"' || c == '\\' || c == '\''
		    || c == ';' || c == '#' || c == '(' || c == ')'
		    || c == ',' || c == '.' || c == '`'
		    || c == '[' || c == ']' || c == '?' || c <= 040
                    || confusing
		    || (i == 1 && confusable_symbol_character_p (c)))
		  {
		    printchar ('\\', printcharfun);
		    confusing = false;
		  }
	      }
	    printchar (c, printcharfun);
	  }
      }
      break;

    case Lisp_Cons:
      /* If deeper than spec'd depth, print placeholder.  */
      if (INTEGERP (Vprint_level)
	  && print_depth > XINT (Vprint_level))
	print_c_string ("...", printcharfun);
      else if (print_quoted && CONSP (XCDR (obj)) && NILP (XCDR (XCDR (obj)))
	       && EQ (XCAR (obj), Qquote))
	{
	  printchar ('\'', printcharfun);
	  print_object (XCAR (XCDR (obj)), printcharfun, escapeflag);
	}
      else if (print_quoted && CONSP (XCDR (obj)) && NILP (XCDR (XCDR (obj)))
	       && EQ (XCAR (obj), Qfunction))
	{
	  print_c_string ("#'", printcharfun);
	  print_object (XCAR (XCDR (obj)), printcharfun, escapeflag);
	}
      else if (print_quoted && CONSP (XCDR (obj)) && NILP (XCDR (XCDR (obj)))
	       && EQ (XCAR (obj), Qbackquote))
	{
	  printchar ('`', printcharfun);
	  new_backquote_output++;
	  print_object (XCAR (XCDR (obj)), printcharfun, escapeflag);
	  new_backquote_output--;
	}
      else if (print_quoted && CONSP (XCDR (obj)) && NILP (XCDR (XCDR (obj)))
	       && new_backquote_output
	       && (EQ (XCAR (obj), Qcomma)
		   || EQ (XCAR (obj), Qcomma_at)
		   || EQ (XCAR (obj), Qcomma_dot)))
	{
	  print_object (XCAR (obj), printcharfun, false);
	  new_backquote_output--;
	  print_object (XCAR (XCDR (obj)), printcharfun, escapeflag);
	  new_backquote_output++;
	}
      else
	{
	  printchar ('(', printcharfun);

	  Lisp_Object halftail = obj;

	  /* Negative values of print-length are invalid in CL.
	     Treat them like nil, as CMUCL does.  */
	  printmax_t print_length = (NATNUMP (Vprint_length)
				     ? XFASTINT (Vprint_length)
				     : TYPE_MAXIMUM (printmax_t));

	  printmax_t i = 0;
	  while (CONSP (obj))
	    {
	      /* Detect circular list.  */
	      if (NILP (Vprint_circle))
		{
		  /* Simple but incomplete way.  */
		  if (i != 0 && EQ (obj, halftail))
		    {
		      int len = sprintf (buf, " . #%"pMd, i / 2);
		      strout (buf, len, len, printcharfun);
		      goto end_of_list;
		    }
		}
	      else
		{
		  /* With the print-circle feature.  */
		  if (i != 0)
		    {
		      Lisp_Object num = Fgethash (obj, Vprint_number_table, Qnil);
		      if (INTEGERP (num))
			{
			  print_c_string (" . ", printcharfun);
			  print_object (obj, printcharfun, escapeflag);
			  goto end_of_list;
			}
		    }
		}

	      if (i)
		printchar (' ', printcharfun);

	      if (print_length <= i)
		{
		  print_c_string ("...", printcharfun);
		  goto end_of_list;
		}

	      i++;
	      print_object (XCAR (obj), printcharfun, escapeflag);

	      obj = XCDR (obj);
	      if (!(i & 1))
		halftail = XCDR (halftail);
	  }

	  /* OBJ non-nil here means it's the end of a dotted list.  */
	  if (!NILP (obj))
	    {
	      print_c_string (" . ", printcharfun);
	      print_object (obj, printcharfun, escapeflag);
	    }

	end_of_list:
	  printchar (')', printcharfun);
	}
      break;

    case Lisp_Vectorlike:
      if (! print_vectorlike (obj, printcharfun, escapeflag, buf))
	goto badtype;
      break;

    case Lisp_Misc:
      switch (XMISCTYPE (obj))
	{
	case Lisp_Misc_Marker:
	  print_c_string ("#<marker ", printcharfun);
	  /* Do you think this is necessary?  */
	  if (XMARKER (obj)->insertion_type != 0)
	    print_c_string ("(moves after insertion) ", printcharfun);
	  if (! XMARKER (obj)->buffer)
	    print_c_string ("in no buffer", printcharfun);
	  else
	    {
	      int len = sprintf (buf, "at %"pD"d in ", marker_position (obj));
	      strout (buf, len, len, printcharfun);
	      print_string (BVAR (XMARKER (obj)->buffer, name), printcharfun);
	    }
	  printchar ('>', printcharfun);
	  break;

	case Lisp_Misc_Overlay:
	  print_c_string ("#<overlay ", printcharfun);
	  if (! XMARKER (OVERLAY_START (obj))->buffer)
	    print_c_string ("in no buffer", printcharfun);
	  else
	    {
	      int len = sprintf (buf, "from %"pD"d to %"pD"d in ",
				 marker_position (OVERLAY_START (obj)),
				 marker_position (OVERLAY_END   (obj)));
	      strout (buf, len, len, printcharfun);
	      print_string (BVAR (XMARKER (OVERLAY_START (obj))->buffer, name),
			    printcharfun);
	    }
	  printchar ('>', printcharfun);
          break;

#ifdef HAVE_MODULES
	case Lisp_Misc_User_Ptr:
	  {
	    print_c_string ("#<user-ptr ", printcharfun);
	    int i = sprintf (buf, "ptr=%p finalizer=%p",
			     XUSER_PTR (obj)->p,
			     XUSER_PTR (obj)->finalizer);
	    strout (buf, i, i, printcharfun);
	    printchar ('>', printcharfun);
	    break;
	  }
#endif

        case Lisp_Misc_Finalizer:
          print_c_string ("#<finalizer", printcharfun);
          if (NILP (XFINALIZER (obj)->function))
            print_c_string (" used", printcharfun);
	  printchar ('>', printcharfun);
          break;

	  /* Remaining cases shouldn't happen in normal usage, but let's
	     print them anyway for the benefit of the debugger.  */

	case Lisp_Misc_Free:
	  print_c_string ("#<misc free cell>", printcharfun);
	  break;

	case Lisp_Misc_Save_Value:
	  {
	    int i;
	    struct Lisp_Save_Value *v = XSAVE_VALUE (obj);

	    print_c_string ("#<save-value ", printcharfun);

	    if (v->save_type == SAVE_TYPE_MEMORY)
	      {
		ptrdiff_t amount = v->data[1].integer;

		/* valid_lisp_object_p is reliable, so try to print up
		   to 8 saved objects.  This code is rarely used, so
		   it's OK that valid_lisp_object_p is slow.  */

		int limit = min (amount, 8);
		Lisp_Object *area = v->data[0].pointer;

		i = sprintf (buf, "with %"pD"d objects", amount);
		strout (buf, i, i, printcharfun);

		for (i = 0; i < limit; i++)
		  {
		    Lisp_Object maybe = area[i];
		    int valid = valid_lisp_object_p (maybe);

		    printchar (' ', printcharfun);
		    if (0 < valid)
		      print_object (maybe, printcharfun, escapeflag);
		    else
		      print_c_string (valid < 0 ? "<some>" : "<invalid>",
				      printcharfun);
		  }
		if (i == limit && i < amount)
		  print_c_string (" ...", printcharfun);
	      }
	    else
	      {
		/* Print each slot according to its type.  */
		int index;
		for (index = 0; index < SAVE_VALUE_SLOTS; index++)
		  {
		    if (index)
		      printchar (' ', printcharfun);

		    switch (save_type (v, index))
		      {
		      case SAVE_UNUSED:
			i = sprintf (buf, "<unused>");
			break;

		      case SAVE_POINTER:
			i = sprintf (buf, "<pointer %p>",
				     v->data[index].pointer);
			break;

		      case SAVE_FUNCPOINTER:
			i = sprintf (buf, "<funcpointer %p>",
				     ((void *) (intptr_t)
				      v->data[index].funcpointer));
			break;

		      case SAVE_INTEGER:
			i = sprintf (buf, "<integer %"pD"d>",
				     v->data[index].integer);
			break;

		      case SAVE_OBJECT:
			print_object (v->data[index].object, printcharfun,
				      escapeflag);
			continue;

		      default:
			emacs_abort ();
		      }

		    strout (buf, i, i, printcharfun);
		  }
	      }
	    printchar ('>', printcharfun);
	  }
	  break;

	default:
	  goto badtype;
	}
      break;

    default:
    badtype:
      {
	int len;
	/* We're in trouble if this happens!
	   Probably should just emacs_abort ().  */
	print_c_string ("#<EMACS BUG: INVALID DATATYPE ", printcharfun);
	if (MISCP (obj))
	  len = sprintf (buf, "(MISC 0x%04x)", (unsigned) XMISCTYPE (obj));
	else if (VECTORLIKEP (obj))
	  len = sprintf (buf, "(PVEC 0x%08zx)", (size_t) ASIZE (obj));
	else
	  len = sprintf (buf, "(0x%02x)", (unsigned) XTYPE (obj));
	strout (buf, len, len, printcharfun);
	print_c_string ((" Save your buffers immediately"
			 " and please report this bug>"),
			printcharfun);
      }
    }

  print_depth--;
}


/* Print a description of INTERVAL using PRINTCHARFUN.
   This is part of printing a string that has text properties.  */

static void
print_interval (INTERVAL interval, Lisp_Object printcharfun)
{
  if (NILP (interval->plist))
    return;
  printchar (' ', printcharfun);
  print_object (make_number (interval->position), printcharfun, 1);
  printchar (' ', printcharfun);
  print_object (make_number (interval->position + LENGTH (interval)),
		printcharfun, 1);
  printchar (' ', printcharfun);
  print_object (interval->plist, printcharfun, 1);
}

/* Initialize debug_print stuff early to have it working from the very
   beginning.  */

void
init_print_once (void)
{
  /* The subroutine object for external-debugging-output is kept here
     for the convenience of the debugger.  */
  DEFSYM (Qexternal_debugging_output, "external-debugging-output");

  defsubr (&Sexternal_debugging_output);
}

void
syms_of_print (void)
{
  DEFSYM (Qtemp_buffer_setup_hook, "temp-buffer-setup-hook");

  DEFVAR_LISP ("standard-output", Vstandard_output,
	       doc: /* Output stream `print' uses by default for outputting a character.
This may be any function of one argument.
It may also be a buffer (output is inserted before point)
or a marker (output is inserted and the marker is advanced)
or the symbol t (output appears in the echo area).  */);
  Vstandard_output = Qt;
  DEFSYM (Qstandard_output, "standard-output");

  DEFVAR_LISP ("float-output-format", Vfloat_output_format,
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

  DEFVAR_LISP ("print-length", Vprint_length,
	       doc: /* Maximum length of list to print before abbreviating.
A value of nil means no limit.  See also `eval-expression-print-length'.  */);
  Vprint_length = Qnil;

  DEFVAR_LISP ("print-level", Vprint_level,
	       doc: /* Maximum depth of list nesting to print before abbreviating.
A value of nil means no limit.  See also `eval-expression-print-level'.  */);
  Vprint_level = Qnil;

  DEFVAR_BOOL ("print-escape-newlines", print_escape_newlines,
	       doc: /* Non-nil means print newlines in strings as `\\n'.
Also print formfeeds as `\\f'.  */);
  print_escape_newlines = 0;

  DEFVAR_BOOL ("print-escape-control-characters", print_escape_control_characters,
	       doc: /* Non-nil means print control characters in strings as `\\OOO'.
\(OOO is the octal representation of the character code.)*/);
  print_escape_control_characters = 0;

  DEFVAR_BOOL ("print-escape-nonascii", print_escape_nonascii,
	       doc: /* Non-nil means print unibyte non-ASCII chars in strings as \\OOO.
\(OOO is the octal representation of the character code.)
Only single-byte characters are affected, and only in `prin1'.
When the output goes in a multibyte buffer, this feature is
enabled regardless of the value of the variable.  */);
  print_escape_nonascii = 0;

  DEFVAR_BOOL ("print-escape-multibyte", print_escape_multibyte,
	       doc: /* Non-nil means print multibyte characters in strings as \\xXXXX.
\(XXXX is the hex representation of the character code.)
This affects only `prin1'.  */);
  print_escape_multibyte = 0;

  DEFVAR_BOOL ("print-quoted", print_quoted,
	       doc: /* Non-nil means print quoted forms with reader syntax.
I.e., (quote foo) prints as \\='foo, (function foo) as #\\='foo.  */);
  print_quoted = true;

  DEFVAR_LISP ("print-gensym", Vprint_gensym,
	       doc: /* Non-nil means print uninterned symbols so they will read as uninterned.
I.e., the value of (make-symbol \"foobar\") prints as #:foobar.
When the uninterned symbol appears multiple times within the printed
expression, and `print-circle' is non-nil, in addition use the #N#
and #N= constructs as needed, so that multiple references to the same
symbol are shared once again when the text is read back.  */);
  Vprint_gensym = Qnil;

  DEFVAR_LISP ("print-circle", Vprint_circle,
	       doc: /* Non-nil means print recursive structures using #N= and #N# syntax.
If nil, printing proceeds recursively and may lead to
`max-lisp-eval-depth' being exceeded or an error may occur:
\"Apparently circular structure being printed.\"  Also see
`print-length' and `print-level'.
If non-nil, shared substructures anywhere in the structure are printed
with `#N=' before the first occurrence (in the order of the print
representation) and `#N#' in place of each subsequent occurrence,
where N is a positive decimal integer.  */);
  Vprint_circle = Qnil;

  DEFVAR_LISP ("print-continuous-numbering", Vprint_continuous_numbering,
	       doc: /* Non-nil means number continuously across print calls.
This affects the numbers printed for #N= labels and #M# references.
See also `print-circle', `print-gensym', and `print-number-table'.
This variable should not be set with `setq'; bind it with a `let' instead.  */);
  Vprint_continuous_numbering = Qnil;

  DEFVAR_LISP ("print-number-table", Vprint_number_table,
	       doc: /* A vector used internally to produce `#N=' labels and `#N#' references.
The Lisp printer uses this vector to detect Lisp objects referenced more
than once.

When you bind `print-continuous-numbering' to t, you should probably
also bind `print-number-table' to nil.  This ensures that the value of
`print-number-table' can be garbage-collected once the printing is
done.  If all elements of `print-number-table' are nil, it means that
the printing done so far has not found any shared structure or objects
that need to be recorded in the table.  */);
  Vprint_number_table = Qnil;

  DEFVAR_LISP ("print-charset-text-property", Vprint_charset_text_property,
	       doc: /* A flag to control printing of `charset' text property on printing a string.
The value must be nil, t, or `default'.

If the value is nil, don't print the text property `charset'.

If the value is t, always print the text property `charset'.

If the value is `default', print the text property `charset' only when
the value is different from what is guessed in the current charset
priorities.  */);
  Vprint_charset_text_property = Qdefault;

  /* prin1_to_string_buffer initialized in init_buffer_once in buffer.c */
  staticpro (&Vprin1_to_string_buffer);

  defsubr (&Sprin1);
  defsubr (&Sprin1_to_string);
  defsubr (&Serror_message_string);
  defsubr (&Sprinc);
  defsubr (&Sprint);
  defsubr (&Sterpri);
  defsubr (&Swrite_char);
  defsubr (&Sredirect_debugging_output);
  defsubr (&Sprint_preprocess);

  DEFSYM (Qprint_escape_newlines, "print-escape-newlines");
  DEFSYM (Qprint_escape_multibyte, "print-escape-multibyte");
  DEFSYM (Qprint_escape_nonascii, "print-escape-nonascii");
  DEFSYM (Qprint_escape_control_characters, "print-escape-control-characters");

  print_prune_charset_plist = Qnil;
  staticpro (&print_prune_charset_plist);
}
