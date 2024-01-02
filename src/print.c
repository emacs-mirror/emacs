/* Lisp object printing and output streams.

Copyright (C) 1985-2024 Free Software Foundation, Inc.

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
#include <math.h>

#if IEEE_FLOATING_POINT
# include <ieee754.h>
#endif

#ifdef WINDOWSNT
# include <sys/socket.h> /* for F_DUPFD_CLOEXEC */
#endif

#ifdef HAVE_TREE_SITTER
#include "treesit.h"
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

struct print_buffer
{
  char *buffer;			/* Allocated buffer.  */
  ptrdiff_t size;		/* Size of allocated buffer.  */
  ptrdiff_t pos;		/* Chars stored in buffer.  */
  ptrdiff_t pos_byte;		/* Bytes stored in buffer.  */
};

/* When printing into a buffer, first we put the text in this
   block, then insert it all at once.  */
static struct print_buffer print_buffer;

/* Vprint_number_table is a table, that keeps objects that are going to
   be printed, to allow use of #n= and #n# to express sharing.
   For any given object, the table can give the following values:
     t    the object will be printed only once.
     -N   the object will be printed several times and will take number N.
     N    the object has been printed so we can refer to it as #N#.
   print_number_index holds the largest N already used.
   N has to be strictly larger than 0 since we need to distinguish -N.  */
static ptrdiff_t print_number_index;
static void print_interval (INTERVAL interval, Lisp_Object printcharfun);

/* GDB resets this to zero on W32 to disable OutputDebugString calls.  */
bool print_output_debug_flag EXTERNALLY_VISIBLE = 1;


/* Low level output routines for characters and strings.  */

/* This is used to free the print buffer; we don't simply record xfree
   since print_buffer can be reallocated during the printing.  */
static void
print_free_buffer (void)
{
  xfree (print_buffer.buffer);
  print_buffer.buffer = NULL;
}

/* This is used to restore the saved contents of print_buffer
   when there is a recursive call to print.  */
static void
print_unwind (Lisp_Object saved_text)
{
  memcpy (print_buffer.buffer, SDATA (saved_text), SCHARS (saved_text));
}

/* Lisp functions to do output using a stream must start with a call to
   print_prepare, and end with calling print_finish.
   Use printchar to output one character, or call strout to output a
   block of characters.  */

/* State carried between print_prepare and print_finish.  */
struct print_context
{
  Lisp_Object printcharfun;
  Lisp_Object old_printcharfun;
  ptrdiff_t old_point, start_point;
  ptrdiff_t old_point_byte, start_point_byte;
  specpdl_ref specpdl_count;
};

static inline struct print_context
print_prepare (Lisp_Object printcharfun)
{
  struct print_context pc = {
    .old_printcharfun = printcharfun,
    .old_point = -1,
    .start_point = -1,
    .old_point_byte = -1,
    .start_point_byte = -1,
    .specpdl_count = SPECPDL_INDEX (),
  };
  bool multibyte = !NILP (BVAR (current_buffer, enable_multibyte_characters));
  record_unwind_current_buffer ();
  specbind(Qprint__unreadable_callback_buffer, Fcurrent_buffer ());
  if (NILP (printcharfun))
    printcharfun = Qt;
  if (BUFFERP (printcharfun))
    {
      if (XBUFFER (printcharfun) != current_buffer)
	Fset_buffer (printcharfun);
      printcharfun = Qnil;
    }
  if (MARKERP (printcharfun))
    {
      if (! XMARKER (printcharfun)->buffer)
	error ("Marker does not point anywhere");
      if (XMARKER (printcharfun)->buffer != current_buffer)
	set_buffer_internal (XMARKER (printcharfun)->buffer);
      ptrdiff_t marker_pos = marker_position (printcharfun);
      if (marker_pos < BEGV || marker_pos > ZV)
	signal_error ("Marker is outside the accessible part of the buffer",
		      printcharfun);
      pc.old_point = PT;
      pc.old_point_byte = PT_BYTE;
      SET_PT_BOTH (marker_pos, marker_byte_position (printcharfun));
      pc.start_point = PT;
      pc.start_point_byte = PT_BYTE;
      printcharfun = Qnil;
    }
  if (NILP (printcharfun))
    {
      if (NILP (BVAR (current_buffer, enable_multibyte_characters))
	  && ! print_escape_multibyte)
	specbind (Qprint_escape_multibyte, Qt);
      if (! NILP (BVAR (current_buffer, enable_multibyte_characters))
	  && ! print_escape_nonascii)
	specbind (Qprint_escape_nonascii, Qt);
      if (print_buffer.buffer != NULL)
	{
	  Lisp_Object string = make_string_from_bytes (print_buffer.buffer,
						       print_buffer.pos,
						       print_buffer.pos_byte);
	  record_unwind_protect (print_unwind, string);
	}
      else
	{
	  int new_size = 1000;
	  print_buffer.buffer = xmalloc (new_size);
	  print_buffer.size = new_size;
	  record_unwind_protect_void (print_free_buffer);
	}
      print_buffer.pos = 0;
      print_buffer.pos_byte = 0;
    }
  if (EQ (printcharfun, Qt) && ! noninteractive)
    setup_echo_area_for_printing (multibyte);
  pc.printcharfun = printcharfun;
  return pc;
}

static inline void
print_finish (struct print_context *pc)
{
  if (NILP (pc->printcharfun))
    {
      if (print_buffer.pos != print_buffer.pos_byte
	  && NILP (BVAR (current_buffer, enable_multibyte_characters)))
	{
	  USE_SAFE_ALLOCA;
	  unsigned char *temp = SAFE_ALLOCA (print_buffer.pos + 1);
	  copy_text ((unsigned char *) print_buffer.buffer, temp,
		     print_buffer.pos_byte, 1, 0);
	  insert_1_both ((char *) temp, print_buffer.pos,
			 print_buffer.pos, 0, 1, 0);
	  SAFE_FREE ();
	}
      else
	insert_1_both (print_buffer.buffer, print_buffer.pos,
		       print_buffer.pos_byte, 0, 1, 0);
      signal_after_change (PT - print_buffer.pos, 0, print_buffer.pos);
    }
  if (MARKERP (pc->old_printcharfun))
    set_marker_both (pc->old_printcharfun, Qnil, PT, PT_BYTE);
  if (pc->old_point >= 0)
    SET_PT_BOTH (pc->old_point
		 + (pc->old_point >= pc->start_point
		    ? PT - pc->start_point : 0),
		 pc->old_point_byte
		 + (pc->old_point_byte >= pc->start_point_byte
		    ? PT_BYTE - pc->start_point_byte : 0));
  unbind_to (pc->specpdl_count, Qnil);
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
	  putc (ch, stream);
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
	  fwrite (SSDATA (encoded_ch), 1, SBYTES (encoded_ch), stream);
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
      ch = XFIXNAT (AREF (dv, i));
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
    call1 (fun, make_fixnum (ch));
  else
    {
      unsigned char str[MAX_MULTIBYTE_LENGTH];
      int len = CHAR_STRING (ch, str);

      maybe_quit ();

      if (NILP (fun))
	{
	  ptrdiff_t incr = len - (print_buffer.size - print_buffer.pos_byte);
	  if (incr > 0)
	    print_buffer.buffer = xpalloc (print_buffer.buffer,
					   &print_buffer.size,
					   incr, -1, 1);
	  memcpy (print_buffer.buffer + print_buffer.pos_byte, str, len);
	  print_buffer.pos += 1;
	  print_buffer.pos_byte += len;
	}
      else if (noninteractive)
	{
	  printchar_stdout_last = ch;
	  if (DISP_TABLE_P (Vstandard_display_table))
	    printchar_to_stream (ch, stdout);
	  else
	    fwrite (str, 1, len, stdout);
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

/* Output an octal escape for C.  If C is less than '\100' consult the
   following character (if any) to see whether to use three octal
   digits to avoid misinterpretation of the next character.  The next
   character after C will be taken from DATA, starting at byte
   location I, if I is less than SIZE.  Use PRINTCHARFUN to output
   each character.  */

static void
octalout (unsigned char c, unsigned char *data, ptrdiff_t i, ptrdiff_t size,
	  Lisp_Object printcharfun)
{
  int digits = (c > '\77' || (i < size && '0' <= data[i] && data[i] <= '7')
		? 3
		: c > '\7' ? 2 : 1);
  printchar ('\\', printcharfun);
  do
    printchar ('0' + ((c >> (3 * --digits)) & 7), printcharfun);
  while (digits != 0);
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
      ptrdiff_t incr = size_byte - (print_buffer.size - print_buffer.pos_byte);
      if (incr > 0)
	print_buffer.buffer = xpalloc (print_buffer.buffer,
				       &print_buffer.size, incr, -1, 1);
      memcpy (print_buffer.buffer + print_buffer.pos_byte, ptr, size_byte);
      print_buffer.pos += size;
      print_buffer.pos_byte += size_byte;
    }
  else if (noninteractive && EQ (printcharfun, Qt))
    {
      if (DISP_TABLE_P (Vstandard_display_table))
	{
	  int len;
	  for (ptrdiff_t i = 0; i < size_byte; i += len)
	    {
	      int ch = string_char_and_length ((const unsigned char *) ptr + i,
					       &len);
	      printchar_to_stream (ch, stdout);
	    }
	}
      else
	fwrite (ptr, 1, size_byte, stdout);

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
	      int ch = string_char_and_length ((const unsigned char *) ptr + i,
					       &len);
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
	      int len, ch = (string_char_and_length
			     ((const unsigned char *) ptr + i, &len));
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
	      str_to_multibyte (SDATA (newstr), SDATA (string), chars);
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
	    int len, ch = string_char_and_length (SDATA (string) + i, &len);
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
  CHECK_FIXNUM (character);
  struct print_context pc = print_prepare (printcharfun);
  printchar (XFIXNUM (character), pc.printcharfun);
  print_finish (&pc);
  return character;
}

/* Print the contents of a unibyte C string STRING using PRINTCHARFUN.
   The caller should arrange to put this inside print_prepare and print_finish.
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
  struct print_context pc = print_prepare (printcharfun);
  print_c_string (data, pc.printcharfun);
  print_finish (&pc);
}


void
temp_output_buffer_setup (const char *bufname)
{
  specpdl_ref count = SPECPDL_INDEX ();
  register struct buffer *old = current_buffer;
  register Lisp_Object buf;

  record_unwind_current_buffer ();

  Fset_buffer (Fget_buffer_create (build_string (bufname), Qnil));

  Fkill_all_local_variables (Qnil);
  delete_all_overlays (current_buffer);
  bset_directory (current_buffer, BVAR (old, directory));
  bset_read_only (current_buffer, Qnil);
  bset_filename (current_buffer, Qnil);
  bset_undo_list (current_buffer, Qt);
  eassert (current_buffer->overlays == NULL);
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
  struct print_context pc = print_prepare (printcharfun);

  if (NILP (ensure))
    val = Qt;
  /* Difficult to check if at line beginning so abort.  */
  else if (FUNCTIONP (pc.printcharfun))
    signal_error ("Unsupported function argument", pc.printcharfun);
  else if (noninteractive && !NILP (pc.printcharfun))
    val = printchar_stdout_last == 10 ? Qnil : Qt;
  else
    val = NILP (Fbolp ()) ? Qt : Qnil;

  if (!NILP (val))
    printchar ('\n', pc.printcharfun);
  print_finish (&pc);
  return val;
}

static Lisp_Object Vprint_variable_mapping;

static void
print_bind_all_defaults (void)
{
  for (Lisp_Object vars = Vprint_variable_mapping; !NILP (vars);
       vars = XCDR (vars))
    {
      Lisp_Object elem = XCDR (XCAR (vars));
      specbind (XCAR (elem), XCAR (XCDR (elem)));
    }
}

static void
print_create_variable_mapping (void)
{
  Lisp_Object total[] = {
    list3 (intern ("length"), intern ("print-length"), Qnil),
    list3 (intern ("level"), intern ("print-level"), Qnil),
    list3 (intern ("circle"), intern ("print-circle"), Qnil),
    list3 (intern ("quoted"), intern ("print-quoted"), Qt),
    list3 (intern ("escape-newlines"), intern ("print-escape-newlines"), Qnil),
    list3 (intern ("escape-control-characters"),
	   intern ("print-escape-control-characters"), Qnil),
    list3 (intern ("escape-nonascii"), intern ("print-escape-nonascii"), Qnil),
    list3 (intern ("escape-multibyte"),
	   intern ("print-escape-multibyte"), Qnil),
    list3 (intern ("charset-text-property"),
	   intern ("print-charset-text-property"), Qnil),
    list3 (intern ("unreadeable-function"),
	   intern ("print-unreadable-function"), Qnil),
    list3 (intern ("gensym"), intern ("print-gensym"), Qnil),
    list3 (intern ("continuous-numbering"),
	   intern ("print-continuous-numbering"), Qnil),
    list3 (intern ("number-table"), intern ("print-number-table"), Qnil),
    list3 (intern ("float-format"), intern ("float-output-format"), Qnil),
    list3 (intern ("integers-as-characters"),
	   intern ("print-integers-as-characters"), Qnil),
  };

  Vprint_variable_mapping = CALLMANY (Flist, total);
}

static void
print_bind_overrides (Lisp_Object overrides)
{
  if (NILP (Vprint_variable_mapping))
    print_create_variable_mapping ();

  if (EQ (overrides, Qt))
    print_bind_all_defaults ();
  else if (!CONSP (overrides))
    xsignal (Qwrong_type_argument, Qconsp);
  else
    {
      while (!NILP (overrides))
	{
	  Lisp_Object setting = XCAR (overrides);
	  if (EQ (setting, Qt))
	    print_bind_all_defaults ();
	  else if (!CONSP (setting))
	    xsignal (Qwrong_type_argument, Qconsp);
	  else
	    {
	      Lisp_Object key = XCAR (setting),
		value = XCDR (setting);
	      Lisp_Object map = Fassq (key, Vprint_variable_mapping);
	      if (NILP (map))
		xsignal2 (Qwrong_type_argument, Qsymbolp, map);
	      specbind (XCAR (XCDR (map)), value);
	    }

	  if (!NILP (XCDR (overrides)) && !CONSP (XCDR (overrides)))
	    xsignal (Qwrong_type_argument, Qconsp);
	  overrides = XCDR (overrides);
	}
    }
}

DEFUN ("prin1", Fprin1, Sprin1, 1, 3, 0,
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
is used instead.

Optional argument OVERRIDES should be a list of settings for print-related
variables.  An element in this list can be the symbol t, which means "reset
all the values to their defaults".  Otherwise, an element should be a pair,
where the `car' or the pair is the setting symbol, and the `cdr' is the
value of the setting to use for this `prin1' call.

For instance:

  (prin1 object nil \\='((length . 100) (circle . t))).

See Info node `(elisp)Output Overrides' for a list of possible values.

As a special case, OVERRIDES can also simply be the symbol t, which
means "use default values for all the print-related settings".  */)
  (Lisp_Object object, Lisp_Object printcharfun, Lisp_Object overrides)
{
  specpdl_ref count = SPECPDL_INDEX ();

  if (NILP (printcharfun))
    printcharfun = Vstandard_output;
  if (!NILP (overrides))
    print_bind_overrides (overrides);

  struct print_context pc = print_prepare (printcharfun);
  print (object, pc.printcharfun, 1);
  print_finish (&pc);

  return unbind_to (count, object);
}

/* A buffer which is used to hold output being built by prin1-to-string.  */
Lisp_Object Vprin1_to_string_buffer;

DEFUN ("prin1-to-string", Fprin1_to_string, Sprin1_to_string, 1, 3, 0,
       doc: /* Return a string containing the printed representation of OBJECT.
OBJECT can be any Lisp object.  This function outputs quoting characters
when necessary to make output that `read' can handle, whenever possible,
unless the optional second argument NOESCAPE is non-nil.  For complex objects,
the behavior is controlled by `print-level' and `print-length', which see.

OBJECT is any of the Lisp data types: a number, a string, a symbol,
a list, a buffer, a window, a frame, etc.

See `prin1' for the meaning of OVERRIDES.

A printed representation of an object is text which describes that object.  */)
  (Lisp_Object object, Lisp_Object noescape, Lisp_Object overrides)
{
  specpdl_ref count = SPECPDL_INDEX ();

  specbind (Qinhibit_modification_hooks, Qt);

  if (!NILP (overrides))
    print_bind_overrides (overrides);

  /* Save and restore this: we are altering a buffer
     but we don't want to deactivate the mark just for that.
     No need for specbind, since errors deactivate the mark.  */
  Lisp_Object save_deactivate_mark = Vdeactivate_mark;

  struct print_context pc = print_prepare (Vprin1_to_string_buffer);
  print (object, pc.printcharfun, NILP (noescape));
  /* Make Vprin1_to_string_buffer be the default buffer after print_finish */
  print_finish (&pc);

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
  struct print_context pc = print_prepare (printcharfun);
  if (STRINGP (object)
      && !string_intervals (object)
      && NILP (Vprint_continuous_numbering))
    /* fast path for plain strings */
    print_string (object, pc.printcharfun);
  else
    print (object, pc.printcharfun, 0);
  print_finish (&pc);
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
  struct print_context pc = print_prepare (printcharfun);
  printchar ('\n', pc.printcharfun);
  print (object, pc.printcharfun, 1);
  printchar ('\n', pc.printcharfun);
  print_finish (&pc);
  return object;
}

DEFUN ("flush-standard-output", Fflush_standard_output, Sflush_standard_output,
       0, 0, 0,
       doc: /* Flush standard-output.
This can be useful after using `princ' and the like in scripts.  */)
  (void)
{
  fflush (stdout);
  return Qnil;
}

DEFUN ("external-debugging-output", Fexternal_debugging_output, Sexternal_debugging_output, 1, 1, 0,
       doc: /* Write CHARACTER to stderr.
You can call `print' while debugging emacs, and pass it this function
to make it write to the debugging output.  */)
  (Lisp_Object character)
{
  CHECK_FIXNUM (character);
  printchar_to_stream (XFIXNUM (character), stderr);
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

  fflush (stderr);
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
  Fprin1 (arg, Qexternal_debugging_output, Qnil);
  fputs ("\r\n", stderr);
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

/* This function formats the given object and returns the result as a
   string. Use this in contexts where you can inspect strings, but
   where stderr output won't work --- e.g., while replaying rr
   recordings.  */
const char * debug_format (const char *, Lisp_Object) EXTERNALLY_VISIBLE;
const char *
debug_format (const char *fmt, Lisp_Object arg)
{
  return SSDATA (CALLN (Fformat, build_string (fmt), arg));
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
      errmsg = Fget (errname, Qerror_message);
      /* During loadup 'substitute-command-keys' might not be available.  */
      if (!NILP (Ffboundp (Qsubstitute_command_keys)))
	{
	  /* `substitute-command-keys' may bug out, which would lead
	     to infinite recursion when we're called from
	     skip_debugger, so ignore errors.  */
	  Lisp_Object subs = safe_call1 (Qsubstitute_command_keys, errmsg);
	  if (!NILP (subs))
	    errmsg = subs;
	}

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

    FOR_EACH_TAIL (tail)
      {
	if (sep)
	  write_string (sep, stream);
	sep = ", ";
	Lisp_Object obj = XCAR (tail);
	if (!NILP (file_error)
	    || EQ (errname, Qend_of_file) || EQ (errname, Quser_error))
	  Fprinc (obj, stream);
	else
	  Fprin1 (obj, stream, Qnil);
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

  if (isinf (data))
    {
      static char const minus_infinity_string[] = "-1.0e+INF";
      bool positive = 0 < data;
      strcpy (buf, minus_infinity_string + positive);
      return sizeof minus_infinity_string - 1 - positive;
    }
#if IEEE_FLOATING_POINT
  if (isnan (data))
    {
      union ieee754_double u = { .d = data };
      uintmax_t hi = u.ieee_nan.mantissa0;
      return sprintf (buf, &"-%"PRIuMAX".0e+NaN"[!u.ieee_nan.negative],
		      (hi << 31 << 1) + u.ieee_nan.mantissa1);
    }
#endif

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

  /* Construct Vprint_number_table for print-circle.  */
  if (!NILP (Vprint_circle))
    {
      /* Construct Vprint_number_table.
	 This increments print_number_index for the objects added.  */
      print_preprocess (obj);

      if (HASH_TABLE_P (Vprint_number_table))
	{ /* Remove unnecessary objects, which appear only once in OBJ;
	     that is, whose status is Qt.  */
	  struct Lisp_Hash_Table *h = XHASH_TABLE (Vprint_number_table);
	  ptrdiff_t i;

	  for (i = 0; i < HASH_TABLE_SIZE (h); ++i)
            {
              Lisp_Object key =  HASH_KEY (h, i);
	      if (!BASE_EQ (key, Qunbound)
		  && EQ (HASH_VALUE (h, i), Qt))
	        Fremhash (key, Vprint_number_table);
            }
	}
    }

  print_depth = 0;
  print_object (obj, printcharfun, escapeflag);
}

#define PRINT_CIRCLE_CANDIDATE_P(obj)			   \
  (STRINGP (obj)                                           \
   || CONSP (obj)					   \
   || (VECTORLIKEP (obj)				   \
       && (VECTORP (obj) || COMPILEDP (obj)		   \
	   || CHAR_TABLE_P (obj) || SUB_CHAR_TABLE_P (obj) \
	   || HASH_TABLE_P (obj) || FONTP (obj)		   \
	   || RECORDP (obj)))				   \
   || (! NILP (Vprint_gensym)				   \
       && SYMBOLP (obj)					   \
       && !SYMBOL_INTERNED_P (obj)))

/* The print preprocess stack, used to traverse data structures.  */

struct print_pp_entry {
  ptrdiff_t n;			/* number of values, or 0 if a single value */
  union {
    Lisp_Object value;		/* when n = 0 */
    Lisp_Object *values;	/* when n > 0 */
  } u;
};

struct print_pp_stack {
  struct print_pp_entry *stack;	 /* base of stack */
  ptrdiff_t size;		 /* allocated size in entries */
  ptrdiff_t sp;			 /* current number of entries */
};

static struct print_pp_stack ppstack = {NULL, 0, 0};

NO_INLINE static void
grow_pp_stack (void)
{
  struct print_pp_stack *ps = &ppstack;
  eassert (ps->sp == ps->size);
  ps->stack = xpalloc (ps->stack, &ps->size, 1, -1, sizeof *ps->stack);
  eassert (ps->sp < ps->size);
}

static inline void
pp_stack_push_value (Lisp_Object value)
{
  if (ppstack.sp >= ppstack.size)
    grow_pp_stack ();
  ppstack.stack[ppstack.sp++] = (struct print_pp_entry){.n = 0,
							.u.value = value};
}

static inline void
pp_stack_push_values (Lisp_Object *values, ptrdiff_t n)
{
  eassume (n >= 0);
  if (n == 0)
    return;
  if (ppstack.sp >= ppstack.size)
    grow_pp_stack ();
  ppstack.stack[ppstack.sp++] = (struct print_pp_entry){.n = n,
							.u.values = values};
}

static inline bool
pp_stack_empty_p (void)
{
  return ppstack.sp <= 0;
}

static inline Lisp_Object
pp_stack_pop (void)
{
  eassume (!pp_stack_empty_p ());
  struct print_pp_entry *e = &ppstack.stack[ppstack.sp - 1];
  if (e->n == 0)		/* single value */
    {
      --ppstack.sp;
      return e->u.value;
    }
  /* Array of values: pop them left to right, which seems to be slightly
     faster than right to left.  */
  e->n--;
  if (e->n == 0)
    --ppstack.sp;		/* last value consumed */
  return (++e->u.values)[-1];
}

/* Construct Vprint_number_table for the print-circle feature
   according to the structure of OBJ.  OBJ itself and all its elements
   will be added to Vprint_number_table recursively if it is a list,
   vector, compiled function, char-table, string (its text properties
   will be traced), or a symbol that has no obarray (this is for the
   print-gensym feature).  The status fields of Vprint_number_table
   mean whether each object appears more than once in OBJ: Qnil at the
   first time, and Qt after that.  */
static void
print_preprocess (Lisp_Object obj)
{
  eassert (!NILP (Vprint_circle));
  ptrdiff_t base_sp = ppstack.sp;

  for (;;)
    {
      if (PRINT_CIRCLE_CANDIDATE_P (obj))
	{
	  if (!HASH_TABLE_P (Vprint_number_table))
	    Vprint_number_table = CALLN (Fmake_hash_table, QCtest, Qeq);

	  Lisp_Object num = Fgethash (obj, Vprint_number_table, Qnil);
	  if (!NILP (num)
	      /* If Vprint_continuous_numbering is non-nil and OBJ is a gensym,
		 always print the gensym with a number.  This is a special for
		 the lisp function byte-compile-output-docform.  */
	      || (!NILP (Vprint_continuous_numbering)
		  && SYMBOLP (obj)
		  && !SYMBOL_INTERNED_P (obj)))
	    { /* OBJ appears more than once.  Let's remember that.  */
	      if (!FIXNUMP (num))
		{
		  print_number_index++;
		  /* Negative number indicates it hasn't been printed yet.  */
		  Fputhash (obj, make_fixnum (- print_number_index),
			    Vprint_number_table);
		}
	    }
	  else
	    {
	      /* OBJ is not yet recorded.  Let's add to the table.  */
	      Fputhash (obj, Qt, Vprint_number_table);

	      switch (XTYPE (obj))
		{
		case Lisp_String:
		  /* A string may have text properties,
		     which can be circular. */
		  traverse_intervals_noorder (string_intervals (obj),
					      print_preprocess_string, NULL);
		  break;

		case Lisp_Cons:
		  if (!NILP (XCDR (obj)))
		    pp_stack_push_value (XCDR (obj));
		  obj = XCAR (obj);
		  continue;

		case Lisp_Vectorlike:
		  {
		    struct Lisp_Vector *vec = XVECTOR (obj);
		    ptrdiff_t size = ASIZE (obj);
		    if (size & PSEUDOVECTOR_FLAG)
		      size &= PSEUDOVECTOR_SIZE_MASK;
		    ptrdiff_t start = (SUB_CHAR_TABLE_P (obj)
				       ? SUB_CHAR_TABLE_OFFSET : 0);
		    pp_stack_push_values (vec->contents + start, size - start);
		    if (HASH_TABLE_P (obj))
		      {
			struct Lisp_Hash_Table *h = XHASH_TABLE (obj);
			obj = h->key_and_value;
			continue;
		      }
		    break;
		  }

		default:
		  break;
		}
	    }
	}

      if (ppstack.sp <= base_sp)
	break;
      obj = pp_stack_pop ();
    }
}

DEFUN ("print--preprocess", Fprint_preprocess, Sprint_preprocess, 1, 1, 0,
       doc: /* Extract sharing info from OBJECT needed to print it.
Fills `print-number-table' if `print-circle' is non-nil.  Does nothing
if `print-circle' is nil.  */)
     (Lisp_Object object)
{
  if (!NILP (Vprint_circle))
    {
      print_number_index = 0;
      print_preprocess (object);
    }
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
  if (! (print_check_string_result & PRINT_STRING_UNSAFE_CHARSET_FOUND))
    {
      ptrdiff_t charpos = interval->position;
      ptrdiff_t bytepos = string_char_to_byte (string, charpos);
      Lisp_Object charset = XCAR (XCDR (val));

      for (ptrdiff_t i = 0; i < LENGTH (interval); i++)
	{
	  int c = fetch_string_char_advance (string, &charpos, &bytepos);
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
  if (NILP (Vprint_charset_text_property)
      || ! (print_check_string_result & PRINT_STRING_UNSAFE_CHARSET_FOUND))
    {
      string = Fcopy_sequence (string);
      if (print_check_string_result & PRINT_STRING_NON_CHARSET_FOUND)
	{
	  if (NILP (print_prune_charset_plist))
	    print_prune_charset_plist = list1 (Qcharset);
	  Fremove_text_properties (make_fixnum (0),
				   make_fixnum (SCHARS (string)),
				   print_prune_charset_plist, string);
	}
      else
	Fset_text_properties (make_fixnum (0), make_fixnum (SCHARS (string)),
			      Qnil, string);
    }
  return string;
}

#ifdef HAVE_MODULES
/* Return a data pointer equal to FUNCPTR.  */

static void const *
data_from_funcptr (void (*funcptr) (void))
{
  /* The module code, and the POSIX API for dynamic linking, already
     assume that function and data pointers are represented
     interchangeably, so it's OK to assume that here too.  */
  return (void const *) funcptr;
}

/* Print the value of the pointer PTR.  */

static void
print_pointer (Lisp_Object printcharfun, char *buf, const char *prefix,
               const void *ptr)
{
  uintptr_t ui = (uintptr_t) ptr;

  /* In theory this assignment could lose info on pre-C99 hosts, but
     in practice it doesn't.  */
  uintmax_t up = ui;

  int len = sprintf (buf, "%s 0x%" PRIxMAX, prefix, up);
  strout (buf, len, len, printcharfun);
}
#endif

static bool
print_vectorlike (Lisp_Object obj, Lisp_Object printcharfun, bool escapeflag,
		  char *buf)
{
  /* First do all the vectorlike types that have a readable syntax.  */
  switch (PSEUDOVECTOR_TYPE (XVECTOR (obj)))
    {
    case PVEC_BIGNUM:
      {
	ptrdiff_t size = bignum_bufsize (obj, 10);
	USE_SAFE_ALLOCA;
	char *str = SAFE_ALLOCA (size);
	ptrdiff_t len = bignum_to_c_string (str, size, obj, 10);
	strout (str, len, len, printcharfun);
	SAFE_FREE ();
      }
      return true;

    case PVEC_BOOL_VECTOR:
      {
	EMACS_INT size = bool_vector_size (obj);
	ptrdiff_t size_in_bytes = bool_vector_bytes (size);
	ptrdiff_t real_size_in_bytes = size_in_bytes;
	unsigned char *data = bool_vector_uchar_data (obj);

	int len = sprintf (buf, "#&%"pI"d\"", size);
	strout (buf, len, len, printcharfun);

	/* Don't print more bytes than the specified maximum.
	   Negative values of print-length are invalid.  Treat them
	   like a print-length of nil.  */
	if (FIXNATP (Vprint_length)
	    && XFIXNAT (Vprint_length) < size_in_bytes)
	  size_in_bytes = XFIXNAT (Vprint_length);

	for (ptrdiff_t i = 0; i < size_in_bytes; i++)
	  {
	    maybe_quit ();
	    unsigned char c = data[i];
	    if (c == '\n' && print_escape_newlines)
	      print_c_string ("\\n", printcharfun);
	    else if (c == '\f' && print_escape_newlines)
	      print_c_string ("\\f", printcharfun);
	    else if (c > '\177'
		     || (print_escape_control_characters && c_iscntrl (c)))
	      {
		/* Use octal escapes to avoid encoding issues.  */
		octalout (c, data, i + 1, size_in_bytes, printcharfun);
	      }
	    else
	      {
		if (c == '\"' || c == '\\')
		  printchar ('\\', printcharfun);
		printchar (c, printcharfun);
	      }
	  }

	if (size_in_bytes < real_size_in_bytes)
	  print_c_string (" ...", printcharfun);
	printchar ('\"', printcharfun);
      }
      return true;

    default:
      break;
    }

  /* Then do all the pseudovector types that don't have a readable
     syntax.  First check whether this is handled by
     `print-unreadable-function'.  */
  if (!NILP (Vprint_unreadable_function)
      && FUNCTIONP (Vprint_unreadable_function))
    {
      specpdl_ref count = SPECPDL_INDEX ();
      /* Bind `print-unreadable-function' to nil to avoid accidental
	 infinite recursion in the function called.  */
      Lisp_Object func = Vprint_unreadable_function;
      specbind (Qprint_unreadable_function, Qnil);

      /* If we're being called from `prin1-to-string' or the like,
	 we're now in the secret " prin1" buffer.  This can lead to
	 problems if, for instance, the callback function switches a
	 window to this buffer -- this will make Emacs segfault.  */
      if (!NILP (Vprint__unreadable_callback_buffer)
	  && !NILP (Fbuffer_live_p (Vprint__unreadable_callback_buffer)))
	{
	  record_unwind_current_buffer ();
	  set_buffer_internal (XBUFFER (Vprint__unreadable_callback_buffer));
	}
      Lisp_Object result = CALLN (Ffuncall, func, obj,
				  escapeflag? Qt: Qnil);
      unbind_to (count, Qnil);

      if (!NILP (result))
	{
	  if (STRINGP (result))
	    print_string (result, printcharfun);
	  /* It's handled, so stop processing here.  */
	  return true;
	}
    }

  /* Not handled; print unreadable object.  */
  switch (PSEUDOVECTOR_TYPE (XVECTOR (obj)))
    {
    case PVEC_MARKER:
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

    case PVEC_SYMBOL_WITH_POS:
      {
        struct Lisp_Symbol_With_Pos *sp = XSYMBOL_WITH_POS (obj);
        if (print_symbols_bare)
          print_object (sp->sym, printcharfun, escapeflag);
        else
          {
            print_c_string ("#<symbol ", printcharfun);
            if (BARE_SYMBOL_P (sp->sym))
              print_object (sp->sym, printcharfun, escapeflag);
            else
              print_c_string ("NOT A SYMBOL!!", printcharfun);
            if (FIXNUMP (sp->pos))
              {
                print_c_string (" at ", printcharfun);
                print_object (sp->pos, printcharfun, escapeflag);
              }
            else
              print_c_string (" NOT A POSITION!!", printcharfun);
            printchar ('>', printcharfun);
          }
      }
      break;

    case PVEC_OVERLAY:
      print_c_string ("#<overlay ", printcharfun);
      if (! OVERLAY_BUFFER (obj))
	print_c_string ("in no buffer", printcharfun);
      else
	{
	  int len = sprintf (buf, "from %"pD"d to %"pD"d in ",
			     OVERLAY_START (obj),
			     OVERLAY_END   (obj));
	  strout (buf, len, len, printcharfun);
	  print_string (BVAR (OVERLAY_BUFFER (obj), name),
			printcharfun);
	}
      printchar ('>', printcharfun);
      break;

    case PVEC_USER_PTR:
      {
	print_c_string ("#<user-ptr ", printcharfun);
	int i = sprintf (buf, "ptr=%p finalizer=%p",
			 XUSER_PTR (obj)->p,
			 (void *) XUSER_PTR (obj)->finalizer);
	strout (buf, i, i, printcharfun);
	printchar ('>', printcharfun);
      }
      break;

    case PVEC_FINALIZER:
      print_c_string ("#<finalizer", printcharfun);
      if (NILP (XFINALIZER (obj)->function))
	print_c_string (" used", printcharfun);
      printchar ('>', printcharfun);
      break;

    case PVEC_MISC_PTR:
      {
	/* This shouldn't happen in normal usage, but let's
	   print it anyway for the benefit of the debugger.  */
	int i = sprintf (buf, "#<ptr %p>", xmint_pointer (obj));
	strout (buf, i, i, printcharfun);
      }
      break;

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

    case PVEC_SUBR:
      print_c_string ("#<subr ", printcharfun);
      print_c_string (XSUBR (obj)->symbol_name, printcharfun);
      printchar ('>', printcharfun);
      break;

    case PVEC_XWIDGET:
#ifdef HAVE_XWIDGETS
      {
	if (NILP (XXWIDGET (obj)->buffer))
	  print_c_string ("#<killed xwidget>", printcharfun);
	else
	  {
#ifdef USE_GTK
	    int len = sprintf (buf, "#<xwidget %u %p>",
			       XXWIDGET (obj)->xwidget_id,
			       XXWIDGET (obj)->widget_osr);
#else
	    int len = sprintf (buf, "#<xwidget %u %p>",
			       XXWIDGET (obj)->xwidget_id,
			       XXWIDGET (obj)->xwWidget);
#endif
	    strout (buf, len, len, printcharfun);
	  }
	break;
      }
#else
      emacs_abort ();
#endif
    case PVEC_XWIDGET_VIEW:
      print_c_string ("#<xwidget view", printcharfun);
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
	  void *p = XTHREAD (obj);
	  int len = sprintf (buf, "%p", p);
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
	  void *p = XMUTEX (obj);
	  int len = sprintf (buf, "%p", p);
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
	  void *p = XCONDVAR (obj);
	  int len = sprintf (buf, "%p", p);
	  strout (buf, len, len, printcharfun);
	}
      printchar ('>', printcharfun);
      break;

#ifdef HAVE_MODULES
    case PVEC_MODULE_FUNCTION:
      {
	print_c_string ("#<module function ", printcharfun);
        const struct Lisp_Module_Function *function = XMODULE_FUNCTION (obj);
        module_funcptr ptr = module_function_address (function);
	char const *file;
	char const *symbol;
	dynlib_addr (ptr, &file, &symbol);

	if (symbol == NULL)
          print_pointer (printcharfun, buf, "at", data_from_funcptr (ptr));
        else
	  print_c_string (symbol, printcharfun);

        void *data = module_function_data (function);
        if (data != NULL)
          print_pointer (printcharfun, buf, " with data", data);

        if (file != NULL)
	  {
	    print_c_string (" from ", printcharfun);
	    print_c_string (file, printcharfun);
	  }

	printchar ('>', printcharfun);
      }
      break;
#endif
#ifdef HAVE_NATIVE_COMP
    case PVEC_NATIVE_COMP_UNIT:
      {
	struct Lisp_Native_Comp_Unit *cu = XNATIVE_COMP_UNIT (obj);
	print_c_string ("#<native compilation unit: ", printcharfun);
	print_string (cu->file, printcharfun);
	printchar (' ', printcharfun);
	print_object (cu->optimize_qualities, printcharfun, escapeflag);
	printchar ('>', printcharfun);
      }
      break;
#endif

#ifdef HAVE_TREE_SITTER
    case PVEC_TS_PARSER:
      print_c_string ("#<treesit-parser for ", printcharfun);
      Lisp_Object language = XTS_PARSER (obj)->language_symbol;
      /* No need to print the buffer because it's not that useful: we
	 usually know which buffer a parser belongs to.  */
      print_string (Fsymbol_name (language), printcharfun);
      printchar ('>', printcharfun);
      break;
    case PVEC_TS_NODE:
      /* Prints #<treesit-node (identifier) in 12-15> or
         #<treesit-node "keyword" in 28-31>. */
      print_c_string ("#<treesit-node", printcharfun);
      if (!treesit_node_uptodate_p (obj))
	{
	  print_c_string ("-outdated>", printcharfun);
	  break;
	}
      printchar (' ', printcharfun);
      /* Now the node must be up-to-date, and calling functions like
	 Ftreesit_node_start will not signal.  */
      bool named = treesit_named_node_p (XTS_NODE (obj)->node);
      /* We used to use () as delimiters for named nodes, but that
	 confuses pretty-printing a tad bit.  There might be more
	 little breakages here and there if we print parenthesizes
	 inside an object, so I guess better not do it.
	 (bug#60696)  */
      const char *delim1 = named ? "" : "\"";
      const char *delim2 = named ? "" : "\"";
      print_c_string (delim1, printcharfun);
      print_string (Ftreesit_node_type (obj), printcharfun);
      print_c_string (delim2, printcharfun);
      print_c_string (" in ", printcharfun);
      print_object (Ftreesit_node_start (obj), printcharfun, escapeflag);
      printchar ('-', printcharfun);
      print_object (Ftreesit_node_end (obj), printcharfun, escapeflag);
      printchar ('>', printcharfun);
      break;
    case PVEC_TS_COMPILED_QUERY:
      print_c_string ("#<treesit-compiled-query>", printcharfun);
      break;
#endif

    case PVEC_SQLITE:
      {
	print_c_string ("#<sqlite ", printcharfun);
	int i = sprintf (buf, "db=%p", XSQLITE (obj)->db);
	strout (buf, i, i, printcharfun);
	if (XSQLITE (obj)->is_statement)
	  {
	    i = sprintf (buf, " stmt=%p", XSQLITE (obj)->stmt);
	    strout (buf, i, i, printcharfun);
	  }
	print_c_string (" name=", printcharfun);
	print_c_string (XSQLITE (obj)->name, printcharfun);
	printchar ('>', printcharfun);
      }
      break;

    default:
      emacs_abort ();
    }

  return true;
}

static char
named_escape (int i)
{
  switch (i)
    {
    case '\b': return 'b';
    case '\t': return 't';
    case '\n': return 'n';
    case '\f': return 'f';
    case '\r': return 'r';
    case ' ':  return 's';
      /* \a, \v, \e and \d are excluded from printing as escapes since
         they are somewhat rare as characters and more likely to be
         plain integers. */
    }
  return 0;
}

enum print_entry_type
  {
    PE_list,			/* print rest of list */
    PE_rbrac,			/* print ")" */
    PE_vector,			/* print rest of vector */
    PE_hash,			/* print rest of hash data */
  };

struct print_stack_entry
{
  enum print_entry_type type;

  union
  {
    struct
    {
      Lisp_Object last;		/* cons whose car was just printed  */
      intmax_t maxlen;		/* max number of elements left to print */
      /* State for Brent cycle detection.  See
	 Brent RP. BIT. 1980;20(2):176-184. doi:10.1007/BF01933190
	 https://maths-people.anu.edu.au/~brent/pd/rpb051i.pdf */
      Lisp_Object tortoise;     /* slow pointer */
      ptrdiff_t n;		/* tortoise step countdown */
      ptrdiff_t m;		/* tortoise step period */
      intmax_t tortoise_idx;	/* index of tortoise */
    } list;

    struct
    {
      Lisp_Object obj;		/* object to print after " . " */
    } dotted_cdr;

    struct
    {
      Lisp_Object obj;		/* vector object */
      ptrdiff_t size;		/* length of vector */
      ptrdiff_t idx;		/* index of next element */
      const char *end;		/* string to print at end */
      bool truncated;		/* whether to print "..." before end */
    } vector;

    struct
    {
      Lisp_Object obj;		/* hash-table object */
      ptrdiff_t nobjs;		/* number of keys and values to print */
      ptrdiff_t idx;		/* index of key-value pair */
      ptrdiff_t printed;	/* number of keys and values printed */
      bool truncated;		/* whether to print "..." before end */
    } hash;
  } u;
};

struct print_stack
{
  struct print_stack_entry *stack;  /* base of stack */
  ptrdiff_t size;		    /* allocated size in entries */
  ptrdiff_t sp;			    /* current number of entries */
};

static struct print_stack prstack = {NULL, 0, 0};

NO_INLINE static void
grow_print_stack (void)
{
  struct print_stack *ps = &prstack;
  eassert (ps->sp == ps->size);
  ps->stack = xpalloc (ps->stack, &ps->size, 1, -1, sizeof *ps->stack);
  eassert (ps->sp < ps->size);
}

static inline void
print_stack_push (struct print_stack_entry e)
{
  if (prstack.sp >= prstack.size)
    grow_print_stack ();
  prstack.stack[prstack.sp++] = e;
}

static void
print_stack_push_vector (const char *lbrac, const char *rbrac,
			 Lisp_Object obj, ptrdiff_t start, ptrdiff_t size,
			 Lisp_Object printcharfun)
{
  print_c_string (lbrac, printcharfun);

  ptrdiff_t print_size = ((FIXNATP (Vprint_length)
			   && XFIXNAT (Vprint_length) < size)
			  ? XFIXNAT (Vprint_length) : size);
  print_stack_push ((struct print_stack_entry){
      .type = PE_vector,
      .u.vector.obj = obj,
      .u.vector.size = print_size,
      .u.vector.idx = start,
      .u.vector.end = rbrac,
      .u.vector.truncated = (print_size < size),
    });
}

static void
print_object (Lisp_Object obj, Lisp_Object printcharfun, bool escapeflag)
{
  ptrdiff_t base_depth = print_depth;
  ptrdiff_t base_sp = prstack.sp;
  char buf[max (sizeof "from..to..in " + 2 * INT_STRLEN_BOUND (EMACS_INT),
		max (sizeof " . #" + INT_STRLEN_BOUND (intmax_t),
		     max ((sizeof " with data 0x"
			   + (sizeof (uintmax_t) * CHAR_BIT + 4 - 1) / 4),
			  40)))];
  current_thread->stack_top = buf;

 print_obj:
  maybe_quit ();

  /* Detect circularities and truncate them.  */
  if (NILP (Vprint_circle))
    {
      /* Simple but incomplete way.  */
      if (print_depth >= PRINT_CIRCLE)
	error ("Apparently circular structure being printed");

      for (int i = 0; i < print_depth; i++)
	if (BASE_EQ (obj, being_printed[i]))
	  {
	    int len = sprintf (buf, "#%d", i);
	    strout (buf, len, len, printcharfun);
	    goto next_obj;
	  }
      being_printed[print_depth] = obj;
    }
  else if (PRINT_CIRCLE_CANDIDATE_P (obj))
    {
      /* With the print-circle feature.  */
      Lisp_Object num = Fgethash (obj, Vprint_number_table, Qnil);
      if (FIXNUMP (num))
	{
	  EMACS_INT n = XFIXNUM (num);
	  if (n < 0)
	    { /* Add a prefix #n= if OBJ has not yet been printed;
		 that is, its status field is nil.  */
	      int len = sprintf (buf, "#%"pI"d=", -n);
	      strout (buf, len, len, printcharfun);
	      /* OBJ is going to be printed.  Remember that fact.  */
	      Fputhash (obj, make_fixnum (- n), Vprint_number_table);
	    }
	  else
	    {
	      /* Just print #n# if OBJ has already been printed.  */
	      int len = sprintf (buf, "#%"pI"d#", n);
	      strout (buf, len, len, printcharfun);
	      goto next_obj;
	    }
	}
    }

  print_depth++;

  switch (XTYPE (obj))
    {
    case_Lisp_Int:
      {
        EMACS_INT i = XFIXNUM (obj);
        char escaped_name;

	if (print_integers_as_characters && i >= 0 && i <= MAX_UNICODE_CHAR
            && ((escaped_name = named_escape (i))
                || graphic_base_p (i)))
	  {
	    printchar ('?', printcharfun);
            if (escaped_name)
              {
                printchar ('\\', printcharfun);
                i = escaped_name;
              }
            else if (escapeflag
                     && (i == ';' || i == '\"' || i == '\'' || i == '\\'
                         || i == '(' || i == ')'
                         || i == '{' || i == '}'
                         || i == '[' || i == ']'))
	      printchar ('\\', printcharfun);
	    printchar (i, printcharfun);
	  }
	else
	  {
	    char *end = buf + sizeof buf;
	    char *start = fixnum_to_string (i, buf, end);
	    ptrdiff_t len = end - start;
	    strout (start, len, len, printcharfun);
	  }
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
	     cannot be taken as part of a hex character escape.	 */
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
		 corresponding character code before handing it to
		 printchar.  */
	      int c = fetch_string_char_advance (obj, &i, &i_byte);

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
		  octalout (c, SDATA (obj), i_byte, size_byte, printcharfun);
		  need_nonhex = false;
		}
	      else if (multibyte
		       && ! ASCII_CHAR_P (c) && print_escape_multibyte)
		{
		  /* When requested, print multibyte chars using
		     hex escapes.  */
		  char outbuf[sizeof "\\x" + INT_STRLEN_BOUND (c)];
		  int len = sprintf (outbuf, "\\x%04x", c + 0u);
		  strout (outbuf, len, len, printcharfun);
		  need_nonhex = true;
		}
	      else
		{
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
			   : c == '\"' || c == '\\')
		    {
		      printchar ('\\', printcharfun);
		      printchar (c, printcharfun);
		    }
		  else if (print_escape_control_characters && c_iscntrl (c))
		    octalout (c, SDATA (obj), i_byte, size_byte, printcharfun);
		  else if (!multibyte
			   && SINGLE_BYTE_CHAR_P (c)
			   && !ASCII_CHAR_P (c))
		    printchar (BYTE8_TO_CHAR (c), printcharfun);
		  else
		    printchar (c, printcharfun);
		  need_nonhex = false;
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
	Lisp_Object name = SYMBOL_NAME (obj);
	ptrdiff_t size_byte = SBYTES (name);

	char *p = SSDATA (name);
	bool signedp = *p == '-' || *p == '+';
	ptrdiff_t len;
	bool confusing =
	  /* Set CONFUSING if NAME looks like a number, calling
	     string_to_number for non-obvious cases.  */
	  ((c_isdigit (p[signedp]) || p[signedp] == '.')
	   && !NILP (string_to_number (p, 10, &len))
	   && len == size_byte)
	  /* We don't escape "." or "?" (unless they're the first
	     character in the symbol name).  */
	  || *p == '?'
	  || *p == '.';

	if (! NILP (Vprint_gensym)
	    && !SYMBOL_INTERNED_IN_INITIAL_OBARRAY_P (obj))
	  print_c_string ("#:", printcharfun);
	else if (size_byte == 0)
	  {
	    print_c_string ("##", printcharfun);
	    break;
	  }

	ptrdiff_t i = 0;
	for (ptrdiff_t i_byte = 0; i_byte < size_byte; )
	  {
	    /* Here, we must convert each multi-byte form to the
	       corresponding character code before handing it to PRINTCHAR.  */
	    int c = fetch_string_char_advance (name, &i, &i_byte);
	    maybe_quit ();

	    if (escapeflag)
	      {
		if (c == '\"' || c == '\\' || c == '\''
		    || c == ';' || c == '#' || c == '(' || c == ')'
		    || c == ',' || c == '`'
		    || c == '[' || c == ']' || c <= 040
		    || c == NO_BREAK_SPACE
		    || confusing)
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
      if (FIXNUMP (Vprint_level)
	  && print_depth > XFIXNUM (Vprint_level))
	print_c_string ("...", printcharfun);
      else if (print_quoted && CONSP (XCDR (obj)) && NILP (XCDR (XCDR (obj)))
	       && EQ (XCAR (obj), Qquote))
	{
	  printchar ('\'', printcharfun);
	  obj = XCAR (XCDR (obj));
	  --print_depth;	/* tail recursion */
	  goto print_obj;
	}
      else if (print_quoted && CONSP (XCDR (obj)) && NILP (XCDR (XCDR (obj)))
	       && EQ (XCAR (obj), Qfunction))
	{
	  print_c_string ("#'", printcharfun);
	  obj = XCAR (XCDR (obj));
	  --print_depth;	/* tail recursion */
	  goto print_obj;
	}
      /* FIXME: Do we really need the new_backquote_output gating of
	 special syntax for comma and comma-at?  There is basically no
	 benefit from it at all, and it would be nice to get rid of
	 the recursion here without additional complexity.  */
      else if (print_quoted && CONSP (XCDR (obj)) && NILP (XCDR (XCDR (obj)))
	       && EQ (XCAR (obj), Qbackquote))
	{
	  printchar ('`', printcharfun);
	  new_backquote_output++;
	  print_object (XCAR (XCDR (obj)), printcharfun, escapeflag);
	  new_backquote_output--;
	}
      else if (print_quoted && CONSP (XCDR (obj)) && NILP (XCDR (XCDR (obj)))
	       && (EQ (XCAR (obj), Qcomma)
		   || EQ (XCAR (obj), Qcomma_at))
	       && new_backquote_output)
	{
	  print_object (XCAR (obj), printcharfun, false);
	  new_backquote_output--;
	  print_object (XCAR (XCDR (obj)), printcharfun, escapeflag);
	  new_backquote_output++;
	}
      else
	{
	  printchar ('(', printcharfun);
	  /* Negative values of print-length are invalid in CL.
	     Treat them like nil, as CMUCL does.  */
	  intmax_t print_length = (FIXNATP (Vprint_length)
				   ? XFIXNAT (Vprint_length)
				   : INTMAX_MAX);
	  if (print_length == 0)
	    print_c_string ("...)", printcharfun);
	  else
	    {
	      print_stack_push ((struct print_stack_entry){
		  .type = PE_list,
		  .u.list.last = obj,
		  .u.list.maxlen = print_length,
		  .u.list.tortoise = obj,
		  .u.list.n = 2,
		  .u.list.m = 2,
		  .u.list.tortoise_idx = 0,
		});
	      /* print the car */
	      obj = XCAR (obj);
	      goto print_obj;
	    }
	}
      break;

    case Lisp_Vectorlike:
      /* First do all the vectorlike types that have a readable syntax.  */
      switch (PSEUDOVECTOR_TYPE (XVECTOR (obj)))
	{
	case PVEC_NORMAL_VECTOR:
	  {
	    print_stack_push_vector ("[", "]", obj, 0, ASIZE (obj),
				     printcharfun);
	    goto next_obj;
	  }
	case PVEC_RECORD:
	  {
	    print_stack_push_vector ("#s(", ")", obj, 0, PVSIZE (obj),
				     printcharfun);
	    goto next_obj;
	  }
	case PVEC_COMPILED:
	  {
	    print_stack_push_vector ("#[", "]", obj, 0, PVSIZE (obj),
				     printcharfun);
	    goto next_obj;
	  }
	case PVEC_CHAR_TABLE:
	  {
	    print_stack_push_vector ("#^[", "]", obj, 0, PVSIZE (obj),
				     printcharfun);
	    goto next_obj;
	  }
	case PVEC_SUB_CHAR_TABLE:
	  {
	    /* Make each lowest sub_char_table start a new line.
	       Otherwise we'll make a line extremely long, which
	       results in slow redisplay.  */
	    if (XSUB_CHAR_TABLE (obj)->depth == 3)
	      printchar ('\n', printcharfun);
	    print_c_string ("#^^[", printcharfun);
	    int n = sprintf (buf, "%d %d",
			     XSUB_CHAR_TABLE (obj)->depth,
			     XSUB_CHAR_TABLE (obj)->min_char);
	    strout (buf, n, n, printcharfun);
	    print_stack_push_vector ("", "]", obj,
				     SUB_CHAR_TABLE_OFFSET, PVSIZE (obj),
				     printcharfun);
	    goto next_obj;
	  }
	case PVEC_HASH_TABLE:
	  {
	    struct Lisp_Hash_Table *h = XHASH_TABLE (obj);
	    /* Implement a readable output, e.g.:
	       #s(hash-table size 2 test equal data (k1 v1 k2 v2)) */
	    /* Always print the size.  */
	    int len = sprintf (buf, "#s(hash-table size %"pD"d",
			       HASH_TABLE_SIZE (h));
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

	    if (h->purecopy)
	      print_c_string (" purecopy t", printcharfun);

	    print_c_string (" data (", printcharfun);

	    ptrdiff_t size = h->count;
	    /* Don't print more elements than the specified maximum.  */
	    if (FIXNATP (Vprint_length) && XFIXNAT (Vprint_length) < size)
	      size = XFIXNAT (Vprint_length);

	    print_stack_push ((struct print_stack_entry){
		.type = PE_hash,
		.u.hash.obj = obj,
		.u.hash.nobjs = size * 2,
		.u.hash.idx = 0,
		.u.hash.printed = 0,
		.u.hash.truncated = (size < h->count),
	      });
	    goto next_obj;
	  }

	default:
	  break;
	}

      if (print_vectorlike (obj, printcharfun, escapeflag, buf))
	break;
      FALLTHROUGH;

    default:
      {
	int len;
	/* We're in trouble if this happens!
	   Probably should just emacs_abort ().	 */
	print_c_string ("#<EMACS BUG: INVALID DATATYPE ", printcharfun);
	if (VECTORLIKEP (obj))
	  len = sprintf (buf, "(PVEC 0x%08zx)", (size_t) ASIZE (obj));
	else
	  len = sprintf (buf, "(0x%02x)", (unsigned) XTYPE (obj));
	strout (buf, len, len, printcharfun);
	print_c_string ((" Save your buffers immediately"
			 " and please report this bug>"),
			printcharfun);
	break;
      }
    }
  print_depth--;

 next_obj:
  if (prstack.sp > base_sp)
    {
      /* Handle a continuation on the print stack.  */
      struct print_stack_entry *e = &prstack.stack[prstack.sp - 1];
      switch (e->type)
	{
	case PE_list:
	  {
	    /* after "(" ELEM (* " " ELEM) */
	    Lisp_Object next = XCDR (e->u.list.last);
	    if (NILP (next))
	      {
		/* end of list: print ")" */
		printchar (')', printcharfun);
		--prstack.sp;
		--print_depth;
		goto next_obj;
	      }
	    else if (CONSP (next))
	      {
		if (!NILP (Vprint_circle))
		  {
		    /* With the print-circle feature.  */
		    Lisp_Object num = Fgethash (next, Vprint_number_table,
						Qnil);
		    if (FIXNUMP (num))
		      {
			print_c_string (" . ", printcharfun);
			obj = next;
			e->type = PE_rbrac;
			goto print_obj;
		      }
		  }

		/* list continues: print " " ELEM ... */

		printchar (' ', printcharfun);

		--e->u.list.maxlen;
		if (e->u.list.maxlen <= 0)
		  {
		    print_c_string ("...)", printcharfun);
		    --prstack.sp;
		    --print_depth;
		    goto next_obj;
		  }

		e->u.list.last = next;
		e->u.list.n--;
		if (e->u.list.n == 0)
		  {
		    /* Double tortoise update period and teleport it.  */
		    e->u.list.tortoise_idx += e->u.list.m;
		    e->u.list.m <<= 1;
		    e->u.list.n = e->u.list.m;
		    e->u.list.tortoise = next;
		  }
		else if (BASE_EQ (next, e->u.list.tortoise))
		  {
		    /* FIXME: This #N tail index is somewhat ambiguous;
		       see bug#55395.  */
		    int len = sprintf (buf, ". #%" PRIdMAX ")",
				       e->u.list.tortoise_idx);
		    strout (buf, len, len, printcharfun);
		    --prstack.sp;
		    --print_depth;
		    goto next_obj;
		  }
		obj = XCAR (next);
	      }
	    else
	      {
		/* non-nil ending: print " . " ELEM ")" */
		print_c_string (" . ", printcharfun);
		obj = next;
		e->type = PE_rbrac;
	      }
	    break;
	  }

	case PE_rbrac:
	  printchar (')', printcharfun);
	  --prstack.sp;
	  --print_depth;
	  goto next_obj;

	case PE_vector:
	  if (e->u.vector.idx >= e->u.vector.size)
	    {
	      if (e->u.vector.truncated)
		{
		  if (e->u.vector.idx > 0)
		    printchar (' ', printcharfun);
		  print_c_string ("...", printcharfun);
		}
	      print_c_string (e->u.vector.end, printcharfun);
	      --prstack.sp;
	      --print_depth;
	      goto next_obj;
	    }
	  if (e->u.vector.idx > 0)
	    printchar (' ', printcharfun);
	  obj = AREF (e->u.vector.obj, e->u.vector.idx);
	  e->u.vector.idx++;
	  break;

	case PE_hash:
	  if (e->u.hash.printed >= e->u.hash.nobjs)
	    {
	      if (e->u.hash.truncated)
		{
		  if (e->u.hash.printed)
		    printchar (' ', printcharfun);
		  print_c_string ("...", printcharfun);
		}
	      print_c_string ("))", printcharfun);
	      --prstack.sp;
	      --print_depth;
	      goto next_obj;
	    }

	  if (e->u.hash.printed)
	    printchar (' ', printcharfun);

	  struct Lisp_Hash_Table *h = XHASH_TABLE (e->u.hash.obj);
	  if ((e->u.hash.printed & 1) == 0)
	    {
	      Lisp_Object key;
	      ptrdiff_t idx = e->u.hash.idx;
	      while (BASE_EQ ((key = HASH_KEY (h, idx)), Qunbound))
		idx++;
	      e->u.hash.idx = idx;
	      obj = key;
	    }
	  else
	    {
	      obj = HASH_VALUE (h, e->u.hash.idx);
	      e->u.hash.idx++;
	    }
	  e->u.hash.printed++;
	  break;
	}
      goto print_obj;
    }
  eassert (print_depth == base_depth);
}


/* Print a description of INTERVAL using PRINTCHARFUN.
   This is part of printing a string that has text properties.  */

static void
print_interval (INTERVAL interval, Lisp_Object printcharfun)
{
  if (NILP (interval->plist))
    return;
  printchar (' ', printcharfun);
  print_object (make_fixnum (interval->position), printcharfun, 1);
  printchar (' ', printcharfun);
  print_object (make_fixnum (interval->position + LENGTH (interval)),
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

  DEFVAR_BOOL ("print-integers-as-characters", print_integers_as_characters,
	       doc: /* Non-nil means integers are printed using characters syntax.
Only independent graphic characters, and control characters with named
escape sequences such as newline, are printed this way.  Other
integers, including those corresponding to raw bytes, are printed
as numbers the usual way.  */);
  print_integers_as_characters = false;

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
The value should be nil, t, or `default'.

If the value is nil, don't print the text property `charset'.

If the value is t, always print the text property `charset'.

If the value is `default', print the text property `charset' only when
the value is different from what is guessed in the current charset
priorities.  Values other than nil or t are also treated as
`default'.  */);
  Vprint_charset_text_property = Qdefault;

  DEFVAR_BOOL ("print-symbols-bare", print_symbols_bare,
               doc: /* A flag to control printing of symbols with position.
If the value is nil, print these objects complete with position.
Otherwise print just the bare symbol.  */);
  print_symbols_bare = false;
  DEFSYM (Qprint_symbols_bare, "print-symbols-bare");

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

  DEFSYM (Qprint_escape_multibyte, "print-escape-multibyte");
  DEFSYM (Qprint_escape_nonascii, "print-escape-nonascii");

  print_prune_charset_plist = Qnil;
  staticpro (&print_prune_charset_plist);

  DEFVAR_LISP ("print-unreadable-function", Vprint_unreadable_function,
	       doc: /* If non-nil, a function to call when printing unreadable objects.
By default, Emacs printing functions (like `prin1') print unreadable
objects as \"#<...>\", where \"...\" describes the object (for
instance, \"#<marker in no buffer>\").

If non-nil, it should be a function that will be called with two
arguments: the object to be printed, and the NOESCAPE flag (see
`prin1-to-string').  If this function returns nil, the object will be
printed as usual.  If it returns a string, that string will then be
printed.  If the function returns anything else, the object will not
be printed.  */);
  Vprint_unreadable_function = Qnil;
  DEFSYM (Qprint_unreadable_function, "print-unreadable-function");

  DEFVAR_LISP ("print--unreadable-callback-buffer",
	       Vprint__unreadable_callback_buffer,
	       doc: /* Dynamically bound to indicate current buffer.  */);
  Vprint__unreadable_callback_buffer = Qnil;
  DEFSYM (Qprint__unreadable_callback_buffer,
	  "print--unreadable-callback-buffer");
  /* Don't export this variable to Elisp.  */
  Funintern (Qprint__unreadable_callback_buffer, Qnil);

  defsubr (&Sflush_standard_output);

  /* Initialized in print_create_variable_mapping.  */
  staticpro (&Vprint_variable_mapping);
}
