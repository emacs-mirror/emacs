/* Lisp parsing and input streams.

Copyright (C) 1985-1989, 1993-1995, 1997-2020 Free Software Foundation,
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

/* Tell globals.h to define tables needed by init_obarray.  */
#define DEFINE_SYMBOLS

#include <config.h>
#include "sysstdio.h"
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/file.h>
#include <errno.h>
#include <math.h>
#include <stat-time.h>
#include "lisp.h"
#include "dispextern.h"
#include "intervals.h"
#include "character.h"
#include "buffer.h"
#include "charset.h"
#include <epaths.h>
#include "commands.h"
#include "keyboard.h"
#include "systime.h"
#include "termhooks.h"
#include "blockinput.h"
#include "pdumper.h"
#include <c-ctype.h>
#include <vla.h>

#ifdef MSDOS
#include "msdos.h"
#endif

#ifdef HAVE_NS
#include "nsterm.h"
#endif

#include <unistd.h>

#ifdef HAVE_SETLOCALE
#include <locale.h>
#endif /* HAVE_SETLOCALE */

#include <fcntl.h>

#ifdef HAVE_FSEEKO
#define file_offset off_t
#define file_tell ftello
#else
#define file_offset long
#define file_tell ftell
#endif

#if IEEE_FLOATING_POINT
# include <ieee754.h>
# ifndef INFINITY
#  define INFINITY ((union ieee754_double) {.ieee = {.exponent = -1}}.d)
# endif
#endif

/* The objects or placeholders read with the #n=object form.

   A hash table maps a number to either a placeholder (while the
   object is still being parsed, in case it's referenced within its
   own definition) or to the completed object.  With small integers
   for keys, it's effectively little more than a vector, but it'll
   manage any needed resizing for us.

   The variable must be reset to an empty hash table before all
   top-level calls to read0.  In between calls, it may be an empty
   hash table left unused from the previous call (to reduce
   allocations), or nil.  */
static Lisp_Object read_objects_map;

/* The recursive objects read with the #n=object form.

   Objects that might have circular references are stored here, so
   that recursive substitution knows not to keep processing them
   multiple times.

   Only objects that are completely processed, including substituting
   references to themselves (but not necessarily replacing
   placeholders for other objects still being read), are stored.

   A hash table is used for efficient lookups of keys.  We don't care
   what the value slots hold.  The variable must be set to an empty
   hash table before all top-level calls to read0.  In between calls,
   it may be an empty hash table left unused from the previous call
   (to reduce allocations), or nil.  */
static Lisp_Object read_objects_completed;

/* File and lookahead for get-file-char and get-emacs-mule-file-char
   to read from.  Used by Fload.  */
static struct infile
{
  /* The input stream.  */
  FILE *stream;

  /* Lookahead byte count.  */
  signed char lookahead;

  /* Lookahead bytes, in reverse order.  Keep these here because it is
     not portable to ungetc more than one byte at a time.  */
  unsigned char buf[MAX_MULTIBYTE_LENGTH - 1];
} *infile;

/* For use within read-from-string (this reader is non-reentrant!!)  */
static ptrdiff_t read_from_string_index;
static ptrdiff_t read_from_string_index_byte;
static ptrdiff_t read_from_string_limit;

/* Number of characters read in the current call to Fread or
   Fread_from_string.  */
static EMACS_INT readchar_count;

/* This contains the last string skipped with #@.  */
static char *saved_doc_string;
/* Length of buffer allocated in saved_doc_string.  */
static ptrdiff_t saved_doc_string_size;
/* Length of actual data in saved_doc_string.  */
static ptrdiff_t saved_doc_string_length;
/* This is the file position that string came from.  */
static file_offset saved_doc_string_position;

/* This contains the previous string skipped with #@.
   We copy it from saved_doc_string when a new string
   is put in saved_doc_string.  */
static char *prev_saved_doc_string;
/* Length of buffer allocated in prev_saved_doc_string.  */
static ptrdiff_t prev_saved_doc_string_size;
/* Length of actual data in prev_saved_doc_string.  */
static ptrdiff_t prev_saved_doc_string_length;
/* This is the file position that string came from.  */
static file_offset prev_saved_doc_string_position;

/* True means inside a new-style backquote with no surrounding
   parentheses.  Fread initializes this to the value of
   `force_new_style_backquotes', so we need not specbind it or worry
   about what happens to it when there is an error.  */
static bool new_backquote_flag;

/* A list of file names for files being loaded in Fload.  Used to
   check for recursive loads.  */

static Lisp_Object Vloads_in_progress;

static int read_emacs_mule_char (int, int (*) (int, Lisp_Object),
                                 Lisp_Object);

static void readevalloop (Lisp_Object, struct infile *, Lisp_Object, bool,
                          Lisp_Object, Lisp_Object,
                          Lisp_Object, Lisp_Object);

static void build_load_history (Lisp_Object, bool);

/* Functions that read one byte from the current source READCHARFUN
   or unreads one byte.  If the integer argument C is -1, it returns
   one read byte, or -1 when there's no more byte in the source.  If C
   is 0 or positive, it unreads C, and the return value is not
   interesting.  */

static int readbyte_for_lambda (int, Lisp_Object);
static int readbyte_from_file (int, Lisp_Object);
static int readbyte_from_string (int, Lisp_Object);

/* Handle unreading and rereading of characters.
   Write READCHAR to read a character,
   UNREAD(c) to unread c to be read again.

   These macros correctly read/unread multibyte characters.  */

#define READCHAR readchar (readcharfun, NULL)
#define UNREAD(c) unreadchar (readcharfun, c)

/* Same as READCHAR but set *MULTIBYTE to the multibyteness of the source.  */
#define READCHAR_REPORT_MULTIBYTE(multibyte) readchar (readcharfun, multibyte)

/* When READCHARFUN is Qget_file_char, Qget_emacs_mule_file_char,
   Qlambda, or a cons, we use this to keep an unread character because
   a file stream can't handle multibyte-char unreading.  The value -1
   means that there's no unread character.  */
static int unread_char;

static int
readchar (Lisp_Object readcharfun, bool *multibyte)
{
  Lisp_Object tem;
  register int c;
  int (*readbyte) (int, Lisp_Object);
  unsigned char buf[MAX_MULTIBYTE_LENGTH];
  int i, len;
  bool emacs_mule_encoding = 0;

  if (multibyte)
    *multibyte = 0;

  readchar_count++;

  if (BUFFERP (readcharfun))
    {
      register struct buffer *inbuffer = XBUFFER (readcharfun);

      ptrdiff_t pt_byte = BUF_PT_BYTE (inbuffer);

      if (! BUFFER_LIVE_P (inbuffer))
	return -1;

      if (pt_byte >= BUF_ZV_BYTE (inbuffer))
	return -1;

      if (! NILP (BVAR (inbuffer, enable_multibyte_characters)))
	{
	  /* Fetch the character code from the buffer.  */
	  unsigned char *p = BUF_BYTE_ADDRESS (inbuffer, pt_byte);
	  BUF_INC_POS (inbuffer, pt_byte);
	  c = STRING_CHAR (p);
	  if (multibyte)
	    *multibyte = 1;
	}
      else
	{
	  c = BUF_FETCH_BYTE (inbuffer, pt_byte);
	  if (! ASCII_CHAR_P (c))
	    c = BYTE8_TO_CHAR (c);
	  pt_byte++;
	}
      SET_BUF_PT_BOTH (inbuffer, BUF_PT (inbuffer) + 1, pt_byte);

      return c;
    }
  if (MARKERP (readcharfun))
    {
      register struct buffer *inbuffer = XMARKER (readcharfun)->buffer;

      ptrdiff_t bytepos = marker_byte_position (readcharfun);

      if (bytepos >= BUF_ZV_BYTE (inbuffer))
	return -1;

      if (! NILP (BVAR (inbuffer, enable_multibyte_characters)))
	{
	  /* Fetch the character code from the buffer.  */
	  unsigned char *p = BUF_BYTE_ADDRESS (inbuffer, bytepos);
	  BUF_INC_POS (inbuffer, bytepos);
	  c = STRING_CHAR (p);
	  if (multibyte)
	    *multibyte = 1;
	}
      else
	{
	  c = BUF_FETCH_BYTE (inbuffer, bytepos);
	  if (! ASCII_CHAR_P (c))
	    c = BYTE8_TO_CHAR (c);
	  bytepos++;
	}

      XMARKER (readcharfun)->bytepos = bytepos;
      XMARKER (readcharfun)->charpos++;

      return c;
    }

  if (EQ (readcharfun, Qlambda))
    {
      readbyte = readbyte_for_lambda;
      goto read_multibyte;
    }

  if (EQ (readcharfun, Qget_file_char))
    {
      eassert (infile);
      readbyte = readbyte_from_file;
      goto read_multibyte;
    }

  if (STRINGP (readcharfun))
    {
      if (read_from_string_index >= read_from_string_limit)
	c = -1;
      else if (STRING_MULTIBYTE (readcharfun))
	{
	  if (multibyte)
	    *multibyte = 1;
	  FETCH_STRING_CHAR_ADVANCE_NO_CHECK (c, readcharfun,
					      read_from_string_index,
					      read_from_string_index_byte);
	}
      else
	{
	  c = SREF (readcharfun, read_from_string_index_byte);
	  read_from_string_index++;
	  read_from_string_index_byte++;
	}
      return c;
    }

  if (CONSP (readcharfun) && STRINGP (XCAR (readcharfun)))
    {
      /* This is the case that read_vector is reading from a unibyte
	 string that contains a byte sequence previously skipped
	 because of #@NUMBER.  The car part of readcharfun is that
	 string, and the cdr part is a value of readcharfun given to
	 read_vector.  */
      readbyte = readbyte_from_string;
      eassert (infile);
      if (EQ (XCDR (readcharfun), Qget_emacs_mule_file_char))
	emacs_mule_encoding = 1;
      goto read_multibyte;
    }

  if (EQ (readcharfun, Qget_emacs_mule_file_char))
    {
      readbyte = readbyte_from_file;
      eassert (infile);
      emacs_mule_encoding = 1;
      goto read_multibyte;
    }

  tem = call0 (readcharfun);

  if (NILP (tem))
    return -1;
  return XFIXNUM (tem);

 read_multibyte:
  if (unread_char >= 0)
    {
      c = unread_char;
      unread_char = -1;
      return c;
    }
  c = (*readbyte) (-1, readcharfun);
  if (c < 0)
    return c;
  if (multibyte)
    *multibyte = 1;
  if (ASCII_CHAR_P (c))
    return c;
  if (emacs_mule_encoding)
    return read_emacs_mule_char (c, readbyte, readcharfun);
  i = 0;
  buf[i++] = c;
  len = BYTES_BY_CHAR_HEAD (c);
  while (i < len)
    {
      buf[i++] = c = (*readbyte) (-1, readcharfun);
      if (c < 0 || ! TRAILING_CODE_P (c))
	{
	  for (i -= c < 0; 0 < --i; )
	    (*readbyte) (buf[i], readcharfun);
	  return BYTE8_TO_CHAR (buf[0]);
	}
    }
  return STRING_CHAR (buf);
}

#define FROM_FILE_P(readcharfun)			\
  (EQ (readcharfun, Qget_file_char)			\
   || EQ (readcharfun, Qget_emacs_mule_file_char))

static void
skip_dyn_bytes (Lisp_Object readcharfun, ptrdiff_t n)
{
  if (FROM_FILE_P (readcharfun))
    {
      block_input ();		/* FIXME: Not sure if it's needed.  */
      fseek (infile->stream, n - infile->lookahead, SEEK_CUR);
      unblock_input ();
      infile->lookahead = 0;
    }
  else
    { /* We're not reading directly from a file.  In that case, it's difficult
	 to reliably count bytes, since these are usually meant for the file's
	 encoding, whereas we're now typically in the internal encoding.
	 But luckily, skip_dyn_bytes is used to skip over a single
	 dynamic-docstring (or dynamic byte-code) which is always quoted such
	 that \037 is the final char.  */
      int c;
      do {
	c = READCHAR;
      } while (c >= 0 && c != '\037');
    }
}

static void
skip_dyn_eof (Lisp_Object readcharfun)
{
  if (FROM_FILE_P (readcharfun))
    {
      block_input ();		/* FIXME: Not sure if it's needed.  */
      fseek (infile->stream, 0, SEEK_END);
      unblock_input ();
      infile->lookahead = 0;
    }
  else
    while (READCHAR >= 0);
}

/* Unread the character C in the way appropriate for the stream READCHARFUN.
   If the stream is a user function, call it with the char as argument.  */

static void
unreadchar (Lisp_Object readcharfun, int c)
{
  readchar_count--;
  if (c == -1)
    /* Don't back up the pointer if we're unreading the end-of-input mark,
       since readchar didn't advance it when we read it.  */
    ;
  else if (BUFFERP (readcharfun))
    {
      struct buffer *b = XBUFFER (readcharfun);
      ptrdiff_t charpos = BUF_PT (b);
      ptrdiff_t bytepos = BUF_PT_BYTE (b);

      if (! NILP (BVAR (b, enable_multibyte_characters)))
	BUF_DEC_POS (b, bytepos);
      else
	bytepos--;

      SET_BUF_PT_BOTH (b, charpos - 1, bytepos);
    }
  else if (MARKERP (readcharfun))
    {
      struct buffer *b = XMARKER (readcharfun)->buffer;
      ptrdiff_t bytepos = XMARKER (readcharfun)->bytepos;

      XMARKER (readcharfun)->charpos--;
      if (! NILP (BVAR (b, enable_multibyte_characters)))
	BUF_DEC_POS (b, bytepos);
      else
	bytepos--;

      XMARKER (readcharfun)->bytepos = bytepos;
    }
  else if (STRINGP (readcharfun))
    {
      read_from_string_index--;
      read_from_string_index_byte
	= string_char_to_byte (readcharfun, read_from_string_index);
    }
  else if (CONSP (readcharfun) && STRINGP (XCAR (readcharfun)))
    {
      unread_char = c;
    }
  else if (EQ (readcharfun, Qlambda))
    {
      unread_char = c;
    }
  else if (FROM_FILE_P (readcharfun))
    {
      unread_char = c;
    }
  else
    call1 (readcharfun, make_fixnum (c));
}

static int
readbyte_for_lambda (int c, Lisp_Object readcharfun)
{
  return read_bytecode_char (c >= 0);
}


static int
readbyte_from_stdio (void)
{
  if (infile->lookahead)
    return infile->buf[--infile->lookahead];

  int c;
  FILE *instream = infile->stream;

  block_input ();

  /* Interrupted reads have been observed while reading over the network.  */
  while ((c = getc (instream)) == EOF && errno == EINTR && ferror (instream))
    {
      unblock_input ();
      maybe_quit ();
      block_input ();
      clearerr (instream);
    }

  unblock_input ();

  return (c == EOF ? -1 : c);
}

static int
readbyte_from_file (int c, Lisp_Object readcharfun)
{
  eassert (infile);
  if (c >= 0)
    {
      eassert (infile->lookahead < sizeof infile->buf);
      infile->buf[infile->lookahead++] = c;
      return 0;
    }

  return readbyte_from_stdio ();
}

static int
readbyte_from_string (int c, Lisp_Object readcharfun)
{
  Lisp_Object string = XCAR (readcharfun);

  if (c >= 0)
    {
      read_from_string_index--;
      read_from_string_index_byte
	= string_char_to_byte (string, read_from_string_index);
    }

  if (read_from_string_index >= read_from_string_limit)
    c = -1;
  else
    FETCH_STRING_CHAR_ADVANCE (c, string,
			       read_from_string_index,
			       read_from_string_index_byte);
  return c;
}


/* Read one non-ASCII character from INFILE.  The character is
   encoded in `emacs-mule' and the first byte is already read in
   C.  */

static int
read_emacs_mule_char (int c, int (*readbyte) (int, Lisp_Object), Lisp_Object readcharfun)
{
  /* Emacs-mule coding uses at most 4-byte for one character.  */
  unsigned char buf[4];
  int len = emacs_mule_bytes[c];
  struct charset *charset;
  int i;
  unsigned code;

  if (len == 1)
    /* C is not a valid leading-code of `emacs-mule'.  */
    return BYTE8_TO_CHAR (c);

  i = 0;
  buf[i++] = c;
  while (i < len)
    {
      buf[i++] = c = (*readbyte) (-1, readcharfun);
      if (c < 0xA0)
	{
	  for (i -= c < 0; 0 < --i; )
	    (*readbyte) (buf[i], readcharfun);
	  return BYTE8_TO_CHAR (buf[0]);
	}
    }

  if (len == 2)
    {
      charset = CHARSET_FROM_ID (emacs_mule_charset[buf[0]]);
      code = buf[1] & 0x7F;
    }
  else if (len == 3)
    {
      if (buf[0] == EMACS_MULE_LEADING_CODE_PRIVATE_11
	  || buf[0] == EMACS_MULE_LEADING_CODE_PRIVATE_12)
	{
	  charset = CHARSET_FROM_ID (emacs_mule_charset[buf[1]]);
	  code = buf[2] & 0x7F;
	}
      else
	{
	  charset = CHARSET_FROM_ID (emacs_mule_charset[buf[0]]);
	  code = ((buf[1] << 8) | buf[2]) & 0x7F7F;
	}
    }
  else
    {
      charset = CHARSET_FROM_ID (emacs_mule_charset[buf[1]]);
      code = ((buf[2] << 8) | buf[3]) & 0x7F7F;
    }
  c = DECODE_CHAR (charset, code);
  if (c < 0)
    Fsignal (Qinvalid_read_syntax,
	     list1 (build_string ("invalid multibyte form")));
  return c;
}


/* An in-progress substitution of OBJECT for PLACEHOLDER.  */
struct subst
{
  Lisp_Object object;
  Lisp_Object placeholder;

  /* Hash table of subobjects of OBJECT that might be circular.  If
     Qt, all such objects might be circular.  */
  Lisp_Object completed;

  /* List of subobjects of OBJECT that have already been visited.  */
  Lisp_Object seen;
};

static Lisp_Object read_internal_start (Lisp_Object, Lisp_Object,
                                        Lisp_Object);
static Lisp_Object read0 (Lisp_Object);
static Lisp_Object read1 (Lisp_Object, int *, bool);

static Lisp_Object read_list (bool, Lisp_Object);
static Lisp_Object read_vector (Lisp_Object, bool);

static Lisp_Object substitute_object_recurse (struct subst *, Lisp_Object);
static void substitute_in_interval (INTERVAL, void *);


/* Get a character from the tty.  */

/* Read input events until we get one that's acceptable for our purposes.

   If NO_SWITCH_FRAME, switch-frame events are stashed
   until we get a character we like, and then stuffed into
   unread_switch_frame.

   If ASCII_REQUIRED, check function key events to see
   if the unmodified version of the symbol has a Qascii_character
   property, and use that character, if present.

   If ERROR_NONASCII, signal an error if the input we
   get isn't an ASCII character with modifiers.  If it's false but
   ASCII_REQUIRED is true, just re-read until we get an ASCII
   character.

   If INPUT_METHOD, invoke the current input method
   if the character warrants that.

   If SECONDS is a number, wait that many seconds for input, and
   return Qnil if no input arrives within that time.  */

static Lisp_Object
read_filtered_event (bool no_switch_frame, bool ascii_required,
		     bool error_nonascii, bool input_method, Lisp_Object seconds)
{
  Lisp_Object val, delayed_switch_frame;
  struct timespec end_time;

#ifdef HAVE_WINDOW_SYSTEM
  if (display_hourglass_p)
    cancel_hourglass ();
#endif

  delayed_switch_frame = Qnil;

  /* Compute timeout.  */
  if (NUMBERP (seconds))
    {
      double duration = XFLOATINT (seconds);
      struct timespec wait_time = dtotimespec (duration);
      end_time = timespec_add (current_timespec (), wait_time);
    }

  /* Read until we get an acceptable event.  */
 retry:
  do
    val = read_char (0, Qnil, (input_method ? Qnil : Qt), 0,
		     NUMBERP (seconds) ? &end_time : NULL);
  while (FIXNUMP (val) && XFIXNUM (val) == -2); /* wrong_kboard_jmpbuf */

  if (BUFFERP (val))
    goto retry;

  /* `switch-frame' events are put off until after the next ASCII
     character.  This is better than signaling an error just because
     the last characters were typed to a separate minibuffer frame,
     for example.  Eventually, some code which can deal with
     switch-frame events will read it and process it.  */
  if (no_switch_frame
      && EVENT_HAS_PARAMETERS (val)
      && EQ (EVENT_HEAD_KIND (EVENT_HEAD (val)), Qswitch_frame))
    {
      delayed_switch_frame = val;
      goto retry;
    }

  if (ascii_required && !(NUMBERP (seconds) && NILP (val)))
    {
      /* Convert certain symbols to their ASCII equivalents.  */
      if (SYMBOLP (val))
	{
	  Lisp_Object tem, tem1;
	  tem = Fget (val, Qevent_symbol_element_mask);
	  if (!NILP (tem))
	    {
	      tem1 = Fget (Fcar (tem), Qascii_character);
	      /* Merge this symbol's modifier bits
		 with the ASCII equivalent of its basic code.  */
	      if (!NILP (tem1))
		XSETFASTINT (val, XFIXNUM (tem1) | XFIXNUM (Fcar (Fcdr (tem))));
	    }
	}

      /* If we don't have a character now, deal with it appropriately.  */
      if (!FIXNUMP (val))
	{
	  if (error_nonascii)
	    {
	      Vunread_command_events = list1 (val);
	      error ("Non-character input-event");
	    }
	  else
	    goto retry;
	}
    }

  if (! NILP (delayed_switch_frame))
    unread_switch_frame = delayed_switch_frame;

#if 0

#ifdef HAVE_WINDOW_SYSTEM
  if (display_hourglass_p)
    start_hourglass ();
#endif

#endif

  return val;
}

DEFUN ("read-char", Fread_char, Sread_char, 0, 3, 0,
       doc: /* Read a character event from the command input (keyboard or macro).
It is returned as a number.
If the event has modifiers, they are resolved and reflected in the
returned character code if possible (e.g. C-SPC yields 0 and C-a yields 97).
If some of the modifiers cannot be reflected in the character code, the
returned value will include those modifiers, and will not be a valid
character code: it will fail the `characterp' test.  Use `event-basic-type'
to recover the character code with the modifiers removed.

If the user generates an event which is not a character (i.e. a mouse
click or function key event), `read-char' signals an error.  As an
exception, switch-frame events are put off until non-character events
can be read.
If you want to read non-character events, or ignore them, call
`read-event' or `read-char-exclusive' instead.

If the optional argument PROMPT is non-nil, display that as a prompt.
If PROMPT is nil or the string \"\", the key sequence/events that led
to the current command is used as the prompt.

If the optional argument INHERIT-INPUT-METHOD is non-nil and some
input method is turned on in the current buffer, that input method
is used for reading a character.

If the optional argument SECONDS is non-nil, it should be a number
specifying the maximum number of seconds to wait for input.  If no
input arrives in that time, return nil.  SECONDS may be a
floating-point value.  */)
  (Lisp_Object prompt, Lisp_Object inherit_input_method, Lisp_Object seconds)
{
  Lisp_Object val;

  if (! NILP (prompt))
    message_with_string ("%s", prompt, 0);
  val = read_filtered_event (1, 1, 1, ! NILP (inherit_input_method), seconds);

  return (NILP (val) ? Qnil
	  : make_fixnum (char_resolve_modifier_mask (XFIXNUM (val))));
}

DEFUN ("read-event", Fread_event, Sread_event, 0, 3, 0,
       doc: /* Read an event object from the input stream.
If the optional argument PROMPT is non-nil, display that as a prompt.
If PROMPT is nil or the string \"\", the key sequence/events that led
to the current command is used as the prompt.

If the optional argument INHERIT-INPUT-METHOD is non-nil and some
input method is turned on in the current buffer, that input method
is used for reading a character.

If the optional argument SECONDS is non-nil, it should be a number
specifying the maximum number of seconds to wait for input.  If no
input arrives in that time, return nil.  SECONDS may be a
floating-point value.  */)
  (Lisp_Object prompt, Lisp_Object inherit_input_method, Lisp_Object seconds)
{
  if (! NILP (prompt))
    message_with_string ("%s", prompt, 0);
  return read_filtered_event (0, 0, 0, ! NILP (inherit_input_method), seconds);
}

DEFUN ("read-char-exclusive", Fread_char_exclusive, Sread_char_exclusive, 0, 3, 0,
       doc: /* Read a character event from the command input (keyboard or macro).
It is returned as a number.  Non-character events are ignored.
If the event has modifiers, they are resolved and reflected in the
returned character code if possible (e.g. C-SPC yields 0 and C-a yields 97).
If some of the modifiers cannot be reflected in the character code, the
returned value will include those modifiers, and will not be a valid
character code: it will fail the `characterp' test.  Use `event-basic-type'
to recover the character code with the modifiers removed.

If the optional argument PROMPT is non-nil, display that as a prompt.
If PROMPT is nil or the string \"\", the key sequence/events that led
to the current command is used as the prompt.

If the optional argument INHERIT-INPUT-METHOD is non-nil and some
input method is turned on in the current buffer, that input method
is used for reading a character.

If the optional argument SECONDS is non-nil, it should be a number
specifying the maximum number of seconds to wait for input.  If no
input arrives in that time, return nil.  SECONDS may be a
floating-point value.  */)
  (Lisp_Object prompt, Lisp_Object inherit_input_method, Lisp_Object seconds)
{
  Lisp_Object val;

  if (! NILP (prompt))
    message_with_string ("%s", prompt, 0);

  val = read_filtered_event (1, 1, 0, ! NILP (inherit_input_method), seconds);

  return (NILP (val) ? Qnil
	  : make_fixnum (char_resolve_modifier_mask (XFIXNUM (val))));
}

DEFUN ("get-file-char", Fget_file_char, Sget_file_char, 0, 0, 0,
       doc: /* Don't use this yourself.  */)
  (void)
{
  if (!infile)
    error ("get-file-char misused");
  return make_fixnum (readbyte_from_stdio ());
}




/* Return true if the lisp code read using READCHARFUN defines a non-nil
   `lexical-binding' file variable.  After returning, the stream is
   positioned following the first line, if it is a comment or #! line,
   otherwise nothing is read.  */

static bool
lisp_file_lexically_bound_p (Lisp_Object readcharfun)
{
  int ch = READCHAR;

  if (ch == '#')
    {
      ch = READCHAR;
      if (ch != '!')
        {
          UNREAD (ch);
          UNREAD ('#');
          return 0;
        }
      while (ch != '\n' && ch != EOF)
        ch = READCHAR;
      if (ch == '\n') ch = READCHAR;
      /* It is OK to leave the position after a #! line, since
         that is what read1 does.  */
    }

  if (ch != ';')
    /* The first line isn't a comment, just give up.  */
    {
      UNREAD (ch);
      return 0;
    }
  else
    /* Look for an appropriate file-variable in the first line.  */
    {
      bool rv = 0;
      enum {
	NOMINAL, AFTER_FIRST_DASH, AFTER_ASTERIX
      } beg_end_state = NOMINAL;
      bool in_file_vars = 0;

#define UPDATE_BEG_END_STATE(ch)				\
  if (beg_end_state == NOMINAL)					\
    beg_end_state = (ch == '-' ? AFTER_FIRST_DASH : NOMINAL);	\
  else if (beg_end_state == AFTER_FIRST_DASH)			\
    beg_end_state = (ch == '*' ? AFTER_ASTERIX : NOMINAL);	\
  else if (beg_end_state == AFTER_ASTERIX)			\
    {								\
      if (ch == '-')						\
	in_file_vars = !in_file_vars;				\
      beg_end_state = NOMINAL;					\
    }

      /* Skip until we get to the file vars, if any.  */
      do
	{
	  ch = READCHAR;
	  UPDATE_BEG_END_STATE (ch);
	}
      while (!in_file_vars && ch != '\n' && ch != EOF);

      while (in_file_vars)
	{
	  char var[100], val[100];
	  unsigned i;

	  ch = READCHAR;

	  /* Read a variable name.  */
	  while (ch == ' ' || ch == '\t')
	    ch = READCHAR;

	  i = 0;
	  beg_end_state = NOMINAL;
	  while (ch != ':' && ch != '\n' && ch != EOF && in_file_vars)
	    {
	      if (i < sizeof var - 1)
		var[i++] = ch;
	      UPDATE_BEG_END_STATE (ch);
	      ch = READCHAR;
	    }

	  /* Stop scanning if no colon was found before end marker.  */
	  if (!in_file_vars || ch == '\n' || ch == EOF)
	    break;

	  while (i > 0 && (var[i - 1] == ' ' || var[i - 1] == '\t'))
	    i--;
	  var[i] = '\0';

	  if (ch == ':')
	    {
	      /* Read a variable value.  */
	      ch = READCHAR;

	      while (ch == ' ' || ch == '\t')
		ch = READCHAR;

	      i = 0;
	      beg_end_state = NOMINAL;
	      while (ch != ';' && ch != '\n' && ch != EOF && in_file_vars)
		{
		  if (i < sizeof val - 1)
		    val[i++] = ch;
		  UPDATE_BEG_END_STATE (ch);
		  ch = READCHAR;
		}
	      if (! in_file_vars)
		/* The value was terminated by an end-marker, which remove.  */
		i -= 3;
	      while (i > 0 && (val[i - 1] == ' ' || val[i - 1] == '\t'))
		i--;
	      val[i] = '\0';

	      if (strcmp (var, "lexical-binding") == 0)
		/* This is it...  */
		{
		  rv = (strcmp (val, "nil") != 0);
		  break;
		}
	    }
	}

      while (ch != '\n' && ch != EOF)
	ch = READCHAR;

      return rv;
    }
}

/* Value is a version number of byte compiled code if the file
   associated with file descriptor FD is a compiled Lisp file that's
   safe to load.  Only files compiled with Emacs are safe to load.
   Files compiled with XEmacs can lead to a crash in Fbyte_code
   because of an incompatible change in the byte compiler.  */

static int
safe_to_load_version (int fd)
{
  char buf[512];
  int nbytes, i;
  int version = 1;

  /* Read the first few bytes from the file, and look for a line
     specifying the byte compiler version used.  */
  nbytes = emacs_read_quit (fd, buf, sizeof buf);
  if (nbytes > 0)
    {
      /* Skip to the next newline, skipping over the initial `ELC'
	 with NUL bytes following it, but note the version.  */
      for (i = 0; i < nbytes && buf[i] != '\n'; ++i)
	if (i == 4)
	  version = buf[i];

      if (i >= nbytes
	  || fast_c_string_match_ignore_case (Vbytecomp_version_regexp,
					      buf + i, nbytes - i) < 0)
	version = 0;
    }

  lseek (fd, 0, SEEK_SET);
  return version;
}


/* Callback for record_unwind_protect.  Restore the old load list OLD,
   after loading a file successfully.  */

static void
record_load_unwind (Lisp_Object old)
{
  Vloads_in_progress = old;
}

/* This handler function is used via internal_condition_case_1.  */

static Lisp_Object
load_error_handler (Lisp_Object data)
{
  return Qnil;
}

static AVOID
load_error_old_style_backquotes (void)
{
  if (NILP (Vload_file_name))
    xsignal1 (Qerror, build_string ("Old-style backquotes detected!"));
  else
    {
      AUTO_STRING (format, "Loading `%s': old-style backquotes detected!");
      xsignal1 (Qerror, CALLN (Fformat_message, format, Vload_file_name));
    }
}

static void
load_warn_unescaped_character_literals (Lisp_Object file)
{
  Lisp_Object warning = call0 (Qbyte_run_unescaped_character_literals_warning);
  if (!NILP (warning))
    {
      AUTO_STRING (format, "Loading `%s': %s");
      CALLN (Fmessage, format, file, warning);
    }
}

DEFUN ("get-load-suffixes", Fget_load_suffixes, Sget_load_suffixes, 0, 0, 0,
       doc: /* Return the suffixes that `load' should try if a suffix is \
required.
This uses the variables `load-suffixes' and `load-file-rep-suffixes'.  */)
  (void)
{
  Lisp_Object lst = Qnil, suffixes = Vload_suffixes;
  FOR_EACH_TAIL (suffixes)
    {
      Lisp_Object exts = Vload_file_rep_suffixes;
      Lisp_Object suffix = XCAR (suffixes);
      FOR_EACH_TAIL (exts)
	lst = Fcons (concat2 (suffix, XCAR (exts)), lst);
    }
  return Fnreverse (lst);
}

/* Return true if STRING ends with SUFFIX.  */
static bool
suffix_p (Lisp_Object string, const char *suffix)
{
  ptrdiff_t suffix_len = strlen (suffix);
  ptrdiff_t string_len = SBYTES (string);

  return (suffix_len <= string_len
	  && strcmp (SSDATA (string) + string_len - suffix_len, suffix) == 0);
}

static void
close_infile_unwind (void *arg)
{
  struct infile *prev_infile = arg;
  eassert (infile && infile != prev_infile);
  fclose (infile->stream);
  infile = prev_infile;
}

DEFUN ("load", Fload, Sload, 1, 5, 0,
       doc: /* Execute a file of Lisp code named FILE.
First try FILE with `.elc' appended, then try with `.el', then try
with a system-dependent suffix of dynamic modules (see `load-suffixes'),
then try FILE unmodified (the exact suffixes in the exact order are
determined by `load-suffixes').  Environment variable references in
FILE are replaced with their values by calling `substitute-in-file-name'.
This function searches the directories in `load-path'.

If optional second arg NOERROR is non-nil,
report no error if FILE doesn't exist.
Print messages at start and end of loading unless
optional third arg NOMESSAGE is non-nil (but `force-load-messages'
overrides that).
If optional fourth arg NOSUFFIX is non-nil, don't try adding
suffixes to the specified name FILE.
If optional fifth arg MUST-SUFFIX is non-nil, insist on
the suffix `.elc' or `.el' or the module suffix; don't accept just
FILE unless it ends in one of those suffixes or includes a directory name.

If NOSUFFIX is nil, then if a file could not be found, try looking for
a different representation of the file by adding non-empty suffixes to
its name, before trying another file.  Emacs uses this feature to find
compressed versions of files when Auto Compression mode is enabled.
If NOSUFFIX is non-nil, disable this feature.

The suffixes that this function tries out, when NOSUFFIX is nil, are
given by the return value of `get-load-suffixes' and the values listed
in `load-file-rep-suffixes'.  If MUST-SUFFIX is non-nil, only the
return value of `get-load-suffixes' is used, i.e. the file name is
required to have a non-empty suffix.

When searching suffixes, this function normally stops at the first
one that exists.  If the option `load-prefer-newer' is non-nil,
however, it tries all suffixes, and uses whichever file is the newest.

Loading a file records its definitions, and its `provide' and
`require' calls, in an element of `load-history' whose
car is the file name loaded.  See `load-history'.

While the file is in the process of being loaded, the variable
`load-in-progress' is non-nil and the variable `load-file-name'
is bound to the file's name.

Return t if the file exists and loads successfully.  */)
  (Lisp_Object file, Lisp_Object noerror, Lisp_Object nomessage,
   Lisp_Object nosuffix, Lisp_Object must_suffix)
{
  FILE *stream UNINIT;
  int fd;
  int fd_index UNINIT;
  ptrdiff_t count = SPECPDL_INDEX ();
  Lisp_Object found, efound, hist_file_name;
  /* True means we printed the ".el is newer" message.  */
  bool newer = 0;
  /* True means we are loading a compiled file.  */
  bool compiled = 0;
  Lisp_Object handler;
  bool safe_p = 1;
  const char *fmode = "r" FOPEN_TEXT;
  int version;

  CHECK_STRING (file);

  /* If file name is magic, call the handler.  */
  /* This shouldn't be necessary any more now that `openp' handles it right.
    handler = Ffind_file_name_handler (file, Qload);
    if (!NILP (handler))
      return call5 (handler, Qload, file, noerror, nomessage, nosuffix); */

  /* The presence of this call is the result of a historical accident:
     it used to be in every file-operation and when it got removed
     everywhere, it accidentally stayed here.  Since then, enough people
     supposedly have things like (load "$PROJECT/foo.el") in their .emacs
     that it seemed risky to remove.  */
  if (! NILP (noerror))
    {
      file = internal_condition_case_1 (Fsubstitute_in_file_name, file,
					Qt, load_error_handler);
      if (NILP (file))
	return Qnil;
    }
  else
    file = Fsubstitute_in_file_name (file);

  /* Avoid weird lossage with null string as arg,
     since it would try to load a directory as a Lisp file.  */
  if (SCHARS (file) == 0)
    {
      fd = -1;
      errno = ENOENT;
    }
  else
    {
      Lisp_Object suffixes;
      found = Qnil;

      if (! NILP (must_suffix))
	{
	  /* Don't insist on adding a suffix if FILE already ends with one.  */
	  if (suffix_p (file, ".el")
	      || suffix_p (file, ".elc")
#ifdef HAVE_MODULES
	      || suffix_p (file, MODULES_SUFFIX)
#endif
	      )
	    must_suffix = Qnil;
	  /* Don't insist on adding a suffix
	     if the argument includes a directory name.  */
	  else if (! NILP (Ffile_name_directory (file)))
	    must_suffix = Qnil;
	}

      if (!NILP (nosuffix))
	suffixes = Qnil;
      else
	{
	  suffixes = Fget_load_suffixes ();
	  if (NILP (must_suffix))
	    suffixes = CALLN (Fappend, suffixes, Vload_file_rep_suffixes);
	}

      fd = openp (Vload_path, file, suffixes, &found, Qnil, load_prefer_newer);
    }

  if (fd == -1)
    {
      if (NILP (noerror))
	report_file_error ("Cannot open load file", file);
      return Qnil;
    }

  /* Tell startup.el whether or not we found the user's init file.  */
  if (EQ (Qt, Vuser_init_file))
    Vuser_init_file = found;

  /* If FD is -2, that means openp found a magic file.  */
  if (fd == -2)
    {
      if (NILP (Fequal (found, file)))
	/* If FOUND is a different file name from FILE,
	   find its handler even if we have already inhibited
	   the `load' operation on FILE.  */
	handler = Ffind_file_name_handler (found, Qt);
      else
	handler = Ffind_file_name_handler (found, Qload);
      if (! NILP (handler))
	return call5 (handler, Qload, found, noerror, nomessage, Qt);
#ifdef DOS_NT
      /* Tramp has to deal with semi-broken packages that prepend
	 drive letters to remote files.  For that reason, Tramp
	 catches file operations that test for file existence, which
	 makes openp think X:/foo.elc files are remote.  However,
	 Tramp does not catch `load' operations for such files, so we
	 end up with a nil as the `load' handler above.  If we would
	 continue with fd = -2, we will behave wrongly, and in
	 particular try reading a .elc file in the "rt" mode instead
	 of "rb".  See bug #9311 for the results.  To work around
	 this, we try to open the file locally, and go with that if it
	 succeeds.  */
      fd = emacs_open (SSDATA (ENCODE_FILE (found)), O_RDONLY, 0);
      if (fd == -1)
	fd = -2;
#endif
    }

  if (0 <= fd)
    {
      fd_index = SPECPDL_INDEX ();
      record_unwind_protect_int (close_file_unwind, fd);
    }

#ifdef HAVE_MODULES
  bool is_module = suffix_p (found, MODULES_SUFFIX);
#else
  bool is_module = false;
#endif

  /* Check if we're stuck in a recursive load cycle.

     2000-09-21: It's not possible to just check for the file loaded
     being a member of Vloads_in_progress.  This fails because of the
     way the byte compiler currently works; `provide's are not
     evaluated, see font-lock.el/jit-lock.el as an example.  This
     leads to a certain amount of ``normal'' recursion.

     Also, just loading a file recursively is not always an error in
     the general case; the second load may do something different.  */
  {
    int load_count = 0;
    Lisp_Object tem = Vloads_in_progress;
    FOR_EACH_TAIL_SAFE (tem)
      if (!NILP (Fequal (found, XCAR (tem))) && (++load_count > 3))
	signal_error ("Recursive load", Fcons (found, Vloads_in_progress));
    record_unwind_protect (record_load_unwind, Vloads_in_progress);
    Vloads_in_progress = Fcons (found, Vloads_in_progress);
  }

  /* All loads are by default dynamic, unless the file itself specifies
     otherwise using a file-variable in the first line.  This is bound here
     so that it takes effect whether or not we use
     Vload_source_file_function.  */
  specbind (Qlexical_binding, Qnil);

  /* Get the name for load-history.  */
  hist_file_name = (! NILP (Vpurify_flag)
                    ? concat2 (Ffile_name_directory (file),
                               Ffile_name_nondirectory (found))
                    : found) ;

  version = -1;

  /* Check for the presence of unescaped character literals and warn
     about them. */
  specbind (Qlread_unescaped_character_literals, Qnil);
  record_unwind_protect (load_warn_unescaped_character_literals, file);

  bool is_elc = suffix_p (found, ".elc");
  if (is_elc
      /* version = 1 means the file is empty, in which case we can
	 treat it as not byte-compiled.  */
      || (fd >= 0 && (version = safe_to_load_version (fd)) > 1))
    /* Load .elc files directly, but not when they are
       remote and have no handler!  */
    {
      if (fd != -2)
	{
	  struct stat s1, s2;
	  int result;

	  if (version < 0
	      && ! (version = safe_to_load_version (fd)))
	    {
	      safe_p = 0;
	      if (!load_dangerous_libraries)
		error ("File `%s' was not compiled in Emacs", SDATA (found));
	      else if (!NILP (nomessage) && !force_load_messages)
		message_with_string ("File `%s' not compiled in Emacs", found, 1);
	    }

	  compiled = 1;

	  efound = ENCODE_FILE (found);
	  fmode = "r" FOPEN_BINARY;

          /* openp already checked for newness, no point doing it again.
             FIXME would be nice to get a message when openp
             ignores suffix order due to load_prefer_newer.  */
          if (!load_prefer_newer && is_elc)
            {
              result = stat (SSDATA (efound), &s1);
              if (result == 0)
                {
                  SSET (efound, SBYTES (efound) - 1, 0);
                  result = stat (SSDATA (efound), &s2);
                  SSET (efound, SBYTES (efound) - 1, 'c');
                }

              if (result == 0
                  && timespec_cmp (get_stat_mtime (&s1), get_stat_mtime (&s2)) < 0)
                {
                  /* Make the progress messages mention that source is newer.  */
                  newer = 1;

                  /* If we won't print another message, mention this anyway.  */
                  if (!NILP (nomessage) && !force_load_messages)
                    {
                      Lisp_Object msg_file;
                      msg_file = Fsubstring (found, make_fixnum (0), make_fixnum (-1));
                      message_with_string ("Source file `%s' newer than byte-compiled file; using older file",
                                           msg_file, 1);
                    }
                }
            } /* !load_prefer_newer */
	}
    }
  else if (!is_module)
    {
      /* We are loading a source file (*.el).  */
      if (!NILP (Vload_source_file_function))
	{
	  Lisp_Object val;

	  if (fd >= 0)
	    {
	      emacs_close (fd);
	      clear_unwind_protect (fd_index);
	    }
	  val = call4 (Vload_source_file_function, found, hist_file_name,
		       NILP (noerror) ? Qnil : Qt,
		       (NILP (nomessage) || force_load_messages) ? Qnil : Qt);
	  return unbind_to (count, val);
	}
    }

  if (fd < 0)
    {
      /* We somehow got here with fd == -2, meaning the file is deemed
	 to be remote.  Don't even try to reopen the file locally;
	 just force a failure.  */
      stream = NULL;
      errno = EINVAL;
    }
  else if (!is_module)
    {
#ifdef WINDOWSNT
      emacs_close (fd);
      clear_unwind_protect (fd_index);
      efound = ENCODE_FILE (found);
      stream = emacs_fopen (SSDATA (efound), fmode);
#else
      stream = fdopen (fd, fmode);
#endif
    }

  /* Declare here rather than inside the else-part because the storage
     might be accessed by the unbind_to call below.  */
  struct infile input;

  if (is_module)
    {
      /* `module-load' uses the file name, so we can close the stream
         now.  */
      if (fd >= 0)
        {
          emacs_close (fd);
          clear_unwind_protect (fd_index);
        }
    }
  else
    {
      if (! stream)
        report_file_error ("Opening stdio stream", file);
      set_unwind_protect_ptr (fd_index, close_infile_unwind, infile);
      input.stream = stream;
      input.lookahead = 0;
      infile = &input;
    }

  if (! NILP (Vpurify_flag))
    Vpreloaded_file_list = Fcons (Fpurecopy (file), Vpreloaded_file_list);

  if (NILP (nomessage) || force_load_messages)
    {
      if (!safe_p)
	message_with_string ("Loading %s (compiled; note unsafe, not compiled in Emacs)...",
		 file, 1);
      else if (is_module)
        message_with_string ("Loading %s (module)...", file, 1);
      else if (!compiled)
	message_with_string ("Loading %s (source)...", file, 1);
      else if (newer)
	message_with_string ("Loading %s (compiled; note, source file is newer)...",
		 file, 1);
      else /* The typical case; compiled file newer than source file.  */
	message_with_string ("Loading %s...", file, 1);
    }

  specbind (Qload_file_name, found);
  specbind (Qinhibit_file_name_operation, Qnil);
  specbind (Qload_in_progress, Qt);

  if (is_module)
    {
#ifdef HAVE_MODULES
      specbind (Qcurrent_load_list, Qnil);
      LOADHIST_ATTACH (found);
      Fmodule_load (found);
      build_load_history (found, true);
#else
      /* This cannot happen.  */
      emacs_abort ();
#endif
    }
  else
    {
      if (lisp_file_lexically_bound_p (Qget_file_char))
        Fset (Qlexical_binding, Qt);

      if (! version || version >= 22)
        readevalloop (Qget_file_char, &input, hist_file_name,
                      0, Qnil, Qnil, Qnil, Qnil);
      else
        {
          /* We can't handle a file which was compiled with
             byte-compile-dynamic by older version of Emacs.  */
          specbind (Qload_force_doc_strings, Qt);
          readevalloop (Qget_emacs_mule_file_char, &input, hist_file_name,
                        0, Qnil, Qnil, Qnil, Qnil);
        }
    }
  unbind_to (count, Qnil);

  /* Run any eval-after-load forms for this file.  */
  if (!NILP (Ffboundp (Qdo_after_load_evaluation)))
    call1 (Qdo_after_load_evaluation, hist_file_name) ;

  xfree (saved_doc_string);
  saved_doc_string = 0;
  saved_doc_string_size = 0;

  xfree (prev_saved_doc_string);
  prev_saved_doc_string = 0;
  prev_saved_doc_string_size = 0;

  if (!noninteractive && (NILP (nomessage) || force_load_messages))
    {
      if (!safe_p)
	message_with_string ("Loading %s (compiled; note unsafe, not compiled in Emacs)...done",
		 file, 1);
      else if (is_module)
        message_with_string ("Loading %s (module)...done", file, 1);
      else if (!compiled)
	message_with_string ("Loading %s (source)...done", file, 1);
      else if (newer)
	message_with_string ("Loading %s (compiled; note, source file is newer)...done",
		 file, 1);
      else /* The typical case; compiled file newer than source file.  */
	message_with_string ("Loading %s...done", file, 1);
    }

  return Qt;
}

Lisp_Object
save_match_data_load (Lisp_Object file, Lisp_Object noerror,
		      Lisp_Object nomessage, Lisp_Object nosuffix,
		      Lisp_Object must_suffix)
{
  ptrdiff_t count = SPECPDL_INDEX ();
  record_unwind_save_match_data ();
  Lisp_Object result = Fload (file, noerror, nomessage, nosuffix, must_suffix);
  return unbind_to (count, result);
}

static bool
complete_filename_p (Lisp_Object pathname)
{
  const unsigned char *s = SDATA (pathname);
  return (IS_DIRECTORY_SEP (s[0])
	  || (SCHARS (pathname) > 2
	      && IS_DEVICE_SEP (s[1]) && IS_DIRECTORY_SEP (s[2])));
}

DEFUN ("locate-file-internal", Flocate_file_internal, Slocate_file_internal, 2, 4, 0,
       doc: /* Search for FILENAME through PATH.
Returns the file's name in absolute form, or nil if not found.
If SUFFIXES is non-nil, it should be a list of suffixes to append to
file name when searching.
If non-nil, PREDICATE is used instead of `file-readable-p'.
PREDICATE can also be an integer to pass to the faccessat(2) function,
in which case file-name-handlers are ignored.
This function will normally skip directories, so if you want it to find
directories, make sure the PREDICATE function returns `dir-ok' for them.  */)
  (Lisp_Object filename, Lisp_Object path, Lisp_Object suffixes, Lisp_Object predicate)
{
  Lisp_Object file;
  int fd = openp (path, filename, suffixes, &file, predicate, false);
  if (NILP (predicate) && fd >= 0)
    emacs_close (fd);
  return file;
}

/* Search for a file whose name is STR, looking in directories
   in the Lisp list PATH, and trying suffixes from SUFFIX.
   On success, return a file descriptor (or 1 or -2 as described below).
   On failure, return -1 and set errno.

   SUFFIXES is a list of strings containing possible suffixes.
   The empty suffix is automatically added if the list is empty.

   PREDICATE t means the files are binary.
   PREDICATE non-nil and non-t means don't open the files,
   just look for one that satisfies the predicate.  In this case,
   return -2 on success.  The predicate can be a lisp function or
   an integer to pass to `access' (in which case file-name-handlers
   are ignored).

   If STOREPTR is nonzero, it points to a slot where the name of
   the file actually found should be stored as a Lisp string.
   nil is stored there on failure.

   If the file we find is remote, return -2
   but store the found remote file name in *STOREPTR.

   If NEWER is true, try all SUFFIXes and return the result for the
   newest file that exists.  Does not apply to remote files,
   or if a non-nil and non-t PREDICATE is specified.  */

int
openp (Lisp_Object path, Lisp_Object str, Lisp_Object suffixes,
       Lisp_Object *storeptr, Lisp_Object predicate, bool newer)
{
  ptrdiff_t fn_size = 100;
  char buf[100];
  char *fn = buf;
  bool absolute;
  ptrdiff_t want_length;
  Lisp_Object filename;
  Lisp_Object string, tail, encoded_fn, save_string;
  ptrdiff_t max_suffix_len = 0;
  int last_errno = ENOENT;
  int save_fd = -1;
  USE_SAFE_ALLOCA;

  /* The last-modified time of the newest matching file found.
     Initialize it to something less than all valid timestamps.  */
  struct timespec save_mtime = make_timespec (TYPE_MINIMUM (time_t), -1);

  CHECK_STRING (str);

  tail = suffixes;
  FOR_EACH_TAIL_SAFE (tail)
    {
      CHECK_STRING_CAR (tail);
      max_suffix_len = max (max_suffix_len,
			    SBYTES (XCAR (tail)));
    }

  string = filename = encoded_fn = save_string = Qnil;

  if (storeptr)
    *storeptr = Qnil;

  absolute = complete_filename_p (str);

  AUTO_LIST1 (just_use_str, Qnil);
  if (NILP (path))
    path = just_use_str;

  /* Go through all entries in the path and see whether we find the
     executable. */
  FOR_EACH_TAIL_SAFE (path)
   {
    ptrdiff_t baselen, prefixlen;

    if (EQ (path, just_use_str))
      filename = str;
    else
      filename = Fexpand_file_name (str, XCAR (path));
    if (!complete_filename_p (filename))
      /* If there are non-absolute elts in PATH (eg ".").  */
      /* Of course, this could conceivably lose if luser sets
	 default-directory to be something non-absolute...  */
      {
	filename = Fexpand_file_name (filename, BVAR (current_buffer, directory));
	if (!complete_filename_p (filename))
	  /* Give up on this path element!  */
	  continue;
      }

    /* Calculate maximum length of any filename made from
       this path element/specified file name and any possible suffix.  */
    want_length = max_suffix_len + SBYTES (filename);
    if (fn_size <= want_length)
      {
	fn_size = 100 + want_length;
	fn = SAFE_ALLOCA (fn_size);
      }

    /* Copy FILENAME's data to FN but remove starting /: if any.  */
    prefixlen = ((SCHARS (filename) > 2
		  && SREF (filename, 0) == '/'
		  && SREF (filename, 1) == ':')
		 ? 2 : 0);
    baselen = SBYTES (filename) - prefixlen;
    memcpy (fn, SDATA (filename) + prefixlen, baselen);

    /* Loop over suffixes.  */
    AUTO_LIST1 (empty_string_only, empty_unibyte_string);
    tail = NILP (suffixes) ? empty_string_only : suffixes;
    FOR_EACH_TAIL_SAFE (tail)
      {
	Lisp_Object suffix = XCAR (tail);
	ptrdiff_t fnlen, lsuffix = SBYTES (suffix);
	Lisp_Object handler;

	/* Make complete filename by appending SUFFIX.  */
	memcpy (fn + baselen, SDATA (suffix), lsuffix + 1);
	fnlen = baselen + lsuffix;

	/* Check that the file exists and is not a directory.  */
	/* We used to only check for handlers on non-absolute file names:
	   if (absolute)
	   handler = Qnil;
	   else
	   handler = Ffind_file_name_handler (filename, Qfile_exists_p);
	   It's not clear why that was the case and it breaks things like
	   (load "/bar.el") where the file is actually "/bar.el.gz".  */
	/* make_string has its own ideas on when to return a unibyte
	   string and when a multibyte string, but we know better.
	   We must have a unibyte string when dumping, since
	   file-name encoding is shaky at best at that time, and in
	   particular default-file-name-coding-system is reset
	   several times during loadup.  We therefore don't want to
	   encode the file before passing it to file I/O library
	   functions.  */
	if (!STRING_MULTIBYTE (filename) && !STRING_MULTIBYTE (suffix))
	  string = make_unibyte_string (fn, fnlen);
	else
	  string = make_string (fn, fnlen);
	handler = Ffind_file_name_handler (string, Qfile_exists_p);
	if ((!NILP (handler) || (!NILP (predicate) && !EQ (predicate, Qt)))
	    && !FIXNATP (predicate))
	  {
	    bool exists;
	    if (NILP (predicate) || EQ (predicate, Qt))
	      exists = !NILP (Ffile_readable_p (string));
	    else
	      {
		Lisp_Object tmp = call1 (predicate, string);
		if (NILP (tmp))
		  exists = false;
		else if (EQ (tmp, Qdir_ok)
			 || NILP (Ffile_directory_p (string)))
		  exists = true;
		else
		  {
		    exists = false;
		    last_errno = EISDIR;
		  }
	      }

	    if (exists)
	      {
		/* We succeeded; return this descriptor and filename.  */
		if (storeptr)
		  *storeptr = string;
		SAFE_FREE ();
		return -2;
	      }
	  }
	else
	  {
	    int fd;
	    const char *pfn;
	    struct stat st;

	    encoded_fn = ENCODE_FILE (string);
	    pfn = SSDATA (encoded_fn);

	    /* Check that we can access or open it.  */
	    if (FIXNATP (predicate))
	      {
		fd = -1;
		if (INT_MAX < XFIXNAT (predicate))
		  last_errno = EINVAL;
		else if (faccessat (AT_FDCWD, pfn, XFIXNAT (predicate),
				    AT_EACCESS)
			 == 0)
		  {
		    if (file_directory_p (encoded_fn))
		      last_errno = EISDIR;
		    else if (errno == ENOENT || errno == ENOTDIR)
		      fd = 1;
		    else
		      last_errno = errno;
		  }
		else if (! (errno == ENOENT || errno == ENOTDIR))
		  last_errno = errno;
	      }
	    else
	      {
		fd = emacs_open (pfn, O_RDONLY, 0);
		if (fd < 0)
		  {
		    if (! (errno == ENOENT || errno == ENOTDIR))
		      last_errno = errno;
		  }
		else
		  {
		    int err = (fstat (fd, &st) != 0 ? errno
			       : S_ISDIR (st.st_mode) ? EISDIR : 0);
		    if (err)
		      {
			last_errno = err;
			emacs_close (fd);
			fd = -1;
		      }
		  }
	      }

	    if (fd >= 0)
	      {
		if (newer && !FIXNATP (predicate))
		  {
		    struct timespec mtime = get_stat_mtime (&st);

		    if (timespec_cmp (mtime, save_mtime) <= 0)
		      emacs_close (fd);
		    else
		      {
			if (0 <= save_fd)
			  emacs_close (save_fd);
			save_fd = fd;
			save_mtime = mtime;
			save_string = string;
		      }
		  }
		else
		  {
		    /* We succeeded; return this descriptor and filename.  */
		    if (storeptr)
		      *storeptr = string;
		    SAFE_FREE ();
		    return fd;
		  }
	      }

	    /* No more suffixes.  Return the newest.  */
	    if (0 <= save_fd && ! CONSP (XCDR (tail)))
	      {
		if (storeptr)
		  *storeptr = save_string;
		SAFE_FREE ();
		return save_fd;
	      }
	  }
      }
    if (absolute)
      break;
   }

  SAFE_FREE ();
  errno = last_errno;
  return -1;
}


/* Merge the list we've accumulated of globals from the current input source
   into the load_history variable.  The details depend on whether
   the source has an associated file name or not.

   FILENAME is the file name that we are loading from.

   ENTIRE is true if loading that entire file, false if evaluating
   part of it.  */

static void
build_load_history (Lisp_Object filename, bool entire)
{
  Lisp_Object tail, prev, newelt;
  Lisp_Object tem, tem2;
  bool foundit = 0;

  tail = Vload_history;
  prev = Qnil;

  FOR_EACH_TAIL (tail)
    {
      tem = XCAR (tail);

      /* Find the feature's previous assoc list...  */
      if (!NILP (Fequal (filename, Fcar (tem))))
	{
	  foundit = 1;

	  /*  If we're loading the entire file, remove old data.  */
	  if (entire)
	    {
	      if (NILP (prev))
		Vload_history = XCDR (tail);
	      else
		Fsetcdr (prev, XCDR (tail));
	    }

	  /*  Otherwise, cons on new symbols that are not already members.  */
	  else
	    {
	      tem2 = Vcurrent_load_list;

	      FOR_EACH_TAIL (tem2)
		{
		  newelt = XCAR (tem2);

		  if (NILP (Fmember (newelt, tem)))
		    Fsetcar (tail, Fcons (XCAR (tem),
		     			  Fcons (newelt, XCDR (tem))));
		  maybe_quit ();
		}
	    }
	}
      else
	prev = tail;
      maybe_quit ();
    }

  /* If we're loading an entire file, cons the new assoc onto the
     front of load-history, the most-recently-loaded position.  Also
     do this if we didn't find an existing member for the file.  */
  if (entire || !foundit)
    Vload_history = Fcons (Fnreverse (Vcurrent_load_list),
			   Vload_history);
}

static void
readevalloop_1 (int old)
{
  load_convert_to_unibyte = old;
}

/* Signal an `end-of-file' error, if possible with file name
   information.  */

static AVOID
end_of_file_error (void)
{
  if (STRINGP (Vload_file_name))
    xsignal1 (Qend_of_file, Vload_file_name);

  xsignal0 (Qend_of_file);
}

static Lisp_Object
readevalloop_eager_expand_eval (Lisp_Object val, Lisp_Object macroexpand)
{
  /* If we macroexpand the toplevel form non-recursively and it ends
     up being a `progn' (or if it was a progn to start), treat each
     form in the progn as a top-level form.  This way, if one form in
     the progn defines a macro, that macro is in effect when we expand
     the remaining forms.  See similar code in bytecomp.el.  */
  val = call2 (macroexpand, val, Qnil);
  if (EQ (CAR_SAFE (val), Qprogn))
    {
      Lisp_Object subforms = XCDR (val);
      val = Qnil;
      FOR_EACH_TAIL (subforms)
	val = readevalloop_eager_expand_eval (XCAR (subforms), macroexpand);
    }
  else
      val = eval_sub (call2 (macroexpand, val, Qt));
  return val;
}

/* UNIBYTE specifies how to set load_convert_to_unibyte
   for this invocation.
   READFUN, if non-nil, is used instead of `read'.

   START, END specify region to read in current buffer (from eval-region).
   If the input is not from a buffer, they must be nil.  */

static void
readevalloop (Lisp_Object readcharfun,
	      struct infile *infile0,
	      Lisp_Object sourcename,
	      bool printflag,
	      Lisp_Object unibyte, Lisp_Object readfun,
	      Lisp_Object start, Lisp_Object end)
{
  int c;
  Lisp_Object val;
  ptrdiff_t count = SPECPDL_INDEX ();
  struct buffer *b = 0;
  bool continue_reading_p;
  Lisp_Object lex_bound;
  /* True if reading an entire buffer.  */
  bool whole_buffer = 0;
  /* True on the first time around.  */
  bool first_sexp = 1;
  Lisp_Object macroexpand = intern ("internal-macroexpand-for-load");

  if (NILP (Ffboundp (macroexpand))
      || (STRINGP (sourcename) && suffix_p (sourcename, ".elc")))
    /* Don't macroexpand before the corresponding function is defined
       and don't bother macroexpanding in .elc files, since it should have
       been done already.  */
    macroexpand = Qnil;

  if (MARKERP (readcharfun))
    {
      if (NILP (start))
	start = readcharfun;
    }

  if (BUFFERP (readcharfun))
    b = XBUFFER (readcharfun);
  else if (MARKERP (readcharfun))
    b = XMARKER (readcharfun)->buffer;

  /* We assume START is nil when input is not from a buffer.  */
  if (! NILP (start) && !b)
    emacs_abort ();

  specbind (Qstandard_input, readcharfun);
  specbind (Qcurrent_load_list, Qnil);
  record_unwind_protect_int (readevalloop_1, load_convert_to_unibyte);
  load_convert_to_unibyte = !NILP (unibyte);

  /* If lexical binding is active (either because it was specified in
     the file's header, or via a buffer-local variable), create an empty
     lexical environment, otherwise, turn off lexical binding.  */
  lex_bound = find_symbol_value (Qlexical_binding);
  specbind (Qinternal_interpreter_environment,
	    (NILP (lex_bound) || EQ (lex_bound, Qunbound)
	     ? Qnil : list1 (Qt)));

  /* Ensure sourcename is absolute, except whilst preloading.  */
  if (!will_dump_p ()
      && !NILP (sourcename) && !NILP (Ffile_name_absolute_p (sourcename)))
    sourcename = Fexpand_file_name (sourcename, Qnil);

  LOADHIST_ATTACH (sourcename);

  continue_reading_p = 1;
  while (continue_reading_p)
    {
      ptrdiff_t count1 = SPECPDL_INDEX ();

      if (b != 0 && !BUFFER_LIVE_P (b))
	error ("Reading from killed buffer");

      if (!NILP (start))
	{
	  /* Switch to the buffer we are reading from.  */
	  record_unwind_protect_excursion ();
	  set_buffer_internal (b);

	  /* Save point in it.  */
	  record_unwind_protect_excursion ();
	  /* Save ZV in it.  */
	  record_unwind_protect (save_restriction_restore, save_restriction_save ());
	  /* Those get unbound after we read one expression.  */

	  /* Set point and ZV around stuff to be read.  */
	  Fgoto_char (start);
	  if (!NILP (end))
	    Fnarrow_to_region (make_fixnum (BEGV), end);

	  /* Just for cleanliness, convert END to a marker
	     if it is an integer.  */
	  if (FIXNUMP (end))
	    end = Fpoint_max_marker ();
	}

      /* On the first cycle, we can easily test here
	 whether we are reading the whole buffer.  */
      if (b && first_sexp)
	whole_buffer = (BUF_PT (b) == BUF_BEG (b) && BUF_ZV (b) == BUF_Z (b));

      eassert (!infile0 || infile == infile0);
    read_next:
      c = READCHAR;
      if (c == ';')
	{
	  while ((c = READCHAR) != '\n' && c != -1);
	  goto read_next;
	}
      if (c < 0)
	{
	  unbind_to (count1, Qnil);
	  break;
	}

      /* Ignore whitespace here, so we can detect eof.  */
      if (c == ' ' || c == '\t' || c == '\n' || c == '\f' || c == '\r'
	  || c == NO_BREAK_SPACE)
	goto read_next;

      if (! HASH_TABLE_P (read_objects_map)
	  || XHASH_TABLE (read_objects_map)->count)
	read_objects_map
	  = make_hash_table (hashtest_eq, DEFAULT_HASH_SIZE,
			     DEFAULT_REHASH_SIZE, DEFAULT_REHASH_THRESHOLD,
			     Qnil, false);
      if (! HASH_TABLE_P (read_objects_completed)
	  || XHASH_TABLE (read_objects_completed)->count)
	read_objects_completed
	  = make_hash_table (hashtest_eq, DEFAULT_HASH_SIZE,
			     DEFAULT_REHASH_SIZE, DEFAULT_REHASH_THRESHOLD,
			     Qnil, false);
      if (!NILP (Vpurify_flag) && c == '(')
	{
	  val = read_list (0, readcharfun);
	}
      else
	{
	  UNREAD (c);
	  if (!NILP (readfun))
	    {
	      val = call1 (readfun, readcharfun);

	      /* If READCHARFUN has set point to ZV, we should
	         stop reading, even if the form read sets point
		 to a different value when evaluated.  */
	      if (BUFFERP (readcharfun))
		{
		  struct buffer *buf = XBUFFER (readcharfun);
		  if (BUF_PT (buf) == BUF_ZV (buf))
		    continue_reading_p = 0;
		}
	    }
	  else if (! NILP (Vload_read_function))
	    val = call1 (Vload_read_function, readcharfun);
	  else
	    val = read_internal_start (readcharfun, Qnil, Qnil);
	}
      /* Empty hashes can be reused; otherwise, reset on next call.  */
      if (HASH_TABLE_P (read_objects_map)
	  && XHASH_TABLE (read_objects_map)->count > 0)
	read_objects_map = Qnil;
      if (HASH_TABLE_P (read_objects_completed)
	  && XHASH_TABLE (read_objects_completed)->count > 0)
	read_objects_completed = Qnil;

      if (!NILP (start) && continue_reading_p)
	start = Fpoint_marker ();

      /* Restore saved point and BEGV.  */
      unbind_to (count1, Qnil);

      /* Now eval what we just read.  */
      if (!NILP (macroexpand))
        val = readevalloop_eager_expand_eval (val, macroexpand);
      else
        val = eval_sub (val);

      if (printflag)
	{
	  Vvalues = Fcons (val, Vvalues);
	  if (EQ (Vstandard_output, Qt))
	    Fprin1 (val, Qnil);
	  else
	    Fprint (val, Qnil);
	}

      first_sexp = 0;
    }

  build_load_history (sourcename,
		      infile0 || whole_buffer);

  unbind_to (count, Qnil);
}

DEFUN ("eval-buffer", Feval_buffer, Seval_buffer, 0, 5, "",
       doc: /* Execute the accessible portion of current buffer as Lisp code.
You can use \\[narrow-to-region] to limit the part of buffer to be evaluated.
When called from a Lisp program (i.e., not interactively), this
function accepts up to five optional arguments:
BUFFER is the buffer to evaluate (nil means use current buffer),
 or a name of a buffer (a string).
PRINTFLAG controls printing of output by any output functions in the
 evaluated code, such as `print', `princ', and `prin1':
  a value of nil means discard it; anything else is the stream to print to.
  See Info node `(elisp)Output Streams' for details on streams.
FILENAME specifies the file name to use for `load-history'.
UNIBYTE, if non-nil, specifies `load-convert-to-unibyte' for this
 invocation.
DO-ALLOW-PRINT, if non-nil, specifies that output functions in the
 evaluated code should work normally even if PRINTFLAG is nil, in
 which case the output is displayed in the echo area.

This function ignores the current value of the `lexical-binding'
variable.  Instead it will heed any
  -*- lexical-binding: t -*-
settings in the buffer, and if there is no such setting, the buffer
will be evaluated without lexical binding.

This function preserves the position of point.  */)
  (Lisp_Object buffer, Lisp_Object printflag, Lisp_Object filename, Lisp_Object unibyte, Lisp_Object do_allow_print)
{
  ptrdiff_t count = SPECPDL_INDEX ();
  Lisp_Object tem, buf;

  if (NILP (buffer))
    buf = Fcurrent_buffer ();
  else
    buf = Fget_buffer (buffer);
  if (NILP (buf))
    error ("No such buffer");

  if (NILP (printflag) && NILP (do_allow_print))
    tem = Qsymbolp;
  else
    tem = printflag;

  if (NILP (filename))
    filename = BVAR (XBUFFER (buf), filename);

  specbind (Qeval_buffer_list, Fcons (buf, Veval_buffer_list));
  specbind (Qstandard_output, tem);
  record_unwind_protect_excursion ();
  BUF_TEMP_SET_PT (XBUFFER (buf), BUF_BEGV (XBUFFER (buf)));
  specbind (Qlexical_binding, lisp_file_lexically_bound_p (buf) ? Qt : Qnil);
  BUF_TEMP_SET_PT (XBUFFER (buf), BUF_BEGV (XBUFFER (buf)));
  readevalloop (buf, 0, filename,
		!NILP (printflag), unibyte, Qnil, Qnil, Qnil);
  return unbind_to (count, Qnil);
}

DEFUN ("eval-region", Feval_region, Seval_region, 2, 4, "r",
       doc: /* Execute the region as Lisp code.
When called from programs, expects two arguments,
giving starting and ending indices in the current buffer
of the text to be executed.
Programs can pass third argument PRINTFLAG which controls output:
 a value of nil means discard it; anything else is stream for printing it.
 See Info node `(elisp)Output Streams' for details on streams.
Also the fourth argument READ-FUNCTION, if non-nil, is used
instead of `read' to read each expression.  It gets one argument
which is the input stream for reading characters.

This function does not move point.  */)
  (Lisp_Object start, Lisp_Object end, Lisp_Object printflag, Lisp_Object read_function)
{
  /* FIXME: Do the eval-sexp-add-defvars dance!  */
  ptrdiff_t count = SPECPDL_INDEX ();
  Lisp_Object tem, cbuf;

  cbuf = Fcurrent_buffer ();

  if (NILP (printflag))
    tem = Qsymbolp;
  else
    tem = printflag;
  specbind (Qstandard_output, tem);
  specbind (Qeval_buffer_list, Fcons (cbuf, Veval_buffer_list));

  /* `readevalloop' calls functions which check the type of start and end.  */
  readevalloop (cbuf, 0, BVAR (XBUFFER (cbuf), filename),
		!NILP (printflag), Qnil, read_function,
		start, end);

  return unbind_to (count, Qnil);
}


DEFUN ("read", Fread, Sread, 0, 1, 0,
       doc: /* Read one Lisp expression as text from STREAM, return as Lisp object.
If STREAM is nil, use the value of `standard-input' (which see).
STREAM or the value of `standard-input' may be:
 a buffer (read from point and advance it)
 a marker (read from where it points and advance it)
 a function (call it with no arguments for each character,
     call it with a char as argument to push a char back)
 a string (takes text from string, starting at the beginning)
 t (read text line using minibuffer and use it, or read from
    standard input in batch mode).  */)
  (Lisp_Object stream)
{
  if (NILP (stream))
    stream = Vstandard_input;
  if (EQ (stream, Qt))
    stream = Qread_char;
  if (EQ (stream, Qread_char))
    /* FIXME: ?! This is used when the reader is called from the
       minibuffer without a stream, as in (read).  But is this feature
       ever used, and if so, why?  IOW, will anything break if this
       feature is removed !?  */
    return call1 (intern ("read-minibuffer"),
		  build_string ("Lisp expression: "));

  return read_internal_start (stream, Qnil, Qnil);
}

DEFUN ("read-from-string", Fread_from_string, Sread_from_string, 1, 3, 0,
       doc: /* Read one Lisp expression which is represented as text by STRING.
Returns a cons: (OBJECT-READ . FINAL-STRING-INDEX).
FINAL-STRING-INDEX is an integer giving the position of the next
remaining character in STRING.  START and END optionally delimit
a substring of STRING from which to read;  they default to 0 and
\(length STRING) respectively.  Negative values are counted from
the end of STRING.  */)
  (Lisp_Object string, Lisp_Object start, Lisp_Object end)
{
  Lisp_Object ret;
  CHECK_STRING (string);
  /* `read_internal_start' sets `read_from_string_index'.  */
  ret = read_internal_start (string, start, end);
  return Fcons (ret, make_fixnum (read_from_string_index));
}

/* Function to set up the global context we need in toplevel read
   calls.  START and END only used when STREAM is a string.  */
static Lisp_Object
read_internal_start (Lisp_Object stream, Lisp_Object start, Lisp_Object end)
{
  Lisp_Object retval;

  readchar_count = 0;
  new_backquote_flag = force_new_style_backquotes;
  /* We can get called from readevalloop which may have set these
     already.  */
  if (! HASH_TABLE_P (read_objects_map)
      || XHASH_TABLE (read_objects_map)->count)
    read_objects_map
      = make_hash_table (hashtest_eq, DEFAULT_HASH_SIZE, DEFAULT_REHASH_SIZE,
			 DEFAULT_REHASH_THRESHOLD, Qnil, false);
  if (! HASH_TABLE_P (read_objects_completed)
      || XHASH_TABLE (read_objects_completed)->count)
    read_objects_completed
      = make_hash_table (hashtest_eq, DEFAULT_HASH_SIZE, DEFAULT_REHASH_SIZE,
			 DEFAULT_REHASH_THRESHOLD, Qnil, false);
  if (EQ (Vread_with_symbol_positions, Qt)
      || EQ (Vread_with_symbol_positions, stream))
    Vread_symbol_positions_list = Qnil;

  if (STRINGP (stream)
      || ((CONSP (stream) && STRINGP (XCAR (stream)))))
    {
      ptrdiff_t startval, endval;
      Lisp_Object string;

      if (STRINGP (stream))
	string = stream;
      else
	string = XCAR (stream);

      validate_subarray (string, start, end, SCHARS (string),
			 &startval, &endval);

      read_from_string_index = startval;
      read_from_string_index_byte = string_char_to_byte (string, startval);
      read_from_string_limit = endval;
    }

  retval = read0 (stream);
  if (EQ (Vread_with_symbol_positions, Qt)
      || EQ (Vread_with_symbol_positions, stream))
    Vread_symbol_positions_list = Fnreverse (Vread_symbol_positions_list);
  /* Empty hashes can be reused; otherwise, reset on next call.  */
  if (HASH_TABLE_P (read_objects_map)
      && XHASH_TABLE (read_objects_map)->count > 0)
    read_objects_map = Qnil;
  if (HASH_TABLE_P (read_objects_completed)
      && XHASH_TABLE (read_objects_completed)->count > 0)
    read_objects_completed = Qnil;
  return retval;
}


/* Signal Qinvalid_read_syntax error.
   S is error string of length N (if > 0)  */

static AVOID
invalid_syntax (const char *s)
{
  xsignal1 (Qinvalid_read_syntax, build_string (s));
}


/* Use this for recursive reads, in contexts where internal tokens
   are not allowed.  */

static Lisp_Object
read0 (Lisp_Object readcharfun)
{
  register Lisp_Object val;
  int c;

  val = read1 (readcharfun, &c, 0);
  if (!c)
    return val;

  xsignal1 (Qinvalid_read_syntax,
	    Fmake_string (make_fixnum (1), make_fixnum (c), Qnil));
}

/* Grow a read buffer BUF that contains OFFSET useful bytes of data,
   by at least MAX_MULTIBYTE_LENGTH bytes.  Update *BUF_ADDR and
   *BUF_SIZE accordingly; 0 <= OFFSET <= *BUF_SIZE.  If *BUF_ADDR is
   initially null, BUF is on the stack: copy its data to the new heap
   buffer.  Otherwise, BUF must equal *BUF_ADDR and can simply be
   reallocated.  Either way, remember the heap allocation (which is at
   pdl slot COUNT) so that it can be freed when unwinding the stack.*/

static char *
grow_read_buffer (char *buf, ptrdiff_t offset,
		  char **buf_addr, ptrdiff_t *buf_size, ptrdiff_t count)
{
  char *p = xpalloc (*buf_addr, buf_size, MAX_MULTIBYTE_LENGTH, -1, 1);
  if (!*buf_addr)
    {
      memcpy (p, buf, offset);
      record_unwind_protect_ptr (xfree, p);
    }
  else
    set_unwind_protect_ptr (count, xfree, p);
  *buf_addr = p;
  return p;
}

/* Return the scalar value that has the Unicode character name NAME.
   Raise 'invalid-read-syntax' if there is no such character.  */
static int
character_name_to_code (char const *name, ptrdiff_t name_len)
{
  /* For "U+XXXX", pass the leading '+' to string_to_number to reject
     monstrosities like "U+-0000".  */
  ptrdiff_t len = name_len - 1;
  Lisp_Object code
    = (name[0] == 'U' && name[1] == '+'
       ? string_to_number (name + 1, 16, &len)
       : call2 (Qchar_from_name, make_unibyte_string (name, name_len), Qt));

  if (! RANGED_FIXNUMP (0, code, MAX_UNICODE_CHAR)
      || len != name_len - 1
      || char_surrogate_p (XFIXNUM (code)))
    {
      AUTO_STRING (format, "\\N{%s}");
      AUTO_STRING_WITH_LEN (namestr, name, name_len);
      xsignal1 (Qinvalid_read_syntax, CALLN (Fformat, format, namestr));
    }

  return XFIXNUM (code);
}

/* Bound on the length of a Unicode character name.  As of
   Unicode 9.0.0 the maximum is 83, so this should be safe.  */
enum { UNICODE_CHARACTER_NAME_LENGTH_BOUND = 200 };

/* Read a \-escape sequence, assuming we already read the `\'.
   If the escape sequence forces unibyte, return eight-bit char.  */

static int
read_escape (Lisp_Object readcharfun, bool stringp)
{
  int c = READCHAR;
  /* \u allows up to four hex digits, \U up to eight.  Default to the
     behavior for \u, and change this value in the case that \U is seen.  */
  int unicode_hex_count = 4;

  switch (c)
    {
    case -1:
      end_of_file_error ();

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
    case ' ':
      if (stringp)
	return -1;
      return ' ';

    case 'M':
      c = READCHAR;
      if (c != '-')
	error ("Invalid escape character syntax");
      c = READCHAR;
      if (c == '\\')
	c = read_escape (readcharfun, 0);
      return c | meta_modifier;

    case 'S':
      c = READCHAR;
      if (c != '-')
	error ("Invalid escape character syntax");
      c = READCHAR;
      if (c == '\\')
	c = read_escape (readcharfun, 0);
      return c | shift_modifier;

    case 'H':
      c = READCHAR;
      if (c != '-')
	error ("Invalid escape character syntax");
      c = READCHAR;
      if (c == '\\')
	c = read_escape (readcharfun, 0);
      return c | hyper_modifier;

    case 'A':
      c = READCHAR;
      if (c != '-')
	error ("Invalid escape character syntax");
      c = READCHAR;
      if (c == '\\')
	c = read_escape (readcharfun, 0);
      return c | alt_modifier;

    case 's':
      c = READCHAR;
      if (stringp || c != '-')
	{
	  UNREAD (c);
	  return ' ';
	}
      c = READCHAR;
      if (c == '\\')
	c = read_escape (readcharfun, 0);
      return c | super_modifier;

    case 'C':
      c = READCHAR;
      if (c != '-')
	error ("Invalid escape character syntax");
      FALLTHROUGH;
    case '^':
      c = READCHAR;
      if (c == '\\')
	c = read_escape (readcharfun, 0);
      if ((c & ~CHAR_MODIFIER_MASK) == '?')
	return 0177 | (c & CHAR_MODIFIER_MASK);
      else if (! SINGLE_BYTE_CHAR_P ((c & ~CHAR_MODIFIER_MASK)))
	return c | ctrl_modifier;
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

	if (i >= 0x80 && i < 0x100)
	  i = BYTE8_TO_CHAR (i);
	return i;
      }

    case 'x':
      /* A hex escape, as in ANSI C.  */
      {
	unsigned int i = 0;
	int count = 0;
	while (1)
	  {
	    c = READCHAR;
	    int digit = char_hexdigit (c);
	    if (digit < 0)
	      {
		UNREAD (c);
		break;
	      }
	    i = (i << 4) + digit;
	    /* Allow hex escapes as large as ?\xfffffff, because some
	       packages use them to denote characters with modifiers.  */
	    if ((CHAR_META | (CHAR_META - 1)) < i)
	      error ("Hex character out of range: \\x%x...", i);
	    count += count < 3;
	  }

	if (count < 3 && i >= 0x80)
	  return BYTE8_TO_CHAR (i);
	return i;
      }

    case 'U':
      /* Post-Unicode-2.0: Up to eight hex chars.  */
      unicode_hex_count = 8;
      FALLTHROUGH;
    case 'u':

      /* A Unicode escape.  We only permit them in strings and characters,
	 not arbitrarily in the source code, as in some other languages.  */
      {
	unsigned int i = 0;
	int count = 0;

	while (++count <= unicode_hex_count)
	  {
	    c = READCHAR;
	    /* `isdigit' and `isalpha' may be locale-specific, which we don't
	       want.  */
	    int digit = char_hexdigit (c);
	    if (digit < 0)
	      error ("Non-hex character used for Unicode escape: %c (%d)",
		     c, c);
	    i = (i << 4) + digit;
	  }
	if (i > 0x10FFFF)
	  error ("Non-Unicode character: 0x%x", i);
	return i;
      }

    case 'N':
      /* Named character.  */
      {
        c = READCHAR;
        if (c != '{')
          invalid_syntax ("Expected opening brace after \\N");
        char name[UNICODE_CHARACTER_NAME_LENGTH_BOUND + 1];
        bool whitespace = false;
        ptrdiff_t length = 0;
        while (true)
          {
            c = READCHAR;
            if (c < 0)
              end_of_file_error ();
            if (c == '}')
              break;
            if (! (0 < c && c < 0x80))
              {
                AUTO_STRING (format,
                             "Invalid character U+%04X in character name");
                xsignal1 (Qinvalid_read_syntax,
                          CALLN (Fformat, format, make_fixed_natnum (c)));
              }
            /* Treat multiple adjacent whitespace characters as a
               single space character.  This makes it easier to use
               character names in e.g. multi-line strings.  */
            if (c_isspace (c))
              {
                if (whitespace)
                  continue;
                c = ' ';
                whitespace = true;
              }
            else
              whitespace = false;
            name[length++] = c;
            if (length >= sizeof name)
              invalid_syntax ("Character name too long");
          }
        if (length == 0)
          invalid_syntax ("Empty character name");
	name[length] = '\0';

	/* character_name_to_code can invoke read1, recursively.
	   This is why read1's buffer is not static.  */
	return character_name_to_code (name, length);
      }

    default:
      return c;
    }
}

/* Return the digit that CHARACTER stands for in the given BASE.
   Return -1 if CHARACTER is out of range for BASE,
   and -2 if CHARACTER is not valid for any supported BASE.  */
static int
digit_to_number (int character, int base)
{
  int digit;

  if ('0' <= character && character <= '9')
    digit = character - '0';
  else if ('a' <= character && character <= 'z')
    digit = character - 'a' + 10;
  else if ('A' <= character && character <= 'Z')
    digit = character - 'A' + 10;
  else
    return -2;

  return digit < base ? digit : -1;
}

static char const invalid_radix_integer_format[] = "integer, radix %"pI"d";

/* Small, as read1 is recursive (Bug#31995).  But big enough to hold
   the invalid_radix_integer string.  */
enum { stackbufsize = max (64,
			   (sizeof invalid_radix_integer_format
			    - sizeof "%"pI"d"
			    + INT_STRLEN_BOUND (EMACS_INT) + 1)) };

static void
invalid_radix_integer (EMACS_INT radix, char stackbuf[VLA_ELEMS (stackbufsize)])
{
  sprintf (stackbuf, invalid_radix_integer_format, radix);
  invalid_syntax (stackbuf);
}

/* Read an integer in radix RADIX using READCHARFUN to read
   characters.  RADIX must be in the interval [2..36].  Use STACKBUF
   for temporary storage as needed.  Value is the integer read.
   Signal an error if encountering invalid read syntax.  */

static Lisp_Object
read_integer (Lisp_Object readcharfun, int radix,
	      char stackbuf[VLA_ELEMS (stackbufsize)])
{
  char *read_buffer = stackbuf;
  ptrdiff_t read_buffer_size = stackbufsize;
  char *p = read_buffer;
  char *heapbuf = NULL;
  int valid = -1; /* 1 if valid, 0 if not, -1 if incomplete.  */
  ptrdiff_t count = SPECPDL_INDEX ();

  int c = READCHAR;
  if (c == '-' || c == '+')
    {
      *p++ = c;
      c = READCHAR;
    }

  if (c == '0')
    {
      *p++ = c;
      valid = 1;

      /* Ignore redundant leading zeros, so the buffer doesn't
	 fill up with them.  */
      do
	c = READCHAR;
      while (c == '0');
    }

  for (int digit; (digit = digit_to_number (c, radix)) >= -1; )
    {
      if (digit == -1)
	valid = 0;
      if (valid < 0)
	valid = 1;
      /* Allow 1 extra byte for the \0.  */
      if (p + 1 == read_buffer + read_buffer_size)
	{
	  ptrdiff_t offset = p - read_buffer;
	  read_buffer = grow_read_buffer (read_buffer, offset,
					  &heapbuf, &read_buffer_size,
					  count);
	  p = read_buffer + offset;
	}
      *p++ = c;
      c = READCHAR;
    }

  UNREAD (c);

  if (valid != 1)
    invalid_radix_integer (radix, stackbuf);

  *p = '\0';
  return unbind_to (count, string_to_number (read_buffer, radix, NULL));
}


/* If the next token is ')' or ']' or '.', we store that character
   in *PCH and the return value is not interesting.  Else, we store
   zero in *PCH and we read and return one lisp object.

   FIRST_IN_LIST is true if this is the first element of a list.  */

static Lisp_Object
read1 (Lisp_Object readcharfun, int *pch, bool first_in_list)
{
  int c;
  bool uninterned_symbol = false;
  bool multibyte;
  char stackbuf[stackbufsize];
  current_thread->stack_top = stackbuf;

  *pch = 0;

 retry:

  c = READCHAR_REPORT_MULTIBYTE (&multibyte);
  if (c < 0)
    end_of_file_error ();

  switch (c)
    {
    case '(':
      return read_list (0, readcharfun);

    case '[':
      return read_vector (readcharfun, 0);

    case ')':
    case ']':
      {
	*pch = c;
	return Qnil;
      }

    case '#':
      c = READCHAR;
      if (c == 's')
	{
	  c = READCHAR;
	  if (c == '(')
	    {
	      /* Accept extended format for hash tables (extensible to
		 other types), e.g.
		 #s(hash-table size 2 test equal data (k1 v1 k2 v2))  */
	      Lisp_Object tmp = read_list (0, readcharfun);
	      Lisp_Object head = CAR_SAFE (tmp);
	      Lisp_Object data = Qnil;
	      Lisp_Object val = Qnil;
	      /* The size is 2 * number of allowed keywords to
		 make-hash-table.  */
	      Lisp_Object params[12];
	      Lisp_Object ht;
	      Lisp_Object key = Qnil;
	      int param_count = 0;

	      if (!EQ (head, Qhash_table))
		{
		  ptrdiff_t size = XFIXNUM (Flength (tmp));
		  Lisp_Object record = Fmake_record (CAR_SAFE (tmp),
						     make_fixnum (size - 1),
						     Qnil);
		  for (int i = 1; i < size; i++)
		    {
		      tmp = Fcdr (tmp);
		      ASET (record, i, Fcar (tmp));
		    }
		  return record;
		}

	      tmp = CDR_SAFE (tmp);

	      /* This is repetitive but fast and simple.  */
	      params[param_count] = QCsize;
	      params[param_count + 1] = Fplist_get (tmp, Qsize);
	      if (!NILP (params[param_count + 1]))
		param_count += 2;

	      params[param_count] = QCtest;
	      params[param_count + 1] = Fplist_get (tmp, Qtest);
	      if (!NILP (params[param_count + 1]))
		param_count += 2;

	      params[param_count] = QCweakness;
	      params[param_count + 1] = Fplist_get (tmp, Qweakness);
	      if (!NILP (params[param_count + 1]))
		param_count += 2;

	      params[param_count] = QCrehash_size;
	      params[param_count + 1] = Fplist_get (tmp, Qrehash_size);
	      if (!NILP (params[param_count + 1]))
		param_count += 2;

	      params[param_count] = QCrehash_threshold;
	      params[param_count + 1] = Fplist_get (tmp, Qrehash_threshold);
	      if (!NILP (params[param_count + 1]))
		param_count += 2;

              params[param_count] = QCpurecopy;
              params[param_count + 1] = Fplist_get (tmp, Qpurecopy);
              if (!NILP (params[param_count + 1]))
                param_count += 2;

	      /* This is the hash table data.  */
	      data = Fplist_get (tmp, Qdata);

	      /* Now use params to make a new hash table and fill it.  */
	      ht = Fmake_hash_table (param_count, params);

	      Lisp_Object last = data;
	      FOR_EACH_TAIL_SAFE (data)
		{
	      	  key = XCAR (data);
	      	  data = XCDR (data);
	      	  if (!CONSP (data))
		    break;
	      	  val = XCAR (data);
		  last = XCDR (data);
	      	  Fputhash (key, val, ht);
		}
	      if (!NILP (last))
		error ("Hash table data is not a list of even length");

	      return ht;
	    }
	  UNREAD (c);
	  invalid_syntax ("#");
	}
      if (c == '^')
	{
	  c = READCHAR;
	  if (c == '[')
	    {
	      Lisp_Object tmp;
	      tmp = read_vector (readcharfun, 0);
	      if (ASIZE (tmp) < CHAR_TABLE_STANDARD_SLOTS)
		error ("Invalid size char-table");
	      XSETPVECTYPE (XVECTOR (tmp), PVEC_CHAR_TABLE);
	      return tmp;
	    }
	  else if (c == '^')
	    {
	      c = READCHAR;
	      if (c == '[')
		{
		  /* Sub char-table can't be read as a regular
		     vector because of a two C integer fields.  */
		  Lisp_Object tbl, tmp = read_list (1, readcharfun);
		  ptrdiff_t size = list_length (tmp);
		  int i, depth, min_char;
		  struct Lisp_Cons *cell;

		  if (size == 0)
		    error ("Zero-sized sub char-table");

		  if (! RANGED_FIXNUMP (1, XCAR (tmp), 3))
		    error ("Invalid depth in sub char-table");
		  depth = XFIXNUM (XCAR (tmp));
		  if (chartab_size[depth] != size - 2)
		    error ("Invalid size in sub char-table");
		  cell = XCONS (tmp), tmp = XCDR (tmp), size--;
		  free_cons (cell);

		  if (! RANGED_FIXNUMP (0, XCAR (tmp), MAX_CHAR))
		    error ("Invalid minimum character in sub-char-table");
		  min_char = XFIXNUM (XCAR (tmp));
		  cell = XCONS (tmp), tmp = XCDR (tmp), size--;
		  free_cons (cell);

		  tbl = make_uninit_sub_char_table (depth, min_char);
		  for (i = 0; i < size; i++)
		    {
		      XSUB_CHAR_TABLE (tbl)->contents[i] = XCAR (tmp);
		      cell = XCONS (tmp), tmp = XCDR (tmp);
		      free_cons (cell);
		    }
		  return tbl;
		}
	      invalid_syntax ("#^^");
	    }
	  invalid_syntax ("#^");
	}
      if (c == '&')
	{
	  Lisp_Object length;
	  length = read1 (readcharfun, pch, first_in_list);
	  c = READCHAR;
	  if (c == '"')
	    {
	      Lisp_Object tmp, val;
	      EMACS_INT size_in_chars = bool_vector_bytes (XFIXNAT (length));
	      unsigned char *data;

	      UNREAD (c);
	      tmp = read1 (readcharfun, pch, first_in_list);
	      if (STRING_MULTIBYTE (tmp)
		  || (size_in_chars != SCHARS (tmp)
		      /* We used to print 1 char too many
			 when the number of bits was a multiple of 8.
			 Accept such input in case it came from an old
			 version.  */
		      && ! (XFIXNAT (length)
			    == (SCHARS (tmp) - 1) * BOOL_VECTOR_BITS_PER_CHAR)))
		invalid_syntax ("#&...");

	      val = make_uninit_bool_vector (XFIXNAT (length));
	      data = bool_vector_uchar_data (val);
	      memcpy (data, SDATA (tmp), size_in_chars);
	      /* Clear the extraneous bits in the last byte.  */
	      if (XFIXNUM (length) != size_in_chars * BOOL_VECTOR_BITS_PER_CHAR)
		data[size_in_chars - 1]
		  &= (1 << (XFIXNUM (length) % BOOL_VECTOR_BITS_PER_CHAR)) - 1;
	      return val;
	    }
	  invalid_syntax ("#&...");
	}
      if (c == '[')
	{
	  /* Accept compiled functions at read-time so that we don't have to
	     build them using function calls.  */
	  Lisp_Object tmp;
	  struct Lisp_Vector *vec;
	  tmp = read_vector (readcharfun, 1);
	  vec = XVECTOR (tmp);
	  if (vec->header.size == 0)
	    invalid_syntax ("Empty byte-code object");
	  make_byte_code (vec);
	  return tmp;
	}
      if (c == '(')
	{
	  Lisp_Object tmp;
	  int ch;

	  /* Read the string itself.  */
	  tmp = read1 (readcharfun, &ch, 0);
	  if (ch != 0 || !STRINGP (tmp))
	    invalid_syntax ("#");
	  /* Read the intervals and their properties.  */
	  while (1)
	    {
	      Lisp_Object beg, end, plist;

	      beg = read1 (readcharfun, &ch, 0);
	      end = plist = Qnil;
	      if (ch == ')')
		break;
	      if (ch == 0)
		end = read1 (readcharfun, &ch, 0);
	      if (ch == 0)
		plist = read1 (readcharfun, &ch, 0);
	      if (ch)
		invalid_syntax ("Invalid string property list");
	      Fset_text_properties (beg, end, plist, tmp);
	    }

	  return tmp;
	}

      /* #@NUMBER is used to skip NUMBER following bytes.
	 That's used in .elc files to skip over doc strings
	 and function definitions.  */
      if (c == '@')
	{
	  enum { extra = 100 };
	  ptrdiff_t i, nskip = 0, digits = 0;

	  /* Read a decimal integer.  */
	  while ((c = READCHAR) >= 0
		 && c >= '0' && c <= '9')
	    {
	      if ((STRING_BYTES_BOUND - extra) / 10 <= nskip)
		string_overflow ();
	      digits++;
	      nskip *= 10;
	      nskip += c - '0';
	      if (digits == 2 && nskip == 0)
		{ /* We've just seen #@00, which means "skip to end".  */
		  skip_dyn_eof (readcharfun);
		  return Qnil;
		}
	    }
	  if (nskip > 0)
	    /* We can't use UNREAD here, because in the code below we side-step
               READCHAR.  Instead, assume the first char after #@NNN occupies
               a single byte, which is the case normally since it's just
               a space.  */
	    nskip--;
	  else
	    UNREAD (c);

	  if (load_force_doc_strings
	      && (FROM_FILE_P (readcharfun)))
	    {
	      /* If we are supposed to force doc strings into core right now,
		 record the last string that we skipped,
		 and record where in the file it comes from.  */

	      /* But first exchange saved_doc_string
		 with prev_saved_doc_string, so we save two strings.  */
	      {
		char *temp = saved_doc_string;
		ptrdiff_t temp_size = saved_doc_string_size;
		file_offset temp_pos = saved_doc_string_position;
		ptrdiff_t temp_len = saved_doc_string_length;

		saved_doc_string = prev_saved_doc_string;
		saved_doc_string_size = prev_saved_doc_string_size;
		saved_doc_string_position = prev_saved_doc_string_position;
		saved_doc_string_length = prev_saved_doc_string_length;

		prev_saved_doc_string = temp;
		prev_saved_doc_string_size = temp_size;
		prev_saved_doc_string_position = temp_pos;
		prev_saved_doc_string_length = temp_len;
	      }

	      if (saved_doc_string_size == 0)
		{
		  saved_doc_string = xmalloc (nskip + extra);
		  saved_doc_string_size = nskip + extra;
		}
	      if (nskip > saved_doc_string_size)
		{
		  saved_doc_string = xrealloc (saved_doc_string, nskip + extra);
		  saved_doc_string_size = nskip + extra;
		}

	      FILE *instream = infile->stream;
	      saved_doc_string_position = (file_tell (instream)
					   - infile->lookahead);

	      /* Copy that many bytes into saved_doc_string.  */
	      i = 0;
	      for (int n = min (nskip, infile->lookahead); 0 < n; n--)
		saved_doc_string[i++]
		  = c = infile->buf[--infile->lookahead];
	      block_input ();
	      for (; i < nskip && 0 <= c; i++)
		saved_doc_string[i] = c = getc (instream);
	      unblock_input ();

	      saved_doc_string_length = i;
	    }
	  else
	    /* Skip that many bytes.  */
	    skip_dyn_bytes (readcharfun, nskip);

	  goto retry;
	}
      if (c == '!')
	{
	  /* #! appears at the beginning of an executable file.
	     Skip the first line.  */
	  while (c != '\n' && c >= 0)
	    c = READCHAR;
	  goto retry;
	}
      if (c == '$')
	return Vload_file_name;
      if (c == '\'')
	return list2 (Qfunction, read0 (readcharfun));
      /* #:foo is the uninterned symbol named foo.  */
      if (c == ':')
	{
	  uninterned_symbol = true;
	  c = READCHAR;
	  if (!(c > 040
		&& c != NO_BREAK_SPACE
		&& (c >= 0200
		    || strchr ("\"';()[]#`,", c) == NULL)))
	    {
	      /* No symbol character follows, this is the empty
		 symbol.  */
	      UNREAD (c);
	      return Fmake_symbol (empty_unibyte_string);
	    }
	  goto read_symbol;
	}
      /* ## is the empty symbol.  */
      if (c == '#')
	return Fintern (empty_unibyte_string, Qnil);

      if (c >= '0' && c <= '9')
	{
	  EMACS_INT n = c - '0';
	  bool overflow = false;

	  /* Read a non-negative integer.  */
	  while ('0' <= (c = READCHAR) && c <= '9')
	    {
	      overflow |= INT_MULTIPLY_WRAPV (n, 10, &n);
	      overflow |= INT_ADD_WRAPV (n, c - '0', &n);
	    }

	  if (!overflow)
	    {
	      if (c == 'r' || c == 'R')
		{
		  if (! (2 <= n && n <= 36))
		    invalid_radix_integer (n, stackbuf);
		  return read_integer (readcharfun, n, stackbuf);
		}

	      if (n <= MOST_POSITIVE_FIXNUM && ! NILP (Vread_circle))
		{
		  /* Reader forms that can reuse previously read objects.  */

		  /* #n=object returns object, but associates it with
		     n for #n#.  */
		  if (c == '=')
		    {
		      /* Make a placeholder for #n# to use temporarily.  */
		      /* Note: We used to use AUTO_CONS to allocate
			 placeholder, but that is a bad idea, since it
			 will place a stack-allocated cons cell into
			 the list in read_objects_map, which is a
			 staticpro'd global variable, and thus each of
			 its elements is marked during each GC.  A
			 stack-allocated object will become garbled
			 when its stack slot goes out of scope, and
			 some other function reuses it for entirely
			 different purposes, which will cause crashes
			 in GC.  */
		      Lisp_Object placeholder = Fcons (Qnil, Qnil);
		      struct Lisp_Hash_Table *h
			= XHASH_TABLE (read_objects_map);
		      Lisp_Object number = make_fixnum (n), hash;

		      ptrdiff_t i = hash_lookup (h, number, &hash);
		      if (i >= 0)
			/* Not normal, but input could be malformed.  */
			set_hash_value_slot (h, i, placeholder);
		      else
			hash_put (h, number, placeholder, hash);

		      /* Read the object itself.  */
		      Lisp_Object tem = read0 (readcharfun);

		      /* If it can be recursive, remember it for
			 future substitutions.  */
		      if (! SYMBOLP (tem)
			  && ! NUMBERP (tem)
			  && ! (STRINGP (tem) && !string_intervals (tem)))
			{
			  struct Lisp_Hash_Table *h2
			    = XHASH_TABLE (read_objects_completed);
			  i = hash_lookup (h2, tem, &hash);
			  eassert (i < 0);
			  hash_put (h2, tem, Qnil, hash);
			}

		      /* Now put it everywhere the placeholder was...  */
                      if (CONSP (tem))
                        {
                          Fsetcar (placeholder, XCAR (tem));
                          Fsetcdr (placeholder, XCDR (tem));
                          return placeholder;
                        }
                      else
                        {
		          Flread__substitute_object_in_subtree
			    (tem, placeholder, read_objects_completed);

		          /* ...and #n# will use the real value from now on.  */
			  i = hash_lookup (h, number, &hash);
			  eassert (i >= 0);
			  set_hash_value_slot (h, i, tem);

		          return tem;
                        }
		    }

		  /* #n# returns a previously read object.  */
		  if (c == '#')
		    {
		      struct Lisp_Hash_Table *h
			= XHASH_TABLE (read_objects_map);
		      ptrdiff_t i = hash_lookup (h, make_fixnum (n), NULL);
		      if (i >= 0)
			return HASH_VALUE (h, i);
		    }
		}
	    }
	  /* Fall through to error message.  */
	}
      else if (c == 'x' || c == 'X')
	return read_integer (readcharfun, 16, stackbuf);
      else if (c == 'o' || c == 'O')
	return read_integer (readcharfun, 8, stackbuf);
      else if (c == 'b' || c == 'B')
	return read_integer (readcharfun, 2, stackbuf);

      UNREAD (c);
      invalid_syntax ("#");

    case ';':
      while ((c = READCHAR) >= 0 && c != '\n');
      goto retry;

    case '\'':
      return list2 (Qquote, read0 (readcharfun));

    case '`':
      {
	int next_char = READCHAR;
	UNREAD (next_char);
	/* Transition from old-style to new-style:
	   If we see "(`" it used to mean old-style, which usually works
	   fine because ` should almost never appear in such a position
	   for new-style.  But occasionally we need "(`" to mean new
	   style, so we try to distinguish the two by the fact that we
	   can either write "( `foo" or "(` foo", where the first
	   intends to use new-style whereas the second intends to use
	   old-style.  For Emacs-25, we should completely remove this
	   first_in_list exception (old-style can still be obtained via
	   "(\`" anyway).  */
	if (!new_backquote_flag && first_in_list && next_char == ' ')
	  load_error_old_style_backquotes ();
	else
	  {
	    Lisp_Object value;
	    bool saved_new_backquote_flag = new_backquote_flag;

	    new_backquote_flag = 1;
	    value = read0 (readcharfun);
	    new_backquote_flag = saved_new_backquote_flag;

	    return list2 (Qbackquote, value);
	  }
      }
    case ',':
      {
	int next_char = READCHAR;
	UNREAD (next_char);
	/* Transition from old-style to new-style:
           It used to be impossible to have a new-style , other than within
	   a new-style `.  This is sufficient when ` and , are used in the
	   normal way, but ` and , can also appear in args to macros that
	   will not interpret them in the usual way, in which case , may be
	   used without any ` anywhere near.
	   So we now use the same heuristic as for backquote: old-style
	   unquotes are only recognized when first on a list, and when
	   followed by a space.
	   Because it's more difficult to peek 2 chars ahead, a new-style
	   ,@ can still not be used outside of a `, unless it's in the middle
	   of a list.  */
	if (new_backquote_flag
	    || !first_in_list
	    || (next_char != ' ' && next_char != '@'))
	  {
	    Lisp_Object comma_type = Qnil;
	    Lisp_Object value;
	    int ch = READCHAR;

	    if (ch == '@')
	      comma_type = Qcomma_at;
	    else
	      {
		if (ch >= 0) UNREAD (ch);
		comma_type = Qcomma;
	      }

	    value = read0 (readcharfun);
	    return list2 (comma_type, value);
	  }
	else
	  load_error_old_style_backquotes ();
      }
    case '?':
      {
	int modifiers;
	int next_char;
	bool ok;

	c = READCHAR;
	if (c < 0)
	  end_of_file_error ();

	/* Accept `single space' syntax like (list ? x) where the
	   whitespace character is SPC or TAB.
	   Other literal whitespace like NL, CR, and FF are not accepted,
	   as there are well-established escape sequences for these.  */
	if (c == ' ' || c == '\t')
	  return make_fixnum (c);

	if (c == '(' || c == ')' || c == '[' || c == ']'
            || c == '"' || c == ';')
	  {
            CHECK_LIST (Vlread_unescaped_character_literals);
            Lisp_Object char_obj = make_fixed_natnum (c);
            if (NILP (Fmemq (char_obj, Vlread_unescaped_character_literals)))
              Vlread_unescaped_character_literals =
                Fcons (char_obj, Vlread_unescaped_character_literals);
	  }

	if (c == '\\')
	  c = read_escape (readcharfun, 0);
	modifiers = c & CHAR_MODIFIER_MASK;
	c &= ~CHAR_MODIFIER_MASK;
	if (CHAR_BYTE8_P (c))
	  c = CHAR_TO_BYTE8 (c);
	c |= modifiers;

	next_char = READCHAR;
	ok = (next_char <= 040
	      || (next_char < 0200
		  && strchr ("\"';()[]#?`,.", next_char) != NULL));
	UNREAD (next_char);
	if (ok)
	  return make_fixnum (c);

	invalid_syntax ("?");
      }

    case '"':
      {
	ptrdiff_t count = SPECPDL_INDEX ();
	char *read_buffer = stackbuf;
	ptrdiff_t read_buffer_size = sizeof stackbuf;
	char *heapbuf = NULL;
	char *p = read_buffer;
	char *end = read_buffer + read_buffer_size;
	int ch;
	/* True if we saw an escape sequence specifying
	   a multibyte character.  */
	bool force_multibyte = false;
	/* True if we saw an escape sequence specifying
	   a single-byte character.  */
	bool force_singlebyte = false;
	bool cancel = false;
	ptrdiff_t nchars = 0;

	while ((ch = READCHAR) >= 0
	       && ch != '\"')
	  {
	    if (end - p < MAX_MULTIBYTE_LENGTH)
	      {
		ptrdiff_t offset = p - read_buffer;
		read_buffer = grow_read_buffer (read_buffer, offset,
						&heapbuf, &read_buffer_size,
						count);
		p = read_buffer + offset;
		end = read_buffer + read_buffer_size;
	      }

	    if (ch == '\\')
	      {
		int modifiers;

		ch = read_escape (readcharfun, 1);

		/* CH is -1 if \ newline or \ space has just been seen.  */
		if (ch == -1)
		  {
		    if (p == read_buffer)
		      cancel = true;
		    continue;
		  }

		modifiers = ch & CHAR_MODIFIER_MASK;
		ch = ch & ~CHAR_MODIFIER_MASK;

		if (CHAR_BYTE8_P (ch))
		  force_singlebyte = true;
		else if (! ASCII_CHAR_P (ch))
		  force_multibyte = true;
		else		/* I.e. ASCII_CHAR_P (ch).  */
		  {
		    /* Allow `\C- ' and `\C-?'.  */
		    if (modifiers == CHAR_CTL)
		      {
			if (ch == ' ')
			  ch = 0, modifiers = 0;
			else if (ch == '?')
			  ch = 127, modifiers = 0;
		      }
		    if (modifiers & CHAR_SHIFT)
		      {
			/* Shift modifier is valid only with [A-Za-z].  */
			if (ch >= 'A' && ch <= 'Z')
			  modifiers &= ~CHAR_SHIFT;
			else if (ch >= 'a' && ch <= 'z')
			  ch -= ('a' - 'A'), modifiers &= ~CHAR_SHIFT;
		      }

		    if (modifiers & CHAR_META)
		      {
			/* Move the meta bit to the right place for a
			   string.  */
			modifiers &= ~CHAR_META;
			ch = BYTE8_TO_CHAR (ch | 0x80);
			force_singlebyte = true;
		      }
		  }

		/* Any modifiers remaining are invalid.  */
		if (modifiers)
		  error ("Invalid modifier in string");
		p += CHAR_STRING (ch, (unsigned char *) p);
	      }
	    else
	      {
		p += CHAR_STRING (ch, (unsigned char *) p);
		if (CHAR_BYTE8_P (ch))
		  force_singlebyte = true;
		else if (! ASCII_CHAR_P (ch))
		  force_multibyte = true;
	      }
	    nchars++;
	  }

	if (ch < 0)
	  end_of_file_error ();

	/* If purifying, and string starts with \ newline,
	   return zero instead.  This is for doc strings
	   that we are really going to find in etc/DOC.nn.nn.  */
	if (!NILP (Vpurify_flag) && NILP (Vdoc_file_name) && cancel)
	  return unbind_to (count, make_fixnum (0));

	if (! force_multibyte && force_singlebyte)
	  {
	    /* READ_BUFFER contains raw 8-bit bytes and no multibyte
	       forms.  Convert it to unibyte.  */
	    nchars = str_as_unibyte ((unsigned char *) read_buffer,
				     p - read_buffer);
	    p = read_buffer + nchars;
	  }

	Lisp_Object result
	  = make_specified_string (read_buffer, nchars, p - read_buffer,
				   (force_multibyte
				    || (p - read_buffer != nchars)));
	return unbind_to (count, result);
      }

    case '.':
      {
	int next_char = READCHAR;
	UNREAD (next_char);

	if (next_char <= 040
	    || (next_char < 0200
		&& strchr ("\"';([#?`,", next_char) != NULL))
	  {
	    *pch = c;
	    return Qnil;
	  }
      }
      /* The atom-reading loop below will now loop at least once,
	 assuring that we will not try to UNREAD two characters in a
	 row.  */
      FALLTHROUGH;
    default:
      if (c <= 040) goto retry;
      if (c == NO_BREAK_SPACE)
	goto retry;

    read_symbol:
      {
	ptrdiff_t count = SPECPDL_INDEX ();
	char *read_buffer = stackbuf;
	ptrdiff_t read_buffer_size = sizeof stackbuf;
	char *heapbuf = NULL;
	char *p = read_buffer;
	char *end = read_buffer + read_buffer_size;
	bool quoted = false;
	EMACS_INT start_position = readchar_count - 1;

	do
	  {
	    if (end - p < MAX_MULTIBYTE_LENGTH + 1)
	      {
		ptrdiff_t offset = p - read_buffer;
		read_buffer = grow_read_buffer (read_buffer, offset,
						&heapbuf, &read_buffer_size,
						count);
		p = read_buffer + offset;
		end = read_buffer + read_buffer_size;
	      }

	    if (c == '\\')
	      {
		c = READCHAR;
		if (c == -1)
		  end_of_file_error ();
		quoted = true;
	      }

	    if (multibyte)
	      p += CHAR_STRING (c, (unsigned char *) p);
	    else
	      *p++ = c;
	    c = READCHAR;
	  }
	while (c > 040
	       && c != NO_BREAK_SPACE
	       && (c >= 0200
		   || strchr ("\"';()[]#`,", c) == NULL));

	*p = 0;
	ptrdiff_t nbytes = p - read_buffer;
	UNREAD (c);

	if (!quoted && !uninterned_symbol)
	  {
	    ptrdiff_t len;
	    Lisp_Object result = string_to_number (read_buffer, 10, &len);
	    if (! NILP (result) && len == nbytes)
	      return unbind_to (count, result);
	  }
	{
	  Lisp_Object result;
	  ptrdiff_t nchars
	    = (multibyte
	       ? multibyte_chars_in_text ((unsigned char *) read_buffer,
					  nbytes)
	       : nbytes);

	  if (uninterned_symbol)
	    {
	      Lisp_Object name
		= ((! NILP (Vpurify_flag)
		    ? make_pure_string : make_specified_string)
		   (read_buffer, nchars, nbytes, multibyte));
	      result = Fmake_symbol (name);
	    }
	  else
	    {
	      /* Don't create the string object for the name unless
		 we're going to retain it in a new symbol.

		 Like intern_1 but supports multibyte names.  */
	      Lisp_Object obarray = check_obarray (Vobarray);
	      Lisp_Object tem = oblookup (obarray, read_buffer,
					  nchars, nbytes);

	      if (SYMBOLP (tem))
		result = tem;
	      else
		{
		  Lisp_Object name
		    = make_specified_string (read_buffer, nchars, nbytes,
					     multibyte);
		  result = intern_driver (name, obarray, tem);
		}
	    }

	  if (EQ (Vread_with_symbol_positions, Qt)
	      || EQ (Vread_with_symbol_positions, readcharfun))
	    Vread_symbol_positions_list
	      = Fcons (Fcons (result, make_fixnum (start_position)),
		       Vread_symbol_positions_list);
	  return unbind_to (count, result);
	}
      }
    }
}

DEFUN ("lread--substitute-object-in-subtree",
       Flread__substitute_object_in_subtree,
       Slread__substitute_object_in_subtree, 3, 3, 0,
       doc: /* In OBJECT, replace every occurrence of PLACEHOLDER with OBJECT.
COMPLETED is a hash table of objects that might be circular, or is t
if any object might be circular.  */)
  (Lisp_Object object, Lisp_Object placeholder, Lisp_Object completed)
{
  struct subst subst = { object, placeholder, completed, Qnil };
  Lisp_Object check_object = substitute_object_recurse (&subst, object);

  /* The returned object here is expected to always eq the
     original.  */
  if (!EQ (check_object, object))
    error ("Unexpected mutation error in reader");
  return Qnil;
}

static Lisp_Object
substitute_object_recurse (struct subst *subst, Lisp_Object subtree)
{
  /* If we find the placeholder, return the target object.  */
  if (EQ (subst->placeholder, subtree))
    return subst->object;

  /* For common object types that can't contain other objects, don't
     bother looking them up; we're done.  */
  if (SYMBOLP (subtree)
      || (STRINGP (subtree) && !string_intervals (subtree))
      || NUMBERP (subtree))
    return subtree;

  /* If we've been to this node before, don't explore it again.  */
  if (!NILP (Fmemq (subtree, subst->seen)))
    return subtree;

  /* If this node can be the entry point to a cycle, remember that
     we've seen it.  It can only be such an entry point if it was made
     by #n=, which means that we can find it as a value in
     COMPLETED.  */
  if (EQ (subst->completed, Qt)
      || hash_lookup (XHASH_TABLE (subst->completed), subtree, NULL) >= 0)
    subst->seen = Fcons (subtree, subst->seen);

  /* Recurse according to subtree's type.
     Every branch must return a Lisp_Object.  */
  switch (XTYPE (subtree))
    {
    case Lisp_Vectorlike:
      {
	ptrdiff_t i = 0, length = 0;
	if (BOOL_VECTOR_P (subtree))
	  return subtree;		/* No sub-objects anyway.  */
	else if (CHAR_TABLE_P (subtree) || SUB_CHAR_TABLE_P (subtree)
		 || COMPILEDP (subtree) || HASH_TABLE_P (subtree)
		 || RECORDP (subtree))
	  length = PVSIZE (subtree);
	else if (VECTORP (subtree))
	  length = ASIZE (subtree);
	else
	  /* An unknown pseudovector may contain non-Lisp fields, so we
	     can't just blindly traverse all its fields.  We used to call
	     `Flength' which signaled `sequencep', so I just preserved this
	     behavior.  */
	  wrong_type_argument (Qsequencep, subtree);

	if (SUB_CHAR_TABLE_P (subtree))
	  i = 2;
	for ( ; i < length; i++)
	  ASET (subtree, i,
		substitute_object_recurse (subst, AREF (subtree, i)));
	return subtree;
      }

    case Lisp_Cons:
      XSETCAR (subtree, substitute_object_recurse (subst, XCAR (subtree)));
      XSETCDR (subtree, substitute_object_recurse (subst, XCDR (subtree)));
      return subtree;

    case Lisp_String:
      {
	/* Check for text properties in each interval.
	   substitute_in_interval contains part of the logic.  */

	INTERVAL root_interval = string_intervals (subtree);
	traverse_intervals_noorder (root_interval,
				    substitute_in_interval, subst);
	return subtree;
      }

      /* Other types don't recurse any further.  */
    default:
      return subtree;
    }
}

/*  Helper function for substitute_object_recurse.  */
static void
substitute_in_interval (INTERVAL interval, void *arg)
{
  set_interval_plist (interval,
		      substitute_object_recurse (arg, interval->plist));
}


/* Convert the initial prefix of STRING to a number, assuming base BASE.
   If the prefix has floating point syntax and BASE is 10, return a
   nearest float; otherwise, if the prefix has integer syntax, return
   the integer; otherwise, return nil.  If PLEN, set *PLEN to the
   length of the numeric prefix if there is one, otherwise *PLEN is
   unspecified.  */

Lisp_Object
string_to_number (char const *string, int base, ptrdiff_t *plen)
{
  char const *cp = string;
  bool float_syntax = false;
  double value = 0;

  /* Negate the value ourselves.  This treats 0, NaNs, and infinity properly on
     IEEE floating point hosts, and works around a formerly-common bug where
     atof ("-0.0") drops the sign.  */
  bool negative = *cp == '-';
  bool positive = *cp == '+';

  bool signedp = negative | positive;
  cp += signedp;

  enum { INTOVERFLOW = 1, LEAD_INT = 2, DOT_CHAR = 4, TRAIL_INT = 8,
	 E_EXP = 16 };
  int state = 0;
  int leading_digit = digit_to_number (*cp, base);
  uintmax_t n = leading_digit;
  if (leading_digit >= 0)
    {
      state |= LEAD_INT;
      for (int digit; 0 <= (digit = digit_to_number (*++cp, base)); )
	{
	  if (INT_MULTIPLY_OVERFLOW (n, base))
	    state |= INTOVERFLOW;
	  n *= base;
	  if (INT_ADD_OVERFLOW (n, digit))
	    state |= INTOVERFLOW;
	  n += digit;
	}
    }
  char const *after_digits = cp;
  if (*cp == '.')
    {
      state |= DOT_CHAR;
      cp++;
    }

  if (base == 10)
    {
      if ('0' <= *cp && *cp <= '9')
	{
	  state |= TRAIL_INT;
	  do
	    cp++;
	  while ('0' <= *cp && *cp <= '9');
	}
      if (*cp == 'e' || *cp == 'E')
	{
	  char const *ecp = cp;
	  cp++;
	  if (*cp == '+' || *cp == '-')
	    cp++;
	  if ('0' <= *cp && *cp <= '9')
	    {
	      state |= E_EXP;
	      do
		cp++;
	      while ('0' <= *cp && *cp <= '9');
	    }
#if IEEE_FLOATING_POINT
	  else if (cp[-1] == '+'
		   && cp[0] == 'I' && cp[1] == 'N' && cp[2] == 'F')
	    {
	      state |= E_EXP;
	      cp += 3;
	      value = INFINITY;
	    }
	  else if (cp[-1] == '+'
		   && cp[0] == 'N' && cp[1] == 'a' && cp[2] == 'N')
	    {
	      state |= E_EXP;
	      cp += 3;
	      union ieee754_double u
		= { .ieee_nan = { .exponent = 0x7ff, .quiet_nan = 1,
				  .mantissa0 = n >> 31 >> 1, .mantissa1 = n }};
	      value = u.d;
	    }
#endif
	  else
	    cp = ecp;
	}

      float_syntax = ((state & (DOT_CHAR|TRAIL_INT)) == (DOT_CHAR|TRAIL_INT)
		      || (state & ~INTOVERFLOW) == (LEAD_INT|E_EXP));
    }

  if (plen)
    *plen = cp - string;

  /* Return a float if the number uses float syntax.  */
  if (float_syntax)
    {
      /* Convert to floating point, unless the value is already known
	 because it is infinite or a NaN.  */
      if (! value)
	value = atof (string + signedp);
      return make_float (negative ? -value : value);
    }

  /* Return nil if the number uses invalid syntax.  */
  if (! (state & LEAD_INT))
    return Qnil;

  /* Fast path if the integer (san sign) fits in uintmax_t.  */
  if (! (state & INTOVERFLOW))
    {
      if (!negative)
	return make_uint (n);
      if (-MOST_NEGATIVE_FIXNUM < n)
	return make_neg_biguint (n);
      EMACS_INT signed_n = n;
      return make_fixnum (-signed_n);
    }

  /* Trim any leading "+" and trailing nondigits, then return a bignum.  */
  string += positive;
  if (!*after_digits)
    return make_bignum_str (string, base);
  ptrdiff_t trimmed_len = after_digits - string;
  USE_SAFE_ALLOCA;
  char *trimmed = SAFE_ALLOCA (trimmed_len + 1);
  memcpy (trimmed, string, trimmed_len);
  trimmed[trimmed_len] = '\0';
  Lisp_Object result = make_bignum_str (trimmed, base);
  SAFE_FREE ();
  return result;
}


static Lisp_Object
read_vector (Lisp_Object readcharfun, bool bytecodeflag)
{
  Lisp_Object tem = read_list (1, readcharfun);
  ptrdiff_t size = list_length (tem);
  if (bytecodeflag && size <= COMPILED_STACK_DEPTH)
    error ("Invalid byte code");
  Lisp_Object vector = make_nil_vector (size);

  Lisp_Object *ptr = XVECTOR (vector)->contents;
  for (ptrdiff_t i = 0; i < size; i++)
    {
      Lisp_Object item = Fcar (tem);
      /* If `load-force-doc-strings' is t when reading a lazily-loaded
	 bytecode object, the docstring containing the bytecode and
	 constants values must be treated as unibyte and passed to
	 Fread, to get the actual bytecode string and constants vector.  */
      if (bytecodeflag && load_force_doc_strings)
	{
	  if (i == COMPILED_BYTECODE)
	    {
	      if (!STRINGP (item))
		error ("Invalid byte code");

	      /* Delay handling the bytecode slot until we know whether
		 it is lazily-loaded (we can tell by whether the
		 constants slot is nil).  */
	      ASET (vector, COMPILED_CONSTANTS, item);
	      item = Qnil;
	    }
	  else if (i == COMPILED_CONSTANTS)
	    {
	      Lisp_Object bytestr = ptr[COMPILED_CONSTANTS];

	      if (NILP (item))
		{
		  /* Coerce string to unibyte (like string-as-unibyte,
		     but without generating extra garbage and
		     guaranteeing no change in the contents).  */
		  STRING_SET_CHARS (bytestr, SBYTES (bytestr));
		  STRING_SET_UNIBYTE (bytestr);

		  item = Fread (Fcons (bytestr, readcharfun));
		  if (!CONSP (item))
		    error ("Invalid byte code");

		  struct Lisp_Cons *otem = XCONS (item);
		  bytestr = XCAR (item);
		  item = XCDR (item);
		  free_cons (otem);
		}

	      /* Now handle the bytecode slot.  */
	      ASET (vector, COMPILED_BYTECODE, bytestr);
	    }
	  else if (i == COMPILED_DOC_STRING
		   && STRINGP (item)
		   && ! STRING_MULTIBYTE (item))
	    {
	      if (EQ (readcharfun, Qget_emacs_mule_file_char))
		item = Fdecode_coding_string (item, Qemacs_mule, Qnil, Qnil);
	      else
		item = Fstring_as_multibyte (item);
	    }
	}
      ASET (vector, i, item);
      struct Lisp_Cons *otem = XCONS (tem);
      tem = Fcdr (tem);
      free_cons (otem);
    }
  return vector;
}

/* FLAG means check for ']' to terminate rather than ')' and '.'.  */

static Lisp_Object
read_list (bool flag, Lisp_Object readcharfun)
{
  Lisp_Object val, tail;
  Lisp_Object elt, tem;
  /* 0 is the normal case.
     1 means this list is a doc reference; replace it with the number 0.
     2 means this list is a doc reference; replace it with the doc string.  */
  int doc_reference = 0;

  /* Initialize this to 1 if we are reading a list.  */
  bool first_in_list = flag <= 0;

  val = Qnil;
  tail = Qnil;

  while (1)
    {
      int ch;
      elt = read1 (readcharfun, &ch, first_in_list);

      first_in_list = 0;

      /* While building, if the list starts with #$, treat it specially.  */
      if (EQ (elt, Vload_file_name)
	  && ! NILP (elt)
	  && !NILP (Vpurify_flag))
	{
	  if (NILP (Vdoc_file_name))
	    /* We have not yet called Snarf-documentation, so assume
	       this file is described in the DOC file
	       and Snarf-documentation will fill in the right value later.
	       For now, replace the whole list with 0.  */
	    doc_reference = 1;
	  else
	    /* We have already called Snarf-documentation, so make a relative
	       file name for this file, so it can be found properly
	       in the installed Lisp directory.
	       We don't use Fexpand_file_name because that would make
	       the directory absolute now.  */
	    {
	      AUTO_STRING (dot_dot_lisp, "../lisp/");
	      elt = concat2 (dot_dot_lisp, Ffile_name_nondirectory (elt));
	    }
	}
      else if (EQ (elt, Vload_file_name)
	       && ! NILP (elt)
	       && load_force_doc_strings)
	doc_reference = 2;

      if (ch)
	{
	  if (flag > 0)
	    {
	      if (ch == ']')
		return val;
	      invalid_syntax (") or . in a vector");
	    }
	  if (ch == ')')
	    return val;
	  if (ch == '.')
	    {
	      if (!NILP (tail))
		XSETCDR (tail, read0 (readcharfun));
	      else
		val = read0 (readcharfun);
	      read1 (readcharfun, &ch, 0);

	      if (ch == ')')
		{
		  if (doc_reference == 1)
		    return make_fixnum (0);
		  if (doc_reference == 2 && FIXNUMP (XCDR (val)))
		    {
		      char *saved = NULL;
		      file_offset saved_position;
		      /* Get a doc string from the file we are loading.
			 If it's in saved_doc_string, get it from there.

			 Here, we don't know if the string is a
			 bytecode string or a doc string.  As a
			 bytecode string must be unibyte, we always
			 return a unibyte string.  If it is actually a
			 doc string, caller must make it
			 multibyte.  */

		      /* Position is negative for user variables.  */
		      EMACS_INT pos = eabs (XFIXNUM (XCDR (val)));
		      if (pos >= saved_doc_string_position
			  && pos < (saved_doc_string_position
				    + saved_doc_string_length))
			{
			  saved = saved_doc_string;
			  saved_position = saved_doc_string_position;
			}
		      /* Look in prev_saved_doc_string the same way.  */
		      else if (pos >= prev_saved_doc_string_position
			       && pos < (prev_saved_doc_string_position
					 + prev_saved_doc_string_length))
			{
			  saved = prev_saved_doc_string;
			  saved_position = prev_saved_doc_string_position;
			}
		      if (saved)
			{
			  ptrdiff_t start = pos - saved_position;
			  ptrdiff_t from, to;

			  /* Process quoting with ^A,
			     and find the end of the string,
			     which is marked with ^_ (037).  */
			  for (from = start, to = start;
			       saved[from] != 037;)
			    {
			      int c = saved[from++];
			      if (c == 1)
				{
				  c = saved[from++];
				  saved[to++] = (c == 1 ? c
						 : c == '0' ? 0
						 : c == '_' ? 037
						 : c);
				}
			      else
				saved[to++] = c;
			    }

			  return make_unibyte_string (saved + start,
						      to - start);
			}
		      else
			return get_doc_string (val, 1, 0);
		    }

		  return val;
		}
	      invalid_syntax (". in wrong context");
	    }
	  invalid_syntax ("] in a list");
	}
      tem = list1 (elt);
      if (!NILP (tail))
	XSETCDR (tail, tem);
      else
	val = tem;
      tail = tem;
    }
}

static Lisp_Object initial_obarray;

/* `oblookup' stores the bucket number here, for the sake of Funintern.  */

static size_t oblookup_last_bucket_number;

/* Get an error if OBARRAY is not an obarray.
   If it is one, return it.  */

Lisp_Object
check_obarray (Lisp_Object obarray)
{
  /* We don't want to signal a wrong-type-argument error when we are
     shutting down due to a fatal error, and we don't want to hit
     assertions in VECTORP and ASIZE if the fatal error was during GC.  */
  if (!fatal_error_in_progress
      && (!VECTORP (obarray) || ASIZE (obarray) == 0))
    {
      /* If Vobarray is now invalid, force it to be valid.  */
      if (EQ (Vobarray, obarray)) Vobarray = initial_obarray;
      wrong_type_argument (Qvectorp, obarray);
    }
  return obarray;
}

/* Intern symbol SYM in OBARRAY using bucket INDEX.  */

static Lisp_Object
intern_sym (Lisp_Object sym, Lisp_Object obarray, Lisp_Object index)
{
  Lisp_Object *ptr;

  XSYMBOL (sym)->u.s.interned = (EQ (obarray, initial_obarray)
				 ? SYMBOL_INTERNED_IN_INITIAL_OBARRAY
				 : SYMBOL_INTERNED);

  if (SREF (SYMBOL_NAME (sym), 0) == ':' && EQ (obarray, initial_obarray))
    {
      make_symbol_constant (sym);
      XSYMBOL (sym)->u.s.redirect = SYMBOL_PLAINVAL;
      SET_SYMBOL_VAL (XSYMBOL (sym), sym);
    }

  ptr = aref_addr (obarray, XFIXNUM (index));
  set_symbol_next (sym, SYMBOLP (*ptr) ? XSYMBOL (*ptr) : NULL);
  *ptr = sym;
  return sym;
}

/* Intern a symbol with name STRING in OBARRAY using bucket INDEX.  */

Lisp_Object
intern_driver (Lisp_Object string, Lisp_Object obarray, Lisp_Object index)
{
  return intern_sym (Fmake_symbol (string), obarray, index);
}

/* Intern the C string STR: return a symbol with that name,
   interned in the current obarray.  */

Lisp_Object
intern_1 (const char *str, ptrdiff_t len)
{
  Lisp_Object obarray = check_obarray (Vobarray);
  Lisp_Object tem = oblookup (obarray, str, len, len);

  return (SYMBOLP (tem) ? tem
	  /* The above `oblookup' was done on the basis of nchars==nbytes, so
	     the string has to be unibyte.  */
	  : intern_driver (make_unibyte_string (str, len),
			   obarray, tem));
}

Lisp_Object
intern_c_string_1 (const char *str, ptrdiff_t len)
{
  Lisp_Object obarray = check_obarray (Vobarray);
  Lisp_Object tem = oblookup (obarray, str, len, len);

  if (!SYMBOLP (tem))
    {
      /* Creating a non-pure string from a string literal not implemented yet.
	 We could just use make_string here and live with the extra copy.  */
      eassert (!NILP (Vpurify_flag));
      tem = intern_driver (make_pure_c_string (str, len), obarray, tem);
    }
  return tem;
}

static void
define_symbol (Lisp_Object sym, char const *str)
{
  ptrdiff_t len = strlen (str);
  Lisp_Object string = make_pure_c_string (str, len);
  init_symbol (sym, string);

  /* Qunbound is uninterned, so that it's not confused with any symbol
     'unbound' created by a Lisp program.  */
  if (! EQ (sym, Qunbound))
    {
      Lisp_Object bucket = oblookup (initial_obarray, str, len, len);
      eassert (FIXNUMP (bucket));
      intern_sym (sym, initial_obarray, bucket);
    }
}

DEFUN ("intern", Fintern, Sintern, 1, 2, 0,
       doc: /* Return the canonical symbol whose name is STRING.
If there is none, one is created by this function and returned.
A second optional argument specifies the obarray to use;
it defaults to the value of `obarray'.  */)
  (Lisp_Object string, Lisp_Object obarray)
{
  Lisp_Object tem;

  obarray = check_obarray (NILP (obarray) ? Vobarray : obarray);
  CHECK_STRING (string);

  tem = oblookup (obarray, SSDATA (string), SCHARS (string), SBYTES (string));
  if (!SYMBOLP (tem))
    tem = intern_driver (NILP (Vpurify_flag) ? string : Fpurecopy (string),
			 obarray, tem);
  return tem;
}

DEFUN ("intern-soft", Fintern_soft, Sintern_soft, 1, 2, 0,
       doc: /* Return the canonical symbol named NAME, or nil if none exists.
NAME may be a string or a symbol.  If it is a symbol, that exact
symbol is searched for.
A second optional argument specifies the obarray to use;
it defaults to the value of `obarray'.  */)
  (Lisp_Object name, Lisp_Object obarray)
{
  register Lisp_Object tem, string;

  if (NILP (obarray)) obarray = Vobarray;
  obarray = check_obarray (obarray);

  if (!SYMBOLP (name))
    {
      CHECK_STRING (name);
      string = name;
    }
  else
    string = SYMBOL_NAME (name);

  tem = oblookup (obarray, SSDATA (string), SCHARS (string), SBYTES (string));
  if (FIXNUMP (tem) || (SYMBOLP (name) && !EQ (name, tem)))
    return Qnil;
  else
    return tem;
}

DEFUN ("unintern", Funintern, Sunintern, 1, 2, 0,
       doc: /* Delete the symbol named NAME, if any, from OBARRAY.
The value is t if a symbol was found and deleted, nil otherwise.
NAME may be a string or a symbol.  If it is a symbol, that symbol
is deleted, if it belongs to OBARRAY--no other symbol is deleted.
OBARRAY, if nil, defaults to the value of the variable `obarray'.
usage: (unintern NAME OBARRAY)  */)
  (Lisp_Object name, Lisp_Object obarray)
{
  register Lisp_Object string, tem;
  size_t hash;

  if (NILP (obarray)) obarray = Vobarray;
  obarray = check_obarray (obarray);

  if (SYMBOLP (name))
    string = SYMBOL_NAME (name);
  else
    {
      CHECK_STRING (name);
      string = name;
    }

  tem = oblookup (obarray, SSDATA (string),
		  SCHARS (string),
		  SBYTES (string));
  if (FIXNUMP (tem))
    return Qnil;
  /* If arg was a symbol, don't delete anything but that symbol itself.  */
  if (SYMBOLP (name) && !EQ (name, tem))
    return Qnil;

  /* There are plenty of other symbols which will screw up the Emacs
     session if we unintern them, as well as even more ways to use
     `setq' or `fset' or whatnot to make the Emacs session
     unusable.  Let's not go down this silly road.  --Stef  */
  /* if (NILP (tem) || EQ (tem, Qt))
       error ("Attempt to unintern t or nil"); */

  XSYMBOL (tem)->u.s.interned = SYMBOL_UNINTERNED;

  hash = oblookup_last_bucket_number;

  if (EQ (AREF (obarray, hash), tem))
    {
      if (XSYMBOL (tem)->u.s.next)
	{
	  Lisp_Object sym;
	  XSETSYMBOL (sym, XSYMBOL (tem)->u.s.next);
	  ASET (obarray, hash, sym);
	}
      else
	ASET (obarray, hash, make_fixnum (0));
    }
  else
    {
      Lisp_Object tail, following;

      for (tail = AREF (obarray, hash);
	   XSYMBOL (tail)->u.s.next;
	   tail = following)
	{
	  XSETSYMBOL (following, XSYMBOL (tail)->u.s.next);
	  if (EQ (following, tem))
	    {
	      set_symbol_next (tail, XSYMBOL (following)->u.s.next);
	      break;
	    }
	}
    }

  return Qt;
}

/* Return the symbol in OBARRAY whose names matches the string
   of SIZE characters (SIZE_BYTE bytes) at PTR.
   If there is no such symbol, return the integer bucket number of
   where the symbol would be if it were present.

   Also store the bucket number in oblookup_last_bucket_number.  */

Lisp_Object
oblookup (Lisp_Object obarray, register const char *ptr, ptrdiff_t size, ptrdiff_t size_byte)
{
  size_t hash;
  size_t obsize;
  register Lisp_Object tail;
  Lisp_Object bucket, tem;

  obarray = check_obarray (obarray);
  /* This is sometimes needed in the middle of GC.  */
  obsize = gc_asize (obarray);
  hash = hash_string (ptr, size_byte) % obsize;
  bucket = AREF (obarray, hash);
  oblookup_last_bucket_number = hash;
  if (EQ (bucket, make_fixnum (0)))
    ;
  else if (!SYMBOLP (bucket))
    error ("Bad data in guts of obarray"); /* Like CADR error message.  */
  else
    for (tail = bucket; ; XSETSYMBOL (tail, XSYMBOL (tail)->u.s.next))
      {
	if (SBYTES (SYMBOL_NAME (tail)) == size_byte
	    && SCHARS (SYMBOL_NAME (tail)) == size
	    && !memcmp (SDATA (SYMBOL_NAME (tail)), ptr, size_byte))
	  return tail;
	else if (XSYMBOL (tail)->u.s.next == 0)
	  break;
      }
  XSETINT (tem, hash);
  return tem;
}

void
map_obarray (Lisp_Object obarray, void (*fn) (Lisp_Object, Lisp_Object), Lisp_Object arg)
{
  ptrdiff_t i;
  register Lisp_Object tail;
  CHECK_VECTOR (obarray);
  for (i = ASIZE (obarray) - 1; i >= 0; i--)
    {
      tail = AREF (obarray, i);
      if (SYMBOLP (tail))
	while (1)
	  {
	    (*fn) (tail, arg);
	    if (XSYMBOL (tail)->u.s.next == 0)
	      break;
	    XSETSYMBOL (tail, XSYMBOL (tail)->u.s.next);
	  }
    }
}

static void
mapatoms_1 (Lisp_Object sym, Lisp_Object function)
{
  call1 (function, sym);
}

DEFUN ("mapatoms", Fmapatoms, Smapatoms, 1, 2, 0,
       doc: /* Call FUNCTION on every symbol in OBARRAY.
OBARRAY defaults to the value of `obarray'.  */)
  (Lisp_Object function, Lisp_Object obarray)
{
  if (NILP (obarray)) obarray = Vobarray;
  obarray = check_obarray (obarray);

  map_obarray (obarray, mapatoms_1, function);
  return Qnil;
}

#define OBARRAY_SIZE 15121

void
init_obarray_once (void)
{
  Vobarray = make_vector (OBARRAY_SIZE, make_fixnum (0));
  initial_obarray = Vobarray;
  staticpro (&initial_obarray);

  for (int i = 0; i < ARRAYELTS (lispsym); i++)
    define_symbol (builtin_lisp_symbol (i), defsym_name[i]);

  DEFSYM (Qunbound, "unbound");

  DEFSYM (Qnil, "nil");
  SET_SYMBOL_VAL (XSYMBOL (Qnil), Qnil);
  make_symbol_constant (Qnil);
  XSYMBOL (Qnil)->u.s.declared_special = true;

  DEFSYM (Qt, "t");
  SET_SYMBOL_VAL (XSYMBOL (Qt), Qt);
  make_symbol_constant (Qt);
  XSYMBOL (Qt)->u.s.declared_special = true;

  /* Qt is correct even if not dumping.  loadup.el will set to nil at end.  */
  Vpurify_flag = Qt;

  DEFSYM (Qvariable_documentation, "variable-documentation");
}


void
defsubr (union Aligned_Lisp_Subr *aname)
{
  struct Lisp_Subr *sname = &aname->s;
  Lisp_Object sym, tem;
  sym = intern_c_string (sname->symbol_name);
  XSETPVECTYPE (sname, PVEC_SUBR);
  XSETSUBR (tem, sname);
  set_symbol_function (sym, tem);
}

#ifdef NOTDEF /* Use fset in subr.el now!  */
void
defalias (struct Lisp_Subr *sname, char *string)
{
  Lisp_Object sym;
  sym = intern (string);
  XSETSUBR (XSYMBOL (sym)->u.s.function, sname);
}
#endif /* NOTDEF */

/* Define an "integer variable"; a symbol whose value is forwarded to a
   C variable of type intmax_t.  Sample call (with "xx" to fool make-docfile):
   DEFxxVAR_INT ("emacs-priority", &emacs_priority, "Documentation");  */
void
defvar_int (struct Lisp_Intfwd const *i_fwd, char const *namestring)
{
  Lisp_Object sym = intern_c_string (namestring);
  XSYMBOL (sym)->u.s.declared_special = true;
  XSYMBOL (sym)->u.s.redirect = SYMBOL_FORWARDED;
  SET_SYMBOL_FWD (XSYMBOL (sym), i_fwd);
}

/* Similar but define a variable whose value is t if 1, nil if 0.  */
void
defvar_bool (struct Lisp_Boolfwd const *b_fwd, char const *namestring)
{
  Lisp_Object sym = intern_c_string (namestring);
  XSYMBOL (sym)->u.s.declared_special = true;
  XSYMBOL (sym)->u.s.redirect = SYMBOL_FORWARDED;
  SET_SYMBOL_FWD (XSYMBOL (sym), b_fwd);
  Vbyte_boolean_vars = Fcons (sym, Vbyte_boolean_vars);
}

/* Similar but define a variable whose value is the Lisp Object stored
   at address.  Two versions: with and without gc-marking of the C
   variable.  The nopro version is used when that variable will be
   gc-marked for some other reason, since marking the same slot twice
   can cause trouble with strings.  */
void
defvar_lisp_nopro (struct Lisp_Objfwd const *o_fwd, char const *namestring)
{
  Lisp_Object sym = intern_c_string (namestring);
  XSYMBOL (sym)->u.s.declared_special = true;
  XSYMBOL (sym)->u.s.redirect = SYMBOL_FORWARDED;
  SET_SYMBOL_FWD (XSYMBOL (sym), o_fwd);
}

void
defvar_lisp (struct Lisp_Objfwd const *o_fwd, char const *namestring)
{
  defvar_lisp_nopro (o_fwd, namestring);
  staticpro (o_fwd->objvar);
}

/* Similar but define a variable whose value is the Lisp Object stored
   at a particular offset in the current kboard object.  */

void
defvar_kboard (struct Lisp_Kboard_Objfwd const *ko_fwd, char const *namestring)
{
  Lisp_Object sym = intern_c_string (namestring);
  XSYMBOL (sym)->u.s.declared_special = true;
  XSYMBOL (sym)->u.s.redirect = SYMBOL_FORWARDED;
  SET_SYMBOL_FWD (XSYMBOL (sym), ko_fwd);
}

/* Check that the elements of lpath exist.  */

static void
load_path_check (Lisp_Object lpath)
{
  Lisp_Object path_tail;

  /* The only elements that might not exist are those from
     PATH_LOADSEARCH, EMACSLOADPATH.  Anything else is only added if
     it exists.  */
  for (path_tail = lpath; !NILP (path_tail); path_tail = XCDR (path_tail))
    {
      Lisp_Object dirfile;
      dirfile = Fcar (path_tail);
      if (STRINGP (dirfile))
        {
          dirfile = Fdirectory_file_name (dirfile);
          if (! file_accessible_directory_p (dirfile))
            dir_warning ("Lisp directory", XCAR (path_tail));
        }
    }
}

/* Return the default load-path, to be used if EMACSLOADPATH is unset.
   This does not include the standard site-lisp directories
   under the installation prefix (i.e., PATH_SITELOADSEARCH),
   but it does (unless no_site_lisp is set) include site-lisp
   directories in the source/build directories if those exist and we
   are running uninstalled.

   Uses the following logic:
   If !will_dump: Use PATH_LOADSEARCH.
   The remainder is what happens when dumping is about to happen:
   If dumping, just use PATH_DUMPLOADSEARCH.
   Otherwise use PATH_LOADSEARCH.

   If !initialized, then just return PATH_DUMPLOADSEARCH.
   If initialized:
   If Vinstallation_directory is not nil (ie, running uninstalled):
   If installation-dir/lisp exists and not already a member,
   we must be running uninstalled.  Reset the load-path
   to just installation-dir/lisp.  (The default PATH_LOADSEARCH
   refers to the eventual installation directories.  Since we
   are not yet installed, we should not use them, even if they exist.)
   If installation-dir/lisp does not exist, just add
   PATH_DUMPLOADSEARCH at the end instead.
   Add installation-dir/site-lisp (if !no_site_lisp, and exists
   and not already a member) at the front.
   If installation-dir != source-dir (ie running an uninstalled,
   out-of-tree build) AND install-dir/src/Makefile exists BUT
   install-dir/src/Makefile.in does NOT exist (this is a sanity
   check), then repeat the above steps for source-dir/lisp, site-lisp.  */

static Lisp_Object
load_path_default (void)
{
  if (will_dump_p ())
    /* PATH_DUMPLOADSEARCH is the lisp dir in the source directory.
       We used to add ../lisp (ie the lisp dir in the build
       directory) at the front here, but that should not be
       necessary, since in out of tree builds lisp/ is empty, save
       for Makefile.  */
    return decode_env_path (0, PATH_DUMPLOADSEARCH, 0);

  Lisp_Object lpath = Qnil;
  const char *normal = PATH_LOADSEARCH;
  const char *loadpath = NULL;

#ifdef HAVE_NS
  loadpath = ns_load_path ();
#endif

  lpath = decode_env_path (0, loadpath ? loadpath : normal, 0);

  if (!NILP (Vinstallation_directory))
    {
      Lisp_Object tem, tem1;

      /* Add to the path the lisp subdir of the installation
         dir, if it is accessible.  Note: in out-of-tree builds,
         this directory is empty save for Makefile.  */
      tem = Fexpand_file_name (build_string ("lisp"),
                               Vinstallation_directory);
      tem1 = Ffile_accessible_directory_p (tem);
      if (!NILP (tem1))
        {
          if (NILP (Fmember (tem, lpath)))
            {
              /* We are running uninstalled.  The default load-path
                 points to the eventual installed lisp directories.
                 We should not use those now, even if they exist,
                 so start over from a clean slate.  */
              lpath = list1 (tem);
            }
        }
      else
        /* That dir doesn't exist, so add the build-time
           Lisp dirs instead.  */
        {
          Lisp_Object dump_path =
            decode_env_path (0, PATH_DUMPLOADSEARCH, 0);
          lpath = nconc2 (lpath, dump_path);
        }

      /* Add site-lisp under the installation dir, if it exists.  */
      if (!no_site_lisp)
        {
          tem = Fexpand_file_name (build_string ("site-lisp"),
                                   Vinstallation_directory);
          tem1 = Ffile_accessible_directory_p (tem);
          if (!NILP (tem1))
            {
              if (NILP (Fmember (tem, lpath)))
                lpath = Fcons (tem, lpath);
            }
        }

      /* If Emacs was not built in the source directory,
         and it is run from where it was built, add to load-path
         the lisp and site-lisp dirs under that directory.  */

      if (NILP (Fequal (Vinstallation_directory, Vsource_directory)))
        {
          Lisp_Object tem2;

          tem = Fexpand_file_name (build_string ("src/Makefile"),
                                   Vinstallation_directory);
          tem1 = Ffile_exists_p (tem);

          /* Don't be fooled if they moved the entire source tree
             AFTER dumping Emacs.  If the build directory is indeed
             different from the source dir, src/Makefile.in and
             src/Makefile will not be found together.  */
          tem = Fexpand_file_name (build_string ("src/Makefile.in"),
                                   Vinstallation_directory);
          tem2 = Ffile_exists_p (tem);
          if (!NILP (tem1) && NILP (tem2))
            {
              tem = Fexpand_file_name (build_string ("lisp"),
                                       Vsource_directory);

              if (NILP (Fmember (tem, lpath)))
                lpath = Fcons (tem, lpath);

              if (!no_site_lisp)
                {
                  tem = Fexpand_file_name (build_string ("site-lisp"),
                                           Vsource_directory);
                  tem1 = Ffile_accessible_directory_p (tem);
                  if (!NILP (tem1))
                    {
                      if (NILP (Fmember (tem, lpath)))
                        lpath = Fcons (tem, lpath);
                    }
                }
            }
        } /* Vinstallation_directory != Vsource_directory */

    } /* if Vinstallation_directory */

  return lpath;
}

void
init_lread (void)
{
  /* First, set Vload_path.  */

  /* Ignore EMACSLOADPATH when dumping.  */
  bool use_loadpath = !will_dump_p ();

  if (use_loadpath && egetenv ("EMACSLOADPATH"))
    {
      Vload_path = decode_env_path ("EMACSLOADPATH", 0, 1);

      /* Check (non-nil) user-supplied elements.  */
      load_path_check (Vload_path);

      /* If no nils in the environment variable, use as-is.
         Otherwise, replace any nils with the default.  */
      if (! NILP (Fmemq (Qnil, Vload_path)))
        {
          Lisp_Object elem, elpath = Vload_path;
          Lisp_Object default_lpath = load_path_default ();

          /* Check defaults, before adding site-lisp.  */
          load_path_check (default_lpath);

          /* Add the site-lisp directories to the front of the default.  */
          if (!no_site_lisp && PATH_SITELOADSEARCH[0] != '\0')
            {
              Lisp_Object sitelisp;
              sitelisp = decode_env_path (0, PATH_SITELOADSEARCH, 0);
              if (! NILP (sitelisp))
                default_lpath = nconc2 (sitelisp, default_lpath);
            }

          Vload_path = Qnil;

          /* Replace nils from EMACSLOADPATH by default.  */
          while (CONSP (elpath))
            {
              elem = XCAR (elpath);
              elpath = XCDR (elpath);
              Vload_path = CALLN (Fappend, Vload_path,
				  NILP (elem) ? default_lpath : list1 (elem));
            }
        }                       /* Fmemq (Qnil, Vload_path) */
    }
  else
    {
      Vload_path = load_path_default ();

      /* Check before adding site-lisp directories.
         The install should have created them, but they are not
         required, so no need to warn if they are absent.
         Or we might be running before installation.  */
      load_path_check (Vload_path);

      /* Add the site-lisp directories at the front.  */
      if (!will_dump_p () && !no_site_lisp && PATH_SITELOADSEARCH[0] != '\0')
        {
          Lisp_Object sitelisp;
          sitelisp = decode_env_path (0, PATH_SITELOADSEARCH, 0);
          if (! NILP (sitelisp)) Vload_path = nconc2 (sitelisp, Vload_path);
        }
    }

  Vvalues = Qnil;

  load_in_progress = 0;
  Vload_file_name = Qnil;
  Vstandard_input = Qt;
  Vloads_in_progress = Qnil;
}

/* Print a warning that directory intended for use USE and with name
   DIRNAME cannot be accessed.  On entry, errno should correspond to
   the access failure.  Print the warning on stderr and put it in
   *Messages*.  */

void
dir_warning (char const *use, Lisp_Object dirname)
{
  static char const format[] = "Warning: %s '%s': %s\n";
  char *diagnostic = emacs_strerror (errno);
  fprintf (stderr, format, use, SSDATA (ENCODE_SYSTEM (dirname)), diagnostic);

  /* Don't log the warning before we've initialized!!  */
  if (initialized)
    {
      ptrdiff_t diaglen = strlen (diagnostic);
      AUTO_STRING_WITH_LEN (diag, diagnostic, diaglen);
      if (! NILP (Vlocale_coding_system))
	{
	  Lisp_Object s
	    = code_convert_string_norecord (diag, Vlocale_coding_system, false);
	  diagnostic = SSDATA (s);
	  diaglen = SBYTES (s);
	}
      USE_SAFE_ALLOCA;
      char *buffer = SAFE_ALLOCA (sizeof format - 3 * (sizeof "%s" - 1)
				  + strlen (use) + SBYTES (dirname) + diaglen);
      ptrdiff_t message_len = esprintf (buffer, format, use, SSDATA (dirname),
					diagnostic);
      message_dolog (buffer, message_len, 0, STRING_MULTIBYTE (dirname));
      SAFE_FREE ();
    }
}

void
syms_of_lread (void)
{
  defsubr (&Sread);
  defsubr (&Sread_from_string);
  defsubr (&Slread__substitute_object_in_subtree);
  defsubr (&Sintern);
  defsubr (&Sintern_soft);
  defsubr (&Sunintern);
  defsubr (&Sget_load_suffixes);
  defsubr (&Sload);
  defsubr (&Seval_buffer);
  defsubr (&Seval_region);
  defsubr (&Sread_char);
  defsubr (&Sread_char_exclusive);
  defsubr (&Sread_event);
  defsubr (&Sget_file_char);
  defsubr (&Smapatoms);
  defsubr (&Slocate_file_internal);

  DEFVAR_LISP ("obarray", Vobarray,
	       doc: /* Symbol table for use by `intern' and `read'.
It is a vector whose length ought to be prime for best results.
The vector's contents don't make sense if examined from Lisp programs;
to find all the symbols in an obarray, use `mapatoms'.  */);

  DEFVAR_LISP ("values", Vvalues,
	       doc: /* List of values of all expressions which were read, evaluated and printed.
Order is reverse chronological.  */);
  XSYMBOL (intern ("values"))->u.s.declared_special = false;

  DEFVAR_LISP ("standard-input", Vstandard_input,
	       doc: /* Stream for read to get input from.
See documentation of `read' for possible values.  */);
  Vstandard_input = Qt;

  DEFVAR_LISP ("read-with-symbol-positions", Vread_with_symbol_positions,
	       doc: /* If non-nil, add position of read symbols to `read-symbol-positions-list'.

If this variable is a buffer, then only forms read from that buffer
will be added to `read-symbol-positions-list'.
If this variable is t, then all read forms will be added.
The effect of all other values other than nil are not currently
defined, although they may be in the future.

The positions are relative to the last call to `read' or
`read-from-string'.  It is probably a bad idea to set this variable at
the toplevel; bind it instead.  */);
  Vread_with_symbol_positions = Qnil;

  DEFVAR_LISP ("read-symbol-positions-list", Vread_symbol_positions_list,
	       doc: /* A list mapping read symbols to their positions.
This variable is modified during calls to `read' or
`read-from-string', but only when `read-with-symbol-positions' is
non-nil.

Each element of the list looks like (SYMBOL . CHAR-POSITION), where
CHAR-POSITION is an integer giving the offset of that occurrence of the
symbol from the position where `read' or `read-from-string' started.

Note that a symbol will appear multiple times in this list, if it was
read multiple times.  The list is in the same order as the symbols
were read in.  */);
  Vread_symbol_positions_list = Qnil;

  DEFVAR_LISP ("read-circle", Vread_circle,
	       doc: /* Non-nil means read recursive structures using #N= and #N# syntax.  */);
  Vread_circle = Qt;

  DEFVAR_LISP ("load-path", Vload_path,
	       doc: /* List of directories to search for files to load.
Each element is a string (directory file name) or nil (meaning
`default-directory').
This list is consulted by the `require' function.
Initialized during startup as described in Info node `(elisp)Library Search'.
Use `directory-file-name' when adding items to this path.  However, Lisp
programs that process this list should tolerate directories both with
and without trailing slashes.  */);

  DEFVAR_LISP ("load-suffixes", Vload_suffixes,
	       doc: /* List of suffixes for Emacs Lisp files and dynamic modules.
This list includes suffixes for both compiled and source Emacs Lisp files.
This list should not include the empty string.
`load' and related functions try to append these suffixes, in order,
to the specified file name if a suffix is allowed or required.  */);
#ifdef HAVE_MODULES
  Vload_suffixes = list3 (build_pure_c_string (".elc"),
			  build_pure_c_string (".el"),
			  build_pure_c_string (MODULES_SUFFIX));
#else
  Vload_suffixes = list2 (build_pure_c_string (".elc"),
			  build_pure_c_string (".el"));
#endif
  DEFVAR_LISP ("module-file-suffix", Vmodule_file_suffix,
	       doc: /* Suffix of loadable module file, or nil if modules are not supported.  */);
#ifdef HAVE_MODULES
  Vmodule_file_suffix = build_pure_c_string (MODULES_SUFFIX);
#else
  Vmodule_file_suffix = Qnil;
#endif
  DEFVAR_LISP ("load-file-rep-suffixes", Vload_file_rep_suffixes,
	       doc: /* List of suffixes that indicate representations of \
the same file.
This list should normally start with the empty string.

Enabling Auto Compression mode appends the suffixes in
`jka-compr-load-suffixes' to this list and disabling Auto Compression
mode removes them again.  `load' and related functions use this list to
determine whether they should look for compressed versions of a file
and, if so, which suffixes they should try to append to the file name
in order to do so.  However, if you want to customize which suffixes
the loading functions recognize as compression suffixes, you should
customize `jka-compr-load-suffixes' rather than the present variable.  */);
  Vload_file_rep_suffixes = list1 (empty_unibyte_string);

  DEFVAR_BOOL ("load-in-progress", load_in_progress,
	       doc: /* Non-nil if inside of `load'.  */);
  DEFSYM (Qload_in_progress, "load-in-progress");

  DEFVAR_LISP ("after-load-alist", Vafter_load_alist,
	       doc: /* An alist of functions to be evalled when particular files are loaded.
Each element looks like (REGEXP-OR-FEATURE FUNCS...).

REGEXP-OR-FEATURE is either a regular expression to match file names, or
a symbol (a feature name).

When `load' is run and the file-name argument matches an element's
REGEXP-OR-FEATURE, or when `provide' is run and provides the symbol
REGEXP-OR-FEATURE, the FUNCS in the element are called.

An error in FUNCS does not undo the load, but does prevent calling
the rest of the FUNCS.  */);
  Vafter_load_alist = Qnil;

  DEFVAR_LISP ("load-history", Vload_history,
	       doc: /* Alist mapping loaded file names to symbols and features.
Each alist element should be a list (FILE-NAME ENTRIES...), where
FILE-NAME is the name of a file that has been loaded into Emacs.
The file name is absolute and true (i.e. it doesn't contain symlinks).
As an exception, one of the alist elements may have FILE-NAME nil,
for symbols and features not associated with any file.

The remaining ENTRIES in the alist element describe the functions and
variables defined in that file, the features provided, and the
features required.  Each entry has the form `(provide . FEATURE)',
`(require . FEATURE)', `(defun . FUNCTION)', `(autoload . SYMBOL)',
`(defface . SYMBOL)', `(define-type . SYMBOL)',
`(cl-defmethod METHOD SPECIALIZERS)', or `(t . SYMBOL)'.
Entries like `(t . SYMBOL)' may precede a `(defun . FUNCTION)' entry,
and mean that SYMBOL was an autoload before this file redefined it
as a function.  In addition, entries may also be single symbols,
which means that symbol was defined by `defvar' or `defconst'.

During preloading, the file name recorded is relative to the main Lisp
directory.  These file names are converted to absolute at startup.  */);
  Vload_history = Qnil;

  DEFVAR_LISP ("load-file-name", Vload_file_name,
	       doc: /* Full name of file being loaded by `load'.  */);
  Vload_file_name = Qnil;

  DEFVAR_LISP ("user-init-file", Vuser_init_file,
	       doc: /* File name, including directory, of user's initialization file.
If the file loaded had extension `.elc', and the corresponding source file
exists, this variable contains the name of source file, suitable for use
by functions like `custom-save-all' which edit the init file.
While Emacs loads and evaluates any init file, value is the real name
of the file, regardless of whether or not it has the `.elc' extension.  */);
  Vuser_init_file = Qnil;

  DEFVAR_LISP ("current-load-list", Vcurrent_load_list,
	       doc: /* Used for internal purposes by `load'.  */);
  Vcurrent_load_list = Qnil;

  DEFVAR_LISP ("load-read-function", Vload_read_function,
	       doc: /* Function used by `load' and `eval-region' for reading expressions.
Called with a single argument (the stream from which to read).
The default is to use the function `read'.  */);
  DEFSYM (Qread, "read");
  Vload_read_function = Qread;

  DEFVAR_LISP ("load-source-file-function", Vload_source_file_function,
	       doc: /* Function called in `load' to load an Emacs Lisp source file.
The value should be a function for doing code conversion before
reading a source file.  It can also be nil, in which case loading is
done without any code conversion.

If the value is a function, it is called with four arguments,
FULLNAME, FILE, NOERROR, NOMESSAGE.  FULLNAME is the absolute name of
the file to load, FILE is the non-absolute name (for messages etc.),
and NOERROR and NOMESSAGE are the corresponding arguments passed to
`load'.  The function should return t if the file was loaded.  */);
  Vload_source_file_function = Qnil;

  DEFVAR_BOOL ("load-force-doc-strings", load_force_doc_strings,
	       doc: /* Non-nil means `load' should force-load all dynamic doc strings.
This is useful when the file being loaded is a temporary copy.  */);
  load_force_doc_strings = 0;

  DEFVAR_BOOL ("load-convert-to-unibyte", load_convert_to_unibyte,
	       doc: /* Non-nil means `read' converts strings to unibyte whenever possible.
This is normally bound by `load' and `eval-buffer' to control `read',
and is not meant for users to change.  */);
  load_convert_to_unibyte = 0;

  DEFVAR_LISP ("source-directory", Vsource_directory,
	       doc: /* Directory in which Emacs sources were found when Emacs was built.
You cannot count on them to still be there!  */);
  Vsource_directory
    = Fexpand_file_name (build_string ("../"),
			 Fcar (decode_env_path (0, PATH_DUMPLOADSEARCH, 0)));

  DEFVAR_LISP ("preloaded-file-list", Vpreloaded_file_list,
	       doc: /* List of files that were preloaded (when dumping Emacs).  */);
  Vpreloaded_file_list = Qnil;

  DEFVAR_LISP ("byte-boolean-vars", Vbyte_boolean_vars,
	       doc: /* List of all DEFVAR_BOOL variables, used by the byte code optimizer.  */);
  Vbyte_boolean_vars = Qnil;

  DEFVAR_BOOL ("load-dangerous-libraries", load_dangerous_libraries,
	       doc: /* Non-nil means load dangerous compiled Lisp files.
Some versions of XEmacs use different byte codes than Emacs.  These
incompatible byte codes can make Emacs crash when it tries to execute
them.  */);
  load_dangerous_libraries = 0;

  DEFVAR_BOOL ("force-load-messages", force_load_messages,
	       doc: /* Non-nil means force printing messages when loading Lisp files.
This overrides the value of the NOMESSAGE argument to `load'.  */);
  force_load_messages = 0;

  DEFVAR_LISP ("bytecomp-version-regexp", Vbytecomp_version_regexp,
	       doc: /* Regular expression matching safe to load compiled Lisp files.
When Emacs loads a compiled Lisp file, it reads the first 512 bytes
from the file, and matches them against this regular expression.
When the regular expression matches, the file is considered to be safe
to load.  See also `load-dangerous-libraries'.  */);
  Vbytecomp_version_regexp
    = build_pure_c_string ("^;;;.\\(in Emacs version\\|bytecomp version FSF\\)");

  DEFSYM (Qlexical_binding, "lexical-binding");
  DEFVAR_LISP ("lexical-binding", Vlexical_binding,
	       doc: /* Whether to use lexical binding when evaluating code.
Non-nil means that the code in the current buffer should be evaluated
with lexical binding.
This variable is automatically set from the file variables of an
interpreted Lisp file read using `load'.  Unlike other file local
variables, this must be set in the first line of a file.  */);
  Vlexical_binding = Qnil;
  Fmake_variable_buffer_local (Qlexical_binding);

  DEFVAR_LISP ("eval-buffer-list", Veval_buffer_list,
	       doc: /* List of buffers being read from by calls to `eval-buffer' and `eval-region'.  */);
  Veval_buffer_list = Qnil;

  DEFVAR_LISP ("lread--unescaped-character-literals",
               Vlread_unescaped_character_literals,
               doc: /* List of deprecated unescaped character literals encountered by `read'.
For internal use only.  */);
  Vlread_unescaped_character_literals = Qnil;
  DEFSYM (Qlread_unescaped_character_literals,
          "lread--unescaped-character-literals");

  /* Defined in lisp/emacs-lisp/byte-run.el.  */
  DEFSYM (Qbyte_run_unescaped_character_literals_warning,
          "byte-run--unescaped-character-literals-warning");

  DEFVAR_BOOL ("load-prefer-newer", load_prefer_newer,
               doc: /* Non-nil means `load' prefers the newest version of a file.
This applies when a filename suffix is not explicitly specified and
`load' is trying various possible suffixes (see `load-suffixes' and
`load-file-rep-suffixes').  Normally, it stops at the first file
that exists unless you explicitly specify one or the other.  If this
option is non-nil, it checks all suffixes and uses whichever file is
newest.
Note that if you customize this, obviously it will not affect files
that are loaded before your customizations are read!  */);
  load_prefer_newer = 0;

  DEFVAR_BOOL ("force-new-style-backquotes", force_new_style_backquotes,
               doc: /* Non-nil means to always use the current syntax for backquotes.
If nil, `load' and `read' raise errors when encountering some
old-style variants of backquote and comma.  If non-nil, these
constructs are always interpreted as described in the Info node
`(elisp)Backquote', even if that interpretation is incompatible with
previous versions of Emacs.  Setting this variable to non-nil makes
Emacs compatible with the behavior planned for Emacs 28.  In Emacs 28,
this variable will become obsolete.  */);
  force_new_style_backquotes = false;

  /* Vsource_directory was initialized in init_lread.  */

  DEFSYM (Qcurrent_load_list, "current-load-list");
  DEFSYM (Qstandard_input, "standard-input");
  DEFSYM (Qread_char, "read-char");
  DEFSYM (Qget_file_char, "get-file-char");

  /* Used instead of Qget_file_char while loading *.elc files compiled
     by Emacs 21 or older.  */
  DEFSYM (Qget_emacs_mule_file_char, "get-emacs-mule-file-char");

  DEFSYM (Qload_force_doc_strings, "load-force-doc-strings");

  DEFSYM (Qbackquote, "`");
  DEFSYM (Qcomma, ",");
  DEFSYM (Qcomma_at, ",@");

  DEFSYM (Qinhibit_file_name_operation, "inhibit-file-name-operation");
  DEFSYM (Qascii_character, "ascii-character");
  DEFSYM (Qfunction, "function");
  DEFSYM (Qload, "load");
  DEFSYM (Qload_file_name, "load-file-name");
  DEFSYM (Qeval_buffer_list, "eval-buffer-list");
  DEFSYM (Qdir_ok, "dir-ok");
  DEFSYM (Qdo_after_load_evaluation, "do-after-load-evaluation");

  staticpro (&read_objects_map);
  read_objects_map = Qnil;
  staticpro (&read_objects_completed);
  read_objects_completed = Qnil;

  Vloads_in_progress = Qnil;
  staticpro (&Vloads_in_progress);

  DEFSYM (Qhash_table, "hash-table");
  DEFSYM (Qdata, "data");
  DEFSYM (Qtest, "test");
  DEFSYM (Qsize, "size");
  DEFSYM (Qpurecopy, "purecopy");
  DEFSYM (Qweakness, "weakness");
  DEFSYM (Qrehash_size, "rehash-size");
  DEFSYM (Qrehash_threshold, "rehash-threshold");

  DEFSYM (Qchar_from_name, "char-from-name");
}
