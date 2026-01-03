/* Lisp parsing and input streams.

Copyright (C) 1985-1989, 1993-1995, 1997-2026 Free Software Foundation,
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
#include <locale.h>
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
#include <fcntl.h>

#if !defined HAVE_ANDROID || defined ANDROID_STUBIFY	\
  || (__ANDROID_API__ < 9)

#define lread_fd	int
#define lread_fd_cmp(n) (fd == (n))
#define lread_fd_p	(fd >= 0)
#define lread_close	emacs_close
#define lread_fstat	sys_fstat
#define lread_read_quit	emacs_read_quit
#define lread_lseek	lseek

#define file_stream		FILE *
#define file_seek		fseek
#define file_stream_valid_p(p)	(p)
#define file_stream_close	emacs_fclose
#define file_stream_invalid	NULL
#define file_get_char		getc

#ifdef HAVE_FSEEKO
#define file_offset off_t
#define file_tell ftello
#else
#define file_offset long
#define file_tell ftell
#endif

#else

#include "android.h"

/* Use an Android file descriptor under Android instead, as this
   allows loading directly from asset files without loading each asset
   into memory and creating a separate file descriptor every time.

   Note that `struct android_fd_or_asset' as used here is different
   from that returned from `android_open_asset'; if fd.asset is NULL,
   then fd.fd is either a valid file descriptor or -1, meaning that
   the file descriptor is invalid.

   However, lread requires the ability to seek inside asset files,
   which is not provided under Android 2.2.  So when building for that
   particular system, fall back to the usual file descriptor-based
   code.  */

#define lread_fd	struct android_fd_or_asset
#define lread_fd_cmp(n)	(!fd.asset && fd.fd == (n))
#define lread_fd_p	(fd.asset || fd.fd >= 0)
#define lread_close	android_close_asset
#define lread_fstat	android_asset_fstat
#define lread_read_quit	android_asset_read_quit
#define lread_lseek	android_asset_lseek

/* The invalid file stream.  */

static struct android_fd_or_asset invalid_file_stream =
  {
    -1,
    NULL,
  };

#define file_stream		struct android_fd_or_asset
#define file_offset		off_t
#define file_tell(n)		android_asset_lseek (n, 0, SEEK_CUR)
#define file_seek		android_asset_lseek
#define file_stream_valid_p(p)	((p).asset || (p).fd >= 0)
#define file_stream_close	android_close_asset
#define file_stream_invalid	invalid_file_stream

/* Return a single character from the file input stream STREAM.
   Value and errors are the same as getc.  */

static int
file_get_char (file_stream stream)
{
  int c;
  char byte;
  ssize_t rc;

 retry:
  rc = android_asset_read (stream, &byte, 1);

  if (rc == 0)
    c = EOF;
  else if (rc == -1)
    {
      if (errno == EINTR)
	goto retry;
      else
	c = EOF;
    }
  else
    c = (unsigned char) byte;

  return c;
}

#define USE_ANDROID_ASSETS
#endif

#if IEEE_FLOATING_POINT
# include <ieee754.h>
# ifndef INFINITY
#  define INFINITY ((union ieee754_double) {.ieee = {.exponent = -1}}.d)
# endif
#else
# ifndef INFINITY
#  define INFINITY HUGE_VAL
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
  file_stream stream;

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

/* Position in object from which characters are being read by `readchar'.  */
static EMACS_INT readchar_offset;

struct saved_string {
  char *string;		        /* string in allocated buffer */
  ptrdiff_t size;		/* allocated size of buffer */
  ptrdiff_t length;		/* length of string in buffer */
  file_offset position;		/* position in file the string came from */
};

/* The last two strings skipped with #@ (most recent first).  */
static struct saved_string saved_strings[2];

/* A list of file names for files being loaded in Fload.  Used to
   check for recursive loads.  */

static Lisp_Object Vloads_in_progress;

static void readevalloop (Lisp_Object, struct infile *, Lisp_Object, bool,
                          Lisp_Object,
                          Lisp_Object, Lisp_Object);

static void build_load_history (Lisp_Object, bool);

static Lisp_Object oblookup_considering_shorthand (Lisp_Object, const char *,
						   ptrdiff_t, ptrdiff_t,
						   char **, ptrdiff_t *,
						   ptrdiff_t *);


/* When READCHARFUN is Qget_file_char or Qget_emacs_mule_file_char,
   we use this to keep an unread character because
   a file stream can't handle multibyte-char unreading.  The value -1
   means that there's no unread character.  */
static int unread_char = -1;

/* Representation of a source stream.
   FIXME: This is not nearly enough; there is a lot of static state that
   is not included.  */
typedef struct source {
  /* Read a character, -1 if at end of stream.  */
  int (*get) (struct source *src);
  /* Unread character C.  Only a single char can be unread at a given time.  */
  void (*unget) (struct source *src, int c);

  /* Object read from: buffer, marker, string, or function.  */
  Lisp_Object object;

  bool multibyte;	  /* whether `get' returns multibyte chars */

  /* For file sources, whether the encoding is the old emacs-mule.  */
  bool emacs_mule_encoding;
} source_t;

static int source_buffer_get (source_t *src);
static void source_buffer_unget (source_t *src, int c);
static int source_marker_get (source_t *src);
static void source_marker_unget (source_t *src, int c);
static int source_string_get (source_t *src);
static void source_string_unget (source_t *src, int c);
static int source_function_get (source_t *src);
static void source_function_unget (source_t *src, int c);
static int source_file_get (source_t *src);
static void source_file_unget (source_t *src, int c);

static void
init_source (source_t *src, Lisp_Object readcharfun)
{
  src->object = readcharfun;
  if (BUFFERP (readcharfun))
    {
      src->get = source_buffer_get;
      src->unget = source_buffer_unget;
      struct buffer *buf = XBUFFER (readcharfun);
      src->multibyte = (BUFFER_LIVE_P (buf)
			&& !NILP (BVAR (buf, enable_multibyte_characters)));
    }
  else if (MARKERP (readcharfun))
    {
      src->get = source_marker_get;
      src->unget = source_marker_unget;
      struct buffer *buf = XMARKER (readcharfun)->buffer;
      src->multibyte = (BUFFER_LIVE_P (buf)
			&& !NILP (BVAR (buf, enable_multibyte_characters)));
    }
  else if (STRINGP (readcharfun))
    {
      src->get = source_string_get;
      src->unget = source_string_unget;
      src->multibyte = STRING_MULTIBYTE (readcharfun);
    }
  else if (BASE_EQ (readcharfun, Qget_file_char)
	   || BASE_EQ (readcharfun, Qget_emacs_mule_file_char))
    {
      src->get = source_file_get;
      src->unget = source_file_unget;
      src->multibyte = true;
      src->emacs_mule_encoding = BASE_EQ (readcharfun,
					  Qget_emacs_mule_file_char);
      eassert (infile != NULL);
    }
  else
    {
      /* Assume callable (will signal error later if not).  */
      src->get = source_function_get;
      src->unget = source_function_unget;
      src->multibyte = true;
    }
}

static int
source_buffer_get (source_t *src)
{
  struct buffer *b = XBUFFER (src->object);
  if (!BUFFER_LIVE_P (b))
    return -1;

  ptrdiff_t pt_byte = BUF_PT_BYTE (b);
  if (pt_byte >= BUF_ZV_BYTE (b))
    return -1;

  int c;
  if (src->multibyte)
    {
      unsigned char *p = BUF_BYTE_ADDRESS (b, pt_byte);
      int clen;
      c = string_char_and_length (p, &clen);
      pt_byte += clen;
    }
  else
    {
      c = BUF_FETCH_BYTE (b, pt_byte);
      if (!ASCII_CHAR_P (c))
	c = BYTE8_TO_CHAR (c);
      pt_byte++;
    }
  SET_BUF_PT_BOTH (b, BUF_PT (b) + 1, pt_byte);
  return c;
}

static void
source_buffer_unget (source_t *src, int c)
{
  struct buffer *b = XBUFFER (src->object);
  ptrdiff_t charpos = BUF_PT (b);
  ptrdiff_t bytepos = BUF_PT_BYTE (b);
  bytepos -= src->multibyte ? buf_prev_char_len (b, bytepos) : 1;
  SET_BUF_PT_BOTH (b, charpos - 1, bytepos);
}

static int
source_marker_get (source_t *src)
{
  Lisp_Object m = src->object;
  struct buffer *b = XMARKER (m)->buffer;
  ptrdiff_t bytepos = marker_byte_position (m);
  if (bytepos >= BUF_ZV_BYTE (b))
    return -1;

  int c;
  if (src->multibyte)
    {
      unsigned char *p = BUF_BYTE_ADDRESS (b, bytepos);
      int clen;
      c = string_char_and_length (p, &clen);
      bytepos += clen;
    }
  else
    {
      c = BUF_FETCH_BYTE (b, bytepos);
      if (!ASCII_CHAR_P (c))
	c = BYTE8_TO_CHAR (c);
      bytepos++;
    }
  XMARKER (m)->bytepos = bytepos;
  XMARKER (m)->charpos++;
  return c;
}

static void
source_marker_unget (source_t *src, int c)
{
  Lisp_Object m = src->object;
  struct buffer *b = XMARKER (m)->buffer;
  ptrdiff_t bytepos = XMARKER (m)->bytepos;
  XMARKER (m)->charpos--;
  bytepos -= src->multibyte ? buf_prev_char_len (b, bytepos) : 1;
  XMARKER (m)->bytepos = bytepos;
}

static int
source_string_get (source_t *src)
{
  if (read_from_string_index >= read_from_string_limit)
    return -1;
  Lisp_Object s = src->object;
  int c;
  if (src->multibyte)
    c = fetch_string_char_advance_no_check
      (s, &read_from_string_index, &read_from_string_index_byte);
  else
    {
      c = SREF (s, read_from_string_index_byte);
      if (!ASCII_CHAR_P (c))
	c = BYTE8_TO_CHAR (c);
      read_from_string_index++;
      read_from_string_index_byte++;
    }
  return c;
}

static void
source_string_unget (source_t *src, int c)
{
  read_from_string_index--;
  read_from_string_index_byte = string_char_to_byte (src->object,
						     read_from_string_index);
}

static int readbyte_from_file (void);
static void unreadbyte_from_file (unsigned char);

static int read_emacs_mule_char (source_t *src, int c);

static int
source_file_get (source_t *src)
{
  if (unread_char >= 0)
    {
      int c = unread_char;
      unread_char = -1;
      return c;
    }

  int c = readbyte_from_file ();
  if (c < 0)
    return c;
  if (ASCII_CHAR_P (c))
    return c;
  if (src->emacs_mule_encoding)
    return read_emacs_mule_char (src, c);
  int i = 0;
  unsigned char buf[MAX_MULTIBYTE_LENGTH];
  buf[i++] = c;
  int len = BYTES_BY_CHAR_HEAD (c);
  while (i < len)
    {
      buf[i++] = c = readbyte_from_file ();
      if (c < 0 || ! TRAILING_CODE_P (c))
	{
	  for (i -= c < 0; 0 < --i; )
	    unreadbyte_from_file (buf[i]);
	  return BYTE8_TO_CHAR (buf[0]);
	}
    }
  return STRING_CHAR (buf);
}

static void
source_file_unget (source_t *src, int c)
{
  unread_char = c;
}

static int
source_function_get (source_t *src)
{
  Lisp_Object x = call0 (src->object);
  return CHARACTERP (x) ? XFIXNUM (x) : -1;
}

static void
source_function_unget (source_t *src, int c)
{
  calln (src->object, make_fixnum (c));
}

/* Read a character from SRC.  */
static inline int
readchar (source_t *src)
{
  readchar_offset++;
  return src->get (src);
}

/* Unread C from (to?) SRC.  Only a single char can be unread at a time.  */
static inline void
unreadchar (source_t *src, int c)
{
  readchar_offset--;
  /* Don't back up the pointer if we're unreading the end-of-input mark,
     since readchar didn't advance it when we read it.  */
  if (c == -1)
    return;
  src->unget (src, c);
}

static bool
from_file_p (source_t *source)
{
  return source->get == source_file_get;
}

static bool
from_buffer_p (source_t *source)
{
  return source->get == source_buffer_get;
}

static void
skip_dyn_bytes (source_t *source, ptrdiff_t n)
{
  if (from_file_p (source))
    {
      block_input ();		/* FIXME: Not sure if it's needed.  */
      file_seek (infile->stream, n - infile->lookahead, SEEK_CUR);
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
	c = readchar (source);
      } while (c >= 0 && c != '\037');
    }
}

static void
skip_dyn_eof (source_t *source)
{
  if (from_file_p (source))
    {
      block_input ();		/* FIXME: Not sure if it's needed.  */
      file_seek (infile->stream, 0, SEEK_END);
      unblock_input ();
      infile->lookahead = 0;
    }
  else
    while (readchar (source) >= 0);
}

/* Read a byte from the current input file.  Return -1 at end of file.  */
static int
readbyte_from_file (void)
{
  if (infile->lookahead)
    return infile->buf[--infile->lookahead];

  int c;
  file_stream instream = infile->stream;

  block_input ();

#if !defined USE_ANDROID_ASSETS

  /* Interrupted reads have been observed while reading over the network.  */
  while ((c = getc (instream)) == EOF && errno == EINTR && ferror (instream))
    {
      unblock_input ();
      maybe_quit ();
      block_input ();
      clearerr (instream);
    }

#else

  {
    char byte;
    ssize_t rc;

  retry:
    rc = android_asset_read (instream, &byte, 1);

    if (rc == 0)
      c = EOF;
    else if (rc == -1)
      {
	if (errno == EINTR)
	  {
	    unblock_input ();
	    maybe_quit ();
	    block_input ();
	    goto retry;
	  }
	else
	  c = EOF;
      }
    else
      c = (unsigned char) byte;
  }

#endif

  unblock_input ();

  return (c == EOF ? -1 : c);
}

static void
unreadbyte_from_file (unsigned char c)
{
  eassert (infile->lookahead < sizeof infile->buf);
  infile->buf[infile->lookahead++] = c;
}

/* Signal Qinvalid_read_syntax error.
   S is error string of length N (if > 0)  */

static AVOID
invalid_syntax_lisp (Lisp_Object s, source_t *source)
{
  if (from_buffer_p (source))
    {
      Lisp_Object buffer = source->object;
      /* Get the line/column in the buffer.  */
      specpdl_ref count = SPECPDL_INDEX ();
      record_unwind_protect_excursion ();
      set_buffer_internal (XBUFFER (buffer));
      ptrdiff_t line = count_lines (BEGV_BYTE, PT_BYTE) + 1;
      ptrdiff_t column = current_column ();
      unbind_to (count, Qnil);

      xsignal (Qinvalid_read_syntax,
	       list3 (s, make_fixnum (line), make_fixnum (column)));
    }
  else
    xsignal1 (Qinvalid_read_syntax, s);
}

static AVOID
invalid_syntax (const char *s, source_t *source)
{
  invalid_syntax_lisp (build_string (s), source);
}


/* Read one non-ASCII character from INFILE.  The character is
   encoded in `emacs-mule' and the first byte is already read in
   C.  */

static int
read_emacs_mule_char (source_t *src, int c)
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
      buf[i++] = c = readbyte_from_file ();
      if (c < 0xA0)
	{
	  for (i -= c < 0; 0 < --i; )
	    unreadbyte_from_file (buf[i]);
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
    invalid_syntax ("invalid multibyte form", src);
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
                                        Lisp_Object, bool);
static Lisp_Object read0 (source_t *source, bool locate_syms);

static Lisp_Object substitute_object_recurse (struct subst *, Lisp_Object);
static void substitute_in_interval (INTERVAL, void *);


typedef enum {
  Cookie_None,			/* no cookie */
  Cookie_Dyn,			/* explicit dynamic binding */
  Cookie_Lex			/* explicit lexical binding */
} lexical_cookie_t;

/* Determine if the lisp code read using READCHARFUN defines a
   `lexical-binding' file variable return its value.
   After returning, the stream is positioned following the first line,
   if it is a comment or #! line, otherwise nothing is read.  */

static lexical_cookie_t
lisp_file_lexical_cookie (Lisp_Object readcharfun)
{
  source_t source;
  init_source (&source, readcharfun);

  int ch = readchar (&source);

  if (ch == '#')
    {
      ch = readchar (&source);
      if (ch != '!')
        {
          unreadchar (&source, ch);
          unreadchar (&source, '#');
          return Cookie_None;
        }
      while (ch != '\n' && ch != EOF)
        ch = readchar (&source);
      if (ch == '\n') ch = readchar (&source);
      /* It is OK to leave the position after a #! line, since
	 that is what read0 does.  */
    }

  if (ch != ';')
    /* The first line isn't a comment, just give up.  */
    {
      unreadchar (&source, ch);
      return Cookie_None;
    }
  else
    /* Look for an appropriate file-variable in the first line.  */
    {
      lexical_cookie_t rv = Cookie_None;
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
	  ch = readchar (&source);
	  UPDATE_BEG_END_STATE (ch);
	}
      while (!in_file_vars && ch != '\n' && ch != EOF);

      while (in_file_vars)
	{
	  char var[100], val[100];
	  unsigned i;

	  ch = readchar (&source);

	  /* Read a variable name.  */
	  while (ch == ' ' || ch == '\t')
	    ch = readchar (&source);

	  i = 0;
	  beg_end_state = NOMINAL;
	  while (ch != ':' && ch != '\n' && ch != EOF && in_file_vars)
	    {
	      if (i < sizeof var - 1)
		var[i++] = ch;
	      UPDATE_BEG_END_STATE (ch);
	      ch = readchar (&source);
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
	      ch = readchar (&source);

	      while (ch == ' ' || ch == '\t')
		ch = readchar (&source);

	      i = 0;
	      beg_end_state = NOMINAL;
	      while (ch != ';' && ch != '\n' && ch != EOF && in_file_vars)
		{
		  if (i < sizeof val - 1)
		    val[i++] = ch;
		  UPDATE_BEG_END_STATE (ch);
		  ch = readchar (&source);
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
		  rv = strcmp (val, "nil") != 0 ? Cookie_Lex : Cookie_Dyn;
		  break;
		}
	    }
	}

      while (ch != '\n' && ch != EOF)
	ch = readchar (&source);

      return rv;
    }
}

/* Value is a version number of byte compiled code if the file
   associated with file descriptor FD is a compiled Lisp file that's
   safe to load.  Only files compiled with Emacs can be loaded.  */

static int
safe_to_load_version (Lisp_Object file, lread_fd fd)
{
  struct stat st;
  char buf[512];
  int nbytes, i;
  int version = 1;

  /* If the file is not regular, then we cannot safely seek it.
     Assume that it is not safe to load as a compiled file.  */
  if (lread_fstat (fd, &st) == 0 && !S_ISREG (st.st_mode))
    return 0;

  /* Read the first few bytes from the file, and look for a line
     specifying the byte compiler version used.  */
  nbytes = lread_read_quit (fd, buf, sizeof buf);
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

  if (lread_lseek (fd, 0, SEEK_SET) < 0)
    report_file_error ("Seeking to start of file", file);

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

static void
load_warn_unescaped_character_literals (Lisp_Object file)
{
  Lisp_Object function
    = Fsymbol_function (Qbyte_run_unescaped_character_literals_warning);
  /* If byte-run.el is being loaded,
     `byte-run--unescaped-character-literals-warning' isn't yet
     defined.  Since it'll be byte-compiled later, ignore potential
     unescaped character literals. */
  Lisp_Object warning = NILP (function) ? Qnil : call0 (function);
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
	{
	  Lisp_Object ext = XCAR (exts);
#ifdef HAVE_MODULES
	  if (SCHARS (ext) > 0
	      && (suffix_p (suffix, MODULES_SUFFIX)
# ifdef MODULES_SECONDARY_SUFFIX
		  || suffix_p (suffix, MODULES_SECONDARY_SUFFIX)
# endif
		 )
	      && !NILP (Fmember (ext, Fsymbol_value (
					Qjka_compr_load_suffixes))))
	    continue;
#endif
	  lst = Fcons (concat2 (suffix, ext), lst);
	}
    }
  return Fnreverse (lst);
}

/* Return true if STRING ends with SUFFIX.  */
bool
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
  file_stream_close (infile->stream);
  infile = prev_infile;
}

/* Compute the filename we want in `load-history' and `load-file-name'.  */

static Lisp_Object
compute_found_effective (Lisp_Object found)
{
  /* Reconstruct the .elc filename.  */
  Lisp_Object src_name =
    Fgethash (Ffile_name_nondirectory (found), Vcomp_eln_to_el_h, Qnil);

  if (NILP (src_name))
    /* Manual eln load.  */
    return found;

  if (suffix_p (src_name, "el.gz"))
    src_name = Fsubstring (src_name, make_fixnum (0), make_fixnum (-3));
  return concat2 (src_name, build_string ("c"));
}

static void
loadhist_initialize (Lisp_Object filename)
{
  eassert (STRINGP (filename) || NILP (filename));
  specbind (Qcurrent_load_list, Fcons (filename, Qnil));
}

#ifdef USE_ANDROID_ASSETS

/* Like `close_file_unwind'.  However, PTR is a pointer to an Android
   file descriptor instead of a system file descriptor.  */

static void
close_file_unwind_android_fd (void *ptr)
{
  struct android_fd_or_asset *fd;

  fd = ptr;
  android_close_asset (*fd);
}

#endif

static Lisp_Object
get_lexical_binding (Lisp_Object stream, Lisp_Object from)
{
  lexical_cookie_t lexc = lisp_file_lexical_cookie (stream);
  return ((lexc == Cookie_Lex
	   ? Qt
	   : (lexc == Cookie_Dyn
	      ? Qnil
	      : ((NILP (from)	/* Loading a byte-compiled file.  */
		  || NILP (Vinternal__get_default_lexical_binding_function))
		 ? Fdefault_toplevel_value (Qlexical_binding)
		 : calln (Vinternal__get_default_lexical_binding_function,
			  from)))));
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
  file_stream stream UNINIT;
  lread_fd fd;
#ifdef USE_ANDROID_ASSETS
  int rc;
  void *asset;
#endif
  specpdl_ref fd_index UNINIT;
  specpdl_ref count = SPECPDL_INDEX ();
  Lisp_Object found, efound, hist_file_name;
  /* True means we printed the ".el is newer" message.  */
  bool newer = 0;
  /* True means we are loading a compiled file.  */
  bool compiled = 0;
  Lisp_Object handler;
  const char *fmode = "r" FOPEN_TEXT;
  int version;

  CHECK_STRING (file);

  /* If file name is magic, call the handler.  */
  handler = Ffind_file_name_handler (file, Qload);
  if (!NILP (handler))
    return
      calln (handler, Qload, file, noerror, nomessage, nosuffix, must_suffix);

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

  bool no_native = suffix_p (file, ".elc");

  /* Avoid weird lossage with null string as arg,
     since it would try to load a directory as a Lisp file.  */
  if (SCHARS (file) == 0)
    {
#if !defined USE_ANDROID_ASSETS
      fd = -1;
#else
      fd.asset = NULL;
      fd.fd = -1;
#endif
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
#ifdef MODULES_SECONDARY_SUFFIX
              || suffix_p (file, MODULES_SECONDARY_SUFFIX)
#endif
#endif
#ifdef HAVE_NATIVE_COMP
              || suffix_p (file, NATIVE_ELISP_SUFFIX)
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

      Lisp_Object load_path = Vload_path;
      if (FUNCTIONP (Vload_path_filter_function))
	load_path = calln (Vload_path_filter_function, load_path, file, suffixes);

#if !defined USE_ANDROID_ASSETS
      fd = openp (load_path, file, suffixes, &found, Qnil,
		  load_prefer_newer, no_native, NULL);
#else
      asset = NULL;
      rc = openp (load_path, file, suffixes, &found, Qnil,
		  load_prefer_newer, no_native, &asset);
      fd.fd = rc;
      fd.asset = asset;

      /* fd.asset will be non-NULL if this is actually an asset
	 file.  */
#endif
    }

  if (lread_fd_cmp (-1))
    {
      if (NILP (noerror))
	report_file_error ("Cannot open load file", file);
      return Qnil;
    }

  /* Tell startup.el whether or not we found the user's init file.  */
  if (EQ (Qt, Vuser_init_file))
    Vuser_init_file = found;

  /* If FD is -2, that means openp found a magic file.  */
  if (lread_fd_cmp (-2))
    {
      if (NILP (Fequal (found, file)))
	/* If FOUND is a different file name from FILE,
	   find its handler even if we have already inhibited
	   the `load' operation on FILE.  */
	handler = Ffind_file_name_handler (found, Qt);
      else
	handler = Ffind_file_name_handler (found, Qload);
      if (! NILP (handler))
	return calln (handler, Qload, found, noerror, nomessage, Qt);
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

#if !defined USE_ANDROID_ASSETS
  if (0 <= fd)
    {
      fd_index = SPECPDL_INDEX ();
      record_unwind_protect_int (close_file_unwind, fd);
    }
#else
  if (fd.asset || fd.fd >= 0)
    {
      /* Use a different kind of unwind_protect here.  */
      fd_index = SPECPDL_INDEX ();
      record_unwind_protect_ptr (close_file_unwind_android_fd,
				 &fd);
    }
#endif

#ifdef HAVE_MODULES
  bool is_module =
    suffix_p (found, MODULES_SUFFIX)
#ifdef MODULES_SECONDARY_SUFFIX
    || suffix_p (found, MODULES_SECONDARY_SUFFIX)
#endif
    ;
#else
  bool is_module = false;
#endif

#ifdef HAVE_NATIVE_COMP
  bool is_native_elisp = suffix_p (found, NATIVE_ELISP_SUFFIX);
#else
  bool is_native_elisp = false;
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

  Lisp_Object found_eff =
    is_native_elisp
    ? compute_found_effective (found)
    : found;

  hist_file_name = (! NILP (Vpurify_flag)
                    ? concat2 (Ffile_name_directory (file),
                               Ffile_name_nondirectory (found_eff))
                    : found_eff);

  version = -1;

  /* Check for the presence of unescaped character literals and warn
     about them. */
  specbind (Qlread_unescaped_character_literals, Qnil);
  record_unwind_protect (load_warn_unescaped_character_literals, file);

  bool is_elc = suffix_p (found, ".elc");
  if (is_elc
      /* version = 1 means the file is empty, in which case we can
	 treat it as not byte-compiled.  */
      || (lread_fd_p
	  && (version = safe_to_load_version (file, fd)) > 1))
    /* Load .elc files directly, but not when they are
       remote and have no handler!  */
    {
      if (!lread_fd_cmp (-2))
	{
	  struct stat s1, s2;
	  int result;

	  struct timespec epoch_timespec = {(time_t)0, 0}; /* 1970-01-01T00:00 UTC */
	  if (version < 0 && !(version = safe_to_load_version (file, fd)))
	    error ("File `%s' was not compiled in Emacs", SDATA (found));

	  compiled = 1;

	  efound = ENCODE_FILE (found);
	  fmode = "r" FOPEN_BINARY;

          /* openp already checked for newness, no point doing it again.
             FIXME would be nice to get a message when openp
             ignores suffix order due to load_prefer_newer.  */
          if (!load_prefer_newer && is_elc)
            {
	      result = emacs_fstatat (AT_FDCWD, SSDATA (efound), &s1, 0);
              if (result == 0)
                {
                  SSET (efound, SBYTES (efound) - 1, 0);
		  result = emacs_fstatat (AT_FDCWD, SSDATA (efound), &s2, 0);
                  SSET (efound, SBYTES (efound) - 1, 'c');
                }

              if (result == 0
                  && timespec_cmp (get_stat_mtime (&s1), get_stat_mtime (&s2)) < 0)
                {
                  /* Make the progress messages mention that source is newer.  */
                  newer = 1;

                  /* If we won't print another message, mention this anyway.  */
                  if (!NILP (nomessage) && !force_load_messages
		      /* We don't want this message during
			 bootstrapping for the "compile-first" .elc
			 files, which have had their timestamps set to
			 the epoch.  See bug #58224.  */
		      && timespec_cmp (get_stat_mtime (&s1), epoch_timespec))
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
  else if (!is_module && !is_native_elisp)
    {
      /* We are loading a source file (*.el).  */
      if (!NILP (Vload_source_file_function))
	{
	  Lisp_Object val;

	  if (lread_fd_p)
	    {
	      lread_close (fd);
	      clear_unwind_protect (fd_index);
	    }
	  val = calln (Vload_source_file_function, found, hist_file_name,
		       NILP (noerror) ? Qnil : Qt,
		       (NILP (nomessage) || force_load_messages) ? Qnil : Qt);
	  return unbind_to (count, val);
	}
    }

  if (!lread_fd_p)
    {
      /* We somehow got here with fd == -2, meaning the file is deemed
	 to be remote.  Don't even try to reopen the file locally;
	 just force a failure.  */
      stream = file_stream_invalid;
      errno = EINVAL;
    }
  else if (!is_module && !is_native_elisp)
    {
#ifdef WINDOWSNT
      emacs_close (fd);
      clear_unwind_protect (fd_index);
      efound = ENCODE_FILE (found);
      stream = emacs_fopen (SSDATA (efound), fmode);
#else
#if !defined USE_ANDROID_ASSETS
      stream = emacs_fdopen (fd, fmode);
#else
      /* Android systems use special file descriptors which can point
	 into compressed data and double as file streams.  FMODE is
	 unused.  */
      ((void) fmode);
      stream = fd;
#endif
#endif
    }

  /* Declare here rather than inside the else-part because the storage
     might be accessed by the unbind_to call below.  */
  struct infile input;

  if (is_module || is_native_elisp)
    {
      /* `module-load' uses the file name, so we can close the stream
         now.  */
      if (lread_fd_p)
        {
          lread_close (fd);
          clear_unwind_protect (fd_index);
        }
    }
  else
    {
      if (!file_stream_valid_p (stream))
        report_file_error ("Opening stdio stream", file);
      set_unwind_protect_ptr (fd_index, close_infile_unwind, infile);
      input.stream = stream;
      input.lookahead = 0;
      infile = &input;
      unread_char = -1;
    }

  if (! NILP (Vpurify_flag))
    Vpreloaded_file_list = Fcons (file, Vpreloaded_file_list);

  if (NILP (nomessage) || force_load_messages)
    {
      if (is_module)
        message_with_string ("Loading %s (module)...", file, 1);
      else if (is_native_elisp)
        message_with_string ("Loading %s (native-compiled elisp)...", file, 1);
      else if (!compiled)
	message_with_string ("Loading %s (source)...", file, 1);
      else if (newer)
	message_with_string ("Loading %s (compiled; note, source file is newer)...",
		 file, 1);
      else /* The typical case; compiled file newer than source file.  */
	message_with_string ("Loading %s...", file, 1);
    }

  specbind (Qload_file_name, hist_file_name);
  specbind (Qload_true_file_name, found);
  specbind (Qinhibit_file_name_operation, Qnil);
  specbind (Qload_in_progress, Qt);

  if (is_module)
    {
#ifdef HAVE_MODULES
      loadhist_initialize (found);
      Fmodule_load (found);
      build_load_history (found, true);
#else
      /* This cannot happen.  */
      emacs_abort ();
#endif
    }
  else if (is_native_elisp)
    {
#ifdef HAVE_NATIVE_COMP
      loadhist_initialize (hist_file_name);
      Fnative_elisp_load (found, Qnil);
      build_load_history (hist_file_name, true);
#else
      /* This cannot happen.  */
      emacs_abort ();
#endif

    }
  else
    {
      Fset (Qlexical_binding,
	    get_lexical_binding (Qget_file_char, compiled ? Qnil : file));

      if (! version || version >= 22)
        readevalloop (Qget_file_char, &input, hist_file_name,
                      0, Qnil, Qnil, Qnil);
      else
        {
          /* We can't handle a file which was compiled with
             byte-compile-dynamic by older version of Emacs.  */
          specbind (Qload_force_doc_strings, Qt);
          readevalloop (Qget_emacs_mule_file_char, &input, hist_file_name,
                        0, Qnil, Qnil, Qnil);
        }
    }
  unbind_to (count, Qnil);

  /* Run any eval-after-load forms for this file.  */
  if (!NILP (Ffboundp (Qdo_after_load_evaluation)))
    calln (Qdo_after_load_evaluation, hist_file_name);

  for (int i = 0; i < ARRAYELTS (saved_strings); i++)
    {
      xfree (saved_strings[i].string);
      saved_strings[i].string = NULL;
      saved_strings[i].size = 0;
    }

  /* The "...done" messages are shown only in interactive mode, because
     the echo-area can display only the last message, and we want to
     avoid the impression that the load is still in progress.  */
  if (!noninteractive && (NILP (nomessage) || force_load_messages))
    {
      if (is_module)
        message_with_string ("Loading %s (module)...done", file, 1);
      else if (is_native_elisp)
	message_with_string ("Loading %s (native-compiled elisp)...done", file, 1);
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
  specpdl_ref count = SPECPDL_INDEX ();
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
  int fd = openp (path, filename, suffixes, &file, predicate, false, true,
		  NULL);
  if (NILP (predicate) && fd >= 0)
    emacs_close (fd);
  return file;
}

#ifdef HAVE_NATIVE_COMP
static bool
maybe_swap_for_eln1 (Lisp_Object src_name, Lisp_Object eln_name,
		     Lisp_Object *filename, int *fd, struct timespec mtime)
{
  struct stat eln_st;
  int eln_fd = emacs_open (SSDATA (ENCODE_FILE (eln_name)), O_RDONLY, 0);

  if (eln_fd > 0)
    {
      if (sys_fstat (eln_fd, &eln_st) || S_ISDIR (eln_st.st_mode))
	emacs_close (eln_fd);
      else
	{
	  struct timespec eln_mtime = get_stat_mtime (&eln_st);
	  if (timespec_cmp (eln_mtime, mtime) >= 0)
	    {
	      emacs_close (*fd);
	      *fd = eln_fd;
	      *filename = eln_name;
	      /* Store the eln -> el relation.  */
	      Fputhash (Ffile_name_nondirectory (eln_name),
			src_name, Vcomp_eln_to_el_h);
	      return true;
	    }
	  else
	    emacs_close (eln_fd);
	}
    }

  return false;
}
#endif

/* Look for a suitable .eln file to be loaded in place of FILENAME.
   If found replace the content of FILENAME and FD. */

static void
maybe_swap_for_eln (bool no_native, Lisp_Object *filename, int *fd,
		    struct timespec mtime)
{
#ifdef HAVE_NATIVE_COMP

  if (no_native
      || load_no_native)
    Fputhash (*filename, Qt, V_comp_no_native_file_h);
  else
    Fremhash (*filename, V_comp_no_native_file_h);

  if (no_native
      || load_no_native
      || !suffix_p (*filename, ".elc"))
    return;

  /* Search eln in the eln-cache directories.  */
  Lisp_Object eln_path_tail = Vnative_comp_eln_load_path;
  Lisp_Object src_name =
    Fsubstring (*filename, Qnil, make_fixnum (-1));
  if (NILP (Ffile_exists_p (src_name)))
    {
      src_name = concat2 (src_name, build_string (".gz"));
      if (NILP (Ffile_exists_p (src_name)))
	{
	  if (!NILP (find_symbol_value (
		       Qnative_comp_warning_on_missing_source)))
	    {
	      /* If we have an installation without any .el files,
		 there's really no point in giving a warning here,
		 because that will trigger a cascade of warnings.  So
		 just do a sanity check and refuse to do anything if we
		 can't find even central .el files.  */
	      if (NILP (Flocate_file_internal (build_string ("simple.el"),
					       Vload_path,
					       Qnil, Qnil)))
		return;
	      Vdelayed_warnings_list
		= Fcons (list2
			 (Qnative_compiler,
			  CALLN (Fformat,
				 build_string ("Cannot look up .eln file "
					       "for %s because no source "
					       "file was found for it"),
				 *filename)),
			 Vdelayed_warnings_list);
	      return;
	    }
	}
    }
  Lisp_Object eln_rel_name = Fcomp_el_to_eln_rel_filename (src_name);

  Lisp_Object dir = Qnil;
  FOR_EACH_TAIL_SAFE (eln_path_tail)
    {
      dir = XCAR (eln_path_tail);
      Lisp_Object eln_name =
	Fexpand_file_name (eln_rel_name,
			   Fexpand_file_name (Vcomp_native_version_dir, dir));
      if (maybe_swap_for_eln1 (src_name, eln_name, filename, fd, mtime))
	return;
    }

  /* Look also in preloaded subfolder of the last entry in
     `comp-eln-load-path'.  */
  dir = Fexpand_file_name (build_string ("preloaded"),
			   Fexpand_file_name (Vcomp_native_version_dir,
					      dir));
  maybe_swap_for_eln1 (src_name, Fexpand_file_name (eln_rel_name, dir),
		       filename, fd, mtime);
#endif
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
   platform-specific files, or if a non-nil and non-t PREDICATE is
   specified.

   If NO_NATIVE is true do not try to load native code.

   If PLATFORM is non-NULL and the file being loaded lies in a special
   directory, such as the Android `/assets' directory, return a handle
   to that directory in *PLATFORM instead of a file descriptor; in
   that case, value is -3.  */

int
openp (Lisp_Object path, Lisp_Object str, Lisp_Object suffixes,
       Lisp_Object *storeptr, Lisp_Object predicate, bool newer,
       bool no_native, void **platform)
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
#ifdef USE_ANDROID_ASSETS
  struct android_fd_or_asset platform_fd;
#endif
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
		Lisp_Object tmp = calln (predicate, string);
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
		else if (sys_faccessat (AT_FDCWD, pfn, XFIXNAT (predicate),
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
                /*  In some systems (like Windows) finding out if a
                    file exists is cheaper to do than actually opening
                    it.  Only open the file when we are sure that it
                    exists.  */
#ifdef WINDOWSNT
                if (sys_faccessat (AT_FDCWD, pfn, R_OK, AT_EACCESS))
                  fd = -1;
                else
#endif
		  {
#if !defined USE_ANDROID_ASSETS
		    fd = emacs_open (pfn, O_RDONLY, 0);
#else
		    if (platform)
		      {
			platform_fd = android_open_asset (pfn, O_RDONLY, 0);

			if (platform_fd.asset
			    && platform_fd.asset != (void *) -1)
			  {
			    *storeptr = string;
			    goto handle_platform_fd;
			  }

			if (platform_fd.asset == (void *) -1)
			  fd = -1;
			else
			  fd = platform_fd.fd;
		      }
		    else
		      fd = emacs_open (pfn, O_RDONLY, 0);
#endif
		  }

		if (fd < 0)
		  {
		    if (! (errno == ENOENT || errno == ENOTDIR))
		      last_errno = errno;
		  }
		else
		  {
		    int err = (sys_fstat (fd, &st) != 0 ? errno
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
		    maybe_swap_for_eln (no_native, &string, &fd,
					get_stat_mtime (&st));
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
		maybe_swap_for_eln (no_native, &save_string, &save_fd,
				    save_mtime);
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

#ifdef USE_ANDROID_ASSETS
 handle_platform_fd:

  /* Here, openp found a platform specific file descriptor.  It can't
     be a directory under Android, so return it in *PLATFORM and then
     -3 as the file descriptor.  */
  *platform = platform_fd.asset;
  return -3;
#endif
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

	  /* If we're loading the entire file, remove old data.  */
	  if (entire)
	    {
	      if (NILP (prev))
		Vload_history = XCDR (tail);
	      else
		Fsetcdr (prev, XCDR (tail));
	    }
	  /* Otherwise, cons on new symbols that are not already
	     members.  */
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
    {
      Lisp_Object tem = Fnreverse (Vcurrent_load_list);
      eassert (!NILP (Fequal (filename, Fcar (tem))));
      Vload_history = Fcons (tem, Vload_history);
      /* FIXME: There should be an unbind_to right after calling us which
         should re-establish the previous value of Vcurrent_load_list.  */
      Vcurrent_load_list = Qt;
    }
}

/* Signal an `end-of-file' error, if possible with file name
   information.  */

static AVOID
end_of_file_error (source_t *source)
{
  if (from_file_p (source))
    /* Only Fload calls read on a file, and Fload always binds
       load-true-file-name around the call.  */
    xsignal1 (Qend_of_file, Vload_true_file_name);
  else if (from_buffer_p (source))
    xsignal1 (Qend_of_file, source->object);
  else
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
  val = calln (macroexpand, val, Qnil);
  if (EQ (CAR_SAFE (val), Qprogn))
    {
      Lisp_Object subforms = XCDR (val);
      val = Qnil;
      FOR_EACH_TAIL (subforms)
	val = readevalloop_eager_expand_eval (XCAR (subforms), macroexpand);
    }
  else
      val = eval_sub (calln (macroexpand, val, Qt));
  return val;
}

/* READFUN, if non-nil, is used instead of `read'.

   START, END specify region to read in current buffer (from eval-region).
   If the input is not from a buffer, they must be nil.  */

static void
readevalloop (Lisp_Object readcharfun,
	      struct infile *infile0,
	      Lisp_Object sourcename,
	      bool printflag,
	      Lisp_Object readfun,
	      Lisp_Object start, Lisp_Object end)
{
  int c;
  Lisp_Object val;
  specpdl_ref count = SPECPDL_INDEX ();
  struct buffer *b = 0;
  bool continue_reading_p;
  Lisp_Object lex_bound;
  /* True if reading an entire buffer.  */
  bool whole_buffer = 0;
  /* True on the first time around.  */
  bool first_sexp = 1;
  Lisp_Object macroexpand;

  if (!NILP (sourcename))
    CHECK_STRING (sourcename);

  macroexpand = Qinternal_macroexpand_for_load;

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

  /* In an .elc file, all shorthand expansion has already taken place, so
     make sure we disable any read-symbol-shorthands set higher up in
     the stack of recursive 'load'. */
  if (STRINGP (sourcename) && suffix_p (sourcename, ".elc"))
    specbind (Qread_symbol_shorthands, Qnil);

  /* If lexical binding is active (either because it was specified in
     the file's header, or via a buffer-local variable), create an empty
     lexical environment, otherwise, turn off lexical binding.  */
  lex_bound = find_symbol_value (Qlexical_binding);
  specbind (Qinternal_interpreter_environment,
	    (NILP (lex_bound) || BASE_EQ (lex_bound, Qunbound)
	     ? Qnil : list1 (Qt)));
  specbind (Qmacroexp__dynvars, Vmacroexp__dynvars);

  /* Ensure sourcename is absolute, except whilst preloading.  */
  if (!will_dump_p ()
      && !NILP (sourcename) && !NILP (Ffile_name_absolute_p (sourcename)))
    sourcename = Fexpand_file_name (sourcename, Qnil);

  loadhist_initialize (sourcename);

  source_t source;
  init_source (&source, readcharfun);

  continue_reading_p = 1;
  while (continue_reading_p)
    {
      specpdl_ref count1 = SPECPDL_INDEX ();

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
	  labeled_restrictions_remove_in_current_buffer ();
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
      c = readchar (&source);
      if (c == ';')
	{
	  while ((c = readchar (&source)) != '\n' && c != -1);
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
      unreadchar (&source, c);

      if (! HASH_TABLE_P (read_objects_map)
	  || XHASH_TABLE (read_objects_map)->count)
	read_objects_map
	  = make_hash_table (&hashtest_eq, DEFAULT_HASH_SIZE, Weak_None);
      if (! HASH_TABLE_P (read_objects_completed)
	  || XHASH_TABLE (read_objects_completed)->count)
	read_objects_completed
	  = make_hash_table (&hashtest_eq, DEFAULT_HASH_SIZE, Weak_None);
      if (!NILP (Vpurify_flag) && c == '(')
	val = read0 (&source, false);
      else
	{
	  if (!NILP (readfun))
	    {
	      val = calln (readfun, readcharfun);

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
	    val = calln (Vload_read_function, readcharfun);
	  else
	    val = read_internal_start (readcharfun, Qnil, Qnil, false);
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
	    Fprin1 (val, Qnil, Qnil);
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
UNIBYTE is obsolete and ignored.
DO-ALLOW-PRINT, if non-nil, specifies that output functions in the
 evaluated code should work normally even if PRINTFLAG is nil, in
 which case the output is displayed in the echo area.

This function ignores the global value of the `lexical-binding'
variable.  Instead it will heed the buffer-local value of that
variable and any
  -*- lexical-binding: t -*-
settings in the buffer; if there is no such setting, and the
buffer-local value of the variable is nil, the buffer will be
evaluated with the value of `lexical-binding' equal to its
top-level default value, as returned by `default-toplevel-value'.

This function preserves the position of point.  */)
  (Lisp_Object buffer, Lisp_Object printflag, Lisp_Object filename,
   Lisp_Object unibyte, Lisp_Object do_allow_print)
{
  specpdl_ref count = SPECPDL_INDEX ();
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
  /* Don't emit a warning about 'lexical-binding' if it already has a
     local binding in the buffer.  */
  if (NILP (Flocal_variable_p (Qlexical_binding, buf)))
    specbind (Qlexical_binding, get_lexical_binding (buf, buf));
  BUF_TEMP_SET_PT (XBUFFER (buf), BUF_BEGV (XBUFFER (buf)));
  readevalloop (buf, 0, filename,
		!NILP (printflag), Qnil, Qnil, Qnil);
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
  specpdl_ref count = SPECPDL_INDEX ();
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
		!NILP (printflag), read_function,
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
    return calln (Qread_minibuffer, build_string ("Lisp expression: "));

  return read_internal_start (stream, Qnil, Qnil, false);
}

DEFUN ("read-positioning-symbols", Fread_positioning_symbols,
       Sread_positioning_symbols, 0, 1, 0,
       doc: /* Read one Lisp expression as text from STREAM, return as Lisp object.
Convert each occurrence of a symbol into a "symbol with pos" object.

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
    /* FIXME: ?! When is this used !?  */
    return calln (Qread_minibuffer, build_string ("Lisp expression: "));

  return read_internal_start (stream, Qnil, Qnil, true);
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
  ret = read_internal_start (string, start, end, false);
  return Fcons (ret, make_fixnum (read_from_string_index));
}

/* Function to set up the global context we need in toplevel read
   calls.  START and END only used when STREAM is a string.
   LOCATE_SYMS true means read symbol occurrences as symbols with
   position.  */
static Lisp_Object
read_internal_start (Lisp_Object stream, Lisp_Object start, Lisp_Object end,
                     bool locate_syms)
{
  Lisp_Object retval;

  readchar_offset = BUFFERP (stream) ? XBUFFER (stream)->pt : 0;
  /* We can get called from readevalloop which may have set these
     already.  */
  if (! HASH_TABLE_P (read_objects_map)
      || XHASH_TABLE (read_objects_map)->count)
    read_objects_map
      = make_hash_table (&hashtest_eq, DEFAULT_HASH_SIZE, Weak_None);
  if (! HASH_TABLE_P (read_objects_completed)
      || XHASH_TABLE (read_objects_completed)->count)
    read_objects_completed
      = make_hash_table (&hashtest_eq, DEFAULT_HASH_SIZE, Weak_None);

  if (STRINGP (stream))
    {
      ptrdiff_t startval, endval;
      validate_subarray (stream, start, end, SCHARS (stream),
			 &startval, &endval);

      read_from_string_index = startval;
      read_from_string_index_byte = string_char_to_byte (stream, startval);
      read_from_string_limit = endval;
    }

  source_t source;
  init_source (&source, stream);
  retval = read0 (&source, locate_syms);
  if (HASH_TABLE_P (read_objects_map)
      && XHASH_TABLE (read_objects_map)->count > 0)
    read_objects_map = Qnil;
  if (HASH_TABLE_P (read_objects_completed)
      && XHASH_TABLE (read_objects_completed)->count > 0)
    read_objects_completed = Qnil;
  return retval;
}

/* Return the scalar value that has the Unicode character name NAME.
   Raise 'invalid-read-syntax' if there is no such character.  */
static int
character_name_to_code (char const *name, ptrdiff_t name_len,
			source_t *source)
{
  /* For "U+XXXX", pass the leading '+' to string_to_number to reject
     monstrosities like "U+-0000".  */
  ptrdiff_t len = name_len - 1;
  Lisp_Object code
    = (name[0] == 'U' && name[1] == '+'
       ? string_to_number (name + 1, 16, &len)
       : calln (Qchar_from_name, make_unibyte_string (name, name_len), Qt));

  if (! RANGED_FIXNUMP (0, code, MAX_UNICODE_CHAR)
      || len != name_len - 1
      || char_surrogate_p (XFIXNUM (code)))
    {
      AUTO_STRING (format, "\\N{%s}");
      AUTO_STRING_WITH_LEN (namestr, name, name_len);
      invalid_syntax_lisp (CALLN (Fformat, format, namestr), source);
    }

  return FIXNUMP (code) ? XFIXNUM (code) : -1;
}

/* Bound on the length of a Unicode character name.  As of
   Unicode 9.0.0 the maximum is 83, so this should be safe.  */
enum { UNICODE_CHARACTER_NAME_LENGTH_BOUND = 200 };

/* Read a character escape sequence, assuming we just read a backslash
   and one more character (next_char).  */
static int
read_char_escape (source_t *source, int next_char)
{
  int modifiers = 0;
  ptrdiff_t ncontrol = 0;
  int chr;

 again: ;
  int c = next_char;
  int unicode_hex_count;
  int mod;

  switch (c)
    {
    case -1:
      end_of_file_error (source);

    case 'a': chr = '\a'; break;
    case 'b': chr = '\b'; break;
    case 'd': chr =  127; break;
    case 'e': chr =   27; break;
    case 'f': chr = '\f'; break;
    case 'n': chr = '\n'; break;
    case 'r': chr = '\r'; break;
    case 't': chr = '\t'; break;
    case 'v': chr = '\v'; break;

    case '\n':
      /* ?\LF is an error; it's probably a user mistake.  */
      error ("Invalid escape char syntax: \\<newline>");

    /* \M-x etc: set modifier bit and parse the char to which it applies,
       allowing for chains such as \M-\S-\A-\H-\s-\C-q.  */
    case 'M': mod = meta_modifier;  goto mod_key;
    case 'S': mod = shift_modifier; goto mod_key;
    case 'H': mod = hyper_modifier; goto mod_key;
    case 'A': mod = alt_modifier;   goto mod_key;
    case 's': mod = super_modifier; goto mod_key;

    mod_key:
      {
	int c1 = readchar (source);
	if (c1 != '-')
	  {
	    if (c == 's')
	      {
		/* \s not followed by a hyphen is SPC.  */
		unreadchar (source, c1);
		chr = ' ';
		break;
	      }
	    else
	      /* \M, \S, \H, \A not followed by a hyphen is an error.  */
	      error ("Invalid escape char syntax: \\%c not followed by -", c);
	  }
	modifiers |= mod;
	c1 = readchar (source);
	if (c1 == '\\')
	  {
	    next_char = readchar (source);
	    goto again;
	  }
	chr = c1;
	break;
      }

    /* Control modifiers (\C-x or \^x) are messy and not actually idempotent.
       For example, ?\C-\C-a = ?\C-\001 = 0x4000001.
       Keep a count of them and apply them separately.  */
    case 'C':
      {
	int c1 = readchar (source);
	if (c1 != '-')
	  error ("Invalid escape char syntax: \\%c not followed by -", c);
      }
      FALLTHROUGH;
    /* The prefixes \C- and \^ are equivalent.  */
    case '^':
      {
	ncontrol++;
	int c1 = readchar (source);
	if (c1 == '\\')
	  {
	    next_char = readchar (source);
	    goto again;
	  }
	chr = c1;
	break;
      }

    /* 1-3 octal digits.  Values in 0x80..0xff are encoded as raw bytes.  */
    case '0': case '1': case '2': case '3':
    case '4': case '5': case '6': case '7':
      {
	int i = c - '0';
	int count = 0;
	while (count < 2)
	  {
	    int c = readchar (source);
	    if (c < '0' || c > '7')
	      {
		unreadchar (source, c);
		break;
	      }
	    i = (i << 3) + (c - '0');
	    count++;
	  }

	if (i >= 0x80 && i < 0x100)
	  i = BYTE8_TO_CHAR (i);
	chr = i;
	break;
      }

    /* 1 or more hex digits.  Values may encode modifiers.
       Values in 0x80..0xff using 2 hex digits are encoded as raw bytes.  */
    case 'x':
      {
	unsigned int i = 0;
	int count = 0;
	while (1)
	  {
	    int c = readchar (source);
	    int digit = char_hexdigit (c);
	    if (digit < 0)
	      {
		unreadchar (source, c);
		break;
	      }
	    i = (i << 4) + digit;
	    /* Allow hex escapes as large as ?\xfffffff, because some
	       packages use them to denote characters with modifiers.  */
	    if (i > (CHAR_META | (CHAR_META - 1)))
	      error ("Hex character out of range: \\x%x...", i);
	    count += count < 3;
	  }

	if (count == 0)
	  error ("Invalid escape char syntax: \\x not followed by hex digit");
	if (count < 3 && i >= 0x80)
	  i = BYTE8_TO_CHAR (i);
	modifiers |= i & CHAR_MODIFIER_MASK;
	chr = i & ~CHAR_MODIFIER_MASK;
	break;
      }

    /* 8-digit Unicode hex escape: \UHHHHHHHH */
    case 'U':
      unicode_hex_count = 8;
      goto unicode_hex;

    /* 4-digit Unicode hex escape: \uHHHH */
    case 'u':
      unicode_hex_count = 4;
    unicode_hex:
      {
	unsigned int i = 0;
	for (int count = 0; count < unicode_hex_count; count++)
	  {
	    int c = readchar (source);
	    if (c < 0)
	      error ("Malformed Unicode escape: \\%c%x",
		     unicode_hex_count == 4 ? 'u' : 'U', i);
	    int digit = char_hexdigit (c);
	    if (digit < 0)
	      error ("Non-hex character used for Unicode escape: %c (%d)",
		     c, c);
	    i = (i << 4) + digit;
	  }
	if (i > 0x10FFFF)
	  error ("Non-Unicode character: 0x%x", i);
	chr = i;
	break;
      }

    /* Named character: \N{name} */
    case 'N':
      {
        int c = readchar (source);
        if (c != '{')
          invalid_syntax ("Expected opening brace after \\N", source);
        char name[UNICODE_CHARACTER_NAME_LENGTH_BOUND + 1];
        bool whitespace = false;
        ptrdiff_t length = 0;
        while (true)
          {
            int c = readchar (source);
            if (c < 0)
              end_of_file_error (source);
            if (c == '}')
              break;
            if (c >= 0x80)
              {
                AUTO_STRING (format,
                             "Invalid character U+%04X in character name");
		invalid_syntax_lisp (CALLN (Fformat, format,
					    make_fixed_natnum (c)),
				     source);
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
              invalid_syntax ("Character name too long", source);
          }
        if (length == 0)
          invalid_syntax ("Empty character name", source);
	name[length] = '\0';

	/* character_name_to_code can invoke read0, recursively.
	   This is why read0 needs to be re-entrant.  */
	chr = character_name_to_code (name, length, source);
	break;
      }

    default:
      chr = c;
      break;
    }
  if (chr < 0)
    end_of_file_error (source);
  eassert (chr >= 0 && chr < (1 << CHARACTERBITS));

  /* Apply Control modifiers, using the rules:
     \C-X = ascii_ctrl(nomod(X)) | mods(X)  if nomod(X) is one of:
                                                A-Z a-z ? @ [ \ ] ^ _

            X | ctrl_modifier               otherwise

     where
         nomod(c) = c without modifiers
	 mods(c)  = the modifiers of c
         ascii_ctrl(c) = 127       if c = '?'
                         c & 0x1f  otherwise
  */
  while (ncontrol > 0)
    {
      if ((chr >= '@' && chr <= '_') || (chr >= 'a' && chr <= 'z'))
	chr &= 0x1f;
      else if (chr == '?')
	chr = 127;
      else
	modifiers |= ctrl_modifier;
      ncontrol--;
    }

  return chr | modifiers;
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

static void
invalid_radix_integer (EMACS_INT radix, source_t *source)
{
  static char const format[] = "integer, radix %"pI"d";
  char buf[sizeof format - sizeof "%"pI"d" + INT_BUFSIZE_BOUND (radix)];
  sprintf (buf, format, radix);
  invalid_syntax (buf, source);
}

/* A character buffer that starts on the C stack and switches to heap
   allocation if more space is needed.  */
typedef struct {
  char *start;	      /* start of buffer, on the C stack or heap */
  char *end;	      /* just past end of buffer */
  char *cur;	      /* where to put next char read */
  char *heap;         /* heap allocation or NULL */
  specpdl_ref count;  /* index for cleanup when a heap allocation is used */
} readbuf_t;

/* Make more room in the buffer, using heap allocation.  */
static NO_INLINE void
readbuf_grow (readbuf_t *rb)
{
  ptrdiff_t used = rb->cur - rb->start;
  ptrdiff_t size = rb->end - rb->start;
  char *p = xpalloc (rb->heap, &size, MAX_MULTIBYTE_LENGTH, -1, 1);
  if (rb->heap == NULL)
    {
      /* Old buffer is on the stack; copy it to the heap.  */
      memcpy (p, rb->start, used);
      rb->count = SPECPDL_INDEX ();
      record_unwind_protect_ptr (xfree, p);
    }
  else
    set_unwind_protect_ptr (rb->count, xfree, p);  /* update cleanup entry */
  rb->start = rb->heap = p;
  rb->cur = rb->start + used;
  rb->end = rb->start + size;
}

static inline void
add_char_to_buffer (readbuf_t *rb, int c, bool multibyte)
{
  /* Make room for a multibyte char and a terminating NUL.  */
  if (rb->end - rb->cur < MAX_MULTIBYTE_LENGTH + 1)
    readbuf_grow (rb);
  if (multibyte)
    rb->cur += CHAR_STRING (c, (unsigned char *) rb->cur);
  else
    *rb->cur++ = c;
}

/* Read an integer in radix RADIX using READCHARFUN to read
   characters.  RADIX must be in the interval [2..36].
   Value is the integer read.
   Signal an error if encountering invalid read syntax.  */

static Lisp_Object
read_integer (source_t *source, int radix)
{
  specpdl_ref count = SPECPDL_INDEX ();
  char stackbuf[20];
  readbuf_t rb = { .start = stackbuf,
		   .end = stackbuf + sizeof stackbuf,
		   .cur = stackbuf,
		   .heap = NULL };

  int valid = -1; /* 1 if valid, 0 if not, -1 if incomplete.  */

  int c = readchar (source);
  if (c == '-' || c == '+')
    {
      *rb.cur++ = c;
      c = readchar (source);
    }

  if (c == '0')
    {
      *rb.cur++ = c;
      valid = 1;

      /* Ignore redundant leading zeros, so the buffer doesn't
	 fill up with them.  */
      do
	c = readchar (source);
      while (c == '0');
    }

  for (int digit; (digit = digit_to_number (c, radix)) >= -1; )
    {
      if (digit == -1)
	valid = 0;
      if (valid < 0)
	valid = 1;
      add_char_to_buffer (&rb, c, false);
      c = readchar (source);
    }

  unreadchar (source, c);

  if (valid != 1)
    invalid_radix_integer (radix, source);

  *rb.cur++ = '\0';
  return unbind_to (count, string_to_number (rb.start, radix, NULL));
}


/* Read a character literal (preceded by `?').  */
static Lisp_Object
read_char_literal (source_t *source)
{
  int ch = readchar (source);
  if (ch < 0)
    end_of_file_error (source);

  /* Accept `single space' syntax like (list ? x) where the
     whitespace character is SPC or TAB.
     Other literal whitespace like NL, CR, and FF are not accepted,
     as there are well-established escape sequences for these.  */
  if (ch == ' ' || ch == '\t')
    return make_fixnum (ch);

  if (   ch == '(' || ch == ')' || ch == '[' || ch == ']'
      || ch == '"' || ch == ';')
    {
      CHECK_LIST (Vlread_unescaped_character_literals);
      Lisp_Object char_obj = make_fixed_natnum (ch);
      if (NILP (Fmemq (char_obj, Vlread_unescaped_character_literals)))
	Vlread_unescaped_character_literals =
	  Fcons (char_obj, Vlread_unescaped_character_literals);
    }

  if (ch == '\\')
    ch = read_char_escape (source, readchar (source));

  int modifiers = ch & CHAR_MODIFIER_MASK;
  ch &= ~CHAR_MODIFIER_MASK;
  if (CHAR_BYTE8_P (ch))
    ch = CHAR_TO_BYTE8 (ch);
  ch |= modifiers;

  int nch = readchar (source);
  unreadchar (source, nch);
  if (nch <= 32
      || nch == '"' || nch == '\'' || nch == ';' || nch == '('
      || nch == ')' || nch == '['  || nch == ']' || nch == '#'
      || nch == '?' || nch == '`'  || nch == ',' || nch == '.')
    return make_fixnum (ch);

  invalid_syntax ("?", source);
}

/* Read a string literal (preceded by '"').  */
static Lisp_Object
read_string_literal (source_t *source)
{
  specpdl_ref count = SPECPDL_INDEX ();
  char stackbuf[1024];
  readbuf_t rb = { .start = stackbuf,
		   .end = stackbuf + sizeof stackbuf,
		   .cur = stackbuf,
		   .heap = NULL };

  /* True if we saw an escape sequence specifying
     a multibyte character.  */
  bool force_multibyte = false;
  /* True if we saw an escape sequence specifying
     a single-byte character.  */
  bool force_singlebyte = false;
  ptrdiff_t nchars = 0;

  int ch;
  while ((ch = readchar (source)) >= 0 && ch != '\"')
    {
      if (ch == '\\')
	{
	  /* First apply string-specific escape rules:  */
	  ch = readchar (source);
	  switch (ch)
	    {
	    case 's':
	      /* `\s' is always a space in strings.  */
	      ch = ' ';
	      break;
	    case ' ':
	    case '\n':
	      /* `\SPC' and `\LF' generate no characters at all.  */
	      continue;
	    default:
	      ch = read_char_escape (source, ch);
	      break;
	    }

	  int modifiers = ch & CHAR_MODIFIER_MASK;
	  ch &= ~CHAR_MODIFIER_MASK;

	  if (CHAR_BYTE8_P (ch))
	    force_singlebyte = true;
	  else if (! ASCII_CHAR_P (ch))
	    force_multibyte = true;
	  else		/* I.e. ASCII_CHAR_P (ch).  */
	    {
	      /* Allow `\C-SPC' and `\^SPC'.  This is done here because
		 the literals ?\C-SPC and ?\^SPC (rather inconsistently)
		 yield (' ' | CHAR_CTL); see bug#55738.  */
	      if (modifiers == CHAR_CTL && ch == ' ')
		{
		  ch = 0;
		  modifiers = 0;
		}
	      if (modifiers & CHAR_SHIFT)
		{
		  /* Shift modifier is valid only with [A-Za-z].  */
		  if (ch >= 'A' && ch <= 'Z')
		    modifiers &= ~CHAR_SHIFT;
		  else if (ch >= 'a' && ch <= 'z')
		    {
		      ch -= ('a' - 'A');
		      modifiers &= ~CHAR_SHIFT;
		    }
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
	    invalid_syntax ("Invalid modifier in string", source);
	  add_char_to_buffer (&rb, ch, true);
	}
      else
	{
	  add_char_to_buffer (&rb, ch, true);
	  if (CHAR_BYTE8_P (ch))
	    force_singlebyte = true;
	  else if (! ASCII_CHAR_P (ch))
	    force_multibyte = true;
	}
      nchars++;
    }

  if (ch < 0)
    end_of_file_error (source);

  if (!force_multibyte && force_singlebyte)
    {
      /* READ_BUFFER contains raw 8-bit bytes and no multibyte
	 forms.  Convert it to unibyte.  */
      nchars = str_as_unibyte ((unsigned char *)rb.start, rb.cur - rb.start);
      rb.cur = rb.start + nchars;
    }

  ptrdiff_t nbytes = rb.cur - rb.start;
  Lisp_Object obj = make_specified_string (rb.start, nchars, nbytes,
					   (force_multibyte
					    || nbytes != nchars));
  return unbind_to (count, obj);
}

/* Make a hash table from the constructor plist.  */
static Lisp_Object
hash_table_from_plist (Lisp_Object plist)
{
  Lisp_Object params[4 * 2];
  Lisp_Object *par = params;

  /* This is repetitive but fast and simple.  */
#define ADDPARAM(name)					\
  do {							\
    Lisp_Object val = plist_get (plist, Q ## name);	\
    if (!NILP (val))					\
      {							\
	*par++ = QC ## name;				\
	*par++ = val;					\
      }							\
  } while (0)

  ADDPARAM (test);
  ADDPARAM (weakness);
  ADDPARAM (purecopy);

  Lisp_Object data = plist_get (plist, Qdata);
  if (!(NILP (data) || CONSP (data)))
    error ("Hash table data is not a list");
  ptrdiff_t data_len = list_length (data);
  if (data_len & 1)
    error ("Hash table data length is odd");
  *par++ = QCsize;
  *par++ = make_fixnum (data_len / 2);

  /* Now use params to make a new hash table and fill it.  */
  Lisp_Object ht = Fmake_hash_table (par - params, params);

  while (!NILP (data))
    {
      Lisp_Object key = XCAR (data);
      data = XCDR (data);
      Lisp_Object val = XCAR (data);
      Fputhash (key, val, ht);
      data = XCDR (data);
    }

  return ht;
}

static Lisp_Object
record_from_list (Lisp_Object elems)
{
  ptrdiff_t size = list_length (elems);
  Lisp_Object obj = Fmake_record (XCAR (elems),
				  make_fixnum (size - 1),
				  Qnil);
  Lisp_Object tl = XCDR (elems);
  for (int i = 1; i < size; i++)
    {
      ASET (obj, i, XCAR (tl));
      tl = XCDR (tl);
    }
  return obj;
}

/* Turn a reversed list into a vector.  */
static Lisp_Object
vector_from_rev_list (Lisp_Object elems)
{
  ptrdiff_t size = list_length (elems);
  Lisp_Object obj = make_nil_vector (size);
  Lisp_Object *vec = XVECTOR (obj)->contents;
  for (ptrdiff_t i = size - 1; i >= 0; i--)
    {
      vec[i] = XCAR (elems);
      Lisp_Object next = XCDR (elems);
      free_cons (XCONS (elems));
      elems = next;
    }
  return obj;
}

static Lisp_Object get_lazy_string (Lisp_Object val);

static Lisp_Object
bytecode_from_rev_list (Lisp_Object elems, source_t *source)
{
  Lisp_Object obj = vector_from_rev_list (elems);
  Lisp_Object *vec = XVECTOR (obj)->contents;
  ptrdiff_t size = ASIZE (obj);

  if (infile && size >= CLOSURE_CONSTANTS)
    {
      /* Always read 'lazily-loaded' bytecode (generated by the
         `byte-compile-dynamic' feature prior to Emacs 30) eagerly, to
         avoid code in the fast path during execution.  */
      if (CONSP (vec[CLOSURE_CODE])
          && FIXNUMP (XCDR (vec[CLOSURE_CODE])))
        vec[CLOSURE_CODE] = get_lazy_string (vec[CLOSURE_CODE]);

      /* Lazily-loaded bytecode is represented by the constant slot being nil
         and the bytecode slot a (lazily loaded) string containing the
         print representation of (BYTECODE . CONSTANTS).  */
      if (NILP (vec[CLOSURE_CONSTANTS]) && STRINGP (vec[CLOSURE_CODE]))
        {
          Lisp_Object enc = vec[CLOSURE_CODE];
	  eassert (!STRING_MULTIBYTE (enc));
	  /* The string (always unibyte) must be decoded to be parsed.  */
	  eassert (from_file_p (source));
	  enc = Fdecode_coding_string (enc,
				       source->emacs_mule_encoding
				       ? Qemacs_mule : Qutf_8_emacs,
				       Qt, Qnil);
	  Lisp_Object pair = Fread (enc);
          if (!CONSP (pair))
	    invalid_syntax ("Invalid byte-code object", source);

          vec[CLOSURE_CODE] = XCAR (pair);
          vec[CLOSURE_CONSTANTS] = XCDR (pair);
        }
    }

  if (!(size >= CLOSURE_STACK_DEPTH && size <= CLOSURE_INTERACTIVE + 1
	&& (FIXNUMP (vec[CLOSURE_ARGLIST])
	    || CONSP (vec[CLOSURE_ARGLIST])
	    || NILP (vec[CLOSURE_ARGLIST]))
	&& ((STRINGP (vec[CLOSURE_CODE]) /* Byte-code function.  */
	     && VECTORP (vec[CLOSURE_CONSTANTS])
	     && size > CLOSURE_STACK_DEPTH
	     && (FIXNATP (vec[CLOSURE_STACK_DEPTH])))
	    || (CONSP (vec[CLOSURE_CODE]) /* Interpreted function.  */
	        && (CONSP (vec[CLOSURE_CONSTANTS])
	            || NILP (vec[CLOSURE_CONSTANTS]))))))
    invalid_syntax ("Invalid byte-code object", source);

  if (STRINGP (vec[CLOSURE_CODE]))
    {
      if (STRING_MULTIBYTE (vec[CLOSURE_CODE]))
        /* BYTESTR must have been produced by Emacs 20.2 or earlier
           because it produced a raw 8-bit string for byte-code and
           now such a byte-code string is loaded as multibyte with
           raw 8-bit characters converted to multibyte form.
           Convert them back to the original unibyte form.  */
        vec[CLOSURE_CODE] = Fstring_as_unibyte (vec[CLOSURE_CODE]);

      /* Bytecode must be immovable.  */
      pin_string (vec[CLOSURE_CODE]);
    }

  XSETPVECTYPE (XVECTOR (obj), PVEC_CLOSURE);
  return obj;
}

static Lisp_Object
char_table_from_rev_list (Lisp_Object elems, source_t *source)
{
  Lisp_Object obj = vector_from_rev_list (elems);
  if (ASIZE (obj) < CHAR_TABLE_STANDARD_SLOTS)
    invalid_syntax ("Invalid size char-table", source);
  XSETPVECTYPE (XVECTOR (obj), PVEC_CHAR_TABLE);
  return obj;

}

static Lisp_Object
sub_char_table_from_rev_list (Lisp_Object elems, source_t *source)
{
  /* A sub-char-table can't be read as a regular vector because of two
     C integer fields.  */
  elems = Fnreverse (elems);
  ptrdiff_t size = list_length (elems);
  if (size < 2)
    error ("Invalid size of sub-char-table");

  if (!RANGED_FIXNUMP (1, XCAR (elems), 3))
    error ("Invalid depth in sub-char-table");
  int depth = XFIXNUM (XCAR (elems));

  if (chartab_size[depth] != size - 2)
    error ("Invalid size in sub-char-table");
  elems = XCDR (elems);

  if (!RANGED_FIXNUMP (0, XCAR (elems), MAX_CHAR))
    error ("Invalid minimum character in sub-char-table");
  int min_char = XFIXNUM (XCAR (elems));
  elems = XCDR (elems);

  Lisp_Object tbl = make_uninit_sub_char_table (depth, min_char);
  for (int i = 0; i < size - 2; i++)
    {
      XSUB_CHAR_TABLE (tbl)->contents[i] = XCAR (elems);
      elems = XCDR (elems);
    }
  return tbl;
}

static Lisp_Object
string_props_from_rev_list (Lisp_Object elems, source_t *source)
{
  elems = Fnreverse (elems);
  if (NILP (elems) || !STRINGP (XCAR (elems)))
    invalid_syntax ("#", source);
  Lisp_Object obj = XCAR (elems);
  for (Lisp_Object tl = XCDR (elems); !NILP (tl);)
    {
      Lisp_Object beg = XCAR (tl);
      tl = XCDR (tl);
      if (NILP (tl))
	invalid_syntax ("Invalid string property list", source);
      Lisp_Object end = XCAR (tl);
      tl = XCDR (tl);
      if (NILP (tl))
	invalid_syntax ("Invalid string property list", source);
      Lisp_Object plist = XCAR (tl);
      tl = XCDR (tl);
      Fset_text_properties (beg, end, plist, obj);
    }
  return obj;
}

/* Read a bool vector (preceded by "#&").  */
static Lisp_Object
read_bool_vector (source_t *source)
{
  EMACS_INT length = 0;
  for (;;)
    {
      int c = readchar (source);
      if (c < '0' || c > '9')
	{
	  if (c != '"')
	    invalid_syntax ("#&", source);
	  break;
	}
      if (ckd_mul (&length, length, 10)
	  || ckd_add (&length, length, c - '0'))
	invalid_syntax ("#&", source);
    }
  if (BOOL_VECTOR_LENGTH_MAX < length)
    invalid_syntax ("#&", source);

  ptrdiff_t size_in_chars = bool_vector_bytes (length);
  Lisp_Object str = read_string_literal (source);
  if (STRING_MULTIBYTE (str)
      || !(size_in_chars == SCHARS (str)
	   /* Emacs 19 printed 1 char too many when the number of bits
	      was a multiple of 8.  Accept such input in case it came
	      from that old version.  */
	   || length == (SCHARS (str) - 1) * BOOL_VECTOR_BITS_PER_CHAR))
    invalid_syntax ("#&...", source);

  Lisp_Object obj = make_uninit_bool_vector (length);
  unsigned char *data = bool_vector_uchar_data (obj);
  memcpy (data, SDATA (str), size_in_chars);
  /* Clear the extraneous bits in the last byte.  */
  if (length != size_in_chars * BOOL_VECTOR_BITS_PER_CHAR)
    data[size_in_chars - 1] &= (1 << (length % BOOL_VECTOR_BITS_PER_CHAR)) - 1;
  return obj;
}

/* Skip (and optionally remember) a lazily-loaded string
   preceded by "#@".  Return true if this was a normal skip,
   false if we read #@00 (which skips to EOB/EOF).  */
static bool
skip_lazy_string (source_t *source)
{
  ptrdiff_t nskip = 0;
  ptrdiff_t digits = 0;
  for (;;)
    {
      int c = readchar (source);
      if (c < '0' || c > '9')
	{
	  if (nskip > 0)
	    /* We can't use UNREAD here, because in the code below we side-step
	       READCHAR.  Instead, assume the first char after #@NNN occupies
	       a single byte, which is the case normally since it's just
	       a space.  */
	    nskip--;
	  else
	    unreadchar (source, c);
	  break;
	}
      if (ckd_mul (&nskip, nskip, 10)
	  || ckd_add (&nskip, nskip, c - '0'))
	invalid_syntax ("#@", source);
      digits++;
      if (digits == 2 && nskip == 0)
	{
	  /* #@00 means "read nil and skip to end" */
	  skip_dyn_eof (source);
	  return false;
	}
    }

  if (load_force_doc_strings && from_file_p (source))
    {
      /* If we are supposed to force doc strings into core right now,
	 record the last string that we skipped,
	 and record where in the file it comes from.  */

      /* First exchange the two saved_strings.  */
      static_assert (ARRAYELTS (saved_strings) == 2);
      struct saved_string t = saved_strings[0];
      saved_strings[0] = saved_strings[1];
      saved_strings[1] = t;

      enum { extra = 100 };
      struct saved_string *ss = &saved_strings[0];
      if (ss->size == 0)
	{
	  ss->size = nskip + extra;
	  ss->string = xmalloc (ss->size);
	}
      else if (nskip > ss->size)
	{
	  ss->size = nskip + extra;
	  ss->string = xrealloc (ss->string, ss->size);
	}

      file_stream instream = infile->stream;
      ss->position = (file_tell (instream) - infile->lookahead);

      /* Copy that many bytes into the saved string.  */
      ptrdiff_t i = 0;
      int c = 0;
      for (int n = min (nskip, infile->lookahead); n > 0; n--)
	ss->string[i++] = c = infile->buf[--infile->lookahead];
      block_input ();
      for (; i < nskip && c >= 0; i++)
	ss->string[i] = c = file_get_char (instream);
      unblock_input ();

      ss->length = i;
    }
  else
    /* Skip that many bytes.  */
    skip_dyn_bytes (source, nskip);

  return true;
}

/* Given a lazy-loaded string designator VAL, return the actual string.
   VAL is (FILENAME . POS).  */
static Lisp_Object
get_lazy_string (Lisp_Object val)
{
  /* Get a doc string from the file we are loading.
     If it's in a saved string, get it from there.

     Here, we don't know if the string is a bytecode string or a doc
     string.  As a bytecode string must be unibyte, we always return a
     unibyte string.  If it is actually a doc string, caller must make
     it multibyte.  */

  /* We used to emit negative positions for 'user variables' (whose doc
     strings started with an asterisk); take the absolute value for
     compatibility.  */
  EMACS_INT pos = eabs (XFIXNUM (XCDR (val)));
  struct saved_string *ss = &saved_strings[0];
  struct saved_string *ssend = ss + ARRAYELTS (saved_strings);
  while (ss < ssend
	 && !(pos >= ss->position && pos < ss->position + ss->length))
    ss++;
  if (ss >= ssend)
    return get_doc_string (val, 1);

  ptrdiff_t start = pos - ss->position;
  char *str = ss->string;
  ptrdiff_t from = start;
  ptrdiff_t to = start;

  /* Process quoting with ^A, and find the end of the string,
     which is marked with ^_ (037).  */
  while (str[from] != 037)
    {
      int c = str[from++];
      if (c == 1)
	{
	  c = str[from++];
	  str[to++] = (c == 1 ? c
		       : c == '0' ? 0
		       : c == '_' ? 037
		       : c);
	}
      else
	str[to++] = c;
    }

  return make_unibyte_string (str + start, to - start);
}


/* Length of prefix only consisting of symbol constituent characters.  */
static ptrdiff_t
symbol_char_span (const char *s)
{
  const char *p = s;
  while (   *p == '^' || *p == '*' || *p == '+' || *p == '-' || *p == '/'
	 || *p == '<' || *p == '=' || *p == '>' || *p == '_' || *p == '|')
    p++;
  return p - s;
}

static void
skip_space_and_comments (source_t *source)
{
  int c;
  do
    {
      c = readchar (source);
      if (c == ';')
	do
	  c = readchar (source);
	while (c >= 0 && c != '\n');
      if (c < 0)
	end_of_file_error (source);
    }
  while (c <= 32 || c == NO_BREAK_SPACE);
  unreadchar (source, c);
}

/* When an object is read, the type of the top read stack entry indicates
   the syntactic context.  */
enum read_entry_type
{
				/* preceding syntactic context */
  RE_list_start,		/* "(" */

  RE_list,			/* "(" (+ OBJECT) */
  RE_list_dot,			/* "(" (+ OBJECT) "." */

  RE_vector,			/* "[" (* OBJECT) */
  RE_record,			/* "#s(" (* OBJECT) */
  RE_char_table,		/* "#^[" (* OBJECT) */
  RE_sub_char_table,		/* "#^^[" (* OBJECT) */
  RE_byte_code,			/* "#[" (* OBJECT) */
  RE_string_props,		/* "#(" (* OBJECT) */

  RE_special,			/* "'" | "#'" | "`" | "," | ",@" */

  RE_numbered,			/* "#" (+ DIGIT) "=" */
};

struct read_stack_entry
{
  enum read_entry_type type;
  union {
    /* RE_list, RE_list_dot */
    struct {
      Lisp_Object head;		/* first cons of list */
      Lisp_Object tail;		/* last cons of list */
    } list;

    /* RE_vector, RE_record, RE_char_table, RE_sub_char_table,
       RE_byte_code, RE_string_props */
    struct {
      Lisp_Object elems;	/* list of elements in reverse order */
      bool old_locate_syms;	/* old value of locate_syms */
    } vector;

    /* RE_special */
    struct {
      Lisp_Object symbol;	/* symbol from special syntax */
    } special;

    /* RE_numbered */
    struct {
      Lisp_Object number;	/* number as a fixnum */
      Lisp_Object placeholder;	/* placeholder object */
    } numbered;
  } u;
};

struct read_stack
{
  struct read_stack_entry *stack;  /* base of stack */
  ptrdiff_t size;		   /* allocated size in entries */
  ptrdiff_t sp;			   /* current number of entries */
};

static struct read_stack rdstack = {NULL, 0, 0};

void
mark_lread (void)
{
  /* Mark the read stack, which may contain data not otherwise traced */
  for (ptrdiff_t i = 0; i < rdstack.sp; i++)
    {
      struct read_stack_entry *e = &rdstack.stack[i];
      switch (e->type)
	{
	case RE_list_start:
	  break;
	case RE_list:
	case RE_list_dot:
	  mark_object (e->u.list.head);
	  mark_object (e->u.list.tail);
	  break;
	case RE_vector:
	case RE_record:
	case RE_char_table:
	case RE_sub_char_table:
	case RE_byte_code:
	case RE_string_props:
	  mark_object (e->u.vector.elems);
	  break;
	case RE_special:
	  mark_object (e->u.special.symbol);
	  break;
	case RE_numbered:
	  mark_object (e->u.numbered.number);
	  mark_object (e->u.numbered.placeholder);
	  break;
	}
    }
}

static inline struct read_stack_entry *
read_stack_top (void)
{
  eassume (rdstack.sp > 0);
  return &rdstack.stack[rdstack.sp - 1];
}

static inline struct read_stack_entry *
read_stack_pop (void)
{
  eassume (rdstack.sp > 0);
  return &rdstack.stack[--rdstack.sp];
}

static inline bool
read_stack_empty_p (ptrdiff_t base_sp)
{
  return rdstack.sp <= base_sp;
}

NO_INLINE static void
grow_read_stack (void)
{
  struct read_stack *rs = &rdstack;
  eassert (rs->sp == rs->size);
  rs->stack = xpalloc (rs->stack, &rs->size, 1, -1, sizeof *rs->stack);
  eassert (rs->sp < rs->size);
}

static inline void
read_stack_push (struct read_stack_entry e)
{
  if (rdstack.sp >= rdstack.size)
    grow_read_stack ();
  rdstack.stack[rdstack.sp++] = e;
}

static void
read_stack_reset (intmax_t sp)
{
  eassert (sp <= rdstack.sp);
  rdstack.sp = sp;
}

static AVOID
invalid_syntax_with_buffer (readbuf_t *rb, source_t *source)
{
  *rb->cur = '\0';
  invalid_syntax (rb->start, source);
}

static inline int
read_and_buffer (readbuf_t *rb, source_t *source)
{
  int c = readchar (source);
  if (c < 0)
    invalid_syntax_with_buffer (rb, source);
  add_char_to_buffer (rb, c, source->multibyte);
  return c;
}

/* Read a Lisp object.
   If LOCATE_SYMS is true, symbols are read with position.  */
static Lisp_Object
read0 (source_t *source, bool locate_syms)
{
  char stackbuf[64];

  specpdl_ref base_pdl = SPECPDL_INDEX ();
  ptrdiff_t base_sp = rdstack.sp;
  record_unwind_protect_intmax (read_stack_reset, base_sp);

  readbuf_t rb = { .start = stackbuf,
		   .end = stackbuf + sizeof stackbuf,
		   .heap = NULL };

  bool uninterned_symbol;
  bool skip_shorthand;

  /* Read an object into `obj'.  */
 read_obj: ;
  Lisp_Object obj;
  int c = readchar (source);
  if (c < 0)
    end_of_file_error (source);

  switch (c)
    {
    case '(':
      read_stack_push ((struct read_stack_entry) {.type = RE_list_start});
      goto read_obj;

    case ')':
      if (read_stack_empty_p (base_sp))
	invalid_syntax (")", source);
      switch (read_stack_top ()->type)
	{
	case RE_list_start:
	  read_stack_pop ();
	  obj = Qnil;
	  break;
	case RE_list:
	  obj = read_stack_pop ()->u.list.head;
	  break;
	case RE_record:
	  {
	    locate_syms = read_stack_top ()->u.vector.old_locate_syms;
	    Lisp_Object elems = Fnreverse (read_stack_pop ()->u.vector.elems);
	    if (NILP (elems))
	      invalid_syntax ("#s", source);

	    if (BASE_EQ (XCAR (elems), Qhash_table))
	      obj = hash_table_from_plist (XCDR (elems));
	    else
	      obj = record_from_list (elems);
	    break;
	  }
	case RE_string_props:
	  locate_syms = read_stack_top ()->u.vector.old_locate_syms;
	  obj = string_props_from_rev_list (read_stack_pop () ->u.vector.elems,
					    source);
	  break;
	default:
	  invalid_syntax (")", source);
	}
      break;

    case '[':
      read_stack_push ((struct read_stack_entry) {
	  .type = RE_vector,
	  .u.vector.elems = Qnil,
	  .u.vector.old_locate_syms = locate_syms,
	});
      /* FIXME: should vectors be read with locate_syms=false?  */
      goto read_obj;

    case ']':
      if (read_stack_empty_p (base_sp))
	invalid_syntax ("]", source);
      switch (read_stack_top ()->type)
	{
	case RE_vector:
	  locate_syms = read_stack_top ()->u.vector.old_locate_syms;
	  obj = vector_from_rev_list (read_stack_pop ()->u.vector.elems);
	  break;
	case RE_byte_code:
	  locate_syms = read_stack_top ()->u.vector.old_locate_syms;
	  obj = bytecode_from_rev_list (read_stack_pop ()->u.vector.elems,
					source);
	  break;
	case RE_char_table:
	  locate_syms = read_stack_top ()->u.vector.old_locate_syms;
	  obj = char_table_from_rev_list (read_stack_pop ()->u.vector.elems,
					  source);
	  break;
	case RE_sub_char_table:
	  locate_syms = read_stack_top ()->u.vector.old_locate_syms;
	  obj = sub_char_table_from_rev_list (read_stack_pop ()->u.vector.elems,
					      source);
	  break;
	default:
	  invalid_syntax ("]", source);
	  break;
	}
      break;

    case '#':
      {
	rb.cur = rb.start;
	*rb.cur++ = '#';
	int ch = read_and_buffer (&rb, source);
	switch (ch)
	  {
	  case '\'':
	    /* #'X -- special syntax for (function X) */
	    read_stack_push ((struct read_stack_entry) {
		.type = RE_special,
		.u.special.symbol = Qfunction,
	      });
	    goto read_obj;

	  case '#':
	    /* ## -- the empty symbol */
	    obj = Fintern (empty_unibyte_string, Qnil);
	    break;

	  case 's':
	    /* #s(...) -- a record or hash-table */
	    ch = read_and_buffer (&rb, source);
	    if (ch != '(')
	      {
		unreadchar (source, ch);
		invalid_syntax_with_buffer (&rb, source);
	      }
	    read_stack_push ((struct read_stack_entry) {
		.type = RE_record,
		.u.vector.elems = Qnil,
		.u.vector.old_locate_syms = locate_syms,
	      });
	    locate_syms = false;
	    goto read_obj;

	  case '^':
	    /* #^[...]  -- char-table
	       #^^[...] -- sub-char-table */
	    ch = read_and_buffer (&rb, source);
	    if (ch == '^')
	      {
		ch = read_and_buffer (&rb, source);
		if (ch == '[')
		  {
		    read_stack_push ((struct read_stack_entry) {
			.type = RE_sub_char_table,
			.u.vector.elems = Qnil,
			.u.vector.old_locate_syms = locate_syms,
		      });
		    locate_syms = false;
		    goto read_obj;
		  }
		else
		  {
		    unreadchar (source, ch);
		    invalid_syntax_with_buffer (&rb, source);
		  }
	      }
	    else if (ch == '[')
	      {
		read_stack_push ((struct read_stack_entry) {
		    .type = RE_char_table,
		    .u.vector.elems = Qnil,
		    .u.vector.old_locate_syms = locate_syms,
		  });
		locate_syms = false;
		goto read_obj;
	      }
	    else
	      {
		unreadchar (source, ch);
		invalid_syntax_with_buffer (&rb, source);
	      }

	  case '(':
	    /* #(...) -- string with properties */
	    read_stack_push ((struct read_stack_entry) {
		.type = RE_string_props,
		.u.vector.elems = Qnil,
		.u.vector.old_locate_syms = locate_syms,
	      });
	    locate_syms = false;
	    goto read_obj;

	  case '[':
	    /* #[...] -- byte-code */
	    read_stack_push ((struct read_stack_entry) {
		.type = RE_byte_code,
		.u.vector.elems = Qnil,
		.u.vector.old_locate_syms = locate_syms,
	      });
	    locate_syms = false;
	    goto read_obj;

	  case '&':
	    /* #&N"..." -- bool-vector */
	    obj = read_bool_vector (source);
	    break;

	  case '!':
	    /* #! appears at the beginning of an executable file.
	       Skip the rest of the line.  */
	    {
	      int c;
	      do
		c = readchar (source);
	      while (c >= 0 && c != '\n');
	      goto read_obj;
	    }

	  case 'x':
	  case 'X':
	    obj = read_integer (source, 16);
	    break;

	  case 'o':
	  case 'O':
	    obj = read_integer (source, 8);
	    break;

	  case 'b':
	  case 'B':
	    obj = read_integer (source, 2);
	    break;

	  case '@':
	    /* #@NUMBER is used to skip NUMBER following bytes.
	       That's used in .elc files to skip over doc strings
	       and function definitions that can be loaded lazily.  */
	    if (skip_lazy_string (source))
	      goto read_obj;
	    obj = Qnil;	      /* #@00 skips to EOB/EOF and yields nil.  */
	    break;

	  case '$':
	    /* #$ -- reference to lazy-loaded string */
	    obj = Vload_file_name;
	    break;

	  case ':':
	    /* #:X -- uninterned symbol */
	    c = readchar (source);
	    if (c <= 32 || c == NO_BREAK_SPACE
		|| c == '"' || c == '\'' || c == ';' || c == '#'
		|| c == '(' || c == ')'  || c == '[' || c == ']'
		|| c == '`' || c == ',')
	      {
		/* No symbol character follows: this is the empty symbol.  */
		unreadchar (source, c);
		obj = Fmake_symbol (empty_unibyte_string);
		break;
	      }
	    uninterned_symbol = true;
	    skip_shorthand = false;
	    goto read_symbol;

	  case '_':
	    /* #_X -- symbol without shorthand */
	    c = readchar (source);
	    if (c <= 32 || c == NO_BREAK_SPACE
		|| c == '"' || c == '\'' || c == ';' || c == '#'
		|| c == '(' || c == ')'  || c == '[' || c == ']'
		|| c == '`' || c == ',')
	      {
		/* No symbol character follows: this is the empty symbol.  */
		unreadchar (source, c);
		obj = Fintern (empty_unibyte_string, Qnil);
		break;
	      }
	    uninterned_symbol = false;
	    skip_shorthand = true;
	    goto read_symbol;

	  default:
	    if (ch >= '0' && ch <= '9')
	      {
		/* #N=OBJ or #N# -- first read the number N */
		EMACS_INT n = ch - '0';
		int c;
		for (;;)
		  {
		    c = read_and_buffer (&rb, source);
		    if (c < '0' || c > '9')
		      break;
		    if (ckd_mul (&n, n, 10)
			|| ckd_add (&n, n, c - '0'))
		      invalid_syntax_with_buffer (&rb, source);
		  }
		if (c == 'r' || c == 'R')
		  {
		    /* #NrDIGITS -- radix-N number */
		    if (n < 2 || n > 36)
		      invalid_radix_integer (n, source);
		    obj = read_integer (source, n);
		    break;
		  }
		else if (n <= MOST_POSITIVE_FIXNUM && !NILP (Vread_circle))
		  {
		    if (c == '=')
		      {
			/* #N=OBJ -- assign number N to OBJ */
			Lisp_Object placeholder = Fcons (Qnil, Qnil);

			struct Lisp_Hash_Table *h
			  = XHASH_TABLE (read_objects_map);
			Lisp_Object number = make_fixnum (n);
			hash_hash_t hash;
			ptrdiff_t i = hash_find_get_hash (h, number, &hash);
			if (i >= 0)
			  /* Not normal, but input could be malformed.  */
			  set_hash_value_slot (h, i, placeholder);
			else
			  hash_put (h, number, placeholder, hash);
			read_stack_push ((struct read_stack_entry) {
			    .type = RE_numbered,
			    .u.numbered.number = number,
			    .u.numbered.placeholder = placeholder,
			  });
			goto read_obj;
		      }
		    else if (c == '#')
		      {
			/* #N# -- reference to numbered object */
			struct Lisp_Hash_Table *h
			  = XHASH_TABLE (read_objects_map);
			ptrdiff_t i = hash_find (h, make_fixnum (n));
			if (i < 0)
			  invalid_syntax_with_buffer (&rb, source);
			obj = HASH_VALUE (h, i);
			break;
		      }
		    else
		      invalid_syntax_with_buffer (&rb, source);
		  }
		else
		  invalid_syntax_with_buffer (&rb, source);
	      }
	    else
	      invalid_syntax_with_buffer (&rb, source);
	  }
	break;
      }

    case '?':
      obj = read_char_literal (source);
      break;

    case '"':
      obj = read_string_literal (source);
      break;

    case '\'':
      read_stack_push ((struct read_stack_entry) {
	  .type = RE_special,
	  .u.special.symbol = Qquote,
	});
      goto read_obj;

    case '`':
      read_stack_push ((struct read_stack_entry) {
	  .type = RE_special,
	  .u.special.symbol = Qbackquote,
	});
      goto read_obj;

    case ',':
      {
	int ch = readchar (source);
	Lisp_Object sym;
	if (ch == '@')
	  sym = Qcomma_at;
	else
	  {
	    if (ch >= 0)
	      unreadchar (source, ch);
	    sym = Qcomma;
	  }
	read_stack_push ((struct read_stack_entry) {
	    .type = RE_special,
	    .u.special.symbol = sym,
	  });
	goto read_obj;
      }

    case ';':
      {
	int c;
	do
	  c = readchar (source);
	while (c >= 0 && c != '\n');
	goto read_obj;
      }

    case '.':
      {
	int nch = readchar (source);
	unreadchar (source, nch);
	if (nch <= 32 || nch == NO_BREAK_SPACE
	    || nch == '"' || nch == '\'' || nch == ';'
	    || nch == '(' || nch == '[' || nch == '#'
	    || nch == '?' || nch == '`' || nch == ',')
	  {
	    if (!read_stack_empty_p (base_sp)
		&& read_stack_top ()->type ==  RE_list)
	      {
		read_stack_top ()->type = RE_list_dot;
		goto read_obj;
	      }
	    invalid_syntax (".", source);
	  }
      }
      /* may be a number or symbol starting with a dot */
      FALLTHROUGH;

    default:
      if (c <= 32 || c == NO_BREAK_SPACE)
	goto read_obj;

      uninterned_symbol = false;
      skip_shorthand = false;
      /* symbol or number */
    read_symbol:
      {
	rb.cur = rb.start;
	bool quoted = false;
	EMACS_INT start_position = readchar_offset - 1;
	ptrdiff_t nchars = 0;

	do
	  {
	    if (c == '\\')
	      {
		c = readchar (source);
		if (c < 0)
		  end_of_file_error (source);
		quoted = true;
	      }

	    add_char_to_buffer (&rb, c, source->multibyte);
	    nchars++;
	    c = readchar (source);
	  }
	while (c > 32
	       && c != NO_BREAK_SPACE
	       && (c >= 128
		   || !(   c == '"' || c == '\'' || c == ';' || c == '#'
			|| c == '(' || c == ')'  || c == '[' || c == ']'
			|| c == '`' || c == ',')));

	*rb.cur = '\0';
	ptrdiff_t nbytes = rb.cur - rb.start;
	unreadchar (source, c);

	/* Only attempt to parse the token as a number if it starts as one.  */
	char c0 = rb.start[0];
	if (((c0 >= '0' && c0 <= '9') || c0 == '.' || c0 == '-' || c0 == '+')
	    && !quoted && !uninterned_symbol && !skip_shorthand)
	  {
	    ptrdiff_t len;
	    Lisp_Object result = string_to_number (rb.start, 10, &len);
	    if (!NILP (result) && len == nbytes)
	      {
		obj = result;
		break;
	      }
	  }

	/* symbol, possibly uninterned */
	Lisp_Object result;
	if (uninterned_symbol)
	  {
	    Lisp_Object name
	      = make_specified_string (rb.start, nchars, nbytes,
				       source->multibyte);
	    result = Fmake_symbol (name);
	  }
	else
	  {
	    /* Don't create the string object for the name unless
	       we're going to retain it in a new symbol.

	       Like intern_1 but supports multibyte names.  */
	    Lisp_Object obarray = check_obarray (Vobarray);

	    char *longhand = NULL;
	    ptrdiff_t longhand_chars = 0;
	    ptrdiff_t longhand_bytes = 0;

	    Lisp_Object found;
	    if (skip_shorthand
		/* We exempt characters used in the "core" Emacs Lisp
		   symbols that are comprised entirely of characters
		   that have the 'symbol constituent' syntax from
		   transforming according to shorthands.  */
		|| symbol_char_span (rb.start) >= nbytes)
	      found = oblookup (obarray, rb.start, nchars, nbytes);
	    else
	      found = oblookup_considering_shorthand (obarray, rb.start,
						      nchars, nbytes, &longhand,
						      &longhand_chars,
						      &longhand_bytes);

	    if (BARE_SYMBOL_P (found))
	      result = found;
	    else if (longhand)
	      {
		Lisp_Object name = make_specified_string (longhand,
							  longhand_chars,
							  longhand_bytes,
							  source->multibyte);
		xfree (longhand);
		result = intern_driver (name, obarray, found);
	      }
	    else
	      {
		Lisp_Object name = make_specified_string (rb.start, nchars,
							  nbytes,
							  source->multibyte);
		result = intern_driver (name, obarray, found);
	      }
	  }
	if (locate_syms && !NILP (result))
	  result = build_symbol_with_pos (result,
					  make_fixnum (start_position));

	obj = result;
	break;
      }
    }

  /* We have read an object in `obj'.  Use the stack to decide what to
     do with it.  */
  while (rdstack.sp > base_sp)
    {
      struct read_stack_entry *e = read_stack_top ();
      switch (e->type)
	{
	case RE_list_start:
	  e->type = RE_list;
	  e->u.list.head = e->u.list.tail = Fcons (obj, Qnil);
	  goto read_obj;

	case RE_list:
	  {
	    Lisp_Object tl = Fcons (obj, Qnil);
	    XSETCDR (e->u.list.tail, tl);
	    e->u.list.tail = tl;
	    goto read_obj;
	  }

	case RE_list_dot:
	  {
	    skip_space_and_comments (source);
	    int ch = readchar (source);
	    if (ch != ')')
	      invalid_syntax ("expected )", source);
	    XSETCDR (e->u.list.tail, obj);
	    read_stack_pop ();
	    obj = e->u.list.head;

	    /* Hack: immediately convert (#$ . FIXNUM) to the corresponding
	       string if load-force-doc-strings is set.  */
	    if (load_force_doc_strings
		&& BASE_EQ (XCAR (obj), Vload_file_name)
		&& !NILP (XCAR (obj))
		&& FIXNUMP (XCDR (obj)))
	      obj = get_lazy_string (obj);

	    break;
	  }

	case RE_vector:
	case RE_record:
	case RE_char_table:
	case RE_sub_char_table:
	case RE_byte_code:
	case RE_string_props:
	  e->u.vector.elems = Fcons (obj, e->u.vector.elems);
	  goto read_obj;

	case RE_special:
	  read_stack_pop ();
	  obj = list2 (e->u.special.symbol, obj);
	  break;

	case RE_numbered:
	  {
	    read_stack_pop ();
	    Lisp_Object placeholder = e->u.numbered.placeholder;
	    if (CONSP (obj))
	      {
		if (BASE_EQ (obj, placeholder))
		  /* Catch silly games like #1=#1# */
		  invalid_syntax ("nonsensical self-reference", source);

		/* Optimization: since the placeholder is already
		   a cons, repurpose it as the actual value.
		   This allows us to skip the substitution below,
		   since the placeholder is already referenced
		   inside OBJ at the appropriate places.  */
		Fsetcar (placeholder, XCAR (obj));
		Fsetcdr (placeholder, XCDR (obj));

		struct Lisp_Hash_Table *h2
		  = XHASH_TABLE (read_objects_completed);
		hash_hash_t hash;
		ptrdiff_t i = hash_find_get_hash (h2, placeholder, &hash);
		eassert (i < 0);
		hash_put (h2, placeholder, Qnil, hash);
		obj = placeholder;
	      }
	    else
	      {
		/* If it can be recursive, remember it for future
		   substitutions.  */
		if (!SYMBOLP (obj) && !NUMBERP (obj)
		    && !(STRINGP (obj) && !string_intervals (obj)))
		  {
		    struct Lisp_Hash_Table *h2
		      = XHASH_TABLE (read_objects_completed);
		    hash_hash_t hash;
		    ptrdiff_t i = hash_find_get_hash (h2, obj, &hash);
		    eassert (i < 0);
		    hash_put (h2, obj, Qnil, hash);
		  }

		/* Now put it everywhere the placeholder was...  */
		Flread__substitute_object_in_subtree (obj, placeholder,
						      read_objects_completed);

		/* ...and #n# will use the real value from now on.  */
		struct Lisp_Hash_Table *h = XHASH_TABLE (read_objects_map);
		hash_hash_t hash;
		ptrdiff_t i = hash_find_get_hash (h, e->u.numbered.number,
						  &hash);
		eassert (i >= 0);
		set_hash_value_slot (h, i, obj);
	      }
	    break;
	  }
	}
    }

  return unbind_to (base_pdl, obj);
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
      || hash_find (XHASH_TABLE (subst->completed), subtree) >= 0)
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
		 || CLOSUREP (subtree) || HASH_TABLE_P (subtree)
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


#if !IEEE_FLOATING_POINT
/* Strings that stand in for +NaN, -NaN, respectively.  */
static Lisp_Object not_a_number[2];
#endif

/* Convert the initial prefix of STRING to a number, assuming base BASE.
   If the prefix has floating point syntax and BASE is 10, return a
   nearest float; otherwise, if the prefix has integer syntax, return
   the integer; otherwise, return nil.  (On antique platforms that lack
   support for NaNs, if the prefix has NaN syntax return a Lisp object that
   will provoke an error if used as a number.)  If PLEN, set *PLEN to the
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

  enum { INTOVERFLOW = 1, LEAD_INT = 2, TRAIL_INT = 4, E_EXP = 16 };
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
#if IEEE_FLOATING_POINT
	      union ieee754_double u
		= { .ieee_nan = { .exponent = 0x7ff, .quiet_nan = 1,
				  .mantissa0 = n >> 31 >> 1, .mantissa1 = n }};
	      value = u.d;
#else
	      if (plen)
		*plen = cp - string;
	      return not_a_number[negative];
#endif
	    }
	  else
	    cp = ecp;
	}

      /* A float has digits after the dot or an exponent.
	 This excludes numbers like "1." which are lexed as integers. */
      float_syntax = ((state & TRAIL_INT)
		      || ((state & LEAD_INT) && (state & E_EXP)));
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


static Lisp_Object initial_obarray;

static Lisp_Object make_obarray (unsigned bits);

/* Slow path obarray check: return the obarray to use or signal an error.  */
Lisp_Object
check_obarray_slow (Lisp_Object obarray)
{
  /* For compatibility, we accept vectors whose first element is 0,
     and store an obarray object there.  */
  if (VECTORP (obarray) && ASIZE (obarray) > 0)
    {
      Lisp_Object obj = AREF (obarray, 0);
      if (OBARRAYP (obj))
	return obj;
      if (BASE_EQ (obj, make_fixnum (0)))
	{
	  /* Put an actual obarray object in the first slot.
	     The rest of the vector remains unused.  */
	  obj = make_obarray (0);
	  ASET (obarray, 0, obj);
	  return obj;
	}
    }
  /* Reset Vobarray to the standard obarray for nicer error handling. */
  if (BASE_EQ (Vobarray, obarray)) Vobarray = initial_obarray;

  wrong_type_argument (Qobarrayp, obarray);
}

static void grow_obarray (struct Lisp_Obarray *o);

/* Intern symbol SYM in OBARRAY using bucket INDEX.  */

/* FIXME: retype arguments as pure C types */
static Lisp_Object
intern_sym (Lisp_Object sym, Lisp_Object obarray, Lisp_Object index)
{
  eassert (BARE_SYMBOL_P (sym) && OBARRAYP (obarray) && FIXNUMP (index));
  struct Lisp_Symbol *s = XBARE_SYMBOL (sym);
  s->u.s.interned = (BASE_EQ (obarray, initial_obarray)
		     ? SYMBOL_INTERNED_IN_INITIAL_OBARRAY
		     : SYMBOL_INTERNED);

  if (SREF (s->u.s.name, 0) == ':' && BASE_EQ (obarray, initial_obarray))
    {
      s->u.s.trapped_write = SYMBOL_NOWRITE;
      s->u.s.redirect = SYMBOL_PLAINVAL;
      /* Mark keywords as special.  This makes (let ((:key 'foo)) ...)
	 in lexically bound elisp signal an error, as documented.  */
      s->u.s.declared_special = true;
      SET_SYMBOL_VAL (s, sym);
    }

  struct Lisp_Obarray *o = XOBARRAY (obarray);
  Lisp_Object *ptr = o->buckets + XFIXNUM (index);
  s->u.s.next = BARE_SYMBOL_P (*ptr) ? XBARE_SYMBOL (*ptr) : NULL;
  *ptr = sym;
  o->count++;
  if (o->count > obarray_size (o))
    grow_obarray (o);
  return sym;
}

/* Intern a symbol with name STRING in OBARRAY using bucket INDEX.  */

Lisp_Object
intern_driver (Lisp_Object string, Lisp_Object obarray, Lisp_Object index)
{
  SET_SYMBOL_VAL (XBARE_SYMBOL (Qobarray_cache), Qnil);
  return intern_sym (Fmake_symbol (string), obarray, index);
}

/* Intern the C string STR: return a symbol with that name,
   interned in the current obarray.  */

Lisp_Object
intern_1 (const char *str, ptrdiff_t len)
{
  Lisp_Object obarray = check_obarray (Vobarray);
  Lisp_Object tem = oblookup (obarray, str, len, len);

  return (BARE_SYMBOL_P (tem) ? tem
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

  if (!BARE_SYMBOL_P (tem))
    {
      Lisp_Object string;

      string = make_string (str, len);

      tem = intern_driver (string, obarray, tem);
    }
  return tem;
}

/* Intern STR of NBYTES bytes and NCHARS characters in the default obarray.  */
Lisp_Object
intern_c_multibyte (const char *str, ptrdiff_t nchars, ptrdiff_t nbytes)
{
  Lisp_Object obarray = check_obarray (Vobarray);
  Lisp_Object sym = oblookup (obarray, str, nchars, nbytes);
  if (BARE_SYMBOL_P (sym))
    return sym;
  return intern_driver (make_multibyte_string (str, nchars, nbytes),
			obarray, sym);
}

static void
define_symbol (Lisp_Object sym, char const *str)
{
  ptrdiff_t len = strlen (str);
  Lisp_Object string = make_string (str, len);
  init_symbol (sym, string);

  /* Qunbound is uninterned, so that it's not confused with any symbol
     'unbound' created by a Lisp program.  */
  if (! BASE_EQ (sym, Qunbound))
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


  char* longhand = NULL;
  ptrdiff_t longhand_chars = 0;
  ptrdiff_t longhand_bytes = 0;
  tem = oblookup_considering_shorthand (obarray, SSDATA (string),
					SCHARS (string), SBYTES (string),
					&longhand, &longhand_chars,
					&longhand_bytes);

  if (!BARE_SYMBOL_P (tem))
    {
      if (longhand)
	{
	  tem = intern_driver (make_multibyte_string (longhand, longhand_chars,
						      longhand_bytes),
			       obarray, tem);
	  xfree (longhand);
	}
      else
	tem = intern_driver (string, obarray, tem);
    }
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
      char *longhand = NULL;
      ptrdiff_t longhand_chars = 0;
      ptrdiff_t longhand_bytes = 0;

      CHECK_STRING (name);
      string = name;
      tem = oblookup_considering_shorthand (obarray, SSDATA (string),
					    SCHARS (string), SBYTES (string),
					    &longhand, &longhand_chars,
					    &longhand_bytes);
      if (longhand)
	xfree (longhand);
      return FIXNUMP (tem) ? Qnil : tem;
    }
  else
    {
      /* If already a symbol, we don't do shorthand-longhand translation,
	 as promised in the docstring.  */
      Lisp_Object sym = maybe_remove_pos_from_symbol (name);
      string = XSYMBOL (name)->u.s.name;
      tem
	= oblookup (obarray, SSDATA (string), SCHARS (string), SBYTES (string));
      return BASE_EQ (sym, tem) ? name : Qnil;
    }
}

/* Bucket index of the string STR of length SIZE_BYTE bytes in obarray OA.  */
static ptrdiff_t
obarray_index (struct Lisp_Obarray *oa, const char *str, ptrdiff_t size_byte)
{
  EMACS_UINT hash = hash_char_array (str, size_byte);
  return knuth_hash (reduce_emacs_uint_to_hash_hash (hash), oa->size_bits);
}

DEFUN ("unintern", Funintern, Sunintern, 2, 2, 0,
       doc: /* Delete the symbol named NAME, if any, from OBARRAY.
The value is t if a symbol was found and deleted, nil otherwise.
NAME may be a string or a symbol.  If it is a symbol, that symbol
is deleted, if it belongs to OBARRAY--no other symbol is deleted.
OBARRAY, if nil, defaults to the value of the variable `obarray'.  */)
  (Lisp_Object name, Lisp_Object obarray)
{
  if (NILP (obarray)) obarray = Vobarray;
  obarray = check_obarray (obarray);

  Lisp_Object sym;
  if (SYMBOLP (name))
    sym = BARE_SYMBOL_P (name) ? name : XSYMBOL_WITH_POS (name)->sym;
  else
    {
      CHECK_STRING (name);
      char *longhand = NULL;
      ptrdiff_t longhand_chars = 0;
      ptrdiff_t longhand_bytes = 0;
      sym = oblookup_considering_shorthand (obarray, SSDATA (name),
					    SCHARS (name), SBYTES (name),
					    &longhand, &longhand_chars,
					    &longhand_bytes);
      xfree(longhand);
      if (FIXNUMP (sym))
	return Qnil;
    }

  /* There are plenty of symbols which will screw up the Emacs
     session if we unintern them, as well as even more ways to use
     `setq' or `fset' or whatnot to make the Emacs session
     unusable.  We don't try to prevent such mistakes here.  */

  struct Lisp_Obarray *o = XOBARRAY (obarray);
  Lisp_Object symname = SYMBOL_NAME (sym);
  ptrdiff_t idx = obarray_index (o, SSDATA (symname), SBYTES (symname));
  Lisp_Object *loc = &o->buckets[idx];
  if (BASE_EQ (*loc, make_fixnum (0)))
    return Qnil;

  struct Lisp_Symbol *s = XBARE_SYMBOL (sym);
  struct Lisp_Symbol *prev = XBARE_SYMBOL (*loc);
  if (prev == s)
    *loc = s->u.s.next ? make_lisp_symbol (s->u.s.next) : make_fixnum (0);
  else
    {
      do
	{
	  struct Lisp_Symbol *next = prev->u.s.next;
	  if (next == s)
	    {
	      prev->u.s.next = next->u.s.next;
	      goto removed;
	    }
	  prev = next;
	}
      while (prev);
      return Qnil;
    }

 removed:
  s->u.s.interned = SYMBOL_UNINTERNED;
  o->count--;
  return Qt;
}


/* Return the symbol in OBARRAY whose name matches the string
   of SIZE characters (SIZE_BYTE bytes) at PTR.
   If there is no such symbol, return the integer bucket number of
   where the symbol would be if it were present.  */

Lisp_Object
oblookup (Lisp_Object obarray, register const char *ptr, ptrdiff_t size, ptrdiff_t size_byte)
{
  struct Lisp_Obarray *o = XOBARRAY (obarray);
  ptrdiff_t idx = obarray_index (o, ptr, size_byte);
  Lisp_Object bucket = o->buckets[idx];

  if (!BASE_EQ (bucket, make_fixnum (0)))
    {
      Lisp_Object sym = bucket;
      while (1)
	{
	  struct Lisp_Symbol *s = XBARE_SYMBOL (sym);
	  Lisp_Object name = s->u.s.name;
	  if (SBYTES (name) == size_byte && SCHARS (name) == size
	      && memcmp (SDATA (name), ptr, size_byte) == 0)
	    return sym;
	  if (s->u.s.next == NULL)
	    break;
	  sym = make_lisp_symbol(s->u.s.next);
	}
    }
  return make_fixnum (idx);
}

/* Like 'oblookup', but considers 'Vread_symbol_shorthands',
   potentially recognizing that IN is shorthand for some other
   longhand name, which is then placed in OUT.  In that case,
   memory is malloc'ed for OUT (which the caller must free) while
   SIZE_OUT and SIZE_BYTE_OUT respectively hold the character and byte
   sizes of the transformed symbol name.  If IN is not recognized
   shorthand for any other symbol, OUT is set to point to NULL and
   'oblookup' is called.  */

Lisp_Object
oblookup_considering_shorthand (Lisp_Object obarray, const char *in,
				ptrdiff_t size, ptrdiff_t size_byte, char **out,
				ptrdiff_t *size_out, ptrdiff_t *size_byte_out)
{
  Lisp_Object tail = Vread_symbol_shorthands;

  /* First, assume no transformation will take place.  */
  *out = NULL;
  /* Then, iterate each pair in Vread_symbol_shorthands.  */
  FOR_EACH_TAIL_SAFE (tail)
    {
      Lisp_Object pair = XCAR (tail);
      /* Be lenient to 'read-symbol-shorthands': if some element isn't a
	 cons, or some member of that cons isn't a string, just skip
	 to the next element.  */
      if (!CONSP (pair))
	continue;
      Lisp_Object sh_prefix = XCAR (pair);
      Lisp_Object lh_prefix = XCDR (pair);
      if (!STRINGP (sh_prefix) || !STRINGP (lh_prefix))
	continue;
      ptrdiff_t sh_prefix_size = SBYTES (sh_prefix);

      /* Compare the prefix of the transformation pair to the symbol
	 name.  If a match occurs, do the renaming and exit the loop.
	 In other words, only one such transformation may take place.
	 Calculate the amount of memory to allocate for the longhand
	 version of the symbol name with xrealloc.  This isn't
	 strictly needed, but it could later be used as a way for
	 multiple transformations on a single symbol name.  */
      if (sh_prefix_size <= size_byte
	  && memcmp (SSDATA (sh_prefix), in, sh_prefix_size) == 0)
	{
	  ptrdiff_t lh_prefix_size = SBYTES (lh_prefix);
	  ptrdiff_t suffix_size = size_byte - sh_prefix_size;
	  *out = xrealloc (*out, lh_prefix_size + suffix_size);
	  memcpy (*out, SSDATA(lh_prefix), lh_prefix_size);
	  memcpy (*out + lh_prefix_size, in + sh_prefix_size, suffix_size);
	  *size_out = SCHARS (lh_prefix) - SCHARS (sh_prefix) + size;
	  *size_byte_out = lh_prefix_size + suffix_size;
	  break;
	}
    }
  /* Now, as promised, call oblookup with the "final" symbol name to
     lookup.  That function remains oblivious to whether a
     transformation happened here or not, but the caller of this
     function can tell by inspecting the OUT parameter.  */
  if (*out)
    return oblookup (obarray, *out, *size_out, *size_byte_out);
  else
    return oblookup (obarray, in, size, size_byte);
}


static struct Lisp_Obarray *
allocate_obarray (void)
{
  return ALLOCATE_PLAIN_PSEUDOVECTOR (struct Lisp_Obarray, PVEC_OBARRAY);
}

static Lisp_Object
make_obarray (unsigned bits)
{
  struct Lisp_Obarray *o = allocate_obarray ();
  o->count = 0;
  o->size_bits = bits;
  ptrdiff_t size = (ptrdiff_t)1 << bits;
  o->buckets = hash_table_alloc_bytes (size * sizeof *o->buckets);
  for (ptrdiff_t i = 0; i < size; i++)
    o->buckets[i] = make_fixnum (0);
  return make_lisp_obarray (o);
}

enum {
  obarray_default_bits = 3,
  word_size_log2 = word_size < 8 ? 5 : 6,  /* good enough */
  obarray_max_bits = min (8 * sizeof (int),
			  8 * sizeof (ptrdiff_t) - word_size_log2) - 1,
};

static void
grow_obarray (struct Lisp_Obarray *o)
{
  ptrdiff_t old_size = obarray_size (o);
  eassert (o->count > old_size);
  Lisp_Object *old_buckets = o->buckets;

  int new_bits = o->size_bits + 1;
  if (new_bits > obarray_max_bits)
    error ("Obarray too big");
  ptrdiff_t new_size = (ptrdiff_t)1 << new_bits;
  o->buckets = hash_table_alloc_bytes (new_size * sizeof *o->buckets);
  for (ptrdiff_t i = 0; i < new_size; i++)
    o->buckets[i] = make_fixnum (0);
  o->size_bits = new_bits;

  /* Rehash symbols.
     FIXME: this is expensive since we need to recompute the hash for every
     symbol name.  Would it be reasonable to store it in the symbol?  */
  for (ptrdiff_t i = 0; i < old_size; i++)
    {
      Lisp_Object obj = old_buckets[i];
      if (BARE_SYMBOL_P (obj))
	{
	  struct Lisp_Symbol *s = XBARE_SYMBOL (obj);
	  while (1)
	    {
	      Lisp_Object name = s->u.s.name;
	      ptrdiff_t idx = obarray_index (o, SSDATA (name), SBYTES (name));
	      Lisp_Object *loc = o->buckets + idx;
	      struct Lisp_Symbol *next = s->u.s.next;
	      s->u.s.next = BARE_SYMBOL_P (*loc) ? XBARE_SYMBOL (*loc) : NULL;
	      *loc = make_lisp_symbol (s);
	      if (next == NULL)
		break;
	      s = next;
	    }
	}
    }

  hash_table_free_bytes (old_buckets, old_size * sizeof *old_buckets);
}

DEFUN ("obarray-make", Fobarray_make, Sobarray_make, 0, 1, 0,
       doc: /* Return a new obarray of size SIZE.
The obarray will grow to accommodate any number of symbols; the size, if
given, is only a hint for the expected number.  */)
  (Lisp_Object size)
{
  int bits;
  if (NILP (size))
    bits = obarray_default_bits;
  else
    {
      CHECK_FIXNAT (size);
      EMACS_UINT n = XFIXNUM (size);
      bits = elogb (n) + 1;
      if (bits > obarray_max_bits)
	xsignal (Qargs_out_of_range, size);
    }
  return make_obarray (bits);
}

DEFUN ("obarrayp", Fobarrayp, Sobarrayp, 1, 1, 0,
       doc: /* Return t iff OBJECT is an obarray.  */)
  (Lisp_Object object)
{
  return OBARRAYP (object) ? Qt : Qnil;
}

DEFUN ("obarray-clear", Fobarray_clear, Sobarray_clear, 1, 1, 0,
       doc: /* Remove all symbols from OBARRAY.  */)
  (Lisp_Object obarray)
{
  CHECK_OBARRAY (obarray);
  struct Lisp_Obarray *o = XOBARRAY (obarray);

  /* This function does not bother setting the status of its contained symbols
     to uninterned.  It doesn't matter very much.  */
  int new_bits = obarray_default_bits;
  int new_size = (ptrdiff_t)1 << new_bits;
  Lisp_Object *new_buckets
    = hash_table_alloc_bytes (new_size * sizeof *new_buckets);
  for (ptrdiff_t i = 0; i < new_size; i++)
    new_buckets[i] = make_fixnum (0);

  int old_size = obarray_size (o);
  hash_table_free_bytes (o->buckets, old_size * sizeof *o->buckets);
  o->buckets = new_buckets;
  o->size_bits = new_bits;
  o->count = 0;

  return Qnil;
}

void
map_obarray (Lisp_Object obarray,
	     void (*fn) (Lisp_Object, Lisp_Object), Lisp_Object arg)
{
  CHECK_OBARRAY (obarray);
  DOOBARRAY (XOBARRAY (obarray), it)
    (*fn) (obarray_iter_symbol (&it), arg);
}

static void
mapatoms_1 (Lisp_Object sym, Lisp_Object function)
{
  calln (function, sym);
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

DEFUN ("internal--obarray-buckets",
       Finternal__obarray_buckets, Sinternal__obarray_buckets, 1, 1, 0,
       doc: /* Symbols in each bucket of OBARRAY.  Internal use only.  */)
    (Lisp_Object obarray)
{
  obarray = check_obarray (obarray);
  ptrdiff_t size = obarray_size (XOBARRAY (obarray));

  Lisp_Object ret = Qnil;
  for (ptrdiff_t i = 0; i < size; i++)
    {
      Lisp_Object bucket = Qnil;
      Lisp_Object sym = XOBARRAY (obarray)->buckets[i];
      if (BARE_SYMBOL_P (sym))
	while (1)
	  {
	    bucket = Fcons (sym, bucket);
	    struct Lisp_Symbol *s = XBARE_SYMBOL (sym)->u.s.next;
	    if (!s)
	      break;
	    sym = make_lisp_symbol (s);
	  }
      ret = Fcons (Fnreverse (bucket), ret);
    }
  return Fnreverse (ret);
}

void
init_obarray_once (void)
{
  Vobarray = make_obarray (15);
  initial_obarray = Vobarray;
  staticpro (&initial_obarray);

  for (int i = 0; i < ARRAYELTS (lispsym); i++)
    define_symbol (builtin_lisp_symbol (i), defsym_name[i]);

  DEFSYM (Qunbound, "unbound");

  DEFSYM (Qnil, "nil");
  SET_SYMBOL_VAL (XBARE_SYMBOL (Qnil), Qnil);
  make_symbol_constant (Qnil);
  XBARE_SYMBOL (Qnil)->u.s.declared_special = true;

  DEFSYM (Qt, "t");
  SET_SYMBOL_VAL (XBARE_SYMBOL (Qt), Qt);
  make_symbol_constant (Qt);
  XBARE_SYMBOL (Qt)->u.s.declared_special = true;

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
#ifdef HAVE_NATIVE_COMP
  eassert (NILP (Vcomp_abi_hash));
  Vcomp_subr_list = Fcons (tem, Vcomp_subr_list);
#endif
}

/* Define an "integer variable"; a symbol whose value is forwarded to a
   C variable of type intmax_t.  Sample call (with "xx" to fool make-docfile):
   DEFxxVAR_INT ("emacs-priority", &emacs_priority, "Documentation");  */
void
defvar_int (struct Lisp_Intfwd const *i_fwd, char const *namestring)
{
  Lisp_Object sym = intern_c_string (namestring);
  XBARE_SYMBOL (sym)->u.s.declared_special = true;
  XBARE_SYMBOL (sym)->u.s.redirect = SYMBOL_FORWARDED;
  SET_SYMBOL_FWD (XBARE_SYMBOL (sym), i_fwd);
}

/* Similar but define a variable whose value is t if 1, nil if 0.  */
void
defvar_bool (struct Lisp_Boolfwd const *b_fwd, char const *namestring)
{
  Lisp_Object sym = intern_c_string (namestring);
  XBARE_SYMBOL (sym)->u.s.declared_special = true;
  XBARE_SYMBOL (sym)->u.s.redirect = SYMBOL_FORWARDED;
  SET_SYMBOL_FWD (XBARE_SYMBOL (sym), b_fwd);
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
  XBARE_SYMBOL (sym)->u.s.declared_special = true;
  XBARE_SYMBOL (sym)->u.s.redirect = SYMBOL_FORWARDED;
  SET_SYMBOL_FWD (XBARE_SYMBOL (sym), o_fwd);
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
  XBARE_SYMBOL (sym)->u.s.declared_special = true;
  XBARE_SYMBOL (sym)->u.s.redirect = SYMBOL_FORWARDED;
  SET_SYMBOL_FWD (XBARE_SYMBOL (sym), ko_fwd);
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

  lpath = decode_env_path (0, PATH_LOADSEARCH, 0);

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
  Vload_true_file_name = Qnil;
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
  defsubr (&Sread_positioning_symbols);
  defsubr (&Sread_from_string);
  defsubr (&Slread__substitute_object_in_subtree);
  defsubr (&Sintern);
  defsubr (&Sintern_soft);
  defsubr (&Sunintern);
  defsubr (&Sget_load_suffixes);
  defsubr (&Sload);
  defsubr (&Seval_buffer);
  defsubr (&Seval_region);
  defsubr (&Smapatoms);
  defsubr (&Slocate_file_internal);
  defsubr (&Sinternal__obarray_buckets);
  defsubr (&Sobarray_make);
  defsubr (&Sobarrayp);
  defsubr (&Sobarray_clear);

  DEFVAR_LISP ("obarray", Vobarray,
	       doc: /* Symbol table for use by `intern' and `read'.
It is a vector whose length ought to be prime for best results.
The vector's contents don't make sense if examined from Lisp programs;
to find all the symbols in an obarray, use `mapatoms'.  */);

  DEFVAR_LISP ("values", Vvalues,
	       doc: /* List of values of all expressions which were read, evaluated and printed.
Order is reverse chronological.
This variable is obsolete as of Emacs 28.1 and should not be used.  */);
  XBARE_SYMBOL (intern ("values"))->u.s.declared_special = false;

  DEFVAR_LISP ("standard-input", Vstandard_input,
	       doc: /* Stream for read to get input from.
See documentation of `read' for possible values.  */);
  Vstandard_input = Qt;

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
  Vload_suffixes = list2 (build_string (".elc"),
			  build_string (".el"));
#ifdef HAVE_MODULES
  Vload_suffixes = Fcons (build_string (MODULES_SUFFIX), Vload_suffixes);
#ifdef MODULES_SECONDARY_SUFFIX
  Vload_suffixes =
    Fcons (build_string (MODULES_SECONDARY_SUFFIX), Vload_suffixes);
#endif
#endif
  DEFVAR_LISP ("module-file-suffix", Vmodule_file_suffix,
	       doc: /* Suffix of loadable module file, or nil if modules are not supported.  */);
#ifdef HAVE_MODULES
  Vmodule_file_suffix = build_string (MODULES_SUFFIX);
#else
  Vmodule_file_suffix = Qnil;
#endif

  DEFVAR_LISP ("dynamic-library-suffixes", Vdynamic_library_suffixes,
	       doc: /* A list of suffixes for loadable dynamic libraries.  */);

#ifndef MSDOS
  Vdynamic_library_suffixes
    = Fcons (build_string (DYNAMIC_LIB_SECONDARY_SUFFIX), Qnil);
  Vdynamic_library_suffixes
    = Fcons (build_string (DYNAMIC_LIB_SUFFIX),
	     Vdynamic_library_suffixes);
#else
  Vdynamic_library_suffixes = Qnil;
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

  DEFSYM (Qjka_compr_load_suffixes, "jka-compr-load-suffixes");

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
The file name is absolute.
As an exception, one of the alist elements may have FILE-NAME nil,
for symbols and features not associated with any file.

The remaining ENTRIES in the alist element describe the functions and
variables defined in that file, the features provided, and the
features required.  Each entry has the form `(provide . FEATURE)',
`(require . FEATURE)', `(defun . FUNCTION)', `(defface . SYMBOL)',
 `(define-type . SYMBOL)', or `(cl-defmethod METHOD SPECIALIZERS)'.
In addition, entries may also be single symbols,
which means that symbol was defined by `defvar' or `defconst'.

During preloading, the file name recorded is relative to the main Lisp
directory.  These file names are converted to absolute at startup.  */);
  Vload_history = Qnil;

  DEFVAR_LISP ("load-file-name", Vload_file_name,
	       doc: /* Full name of file being loaded by `load'.

In case of native code being loaded this is indicating the
corresponding bytecode filename.  Use `load-true-file-name' to obtain
the .eln filename.  */);
  Vload_file_name = Qnil;

  DEFVAR_LISP ("load-true-file-name", Vload_true_file_name,
	       doc: /* Full name of file being loaded by `load'.  */);
  Vload_true_file_name = Qnil;

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
	       doc: /* Function used for reading expressions.
It is used by `load' and `eval-region'.

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
to load.  */);
  Vbytecomp_version_regexp
    = build_string ("^;;;.\\(in Emacs version\\|bytecomp version FSF\\)");

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

  DEFVAR_BOOL ("load-no-native", load_no_native,
               doc: /* Non-nil means not to load native code unless explicitly requested.

To load a `.eln' file when this variable is non-nil, use `(load FILE)'
where FILE is the filename of the eln file, including the .eln extension.
`load-no-native' non-nil will also make Emacs not load native code
through `require'.  */);
  load_no_native = false;

  DEFVAR_LISP ("load-path-filter-function",
	       Vload_path_filter_function,
	       doc: /* If non-nil, a function to filter `load-path' for `load'.

If this variable is a function, it is called when `load' is about to
search for a file along `load-path'.  This function is called with three
arguments: the current value of `load-path' (a list of directories),
the FILE argument to `load', and the current list of load-suffixes.

It should return a (hopefully shorter) list of directories, which `load'
will use instead of `load-path' to look for the file to load.  */);
  Vload_path_filter_function = Qnil;

  /* Vsource_directory was initialized in init_lread.  */

  DEFSYM (Qcurrent_load_list, "current-load-list");
  DEFSYM (Qstandard_input, "standard-input");
  DEFSYM (Qread_char, "read-char");

  DEFSYM (Qget_file_char, "get-file-char");

  /* Used instead of Qget_file_char while loading *.elc files compiled
     by Emacs 21 or older.  */
  DEFSYM (Qget_emacs_mule_file_char, "get-emacs-mule-file-char");

  /* These are only used as internal READCHARFUN in the C code and
     cannot be used from Lisp.  */
  Funintern (Qget_file_char, Qnil);
  Funintern (Qget_emacs_mule_file_char, Qnil);

  DEFSYM (Qload_force_doc_strings, "load-force-doc-strings");

  DEFSYM (Qbackquote, "`");
  DEFSYM (Qcomma, ",");
  DEFSYM (Qcomma_at, ",@");

#if !IEEE_FLOATING_POINT
  for (int negative = 0; negative < 2; negative++)
    {
      not_a_number[negative] = build_string (&"-0.0e+NaN"[!negative]);
      staticpro (&not_a_number[negative]);
    }
#endif

  DEFSYM (Qinhibit_file_name_operation, "inhibit-file-name-operation");
  DEFSYM (Qfunction, "function");
  DEFSYM (Qload, "load");
  DEFSYM (Qload_file_name, "load-file-name");
  DEFSYM (Qload_true_file_name, "load-true-file-name");
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

  DEFSYM (Qchar_from_name, "char-from-name");

  DEFVAR_LISP ("internal--get-default-lexical-binding-function",
	       Vinternal__get_default_lexical_binding_function,
	       doc: /* Function to decide default lexical-binding.  */);
  Vinternal__get_default_lexical_binding_function = Qnil;

  DEFSYM (Qread_symbol_shorthands, "read-symbol-shorthands");
  DEFVAR_LISP ("read-symbol-shorthands", Vread_symbol_shorthands,
          doc: /* Alist of known symbol-name shorthands.
This variable's value can only be set via file-local variables.
See Info node `(elisp)Shorthands' for more details.  */);
  Vread_symbol_shorthands = Qnil;
  DEFSYM (Qobarray_cache, "obarray-cache");
  DEFSYM (Qobarrayp, "obarrayp");

  DEFSYM (Qmacroexp__dynvars, "macroexp--dynvars");
  DEFVAR_LISP ("macroexp--dynvars", Vmacroexp__dynvars,
        doc:   /* List of variables declared dynamic in the current scope.
Only valid during macro-expansion.  Internal use only. */);
  Vmacroexp__dynvars = Qnil;

  DEFSYM (Qinternal_macroexpand_for_load,
	  "internal-macroexpand-for-load");
  DEFSYM (Qread_minibuffer, "read-minibuffer");
}
