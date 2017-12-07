/* Interface to zlib.
   Copyright (C) 2013-2017 Free Software Foundation, Inc.

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

#ifdef HAVE_ZLIB

#include <zlib.h>

#include "lisp.h"
#include "buffer.h"

#include <verify.h>

#ifdef WINDOWSNT
# include <windows.h>
# include "w32.h"

DEF_DLL_FN (int, inflateInit2_,
	    (z_streamp strm, int windowBits, const char *version,
	     int stream_size));
DEF_DLL_FN (int, inflate, (z_streamp strm, int flush));
DEF_DLL_FN (int, inflateEnd, (z_streamp strm));

static bool zlib_initialized;

static bool
init_zlib_functions (void)
{
  HMODULE library = w32_delayed_load (Qzlib);

  if (!library)
    return false;

  LOAD_DLL_FN (library, inflateInit2_);
  LOAD_DLL_FN (library, inflate);
  LOAD_DLL_FN (library, inflateEnd);
  return true;
}

# undef inflate
# undef inflateEnd
# undef inflateInit2_

# define inflate fn_inflate
# define inflateEnd fn_inflateEnd
# define inflateInit2_ fn_inflateInit2_

#endif	/* WINDOWSNT */


struct decompress_unwind_data
{
  ptrdiff_t old_point, start, nbytes;
  z_stream *stream;
};

static void
unwind_decompress (void *ddata)
{
  struct decompress_unwind_data *data = ddata;
  inflateEnd (data->stream);

  /* Delete any uncompressed data already inserted on error.  */
  if (data->start)
    del_range (data->start, data->start + data->nbytes);

  /* Put point where it was, or if the buffer has shrunk because the
     compressed data is bigger than the uncompressed, at
     point-max.  */
  SET_PT (min (data->old_point, ZV));
}

DEFUN ("zlib-available-p", Fzlib_available_p, Szlib_available_p, 0, 0, 0,
       doc: /* Return t if zlib decompression is available in this instance of Emacs.  */)
     (void)
{
#ifdef WINDOWSNT
  Lisp_Object found = Fassq (Qzlib, Vlibrary_cache);
  if (CONSP (found))
    return XCDR (found);
  else
    {
      Lisp_Object status;
      zlib_initialized = init_zlib_functions ();
      status = zlib_initialized ? Qt : Qnil;
      Vlibrary_cache = Fcons (Fcons (Qzlib, status), Vlibrary_cache);
      return status;
    }
#else
  return Qt;
#endif
}

DEFUN ("zlib-decompress-region", Fzlib_decompress_region,
       Szlib_decompress_region,
       2, 2, 0,
       doc: /* Decompress a gzip- or zlib-compressed region.
Replace the text in the region by the decompressed data.
On failure, return nil and leave the data in place.
This function can be called only in unibyte buffers.  */)
  (Lisp_Object start, Lisp_Object end)
{
  ptrdiff_t istart, iend, pos_byte;
  z_stream stream;
  int inflate_status;
  struct decompress_unwind_data unwind_data;
  ptrdiff_t count = SPECPDL_INDEX ();

  validate_region (&start, &end);

  if (! NILP (BVAR (current_buffer, enable_multibyte_characters)))
    error ("This function can be called only in unibyte buffers");

#ifdef WINDOWSNT
  if (!zlib_initialized)
    zlib_initialized = init_zlib_functions ();
  if (!zlib_initialized)
    {
      message1 ("zlib library not found");
      return Qnil;
    }
#endif

  /* This is a unibyte buffer, so character positions and bytes are
     the same.  */
  istart = XINT (start);
  iend = XINT (end);
  move_gap_both (iend, iend);

  stream.zalloc = Z_NULL;
  stream.zfree = Z_NULL;
  stream.opaque = Z_NULL;
  stream.avail_in = 0;
  stream.next_in = Z_NULL;

  /* The magic number 32 apparently means "autodetect both the gzip and
     zlib formats" according to zlib.h.  */
  if (inflateInit2 (&stream, MAX_WBITS + 32) != Z_OK)
    return Qnil;

  unwind_data.start = iend;
  unwind_data.stream = &stream;
  unwind_data.old_point = PT;
  unwind_data.nbytes = 0;
  record_unwind_protect_ptr (unwind_decompress, &unwind_data);

  /* Insert the decompressed data at the end of the compressed data.  */
  SET_PT (iend);

  pos_byte = istart;

  /* Keep calling 'inflate' until it reports an error or end-of-input.  */
  do
    {
      /* Maximum number of bytes that one 'inflate' call should read and write.
	 Do not make avail_out too large, as that might unduly delay C-g.
	 zlib requires that avail_in and avail_out not exceed UINT_MAX.  */
      ptrdiff_t avail_in = min (iend - pos_byte, UINT_MAX);
      int avail_out = 16 * 1024;
      int decompressed;

      if (GAP_SIZE < avail_out)
	make_gap (avail_out - GAP_SIZE);
      stream.next_in = BYTE_POS_ADDR (pos_byte);
      stream.avail_in = avail_in;
      stream.next_out = GPT_ADDR;
      stream.avail_out = avail_out;
      inflate_status = inflate (&stream, Z_NO_FLUSH);
      pos_byte += avail_in - stream.avail_in;
      decompressed = avail_out - stream.avail_out;
      insert_from_gap (decompressed, decompressed, 0);
      unwind_data.nbytes += decompressed;
      maybe_quit ();
    }
  while (inflate_status == Z_OK);

  if (inflate_status != Z_STREAM_END)
    return unbind_to (count, Qnil);

  unwind_data.start = 0;

  /* Delete the compressed data.  */
  del_range (istart, iend);

  return unbind_to (count, Qt);
}


/***********************************************************************
			    Initialization
 ***********************************************************************/
void
syms_of_decompress (void)
{
  defsubr (&Szlib_decompress_region);
  defsubr (&Szlib_available_p);
}

#endif /* HAVE_ZLIB */
