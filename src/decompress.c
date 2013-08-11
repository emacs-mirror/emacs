/* Interface to zlib.
   Copyright (C) 2013 Free Software Foundation, Inc.

This file is part of GNU Emacs.

GNU Emacs is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.  */

#include <config.h>

#ifdef HAVE_ZLIB

#include <zlib.h>

#include "lisp.h"
#include "character.h"
#include "buffer.h"


#define BUFFER_SIZE 16384

struct decompress_unwind_data {
  ptrdiff_t old_point, start;
  z_stream *stream;
};

static void
unwind_decompress (void *ddata) {
  struct decompress_unwind_data *data = ddata;
  inflateEnd (data->stream);
  /* Delete any uncompressed data already inserted and restore
     point. */
  if (data->start) {
    del_range (data->start, PT);
    SET_PT (data->old_point);
  }
}

DEFUN ("decompress-gzipped-region", Fdecompress_gzipped_region,
       Sdecompress_gzipped_region,
       2, 2, 0,
       doc: /* Decompress a gzip-compressed region.
The text in the region will be replaced by the decompressed data.
On failure, nil is returned and the data is left in place.
This function can only be called in unibyte buffers.*/)
  (Lisp_Object start, Lisp_Object end)
{
  ptrdiff_t istart, iend, point = PT;
  z_stream stream;
  int decompressed;
  char out[16384];
  struct decompress_unwind_data unwind_data;
  ptrdiff_t count = SPECPDL_INDEX ();

  validate_region (&start, &end);

  if (! NILP (BVAR (current_buffer, enable_multibyte_characters)))
    error ("This function can only be called in unibyte buffers");

  /* This is a unibyte buffer, so character positions and bytes are
     the same. */
  istart = XINT (start);
  iend = XINT (end);
  move_gap_both (iend, iend);

  stream.zalloc = Z_NULL;
  stream.zfree = Z_NULL;
  stream.opaque = Z_NULL;
  stream.avail_in = 0;
  stream.next_in = Z_NULL;

  /* This magic number apparently means "this is gzip". */
  if (inflateInit2 (&stream, 16 + MAX_WBITS) != Z_OK)
    return Qnil;

  /* We're inserting the decompressed data at the end of the
     compressed data. */
  SET_PT (iend);

  stream.avail_in = iend - istart;
  stream.next_in = (char *) BYTE_POS_ADDR (istart);

  unwind_data.start = iend;
  unwind_data.stream = &stream;
  unwind_data.old_point = point;
  record_unwind_protect_ptr (unwind_decompress, &unwind_data);

  immediate_quit = 1;

  /* Run inflate() on input until the output buffer isn't full. */
  do {
    int result;
    stream.avail_out = BUFFER_SIZE;
    stream.next_out = out;
    result = inflate (&stream, Z_NO_FLUSH);
    if (result < 0) {
      unbind_to (count, Qnil);
      return Qnil;
    }

    decompressed = BUFFER_SIZE - stream.avail_out;
    insert_1_both (out, decompressed, decompressed, 0, 0, 0);
    QUIT;
  } while (stream.avail_out == 0);

  immediate_quit = 0;

  unwind_data.start = 0;
  unbind_to (count, Qnil);

  /* Delete the compressed data. */
  del_range (istart, iend);

  return Qt;
}


/***********************************************************************
			    Initialization
 ***********************************************************************/
void
syms_of_decompress (void)
{
  defsubr (&Sdecompress_gzipped_region);
}

#endif /* HAVE_ZLIB */
