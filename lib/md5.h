/* Declaration of functions and data types used for MD5 sum computing
   library functions.
   Copyright (C) 1995-1997, 1999-2001, 2004-2006, 2008-2011 Free Software
   Foundation, Inc.
   This file is part of the GNU C Library.

   This program is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by the
   Free Software Foundation; either version 3, or (at your option) any
   later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software Foundation,
   Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.  */

#ifndef _MD5_H
#define _MD5_H 1

#include <stdio.h>

#define MD5_DIGEST_SIZE 16
#define MD5_BLOCK_SIZE 64

#ifndef __GNUC_PREREQ
# if defined __GNUC__ && defined __GNUC_MINOR__
#  define __GNUC_PREREQ(maj, min)                                       \
  ((__GNUC__ << 16) + __GNUC_MINOR__ >= ((maj) << 16) + (min))
# else
#  define __GNUC_PREREQ(maj, min) 0
# endif
#endif

#ifndef __THROW
# if defined __cplusplus && __GNUC_PREREQ (2,8)
#  define __THROW       throw ()
# else
#  define __THROW
# endif
#endif

#ifndef _LIBC
# define __md5_buffer md5_buffer
# define __md5_finish_ctx md5_finish_ctx
# define __md5_init_ctx md5_init_ctx
# define __md5_process_block md5_process_block
# define __md5_process_bytes md5_process_bytes
# define __md5_read_ctx md5_read_ctx
# define __md5_stream md5_stream
#endif

# ifdef __cplusplus
extern "C" {
# endif

/* The following contortions are an attempt to use the C preprocessor
   to determine an unsigned integral type that is 32 bits wide.  An
   alternative approach is to use autoconf's AC_CHECK_SIZEOF macro, but
   doing that would require that the configure script compile and *run*
   the resulting executable.  Locally running cross-compiled executables
   is usually not possible.  */

#if defined _LIBC
# include <stdint.h>
typedef uint32_t md5_uint32;
#else
# if defined __STDC__ && __STDC__
#  define UINT_MAX_32_BITS 4294967295U
# else
#  define UINT_MAX_32_BITS 0xFFFFFFFF
# endif

# include <limits.h>

# if UINT_MAX == UINT_MAX_32_BITS
   typedef unsigned int md5_uint32;
# else
#  if USHRT_MAX == UINT_MAX_32_BITS
    typedef unsigned short md5_uint32;
#  else
#   if ULONG_MAX == UINT_MAX_32_BITS
     typedef unsigned long md5_uint32;
#   else
     /* A machine this weird should have <stdint.h>.  */
#    include <stdint.h>
     typedef uint32_t md5_uint32;
#   endif
#  endif
# endif
#endif

/* Structure to save state of computation between the single steps.  */
struct md5_ctx
{
  md5_uint32 A;
  md5_uint32 B;
  md5_uint32 C;
  md5_uint32 D;

  md5_uint32 total[2];
  md5_uint32 buflen;
  md5_uint32 buffer[32];
};

/*
 * The following three functions are build up the low level used in
 * the functions `md5_stream' and `md5_buffer'.
 */

/* Initialize structure containing state of computation.
   (RFC 1321, 3.3: Step 3)  */
extern void __md5_init_ctx (struct md5_ctx *ctx) __THROW;

/* Starting with the result of former calls of this function (or the
   initialization function update the context for the next LEN bytes
   starting at BUFFER.
   It is necessary that LEN is a multiple of 64!!! */
extern void __md5_process_block (const void *buffer, size_t len,
                                 struct md5_ctx *ctx) __THROW;

/* Starting with the result of former calls of this function (or the
   initialization function update the context for the next LEN bytes
   starting at BUFFER.
   It is NOT required that LEN is a multiple of 64.  */
extern void __md5_process_bytes (const void *buffer, size_t len,
                                 struct md5_ctx *ctx) __THROW;

/* Process the remaining bytes in the buffer and put result from CTX
   in first 16 bytes following RESBUF.  The result is always in little
   endian byte order, so that a byte-wise output yields to the wanted
   ASCII representation of the message digest.  */
extern void *__md5_finish_ctx (struct md5_ctx *ctx, void *resbuf) __THROW;


/* Put result from CTX in first 16 bytes following RESBUF.  The result is
   always in little endian byte order, so that a byte-wise output yields
   to the wanted ASCII representation of the message digest.  */
extern void *__md5_read_ctx (const struct md5_ctx *ctx, void *resbuf) __THROW;


/* Compute MD5 message digest for bytes read from STREAM.  The
   resulting message digest number will be written into the 16 bytes
   beginning at RESBLOCK.  */
extern int __md5_stream (FILE *stream, void *resblock) __THROW;

/* Compute MD5 message digest for LEN bytes beginning at BUFFER.  The
   result is always in little endian byte order, so that a byte-wise
   output yields to the wanted ASCII representation of the message
   digest.  */
extern void *__md5_buffer (const char *buffer, size_t len,
                           void *resblock) __THROW;

# ifdef __cplusplus
}
# endif

#endif /* md5.h */
