/* sha3.h - Functions to calculate SHA-3 hashes as specified by FIPS-202.
   Copyright (C) 2025-2026 Free Software Foundation, Inc.

   This file is free software: you can redistribute it and/or modify
   it under the terms of the GNU Lesser General Public License as
   published by the Free Software Foundation; either version 2.1 of the
   License, or (at your option) any later version.

   This file is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public License
   along with this program.  If not, see <https://www.gnu.org/licenses/>.  */

/* Written by Collin Funk <collin.funk1@gmail.com>, 2025.  */

#ifndef SHA3_H
# define SHA3_H 1

# include <stddef.h>
# include <stdio.h>
# include <stdint.h>

# include "u64.h"

# ifdef __cplusplus
extern "C" {
# endif

/* OpenSSL does not have the Init, Update, Final API for SHA-3.  We must use
   the EVP API.  */
# if HAVE_OPENSSL_SHA3
#  include <openssl/evp.h>
# endif

/* Digest sizes in bytes.  */
enum { SHA3_224_DIGEST_SIZE = 224 / 8 };
enum { SHA3_256_DIGEST_SIZE = 256 / 8 };
enum { SHA3_384_DIGEST_SIZE = 384 / 8 };
enum { SHA3_512_DIGEST_SIZE = 512 / 8 };

/* Block sizes in bytes.  */
enum { SHA3_224_BLOCK_SIZE = 1152 / 8 };
enum { SHA3_256_BLOCK_SIZE = 1088 / 8 };
enum { SHA3_384_BLOCK_SIZE = 832 / 8 };
enum { SHA3_512_BLOCK_SIZE = 576 / 8 };

/* Structure to save state of computation between the single steps.  */
struct sha3_ctx
{
# if HAVE_OPENSSL_SHA3
  /* EVP_MD_CTX is an incomplete type.  It cannot be placed on the stack.  */
  EVP_MD_CTX *evp_ctx;
# else
  u64 state[25];
  uint8_t buffer[144]; /* Up to BLOCKLEN in use.  */
  size_t buflen;       /* ≥ 0, ≤ BLOCKLEN  */
  size_t digestlen;    /* One of SHA3_{224,256,384,512}_DIGEST_SIZE.  */
  size_t blocklen;     /* One of SHA3_{224,256,384,512}_BLOCK_SIZE.  */
# endif
};

/* Initialize structure containing state of computation.  */
extern bool sha3_224_init_ctx (struct sha3_ctx *ctx);
extern bool sha3_256_init_ctx (struct sha3_ctx *ctx);
extern bool sha3_384_init_ctx (struct sha3_ctx *ctx);
extern bool sha3_512_init_ctx (struct sha3_ctx *ctx);

/* Free memory allocated by the init_structure.  */
extern void sha3_free_ctx (struct sha3_ctx *ctx);

/* Starting with the result of former calls of this function (or the
   initialization function update the context for the next LEN bytes
   starting at BUFFER.
   It is necessary that LEN is a multiple of the BLOCKLEN member of CTX!!!
   Return false if an OpenSSL function fails.  */
extern bool sha3_process_block (void const *restrict buffer, size_t len,
                                struct sha3_ctx *restrict ctx);

/* Starting with the result of former calls of this function (or the
   initialization function update the context for the next LEN bytes
   starting at BUFFER.
   It is NOT required that LEN is a multiple of the BLOCKLEN member of CTX.
   Return false if an OpenSSL function fails.  */
extern bool sha3_process_bytes (void const *restrict buffer, size_t len,
                                struct sha3_ctx *restrict ctx);

/* Process the remaining bytes in the buffer and put result from CTX in RESBUF.
   The result is always in little endian byte order, so that a byte-wise output
   yields to the wanted ASCII representation of the message digest.
   Return NULL if an OpenSSL function fails.  */
extern void *sha3_finish_ctx (struct sha3_ctx *restrict ctx,
                              void *restrict resbuf);

/* Put result from CTX in RESBUF.  The result is always in little endian byte
   order, so that a byte-wise output yields to the wanted ASCII representation
   of the message digest.
   Return NULL if an OpenSSL function fails.  */
extern void *sha3_read_ctx (struct sha3_ctx const *restrict ctx,
                            void *restrict resbuf);

/* Compute a SHA-3 message digest for LEN bytes beginning at BUFFER.
   The result is always in little endian byte order, so that a byte-wise
   output yields to the wanted ASCII representation of the message
   digest.
   Return NULL if an OpenSSL function fails.  */
extern void *sha3_224_buffer (char const *restrict buffer, size_t len,
                              void *restrict resblock);
extern void *sha3_256_buffer (char const *restrict buffer, size_t len,
                              void *restrict resblock);
extern void *sha3_384_buffer (char const *restrict buffer, size_t len,
                              void *restrict resblock);
extern void *sha3_512_buffer (char const *restrict buffer, size_t len,
                              void *restrict resblock);

/* Compute SHA-3 message digest for bytes read from STREAM.  STREAM is an open
   file stream.  Regular files are handled more efficiently.  The contents of
   STREAM from its current position to its end will be read.  The case that the
   last operation on STREAM was an 'ungetc' is not supported.  The resulting
   message digest number will be written into RESBLOCK.  */
extern int sha3_224_stream (FILE *restrict stream, void *restrict resblock);
extern int sha3_256_stream (FILE *restrict stream, void *restrict resblock);
extern int sha3_384_stream (FILE *restrict stream, void *restrict resblock);
extern int sha3_512_stream (FILE *restrict stream, void *restrict resblock);

# ifdef __cplusplus
}
# endif

#endif
