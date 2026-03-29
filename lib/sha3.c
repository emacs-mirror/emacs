/* sha3.c - Functions to calculate SHA-3 hashes as specified by FIPS-202.
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

#include <config.h>

/* Specification.  */
#include "sha3.h"

#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#include <byteswap.h>
#ifdef WORDS_BIGENDIAN
# define SWAP(n) u64bswap (n)
#else
# define SWAP(n) (n)
#endif

#if ! HAVE_OPENSSL_SHA3

static const u64 rc[] = {
  u64init (0x00000000, 0x00000001), u64init (0x00000000, 0x00008082),
  u64init (0x80000000, 0x0000808A), u64init (0x80000000, 0x80008000),
  u64init (0x00000000, 0x0000808B), u64init (0x00000000, 0x80000001),
  u64init (0x80000000, 0x80008081), u64init (0x80000000, 0x00008009),
  u64init (0x00000000, 0x0000008A), u64init (0x00000000, 0x00000088),
  u64init (0x00000000, 0x80008009), u64init (0x00000000, 0x8000000A),
  u64init (0x00000000, 0x8000808B), u64init (0x80000000, 0x0000008B),
  u64init (0x80000000, 0x00008089), u64init (0x80000000, 0x00008003),
  u64init (0x80000000, 0x00008002), u64init (0x80000000, 0x00000080),
  u64init (0x00000000, 0x0000800A), u64init (0x80000000, 0x8000000A),
  u64init (0x80000000, 0x80008081), u64init (0x80000000, 0x00008080),
  u64init (0x00000000, 0x80000001), u64init (0x80000000, 0x80008008)
};

#define DEFINE_SHA3_INIT_CTX(SIZE)                                      \
  bool                                                                  \
  sha3_##SIZE##_init_ctx (struct sha3_ctx *ctx)                         \
  {                                                                     \
    memset (&ctx->state, '\0', sizeof ctx->state);                      \
    ctx->buflen = 0;                                                    \
    ctx->digestlen = SHA3_##SIZE##_DIGEST_SIZE;                         \
    ctx->blocklen = SHA3_##SIZE##_BLOCK_SIZE;                           \
    return true;                                                        \
  }

DEFINE_SHA3_INIT_CTX (224)
DEFINE_SHA3_INIT_CTX (256)
DEFINE_SHA3_INIT_CTX (384)
DEFINE_SHA3_INIT_CTX (512)

void
sha3_free_ctx (_GL_UNUSED struct sha3_ctx *ctx)
{
  /* Do nothing.  */
}

/* Copy the value from V into the memory location pointed to by *CP,
   If your architecture allows unaligned access, this is equivalent to
   * (__typeof__ (v) *) cp = v  */
static void
set_uint64 (char *cp, u64 v)
{
  memcpy (cp, &v, sizeof v);
}

void *
sha3_read_ctx (struct sha3_ctx const *restrict ctx, void *restrict resbuf)
{
  char *r = resbuf;
  size_t words = ctx->digestlen / sizeof *ctx->state;
  size_t bytes = ctx->digestlen % sizeof *ctx->state;

  int i;
  for (i = 0; i < words; ++i, r += sizeof *ctx->state)
    set_uint64 (r, SWAP (ctx->state[i]));
  if (bytes)
    {
      u64 word = ctx->state[i];
      do
        {
          *r++ = u64getlo (word) & 0xFF;
          word = u64shr (word, 8);
        }
      while (--bytes);
    }
  return resbuf;
}

static void
sha3_conclude_ctx (struct sha3_ctx *ctx)
{
  ctx->buffer[ctx->buflen++] = 0x06;
  memset (ctx->buffer + ctx->buflen, '\0', ctx->blocklen - ctx->buflen);
  ctx->buffer[ctx->blocklen - 1] |= 0x80;
  sha3_process_block (ctx->buffer, ctx->blocklen, ctx);
}

void *
sha3_finish_ctx (struct sha3_ctx *restrict ctx, void *restrict resbuf)
{
  sha3_conclude_ctx (ctx);
  return sha3_read_ctx (ctx, resbuf);
}

#define DEFINE_SHA3_BUFFER(SIZE)                                        \
  void *                                                                \
  sha3_##SIZE##_buffer (char const *restrict buffer, size_t len,        \
                        void *restrict resblock)                        \
  {                                                                     \
    struct sha3_ctx ctx;                                                \
    sha3_##SIZE##_init_ctx (&ctx);                                      \
    sha3_process_bytes (buffer, len, &ctx);                             \
    return sha3_finish_ctx (&ctx, resblock);                            \
  }

DEFINE_SHA3_BUFFER (224)
DEFINE_SHA3_BUFFER (256)
DEFINE_SHA3_BUFFER (384)
DEFINE_SHA3_BUFFER (512)

bool
sha3_process_bytes (void const *restrict buffer, size_t len,
                    struct sha3_ctx *restrict ctx)
{
  char const *buf = buffer;

  if (0 < ctx->buflen)
    {
      size_t left = ctx->blocklen - ctx->buflen;
      if (len < left)
        {
          /* Not enough to fill a full block.  */
          memcpy (ctx->buffer + ctx->buflen, buf, len);
          ctx->buflen += len;
          return true;
        }
      /* Process the block that already had bytes buffered.  */
      memcpy (ctx->buffer + ctx->buflen, buf, left);
      buf += left;
      len -= left;
      sha3_process_block (ctx->buffer, ctx->blocklen, ctx);
    }

  /* Process as many complete blocks as possible.  */
  size_t full_len = len - len % ctx->blocklen;
  sha3_process_block (buf, full_len, ctx);
  buf += full_len;
  len -= full_len;

  memcpy (ctx->buffer, buf, len);
  ctx->buflen = len;
  return true;
}

bool
sha3_process_block (void const *restrict buffer, size_t len,
                    struct sha3_ctx *restrict ctx)
{
  u64 *a = ctx->state;
  const u64 *words = buffer;
  size_t nwords = len / sizeof *words;
  const u64 *endp = words + nwords;

  while (words < endp)
    {
      for (size_t i = 0; i < ctx->blocklen / sizeof *ctx->state; ++i, ++words)
        ctx->state[i] = u64xor (ctx->state[i], SWAP (*words));
      for (int i = 0; i < 24; ++i)
        {
          u64 c[5];
          u64 d[5];
          u64 t1;
          u64 t2;

          /* Theta step 1.  */
          c[0] = u64xor (u64xor (u64xor (u64xor (a[0], a[5]), a[10]),
                                 a[15]), a[20]);
          c[1] = u64xor (u64xor (u64xor (u64xor (a[1], a[6]), a[11]),
                                 a[16]), a[21]);
          c[2] = u64xor (u64xor (u64xor (u64xor (a[2], a[7]), a[12]),
                                 a[17]), a[22]);
          c[3] = u64xor (u64xor (u64xor (u64xor (a[3], a[8]), a[13]),
                                 a[18]), a[23]);
          c[4] = u64xor (u64xor (u64xor (u64xor (a[4], a[9]), a[14]),
                                 a[19]), a[24]);

          /* Theta step 2.  */
          d[0] = u64xor (c[4], u64rol (c[1], 1));
          d[1] = u64xor (c[0], u64rol (c[2], 1));
          d[2] = u64xor (c[1], u64rol (c[3], 1));
          d[3] = u64xor (c[2], u64rol (c[4], 1));
          d[4] = u64xor (c[3], u64rol (c[0], 1));

          /* Theta step 3.  */
          a[0] = u64xor (a[0], d[0]);
          a[5] = u64xor (a[5], d[0]);
          a[10] = u64xor (a[10], d[0]);
          a[15] = u64xor (a[15], d[0]);
          a[20] = u64xor (a[20], d[0]);
          a[1] = u64xor (a[1], d[1]);
          a[6] = u64xor (a[6], d[1]);
          a[11] = u64xor (a[11], d[1]);
          a[16] = u64xor (a[16], d[1]);
          a[21] = u64xor (a[21], d[1]);
          a[2] = u64xor (a[2], d[2]);
          a[7] = u64xor (a[7], d[2]);
          a[12] = u64xor (a[12], d[2]);
          a[17] = u64xor (a[17], d[2]);
          a[22] = u64xor (a[22], d[2]);
          a[3] = u64xor (a[3], d[3]);
          a[8] = u64xor (a[8], d[3]);
          a[13] = u64xor (a[13], d[3]);
          a[18] = u64xor (a[18], d[3]);
          a[23] = u64xor (a[23], d[3]);
          a[4] = u64xor (a[4], d[4]);
          a[9] = u64xor (a[9], d[4]);
          a[14] = u64xor (a[14], d[4]);
          a[19] = u64xor (a[19], d[4]);
          a[24] = u64xor (a[24], d[4]);

          /* Rho and Pi.  */
          t1 = a[1];
          t2 = u64rol (t1, 1);
          t1 = a[10];
          a[10] = t2;
          t2 = u64rol (t1, 3);
          t1 = a[7];
          a[7] = t2;
          t2 = u64rol (t1, 6);
          t1 = a[11];
          a[11] = t2;
          t2 = u64rol (t1, 10);
          t1 = a[17];
          a[17] = t2;
          t2 = u64rol (t1, 15);
          t1 = a[18];
          a[18] = t2;
          t2 = u64rol (t1, 21);
          t1 = a[3];
          a[3] = t2;
          t2 = u64rol (t1, 28);
          t1 = a[5];
          a[5] = t2;
          t2 = u64rol (t1, 36);
          t1 = a[16];
          a[16] = t2;
          t2 = u64rol (t1, 45);
          t1 = a[8];
          a[8] = t2;
          t2 = u64rol (t1, 55);
          t1 = a[21];
          a[21] = t2;
          t2 = u64rol (t1, 2);
          t1 = a[24];
          a[24] = t2;
          t2 = u64rol (t1, 14);
          t1 = a[4];
          a[4] = t2;
          t2 = u64rol (t1, 27);
          t1 = a[15];
          a[15] = t2;
          t2 = u64rol (t1, 41);
          t1 = a[23];
          a[23] = t2;
          t2 = u64rol (t1, 56);
          t1 = a[19];
          a[19] = t2;
          t2 = u64rol (t1, 8);
          t1 = a[13];
          a[13] = t2;
          t2 = u64rol (t1, 25);
          t1 = a[12];
          a[12] = t2;
          t2 = u64rol (t1, 43);
          t1 = a[2];
          a[2] = t2;
          t2 = u64rol (t1, 62);
          t1 = a[20];
          a[20] = t2;
          t2 = u64rol (t1, 18);
          t1 = a[14];
          a[14] = t2;
          t2 = u64rol (t1, 39);
          t1 = a[22];
          a[22] = t2;
          t2 = u64rol (t1, 61);
          t1 = a[9];
          a[9] = t2;
          t2 = u64rol (t1, 20);
          t1 = a[6];
          a[6] = t2;
          t2 = u64rol (t1, 44);
          t1 = a[1];
          a[1] = t2;

          /* Chi.  */
          for (int j = 0; j < 25; j += 5)
            {
              t1 = a[j];
              t2 = a[j + 1];
              a[j] = u64xor (a[j], u64and (u64not (a[j + 1]), a[j + 2]));
              a[j + 1] = u64xor (a[j + 1], u64and (u64not (a[j + 2]),
                                                   a[j + 3]));
              a[j + 2] = u64xor (a[j + 2], u64and (u64not (a[j + 3]),
                                                   a[j + 4]));
              a[j + 3] = u64xor (a[j + 3], u64and (u64not (a[j + 4]), t1));
              a[j + 4] = u64xor (a[j + 4], u64and (u64not (t1), t2));
            }

          /* Iota.  */
          a[0] = u64xor (a[0], rc[i]);
        }
    }
  return true;
}

#else /* OpenSSL implementation.  */

/* We avoid using all of EVP error strings.  Just guess a reasonable errno.  */
#include <errno.h>

#define DEFINE_SHA3_INIT_CTX(SIZE)                                      \
  bool                                                                  \
  sha3_##SIZE##_init_ctx (struct sha3_ctx *ctx)                         \
  {                                                                     \
    EVP_MD_CTX *evp_ctx = EVP_MD_CTX_new ();                            \
    if (evp_ctx && ! EVP_DigestInit_ex (evp_ctx, EVP_sha3_##SIZE (), NULL)) \
      {                                                                 \
        EVP_MD_CTX_free (evp_ctx);                                      \
        evp_ctx = NULL;                                                 \
      }                                                                 \
    ctx->evp_ctx = evp_ctx;                                             \
    errno = ENOMEM;  /* OK to set errno even if successful.  */         \
    return !!evp_ctx;                                                   \
  }

DEFINE_SHA3_INIT_CTX (224)
DEFINE_SHA3_INIT_CTX (256)
DEFINE_SHA3_INIT_CTX (384)
DEFINE_SHA3_INIT_CTX (512)

void
sha3_free_ctx (struct sha3_ctx *ctx)
{
  if (ctx->evp_ctx != NULL)
    {
      int saved_errno = errno;
      EVP_MD_CTX_free (ctx->evp_ctx);
      ctx->evp_ctx = NULL;
      errno = saved_errno;
    }
}

void *
sha3_read_ctx (struct sha3_ctx const *restrict ctx, void *restrict resbuf)
{
  void *result = NULL;
  int err = ENOMEM;
  EVP_MD_CTX *evp_ctx = EVP_MD_CTX_new ();
  if (evp_ctx)
    {
      if (EVP_MD_CTX_copy_ex (evp_ctx, ctx->evp_ctx))
        {
          if (EVP_DigestFinal_ex (evp_ctx, resbuf, NULL))
            result = resbuf;
          err = EINVAL;
        }
      EVP_MD_CTX_free (evp_ctx);
    }
  errno = err; /* OK to set errno even if successful.  */
  return result;
}

void *
sha3_finish_ctx (struct sha3_ctx *restrict ctx, void *restrict resbuf)
{
  int result = EVP_DigestFinal_ex (ctx->evp_ctx, resbuf, NULL);
  if (result == 0)
    {
      errno = EINVAL;
      return NULL;
    }
  return resbuf;
}

#define DEFINE_SHA3_BUFFER(SIZE)                                        \
  void *                                                                \
  sha3_##SIZE##_buffer (char const *restrict buffer, size_t len,        \
                        void *restrict resblock)                        \
  {                                                                     \
    struct sha3_ctx ctx;                                                \
    void *result = ((sha3_##SIZE##_init_ctx (&ctx)                      \
                     && sha3_process_bytes (buffer, len, &ctx))         \
                    ? sha3_finish_ctx (&ctx, resblock)                  \
                    : NULL);                                            \
    sha3_free_ctx (&ctx);                                               \
    return result;                                                      \
  }

DEFINE_SHA3_BUFFER (224)
DEFINE_SHA3_BUFFER (256)
DEFINE_SHA3_BUFFER (384)
DEFINE_SHA3_BUFFER (512)

bool
sha3_process_bytes (void const *restrict buffer, size_t len,
                    struct sha3_ctx *restrict ctx)
{
  int result = EVP_DigestUpdate (ctx->evp_ctx, buffer, len);
  if (result == 0)
    {
      errno = EINVAL;
      return false;
    }
  return true;
}

bool
sha3_process_block (void const *restrict buffer, size_t len,
                    struct sha3_ctx *restrict ctx)
{
  return sha3_process_bytes (buffer, len, ctx);
}

#endif
