/* mpslibcb.c: RAVENBROOK MEMORY POOL SYSTEM LIBRARY INTERFACE (CALLBACK)
 *
 * $Header$
 * Copyright (c) 2005 Ravenbrook Limited.  See end of file for license.
 *
 * .purpose: The purpose of this code is
 *   1. permit the MPS Library Interface to be used conveniently when
 *      the MPS is packaged as a dynamic library (and in particular a
 *      Windows DLL).
 *
 * .readership: For MPS client application developers and MPS developers.
 * .sources: <design/lib/>
 *
 * .freestanding: This is designed to be deployed in a freestanding
 * environment, so we can't use strcmp from <string.h>, so we have to
 * roll our own (in fact we only ever need equality so we define a
 * simpler interface).
 *
 * .mpm.not: This module occupies a halfway house between the MPM and
 * the client.  Let's make it clearer: this module should not use any
 * services of the MPM.  That is, it should be written as if the client
 * could have, in principle, written it.  .mpm.not.why: Perhaps the most
 * compelling reason is that if config.h is included (via mpm.h) then
 * the compile breaks on platform.w3i3mv because of "#define
 * mps_lib_memset memset" in config.h.
 */

#include "mpslibcb.h"
#include "mpslib.h"
#include "mps.h"

/* Forward declarations. */

int mps_lib_callback_default_get_EOF(void);
mps_lib_FILE *mps_lib_callback_default_get_stderr(void);
mps_lib_FILE *mps_lib_callback_default_get_stdout(void);
int mps_lib_callback_default_fputc(int c_, mps_lib_FILE *f_);
int mps_lib_callback_default_fputs(const char *s_, mps_lib_FILE *f_);
void *mps_lib_callback_default_memset(void *p_, int c_, size_t n_);
void *mps_lib_callback_default_memcpy(void *p_, const void *q_, size_t n_);
int mps_lib_callback_default_memcmp(const void *p_, const void *q_, size_t n_);
mps_clock_t mps_lib_callback_default_clock(void);
mps_clock_t mps_lib_callback_default_clocks_per_sec(void);
unsigned long mps_lib_callback_default_telemetry_control(void);
int mps_lib_callback_streq(const char *, const char *);

/* Macros */

/* See .freestanding */
#define EQ(p, q) (mps_lib_callback_streq((p), (q)))
/* We use this to call mps_lib_asssert_fail (which we only ever do
 * unconditionally).  See .mpm.not on why we cannot use ASSERT from
 * mpm.h */
#define AFAIL mps_lib_assert_fail
/* Replaced UNUSED from mpm.h, see .mpm.not */
#define UNUSED(x) ((void)(x))

/* Structures and Types */

struct mps_lib_callback_s
{
  int (*lib_get_EOF)(void);
  mps_lib_FILE * (*lib_get_stderr)(void);
  mps_lib_FILE * (*lib_get_stdout)(void);
  int (*lib_fputc)(int, mps_lib_FILE *);
  int (*lib_fputs)(const char *, mps_lib_FILE *);
  void (*lib_assert_fail)(const char *);
  void * (*lib_memset)(void *, int, size_t);
  void * (*lib_memcpy)(void *, const void *, size_t);
  int (*lib_memcmp)(const void *, const void *, size_t);
  mps_clock_t (*clock)(void);
  mps_clock_t (*clocks_per_sec)(void);
  unsigned long (*lib_telemetry_control)(void);
};

/* Globals */

/* .global.why: A global is necessary so that we can store the function
 * pointers that the client gives us.  The functions in the mpslib.h
 * interface _are_ global.  There is no scope for having one memset
 * function for one Arena and a different memset function for another.
 * */

/* The default functions are stubs that assert.  Except for the
 * assert_fail function (which is called when assertions fail) which
 * will be NULL.  This means: if you provide assert_fail and forget
 * something else, you'll know about it.  If you do not provide
 * assert_fail then it will probably stop anyway.
 *
 * These functions really do need to fail, so they subvert the checking
 * mechanism (which is in mpm.h and not available to us, see .mpm.not)
 */
  
struct mps_lib_callback_s mps_lib_callback_global = {
  mps_lib_callback_default_get_EOF,
  mps_lib_callback_default_get_stderr,
  mps_lib_callback_default_get_stdout,
  mps_lib_callback_default_fputc,
  mps_lib_callback_default_fputs,
  NULL, /* assert_fail */
  mps_lib_callback_default_memset,
  mps_lib_callback_default_memcpy,
  mps_lib_callback_default_memcmp,
  mps_lib_callback_default_clock,
  mps_lib_callback_default_clocks_per_sec,
  mps_lib_callback_default_telemetry_control
};

/* Functions */

int mps_lib_callback_register(const char *name, mps_lib_function_t f)
{
  if(NULL == name) {
    return MPS_RES_FAIL;
  }
  if(0) {
    /* just to make the "else if" neater. */
  } else if(EQ(name, "mps_lib_get_EOF")) {
    mps_lib_callback_global.lib_get_EOF = (int(*)(void))f;
  } else if(EQ(name, "mps_lib_get_stderr")) {
    mps_lib_callback_global.lib_get_stderr = (mps_lib_FILE *(*)(void))f;
  } else if(EQ(name, "mps_lib_get_stdout")) {
    mps_lib_callback_global.lib_get_stdout = (mps_lib_FILE *(*)(void))f;
  } else if(EQ(name, "mps_lib_fputc")) {
    mps_lib_callback_global.lib_fputc = (int(*)(int, mps_lib_FILE *))f;
  } else if(EQ(name, "mps_lib_fputs")) {
    mps_lib_callback_global.lib_fputs =
      (int(*)(const char *, mps_lib_FILE *))f;
  } else if(EQ(name, "mps_lib_assert_fail")) {
    mps_lib_callback_global.lib_assert_fail = (void(*)(const char *))f;
  } else if(EQ(name, "mps_lib_memset")) {
    mps_lib_callback_global.lib_memset = (void *(*)(void *, int, size_t))f;
  } else if(EQ(name, "mps_lib_memcpy")) {
    mps_lib_callback_global.lib_memcpy =
      (void *(*)(void *, const void *, size_t))f;
  } else if(EQ(name, "mps_lib_memcmp")) {
    mps_lib_callback_global.lib_memcmp =
      (int(*)(const void *, const void *, size_t))f;
  } else if(EQ(name, "mps_clock")) {
    mps_lib_callback_global.clock = (mps_clock_t(*)(void))f;
  } else if(EQ(name, "mps_clocks_per_sec")) {
    mps_lib_callback_global.clocks_per_sec = (mps_clock_t(*)(void))f;
  } else if(EQ(name, "mps_lib_telemetry_control")) {
    mps_lib_callback_global.lib_telemetry_control =
      (unsigned long(*)(void))f;
  } else {
    return MPS_RES_UNIMPL;
  }
  return MPS_RES_OK;
}

/* Return non-zero if and only if string p equals string q. */
int mps_lib_callback_streq(const char *p, const char *q)
{
  do {
    if(*p == '\0' && *q == '\0') {
      return 1;
    }
  } while(*p++ == *q++);
  return 0;
}

int mps_lib_callback_default_get_EOF(void)
{
  AFAIL("mps_lib_get_EOF needs to be provided");
  return 0;
}

mps_lib_FILE *mps_lib_callback_default_get_stderr(void)
{
  AFAIL("mps_lib_get_stderr needs to be provided");
  return NULL;
}

mps_lib_FILE *mps_lib_callback_default_get_stdout(void)
{
  AFAIL("mps_lib_get_stdout needs to be provided");
  return NULL;
}

int mps_lib_callback_default_fputc(int c_, mps_lib_FILE *f_)
{
  UNUSED(c_);
  UNUSED(f_);
  AFAIL("mps_lib_fputc needs to be provided");
  return 0;
}

int mps_lib_callback_default_fputs(const char *s_, mps_lib_FILE *f_)
{
  UNUSED(s_);
  UNUSED(f_);
  AFAIL("mps_lib_fputs needs to be provided");
  return 0;
}

/* No default implementation for mps_lib_assert_fail */

void *mps_lib_callback_default_memset(void *p_, int c_, size_t n_)
{
  UNUSED(p_);
  UNUSED(c_);
  UNUSED(n_);
  AFAIL("mps_lib_memset needs to be provided");
  return NULL;
}

void *mps_lib_callback_default_memcpy(void *p_, const void *q_, size_t n_)
{
  UNUSED(p_);
  UNUSED(q_);
  UNUSED(n_);
  AFAIL("mps_lib_memcpy needs to be provided");
  return NULL;
}

int mps_lib_callback_default_memcmp(const void *p_, const void *q_, size_t n_)
{
  UNUSED(p_);
  UNUSED(q_);
  UNUSED(n_);
  AFAIL("mps_lib_memcmp needs to be provided");
  return 0;
}

mps_clock_t mps_lib_callback_default_clock(void)
{
  AFAIL("mps_clock needs to be provided");
  return 0;
}

mps_clock_t mps_lib_callback_default_clocks_per_sec(void)
{
  AFAIL("mps_clocks_per_sec needs to be provided");
  return 0;
}

unsigned long mps_lib_callback_default_telemetry_control(void)
{
  AFAIL("mps_lib_telemetry_control needs to be provided");
  return 0;
}

int mps_lib_get_EOF(void)
{
  return mps_lib_callback_global.lib_get_EOF();
}

mps_lib_FILE *mps_lib_get_stderr(void)
{
  return mps_lib_callback_global.lib_get_stderr();
}

mps_lib_FILE *mps_lib_get_stdout(void)
{
  return mps_lib_callback_global.lib_get_stdout();
}

int mps_lib_fputc(int c, mps_lib_FILE *f)
{
  return mps_lib_callback_global.lib_fputc(c, f);
}

int mps_lib_fputs(const char *s, mps_lib_FILE *f)
{
  return mps_lib_callback_global.lib_fputs(s, f);
}

void mps_lib_assert_fail(const char *m)
{
  mps_lib_callback_global.lib_assert_fail(m);
}

void *(mps_lib_memset)(void *p, int c, size_t n)
{
  return mps_lib_callback_global.lib_memset(p, c, n);
}

void *(mps_lib_memcpy)(void *p, const void *q, size_t n)
{
  return mps_lib_callback_global.lib_memcpy(p, q, n);
}

int (mps_lib_memcmp)(const void *p, const void *q, size_t n)
{
  return mps_lib_callback_global.lib_memcmp(p, q, n);
}

mps_clock_t mps_clock(void)
{
  return mps_lib_callback_global.clock();
}

mps_clock_t mps_clocks_per_sec(void)
{
  return mps_lib_callback_global.clocks_per_sec();
}

unsigned long mps_lib_telemetry_control(void)
{
  return mps_lib_callback_global.lib_telemetry_control();
}



/* C. COPYRIGHT AND LICENSE
 *
 * Copyright (C) 2005 Ravenbrook Limited <http://www.ravenbrook.com/>.
 * All rights reserved.  This is an open source license.  Contact
 * Ravenbrook for commercial licensing options.
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 * 
 * 1. Redistributions of source code must retain the above copyright
 * notice, this list of conditions and the following disclaimer.
 * 
 * 2. Redistributions in binary form must reproduce the above copyright
 * notice, this list of conditions and the following disclaimer in the
 * documentation and/or other materials provided with the distribution.
 * 
 * 3. Redistributions in any form must be accompanied by information on how
 * to obtain complete source code for this software and any accompanying
 * software that uses this software.  The source code must either be
 * included in the distribution or be available for no more than the cost
 * of distribution plus a nominal fee, and must be freely redistributable
 * under reasonable conditions.  For an executable file, complete source
 * code means the source code for all modules it contains. It does not
 * include source code for modules or files that typically accompany the
 * major components of the operating system on which the executable file
 * runs.
 * 
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
 * IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 * TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR
 * PURPOSE, OR NON-INFRINGEMENT, ARE DISCLAIMED. IN NO EVENT SHALL THE
 * COPYRIGHT HOLDERS AND CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 * NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF
 * USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
 * ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 * THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
