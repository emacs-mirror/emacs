/* mpslib.h: RAVENBROOK MEMORY POOL SYSTEM LIBRARY INTERFACE
 *
 * $Id$
 * Copyright (c) 2001-2017 Ravenbrook Limited.  See end of file for license.
 *
 * .readership: MPS client application developers, MPS developers.
 * .sources: <design/lib/>
 *
 * .purpose: The purpose of this file is to declare the functions and types
 * required for the MPS library interface.
 */

#ifndef mpslib_h
#define mpslib_h

#include <stddef.h>
#include "mps.h"  /* mps_clock_t */

/* Return the token that will be returned by I/O functions when the end
   of file is reached.  Analogous to `EOF` from stdio.h. */
extern int mps_lib_get_EOF(void);
#define mps_lib_EOF     (mps_lib_get_EOF())

/* An anonymous structure type used to represent files.  Analagous to
   `FILE *` from stdio.h. */
typedef struct mps_lib_stream_s mps_lib_FILE;

/* Return the standard output and standard error streams.  Analagous to
   `stdout` and `stderr` from stdio.h. */
extern mps_lib_FILE *mps_lib_get_stderr(void);
extern mps_lib_FILE *mps_lib_get_stdout(void);
#define mps_lib_stderr  (mps_lib_get_stderr())
#define mps_lib_stdout  (mps_lib_get_stdout())

/* Send a character or string to a stream.  Analagous to `fputc` and `fputs`
   from stdio.h. */
extern int mps_lib_fputc(int, mps_lib_FILE *);
extern int mps_lib_fputs(const char *, mps_lib_FILE *);

/* Assertion handler.  When the MPS detects an illegal condition, it calls
   `mps_lib_assert_fail` with the source code filename, line number, and
   a string representing the condition.  That function should log or report
   the condition, and preferably allow for debugging, though in a production
   environment it can return and the MPS will attempt to continue, though
   this may cause failure of the process soon after. */
extern void mps_lib_assert_fail(const char *, unsigned, const char *);

/* The default ANSI plinth in mpsliban.c allows the assertion handler to be
   replaced by passing a replacement to `mps_lib_assert_fail_install`,
   which returns the previous handler.  This is for convenience so that
   a complete replacement plinth need not be supplied just to achieve the
   same thing.  The MPS itself does not use `mps_lib_assert_fail_install`
   and so it need not be supplied by the plinth. */
typedef void (*mps_lib_assert_fail_t)(const char *, unsigned, const char *);
extern mps_lib_assert_fail_t mps_lib_assert_fail_install(mps_lib_assert_fail_t);


/* Set, copy, or compare memory.  Analagous to `memset`, `memcpy`, and
   `memcmp` from string.h. */
extern void *(mps_lib_memset)(void *, int, size_t);
extern void *(mps_lib_memcpy)(void *, const void *, size_t);
extern int (mps_lib_memcmp)(const void *, const void *, size_t);

/* Return a measure of time since process start.  Equivalent to `clock`
   from time.h. */
extern mps_clock_t mps_clock(void);
extern mps_clock_t mps_clocks_per_sec(void);


/* Return a telemetry control word from somewhere.  This controls which kinds
   of events get output to the telemetry stream.  Each bit in the word
   switches on the corresponding EventKind defined in eventcom.h. */
extern unsigned long mps_lib_telemetry_control(void);


#endif /* mpslib_h */


/* C. COPYRIGHT AND LICENSE
 *
 * Copyright (C) 2001-2017 Ravenbrook Limited <http://www.ravenbrook.com/>.
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
