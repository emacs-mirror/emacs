/* mpstd.h: RAVENBROOK MEMORY POOL SYSTEM TARGET DETECTION
 *
 * $Id$
 * Copyright (c) 2001 Ravenbrook Limited.  See end of file for license.
 * Portions copyright (C) 2001 Global Graphics Software.
 *
 * Detect the target platform using predefined preprocessor symbols
 * defined by the build environment.  The symbols are derived from the
 * documentation, or, in the case of GCC, from the compiler itself.
 * References to the documentation appear above each detection line.
 *
 * .macos.ppc.align: MacOS / PowerPC requires 8 bytes alignment (in
 * general).  See "Mac OS Runtime Architecture", table 4-2.
 */

#ifndef mpstd_h
#define mpstd_h

/* DESIGN NOTES
 * ------------
 * [These should be moved to a proper buildsys design doc.  RHSK]
 *
 * mpstd.h does two main things:
 *   1. platform detection by looking at preprocessor symbols;
 *   2. setting variables (eg. MPS_PF_STRING, MPS_WORD_WIDTH).
 *
 * Sometimes the platform is *already* known by the buildsystem:
 *   - the Global Graphics buildsystem always sets CONFIG_PF_*.
 *   - the Ravenbrook buildsystem knows the platform and may (but
 *     typically does not) set CONFIG_PF_*.
 *
 * Regardless of this, mpstd.h still attempts to detect the platform.
 * (This is intentional).  However if both CONFIG_PF_* and
 * CONFIG_PF_STRING are set, then mpstd.h performs a third function:
 *   3. checking that the detected platform corresponds to that
 *      specified by CONFIG_PF_*.
 *
 * Sometimes no MPS buildsystem is in use, so the platform *must*
 * be detected.  For example, when client software #includes mps.h,
 * we want it to just work out of the box with whatever compiler is
 * being used.  In other words we do not require the client to define
 * CONFIG_PF_*.
 * (This is the case that justifes mpstd.h doing platform detection
 * by looking at preprocessor symbols; otherwise we'd simply use
 * CONFIG_PF_*).
 *
 * mpstd.h fails if it cannot detect the platform (even if CONFIG_PF_*
 * is specified).  This is intentional.  mpstd.h does *not* allow
 * CONFIG_PF_* to override the platform as detected from preprocessor
 * symbols.  This is intentional.
 *
 * References:
 * GG buildsys use of CONFIG_PF_*:
 *   <http://info.ravenbrook.com/mail/2005/03/01/15-45-17/0.txt>
 */


/* Visual C++ 2.0, Books Online, C/C++ Book, Preprocessor Reference,
 * Chapter 1: The Preprocessor, Macros, Predefined Macros.
 * Alignment of 4 would work, but the MS library uses 8 bytes for
 * doubles and __int64, so we choose that.  The actual granularity of
 * VC malloc is 16!
 */

#if defined(_MSC_VER) && defined(_WIN32) && defined(_M_IX86)
#if defined(CONFIG_PF_STRING) && ! defined(CONFIG_PF_W3I3MV)
#error "specified CONFIG_PF_... inconsistent with detected w3i3mv"
#endif
#define MPS_PF_W3I3MV
#define MPS_PF_STRING   "w3i3mv"
#define MPS_OS_W3
#define MPS_ARCH_I3
#define MPS_BUILD_MV
#define MPS_T_WORD      unsigned long
#define MPS_T_ULONGEST  unsigned long
#define MPS_WORD_WIDTH  32
#define MPS_WORD_SHIFT  5
#define MPS_PF_ALIGN    8


/* "Predefined Macros" from "Visual Studio 2010" on MSDN
 * <http://msdn.microsoft.com/en-us/library/b0084kay(v=vs.100).aspx>.
 * Note that Win32 includes 64-bit Windows!
 * We use the same alignment as MS malloc: 16, which is used for XMM
 * operations.
 * See MSDN -> x64 Software Conventions -> Overview of x64 Calling Conventions
 * <http://msdn.microsoft.com/en-us/library/ms235286> 
 */

#elif defined(_MSC_VER) && defined(_WIN32) && defined(_WIN64) && defined(_M_X64)
#if defined(CONFIG_PF_STRING) && ! defined(CONFIG_PF_W3I6MV)
#error "specified CONFIG_PF_... inconsistent with detected w3i6mv"
#endif
#define MPS_PF_W3I6MV
#define MPS_PF_STRING   "w3i6mv"
#define MPS_OS_W3
#define MPS_ARCH_I6
#define MPS_BUILD_MV
#define MPS_T_WORD      unsigned __int64
#define MPS_T_ULONGEST  unsigned __int64
#define MPS_WORD_WIDTH  64
#define MPS_WORD_SHIFT  6
#define MPS_PF_ALIGN    16


/* GCC 4.0.1 (As supplied by Apple on Mac OS X 10.4.8 on an Intel Mac),
 * gcc -E -dM
 * And above for xcppgc.
 * Note that Clang also defines __GNUC__ since it's generally GCC compatible,
 * but that doesn't fit our system so we exclude Clang here.
 */

#elif defined(__APPLE__) && defined(__i386__) && defined(__MACH__) \
      && defined(__GNUC__) && !defined(__clang__)
#if defined(CONFIG_PF_STRING) && ! defined(CONFIG_PF_XCI3GC)
#error "specified CONFIG_PF_... inconsistent with detected xci3gc"
#endif
#define MPS_PF_XCI3GC
#define MPS_PF_STRING   "xci3gc"
#define MPS_OS_XC
#define MPS_ARCH_I3
#define MPS_BUILD_GC
#define MPS_T_WORD      unsigned long
#define MPS_T_ULONGEST  unsigned long
#define MPS_WORD_WIDTH  32
#define MPS_WORD_SHIFT  5
#define MPS_PF_ALIGN    4       /* I'm just guessing. */


/* Apple clang version 3.1, clang -E -dM */

#elif defined(__APPLE__) && defined(__i386__) && defined(__MACH__) \
      && defined(__clang__)
#if defined(CONFIG_PF_STRING) && ! defined(CONFIG_PF_XCI3LL)
#error "specified CONFIG_PF_... inconsistent with detected xci3ll"
#endif
#define MPS_PF_XCI3LL
#define MPS_PF_STRING   "xci3ll"
#define MPS_OS_XC
#define MPS_ARCH_I3
#define MPS_BUILD_LL
#define MPS_T_WORD      unsigned long
#define MPS_T_ULONGEST  unsigned long
#define MPS_WORD_WIDTH  32
#define MPS_WORD_SHIFT  5
#define MPS_PF_ALIGN    4       /* I'm just guessing. */


/* Apple clang version 3.1, clang -E -dM */

#elif defined(__APPLE__) && defined(__x86_64__) && defined(__MACH__) \
      && defined(__clang__)
#if defined(CONFIG_PF_STRING) && ! defined(CONFIG_PF_XCI6LL)
#error "specified CONFIG_PF_... inconsistent with detected xci6ll"
#endif
#define MPS_PF_XCI6LL
#define MPS_PF_STRING   "xci6ll"
#define MPS_OS_XC
#define MPS_ARCH_I6
#define MPS_BUILD_LL
#define MPS_T_WORD      unsigned long
#define MPS_T_ULONGEST  unsigned long
#define MPS_WORD_WIDTH  64
#define MPS_WORD_SHIFT  6
#define MPS_PF_ALIGN    8


/* GCC 2.6.3, gcc -E -dM
 * The actual granularity of GNU malloc is 8, but field alignments are
 * all 4.
 */

#elif defined(__linux__) && defined(__i386__) && defined(__GNUC__)
#if defined(CONFIG_PF_STRING) && ! defined(CONFIG_PF_LII3GC)
#error "specified CONFIG_PF_... inconsistent with detected lii3gc"
#endif
#define MPS_PF_LII3GC
#define MPS_PF_STRING   "lii3gc"
#define MPS_OS_LI
#define MPS_ARCH_I3
#define MPS_BUILD_GC
#define MPS_T_WORD      unsigned long
#define MPS_T_ULONGEST  unsigned long
#define MPS_WORD_WIDTH  32
#define MPS_WORD_SHIFT  5
#define MPS_PF_ALIGN    4

#ifndef _REENTRANT        /* it's also defined by cc -pthread */
#define _REENTRANT        /* defines, e.g., pthread_mutexattr_settype */
#endif
#define _XOPEN_SOURCE 500 /* to get POSIX signal handling */
#define _GNU_SOURCE       /* to get register numbers for prmci3li.c */


/* GCC 4.6.3, gcc -E -dM */

#elif defined(__linux__) && defined(__x86_64) && defined(__GNUC__)
#if defined(CONFIG_PF_STRING) && ! defined(CONFIG_PF_LII6GC)
#error "specified CONFIG_PF_... inconsistent with detected lii6gc"
#endif
#define MPS_PF_LII6GC
#define MPS_PF_STRING   "lii6gc"
#define MPS_OS_LI
#define MPS_ARCH_I6
#define MPS_BUILD_GC
#define MPS_T_WORD      unsigned long
#define MPS_T_ULONGEST  unsigned long
#define MPS_WORD_WIDTH  64
#define MPS_WORD_SHIFT  6
#define MPS_PF_ALIGN    8

#ifndef _REENTRANT        /* it's also defined by cc -pthread */
#define _REENTRANT        /* defines, e.g., pthread_mutexattr_settype */
#endif
#define _XOPEN_SOURCE 500 /* to get POSIX signal handling */
#define _GNU_SOURCE       /* to get register numbers for prmci3li.c */


/* GCC 2.95.3, gcc -E -dM */

#elif defined(__FreeBSD__) && defined (__i386__) && defined (__GNUC__)
#if defined(CONFIG_PF_STRING) && ! defined(CONFIG_PF_FRI3GC)
#error "specified CONFIG_PF_... inconsistent with detected fri3gc"
#endif
#define MPS_PF_FRI3GC
#define MPS_PF_STRING   "fri3gc"
#define MPS_OS_FR
#define MPS_ARCH_I3
#define MPS_BUILD_GC
#define MPS_T_WORD      unsigned long
#define MPS_T_ULONGEST  unsigned long
#define MPS_WORD_WIDTH  32
#define MPS_WORD_SHIFT  5
#define MPS_PF_ALIGN    4


#elif defined(__FreeBSD__) && defined (__x86_64__) && defined (__GNUC__)
#if defined(CONFIG_PF_STRING) && ! defined(CONFIG_PF_FRI6GC)
#error "specified CONFIG_PF_... inconsistent with detected fri6gc"
#endif
#define MPS_PF_FRI6GC
#define MPS_PF_STRING   "fri6gc"
#define MPS_OS_FR
#define MPS_ARCH_I6
#define MPS_BUILD_GC
#define MPS_T_WORD      unsigned long
#define MPS_T_ULONGEST  unsigned long
#define MPS_WORD_WIDTH  64
#define MPS_WORD_SHIFT  6
#define MPS_PF_ALIGN    8


#else
#error "The MPS Kit does not have a configuration for this platform out of the box; see manual/build.txt"
#endif


#endif /* mpstd_h */


/* C. COPYRIGHT AND LICENSE
 *
 * Copyright (C) 2001-2002,2008 Ravenbrook Limited <http://www.ravenbrook.com/>.
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
