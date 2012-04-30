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


/* Irix 5/6 man cc and man abi.  We can't check for _ABIO32 (see
 * os.i5), as we have to support Irix 5.2, which doesn't define it.  We
 * check the value of _MIPS_FPSET, as it is defined across all Irix 5
 * and 6 platforms, and on Irix 6 distinguishes O32 from the other two
 * ABIs.  When we support the other ABIs, we need a new OS name for
 * them.  Alignment from testing.
 */

#if defined(__sgi) && defined(__unix) && defined(__mips) \
    && defined(_SYSTYPE_SVR4) && (_MIPS_FPSET == 16)
#if defined(CONFIG_PF_STRING) && ! defined(CONFIG_PF_I5M2CC)
#error "specified CONFIG_PF_... inconsistent with detected i5m2cc"
#endif
#define MPS_PF_I5M2CC
#define MPS_PF_STRING   "i5m2cc"
#define MPS_OS_I5
#define MPS_ARCH_M2
#define MPS_BUILD_CC
#define MPS_T_WORD      unsigned long
#define MPS_WORD_WIDTH  32
#define MPS_WORD_SHIFT  5
#define MPS_PF_ALIGN    8

/* See above.  Alignment from testing. */

#elif defined(__sgi) && defined(__unix) && defined(__mips) \
      && defined(_SYSTYPE_SVR4) && defined(_ABIN32)
#if defined(CONFIG_PF_STRING) && ! defined(CONFIG_PF_IAM4CC)
#error "specified CONFIG_PF_... inconsistent with detected iam4cc"
#endif
#define MPS_PF_IAM4CC
#define MPS_PF_STRING   "iam4cc"
#define MPS_OS_IA
#define MPS_ARCH_M4
#define MPS_BUILD_CC
#define MPS_T_WORD      unsigned long
#define MPS_WORD_WIDTH  32
#define MPS_WORD_SHIFT  5
#define MPS_PF_ALIGN    8

/* winnt.h from MS VC 2.0 */

#elif defined(_MSC_VER) && defined(_WIN32) && defined(_M_ALPHA)
#if defined(CONFIG_PF_STRING) && ! defined(CONFIG_PF_W3ALMV)
#error "specified CONFIG_PF_... inconsistent with detected w3almv"
#endif
#define MPS_PF_W3ALMV
#define MPS_PF_STRING   "w3almv"
#define MPS_OS_W3
#define MPS_ARCH_AL
#define MPS_BUILD_MV
#define MPS_T_WORD      unsigned long
#define MPS_WORD_WIDTH  32
#define MPS_WORD_SHIFT  5
#define MPS_PF_ALIGN    4

/* winnt.h from MS VC 2.0 */

#elif defined(_MSC_VER) && defined(_WIN32) && defined(_M_PPC)
#if defined(CONFIG_PF_STRING) && ! defined(CONFIG_PF_W3PPMV)
#error "specified CONFIG_PF_... inconsistent with detected w3ppmv"
#endif
#define MPS_PF_W3PPMV
#define MPS_PF_STRING   "w3ppmv"
#define MPS_OS_W3
#define MPS_ARCH_PP
#define MPS_BUILD_MV
#define MPS_T_WORD      unsigned long
#define MPS_WORD_WIDTH  32
#define MPS_WORD_SHIFT  5
#define MPS_PF_ALIGN    4


/* Visual C++ 2.0, Books Online, C/C++ Book, Preprocessor Reference,
 * Chapter 1: The Preprocessor, Macros, Predefined Macros.
 * Alignment of 4 would work, but the MS library uses 8 bytes for
 * doubles and __int64, so we choose that.  The actual granularity of
 * VC malloc is 16!
 */

#elif defined(_MSC_VER) && defined(_WIN32) && defined(_M_IX86)
#if defined(CONFIG_PF_STRING) && ! defined(CONFIG_PF_W3I3MV)
#error "specified CONFIG_PF_... inconsistent with detected w3i3mv"
#endif
#define MPS_PF_W3I3MV
#define MPS_PF_STRING   "w3i3mv"
#define MPS_OS_W3
#define MPS_ARCH_I3
#define MPS_BUILD_MV
#define MPS_T_WORD      unsigned long
#define MPS_WORD_WIDTH  32
#define MPS_WORD_SHIFT  5
#define MPS_PF_ALIGN    8


/* Duplicated from W3I3MV, then... @@@@
 * "Predefined Macros" from "Visual Studio 2010" on MSDN
 * <http://msdn.microsoft.com/en-us/library/b0084kay(v=vs.100).aspx>.
 * Note that Win32 includes 64-bit Windows!
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
#define MPS_T_LONGEST   __int64
#define MPS_T_ULONGEST  unsigned __int64
#define MPS_WORD_WIDTH  64
#define MPS_WORD_SHIFT  6
#define MPS_PF_ALIGN    16 /* @@@@ ref? */


/* MW C/C++/ASM Lang Ref (CW9), pp. 184-186.  Metrowerks does not document
 * a way to determine the OS -- we assume MacOS 7.
 */

#elif (defined(__MWERKS__) && __MC68K__ == 1)
#if defined(CONFIG_PF_STRING) && ! defined(CONFIG_PF_S760MW)
#error "specified CONFIG_PF_... inconsistent with detected s760mw"
#endif
#define MPS_PF_S760MW
#define MPS_PF_STRING   "s760mw"
#define MPS_OS_S7
#define MPS_ARCH_60
#define MPS_BUILD_MW
#define MPS_T_WORD      unsigned long
#define MPS_WORD_WIDTH  32
#define MPS_WORD_SHIFT  5
#define MPS_PF_ALIGN    1

/* MW C/C++/ASM Lang Ref (CW9), pp. 184-186.  Metrowerks does not document
 * a way to determine the OS -- we assume MacOS 7.
 */

#elif defined(__MWERKS__) && __POWERPC__ == 1
#if defined(CONFIG_PF_STRING) && ! defined(CONFIG_PF_S7PPMW)
#error "specified CONFIG_PF_... inconsistent with detected s7ppmw"
#endif
#define MPS_PF_S7PPMW
#define MPS_PF_STRING   "s7ppmw"
#define MPS_OS_S7
#define MPS_ARCH_PP
#define MPS_BUILD_MW
#define MPS_T_WORD      unsigned long
#define MPS_WORD_WIDTH  32
#define MPS_WORD_SHIFT  5
#define MPS_PF_ALIGN    8 /* .macos.ppc.align */

/* 1. MPW 3.0 C Ref, p. 43.
 * 2. MPW SC/SCpp C/C++ Compiler for 68k Macintosh, p 3-60.
 * These are the two MPW 68k compilers.  They do not define anything
 * which lets us determine the system version.
 */

#elif defined(m68k) && (defined (applec) || defined(__SC__))
#if defined(CONFIG_PF_STRING) && ! defined(CONFIG_PF_S760AC)
#error "specified CONFIG_PF_... inconsistent with detected s760ac"
#endif
#define MPS_PF_S760AC
#define MPS_PF_STRING   "s760ac"
#define MPS_OS_S7
#define MPS_ARCH_60
#define MPS_BUILD_AC
#define MPS_T_WORD      unsigned long
#define MPS_WORD_WIDTH  32
#define MPS_WORD_SHIFT  5
#define MPS_PF_ALIGN    1

/* 1. C++/C Compiler for Macintosh with PowerPC, p 3-36.
 * 2. MPW MrC/MrCpp C/C++ Compiler for Power Macintosh, p 3-57.
 * These are the two MPW PowerPC compilers.  They do not define anything
 * which lets us determine the system version.
 */

#elif defined(__PPCC__) || (defined(__MRC__) && defined(__POWERPC))
#if defined(CONFIG_PF_STRING) && ! defined(CONFIG_PF_S7PPAC)
#error "specified CONFIG_PF_... inconsistent with detected s7ppac"
#endif
#define MPS_PF_S7PPAC
#define MPS_PF_STRING   "s7ppac"
#define MPS_OS_S7
#define MPS_ARCH_PP
#define MPS_BUILD_AC
#define MPS_T_WORD      unsigned long
#define MPS_WORD_WIDTH  32
#define MPS_WORD_SHIFT  5
#define MPS_PF_ALIGN    8 /* .macos.ppc.align */

/* GCC 2.7.2.1, gcc -E -dM -traditional-cpp and <URL:http://developer.apple.c
 * om/techpubs/macosx/System/Documentation/Developer/YellowBox/Reference/DevT
 * ools/Preprocessor/Preprocessor.[ef].html>
 */

#elif defined(__APPLE__) && defined(__ppc__) && defined(__MACH__) \
      && defined(__GNUC__)
#if defined(CONFIG_PF_STRING) && ! defined(CONFIG_PF_XCPPGC)
#error "specified CONFIG_PF_... inconsistent with detected xcppgc"
#endif
#define MPS_PF_XCPPGC
#define MPS_PF_STRING   "xcppgc"
#define MPS_OS_XC
#define MPS_ARCH_PP
#define MPS_BUILD_GC
#define MPS_T_WORD      unsigned long
#define MPS_WORD_WIDTH  32
#define MPS_WORD_SHIFT  5
#define MPS_PF_ALIGN    8 /* .macos.ppc.align */

/* GCC 4.0.1 (As supplied by Apple on Mac OS X 10.4.8 on an Intel Mac),
 * gcc -E -dM
 * And above for xcppgc.
 */

#elif defined(__APPLE__) && defined(__i386__) && defined(__MACH__) \
      && defined(__GNUC__)
#if defined(CONFIG_PF_STRING) && ! defined(CONFIG_PF_XCI3GC)
#error "specified CONFIG_PF_... inconsistent with detected xci3gc"
#endif
#define MPS_PF_XCI3GC
#define MPS_PF_STRING   "xci3gc"
#define MPS_OS_XC
#define MPS_ARCH_I3
#define MPS_BUILD_GC
#define MPS_T_WORD      unsigned long
#define MPS_WORD_WIDTH  32
#define MPS_WORD_SHIFT  5
#define MPS_PF_ALIGN    4       /* I'm just guessing. */

/* @@@@ fill in */

#elif defined(__APPLE__) && defined(__x86_64__) && defined(__MACH__) \
      && defined(__GNUC__)
#if defined(CONFIG_PF_STRING) && ! defined(CONFIG_PF_XCI6GC)
#error "specified CONFIG_PF_... inconsistent with detected xci6gc"
#endif
#define MPS_PF_XCI6GC
#define MPS_PF_STRING   "xci6gc"
#define MPS_OS_XC
#define MPS_ARCH_I6
#define MPS_BUILD_GC
#define MPS_T_WORD      unsigned long
#define MPS_WORD_WIDTH  64
#define MPS_WORD_SHIFT  6
#define MPS_PF_ALIGN    8       /* I'm just guessing. */

/* GCC 2.5.8, gcc -E -dM, (__SVR4 indicates Solaris) */

#elif defined(__sun__) && defined(__sparc__) && defined(__GNUC__) \
      && !defined(__svr4__)
#if defined(CONFIG_PF_STRING) && ! defined(CONFIG_PF_SUS8GC)
#error "specified CONFIG_PF_... inconsistent with detected sus8gc"
#endif
#define MPS_PF_SUS8GC
#define MPS_PF_STRING   "sus8gc"
#define MPS_OS_SU
#define MPS_ARCH_S8
#define MPS_BUILD_GC
#define MPS_T_WORD      unsigned long
#define MPS_WORD_WIDTH  32
#define MPS_WORD_SHIFT  5
#define MPS_PF_ALIGN    8

/* LCC 3.4 (ish), man page */

#elif defined(sun) && defined(sparc) && defined(__LCC__) \
      && !defined(__svr4__)
#if defined(CONFIG_PF_STRING) && ! defined(CONFIG_PF_SUS8LC)
#error "specified CONFIG_PF_... inconsistent with detected sus8lc"
#endif
#define MPS_PF_SUS8LC
#define MPS_PF_STRING   "sus8lc"
#define MPS_OS_SU
#define MPS_ARCH_S8
#define MPS_BUILD_LC
#define MPS_T_WORD      unsigned long
#define MPS_WORD_WIDTH  32
#define MPS_WORD_SHIFT  5
#define MPS_PF_ALIGN    8

/* GCC 2.5.8, gcc -E -dM */

#elif defined(__sun__) && defined(__sparc__) && defined(__GNUC__) \
      && defined(__svr4__)
#if defined(CONFIG_PF_STRING) && ! defined(CONFIG_PF_SOS8GC)
#error "specified CONFIG_PF_... inconsistent with detected sos8gc"
#endif
#define MPS_PF_SOS8GC
#define MPS_PF_STRING   "sos8gc"
#define MPS_OS_SO
#define MPS_ARCH_S8
#define MPS_BUILD_GC
#define MPS_T_WORD      unsigned long
#define MPS_WORD_WIDTH  32
#define MPS_WORD_SHIFT  5
#define MPS_PF_ALIGN    8

/* SunPro C, man cc (confirmed by grep).  Note that this doesn't
 * actually nail down UltraSPARCs; there are no compiler predefined
 * macros for that.
 */

#elif defined(__sun) && defined(__SUNPRO_C) && defined(__SVR4) \
      && defined(__sparc)
#if defined(CONFIG_PF_STRING) && ! defined(CONFIG_PF_SOS9SC)
#error "specified CONFIG_PF_... inconsistent with detected sos9sc"
#endif
#define MPS_PF_SOS9SC
#define MPS_PF_STRING   "sos9sc"
#define MPS_OS_SO
#define MPS_ARCH_S9
#define MPS_BUILD_SC
#define MPS_T_WORD      unsigned long
#define MPS_WORD_WIDTH  32
#define MPS_WORD_SHIFT  5
#define MPS_PF_ALIGN    8

/* GCC 2.6.3, gcc -E -dM */

#elif defined(__osf__) && defined(__alpha__) && defined(__GNUC__)
#if defined(CONFIG_PF_STRING) && ! defined(CONFIG_PF_O1ALGC)
#error "specified CONFIG_PF_... inconsistent with detected o1algc"
#endif
#define MPS_PF_O1ALGC
#define MPS_PF_STRING   "o1algc"
#define MPS_OS_O1
#define MPS_ARCH_AL
#define MPS_BUILD_GC
#define MPS_T_WORD      unsigned long
#define MPS_WORD_WIDTH  64
#define MPS_WORD_SHIFT  6
#define MPS_PF_ALIGN    8

/* From the cc(1) man page */

#elif defined(__osf__) && defined(__alpha) && defined(__DECC)
#if defined(CONFIG_PF_STRING) && ! defined(CONFIG_PF_O1ALCC)
#error "specified CONFIG_PF_... inconsistent with detected o1alcc"
#endif
#define MPS_PF_O1ALCC
#define MPS_PF_STRING   "o1alcc"
#define MPS_OS_O1
#define MPS_ARCH_AL
#define MPS_BUILD_CC
#define MPS_T_WORD      unsigned long
#define MPS_WORD_WIDTH  64
#define MPS_WORD_SHIFT  6
#define MPS_PF_ALIGN    8

/* GCC 2.6.3, gcc -E -dM
 * The actual granularity of GNU malloc is 8, but field alignments are
 * all 4.
 */

#elif defined(__linux__) && defined(__i386__) && defined(__GNUC__)
#if defined(CONFIG_PF_STRING) && ! defined(CONFIG_PF_LII4GC)
#error "specified CONFIG_PF_... inconsistent with detected lii4gc"
#endif
#define MPS_PF_LII4GC
#define MPS_PF_STRING   "lii4gc"
#define MPS_OS_LI
#define MPS_ARCH_I4
#define MPS_BUILD_GC
#define MPS_T_WORD      unsigned long
#define MPS_WORD_WIDTH  32
#define MPS_WORD_SHIFT  5
#define MPS_PF_ALIGN    4

/* GCC 2.7.2, gcc -E -dM */

#elif defined(__linux__) && defined(__PPC__) && defined(__GNUC__)
#if defined(CONFIG_PF_STRING) && ! defined(CONFIG_PF_LIPPGC)
#error "specified CONFIG_PF_... inconsistent with detected lippgc"
#endif
#define MPS_PF_LIPPGC
#define MPS_PF_STRING   "lippgc"
#define MPS_OS_LI
#define MPS_ARCH_PP
#define MPS_BUILD_GC
#define MPS_T_WORD      unsigned long
#define MPS_WORD_WIDTH  32
#define MPS_WORD_SHIFT  5
#define MPS_PF_ALIGN    8 /* @@@@ not tested */

/* GCC 2.95.3, gcc -E -dM
 */

#elif defined(__FreeBSD__) && defined (__i386__) && defined (__GNUC__)
#if defined(CONFIG_PF_STRING) && ! defined(CONFIG_PF_FRI4GC)
#error "specified CONFIG_PF_... inconsistent with detected fri4gc"
#endif
#define MPS_PF_FRI4GC
#define MPS_PF_STRING   "fri4gc"
#define MPS_OS_FR
#define MPS_ARCH_I4
#define MPS_BUILD_GC
#define MPS_T_WORD      unsigned long
#define MPS_WORD_WIDTH  32
#define MPS_WORD_SHIFT  5
#define MPS_PF_ALIGN    4

#else
#error "Unable to detect target platform"
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
