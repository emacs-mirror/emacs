/* impl.h.mpstd: HARLEQUIN MEMORY POOL SYSTEM TARGET DETECTION
 *
 * $HopeName: !mpstd.h(trunk.25) $
 * Copyright (C) 2000 Harlequin Limited.  All rights reserved.
 *
 * Detect the target platform using predefined preprocessor symbols
 * defined by the build environment.  The symbols are derived from the
 * documentation, or, in the case of GCC, from the compiler itself.
 * References to the documentation appear above each detection line.
 *
 * .hack.align:  All alignments have been hacked to be at least 8.
 * This is a short term fix to meet req.epcore.attr.align.  (some
 * of them really ought to be 8, others should be smaller)
 *
 * .macos.ppc.align: MacOS / PowerPC requires 8 bytes alignment (in
 * general).  See arch.pp.mac and arch.pp.align.
 */

#ifndef mpstd_h
#define mpstd_h

/* Irix 5/6 man cc and man abi.  We can't check for _ABIO32 (see
 * os.i5), as we have to support Irix 5.2, which doesn't define it.  We
 * check the value of _MIPS_FPSET, as it is defined across all Irix 5
 * and 6 platforms, and on Irix 6 distinguishes O32 from the other two
 * ABIs.  When we support the other ABIs, we need a new OS name for
 * them.  See analysis.irix-cpp. */

#if defined(__sgi) && defined(__unix) && defined(__mips) && defined(_SYSTYPE_SVR4) && (_MIPS_FPSET == 16)
#define MPS_PF_I5M2CC
#define MPS_PF_STRING   "i5m2cc"
#define MPS_OS_I5
#define MPS_ARCH_M2
#define MPS_BUILD_CC
#define MPS_T_WORD      unsigned long
#define MPS_WORD_WIDTH  32
#define MPS_WORD_SHIFT  5
#define MPS_PF_ALIGN    8 /* .hack.align */

/* See analysis.irix-cpp */

#elif defined(__sgi) && defined(__unix) && defined(__mips) && defined(_SYSTYPE_SVR4) && defined(_ABIN32)
#define MPS_PF_IAM4CC
#define MPS_PF_STRING   "iam4cc"
#define MPS_OS_IA
#define MPS_ARCH_M4
#define MPS_BUILD_CC
#define MPS_T_WORD      unsigned long
#define MPS_WORD_WIDTH  32
#define MPS_WORD_SHIFT  5
#define MPS_PF_ALIGN    8 /* .hack.align */

/* winnt.h (D:\packages\msvc20\include\winnt.h on aaron) */
/* really ought to check this more thoroughly */

#elif defined(_MSC_VER) && defined(_WIN32) && defined(_M_ALPHA)
#define MPS_PF_W3ALMV
#define MPS_PF_STRING   "w3almv"
#define MPS_OS_W3
#define MPS_ARCH_AL
#define MPS_BUILD_MV
#define MPS_T_WORD      unsigned long
#define MPS_WORD_WIDTH  32
#define MPS_WORD_SHIFT  5
#define MPS_PF_ALIGN    8 /* .hack.align */

/* winnt.h (D:\packages\msvc20\include\winnt.h on aaron) */
/* really ought to check this more thoroughly */

#elif defined(_MSC_VER) && defined(_WIN32) && defined(_M_PPC)
#define MPS_PF_W3PPMV
#define MPS_PF_STRING   "w3ppmv"
#define MPS_OS_W3
#define MPS_ARCH_PP
#define MPS_BUILD_MV
#define MPS_T_WORD      unsigned long
#define MPS_WORD_WIDTH  32
#define MPS_WORD_SHIFT  5
#define MPS_PF_ALIGN    8 /* .hack.align */

/* Visual C++ 2.0, Books Online, C/C++ Book, Preprocessor Reference, */
/* Chapter 1: The Preprocessor, Macros, Predefined Macros. */

#elif defined(_MSC_VER) && defined(_WIN32) && defined(_M_IX86)
#define MPS_PF_W3I3MV
#define MPS_PF_STRING   "w3i3mv"
#define MPS_OS_W3
#define MPS_ARCH_I3
#define MPS_BUILD_MV
#define MPS_T_WORD      unsigned long
#define MPS_WORD_WIDTH  32
#define MPS_WORD_SHIFT  5
#define MPS_PF_ALIGN    8 /* .hack.align */

/* MW C/C++/ASM Lang Ref (CW9), pp184-186.  Metrowerks does not document */
/* a way to determine the OS -- we assume MacOS 7.  */

#elif defined(__MWERKS__) && __MC68K__ == 1
#define MPS_PF_S760MW
#define MPS_PF_STRING   "s760mw"
#define MPS_OS_S7
#define MPS_ARCH_60
#define MPS_BUILD_MW
#define MPS_T_WORD      unsigned long
#define MPS_WORD_WIDTH  32
#define MPS_WORD_SHIFT  5
#define MPS_PF_ALIGN    8 /* .hack.align */

/* MW C/C++/ASM Lang Ref (CW9), pp184-186.  Metrowerks does not document */
/* a way to determine the OS -- we assume MacOS 7.  */

#elif defined(__MWERKS__) && __POWERPC__ == 1
#define MPS_PF_S7PPMW
#define MPS_PF_STRING   "s7ppmw"
#define MPS_OS_S7
#define MPS_ARCH_PP
#define MPS_BUILD_MW
#define MPS_T_WORD      unsigned long
#define MPS_WORD_WIDTH  32
#define MPS_WORD_SHIFT  5
#define MPS_PF_ALIGN    8 /* .macos.ppc.align */

/* 1. MPW 3.0 C Ref, p. 43.                                             */
/* 2. MPW SC/SCpp C/C++ Compiler for 68k Macintosh, p 3-60.             */
/* These are the two MPW 68k compilers. They do not define anything     */
/* which lets us determine the system version.                          */

#elif defined(m68k) && (defined (applec) || defined(__SC__))
#define MPS_PF_S760AC
#define MPS_PF_STRING   "s760ac"
#define MPS_OS_S7
#define MPS_ARCH_60
#define MPS_BUILD_AC
#define MPS_T_WORD      unsigned long
#define MPS_WORD_WIDTH  32
#define MPS_WORD_SHIFT  5
#define MPS_PF_ALIGN    8 /* .hack.align */

/* 1. C++/C Compiler for Macintosh with PowerPC, p 3-36.                */
/* 2. MPW MrC/MrCpp C/C++ Compiler for Power Macintosh, p 3-57.         */
/* These are the two MPW PowerPC compilers. They do not define anything */
/* which lets us determine the system version.                          */

#elif defined(__PPCC__) || (defined(__MRC__) && defined(__POWERPC))
#define MPS_PF_S7PPAC
#define MPS_PF_STRING   "s7ppac"
#define MPS_OS_S7
#define MPS_ARCH_PP
#define MPS_BUILD_AC
#define MPS_T_WORD      unsigned long
#define MPS_WORD_WIDTH  32
#define MPS_WORD_SHIFT  5
#define MPS_PF_ALIGN    8 /* .macos.ppc.align */

/* GCC 2.5.8, gcc -E -dM, (__SVR4 indicates Solaris) */

#elif defined(__sun__) && defined(__sparc__) && defined(__GNUC__) && \
      !defined(__svr4__)
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

#elif defined(sun) && defined(sparc) && defined(__LCC__) && \
      !defined(__svr4__)
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

#elif defined(__sun__) && defined(__sparc__) && defined(__GNUC__) && \
      defined(__svr4__)
#define MPS_PF_SOS8GC
#define MPS_PF_STRING   "sos8gc"
#define MPS_OS_SO
#define MPS_ARCH_S8
#define MPS_BUILD_GC
#define MPS_T_WORD      unsigned long
#define MPS_WORD_WIDTH  32
#define MPS_WORD_SHIFT  5
#define MPS_PF_ALIGN    8

/* SunPro C, man cc (confirmed by grep). Note that this doesn't
 * actually nail down UltraSPARCs; there are no compiler predefined
 * macros for that. */

#elif defined(__sun) && defined(__SUNPRO_C) && defined(__SVR4) && defined(__sparc)

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
#define MPS_PF_O1ALGC
#define MPS_PF_STRING   "o1algc"
#define MPS_OS_O1
#define MPS_ARCH_AL
#define MPS_BUILD_GC
#define MPS_T_WORD      unsigned long
#define MPS_T_SHORT     unsigned
#define MPS_WORD_WIDTH  64
#define MPS_WORD_SHIFT  6
#define MPS_PF_ALIGN    8

/* From the cc(1) man page on schiele */

#elif defined(__osf__) && defined(__alpha) && defined(__DECC)
#define MPS_PF_O1ALCC
#define MPS_PF_STRING   "o1alcc"
#define MPS_OS_O1
#define MPS_ARCH_AL
#define MPS_BUILD_CC
#define MPS_T_WORD      unsigned long
#define MPS_T_SHORT     unsigned
#define MPS_WORD_WIDTH  64
#define MPS_WORD_SHIFT  6
#define MPS_PF_ALIGN    8

/* GCC 2.6.3, gcc -E -dM */

#elif defined(__linux__) && defined(__i386__) && defined(__GNUC__)
#define MPS_PF_LII3GC
#define MPS_PF_STRING   "lii4gc"
#define MPS_OS_LI
#define MPS_ARCH_I4
#define MPS_BUILD_GC
#define MPS_T_WORD      unsigned long
#define MPS_WORD_WIDTH  32
#define MPS_WORD_SHIFT  5
#define MPS_PF_ALIGN    8 /* .hack.align */

/* GCC 2.7.2, gcc -E -dM */
/* platform.lippgc knocked up very quickly by drj */

#elif defined(__linux__) && defined(__PPC__) && defined(__GNUC__)
#define MPS_PF_LIPPGC
#define MPS_PF_STRING   "lippgc"
#define MPS_OS_LI
#define MPS_ARCH_PP
#define MPS_BUILD_GC
#define MPS_T_WORD      unsigned long
#define MPS_WORD_WIDTH  32
#define MPS_WORD_SHIFT  5
#define MPS_PF_ALIGN    8 /* .hack.align; maybe it should be 8, who knows? */

/* GCC 2.95.3, gcc -E -dM
 */

#elif defined(__FreeBSD__) && defined (__i386__) && defined (__GNUC__)
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
