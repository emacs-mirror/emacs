/* impl.h.mpstd: HARLEQUIN MEMORY POOL SYSTEM TARGET DETECTION
 *
 * $HopeName: MMsrc!mpstd.h(trunk.9) $
 * Copyright (C) 1996 Harlequin Group, all rights reserved
 *
 * Detect the target platform using predefined preprocessor symbols
 * defined by the build environment.  The symbols are derived from the
 * documentation, or, in the case of GCC, from the compiler itself.
 * References to the documentation appear above each detection line.
 *
 * .hack.align:  All alignments have been hacked to be at least 8.
 *   This is a short term fix to meet req.epcore.attr.align
 */

#ifndef mpstd_h
#define mpstd_h

/* Some random pickings from cc(1) on a mips IRIX 5.2 machine (atilla) */

#if defined(__DSO__) && defined(__sgi) && defined(__unix) && defined(__mips)
#define MPS_PF_IRR4CC
#define MPS_OS_IR
#define MPS_ARCH_R4
#define MPS_BUILD_CC
#define MPS_T_WORD	unsigned long
#define MPS_WORD_WIDTH	32
#define MPS_WORD_SHIFT	5
#define MPS_PF_ALIGN	8 /* .hack.align */

/* winnt.h (D:\packages\msvc20\include\winnt.h on aaron) */
/* really ought to check this more thoroughly */

#elif defined(_MSC_VER) && defined(_WIN32) && defined(_M_ALPHA)
#define MPS_PF_W3ALMV
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
#define MPS_OS_W3
#define MPS_ARCH_I3
#define MPS_BUILD_MV
#define MPS_T_WORD      unsigned long
#define MPS_WORD_WIDTH  32
#define MPS_WORD_SHIFT  5
#define MPS_PF_ALIGN    8 /* .hack.align */

/* MW C/C++/ASM Lang Ref, pp175-176.  Metrowerks does not document */
/* a way to determine the OS -- we assume MacOS 7.  */

#elif defined(__MWERKS__) && __MC68K__ == 1
#define MPS_PF_S7M6MW
#define MPS_OS_S7
#define MPS_ARCH_M6
#define MPS_BUILD_MW
#define MPS_T_WORD      unsigned long
#define MPS_WORD_WIDTH  32
#define MPS_WORD_SHIFT  5
#define MPS_PF_ALIGN    8 /* .hack.align */

/* 1. MPW 3.0 C Ref, p. 43.						*/
/* 2. MPW SC/SCpp C/C++ Compiler for 68k Macintosh, p 3-60.		*/
/* These are the two MPW 68k compilers. They do not define anything 	*/
/* which lets us determine the system version. 				*/

#elif defined(m68k) && (defined (applec) || defined(__SC__))
#define MPS_PF_S7M6AC
#define MPS_OS_S7
#define MPS_ARCH_M6
#define MPS_BUILD_AC
#define MPS_T_WORD      unsigned long
#define MPS_WORD_WIDTH  32
#define MPS_WORD_SHIFT  5
#define MPS_PF_ALIGN    8 /* .hack.align */

/* 1. C++/C Compiler for Macintosh with PowerPC, p 3-36.		*/
/* 2. MPW MrC/MrCpp C/C++ Compiler for Power Macintosh, p 3-57.		*/
/* These are the two MPW PowerPC compilers. They do not define anything	*/
/* which lets us determine the system version. 				*/

#elif defined(__PPCC__) || (defined(__MRC__) && defined(__POWERPC__))
#define MPS_PF_S7PPAC
#define MPS_OS_S7
#define MPS_ARCH_PP
#define MPS_BUILD_AC
#define MPS_T_WORD      unsigned long
#define MPS_WORD_WIDTH  32
#define MPS_WORD_SHIFT  5
#define MPS_PF_ALIGN    8 /* .hack.align */

/* GCC 2.5.8, gcc -E -dM, (__SVR4 indicates Solaris) */

#elif defined(__sun__) && defined(__sparc__) && defined(__GNUC__) && \
      !defined(__svr4__)
#define MPS_PF_SUSPGC
#define MPS_OS_SU
#define MPS_ARCH_SP
#define MPS_BUILD_GC
#define MPS_T_WORD      unsigned long
#define MPS_WORD_WIDTH  32
#define MPS_WORD_SHIFT  5
#define MPS_PF_ALIGN    8

/* GCC 2.5.8, gcc -E -dM */

#elif defined(__sun__) && defined(__sparc__) && defined(__GNUC__) && \
      defined(__svr4__)
#define MPS_PF_SOSPGC
#define MPS_OS_SO
#define MPS_ARCH_SP
#define MPS_BUILD_GC
#define MPS_T_WORD      unsigned long
#define MPS_WORD_WIDTH  32
#define MPS_WORD_SHIFT  5
#define MPS_PF_ALIGN    8

/* GCC 2.6.3, gcc -E -dM */

#elif defined(__osf__) && defined(__alpha__) && defined(__GNUC__)
#define MPS_PF_O1ALGC
#define MPS_OS_O1
#define MPS_ARCH_AL
#define MPS_BUILD_GC
#define MPS_T_WORD      unsigned long
#define MPS_T_SHORT     unsigned
#define MPS_WORD_WIDTH  64
#define MPS_WORD_SHIFT  6
#define MPS_PF_ALIGN    8

/* GCC 2.6.3, gcc -E -dM */

#elif defined(__linux__) && defined(__i386__) && defined(__GNUC__)
#define MPS_PF_LII3GC
#define MPS_OS_LI
#define MPS_ARCH_I3
#define MPS_BUILD_GC
#define MPS_T_WORD      unsigned long
#define MPS_WORD_WIDTH  32
#define MPS_WORD_SHIFT  5
#define MPS_PF_ALIGN    8 /* .hack.align */

#else
#error "Unable to detect target platform"
#endif


#endif /* mpstd_h */
