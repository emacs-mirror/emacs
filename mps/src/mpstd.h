/* impl.h.mpstd: HARLEQUIN MEMORY POOL SYSTEM TARGET DETECTION
 *
 * Detect the target platform using predefined preprocessor symbols
 * defined by the build environment.  The symbols are derived from the
 * documentation, or, in the case of GCC, from the compiler itself.
 * References to the documentation appear above each detection line.
 */

#ifndef mpstd_h
#define mpstd_h

/* Some random pickings from cc(1) on a mips IRIX 5.2 machine (atilla) */

#if defined(__DSO__) && defined(__sgi) && defined(__unix) && defined(__mips)
#define MPS_PF_I4R4CC
#define MPS_OS_I4
#define MPS_ARCH_R4
#define MPS_BUILD_CC
#define MPS_T_WORD	unsigned long
#define MPS_WORD_WIDTH	32
#define MPS_WORD_SHIFT	5
#define MPS_PF_ALIGN	4

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
#define MPS_PF_ALIGN    4

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
#define MPS_PF_ALIGN    1

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
#define MPS_PF_ALIGN    4

#else
#error "Unable to detect target platform"
#endif


#endif /* mpstd_h */
