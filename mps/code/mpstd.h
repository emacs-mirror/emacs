/* mpstd.h: RAVENBROOK MEMORY POOL SYSTEM TARGET DETECTION
 *
 * $Id$
 * Copyright (c) 2001-2020 Ravenbrook Limited.  See end of file for license.
 * Portions copyright (C) 2001 Global Graphics Software.
 *
 * Detect the target platform using predefined preprocessor symbols
 * defined by the build environment.  The symbols are derived from the
 * documentation, or, in the case of GCC, from the compiler itself.
 * References to the documentation appear above each detection line.
 *
 * For more details on how this file fits into the MPS build system,
 * and an explanation of all the MPS_* defines, see design.config.pf
 * "MPS Configuration" <../design/config.txt>
 *
 * .macos.ppc.align: MacOS / PowerPC requires 8 bytes alignment (in
 * general).  See "Mac OS Runtime Architecture", table 4-2.
 *
 * mpstd.h fails if it cannot detect the platform (even if CONFIG_PF_*
 * is specified).  This is intentional.  mpstd.h does *not* allow
 * CONFIG_PF_* to override the platform as detected from preprocessor
 * symbols.  This is intentional. [This needs justifying. RB 2013-05-11]
 */

#ifndef mpstd_h
#define mpstd_h


/* Visual C++ 2.0, Books Online, C/C++ Book, Preprocessor Reference,
 * Chapter 1: The Preprocessor, Macros, Predefined Macros.
 * Alignment of 4 would work, but the MS library uses 8 bytes for
 * doubles and __int64, so we choose that.  The actual granularity of
 * VC malloc is 16!
 *
 * PellesC /Ze (Microsoft compatibility mode) defines _MSC_VER but
 * isn't compatible enough for MPS purposes.
 */

#if defined(_MSC_VER) && defined(_WIN32) && defined(_M_IX86) && !defined(__POCC__)
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
 * <https://docs.microsoft.com/en-us/previous-versions/visualstudio/visual-studio-2010/b0084kay(v=vs.100)>.
 * Note that Win32 includes 64-bit Windows!
 * We use the same alignment as MS malloc: 16, which is used for XMM
 * operations.
 * See MSDN -> x64 Software Conventions -> Overview of x64 Calling Conventions
 * <https://docs.microsoft.com/en-gb/cpp/build/overview-of-x64-calling-conventions>
 */

#elif defined(_MSC_VER) && defined(_WIN32) && defined(_WIN64) && defined(_M_X64) && !defined(__POCC__)
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


/* PellesC version 7.00.25 with /Ze option (Microsoft compatibility mode)
 * Help node "Predefined preprocessor symbols (POCC)"
 */

#elif defined(__POCC__) && defined(_WIN32) && defined(_M_IX86)
#if defined(CONFIG_PF_STRING) && ! defined(CONFIG_PF_W3I3PC)
#error "specified CONFIG_PF_... inconsistent with detected w3i3pc"
#endif
#define MPS_PF_W3I3PC
#define MPS_PF_STRING   "w3i3pc"
#define MPS_OS_W3
#define MPS_ARCH_I3
#define MPS_BUILD_PC
#define MPS_T_WORD      unsigned long
#define MPS_T_ULONGEST  unsigned long
#define MPS_WORD_WIDTH  32
#define MPS_WORD_SHIFT  5
#define MPS_PF_ALIGN    8


/* PellesC version 7.00.25 with /Ze option (Microsoft compatibility mode)
 * and /Tarm64-coff (Create a COFF object file for a X64 processor).
 * Help node "Predefined preprocessor symbols (POCC)"
 */

#elif defined(__POCC__) && defined(_WIN32) && defined(_WIN64) && defined(_M_X64)
#if defined(CONFIG_PF_STRING) && ! defined(CONFIG_PF_W3I6PC)
#error "specified CONFIG_PF_... inconsistent with detected w3i6pc"
#endif
#define MPS_PF_W3I6PC
#define MPS_PF_STRING   "w3i6pc"
#define MPS_OS_W3
#define MPS_ARCH_I6
#define MPS_BUILD_PC
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


/* gcc-mp-4.7 (MacPorts gcc47 4.7.4_5) 4.7.4
 * gcc -E -dM
 * Note that Clang also defines __GNUC__ since it's generally GCC compatible,
 * but that doesn't fit our system so we exclude Clang here.
 */

#elif defined(__APPLE__) && defined(__x86_64__) && defined(__MACH__) \
      && defined(__GNUC__) && !defined(__clang__)
#if defined(CONFIG_PF_STRING) && ! defined(CONFIG_PF_XCI6GC)
#error "specified CONFIG_PF_... inconsistent with detected xci6gc"
#endif
#define MPS_PF_XCI6GC
#define MPS_PF_STRING   "xci6gc"
#define MPS_OS_XC
#define MPS_ARCH_I6
#define MPS_BUILD_GC
#define MPS_T_WORD      unsigned long
#define MPS_T_ULONGEST  unsigned long
#define MPS_WORD_WIDTH  64
#define MPS_WORD_SHIFT  6
#define MPS_PF_ALIGN    8


/* Apple clang version 12.0, clang -E -dM */

#elif defined(__APPLE__) && defined(__arm64__) && defined(__MACH__) \
      && defined(__clang__)
#if defined(CONFIG_PF_STRING) && ! defined(CONFIG_PF_XCA6LL)
#error "specified CONFIG_PF_... inconsistent with detected xca6ll"
#endif
#define MPS_PF_XCA6LL
#define MPS_PF_STRING   "xca6ll"
#define MPS_OS_XC
#define MPS_ARCH_A6
#define MPS_BUILD_LL
#define MPS_T_WORD      unsigned long
#define MPS_T_ULONGEST  unsigned long
#define MPS_WORD_WIDTH  64
#define MPS_WORD_SHIFT  6
#define MPS_PF_ALIGN    8


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
 * Note that Clang also defines __GNUC__ since it's generally GCC compatible,
 * but that doesn't fit our system so we exclude Clang here.
 */

#elif defined(__linux__) && defined(__i386__) && defined(__GNUC__) \
      && !defined(__clang__)
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


/* GCC 4.6.3, gcc -E -dM */

#elif defined(__linux__) && defined(__x86_64) && defined(__GNUC__) \
      && !defined(__clang__)
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


/* Clang/LLVM 3.0, clang -E -dM */

#elif defined(__linux__) && defined(__x86_64) && defined(__GNUC__) \
      && defined(__clang__)
#if defined(CONFIG_PF_STRING) && ! defined(CONFIG_PF_LII6LL)
#error "specified CONFIG_PF_... inconsistent with detected lii6ll"
#endif
#define MPS_PF_LII6LL
#define MPS_PF_STRING   "lii6ll"
#define MPS_OS_LI
#define MPS_ARCH_I6
#define MPS_BUILD_LL
#define MPS_T_WORD      unsigned long
#define MPS_T_ULONGEST  unsigned long
#define MPS_WORD_WIDTH  64
#define MPS_WORD_SHIFT  6
#define MPS_PF_ALIGN    8


/* GCC 2.95.3, gcc -E -dM */

#elif defined(__FreeBSD__) && defined (__i386__) && defined (__GNUC__) \
      && !defined(__clang__)
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


#elif defined(__FreeBSD__) && defined (__i386__) && defined (__GNUC__) \
      && defined(__clang__)
#if defined(CONFIG_PF_STRING) && ! defined(CONFIG_PF_FRI3LL)
#error "specified CONFIG_PF_... inconsistent with detected fri3ll"
#endif
#define MPS_PF_FRI3LL
#define MPS_PF_STRING   "fri3ll"
#define MPS_OS_FR
#define MPS_ARCH_I3
#define MPS_BUILD_LL
#define MPS_T_WORD      unsigned long
#define MPS_T_ULONGEST  unsigned long
#define MPS_WORD_WIDTH  32
#define MPS_WORD_SHIFT  5
#define MPS_PF_ALIGN    4


#elif defined(__FreeBSD__) && defined (__x86_64__) && defined (__GNUC__) \
      && !defined(__clang__)
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


#elif defined(__FreeBSD__) && defined (__x86_64__) && defined (__GNUC__) \
      && defined(__clang__)
#if defined(CONFIG_PF_STRING) && ! defined(CONFIG_PF_FRI6LL)
#error "specified CONFIG_PF_... inconsistent with detected fri6ll"
#endif
#define MPS_PF_FRI6LL
#define MPS_PF_STRING   "fri6ll"
#define MPS_OS_FR
#define MPS_ARCH_I6
#define MPS_BUILD_LL
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
 * Copyright (C) 2001-2020 Ravenbrook Limited <https://www.ravenbrook.com/>.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the
 *    distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
 * IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 * TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
 * PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 * HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
