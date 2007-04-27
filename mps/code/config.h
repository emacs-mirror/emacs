/* config.h: MPS CONFIGURATION
 *
 * $Id$
 * Copyright (c) 2001-2003, 2006 Ravenbrook Limited.  See end of file for license.
 * Portions copyright (c) 2002 Global Graphics Software.
 *
 * PURPOSE
 *
 * This module translates from high-level symbols defined by the
 * external build system (gnumake, nmake, etc.) into specific sets
 * of features used by MPS modules.
 *
 * DESIGN
 *
 * See <design/config/>.
 */

#ifndef config_h
#define config_h


/* Variety Configuration */

/* First deal with old-style CONFIG_VAR_* build directives.  These
 * must be translated into the new directives CONFIG_ASSERT,
 * CONFIG_DEBUG, and CONFIG_LOG.
 *
 * One day the old build system may be converted to use the new
 * directives.
 */

#if defined(CONFIG_VAR_WI) || defined(CONFIG_VAR_WE) /* White-hot varieties */
/* no asserts */
/* ... so CHECKLEVEL_INITIAL is irrelevant */
/* no debug diagnostic statistic meters */
/* no telemetry log events */

#elif defined(CONFIG_VAR_HI) || defined(CONFIG_VAR_HE) /* Hot varieties */
#define CONFIG_ASSERT
#define CHECKLEVEL_INITIAL CheckLevelMINIMAL
/* no debug diagnostic statistic meters */
/* no telemetry log events */

#elif defined(CONFIG_VAR_CI) || defined(CONFIG_VAR_CE) /* Cool varieties */
#define CONFIG_ASSERT
/* ... let PRODUCT determine CHECKLEVEL_INITIAL */
#define CONFIG_DEBUG
/* no telemetry log events */

#elif defined(CONFIG_VAR_TI)    /* Telemetry, Internal; variety.ti */
#define CONFIG_ASSERT
/* ... let PRODUCT determine CHECKLEVEL_INITIAL */
#define CONFIG_DEBUG
#define CONFIG_LOG

#elif defined(CONFIG_VAR_II)    /* Ice, Internal; variety.ii (HotLog) */
#define CONFIG_ASSERT
#define CHECKLEVEL_INITIAL CheckLevelMINIMAL
/* no debug diagnostic statistic meters */
#define CONFIG_LOG
#endif


/* Build Features */


#if defined(CONFIG_ASSERT)
/* asserts: AVER, AVERT, NOTREACHED, CHECKx */
/* note: a direct call to ASSERT() will *still* fire */
#define AVER_AND_CHECK
#define MPS_ASSERT_STRING "asserted"
#else
#define AVER_AND_CHECK_NONE
#define MPS_ASSERT_STRING "nonasserted"
#endif


#if defined(CONFIG_DEBUG)
/* DEBUG = DIAGNOSTICS = STATISTICs = METERs */
/* WARNING: this changes the size and fields of MPS structs */
#define DIAGNOSTICS
#define MPS_DEBUG_STRING "debug"
#else
#define DIAGNOSTICS_NONE
#define MPS_DEBUG_STRING "nondebug"
#endif


#if defined(CONFIG_LOG)
/* TELEMETRY = LOG = EVENTs */
#define EVENT
#define MPS_LOG_STRING "logging"
#else
#define EVENT_NONE
#define MPS_LOG_STRING "nonlogging"
#endif


#define MPS_VARIETY_STRING \
  MPS_ASSERT_STRING "." MPS_LOG_STRING "." MPS_DEBUG_STRING


/* Platform Configuration */

#include "mpstd.h"

/* Suppress Visual C warnings at warning level 4, */
/* see mail.richard.1997-09-25.13-26. */
/* Essentially the same settings are done in testlib.h. */

#ifdef MPS_BUILD_MV

/* "unreferenced inline function has been removed" (windows.h) */
#pragma warning(disable: 4514)

/* "constant conditional" (MPS_END) */
#pragma warning(disable: 4127)

/* "unreachable code" (ASSERT, if cond is constantly true). */
#pragma warning(disable: 4702)

/* "expression evaluates to a function which is missing an argument list" */
#pragma warning(disable: 4550)

/* "local variable is initialized but not referenced" */
#pragma warning(disable: 4189)

/* "not all control paths return a value" */
#pragma warning(disable: 4715)

/* MSVC 2.0 generates a warning when using NOCHECK or UNUSED */
#ifdef _MSC_VER
#if _MSC_VER < 1000
#pragma warning(disable: 4705)
#endif
#else /* _MSC_VER */
#error "Expected _MSC_VER to be defined for builder.mv"
#endif /* _MSC_VER */

/* MSVC 10.00 on PowerPC generates erroneous warnings about */
/* uninitialized local variables, if you take their address. */
#ifdef MPS_ARCH_PP
#pragma warning(disable: 4701)
#endif /* MPS_ARCH_PP */


/* Non-checking varieties give many spurious warnings because parameters
 * are suddenly unused, etc.  We aren't interested in these
 */

#if defined(AVER_AND_CHECK_NONE)

/* "unreferenced formal parameter" */
#pragma warning(disable: 4100)

/* "unreferenced local function has been removed" */
#pragma warning(disable: 4505)

#endif /* AVER_AND_CHECK_NONE */

#endif /* MPS_BUILD_MV */


/* EPVMDefaultSubsequentSegSIZE is a default for the alignment of
 * subsequent segments (non-initial at each save level) in EPVM.  See
 * design.mps.poolepvm.arch.segment.size.
 */

#define EPVMDefaultSubsequentSegSIZE (64ul * 1024)


/* Arena Configuration -- see <code/arena.c>
 *
 * .client.seg-size: ARENA_CLIENT_PAGE_SIZE is the size in bytes of a
 * "page" (i.e., segment granule) in the client arena.  It's set at 8192
 * with no particular justification.
 */

#define ARENA_CONTROL_EXTENDBY  ((Size)4096)
#define ARENA_CONTROL_AVGSIZE   ((Size)32)
#define ARENA_CONTROL_MAXSIZE   ((Size)65536)

#define ArenaPollALLOCTIME (65536.0)

#define ARENA_ZONESHIFT         ((Shift)20)

#define ARENA_CLIENT_PAGE_SIZE          ((Size)8192)

#define ArenaDefaultZONESET (ZoneSetUNIV << (MPS_WORD_WIDTH / 2))
/* @@@@ knows the implementation of ZoneSets */

/* .segpref.default: For EPcore, non-DL segments should be placed high */
/* to reduce fragmentation of DL pools (see request.epcore.170193). */
#define SegPrefDEFAULT { \
  SegPrefSig,          /* sig */ \
  TRUE,                /* high */ \
  ArenaDefaultZONESET, /* zoneSet */ \
  FALSE,               /* isCollected */ \
  FALSE,               /* isGen */ \
  (Serial)0,           /* gen */ \
}

#define LDHistoryLENGTH ((Size)4)


/* Stack configuration */

/* Currently StackProbe has a useful implementation only on
 * Intel platforms and only when using Microsoft build tools (builder.mv)
 */
#if defined(MPS_ARCH_I3) && defined(MPS_BUILD_MV)
#define StackProbeDEPTH ((Size)500)
#else
#define StackProbeDEPTH ((Size)0)
#endif /* MPS_ARCH_I3 */


/* Shield Configuration -- see <code/shield.c> */

#define ShieldCacheSIZE ((size_t)16)
#define ShieldDepthWIDTH (4)


/* VM Configuration -- see <code/vm*.c> */

#define VMANPageALIGNMENT ((Align)4096)
#define VMJunkBYTE ((unsigned char)0xA9)

/* Protection Configuration see <code/prot*.c>

   For each architecture/OS that uses protix.c or protsgix.c, we need to
   define what signal number to use, and what si_code value to check.
*/

#if defined(MPS_OS_O1) || defined(MPS_OS_SO)
#define PROT_SIGNAL (SIGSEGV)
#elif defined(MPS_OS_FR) || defined(MPS_OS_XC)
#define PROT_SIGNAL (SIGBUS)
#endif

#if defined(MPS_OS_XC)
#define PROT_SIGINFO_GOOD(info) (1)
#elif defined(MPS_OS_O1)
#define PROT_SIGINFO_GOOD(info) ((info)->si_code == SEGV_ACCERR)
#elif defined(MPS_OS_FR)
#define PROT_SIGINFO_GOOD(info) ((info)->si_code == BUS_PAGE_FAULT)
#endif


/* Tracer Configuration -- see <code/trace.c> */

#define TraceLIMIT ((size_t)1)
/* I count 4 function calls to scan, 10 to copy. */
#define TraceCopyScanRATIO (1.5)
/* Length (in chars) of a char buffer used to store the reason why a
   collection started in the TraceStartMessageStruct (used by
   mps_message_type_gc_start). */
#define TRACE_START_MESSAGE_WHY_LEN 128

/* Chosen so that the RememberedSummaryBlockStruct packs nicely into
   pages */
#define RememberedSummaryBLOCK 15



/* Events
 *
 * EventBufferSIZE is the number of words in the global event buffer.
 */

#define EventBufferSIZE ((size_t)4096)
#define EventStringLengthMAX ((size_t)255) /* Not including NUL */


/* Assert Buffer */

#define ASSERT_BUFFER_SIZE      ((Size)512)


/* memory operator configuration
 *
 * We need efficient operators similar to memcpy, memset, and memcmp.
 * In general, we cannot use the C library mem functions directly as
 * that would not be freestanding.  However, on some platforms we can do
 * this, because they are inlined by the compiler and so do not actually
 * create a dependence on an external library.
 */

#if defined(MPS_PF_W3I3MV)
/* MSVC on Intel inlines mem* when optimizing */
#define mps_lib_memset memset
#define mps_lib_memcpy memcpy
#define mps_lib_memcmp memcmp
/* get prototypes for ANSI mem* */
#include <string.h>
#endif


/* Product Configuration
 *
 * Convert CONFIG_PROD_* defined on compiler command line into
 * internal configuration parameters.  See <design/config/#prod>.
 */

#if defined(CONFIG_PROD_EPCORE)
#define MPS_PROD_STRING         "epcore"
#define MPS_PROD_EPCORE
#define ARENA_INIT_SPARE_COMMIT_LIMIT   ((Size)0)
/* .nosync.why: ScriptWorks is single-threaded when using the MM. */
#define THREAD_SINGLE
#define PROTECTION_NONE
#define DONGLE_NONE
#define PROD_CHECKLEVEL_INITIAL CheckLevelMINIMAL /* CheckLevelSHALLOW is too slow for SW */

#elif defined(CONFIG_PROD_DYLAN)
#define MPS_PROD_STRING         "dylan"
#define MPS_PROD_DYLAN
#define ARENA_INIT_SPARE_COMMIT_LIMIT   ((Size)10uL*1024uL*1024uL)
#define THREAD_MULTI
#define PROTECTION
#define DONGLE_NONE
#define PROD_CHECKLEVEL_INITIAL CheckLevelSHALLOW

#elif defined(CONFIG_PROD_MPS)
#define MPS_PROD_STRING         "mps"
#define MPS_PROD_MPS
#define ARENA_INIT_SPARE_COMMIT_LIMIT   ((Size)10uL*1024uL*1024uL)
#define THREAD_MULTI
#define PROTECTION
#define DONGLE_NONE
#define PROD_CHECKLEVEL_INITIAL CheckLevelSHALLOW

#else
#error "No target product configured."
#endif

/* .prod.arena-size: ARENA_SIZE is currently set larger for the
 * MM/Dylan product as an interim solution.
 * See request.dylan.170170.sol.patch and change.dylan.buffalo.170170.
 * Note that this define is only used by the implementation of the
 * deprecated mps_space_create interface.
 */
#define ARENA_SIZE              ((Size)1<<30)

/* if CHECKLEVEL_INITIAL hasn't been defined already (e.g. by a variety, or
 * in a makefile), take the value from the product. */

#ifndef CHECKLEVEL_INITIAL
#define CHECKLEVEL_INITIAL PROD_CHECKLEVEL_INITIAL
#endif


/* Dongle configuration */

#if defined(DONGLE)

#define DONGLE_TEST_FREQUENCY ((unsigned int)4000)

#elif defined(DONGLE_NONE)

/* nothing to do */

#else
#error "No dongle configured."
#endif


#endif /* config_h */


/* C. COPYRIGHT AND LICENSE
 *
 * Copyright (C) 2001-2003, 2006 Ravenbrook Limited <http://www.ravenbrook.com/>.
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
