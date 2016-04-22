/* config.h: MPS CONFIGURATION
 *
 * $Id$
 * Copyright (c) 2001-2016 Ravenbrook Limited.  See end of file for license.
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


/* CONFIG_VAR_* -- variety Configuration
 *
 * These are translated into the directives CONFIG_ASSERT, CONFIG_STATS,
 * CONFIG_LOG, etc. which control actual compilation features.
 */

/* CONFIG_VAR_RASH -- the rash and reckless variety
 *
 * This variety switches off as many features as possible for maximum
 * performance, but is therefore unsafe and undebuggable.  It is not intended
 * for use, but for comparison with the hot variety, to check that assertion,
 * logging, etc. have negligible overhead.
 */

#if defined(CONFIG_VAR_RASH)
/* no asserts */
/* no statistic meters */
/* no telemetry log events */


/* CONFIG_VAR_COOL -- cool variety
 *
 * The cool variety is intended for use when developing an integration with
 * the MPS or debugging memory problems or collecting detailed telemetry
 * data for performance analysis.  It has more thorough consistency checking
 * and data collection and output, and full debugging information.
 */

#elif defined(CONFIG_VAR_COOL)
#define CONFIG_ASSERT
#define CONFIG_ASSERT_ALL
#define CONFIG_ASSERT_ABORT
#define CONFIG_STATS
#ifndef CHECKLEVEL
#define CHECKLEVEL      CheckLevelSHALLOW
#endif
#define CONFIG_LOG
#define CONFIG_LOG_ALL


#else /* CONFIG_VAR_* */

/* CONFIG_VAR_HOT -- the hot variety
 *
 * This variety is the default variety for distribution in products that use
 * the MPS.  It has maximum performance while retaining a good level of
 * consistency checking and allowing some debugging and telemetry features.
 */

/* #elif defined(CONFIG_VAR_HOT) */
#define CONFIG_ASSERT
/* Note, not CONFIG_ASSERT_ABORT */
#ifndef CHECKLEVEL
#define CHECKLEVEL      CheckLevelMINIMAL
#endif
/* no statistic meters */
#define CONFIG_LOG

#endif /* CONFIG_VAR_* */


/* Build Features */


#if defined(CONFIG_ASSERT)
/* asserts: AVER, AVERT, NOTREACHED, CHECKx */
/* note: a direct call to ASSERT() will *still* fire */
#define AVER_AND_CHECK
#if defined(CONFIG_ASSERT_ALL)
#define AVER_AND_CHECK_ALL
#define MPS_ASSERT_STRING "assertastic"
#else /* CONFIG_ASSERT_ALL, not */
#define MPS_ASSERT_STRING "asserted"
#endif /* CONFIG_ASSERT_ALL */
#else /* CONFIG_ASSERT, not */
#define AVER_AND_CHECK_NONE
#define MPS_ASSERT_STRING "nonasserted"
#endif
#if defined(CONFIG_ASSERT_ABORT)
#define ASSERT_ABORT() abort()
#else
#define ASSERT_ABORT() NOOP
#endif


#if defined(CONFIG_STATS)
/* CONFIG_STATS = STATISTICS = METERs */
#define STATISTICS
#define MPS_STATS_STRING "stats"
#else
#define STATISTICS_NONE
#define MPS_STATS_STRING "nonstats"
#endif


#if defined(CONFIG_LOG)
/* TELEMETRY = LOG = EVENTs */
#define EVENT
#if defined(CONFIG_LOG_ALL)
#define EVENT_ALL 1     /* log events on critical path */
#define MPS_LOG_STRING "logtastic"
#else /* CONFIG_LOG_ALL, not */
#define EVENT_ALL 0     /* don't log events on critical path */
#define MPS_LOG_STRING "logging"
#endif /* CONFIG_LOG_ALL */
#else /* CONFIG_LOG, not */
#define EVENT_NONE
#define MPS_LOG_STRING "nonlogging"
#endif /* CONFIG_LOG */


/* CONFIG_PLINTH_NONE -- exclude the ANSI plinth
 *
 * Some MPS deployment environments want to avoid dependencies on the
 * standard C library.  In this case, the plinth, defined in mpslib.h must
 * be supplied when linking.
 *
 * For example, Open Dylan on Windows does not link the C library, but
 * supplies its own plinth directly using Windows and Dylan interfaces.
 *
 * CONFIG_PLINTH_NONE tells mps.c to exclude the ANSI plinth and removes
 * all standard C library dependencies.  e.g.
 *
 *     cc -O2 -c -DCONFIG_PLINTH_NONE mps.c
 */

#if !defined(CONFIG_PLINTH_NONE)
#define PLINTH
#else
#define PLINTH_NONE
#endif


/* CONFIG_PF_ANSI -- use the ANSI platform 
 *
 * This symbol tells mps.c to exclude the sources for the
 * auto-detected platform, and use the generic ("ANSI") platform
 * instead.
 */

#if defined(CONFIG_PF_ANSI)
#define PLATFORM_ANSI
#endif


/* CONFIG_THREAD_SINGLE -- support single-threaded execution only
 *
 * This symbol causes the MPS to be built for single-threaded
 * execution only, where locks are not needed and so lock operations
 * can be defined as no-ops by lock.h.
 */

#if !defined(CONFIG_THREAD_SINGLE)
#define LOCK
#else
#define LOCK_NONE
#endif


/* CONFIG_POLL_NONE -- no support for polling
 *
 * This symbol causes the MPS to built without support for polling.
 * This means that garbage collections will only happen if requested
 * explicitly via mps_arena_collect() or mps_arena_step(), but it also
 * means that protection is not needed, and so shield operations can
 * be replaced with no-ops in mpm.h.
 */

#if !defined(CONFIG_POLL_NONE)
#define REMEMBERED_SET
#define SHIELD
#else
#if !defined(CONFIG_THREAD_SINGLE)
#error "CONFIG_POLL_NONE without CONFIG_THREAD_SINGLE"
#endif
#define REMEMBERED_SET_NONE
#define SHIELD_NONE
#endif


#define MPS_VARIETY_STRING \
  MPS_ASSERT_STRING "." MPS_LOG_STRING "." MPS_STATS_STRING


/* Platform Configuration */

#include "mpstd.h"

/* Suppress Visual C warnings at /W4 (warning level 4) */
/* This is also done in testlib.h. */

#ifdef MPS_BUILD_MV

/* "constant conditional" (provoked by MPS_END) */
#pragma warning(disable: 4127)

#endif /* MPS_BUILD_MV */


/* Suppress Pelles C warnings at /W2 (warning level 2) */
/* Some of the same settings are done in testlib.h. */

#ifdef MPS_BUILD_PC

/* "Unreachable code" (provoked by AVER, if condition is constantly true). */
#pragma warn(disable: 2154)

/* "Consider changing type to 'size_t' for loop variable" */
#pragma warn(disable: 2804)

#endif /* MPS_BUILD_PC */


/* MPS_FILE -- expands to __FILE__ in nested macros */

#ifdef MPS_BUILD_PC

/* Pelles C loses definition of __FILE__ in deeply nested macro
 * expansions. See <http://forum.pellesc.de/index.php?topic=5474.0>
 */
#define MPS_FILE "<__FILE__ unavailable in " MPS_PF_STRING ">"

#else

#define MPS_FILE __FILE__

#endif


/* Function attributes */
/* Some of these are also defined in testlib.h */

/* Attribute for functions that take a printf-like format argument, so
 * that the compiler can check the format specifiers against the types
 * of the arguments.
 * GCC: <http://gcc.gnu.org/onlinedocs/gcc/Function-Attributes.html#index-Wformat-2850>
 * Clang: <http://clang.llvm.org/docs/AttributeReference.html#format-gnu-format>
 */
#if defined(MPS_BUILD_GC) || defined(MPS_BUILD_LL)
#define ATTRIBUTE_FORMAT(ARGLIST) __attribute__((__format__ ARGLIST))
#else
#define ATTRIBUTE_FORMAT(ARGLIST)
#endif

/* Attribute for functions that should not be instrumented by Clang's
 * address sanitizer.
 * <http://clang.llvm.org/docs/AddressSanitizer.html#attribute-no-sanitize-address>
 */
#if defined(MPS_BUILD_LL)
#if __has_feature(address_sanitizer)
#define ATTRIBUTE_NO_SANITIZE_ADDRESS __attribute__((__no_sanitize_address__))
#else
#define ATTRIBUTE_NO_SANITIZE_ADDRESS
#endif
#else
#define ATTRIBUTE_NO_SANITIZE_ADDRESS
#endif

/* Attribute for functions that do not return.
 * GCC: <http://gcc.gnu.org/onlinedocs/gcc/Function-Attributes.html>
 * Clang: <http://clang.llvm.org/docs/AttributeReference.html#id1>
 */
#if defined(MPS_BUILD_GC) || defined(MPS_BUILD_LL)
#define ATTRIBUTE_NORETURN __attribute__((__noreturn__))
#else
#define ATTRIBUTE_NORETURN
#endif

/* Attribute for functions that may be unused in some build configurations.
 * GCC: <http://gcc.gnu.org/onlinedocs/gcc/Function-Attributes.html>
 *
 * This attribute must be applied to all Check functions, otherwise
 * the RASH variety fails to compile with -Wunused-function. (It
 * should not be applied to functions that are unused in all build
 * configurations: these functions should not be compiled.)
 */
#if defined(MPS_BUILD_GC) || defined(MPS_BUILD_LL)
#define ATTRIBUTE_UNUSED __attribute__((__unused__))
#else
#define ATTRIBUTE_UNUSED
#endif


/* Compiler extensions */

/* LIKELY -- likely conditions
 *
 * Use to annotate conditions that are likely to be true, such as
 * assertions, to help move unlikely code out-of-line.  See
 * <https://gcc.gnu.org/onlinedocs/gcc/Other-Builtins.html>.
 */

#if defined(MPS_BUILD_GC) || defined(MPS_BUILD_LL)
#define LIKELY(exp) __builtin_expect((exp) != 0, 1)
#else
#define LIKELY(exp) ((exp) != 0)
#endif


/* EPVMDefaultSubsequentSegSIZE is a default for the alignment of
 * subsequent segments (non-initial at each save level) in EPVM.  See
 * design.mps.poolepvm.arch.segment.size.
 */

#define EPVMDefaultSubsequentSegSIZE ((Size)64 * 1024)


/* Buffer Configuration -- see <code/buffer.c> */

#define BUFFER_RANK_DEFAULT (mps_rank_exact())


/* Format defaults: see <code/format.c> */

#define FMT_ALIGN_DEFAULT ((Align)MPS_PF_ALIGN)
#define FMT_HEADER_SIZE_DEFAULT ((Size)0)
#define FMT_SCAN_DEFAULT (&FormatNoScan)
#define FMT_SKIP_DEFAULT (&FormatNoSkip)
#define FMT_FWD_DEFAULT (&FormatNoMove)
#define FMT_ISFWD_DEFAULT (&FormatNoIsMoved)
#define FMT_PAD_DEFAULT (&FormatNoPad)
#define FMT_CLASS_DEFAULT (&FormatDefaultClass)


/* Pool AMC Configuration -- see <code/poolamc.c> */

#define AMC_INTERIOR_DEFAULT TRUE
/* AMC treats objects larger than or equal to this as "Large" */
#define AMC_LARGE_SIZE_DEFAULT ((Size)32768)
#define AMC_EXTEND_BY_DEFAULT  ((Size)8192)


/* Pool AMS Configuration -- see <code/poolams.c> */

#define AMS_SUPPORT_AMBIGUOUS_DEFAULT TRUE
#define AMS_GEN_DEFAULT       0


/* Pool AWL Configuration -- see <code/poolawl.c> */

#define AWL_GEN_DEFAULT       0
#define AWL_HAVE_SEG_SA_LIMIT   TRUE
#define AWL_SEG_SA_LIMIT        200     /* TODO: Improve guesswork with measurements */
#define AWL_HAVE_TOTAL_SA_LIMIT FALSE
#define AWL_TOTAL_SA_LIMIT      0


/* Pool LO Configuration -- see <code/poollo.c> */

#define LO_GEN_DEFAULT       0


/* Pool MV Configuration -- see <code/poolmv.c> */

#define MV_ALIGN_DEFAULT      MPS_PF_ALIGN
#define MV_EXTEND_BY_DEFAULT  ((Size)65536)
#define MV_AVG_SIZE_DEFAULT   ((Size)32)
#define MV_MAX_SIZE_DEFAULT   ((Size)65536)


/* Pool MFS Configuration -- see <code/poolmfs.c> */

#define MFS_EXTEND_BY_DEFAULT ((Size)65536)


/* Pool MVFF Configuration -- see <code/poolmvff.c> */

#define MVFF_EXTEND_BY_DEFAULT   ((Size)65536)
#define MVFF_AVG_SIZE_DEFAULT    ((Size)32)
#define MVFF_ALIGN_DEFAULT       MPS_PF_ALIGN
#define MVFF_SLOT_HIGH_DEFAULT   FALSE
#define MVFF_ARENA_HIGH_DEFAULT  FALSE
#define MVFF_FIRST_FIT_DEFAULT   TRUE
#define MVFF_SPARE_DEFAULT       0.75


/* Pool MVT Configuration -- see <code/poolmv2.c> */
/* FIXME: These numbers were lifted from mv2test and need thought. */

#define MVT_ALIGN_DEFAULT         MPS_PF_ALIGN
#define MVT_MIN_SIZE_DEFAULT      MPS_PF_ALIGN
#define MVT_MEAN_SIZE_DEFAULT     32
#define MVT_MAX_SIZE_DEFAULT      8192
#define MVT_RESERVE_DEPTH_DEFAULT 1024
#define MVT_FRAG_LIMIT_DEFAULT    30


/* Arena Configuration -- see <code/arena.c> */

#define ArenaPollALLOCTIME (65536.0)

/* .client.seg-size: ARENA_CLIENT_GRAIN_SIZE is the minimum size, in
 * bytes, of a grain in the client arena. It's set at 8192 with no
 * particular justification. */

#define ARENA_CLIENT_GRAIN_SIZE          ((Size)8192)

#define ARENA_DEFAULT_COMMIT_LIMIT ((Size)-1)

/* TODO: This should be proportional to the memory usage of the MPS, not
 * a constant.  That will require design, and then some interface and
 * documentation changes. */
#define ARENA_DEFAULT_SPARE_COMMIT_LIMIT   ((Size)10uL*1024uL*1024uL)

/* ARENA_DEFAULT_PAUSE_TIME is the maximum time (in seconds) that
 * operations within the arena may pause the mutator for.  The default
 * is set for typical human interaction.  See mps_arena_pause_time_set
 * in the manual. */

#define ARENA_DEFAULT_PAUSE_TIME (0.1)

#define ARENA_DEFAULT_ZONED     TRUE

/* ARENA_MINIMUM_COLLECTABLE_SIZE is the minimum size (in bytes) of
 * collectable memory that might be considered worthwhile to run a
 * full garbage collection. */

#define ARENA_MINIMUM_COLLECTABLE_SIZE ((Size)1000000)

/* ARENA_DEFAULT_COLLECTION_RATE is an estimate of the MPS's
 * collection rate (in work per second; see <design/type/#work>), for
 * use in the case where there isn't enough data to use a measured
 * value. */

#define ARENA_DEFAULT_COLLECTION_RATE (25000000.0)

/* ARENA_DEFAULT_COLLECTION_OVERHEAD is an estimate of the MPS's
 * collection overhead (in seconds), for use in the case where there
 * isn't enough data to use a measured value. */

#define ARENA_DEFAULT_COLLECTION_OVERHEAD (0.1)

/* ARENA_MAX_COLLECT_FRACTION is the maximum fraction of runtime that
 * ArenaStep is prepared to spend in collections. */

#define ARENA_MAX_COLLECT_FRACTION (0.1)

/* ArenaDefaultZONESET is the zone set used by LocusPrefDEFAULT.
 *
 * TODO: This is left over from before branches 2014-01-29/mps-chain-zones
 * and 2014-01-17/cbs-tract-alloc reformed allocation, and may now be
 * doing more harm than good. Experiment with setting to ZoneSetUNIV. */

#define ArenaDefaultZONESET (ZoneSetUNIV << (MPS_WORD_WIDTH / 2))

/* LocusPrefDEFAULT is the allocation preference used by manual pool
 * classes (these don't care where they allocate). */

#define LocusPrefDEFAULT { \
  LocusPrefSig,        /* sig */ \
  FALSE,               /* high */ \
  ArenaDefaultZONESET, /* zoneSet */ \
  ZoneSetEMPTY,        /* avoid */ \
}

#define LDHistoryLENGTH ((Size)4)

/* Value of MPS_KEY_EXTEND_BY for the arena control pool. */
#define CONTROL_EXTEND_BY ((Size)32768)

#define VM_ARENA_SIZE_DEFAULT ((Size)1 << 28)


/* Stack configuration -- see <code/sp*.c> */

/* Currently StackProbe has a useful implementation only on Windows. */
#if defined(MPS_OS_W3)
/* See <design/sp/#sol.depth.analysis> for a justification of this value. */
#define StackProbeDEPTH ((Size)500)
#else
#define StackProbeDEPTH ((Size)0)
#endif


/* Shield Configuration -- see <code/shield.c> */

#define ShieldQueueLENGTH  512  /* initial length of shield queue */
#define ShieldDepthWIDTH     4  /* log2(max nested exposes + 1) */


/* VM Configuration -- see <code/vm*.c> */

#define VMAN_PAGE_SIZE ((Align)4096)
#define VMJunkBYTE ((unsigned char)0xA9)
#define VMParamSize (sizeof(Word))


/* .feature.li: Linux feature specification
 *
 * The MPS needs the following symbols which are not defined if the
 * -ansi option is given to GCC:
 *
 * Source      Symbols                   Header        Feature
 * =========== ========================= ============= ====================
 * eventtxt.c  setenv                    <stdlib.h>    _GNU_SOURCE
 * lockli.c    pthread_mutexattr_settype <pthread.h>   _XOPEN_SOURCE >= 500
 * prmci3li.c  REG_EAX etc.              <ucontext.h>  _GNU_SOURCE
 * prmci6li.c  REG_RAX etc.              <ucontext.h>  _GNU_SOURCE
 * prmcix.h    stack_t, siginfo_t        <signal.h>    _XOPEN_SOURCE
 * pthrdext.c  sigaction etc.            <signal.h>    _XOPEN_SOURCE
 * vmix.c      MAP_ANON                  <sys/mman.h>  _GNU_SOURCE
 *
 * It is not possible to localize these feature specifications around
 * the individual headers: all headers share a common set of features
 * (via <feature.h>) and so all sources in the same compilation unit
 * must turn on the same set of features.
 *
 * See "Feature Test Macros" in the Glibc Manual:
 * <http://www.gnu.org/software/libc/manual/html_node/Feature-Test-Macros.html>
 */

#if defined(MPS_OS_LI)

#if defined(_XOPEN_SOURCE) && _XOPEN_SOURCE < 500
#undef _XOPEN_SOURCE
#endif
#if !defined(_XOPEN_SOURCE)
#define _XOPEN_SOURCE 500
#endif

#if !defined(_GNU_SOURCE)
#define _GNU_SOURCE
#endif

#endif


/* .feature.xc: OS X feature specification
 *
 * The MPS needs the following symbols which are not defined by default
 *
 * Source      Symbols                   Header        Feature
 * =========== ========================= ============= ====================
 * prmci3li.c  __eax etc.                <ucontext.h>  _XOPEN_SOURCE
 * prmci6li.c  __rax etc.                <ucontext.h>  _XOPEN_SOURCE
 *
 * It is not possible to localize these feature specifications around
 * the individual headers: all headers share a common set of features
 * (via <sys/cdefs.h>) and so all sources in the same compilation unit
 * must turn on the same set of features.
 */

#if defined(MPS_OS_XC)

#if !defined(_XOPEN_SOURCE)
#define _XOPEN_SOURCE
#endif

#endif


/* Protection Configuration see <code/prot*.c>

   For each architecture/OS that uses protix.c or protsgix.c, we need to
   define what signal number to use, and what si_code value to check.
*/

#if defined(MPS_OS_FR)
#define PROT_SIGNAL (SIGSEGV)
#endif

#if defined(MPS_OS_FR)
#define PROT_SIGINFO_GOOD(info) ((info)->si_code == SEGV_ACCERR)
#endif


/* Almost all of protxc.c etc. are architecture-independent, but unfortunately
   the Mach headers don't provide architecture neutral symbols for simple
   things like thread states.  These definitions fix that. */

#if defined(MPS_OS_XC)
#if defined(MPS_ARCH_I6)

#define THREAD_STATE_COUNT x86_THREAD_STATE64_COUNT
#define THREAD_STATE_FLAVOR x86_THREAD_STATE64
#define THREAD_STATE_S x86_thread_state64_t

#elif defined(MPS_ARCH_I3)

#define THREAD_STATE_COUNT x86_THREAD_STATE32_COUNT
#define THREAD_STATE_FLAVOR x86_THREAD_STATE32
#define THREAD_STATE_S x86_thread_state32_t

#else

#error "Unknown OS X architecture"

#endif
#endif




/* Tracer Configuration -- see <code/trace.c> */

#define TraceLIMIT ((size_t)1)
/* I count 4 function calls to scan, 10 to copy. */
#define TraceCopyScanRATIO (1.5)

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
#define mps_lib_memset(p, c, l) memset(p, c, l)
#define mps_lib_memcpy(p, q, s) memcpy(p, q, s)
#define mps_lib_memcmp(p, q, s) memcmp(p, q, s)
/* get prototypes for ANSI mem* */
#include <string.h>
#endif


/* Product Configuration
 *
 * Deprecated, see design/config/#req.prod>.  This now only contains the
 * configuration used by the former "MPS" product, which is now the only
 * product.
 */

#define MPS_PROD_STRING         "mps"
#define MPS_PROD_MPS


/* Default chain for GC pools
 *
 * TODO: The default should be to measure liveness and make sensible
 * decisions. See job003794.
 */

#define ChainDEFAULT \
  { \
    {  8 * 1024, 0.85 }, /* nursery */ \
    { 36 * 1024, 0.45 }  /* second gen, after which dynamic */ \
  }


/* Write barrier deferral
 *
 * See design.mps.write-barrier.deferral.
 *
 * TODO: These settings were determined by trial and error, but should
 * be based on measurement of the protection overhead on each
 * platform.  We know it's extremely different between OS X and
 * Windows, for example.  See design.mps.write-barrier.improv.by-os.
 *
 * TODO: Consider basing the count on the amount of time that has
 * passed in the mutator rather than the number of scans.
 */

#define WB_DEFER_BITS  2  /* bitfield width for deferral count */
#define WB_DEFER_INIT  3  /* boring scans after new segment */
#define WB_DEFER_DELAY 3  /* boring scans after interesting scan */
#define WB_DEFER_HIT   1  /* boring scans after barrier hit */


#endif /* config_h */


/* C. COPYRIGHT AND LICENSE
 *
 * Copyright (C) 2001-2016 Ravenbrook Limited <http://www.ravenbrook.com/>.
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
