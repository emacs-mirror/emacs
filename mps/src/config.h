/* impl.h.config: MPS CONFIGURATION
 *
 * $HopeName: !config.h(trunk.40) $
 * Copyright (C) 2000 Harlequin Limited.  All rights reserved.
 *
 * PURPOSE
 *
 * This module translates from high-level symbols defined by the
 * external build system (gnumake, nmake, etc.) into specific sets
 * of features used by MPS modules.  For example, the build system
 * will defined one of the CONFIG_VAR_* symbols to indicate which
 * variety it is building, this file translates that into a certain
 * level of checking, and a certain level of telemetry.
 *
 * DESIGN
 *
 * See design.mps.config.
 */

#ifndef config_h
#define config_h


/* Variety Configuration
 *
 * Convert CONFIG_VAR_* defined on compiler command line into
 * internal configuration parameters.  See design.mps.config.var
 * and design.mps.variety.macro.  Note that MPS_HOT is subclassed
 * into MPS_HOT_RED and MPS_HOT_WHITE; this distinction should
 * be rarely used.
 */

#if defined(CONFIG_VAR_HI)      /* Hot, Internal; variety.hi */
#define MPS_VARIETY_STRING      "hi"
#define MPS_HOT
#define MPS_HOT_RED
#define EVENT_NONE
#elif defined(CONFIG_VAR_CI)    /* Cool, Internal; variety.ci */
#define MPS_VARIETY_STRING      "ci"
#define MPS_COOL
#define EVENT_NONE
#elif defined(CONFIG_VAR_TI)    /* Telemetry, Internal; variety.ti */
#define MPS_VARIETY_STRING      "ti"
#define MPS_COOL
#define EVENT
#elif defined(CONFIG_VAR_HE)    /* Hot, External; variety.he */
#define MPS_VARIETY_STRING      "he"
#define MPS_HOT
#define MPS_HOT_RED
#define EVENT_NONE
#elif defined(CONFIG_VAR_CE)    /* Cool, External; variety.ce */
#define MPS_VARIETY_STRING      "ce"
#define MPS_COOL
#define EVENT_NONE
#elif defined(CONFIG_VAR_WI)    /* White hot, Internal; variety.wi */
#define MPS_VARIETY_STRING      "wi"
#define MPS_HOT
#define MPS_HOT_WHITE
#define EVENT_NONE
#elif defined(CONFIG_VAR_WE)    /* White hot, External; variety.we */
#define MPS_VARIETY_STRING      "we"
#define MPS_HOT
#define MPS_HOT_WHITE
#define EVENT_NONE
#elif defined(CONFIG_VAR_II)    /* Ice, Internal; variety.ii */
#define MPS_VARIETY_STRING      "ii"
#define MPS_HOT
#define MPS_HOT_RED
#define EVENT
#else
#error "No target variety configured."
#endif


#if defined(EVENT)
#define DIAGNOSTICS
#elif defined(EVENT_NONE)
#define DIAGNOSTICS_NONE
#else
#error "Events not configured."
#endif


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


/* In white-hot versions, absolutely no checking is done.  This leads to
 * many spurious warnings because parameters are suddenly unused, etc.
 * We aren't interested in these.
 */

#if defined(MPS_HOT_WHITE)

/* "unreferenced formal parameter" */
#pragma warning(disable: 4100)

/* "unreferenced local function has been removed" */
#pragma warning(disable: 4505)

#endif /* MPS_HOT_WHITE */

#endif /* MPS_BUILD_MV */


/* .epvm.subsequent-segment: POOLEPVM_DEFAULT_SUBSEQUENT_SEG_SIZE is
 * a default for the alignment of subsequent segments (non-initial
 * at each save level) in EPVM.  See
 * design.mps.poolepvm.arch.segment.size.
 */

#define POOLEPVM_DEFAULT_SUBSEQUENT_SEG_SIZE (64ul * 1024)


/* Arena Configuration -- see impl.c.arena
 *
 * .client.seg-size: ARENA_CLIENT_PAGE_SIZE is the size in bytes of a
 * "page" (i.e., segment granule) in the client arena.  It's set at 8192
 * with no particular justification.
 *
 * .segpref.default: ARENA_DEFAULT_SEG_HIGH is a Bool governing whether
 * segments default 'high' (TRUE) or 'low' (FALSE).  For EPcore, non-DL
 * segments should be high to reduce fragmentation of DL pools (see
 * request.epcore.170193).  ARENA_DEFAULT_REFSET has the same role for
 * refset-based placement; again, for EPcore, we reserve half the arena
 * for non-DL.
 */

#define ARENA_CONTROL_EXTENDBY  ((Size)4096)
#define ARENA_CONTROL_AVGSIZE   ((Size)32)
#define ARENA_CONTROL_MAXSIZE   ((Size)65536)
#define ARENA_POLL_MAX          (65536.0)
#define ARENA_LD_LENGTH         ((Size)4)
#define ARENA_ZONESHIFT         ((Shift)20)

#define ARENA_CLIENT_PAGE_SIZE          ((Size)8192)
#define ARENA_DEFAULT_SEG_HIGH          TRUE
#define ARENA_DEFAULT_REFSET            BS_UPPER_HALF(RefSet)


/* Stack configuration */

/* Currently StackProbe has a useful implementation only on
 * Intel platforms and only when using Microsoft build tools
 * (builder.mv) */
#if defined(MPS_ARCH_I3) && defined(MPS_BUILD_MV)
#define STACK_PROBE_DEPTH       ((Word)500)
#else
#define STACK_PROBE_DEPTH       ((Word)0)
#endif /* MPS_ARCH_I3 */


/* Shield Configuration -- see impl.c.shield */

#define SHIELD_CACHE_SIZE       ((Size)16)
#define SHIELD_DEPTH_WIDTH      ((Size)4)
#define SHIELD_DEPTH            ((Count)1<<SHIELD_DEPTH_WIDTH)


/* VM Configuration -- see impl.c.vm* */

#define VMAN_ALIGN              ((Align)4096)
#define VM_JUNKBYTE             ((unsigned char)0xA9)


/* Tracer Configuration -- see impl.c.trace */

#define TRACE_MAX               ((Size)1)


/* Events
 *
 *  EVENT_BUFFER_SIZE is the number of words in the global event buffer.
 */

#define EVENT_BUFFER_SIZE ((size_t)4096)
#define EventStringLengthMAX ((size_t)255) /* Not including NUL */


/* Assert Buffer
 *
 * The Assert Buffer lives in assert.c */

#define ASSERT_BUFFER_SIZE      ((Size)512)


/* memory operator configuration
 *
 * We need efficient operators similar to memcpy, memset, and memcmp.
 * In general, we cannot use the C library mem functions directly as
 * that would not be freestanding.  However, on some platforms we can do
 * this, because they are inlined by the compiler and so do not actually
 * create a dependence on an external library.
 */

#if defined(MPS_PF_W3I3MV) && defined(MPS_HOT)
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
 * internal configuration parameters.  See design.mps.config.prod.
 */

#if defined(CONFIG_PROD_EPCORE)
#define MPS_PROD_STRING         "epcore"
#define MPS_PROD_EPCORE
#define ARENA_INIT_SPARE_COMMIT_LIMIT   ((Size)0)
/* .nosync.why: ScriptWorks is single-threaded when using the MM. */
#define THREAD_SINGLE
#define PROTECTION_NONE
#define DONGLE_NONE

#elif defined(CONFIG_PROD_DYLAN)
#define MPS_PROD_STRING         "dylan"
#define MPS_PROD_DYLAN
/* .prod.arena-size: ARENA_SIZE is currently set larger for the
 * MM/Dylan product as an interim solution.
 * See request.dylan.170170.sol.patch and change.dylan.buffalo.170170.
 */
#define ARENA_SIZE              ((Size)1<<30)
#define ARENA_INIT_SPARE_COMMIT_LIMIT   ((Size)10uL*1024uL*1024uL)
#define THREAD_MULTI
#define PROTECTION
#define DONGLE_NONE

#elif defined(CONFIG_PROD_CONFIGURA)
#define MPS_PROD_STRING         "configura"
#define MPS_PROD_CONFIGURA
#define ARENA_INIT_SPARE_COMMIT_LIMIT   ((Size)10uL*1024uL*1024uL)
#define THREAD_SINGLE
#define PROTECTION
#define DONGLE_NONE

#elif defined(CONFIG_PROD_MPS)
#define MPS_PROD_STRING         "mps"
#define MPS_PROD_MPS
#define ARENA_INIT_SPARE_COMMIT_LIMIT   ((Size)10uL*1024uL*1024uL)
#define THREAD_MULTI
#define PROTECTION
#define DONGLE_NONE

#else
#error "No target product configured."
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
