/* impl.h.config: MPS CONFIGURATION
 *
 * $HopeName: MMsrc!config.h(trunk.27) $
 * Copyright (C) 1998.  Harlequin Group plc.  All rights reserved.
 *
 * PURPOSE
 *
 * This module translates from high-level symbols defined by the
 * external build system (gnumake, nmake, etc) into specific sets
 * of features used by MPS modules.  For example, the build system
 * will defined one of the CONFIG_VAR_* symbols to indicate which
 * variety it is building, this file translates that into a certain
 * level of checking, and a certain level of telemetry.
 *
 * DESIGN
 *
 * .design: See design.mps.config.
 *
 * READERSHIP
 *
 * .readership: MPS developers.
 */

#ifndef config_h
#define config_h


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

#if defined(CONFIG_VAR_WI)

/* "unreferenced formal parameter" */
#pragma warning(disable: 4100)

/* "unreferenced local function has been removed" */
#pragma warning(disable: 4505)

#endif /* CONFIG_VAR_WI */

#endif /* MPS_BUILD_MV */



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
#elif defined(CONFIG_VAR_CI)    /* Cool, Internal; variety.ci */
#define MPS_VARIETY_STRING      "ci"
#define MPS_COOL
#elif defined(CONFIG_VAR_TI)    /* Telemetry, Internal; variety.ti */
#define MPS_VARIETY_STRING      "ti"
#define EVENT
#define MPS_COOL
#elif defined(CONFIG_VAR_HE)    /* Hot, External; variety.he */
#define MPS_VARIETY_STRING      "he"
#define MPS_HOT
#define MPS_HOT_RED
#elif defined(CONFIG_VAR_CE)    /* Cool, External; variety.ce */
#define MPS_VARIETY_STRING      "ce"
#define MPS_COOL
#elif defined(CONFIG_VAR_WI)    /* White hot, Internal; variety.wi */
#define MPS_VARIETY_STRING      "wi"
#define MPS_HOT
#define MPS_HOT_WHITE
#elif defined(CONFIG_VAR_II)    /* Ice, Internal; variety.ii */
#define MPS_VARIETY_STRING      "ii"
#define MPS_HOT
#define MPS_HOT_RED
#define EVENT
#else
#error "No target variety configured."
#endif


/* Product Configuration
 *
 * Convert CONFIG_PROD_* defined on compiler command line into
 * internal configuration parameters.  See design.mps.config.prod.
 */

/* .ams-size: POOLAMS_MINIMUM_BENEFIT_SIZE is the size at which an AMS
 * pool will first be recommended for condemnation. See
 * design.mps.poolams.benefit.guess. This benefit computation is bogus
 * and will go away in time. */

#define POOLAMS_MINIMUM_BENEFIT_SIZE (1024ul * 1024)
 
/* .prod.arena-size: ARENA_SIZE is currently set larger for the
 * MM/Dylan product as an interim solution.
 * See request.dylan.170170.sol.patch and change.dylan.buffalo.170170.
 *
 * .mac.arena-size: ARENA_SIZE is set to 2Mb when compiling on
 * os.s7 (Mac OS) with the VM Arena, in order to allow development in
 * an environment where real memory is used to simulate memory mapping.
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

#define ARENA_CLIENT_PAGE_SIZE          ((Size)8192)
#define ARENA_DEFAULT_SEG_HIGH          TRUE
#define ARENA_DEFAULT_REFSET            BS_UPPER_HALF(RefSet)

#if defined(CONFIG_PROD_EPCORE)
#define MPS_PROD_STRING         "epcore"
#define MPS_PROD_EPCORE
#define ARENA_SIZE              ((Size)2<<20)
#define AMC_SIZE_LIMIT          ARENA_SIZE
/* .nosync.why: ScriptWorks is single-threaded when using the MM. */
#define THREAD_SINGLE
#define PROTECTION_NONE

#elif defined(CONFIG_PROD_DYLAN)
#define MPS_PROD_STRING         "dylan"
#define MPS_PROD_DYLAN
#define ARENA_SIZE              ((Size)1<<30)
#define AMC_SIZE_LIMIT          ((Size)64<<20)  /* experimentally reasonable limit */
#define THREAD_MULTI
#define PROTECTION

#elif defined(CONFIG_PROD_MPS)
#define MPS_PROD_STRING         "mps"
#define MPS_PROD_MPS
#ifdef MPS_OS_S7
#define ARENA_SIZE              ((Size)2<<20)
#define AMC_SIZE_LIMIT          ARENA_SIZE
#else
#define ARENA_SIZE              ((Size)64<<20)
#define AMC_SIZE_LIMIT          ARENA_SIZE
#endif /* MPS_OS_S7 */
#define THREAD_MULTI
#define PROTECTION

#else
#error "No target product configured."
#endif


/* Arena Configuration -- see impl.c.arena */

#define ARENA_CONTROL_EXTENDBY  ((Size)4096)
#define ARENA_CONTROL_AVGSIZE   ((Size)32)
#define ARENA_CONTROL_MAXSIZE   ((Size)65536)
#define ARENA_POLL_MAX          (65536.0)
#define ARENA_LD_LENGTH         ((Size)4)
#define ARENA_ZONESHIFT         ((Shift)20)

/* Stack configuration */

/* Currently StackProbe has a useful implementation only on
 * Intel platforms */
#ifdef MPS_ARCH_I3
#define STACK_PROBE_DEPTH       ((Word)500)
#else
#define STACK_PROBE_DEPTH       ((Word)0)
#endif /* MPS_ARCH_I3 */


/* ANSI Arena Configuration -- see impl.c.arenaan */

#define ARENA_ANSI_ALIGN        ((Align)4096)
#define ARENA_ANSI_ZONESHIFT    ((Shift)20)


/* Shield Configuration -- see impl.c.shield */

#define SHIELD_CACHE_SIZE       ((Size)16)
#define SHIELD_DEPTH_WIDTH      ((Size)4)
#define SHIELD_DEPTH            ((Count)1<<SHIELD_DEPTH_WIDTH)


/* VM Configuration -- see impl.c.vm* */

#define VMAN_ALIGN              ((Align)4096)
#define VM_JUNKBYTE             ((unsigned char)0xA9)


/* Tracer Configuration -- see impl.c.trace */

#define TRACE_MAX               ((Size)1)


/*  impl.c.event
 *
 *  EVENT_BUFFER_SIZE is the number of words in the global event buffer.
 *  EVENT_HEADER_SIZE is the number of words in each event header
 */

#define EVENT_BUFFER_SIZE       ((Count)4096)
#define EVENT_HEADER_SIZE       ((Count)3)


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


#endif /* config_h */
