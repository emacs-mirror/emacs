/* impl.h.config: MPS CONFIGURATION
 *
 * Copyright (C) 1997 Harlequin Group, all rights reserved.
 * $HopeName: MMsrc!config.h(trunk.14) $
 */

#ifndef config_h
#define config_h

/* Platform Configuration */

#include "mpstd.h"


/* Variety Configuration
 *
 * Convert CONFIG_VAR_* defined on compiler command line into
 * internal configuration parameters.  See design.mps.config.var
 * and design.mps.variety.macro.  Note that MPS_HOT is subclassed
 * into MPS_HOT_RED and MPS_HOT_WHITE; this distinction should
 * be rarely used.
 */

#if defined(CONFIG_VAR_HI)      /* Hot, Internal; variety.hi */
#define MPS_HOT
#define MPS_HOT_RED
#elif defined(CONFIG_VAR_CI)    /* Cool, Internal; variety.ci */
#define MPS_COOL
#elif defined(CONFIG_VAR_TI)    /* Telemetry, Internal; variety.ti */
#define EVENT
#define MPS_COOL
#elif defined(CONFIG_VAR_HE)    /* Hot, External; variety.he */
#define MPS_HOT
#define MPS_HOT_RED
#elif defined(CONFIG_VAR_CE)    /* Cool, External; variety.ce */
#define MPS_COOL
#elif defined(CONFIG_VAR_WI)    /* White hot, Internal; variety.wi */
#define MPS_HOT
#define MPS_HOT_WHITE
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
 * ARENA_CLIENT_PAGE_SIZE is the size in bytes of a "page" (i.e. segment
 * granule) in the client arena. The number 8192 is an initial value with no
 * particular justification.
 *
 * ARENA_CLIENT_DEFAULT_SEG_HIGH is a Bool governing whether segments default
 * 'high' (TRUE) or 'low' (FALSE). For EP-core, non-DL segments should be high
 * to reduce fragmentation of DL pools (See req.epcore.attr.footprint
 * and change 170193/trapping.beta.3 */

#define ARENA_CLIENT_PAGE_SIZE          ((Size)8192)
#define ARENA_CLIENT_DEFAULT_SEG_HIGH   TRUE

#if defined(CONFIG_PROD_EPCORE)
#define MPS_PROD_EPCORE
#define ARENA_SIZE              ((Size)2<<20)
#define AMC_SIZE_LIMIT          ARENA_SIZE
/* .nosync.why: ScriptWorks is single-threaded when using the MM. */
#define THREAD_SINGLE
#define PROTECTION_NONE
#elif defined(CONFIG_PROD_DYLAN)
#define MPS_PROD_DYLAN
#define ARENA_SIZE              ((Size)1<<30)
#define AMC_SIZE_LIMIT          ((Size)64<<20)  /* experimentally reasonable limit */
#define THREAD_MULTI
#define PROTECTION
#elif defined(CONFIG_PROD_MPS)
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
#define ARENA_POLL_MAX          ((Size)65536)
#define ARENA_LD_LENGTH         ((Size)4)
#define ARENA_ZONESHIFT         ((Shift)20)


/* ANSI Arena Configuration -- see impl.c.arenaan */

#define ARENA_ANSI_ALIGN        ((Align)4096)
#define ARENA_ANSI_ZONESHIFT    ((Shift)20)


/* Shield Configuration -- see impl.c.shield */

#define SHIELD_CACHE_SIZE       ((Size)2)
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

#endif /* config_h */
