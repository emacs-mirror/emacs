/* impl.h.config: MPS CONFIGURATION
 *
 * Copyright (C) 1997 Harlequin Group, all rights reserved.
 * $HopeName: MMsrc!config.h(trunk.3) $
 */

#ifndef config_h
#define config_h

/* Platform Configuration */

#include "mpstd.h"


/* Variety Configuration
 *
 * Convert CONFIG_VAR_* defined on compiler command line into
 * internal configuration parameters.  See design.mps.config.var.
 */

#if defined(CONFIG_VAR_DF)
#define MPS_VAR_DF
#define ASSERT_MPSI		/* impl.c.mpsi */
#define ASSERT_MPM 		/* impl.h.mpm */
#define CHECK_DEEP       	/* impl.h.assert */
#define CHECK_ASSERT    	/* impl.h.assert */
#elif defined(CONFIG_VAR_DP)    /* debug, partial checking */
#define MPS_VAR_DP
#define ASSERT_MPSI
#define ASSERT_MPM
#define CHECK_SHALLOW
#define CHECK_ASSERT     	/* impl.h.assert */
#elif defined(CONFIG_VAR_DS)    /* debug, sig checking only */
#define MPS_VAR_DS
#define ASSERT_MPSI
#define ASSERT_MPM
#elif defined(CONFIG_VAR_RO)
#define MPS_VAR_RO
#else
#error "No target variety configured."
#endif


/* Product Configuration
 *
 * Convert CONFIG_PROD_* defined on compiler command line into
 * internal configuration parameters.  See design.mps.config.prod.
 */

#if defined(CONFIG_PROD_EPCORE)
#define MPS_PROD_EPCORE
#define VM_RM			/* impl.h.mpmst.vm */
#define ARENA_SIZE              ((Size)64<<20)
#elif defined(CONFIG_PROD_DYLAN)
#define MPS_PROD_DYLAN
#define ARENA_SIZE              ((Size)1<<30)
#elif defined(CONFIG_PROD_MPS)
#define MPS_PROD_MPS
#define ARENA_SIZE              ((Size)64<<20)
#else
#error "No target product configured."
#endif


/* Space Configuration -- see impl.c.space */

#define SPACE_CONTROL_EXTENDBY  ((Size)4096)
#define SPACE_CONTROL_AVGSIZE   ((Size)32)
#define SPACE_CONTROL_MAXSIZE   ((Size)65536)
#define SPACE_POLL_MAX          ((Size)262144)
#define SPACE_LD_LENGTH         ((Size)4)


/* Arena Configuration -- see impl.c.arena* */

#define ARENA_ANSI_ALIGN        ((Align)4096)
#define ARENA_ANSI_ZONESHIFT    ((Shift)20)


/* Shield Configuration -- see impl.c.shield */

#define SHIELD_CACHE_SIZE ((Size)2)


/* VM Configuration -- see impl.c.vm* */

#define VMAN_ALIGN              ((Align)4096)
#define VM_JUNKBYTE             ((unsigned char)0xA9)
#define VMRM_ALIGN		((Align)4096)


/* Tracer Configuration -- see impl.c.trace */

#define TRACE_MAX               ((Size)1)


#endif /* config_h */
