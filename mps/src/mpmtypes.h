/* impl.h.mpmtypes: MEMORY POOL MANAGER TYPES
 *
 * $HopeName: MMsrc!mpmtypes.h(trunk.3) $
 * Copyright (C) 1996 Harlequin Group, all rights reserved.
 *
 * .rationale: Types and type constants are almost all defined
 * in this header, in advance of any declarations of prototypes
 * or structures.  This avoids difficulties in defining recursive
 * data structures, and also provides a nice browsable list of
 * types.
 */

#ifndef mpmtypes_h
#define mpmtypes_h

#include "std.h"
#include "lib.h"


/* TYPES */

/* Word, Byte, Index, Addr, etc. -- machine types
 *
 * Word is an unsigned integral type which matches the size of the
 * machine word, i.e. the natural size of the machine registers and
 * addresses.  It should be used where an unsigned integer is
 * required that might range as large as the machine word.
 *
 * Byte is an unsigned integral type corresponding to the unit in
 * which most sizes are measured, and also the units of sizeof().
 *
 * Index is an unsigned integral type which is large enough to hold
 * any array index.  It should be used where the maximum size of the
 * array cannot be statically determined.  However, where it can,
 * smaller integers should be used.
 *
 * Addr is the type used for "managed addresses", that is, addresses
 * of objects managed by the MPS.  Limited arithmetic is allowed on
 * addresses using AddrAdd and AddrOffset.
 *
 * Size is an unsigned integral type large enough to hold the size
 * of any object which the MPS might manage.
 *
 * Align is an unsigned integral type which is used to hold alignments.
 * All alignments are positive powers of two.  Align is large enough to
 * hold the maximum possible alignment.
 *
 * Shift is an unsigned integral type which can hold the amount by
 * which a Word can be shifted.  It is therefore large enough to hold
 * the binary log of the word width (in bits).
 *
 * Ref is a reference to a managed object (as opposed to any old
 * managed address).  Ref should be used where a reference is intended.
 */

typedef MPS_T_WORD Word;                /* machine word */
typedef unsigned char Byte;             /* units of size */
typedef struct AddrStruct *Addr;        /* managed address */
typedef Word Size;                      /* size of managed object */
typedef Word Index;                     /* index for any array */
typedef Word Align;                     /* alignment (power of two) */
typedef unsigned Shift;                 /* shift for any word */
typedef Addr Ref;                       /* reference to managed object */

typedef Word RefSet;                    /* see impl.c.ref */
typedef unsigned Rank;                  /* see impl.c.ref */
typedef Size Epoch;                     /* see impl.c.ld */
typedef unsigned TraceId;               /* impl.c.mpm.check.ti */
typedef unsigned TraceSet;              /* impl.c.mpm.check.ts */
typedef unsigned AccessSet;
typedef unsigned Attr;                  /* impl.h.mpmst.class */
typedef int RootVar;                    /* impl.h.mpmst.root */
typedef unsigned Serial;
typedef struct RingStruct *Ring;        /* impl.c.ring */
typedef struct BufferStruct *Buffer;    /* impl.c.buffer */
typedef struct APStruct *AP;            /* impl.c.buffer */
typedef struct FormatStruct *Format;    /* impl.c.format */
typedef struct LDStruct *LD;            /* impl.c.ld */
typedef struct LockStruct *Lock;        /* impl.c.lock* */
typedef struct PoolStruct *Pool;        /* impl.c.pool */
typedef struct SpaceStruct *Space;      /* impl.c.space */
typedef struct PoolClassStruct *PoolClass; /* impl.c.poolclas */
typedef struct TraceStruct *Trace;      /* impl.c.trace */
typedef struct ScanStateStruct *ScanState; /* impl.c.trace */
typedef struct SegStruct *Seg;          /* impl.c.arena* */
typedef struct ArenaStruct *Arena;      /* impl.c.arena* */
typedef struct VMStruct *VM;            /* impl.c.vm* */
typedef struct RootStruct *Root;        /* impl.c.root */
typedef struct ThreadStruct *Thread;    /* impl.c.th* */


/* Pool*Method -- Pool Class Interface types
 *
 * These methods are provided by pool classes as part of the PoolClass
 * object (see impl.h.mpmst.class).  They form the interface which
 * allows the MPM to treat pools in a uniform manner.
 *
 * See design.mps.class-interface.
 */

typedef Res  (*PoolInitMethod)         (Pool pool, va_list arg);
typedef void (*PoolFinishMethod)       (Pool pool);
typedef Res  (*PoolAllocMethod)        (Addr *pReturn, Pool pool, Size size);
typedef void (*PoolFreeMethod)         (Pool pool, Addr old, Size size);
typedef Res  (*PoolBufferInitMethod)   (Pool pool, Buffer buf);
typedef void (*PoolBufferFinishMethod) (Pool pool, Buffer buf);
typedef Res  (*PoolBufferFillMethod)   (Addr *baseReturn, Pool pool,
                                        Buffer buffer, Size size);
typedef Bool (*PoolBufferTripMethod)   (Pool pool, Buffer buffer,
                                        Addr base, Size size);
typedef void (*PoolBufferExposeMethod) (Pool pool, Buffer buffer);
typedef void (*PoolBufferCoverMethod)  (Pool pool, Buffer buffer);
typedef Res  (*PoolDescribeMethod)     (Pool pool, Lib_FILE *stream);
typedef Res  (*PoolCondemnMethod)      (RefSet *condemnedReturn, Pool pool,
                                        Space space, TraceId ti);
typedef void (*PoolGreyMethod)         (Pool pool, Space space, TraceId ti);
typedef Res  (*PoolScanMethod)         (ScanState ss, Pool pool, 
                                        Bool *finishedReturn);
typedef Res  (*PoolFixMethod)          (Pool pool, ScanState ss, Seg seg,
                                        Ref *refIO);
typedef void (*PoolReclaimMethod)      (Pool pool, Space space, TraceId ti);
typedef void (*PoolAccessMethod)       (Pool pool, Seg seg, AccessSet mode);


/* Format*Method -- Object Format Interface types
 *
 * These methods are provided by clients in order to describe to
 * a pool class how to manage objects in client-specific formats.  They
 * are part of the Format object (see impl.h.mpmst.format).
 *
 * See design.mps.format-interface.
 */

typedef Res  (*FormatScanMethod)   (ScanState ss, Addr base, Addr limit);
typedef Addr (*FormatSkipMethod)   (Addr object);
typedef void (*FormatMoveMethod)   (Addr object, Addr to);
typedef Addr (*FormatIsMovedMethod)(Addr object);
typedef void (*FormatCopyMethod)   (Addr object, Addr to);
typedef void (*FormatPadMethod)    (Addr base, Size size);


/* Root*Method -- Root Interface types
 *
 * These methods are provided by the client so that the MPS can locate
 * the root set.
 *
 * See design.mps.root-interface.
 */

typedef Res (*RootScanMethod)   (ScanState ss, void *p, size_t s);
typedef Res (*RootScanRegMethod)(ScanState ss, Thread thread, void *p);


/* CONSTANTS */

#define AccessSetEMPTY  ((AccessSet)0)
#define AccessREAD      ((AccessSet)(1<<0))
#define AccessWRITE     ((AccessSet)(1<<1))
#define RingNONE        ((Ring)0)       /* impl.c.ring */
#define TraceIdNONE     ((TraceId)-1)
#define TraceSetEMPTY   ((TraceSet)0)
#define AttrFMT         ((Attr)0x0001)  /* is formatted */
#define AttrSCAN        ((Attr)0x0002)  /* is scannable */
#define AttrPM_NO_READ  ((Attr)0x0004)  /* may not be read-protected */
#define AttrPM_NO_WRITE ((Attr)0x0008)  /* may not be write-protected */
#define AttrALLOC       ((Attr)0x0010)  /* has alloc */
#define AttrFREE        ((Attr)0x0020)  /* has free */
#define AttrBUF         ((Attr)0x0100)  /* supports allocation points */
#define AttrBUF_RESERVE ((Attr)0x0200)  /* has reserve/commit */
#define AttrBUF_ALLOC   ((Attr)0x0400)  /* has buf_alloc */
#define AttrGC          ((Attr)0x1000)  /* garbage collecting */
#define AttrINCR_RB     ((Attr)0x2000)  /* read-barrier incremental */
#define AttrINCR_WB     ((Attr)0x4000)  /* write-barrier incremental */

enum {                          /* rank constants */
  RankAMBIG,                    /* ambiguous reference */
  RankEXACT,                    /* exact strong reference */
  RankWEAK,                     /* exact weak reference */
  RankFINAL,                    /* exact strong notifying reference */
  RankMAX                       /* the number of ranks, not a rank */
};

enum {                          /* root variants, see impl.h.mpmst.root */
  RootFUN,                      /* function closure */
  RootTABLE,                    /* table */
  RootREG,                      /* register */
  RootFMT                       /* formatted */
};

#endif /* mpmtypes_h */
