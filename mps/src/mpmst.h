/* impl.h.mpmst: MEMORY POOL MANAGER DATA STRUCTURES
 *
 * $HopeName: MMsrc!mpmst.h(MMdevel_restr.7) $
 * Copyright (C) 1996 Harlequin Group, all rights reserved.
 *
 * .rationale: Almost all MPM data structures are defined in this
 * header, or in headers selected from here.  Most structures have
 * already been partially declared in impl.h.mpmtypes.  This
 * organization means that there is an easily browsable view of the
 * data structures, and that it is easy to experiment.
 *
 * .rationale.sig: Object signatures (PoolSig, etc.) are defined
 * here, along with the structures, so that any code which can see
 * a structure can also check its signature before using any of its
 * fields.
 */

#ifndef mpmst_h
#define mpmst_h

#include "mpmtypes.h"

#if defined(MPS_OS_W3)
#include <windows.h>
#endif /* MPS_OS_w3 */


/* RingStruct -- double-ended queue structure
 *
 * See impl.c.ring.
 */

typedef struct RingStruct {     /* double-ended queue structure */
  Ring next, prev;              /* links to next and prev element */
} RingStruct;


/* PoolClassStruct -- pool class structure
 *
 * See impl.c.poolclas.
 */

#define PoolClassSig    ((Sig)0x519C1A55)

typedef struct PoolClassStruct {
  Sig sig;                      /* impl.h.misc.sig */
  const char *name;             /* class name string */
  size_t size;                  /* size of instance structure */
  size_t offset;                /* offset of PoolStruct in instance */
  PoolCreateMethod create;
  PoolDestroyMethod destroy;
  PoolAllocMethod alloc;
  PoolFreeMethod free;
  PoolBufferCreateMethod bufferCreate;
  PoolBufferDestroyMethod bufferDestroy;
  PoolBufferFillMethod bufferFill;
  PoolBufferTripMethod bufferTrip;
  PoolBufferExposeMethod bufferExpose;
  PoolBufferCoverMethod bufferCover;
  PoolCondemnMethod condemn;
  PoolGreyMethod grey;
  PoolScanMethod scan;
  PoolFixMethod fix;
  PoolReclaimMethod reclaim;
  PoolAccessMethod access;
  PoolDescribeMethod describe;
} PoolClassStruct;


/* PoolStruct -- pool instance structure
 *
 * See impl.c.pool.
 */

#define PoolSig         ((Sig)0x519B0011)

typedef struct PoolStruct {     /* Pool instance structure */
  Sig sig;                      /* impl.h.misc.sig */
  Serial serial;                /* from space->poolSerial */
  PoolClass class;              /* pool class structure */
  Space space;                  /* owning space */
  RingStruct spaceRing;         /* link in list of pools in space */
  RingStruct bufferRing;        /* allocation buffers are attached to pool */
  Serial bufferSerial;          /* serial of next buffer */
  Align alignment;              /* alignment for units */
} PoolStruct;


/* MFSStruct -- MFS pool instance structure
 *
 * See impl.c.poolmfs.
 *
 * The MFS pool instance structure is declared here because it is in-lined
 * in the control pool structure which is in-lined in the space.  Normally,
 * pool instance structures are declared with the pools.
 *
 * Note that the signature appears at the end.  There's already one at the
 * beginning (in the poolStruct) so putting it at the end gives some extra
 * fencepost checking.
 */

#define MFSSig          ((Sig)0x5193F5B1)

typedef struct MFSStruct {      /* MFS instance structure */
  PoolStruct poolStruct;        /* generic pool structure */
  Size unroundedUnitSize;       /* the unit size requested */
  Size extendBy;                /* segment size rounded using unitSize */
  Size unitSize;                /* rounded for management purposes */
  unsigned unitsPerSeg;         /* number of units per segment */
  struct MFSHeaderStruct *freeList; /* head of the free list */
  Seg segList;                  /* the first segment */
  Sig sig;                      /* impl.h.misc.sig */
} MFSStruct;


/* MVStruct -- MV pool instance structure
 *
 * See impl.c.poolmv.
 *
 * The MV pool instance structure is declared here because it is the
 * control pool structure which is in-lined in the space.  Normally,
 * pool instance structures are declared with the pools.
 *
 * Note that the signature appears at the end.  There's already one at the
 * beginning (in the poolStruct) so putting it at the end gives some extra
 * fencepost checking.
 */

#define MVSig           ((Sig)0x519E3FEE)

typedef struct MVStruct {
  PoolStruct poolStruct;        /* generic pool structure */
  MFSStruct blockPoolStruct;    /* for managing block descriptors */
  MFSStruct spanPoolStruct;     /* for managing span descriptors */
  Size extendBy;                /* segment size to extend pool by */
  Size avgSize;                 /* client estimate of allocation size */
  Size maxSize;                 /* client estimate of maximum size */
  Size space;                   /* total free space in pool */
  Size lost;                    /* lost because free couldn't allocate(!) */
  struct MVSpanStruct *spans;   /* span chain */
  Sig sig;                      /* impl.h.misc.sig */
} MVStruct;


/* VMStruct -- virtual memory structure */

#define VMSig   ((Sig)0x519FEE33)

#if defined(MPS_OS_W3)

typedef struct VMStruct {       /* Win32 VM structure; impl.c.vmnt */
  Sig sig;                      /* impl.h.misc.sig */
  Align align;                  /* page size */
  Addr base, limit;             /* boundaries of reserved space */
  Size reserved;                /* total reserved address space */
  Size mapped;                  /* total mapped memory */
} VMStruct;

#elif defined(MPS_OS_O1) || defined(MPS_OS_S7)

typedef struct VMStruct {       /* ANSI fake VM structure; impl.c.vman */
  Sig sig;                      /* impl.h.misc.sig */
  Addr base, limit;             /* boundaries of malloc'd memory */
  void *block;                  /* pointer to malloc'd block, for free() */
  Size reserved;                /* total reserved address space */
  Size mapped;                  /* total mapped memory */
} VMStruct;

#elif defined(MPS_OS_SU)

typedef struct VMStruct {       /* SunOS 4 VM structure; impl.c.vmsu */
  Sig sig;                      /* impl.h.misc.sig */
  int zero_fd;                  /* see impl.c.vmsu */
  int none_fd;                  /* see impl.c.vmsu */
  Align align;                  /* page size */
  Addr base, limit;             /* boundaries of reserved space */
  Size reserved;                /* total reserved address space */
  Size mapped;                  /* total mapped memory */
} VMStruct;

#else
#error "No definition of VMStruct for this OS."
#endif


/* SegStruct -- segment structure
 *
 * .seg.def: Segments are the basic units of memory allocation from
 * the arena, and also the units of scanning, shielding, and colour
 * for the MPM (pool classes may subdivide segments and have a finer
 * grained idea of colour, for example).
 *
 * .seg.pm: The pm field is used by both the shield (impl.c.shield)
 * and the ANSI fake protection (impl.c.protan).
 */

typedef struct SegStruct {      /* segment structure */
  Pool pool;                    /* owner, MUST BE FIRST, impl.c.arenvm.page */
  Bool single;                  /* single page segment */
  AccessSet pm, sm;             /* protection and shield modes */
  Size depth;                   /* see impl.c.shield.def.depth */
  void *p;                      /* pointer for use of owning pool */
  TraceId condemned;            /* seg condemned? for which trace? */
} SegStruct;


/* ArenaStruct -- arena structure
 *
 * .def: The arena structure is in-lined in the space structure
 * (impl.h.mpmst.space).
 */

#define ArenaSig	((Sig)0x519A7E9A)

#ifdef TARGET_ARENA_ANSI

/* This is the arena structure used by the ANSI-based  */
/* arena implementation, impl.c.arenaan. */

typedef struct ArenaStruct {	/* ANSI arena structure */
  Sig sig;			/* impl.h.misc.sig */
  RingStruct blockRing;		/* list of blocks in arena */
  Size committed;		/* total allocated memory */
} ArenaStruct;

#else /* TARGET_ARENA_ANSI not */

/* This is the arena structure used by the virtual memory based */
/* arena implementation, impl.c.arenavm. */

typedef struct PageStruct *Page;/* page type */
typedef Word *BT;               /* bool table type */

typedef struct ArenaStruct {    /* VM arena structure */
  Sig sig;                      /* impl.h.misc.sig */
  VMStruct vmStruct;            /* virtual memory structure */
  Addr base;                    /* base address of arena area */
  Addr limit;                   /* limit address of arena area */
  Size pageSize;                /* size of block managed by PageStruct */
  Shift pageShift;              /* log2 of page size, for shifts */
  Size pages;                   /* number of pages in table */
  Page pageTable;               /* the page table */
  BT freeTable;                 /* page free table */
  Size tablesSize;              /* size of area occupied by tables */
  Size tablePages;              /* number of pages occupied by tables */
} ArenaStruct;

#endif /* TARGET_ARENA_ANSI */


/* APStruct -- allocation point structure
 *
 * See impl.c.buffer.
 *
 * The allocation point is exported to the client code so that it can
 * in-line buffered allocation.
 *
 * .ap: This structure must match impl.h.mps.ap.
 * See also impl.c.mpsi.check.ap.
 */

typedef struct APStruct {
  Addr init;                    /* limit of initialized area */
  Addr alloc;                   /* limit of allocated area */
  Addr limit;                   /* limit of allocation buffer */
} APStruct;


/* BufferStruct -- allocation buffer structure
 *
 * See impl.c.buffer.
 *
 * The buffer contains an AP which may be exported to the client.
 */

#define BufferSig       ((Sig)0x519B0FFA)

typedef struct BufferStruct {
  Sig sig;                      /* impl.h.misc.sig */
  Serial serial;                /* from pool->bufferSerial */
  Space space;                  /* owning space */
  Pool pool;                    /* owning pool */
  Seg seg;                      /* segment being buffered */
  Addr base;                    /* base address of allocation buffer */
  APStruct ap;                  /* the allocation point */
  Align alignment;              /* allocation alignment */
  Bool exposed;                 /* is buffer memory exposed? */
  RingStruct poolRing;          /* buffers are attached to pools */
  AccessSet shieldMode;         /* shielding for allocated memory */
  TraceSet grey;                /* colour for allocated memory */
  void *p; int i;               /* closure variables */
} BufferStruct;


/* FormatStruct -- object format structure
 *
 * See impl.c.format.
 *
 * The only format actually implemented is variant "A" described by
 * the MPS Interface.  In future, when more variants are added, the
 * FormatStruct will have to be adapted in some way to cope.
 */

#define FormatSig       ((Sig)0x519F43A2)

typedef struct FormatStruct {
  Sig sig;                      /* impl.h.misc.sig */
  Serial serial;                /* from space->formatSerial */
  Space space;                  /* owning space */
  RingStruct spaceRing;         /* link in list of formats in space */
  Align alignment;              /* alignment of formatted objects */
  FormatScanMethod scan;
  FormatSkipMethod skip;
  FormatMoveMethod move;
  FormatIsMovedMethod isMoved;
  FormatCopyMethod copy;
  FormatPadMethod pad;
} FormatStruct;


/* LDStruct -- location dependency structure
 *
 * See impl.c.ld.
 *
 * A version of this structure is exported to the client.
 * See impl.h.mps.ld and impl.c.mpsi.check.ld.
 */

typedef struct LDStruct {
  Epoch epoch;
  RefSet rs;
} LDStruct;


/* LockStruct and ThreadStruct -- locking and thread structures */

#define LockSig         ((Sig)0x519110CC)
#define ThreadSig       ((Sig)0x51924EAD)

#if defined(MPS_OS_W3)

typedef struct LockStruct {     /* Win32 lock structure */
  Sig sig;                      /* impl.h.misc.sig */
  unsigned long claims;         /* # claims held by the owning thread */
  CRITICAL_SECTION cs;          /* Win32's recursive lock thing */
} LockStruct;

typedef struct ThreadStruct {   /* Win32 thread structure */
  Sig sig;                      /* impl.h.misc.sig */
  Serial serial;                /* from space->threadSerial */
  Space space;                  /* owning space */
  RingStruct spaceRing;         /* threads attached to space */
  HANDLE handle;                /* Handle of thread impl.c.thnti3.thread.handle */
  DWORD id;                     /* Thread id of thread */
} ThreadStruct;

#elif defined(MPS_OS_SU) || defined(MPS_OS_O1) || defined(MPS_OS_S7)

typedef struct LockStruct {     /* ANSI fake lock structure */
  Sig sig;                      /* impl.h.misc.sig */
  unsigned long claims;         /* # claims held by owner */
} LockStruct;

typedef struct ThreadStruct {   /* ANSI fake thread structure */
  Sig sig;                      /* impl.h.misc.sig */
  Serial serial;                /* from space->threadSerial */
  Space space;                  /* owning space */
  RingStruct spaceRing;         /* attaches to space */
} ThreadStruct;

#else
#error "No definition of LockStruct or ThreadStruct for this OS."
#endif


/* RootStruct -- tracing root structure
 *
 * See impl.c.root.
 *
 * .root: The discriminator for the union is of type RootVar.
 */

#define RootSig         ((Sig)0x51940022)

typedef struct RootStruct {
  Sig sig;                      /* impl.h.misc.sig */
  Serial serial;                /* from space->rootSerial */
  Space space;                  /* owning space */
  RingStruct spaceRing;         /* attachment to space */
  Rank rank;                    /* rank of references in this root */
  TraceSet grey;                /* marked but not scanned for per trace */
  RootVar var;                  /* union discriminator */
  union RootUnion {
    struct {
      RootScanMethod scan;      /* the function which does the scanning */
      void *p;                  /* environment for scan */
      size_t s;                 /* environment for scan */
    } fun;
    struct {
      Addr *base;               /* first reference in table */
      Addr *limit;              /* last reference, plus one */
    } table;
    struct {
      RootScanRegMethod scan;
      Thread thread;
      void *p;
    } reg;
    struct {
      FormatScanMethod scan;
      Addr base, limit;
    } fmt;
  } the;
} RootStruct;


/* Scan State
 *
 * See impl.c.trace.
 *
 * The first four fields of the trace structure must match the
 * external scan state structure (mps_ss_s) thus:
 *   ss->fix            mps_ss->fix
 *   ss->zoneShift      mpm_ss->w0
 *   ss->condemned      mpm_ss->w1
 *   ss->summary        mpm_ss->w2
 * See impl.h.mps.ss and impl.c.mpsi.check.ss.  This is why the
 * Sig field is in the middle of this structure.
 *
 * .ss.zone: The zoneShift field is therefore declared as Word
 * rather than Shift.
 */

#define ScanStateSig    ((Sig)0x5195CA95)

typedef struct ScanStateStruct {
  Res (*fix)(ScanState ss, Addr *refIO);
  Word zoneShift;
  RefSet condemned;             /* condemned set, for inline fix test */
  RefSet summary;               /* accumulated summary of scanned references */
  Sig sig;                      /* impl.h.misc.sig */
  Space space;                  /* owning space */
  TraceId traceId;              /* trace ID of scan */
  Rank rank;                    /* reference rank of scanning */
} ScanStateStruct;

typedef struct TraceStruct {
  RefSet condemned;
} TraceStruct;


/* SpaceStruct -- the space structure
 *
 * See impl.c.space.
 *
 * .space: The space structure is the top-level state of the
 * MPS, and as such contains a lot of fields which are considered
 * "global".  These fields belong to different modules.  The module
 * which owns each group of fields is commented.
 */

#define SpaceSig        ((Sig)0x5195BACE)

typedef struct SpaceStruct {
  /* space fields (impl.c.space) */
  Sig sig;                      /* impl.h.misc.sig */
  Serial serial;                /* from static spaceSerial */
  RingStruct globalRing;        /* node in global ring of spaces */
  Bool poolReady;               /* has control pool been initialized? */
  MVStruct controlPoolStruct;   /* pool for miscellaneous items */
  LockStruct lockStruct;        /* space's lock */
  Size pollThreshold;           /* see SpacePoll() */
  Bool insidePoll;              /* prevent recursive polling */

  /* arena fields (impl.c.arena*) */
  ArenaStruct arenaStruct;      /* the arena */

  /* pool fields (impl.c.pool) */
  RingStruct poolRing;          /* list of pools in space */
  Serial poolSerial;            /* serial of next pool */

  /* root fields (impl.c.root) */
  RingStruct rootRing;          /* ring of roots attached to space */
  Serial rootSerial;            /* serial of next root */

  /* format fields (impl.c.format) */
  RingStruct formatRing;        /* ring of formats attached to space */
  Serial formatSerial;          /* serial of next format */

  /* thread fields (impl.c.thread) */
  RingStruct threadRing;        /* ring of attached threads */
  Serial threadSerial;          /* serial of next thread */
  
  /* shield fields (impl.c.shield) */
  Bool insideShield;             /* TRUE iff inside shield */
  Seg shCache[SHIELD_CACHE_SIZE];/* Cache of unsynced segs */
  Size shCacheI;                 /* index into cache */
  Size shDepth;                  /* sum of depths of all segs */
  Bool suspended;                /* TRUE iff mutator suspended */

  /* trace fields (impl.c.trace) */
  TraceSet busyTraces;          /* set of running traces */
  TraceStruct trace[TRACE_MAX]; /* trace structures */
  Shift zoneShift;              /* see impl.c.ref */

  /* location dependeny fields (impl.c.ld) */
  Epoch epoch;                  /* current epoch */
  RefSet prehistory;            /* all-time history of movements */
  RefSet history[SPACE_LD_LENGTH]; /* history of object movements */
} SpaceStruct;


#endif /* mpmst_h */
