/* impl.h.mpmst: MEMORY POOL MANAGER DATA STRUCTURES
 *
 * $HopeName: MMsrc!mpmst.h(trunk.16) $
 * Copyright (C) 1996,1997 Harlequin Group, all rights reserved.
 *
 * .readership: MM developers.
 *
 * .design: This header file crosses module boundaries.  The relevant design
 * a module's structures should be found in that module's design document.
 *
 * .requirements: There are none [maybe being easy to experiment is a
 * requirement].
 *
 * .rationale: Almost all MPM data structures are defined in this
 * header, or in headers selected from here.  Most structures have
 * already been declared as incomplete types in impl.h.mpmtypes.  This
 * organization means that there is an easily browsable view of the
 * data structures, and that it is easy to experiment.
 *
 * Most of the structures are the underlying aggregate types for an
 * abstract data type.  See
 * guide.impl.c.naming.type.adt-aggregate.relate.
 *
 * .rationale.sig: Object signatures (PoolSig, etc.) are defined
 * here, along with the structures, so that any code which can see
 * a structure can also check its signature before using any of its
 * fields.  See design.mps.sig.test.uniq to check that signatures are
 * unique.
 *
 * TRANSGRESSIONS
 *
 * .fildes.name: the VMStruct used by impl.c.vmso and impl.c.vmsu has
 * two fields whose names violate our naming conventions.  They are
 * called none_fd and zero_fd to emphasize the fact that they are file
 * descriptors and this fact is not reflected in their type.
 */

#ifndef mpmst_h
#define mpmst_h

#include "config.h"
#include "mpmtypes.h"

#if defined(MPS_OS_W3)
/* windows.h included for CRITICAL_SECTION only, see .lock.win32 */
#include <windows.h>            
#endif /* MPS_OS_w3 */


/* RingStruct -- double-ended queue structure
 *
 * .ring: The ring structure is used as a field in other structures
 * in order to link them together into "rings".  See impl.c.ring.
 */

typedef struct RingStruct {     /* double-ended queue structure */
  Ring next, prev;              /* links to next and prev element */
} RingStruct;


/* PoolClassStruct -- pool class structure
 *
 * See design.mps.pool.
 *
 * .class: The pool class structure is defined by each pool class
 * implementation in order to provide an interface between the MPM
 * and the class (see design.mps.class-interface) via generic
 * functions (see impl.c.pool).  A class XXX defines a function
 * PoolClassXXX() returning a PoolClass pointing to a PoolClassStruct
 * of methods which implement the memory management policy.
 *
 * .class.end-sig: The class structure has another copy of the
 * signature at the end.  This causes the compiler to complain
 * if the class structure is extended without modifying static
 * initializers.
 */

#define PoolClassSig    ((Sig)0x519C1A55)

typedef struct PoolClassStruct {
  Sig sig;                      /* design.mps.sig */
  const char *name;             /* class name string */
  size_t size;                  /* size of outer structure */
  size_t offset;                /* offset of generic struct in outer struct */
  Attr attr;                    /* attributes */
  PoolInitMethod init;          /* initialize the pool descriptor */
  PoolFinishMethod finish;      /* finish the pool descriptor */
  PoolAllocMethod alloc;        /* allocate memory from pool */
  PoolFreeMethod free;          /* free memory to pool */
  PoolBufferInitMethod bufferInit;      /* additional buffer init */
  PoolBufferFinishMethod bufferFinish;  /* additional buffer finish */
  PoolBufferFillMethod bufferFill;      /* out-of-line reserve */
  PoolBufferTripMethod bufferTrip;      /* out-of-line commit */
  PoolBufferExposeMethod bufferExpose;  /* remove protection */
  PoolBufferCoverMethod bufferCover;    /* reinstate protection */
  PoolCondemnMethod condemn;    /* condemn (some or all) objects */
  PoolGreyMethod grey;          /* grey uncondemned objects */
  PoolScanMethod scan;          /* find references during tracing */
  PoolFixMethod fix;            /* referent reachable during tracing */
  PoolReclaimMethod reclaim;    /* reclaim dead objects after tracing */
  PoolAccessMethod access;      /* handle an access to shielded memory */
  PoolDescribeMethod describe;  /* describe the contents of the pool */
  Sig endSig;                   /* .class.end-sig */
} PoolClassStruct;


/* PoolStruct -- generic structure
 *
 * .pool: A generic structure is created when a pool is created
 * and holds the generic part of the pool's state.  Each pool class
 * defines a "subclass" of the pool structure (the "outer structure")
 * which contains PoolStruct as a a field.  The outer structure holds
 * the class-specific part of the pool's state.  See impl.c.pool,
 * design.mps.pool.
 */

#define PoolSig         ((Sig)0x519B0011)

typedef struct PoolStruct {     /* generic structure */
  Sig sig;                      /* design.mps.sig */
  Serial serial;                /* from space->poolSerial */
  PoolClass class;              /* pool class structure */
  Space space;                  /* owning space */
  RingStruct spaceRing;         /* link in list of pools in space */
  RingStruct bufferRing;        /* allocation buffers are attached to pool */
  Serial bufferSerial;          /* serial of next buffer */
  RingStruct segRing;           /* segs are attached to pool */
  Align alignment;              /* alignment for units */
} PoolStruct;


/* MFSStruct -- MFS (Manual Fixed Small) pool outer structure
 *
 * .mfs: See impl.c.poolmfs, design.mps.poolmfs.
 *
 * The MFS outer structure is declared here because it is in-lined
 * in the control pool structure which is in-lined in the space.  Normally,
 * pool outer structures are declared with the pools.
 *
 * The signature is placed at the end, see
 * design.mps.pool.outer-structure.sig
 */

#define MFSSig          ((Sig)0x5193F5B1)

typedef struct MFSStruct {      /* MFS outer structure */
  PoolStruct poolStruct;        /* generic structure */
  Size unroundedUnitSize;       /* the unit size requested */
  Size extendBy;                /* segment size rounded using unitSize */
  Size unitSize;                /* rounded for management purposes */
  Word unitsPerSeg;             /* number of units per segment */
  struct MFSHeaderStruct *freeList; /* head of the free list */
  Seg segList;                  /* the first segment */
  Sig sig;                      /* design.mps.sig */
} MFSStruct;


/* MVStruct -- MV (Manual Variable) pool outer structure
 *
 * .mv: See impl.c.poolmv, design.mps.poolmv.
 *
 * The MV pool outer structure is declared here because it is the
 * control pool structure which is in-lined in the space.  Normally,
 * pool outer structures are declared with the pools.
 *
 * The signature is placed at the end, see
 * design.mps.pool.outer-structure.sig
 */

#define MVSig           ((Sig)0x519E3FEE)

typedef struct MVStruct {       /* MV pool outer structure */
  PoolStruct poolStruct;        /* generic structure */
  MFSStruct blockPoolStruct;    /* for managing block descriptors */
  MFSStruct spanPoolStruct;     /* for managing span descriptors */
  Size extendBy;                /* segment size to extend pool by */
  Size avgSize;                 /* client estimate of allocation size */
  Size maxSize;                 /* client estimate of maximum size */
  Size space;                   /* total free space in pool */
  Size lost;                    /* design.mps.poolmv.lost */
  RingStruct spans;             /* span chain */
  Sig sig;                      /* design.mps.sig */
} MVStruct;


/* VMStruct -- virtual memory structure
 *
 * .vm: The VM structure is used when the MPM is configured to use a
 * virtual-memory based arena (impl.c.arenavm) which uses memory mapping
 * (impl.h.mpm.vm).  It holds the state information necessary to provide
 * that mapping, and as such, is specific to the implementation of that
 * vm (which is usually specific to an operating system).
 */

#define VMSig           ((Sig)0x519FEE33)

#ifdef VM_RM			/* impl.h.config */

typedef struct VMStruct {       /* Real Memory fake VM; impl.c.vmrm */
  Sig sig;                      /* design.mps.sig */
  Align align;                  /* made-up alignment */
  Addr base, limit;             /* boundaries of reserved space */
  Size reserved;                /* total reserved address space */
  Size mapped;                  /* total mapped memory */
} VMStruct;

#elif defined(MPS_OS_W3)

typedef struct VMStruct {       /* Win32 VM structure; impl.c.vmnt */
  Sig sig;                      /* design.mps.sig */
  Align align;                  /* page size */
  Addr base, limit;             /* boundaries of reserved space */
  Size reserved;                /* total reserved address space */
  Size mapped;                  /* total mapped memory */
} VMStruct;

#elif defined(MPS_OS_O1) || defined(MPS_OS_S7) || defined(MPS_OS_IR)

/* These platforms use vman, since no platform specific VM */

/* ANSI fake VM structure, see impl.c.vman, design.mps.vman */
typedef struct VMStruct {
  Sig sig;                      /* design.mps.sig */
  Addr base, limit;             /* boundaries of malloc'd memory */
  void *block;                  /* pointer to malloc'd block, for free() */
  Size reserved;                /* total reserved address space */
  Size mapped;                  /* total mapped memory */
} VMStruct;

#elif defined(MPS_OS_SU) || defined(MPS_OS_SO)

/* SunOS 4 & Solaris 2 use the same VM struct (only the prototypes of
 * mmap and so on are different) */

/* SunOS 4 & Solaris 2 VM structure; impl.c.vmsu, impl.c.vmso */
/* The names of zero_fd and none_fd are transgressions, see .fildes.name */
typedef struct VMStruct {
  Sig sig;                      /* design.mps.sig */
  int zero_fd;                  /* fildes for mmap, see impl.c.vms{o,u} */
  int none_fd;                  /* fildes for mmap, see impl.c.vms{o,u} */
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
 * .seg: Segments are the basic units of memory allocation from
 * the arena, and also the units of scanning, shielding, and colour
 * for the MPM (pool classes may subdivide segments and be able to
 * maintain colour on a finer grain (down to the object level for example)).
 *
 * .seg.pm: The pm field is used by both the shield (impl.c.shield)
 * and the ANSI fake protection (impl.c.protan).
 *
 * .seg.pool: This field must be first.  See
 * design.mps.seg.assume.pointer-conversion for why.
 */

typedef struct SegStruct {      /* segment structure */
  Pool pool;                    /* MUST BE FIRST, see .seg.pool */
  Bool single;                  /* single page segment */
  Rank rank;                    /* rank of all references in this seg */
  AccessSet pm, sm;             /* protection and shield modes */
  Size depth;                   /* see impl.c.shield.def.depth */
  void *p;                      /* pointer for use of owning pool */
  TraceId condemned;            /* seg condemned? for which trace? */
  TraceSet grey;                /* traces for which seg is grey */
  Buffer buffer;                /* non-NULL if seg is buffered */
  RingStruct poolRing;          /* link in list of segs in pool */
} SegStruct;


/* ArenaStruct -- arena structure
 *
 * .def: The arena structure is in-lined in the space structure
 * (impl.h.mpmst.space).
 */

#define ArenaSig        ((Sig)0x519A7E9A)

#ifdef ARENA_ANSI

/* This is the arena structure used by the ANSI-based  */
/* arena implementation, impl.c.arenaan. */

typedef struct ArenaStruct {    /* ANSI arena structure */
  Sig sig;                      /* design.mps.sig */
  RingStruct blockRing;         /* list of blocks in arena */
  Size committed;               /* total committed (alloced by pools) memory */
} ArenaStruct;

#else /* ARENA_ANSI not */

/* This is the arena structure used by the virtual memory based */
/* arena implementation, impl.c.arenavm. */

/* Types used in ArenaStruct, but otherwise defined in impl.c.arenavm. */
typedef struct PageStruct *Page;/* page type */
typedef Word *ABT;              /* Arena bool table type */

typedef struct ArenaStruct {    /* VM arena structure */
  Sig sig;                      /* design.mps.sig */
  VMStruct vmStruct;            /* virtual memory structure */
  Addr base;                    /* base address of arena area */
  Addr limit;                   /* limit address of arena area */
  Size pageSize;                /* size of block managed by PageStruct */
  Shift pageShift;              /* log2 of page size, for shifts */
  Index pages;                  /* number of pages in table */
  Page pageTable;               /* the page table */
  ABT freeTable;                /* page free table */
  Size tablesSize;              /* size of area occupied by tables */
  Index tablePages;             /* number of pages occupied by tables */
} ArenaStruct;

#endif /* ARENA_ANSI */


/* APStruct -- allocation point structure
 *
 * AP are part of the design of buffers see design.mps.buffer.
 *
 * The allocation point is exported to the client code so that it can
 * do in-line buffered allocation.
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
 * See impl.c.buffer, design.mps.buffer.
 *
 * The buffer contains an AP which may be exported to the client.
 */

#define BufferSig       ((Sig)0x519B0FFA)

typedef struct BufferStruct {
  Sig sig;                      /* design.mps.sig */
  Serial serial;                /* from pool->bufferSerial */
  Space space;                  /* owning space */
  Pool pool;                    /* owning pool */
  Seg seg;                      /* segment being buffered */
  Rank rank;                    /* rank of references being created */
  Addr base;                    /* base address of allocation buffer */
  APStruct apStruct;            /* the allocation point */
  Align alignment;              /* allocation alignment */
  RingStruct poolRing;          /* buffers are attached to pools */
  AccessSet shieldMode;         /* shielding for allocated memory */
#if 0
  Bool exposed;                 /* is buffer memory exposed? */
  TraceSet grey;                /* colour for allocated memory */
#endif
  void *p;
  int i;                        /* (p and i) closure variables (for pool) */
} BufferStruct;


/* FormatStruct -- object format structure
 *
 * See design.mps.format-interface, impl.c.format.
 *
 * .single: The only format actually implemented is variant "A" described
 * by the MPS Interface (impl.c.mpsi, impl.h.mps).  In future, when
 * more variants are added, the FormatStruct will have to be adapted in
 * some way to cope.
 */

#define FormatSig       ((Sig)0x519F43A2)

typedef struct FormatStruct {
  Sig sig;                      /* design.mps.sig */
  Serial serial;                /* from space->formatSerial */
  Space space;                  /* owning space */
  RingStruct spaceRing;         /* formats are attached to the space */
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
 * See design.mps.ld, and impl.c.ld.
 *
 * A version of this structure is exported to the client.
 * .ld.struct: This must be kept in sync with impl.h.mps.ld.
 * See also impl.c.mpsi.check.ld.
 */

typedef struct LDStruct {
  Epoch epoch;          /* epoch when ld was last reset / init'ed */
  RefSet rs;            /* RefSet of Add'ed references */
} LDStruct;


/* LockStruct and ThreadStruct -- locking and thread structures
 *
 * See design.mps.lock, design.mps.thread-manager.
 *
 * There are no standard interfaces to locks and threads, typically
 * the implementations of these modules (and hence the structures used
 * by them) will depend on an OS interface.
 */

#define LockSig         ((Sig)0x519110CC)
#define ThreadSig       ((Sig)0x51924EAD)

#if defined(MPS_OS_W3)

/* .lock.win32: Win32 lock structure; uses CRITICAL_SECTION */
typedef struct LockStruct {
  Sig sig;                      /* design.mps.sig */
  unsigned long claims;         /* # claims held by the owning thread */
  CRITICAL_SECTION cs;          /* Win32's recursive lock thing */
} LockStruct;

typedef struct ThreadStruct {   /* Win32 thread structure */
  Sig sig;                      /* design.mps.sig */
  Serial serial;                /* from space->threadSerial */
  Space space;                  /* owning space */
  RingStruct spaceRing;         /* threads attached to space */
  HANDLE handle;                /* Handle of thread, see
                                 * impl.c.thnti3.thread.handle */
  DWORD id;                     /* Thread id of thread */
} ThreadStruct;

#elif defined(MPS_OS_SU) || defined(MPS_OS_O1) || \
 defined(MPS_OS_S7) || defined(MPS_OS_IR) || defined(MPS_OS_SO)

/* All these platforms use the trivial ANSI locks, since nothing better */

typedef struct LockStruct {     /* ANSI fake lock structure */
  Sig sig;                      /* design.mps.sig */
  unsigned long claims;         /* # claims held by owner */
} LockStruct;

typedef struct ThreadStruct {   /* ANSI fake thread structure */
  Sig sig;                      /* design.mps.sig */
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
 * Synchronize with impl.c.root.
 */

#define RootSig         ((Sig)0x51940022)

typedef struct RootStruct {
  Sig sig;                      /* design.mps.sig */
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
      Addr *base;               /* beginning of table */
      Addr *limit;              /* one off end of table */
    } table;
    struct {
      RootScanRegMethod scan;   /* function for scanning registers */
      Thread thread;            /* passed to scan */
      void *p;                  /* passed to scan */
      size_t s;                 /* passed to scan */
    } reg;
    struct {
      FormatScanMethod scan;    /* format-like scanner */
      Addr base, limit;         /* passed to scan */
    } fmt;
  } the;
} RootStruct;


/* ScanState and TraceStruct
 *
 * .ss: See impl.c.trace.
 *
 * .ss: The first four fields of the trace structure must match the
 * external scan state structure (mps_ss_s) thus:
 *   ss->fix            mps_ss->fix
 *   ss->zoneShift      mps_ss->w0
 *   ss->condemned      mps_ss->w1
 *   ss->summary        mps_ss->w2
 * See impl.h.mps.ss and impl.c.mpsi.check.ss.  This is why the
 * Sig field is in the middle of this structure.
 *
 * .ss.zone: The zoneShift field is therefore declared as Word
 * rather than Shift.
 *
 * The weakSplat field forms part of the design for weakness.
 * See design.mps.weakness.
 */

#define ScanStateSig    ((Sig)0x5195CA95)

typedef struct ScanStateStruct {
  Res (*fix)(ScanState, Addr *);/* fix function */
  Word zoneShift;               /* copy of space->zoneShift.  See .ss.zone */
  RefSet condemned;             /* condemned set, for inline fix test */
  RefSet summary;               /* accumulated summary of scanned references */
  Sig sig;                      /* design.mps.sig */
  Space space;                  /* owning space */
  TraceId traceId;              /* trace ID of scan */
  Rank rank;                    /* reference rank of scanning */
  Addr weakSplat;               /* value of weak refs to unforwarded objects */
  Bool wasMarked;               /* design.mps.fix.protocol.was-ready */
} ScanStateStruct;

typedef struct TraceStruct {
  RefSet condemned;             /* summary of comdemnded set */
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
  Sig sig;                      /* design.mps.sig */
  Serial serial;                /* from static spaceSerial */
  RingStruct globalRing;        /* node in global ring of spaces */
  Bool poolReady;               /* has control pool been initialized? */
  MVStruct controlPoolStruct;   /* pool for miscellaneous items */
  LockStruct lockStruct;        /* space's lock */
  Size pollThreshold;           /* see impl.c.mpsi.poll and SpacePoll */
  Bool insidePoll;              /* prevent recursive polling, see SpacePoll */

  /* arena fields (impl.c.arena*) */
  ArenaStruct arenaStruct;      /* the arena */
  Shift zoneShift;              /* see also impl.c.ref */

  /* pool fields (impl.c.pool) */
  RingStruct poolRing;          /* ring of pools in space */
  Serial poolSerial;            /* serial of next created pool */

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
  Bool insideShield;             /* TRUE if and only if inside shield */
  Seg shCache[SHIELD_CACHE_SIZE];/* Cache of unsynced segs */
  Size shCacheI;                 /* index into cache */
  Size shDepth;                  /* sum of depths of all segs */
  Bool suspended;                /* TRUE if and only if mutator suspended */

  /* trace fields (impl.c.trace) */
  TraceSet busyTraces;          /* set of running traces */
  TraceStruct trace[TRACE_MAX]; /* trace structures.  See
                                   design.mps.trace.intance.limit */

  /* location dependency fields (impl.c.ld) */
  Epoch epoch;                  /* current epoch */
  RefSet prehistory;            /* all-time history of movements */
  RefSet history[SPACE_LD_LENGTH]; /* history of object movements */
} SpaceStruct;


#endif /* mpmst_h */
