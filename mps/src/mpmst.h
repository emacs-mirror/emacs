/* impl.h.mpmst: MEMORY POOL MANAGER DATA STRUCTURES
 *
 * $HopeName: MMsrc!mpmst.h(trunk.29) $
 * Copyright (C) 1997 The Harlequin Group Limited.  All rights reserved.
 *
 * .readership: MM developers.
 *
 * .design: This header file crosses module boundaries.  The relevant
 * design a module's structures should be found in that module's design
 * document.
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

#define PoolClassSig    ((Sig)0x519C7A55) /* SIGnature pool CLASS */

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
  PoolBufferInitMethod bufferInit;
  PoolBufferFillMethod bufferFill;
  PoolBufferEmptyMethod bufferEmpty;
  PoolBufferFinishMethod bufferFinish;
  PoolTraceBeginMethod traceBegin;
  PoolCondemnMethod condemn;    /* condemn (some or all) objects */
  PoolGreyMethod grey;          /* grey non-white objects */
  PoolScanMethod scan;          /* find references during tracing */
  PoolFixMethod fix;            /* referent reachable during tracing */
  PoolReclaimMethod reclaim;    /* reclaim dead objects after tracing */
  PoolTraceEndMethod traceEnd;
  PoolBenefitMethod benefit;
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

#define PoolSig         ((Sig)0x519B0019) /* SIGnature POOL */

typedef struct PoolStruct {     /* generic structure */
  Sig sig;                      /* design.mps.sig */
  Serial serial;                /* from space->poolSerial */
  PoolClass class;              /* pool class structure */
  Space space;                  /* owning space */
  RingStruct spaceRing;         /* link in list of pools in space */
  RingStruct bufferRing;        /* allocation buffers are attached to pool */
  Serial bufferSerial;          /* serial of next buffer */
  RingStruct segRing;           /* segs are attached to pool */
  RingStruct actionRing;        /* actions are attached to pool */
  Serial actionSerial;          /* serial of next action */
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

#define MFSSig          ((Sig)0x5193F599) /* SIGnature MFS */ 

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

#define MVSig           ((Sig)0x5193B999) /* SIGnature MV */

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

#define VMSig           ((Sig)0x519B3999) /* SIGnature VM */

#ifdef VM_RM                    /* impl.h.config */

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

#elif defined(MPS_OS_O1) 

typedef struct VMStruct {       /* DEC UNIX VM structure; impl.c.vmo1 */
  Sig sig;                      /* design.mps.sig */
  Align align;                  /* page size */
  Addr base, limit;             /* boundaries of reserved space */
  Size reserved;                /* total reserved address space */
  Size mapped;                  /* total mapped memory */
  int none_fd;                  /* fildes for reserved memory */
} VMStruct;

#elif defined(MPS_OS_S7) || defined(MPS_OS_I4) || defined (MPS_OS_I5)

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
 * the arena.  See design.mps.seg.
 */

typedef struct SegStruct {      /* segment structure */
  Pool _pool;                   /* MUST BE FIRST (design.mps.seg.field.pool) */
  RingStruct _poolRing;         /* link in list of segs in pool */
  void *_p;                     /* pointer for use of owning pool */
  Buffer _buffer;               /* non-NULL if seg is buffered */
  RefSet _summary;              /* summary of references out of seg */
  unsigned _depth : SHIELD_DEPTH_WIDTH; /* see impl.c.shield.def.depth */
  AccessSet _pm : AccessMAX;    /* protection mode, impl.c.shield */
  AccessSet _sm : AccessMAX;    /* shield mode, impl.c.shield */
  TraceSet _grey : TRACE_MAX;   /* traces for which seg is grey */
  TraceSet _white : TRACE_MAX;  /* traces for which seg is white */
  unsigned int _single : 1;     /* is a single page segment? */
  RankSet _rankSet : RankMAX;   /* ranks of references in this seg */
} SegStruct;


/* SegPrefStruct -- segment preference structure
 * 
 * .seg-pref: segment users (pool class code) need a way of expressing
 * preferences about the segments they allocate.
 */

#define SegPrefSig      ((Sig)0x5195E9B6) /* SIGnature SEG PRef */ 

typedef struct SegPrefStruct {  /* segment placement preferences */
  Sig sig;                      /* impl.h.misc.sig */
  Bool high;                    /* high or low */
  RefSet refSet;                /* preferred RefSetOfSeg */
} SegPrefStruct;


/* ArenaStruct -- arena structure
 *
 * .def: The arena structure is in-lined in the space structure
 * (impl.h.mpmst.space).
 */

#define ArenaSig        ((Sig)0x519A6E4A) /* SIGnature ARENA */ 

#ifdef ARENA_ANSI

/* This is the arena structure used by the ANSI-based  */
/* arena implementation, impl.c.arenaan. */

typedef struct ArenaStruct {    /* ANSI arena structure */
  Sig sig;                      /* design.mps.sig */
  RingStruct blockRing;         /* list of blocks in arena */
  Size committed;               /* total committed (alloced by pools) memory */
} ArenaStruct;

#elif defined(ARENA_CLIENT)

typedef struct ArenaStruct {    /* arena structure */
  Sig sig;                      /* impl.h.misc.sig */
  RingStruct chunkRing;         /* all the chunks */
  Serial chunkSerial;           /* next chunk number */
  Shift pageShift;              /* log2(pageSize), for shifts */
  Size pageSize;                /* size of block managed by PageStruct */
} ArenaStruct;

#else  /* neither ARENA_ANSI nor ARENA_CLIENT */

/* This is the arena structure used by the virtual memory based */
/* arena implementation, impl.c.arenavm. */

/* Types used in ArenaStruct, but otherwise defined in impl.c.arenavm. */
typedef struct PageStruct *Page; /* page type */

typedef struct ArenaStruct {    /* VM arena structure */
  Sig sig;                      /* design.mps.sig */
  VMStruct vmStruct;            /* virtual memory structure */
  Addr base;                    /* base address of arena area */
  Addr limit;                   /* limit address of arena area */
  Size pageSize;                /* size of block managed by PageStruct */
  Shift pageShift;              /* log2 of page size, for shifts */
  Index pages;                  /* number of pages in table */
  Page pageTable;               /* the page table */
  BT allocTable;                /* page allocation table */
  Size tablesSize;              /* size of area occupied by tables */
  Index tablePages;             /* number of pages occupied by tables */
} ArenaStruct;

#endif /* ARENA_CLIENT, ARENA_ANSI */

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

#define BufferSig       ((Sig)0x519B0FFE) /* SIGnature BUFFEr */

typedef struct BufferStruct {
  Sig sig;                      /* design.mps.sig */
  Serial serial;                /* from pool->bufferSerial */
  Space space;                  /* owning space */
  Pool pool;                    /* owning pool */
  RingStruct poolRing;          /* buffers are attached to pools */
  Rank rankSet;                 /* ranks of references being created */
  Seg seg;                      /* segment being buffered */
  Addr base;                    /* base address of allocation buffer */
  Addr initAtFlip;              /* limit of initialized data at flip */
  APStruct apStruct;            /* the allocation point */
  Addr poolLimit;               /* the pool's idea of the limit */
  Align alignment;              /* allocation alignment */
  void *p;                      /* closure variable for pool */
  int i;                        /* closure variable for pool */
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

#define FormatSig       ((Sig)0x519F63A2) /* Signature FoRMAT */

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

#define LockSig         ((Sig)0x51970CC9) /* SIGnature LOCK */ 
#define ThreadSig       ((Sig)0x519286ED) /* SIGnature THREaD */

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
 defined(MPS_OS_S7) || defined(MPS_OS_I4) || defined(MPS_OS_I5) || defined(MPS_OS_SO)

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

#define RootSig         ((Sig)0x51960029) /* SIGnature ROOT */

typedef struct RootStruct {
  Sig sig;                      /* design.mps.sig */
  Serial serial;                /* from space->rootSerial */
  Space space;                  /* owning space */
  RingStruct spaceRing;         /* attachment to space */
  Rank rank;                    /* rank of references in this root */
  TraceSet grey;                /* traces for which root is grey */
  RefSet summary;               /* summary of references in root */
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
      Addr *base;               /* beginning of table */
      Addr *limit;              /* one off end of table */
      Word mask;                /* tag mask for scanning */
    } tableMasked;
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


/* ScanState
 *
 * .ss: See impl.c.trace.
 *
 * .ss: The first four fields of the trace structure must match the
 * external scan state structure (mps_ss_s) thus:
 *   ss->fix            mps_ss->fix
 *   ss->zoneShift      mps_ss->w0
 *   ss->white          mps_ss->w1
 *   ss->summary        mps_ss->w2
 * See impl.h.mps.ss and impl.c.mpsi.check.ss.  This is why the
 * Sig field is in the middle of this structure.
 *
 * .ss.zone: The zoneShift field is therefore declared as Word
 * rather than Shift.
 */

#define ScanStateSig    ((Sig)0x5195CA45) /* SIGnature SCAN State */

typedef struct ScanStateStruct {
  Res (*fix)(ScanState, Addr *);/* fix function */
  Word zoneShift;               /* copy of space->zoneShift.  See .ss.zone */
  RefSet white;                 /* white set, for inline fix test */
  RefSet summary;               /* accumulated summary of scanned references */
  Sig sig;                      /* design.mps.sig */
  Space space;                  /* owning space */
  TraceSet traces;              /* traces to scan for */
  Rank rank;                    /* reference rank of scanning */
  Bool wasMarked;               /* design.mps.fix.protocol.was-ready */
  RefSet fixed;                 /* accumulated summary of fixed references */
} ScanStateStruct;


/* TraceStruct -- tracer state structure */

#define TraceSig        ((Sig)0x51924ACE)

typedef struct TraceStruct {
  Sig sig;                      /* design.mps.sig */
  TraceId ti;                   /* index into TraceSets */
  Space space;                  /* owning space */
  Action action;                /* the action that launched the trace */
  RefSet white;                 /* superset of refs in white set */
  RankSet grey;                 /* ranks for which grey segs (may) exist */
  TraceState state;             /* current state of trace */
  Size interval;                /* polling interval */
} TraceStruct;


/* ActionStruct -- action structure
 *
 * See design.mps.action.
 */

#define ActionSig       ((Sig)0x519AC209)

typedef struct ActionStruct {
  Sig sig;                      /* design.mps.sig */
  Serial serial;                /* from pool->actionSerial */
  Pool pool;                    /* owning pool */
  RingStruct poolRing;          /* link in list of actions in pool */
} ActionStruct;


/* SpaceStruct -- the space structure
 *
 * See impl.c.space.
 *
 * .space: The space structure is the top-level state of the
 * MPS, and as such contains a lot of fields which are considered
 * "global".  These fields belong to different modules.  The module
 * which owns each group of fields is commented.
 */

#define SpaceSig        ((Sig)0x5195BACE) /* SIGnature SPACE */

typedef struct SpaceStruct {
  /* space fields (impl.c.space) */
  Sig sig;                      /* design.mps.sig */
  Serial serial;                /* design.mps.space.static.serial */
  RingStruct globalRing;        /* node in global ring of spaces */
  Bool poolReady;               /* design.mps.space.pool.ready */
  MVStruct controlPoolStruct;   /* design.mps.space.pool */
  LockStruct lockStruct;        /* space's lock */
  Size pollThreshold;           /* design.mps.space.poll */
  Bool insidePoll;              /* design.mps.space.poll */
  Size actionInterval;          /* design.mps.space.poll.interval */
  double allocTime;             /* "time" in allocated bytes */

  /* arena fields (impl.c.arena*) */
  ArenaStruct arenaStruct;      /* design.mps.space.arena */
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
  TraceSet flippedTraces;       /* set of running and flipped traces */
  TraceStruct trace[TRACE_MAX]; /* trace structures.  See
                                   design.mps.trace.intance.limit */

  /* location dependency fields (impl.c.ld) */
  Epoch epoch;                     /* design.mps.space.ld.epoch */
  RefSet prehistory;               /* design.mps.space.ld.prehistory */
  RefSet history[SPACE_LD_LENGTH]; /* design.mps.space.ld.history */
} SpaceStruct;


#endif /* mpmst_h */
