/* impl.h.mpmst: MEMORY POOL MANAGER DATA STRUCTURES
 *
 * $HopeName: MMsrc!mpmst.h(trunk.69) $
 * Copyright (C) 1998 Harlequin Group plc.  All rights reserved.
 *
 * .readership: MM developers.
 *
 * .design: This header file crosses module boundaries.  The relevant
 * design a module's structures should be found in that module's design
 * document.
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
 */

#ifndef mpmst_h
#define mpmst_h

#include "config.h"
#include "mpmtypes.h"

#if defined(MPS_OS_W3)
/* windows.h included for CRITICAL_SECTION only, see .lock.win32 */
#include "mpswin.h"
#endif /* MPS_OS_W3 */


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
  PoolClass super;              /* superclass */
  Attr attr;                    /* attributes */
  PoolInitMethod init;          /* initialize the pool descriptor */
  PoolFinishMethod finish;      /* finish the pool descriptor */
  PoolAllocMethod alloc;        /* allocate memory from pool */
  PoolFreeMethod free;          /* free memory to pool */
  PoolBufferInitMethod bufferInit;      /* additional buffer init */
  PoolBufferFillMethod bufferFill;      /* out-of-line reserve */
  PoolBufferEmptyMethod bufferEmpty;    /* out-of-line commit */
  PoolBufferFinishMethod bufferFinish;  /* additional buffer finish */
  PoolTraceBeginMethod traceBegin;      /* no idea what this does @@@@ */
  PoolAccessMethod access;      /* handles read/write accesses */
  PoolWhitenMethod whiten;      /* whiten objects in a segment */
  PoolGreyMethod grey;          /* grey non-white objects */
  PoolBlackenMethod blacken;    /* blacken grey objects without scanning */
  PoolScanMethod scan;          /* find references during tracing */
  PoolFixMethod fix;            /* referent reachable during tracing */
  PoolFixEmergencyMethod fixEmergency;  /* as fix, no failure allowed */
  PoolReclaimMethod reclaim;    /* reclaim dead objects after tracing */
  PoolBenefitMethod benefit;    /* calculate benefit of action */
  PoolActMethod act;            /* do an action */
  PoolRampBeginMethod rampBegin;/* begin a ramp pattern */
  PoolRampEndMethod rampEnd;    /* end a ramp pattern */
  PoolWalkMethod walk;          /* walk over a segment */
  PoolDescribeMethod describe;  /* describe the contents of the pool */
  PoolDebugMixinMethod debugMixin; /* find the debug mixin, if any */
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
  Serial serial;                /* from arena->poolSerial */
  PoolClass class;              /* pool class structure */
  Arena arena;                  /* owning arena */
  RingStruct arenaRing;         /* link in list of pools in arena */
  RingStruct bufferRing;        /* allocation buffers are attached to pool */
  Serial bufferSerial;          /* serial of next buffer */
  RingStruct segRing;           /* segs are attached to pool */
  RingStruct actionRing;        /* actions are attached to pool */
  Serial actionSerial;          /* serial of next action */
  Align alignment;              /* alignment for units */
  Format format;                /* format only if class->attr&AttrFMT */
  double fillMutatorSize;       /* bytes filled, mutator buffers */
  double emptyMutatorSize;      /* bytes emptied, mutator buffers */
  double fillInternalSize;      /* bytes filled, internal buffers */
  double emptyInternalSize;     /* bytes emptied, internal buffers */
} PoolStruct;

/* MFSStruct -- MFS (Manual Fixed Small) pool outer structure
 *
 * .mfs: See impl.c.poolmfs, design.mps.poolmfs.
 *
 * The MFS outer structure is declared here because it is in-lined
 * in the control pool structure which is in-lined in the arena.  Normally,
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
 * control pool structure which is in-lined in the arena.  Normally,
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


/* NSEGStruct -- NSEG (Non-allocating SEGment) pool outer structure
 *
 * .nseg: See impl.c.arena, design.mps.poolnseg.
 *
 * The NSEG outer structure is declared here because it is in-lined 
 * in the arena for storing segments for the low-memory reservoir.
 * Normally, pool outer structures are declared with the pools.
 *
 * The signature is placed at the end, see
 * design.mps.pool.outer-structure.sig
 */

#define NSEGSig          ((Sig)0x51945e99) /* SIGnature NSEG */ 

typedef struct NSEGStruct {     /* NSEG outer structure */
  PoolStruct poolStruct;        /* generic structure */
  Sig sig;                      /* design.mps.sig */
} NSEGStruct;


/* MessageClassStruct -- Message Class structure 
 *
 * see design.mps.message.class.struct (and design.mps.message.message,
 * and design.mps.message.class).
 */

#define MessageClassSig ((Sig)0x519359c1) /* SIGnature MeSsaGe CLass */

typedef struct MessageClassStruct {
  Sig sig;                      /* design.mps.sig */
  const char *name;             /* Human readable Class name */

  /* generic methods */
  MessageDeleteMethod delete;   /* terminates a message */

  /* methods specific to MessageTypeFinalization */
  MessageFinalizationRefMethod
    finalizationRef;        

  /* methods specific to MessageTypeCollectionStats */
  MessageCollectionStatsLiveSizeMethod collectionStatsLiveSize;
  MessageCollectionStatsCondemnedSizeMethod 
    collectionStatsCondemnedSize;
  MessageCollectionStatsNotCondemnedSizeMethod 
    collectionStatsNotCondemnedSize;

  Sig endSig;                   /* design.mps.message.class.sig.double */
} MessageClassStruct;

#define MessageSig      ((Sig)0x5193e559) /* SIG MESSaGe */

/* MessageStruct -- Message structure
 *
 * see design.mps.message.message.struct.
 */

typedef struct MessageStruct {
  Sig sig;                      /* design.mps.sig */
  Arena arena;                  /* owning arena */
  MessageType type;             /* Message Type */
  MessageClass class;           /* Message Class Structure */
  RingStruct queueRing;         /* Message queue ring */
} MessageStruct;


/* SegStruct -- segment structure
 *
 * .seg: Segments are the basic units of memory allocation from
 * the arena.  See design.mps.seg.
 * .seg.fieldnames: @@@@ Fieldnames all begin with underscore, for no 
 *   documented reason.  GavinM tells me this was done by Richard as part 
 *   of finding and removing some modularity-breaking accesses of SegStruct 
 *   fields.  There is no reason not to revert these names at some point in 
 *   the future.  richardk19980312.
 */

typedef struct SegStruct {      /* segment structure */
  Pool _pool;                   /* MUST BE FIRST (design.mps.seg.field.pool) */
  RingStruct _poolRing;         /* link in list of segs in pool */
  RingStruct _greyRing;         /* link in list of grey segs */
  void *_p;                     /* pointer for use of owning pool */
  Buffer _buffer;               /* non-NULL if seg is buffered */
  RefSet _summary;              /* summary of references out of seg */
  unsigned _depth : SHIELD_DEPTH_WIDTH; /* see impl.c.shield.def.depth */
  AccessSet _pm : AccessMAX;    /* protection mode, impl.c.shield */
  AccessSet _sm : AccessMAX;    /* shield mode, impl.c.shield */
  TraceSet _grey : TRACE_MAX;   /* traces for which seg is grey */
  TraceSet _white : TRACE_MAX;  /* traces for which seg is white */
  TraceSet _nailed : TRACE_MAX; /* traces for which seg has nailed objects */
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
  Bool isCollected;             /* whether segment will be collected */
  Bool isGen;                   /* whether gen is set */
  Serial gen;                   /* associated geneation */
} SegPrefStruct;


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
  Arena arena;                  /* owning arena */
  Pool pool;                    /* owning pool */
  RingStruct poolRing;          /* buffers are attached to pools */
  Bool isMutator;               /* TRUE iff buffer used by mutator */
  BufferMode mode;              /* Attached/Logged/Flipped/etc */
  double fillSize;              /* bytes filled in this buffer */
  double emptySize;             /* bytes emptied from this buffer */
  RankSet rankSet;              /* ranks of references being created */
  Seg seg;                      /* segment being buffered */
  Addr base;                    /* base address of allocation buffer */
  Addr initAtFlip;              /* limit of initialized data at flip */
  APStruct apStruct;            /* the allocation point */
  Addr poolLimit;               /* the pool's idea of the limit */
  Align alignment;              /* allocation alignment */
  unsigned rampCount;           /* see impl.c.buffer.ramp.hack */
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
  Serial serial;                /* from arena->formatSerial */
  FormatVariety variety;        /* format variety (e.g. A) */
  Arena arena;                  /* owning arena */
  RingStruct arenaRing;         /* formats are attached to the arena */
  Align alignment;              /* alignment of formatted objects */
  FormatScanMethod scan;
  FormatSkipMethod skip;
  FormatMoveMethod move;
  FormatIsMovedMethod isMoved;
  FormatCopyMethod copy;
  FormatPadMethod pad;
  FormatClassMethod class;      /* pointer indicating class */
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
  Serial serial;                /* from arena->threadSerial */
  Arena arena;                  /* owning arena */
  RingStruct arenaRing;         /* threads attached to arena */
  HANDLE handle;                /* Handle of thread, see
                                 * impl.c.thnti3.thread.handle */
  DWORD id;                     /* Thread id of thread */
} ThreadStruct;

#elif defined(MPS_OS_SU) || defined(MPS_OS_SO) || defined(MPS_OS_O1)\
      || defined(MPS_OS_S7) || defined(MPS_OS_I4) || defined(MPS_OS_I5)\
      || defined(MPS_OS_IA) || defined(MPS_OS_LI)
/* All these platforms use the trivial ANSI locks, since nothing better */

typedef struct LockStruct {     /* ANSI fake lock structure */
  Sig sig;                      /* design.mps.sig */
  unsigned long claims;         /* # claims held by owner */
} LockStruct;

typedef struct ThreadStruct {   /* ANSI fake thread structure */
  Sig sig;                      /* design.mps.sig */
  Serial serial;                /* from arena->threadSerial */
  Arena arena;                  /* owning arena */
  RingStruct arenaRing;         /* attaches to arena */
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
  Serial serial;                /* from arena->rootSerial */
  Arena arena;                  /* owning arena */
  RingStruct arenaRing;         /* attachment to arena */
  Rank rank;                    /* rank of references in this root */
  TraceSet grey;                /* traces for which root is grey */
  RefSet summary;               /* summary of references in root */
  RootMode mode;                /* mode */
  Bool protectable;             /* Can protect root? */
  Addr protBase;                /* base of protectable area */
  Addr protLimit;               /* limit of protectable area */
  AccessSet pm;                 /* Protection Mode */
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
 *   ss->unfixedSummary mps_ss->w2
 * See impl.h.mps.ss and impl.c.mpsi.check.ss.  This is why the
 * Sig field is in the middle of this structure.
 *
 * .ss.zone: The zoneShift field is therefore declared as Word
 * rather than Shift.
 */

#define ScanStateSig    ((Sig)0x5195CA45) /* SIGnature SCAN State */

typedef struct ScanStateStruct {
  TraceFixMethod fix;           /* fix function */
  Word zoneShift;               /* copy of arena->zoneShift.  See .ss.zone */
  RefSet white;                 /* white set, for inline fix test */
  RefSet unfixedSummary;        /* accumulated summary of scanned references */
  Sig sig;                      /* design.mps.sig */
  Arena arena;                  /* owning arena */
  TraceSet traces;              /* traces to scan for */
  Rank rank;                    /* reference rank of scanning */
  Bool wasMarked;               /* design.mps.fix.protocol.was-ready */
  RefSet fixedSummary;          /* accumulated summary of fixed references */
  Count fixRefCount;            /* refs which pass zone check */
  Count segRefCount;            /* refs which refer to segs */
  Count whiteSegRefCount;       /* refs which refer to white segs */
  Count nailCount;              /* segments nailed by ambig refs */
  Count snapCount;              /* refs snapped to forwarded objs */
  Count forwardedCount;         /* objects preserved by moving */
  Size forwardedSize;           /* bytes preserved by moving */
  Count preservedInPlaceCount;  /* objects preserved in place */
  Size preservedInPlaceSize;    /* bytes preserved in place */
  Size copiedSize;              /* bytes copied */
  Size scannedSize;             /* bytes scanned */
} ScanStateStruct;


/* TraceStruct -- tracer state structure */

#define TraceSig        ((Sig)0x51924ACE)

typedef struct TraceStruct {
  Sig sig;                      /* design.mps.sig */
  TraceId ti;                   /* index into TraceSets */
  Arena arena;                  /* owning arena */
  RefSet white;                 /* superset of refs in white set */
  RefSet mayMove;               /* superset of refs in moving set */
  TraceState state;             /* current state of trace */
  Bool emergency;               /* true iff ran out of memory during trace */
  Size condemned;               /* condemned bytes */
  Size notCondemned;            /* collectable but not condemned */
  Size foundation;              /* initial grey set size */
  Size rate;                    /* segs to scan per increment */
  Count rootScanCount;          /* number of roots scanned */
  Count rootScanSize;           /* total size of scanned roots */
  Size rootCopiedSize;          /* bytes copied by scanning roots */
  Count segScanCount;           /* number of segs scanned */
  Count segScanSize;            /* total size of scanned segments */
  Size segCopiedSize;           /* bytes copied by scanning segments */
  Size singleCopiedSize;        /* bytes copied by scanning single refs */
  Count singleScanCount;        /* number of single refs scanned */
  Count singleScanSize;         /* total size of single refs scanned */
  Count fixRefCount;            /* refs which pass zone check */
  Count segRefCount;            /* refs which refer to segs */
  Count whiteSegRefCount;       /* refs which refer to white segs */
  Count nailCount;              /* segments nailed by ambig refs */
  Count snapCount;              /* refs snapped to forwarded objs */
  Count faultCount;             /* read barrier faults */
  Count forwardedCount;         /* objects preserved by moving */
  Size forwardedSize;           /* bytes preserved by moving */
  Count preservedInPlaceCount;  /* objects preserved in place */
  Size preservedInPlaceSize;    /* bytes preserved in place */
  Count reclaimCount;           /* segments reclaimed */
  Count reclaimSize;            /* bytes reclaimed */
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


/* ArenaClassStruct -- generic arena class interface */

#define ArenaClassSig   ((Sig)0x519A6C1A) /* SIGnature ARena CLAss */

typedef struct ArenaClassStruct {
  Sig sig;
  char *name;                   /* class name string */
  size_t size;                  /* size of outer structure */
  size_t offset;                /* offset of generic struct in outer struct */
  ArenaInitMethod init;
  ArenaFinishMethod finish;
  ArenaReservedMethod reserved;
  ArenaCommittedMethod committed;
  ArenaExtendMethod extend;
  ArenaRetractMethod retract;
  ArenaIsReservedAddrMethod isReserved;
  ArenaSegAllocMethod segAlloc;
  ArenaSegFreeMethod segFree;
  ArenaSegBaseMethod segBase;
  ArenaSegLimitMethod segLimit;
  ArenaSegSizeMethod segSize;
  ArenaSegOfAddrMethod segOfAddr;
  ArenaSegFirstMethod segFirst;
  ArenaSegNextMethod segNext;
  ArenaDescribeMethod describe;
  Sig endSig;
} ArenaClassStruct;


/* ArenaStruct -- generic arena
 *
 * See impl.c.arena.
 *
 * .space: The arena structure is the top-level state of the
 * MPS, and as such contains a lot of fields which are considered
 * "global".  These fields belong to different modules.  The module
 * which owns each group of fields is commented.
 */

#define ArenaSig        ((Sig)0x519A6E4A) /* SIGnature ARENA */

typedef struct ArenaStruct {
  /* arena fields (impl.c.arena) */
  Sig sig;                      /* design.mps.sig */
  Serial serial;                /* design.mps.arena.static.serial */
  ArenaClass class;             /* arena class structure */
  RingStruct globalRing;        /* node in global ring of arenas */

  Bool poolReady;               /* design.mps.arena.pool.ready */
  MVStruct controlPoolStruct;   /* design.mps.arena.pool */
  NSEGStruct reservoirStruct;   /* design.mps.reservoir */
  Size reservoirLimit;          /* desired reservoir size */
  Size reservoirSize;           /* actual reservoir size */
  LockStruct lockStruct;        /* arena's lock */
  double pollThreshold;         /* design.mps.arena.poll */
  Bool insidePoll;
  Bool clamped;                 /* prevent background activity */

  Bool bufferLogging;           /* design.mps.buffer.logging.control */
  double fillMutatorSize;       /* total bytes filled, mutator buffers */
  double emptyMutatorSize;      /* total bytes emptied, mutator buffers */
  double allocMutatorSize;      /* fill-empty, only asymptotically accurate */
  double fillInternalSize;      /* total bytes filled, internal buffers */
  double emptyInternalSize;     /* total bytes emptied, internal buffers */

  Size commitLimit;             /* Client configurable commit limit */

  Shift zoneShift;              /* see also impl.c.ref */
  Align alignment;              /* minimum alignment of segments */

  /* pool fields (impl.c.pool) */
  RingStruct poolRing;          /* ring of pools in arena */
  Serial poolSerial;            /* serial of next created pool */

  /* root fields (impl.c.root) */
  RingStruct rootRing;          /* ring of roots attached to arena */
  Serial rootSerial;            /* serial of next root */

  /* format fields (impl.c.format) */
  RingStruct formatRing;        /* ring of formats attached to arena */
  Serial formatSerial;          /* serial of next format */

  /* message fields (design.mps.message, impl.c.message) */
  RingStruct messageRing;       /* ring of pending messages */
  BT enabledMessageTypes;       /* map of which types are enabled */

  /* finalization fields (design.mps.finalize), impl.c.space */
  Bool isFinalPool;             /* indicator for finalPool */
  Pool finalPool;               /* either NULL or an MRG pool */

  /* thread fields (impl.c.thread) */
  RingStruct threadRing;        /* ring of attached threads */
  Serial threadSerial;          /* serial of next thread */
  
  /* shield fields (impl.c.shield) */
  Bool insideShield;             /* TRUE if and only if inside shield */
  Seg shCache[SHIELD_CACHE_SIZE];/* Cache of unsynced segs */
  Size shCacheI;                 /* index into cache */
  Size shCacheLimit;             /* High water mark for cache usage */
  Size shDepth;                  /* sum of depths of all segs */
  Bool suspended;                /* TRUE if and only if mutator suspended */

  /* trace fields (impl.c.trace) */
  TraceSet busyTraces;          /* set of running traces */
  TraceSet flippedTraces;       /* set of running and flipped traces */
  TraceStruct trace[TRACE_MAX]; /* trace structures.  See
                                   design.mps.trace.intance.limit */
  RingStruct greyRing[RankMAX]; /* ring of grey segments at each rank */

  /* location dependency fields (impl.c.ld) */
  Epoch epoch;                     /* design.mps.arena.ld.epoch */
  RefSet prehistory;               /* design.mps.arena.ld.prehistory */
  RefSet history[ARENA_LD_LENGTH]; /* design.mps.arena.ld.history */
} ArenaStruct;


typedef struct AllocPatternStruct {
  char dummy;
} AllocPatternStruct;


/* Splay Trees 
 *
 * See design.mps.splay
 */

typedef struct SplayTreeStruct {
  SplayCompareMethod compare;
  SplayUpdateNodeMethod updateNode;
  SplayNode root;
} SplayTreeStruct;

typedef struct SplayNodeStruct {
  SplayNode left;     /* left child */
  SplayNode right;    /* right child */
} SplayNodeStruct;


/* CBS -- Coalescing Block Structure
 *
 * See design.mps.cbs.
 */

#define CBSSig ((Sig)0x519CB599) /* SIGnature CBS */

typedef struct CBSStruct {
  SplayTreeStruct splayTree;
  Pool blockPool;
  CBSChangeSizeMethod new;
  CBSChangeSizeMethod delete;
  CBSChangeSizeMethod grow;
  CBSChangeSizeMethod shrink;
  Size minSize;
  Align alignment;
  Bool mayUseInline;
  Bool fastFind;
  Bool inCBS; /* prevent reentrance */
  struct CBSEmergencyBlockStruct *emergencyBlockList;
  struct CBSEmergencyGrainStruct *emergencyGrainList;
  Sig sig; /* sig at end because embeded */
} CBSStruct;

typedef struct CBSBlockStruct {
  SplayNodeStruct splayNode;
  Addr base;
  Addr limit;
  Size maxSize; /* accurate maximum block size of sub-tree */
} CBSBlockStruct;


#endif /* mpmst_h */
