/* mpmst.h: MEMORY POOL MANAGER DATA STRUCTURES
 *
 * $Id$
 * Copyright (c) 2001-2016 Ravenbrook Limited.  See end of file for license.
 * Portions copyright (C) 2001 Global Graphics Software.
 *
 * .design: This header file crosses module boundaries.  The relevant
 * design a module's structures should be found in that module's design
 * document.
 *
 * .structure: Most structures have already been declared as incomplete
 * types in <code/mpmtypes.h>.  Most of the structures are the underlying
 * aggregate types for an abstract data type.  See
 * guide.impl.c.naming.type.adt-aggregate.relate.
 *
 * .rationale.sig: Object signatures (PoolSig, etc.) are defined here,
 * along with the structures, so that any code which can see a structure
 * can also check its signature before using any of its fields.  See
 * <design/sig/#test.uniq> to check that signatures are unique.  */

#ifndef mpmst_h
#define mpmst_h

#include "config.h"
#include "mpmtypes.h"

#include "protocol.h"
#include "ring.h"
#include "locus.h"
#include "splay.h"
#include "meter.h"


/* PoolClassStruct -- pool class structure
 *
 * See <design/pool/>.
 *
 * .class: The pool class structure is defined by each pool class
 * implementation in order to provide an interface between the MPM and
 * the class (see <design/pool/>) via generic functions (see
 * <code/pool.c>). Pool classes use the class protocol (see
 * <design/protocol/>) and so CLASS(ABCPool) returns a PoolClass
 * pointing to a PoolClassStruct of methods which implement the memory
 * management policy for pool class ABC.
 *
 * .class.end-sig: The class structure has a signature at the end.  This
 * causes the compiler to complain if the class structure is extended
 * without modifying static initializers.  */

#define PoolClassSig    ((Sig)0x519C7A55) /* SIGnature pool CLASS */

typedef struct mps_pool_class_s {
  InstClassStruct instClassStruct;
  size_t size;                  /* size of outer structure */
  Attr attr;                    /* attributes */
  PoolVarargsMethod varargs;    /* convert deprecated varargs into keywords */
  PoolInitMethod init;          /* initialize the pool descriptor */
  PoolAllocMethod alloc;        /* allocate memory from pool */
  PoolFreeMethod free;          /* free memory to pool */
  PoolBufferFillMethod bufferFill;      /* out-of-line reserve */
  PoolBufferEmptyMethod bufferEmpty;    /* out-of-line commit */
  PoolAccessMethod access;      /* handles read/write accesses */
  PoolScanMethod scan;          /* find references during tracing */
  PoolFixMethod fix;            /* referent reachable during tracing */
  PoolFixMethod fixEmergency;   /* as fix, no failure allowed */
  PoolReclaimMethod reclaim;    /* reclaim dead objects after tracing */
  PoolRampBeginMethod rampBegin;/* begin a ramp pattern */
  PoolRampEndMethod rampEnd;    /* end a ramp pattern */
  PoolFramePushMethod framePush; /* push an allocation frame */
  PoolFramePopMethod framePop;  /* pop an allocation frame */
  PoolWalkMethod walk;          /* walk over a segment */
  PoolFreeWalkMethod freewalk;  /* walk over free blocks */
  PoolBufferClassMethod bufferClass; /* default BufferClass of pool */
  PoolDebugMixinMethod debugMixin; /* find the debug mixin, if any */
  PoolSizeMethod totalSize;     /* total memory allocated from arena */
  PoolSizeMethod freeSize;      /* free memory (unused by client program) */
  Sig sig;                      /* .class.end-sig */
} PoolClassStruct;


/* PoolStruct -- generic structure
 *
 * .pool: A generic structure is created when a pool is created and
 * holds the generic part of the pool's state.  Each pool class defines
 * a "subclass" of the pool structure (the "outer structure") which
 * contains PoolStruct as a a field.  The outer structure holds the
 * class-specific part of the pool's state.  See <code/pool.c>,
 * <design/pool/>.
 */

#define PoolSig         ((Sig)0x519B0019) /* SIGnature POOL */

typedef struct mps_pool_s {     /* generic structure */
  InstStruct instStruct;
  Sig sig;                      /* <design/sig/> */
  Serial serial;                /* from arena->poolSerial */
  Arena arena;                  /* owning arena */
  RingStruct arenaRing;         /* link in list of pools in arena */
  RingStruct bufferRing;        /* allocation buffers are attached to pool */
  Serial bufferSerial;          /* serial of next buffer */
  RingStruct segRing;           /* segs are attached to pool */
  Align alignment;              /* alignment for units */
  Format format;                /* format only if class->attr&AttrFMT */
  PoolFixMethod fix;            /* fix method */
} PoolStruct;


/* MFSStruct -- MFS (Manual Fixed Small) pool outer structure
 *
 * .mfs: See <code/poolmfs.c>, <design/poolmfs/>.
 *
 * The MFS outer structure is declared here because it is inlined
 * in the control pool structure which is inlined in the arena.  Normally,
 * pool outer structures are declared with the pools.
 *
 * The signature is placed at the end, see
 * <design/pool/#outer-structure.sig>. */

#define MFSSig          ((Sig)0x5193F599) /* SIGnature MFS */

typedef struct MFSStruct {      /* MFS outer structure */
  PoolStruct poolStruct;        /* generic structure */
  Size unroundedUnitSize;       /* the unit size requested */
  Size extendBy;                /* arena alloc size rounded using unitSize */
  Bool extendSelf;              /* whether to allocate tracts */
  Size unitSize;                /* rounded for management purposes */
  struct MFSHeaderStruct *freeList; /* head of the free list */
  Size total;                   /* total size allocated from arena */
  Size free;                    /* free space in pool */
  Tract tractList;              /* the first tract */
  Sig sig;                      /* <design/sig/> */
} MFSStruct;


/* MVStruct -- MV (Manual Variable) pool outer structure
 *
 * .mv: See <code/poolmv.c>, <design/poolmv/>.
 *
 * The MV pool outer structure is declared here because it is the
 * control pool structure which is inlined in the arena.  Normally,
 * pool outer structures are declared with the pools.  */

#define MVSig           ((Sig)0x5193B999) /* SIGnature MV */

typedef struct MVStruct {       /* MV pool outer structure */
  PoolStruct poolStruct;        /* generic structure */
  MFSStruct blockPoolStruct;    /* for managing block descriptors */
  MFSStruct spanPoolStruct;     /* for managing span descriptors */
  Size extendBy;                /* segment size to extend pool by */
  Size avgSize;                 /* client estimate of allocation size */
  Size maxSize;                 /* client estimate of maximum size */
  Size free;                    /* free space in pool */
  Size lost;                    /* <design/poolmv/#lost> */
  RingStruct spans;             /* span chain */
  Sig sig;                      /* <design/sig/> */
} MVStruct;


/* MessageClassStruct -- Message Class structure
 *
 * See <design/message/#class.struct> (and <design/message/#message>,
 * and <design/message/#class>).  */

#define MessageClassSig ((Sig)0x519359c1) /* SIGnature MeSsaGe CLass */

typedef struct MessageClassStruct {
  Sig sig;                      /* <design/sig/> */
  const char *name;             /* Human readable Class name */

  MessageType type;             /* Message Type */

  /* generic methods */
  MessageDeleteMethod delete;   /* terminates a message */

  /* methods specific to MessageTypeFINALIZATION */
  MessageFinalizationRefMethod finalizationRef;       

  /* methods specific to MessageTypeGC */
  MessageGCLiveSizeMethod gcLiveSize;
  MessageGCCondemnedSizeMethod gcCondemnedSize;
  MessageGCNotCondemnedSizeMethod gcNotCondemnedSize;

  /* methods specific to MessageTypeGCSTART */
  MessageGCStartWhyMethod gcStartWhy;

  Sig endSig;                   /* <design/message/#class.sig.double> */
} MessageClassStruct;

#define MessageSig      ((Sig)0x5193e559) /* SIG MESSaGe */

/* MessageStruct -- Message structure
 *
 * See <design/message/#message.struct>.  */

typedef struct mps_message_s {
  Sig sig;                      /* <design/sig/> */
  Arena arena;                  /* owning arena */
  MessageClass klass;           /* Message Class Structure */
  Clock postedClock;            /* mps_clock() at post time, or 0 */
  RingStruct queueRing;         /* Message queue ring */
} MessageStruct;


/* SegClassStruct -- segment class structure
 *
 * See <design/seg/> & <design/protocol/>.
 *
 * .seg.class: The segment class structure is defined by each segment
 * class implementation in order to provide a generic interface to
 * segments.  */

#define SegClassSig    ((Sig)0x5195E9C7) /* SIGnature SEG CLass */

typedef struct SegClassStruct {
  InstClassStruct instClassStruct;
  size_t size;                  /* size of outer structure */
  SegInitMethod init;           /* initialize the segment */
  SegSetSummaryMethod setSummary; /* set the segment summary  */
  SegBufferMethod buffer;       /* get the segment buffer  */
  SegSetBufferMethod setBuffer; /* set the segment buffer  */
  SegUnsetBufferMethod unsetBuffer; /* unset the segment buffer */
  SegSetGreyMethod setGrey;     /* change greyness of segment */
  SegSetWhiteMethod setWhite;   /* change whiteness of segment */
  SegSetRankSetMethod setRankSet; /* change rank set of segment */
  SegSetRankSummaryMethod setRankSummary; /* change rank set & summary */
  SegMergeMethod merge;         /* merge two adjacent segments */
  SegSplitMethod split;         /* split a segment into two */
  SegWhitenMethod whiten;       /* whiten objects */
  SegGreyenMethod greyen;       /* greyen non-white objects */
  SegBlackenMethod blacken;     /* blacken grey objects without scanning */
  Sig sig;                      /* .class.end-sig */
} SegClassStruct;


/* SegStruct -- segment structure
 *
 * .seg: Segments are the basic units of protection and tracer activity
 * for allocated memory.  See <design/seg/>.  */

#define SegSig      ((Sig)0x5195E999) /* SIGnature SEG  */

typedef struct SegStruct {      /* segment structure */
  InstStruct instStruct;
  Sig sig;                      /* <code/misc.h#sig> */
  Tract firstTract;             /* first tract of segment */
  RingStruct poolRing;          /* link in list of segs in pool */
  Addr limit;                   /* limit of segment */
  unsigned depth : ShieldDepthWIDTH; /* see design.mps.shield.def.depth */
  BOOLFIELD(queued);            /* in shield queue? */
  AccessSet pm : AccessLIMIT;   /* protection mode, <code/shield.c> */
  AccessSet sm : AccessLIMIT;   /* shield mode, <code/shield.c> */
  TraceSet grey : TraceLIMIT;   /* traces for which seg is grey */
  TraceSet white : TraceLIMIT;  /* traces for which seg is white */
  TraceSet nailed : TraceLIMIT; /* traces for which seg has nailed objects */
  RankSet rankSet : RankLIMIT;  /* ranks of references in this seg */
  unsigned defer : WB_DEFER_BITS; /* defer write barrier for this many scans */
} SegStruct;


/* GCSegStruct -- GCable segment structure
 *
 * .seggc: GCSeg is a subclass of Seg with support for buffered
 * allocation and GC.  See <design/seg/>.  */

#define GCSegSig      ((Sig)0x5199C5E9) /* SIGnature GC SEG  */

typedef struct GCSegStruct {    /* GC segment structure */
  SegStruct segStruct;          /* superclass fields must come first */
  RingStruct greyRing;          /* link in list of grey segs */
  RefSet summary;               /* summary of references out of seg */
  Buffer buffer;                /* non-NULL if seg is buffered */
  RingStruct genRing;           /* link in list of segs in gen */
  Sig sig;                      /* <design/sig/> */
} GCSegStruct;


/* LocusPrefStruct -- locus preference structure
 *
 * .locus-pref: arena memory users (pool class code) need a way of
 * expressing preferences about the locus of the segments they
 * allocate. See <design/locus/>.
 */

#define LocusPrefSig      ((Sig)0x51970CB6) /* SIGnature LOCus PRef */

typedef struct LocusPrefStruct { /* locus placement preferences */
  Sig sig;                      /* <code/misc.h#sig> */
  Bool high;                    /* high or low */
  ZoneSet zones;                /* preferred zones */
  ZoneSet avoid;                /* zones to avoid */
} LocusPrefStruct;


/* BufferClassStruct -- buffer class structure
 *
 * See <design/buffer/> & <design/protocol/>.
 *
 * .buffer.class: The buffer class structure is defined by each buffer
 * class implementation in order to provide a generic interface to
 * buffers.  */

#define BufferClassSig    ((Sig)0x519B0FC7) /* SIGnature BUFfer CLass */

typedef struct BufferClassStruct {
  InstClassStruct instClassStruct;
  size_t size;                  /* size of outer structure */
  BufferVarargsMethod varargs;  /* parse obsolete varargs */
  BufferInitMethod init;        /* initialize the buffer */
  BufferAttachMethod attach;    /* attach the buffer */
  BufferDetachMethod detach;    /* detach the buffer */
  BufferSegMethod seg;          /* seg of buffer */
  BufferRankSetMethod rankSet;  /* rank set of buffer */
  BufferSetRankSetMethod setRankSet; /* change rank set of buffer */
  BufferReassignSegMethod reassignSeg; /* change seg of attached buffer */
  Sig sig;                      /* .class.end-sig */
} BufferClassStruct;


/* BufferStruct -- allocation buffer structure
 *
 * See <code/buffer.c>, <design/buffer/>.
 *
 * The buffer contains an AP which may be exported to the client.
 * AP are part of the design of buffers see <design/buffer/>.
 * The allocation point is exported to the client code so that it can
 * do in-line buffered allocation.
 */

#define BufferSig       ((Sig)0x519B0FFE) /* SIGnature BUFFEr */

typedef struct BufferStruct {
  InstStruct instStruct;
  Sig sig;                      /* <design/sig/> */
  Serial serial;                /* from pool->bufferSerial */
  Arena arena;                  /* owning arena */
  Pool pool;                    /* owning pool */
  RingStruct poolRing;          /* buffers are attached to pools */
  Bool isMutator;               /* TRUE iff buffer used by mutator */
  BufferMode mode;              /* Attached/Logged/Flipped/etc */
  double fillSize;              /* bytes filled in this buffer */
  double emptySize;             /* bytes emptied from this buffer */
  Addr base;                    /* base address of allocation buffer */
  Addr initAtFlip;              /* limit of initialized data at flip */
  mps_ap_s ap_s;                /* the allocation point */
  Addr poolLimit;               /* the pool's idea of the limit */
  Align alignment;              /* allocation alignment */
  unsigned rampCount;           /* see <code/buffer.c#ramp.hack> */
} BufferStruct;


/* SegBufStruct -- Buffer structure associated with segments
 *
 * .segbuf: SegBuf is a subclass of Buffer with support for attachment
 * to segments.  */

#define SegBufSig ((Sig)0x51959B0F) /* SIGnature SeG BUFfer  */

typedef struct SegBufStruct {
  BufferStruct bufferStruct;    /* superclass fields must come first */
  RankSet rankSet;              /* ranks of references being created */
  Seg seg;                      /* segment being buffered */
  Sig sig;                      /* <design/sig/> */
} SegBufStruct;


/* FormatStruct -- object format structure
 *
 * See design.mps.format-interface, <code/format.c>.
 *
 * .single: In future, when more variants are added, FormatStruct should
 * really be replaced by a collection of format classes.  */

#define FormatSig       ((Sig)0x519F63A2) /* Signature FoRMAT */

typedef struct mps_fmt_s {
  Sig sig;
  Serial serial;                /* from arena->formatSerial */
  Arena arena;                  /* owning arena */
  RingStruct arenaRing;         /* formats are attached to the arena */
  Count poolCount;              /* number of pools using the format */
  Align alignment;              /* alignment of formatted objects */
  mps_fmt_scan_t scan;
  mps_fmt_skip_t skip;
  mps_fmt_fwd_t move;
  mps_fmt_isfwd_t isMoved;
  mps_fmt_pad_t pad;
  mps_fmt_class_t klass;        /* pointer indicating class */
  Size headerSize;              /* size of header */
} FormatStruct;


/* ScanState
 *
 * .ss: See <code/trace.c>.
 *
 * .ss: The mps_ss field of the scan state structure is exported
 * through the MPS interface to optimise the critical path scan loop.
 * See ["The critical path through the MPS"](../design/critical-path.txt).
 *
 * .ss.zone: For binary compatibility, the zone shift is exported as
 * a word rather than a shift, so that the external mps_ss_s is a uniform
 * three-word structure.  See <code/mps.h#ss> and <design/interface-c>.
 *
 *   zs  Shift   zoneShift       copy of arena->zoneShift.  See .ss.zone
 *   w   ZoneSet white           white set, for inline fix test
 *   ufs RefSet  unfixedSummary  accumulated summary of scanned references
 *
 * NOTE: The mps_ss structure used to be obfuscated to preserve Harlequin's
 * trade secrets in the MPS technology.  These days they just seek to
 * emphasize the abstraction, and could maybe be given better names and
 * types.  RB 2012-09-07
 */

#define ScanStateSig    ((Sig)0x5195CA45) /* SIGnature SCAN State */

typedef struct ScanStateStruct {
  Sig sig;                      /* <design/sig/> */
  struct mps_ss_s ss_s;         /* .ss <http://bash.org/?400459> */
  Arena arena;                  /* owning arena */
  PoolFixMethod fix;            /* third stage fix function */
  void *fixClosure;             /* closure data for fix */
  TraceSet traces;              /* traces to scan for */
  Rank rank;                    /* reference rank of scanning */
  Bool wasMarked;               /* design.mps.fix.protocol.was-ready */
  RefSet fixedSummary;          /* accumulated summary of fixed references */
  STATISTIC_DECL(Count fixRefCount) /* refs which pass zone check */
  STATISTIC_DECL(Count segRefCount) /* refs which refer to segs */
  STATISTIC_DECL(Count whiteSegRefCount) /* refs which refer to white segs */
  STATISTIC_DECL(Count nailCount) /* segments nailed by ambig refs */
  STATISTIC_DECL(Count snapCount) /* refs snapped to forwarded objs */
  STATISTIC_DECL(Count forwardedCount) /* objects preserved by moving */
  STATISTIC_DECL(Count preservedInPlaceCount) /* objects preserved in place */
  STATISTIC_DECL(Size copiedSize) /* bytes copied */
  Size scannedSize;             /* bytes scanned */
} ScanStateStruct;


/* TraceStruct -- tracer state structure */

#define TraceSig ((Sig)0x51924ACE) /* SIGnature TRACE */

typedef struct TraceStruct {
  Sig sig;                      /* <design/sig/> */
  TraceId ti;                   /* index into TraceSets */
  Arena arena;                  /* owning arena */
  int why;                      /* why the trace began */
  ZoneSet white;                /* zones in the white set */
  ZoneSet mayMove;              /* zones containing possibly moving objs */
  TraceState state;             /* current state of trace */
  Rank band;                    /* current band */
  Bool firstStretch;            /* in first stretch of band (see accessor) */
  PoolFixMethod fix;            /* fix method to apply to references */
  void *fixClosure;             /* closure information for fix method */
  Chain chain;                  /* chain being incrementally collected */
  STATISTIC_DECL(Size preTraceArenaReserved) /* ArenaReserved before this trace */
  Size condemned;               /* condemned bytes */
  Size notCondemned;            /* collectable but not condemned */
  Size foundation;              /* initial grey set size */
  Work quantumWork;             /* tracing work to be done in each poll */
  STATISTIC_DECL(Count greySegCount) /* number of grey segs */
  STATISTIC_DECL(Count greySegMax) /* max number of grey segs */
  STATISTIC_DECL(Count rootScanCount) /* number of roots scanned */
  Count rootScanSize;           /* total size of scanned roots */
  STATISTIC_DECL(Size rootCopiedSize) /* bytes copied by scanning roots */
  STATISTIC_DECL(Count segScanCount) /* number of segs scanned */
  Count segScanSize;            /* total size of scanned segments */
  STATISTIC_DECL(Size segCopiedSize) /* bytes copied by scanning segments */
  STATISTIC_DECL(Count singleScanCount) /* number of single refs scanned */
  STATISTIC_DECL(Count singleScanSize) /* total size of single refs scanned */
  STATISTIC_DECL(Size singleCopiedSize) /* bytes copied by scanning single refs */
  STATISTIC_DECL(Count fixRefCount) /* refs which pass zone check */
  STATISTIC_DECL(Count segRefCount) /* refs which refer to segs */
  STATISTIC_DECL(Count whiteSegRefCount) /* refs which refer to white segs */
  STATISTIC_DECL(Count nailCount) /* segments nailed by ambig refs */
  STATISTIC_DECL(Count snapCount) /* refs snapped to forwarded objs */
  STATISTIC_DECL(Count readBarrierHitCount) /* read barrier faults */
  STATISTIC_DECL(Count pointlessScanCount) /* pointless seg scans */
  STATISTIC_DECL(Count forwardedCount) /* objects preserved by moving */
  Size forwardedSize;           /* bytes preserved by moving */
  STATISTIC_DECL(Count preservedInPlaceCount) /* objects preserved in place */
  Size preservedInPlaceSize;    /* bytes preserved in place */
  STATISTIC_DECL(Count reclaimCount) /* segments reclaimed */
  STATISTIC_DECL(Count reclaimSize) /* bytes reclaimed */
} TraceStruct;


/* ArenaClassStruct -- generic arena class interface */

#define ArenaClassSig   ((Sig)0x519A6C1A) /* SIGnature ARena CLAss */

typedef struct mps_arena_class_s {
  InstClassStruct instClassStruct;
  size_t size;                  /* size of outer structure */
  ArenaVarargsMethod varargs;
  ArenaInitMethod init;
  ArenaCreateMethod create;
  ArenaDestroyMethod destroy;
  ArenaPurgeSpareMethod purgeSpare;
  ArenaExtendMethod extend;
  ArenaGrowMethod grow;
  ArenaFreeMethod free;
  ArenaChunkInitMethod chunkInit;
  ArenaChunkFinishMethod chunkFinish;
  ArenaCompactMethod compact;
  ArenaPagesMarkAllocatedMethod pagesMarkAllocated;
  ArenaChunkPageMappedMethod chunkPageMapped;
  Sig sig;
} ArenaClassStruct;


/* GlobalsStruct -- the global state associated with an arena
 *
 * .space: The arena structure holds the entire state of the MPS, and as
 * such contains a lot of fields which are considered "global".  These
 * fields belong to different modules.  The module which owns each group
 * of fields is commented.  */

#define GlobalsSig ((Sig)0x519970BA) /* SIGnature GLOBAls */

typedef struct GlobalsStruct {
  Sig sig;

  /* general fields (<code/global.c>) */
  RingStruct globalRing;        /* node in global ring of arenas */
  Lock lock;                    /* arena's lock */

  /* polling fields (<code/global.c>) */
  double pollThreshold;         /* <design/arena/#poll> */
  Bool insidePoll;
  Bool clamped;                 /* prevent background activity */
  double fillMutatorSize;       /* total bytes filled, mutator buffers */
  double emptyMutatorSize;      /* total bytes emptied, mutator buffers */
  double allocMutatorSize;      /* fill-empty, only asymptotically accurate */
  double fillInternalSize;      /* total bytes filled, internal buffers */
  double emptyInternalSize;     /* total bytes emptied, internal buffers */

  /* version field (<code/version.c>) */
  const char *mpsVersionString; /* MPSVersion() */

  /* buffer fields (<code/buffer.c>) */
  Bool bufferLogging;           /* <design/buffer/#logging.control> */

  /* pool fields (<code/pool.c>) */
  RingStruct poolRing;          /* ring of pools in arena */
  Serial poolSerial;            /* serial of next created pool */

  /* root fields (<code/root.c>) */
  RingStruct rootRing;          /* ring of roots attached to arena */
  Serial rootSerial;            /* serial of next root */

  /* remember summary (<code/trace.c>) */
  RingStruct rememberedSummaryRing;
  /* index into next free slot in block.  0 means that a new
     block should be allocated and appended. */
  Index rememberedSummaryIndex;
  
  /* locus (<code/locus.c>) */
  Chain defaultChain;           /* default chain for GC pool */
} GlobalsStruct;


/* LandClassStruct -- land class structure
 *
 * See <design/land/>.
 */

#define LandClassSig    ((Sig)0x5197A4DC) /* SIGnature LAND Class */

typedef struct LandClassStruct {
  InstClassStruct instClassStruct;
  size_t size;                  /* size of outer structure */
  LandSizeMethod sizeMethod;    /* total size of ranges in land */
  LandInitMethod init;          /* initialize the land */
  LandInsertMethod insert;      /* insert a range into the land */
  LandDeleteMethod delete;      /* delete a range from the land */
  LandIterateMethod iterate;    /* iterate over ranges in the land */
  LandIterateAndDeleteMethod iterateAndDelete; /* iterate and maybe delete */
  LandFindMethod findFirst;     /* find first range of given size */
  LandFindMethod findLast;      /* find last range of given size */
  LandFindMethod findLargest;   /* find largest range */
  LandFindInZonesMethod findInZones; /* find first range of given size in zone set */
  Sig sig;                      /* .class.end-sig */
} LandClassStruct;


/* LandStruct -- generic land structure
 *
 * See <design/land/>, <code/land.c>
 */

#define LandSig ((Sig)0x5197A4D9) /* SIGnature LAND */

typedef struct LandStruct {
  InstStruct instStruct;
  Sig sig;                      /* <design/sig/> */
  Arena arena;                  /* owning arena */
  Align alignment;              /* alignment of addresses */
  Bool inLand;                  /* prevent reentrance */
} LandStruct;


/* CBSStruct -- coalescing block structure
 *
 * CBS is a Land implementation that maintains a collection of
 * disjoint ranges in a splay tree.
 *
 * See <code/cbs.c>.
 */

#define CBSSig ((Sig)0x519CB599) /* SIGnature CBS */

typedef struct CBSStruct {
  LandStruct landStruct;        /* superclass fields come first */
  SplayTreeStruct splayTreeStruct;
  STATISTIC_DECL(Count treeSize)
  Pool blockPool;               /* pool that manages blocks */
  Size blockStructSize;         /* size of block structure */
  Bool ownPool;                 /* did we create blockPool? */
  Size size;                    /* total size of ranges in CBS */
  /* meters for sizes of search structures at each op */
  METER_DECL(treeSearch)
  Sig sig;                      /* .class.end-sig */
} CBSStruct;


/* FailoverStruct -- fail over from one land to another
 *
 * Failover is a Land implementation that combines two other Lands,
 * using primary until it fails, and then using secondary.
 *
 * See <code/failover.c>.
 */

#define FailoverSig ((Sig)0x519FA170) /* SIGnature FAILOver */

typedef struct FailoverStruct {
  LandStruct landStruct;        /* superclass fields come first */
  Land primary;                 /* use this land normally */
  Land secondary;               /* but use this one if primary fails */
  Sig sig;                      /* .class.end-sig */
} FailoverStruct;


/* FreelistStruct -- address-ordered freelist
 *
 * Freelist is a subclass of Land that maintains a collection of
 * disjoint ranges in an address-ordered freelist.
 *
 * See <code/freelist.c>.
 */

#define FreelistSig ((Sig)0x519F6331) /* SIGnature FREEL */

typedef union FreelistBlockUnion *FreelistBlock;

typedef struct FreelistStruct {
  LandStruct landStruct;        /* superclass fields come first */
  FreelistBlock list;           /* first block in list or NULL if empty */
  Count listSize;               /* number of blocks in list */
  Size size;                    /* total size of ranges in list */
  Sig sig;                      /* .class.end-sig */
} FreelistStruct;


/* SortStruct -- extra memory required by sorting
 *
 * See QuickSort in mpm.c.  This exists so that the caller can make
 * the choice about where to allocate the memory, since the MPS has to
 * operate in tight stack constraints -- see design.mps.sp.
 */

typedef struct SortStruct {
  struct {
    Index left, right;
  } stack[MPS_WORD_WIDTH];
} SortStruct;


/* ShieldStruct -- per-arena part of the shield
 *
 * See design.mps.shield, impl.c.shield.
 */

#define ShieldSig      ((Sig)0x519581E1) /* SIGnature SHEILd */

typedef struct ShieldStruct {
  Sig sig;           /* design.mps.sig */
  BOOLFIELD(inside); /* design.mps.shield.def.inside */
  BOOLFIELD(suspended); /* mutator suspended? */
  BOOLFIELD(queuePending); /* queue insertion pending? */
  Seg *queue;        /* queue of unsynced segs */
  Count length;      /* number of elements in shield queue */
  Index next;        /* next free element in shield queue */
  Index limit;       /* high water mark for cache usage */
  Count depth;       /* sum of depths of all segs */
  Count unsynced;    /* number of unsynced segments */
  Count holds;       /* number of holds */
  SortStruct sortStruct; /* workspace for queue sort */
} ShieldStruct;


/* History -- location dependency history
 *
 * See design.mps.arena.ld.
 */

#define HistorySig     ((Sig)0x51981520) /* SIGnature HISTOry */

typedef struct HistoryStruct {
  Sig sig;                         /* design.mps.sig */
  Epoch epoch;                     /* <design/arena/#ld.epoch> */
  RefSet prehistory;               /* <design/arena/#ld.prehistory> */
  RefSet history[LDHistoryLENGTH]; /* <design/arena/#ld.history> */
} HistoryStruct;  


/* ArenaStruct -- generic arena
 *
 * See <code/arena.c>.
 */

#define ArenaSig        ((Sig)0x519A6E4A) /* SIGnature ARENA */

typedef struct mps_arena_s {
  InstStruct instStruct;
  
  GlobalsStruct globals; /* must be first, see <design/arena/#globals> */
  Serial serial;

  Bool poolReady;               /* <design/arena/#pool.ready> */
  MVStruct controlPoolStruct;   /* <design/arena/#pool> */

  Size reserved;                /* total reserved address space */
  Size committed;               /* total committed memory */
  Size commitLimit;             /* client-configurable commit limit */

  Size spareCommitted;          /* Amount of memory in hysteresis fund */
  Size spareCommitLimit;        /* Limit on spareCommitted */
  double pauseTime;             /* Maximum pause time, in seconds. */

  Shift zoneShift;              /* see also <code/ref.c> */
  Size grainSize;               /* <design/arena/#grain> */

  Tract lastTract;              /* most recently allocated tract */
  Addr lastTractBase;           /* base address of lastTract */

  Chunk primary;                /* the primary chunk */
  RingStruct chunkRing;         /* all the chunks, in a ring for iteration */
  Tree chunkTree;               /* all the chunks, in a tree for fast lookup */
  Serial chunkSerial;           /* next chunk number */

  Bool hasFreeLand;              /* Is freeLand available? */
  MFSStruct freeCBSBlockPoolStruct;
  CBSStruct freeLandStruct;
  ZoneSet freeZones;            /* zones not yet allocated */
  Bool zoned;                   /* use zoned allocation? */

  /* locus fields (<code/locus.c>) */
  GenDescStruct topGen;         /* generation descriptor for dynamic gen */

  /* format fields (<code/format.c>) */
  RingStruct formatRing;        /* ring of formats attached to arena */
  Serial formatSerial;          /* serial of next format */

  /* message fields (<design/message/>, <code/message.c>) */
  RingStruct messageRing;       /* ring of pending messages */
  BT enabledMessageTypes;       /* map of which types are enabled */
  Count droppedMessages;        /* <design/message-gc/#lifecycle> */

  /* finalization fields (<design/finalize/>), <code/poolmrg.c> */
  Bool isFinalPool;             /* indicator for finalPool */
  Pool finalPool;               /* either NULL or an MRG pool */

  /* thread fields (<code/thread.c>) */
  RingStruct threadRing;        /* ring of attached threads */
  RingStruct deadRing;          /* ring of dead threads */
  Serial threadSerial;          /* serial of next thread */

  ShieldStruct shieldStruct;
  
  /* trace fields (<code/trace.c>) */
  TraceSet busyTraces;          /* set of running traces */
  TraceSet flippedTraces;       /* set of running and flipped traces */
  TraceStruct trace[TraceLIMIT]; /* trace structures.  See
                                   <design/trace/#intance.limit> */

  /* trace ancillary fields (<code/traceanc.c>) */
  TraceStartMessage tsMessage[TraceLIMIT];  /* <design/message-gc/> */
  TraceMessage tMessage[TraceLIMIT];  /* <design/message-gc/> */

  /* policy fields */
  double tracedWork;
  double tracedTime;
  Clock lastWorldCollect;

  RingStruct greyRing[RankLIMIT]; /* ring of grey segments at each rank */
  STATISTIC_DECL(Count writeBarrierHitCount) /* write barrier hits */
  RingStruct chainRing;         /* ring of chains */

  struct HistoryStruct historyStruct;
  
  Bool emergency;               /* garbage collect in emergency mode? */

  Word *stackAtArenaEnter;  /* NULL or hot end of client stack, in the thread */
                            /* that then entered the MPS. */

  Sig sig;
} ArenaStruct;


typedef struct AllocPatternStruct {
  char dummy;
} AllocPatternStruct;


#endif /* mpmst_h */


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
