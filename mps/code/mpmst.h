/* mpmst.h: MEMORY POOL MANAGER DATA STRUCTURES
 *
 * $Id$
 * Copyright (c) 2001-2013 Ravenbrook Limited.  See end of file for license.
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
#include "chain.h"


/* PoolClassStruct -- pool class structure
 *
 * See <design/pool/>.
 *
 * .class: The pool class structure is defined by each pool class
 * implementation in order to provide an interface between the MPM
 * and the class (see <design/class-interface/>) via generic
 * functions (see <code/pool.c>).  A class XXX defines a function
 * PoolClassXXX() returning a PoolClass pointing to a PoolClassStruct
 * of methods which implement the memory management policy.
 *
 * .class.end-sig: The class structure has a signature at the end.  This
 * causes the compiler to complain if the class structure is extended
 * without modifying static initializers.  */

#define PoolClassSig    ((Sig)0x519C7A55) /* SIGnature pool CLASS */

typedef struct mps_class_s {
  ProtocolClassStruct protocol;
  const char *name;             /* class name string */
  size_t size;                  /* size of outer structure */
  size_t offset;                /* offset of generic struct in outer struct */
  Attr attr;                    /* attributes */
  PoolVarargsMethod varargs;    /* convert deprecated varargs into keywords */
  PoolInitMethod init;          /* initialize the pool descriptor */
  PoolFinishMethod finish;      /* finish the pool descriptor */
  PoolAllocMethod alloc;        /* allocate memory from pool */
  PoolFreeMethod free;          /* free memory to pool */
  PoolBufferFillMethod bufferFill;      /* out-of-line reserve */
  PoolBufferEmptyMethod bufferEmpty;    /* out-of-line commit */
  PoolAccessMethod access;      /* handles read/write accesses */
  PoolWhitenMethod whiten;      /* whiten objects in a segment */
  PoolGreyMethod grey;          /* grey non-white objects */
  PoolBlackenMethod blacken;    /* blacken grey objects without scanning */
  PoolScanMethod scan;          /* find references during tracing */
  PoolFixMethod fix;            /* referent reachable during tracing */
  PoolFixEmergencyMethod fixEmergency;  /* as fix, no failure allowed */
  PoolReclaimMethod reclaim;    /* reclaim dead objects after tracing */
  PoolTraceEndMethod traceEnd;  /* do something after all reclaims */
  PoolRampBeginMethod rampBegin;/* begin a ramp pattern */
  PoolRampEndMethod rampEnd;    /* end a ramp pattern */
  PoolFramePushMethod framePush; /* push an allocation frame */
  PoolFramePopMethod framePop;  /* pop an allocation frame */
  PoolFramePopPendingMethod framePopPending;  /* notify pending pop */
  PoolAddrObjectMethod addrObject; /* find client pointer to object */
  PoolWalkMethod walk;          /* walk over a segment */
  PoolFreeWalkMethod freewalk;  /* walk over free blocks */
  PoolBufferClassMethod bufferClass; /* default BufferClass of pool */
  PoolDescribeMethod describe;  /* describe the contents of the pool */
  PoolDebugMixinMethod debugMixin; /* find the debug mixin, if any */
  Bool labelled;                /* whether it has been EventLabelled */
  Sig sig;                      /* .class.end-sig */
} PoolClassStruct;


/* PoolStruct -- generic structure
 *
 * .pool: A generic structure is created when a pool is created and
 * holds the generic part of the pool's state.  Each pool class defines
 * a "subclass" of the pool structure (the "outer structure") which
 * contains PoolStruct as a a field.  The outer structure holds the
 * class-specific part of the pool's state.  See <code/pool.c>,
 * <design/pool/>.  */

#define PoolSig         ((Sig)0x519B0019) /* SIGnature POOL */

typedef struct mps_pool_s {     /* generic structure */
  Sig sig;                      /* <design/sig/> */
  Serial serial;                /* from arena->poolSerial */
  PoolClass class;              /* pool class structure */
  Arena arena;                  /* owning arena */
  RingStruct arenaRing;         /* link in list of pools in arena */
  RingStruct bufferRing;        /* allocation buffers are attached to pool */
  Serial bufferSerial;          /* serial of next buffer */
  RingStruct segRing;           /* segs are attached to pool */
  Align alignment;              /* alignment for units */
  Format format;                /* format only if class->attr&AttrFMT */
  PoolFixMethod fix;            /* fix method */
  double fillMutatorSize;       /* bytes filled, mutator buffers */
  double emptyMutatorSize;      /* bytes emptied, mutator buffers */
  double fillInternalSize;      /* bytes filled, internal buffers */
  double emptyInternalSize;     /* bytes emptied, internal buffers */
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
  Size unitSize;                /* rounded for management purposes */
  Word unitsPerExtent;          /* number of units per arena alloc */
  struct MFSHeaderStruct *freeList; /* head of the free list */
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
  Size space;                   /* total free space in pool */
  Size lost;                    /* <design/poolmv/#lost> */
  RingStruct spans;             /* span chain */
  Sig sig;                      /* <design/sig/> */
} MVStruct;


/* ReservoirStruct -- Reservoir structure
 *
 * .reservoir: See <code/reserv.c>, <design/reservoir/>.
 *
 * The Reservoir structure is declared here because it is in-lined in
 * the arena for storing segments for the low-memory reservoir.  It is
 * implemented as a pool - but doesn't follow the normal pool naming
 * conventions because it's not intended for general use and the use of
 * a pool is an incidental detail.  */

#define ReservoirSig ((Sig)0x5196e599) /* SIGnature REServoir */

typedef struct ReservoirStruct {   /* Reservoir structure */
  PoolStruct poolStruct;        /* generic pool structure */
  Tract reserve;                /* linked list of reserve tracts */
  Size reservoirLimit;          /* desired reservoir size */
  Size reservoirSize;           /* actual reservoir size */
  Sig sig;                      /* <design/sig/> */
} ReservoirStruct;


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

  /* methods specific to MessageTypeFinalization */
  MessageFinalizationRefMethod finalizationRef;       

  /* methods specific to MessageTypeGC */
  MessageGCLiveSizeMethod gcLiveSize;
  MessageGCCondemnedSizeMethod gcCondemnedSize;
  MessageGCNotCondemnedSizeMethod gcNotCondemnedSize;

  /* methods specific to MessageTypeGCStart */
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
  MessageClass class;           /* Message Class Structure */
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
  ProtocolClassStruct protocol;
  const char *name;             /* class name string */
  size_t size;                  /* size of outer structure */
  SegInitMethod init;           /* initialize the segment */
  SegFinishMethod finish;       /* finish the segment */
  SegSetSummaryMethod setSummary; /* set the segment summary  */
  SegBufferMethod buffer;       /* get the segment buffer  */
  SegSetBufferMethod setBuffer; /* set the segment buffer  */
  SegSetGreyMethod setGrey;     /* change greyness of segment */
  SegSetWhiteMethod setWhite;   /* change whiteness of segment */
  SegSetRankSetMethod setRankSet; /* change rank set of segment */
  SegSetRankSummaryMethod setRankSummary; /* change rank set & summary */
  SegDescribeMethod describe;   /* describe the contents of the seg */
  SegMergeMethod merge;         /* merge two adjacent segments */
  SegSplitMethod split;         /* split a segment into two */
  Sig sig;                      /* .class.end-sig */
} SegClassStruct;


/* SegStruct -- segment structure
 *
 * .seg: Segments are the basic units of protection and tracer activity
 * for allocated memory.  See <design/seg/>.  */

#define SegSig      ((Sig)0x5195E999) /* SIGnature SEG  */

typedef struct SegStruct {      /* segment structure */
  Sig sig;                      /* <code/misc.h#sig> */
  SegClass class;               /* segment class structure */
  Tract firstTract;             /* first tract of segment */
  RingStruct poolRing;          /* link in list of segs in pool */
  Addr limit;                   /* limit of segment */
  unsigned depth : ShieldDepthWIDTH; /* see <code/shield.c#def.depth> */
  AccessSet pm : AccessSetWIDTH; /* protection mode, <code/shield.c> */
  AccessSet sm : AccessSetWIDTH; /* shield mode, <code/shield.c> */
  TraceSet grey : TraceLIMIT;   /* traces for which seg is grey */
  TraceSet white : TraceLIMIT;  /* traces for which seg is white */
  TraceSet nailed : TraceLIMIT; /* traces for which seg has nailed objects */
  RankSet rankSet : RankLIMIT;  /* ranks of references in this seg */
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
  Sig sig;                      /* <design/sig/> */
} GCSegStruct;


/* SegPrefStruct -- segment preference structure
 *
 * .seg-pref: arena memory users (pool class code) need a way of
 * expressing preferences about the segments they allocate.
 *
 * .seg-pref.misleading: The name is historical and misleading. SegPref
 * objects need have nothing to do with segments. @@@@ */

#define SegPrefSig      ((Sig)0x5195E9B6) /* SIGnature SEG PRef */

typedef struct SegPrefStruct {  /* segment placement preferences */
  Sig sig;                      /* <code/misc.h#sig> */
  Bool high;                    /* high or low */
  ZoneSet zones;                /* preferred zones */
  Bool isCollected;             /* whether segment will be collected */
  Bool isGen;                   /* whether gen is set */
  Serial gen;                   /* associated geneation */
} SegPrefStruct;


/* BufferClassStruct -- buffer class structure
 *
 * See <design/buffer/> & <design/protocol/>.
 *
 * .buffer.class: The buffer class structure is defined by each buffer
 * class implementation in order to provide a generic interface to
 * buffers.  */

#define BufferClassSig    ((Sig)0x519B0FC7) /* SIGnature BUFfer CLass */

typedef struct BufferClassStruct {
  ProtocolClassStruct protocol;
  const char *name;             /* class name string */
  size_t size;                  /* size of outer structure */
  BufferVarargsMethod varargs;  /* parse obsolete varargs */
  BufferInitMethod init;        /* initialize the buffer */
  BufferFinishMethod finish;    /* finish the buffer */
  BufferAttachMethod attach;    /* attach the buffer */
  BufferDetachMethod detach;    /* detach the buffer */
  BufferDescribeMethod describe;/* describe the contents of the buffer */
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
  Sig sig;                      /* <design/sig/> */
  BufferClass class;            /* buffer class structure */
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
  Align alignment;              /* alignment of formatted objects */
  mps_fmt_scan_t scan;
  mps_fmt_skip_t skip;
  mps_fmt_fwd_t move;
  mps_fmt_isfwd_t isMoved;
  mps_fmt_pad_t pad;
  mps_fmt_class_t class;        /* pointer indicating class */
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
  STATISTIC_DECL(Count fixRefCount); /* refs which pass zone check */
  STATISTIC_DECL(Count segRefCount); /* refs which refer to segs */
  STATISTIC_DECL(Count whiteSegRefCount); /* refs which refer to white segs */
  STATISTIC_DECL(Count nailCount); /* segments nailed by ambig refs */
  STATISTIC_DECL(Count snapCount); /* refs snapped to forwarded objs */
  STATISTIC_DECL(Count forwardedCount); /* objects preserved by moving */
  Size forwardedSize;           /* bytes preserved by moving */
  STATISTIC_DECL(Count preservedInPlaceCount); /* objects preserved in place */
  Size preservedInPlaceSize;    /* bytes preserved in place */
  STATISTIC_DECL(Size copiedSize); /* bytes copied */
  STATISTIC_DECL(Size scannedSize); /* bytes scanned */
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
  STATISTIC_DECL(Size preTraceArenaReserved); /* ArenaReserved before this trace */
  Size condemned;               /* condemned bytes */
  Size notCondemned;            /* collectable but not condemned */
  Size foundation;              /* initial grey set size */
  Size rate;                    /* segs to scan per increment */
  STATISTIC_DECL(Count greySegCount); /* number of grey segs */
  STATISTIC_DECL(Count greySegMax); /* max number of grey segs */
  STATISTIC_DECL(Count rootScanCount); /* number of roots scanned */
  Count rootScanSize;           /* total size of scanned roots */
  Size rootCopiedSize;          /* bytes copied by scanning roots */
  STATISTIC_DECL(Count segScanCount); /* number of segs scanned */
  Count segScanSize;            /* total size of scanned segments */
  Size segCopiedSize;           /* bytes copied by scanning segments */
  STATISTIC_DECL(Count singleScanCount); /* number of single refs scanned */
  STATISTIC_DECL(Count singleScanSize); /* total size of single refs scanned */
  STATISTIC_DECL(Size singleCopiedSize); /* bytes copied by scanning single refs */
  STATISTIC_DECL(Count fixRefCount); /* refs which pass zone check */
  STATISTIC_DECL(Count segRefCount); /* refs which refer to segs */
  STATISTIC_DECL(Count whiteSegRefCount); /* refs which refer to white segs */
  STATISTIC_DECL(Count nailCount); /* segments nailed by ambig refs */
  STATISTIC_DECL(Count snapCount); /* refs snapped to forwarded objs */
  STATISTIC_DECL(Count readBarrierHitCount); /* read barrier faults */
  STATISTIC_DECL(Count pointlessScanCount); /* pointless seg scans */
  STATISTIC_DECL(Count forwardedCount); /* objects preserved by moving */
  Size forwardedSize;           /* bytes preserved by moving */
  STATISTIC_DECL(Count preservedInPlaceCount); /* objects preserved in place */
  Size preservedInPlaceSize;    /* bytes preserved in place */
  STATISTIC_DECL(Count reclaimCount); /* segments reclaimed */
  STATISTIC_DECL(Count reclaimSize); /* bytes reclaimed */
} TraceStruct;


/* ChunkCacheEntryStruct -- cache entry in the chunk cache */

#define ChunkCacheEntrySig ((Sig)0x519C80CE) /* SIGnature CHUnk Cache Entry */

typedef struct ChunkCacheEntryStruct {
  Sig sig;
  Chunk chunk;
  Addr base;
  Addr limit;
} ChunkCacheEntryStruct;


/* ArenaClassStruct -- generic arena class interface */

#define ArenaClassSig   ((Sig)0x519A6C1A) /* SIGnature ARena CLAss */

typedef struct mps_arena_class_s {
  ProtocolClassStruct protocol;
  const char *name;             /* class name string */
  size_t size;                  /* size of outer structure */
  size_t offset;                /* offset of generic struct in outer struct */
  ArenaVarargsMethod varargs;
  ArenaInitMethod init;
  ArenaFinishMethod finish;
  ArenaReservedMethod reserved;
  ArenaSpareCommitExceededMethod spareCommitExceeded;
  ArenaExtendMethod extend;
  ArenaAllocMethod alloc;
  ArenaFreeMethod free;
  ArenaChunkInitMethod chunkInit;
  ArenaChunkFinishMethod chunkFinish;
  ArenaCompactMethod compact;
  ArenaDescribeMethod describe;
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
} GlobalsStruct;


/* ArenaStruct -- generic arena
 *
 * See <code/arena.c>.  */

#define ArenaSig        ((Sig)0x519A6E4A) /* SIGnature ARENA */

typedef struct mps_arena_s {
  GlobalsStruct globals; /* must be first, see <design/arena/#globals> */
  Serial serial;

  ArenaClass class;             /* arena class structure */

  Bool poolReady;               /* <design/arena/#pool.ready> */
  MVStruct controlPoolStruct;   /* <design/arena/#pool> */

  ReservoirStruct reservoirStruct; /* <design/reservoir/> */

  Size committed;               /* amount of committed RAM */
  Size commitLimit;             /* client-configurable commit limit */

  Size spareCommitted;          /* Amount of memory in hysteresis fund */
  Size spareCommitLimit;        /* Limit on spareCommitted */

  Shift zoneShift;              /* see also <code/ref.c> */
  Align alignment;              /* minimum alignment of tracts */

  Tract lastTract;              /* most recently allocated tract */
  Addr lastTractBase;           /* base address of lastTract */

  Chunk primary;                /* the primary chunk */
  RingStruct chunkRing;         /* all the chunks */
  Serial chunkSerial;           /* next chunk number */
  ChunkCacheEntryStruct chunkCache; /* just one entry */

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
  Serial threadSerial;          /* serial of next thread */
 
  /* shield fields (<code/shield.c>) */
  Bool insideShield;             /* TRUE if and only if inside shield */
  Seg shCache[ShieldCacheSIZE];  /* Cache of unsynced segs */
  Size shCacheI;                 /* index into cache */
  Size shCacheLimit;             /* High water mark for cache usage */
  Size shDepth;                  /* sum of depths of all segs */
  Bool suspended;                /* TRUE iff mutator suspended */

  /* trace fields (<code/trace.c>) */
  TraceSet busyTraces;          /* set of running traces */
  TraceSet flippedTraces;       /* set of running and flipped traces */
  TraceStruct trace[TraceLIMIT]; /* trace structures.  See
                                   <design/trace/#intance.limit> */

  /* trace ancillary fields (<code/traceanc.c>) */
  TraceStartMessage tsMessage[TraceLIMIT];  /* <design/message-gc/> */
  TraceMessage tMessage[TraceLIMIT];  /* <design/message-gc/> */

  /* policy fields */
  double tracedSize;
  double tracedTime;
  Clock lastWorldCollect;

  RingStruct greyRing[RankLIMIT]; /* ring of grey segments at each rank */
  STATISTIC_DECL(Count writeBarrierHitCount); /* write barrier hits */
  RingStruct chainRing;         /* ring of chains */

  /* location dependency fields (<code/ld.c>) */
  Epoch epoch;                     /* <design/arena/#ld.epoch> */
  RefSet prehistory;               /* <design/arena/#ld.prehistory> */
  RefSet history[LDHistoryLENGTH]; /* <design/arena/#ld.history> */

  Bool emergency;               /* garbage collect in emergency mode? */

  Addr *stackAtArenaEnter;  /* NULL or top of client stack, in the thread */
                            /* that then entered the MPS. */

  Sig sig;
} ArenaStruct;


typedef struct AllocPatternStruct {
  char dummy;
} AllocPatternStruct;


#endif /* mpmst_h */


/* C. COPYRIGHT AND LICENSE
 *
 * Copyright (C) 2001-2013 Ravenbrook Limited <http://www.ravenbrook.com/>.
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
