/* impl.h.mpmtypes: MEMORY POOL MANAGER TYPES
 *
 * $HopeName: MMsrc!mpmtypes.h(trunk.69) $
 * Copyright (C) 1997, 1998 Harlequin Group plc.  All rights reserved.
 *
 * .readership: MM developers.
 * .design: design.mps.type
 *
 * .rationale: Types and type constants are almost all defined
 * in this header, in advance of any declarations of prototypes
 * or structures.  This avoids difficulties in defining recursive
 * data structures, and also provides a nice browsable list of
 * types.
 */

#ifndef mpmtypes_h
#define mpmtypes_h

#include "config.h"     /* this must come first: it defines target */
                        /* options */
#include "misc.h"       /* miscellaneous non-specific bits and bobs */
#include "mpslib.h"


/* TYPES */

typedef unsigned long Sig;              /* design.mps.sig */
typedef int Bool;                       /* design.mps.type.bool */
typedef int Res;                        /* design.mps.type.res */
typedef void (*Fun)(void);              /* design.mps.type.fun */
typedef MPS_T_WORD Word;                /* design.mps.type.word */
typedef unsigned char Byte;             /* design.mps.type.byte */
typedef struct AddrStruct *Addr;        /* design.mps.type.addr */
typedef Word Size;                      /* design.mps.type.size */
typedef Word Count;                     /* design.mps.type.count */
typedef Word Index;                     /* design.mps.type.index */
typedef Word Align;                     /* design.mps.type.align */
typedef unsigned Shift;                 /* design.mps.type.shift */
typedef Addr Ref;                       /* design.mps.type.ref */
typedef void *Pointer;                  /* design.mps.type.pointer */
typedef Word RefSet;                    /* design.mps.refset */
typedef unsigned Rank;                  /* design.mps.ref */
typedef unsigned RankSet;
typedef unsigned RootMode;
typedef Size Epoch;                     /* design.mps.ld */
typedef unsigned TraceId;               /* design.mps.tracer */
typedef unsigned TraceSet;              /* design.mps.tracer */
typedef unsigned TraceState;            /* design.mps.tracer */
typedef unsigned AccessSet;             /* design.mps.type.access-set */
typedef unsigned Attr;                  /* design.mps.type.attr */
typedef unsigned FormatVariety;         
typedef int RootVar;                    /* design.mps.type.rootvar */
typedef unsigned Serial;                /* design.mps.type.serial */
typedef struct RingStruct *Ring;        /* design.mps.ring */
typedef Word *BT;                       /* design.mps.bt */
typedef struct BufferStruct *Buffer;    /* design.mps.buffer */
typedef unsigned BufferMode;            /* design.mps.buffer */
typedef unsigned FrameState;            /* design.mps.alloc-frame */
typedef struct APStruct *AP;            /* design.mps.buffer */
typedef struct FormatStruct *Format;    /* design.mps.format */
typedef struct LDStruct *LD;            /* design.mps.ld */
typedef struct LockStruct *Lock;        /* impl.c.lock* */
typedef struct PoolStruct *Pool;        /* design.mps.pool */
typedef struct PoolClassStruct *PoolClass; /* impl.c.poolclas */
typedef PoolClass AbstractPoolClass;    /* impl.c.poolabs */
typedef PoolClass AbstractAllocFreePoolClass; /* impl.c.poolabs */
typedef PoolClass AbstractBufferPoolClass; /* impl.c.poolabs */
typedef PoolClass AbstractScanPoolClass; /* impl.c.poolabs */
typedef PoolClass AbstractCollectPoolClass; /* impl.c.poolabs */
typedef struct TraceStruct *Trace;      /* design.mps.tracer */
typedef struct ScanStateStruct *ScanState; /* design.mps.tracer */
typedef struct SegStruct *Seg;          /* impl.c.seg */
typedef struct SegPrefStruct *SegPref;  /* design.mps.pref, impl.c.arena* */
typedef int SegPrefKind;                /* design.mps.pref, impl.c.arena* */
typedef struct ArenaClassStruct *ArenaClass; /* design.mps.arena */
typedef struct ArenaStruct *Arena;      /* design.mps.arena */
typedef Arena Space;                    /* until all files have been updated */
typedef struct VMStruct *VM;            /* impl.c.vm* */
typedef struct RootStruct *Root;        /* impl.c.root */
typedef struct ThreadStruct *Thread;    /* impl.c.th* */
typedef struct ActionStruct *Action;    /* design.mps.action */
typedef struct MutatorFaultContextStruct
        *MutatorFaultContext;           /* design.mps.prot */
typedef struct PoolDebugMixinStruct *PoolDebugMixin;
typedef struct AllocPatternStruct *AllocPattern;
typedef struct AllocFrameStruct *AllocFrame; /* design.mps.alloc-frame */


/* Splay* -- See design.mps.splay */

typedef struct SplayTreeStruct *SplayTree;
typedef struct SplayNodeStruct *SplayNode;
typedef unsigned Compare;
typedef Compare (*SplayCompareMethod)(void *key, SplayNode node);
typedef Bool (*SplayTestNodeMethod)(SplayTree tree, SplayNode node,
                                    void *closureP, unsigned long closureS);
typedef Bool (*SplayTestTreeMethod)(SplayTree tree, SplayNode node,
                                    void *closureP, unsigned long closureS);
typedef void (*SplayUpdateNodeMethod)(SplayTree tree, SplayNode node,
                                      SplayNode leftChild, 
                                      SplayNode rightChild);
typedef Res (*SplayNodeDescribeMethod)(SplayNode node, mps_lib_FILE *stream);
enum {
  CompareLESS,
  CompareEQUAL,
  CompareGREATER
};


/* CBS* -- See design.mps.cbs */

typedef struct CBSStruct *CBS;
typedef struct CBSBlockStruct *CBSBlock;
typedef void (*CBSChangeSizeMethod)(CBS cbs, CBSBlock block,
              Size oldSize, Size newSize);
typedef Bool (*CBSIterateMethod)(CBS cbs, CBSBlock block,
                                 void *closureP, unsigned long closureS);
typedef unsigned CBSFindDelete;
enum {
  CBSFindDeleteNONE,    /* don't delete after finding */
  CBSFindDeleteLOW,     /* delete precise size from low end */
  CBSFindDeleteHIGH,    /* delete precise size from high end */
  CBSFindDeleteENTIRE   /* delete entire range */
};

/* See design.mps.cbs.impl.low-mem.inline.block */
typedef void **CBSEmergencyBlock; /* next, limit */

/* See design.mps.cbs.impl.low-mem.inline.block */
typedef void **CBSEmergencyGrain; /* next */



/* Arena*Method -- see @@@@ */

typedef Res (*ArenaInitMethod)(Arena *arenaReturn,
                               ArenaClass class, va_list args);
typedef void (*ArenaFinishMethod)(Arena arena);
typedef Size (*ArenaReservedMethod)(Arena arena);
typedef Size (*ArenaCommittedMethod)(Arena arena);
typedef Res (*ArenaExtendMethod)(Arena arena, Addr base, Size size);
typedef Res (*ArenaRetractMethod)(Arena arena, Addr base, Size size);
typedef Bool (*ArenaIsReservedAddrMethod)(Arena arena, Addr addr);
typedef Res (*ArenaSegAllocMethod)(Seg *segReturn, SegPref pref,
                                   Size size, Pool pool);
typedef void (*ArenaSegFreeMethod)(Seg seg);
typedef Addr (*ArenaSegBaseMethod)(Seg seg);
typedef Addr (*ArenaSegLimitMethod)(Seg seg);
typedef Size (*ArenaSegSizeMethod)(Seg seg);
typedef Bool (*ArenaSegOfAddrMethod)(Seg *segReturn, Arena arena, Addr addr);
typedef Bool (*ArenaSegFirstMethod)(Seg *segReturn, Arena arena);
typedef Bool (*ArenaSegNextMethod)(Seg *segReturn, Arena arena, Addr addr);
typedef Res (*ArenaDescribeMethod)(Arena arena, mps_lib_FILE *stream);


/* Messages
 *
 * See design.mps.message */
typedef unsigned MessageType;
typedef struct MessageStruct *Message;
typedef struct MessageClassStruct *MessageClass;


/* TraceFixMethod */

typedef Res (*TraceFixMethod)(ScanState ss, Ref *refIO);


/* Heap Walker */

/* This type is used by the PoolClass method Walk */
typedef void (*FormattedObjectsStepMethod)(Addr, Format, Pool,
                                           void *, Size);


/* Pool*Method -- see design.mps.class-interface */

/* Order of types corresponds to PoolClassStruct in impl.h.mpmst */

typedef Res (*PoolInitMethod)(Pool pool, va_list args);
typedef void (*PoolFinishMethod)(Pool pool);
typedef Res (*PoolAllocMethod)(Addr *pReturn, Pool pool, Size size,
                               Bool withReservoirPermit);
typedef void (*PoolFreeMethod)(Pool pool, Addr old, Size size);
typedef Res (*PoolBufferInitMethod)(Pool pool, Buffer buf, va_list args);
typedef Res (*PoolBufferFillMethod)(Seg *segReturn,
                                    Addr *baseReturn, Addr *limitReturn,
                                    Pool pool, Buffer buffer, Size size,
                                    Bool withReservoirPermit);
typedef void (*PoolBufferEmptyMethod)(Pool pool, Buffer buffer);
typedef void (*PoolBufferFinishMethod)(Pool pool, Buffer buf);
typedef Res (*PoolTraceBeginMethod)(Pool pool, Trace trace);
typedef Res (*PoolAccessMethod)(Pool pool, Seg seg, Addr addr,
                                AccessSet mode, MutatorFaultContext context);
typedef Res (*PoolWhitenMethod)(Pool pool, Trace trace, Seg seg);
typedef void (*PoolGreyMethod)(Pool pool, Trace trace, Seg seg);
typedef void (*PoolBlackenMethod)(Pool pool, TraceSet traceSet, Seg seg);
typedef Res (*PoolScanMethod)(Bool *totalReturn, ScanState ss,
                              Pool pool, Seg seg);
typedef Res (*PoolFixMethod)(Pool pool, ScanState ss, Seg seg,
                             Ref *refIO);
typedef Res (*PoolFixEmergencyMethod)(Pool pool, ScanState ss,
                                      Seg seg, Ref *refIO);
typedef void (*PoolReclaimMethod)(Pool pool, Trace trace, Seg seg);
typedef double (*PoolBenefitMethod)(Pool pool, Action action);
typedef Res (*PoolActMethod)(Pool pool, Action action);
typedef void (*PoolRampBeginMethod)(Pool pool, Buffer buf, Bool collectAll);
typedef void (*PoolRampEndMethod)(Pool pool, Buffer buf);
typedef Res (*PoolFramePushMethod)(AllocFrame *frameReturn, 
                                   Pool pool, Buffer buf);
typedef Res (*PoolFramePopMethod)(Pool pool, Buffer buf,
                                  AllocFrame frame);
typedef void (*PoolFramePopPendingMethod)(Pool pool, Buffer buf,
                                          AllocFrame frame);
typedef void (*PoolWalkMethod)(Pool pool, Seg seg,
                               FormattedObjectsStepMethod f,
                               void *p, unsigned long s);
typedef Res (*PoolDescribeMethod)(Pool pool, mps_lib_FILE *stream);
typedef PoolDebugMixin (*PoolDebugMixinMethod)(Pool pool);


/* Message*Method -- design.mps.message */

typedef void (*MessageDeleteMethod)(Message message);
typedef void (*MessageFinalizationRefMethod)
  (Ref *refReturn, Arena arena, Message message);
typedef Size (*MessageGCLiveSizeMethod)(Message message);
typedef Size (*MessageGCCondemnedSizeMethod)(Message message);
typedef Size (*MessageGCNotCondemnedSizeMethod)(Message message);


/* Message Types -- design.mps.message and elsewhere */

typedef struct MessageFinalizationStruct *MessageFinalization;


/* Format*Method -- see design.mps.format-interface */
/* .fmt-methods: These methods must match those defined in the */
/* MPS C Interface.  (See impl.h.mps.fmt-methods.) */

typedef Res  (*FormatScanMethod)(ScanState ss, Addr base, Addr limit);
typedef Addr (*FormatSkipMethod)(Addr object);
typedef void (*FormatMoveMethod)(Addr object, Addr to);
typedef Addr (*FormatIsMovedMethod)(Addr object);
typedef void (*FormatCopyMethod)(Addr object, Addr to);
typedef void (*FormatPadMethod)(Addr base, Size size);
typedef Addr (*FormatClassMethod)(Addr object);


/* Root*Method -- see design.mps.root-interface */
/* .root-methods: These methods must match those defined in the */
/* MPS C Interface.  (See impl.h.mps.root-methods.) */

typedef Res (*RootScanMethod)(ScanState ss, void *p, size_t s);
typedef Res (*RootScanRegMethod)(ScanState ss, Thread thread, void *p, 
                                 size_t s);


/* CONSTANTS */

/* design.mps.sig SIGnature IS BAD */
#define SigInvalid      ((Sig)0x51915BAD) 

#define AccessSetEMPTY  ((AccessSet)0) /* design.mps.type.access-set */
#define AccessREAD      ((AccessSet)(1<<0))
#define AccessWRITE     ((AccessSet)(1<<1))
#define AccessMAX       ((Size)2)
#define RingNONE        ((Ring)0)       /* design.mps.ring */
#define TraceIdNONE     ((TraceId)-1)   /* design.mps.tracer */
#define RefSetEMPTY     BS_EMPTY(RefSet)
#define RefSetUNIV      BS_UNIV(RefSet)
#define TraceSetEMPTY   BS_EMPTY(TraceSet) /* design.mps.tracer */
#define RankSetEMPTY    BS_EMPTY(RankSet)
#define RankSetUNIV     ((1uL<<RankMAX)-1)
#define AttrFMT         ((Attr)(1<<0))  /* design.mps.type.attr */
#define AttrSCAN        ((Attr)(1<<1))
#define AttrPM_NO_READ  ((Attr)(1<<2))
#define AttrPM_NO_WRITE ((Attr)(1<<3))
#define AttrALLOC       ((Attr)(1<<4))
#define AttrFREE        ((Attr)(1<<5))
#define AttrBUF         ((Attr)(1<<6))
#define AttrBUF_RESERVE ((Attr)(1<<7))
#define AttrBUF_ALLOC   ((Attr)(1<<8))
#define AttrGC          ((Attr)(1<<9))
#define AttrINCR_RB     ((Attr)(1<<10))
#define AttrINCR_WB     ((Attr)(1<<11))
#define AttrMOVINGGC    ((Attr)(1<<12))
#define AttrMASK        (AttrFMT | AttrSCAN | AttrPM_NO_READ | \
                         AttrPM_NO_WRITE | AttrALLOC | AttrFREE | \
                         AttrBUF | AttrBUF_RESERVE | AttrBUF_ALLOC | \
                         AttrGC | AttrINCR_RB | AttrINCR_WB | AttrMOVINGGC)
#define FormatVarietyA  ((FormatVariety)0)
#define FormatVarietyB  ((FormatVariety)1)
#define SegPrefHigh     ((SegPrefKind)0)
#define SegPrefLow      ((SegPrefKind)1)
#define SegPrefRefSet   ((SegPrefKind)2)
#define SegPrefGen      ((SegPrefKind)3)
#define SegPrefCollected  ((SegPrefKind)4)

/* Buffer modes */
#define BufferModeATTACHED      ((BufferMode)(1<<0))
#define BufferModeFLIPPED       ((BufferMode)(1<<1))
#define BufferModeLOGGED        ((BufferMode)(1<<2))

/* Buffer frame states. See design.mps.alloc-frame.lw-frame.states */
enum {
  BufferFrameVALID,
  BufferFramePOP_PENDING,
  BufferFrameDISABLED,
  BufferFrameMAX
};


/* Rank constants -- see design.mps.type.rank */
/* These definitions must match impl.h.mps.rank. */
/* This is checked by impl.c.mpsi.check. */

enum {
  RankAMBIG = 0,
  RankEXACT = 1,
  RankFINAL = 2,
  RankWEAK = 3,
  RankMAX
};


/* Root Modes -- not implemented */
/* .rm: Synchronize with impl.h.mps.rm. */
/* This comment exists as a placeholder for when root modes are */
/* implemented. */

#define RootModeCONSTANT          ((RootMode)1<<0)
#define RootModePROTECTABLE       ((RootMode)1<<1)
#define RootModePROTECTABLE_INNER ((RootMode)1<<2)


/* Root Variants -- see design.mps.type.rootvar
 *
 * .rootvar: Synchonize with impl.c.root.rootvarcheck
 */

enum {
  RootFUN,
  RootTABLE,
  RootTABLE_MASKED,
  RootREG,
  RootFMT
};

/* Boolean Constants -- see design.mps.type.bool */

enum {
  FALSE = 0,
  TRUE = 1
};


/* .result-codes: Result Codes -- see design.mps.type.res */
/* These definitions must match impl.h.mps.result-codes. */
/* This is checked by impl.c.mpsi.check.rc. */
/* Changing this list entails changing the list in */
/* impl.h.mps.result-codes and the check in impl.c.mpsi.check.rc */

enum {
  ResOK = 0,
  ResFAIL,
  ResRESOURCE,
  ResMEMORY,
  ResLIMIT,
  ResUNIMPL,
  ResIO,
  ResCOMMIT_LIMIT,
  ResPARAM
};


/* TraceStates -- see design.mps.tracer */

enum {
  TraceINIT,
  TraceUNFLIPPED,
  TraceFLIPPED,
  TraceRECLAIM,
  TraceFINISHED
};

/* CheckLevel's --- Used to control check method behaviour */
enum {
  CheckNONE = 0,
  CheckSHALLOW = 1,
  CheckDEEP = 2
};

/* MessageTypes -- see design.mps.message */
/* .message.types: Keep in sync with impl.h.mps.message.types */

enum {
  MessageTypeFinalization,
  MessageTypeGC,
  MessageTypeMAX
};


/* Types for WriteF formats */
/* These should be used with calls to WriteF. */
/* These must be unpromotable types. */

typedef Addr WriteFA;
typedef Pointer WriteFP;
typedef const char *WriteFS;
typedef Word WriteFW;
typedef unsigned long WriteFU;
typedef unsigned long WriteFB;
typedef void *(*WriteFF)(void);
typedef int WriteFC; /* Promoted */
typedef double WriteFD;


#endif /* mpmtypes_h */
