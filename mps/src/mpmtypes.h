/* impl.h.mpmtypes: MEMORY POOL MANAGER TYPES
 *
 * $HopeName: MMsrc!mpmtypes.h(trunk.36) $
 * Copyright (C) 1997 The Harlequin Group Limited.  All rights reserved.
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
typedef unsigned TraceState;		/* design.mps.tracer */
typedef unsigned AccessSet;             /* design.mps.type.access-set */
typedef unsigned Attr;                  /* design.mps.type.attr */
typedef int RootVar;                    /* design.mps.type.rootvar */
typedef unsigned Serial;                /* design.mps.type.serial */
typedef struct RingStruct *Ring;        /* design.mps.ring */
typedef Word *BT;                       /* design.mps.bt */
typedef struct BufferStruct *Buffer;    /* design.mps.buffer */
typedef struct APStruct *AP;            /* design.mps.buffer */
typedef struct FormatStruct *Format;    /* design.mps.format */
typedef struct LDStruct *LD;            /* design.mps.ld */
typedef struct LockStruct *Lock;        /* impl.c.lock* */
typedef struct PoolStruct *Pool;        /* design.mps.pool */
typedef struct PoolClassStruct *PoolClass; /* impl.c.poolclas */
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
typedef struct ActionStruct *Action;	/* design.mps.action */


/* Arena*Method -- see @@@@ */


typedef Res (*ArenaInitMethod)(Arena *arenaReturn, va_list args);
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

typedef unsigned MessageType;           /* design.mps.message */
typedef struct MessageStruct *Message;  /* design.mps.message */
typedef struct MessageClassStruct *MessageClass; /* design.mps.message */


/* Pool*Method -- see design.mps.class-interface */

typedef Res (*PoolInitMethod)(Pool pool, va_list args);
typedef void (*PoolFinishMethod)(Pool pool);
typedef Res (*PoolAllocMethod)(Addr *pReturn, Pool pool, Size size);
typedef void (*PoolFreeMethod)(Pool pool, Addr old, Size size);
typedef Res (*PoolBufferInitMethod)(Pool pool, Buffer buf, va_list args);
typedef void (*PoolBufferFinishMethod)(Pool pool, Buffer buf);
typedef Res (*PoolBufferFillMethod)(Seg *segReturn,
                                    Addr *baseReturn, Addr *limitReturn,
                                    Pool pool, Buffer buffer, Size size);
typedef void (*PoolBufferEmptyMethod)(Pool pool, Buffer buffer);
typedef Res (*PoolDescribeMethod)(Pool pool, mps_lib_FILE *stream);
typedef Res (*PoolTraceBeginMethod)(Pool pool, Trace trace);
typedef Res (*PoolWhitenMethod)(Pool pool, Trace trace, Seg seg);
typedef void (*PoolGreyMethod)(Pool pool, Trace trace, Seg seg);
typedef void (*PoolBlackenMethod)(Pool pool, TraceSet traceSet, Seg seg);
typedef Res (*PoolScanMethod)(ScanState ss, Pool pool, Seg seg);
typedef Res (*PoolFixMethod)(Pool pool, ScanState ss, Seg seg,
                             Ref *refIO);
typedef void (*PoolReclaimMethod)(Pool pool, Trace trace, Seg seg);
typedef double (*PoolBenefitMethod)(Pool pool, Action action);


/* Message*Method -- design.mps.message */

typedef void (*MessageDeleteMethod)(Message message);
typedef void (*MessageFinalizationRefMethod)
  (Ref *refReturn, Space space, Message message);


/* Message Types -- design.mps.message and elsewhere */

typedef struct MessageFinalizationStruct *MessageFinalization;


typedef Res (*PoolActMethod)(Pool pool, Action action);


/* Format*Method -- see design.mps.format-interface */
/* .fmt-methods: These methods must match those defined in the */
/* MPS C Interface.  (See impl.h.mps.fmt-methods.) */

typedef Res  (*FormatScanMethod)   (ScanState ss, Addr base, 
                                    Addr limit);
typedef Addr (*FormatSkipMethod)   (Addr object);
typedef void (*FormatMoveMethod)   (Addr object, Addr to);
typedef Addr (*FormatIsMovedMethod)(Addr object);
typedef void (*FormatCopyMethod)   (Addr object, Addr to);
typedef void (*FormatPadMethod)    (Addr base, Size size);


/* Root*Method -- see design.mps.root-interface */
/* .root-methods: These methods must match those defined in the */
/* MPS C Interface.  (See impl.h.mps.root-methods.) */

typedef Res (*RootScanMethod)   (ScanState ss, void *p, size_t s);
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
#define TraceSetEMPTY	BS_EMPTY(TraceSet) /* design.mps.tracer */
#define RankSetEMPTY	BS_EMPTY(RankSet)
#define RankSetUNIV	((1uL<<RankMAX)-1)
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
#define SegPrefHigh     ((SegPrefKind)0)
#define SegPrefLow      ((SegPrefKind)1)
#define SegPrefRefSet   ((SegPrefKind)2)


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


/* Result Codes -- see design.mps.type.res */
/* These definitions must match impl.h.mps.res. */
/* This is checked by impl.c.mpsi.check. */

enum {
  ResOK = 0,
  ResFAIL,
  ResRESOURCE,
  ResMEMORY,
  ResLIMIT,
  ResUNIMPL,
  ResIO
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
  MessageTypeMAX
};


/* Types for WriteF formats */
/* These should be used with calls to WriteF. */
/* These must be unpromotable types. */

typedef Addr WriteFA;
typedef Pointer WriteFP;
typedef char *WriteFS;
typedef Word WriteFW;
typedef unsigned long WriteFU;
typedef unsigned long WriteFB;
typedef void *(*WriteFF)(void);
typedef int WriteFC; /* Promoted */


/* Event Codes -- see design.mps.telemetry
 *
 * These names are intended to be mnemonic.  They are derived from 
 * selected letters as indicated, using the transliteration in 
 * guide.hex.trans.
 */

                                                   /* EVent ... */
#define EventEventTime      ((EventType)0xEF213E99) /* TIME */
#define EventSpaceCreate    ((EventType)0xEF5BCC6E) /* SPaCe CREate */
#define EventSpaceDestroy   ((EventType)0xEF5BCDE5) /* SPaCe DEStroy */
#define EventPoolInit       ((EventType)0xEFB07141) /* POoL INIt */
#define EventPoolFinish     ((EventType)0xEFB07F14) /* POoL FINish */
#define EventPoolAlloc      ((EventType)0xEFB07A77) /* POoL ALLoc */
#define EventPoolFree       ((EventType)0xEFB07F6E) /* POoL FREe */
#define EventArenaCreate    ((EventType)0xEFA64C6E) /* AReNa CREate */
#define EventArenaDestroy   ((EventType)0xEFA64DE5) /* AReNa DEStroy */
#define EventSegAlloc       ((EventType)0xEF5E9A77) /* SEG ALLoc */
#define EventSegFree        ((EventType)0xEF5E9F6E) /* SEG FREe */
#define EventAMCGenCreate   ((EventType)0xEFA3C94C) /* AMC GeN Create */
#define EventAMCGenDestroy  ((EventType)0xEFA3C94D) /* AMC GeN Destroy */
#define EventAMCInit        ((EventType)0xEFA3C141) /* AMC INIt */
#define EventAMCFinish      ((EventType)0xEFA3CF14) /* AMC FINish */
#define EventAMCBufferInit  ((EventType)0xEFA3CBF1) /* AMC BuFfer Init */
#define EventAMCBufferFill  ((EventType)0xEFA3CBFF) /* AMC BuFfer Fill */
#define EventAMCBufferEmpty ((EventType)0xEFA3CBFE) /* AMC BuFfer Empty */
#define EventAMCTraceBegin  ((EventType)0xEFA3C26B) /* AMC TRace Begin */
#define EventAMCCondemn     ((EventType)0xEFA3CC04) /* AMC CONdemn */
#define EventAMCScanBegin   ((EventType)0xEFA3C5CB) /* AMC SCan Begin */
#define EventAMCScanEnd     ((EventType)0xEFA3C5CE) /* AMC SCan End */
#define EventAMCFix         ((EventType)0xEFA3CF18) /* AMC FIX */
#define EventAMCFixAmbig    ((EventType)0xEFA3CF8A) /* AMC FiX Ambig */
#define EventAMCFixForward  ((EventType)0xEFA3CF8F) /* AMC FiX Forward */
#define EventAMCReclaim     ((EventType)0xEFA3C6EC) /* AMC REClaim */
#define EventAMCTraceEnd    ((EventType)0xEFA3C26E) /* AMC TRace End */
#define EventTraceStart     ((EventType)0xEF26AC52) /* TRACe STart */
#define EventTraceCreate    ((EventType)0xEF26ACC6) /* TRACe CReate */
#define EventTraceDestroy   ((EventType)0xEF26ACDE) /* TRACe DEstroy */
#define EventTraceSegGreyen ((EventType)0xEF26A599) /* TRAce SeG Greyen */
#define EventTraceFlipBegin ((EventType)0xEF26AF7B) /* TRAce FLip Begin */
#define EventTraceFlipEnd   ((EventType)0xEF26AF7E) /* TRAce FLip End */
#define EventTraceReclaim   ((EventType)0xEF26A6EC) /* TRAce REClaim */
#define EventTraceScan      ((EventType)0xEF26AC5C) /* TRACe SCan */
#define EventTraceAccess    ((EventType)0xEF26AACC) /* TRAce ACCess */
#define EventTracePoll      ((EventType)0xEF26AB01) /* TRAce POLl */
#define EventTraceFix       ((EventType)0xEF26AF18) /* TRAce FIX */
#define EventTraceFixSeg    ((EventType)0xEF26AF85) /* TRAce FiX Seg */
#define EventTraceFixWhite  ((EventType)0xEF26AF83) /* TRAce FiX White */
#define EventTraceScanArea  ((EventType)0xEF26A5CA) /* TRAce SCan Area */
#define EventTraceScanAreaTagged ((EventType)0xEF26A5C2) /* TRAce SCan area Tagged */
#define EventVMCreate       ((EventType)0xEFF3C6EA) /* VM CREAte */
#define EventVMDestroy      ((EventType)0xEFF3DE52) /* VM DESTroy */
#define EventVMMap          ((EventType)0xEFF33AB9) /* VM MAP */
#define EventVMUnmap        ((EventType)0xEFF3043B) /* VM UNMaP */

#endif /* mpmtypes_h */
