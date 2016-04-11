/* mpmtypes.h: MEMORY POOL MANAGER TYPES
 *
 * $Id$
 * Copyright (c) 2001-2014 Ravenbrook Limited.  See end of file for license.
 * Portions copyright (c) 2001 Global Graphics Software.
 *
 * .design: <design/type/>
 *
 * .rationale: Types and type constants are almost all defined
 * in this header, in advance of any declarations of prototypes
 * or structures.  This avoids difficulties in defining recursive
 * data structures.
 */

#ifndef mpmtypes_h
#define mpmtypes_h

#include "config.h"     /* this must come first: it defines target options */
#include "misc.h"       /* miscellaneous non-specific bits and bobs */
#include "mpslib.h"
#include "mpstd.h"      /* for MPS_T_ULONGEST */

#include <stdarg.h>
#include <stddef.h>


/* TYPES */

typedef unsigned long Sig;              /* <design/sig/> */
typedef int Res;                        /* <design/type/#res> */

typedef void (*Fun)(void);              /* <design/type/#fun> */
typedef MPS_T_WORD Word;                /* <design/type/#word> */
typedef unsigned char Byte;             /* <design/type/#byte> */
typedef struct AddrStruct *Addr;        /* <design/type/#addr> */
typedef const struct AddrStruct *ReadonlyAddr; /* <design/type/#readonlyaddr> */
typedef Word Size;                      /* <design/type/#size> */
typedef Word Count;                     /* <design/type/#count> */
typedef Word Index;                     /* <design/type/#index> */
typedef Word Align;                     /* <design/type/#align> */
typedef Word Work;                      /* <design/type/#work> */
typedef unsigned Shift;                 /* <design/type/#shift> */
typedef unsigned Serial;                /* <design/type/#serial> */
typedef Addr Ref;                       /* <design/type/#ref> */
typedef void *Pointer;                  /* <design/type/#pointer> */
typedef Word Clock;                     /* <design/type/#clock> */
typedef MPS_T_ULONGEST ULongest;        /* <design/type/#ulongest> */

typedef mps_arg_s ArgStruct;
typedef mps_arg_s *Arg;
typedef mps_arg_s *ArgList;
typedef mps_key_t Key;

typedef Word RefSet;                    /* design.mps.refset */
typedef Word ZoneSet;                   /* design.mps.refset */
typedef unsigned Rank;
typedef unsigned RankSet;
typedef unsigned RootMode;
typedef Size Epoch;                     /* design.mps.ld */
typedef unsigned TraceId;               /* <design/trace/> */
typedef unsigned TraceSet;              /* <design/trace/> */
typedef unsigned TraceState;            /* <design/trace/> */
typedef unsigned AccessSet;             /* <design/type/#access-set> */
typedef unsigned Attr;                  /* <design/type/#attr> */
typedef int RootVar;                    /* <design/type/#rootvar> */

typedef Word *BT;                       /* <design/bt/> */
typedef struct BootBlockStruct *BootBlock; /* <code/boot.c> */
typedef struct BufferStruct *Buffer;    /* <design/buffer/> */
typedef struct SegBufStruct *SegBuf;    /* <design/buffer/> */
typedef struct BufferClassStruct *BufferClass; /* <design/buffer/> */
typedef BufferClass SegBufClass;        /* <design/buffer/> */
typedef BufferClass RankBufClass;       /* <design/buffer/> */
typedef unsigned BufferMode;            /* <design/buffer/> */
typedef unsigned FrameState;            /* <design/alloc-frame/> */
typedef struct mps_fmt_s *Format;       /* design.mps.format */
typedef struct LockStruct *Lock;        /* <code/lock.c>* */
typedef struct mps_pool_s *Pool;        /* <design/pool/> */
typedef struct mps_pool_class_s *PoolClass;  /* <code/poolclas.c> */
typedef PoolClass AbstractPoolClass;    /* <code/poolabs.c> */
typedef PoolClass AbstractBufferPoolClass; /* <code/poolabs.c> */
typedef PoolClass AbstractSegBufPoolClass; /* <code/poolabs.c> */
typedef PoolClass AbstractScanPoolClass; /* <code/poolabs.c> */
typedef PoolClass AbstractCollectPoolClass; /* <code/poolabs.c> */
typedef struct TraceStruct *Trace;      /* <design/trace/> */
typedef struct ScanStateStruct *ScanState; /* <design/trace/> */
typedef struct mps_chain_s *Chain;      /* <design/trace/> */
typedef struct TractStruct *Tract;      /* <design/arena/> */
typedef struct ChunkStruct *Chunk;      /* <code/tract.c> */
typedef struct ChunkCacheEntryStruct *ChunkCacheEntry; /* <code/tract.c> */
typedef union PageUnion *Page;          /* <code/tract.c> */
typedef struct SegStruct *Seg;          /* <code/seg.c> */
typedef struct GCSegStruct *GCSeg;      /* <code/seg.c> */
typedef struct SegClassStruct *SegClass; /* <code/seg.c> */
typedef SegClass GCSegClass;            /* <code/seg.c> */
typedef struct LocusPrefStruct *LocusPref; /* <design/locus/>, <code/locus.c> */
typedef int LocusPrefKind;              /* <design/locus/>, <code/locus.c> */
typedef struct mps_arena_class_s *ArenaClass; /* <design/arena/> */
typedef ArenaClass AbstractArenaClass;  /* <code/arena.c> */
typedef struct mps_arena_s *Arena;      /* <design/arena/> */
typedef struct GlobalsStruct *Globals;  /* <design/arena/> */
typedef struct VMStruct *VM;            /* <code/vm.c>* */
typedef struct RootStruct *Root;        /* <code/root.c> */
typedef struct mps_thr_s *Thread;       /* <code/th.c>* */
typedef struct MutatorFaultContextStruct
        *MutatorFaultContext;           /* <design/prot/> */
typedef struct PoolDebugMixinStruct *PoolDebugMixin;
typedef struct AllocPatternStruct *AllocPattern;
typedef struct AllocFrameStruct *AllocFrame; /* <design/alloc-frame/> */
typedef struct StackContextStruct *StackContext;
typedef struct RangeStruct *Range;      /* <design/range/> */
typedef struct LandStruct *Land;        /* <design/land/> */
typedef struct LandClassStruct *LandClass; /* <design/land/> */
typedef unsigned FindDelete;            /* <design/land/> */
typedef struct ShieldStruct *Shield; /* design.mps.shield */


/* Arena*Method -- see <code/mpmst.h#ArenaClassStruct> */

typedef void (*ArenaVarargsMethod)(ArgStruct args[], va_list varargs);
typedef Res (*ArenaInitMethod)(Arena *arenaReturn,
                               ArenaClass class, ArgList args);
typedef void (*ArenaFinishMethod)(Arena arena);
typedef Size (*ArenaPurgeSpareMethod)(Arena arena, Size size);
typedef Res (*ArenaExtendMethod)(Arena arena, Addr base, Size size);
typedef Res (*ArenaGrowMethod)(Arena arena, LocusPref pref, Size size);
typedef void (*ArenaFreeMethod)(Addr base, Size size, Pool pool);
typedef Res (*ArenaChunkInitMethod)(Chunk chunk, BootBlock boot);
typedef void (*ArenaChunkFinishMethod)(Chunk chunk);
typedef void (*ArenaCompactMethod)(Arena arena, Trace trace);
typedef Res (*ArenaDescribeMethod)(Arena arena, mps_lib_FILE *stream, Count depth);
typedef Res (*ArenaPagesMarkAllocatedMethod)(Arena arena, Chunk chunk,
                                             Index baseIndex, Count pages,
                                             Pool pool);


/* These are not generally exposed and public, but are part of a commercial
   extension to the MPS. */
typedef void (*ArenaVMExtendedCallback)(Arena arena, Addr base, Size size);
typedef void (*ArenaVMContractedCallback)(Arena arena, Addr base, Size size);


/* TraceFixMethod */

typedef Res (*TraceFixMethod)(ScanState ss, Ref *refIO);


/* Heap Walker */

/* This type is used by the PoolClass method Walk */
typedef void (*FormattedObjectsVisitor)(Addr obj, Format fmt, Pool pool,
                                           void *v, size_t s);

/* This type is used by the PoolClass method Walk */
typedef void (*FreeBlockVisitor)(Addr base, Addr limit, Pool pool, void *p);


/* Seg*Method -- see <design/seg/> */

typedef Res (*SegInitMethod)(Seg seg, Pool pool, Addr base, Size size,
                             ArgList args);
typedef void (*SegFinishMethod)(Seg seg);
typedef void (*SegSetGreyMethod)(Seg seg, TraceSet grey);
typedef void (*SegSetWhiteMethod)(Seg seg, TraceSet white);
typedef void (*SegSetRankSetMethod)(Seg seg, RankSet rankSet);
typedef void (*SegSetRankSummaryMethod)(Seg seg, RankSet rankSet,
                                        RefSet summary);
typedef void (*SegSetSummaryMethod)(Seg seg, RefSet summary);
typedef Buffer (*SegBufferMethod)(Seg seg);
typedef void (*SegSetBufferMethod)(Seg seg, Buffer buffer);
typedef Res (*SegDescribeMethod)(Seg seg, mps_lib_FILE *stream, Count depth);
typedef Res (*SegMergeMethod)(Seg seg, Seg segHi,
                              Addr base, Addr mid, Addr limit);
typedef Res (*SegSplitMethod)(Seg seg, Seg segHi,
                              Addr base, Addr mid, Addr limit);

/* Buffer*Method -- see <design/buffer/> */

typedef void (*BufferVarargsMethod)(ArgStruct args[], va_list varargs);
typedef Res (*BufferInitMethod)(Buffer buffer, Pool pool, Bool isMutator, ArgList args);
typedef void (*BufferFinishMethod)(Buffer buffer);
typedef void (*BufferAttachMethod)(Buffer buffer, Addr base, Addr limit,
                                   Addr init, Size size);
typedef void (*BufferDetachMethod)(Buffer buffer);
typedef Seg (*BufferSegMethod)(Buffer buffer);
typedef RankSet (*BufferRankSetMethod)(Buffer buffer);
typedef void (*BufferSetRankSetMethod)(Buffer buffer, RankSet rankSet);
typedef void (*BufferReassignSegMethod)(Buffer buffer, Seg seg);
typedef Res (*BufferDescribeMethod)(Buffer buffer, mps_lib_FILE *stream, Count depth);


/* Pool*Method -- see <design/class-interface/> */

/* Order of types corresponds to PoolClassStruct in <code/mpmst.h> */

typedef void (*PoolVarargsMethod)(ArgStruct args[], va_list varargs);
typedef Res (*PoolInitMethod)(Pool pool, Arena arena, PoolClass class, ArgList args);
typedef void (*PoolFinishMethod)(Pool pool);
typedef Res (*PoolAllocMethod)(Addr *pReturn, Pool pool, Size size);
typedef void (*PoolFreeMethod)(Pool pool, Addr old, Size size);
typedef Res (*PoolBufferFillMethod)(Addr *baseReturn, Addr *limitReturn,
                                    Pool pool, Buffer buffer, Size size);
typedef void (*PoolBufferEmptyMethod)(Pool pool, Buffer buffer,
                                      Addr init, Addr limit);
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
typedef void (*PoolTraceEndMethod)(Pool pool, Trace trace);
typedef void (*PoolRampBeginMethod)(Pool pool, Buffer buf, Bool collectAll);
typedef void (*PoolRampEndMethod)(Pool pool, Buffer buf);
typedef Res (*PoolFramePushMethod)(AllocFrame *frameReturn,
                                   Pool pool, Buffer buf);
typedef Res (*PoolFramePopMethod)(Pool pool, Buffer buf,
                                  AllocFrame frame);
typedef void (*PoolFramePopPendingMethod)(Pool pool, Buffer buf,
                                          AllocFrame frame);
typedef Res (*PoolAddrObjectMethod)(Addr *pReturn,
                                    Pool pool, Seg seg, Addr addr);
typedef void (*PoolWalkMethod)(Pool pool, Seg seg, FormattedObjectsVisitor f,
                               void *v, size_t s);
typedef void (*PoolFreeWalkMethod)(Pool pool, FreeBlockVisitor f, void *p);
typedef BufferClass (*PoolBufferClassMethod)(void);
typedef Res (*PoolDescribeMethod)(Pool pool, mps_lib_FILE *stream, Count depth);
typedef PoolDebugMixin (*PoolDebugMixinMethod)(Pool pool);
typedef Size (*PoolSizeMethod)(Pool pool);


/* Messages
 *
 * See <design/message/>
 */

typedef unsigned MessageType;
typedef struct mps_message_s *Message;
typedef struct MessageClassStruct *MessageClass;

/* Message*Method -- <design/message/> */

typedef void (*MessageDeleteMethod)(Message message);
typedef void (*MessageFinalizationRefMethod)
  (Ref *refReturn, Arena arena, Message message);
typedef Size (*MessageGCLiveSizeMethod)(Message message);
typedef Size (*MessageGCCondemnedSizeMethod)(Message message);
typedef Size (*MessageGCNotCondemnedSizeMethod)(Message message);
typedef const char * (*MessageGCStartWhyMethod)(Message message);

/* Message Types -- <design/message/> and elsewhere */

typedef struct TraceStartMessageStruct *TraceStartMessage;
typedef struct TraceMessageStruct *TraceMessage;  /* trace end */


/* Land*Method -- see <design/land/> */

typedef Res (*LandInitMethod)(Land land, Arena arena, Align alignment, ArgList args);
typedef void (*LandFinishMethod)(Land land);
typedef Size (*LandSizeMethod)(Land land);
typedef Res (*LandInsertMethod)(Range rangeReturn, Land land, Range range);
typedef Res (*LandDeleteMethod)(Range rangeReturn, Land land, Range range);
typedef Bool (*LandVisitor)(Land land, Range range, void *closure);
typedef Bool (*LandDeleteVisitor)(Bool *deleteReturn, Land land, Range range, void *closure);
typedef Bool (*LandIterateMethod)(Land land, LandVisitor visitor, void *closure);
typedef Bool (*LandIterateAndDeleteMethod)(Land land, LandDeleteVisitor visitor, void *closure);
typedef Bool (*LandFindMethod)(Range rangeReturn, Range oldRangeReturn, Land land, Size size, FindDelete findDelete);
typedef Res (*LandFindInZonesMethod)(Bool *foundReturn, Range rangeReturn, Range oldRangeReturn, Land land, Size size, ZoneSet zoneSet, Bool high);
typedef Res (*LandDescribeMethod)(Land land, mps_lib_FILE *stream, Count depth);


/* CONSTANTS */


/* <design/sig/> SIGnature IS BAD */
#define SigInvalid      ((Sig)0x51915BAD)

#define SizeMAX         ((Size)-1)
#define AccessSetEMPTY  ((AccessSet)0) /* <design/type/#access-set> */
#define AccessREAD      ((AccessSet)(1<<0))
#define AccessWRITE     ((AccessSet)(1<<1))
#define AccessLIMIT     (2)
#define RefSetEMPTY     BS_EMPTY(RefSet)
#define RefSetUNIV      BS_UNIV(RefSet)
#define ZoneSetEMPTY    BS_EMPTY(ZoneSet)
#define ZoneSetUNIV     BS_UNIV(ZoneSet)
#define TraceSetEMPTY   BS_EMPTY(TraceSet)
#define TraceSetUNIV    ((TraceSet)((1u << TraceLIMIT) - 1))
#define RankSetEMPTY    BS_EMPTY(RankSet)
#define RankSetUNIV     ((RankSet)((1u << RankLIMIT) - 1))
#define AttrFMT         ((Attr)(1<<0))  /* <design/type/#attr> */
#define AttrGC          ((Attr)(1<<1))
#define AttrMOVINGGC    ((Attr)(1<<2))
#define AttrMASK        (AttrFMT | AttrGC | AttrMOVINGGC)


/* Locus preferences */
enum {
  LocusPrefHIGH = 1,
  LocusPrefLOW, 
  LocusPrefZONESET,
  LocusPrefLIMIT
};


/* Buffer modes */
#define BufferModeATTACHED      ((BufferMode)(1<<0))
#define BufferModeFLIPPED       ((BufferMode)(1<<1))
#define BufferModeLOGGED        ((BufferMode)(1<<2))
#define BufferModeTRANSITION    ((BufferMode)(1<<3))


/* Buffer frame states. See <design/alloc-frame/#lw-frame.states> */
enum {
  BufferFrameVALID = 1,
  BufferFramePOP_PENDING,
  BufferFrameDISABLED
};


/* Rank constants -- see <design/type/#rank> */
/* These definitions must match <code/mps.h#rank>. */
/* This is checked by <code/mpsi.c#check>. */

enum {
  RankMIN = 0,
  RankAMBIG = 0,
  RankEXACT = 1,
  RankFINAL = 2,
  RankWEAK = 3,
  RankLIMIT
};


/* Root Modes -- not implemented */
/* .rm: Synchronize with <code/mps.h#rm>. */
/* This comment exists as a placeholder for when root modes are */
/* implemented. */

#define RootModeCONSTANT          ((RootMode)1<<0)
#define RootModePROTECTABLE       ((RootMode)1<<1)
#define RootModePROTECTABLE_INNER ((RootMode)1<<2)


/* Root Variants -- see <design/type/#rootvar>
 *
 * .rootvar: Synchonize with <code/root.c#rootvarcheck>
 */

enum {
  RootFUN,
  RootAREA,
  RootAREA_TAGGED,
  RootTHREAD,
  RootTHREAD_TAGGED,
  RootFMT,
  RootLIMIT
};


/* .result-codes: Result Codes -- see <design/type/#res> */

_mps_ENUM_DEF(_mps_RES_ENUM, Res)


/* TraceStates -- see <design/trace/> */

enum {
  TraceINIT = 1,
  TraceUNFLIPPED,
  TraceFLIPPED,
  TraceRECLAIM,
  TraceFINISHED
};


/* TraceStart reasons: the trigger that caused a trace to start. */
/* Make these specific trigger names, not broad categories; */
/* and if a new trigger is added, add a new reason. */
/* TODO: A better way for MPS extensions to extend the list of reasons
   instead of the catch-all TraceStartWhyEXTENSION. */

enum {
  TraceStartWhyBASE = 1, /* not a reason, the base of the enum. */
  TraceStartWhyCHAIN_GEN0CAP = TraceStartWhyBASE,  /* start minor */
  TraceStartWhyDYNAMICCRITERION, /* start full */
  TraceStartWhyOPPORTUNISM,      /* start full */
  TraceStartWhyCLIENTFULL_INCREMENTAL,   /* start full */
  TraceStartWhyCLIENTFULL_BLOCK, /* do full */
  TraceStartWhyWALK,            /* walking references -- see walk.c */
  TraceStartWhyEXTENSION,       /* MPS extension using traces */
  TraceStartWhyLIMIT /* not a reason, the limit of the enum. */
};


/* MessageTypes -- see <design/message/> */
/* .message.types: Keep in sync with <code/mps.h#message.types> */

enum {
  MessageTypeFINALIZATION,  /* MPS_MESSAGE_TYPE_FINALIZATION */
  MessageTypeGC,  /* MPS_MESSAGE_TYPE_GC = trace end */
  MessageTypeGCSTART,  /* MPS_MESSAGE_TYPE_GC_START */
  MessageTypeLIMIT /* not a message type, the limit of the enum. */
};


/* FindDelete operations -- see <design/land/> */

enum {
  FindDeleteNONE = 1, /* don't delete after finding */
  FindDeleteLOW,      /* delete size bytes from low end of block */
  FindDeleteHIGH,     /* delete size bytes from high end of block */
  FindDeleteENTIRE,   /* delete entire range */
  FindDeleteLIMIT     /* not a FindDelete operation; the limit of the enum. */
};


/* Types for WriteF formats */
/* These should be used with calls to WriteF. */
/* These must be unpromotable types. */

typedef Addr WriteFA;
typedef Pointer WriteFP;
typedef const char *WriteFS;
typedef Word WriteFW;
typedef ULongest WriteFU;
typedef ULongest WriteFB;
typedef void *(*WriteFF)(void);
typedef int WriteFC; /* Promoted */
typedef double WriteFD;


/* STATISTIC_DECL -- declare a field to accumulate statistics in
 *
 * The argument is a field declaration (a struct-declaration minus the
 * semicolon) for a single field (no commas).  Currently, we always
 * leave them in, see design.mps.metrics.
 */

#if defined(STATISTICS)
#define STATISTIC_DECL(field) field
#elif defined(STATISTICS_NONE)
#define STATISTIC_DECL(field) field
#else
#error "No statistics configured."
#endif


#endif /* mpmtypes_h */


/* C. COPYRIGHT AND LICENSE
 *
 * Copyright (C) 2001-2014 Ravenbrook Limited <http://www.ravenbrook.com/>.
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
