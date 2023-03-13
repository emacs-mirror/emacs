/* mpmtypes.h: MEMORY POOL MANAGER TYPES
 *
 * $Id$
 * Copyright (c) 2001-2020 Ravenbrook Limited.  See end of file for license.
 * Portions copyright (c) 2001 Global Graphics Software.
 *
 * .design: <design/type>
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

typedef unsigned long Sig;              /* design.mps.sig */
typedef int Res;                        /* <design/type#.res> */

typedef void (*Fun)(void);              /* <design/type#.fun> */
typedef MPS_T_WORD Word;                /* <design/type#.word> */
typedef unsigned char Byte;             /* <design/type#.byte> */
typedef struct AddrStruct *Addr;        /* <design/type#.addr> */
typedef const struct AddrStruct *ReadonlyAddr; /* <design/type#.readonlyaddr> */
typedef Word Size;                      /* <design/type#.size> */
typedef Word Count;                     /* <design/type#.count> */
typedef Word Index;                     /* <design/type#.index> */
typedef Word Align;                     /* <design/type#.align> */
typedef Word Work;                      /* <design/type#.work> */
typedef unsigned Shift;                 /* <design/type#.shift> */
typedef unsigned Serial;                /* <design/type#.serial> */
typedef Addr Ref;                       /* <design/type#.ref> */
typedef void *Pointer;                  /* <design/type#.pointer> */
typedef Word Clock;                     /* <design/type#.clock> */
typedef MPS_T_ULONGEST ULongest;        /* <design/type#.ulongest> */

typedef mps_arg_s ArgStruct;
typedef mps_arg_s *Arg;
typedef mps_arg_s *ArgList;
typedef mps_key_t Key;

typedef Word RefSet;                    /* <design/collection#.refsets> */
typedef Word ZoneSet;                   /* <design/collection#.refsets> */
typedef unsigned Rank;                  /* <design/type#.rank> */
typedef unsigned RankSet;               /* <design/type#.rankset> */
typedef unsigned RootMode;              /* <design/type#.rootmode> */
typedef Size Epoch;                     /* <design/type#.epoch> */
typedef unsigned TraceId;               /* <design/type#.traceid> */
typedef unsigned TraceSet;              /* <design/type#.traceset> */
typedef unsigned TraceState;            /* <design/type#.tracestate> */
typedef unsigned TraceStartWhy;         /* <design/type#.tracestartwhy> */
typedef unsigned AccessSet;             /* <design/type#.access-set> */
typedef unsigned Attr;                  /* <design/type#.attr> */
typedef unsigned RootVar;               /* <design/type#.rootvar> */

typedef Word *BT;                       /* <design/bt> */
typedef struct BootBlockStruct *BootBlock; /* <code/boot.c> */
typedef struct BufferStruct *Buffer;    /* <design/buffer> */
typedef struct SegBufStruct *SegBuf;    /* <design/buffer> */
typedef struct BufferClassStruct *BufferClass; /* <design/buffer> */
typedef unsigned BufferMode;            /* <design/buffer> */
typedef struct mps_fmt_s *Format;       /* <design/format> */
typedef struct LockStruct *Lock;        /* <code/lock.c>* */
typedef struct mps_pool_s *Pool;        /* <design/pool> */
typedef Pool AbstractPool;
typedef struct mps_pool_class_s *PoolClass;  /* <code/poolclas.c> */
typedef struct TraceStruct *Trace;      /* <design/trace> */
typedef struct ScanStateStruct *ScanState; /* <design/trace> */
typedef struct mps_chain_s *Chain;      /* <design/trace> */
typedef struct TractStruct *Tract;      /* <design/arena> */
typedef struct ChunkStruct *Chunk;      /* <code/tract.c> */
typedef struct ChunkCacheEntryStruct *ChunkCacheEntry; /* <code/tract.c> */
typedef union PageUnion *Page;          /* <code/tract.c> */
typedef struct SegStruct *Seg;          /* <code/seg.c> */
typedef struct GCSegStruct *GCSeg;      /* <code/seg.c> */
typedef struct SegClassStruct *SegClass; /* <code/seg.c> */
typedef struct LocusPrefStruct *LocusPref; /* <design/locus>, <code/locus.c> */
typedef unsigned LocusPrefKind;         /* <design/locus>, <code/locus.c> */
typedef struct mps_arena_class_s *ArenaClass; /* <design/arena> */
typedef struct mps_arena_s *Arena;      /* <design/arena> */
typedef Arena AbstractArena;
typedef struct GlobalsStruct *Globals;  /* <design/arena> */
typedef struct VMStruct *VM;            /* <code/vm.c>* */
typedef struct RootStruct *Root;        /* <code/root.c> */
typedef struct mps_thr_s *Thread;       /* <code/th.c>* */
typedef struct MutatorContextStruct *MutatorContext; /* <design/prmc> */
typedef struct PoolDebugMixinStruct *PoolDebugMixin;
typedef struct AllocPatternStruct *AllocPattern;
typedef struct AllocFrameStruct *AllocFrame; /* <design/alloc-frame> */
typedef struct StackContextStruct *StackContext;
typedef struct RangeStruct *Range;      /* <design/range> */
typedef struct RangeTreeStruct *RangeTree;
typedef struct LandStruct *Land;        /* <design/land> */
typedef struct LandClassStruct *LandClass; /* <design/land> */
typedef unsigned FindDelete;            /* <design/land> */
typedef struct ShieldStruct *Shield; /* <design/shield> */
typedef struct HistoryStruct *History;  /* <design/arena#.ld> */
typedef struct PoolGenStruct *PoolGen;  /* <design/strategy> */


/* Arena*Method -- see <code/mpmst.h#ArenaClassStruct> */

typedef void (*ArenaVarargsMethod)(ArgStruct args[], va_list varargs);
typedef Res (*ArenaCreateMethod)(Arena *arenaReturn, ArgList args);
typedef void (*ArenaDestroyMethod)(Arena arena);
typedef Res (*ArenaInitMethod)(Arena arena, Size grainSize, ArgList args);
typedef Size (*ArenaPurgeSpareMethod)(Arena arena, Size size);
typedef Res (*ArenaExtendMethod)(Arena arena, Addr base, Size size);
typedef Res (*ArenaGrowMethod)(Arena arena, LocusPref pref, Size size);
typedef void (*ArenaFreeMethod)(Addr base, Size size, Pool pool);
typedef Res (*ArenaChunkInitMethod)(Chunk chunk, BootBlock boot);
typedef void (*ArenaChunkFinishMethod)(Chunk chunk);
typedef void (*ArenaCompactMethod)(Arena arena, Trace trace);
typedef Res (*ArenaPagesMarkAllocatedMethod)(Arena arena, Chunk chunk,
                                             Index baseIndex, Count pages,
                                             Pool pool);
typedef Bool (*ArenaChunkPageMappedMethod)(Chunk chunk, Index index);


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


/* Seg*Method -- see <design/seg> */

typedef Res (*SegInitMethod)(Seg seg, Pool pool, Addr base, Size size,
                             ArgList args);
typedef void (*SegSetGreyMethod)(Seg seg, TraceSet grey);
typedef void (*SegFlipMethod)(Seg seg, Trace trace);
typedef void (*SegSetWhiteMethod)(Seg seg, TraceSet white);
typedef void (*SegSetRankSetMethod)(Seg seg, RankSet rankSet);
typedef void (*SegSetRankSummaryMethod)(Seg seg, RankSet rankSet,
                                        RefSet summary);
typedef void (*SegSetSummaryMethod)(Seg seg, RefSet summary);
typedef Bool (*SegBufferMethod)(Buffer *bufferReturn, Seg seg);
typedef void (*SegSetBufferMethod)(Seg seg, Buffer buffer);
typedef void (*SegUnsetBufferMethod)(Seg seg);
typedef Bool (*SegBufferFillMethod)(Addr *baseReturn, Addr *limitReturn,
                                    Seg seg, Size size, RankSet rankSet);
typedef void (*SegBufferEmptyMethod)(Seg seg, Buffer buffer);
typedef Res (*SegMergeMethod)(Seg seg, Seg segHi,
                              Addr base, Addr mid, Addr limit);
typedef Res (*SegSplitMethod)(Seg seg, Seg segHi,
                              Addr base, Addr mid, Addr limit);
typedef Res (*SegAccessMethod)(Seg seg, Arena arena, Addr addr,
                               AccessSet mode, MutatorContext context);
typedef Res (*SegWhitenMethod)(Seg seg, Trace trace);
typedef void (*SegGreyenMethod)(Seg seg, Trace trace);
typedef void (*SegBlackenMethod)(Seg seg, TraceSet traceSet);
typedef Res (*SegScanMethod)(Bool *totalReturn, Seg seg, ScanState ss);
typedef Res (*SegFixMethod)(Seg seg, ScanState ss, Ref *refIO);
typedef void (*SegReclaimMethod)(Seg seg, Trace trace);
typedef void (*SegWalkMethod)(Seg seg, Format format, FormattedObjectsVisitor f,
                              void *v, size_t s);


/* Buffer*Method -- see <design/buffer> */

typedef void (*BufferVarargsMethod)(ArgStruct args[], va_list varargs);
typedef Res (*BufferInitMethod)(Buffer buffer, Pool pool, Bool isMutator, ArgList args);
typedef void (*BufferAttachMethod)(Buffer buffer, Addr base, Addr limit,
                                   Addr init, Size size);
typedef void (*BufferDetachMethod)(Buffer buffer);
typedef Seg (*BufferSegMethod)(Buffer buffer);
typedef RankSet (*BufferRankSetMethod)(Buffer buffer);
typedef void (*BufferSetRankSetMethod)(Buffer buffer, RankSet rankSet);
typedef void (*BufferReassignSegMethod)(Buffer buffer, Seg seg);


/* Pool*Method -- see <design/pool> */

/* Order of types corresponds to PoolClassStruct in <code/mpmst.h> */

typedef void (*PoolVarargsMethod)(ArgStruct args[], va_list varargs);
typedef Res (*PoolInitMethod)(Pool pool, Arena arena, PoolClass klass, ArgList args);
typedef Res (*PoolAllocMethod)(Addr *pReturn, Pool pool, Size size);
typedef void (*PoolFreeMethod)(Pool pool, Addr old, Size size);
typedef PoolGen (*PoolSegPoolGenMethod)(Pool pool, Seg seg);
typedef Res (*PoolBufferFillMethod)(Addr *baseReturn, Addr *limitReturn,
                                    Pool pool, Buffer buffer, Size size);
typedef void (*PoolBufferEmptyMethod)(Pool pool, Buffer buffer);
typedef void (*PoolRampBeginMethod)(Pool pool, Buffer buf, Bool collectAll);
typedef void (*PoolRampEndMethod)(Pool pool, Buffer buf);
typedef Res (*PoolFramePushMethod)(AllocFrame *frameReturn,
                                   Pool pool, Buffer buf);
typedef Res (*PoolFramePopMethod)(Pool pool, Buffer buf,
                                  AllocFrame frame);
typedef void (*PoolFreeWalkMethod)(Pool pool, FreeBlockVisitor f, void *p);
typedef BufferClass (*PoolBufferClassMethod)(void);
typedef PoolDebugMixin (*PoolDebugMixinMethod)(Pool pool);
typedef Size (*PoolSizeMethod)(Pool pool);


/* Messages
 *
 * <design/message>
 */

typedef unsigned MessageType;
typedef struct mps_message_s *Message;
typedef struct MessageClassStruct *MessageClass;

/* Message*Method -- <design/message> */

typedef void (*MessageDeleteMethod)(Message message);
typedef void (*MessageFinalizationRefMethod)
  (Ref *refReturn, Arena arena, Message message);
typedef Size (*MessageGCLiveSizeMethod)(Message message);
typedef Size (*MessageGCCondemnedSizeMethod)(Message message);
typedef Size (*MessageGCNotCondemnedSizeMethod)(Message message);
typedef const char * (*MessageGCStartWhyMethod)(Message message);

/* Message Types -- <design/message> and elsewhere */

typedef struct TraceStartMessageStruct *TraceStartMessage;
typedef struct TraceMessageStruct *TraceMessage;  /* trace end */


/* Land*Method -- see <design/land> */

typedef Res (*LandInitMethod)(Land land, Arena arena, Align alignment, ArgList args);
typedef Size (*LandSizeMethod)(Land land);
typedef Res (*LandInsertMethod)(Range rangeReturn, Land land, Range range);
typedef Res (*LandDeleteMethod)(Range rangeReturn, Land land, Range range);
typedef Bool (*LandVisitor)(Land land, Range range, void *closure);
typedef Bool (*LandDeleteVisitor)(Bool *deleteReturn, Land land, Range range, void *closure);
typedef Bool (*LandIterateMethod)(Land land, LandVisitor visitor, void *closure);
typedef Bool (*LandIterateAndDeleteMethod)(Land land, LandDeleteVisitor visitor, void *closure);
typedef Bool (*LandFindMethod)(Range rangeReturn, Range oldRangeReturn, Land land, Size size, FindDelete findDelete);
typedef Res (*LandFindInZonesMethod)(Bool *foundReturn, Range rangeReturn, Range oldRangeReturn, Land land, Size size, ZoneSet zoneSet, Bool high);


/* CONSTANTS */


/* design.mps.sig  */
#define SigInvalid      ((Sig)0x51915BAD) /* SIGnature IS BAD */

#define SizeMAX         ((Size)-1)
#define AccessSetEMPTY  ((AccessSet)0) /* <design/type#.access-set> */
#define AccessREAD      ((AccessSet)(1<<0))
#define AccessWRITE     ((AccessSet)(1<<1))
#define AccessLIMIT     (2)
#define RefSetEMPTY     BS_EMPTY(RefSet)
#define RefSetUNIV      BS_UNIV(RefSet)
#define ZoneSetEMPTY    BS_EMPTY(ZoneSet)
#define ZoneSetUNIV     BS_UNIV(ZoneSet)
#define ZoneShiftUNSET  ((Shift)-1)
#define TraceSetEMPTY   BS_EMPTY(TraceSet)
#define TraceSetUNIV    ((TraceSet)((1u << TraceLIMIT) - 1))
#define RankSetEMPTY    BS_EMPTY(RankSet)
#define RankSetUNIV     ((RankSet)((1u << RankLIMIT) - 1))
#define AttrGC          ((Attr)(1<<0))
#define AttrMOVINGGC    ((Attr)(1<<1))
#define AttrMASK        (AttrGC | AttrMOVINGGC)


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


/* Rank constants -- see <design/type#.rank> */
/* These definitions must match <code/mps.h#rank>. */
/* This is checked by <code/mpsi.c#check>. */

#define RANK_LIST(X) X(AMBIG) X(EXACT) X(FINAL) X(WEAK)

enum {
#define X(RANK) Rank ## RANK,
  RANK_LIST(X)
#undef X
  RankLIMIT,
  RankMIN = 0
};


/* Root Modes -- not implemented */
/* .rm: Synchronize with <code/mps.h#rm>. */
/* This comment exists as a placeholder for when root modes are */
/* implemented. */

#define RootModeCONSTANT          ((RootMode)1<<0)
#define RootModePROTECTABLE       ((RootMode)1<<1)
#define RootModePROTECTABLE_INNER ((RootMode)1<<2)


/* Root Variants -- see <design/type#.rootvar>
 *
 * .rootvar: Synchronize with <code/root.c#rootvarcheck>
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


/* .result-codes: Result Codes -- see <design/type#.res> */

_mps_ENUM_DEF(_mps_RES_ENUM, Res)


/* TraceStates -- see <design/trace> */

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

#define TRACE_START_WHY_LIST(X)                                         \
  X(CHAIN_GEN0CAP, "gen 0 capacity",                                    \
    "Generation 0 of a chain has reached capacity: start a minor "      \
    "collection.")                                                      \
  X(DYNAMICCRITERION, "dynamic criterion",                              \
    "Need to start full collection now, or there won't be enough "      \
    "memory (ArenaAvail) to complete it.")                              \
  X(OPPORTUNISM, "opportunism",                                         \
    "Opportunism: client predicts plenty of idle time, so start full "  \
    "collection.")                                                      \
  X(CLIENTFULL_INCREMENTAL, "full incremental", \
    "Client requests: start incremental full collection now.")          \
  X(CLIENTFULL_BLOCK, "full", \
    "Client requests: immediate full collection.")                      \
  X(WALK, "walk", "Walking all live objects.")                          \
  X(EXTENSION, "extension", \
    "Extension: an MPS extension started the trace.")

enum {
#define X(WHY, SHORT, LONG) TraceStartWhy ## WHY,
  TRACE_START_WHY_LIST(X)
#undef X
  TraceStartWhyLIMIT
};


/* MessageTypes -- see <design/message> */
/* .message.types: Keep in sync with <code/mps.h#message.types> */

enum {
  MessageTypeFINALIZATION,  /* MPS_MESSAGE_TYPE_FINALIZATION */
  MessageTypeGC,  /* MPS_MESSAGE_TYPE_GC = trace end */
  MessageTypeGCSTART,  /* MPS_MESSAGE_TYPE_GC_START */
  MessageTypeLIMIT /* not a message type, the limit of the enum. */
};


/* FindDelete operations -- see <design/land> */

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
typedef void (*WriteFF)(void);
typedef int WriteFC; /* Promoted */
typedef double WriteFD;


/* STATISTIC_DECL -- declare a field to accumulate statistics in
 *
 * The argument is a field declaration (a struct-declaration minus the
 * semicolon) for a single field (no commas).
 */

#if defined(STATISTICS)
#define STATISTIC_DECL(field) field;
#elif defined(STATISTICS_NONE)
#define STATISTIC_DECL(field)
#else
#error "No statistics configured."
#endif


#endif /* mpmtypes_h */


/* C. COPYRIGHT AND LICENSE
 *
 * Copyright (C) 2001-2020 Ravenbrook Limited <https://www.ravenbrook.com/>.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the
 *    distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
 * IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 * TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
 * PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 * HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
