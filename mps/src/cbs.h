/* impl.h.cbs: CBS -- Coalescing Block Structure
 *
 * $HopeName: MMsrc!cbs.h(trunk.3) $
 * Copyright (C) 1999 Harlequin Limited.  All rights reserved.
 *
 * .source: design.mps.cbs.
 */

#ifndef cbs_h
#define cbs_h

#include "meter.h"
#include "splay.h"
#include "mpmtypes.h"


typedef struct CBSStruct *CBS;
typedef struct CBSBlockStruct *CBSBlock;
typedef void (*CBSChangeSizeMethod)(CBS cbs, CBSBlock block,
              Size oldSize, Size newSize);
typedef Bool (*CBSIterateMethod)(CBS cbs, CBSBlock block, void *closureP);


/* See design.mps.cbs.impl.low-mem.inline.block */
typedef void **CBSEmergencyBlock; /* next, limit */

/* See design.mps.cbs.impl.low-mem.inline.block */
typedef void **CBSEmergencyGrain; /* next */


#define CBSSig ((Sig)0x519CB599) /* SIGnature CBS */

typedef struct CBSStruct {
  SplayTreeStruct splayTree;
  Count splayTreeSize;
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
  CBSEmergencyBlock emergencyBlockList;
  Count eblSize;
  CBSEmergencyGrain emergencyGrainList;
  Count eglSize;
  /* meters for sizes of search structures at each op */
  METER_DECL(splaySearch);
  METER_DECL(eblSearch);
  METER_DECL(eglSearch);
  Sig sig; /* sig at end because embeded */
} CBSStruct;

typedef struct CBSBlockStruct {
  SplayNodeStruct splayNode;
  Addr base;
  Addr limit;
  Size maxSize; /* accurate maximum block size of sub-tree */
} CBSBlockStruct;


extern Bool CBSCheck(CBS cbs);
extern Bool CBSBlockCheck(CBSBlock block);

extern Res CBSInit(Arena arena, CBS cbs, void *owner,
                   CBSChangeSizeMethod new,
                   CBSChangeSizeMethod delete,
                   CBSChangeSizeMethod grow,
                   CBSChangeSizeMethod shrink,
                   Size minSize,
                   Align alignment,
                   Bool mayUseInline,
                   Bool fastFind); 
extern void CBSFinish(CBS cbs);

extern Res CBSInsert(CBS cbs, Addr base, Addr limit);
extern Res CBSInsertReturningRange(Addr *baseReturn, Addr *limitReturn,
                                   CBS cbs, Addr base, Addr limit);
extern Res CBSDelete(CBS cbs, Addr base, Addr limit);
extern void CBSIterate(CBS cbs, CBSIterateMethod iterate, void *closureP);
extern void CBSIterateLarge(CBS cbs, CBSIterateMethod iterate, void *closureP);
extern void CBSSetMinSize(CBS cbs, Size minSize);

extern Res CBSDescribe(CBS cbs, mps_lib_FILE *stream);
extern Res CBSBlockDescribe(CBSBlock block, mps_lib_FILE *stream);

/* CBSBlockBase -- See design.mps.cbs.function.cbs.block.base */
#define CBSBlockBase(block) ((block)->base)
/* CBSBlockLimit -- See design.mps.cbs.function.cbs.block.limit */
#define CBSBlockLimit(block) ((block)->limit)
#define CBSBlockSize(block) AddrOffset((block)->base, (block)->limit)
extern Size (CBSBlockSize)(CBSBlock block);

typedef unsigned CBSFindDelete;
enum {
  CBSFindDeleteNONE = 1,/* don't delete after finding */
  CBSFindDeleteLOW,     /* delete precise size from low end */
  CBSFindDeleteHIGH,    /* delete precise size from high end */
  CBSFindDeleteENTIRE   /* delete entire range */
};

extern Bool CBSFindFirst(Addr *baseReturn, Addr *limitReturn,
                         CBS cbs, Size size, CBSFindDelete findDelete);
extern Bool CBSFindLast(Addr *baseReturn, Addr *limitReturn,
                        CBS cbs, Size size, CBSFindDelete findDelete);

extern Bool CBSFindLargest(Addr *baseReturn, Addr *limitReturn,
                           CBS cbs, CBSFindDelete findDelete);


#endif /* cbs_h */
