/* impl.h.cbs: CBS -- Coalescing Block Structure
 *
 * $HopeName$
 * Copyright (C) 1998 Harlequin Group plc.  All rights reserved.
 *
 * .source: design.mps.cbs.
 */

#include "mpmtypes.h"
#include "meter.h"
#include "mpmst.h" /* just for SplayTreeStruct */


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
extern void CBSIterate(CBS cbs, CBSIterateMethod iterate, 
                       void *closureP, unsigned long closureS);
extern void CBSIterateLarge(CBS cbs, CBSIterateMethod iterate, 
                            void *closureP, unsigned long closureS);
extern void CBSSetMinSize(CBS cbs, Size minSize);

extern Res CBSDescribe(CBS cbs, mps_lib_FILE *stream);
extern Res CBSBlockDescribe(CBSBlock block, mps_lib_FILE *stream);

#define CBSBlockBase(block) ((block)->base)
extern Addr (CBSBlockBase)(CBSBlock block);
#define CBSBlockLimit(block) ((block)->limit)
extern Addr (CBSBlockLimit)(CBSBlock block);
/* ANSI C doesn't define subtraction of zero pointers. */
#define CBSBlockSize(block) \
  (CBSBlockBase((block)) == (Addr)0 ? (Size)0 : \
    (AddrOffset(CBSBlockBase((block)), CBSBlockLimit((block)))))
extern Size (CBSBlockSize)(CBSBlock block);

extern Bool CBSFindFirst(Addr *baseReturn, Addr *limitReturn,
                         CBS cbs, Size size, CBSFindDelete findDelete);
extern Bool CBSFindLast(Addr *baseReturn, Addr *limitReturn,
                        CBS cbs, Size size, CBSFindDelete findDelete);
