/* impl.h.dbgpool: POOL DEBUG MIXIN
 *
 * $HopeName: MMsrc!dbgpool.h(trunk.3) $
 * Copyright (C) 1998 Harlequin Group plc.  All rights reserved.
 */

#ifndef dbgpool_h
#define dbgpool_h

#include "splay.h"
#include "mpmtypes.h"
#include <stdarg.h>


/* tag init methods: copying the user-supplied data into the tag */

typedef void (*TagInitMethod)(void* tag, va_list args);


/* PoolDebugOptions -- option structure for debug pool init
 *
 * This must be kept in sync with impl.h.mps.mps_pool_debug_option_s.
 */

typedef struct PoolDebugOptionsStruct {
  void* fenceTemplate;
  Size  fenceSize;
  /* TagInitMethod tagInit; */
  /* Size  tagSize; */
} PoolDebugOptionsStruct;

typedef PoolDebugOptionsStruct *PoolDebugOptions;


/* PoolDebugMixinStruct -- internal structure for debug mixins */

#define PoolDebugMixinSig ((Sig)0x519B0DB9)  /* SIGnature POol DeBuG */

typedef struct PoolDebugMixinStruct {
  Sig sig;
  Addr fenceTemplate;
  Size fenceSize;
  TagInitMethod tagInit;
  Size tagSize;
  Pool tagPool;
  Count missingTags;
  SplayTreeStruct index;
} PoolDebugMixinStruct;


extern Bool PoolDebugMixinCheck(PoolDebugMixin dbg);


extern void PoolClassMixInDebug(PoolClass class);


#endif /* dbgpool_h */
