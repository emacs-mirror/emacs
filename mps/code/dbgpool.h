/* impl.h.dbgpool: POOL DEBUG MIXIN
 *
 * $Id$
 * Copyright (c) 2001 Ravenbrook Limited.
 * Copyright (C) 2002 Global Graphics Software.
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
  void* freeTemplate;
  Size  freeSize;
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
  Addr freeTemplate;
  Size freeSize;
  TagInitMethod tagInit;
  Size tagSize;
  Pool tagPool;
  Count missingTags;
  SplayTreeStruct index;
} PoolDebugMixinStruct;


extern Bool PoolDebugMixinCheck(PoolDebugMixin dbg);

extern void PoolClassMixInDebug(PoolClass class);

extern void DebugPoolCheckFences(Pool pool);
extern void DebugPoolCheckFreeSpace(Pool pool);

extern void DebugPoolFreeSplat(Pool pool, Addr base, Addr limit);
extern void DebugPoolFreeCheck(Pool pool, Addr base, Addr limit);


#endif /* dbgpool_h */
