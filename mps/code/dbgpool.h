/* dbgpool.h: POOL DEBUG MIXIN
 *
 * <design/object-debug>.
 *
 * $Id$
 * Copyright (c) 2001-2020 Ravenbrook Limited.  See end of file for license.
 * Portions copyright (C) 2002 Global Graphics Software.
 */

#ifndef dbgpool_h
#define dbgpool_h

#include "splay.h"
#include "mpmtypes.h"
#include <stdarg.h>


/* tag init function: copies the user-supplied data into the tag */

typedef void (*TagInitFunction)(void *tag, va_list args);


/* PoolDebugOptions -- option structure for debug pool init
 *
 * This must be kept in sync with <code/mps.h#mps_pool_debug_option_s>.
 */

typedef struct PoolDebugOptionsStruct {
  const void *fenceTemplate;
  Size  fenceSize;
  const void *freeTemplate;
  Size  freeSize;
  /* TagInitFunction tagInit; */
  /* Size  tagSize; */
} PoolDebugOptionsStruct;

typedef PoolDebugOptionsStruct *PoolDebugOptions;


/* PoolDebugMixinStruct -- internal structure for debug mixins */

#define PoolDebugMixinSig ((Sig)0x519B0DB9)  /* SIGnature POol DeBuG */

typedef struct PoolDebugMixinStruct {
  Sig sig;
  const struct AddrStruct *fenceTemplate;
  Size fenceSize;
  const struct AddrStruct *freeTemplate;
  Size freeSize;
  TagInitFunction tagInit;
  Size tagSize;
  Pool tagPool;
  Count missingTags;
  SplayTreeStruct index;
} PoolDebugMixinStruct;


extern Bool PoolDebugOptionsCheck(PoolDebugOptions opt);

extern Bool PoolDebugMixinCheck(PoolDebugMixin dbg);

extern void PoolClassMixInDebug(PoolClass klass);

extern void DebugPoolCheckFences(Pool pool);
extern void DebugPoolCheckFreeSpace(Pool pool);

extern void DebugPoolFreeSplat(Pool pool, Addr base, Addr limit);
extern void DebugPoolFreeCheck(Pool pool, Addr base, Addr limit);


#endif /* dbgpool_h */


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
