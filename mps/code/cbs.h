/* cbs.h: CBS -- Coalescing Block Structure
 *
 * $Id$
 * Copyright (c) 2001-2020 Ravenbrook Limited.  See end of file for license.
 *
 * .source: <design/cbs>.
 */

#ifndef cbs_h
#define cbs_h

#include "arg.h"
#include "mpmtypes.h"
#include "mpm.h"
#include "mpmst.h"
#include "rangetree.h"
#include "splay.h"

typedef struct CBSFastBlockStruct *CBSFastBlock;
typedef struct CBSFastBlockStruct {
  struct RangeTreeStruct rangeTreeStruct;
  Size maxSize; /* accurate maximum block size of sub-tree */
} CBSFastBlockStruct;

typedef struct CBSZonedBlockStruct *CBSZonedBlock;
typedef struct CBSZonedBlockStruct {
  struct CBSFastBlockStruct cbsFastBlockStruct;
  ZoneSet zones; /* union zone set of all ranges in sub-tree */
} CBSZonedBlockStruct;

typedef struct CBSStruct *CBS, *CBSFast, *CBSZoned;

extern Bool CBSCheck(CBS cbs);


/* CBSLand -- convert CBS to Land
 *
 * We would like to use MustBeA(Land, cbs) for this, but it produces
 * bogus warnings about strict aliasing from GCC 4.7 (and probably
 * 4.8).  We can abolish this macro when those are no longer in use in
 * MPS development.
 */

#define CBSLand(cbs) (&(cbs)->landStruct)


DECLARE_CLASS(Land, CBS, Land);
DECLARE_CLASS(Land, CBSFast, CBS);
DECLARE_CLASS(Land, CBSZoned, CBSFast);

extern const struct mps_key_s _mps_key_cbs_block_pool;
#define CBSBlockPool (&_mps_key_cbs_block_pool)
#define CBSBlockPool_FIELD pool

#endif /* cbs_h */


/* C. COPYRIGHT AND LICENSE
 *
 * Copyright (C) 2001-2020 Ravenbrook Limited <http://www.ravenbrook.com/>.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright
 *   notice, this list of conditions and the following disclaimer in the
 *   documentation and/or other materials provided with the
 *   distribution.
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
