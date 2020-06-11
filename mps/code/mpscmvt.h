/* mpscmvt.h: MEMORY POOL SYSTEM CLASS "MVT"
 *
 * $Id$
 * Copyright (c) 2001-2020 Ravenbrook Limited.  See end of file for license.
 */

#ifndef mpscmvt_h
#define mpscmvt_h

#include "mps.h"

extern const struct mps_key_s _mps_key_MVT_RESERVE_DEPTH;
#define MPS_KEY_MVT_RESERVE_DEPTH (&_mps_key_MVT_RESERVE_DEPTH)
#define MPS_KEY_MVT_RESERVE_DEPTH_FIELD count
extern const struct mps_key_s _mps_key_MVT_FRAG_LIMIT;
#define MPS_KEY_MVT_FRAG_LIMIT (&_mps_key_MVT_FRAG_LIMIT)
#define MPS_KEY_MVT_FRAG_LIMIT_FIELD d

extern mps_pool_class_t mps_class_mvt(void);

#define mps_mvt_free_size mps_pool_free_size
#define mps_mvt_size mps_pool_total_size

#endif /* mpscmvt_h */


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
