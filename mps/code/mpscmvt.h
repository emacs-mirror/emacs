/* mpscmvt.h: MEMORY POOL SYSTEM CLASS "MVT"
 *
 * $Id$
 * Copyright (c) 2001-2013 Ravenbrook Limited.  See end of file for license.
 */

#ifndef mpscmvt_h
#define mpscmvt_h

#include "mps.h"

extern const struct mps_key_s _mps_key_mvt_reserve_depth;
#define MPS_KEY_MVT_RESERVE_DEPTH (&_mps_key_mvt_reserve_depth)
#define MPS_KEY_MVT_RESERVE_DEPTH_FIELD count
extern const struct mps_key_s _mps_key_mvt_frag_limit;
#define MPS_KEY_MVT_FRAG_LIMIT (&_mps_key_mvt_frag_limit)
#define MPS_KEY_MVT_FRAG_LIMIT_FIELD d

/* The mvt pool class has five extra parameters to mps_pool_create:
 *  mps_res_t mps_pool_create(mps_pool_t * pool, mps_arena_t arena,
 *                            mps_class_t mvt_class,
 *                            size_t minimum_size,
 *                            size_t mean_size,
 *                            size_t maximum_size,
 *                            mps_count_t reserve_depth
 *                            mps_count_t fragmentation_limit);
 * minimum_, mean_, and maximum_size are the mimimum, mean, and
 * maximum (typical) size of objects expected to be allocated in the
 * pool.  reserve_depth is a measure of the expected hysteresis of the
 * object population.  fragmentation_limit is a percentage (between 0
 * and 100):  if the free space managed by the pool exceeds the
 * specified percentage, the pool will resort to a "first fit"
 * allocation policy.
 */
extern mps_class_t mps_class_mvt(void);

/* The mvt pool class formerly supported two extensions to the pool
   protocol: size and free_size. These are deprecated in favour of the
   generic pool function. */

#define mps_mvt_free_size(pool) (mps_pool_free_size(pool))
#define mps_mvt_size(pool) (mps_pool_total_size(pool))

#endif /* mpscmvt_h */


/* C. COPYRIGHT AND LICENSE
 *
 * Copyright (C) 2001-2013 Ravenbrook Limited <http://www.ravenbrook.com/>.
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
