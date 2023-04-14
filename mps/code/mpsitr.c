/* mpsitr.c: MEMORY POOL SYSTEM C INTERFACE LAYER TO TRANSFORMS
 *
 * $Id$
 * Copyright 2011 Ravenbrook Limited.  See end of file for license.
 *
 * .purpose: This code bridges between the MPS interface to C to Transforms
 * <code/mpstr.h> and the internal implementation of Transforms.  It is
 * analogous to the MPS C Interface Layer <code/mpsi.c>, but for the Transforms
 * extension.
 */

#include "mpm.h"
#include "mpstr.h"
#include "trans.h"
#include "ss.h"


SRCID(mpsitr, "$Id$");


mps_res_t mps_transform_create(mps_transform_t *mps_transform_o,
                               mps_arena_t arena)
{
  Transform transform = NULL;
  Res res;

  AVER(mps_transform_o != NULL);

  ArenaEnter(arena);
  res = TransformCreate(&transform, arena);
  ArenaLeave(arena);
  if (res != ResOK)
    return res;

  *mps_transform_o = (mps_transform_t)transform;
  return MPS_RES_OK;
}


mps_res_t mps_transform_add_oldnew(mps_transform_t transform,
                                   mps_addr_t *mps_old_list,
                                   mps_addr_t *mps_new_list,
                                   size_t mps_count)
{
  Ref *old_list = (Ref *)mps_old_list;
  Ref *new_list = (Ref *)mps_new_list;
  Count count = mps_count;
  Arena arena;
  Res res;

  AVER(mps_old_list != NULL);
  AVER(mps_new_list != NULL);
  /* count: cannot check */

  arena = TransformArena(transform);

  ArenaEnter(arena);
  res = TransformAddOldNew(transform, old_list, new_list, count);
  ArenaLeave(arena);

  return res;
}


mps_res_t mps_transform_apply(mps_bool_t *applied_o,
                              mps_transform_t transform)
{
  Arena arena;
  Res res;

  AVER(applied_o != NULL);

  arena = TransformArena(transform);
  ArenaEnter(arena);
  STACK_CONTEXT_BEGIN(arena) {
    res = TransformApply(applied_o, transform);
  } STACK_CONTEXT_END(arena);
  ArenaLeave(arena);

  return res;
}


void mps_transform_destroy(mps_transform_t transform)
{
  Arena arena;

  arena = TransformArena(transform);

  ArenaEnter(arena);
  TransformDestroy(transform);
  ArenaLeave(arena);
}


/* C. COPYRIGHT AND LICENSE
 *
 * Copyright (C) 2011 Ravenbrook Limited <http://www.ravenbrook.com/>.
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
