/* impl.c.arenavmx: STUB FOR ARENAVM
 *
 * $HopeName$
 * Copyright (C) 1998 The Harlequin Group Limited.  All rights reserved.
 *
 * This is a stub-file, providing stub-functions to take the place of 
 * functions exported from impl.c.arenavm.  Calling a stub-function 
 * causes a run-time assertion.
 *
 * DESIGN
 *
 * .design.interim:
 * mail.richardk.1998-04-17.14-52.1.req.unimpl.cli-interface.run-assert
 * See also impl.c.arenavm.
 * 
 * .design.filename: The suffix "x" means 'contains stubs'.  This is a 
 * bit lame...
 *
 *
 * IMPROVEMENTS
 *
 * .improve.reaction.ifdef: Clients might want one of a range of 
 * reactions to calling a stub-function 
 * (mail.richardk.1998-04-17.14-52.1.req.unimpl.cli-interface).  
 * This variation could be controlled by a config.h define.
 */


#include "mpm.h"
#include "mpsavm.h"

SRCID(arenavmx, "$HopeName$");

/* mps_arena_class_vm -- return the arena class VM
 * Run-time asserting stub-function.
 */

mps_arena_class_t mps_arena_class_vm(void)
{
  NOTREACHED;
  return (mps_arena_class_t)NULL;
}
