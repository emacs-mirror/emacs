/* impl.c.arenavmx: STUB FOR ARENAVM
 *
 * $HopeName: MMsrc!arenavmx.c(trunk.1) $
 * Copyright (C) 1998 Harlequin Group plc.  All rights reserved.
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
 * .design.filename: The suffix "x" on this filename is to designate it 
 * as containing stubs.  This is a bit lame...
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

SRCID(arenavmx, "$HopeName: MMsrc!arenavmx.c(trunk.1) $");

/* mps_arena_class_vm -- return the arena class VM
 * Run-time asserting stub-function.
 * The purpose is to allow links against this symbol to succeed.
 * Don't actually call it -- it will cause a run-time assert.
 */

mps_arena_class_t mps_arena_class_vm(void)
{
  NOTREACHED;
  return (mps_arena_class_t)NULL;
}
