/* impl.c.poolamsi: AUTOMATIC MARK & SWEEP POOL CLASS C INTERFACE
 *
 * $Id: poolamsi.c,v 1.2 2002/02/01 14:27:28 pekka Exp $
 * $HopeName: MMsrc!poolamsi.c(trunk.2) $
 * Copyright (C) 2002 Global Graphics Software.
 */

#include "mpscams.h"
#include "mps.h"
#include "poolams.h"

SRCID(poolamsi, "$Id: poolamsi.c,v 1.2 2002/02/01 14:27:28 pekka Exp $");


/* mps_class_ams -- return the AMS pool class descriptor */

mps_class_t mps_class_ams(void)
{
  return (mps_class_t)AMSPoolClassGet();
}


/* mps_class_ams_debug -- return the AMS (debug) pool class descriptor */

mps_class_t mps_class_ams_debug(void)
{
  return (mps_class_t)AMSDebugPoolClassGet();
}
