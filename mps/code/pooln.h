/* impl.h.pooln: NULL POOL
 *
 * $HopeName$
 * Copyright (C) 1995 Harlequin Limited.  All rights reserved.
 *
 * .purpose: The null pool class is here for pedagogical purposes.  It
 * is a skeleton of a pool class.  The class exhibits all the generic
 * pool functions; none of them have non-trivial implementations.
 *
 * .create: The generic create method for this class takes no extra
 * parameters.
 */


#ifndef pooln_h
#define pooln_h

#include "mpmtypes.h"


/* PoolN -- instance type  */

typedef struct PoolNStruct *PoolN;


/* PoolClassN -- returns the PoolClass for the null pool class */

extern PoolClass PoolClassN(void);


/* PoolNCheck -- check a pool of class N
 *
 * Validates a PoolN object.  This function conforms to the validation
 * protocol defined in design.mps.check.
 */

extern Bool PoolNCheck(PoolN poolN);


#endif /* pooln_h */
