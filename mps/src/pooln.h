/*  impl.h.pooln
 *
 *                          NULL POOL
 * 
 *  $HopeName$
 *
 *  Copyright (C) 1995 Harlequin Group, all rights reserved
 *
 *  The null pool class is here for pedagogical purposes.  It is a
 *  skeleton of a pool class.  The class exhibits all the generic pool
 *  functions; none of them have non-trivial implementations.
 *
 *  The generic create method for this class takes no extra parameters.
 */


#ifndef pooln_h
#define pooln_h


#include "std.h"


/*  Instance Type  */
typedef struct PoolNStruct *PoolN;


#include "pool.h"
#include "poolclas.h"
#include "space.h"


/*  PoolClass
 *
 *  Returns the PoolClass for the null pool class */
extern PoolClass PoolClassN(void);


/*  Validation
 *
 *  Validates a PoolN object.  This function conforms to the validation
 *  protocol defined in impl.h.valid
 */
extern Bool PoolNIsValid(PoolN poolN, ValidationType validParam);


/*  Create / Destroy; Class Specific
 *
 *  These functions create and destroy instances of this class.  Note
 *  that the generic pool create and destroy (PoolCreate/Destroy)
 *  methods can also be used.
 */
extern Error PoolNCreate(PoolN *poolNReturn, Space space);
extern void PoolNDestroy(PoolN poolN);


/*  Init / finish; Class Specific (for friends only)
 *
 *  These function respectively initialize and finish a null pool.
 *  Init does not allocate any memory for the PoolN object, and
 *  Finish does not release it.  They are expected to be used
 *  when you have already allocated a PoolNStruct object.
 */
extern Error PoolNInit(PoolN poolN, Space space);
extern void PoolNFinish(PoolN poolN);


/*  Conversion
 *
 *  Converts a PoolN object into a generic Pool object (so that you can
 *  apply generic pool methods).
 */
extern Pool (PoolNPool)(PoolN poolN);


#endif /* pooln_h */
