/*  impl.h.lo
 *
 *                   LEAF OBJECT POOL CLASS
 *
 *  $HopeName$
 *
 *  Copyright (C) 1996 Harlequin Group, all rights reserved
 *
 *  The Leaf Object PoolClass is an automatically managed (ie garbage
 *  collected) pool for managing "leaf" objects.  Leaf objects are
 *  objects that have no references or no references that need tracing
 *  (ie the objects they refer too are non-moving and are manually
 *  managed).
 * 
 *  This Class has the following features:
 * 
 *  Approximately 6% (asymptotically) space overhead on managed objects.
 * 
 *  Automatically reclaims memory used by objects no longer reachable
 *  from the roots.
 * 
 *  Non-moving.  References to objects in this pool will never change
 *  due to "fixing".
 * 
 *  Buffers will always "commit".  When allocating using a buffer,
 *  commit will never fail.
 * 
 *  The following caveat applies:
 * 
 *  Space and time performance will degrade when fragmentation
 *  increases.
 * 
 */


#ifndef lo_h
#define lo_h


#include "std.h"
#include "format.h"
#include "pool.h"
#include "poolclas.h"


typedef struct LOStruct *LO;

extern PoolClass PoolClassLO(void);
extern Bool LOIsValid(LO lo, ValidationType validParam);
extern Error LOCreate(LO *loReturn, Space space, Format format);
extern void LODestroy(LO lo);
extern Pool (LOPool)(LO lo);


#endif /* lo_h */
