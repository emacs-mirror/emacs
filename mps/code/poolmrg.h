/*  impl.h.amc  draft impl
 *
 *               MANUAL RANK GUARDIAN POOL CLASS
 *
 *  $Id$
 *  Copyright (c) 2001 Ravenbrook Limited.
 */

#ifndef poolmrg_h
#define poolmrg_h

#include "mpm.h"

typedef struct MRGStruct *MRG;

extern PoolClass PoolClassMRG(void);
extern Res MRGRegister(Pool, Ref);

#endif /* poolmrg_h */
