/*  impl.h.poolmrg: MANUAL RANK GUARDIAN POOL CLASS INTERFACE
 *
 *  $Id$
 *
 * Copyright (c) 2001 Ravenbrook Limited.
 * Copyright (c) 2002 Global Graphics Software.
 */

#ifndef poolmrg_h
#define poolmrg_h

#include "mpmtypes.h"

typedef struct MRGStruct *MRG;

extern PoolClass PoolClassMRG(void);
extern Res MRGRegister(Pool, Ref);
extern Res MRGDeregister(Pool, Ref);

#endif /* poolmrg_h */
