/*  impl.h.amc  draft impl
 *
 *               MANUAL RANK GUARDIAN POOL CLASS
 *
 *  $HopeName: MMsrc!poolmrg.h(trunk.1) $
 *  Copyright (C) 1995,1997 Harlequin Group, all rights reserved
 */

#ifndef poolmrg_h
#define poolmrg_h

#include "mpm.h"

typedef struct MRGStruct *MRG;

extern PoolClass PoolClassMRG(void);
extern Res MRGRegister(Pool, Ref);

#endif /* poolmrg_h */
