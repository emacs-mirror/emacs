/* impl.h.prmcw3:  PROTECTION FOR WIN32
 *
 * $HopeName: $
 * Copyright (C) 1998, 1999 The Harlequin Group Limited.  All rights reserved.
 *
 * .readership: MPS developers.
 */

#ifndef prmcw3_h
#define prmcw3_h


#include "mpm.h"

#include "mpswin.h"


typedef struct MutatorFaultContextStruct { /* Protection fault context data */
  LPEXCEPTION_POINTERS ep;                   /* Windows Exception Pointers */
} MutatorFaultContextStruct;


#endif /* prmcw3_h */
