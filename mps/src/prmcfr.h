/* impl.h.prmcli:  PROTECTION MUTATOR CONTEXT (FreeBSD)
 *
 * $HopeName: $
 * Copyright (C) 1998 The Harlequin Group Limited.  All rights reserved.
 *
 * .readership: MPS developers.
 */

#ifndef prmcfr_h
#define prmcfr_h

#include "mpm.h"

#include <signal.h>

typedef struct MutatorFaultContextStruct { /* Protection fault context data */
  ucontext_t *ucontext;
} MutatorFaultContextStruct;


#endif /* prmcfr_h */
