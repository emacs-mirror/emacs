/* impl.h.prmcfr:  PROTECTION MUTATOR CONTEXT (FREEBSD)
 *
 * $Id$
 * Copyright (c) 2001 Ravenbrook Limited.
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
