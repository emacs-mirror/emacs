/* impl.h.prmcli:  PROTECTION MUTATOR CONTEXT (Linux)
 *
 * $Id$
 * $HopeName: MMsrc!prmcli.h(trunk.1) $
 * Copyright (c) 2001 Ravenbrook Limited.
 *
 * .readership: MPS developers.
 */

#ifndef prmcli_h
#define prmcli_h


/* open sesame magic */
#define _BSD_SOURCE 1
#define _POSIX_C_SOURCE 1

#include "mpm.h"

#include <signal.h>

typedef struct MutatorFaultContextStruct { /* Protection fault context data */
  struct sigcontext *scp;                   /* Linux sigcontext */
} MutatorFaultContextStruct;


#endif /* prmcli_h */
