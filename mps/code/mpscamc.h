/* impl.h.mpscamc: MEMORY POOL SYSTEM CLASS "AMC"
 *
 * $Id$
 * Copyright (c) 2001 Ravenbrook Limited.
 */

#ifndef mpscamc_h
#define mpscamc_h

#include "mps.h"

extern mps_class_t mps_class_amc(void);
extern mps_class_t mps_class_amcz(void);

extern void mps_amc_apply(mps_pool_t,
                          void (*)(mps_addr_t, void *, size_t),
                          void *, size_t);

#endif /* mpscamc_h */
