/* impl.h.mpscmv: MEMORY POOL SYSTEM CLASS "MV"
 *
 * $HopeName: MMsrc!mpscmv.h(trunk.3) $
 * Copyright (C) 1998 Harlequin Limited.  All rights reserved.
 */

#ifndef mpscmv_h
#define mpscmv_h

#include "mps.h"

extern size_t mps_mv_free_size(mps_pool_t mps_pool);
extern size_t mps_mv_size(mps_pool_t mps_pool);
extern mps_class_t mps_class_mv(void);
extern mps_class_t mps_class_mv_debug(void);

#endif /* mpscmv_h */
