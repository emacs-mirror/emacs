/* impl.h.mpsio: HARLEQUIN MEMORY POOL SYSTEM I/O INTERFACE
 *
 * $HopeName: MMsrc!mpsio.h(trunk.2) $
 * Copyright (C) 1996 Harlequin Limited.  All rights reserved.
 *
 * .readership: For MPS client application developers, MPS developers.
 * .sources: design.mps.io
 */

#ifndef mpsio_h
#define mpsio_h

#include "mps.h"	/* for mps_res_t */


typedef struct mps_io_s *mps_io_t;

extern mps_res_t mps_io_create(mps_io_t *);
extern void mps_io_destroy(mps_io_t);

extern mps_res_t mps_io_write(mps_io_t, void *, size_t);
extern mps_res_t mps_io_flush(mps_io_t);


#endif /* mpsio_h */
