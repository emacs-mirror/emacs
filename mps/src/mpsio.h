/* impl.h.mpsio: HARLEQUIN MEMORY POOL SYSTEM I/O INTERFACE
 *
 * $HopeName: MMsrc!mpsio.h(MMdevel_event.2) $
 * Copyright (C) 1996 Harlequin Group, all rights reserved.
 *
 * .readership: MPS client application developers, MPS developers.
 * .sources: design.mps.io
 *
 * TRANSGRESSIONS (rule.impl.trans)
 *
 * There's no way this meets all the reqiurements yet.
 */

#ifndef mpsio_h
#define mpsio_h

#include "mps.h"	/* for mps_res_t */

typedef struct mps_io_s *mps_io_t;

extern mps_res_t mps_io_create(mps_io_t *mps_io_r);
extern void mps_io_destroy(mps_io_t mps_io);

extern mps_res_t mps_io_write(mps_io_t mps_io, void *mps_buf, size_t mps_size);
extern mps_res_t mps_io_flush(mps_io_t mps_io);

#endif /* mpsio_h */
