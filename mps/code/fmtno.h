/* impl.h.fmtdy: NULL OBJECT FORMAT
 *
 * $Id$
 * Copyright (c) 2001 Ravenbrook Limited.
 */

#ifndef fmtno_h
#define fmtno_h

#include "mps.h"

extern mps_res_t no_scan(mps_ss_t, mps_addr_t, mps_addr_t);
extern mps_addr_t no_skip(mps_addr_t);
extern void no_copy(mps_addr_t, mps_addr_t);
extern void no_fwd(mps_addr_t, mps_addr_t);
extern mps_addr_t no_isfwd(mps_addr_t);
extern void no_pad(mps_addr_t, size_t);
extern mps_addr_t no_class(mps_addr_t);

extern mps_fmt_A_s *no_fmt_A(void);
extern mps_fmt_B_s *no_fmt_B(void);
extern mps_res_t no_fmt(mps_fmt_t *, mps_arena_t);

#endif /* fmtno_h */
