/* impl.h.fmtdy: DYLAN OBJECT FORMAT
 *
 * $HopeName: MMsrc!fmtdy.h(trunk.1) $
 * Copyright (C) 1996 Harlequin Group, all rights reserved
 */

#ifndef fmtdy_h
#define fmtdy_h

#include "mps.h"

extern mps_fmt_A_s *dylan_fmt_A(void);
extern mps_res_t dylan_init(mps_addr_t addr, size_t size,
                            mps_addr_t *refs, size_t nr_refs);
extern void dylan_write(mps_addr_t addr,
                        mps_addr_t *refs, size_t nr_refs);
extern mps_addr_t dylan_read(mps_addr_t addr);
extern mps_bool_t dylan_check(mps_addr_t addr);

#endif /* fmtdy_h */
