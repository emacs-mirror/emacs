/* impl.h.fmtdytst: DYLAN OBJECT FORMAT TESTING
 *
 * $Id: //info.ravenbrook.com/project/mps/master/code/fmtdy.h#7 $
 * Copyright (c) 2001 Ravenbrook Limited.
 */

#ifndef fmtdytst_h
#define fmtdytst_h

#include "mps.h"

extern mps_res_t dylan_init(mps_addr_t addr, size_t size,
                            mps_addr_t *refs, size_t nr_refs);
extern void dylan_write(mps_addr_t addr,
                        mps_addr_t *refs, size_t nr_refs);
extern mps_addr_t dylan_read(mps_addr_t addr);
extern mps_bool_t dylan_check(mps_addr_t addr);
extern void dylan_pad(mps_addr_t addr, size_t size);
extern int dylan_wrapper_check(mps_word_t *w);

#endif /* fmtdy_h */
