/* impl.h.formdyl: DYLAN OBJECT FORMAT
 *
 * $HopeName$
 * Copyright (C) 1996 Harlequin Group, all rights reserved
 */

#ifndef formdyl_h
#define formdyl_h

#include "mps.h"

extern mps_form_A_s *dylan_form_A(void);
extern mps_res_t dylan_init(mps_addr_t base, size_t size,
                            mps_addr_t *refs, size_t nr_refs);
extern void dylan_write(mps_addr_t base,
                        mps_addr_t *refs, size_t nr_refs);
extern mps_addr_t dylan_read(mps_addr_t base);

#endif /* formdyl_h */
