/* impl.h.fmtdy: DYLAN OBJECT FORMAT
 *
 * $HopeName: MMsrc!fmtdy.h(trunk.3) $
 * Copyright (C) 1997 Harlequin Group, all rights reserved
 */

#ifndef fmtdy_h
#define fmtdy_h

#include "mps.h"

/* Format */
extern mps_fmt_A_s *dylan_fmt_A(void);
extern mps_fmt_A_s *dylan_fmt_A_weak(void);


/* Used only for debugging / testing */
extern mps_res_t dylan_init(mps_addr_t addr, size_t size,
                            mps_addr_t *refs, size_t nr_refs);
extern void dylan_write(mps_addr_t addr,
                        mps_addr_t *refs, size_t nr_refs);
extern mps_addr_t dylan_read(mps_addr_t addr);
extern mps_bool_t dylan_check(mps_addr_t addr);
extern void dylan_pad(mps_addr_t addr, size_t size);
extern int dylan_wrapper_check(mps_word_t *w);

#define WW 0
#define WC 1
#define WM 2
#define WF 3
#define WV 4
#define WS 5
#define WP 6

#define BASIC_WRAPPER_SIZE WS

#define ALIGN           sizeof(mps_word_t)

#endif /* fmtdy_h */
