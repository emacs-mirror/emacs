/* impl.h.fmtdy: DYLAN OBJECT FORMAT
 *
 * $HopeName: MMsrc!fmtdy.h(trunk.6) $
 * Copyright (C) 1997 Harlequin Group, all rights reserved
 */

#ifndef fmtdy_h
#define fmtdy_h

#include "mps.h"

/* Format */
extern mps_fmt_A_s *dylan_fmt_A(void);
extern mps_fmt_A_s *dylan_fmt_A_weak(void);
extern mps_fmt_B_s *dylan_fmt_B(void);
extern mps_fmt_B_s *dylan_fmt_B_weak(void);
extern mps_res_t dylan_fmt(mps_fmt_t *, mps_arena_t);
extern mps_res_t dylan_fmt_weak(mps_fmt_t *, mps_arena_t);


/* Used only for debugging / testing */
extern mps_res_t dylan_init(mps_addr_t addr, size_t size,
                            mps_addr_t *refs, size_t nr_refs);
extern void dylan_write(mps_addr_t addr,
                        mps_addr_t *refs, size_t nr_refs);
extern mps_addr_t dylan_read(mps_addr_t addr);
extern mps_bool_t dylan_check(mps_addr_t addr);
extern void dylan_pad(mps_addr_t addr, size_t size);
extern int dylan_wrapper_check(mps_word_t *w);

/* Constants describing wrappers. Used only for debugging / testing */
#define WW 0    /* offset of Wrapper-Wrapper */
#define WC 1    /* offset of Class pointer*/
#define WM 2    /* offset of subtype Mask */
#define WF 3    /* offset of Fixed part descriptor */
#define WV 4    /* offset of Vector part descriptor */
#define WS 5    /* offset of Size field for pattern vector */
#define WP 6    /* offset of Pattern 0, if present */

#define BASIC_WRAPPER_SIZE (WS + 1) /* size of wrapper with no patterns */

#define ALIGN sizeof(mps_word_t)    /* alignment for Dylan format */

#endif /* fmtdy_h */
