/* impl.h.fmthe: DYLAN-LIKE OBJECT FORMAT WITH HEADERS
 *
 * $Id$
 * Copyright (c) 2001 Ravenbrook Limited.
 */

#ifndef fmthe_h
#define fmthe_h

#include "mps.h"


/* Formats */
extern mps_res_t EnsureHeaderFormat(mps_fmt_t *, mps_arena_t);
extern mps_res_t EnsureHeaderWeakFormat(mps_fmt_t *, mps_arena_t);
extern mps_res_t HeaderFormatCheck(mps_addr_t addr);
extern mps_res_t HeaderWeakFormatCheck(mps_addr_t addr);

/* dependent object function for weak pool */
extern mps_addr_t dylan_weak_dependent(mps_addr_t);

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


#define headerSIZE (32)
#define headerTypeBits 1
#define realTYPE 0
#define realHeader realTYPE
#define padTYPE  1
#define headerType(header) ((header) & ((1 << headerTypeBits) - 1))
#define headerPadSize(header) ((header) >> headerTypeBits)
#define padHeader(size) ((size << headerTypeBits) | padTYPE)

#endif /* fmthe_h */
