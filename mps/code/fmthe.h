/* impl.h.fmthe: HEADERS FOR DYLAN-LIKE OBJECT FORMATS
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

#define headerSIZE (32)
#define headerTypeBits 1
#define realTYPE 0
#define realHeader realTYPE
#define padTYPE  1
#define headerType(header) ((header) & ((1 << headerTypeBits) - 1))
#define headerPadSize(header) ((header) >> headerTypeBits)
#define padHeader(size) ((size << headerTypeBits) | padTYPE)

#endif /* fmthe_h */
