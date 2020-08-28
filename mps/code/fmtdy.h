/* fmtdy.h: DYLAN OBJECT FORMAT
 *
 * $Id$
 * Copyright (c) 2001-2020 Ravenbrook Limited.  See end of file for license.
 */

#ifndef fmtdy_h
#define fmtdy_h

#include "mps.h"

/* Low-level routines, exposed here so that the with-header format
 * can use common code. */
extern mps_res_t dylan_scan1(mps_ss_t, mps_addr_t *);
extern mps_res_t dylan_scan1_weak(mps_ss_t, mps_addr_t *);

/* Format */
extern mps_fmt_A_s *dylan_fmt_A(void);
extern mps_fmt_A_s *dylan_fmt_A_weak(void);
extern mps_fmt_B_s *dylan_fmt_B(void);
extern mps_fmt_B_s *dylan_fmt_B_weak(void);
extern mps_res_t dylan_fmt(mps_fmt_t *, mps_arena_t);
extern mps_res_t dylan_fmt_weak(mps_fmt_t *, mps_arena_t);

extern mps_addr_t dylan_weak_dependent(mps_addr_t);

extern mps_addr_t dylan_skip(mps_addr_t);
extern void dylan_pad(mps_addr_t, size_t);
extern mps_bool_t dylan_ispad(mps_addr_t);
extern int dylan_wrapper_check(mps_word_t *);

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


/* C. COPYRIGHT AND LICENSE
 *
 * Copyright (C) 2001-2020 Ravenbrook Limited <https://www.ravenbrook.com/>.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright
 *   notice, this list of conditions and the following disclaimer in the
 *   documentation and/or other materials provided with the
 *   distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
 * IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 * TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
 * PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 * HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
