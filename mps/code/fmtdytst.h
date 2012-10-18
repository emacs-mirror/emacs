/* fmtdytst.h: DYLAN OBJECT FORMAT TESTING
 *
 * $Id$
 * Copyright (c) 2001 Ravenbrook Limited.  See end of file for license.
 */

#ifndef fmtdytst_h
#define fmtdytst_h

#include "mps.h"
#include "testlib.h"

extern mps_res_t dylan_init(mps_addr_t addr, size_t size,
                            mps_addr_t *refs, size_t nr_refs);
extern void dylan_write(mps_addr_t addr,
                        mps_addr_t *refs, size_t nr_refs);
extern void dylan_mutate(mps_addr_t addr);
extern mps_addr_t dylan_read(mps_addr_t addr);
extern mps_bool_t dylan_check(mps_addr_t addr);
extern void dylan_pad(mps_addr_t addr, size_t size);
extern int dylan_wrapper_check(mps_word_t *w);

extern mps_res_t make_dylan_vector(mps_word_t *v, mps_ap_t ap, size_t slots);

#define DYLAN_VECTOR_SLOT(o,n) (((mps_word_t *) (o))[(n)+2])

#define DYLAN_INT(n) (((mps_word_t)(n) << 2) | 1)

#define INT_DYI(n)  ( (n) <= DYLAN_UINT_MAX ? DYLAN_INT(n) : (mps_word_t)fail() )


#define DYLAN_INT_INT(d)  ((mps_word_t)(d) >> 2)

#define DYI_INT(d)  ( ((d) & 0x3) == 0x1 ? DYLAN_INT_INT(d) : (mps_word_t)fail() )

#define DYLAN_UINT_MAX  ((mps_word_t)-1 >> 2)
#define DYLAN_UINT_MASK  DYLAN_UINT_MAX

#endif /* fmtdy_h */


/* C. COPYRIGHT AND LICENSE
 *
 * Copyright (C) 2001-2002 Ravenbrook Limited <http://www.ravenbrook.com/>.
 * All rights reserved.  This is an open source license.  Contact
 * Ravenbrook for commercial licensing options.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 * 1. Redistributions of source code must retain the above copyright
 * notice, this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright
 * notice, this list of conditions and the following disclaimer in the
 * documentation and/or other materials provided with the distribution.
 *
 * 3. Redistributions in any form must be accompanied by information on how
 * to obtain complete source code for this software and any accompanying
 * software that uses this software.  The source code must either be
 * included in the distribution or be available for no more than the cost
 * of distribution plus a nominal fee, and must be freely redistributable
 * under reasonable conditions.  For an executable file, complete source
 * code means the source code for all modules it contains. It does not
 * include source code for modules or files that typically accompany the
 * major components of the operating system on which the executable file
 * runs.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
 * IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 * TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR
 * PURPOSE, OR NON-INFRINGEMENT, ARE DISCLAIMED. IN NO EVENT SHALL THE
 * COPYRIGHT HOLDERS AND CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 * NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF
 * USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
 * ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 * THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
