/* fmtno.c: NULL OBJECT FORMAT IMPLEMENTATION
 *
 *  $Id$
 *  Copyright (c) 2001-2020 Ravenbrook Limited.  See end of file for license.
 *
 * .readership: MPS developers
 */


#include "fmtno.h"
#include "mps.h"
#include <assert.h>
#include <string.h>
#include <stdlib.h>


#define notreached()    assert(0)
#define unused(param)   ((void)param)


#define ALIGN           sizeof(mps_word_t)

/* Functions for the null format. */

mps_res_t no_scan(mps_ss_t mps_ss,
                  mps_addr_t base,
                  mps_addr_t limit)
{
    unused(mps_ss); unused(base); unused(limit);
    notreached();
    return 0;
}

mps_addr_t no_skip(mps_addr_t object)
{
    unused(object);
    notreached();
    return 0;
}

void no_copy(mps_addr_t old,
             mps_addr_t new)
{
    unused(old); unused(new);
    notreached();
}

void no_fwd(mps_addr_t old,
            mps_addr_t new)
{
    unused(old); unused(new);
    notreached();
}

mps_addr_t no_isfwd(mps_addr_t object)
{
    unused(object);
    notreached();
    return 0;
}

void no_pad(mps_addr_t addr,
            size_t size)
{
    unused(addr); unused(size);
    notreached();
}

mps_addr_t no_class(mps_addr_t obj)
{
    unused(obj);
    notreached();
    return 0;
}

/* The null format structures */

static struct mps_fmt_A_s no_fmt_A_s =
{
    ALIGN,
    no_scan,
    no_skip,
    no_copy,
    no_fwd,
    no_isfwd,
    no_pad
};

static struct mps_fmt_B_s no_fmt_B_s =
{
    ALIGN,
    no_scan,
    no_skip,
    no_copy,
    no_fwd,
    no_isfwd,
    no_pad,
    no_class
};

/* Functions returning the null format structures. */

mps_fmt_A_s *no_fmt_A(void)
{
    return &no_fmt_A_s;
}

mps_fmt_B_s *no_fmt_B(void)
{
    return &no_fmt_B_s;
}

/* Format variety-independent version that picks the right format
 * variety and creates it.  */

mps_res_t no_fmt(mps_fmt_t *mps_fmt_o, mps_arena_t arena)
{
    return mps_fmt_create_B(mps_fmt_o, arena, no_fmt_B());
}


/* C. COPYRIGHT AND LICENSE
 *
 * Copyright (C) 2001-2020 Ravenbrook Limited <http://www.ravenbrook.com/>.
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
