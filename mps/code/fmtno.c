/* impl.c.fmtno: NULL OBJECT FORMAT IMPLEMENTATION
 *
 *  $Id$
 *  Copyright (c) 2001 Ravenbrook Limited.
 *
 * .readership: MPS developers
 */


#include "fmtno.h"
#include "mps.h"
#include <assert.h>
#include <string.h>
#include <stdlib.h>

#ifdef MPS_PF_SUS8LC
/* .hack.stderr: builder.lc (LCC) uses Sun's header files.  Sun's
 * assert.h is broken, as it assumes it can use stderr.  We have to
 * fix it by supplying stderr.
 */
#include <stdio.h>
#endif


#define notreached()    assert(0)
#define unused(param)   ((void)param)

#ifdef MPS_BUILD_MV

/* MSVC 2.0 generates a warning for unused(). */
#ifdef _MSC_VER
#if _MSC_VER < 1000
#pragma warning(disable: 4705)
#endif
#else /* _MSC_VER */
#error "Expected _MSC_VER to be defined for builder.mv"
#endif /* _MSC_VER */

/* windows.h causes warnings about "unreferenced inline function */
/* has been removed". */
#pragma warning(disable: 4514)

#endif /* MPS_BUILD_MV */

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
