.. _topic-format:

==============
Object formats
==============

See //info.ravenbrook.com/project/mps/doc/2002-06-18/obsolete-mminfo/mmdoc/protocol/mps/format/index.html

::

    mps_fmt_t create_format(mps_arena_t arena)
    {
        mps_fmt my_format;
        mps_res_t res;
        mps_fmt_A_s my_format_A = {
            my_alignment, &my_scan, &my_skip, &my_copy, &my_fwd,
            &my_isfwd, &my_pad
        };

        res = mps_fmt_create_A(&my_format, arena, &my_format_A);
        assert(res != MPS_RES_OK);

        return my_format;
    }

::

    mps_fmt_t create_format(mps_arena_t arena)
    {
        mps_fmt my_format;
        mps_res_t res;
        mps_fmt_B_s my_format_B = {
            my_alignment, &my_scan, &my_skip, &my_copy,
            &my_fwd, &my_isfwd, &my_pad, &my_class
        };

        res = mps_fmt_create_B(&my_format, arena, &my_format_B);
        assert(res != MPS_RES_OK);

        return my_format;
    }

::

    mps_fmt_t create_format(mps_arena_t arena)
    {
        mps_fmt format;
        mps_res_t res;
        mps_fmt_auto_header_s format_desc = { 
            my_alignment, &my_scan, &my_skip, &my_fwd,
            &my_isfwd, &my_pad, HEADER_SIZE
        };

        res = mps_fmt_create_auto_header(&format, arena, &format_desc);
        assert(res != MPS_RES_OK);

        return format;
    }

::

    mps_addr_t my_class_method(mps_addr_t object) {
        my_object_generic_t generic_object = object;
        return (mps_addr_t)(generic_object.class);
    }

::

    /* define the function */

    void example_fwd(mps_addr_t old, mps_addr_t new)
    {
        /* ... */
    }

    /* also define example_scan, example_skip, etc */
    /* store pointer to function in the format variant struct */
    struct mps_fmt_B_s example_fmt_B = {
        4, /* align */
        example_scan,
        example_skip,
        example_copy,
        example_fwd,
        example_isfwd,
        example_pad,
        example_class
    };

    /* The (address of the) example_fmt_B object can now be passed to */
    /* mps_fmt_create_B to create a format. */

::

    mps_addr_t my_skip_method(mps_addr_t object)
    {
        char *p = (char *)object;
        my_object_t my_object = (my_object_t)object;
        return ((mps_addr_t)(p + my_object->length));
    }

::

    #include "mps.h"
    #include "mpscamc.h"
    #include <stdlib.h>

    struct mps_fmt_A_s fmt_A_s = {
        (mps_align_t)4,
        scan, skip, copy, move, isMoved, pad
    };

    void go(mps_space_t space)
    {
        mps_fmt_t format;
        mps_res_t res;
        mps_pool_t pool;

        res = mps_fmt_create_A(&format, space, &mps_fmt_A_s);
        if (res != MPS_RES_OK)
            abort();

        res = mps_pool_create(&pool, space, mps_class_amc(), format);
        if (res != MPS_RES_OK)
            abort();

        /* do some stuff here */

        mps_pool_destroy(pool);
        mps_format_destroy(format);
    }
