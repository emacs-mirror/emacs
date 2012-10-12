.. _topic-root:

=====
Roots
=====

:c:macro:`MPS_RM_CONST` is a preprocessor macro defining a constant that can be or'ed with other ``MPS_RM`` constants, and passed as the :term:`root mode` argument to certain root creation functions (:c:func:`mps_root_create`, :c:func:`mps_root_create_fmt`, :c:func:`mps_root_create_table`, :c:func:`mps_root_create_table_masked`, and :c:func:`mps_root_create_reg`).

from :c:macro:`MPS_RM_PROT`:

No page may contain parts of two or more roots with :c:macro:`MPS_RM_PROT` [how does one prevent
that?]. You mustn't specify :c:macro:`MPS_RM_PROT` if the client program or
anything other than (this instance of) the MPS is going to protect or
unprotect the relevant pages.


Internal Notes

Future meaning: The MPS may place a hardware read and/or write barrier on any pages which any part of the root covers. Format methods and scanning functions (except for the one for this root) may not read or write data in this root. You may specify :c:macro:`MPS_RM_PROT` on a root allocated from the MPS, as long as it's not from a GCd pool. - drj 1997-12-18</p>

This feature is far too technical for most of our clients: we should think about producing some guidelines on how to use it. - pekka 1998-01-27

There may be problems if the client wants the OS to access the root. Lots of OSes can't cope with writing to protected pages. So we'll need to document that caveat too. drj 1998-05-20


::

    static mps_root_t mmRoot;

    int main(void)
    {
        mps_res_t res;

        /* ... */

        res = mps_root_create(&mmRoot, arena, MPS_RANK_EXACT, (mps_rm_t)0,
                              &rootScanner, NULL, 0);
        /* see doc of mps_root_scan_t for definition of rootScanner */
        if (res != MPS_RES_OK)
            exit(1);

        /* ... */
    }




.. note::

    Unless the rank of the root is not :c:macro:`MPS_RANK_AMBIG`,
    the contents of the root have to be valid whenever a
    :term:`garbage collection` happens. That is, all the
    references fixed by the root scanning function have to be
    references to actual objects or null pointers. If you're using
    :term:`asynchronous` garbage collection, this could be as soon
    as the root is registered, so the root has to be valid when it
    is registered. As with an ordinary :term:`scan method`, a root
    scanning function is allowed to fix references which point to
    memory not managed by the MPS. These references will be
    ignored.

::

    static mps_root_t mmRoot;
    SegmentDescriptor DataSegment;

    int main(void)
    {
        mps_res_t res;

        /* ... */

        mps_addr_t base = DataSegment.base;
        mps_addr_t limit = DataSegment.base + SegmentLength;
        res = mps_root_create_fmt(&mmRoot, arena, MPS_RANK_EXACT, (mps_rm_t)0,
                                  &scan_objs, base, limit);

        /* see doc of mps_fmt_scan_t for definition of scan_objs */

        if (res != MPS_RES_OK)
            exit( EXIT_FAILURE );

        /* ... */
    }

::

    typedef struct {
        mps_root_t mmRoot;
        mps_thr_t thread;
        /* ...  */
    } ThreadLocals;

    void InitThread(ThreadLocals *thr)
    {
        /* This is a hack to find the bottom of the stack. */
        void *stackBottom = &stackBottom;

        mps_thread_reg(&thr->thread, arena);
        mps_root_create_reg(&thr->mmRoot, arena, MPS_RANK_AMBIG, (mps_rm_t) 0,
                            thr->thread, mps_stack_scan_ambig, stackBottom, 0);

        /* ...  */
    }

::

    static mps_root_t mmRoot;
    Object *Objects[rootCOUNT];

    int main(void)
    {
      mps_res_t res;

      /* ... */

      res = mps_root_create_table(&mmRoot, arena, MPS_RANK_EXACT, (mps_rm_t)0,
                                  (mps_addr_t)&Objects, rootCOUNT);

      if (res != MPS_RES_OK)
          exit(1);

      /* ... */
    }

::

    #define tagMASK 0x0003

    static mps_root_t mmRoot;
    Object *Objects[rootCOUNT];

    int main(void)
    {
        mps_res_t res;

        /* ... */

        res = mps_root_create_table_masked(&mmRoot, arena, MPS_RANK_EXACT,
                                           (mps_rm_t)0,
                                           (mps_addr_t)&Objects, rootCOUNT,
                                           (mps_word_t)tagMASK);
        if (res != MPS_RES_OK)
            exit(1);

        /* ... */
    }

::

    static StackFrame *stackBottom;

    /* root scanner for an imaginary interpreter for a stack-oriented language */
    static mps_res_t rootScanner(mps_ss_t ss, void * p, size_t s)
    {
        StackFrame *frame;
        size_t i;
        mps_res_t res;

        UNUSED(p);
        UNUSED(s);

        for(frame = stackBottom; frame != NULL; frame = frame->next) {
            for(i = frame->size; i > 0; --i) {
                res = mps_fix(ss, &frame->locals[i]);
                if (res != MPS_RES_OK) return res;
            }
        }

        return res;
    }
