.. sources:

    `<https://info.ravenbrook.com/project/mps/master/design/poolawl/>`_

.. index::
   single: weak references

.. _topic-weak:

Weak references
===============

A :dfn:`weak reference` is a :term:`reference` that does not keep the
block it refers to :term:`alive <live>`.

The open source MPS supports weak references only:

#. in :term:`roots` that are registered with :term:`rank`
   :c:func:`mps_rank_weak`;

#. in objects allocated on an :term:`allocation point` in a pool of
   class :ref:`pool-awl` that was created with :term:`rank`
   :c:func:`mps_rank_weak`.

.. note::

    If you need more general handling of weak references,
    :ref:`contact us <contact>`.

When the MPS determines that a block is only kept alive by one or more
weak references, it may choose to :term:`splat` those references by
replacing them with null pointers when they are :term:`fixed`. When
all weak references to the block have been splatted, the block may be
reclaimed.

For example, a :term:`scan method` for objects in an AWL pool might
look like this::

    mps_res_t obj_scan(mps_ss_t ss, mps_addr_t base, mps_addr_t limit)
    {
        MPS_SCAN_BEGIN(ss) {
            while (base < limit) {
                obj_t obj = base;
                mps_addr_t p = obj->ref;
                if (MPS_FIX1(ss, p)) {
                    mps_res_t res = MPS_FIX2(ss, &p);
                    if (res != MPS_RES_OK) return res;
                    if (p == NULL) {
                        /* reference was splatted */
                    }
                    obj->ref = p;
                }
                base += sizeof(*obj);
            }
        } MPS_SCAN_END(ss);
        return MPS_RES_OK;
    }

A reference that passes the "interesting" test :c:func:`MPS_FIX1`
can't be a null pointer, so if the reference is discovered to be null
after calling :c:func:`MPS_FIX2` then it must have just been splatted.

.. note::

    Because weak references are splatted when they are fixed, not all
    weak references to a block are splatted at the same time.
    Depending on the decisions the MPS makes about which objects to
    scan, a weak reference may live on for some time after other weak
    references to the same block have been splatted.

.. note::

    A common way in which weak references are used in programming
    languages is in :term:`weak-key <weak-key hash table>` and
    :term:`weak-value hash tables`. A weak-key hash table contains
    weak references to its keys: when it detects that a key has been
    splatted, it deletes the corresponding value. The :ref:`pool-awl`
    pool class supports this by allowing you to specify for each
    object, a :term:`dependent object` which may be written to by the
    :term:`scan method`. See :ref:`pool-awl-dependent`.

.. note::

    Weak references do not prevent blocks from being :term:`finalized
    <finalization>`. At the point that a block is finalized, weak
    references will still validly refer to the block. The fact that a
    block is registered for finalization prevents weak references to
    that block from being splatted. See :ref:`topic-finalization`.
