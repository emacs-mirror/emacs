.. _topic-finalization:

Finalization
============

An object becomes finalizable if it is registered for finalization and the collector observes that it would otherwise be reclaimable. Once an object is finalizable the MPS may choose to finalize it (by posting a finalization message, see below) at <em>any</em> future time. Note that the subsequent creation of strong references to the object (from, say, weak references) may cause finalization to occur when an object is not otherwise reclaimable. 

When an object is finalizable, it may be finalized up to N times, where N is the number of times it has been registered for finalization. When an object is finalized, it is also deregistered for finalization (so that it will not be finalized again from the same registration).

Finalization is performed by passing a finalization message to the client, containing an exact reference to the object. See the message protocol, :c:func:`mps_message_type_finalization`, and :c:func:`mps_message_finalization_ref` for details.

If an object is registered for finalization multiple times, then there may be multiple finalization messages on the queue at the same time. On the other hand it may be necessary to discard previous finalization messages for an object before all such messages are posted on the message queue. In other words a finalization message may prevent other finalizations of the same object from occurring until the message is deleted; or, it may not.  We don't provide any guarantees either way. Clients performing multiple registrations must cope with both behaviors. In any case we expect it to be unusual for clients to register the same object multiple times.

Note that there is no guarantee that finalization will be prompt.

<a href="#mps_rank_weak">Weak references</a> do not prevent objects from being finalized.  At the point that an object is finalized, weak references will still validly refer to the object.  The fact that an object is registered for finalization prevents weak references to that object from being deleted.

Note that there will be no attempt to finalize objects in the context of :c:func:`mps_arena_destroy` or :c:func:`mps_pool_destroy`. :c:func:`mps_pool_destroy` should therefore not be invoked on pools containing objects registered for finalization.

Not all pool classes support finalization of objects.  In general only pools that manage objects whose liveness is determined by garbage collection will support finalization of objects.  For more information, see the Pool Class Catalog.

::

    mps_message_type_t type;

    if (mps_message_queue_type(&type, arena)) {
        if (type == mps_message_type_finalization()) {
            process_finalization_message_from_queue();
        } else {
            unknown_message_type();
        }
    }


Interface
---------

.. c:function:: mps_res_t mps_definalize(mps_arena_t arena, mps_addr_t *ref)

    Deregister a :term:`block` for :term:`finalization`.

    ``arena`` is the arena in which the block lives.

    ``ref`` points to a :term:`reference` to the block to be
    deregistered for finalization.

    Returns :c:macro:`MPS_RES_OK` if successful, or
    :c:macro:`MPS_RES_FAIL` if the block was not previously registered
    for finalization.

    .. note::

        This function receives a pointer to a reference. This is to
        avoid placing the restriction on the :term:`client program`
        that the C call stack be a :term:`root`.


.. c:function:: mps_res_t mps_finalize(mps_arena_t arena, mps_addr_t *ref)

    Register a :term:`block` for :term:`finalization`.

    ``arena`` is the arena in which the block lives.

    ``ref`` points to a :term:`reference` to the block to be
    registered for finalization.
 
    Returns :c:macro:`MPS_RES_OK` if successful, or another
    :term:`result code` if not.

    This function registers the block pointed to by ``*ref`` for
    finalization. This block must have been allocated from a
    :term:`pool` in ``arena``. Violations of this constraint may not
    be checked by the MPS, and may be unsafe, causing the MPS to crash
    in undefined ways.

    .. note::

        This function receives a pointer to a reference. This is to
        avoid placing the restriction on the :term:`client program`
        that the C call stack be a :term:`root`.


Finalization messages
---------------------

.. c:function:: mps_message_type_t mps_message_type_finalization(void)

    Return the :term:`message type` of finalization messages.

    Finalization messages are used by the MPS to implement
    :term:`finalization`. When the MPS detects that a block that has
    been registered for finalization (by calling
    :c:func:`mps_finalize`) is finalizable, it finalizes it by posting
    a :term:`message` of this type.

    Note that there might be delays between the block becoming
    finalizable, the MPS detecting that, and the message being
    posted.

    In addition to the usual methods applicable to messages,
    finalization messages support the
    :c:func:`mps_message_finalization_ref` method which returns a
    reference to the block that was registered for finalization.

    .. seealso::

        :ref:`topic-message`.


.. c:function:: void mps_message_finalization_ref(mps_addr_t *ref_o, mps_arena_t arena, mps_message_t message)

    Returns the finalization reference for a finalization message.

    ``ref_o`` points to a location that will hold the finalization
    reference.

    ``arena`` is the :term:`arena` which posted the message.

    ``message`` is a message retrieved by :c:func:`mps_message_get` and
    not yet discarded. It must be a finalization message: see
    :c:func:`mps_message_type_finalization`.

    The reference returned by this method is a reference to the block
    that was originally registered for :term:`finalization` by a call
    to :c:func:`mps_finalize`.

    .. note::

        The reference returned is subject to the normal constraints,
        such as might be imposed by a :term:`moving <moving garbage
        collector>` collection, if appropriate. For this reason, it is
        stored into the location pointed to by ``ref_o`` in order to
        enable the :term:`client program` to place it directly into
        scanned memory, without imposing the restriction that the C
        stack be a :term:`root`.

        The message itself is not affected by invoking this method.
        Until the client program calls :c:func:`mps_message_discard`
        to discard the message, it will refer to the object and
        prevent its reclamation.

    .. seealso::

        :ref:`topic-message`.
