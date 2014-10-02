.. Sources:

    `<https://info.ravenbrook.com/project/mps/master/design/finalize/>`_

.. index::
   single: finalization

.. _topic-finalization:

Finalization
============

It is sometimes necessary to perform actions when a block of memory
:term:`dies <dead>`. For example, a block may represent the
acquisition of an external resource such as a file handle or a network
connection. When the block dies, the corresponding resource must be
released. This procedure is known as :term:`finalization`.

A block requiring finalization must be registered by calling :c:func:`mps_finalize`::

    mps_addr_t ref = block_requiring_finalization;
    mps_finalize(arena, &ref);

A block that been registered for finalization becomes *finalizable* as
soon as the :term:`garbage collector` observes that it would otherwise
be :term:`reclaimed` (that is, the only thing keeping it alive is the
fact that it needs to be finalized). If a block is finalizable the MPS
may choose to finalize it (by posting a finalization message: see
below) at *any* future time.

.. note::

    This means that a block that was determined to be finalizable, but
    then became unconditionally :term:`live` by the creation of a new
    :term:`strong reference` to it, may still be finalized.

:term:`Weak references (1)` do not prevent blocks
from being finalized. At the point that a block is finalized, weak
references will still validly refer to the block. The fact that a
block is registered for finalization prevents weak references to that
block from being :term:`splatted <splat>`. See :ref:`topic-weak`.

The Memory Pool System finalizes a block by posting a *finalization
message* to the :term:`message queue` of the :term:`arena` in which
the block was allocated.

.. note::

    This design avoids the problems that can result from the
    :term:`garbage collector` calling a function in the client program
    to do the finalization. In such an implementation, the client
    program's finalization code may end up running concurrently with
    other code that accesses the underlying resource, and so access to
    the resource needs to be guarded with a lock, but then an unlucky
    scheduling of finalization can result in deadlock. See :ref:`Boehm
    (2002) <BOEHM02>` for a detailed discussion of this issue.

The :term:`message type` of finalization messages is
:c:func:`mps_message_type_finalization`, and the client program must
enable the posting of these messages by calling
:c:func:`mps_message_type_enable` before any block becomes
finalizable::

    mps_message_type_enable(arena, mps_message_type_finalization());

When a finalization message has been retrieved from the message queue
by calling :c:func:`mps_message_get`, the finalization reference may
be accessed by calling :c:func:`mps_message_finalization_ref`. The
finalization message keeps the block alive until it is discarded by
calling :c:func:`mps_message_discard`.

.. note::

    The client program may choose to keep the finalized block alive by
    keeping a strong reference to the finalized object after
    discarding the finalization message.

    This process is known as :term:`resurrection` and in some
    finalization systems requires special handling, but in the MPS
    this just is just the usual result of the rule that strong
    references keep objects alive.

    It is fine to re-register a block for finalization after
    retrieving its finalization message from the message queue. This
    will cause it to be finalized again should all strong references
    disappear again.

.. note::

    Calling :c:func:`mps_message_discard` does not reclaim the space
    occupied by the finalized block (that happens at the next
    collection, if the block is found to be dead at that point), and
    so the block must remain validly formatted (:term:`scannable <scan
    method>`, :term:`skippable <skip method>`, and so on). It might
    make sense to replace it with a :term:`padding object`.

See :ref:`topic-message` for details of the message mechanism.


.. index::
   single: finalization; multiple

Multiple finalizations
----------------------

A block may be registered for finalization multiple times. A block
that has been registered for finalization *n* times will be finalized
at most *n* times.

This may mean that there are multiple finalization messages on the
queue at the same time, or it may not (it may be necessary for the
client program to discard previous finalization messages for a block
before a new finalization messages for that block are posted to the
message queue). The MPS provides no guarantees either way: a client
program that registers the same block multiple times must cope with
either behaviour.


.. index::
   pair: finalization; cautions

.. _topic-finalization-cautions:

Cautions
--------

#.  Don't rely on finalization for your program to work. Treat it as an
    optimization that enables the freeing of resources that the
    garbage collector can prove are unreachable.

#.  The MPS provides no guarantees about the promptness of
    finalization. The MPS does not finalize a block until it
    determines that the block is finalizable, which may require a full
    garbage collection in the worst case, and such a collection may
    not be :ref:`scheduled <topic-collection-schedule>` for some time.
    Or the block may never become finalizable because it is
    incorrectly determined to be reachable due to an :term:`ambiguous
    reference` pointing to it. Or the block may never become
    finalizable because it remains reachable through a reference, even
    if that reference might never be used.

#.  Even when blocks are finalized in a reasonably timely fashion, the
    client needs to process the finalization messages in time to avoid
    the resource running out. For example, in the Scheme interpreter,
    finalization messages are only processed at the end of the
    read–eval–print loop, so a program that opens many files may run
    out of handles even though the associated objects are all
    finalizable, as shown here:

    .. code-block:: none

        MPS Toy Scheme Example
        9960, 0> (define (repeat n f _) (if (eqv? n 0) '() (repeat (- n 1) f (f))))
        repeat
        10840, 0> (repeat 300 (lambda () (open-input-file "scheme.c")) 0)
        open-input-file: cannot open input file

    A less naïve interpreter might process finalization messages on a
    more regular schedule, or might take emergency action in the event
    of running out of open file handles by carrying out a full garbage
    collection and processing any finalization messages that are
    posted as a result.

    If you are designing a programming language then it is generally a
    good idea to provide the programmer with a mechanism for ensuring
    prompt release of scarce resources. For example, Scheme provides
    the ``(with-input-from-file)`` procedure which specifies that the
    created port has :term:`dynamic extent` (and so can be closed as
    soon as the procedure exits, even if it is still reachable).

#.  The MPS does not finalize objects in the context of
    :c:func:`mps_arena_destroy` or :c:func:`mps_pool_destroy`.
    Moreover, if you have pools containing objects registered for
    finalization, you must destroy these pools by following the “safe
    tear-down” procedure described under :c:func:`mps_pool_destroy`.

    .. note::

        Under normal circumstances, finalization code can assume that
        objects referenced by the object being finalized ("object F")
        have themselves not yet been finalized. (Because object F is
        keeping them alive.) If finalization code is run at program
        exit, this assumption is no longer true. It is much more
        difficult to write correct code if it has to run under both
        circumstances.

        This is why Java's ``System.runFinalizersOnExit`` is
        deprecated. See Appendix A of :ref:`Boehm (2002) <BOEHM02>`
        for a discussion of this problem.

    .. note::

        The only reliable way to ensure that all finalizable objects
        are finalized is to maintain a table of :term:`weak
        references (1)` to all such objects. The weak references don't
        prevent the objects from being finalized, but you can iterate
        over the list at an appropriate point and finalize any
        remaining objects yourself.

#.  Not all :term:`pool classes` support finalization. In general, only
    pools that manage objects whose liveness is determined by garbage
    collection do so. See the :ref:`pool`.


.. index::
   single: finalization; interface

Finalization interface
----------------------

.. c:function:: mps_res_t mps_finalize(mps_arena_t arena, mps_addr_t *ref_p)

    Register a :term:`block` for :term:`finalization`.

    ``arena`` is the arena in which the block lives.

    ``ref_p`` points to a :term:`reference` to the block to be
    registered for finalization.
 
    Returns :c:macro:`MPS_RES_OK` if successful, or another
    :term:`result code` if not.

    This function registers the block pointed to by ``*ref_p`` for
    finalization. This block must have been allocated from an
    automatically managed :term:`pool` in ``arena``.

    .. note::

        This function receives a pointer to a reference. This is to
        avoid placing the restriction on the :term:`client program`
        that the C call stack be a :term:`root`.


.. c:function:: mps_res_t mps_definalize(mps_arena_t arena, mps_addr_t *ref_p)

    Deregister a :term:`block` for :term:`finalization`.

    ``arena`` is the arena in which the block lives.

    ``ref_p`` points to a :term:`reference` to the block to be
    deregistered for finalization.

    Returns :c:macro:`MPS_RES_OK` if successful, or
    :c:macro:`MPS_RES_FAIL` if the block was not previously registered
    for finalization.

    .. note::

        This function receives a pointer to a reference. This is to
        avoid placing the restriction on the :term:`client program`
        that the C call stack be a :term:`root`.

    .. warning::

        Definalization is not yet efficient: the current
        implementation just loops over all finalized objects. If you
        need efficient definalization, please :ref:`contact us
        <contact>`.


.. index::
   pair: finalization; message

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
