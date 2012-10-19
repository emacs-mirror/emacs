.. _topic-message:

Messages
========

::

    mps_message_t message;
    mps_clock_t posted_at;

    if (mps_message_get(&message, arena, mps_message_type_gc_start())) {
        posted_at = mps_message_clock(arena, message);
        printf("Collection started at %ul.\n", (unsigned long)posted_at);
    }

::

    mps_message_t message;
    if (mps_message_get(&message, arena, mps_message_type_gc())) {
        size_t live, condemned, not_condemned;
        live = mps_message_gc_live_size(arena, message);
        condemned = mps_message_gc_condemned_size(arena, message);
        not_condemned = mps_message_gc_not_condemned_size(arena,message);
        mps_message_discard(arena, message);
        process_collection_stats(live, condemned, not_condemned);
    }

::

    mps_message_t message;
    if (mps_message_get(&message, arena, mps_message_type_gc_start())) {
        printf("Collection started; reason: %s\n",
               mps_message_gc_start_why(arena, message));
    }


Interface
---------

.. c:function:: mps_clock_t mps_message_clock(mps_arena_t arena, mps_message_t message)

    Returns the time at which the MPS posted a :term:`message`.

    ``arena`` is the :term:`arena` which posted the message.

    ``message`` is a message retrieved by :c:func:`mps_message_get` and
    not yet discarded.

    If ``message`` belongs to one of the following supported message,
    return the time at which the MPS posted the message:

    * :c:type:`mps_message_type_gc`;
    * :c:type:`mps_message_type_gc_start`.

    For other message types, the value returned is always zero.

    Messages are asynchronous: they are posted by the MPS, wait on a
    queue, and are later collected by the :term:`client program`. Each
    message (of the supported message types) records the time that it
    was posted, and this is what :c:func:`mps_message_clock` returns.

    The time returned is the :c:func:`mps_clock_t` value returned by
    the library function :c:func:`mps_clock` at the time the message
    was posted. You can subtract one clock value from another to get
    the time interval between the posting of two messages.


.. c:function:: void mps_message_discard(mps_arena_t arena, mps_message_t message)

    Indicate to the MPS that the :term:`client program` has no further
    use for a :term:`message` and the MPS can now reclaim any storage
    associated with the message.

    ``arena`` is the :term:`arena` which posted the message.

    ``message`` is the message. After this call, ``message`` is invalid
    and should not be passed as an argument to any message functions.

    Messages are essentially :term:`manually <manual memory
    management>` managed. This function allows the MPS to reclaim
    storage associated with messages. If the client does not discard
    messages then the resources used may grow without bound.

    As well as consuming resources, messages may have other effects
    that require them to be tidied by calling this function. In
    particular finalization messages refer to a :term:`finalized
    block`, and prevent the object from being reclaimed (subject to
    the usual :term:`garbage collection` liveness analysis). A
    finalized block cannot be reclaimed until all its finalization
    messages have been discarded. See
    :c:func:`mps_message_type_finalization`.

    .. seealso::

        :ref:`topic-finalization`.


.. c:function:: size_t mps_message_gc_condemned_size(mps_arena_t arena, mps_message_t message)

    Return the "condemned size" property of a :term:`message`.

    ``arena`` is the arena which posted the message.

    ``message`` is a message retrieved by :c:func:`mps_message_get` and
    not yet discarded.  It must be a garbage collection message: see
    :c:func:`mps_message_type_gc`.

    The "condemned size" property is the approximate :term:`size` of
    the :term:`condemned set` in the :term:`garbage collection` that
    generated the message.

    .. seealso::

        :ref:`topic-collection`.


.. c:function:: size_t mps_message_gc_live_size(mps_arena_t arena, mps_message_t message)

    Return the "live size" property of a :term:`message`.

    ``arena`` is the arena which posted the message.

    ``message`` is a message retrieved by :c:func:`mps_message_get` and
    not yet discarded.  It must be a garbage collection message: see
    :c:func:`mps_message_type_gc`.

    The "live size" property is the total size of the set of objects
    that survived the :term:`garbage collection` that generated the
    message.

    .. seealso::

        :ref:`topic-collection`.


.. c:function:: size_t mps_message_gc_not_condemned_size(mps_arena_t arena, mps_message_t message)

    Return the "not condemned size" property of a :term:`message`.

    ``arena`` is the arena which posted the message.

    ``message`` is a message retrieved by :c:func:`mps_message_get` and
    not yet discarded.  It must be a garbage collection message: see
    :c:func:`mps_message_type_gc`.

    The "not condemned size" property is the approximate size of the
    set of objects that were in collected :term:`pools <pool>`, but
    were not in the :term:`condemned set` in the :term:`garbage
    collection` that generated the message.

    .. seealso::

        :ref:`topic-collection`.


.. c:function:: const char *mps_message_gc_start_why(mps_arena_t arena, mps_message_t message)

    Return a string that describes why the :term:`garbage collection`
    that posted a :term:`message` started.

    ``arena`` is the arena which posted the message.

    ``message`` is a message retrieved by :c:func:`mps_message_get` and
    not yet discarded.  It must be a garbage collection message: see
    :c:func:`mps_message_type_gc`.

    Returns a pointer to a string that is describes (in English) why
    this collection started. The contents of the string must not be
    modified by the client. The string and the pointer are valid until
    the message is discarded with :c:func:`mps_message_discard`.

    .. seealso::

        :ref:`topic-collection`.


.. c:function:: mps_bool_t mps_message_get(mps_message_t *message_o, mps_arena_t arena, mps_message_type_t message_type)

    Get a :term:`message` of a specified type from the :term:`message
    queue` for an :term:`arena`.

    ``message_o`` points to a location that will hold the address of the
    message if the function succeeds.

    ``arena`` is the arena.

    ``message_type`` is the type of message to return.

    If there is at least one message of the specified type on the
    message queue of the specified arena, then this function removes
    one such message from the queue, stores a pointer to the message
    in the location pointed to by ``message_o``, and returns true.
    Otherwise it returns false.


.. c:function:: mps_bool_t mps_message_poll(mps_arena_t arena)

    Determine whether there are currently any :term:`messages
    <message>` on a :term:`message queue` for an :term:`arena`.

    ``arena`` is the arena whose message queue will be polled.

    Returns true if there is at least one message on the message queue
    for ``arena``, or false if the message queue is empty.

    .. note::

        If you are interested in a particular type of message, it is
        usually simpler to call :c:func:`mps_message_get`.


.. c:function:: mps_bool_t mps_message_queue_type(mps_message_type_t *message_type_o, mps_arena_t arena)

    Determine whether there are currently any :term:`messages
    <message>` on a :term:`message queue` for an :term:`arena`, and
    return the :term:`message type` of the first message, if any.

    ``message_type_o`` points to a location that will hold the message
    type of the first message on the queue, if any.

    ``arena`` is the arena whose message queue will be polled.

    If there is at least one message on the message queue of ``arena``,
    then this function returns true, and also writes the message type
    of the first message on the queue into the location pointed to by
    ``message_type_o``. If there are no messages on the message queue,
    it returns false.


.. c:type:: mps_message_t

    The type of a :term:`message`.

    Messages are :term:`manually <manual memory management>` managed.
    They are created at the instigation of the MPS (but see
    :c:func:`mps_message_type_enable`), and are deleted by the
    :term:`client program` by calling :c:func:`mps_message_discard`.

    An :term:`arena` has a :term:`message queue` from which messages
    can be obtained by calling :c:func:`mps_message_get`.

    An :c:func:`mps_message_t` is a :term:`reference` into MPS managed
    memory, and can safely be :term:`fixed <fix>`.


.. c:function:: mps_message_type_t mps_message_type(mps_arena_t arena, mps_message_t message)

    Return the :term:`message type` of a :term:`message`.

    ``arena`` is the arena that posted the message.

    ``message`` is a message retrieved by :c:func:`mps_message_get` and
    not yet discarded.


.. c:function:: void mps_message_type_disable(mps_arena_t arena, mps_message_type_t message_type)

    Restore an :term:`arena` to the default state whereby
    :term:`messages <message>` of the specified :term:`message type`
    are not posted, reversing the effect of an earlier call to
    :c:func:`mps_message_type_enable`.

    ``arena`` is an arena.

    ``message_type`` is the message type to be disabled.

    Any existing messages of the specified type are flushed from the
    :term:`message queue` of ``arena``.

    .. note::

        It is permitted to call this function when ``message_type`` is
        already disabled, in which case it has no effect.


.. c:function:: void mps_message_type_enable(mps_arena_t arena, mps_message_type_t message_type)

    Enable an :term:`arena` to post :term:`messages <message>` of a
    specified :term:`message type`.

    ``arena`` is an arena.

    ``message_type`` is the message type to be disabled.

    This function tells the MPS that ``arena`` may post messages of
    ``message_type`` to its :term:`message queue`. By default, the MPS
    does not generate any messages of any type.

    A :term:`client program` that enables messages for a message type
    must access messages by calling :c:func:`mps_message_get` and
    discard them by calling :c:func:`mps_message_discard`, or the
    message queue may consume unbounded resources.

    The client program may disable the posting of messages by calling
    :c:func:`mps_message_type_disable`.

    .. note::

        It is permitted to call this function when ``message_type`` is
        already enabled, in which case it has no effect.


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

        :ref:`topic-finalization`.


.. c:function:: mps_message_type_t mps_message_type_gc(void)

    Return the :term:`message type` of garbage collection statistic
    messages.

    Garbage collection statistic messages are used by the MPS to give
    the :term:`client program` information about a :term:`garbage
    collection` that has taken place. Such information may be useful in
    analysing the client program's memory usage over time.

    The access methods specific to a message of this type are:

    * :c:func:`mps_message_gc_live_size` returns the total size of the
      :term:`condemned set` that survived the garbage collection that
      generated the message;

    * :c:func:`mps_message_gc_condemned_size` returns the approximate
      size of :term:`condemned set` in the garbage collection that
      generated the message;

    * :c:func:`mps_message_gc_not_condemned_size` returns the
      approximate size of the set of objects that were in collected
      :term:`pools <pool>`, but were not condemned in the garbage
      collection that generated the message.

    .. seealso::

        :ref:`topic-collection`.


.. c:function:: mps_message_type_t mps_message_type_gc_start(void)

    Return the :term:`message type` of garbage collection start
    messages.

    Garbage collection start messages contain information about why
    the :term:`garbage collection` started.

    The access method specific to a :term:`message` of this message
    type is:

    * :c:func:`mps_message_gc_start_why` returns a string that
      describes why the garbage collection started.

    .. seealso::

        :ref:`topic-collection`.


.. c:type:: mps_message_type_t

    The type of :term:`message types <message type>`.


