.. _topic-message:

Messages
========

*Messages* are the mechanism by which the MPS communicates with the
:term:`client program`. The MPS normally runs :term:`asynchronously
<asynchronous garbage collector>` with respect to the client program,
so messages are implemented via a :term:`message queue` attached to
each :term:`arena`.

By default only finalization messages are posted to the queue (and
then only if blocks have been registered for finalization; see
below). The client program must enable each other message type that
they are prepared to handle, by calling
:c:func:`mps_message_type_enable`. Then it must poll the message queue
at regular intervals when it is convenient to do so, calling
:c:func:`mps_message_get` to retrieve each message from the queue, and
then calling :c:func:`mps_message_discard` when it is done with it.

Messages are thus :term:`manually managed <manual memory management>`:
if the client program enables one or more message types, and then
neglects to poll the message queue or neglects to discard the messages
it retrieved, then messages will :term:`leak`.


Finalization messages
---------------------

:term:`Finalization` is implemented by posting a finalization message
(of type :c:func:`mps_message_type_finalization`) to the arena's
message queue. This allows the :term:`client program` to perform the
finalization at a convenient time and so avoid synchronization
difficulties.

Unlike other message types, finalization messages do not need to be
enabled. Instead, the block to be finalized is registered for finalization by calling :c:func:`mps_finalize`.

The block is not actually reclaimed until the finalization message is
removed from the message queue and discarded, by calling
:c:func:`mps_message_get` followed by :c:func:`mps_messsage_discard`.

See :ref:`topic-finalization`.


Example: interactive chatter
----------------------------

The example Scheme interpreter turns on garbage collection messages at
startup::

    mps_message_type_enable(arena, mps_message_type_gc());
    mps_message_type_enable(arena, mps_message_type_gc_start());

And then after every interactive command finishes, it reads all
messages from the message queue and prints a description of the
content of each one::

    static void mps_chat(void)
    {
        mps_message_type_t type;

        while (mps_message_queue_type(&type, arena)) {
            mps_message_t message;
            mps_bool_t b;
            b = mps_message_get(&message, arena, type);
            assert(b); /* we just checked there was one */

            if (type == mps_message_type_gc_start()) {
                printf("Collection started.\n");
                printf("  Why: %s\n", mps_message_gc_start_why(arena, message));
                printf("  Clock: %lu\n", (unsigned long)mps_message_clock(arena, message));
            if (type == mps_message_type_gc()) {
                /* ... and so on for other message types ... */
            } else {
                printf("Unknown message from MPS!\n");
            }

            mps_message_discard(arena, message);
        }
    }

Here's how this looks in operation:

.. code-block:: none

    bash-3.2$ ./scheme
    MPS Toy Scheme Example
    The prompt shows total allocated bytes and number of collections.
    Try (vector-length (make-vector 100000 1)) to see the MPS in action.
    You can force a complete garbage collection with (gc).
    If you recurse too much the interpreter may crash from using too much C stack.
    9960, 0> (define (make-list n e) (if (eqv? n 0) '() (cons e (make-list (- n 1) e))))
    make-list
    10824, 0> (length (make-list 1000 #t))
    1000
    Collection started.
      Why: Generation 0 of a chain has reached capacity: start a minor collection.
      Clock: 6649
    507408, 1> (length (make-list 200 #f))
    200
    Collection finished.
        live 112360
        condemned 196600
        not_condemned 0
        clock: 18431
    607192, 1> Bye.

.. note::

    This kind of interative "chatter" may be useful when testing and
    debugging memory management, but should not be used otherwise. The
    scheduling of garbage collections is not normally of interest even
    to programmers, and chatter of this sort may give the illusion
    that a program is spending much more time garbage collecting than
    is actually the case.

    Versions of GNU Emacs prior to 19.31 (May 1996) used to display
    the message "Garbage collecting..." during a collection. Erik
    Naggum commented on this feature:

        I have run some tests at the U of Oslo with about 100
        users who generally agreed that Emacs had become faster in
        the latest Emacs pretest. All I had done was to remove the
        "Garbage collecting" message which people perceive as
        slowing Emacs down and tell them that it had been sped up.
        It is, somehow, permissible for a program to take a lot of
        time doing any other task than administrative duties like
        garbage collection.


Example: collection statistics
------------------------------

Here's a function that could be added to the example Scheme interpeter
to run a complete garbage collection and return some statistics about
that collection as a list to the caller::

    /* (gc) -- run full garbage collection now
     * Returns a list of four objects:
     * 1. The collection number
     * 2. The time taken by the collection (in units of mps_clock_t)
     * 3. The approximate size of the condemned set.
     * 4. The size of the subset of the condemned set that survived.
     */
    static obj_t entry_gc(obj_t env, obj_t op_env, obj_t operator, obj_t operands)
    {
        mps_message_t message;
        mps_clock_t start, finish;
        size_t condemned_size, live_size;
        mps_bool_t b;
        mps_arena_park(arena);
        mps_message_type_enable(arena, mps_message_type_gc());
        mps_message_type_enable(arena, mps_message_type_gc_start());
        mps_arena_collect(arena);
        b = mps_message_get(&message, arena, mps_message_type_gc_start());
        assert(b); /* there must be one since we just ran a collection */
        start = mps_message_clock(arena, message);
        mps_message_discard(arena, message);
        b = mps_message_get(&message, arena, mps_message_type_gc());
        assert(b); /* there must be one since we just ran a collection */
        finish = mps_message_clock(arena, message);
        condemned_size = mps_message_gc_condemned_size(arena, message);
        live_size = mps_message_gc_live_size(arena, message);
        mps_message_discard(arena, message);
        mps_message_type_disable(arena, mps_message_type_gc());
        mps_message_type_disable(arena, mps_message_type_gc_start());
        mps_arena_release(arena);
        return make_pair(make_integer(mps_collections(arena)),
                         make_pair(make_integer(finish - start),
                                   make_pair(make_integer(condemned_size),
                                             make_pair(make_integer(live_size),
                                                       obj_empty))));
    }

.. note::

    1. This code assumes that :c:func:`mps_message_type_gc` and
       :c:func:`mps_message_type_gc_start` are not enabled elsewhere in
       the code (in particular, if you're going to try this out, you'll
       need to remove the two calls to :c:func:`mps_message_type_enable`
       from ``main``).

    2. The arena is :term:`parked <parked state>` by calling
       :c:func:`mps_arena_park` before enabling these message
       types. That's because we need to ensure that there are no
       collections running, so that no spurious messages of these types
       (from other collections) get posted.

    3. Similarly, the message types are disabled before calling
       :c:func:`mps_arena_release`.

Here's the above code in action:

.. code-block:: none

    $ ./scheme
    MPS Toy Scheme Example
    The prompt shows total allocated bytes and number of collections.
    Try (vector-length (make-vector 100000 1)) to see the MPS in action.
    You can force a complete garbage collection with (gc).
    If you recurse too much the interpreter may crash from using too much C stack.
    9960, 0> (gc)
    (1 357 9984 9984)
    10144, 1> (define (range n) (if (eqv? n 0) '() (append (range (- n 1)) (list n))))
    range
    10976, 1> (range 10)
    (1 2 3 4 5 6 7 8 9 10)
    16984, 1> (gc)
    (2 465 29320 11016)
    17168, 2> (length (range 100))
    100
    181280, 2> (gc)
    (3 668 190064 13416)


Message types
-------------

.. c:type:: mps_message_type_t

    The type of :term:`message types <message type>`.

    There are three message types:

    1. :c:func:`mps_message_type_finalization`
    2. :c:func:`mps_message_type_gc`
    3. :c:func:`mps_message_type_gc_start`


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



Message interface
-----------------

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


.. c:function:: mps_message_type_t mps_message_type(mps_arena_t arena, mps_message_t message)

    Return the :term:`message type` of a :term:`message`.

    ``arena`` is the arena that posted the message.

    ``message`` is a message retrieved by :c:func:`mps_message_get` and
    not yet discarded.


Message queue interface
-----------------------

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

    .. note::

        If you are interested in a particular type of message, it is
        usually simpler to call :c:func:`mps_message_get`.
