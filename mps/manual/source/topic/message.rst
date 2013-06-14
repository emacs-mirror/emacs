.. Sources:

    `<https://info.ravenbrook.com/project/mps/master/design/message/>`_
    `<https://info.ravenbrook.com/mail/2005/04/05/13-33-11/0.txt>`_

.. index::
   single: message; introduction

.. _topic-message:

Messages
========

The MPS sometimes needs to communicate with the :term:`client program`
about events which occur :term:`asynchronously <asynchronous garbage
collector>`, and so information cannot be returned as function call
results.

*Messages* are the mechanism for this asynchronous communication,
implemented in the form of a :term:`message queue` attached to each
:term:`arena`.

The client program must enable each message type that they are
prepared to handle, by calling :c:func:`mps_message_type_enable`. Then
it must poll the message queue at regular intervals when it is
convenient to do so, calling :c:func:`mps_message_get` to retrieve
each message from the queue, and then calling
:c:func:`mps_message_discard` when it is done with it.

Messages are thus :term:`manually managed <manual memory management>`:
if the client program enables one or more message types, and then
neglects to poll the message queue or neglects to discard the messages
it retrieved, then messages will :term:`leak <memory leak>`.

There is no requirement on the client program to retrieve and discard
messages promptly. However, a client program that allows the number of
garbage collection (or garbage collection start) messages on the
message queue to grow without limit will eventually find that new
garbage collections no longer start until some of these messages are
retrieved and discarded.


.. index::
   pair: message; finalization

Finalization messages
---------------------

:term:`Finalization` is implemented by posting a finalization message
(of type :c:func:`mps_message_type_finalization`) to the arena's
message queue. This allows the :term:`client program` to perform the
finalization at a convenient time and so avoid synchronization
difficulties.

The block is not actually reclaimed until the finalization message is
removed from the message queue and discarded, by calling
:c:func:`mps_message_get` followed by :c:func:`mps_message_discard`.

See :ref:`topic-finalization`.


.. index::
   single: message; example
   single: Scheme; interactive chatter

Example: interactive chatter
----------------------------

The toy Scheme interpreter enables garbage collection messages when
started in interactive mode::

    mps_message_type_enable(arena, mps_message_type_gc());
    mps_message_type_enable(arena, mps_message_type_gc_start());

Then, after every interactive command finishes, it reads these
messages from the message queue and prints a description of the
contents of each one::

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

    MPS Toy Scheme Example
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

    This kind of interactive "chatter" may be useful when testing and
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


.. index::
   single: message; types

Message types
-------------

.. c:type:: mps_message_type_t

    The type of :term:`message types`.

    There are three message types:

    1. :c:func:`mps_message_type_finalization`
    2. :c:func:`mps_message_type_gc`
    3. :c:func:`mps_message_type_gc_start`


.. c:function:: void mps_message_type_disable(mps_arena_t arena, mps_message_type_t message_type)

    Restore an :term:`arena` to the default state whereby
    :term:`messages` of the specified :term:`message type`
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

    Enable an :term:`arena` to post :term:`messages` of a
    specified :term:`message type`.

    ``arena`` is an arena.

    ``message_type`` is the message type to be enabled.

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


.. index::
   single: message; interface

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

    An :c:type:`mps_message_t` is a :term:`reference` into MPS managed
    memory, and can safely be :term:`fixed`.


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
    the :term:`plinth` function :c:func:`mps_clock` at the time the
    message was posted. You can subtract one clock value from another
    to get the time interval between the posting of two messages.


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


.. index::
   single: message; queue interface

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

    Determine whether there are currently any :term:`messages` on a :term:`message queue` for an :term:`arena`.

    ``arena`` is the arena whose message queue will be polled.

    Returns true if there is at least one message on the message queue
    for ``arena``, or false if the message queue is empty.

    .. note::

        If you are interested in a particular type of message, it is
        usually simpler to call :c:func:`mps_message_get`.


.. c:function:: mps_bool_t mps_message_queue_type(mps_message_type_t *message_type_o, mps_arena_t arena)

    Determine whether there are currently any :term:`messages` on a :term:`message queue` for an :term:`arena`, and
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
