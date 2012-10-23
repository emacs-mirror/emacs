.. _topic-thread:

Threads
=======

Signal handling issues
----------------------

The MPS uses :term:`barriers <barrier (1)>` to :term:`protect
<protection>` memory from the :term:`client program` and handles the
signals that result from barrier hits.

This means that a program that handles ``SIGBUS`` (on OS X),
``SIGSEGV`` (on FreeBSD or Linux), or first-chance exceptions (on
Windows) needs to co-operate with the MPS. The mechanism for
co-operation is currently undocumented: please :ref:`contact us
<contact>`.


Interface
---------

.. c:function:: mps_reg_scan_t mps_stack_scan_ambig

    A root scanning function for :term:`ambiguous <ambiguous
    reference>` scanning of :term:`threads <thread>`, suitable for
    passing to :c:func:`mps_root_create_reg`.

    It scans all integer registers and everything on the stack of the
    thread given, and can therefore only be used with :term:`ambiguous
    roots <ambiguous root>`. It only scans locations that are at, or
    higher on the stack (that is, more recently added), the stack
    bottom that was passed to :c:func:`mps_thread_reg`. References
    are assumed to be represented as machine words, and are required
    to be 4-byte-aligned; unaligned values are ignored.

    .. seealso::

        :ref:`topic-platform`, :ref:`topic-root`.

    .. note::

        The MPS provides this function because it's hard to write: it
        depends on the operating system, the architecture, and in some
        cases the compiler.


.. c:function:: mps_res_t mps_thread_reg(mps_thr_t *thr_o, mps_arena_t arena)

    Register the current :term:`thread` with an :term:`arena`.

    ``thr_o`` points to a location that will hold the address of the
    registered thread description, if successful.

    ``arena`` is the arena.

    Returns :c:macro:`MPS_RES_OK` if successful, or another
    :term:`result code` if not.

    A thread must be registered with an arena if it ever uses a
    pointer to a location in an :term:`automatically managed
    <automatic memory management>` :term:`pool` belonging to that
    arena.

    .. note::

        It is recommended that all threads be registered with all
        arena.


.. c:function:: void mps_thread_dereg(mps_thr_t thr)

    Deregister a :term:`thread <thread>`.

    ``thr`` is the description of the thread.

    After calling this function, the thread whose registration with an
    :term:`arena` was recorded in ``thr`` must not use a pointer to a
    location in an :term:`automatically managed <automatic memory
    management>` :term:`pool` belonging to that arena.

    .. note::

        It is recommended that threads be deregistered only when they
        are just about to exit.


.. c:type:: mps_thr_t

    The type of registered :term:`thread` descriptions.

    In a multi-threaded environment where :term:`incremental garbage
    collection` is used, threads must be registered with the MPS by
    calling :c:func:`mps_thread_reg` so that the MPS can suspend them
    as necessary in order to have exclusive access to their state.

    Even in a single-threaded environment it may be necessary to
    register a thread with the MPS so that its stack can be registered
    as a :term:`root` by calling :c:func:`mps_root_create_reg`.


.. c:function:: void mps_tramp(void **r_o, mps_tramp_t f, void *p, size_t s)

    Call a function via the MPS trampoline.

    ``r_o`` points to a location that will store the result of calling
    ``f``.

    ``f`` is the function to call.

    ``p`` and ``s`` are arguments that will be passed to ``f`` each
    time it is called. This is intended to make it easy to pass, for
    example, an array and its size as parameters.

    The MPS relies on :term:`barriers <barrier (1)>` to protect memory
    that is in an inconsistent state. On some operating systems,
    barrier hits generate exceptions that have to be caught by a
    handler that is on the stack. On these operating systems, any code
    that uses memory managed by the MPS must be called from inside
    such an exception handler, that is, inside a call to
    :c:func:`mps_tramp`.

    If you have multiple threads that run code that uses memory
    managed by the MPS, each thread must execute such code inside a
    call to :c:func:`mps_tramp`.


.. c:type:: void *(*mps_tramp_t)(void *p, size_t s)

    The type of a function called by :c:func:`mps_tramp`.

    ``p`` and ``s`` are the corresponding arguments that were passed
    to :c:func:`mps_tramp`.




