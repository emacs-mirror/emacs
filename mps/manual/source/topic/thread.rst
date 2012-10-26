.. sources:

    `<https://info.ravenbrook.com/project/mps/master/design/thread-safety/>`_

.. _topic-thread:

Threads
=======

Thread safety
-------------

The MPS is designed to run in an environment with multiple threads all
calling into the MPS. Some code is known to operate with exclusive
access to the data it manipulates (for example, allocation via
:term:`allocation points <allocation point>`, in the common case where
the buffer does not need to be refilled, and :term:`location
dependencies <location dependency>`), so this code is safe. For the
rest of the code, shared data structures are locked by the use of a
single lock per :term:`arena`. This lock is claimed on entry to the
MPS and released on exit from it. So there is at most a single thread
(per arena) running "inside" the MPS at a time.


Thread registration
-------------------

In order to scan a thread's registers for references (which happens at
each :term:`flip`), the MPS needs to be able to suspend that thread.
This means that threads must be registered with the MPS by calling
:c:func:`mps_thread_reg` (and thread roots created; see
:ref:`topic-root-thread`).
 
A thread must be registered with an :term:`arena` if it ever uses a
pointer to a location in an :term:`automatically managed <automatic
memory management>` :term:`pool` belonging to that arena. But for
simplicity we recommend that all threads be registered with all
arenas.


Signal handling issues
----------------------

The MPS uses :term:`barriers <barrier (1)>` to :term:`protect
<protection>` memory from the :term:`client program` and handles the
signals that result from barrier hits.

On some operating systems, barrier hits generate exceptions that have
to be caught by a handler that is on the stack. On these operating
systems, any code that uses memory managed by the MPS must be called
from inside such an exception handler, that is, inside a call to
:c:func:`mps_tramp`.

.. note::

    In fact, it's only Windows that requires the structured exception
    handler to be on the stack. On other operating systems you can get
    away without calling the trampoline. Nonetheless, for portability
    and forwards compatibility we recommend that each thread that runs
    code that accesses memory managed by the MPS should run it inside
    a call to :c:func:`mps_tramp`.

    On Windows, the requirement to call the trampoline extends to your
    structured exception handlers as well as your threads: any
    structured exception handler that accesses memory managed by the
    MPS must perform the access inside a call to :c:func:`mps_tramp`.

The use of barriers has the additional consequence that a program that
handles ``SIGBUS`` (on OS X), ``SIGSEGV`` (on FreeBSD or Linux), or
first-chance exceptions (on Windows) needs to co-operate with the MPS.
The mechanism for co-operation is currently undocumented: please
:ref:`contact us <contact>`.


Thread interface
----------------

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
        arenas.


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




