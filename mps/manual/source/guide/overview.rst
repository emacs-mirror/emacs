.. _guide-overview:

Overview of the Memory Pool System
==================================

The figure below gives a (simplified) picture of a program's memory
from the point of view of the Memory Pool System.

    .. figure:: ../diagrams/overview.svg
        :align: center
        :alt: Diagram: Overview of the Memory Pool System.

        Overview of the Memory Pool System.

The :term:`arena` is the top-level data structure in the MPS. An arena
is responsible for requesting :term:`memory (3)` from the operating
system (and returning it), making memory available to :term:`pools
<pool>`, and for :term:`garbage collection`. It's best to have only
one arena in your program, because the MPS can't collect cyclic
structures that span multiple arenas, but multiple arenas are
supported. (See :ref:`topic-arena`.)

Within the arena you create one or more :term:`pools <pool>`. A pool
is responsible for requesting memory from the :term:`arena` and making
it available to your program. (See :ref:`topic-pool`.)

Pools belong to :term:`pool classes <pool class>` that specify
policies for how their memory is managed. Some pools are
:term:`manually managed <manual memory management>` (you must call
:c:func:`mps_free` to return a block of memory to the pool) and others
are :term:`automatically managed <automatic memory management>` (the
:term:`garbage collector` reclaims :term:`unreachable`
blocks). Automatically managed pools need you to tell them how to find
:term:`references <reference>` to allocated blocks. (See
:ref:`topic-format`.)

The MPS is designed to work with multi-threaded programs. Functions in
the C interface are thread safe, except in a few documented
cases. (See :ref:`topic-thread`.) The :term:`allocation point
protocol` provides fast lock-free allocation on multiple threads
simultaneously. (See :ref:`topic-allocation`.)

The MPS is designed to co-operate with other memory managers (for
example :term:`malloc` and :term:`free (2)` in :term:`C`, or operators
``new`` and ``delete`` in :term:`C++`), so you need not move all your
memory management to the MPS at once, and you can co-operate with
libraries that use standard allocation mechanisms.

The garbage collector is :term:`incremental <incremental garbage
collection>`: it proceeds in small steps interleaved with the execution
of your program, so there are no long waits. (See
:ref:`topic-collection`.)
