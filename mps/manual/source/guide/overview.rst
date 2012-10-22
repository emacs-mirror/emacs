.. _guide-overview:

Overview of the Memory Pool System
==================================

The figure below gives a simplified picture of a program's memory from
the point of view of the Memory Pool System.

    .. figure:: ../diagrams/overview.svg
        :align: center
        :alt: Diagram: Overview of the Memory Pool System.

        Overview of the Memory Pool System.

The **arena** is the top-level data structure in the MPS. An
:term:`arena` is responsible for requesting :term:`memory (3)` from
the operating system (and returning it), making memory available to
:term:`pools <pool>`, and for :term:`garbage collection`. It's best to
have only one arena in your program, because the MPS can't collect
cyclic structures that span multiple arenas, but multiple arenas are
supported. See :ref:`topic-arena`.

The MPS is designed to co-operate with other memory managers (for
example :term:`malloc` and :term:`free (2)` in :term:`C`, or operators
``new`` and ``delete`` in :term:`C++`), so you need not move all your
memory management to the MPS at once, and you can co-operate with
libraries that use other allocation mechanisms.

Within the arena you create one or more **pools**. A :term:`pool` is
responsible for requesting memory from the :term:`arena` and making it
available to your program. See :ref:`topic-pool`.

Pools belong to **pool classes** that specify policies for how their
memory is managed. Some pools are :term:`manually managed <manual
memory management>` (you must call :c:func:`mps_free` to return a
block of memory to the pool) and others are :term:`automatically
managed <automatic memory management>` (the :term:`garbage collector`
reclaims :term:`unreachable` blocks). See :ref:`pool`.

Automatically managed pools need you to tell them how to **scan** for
:term:`references <reference>` to allocated blocks. See
:ref:`topic-scanning`.

The arena needs you to tell it how to find your **roots**: references
to allocated blocks that are stored in static data, in memory not
managed by the MPS, or on your program's :term:`registers <register>` or :term:`control stack`. See :ref:`topic-root`.

The MPS is designed to work with multi-threaded programs. Functions in
the C interface are thread safe, except in a few documented
cases. See :ref:`topic-thread`. The :term:`allocation point
protocol` provides fast lock-free allocation on multiple threads
simultaneously. See :ref:`topic-allocation`.

The garbage collector is :term:`incremental <incremental garbage
collection>`: it proceeds in small steps interleaved with the execution
of your program, so there are no long waits. See
:ref:`topic-collection`.
