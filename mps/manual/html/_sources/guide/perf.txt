.. index::
   single: Memory Pool System; performance
   single: performance
   single: generation; choosing size

.. _guide-perf:

Tuning the Memory Pool System for performance
=============================================

.. note::

    When developing a benchmark to profile your program against, bear
    in mind that the benchmark should allocate several times the
    amount of physical memory that you expect to be available to the
    process. If the total allocation fits into the available memory,
    there's no point running a garbage collector at all: you might as
    well just allocate and never collect.

The most important aspect of tuning the MPS is to choose good sizes
for the :term:`generations` in your :term:`generation chain`. The
ideal size of a generation should be such that when it is collected,
most of the blocks allocated in that generation should be found to be
:term:`dead` (and so the cost of :term:`scanning <scan>` and
:term:`copying <copying garbage collection>` them can be avoided
entirely). If a generation is collected when its blocks are mostly
alive, that is a waste of time.

In the table below I give the execution time of ``test-leaf.scm`` in
the toy Scheme interpreter under different settings for its generation
chain. (This test case allocates millions of small short-lived
objects.) In each case the AMC pool is given a chain with a single
generation with the specified capacity and mortality.

========  =========  =========================
Capacity  Mortality  Execution time (user+sys)
========  =========  =========================
100            0.80                       39.9
200            0.80                       30.2
400            0.80                       25.5
800            0.80                       16.3
1600           0.80                        9.0
3200           0.80                        5.8
6400           0.20                        4.2
6400           0.40                        4.1
6400           0.60                        4.1
6400           0.80                        4.1
6400           0.99                        4.2
12800          0.80                        4.2
25600          0.80                        5.2
========  =========  =========================

This table suggests that:

1. The improvement in performance to be gained by getting generation
   sizes right is dramatic: much bigger than the small improvements to
   gained from other techniques.

2. The predicted mortality doesn't make much difference to the overall
   execution time (it does affect the distribution of pause times,
   however: see :ref:`topic-collection-schedule`.)

3. You can make generations too big as well as too small.

.. note::

    :ref:`topic-telemetry` can be used to discover when generations
    are being collected and what proportion of blocks were found to be
    alive.
