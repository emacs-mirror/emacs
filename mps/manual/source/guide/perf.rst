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

In the tables below I give the execution time of ``test-leaf.scm`` in
the toy Scheme interpreter under different settings for its generation
chain. (This test case allocates hundreds of millions of small
short-lived objects.)

First, the effect of varying the capacity of a chain with a single
generation.

========  =========  =========================
Capacity  Mortality  Execution time (user+sys)
========  =========  =========================
100            0.80                      362.6
200            0.80                      354.9
400            0.80                      349.7
800            0.80                      314.4
1600           0.80                      215.7
3200           0.80                       94.0
6400           0.80                       53.5
12800          0.80                       79.6
25600          0.80                       77.6
========  =========  =========================

Second, the effect of varying the mortality of a chain with a single
generation.

========  =========  =========================
Capacity  Mortality  Execution time (user+sys)
========  =========  =========================
6400           0.20                       55.4
6400           0.40                       54.0
6400           0.60                       54.0
6400           0.80                       53.5
6400           0.99                       54.8
========  =========  =========================

Third, the effect of varying the number of generations (all
generations being identical).

===========  ========  =========  =========================
Generations  Capacity  Mortality  Execution time (user+sys)
===========  ========  =========  =========================
1                6400       0.80                       53.5
2                6400       0.80                       42.4
3                6400       0.80                       42.1
4                6400       0.80                       42.2
5                6400       0.80                       42.2
===========  ========  =========  =========================

These tables suggest that:

1. The improvement in performance to be gained by getting generation
   sizes right is dramatic: much bigger than the small improvements to
   gained from other techniques.

2. The predicted mortality doesn't make much difference to the overall
   execution time (it does affect the distribution of pause times,
   however: see :ref:`topic-collection-schedule`.)

3. You can make generations too big as well as too small.

4. There are rapidly diminishing returns to be gained from adding
   generations.

.. note::

    :ref:`topic-telemetry` can be used to discover when generations
    are being collected and what proportion of blocks were found to be
    alive.

The table below shows the effect of varying the initial allocation of
address space to the arena (using three generations each with capacity
6400 kB, mortality 0.80).

=============  ==========  ===========  =========================
Address space  Extensions  Collections  Execution time (user+sys)
=============  ==========  ===========  =========================
2                      32          371                       52.0
4                      21          370                       47.0
8                       0         [1]_                       [1]_
14                      0         [1]_                       [1]_
16                      0         2436                      160.5
18                      0         1135                       89.1
20                      0          673                       60.6
22                      0          484                       48.7
24                      0          400                       43.1
32                      0          368                       41.2
64                      0          368                       43.1
128                     0          368                       46.4
256                     0          368                       46.3
512                     0          368                       49.3
1024                    0          368                       42.0
2048                    0          368                       43.2
4096                    0          368                       43.5
8192                    0          368                       46.1
16384                   0          368                       49.2
32768                   0          368                       57.1
65536                   0          368                       71.1
131072                  0          368                      101.3
262144                  0          368                      161.3
524288                  0          368                      273.0
1048576                 0          368                      504.6
=============  ==========  ===========  =========================

.. note::

    .. [1] With this initial allocation of address space, the test
           case failed to run to completion after thousands of seconds
           and tens of thousands of garbage collection cycles.

The lesson here is that the allocation of address space has to be
comfortably larger than the working set of the program, but that a
very large address space is ruinous to performance.
