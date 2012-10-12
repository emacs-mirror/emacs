.. _topic-plinth:

==========
The plinth
==========

The example ANSI plinth, ``mpsliban.c``, implements :c:func:`mps_clock` by calling the ISO C function ``clock`` in ``time.h``.  The difference between two of these clock values may be converted to seconds by dividing by the ``CLOCKS_PER_SEC`` conversion factor.
