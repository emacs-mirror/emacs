.. _topic-plinth:

==========
The plinth
==========



The example ANSI plinth, mpsliban.c, implements :c:func:`mps_clock` by calling ISO C time.h's clock().  The difference between two of these clock values may be converted to seconds by dividing by ISO C time.h's CLOCKS_PER_SEC conversion factor.
