.. _topic-plinth:

==========
The plinth
==========

From //info.ravenbrook.com/project/mps/doc/2002-06-18/obsolete-mminfo/mmdoc/doc/mps/ref-man/concepts/index.html

A program module providing the MPS with all the support it needs from the execution environment. Mainly this includes simple I/O facilities to support debugging.

The plinth is provided by the client application; however, a sample implementation of the plinth using standard ANSI C library facilities is included with the MPS, and this is good enough for most applications.


From //info.ravenbrook.com/project/mps/doc/2002-06-18/obsolete-mminfo/mmdoc/doc/mps/guide/interface/index.html

To perform its various duties, the MPS needs very little external support. Indeed, there is a way of using it so that it needs none at all, making it possible to use the MPS in embedded applications. There are two key components to this: the client arena and the plinth . This section concerns the plinth; for more information about the client arena, see mps_arena_class_cl in the Reference Manual.

The plinth also has another purpose: it gives the application programmer complete control of how interaction between the MPS and the user happens. This comprises things like debugging messages and logging. The two purposes are intertwined, because both relate to operating system support.

The plinth is a program module providing the MPS with all the support functions it needs from the execution environment. The plinth is provided by the application programmer; this is how the plinth removes the need for external libraries, by getting the support from the client application, and at the same time, gives the application programmer control over the implementation of its features.

However, before you panic, a sample implementation of a plinth using standard ISO C library facilities is provided with the MPS (mpsliban.c and mpsioan.c), and this is often adequate for your needs, so you don't have to write your own. Naturally, if you use the ISO plinth, you then need to link with the C library.

There are many reasons why you might want to write your own plinth. For embedded applications, the MPS will work in what the C standard calls a freestanding environment, as long as you provide it with a plinth that works in that environment, and use the client arena (virtual memory arenas contain OS-specific code that calls the VM interfaces of the OS). Programmers of GUI applications might want a plinth that directs debugging output to a convenient window.

See //info.ravenbrook.com/project/mps/doc/2002-06-18/obsolete-mminfo/mmdoc/doc/mps/guide/appendix/plinth/index.html

The example ANSI plinth, ``mpsliban.c``, implements :c:func:`mps_clock` by calling the ISO C function ``clock`` in ``time.h``.  The difference between two of these clock values may be converted to seconds by dividing by the ``CLOCKS_PER_SEC`` conversion factor.


See also //info.ravenbrook.com/project/mps/master/design/io/index.html
