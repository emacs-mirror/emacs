.. _guide-lang:

=========================================
Garbage collecting a programming language
=========================================

Have you implemented a programming language, written the lexer,
parser, code generator and the runtime system, and just come to the
realization that you are going to need a memory manager too? If so,
you've come to the right place.

This guide explains how to add generational garbage collection to the
runtime system for a programming language. I'll be using, as a running
example, a small interpreter for a subset of :term:`Scheme`. I'll be
quoting the relevant sections of code as needed, but you may find it
helpful to download and experiment with this interpeter yourself:

    :download:`scheme-before.c`

        The Scheme interpreter before integration with the MPS, using
        :term:`malloc` for memory management.

    :download:`scheme-after.c`

        The Scheme interpreter after integration with the MPS.
