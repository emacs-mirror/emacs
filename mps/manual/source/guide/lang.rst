.. highlight:: c

.. _guide-lang:

==========================================
Garbage collecting a language with the MPS
==========================================

Have you implemented a programming language, written the lexer,
parser, code generator and the runtime system, and come to the
realization that you are going to need a memory manager too? If so,
you've come to the right place.

In this guide, I'll explains how to use the MPS to add generational
garbage collection to the runtime system for a programming language.

I'm assuming that you've downloaded and compiled the MPS (see
:ref:`guide-download`), and that you are familiar with the overall
architecture of the MPS (:ref:`guide-overview`).


------------------
The Scheme example
------------------

As a running example throughout this guide, I'll be using a small
interpreter for a subset of the :term:`Scheme` programming
language. I'll be quoting the relevant sections of code as needed, but
you may find it helpful to download and experiment with this
interpeter yourself:

    :download:`scheme-before.c`

        The Scheme interpreter before integration with the MPS, using
        :term:`malloc` and :term:`free (2)` for memory management.

    :download:`scheme-after.c`

        The Scheme interpreter after integration with the MPS.

This simple interpreter allocates two kinds of objects on the
:term:`heap`:

1. All Scheme objects (there are no :term:`unboxed` objects).

2. The global symbol table, a hash table consisting of a vector of
   pointers to strings.

A Scheme object whose type is not necessarily known is represented by
an ``obj_t``, which is a pointer to a union of every type in the
language::

    typedef union obj_u *obj_t;
    typedef union obj_u {
        type_s type;
        pair_s pair;
        symbol_s symbol;
        integer_s integer;
        special_s special;
        operator_s operator;
        string_s string;
        port_s port;
        character_s character;
        vector_s vector;
    } obj_s;

Every object type is a structure whose first word is the type of the
object. For example, pairs are represented by a pointer to the
structure ``pair_s`` defined as follows::

    typedef struct pair_s {
        type_t type;        /* TYPE_PAIR */
        obj_t car, cdr;     /* first and second projections */
    } pair_s;

The ``type`` member of the ``pair_s`` structure always takes the
constant value ``TYPE_PAIR``.

Because the first word of every object is its type, functions can
operate on objects generically, testing ``TYPE(obj)`` as necessary. For example, the ``print`` function is implemented like this::

    static void print(obj_t obj, unsigned depth, FILE *stream)
    {
        switch (TYPE(obj)) {
            case TYPE_INTEGER: {
                fprintf(stream, "%ld", obj->integer.integer);
            } break;

            case TYPE_SYMBOL: {
                fputs(obj->symbol.string, stream);
            } break;

            /* ... and so on for the other types ... */
        }
    }

Each constructor (for example ``make_pair`` is the constructor for
pairs) allocates memory for the new object by calling ``malloc``,
but objects do not get freed, because they have :term:`indefinite
extent`, so it is necessary to prove that they are :term:`dead` before
their memory can be :term:`reclaimed <reclaim>`. And that task falls
to the :term:`garbage collector`.


-------------------------------
Choosing arena and pool classes
-------------------------------

You'll recall from the :ref:`overview <guide-overview>` that the
functionality of the MPS is divided between the :term:`arena`, which
requests memory from (and returns it to) the operating system, and
:term:`pools <pool>`, which allocate blocks of memory on behalf of the
:term:`client program`.

There are two main classes of arena: the :term:`client arena` (see
:c:func:`mps_arena_class_cl`) which gets its memory from the client
program, and the :term:`virtual memory arena` (see
:c:func:`mps_arena_class_vm`) which gets its memory from the operating
system's :term:`virtual memory` interface.

The client arena is intended for use on embedded systems where there
is no virtual memory, and has a couple of disadvantages (you have to
decide up front how much memory you are going to need; and the MPS
can't return memory to the operating system for use by other
processes) so in most situations you'll want to use the virtual memory
arena.

Pool classes come with a policy for how their memory will be managed:
some pool classes use :term:`automatic memory management` and others
use :term:`manual <manual memory management>`; some use :term:`moving
garbage collection` and others :term:`non-moving <non-moving garbage
collection>`. See the :ref:`Pool reference <pool>` for a list of
available pool classes and their properties.

In this example, we'll use :ref:`pool-amc`. This pool class uses
automatic memory management, moving garbage collection,
:term:`allocation points <allocation point>` and :term:`formatted
objects <formatted object>`.

.. note::

    The MPS is designed to co-operate with other memory managers, so
    when integrating your language with the MPS you need not feel
    obliged to move all your memory management to the MPS: you can
    continue to use ``malloc`` and ``free`` to manage some of your
    memory, for example, while using the MPS for the rest.

    The MPS is also designed for pools of different classes to
    co-exist in the same arena, so that objects requiring different
    memory management policies can be allocated in pools of suitable
    classes.

