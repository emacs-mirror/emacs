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
:ref:`guide-install`), and that you are familiar with the overall
architecture of the MPS (see :ref:`guide-overview`).


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

Each constructor allocates memory for the new object by calling
``malloc``. For example ``make_pair`` is the constructor for pairs::

    static obj_t make_pair(obj_t car, obj_t cdr)
    {
        obj_t obj = (obj_t)malloc(sizeof(pair_s));
        if (obj == NULL) error("out of memory");
        total += sizeof(pair_s);
        obj->pair.type = TYPE_PAIR;
        CAR(obj) = car;
        CDR(obj) = cdr;
        return obj;
    }

Objects do not get freed, because it is necessary to prove that they
are :term:`dead` before their memory can be :term:`reclaimed
<reclaim>`. And that task falls to the :term:`garbage collector`.


-----------------------
Choosing an arena class
-----------------------

You'll recall from the :ref:`overview <guide-overview>` that the
functionality of the MPS is divided between the :term:`arena`, which
requests memory from (and returns it to) the operating system, and
:term:`pools <pool>`, which allocate blocks of memory on behalf of
your program.

There are two main classes of arena: the :term:`client arena` (see
:c:func:`mps_arena_class_cl`) which gets its memory from your program,
and the :term:`virtual memory arena` (see
:c:func:`mps_arena_class_vm`) which gets its memory from the operating
system's :term:`virtual memory` interface.

The client arena is intended for use on embedded systems where there
is no virtual memory, and has a couple of disadvantages (you have to
decide up front how much memory you are going to need; and the MPS
can't return memory to the operating system for use by other
processes) so in most situations you'll want to use the virtual memory
arena.

We'll need a couple of headers (``mps.h`` for the MPS interface, and
``mpsavm.h`` for the virtual memory arena class)::

    #include "mps.h"
    #include "mpsavm.h"

Many MPS functions take an arena as an argument, so it makes sense for the arena to be a global variable::

    static mps_arena_t arena;

Create an arena by calling :c:func:`mps_arena_create`. This function
takes a third argument when creating a virtual memory arena: the size
of the initial amount of virtual address space, in bytes, that the
arena will reserve (though it may later ask for more). Let's start
with a megabyte::

    mps_res_t res;
    res = mps_arena_create(&arena,
                           mps_arena_class_vm(), 
                           (size_t)(1024 * 1024));
    if (res != MPS_RES_OK) error("Couldn't create arena");

:c:func:`mps_arena_create` is typical of functions in the MPS
interface in that it stores its result in a location pointed to by an
:term:`out parameter` (here, ``&arena``) and returns a :term:`result
code` which is :c:macro:`MPS_RES_OK` if the function succeeded, or
some other value if it failed.

.. note::

    The MPS is designed to co-operate with other memory managers, so
    when integrating your language with the MPS you need not feel
    obliged to move all your memory management to the MPS: you can
    continue to use ``malloc`` and ``free`` to manage some of your
    memory, for example, while using the MPS for the rest.

.. topics::

    :ref:`topic-arena`.


---------------------
Choosing a pool class
---------------------

Pool classes come with a policy for how their memory will be managed:
some pool classes use :term:`automatic memory management` and others
use :term:`manual <manual memory management>`; some use :term:`moving
collection <moving garbage collector>` and others :term:`non-moving
<non-moving garbage collector>`. See the :ref:`Pool reference <pool>`
for a list of available pool classes and their properties.

In this example, we'll use :ref:`pool-amc`. This pool class uses
automatic memory management, moving garbage collection,
:term:`allocation points <allocation point>` and :term:`formatted
objects <formatted object>`, so it will provide an introduction to
these features of the MPS.

.. note::

    The MPS is also designed for pools of different classes to
    co-exist in the same arena, so that objects requiring different
    memory management policies can be allocated in pools of suitable
    classes.


-----------------------
Describing your objects
-----------------------

In order for the MPS to be able to manage your objects, you need to
tell it how to perform various operations on those objects, which you
do by creating an :term:`object format`. Here's the code for creating
the object format for the Scheme interpreter::

    struct mps_fmt_A_s obj_fmt_s = {
        sizeof(mps_word_t),
        obj_scan,
        obj_skip,
        NULL,
        obj_fwd,
        obj_isfwd,
        obj_pad,
    };

    mps_fmt_t obj_fmt;
    res = mps_fmt_create_A(&obj_fmt, arena, &obj_fmt_s);
    if (res != MPS_RES_OK) error("Couldn't create obj format");

The structure :c:type:`mps_fmt_A_s` is the simplest of several object
format variants that are appropriate for moving pools like AMC.

The first element of the structure is the :term:`alignment` of objects
belonging to this format. The Scheme interpreter needs its objects to
be allocated at addresses which are multiples of the machine's word
size.

The other elements of the structure are the :term:`format methods
<format method>`, which are described in the following sections.

.. topics::

    :ref:`topic-format`.


.. _guide-lang-scan:

^^^^^^^^^^^^^^^
The scan method
^^^^^^^^^^^^^^^

The :term:`scan method` is a function of type
:c:type:`mps_fmt_scan_t`. It is called by the MPS to :term:`scan` a
block of memory. Its task is to identify all references within the
objects in the block of memory, and "fix" them, by calling the macros
:c:func:`MPS_FIX1` and :c:func:`MPS_FIX2` on each reference (usually
via the convenience macro :c:func:`MPS_FIX12`).

"Fixing" is a generic operation whose effect depends on the context in
which the scan method was called. The scan method is called to
discover references and so determine which objects are :term:`alive
<live>` and which are :term:`dead`, and also to update references
after objects have been moved.

Here's the scan method for the Scheme example::

    static mps_res_t obj_scan(mps_ss_t ss, mps_addr_t base, mps_addr_t limit)
    {
        MPS_SCAN_BEGIN(ss) {
            while (base < limit) {
                obj_t obj = base;
                switch (obj->type.type) {
                case TYPE_PAIR:
                    FIX(obj->pair.car);
                    FIX(obj->pair.cdr);
                    base = (char *)base + ALIGN(sizeof(pair_s));
                    break;
                case TYPE_INTEGER:
                    base = (char *)base + ALIGN(sizeof(integer_s));
                    break;
                /* ... and so on for the other types ... */
                default:
                    fprintf(stderr, "Unexpected object on the heap\n");
                    abort();
                }
            }
        } MPS_SCAN_END(ss);
        return MPS_RES_OK;
    }

The scan method receives a :term:`scan state` (``ss``) argument, and
the block of memory to scan, from ``base`` (inclusive) to ``limit``
(exclusive). This block of memory is known to be packed with objects
belonging to the object format, and so the scan method loops over the
objects in the block, dispatching on the type of each object, and then
updating ``base`` to point to the next object in the block.

For each reference in an object the scan method fixes it by calling
:c:func:`MPS_FIX12` via the macro ``FIX``, which is defined as
follows::

    #define FIX(ref)                                                        \
        do {                                                                \
            mps_addr_t _addr = (ref); /* copy to local to avoid type pun */ \
            mps_res_t res = MPS_FIX12(ss, &_addr);                          \
            if (res != MPS_RES_OK) return res;                              \
            (ref) = _addr;                                                  \
        } while (0)

Each call to :c:func:`MPS_FIX12` must appear between calls to the
macros :c:func:`MPS_SCAN_BEGIN` and :c:func:`MPS_SCAN_END`. It's
usually most convenient to call :c:func:`MPS_SCAN_BEGIN` at the start
of the function and :c:func:`MPS_SCAN_END` at the end.

There are a few things to watch out for:

1. When the MPS calls your scan method, it may be part-way through
   moving your objects. It is therefore essential that the scan method
   only examine objects in the range of addresses it is given. Objects
   in other ranges of addresses are not guaranteed to be in a
   consistent state.

2. Scanning is an operation on the :term:`critical path` of the MPS,
   which means that it is important that it runs as quickly as
   possible.

3. The "fix" operation may update the reference. So if your language
   has :term:`tagged references <tagged reference>`, you must make
   sure that the tag is restored after the reference is updated.

4. The "fix" operation may fail by returning a :term:`result code`
   other than :c:macro:`MPS_RES_OK`. A scan function must propagate
   such a result code to the caller, and should do so as soon as
   practicable.

.. topics::

    :ref:`topic-format`, :ref:`topic-scanning`.


^^^^^^^^^^^^^^^
The skip method
^^^^^^^^^^^^^^^

The :term:`skip method` is a function of type
:c:type:`mps_fmt_skip_t`. It is called by the MPS to skip over an
object belonging to the format.

Here's the skip method for the Scheme example::

    static mps_addr_t obj_skip(mps_addr_t base)
    {
        obj_t obj = base;
        switch (obj->type.type) {
        case TYPE_PAIR:
            base = (char *)base + ALIGN(sizeof(pair_s));
            break;
        case TYPE_INTEGER:
            base = (char *)base + ALIGN(sizeof(integer_s));
            break;
        /* ... and so on for the other types ... */
        default:
            fprintf(stderr, "Unexpected object on the heap\n");
            abort();
        }
        return base;
    }

The argument ``base`` is the address to the base of the object. The
skip method must return the address of the base of the "next object":
in formats of variant A like this one, this is the address of the word
just past the end of the object.

You'll see that the code in the skip method that computes the "next
object" is the same as the corresponding code in the :term:`scan
method`, so that it's tempting for the latter to delegate this part of
its functionality to the former.

.. topics::

    :ref:`topic-format`, :ref:`topic-scanning`.


^^^^^^^^^^^^^^^^^^
The forward method
^^^^^^^^^^^^^^^^^^

The :term:`forward method` is a function of type
:c:type:`mps_fmt_fwd_t`. It is called by the MPS after it has moved an
object, and its task is to replace the old object with a
:term:`forwarding object` pointing to the new location of the object.

The forwarding object must satisfy these properties:

1. It must be scannable and skippable, and so it will need to have a
   type field to distinguish it from other Scheme objects.

2. It must contain a pointer to the new location of the object (a
   :term:`forwarding pointer`).

3. The :ref:`scan method <guide-lang-scan>` and the skip method will
   both need to know the length of the forwarding object. This can be
   arbitarily long (in the case of string objects, for example) so it
   must contain a length field.

This poses a problem, because the above analysis suggests that
forwarding objects need to contain at least three words, but Scheme
objects might be as small as two words (for example, integers).

This conundrum can be solved by having two types of forwarding object.
The first type is suitable for forwarding objects of three words or
longer::

    typedef struct fwd_s {
        type_t type;                  /* TYPE_FWD */
        obj_t fwd;                    /* forwarded object */
        size_t size;                  /* total size of this object */
    } fwd_s;

while the second type is suitable for forwarding objects of two words::

    typedef struct fwd2_s {
        type_t type;                  /* TYPE_FWD2 */
        obj_t fwd;                    /* forwarded object */
    } fwd2_s;

Here's the forward method for the Scheme example::

    static void obj_fwd(mps_addr_t old, mps_addr_t new)
    {
        obj_t obj = old;
        mps_addr_t limit = obj_skip(old);
        size_t size = (char *)limit - (char *)old;
        assert(size >= ALIGN(sizeof(fwd2_s)));
        if (size == ALIGN(sizeof(fwd2_s))) {
            obj->type.type = TYPE_FWD2;
            obj->fwd2.fwd = new;
        } else {
            obj->type.type = TYPE_FWD;
            obj->fwd.fwd = new;
            obj->fwd.size = size;
        }
    }

The argument ``old`` is the old address of the object, and ``new`` is
the location to which it has been moved.

The fowarding objects must be scannable and skippable, so the
following code must be added to ``obj_scan`` and ``obj_skip``::

    case TYPE_FWD:
        base = (char *)base + ALIGN(obj->fwd.size);
        break;
    case TYPE_FWD2:
        base = (char *)base + ALIGN(sizeof(fwd2_s));
        break;

.. note::

    The Scheme interpreter has no objects consisting of a single word.
    If it did, this would present problems for the design of the
    forwarding object. The best approach in such a case would be to
    allocate the single-word objects from a separate pool: if, as
    seems likely, these objects do not contain references, they could
    be allocated from the :ref:`pool-lo` pool, and so the cost of
    scanning them could be avoided.
    
.. topics::

    :ref:`topic-format`.


^^^^^^^^^^^^^^^^^^^^^^^
The is-forwarded method
^^^^^^^^^^^^^^^^^^^^^^^

The :term:`is-forwarded method` is a function of type
:c:type:`mps_fmt_isfwd_t`. It is called by the MPS to determine if an
object is a :term:`forwarding object`, and if it is, to determine the
location where that object was moved.

Here's the is-forwarded method for the Scheme example::

    static mps_addr_t obj_isfwd(mps_addr_t addr)
    {
        obj_t obj = addr;
        switch (obj->type.type) {
        case TYPE_FWD2:
            return obj->fwd2.fwd;
        case TYPE_FWD:
            return obj->fwd.fwd;
        }
        return NULL;
    }

It receives the address of an object, and returns the address to which
that object was moved, or ``NULL`` if the object was not moved.

.. topics::

    :ref:`topic-format`.


^^^^^^^^^^^^^^^^^^
The padding method
^^^^^^^^^^^^^^^^^^

The :term:`padding method` is a function of type
:c:type:`mps_fmt_pad_t`. It is called by the MPS to fill a block of
memory with a :term:`padding object`: this is an object that fills
gaps in a block of :term:`formatted objects <formatted object>`, for
example to enable the MPS to pack objects into fixed-size units (such
as operating system :term:`pages <page>`).

A padding object must be scannable and skippable, and not confusable
with a :term:`forwarding object`. This means they need a type and a
size. However, padding objects might need to be as small as the
alignment of the object format, which was specified to be a single
word. As with forwarding objects, this can be solved by having two
types of padding object. The first type is suitable for paddding
objects of two words or longer::

    typedef struct pad_s {
        type_t type;                  /* TYPE_PAD */
        size_t size;                  /* total size of this object */
    } pad_s;

while the second type is suitable for padding objects consisting of a
single word::

    typedef struct pad1_s {
        type_t type;                  /* TYPE_PAD1 */
    } pad1_s;

Here's the padding method::

    static void obj_pad(mps_addr_t addr, size_t size)
    {
        obj_t obj = addr;
        assert(size >= ALIGN(sizeof(pad1_s)));
        if (size == ALIGN(sizeof(pad1_s))) {
            obj->type.type = TYPE_PAD1;
        } else {
            obj->type.type = TYPE_PAD;
            obj->pad.size = size;
        }
    }

The argument ``addr`` is the address at which the padding object must be created, and ``size`` is its size in bytes: this will always be a multiple of the alignment of the object format.

The padding objects must be scannable and skippable, so the following
code must be added to ``obj_scan`` and ``obj_skip``::

    case TYPE_PAD:
        base = (char *)base + ALIGN(obj->pad.size);
        break;
    case TYPE_PAD1:
        base = (char *)base + ALIGN(sizeof(pad1_s));
        break;

.. topics::

    :ref:`topic-format`.


-----------------
Generation chains
-----------------

The AMC pool requires not only an object format but a
:term:`generation chain`. This specifies the generation structure of
the :term:`generational garbage collection`.

You create a generation chain by constructing an array of structures
of type :c:type:`mps_gen_param_s` and passing them to
:c:func:`mps_chain_create`. Here's the code for creating the
generation chain for the Scheme interpreter::

    mps_gen_param_s obj_gen_params[] = {
        { 150, 0.85 },
        { 170, 0.45 },
    };

    res = mps_chain_create(&obj_chain,
                           arena,
                           LENGTH(obj_gen_params),
                           obj_gen_params);
    if (res != MPS_RES_OK) error("Couldn't create obj chain");


-----------------
Creating the pool
-----------------

Now you know enough to create an :ref:`pool-amc` pool! Let's review
the pool creation code. First the :term:`object format`::

    struct mps_fmt_A_s obj_fmt_s = {
        sizeof(mps_word_t),
        obj_scan,
        obj_skip,
        NULL,
        obj_fwd,
        obj_isfwd,
        obj_pad,
    };

    mps_fmt_t obj_fmt;
    res = mps_fmt_create_A(&obj_fmt, arena, &obj_fmt_s);
    if (res != MPS_RES_OK) error("Couldn't create obj format");

then the :term:`generation chain`::

    mps_gen_param_s obj_gen_params[] = {
        { 150, 0.85 },
        { 170, 0.45 },
    };

    mps_chain_t obj_chain;
    res = mps_chain_create(&obj_chain,
                           arena,
                           LENGTH(obj_gen_params),
                           obj_gen_params);
    if (res != MPS_RES_OK) error("Couldn't create obj chain");

and finally the :term:`pool`::

    mps_pool_t obj_pool;
    res = mps_pool_create(&obj_pool,
                          arena,
                          mps_class_amc(),
                          obj_fmt,
                          obj_chain);
    if (res != MPS_RES_OK) error("Couldn't create obj pool");


-----
Roots
-----

The :term:`object format` tells the MPS how to find :term:`references
<reference>` from one object to another. This allows the MPS to
extrapolate the reachability property: if object *A* is
:term:`reachable`, and the :term:`scan method` fixes a reference from
*A* to another object *B*, then *B* is reachable too.

But how does this process get started? How does the MPS know which
objects are reachable *a priori*? Such objects are known as
:term:`roots <root>`, and you must register them with the MPS,
creating root descriptions of type :c:type:`mps_root_t`.

The most important root consists of the contents of the
:term:`registers <register>` and the :term:`control stack` of each
:term:`thread` in your program: this is covered in :ref:`Threads <guide-lang-threads>`, below.

Other roots may be found in static variables in your program, or in
memory allocated by other memory managers. For these roots you must
describe to the MPS how to :term:`scan` them for references.

The Scheme interpreter has a number of static variables that point to
heap-allocated objects. First, the special objects, including::

    static obj_t obj_empty;         /* (), the empty list */

Second, the predefined symbols, including::

    static obj_t obj_quote;         /* "quote" symbol */

And third, the global symbol table::

    static obj_t *symtab;
    static size_t symtab_size;

You tell the MPS how to scan these by writing root scanning functions
of type :c:type:`mps_reg_scan_t`. These function are similar to the
:ref:`scan method <guide-lang-scan>` in an :term:`object format`,
described above.

In the case of the Scheme interpreter, the root scanning function for
the special objects and the predefined symbols could be written like
this::

    static mps_res_t globals_scan(mps_ss_t ss, void *p, size_t s)
    {
        MPS_SCAN_BEGIN(ss) {
            FIX(obj_empty);
            /* ... and so on for the special objects ... */
            FIX(obj_quote);
            /* ... and so on for the predefined symbols ... */
        } MPS_SCAN_END(ss);
        return MPS_RES_OK;
    }

but in fact the interpreter already has tables of these global
objects, so it's simpler and more extensible for the root scanning
function to iterate over them::

    static mps_res_t globals_scan(mps_ss_t ss, void *p, size_t s)
    {
        MPS_SCAN_BEGIN(ss) {
            size_t i;
            for (i = 0; i < LENGTH(sptab); ++i)
                FIX(*sptab[i].varp);
            for (i = 0; i < LENGTH(isymtab); ++i)
                FIX(*isymtab[i].varp);
        } MPS_SCAN_END(ss);
        return MPS_RES_OK;
    }

Each root scanning function must be registered with the MPS by calling
:c:func:`mps_root_create`, like this::

    mps_root_t globals_root;
    res = mps_root_create(&globals_root, arena, mps_rank_exact(), 0,
                          globals_scan, NULL, 0);
    if (res != MPS_RES_OK) error("Couldn't register globals root");

The third argument (here :c:func:`mps_rank_exact`) is the :term:`rank`
of references in the root. ":term:`Exact <exact reference>`" means
that:

1. all references in the root point to another object (there are no
   :term:`ambiguous references <ambiguous reference>`); and

2. each reference keeps the target of the reference alive (unlike
   :term:`weak references <weak reference (1)>`.

The fourth argument (here ``0``) is the :term:`root mode`: see
:ref:`topic-root`.

The sixth and seventh arguments (here ``NULL`` and ``0``) are passed
to the root scanning function where they are received as the
parameters ``p`` and ``s`` respectively. In this case there was no
need to use them.

What about the global symbol table? This is trickier, because it gets
rehashed from time to time, and during the rehashing process there are
two copies of the symbol table in existence. Because the MPS is
:term:`asynchronous <asynchronous garbage collector>`, it might be
scanning, moving, or collecting at any point in time, and if it is
doing so during the rehashing of the symbol table it had better scan
both the old and new copies of the table. This is most conveniently
done by registering a new root to refer to the new copy, and then
after the rehash has completed, de-registering the old root by calling
:c:func:`mps_root_destroy`.

It would be possible to write a root scanning functions of type
:c:type:`mps_reg_scan_t`, as described above, to fix the references in
the global symbol table, but the case of a table of references is
sufficiently common that the MPS provides a convenience function,
:c:func:`mps_root_create_table`, for registering it::

    static mps_root_t symtab_root;
    res = mps_root_create_table(&symtab_root, arena, mps_rank_exact(), 0,
                                (mps_addr_t *)symtab, symtab_size);
    if (res != MPS_RES_OK) error("Couldn't register new symtab root");

The root must be re-registered whenever the global symbol table
changes size::

    static void rehash(void) {
        obj_t *old_symtab = symtab;
        unsigned old_symtab_size = symtab_size;
        mps_root_t old_symtab_root = symtab_root;
        unsigned i;
        mps_res_t res;

        symtab_size *= 2;
        symtab = malloc(sizeof(obj_t) * symtab_size);
        if (symtab == NULL) error("out of memory");

        /* Initialize the new table to NULL so that "find" will work. */
        for (i = 0; i < symtab_size; ++i)
            symtab[i] = NULL;

        res = mps_root_create_table(&symtab_root, arena, mps_rank_exact(), 0,
                                    (mps_addr_t *)symtab, symtab_size);
        if (res != MPS_RES_OK) error("Couldn't register new symtab root");

        for (i = 0; i < old_symtab_size; ++i)
            if (old_symtab[i] != NULL) {
                obj_t *where = find(old_symtab[i]->symbol.string);
                assert(where != NULL);    /* new table shouldn't be full */
                assert(*where == NULL);   /* shouldn't be in new table */
                *where = old_symtab[i];
            }

        mps_root_destroy(old_symtab_root);
        free(old_symtab);
    }

There are a few things to take note of here:

1. The old root description (referring to the old copy of the symbol
   table) is not destroyed until after the new root description has
   been registered. This is because the MPS is :term:`asynchronous
   <asynchronous garbage collector>`: it might be scanning, moving, or
   collecting at any point in time. If the old root description were
   destroyed before the new root description was registered, there
   would be a period in which the symbol table was not reachable (at
   least as far as the MPS was concerned) and so all the objects
   referenced by it (and all the objects reachable from *those*
   objects) might be dead.

2. The root might be scanned as soon as it is registered, so it is
   important to fill it with scannable references (``NULL`` in this
   case) before registering it.

3. The order of operations at the end is important: the old root must
   be de-registered before its memory is freed.

.. topics::

    :ref:`topic-root`.


.. _guide-lang-threads:

-------
Threads
-------




----------
Allocation
----------



.. topics::

    :ref:`topic-allocation`.




-----------------------
Maintaining consistency
-----------------------

The collector runs asynchronously. So when do objects have to be valid (scannable)?


----------
Tidying up
----------



---------------
Advanced topics
---------------

* Messages.

* Some of the Scheme objects could be moved to a leaf-only pool.

* Telemetry labels.
