.. _mmref-overview:

Overview
========

Memory management is a complex field of computer science and there are
many techniques being developed to make it more efficient. This guide
is designed to introduce you to some of the basic memory management
issues that programmers face.

This guide attempts to explain any terms it uses as it introduces
them. In addition, there is a :ref:`glossary` of memory management
terms that gives fuller information; some terms are linked to the
relevant entries.

:term:`Memory management` is usually divided into three areas,
although the distinctions are a little fuzzy:

* :ref:`mmref-overview-hardware`
* :ref:`mmref-overview-os`
* :ref:`mmref-overview-app`

These are described in more detail below. In most computer systems,
all three are present to some extent, forming layers between the
user's program and the actual memory hardware. The Memory Management
Reference is mostly concerned with application memory management.


.. _mmref-overview-hardware:

Hardware memory management
--------------------------

Memory management at the hardware level is concerned with the
electronic devices that actually store data. This includes things like
:term:`RAM` and :term:`memory caches <cache (1)>`.


.. _mmref-overview-os:

Operating system memory management
----------------------------------

In the operating system, memory must be allocated to user programs,
and reused by other programs when it is no longer required. The
operating system can pretend that the computer has more memory than it
actually does, and also that each program has the machine's memory to
itself; both of these are features of :term:`virtual memory` systems.


.. _mmref-overview-app:

Application memory management
-----------------------------

Application memory management involves supplying the memory needed for
a program's objects and data structures from the limited resources
available, and recycling that memory for reuse when it is no longer
required. Because application programs cannot in general predict in
advance how much memory they are going to require, they need
additional code to handle their changing memory requirements.

Application memory management combines two related tasks:

**Allocation**

    When the program requests a block of memory, the memory manager
    must allocate that block out of the larger blocks it has received
    from the operating system. The part of the memory manager that
    does this is known as the :term:`allocator`. There are many ways
    to perform allocation, a few of which are discussed in
    :ref:`mmref-alloc`.

**Recycling**

    When memory blocks have been allocated, but the data they contain
    is no longer required by the program, then the blocks can be
    recycled for reuse. There are two approaches to recycling memory:
    either the programmer must decide when memory can be reused (known
    as :term:`manual memory management`); or the memory manager must
    be able to work it out (known as :term:`automatic memory
    management`). These are both described in more detail below.

An application memory manager must usually work to several
constraints, such as:

**CPU overhead**

    The additional time taken by the memory manager while the program
    is running.

**Pause times**

    The time it takes for the memory manager to complete an operation
    and return control to the program.

    This affects the program's ability to respond promptly to
    interactive events, and also to any asynchronous event such as a
    network connection.

**Memory overhead**

    How much space is wasted for administration, rounding (known as
    :term:`internal fragmentation`), and poor layout (known as
    :term:`external fragmentation`).

Some of the common problems encountered in application memory
management are considered in the next section.


.. _mmref-overview-problem:

Memory management problems
--------------------------

The basic problem in managing memory is knowing when to keep the data
it contains, and when to throw it away so that the memory can be
reused. This sounds easy, but is, in fact, such a hard problem that it
is an entire field of study in its own right. In an ideal world, most
programmers wouldn't have to worry about memory management issues.
Unfortunately, there are many ways in which poor memory management
practice can affect the robustness and speed of programs, both in
manual and in automatic memory management.

Typical problems include:

**Premature frees and dangling pointers**

    Many programs give up memory, but attempt to access it later and
    crash or behave randomly. This condition is known as a
    :term:`premature free`, and the surviving reference to the memory
    is known as a :term:`dangling pointer`. This is usually confined
    to :term:`manual memory management`.

**Memory leak**

    Some programs continually allocate memory without ever giving it
    up and eventually run out of memory. This condition is known as a
    :term:`memory leak`.

**External fragmentation**

    A poor allocator can do its job of giving out and receiving blocks
    of memory so badly that it can no longer give out big enough
    blocks despite having enough spare memory. This is because the
    free memory can become split into many small blocks, separated by
    blocks still in use. This condition is known as :term:`external
    fragmentation`.

**Poor locality of reference**

    Another problem with the layout of allocated blocks comes from the
    way that modern hardware and operating system memory managers
    handle memory: successive memory accesses are faster if they are
    to nearby memory locations. If the memory manager places far apart
    the blocks a program will use together, then this will cause
    performance problems. This condition is known as poor
    :term:`locality of reference`.

**Inflexible design**

    Memory managers can also cause severe performance problems if they
    have been designed with one use in mind, but are used in a
    different way. These problems occur because any memory management
    solution tends to make assumptions about the way in which the
    program is going to use memory, such as typical block sizes,
    reference patterns, or lifetimes of objects. If these assumptions
    are wrong, then the memory manager may spend a lot more time doing
    bookkeeping work to keep up with what's happening.

**Interface complexity**

    If objects are passed between modules, then the interface design
    must consider the management of their memory.

A well-designed memory manager can make it easier to write debugging
tools, because much of the code can be shared. Such tools could
display objects, navigate links, validate objects, or detect abnormal
accumulations of certain object types or block sizes.


.. _mmref-overview-manual:

Manual memory management
------------------------

Manual memory management is where the programmer has direct control
over when memory may be recycled. Usually this is either by explicit
calls to :term:`heap` management functions (for example,
:term:`malloc` and :term:`free (2)` in :term:`C`), or by language
constructs that affect the :term:`control stack` (such as local
variables). The key feature of a manual memory manager is that it
provides a way for the program to say, "Have this memory back; I've
finished with it"; the memory manager does not recycle any memory
without such an instruction.

The advantages of manual memory management are:

* it can be easier for the programmer to understand exactly what is
  going on;

* some manual memory managers perform better when there is a shortage
  of memory.

The disadvantages of manual memory management are:

* the programmer must write a lot of code to do repetitive bookkeeping
  of memory;

* memory management must form a significant part of any module interface;

* manual memory management typically requires more memory overhead per
  object;

* memory management bugs are common.

It is very common for programmers, faced with an inefficient or
inadequate manual memory manager, to write code to duplicate the
behavior of a memory manager, either by allocating large blocks and
splitting them for use, or by recycling blocks internally. Such code
is known as a :term:`suballocator`. Suballocators can take advantage
of special knowledge of program behavior, but are less efficient in
general than fixing the underlying allocator. Unless written by a
memory management expert, suballocators may be inefficient or
unreliable.

The following languages use mainly manual memory management in most
implementations, although many have :term:`conservative garbage
collection` extensions: :term:`Algol`; :term:`C`; :term:`C++`;
:term:`COBOL`; :term:`Fortran`; :term:`Pascal`.


.. _mmref-overview-automatic:

Automatic memory management
---------------------------

Automatic memory management is a service, either as a part of the
language or as an extension, that automatically recycles memory that a
program would not otherwise use again. Automatic memory managers
(often known as garbage collectors, or simply collectors) usually do
their job by recycling blocks that are :term:`unreachable` from the
program variables (that is, blocks that cannot be reached by following
pointers).

The advantages of automatic memory management are:

* the programmer is freed to work on the actual problem;

* module interfaces are cleaner;

* there are fewer memory management bugs;

* memory management is often more efficient.

The disadvantages of automatic memory management are:

* memory may be retained because it is reachable, but won't be used again;

* automatic memory managers (currently) have limited availability.

There are many ways of performing automatic recycling of memory, a few
of which are discussed in :ref:`mmref-recycle`.

Most modern languages use mainly automatic memory management:
:term:`BASIC`, :term:`Dylan`, Erlang, Haskell, :term:`Java`,
:term:`JavaScript`, :term:`Lisp`, :term:`ML`, :term:`Modula-3`,
:term:`Perl`, :term:`PostScript`, :term:`Prolog`, Python,
:term:`Scheme`, :term:`Smalltalk`, etc.


More information
----------------

For more detailed information on the topics covered briefly above,
please have a look at the :ref:`glossary`. Books and research papers
are available on many specific techniques, and can be found via our
:ref:`bibliography`; particularly recommended are: :ref:`Wilson (1994)
<WIL94>`, which is survey of garbage collection techniques;
:ref:`Wilson et al. (1995) <WIL95>`, which is a survey of allocation
techniques; and :ref:`Jones et al. (2012) <JONES12>`, which is a
handbook covering all aspects of garbage collection.
