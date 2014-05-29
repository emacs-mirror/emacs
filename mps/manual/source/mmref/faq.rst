.. _mmref-faq:

Frequently Asked Questions
==========================

This is a list of questions that represent the problems people often
have with memory management. Some answers appear below, with links to
helpful supporting material, such as the :ref:`glossary`, the
:ref:`bibliography`, and external sites. For a full explanation of any
terms used, see the glossary.


C-specific questions
--------------------


Can I use garbage collection in C?
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Yes. Various :term:`conservative garbage collectors <conservative
garbage collection>` for :term:`C` exist as add-on libraries.

.. link::

    `Memory Pool System <http://www.ravenbrook.com/project/mps/>`_,
    `Boehm–Demers–Weiser collector <http://hboehm.info/gc/>`_.


Why do I need to test the return value from malloc?  Surely it always succeeds?
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

For small programs, and during light testing, it is true that
:term:`malloc` usually succeeds. Unfortunately, there are all sorts of
unpredictable reasons why :term:`malloc` might fail one day; for
example:

* someone uses your program for a far larger data set than you
  anticipated;

* your program is running on a machine with less memory than you
  expected;

* the machine your program is running on is heavily loaded.

In this case, :term:`malloc` will return ``NULL``, and your program
will attempt to store data by resolving the null pointer. This might
cause your program to exit immediately with a helpful message, but it
is more likely to provoke mysterious problems later on.

If you want your code to be robust, and to stand the test of time, you
must check all error or status codes that may be returned by functions
you call, especially those in other libraries, such as the C run-time
library.

If you really don't want to check the return value from
:term:`malloc`, and you don't want your program to behave mysteriously
when out of memory, wrap :term:`malloc` in something like this::

    #include <stdio.h>
    #include <stdlib.h>

    void *my_malloc(size_t size)
    {
        void *p = malloc(size);

        if (p == NULL) {
            fputs("Out of memory.\n", stderr);
            exit(EXIT_FAILURE);
        }

        return p;
    }

Undefined behavior is worth eliminating even in small programs.


What's the point of having a garbage collector? Why not use malloc and free?
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

:term:`Manual memory management`, such as :term:`malloc` and
:term:`free (2)`, forces the programmer to keep track of which memory
is still required, and who is responsible for freeing it. This works
for small programs without internal interfaces, but becomes a rich
source of bugs in larger programs, and is a serious problem for
interface abstraction.

:term:`Automatic memory management` frees the programmer from these
concerns, making it easier for him to code in the language of his
problem, rather than the tedious details of the implementation.

.. seealso:: :term:`garbage collection`


What's wrong with ANSI malloc in the C library?
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The :term:`malloc` function provides a very basic :term:`manual memory
management` service. However, it does not provide the following
things, which may be desirable in your memory manager:

* high performance for specified block sizes;
* :term:`tagged references`;
* simultaneous frees;
* :term:`locality of reference` hints;
* :term:`formatted objects`;
* garbage collection;
* deallocation of partial blocks;
* multi-threading without synchronization;
* inlined allocation code;
* :term:`finalization`.

Many of these can be added on top of :term:`malloc`, but not with full
performance.


C++-specific questions
----------------------


.. _mmref-faq-c++-gc:

Can I use garbage collection in C++?
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Yes. The C++ specification has always permitted garbage collection.
Bjarne Stroustrup (C++'s designer) has proposed that this be made
explicit in the standard. There exist various conservative and
semi-conservative garbage collectors for C++.

.. seealso:: :term:`C++`, :term:`conservative garbage collection`, :term:`semi-conservative garbage collection`.

.. link::

    `Memory Pool System <http://www.ravenbrook.com/project/mps/>`_,
    `Boehm–Demers–Weiser collector <http://hboehm.info/gc/>`_.


Why is delete so slow?
^^^^^^^^^^^^^^^^^^^^^^

Often ``delete`` must perform a more complex task than simply freeing
the memory associated with an object; this is known as
:term:`finalization`. Finalization typically involves releasing any
resources indirectly associated with the object, such as files that
must be closed or ancillary objects that must be finalized themselves.
This may involve traversing memory that has been unused for some time
and hence is :term:`paged out`.

With :term:`manual memory management` (such as ``new`` and
``delete``), it is perfectly possible for the :term:`deallocation
<free (1)>` operation to vary in complexity. Some systems do quite a
lot of processing on freed blocks to :term:`coalesce` adjacent blocks,
sort free blocks by size (in a :term:`buddy system`, say), or sort the
:term:`free list` by address. In the last case, deallocating blocks in
address order (or sometimes reverse address order) can result in poor
performance.


What happens if you use class libraries that leak memory?
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

In :term:`C++`, it may be that class libraries expect you to call
``delete`` on objects they create, to invoke the :term:`destructor
(2)`. Check the interface documentation.

Failing this, if there is a genuine :term:`memory leak` in a class
library for which you don't have the source, then the only thing you
can try is to add a :term:`garbage collector`.

.. link::

    `Memory Pool System <http://www.ravenbrook.com/project/mps/>`_,
    `Boehm–Demers–Weiser collector <http://hboehm.info/gc/>`_.


Can't I get all the benefits of garbage collection using C++ constructors and destructors?
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Carefully designed :term:`C++` :term:`constructors (2)` and
:term:`destructors (2)` can go a long way towards easing the pain of
:term:`manual memory management`. Objects can know how to deallocate
all their associated resources, including dependent objects (by
recursive destruction). This means that clients of a class library do
not need to worry about how to free resources allocated on their
behalf.

Unfortunately, they still need to worry about *when* to free such
resources. Unless all objects are allocated for precisely one purpose,
and referred to from just one place (or from within one compound data
structure that will be destroyed atomically), then a piece of code
that has finished with an object cannot determine that it is safe to
call the destructor; it cannot be certain (especially when working
with other people's code) that there is not another piece of code that
will try to use the object subsequently.

This is where garbage collection has the advantage, because it can
determine when a given object is no longer of interest to anyone (or
at least when there are no more references to it). This neatly avoids
the problems of having multiple copies of the same data or complex
conditional destruction. The program can construct objects and store
references to them anywhere it finds convenient; the garbage collector
will deal with all the problems of data sharing.


Common objections to garbage collection
---------------------------------------


What languages use garbage collection?
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

:term:`Java`, :term:`C#`, :term:`Python`, :term:`Lisp`, :term:`ML`, …
the list goes on. It surprises many to learn that many implementations
of :term:`BASIC` use :term:`garbage collection` to manage character
strings efficiently.

:term:`C++` is sometimes characterized as the last holdout against
garbage collection, but this is not accurate. See
:ref:`mmref-faq-c++-gc`

The notion of :term:`automatic memory management` has stood the test
of time and is becoming a standard part of modern programming
environments. Some will say "the right tool for the right job",
rejecting automatic memory management in some cases; few today are
bold enough to suggest that there is never a place for garbage
collection among tools of the modern programmer---either as part of a
language or as an add-on component.


What's the advantage of garbage collection?
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

:term:`Garbage collection` frees you from having to keep track of
which part of your program is responsible for the deallocation of
which memory. This freedom from tedious and error-prone bookkeeping
allows you to concentrate on the problem you are trying to solve,
without introducing additional problems of implementation.

This is particularly important in large-scale or highly modular programs,
especially libraries, because the problems of manual memory management
often dominate interface complexity.  Additionally, garbage collection can reduce the amount of memory used because the interface problems of manual memory management are often solved by creating extra copies of data.

In terms of performance, garbage collection is often faster than manual memory management.  It can also improve performance indirectly, by increasing :term:`locality of reference` and hence reducing the size of the :term:`working set`, and decreasing :term:`paging`.

.. bibref:: :ref:`Zorn (1992) <ZORN92>`.


Programs with GC are huge and bloated; GC isn't suitable for small programs or systems
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

While it is true that the major advantages of :term:`garbage
collection` are only seen in complex systems, there is no reason for
garbage collection to introduce any significant overhead at any scale.
The data structures associated with garbage collection compare
favorably in size with those required for :term:`manual memory
management`.

Some older systems gave garbage collection a bad name in terms of
space or time overhead, but many modern techniques exist that make
such overheads a thing of the past. Additionally, some garbage
collectors are designed to work best in certain problem domains, such
as large programs; these may perform poorly outside their target
environment.

.. bibref:: :ref:`Zorn (1992) <ZORN92>`.


I can't use GC because I can't afford to have my program pause
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

While early garbage collectors had to complete without interruption
and hence would pause observably, many techniques are now available to
ensure that modern collectors can be unobtrusive.

.. seealso:: :term:`incremental garbage collection`, :term:`parallel garbage collection`.


Isn't it much cheaper to use reference counts rather than garbage collection?
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

No, updating :term:`reference counts <reference counting>` is quite
expensive, and they have a couple of problems:

* They can't cope with :term:`cyclic data structures`; that is, sets
  of objects that are referred to only by objects in that set, but
  that don't have a zero reference count.

* Reference counting gets more expensive if you have to allow for the
  count overflowing.

There are many systems that use reference counts, and avoid the
problems described above by using a conventional :term:`garbage
collector` to complement it. This is usually done for real-time
benefits. Unfortunately, experience shows that this is generally less
efficient than implementing a proper real-time garbage collector,
except in the case where most reference counts are one.

.. bibref:: :ref:`Wise (1993) <WISE93>`.


Isn't GC unreliable? I've heard that GCs often kill the program
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

:term:`Garbage collectors` usually have to manipulate vulnerable data
structures and must often use poorly-documented, low-level interfaces.
Additionally, any garbage collection problems may not be detected
until some time later. These factors combine to make most garbage
collection bugs severe in effect, hard to reproduce, and difficult to
work around.

On the other hand, commercial garbage collection code will generally
be heavily tested and widely used, which implies it must be reliable.
It will be hard to match that reliability in a manual memory manager
written for one program, especially given that :term:`manual memory
management` doesn't scale as well as the automatic variety.

In addition, bugs in the compiler or run-time (or application if the
language is as low-level as :term:`C`) can corrupt the heap in ways
that only the garbage collector will detect later. The collector is
blamed because it found the corruption. This is a classic case of
shooting the messenger.


I've heard that GC uses twice as much memory
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

This may be true of primitive collectors (like the :term:`two-space
collector`), but this is not generally true of garbage collection. The
data structures used for garbage collection need be no larger than
those for :term:`manual memory management`.


Doesn't garbage collection make programs slow?
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

No. :ref:`Benjamin Zorn (1992) <ZORN92>` found that:

    the CPU overhead of :term:`conservative garbage collection` is
    comparable to that of explicit storage management techniques. […]
    Conservative garbage collection performs faster than some explicit
    algorithms and slower than others, the relative performance being
    largely dependent on the program.

Note also that the version of the conservative collector used in this
paper is now rather old and the collector has been much improved since
then.


Manual memory management gives me control---it doesn't pause
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

It is possible for :term:`manual memory management` to pause for
considerable periods, either on :term:`allocation <allocate>` or
:term:`deallocation <free (1)>`. It certainly gives no guarantees
about performance, in general.

With :term:`automatic memory management`, such as :term:`garbage
collection`, modern techniques can give guarantees about interactive
pause times, and so on.

.. seealso:: :term:`incremental garbage collection`, :term:`parallel garbage collection`.


Miscellaneous
-------------

Why does my disk rattle so much?
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

When you are using a :term:`virtual memory` system, the computer may
have to fetch :term:`pages` of memory from disk before they can be
accessed. If the total :term:`working set` of your active programs
exceeds the :term:`physical memory (1)` available, :term:`paging` will
happen continually, your disk will rattle, and performance will
degrade significantly. The only solutions are to install more physical
memory, run fewer programs at the same time, or tune the memory
requirements of your programs.

The problem is aggravated because virtual memory systems approximate
the theoretical working set with the set of pages on which the working
set lies. If the actual working set is spread out onto a large number
of pages, then the working page-set is large.

When objects that refer to each other are distant in memory, this is
known as poor :term:`locality of reference`. This happens either
because the program's designer did not worry about this, or the memory
manager used in the program doesn't permit the designer to do anything
about it.

Note that :term:`copying garbage collection` can dynamically organize
your data according to the program's reference patterns and thus
mitigate this problem.

.. seealso:: :term:`thrash`

.. bibref:: :ref:`Denning (1968) <DENNING68>`.


Where can I find out more about garbage collection?
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Many modern languages have :term:`garbage collection` built in, and
the language documentation should give details. For some other
languages, garbage collection can be added, for example via the
Memory Pool System, or the Boehm–Demers–Weiser collector.

.. seealso:: :term:`garbage collection`

.. bibref:: :ref:`Jones et al. (2012) <JONES12>`, :ref:`Wilson (1994) <WIL94>`.

.. link::

    `Memory Pool System <http://www.ravenbrook.com/project/mps/>`_,
    `Boehm–Demers–Weiser collector <http://hboehm.info/gc/>`_,
    `GC-LIST FAQ <http://iecc.com/gclist/GC-faq.html>`_.


Where can I get a garbage collector?
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The Memory Pool System and the Boehm–Demers–Weiser collector are
suitable for C or C++. The best way to get a garbage collector,
however, is to program in a language that provides garbage collection
natively.

.. seealso:: :term:`garbage collection`

.. link::

    `Memory Pool System <http://www.ravenbrook.com/project/mps/>`_,
    `Boehm–Demers–Weiser collector <http://hboehm.info/gc/>`_.


Why does my program use so much memory?
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

If you are using :term:`manual memory management` (for example,
:term:`malloc` and :term:`free (2)` in :term:`C`), it is likely that
your program is failing to free memory blocks after it stops using
them. When your code allocates memory on the heap, there is an implied
responsibility to free that memory. If a function uses heap memory for
returning data, you must decide who takes on that responsibility. Pay
special attention to the interfaces between functions and modules.
Remember to check what happens to allocated memory in the event of an
error or an exception.

If you are using :term:`automatic memory management` (almost certainly
:term:`garbage collection`), it is probable that your code is
remembering some blocks that it will never use in future. This is
known as the difference between :term:`liveness <live>` and
:term:`reachability <reachable>`. Consider clearing variables that
refer to large blocks or networks of blocks, when the data structure
is no longer required.


I use a library, and my program grows every time I call it. Why?
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

If you are using :term:`manual memory management`, it is likely that
the library is allocating data structures on the heap every time it is
used, but that they are not being freed. Check the interface
documentation for the library; it may expect you to take some action
when you have finished with returned data. It may be necessary to
close down the library and re-initialize it to recover allocated
memory.

Unfortunately, it is all too possible that the library has a memory
management bug. In this case, unless you have the source code, there
is little you can do except report the problem to the supplier. It may
be possible to add a garbage collector to your language, and this
might solve your problems.

With a :term:`garbage collector`, sometimes objects are retained
because there is a reference to them from some global data structure.
Although the library might not make any further use of the objects,
the collector must retain the objects because they are still
:term:`reachable`.

If you know that a particular reference will never be used in future,
it can be worthwhile to overwrite it. This means that the collector
will not retain the referred object because of that reference. Other
references to the same object will keep it :term:`alive <live>`, so
your program doesn't need to determine whether the object itself will
ever be accessed in future. This should be done judiciously, using the
garbage collector's tools to find what objects are being retained and
why.

If your garbage collector is :term:`generational <generational garbage collection>`, it is possible that you are suffering from :term:`premature tenuring`, which can often be solved by tuning the collector or using a separate memory area for the library.


Should I write my own memory allocator to make my program fast?
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

If you are sure that your program is spending a large proportion of
its time in :term:`memory management`, and you know what you're doing,
then it is certainly possible to improve performance by writing a
:term:`suballocator`. On the other hand, advances in memory management
technology make it hard to keep up with software written by experts.
In general, improvements to memory management don't make as much
difference to performance as improvements to the program algorithms.

:ref:`Benjamin Zorn (1992) <ZORN92>` found that:

    In four of the programs investigated, the programmer felt
    compelled to avoid using the general-purpose storage allocator by
    writing type-specific allocation routines for the most common
    object types in the program. […] The general conclusion […] is
    that programmer optimizations in these programs were mostly
    unnecessary. […] simply using a different algorithm
    appears to improve the performance even more.

and concluded:

    programmers, instead of spending time writing domain-specific
    storage allocators, should consider using other publicly-available
    implementations of storage management algorithms if the one they
    are using performs poorly.


Why can't I just use local data on the stack or in global variables?
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Global, or static, data is fixed size; it cannot grow in response to
the size or complexity of the data set received by a program.
Stack-allocated data doesn't exist once you leave the function (or
program block) in which it was declared.

If your program's memory requirements are entirely predictable and
fixed at compile-time, or you can structure your program to rely on
stack data only while it exists, then you can entirely avoid using
heap allocation. Note that, with some compilers, use of large global
memory blocks can bloat the object file size.

It may often seem simpler to allocate a global block that seems
"probably large enough" for any plausible data set, but this
simplification will almost certainly cause trouble sooner or later.

.. seealso:: :term:`stack allocation`, :term:`heap allocation`, :term:`static allocation`.


Why should I worry about virtual memory? Can't I just use as much memory as I want?
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

While :term:`virtual memory` can greatly increase your capacity to
store data, there are three problems typically experienced with it:

* It does not provide an unlimited amount of memory. In particular,
  all memory that you actually allocate (as opposed to reserve) has to
  be stored somewhere. Usually you must have disk space available for
  all pages containing allocated memory. In a few systems, you can
  subtract the available physical memory from the disk space required.
  If the memory contains images of program or data files, then
  :term:`file mapping`, or assigning existing files to regions of the
  virtual address space, can help considerably.

* In most computers, there is a large difference in speed between main
  memory and disk; running a program with a :term:`working set` that
  does not fit in physical memory almost always results in
  unacceptable performance.

* An additional problem with using unnecessary quantities of memory is
  that poor :term:`locality of reference` can result in heavy paging.

.. seealso:: :term:`thrash`.
