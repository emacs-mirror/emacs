.. _mmref-alloc:

Allocation techniques
*********************

Memory allocation is the process of assigning blocks of memory on
request. Typically the :term:`allocator` receives memory from the
operating system in a small number of large blocks that it must divide
up to satisfy the requests for smaller blocks. It must also make any
returned blocks available for reuse. There are many common ways to
perform this, with different strengths and weaknesses. A few are
described briefly below.

* :ref:`First fit <mmref-alloc-first-fit>`
* :ref:`Buddy system <mmref-alloc-buddy>`
* :ref:`Suballocators <mmref-alloc-suballocator>`

These techniques can often be used in combination.


.. _mmref-alloc-first-fit:

First fit
---------

In the :term:`first fit` algorithm, the allocator keeps a list of free
blocks (known as the :term:`free list`) and, on receiving a request
for memory, scans along the list for the first block that is large
enough to satisfy the request. If the chosen block is significantly
larger than that requested, then it is usually split, and the
remainder added to the list as another free block.

The first fit algorithm performs reasonably well, as it ensures that
allocations are quick. When recycling free blocks, there is a choice
as to where to add the blocks to the free list---effectively in what
order the free list is kept:

**Memory location (address)**

    This is not fast for allocation or recycling, but supports
    efficient merging of adjacent free blocks (known as
    :term:`coalescence <coalesce>`). According to :ref:`Wilson et al.
    (1995) <WIL95>`, this ordering reduces :term:`fragmentation`. It
    can also improve :term:`locality of reference`.

**Increasing size**

     This is equivalent to the :term:`best fit` algorithm, in that the
     free block with the "tightest fit" is always chosen. The fit is
     usually sufficiently tight that the remainder of the block is
     unusably small.

**Decreasing size**

    This is equivalent to the :term:`worst fit` algorithm. The first
    block on the free list will always be large enough, if a large
    enough block is available. This approach encourages
    :term:`external fragmentation`, but allocation is very fast.

**Increasing time since last use**

    This is very fast at adding new free blocks, because they are
    added to the beginning of the list. It encourages good
    :term:`locality of reference` (where blocks used together are not
    spread throughout memory), but can lead to bad external
    fragmentation.

A variation of first fit, known as :term:`next fit`, continues each
search for a suitable block where the previous one left off, by using
a roving pointer into the free block chain. This is not usually
combined with increasing or decreasing size ordering because it would
eliminate their advantages.


.. _mmref-alloc-buddy:

Buddy system
------------

In a :term:`buddy system`, the allocator will only allocate blocks of
certain sizes, and has many free lists, one for each permitted size.
The permitted sizes are usually either powers of two, or form a
Fibonacci sequence (see below for example), such that any block except
the smallest can be divided into two smaller blocks of permitted
sizes.

When the allocator receives a request for memory, it rounds the
requested size up to a permitted size, and returns the first block
from that size's free list. If the free list for that size is empty,
the allocator splits a block from a larger size and returns one of the
pieces, adding the other to the appropriate free list.

When blocks are recycled, there may be some attempt to merge adjacent
blocks into ones of a larger permitted size (:term:`coalescence
<coalesce>`). To make this easier, the free lists may be stored in
order of address. The main advantage of the buddy system is that
coalescence is cheap because the "buddy" of any free block can be
calculated from its address.

.. figure:: ../diagrams/buddy1.svg
    :align: center
    :alt: Diagram: A binary buddy heap before allocation.

    A binary buddy heap before allocation

.. figure:: ../diagrams/buddy2.svg
    :align: center
    :alt: Diagram: A binary buddy heap after allocating a 8 kB block.

    A binary buddy heap after allocating a 8 kB block.

.. figure:: ../diagrams/buddy3.svg
    :align: center
    :alt: Diagram: A binary buddy heap after allocating a 10 kB block; note the 6 kB wasted because of rounding up.

    A binary buddy heap after allocating a 10 kB block; note the 6 kB wasted because of rounding up.

For example, an allocator in a binary buddy system might have sizes of
16, 32, 64, …, 64 kB. It might start off with a single block of 64 kB.
If the application requests a block of 8 kB, the allocator would check
its 8 kB free list and find no free blocks of that size. It would then
split the 64 kB block into two block of 32 kB, split one of them into
two blocks of 16 kB, and split one of them into two blocks of 8 kB.
The allocator would then return one of the 8 kB blocks to the
application and keep the remaining three blocks of 8 kB, 16 kB, and 32
kB on the appropriate free lists. If the application then requested a
block of 10 kB, the allocator would round this request up to 16 kB,
and return the 16 kB block from its free list, wasting 6 kB in the
process.

A Fibonacci buddy system might use block sizes 16, 32, 48, 80, 128,
208, … bytes, such that each size is the sum of the two preceding
sizes. When splitting a block from one free list, the two parts get
added to the two preceding free lists.

A buddy system can work very well or very badly, depending on how the
chosen sizes interact with typical requests for memory and what the
pattern of returned blocks is. The rounding typically leads to a
significant amount of wasted memory, which is called :term:`internal
fragmentation`. This can be reduced by making the permitted block
sizes closer together.


.. _mmref-alloc-suballocator:
  
Suballocators
-------------

There are many examples of application programs that include
additional memory management code called a :term:`suballocator`. A
suballocator obtains large blocks of memory from the system memory
manager and allocates the memory to the application in smaller pieces.
Suballocators are usually written for one of the following reasons:

* To avoid general inefficiency in the system memory manager;

* To take advantage of special knowledge of the application's memory
  requirements that cannot be expressed to the system memory manager;

* To provide memory management services that the system memory manager
  does not supply.

In general, suballocators are less efficient than having a single
memory manager that is well-written and has a flexible interface. It
is also harder to avoid memory management bugs if the memory manager
is composed of several layers, and if each application has its own
variation of suballocator.

Many applications have one or two sizes of block that form the vast
majority of their allocations. One of the most common uses of a
suballocator is to supply the application with objects of one size.
This greatly reduces the problem of :term:`external fragmentation`.
Such a suballocator can have a very simple allocation policy.

There are dangers involved in making use of special knowledge of the
application's memory requirements. If those requirements change, then
the performance of the suballocator is likely to be much worse than
that of a general allocator. It is often better to have a memory
manager that can respond dynamically to changing requirements.
