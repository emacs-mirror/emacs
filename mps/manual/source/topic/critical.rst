.. Sources:

    `<https://info.ravenbrook.com/project/mps/master/design/critical-path.txt>`_

.. _topic-critical:

The critical path
=================

The critical path is a key concept in the design of the Memory Pool
System. Code on the critical path is usually executed more than any
other code in the process. A change of just one instruction on the
critical path can make as much as a 1% difference in overall run-time.
The MPS is designed to make the critical path as short and fast as
possible.


What makes the critical path critical
-------------------------------------

In order to determine which object can be recycled, the :term:`garbage
collector` has to frequently examine a very large number of pointers
in the program's objects. It does this by :term:`scanning <scan>`
memory, both allocated objects and :term:`roots <root>` (such as the
:term:`threads' <thread>` :term:`control stacks <control stack>`).

This means that the scanning functions must loop over pretty much
*every word in memory* sooner or later. The MPS takes great pains to
avoid scanning memory which does not need scanning, but to get good
performance, scanning must be highly optimised.

What's more, the scanning functions apply an operation called "fix" to
every :term:`reference` (or potential reference) that they find in the
objects in memory. Fixing also attempts to eliminate uninteresting
pointers as fast as possible, but it has to do some work on every
object that is being considered for recycling, and that can be a large
proportion of the objects in existence. The path through fixing must
also be highly optimised, especially in the early stages.


How the MPS avoids scanning and fixing
--------------------------------------

Firstly, the MPS must occasionally decide which objects to try to
recycle. It does this using various facts it knows about the objects,
primarily their age and whether they've survived previous attempts at
recycling them. It then :term:`condemns <condemned set>` a large
number of objects at once, and each of these objects must be
"preserved" by fixing references to them.

When the MPS condemns objects it chooses sets of objects in a small
set of "zones" in memory (preferably a single zone). The zone of an
object can be determined quickly from its address, without looking at
the object or any other data structure.

The MPS arranges that objects which it predicts will die at about the
same time are in the same zones.

The MPS allocates in "segments". Each segment is of the order of one
"tract" of memory (generally the same as the operating system
:term:`page` size, usually 4 KiB or 8 KiB) but may be larger if there
are large objects inside. The MPS maintains a "summary" of the zones
pointed to by all the pointers in a segment from previous scans.

So, once the MPS has decided what to condemn, it can quickly eliminate
all segments which definitely do not point to anything in those zones.
This avoids a large amount of scanning. It is an implementation of a
:term:`remembered set`, though it is unlike that in most other
collectors.

In addition, the :term:`fix` operation can quickly ignore pointers to
the wrong zones. This is called the "zone check" and is a
:term:`BIBOP` technique.

Even if a pointer passes the zone check, it may still not point to a
segment containing condemned objects. The next stage of the fix
operation is to look up the segment pointed to by the pointer and see
if it was condemned. This is a fast lookup.

After that, each pool class must decide whether the pointer is to a
condemned object and do something to preserve it. This code is still
critical. The MPS will have tried to condemn objects that are dead,
but those objects are still likely to be in segments with other
objects that must be preserved. The pool class fix method must quickly
distinguish between them.

Furthermore, many objects will be preserved at least once in their
lifetime, so even the code that preserves an object needs to be highly
efficient. (Programs in languages like :term:`ML` might not preserve
95% of their objects even once, but programs in many other languages
tend to preserve nearly all of theirs many times.)


Where to find the critical path
-------------------------------

Very briefly, the critical path consists of five stages:

1. The scanner, which iterates over pointers in objects. The MPS has
   several internal scanners, but the most important ones will be the
   :term:`scan methods <scan method>` in the client program's
   :term:`object formats <object format>`. See :ref:`topic-scanning`.

2. The first-stage fix, which filters out pointers inline in the
   scanner. This is implemented in the :c:func:`MPS_FIX1` macro.

3. The second-stage fix, which filters out pointers using general
   information about segments. This is implemented by the
   :c:func:`MPS_FIX2` acro, which calls ``_mps_fix`` in ``trace.c``.

4. The third-stage fix, which filters out pointers using pool-specific
   information. Implemented in pool class functions called ``AMCFix``,
   ``LOFix``, and so on in ``pool*.c``.

5. Preserving the object, which might entail:

   * :term:`marking` it to prevent it being recycled; and/or

   * :term:`copying <copying garbage collection>` it and updating the
     original pointer (or just updating the pointer, if the object has
     previously been copied); and/or

   * adding it to a queue of objects to be scanned later, if it
     contains pointers.

   Found in or near the pool class fix functions.


The format scanner
------------------

The critical path starts when an :term:`object format's <object
format>` :term:`scan method` is called. That is a call from the MPS to
a client function of type :c:type:`mps_fmt_scan_t` registered with one
of the format creation functions. (See :ref:`topic-format`.)

Here is an example of part of a format scanner for scanning contiguous
runs of pointers, from ``fmtdy.c``, the scanner for the `Open Dylan
<http://opendylan.org/>`_ runtime::

    static mps_res_t dylan_scan_contig(mps_ss_t mps_ss,
                                       mps_addr_t *base, mps_addr_t *limit)
    {
      mps_res_t res;
      mps_addr_t *p;        /* reference cursor */
      mps_addr_t r;         /* reference to be fixed */
    
      MPS_SCAN_BEGIN(mps_ss) {
              p = base;
        loop: if(p >= limit) goto out;
              r = *p++;
              if(((mps_word_t)r&3) != 0) /* pointers tagged with 0 */
                goto loop;             /* not a pointer */
              if(!MPS_FIX1(mps_ss, r)) goto loop;
              res = MPS_FIX2(mps_ss, p-1);
              if(res == MPS_RES_OK) goto loop;
              return res;
        out:  assert(p == limit);
      } MPS_SCAN_END(mps_ss);
    
      return MPS_RES_OK;
    }

(To help with understanding optimisation of this code, it's written in a
pseudo-assembler style, with one line roughly corresponding to each
instruction of an idealized intermediate code.)

The MPS C interface provides macros to try to help optimise this code.
The first parameter, of type :c:type:`mps_ss_t`, is a :term:`scan
state` and contains data that is used to eliminate uninteresting
pointers now, and record information which will be used to reduce
scanning in future by maintaining the remembered set.

The macros :c:func:`MPS_SCAN_BEGIN` and :c:func:`MPS_SCAN_END` load
key data from the scan state into local variables, and hopefully into
processor registers. This avoids aliasing values that we know won't
change when calls are made to :c:func:`MPS_FIX2` later, and so allows
the compiler to keep the scan loop small and avoid unnecessary memory
references.

This scanner knows that words not ending in ``0b00`` aren't pointers
to objects, so it eliminates them straight away. This is a :term:`tag`
chosen by the client program for its object representation.

Next, the pointer is tested using :c:func:`MPS_FIX1`. This performs
fast tests on the pointer without using any other memory. In
particular, it does the "zone check" described above. If a pointer
fails these tests, it isn't interesting and can be skipped. It is very
important to proceed to the next pointer as fast as possible in this
case.

Having passed these tests, we need to fix the pointer using other data
in memory, and possibly call the MPS to preserve the object. This is
what :c:func:`MPS_FIX2` does. The important distinction here is that
:c:func:`MPS_FIX2` can fail and return an error code, which must be
propagated without ado by returning from the scanner. Separating
:c:func:`MPS_FIX1` from :c:func:`MPS_FIX2` helps keep the error
handling code away from the tight loop with the zone check.

The macro/inline parts of the fix operation are referred to as "fix
stage 1" or "the first stage fix" in other documents and comments.

If these inline checks pass, ``_mps_fix2`` is called. If the MPS has
been built as a separate object file or library, this is where the
function call out of the scan loop happens. Since version 1.110 of the
MPS, we encourage clients to compile the MPS in the same translation
unit as their format code, so that the compiler can be intelligent
about inlining parts of ``_mps_fix2`` in the format scanner. See :ref:`guide-build`.


The second stage fix
--------------------

If a pointer gets past the first-stage fix filters, it is passed to
``_mps_fix2``, the "second stage fix". The second stage can filter out
yet more pointers using information about segments before it has to
consult the pool class.

The first test applied is the "tract test". The MPS looks up the tract
containing the address in the tract table, which is a simple linear
table indexed by the address shifted: a kind of flat :term:`page
table`.

Note that if the arena has been extended, the tract table becomes less
simple, and this test may involved looking in more than one table.
This will cause a considerable slow-down in garbage collection
scanning. This is the reason that it's important to give a good
estimate of the amount of address space you will ever occupy with
objects when you initialize the arena.

The pointer might not even be in the arena (and so not in any tract).
The first stage fix doesn't guarantee it. So we eliminate any pointers
not in the arena at this stage.

If the pointer is in an allocated tract, then the table also contains
a cache of the "white set", the set of garbage collection traces for
which the tract is "interesting". If a tract isn't interesting, then
we know that it contains no condemned objects, and we can filter out
the pointer.

If the tract is interesting them it's part of a segment containing
objects that have been condemned. The MPM can't know anything about
the internal layout of the segment, so at this point we dispatch to
the third stage fix.

This dispatch is slightly subtle. We have a cache of the function to
dispatch to in the :term:`scan state`, which has recently been looked
at and is with luck still in the processor cache. The reason there is
a dispatch at all is to allow for a fast changeover to emergency
garbage collection, or overriding of garbage collection with extra
operations. Those are beyond the scope of this document. Normally,
``ss->fix`` points at ``PoolFix``, and we rely somewhat on modern
processor `branch target prediction
<https://en.wikipedia.org/wiki/Branch_target_predictor>`_. ``PoolFix``
is passed the pool, which is fetched from the tract table entry, and
that should be in the cache.

``PoolFix`` itself dispatches to the pool class. Normally, a dispatch
to a pool class would indirect through the pool class object. That
would be a double indirection from the tract, so instead we have a
cache of the pool's fix method in the pool object. This also allows a
pool class to vary its fix method per pool instance, a fact that is
exploited to optimize fixing in :ref:`pool-amc`, depending on what
kind of object format it is managing.


The third stage fix in the pool class
-------------------------------------

The final stage of fixing is entirely dependent on the :term:`pool
class`. The Memory Pool Manager can't, in general, know how the
objects within a pool are arranged, so this is pool class specific
code.

Furthermore, the pool class must make decisions based on the
:term:`reference rank <rank>` of the pointer. If a pointer is
:term:`ambiguous <ambiguous reference>` then it can't be changed, so
even a :term:`copying <copying garbage collection>` pool class can't
move an object. On the other hand, if the pointer is :term:`weak <weak
reference (1)>` then the pool fix method shouldn't preserve the object
at all, even if it's condemned.

The exact details of the logic that the pool fix must implement in
order to co-operate with the MPM and other pools are beyond the scope
of this document, which is about the critical path. Since it is on the
critical path, it's important that whatever the pool fix does is
simple and fast and returns to scanning as soon as possible.

The first step, though, is to further filter out pointers which aren't
to objects, if that's its policy. Then, it may preserve the object,
according to its policy, and possibly ensure that the object gets
scanned at some point in the future, if it contains more pointers.

If the object is moved to preserve it (for instance, if the pool class
implements copying garbage collection), or was already moved when
fixing a previous reference to it, the reference being fixed must be
updated (this is the origin of the term "fix").

As a simple example, ``LOFix`` is the pool fix method for the
:ref:`pool-lo` pool class. It implements a :term:`marking` garbage
collector, and does not have to worry about scanning preserved objects
because it is used to store objects that don't contain pointers. (It
is used in compiler run-time systems to store binary data such as
character strings, thus avoiding any scanning, decoding, or remembered
set overhead for them.)

``LOFix`` filters any ambiguous pointers that aren't aligned, since
they can't point to objects it allocated. Otherwise it subtracts the
segment base address and shifts the result to get an index into a mark
bit table. If the object wasn't marked and the pointer is weak, then
it sets the pointer to zero, since the object is about to be recycled.
Otherwise, the mark bit is set, which preserves the object from
recycling when ``LOReclaim`` is called later on. ``LOFix`` illustrates
about the minimum and most efficient thing a pool fix method can do.


Other considerations
--------------------

So far this document has described the ways in which the garbage
collector is designed around optimising the critical path. There are a
few other things that the MPS does that are important.

Firstly, inlining is very important. The first stage fix is inlined
into the format scanner by being implemented in macros in ``mps.h``.
And to get even better inlining, we recommend that the whole MPS is
compiled in a single translation unit with the client format and that
strong global optimisation is applied. See :ref:`guide-build`.

Secondly, we are very careful with code annotations on the critical
path. Assertions, statistics, and telemetry are all disabled on the
critical path in :term:`hot` (production) builds. (In fact, it's
because the critical path is critical that we can afford to leave
annotations switched on elsewhere.)

Last, but by no means least, we pay a lot of brainpower and
measurement to the critical path, and are very very careful about
changing it. Code review around the critical path is especially
vigilant.

And we write long documents about it.
