.. _glossary-w:

=============================
Memory Management Glossary: W
=============================

.. include:: alphabet.txt

.. glossary::

    weak-key hash table

        A hash table which has :term:`weak references (1)` to its
        keys. If the key dies, the value for that key is automatically
        deleted from the table too. It can be used to store extra
        information about objects without keeping them alive.

        .. similar:: :term:`doubly weak hash table`, :term:`weak-value hash table`.

        .. mps:specific:: See :ref:`pool-awl`.

    weak-value hash table

        A hash table which has :term:`weak references (1)` to its
        value. If the value dies, any keys that refer to that value
        are automatically deleted from the table too. It can be used
        to index a set of objects without keeping them alive.

        .. similar:: :term:`doubly weak hash table`, :term:`weak-key hash table`.

        .. mps:specific:: See :ref:`pool-awl`.

    weak hash table

        A :term:`weak-key <weak-key hash table>` or :term:`weak-value
        hash table` (usually the former).

    weak reference (1)

        In :term:`tracing garbage collection`, a weak reference is a
        :term:`reference` that does not keep the :term:`object` it
        refers to :term:`alive <live>`.

        A weak reference does not keep the referent alive, but it will
        continue to refer to the object as long as it remains
        otherwise alive. When only weak references to the object
        remain, the weak references can be deleted ("splatted" or
        "cleared") and the object :term:`reclaimed`.

        :term:`Java` offers three kinds of weak references, called
        :term:`soft references`, :term:`weak references (2)`, and
        :term:`phantom references`, in order of increasing weakness.

        .. opposite:: :term:`strong reference`.

        .. seealso:: :term:`weak root`.

    weak reference (2)

        In :term:`Java` terminology, *weak reference* is used to mean
        a :term:`reference` encapsulated in a :term:`reference object`
        of class ``WeakReference``.

        Weak references form one of three kinds of :term:`weak
        reference (1)` in Java. They are handy for associating extra
        data with objects when you cannot store it in the objects
        themselves.

        .. seealso:: :term:`weakly reachable`.

        .. link::

            `Class java.lang.ref.WeakReference <http://download.java.net/jdk8/docs/api/java/lang/ref/WeakReference.html>`_, `Reference Objects and Garbage Collection <http://pawlan.com/monica/articles/refobjs/>`_.

    weak root

        A weak root is a :term:`root`, such that all
        :term:`references` in it are :term:`weak references (1)`; that
        is, they do not affect the :term:`liveness <live>` of the
        :term:`objects` referred to.

        .. opposite:: :term:`strong root`.

        .. mps:specific::

            A weak root has :term:`rank` :c:func:`mps_rank_weak`.

    weak tri-color invariant
    weak tri-colour invariant
    weak tricolor invariant
    weak tricolour invariant

        The weak :term:`tri-color invariant` is the property of a
        :term:`reference` :term:`graph` that all :term:`white`
        :term:`nodes` pointed to by a :term:`black` node are
        also :term:`reachable` from some :term:`gray` node through a
        chain of white nodes.

        By preserving this property throughout :term:`tri-color
        marking`, a :term:`tracing <trace>` algorithm can ensure that
        the :term:`collector (2)` will not miss reachable objects,
        even if the :term:`mutator` manipulates the graph during the
        collection. Mutator actions might need to change the
        :term:`color` of the nodes affected in order to preserve the
        invariant.

        Algorithms using this invariant are
        :term:`snapshot-at-the-beginning <snapshot at the beginning>`
        algorithms.

        .. seealso:: :term:`barrier (1)`, :term:`strong tri-color invariant`.

        .. bibref:: :ref:`Johnstone (1997) <JOHNSTONE97>`, :ref:`Pirinen (1998) <PIRINEN98>`.

    weakly reachable

        In :term:`Java`, an object is *weakly reachable* if it is
        neither :term:`strongly <strongly reachable>` nor
        :term:`softly reachable` and there is a path from the
        :term:`roots` to it that contains at least one
        :term:`weak reference (2)` but no :term:`phantom references`.

        When the Java :term:`collector (1)` determines that an object
        is weakly reachable, it clears all the weak references
        involved, and declares the object :term:`finalizable
        <finalization>`. (Operationally, finalization works as if it
        was implemented by a class of "final references" that stand
        between weak and phantom references.) Also, the
        :term:`reference objects` containing the
        weak references are enqueued, if they were registered with a
        queue.

        .. seealso:: :term:`reachability <reachable>`, :term:`phantom reachable`.

        .. link::

            `Class java.lang.ref.WeakReference <http://download.java.net/jdk8/docs/api/java/lang/ref/WeakReference.html>`_, `Reference Objects and Garbage Collection <http://pawlan.com/monica/articles/refobjs/>`_.

    weighted buddies

        A :term:`buddy system` :term:`allocation mechanism` using two
        series of size classes: :term:`binary buddies` (2, 4, 8, …)
        and three-times-power-of-two (3, 6, 12, …). A block that is
        in the latter series may be :term:`split` in two different
        ways. Thus a block of size 12 may be split into two blocks of
        size 6 or one block of size 4 and one block of size 8. The
        same applies for :term:`coalescing <coalesce>`. This gives
        this system more flexibility than a regular buddy system.

        .. seealso:: :term:`buddy system`, :term:`allocation mechanism`, :term:`binary buddies`.

        .. bibref:: :ref:`Wilson et al. (1995) <WIL95>`.

    weighted reference counting

        A technique for :term:`reference counting` which is in common
        use for :term:`distributed garbage collection` because of the
        low level of inter-process communication it requires.

        Inter-process :term:`references` to :term:`objects` are
        counted, but instead of simply counting the number of
        references, each reference is given a weight. When an object
        is created, the initial pointer to it is assigned a weight,
        which is usually a power of 2 for easy division. The object
        records the sum of all the weights of all of its references.
        Whenever a reference is copied, its weight is divided equally
        between the new and original copies. Since this operation
        preserves the weighted reference sum, there is no need for
        communication with the object at this time. When a reference
        is deleted, the weighted reference sum is decremented by the
        weight of the reference. This is communicated to the object by
        sending it a message. When the object detects that the
        weighted reference sum has dropped to zero, it may be
        :term:`reclaimed`. The algorithm is tolerant of communication
        protocols which don't guarantee order of arrival of deletion
        messages.

    white

        In a :term:`tri-color marking` scheme, white :term:`objects`
        are objects that were :term:`condemned <condemned set>` at the
        beginning of the :term:`collection cycle` and have not been
        shown to be :term:`reachable`. When :term:`tracing <trace>` is
        complete, white objects will be subject to :term:`reclamation
        <reclaim>`.

        .. opposite:: :term:`gray`, :term:`black`.

    word

        .. aka:: *machine word*.

        Almost all processor architectures have a characteristic data
        size that is handled most efficiently. This is known as the
        *word size*, and data of that size are known as *words*. The
        word size is usually a power of two multiple of :term:`bytes
        (2)`.

        Often the platform's word size is used to characterize the
        architecture by quoting the number of bits in it. For example,
        a 32-bit platform has a word size of four bytes and a 64-bit
        platform has eight-byte words (assuming 8-bit bytes).
        Typically, :term:`pointers` are the size of a word,
        and traditionally this determined the word size. Nowadays,
        word size is usually driven by the need for more accuracy and
        range in mathematical calculations.

        .. historical::

            In the past, the convenience of dealing with powers of two was not as significant, and word sizes such as 36- or 72-bits were not unknown.

        .. seealso:: :term:`alignment`, :term:`grain`.

    working set

        The working set of a program or system is that :term:`memory
        (2)` or set of :term:`addresses` which it will use
        in the near future.

        This term is generally used when discussing :term:`miss rates`
        at some :term:`storage level`; the time scale of "near future"
        depends upon the cost of a :term:`miss`. The working set
        should fit in the storage level; otherwise the system may
        :term:`thrash`.

        .. seealso:: :term:`resident set`, :term:`cache (2)`, :term:`storage hierarchy`.

        .. bibref:: :ref:`Denning & Schwartz (1972) <DS72>`.

    worst fit

        The :term:`allocation policy` that always allocates from the
        largest :term:`free block`. Commonly implemented using a
        size-ordered :term:`free block chain` (largest first).

        In practice, this tends to work quite badly because it
        eliminates all large blocks, so large requests cannot be met.

        .. seealso:: :term:`allocation policy`, :term:`first fit`, :term:`best fit`.

        .. bibref:: :ref:`Wilson et al. (1995) <WIL95>`.

    wrapped

        A value is wrapped if it is encoded with type information.

        .. opposite:: :term:`unwrapped`.

        .. seealso:: :term:`wrapper`, :term:`boxed`, :term:`tag`.

        .. bibref:: :ref:`Gudeman (1993) <GUDEMAN93>`.

    wrapper

        A wrapper is that part of a :term:`wrapped` representation
        that is copied when the value is passed by value.

        The wrapper does not include parts of the representation that
        are accessed indirectly, and are not copied when the value is
        passed.

        For instance, a :term:`Lisp` implementation might use the top
        two bits of a value representation as a :term:`tag` to
        distinguish between integers and :term:`cons (1)` cells,
        setting these bits to 01 for a :term:`pointer` to a cons cell
        and 11 for an integer. Then the wrapped value of the number 4
        would have binary representation 11000…00100, and the
        wrapper for this number is the whole of this wrapped value.
        The pointer to a cons cell stored at location 4 would have
        binary representation 01000…00100. The wrapped value of the
        cons cell is the combination of this pointer and the cons cell
        in memory itself. The wrapper of the cons cell is just the
        pointer; when the cons cell is passed as a function argument,
        just the pointer is passed.

        .. seealso:: :term:`wrapped`, :term:`boxed`.

        .. bibref:: :ref:`Gudeman (1993) <GUDEMAN93>`.

    write barrier

        A write :term:`barrier (1)` is a block on writing to certain
        :term:`memory (2)` :term:`locations <memory location>` by
        certain threads or processes.

        .. relevance::

            Write barriers are used for :term:`incremental
            <incremental garbage collection>` or :term:`concurrent
            <parallel garbage collection>` :term:`garbage collection`.
            They are also used to maintain :term:`remembered sets` for
            :term:`generational <generational garbage collection>`
            :term:`collectors (1) <garbage collector>`.

        .. seealso:: :term:`read barrier`.

    write fault

        An exception which occurs when writing to an address in
        :term:`virtual memory`.

        This is probably either a :term:`page fault`, an
        :term:`invalid page fault` or a :term:`protection fault`.

        .. similar:: :term:`segmentation violation`.

        .. seealso:: :term:`read fault`.
