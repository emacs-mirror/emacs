.. _topic-collection:

Garbage collection
==================


The graph of managed references
-------------------------------

The MPS is a moving garbage collector: it supports preserve-by-copying pools, whose objects are 'mobile'. Whenever the MPS moves an object, it will ensure that all managed references are updated to point to the new location -- and this happens instantaneously as far as the client sees it.

The client should assume that, between any pair of instructions, the MPS may 'shake' this graph, moving all the mobile objects, and updating all the managed references.

Any parts of the graph that are no longer connected (no longer reachable from declared roots) may be collected, and the memory that those objects occupied may be unmapped, or re-used for different objects.

The client usually takes care to ensure that all the references it holds are managed. To be managed, the reference must be in a declared root (such as a scanned stack or a global variable), or in a formatted object that is reachable from a root.

It is okay for a careful client to hold unmanaged references, but:

they'd better not be to a mobile object! Remember, mobile objects could move at any time, and unmanaged references will be left 'dangling'.
they'd better not be the only reference to an object, or that object might get collected, again leaving a dangling reference.


Interface
---------

.. c:function:: mps_res_t mps_chain_create(mps_chain_t *chain_o, mps_arena_t arena, size_t gen_count, mps_gen_param_s *gen_params)

    Create a :term:`generation chain`.

    ``chain_o`` points to a location that will hold a pointer to the
    new generation chain.

    ``arena`` is the arena to which the generation chain will belong.

    ``gen_count`` is the number of :term:`generations <generation>` in
    the chain.

    ``gen_params`` points to an array describing the generations.

    Returns :c:macro:`MPS_RES_OK` if the generation chain is created
    successfully, or another :term:`result code` if it fails.

    The generation chain persists until it is destroyed by calling
    :c:func:`mps_chain_destroy`.


.. c:function:: void mps_chain_destroy(mps_chain_t chain)

    Destroy a :term:`generation chain`.

    ``chain`` is the generation chain.


.. c:type:: mps_chain_t

    The type of :term:`generation chains <generation chain>`. A
    generation chain describes the structure of :term:`generations
    <generation>` in a :term:`pool`.


.. c:type:: mps_gen_param_s

    The type of the structure used to specify a :term:`generation` in
    a :term:`generation chain`. ::

        typedef struct mps_gen_param_s {
            size_t mps_capacity;
            double mps_mortality;
        } mps_gen_param_s;

    ``mps_capacity`` is the capacity of the generation, in
    :term:`kilobytes <kilobyte>`.

    ``mps_mortality`` is the predicted mortality of the generation:
    the proportion (between 0 and 1) of blocks in the generation that
    are expected to be :term:`dead` when the generation is collected.

    These numbers are hints to the MPS that it may use to make
    decisions about when and what to collect: nothing will go wrong
    (other than suboptimal performance) if you make poor
    choices. Making good choices for the capacity and mortality of
    each generation is discussed in the guide :ref:`guide-perf`.


Garbage collection start messages
---------------------------------

.. c:function:: mps_message_type_t mps_message_type_gc_start(void)

    Return the :term:`message type` of garbage collection start
    messages.

    Garbage collection start messages contain information about why
    the :term:`garbage collection` started.

    The access method specific to a :term:`message` of this message
    type is:

    * :c:func:`mps_message_gc_start_why` returns a string that
      describes why the garbage collection started.

    .. seealso::

        :ref:`topic-message`.


.. c:function:: const char *mps_message_gc_start_why(mps_arena_t arena, mps_message_t message)

    Return a string that describes why the :term:`garbage collection`
    that posted a :term:`message` started.

    ``arena`` is the arena which posted the message.

    ``message`` is a message retrieved by :c:func:`mps_message_get` and
    not yet discarded.  It must be a garbage collection message: see
    :c:func:`mps_message_type_gc`.

    Returns a pointer to a string that is describes (in English) why
    this collection started. The contents of the string must not be
    modified by the client. The string and the pointer are valid until
    the message is discarded with :c:func:`mps_message_discard`.

    .. seealso::

        :ref:`topic-message`.


Garbage collection messages
---------------------------

.. c:function:: mps_message_type_t mps_message_type_gc(void)

    Return the :term:`message type` of garbage collection statistic
    messages.

    Garbage collection statistic messages are used by the MPS to give
    the :term:`client program` information about a :term:`garbage
    collection` that has taken place. Such information may be useful in
    analysing the client program's memory usage over time.

    The access methods specific to a message of this type are:

    * :c:func:`mps_message_gc_live_size` returns the total size of the
      :term:`condemned set` that survived the garbage collection that
      generated the message;

    * :c:func:`mps_message_gc_condemned_size` returns the approximate
      size of :term:`condemned set` in the garbage collection that
      generated the message;

    * :c:func:`mps_message_gc_not_condemned_size` returns the
      approximate size of the set of objects that were in collected
      :term:`pools <pool>`, but were not condemned in the garbage
      collection that generated the message.

    .. seealso::

        :ref:`topic-message`.


.. c:function:: size_t mps_message_gc_condemned_size(mps_arena_t arena, mps_message_t message)

    Return the "condemned size" property of a :term:`message`.

    ``arena`` is the arena which posted the message.

    ``message`` is a message retrieved by :c:func:`mps_message_get` and
    not yet discarded.  It must be a garbage collection message: see
    :c:func:`mps_message_type_gc`.

    The "condemned size" property is the approximate :term:`size` of
    the :term:`condemned set` in the :term:`garbage collection` that
    generated the message.

    .. seealso::

        :ref:`topic-message`.


.. c:function:: size_t mps_message_gc_live_size(mps_arena_t arena, mps_message_t message)

    Return the "live size" property of a :term:`message`.

    ``arena`` is the arena which posted the message.

    ``message`` is a message retrieved by :c:func:`mps_message_get` and
    not yet discarded.  It must be a garbage collection message: see
    :c:func:`mps_message_type_gc`.

    The "live size" property is the total size of the set of objects
    that survived the :term:`garbage collection` that generated the
    message.

    .. seealso::

        :ref:`topic-message`.


.. c:function:: size_t mps_message_gc_not_condemned_size(mps_arena_t arena, mps_message_t message)

    Return the "not condemned size" property of a :term:`message`.

    ``arena`` is the arena which posted the message.

    ``message`` is a message retrieved by :c:func:`mps_message_get` and
    not yet discarded.  It must be a garbage collection message: see
    :c:func:`mps_message_type_gc`.

    The "not condemned size" property is the approximate size of the
    set of objects that were in collected :term:`pools <pool>`, but
    were not in the :term:`condemned set` in the :term:`garbage
    collection` that generated the message.

    .. seealso::

        :ref:`topic-message`.
