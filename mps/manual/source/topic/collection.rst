.. sources:

    `<https://info.ravenbrook.com/project/mps/master/design/message-gc/>`_
    `<https://info.ravenbrook.com/project/mps/doc/2002-06-18/obsolete-mminfo/mminfo/strategy/lisp-machine/>`_


.. index::
   single: collection
   single: garbage collection

.. _topic-collection:

Garbage collection
==================

The :term:`arena` contains a :term:`garbage collector` that
coordinates the collection of garbage in all of its
:term:`automatically managed <automatic memory management>`
:term:`pools`. The collector efficiently traces references between
:term:`roots` and pools, and between objects in different pools. It is
capable of collecting many automatically managed pools simultaneously.


.. index::
   single: chain; generation
   single: generation chain

Generation chains
-----------------

A :term:`generation chain` describes a sequence of :term:`generations`
used by a set of :term:`automatically managed <automatic memory
management>` :term:`pools`.

A generation is a set of blocks that are managed together by the
:term:`garbage collector`: they are :term:`condemned <condemned set>`
together, and in :term:`moving <moving memory manager>` pools they are
moved together. If two pools allocate blocks that are expected to live
and die together, then it is efficient for them to share a chain.

Typically blocks are allocated in the first generation in the chain,
the :term:`nursery generation` (though you can change this using the
:c:macro:`MPS_KEY_GEN` keyword argument to :c:func:`mps_pool_create`),
and each time a block survives one collection then it is
:term:`promoted <promotion>` to the next generation. Thus a generation
contains a set of blocks of similar ages.

By default, all pools in an arena share the same generation chain
("the arena's default generation chain"), but if this doesn't meet
your requirements, then when creating an automatically managed pool,
you can choose which chain it should use by passing the
:c:macro:`MPS_KEY_CHAIN` keyword argument to
:c:func:`mps_pool_create`.

Create a generation chain by preparing an array of
:c:type:`mps_gen_param_s` structures giving the *capacity* (in
kilobytes) and *predicted mortality* (between 0 and 1) of each
generation, and passing them to :c:func:`mps_chain_create`.

When the size of the generation exceeds the capacity, the MPS will be
prepared to start collecting the generation. See
:ref:`topic-collection-schedule` below.

For example::

    mps_gen_param_s gen_params[] = {
        { 1024, 0.8 },
        { 2048, 0.4 },
    };

    mps_chain_t chain;
    mps_res_t res;
    res = mps_chain_create(&chain, arena,
                           sizeof(gen_params) / sizeof(gen_params[0]),
                           gen_params);
    if (res != MPS_RES_OK) error("Couldn't create chain");


.. c:type:: mps_chain_t

    The type of :term:`generation chains`. A generation chain
    describes the structure of :term:`generations` in a set of
    :term:`pools`.


.. c:type:: mps_gen_param_s

    The type of the structure used to specify a :term:`generation` in
    a :term:`generation chain`. ::

        typedef struct mps_gen_param_s {
            size_t mps_capacity;
            double mps_mortality;
        } mps_gen_param_s;

    ``mps_capacity`` is the capacity of the generation, in
    :term:`kilobytes`. When the size of the generation
    exceeds this, the MPS will be prepared to start collecting it.

    ``mps_mortality`` is the predicted mortality of the generation:
    the proportion (between 0 and 1) of blocks in the generation that
    are expected to be :term:`dead` when the generation is collected.

    These numbers are hints to the MPS that it may use to make
    decisions about when and what to collect: nothing will go wrong
    (other than suboptimal performance) if you make poor
    choices. See :ref:`topic-collection-schedule`.


.. c:function:: mps_res_t mps_chain_create(mps_chain_t *chain_o, mps_arena_t arena, size_t gen_count, mps_gen_param_s *gen_params)

    Create a :term:`generation chain`.

    ``chain_o`` points to a location that will hold a pointer to the
    new generation chain.

    ``arena`` is the arena to which the generation chain will belong.

    ``gen_count`` is the number of :term:`generations` in
    the chain.

    ``gen_params`` points to an array describing the generations.

    Returns :c:macro:`MPS_RES_OK` if the generation chain is created
    successfully, or another :term:`result code` if it fails.

    The generation chain persists until it is destroyed by calling
    :c:func:`mps_chain_destroy`.


.. c:function:: void mps_chain_destroy(mps_chain_t chain)

    Destroy a :term:`generation chain`.

    ``chain`` is the generation chain.

    It is an error to destroy a generation chain if there exists a
    :term:`pool` using the chain. The pool must be destroyed first.


.. index::
   single: collection; scheduling
   single: garbage collection; scheduling

.. _topic-collection-schedule:

Scheduling of collections
-------------------------

.. note::

    It's likely that the algorithm the MPS uses to schedule its
    collections will change in future releases. There's a lot of room
    for improvement here.

The :dfn:`new size` of a generation is the total size of the newly
allocated (in generation 0) or newly promoted (in other generations)
blocks in that generation. These are the blocks that have not been
:term:`condemned <condemned set>` since they were allocated or
promoted into this generation. In pools like :ref:`pool-amc` where the
survivors get promoted to the next generation in the chain, the *new
size* of each generation (other than the topmost) is the same as its
total size, but in pools like :ref:`pool-ams` where survivors do not
get promoted, the two sizes can be different.

The first generation in a pool's chain is the :term:`nursery space`.
When the nursery's *new size* exceeds its capacity, the MPS considers
collecting the pool. (How long it takes to get around to it depends on
which other collections on other pools are in progress.)

.. note::

    You can affect the decision as to when to collect the nursery
    space by using the :ref:`ramp allocation pattern
    <topic-pattern-ramp>`.

If the MPS decides to collect a pool at all, all generations are
collected below the first generation whose *new size* is less than its
capacity.

In pools such as :ref:`pool-amc`, blocks in generation *g* that
survive collection get promoted to generation *g*\+1. If the last
generation in the chain is collected, the survivors are promoted into
an :term:`arena`\-wide "top" generation.

The predicted mortality is used to estimate how long the collection
will take, and this is used in turn to decide how much work the
collector will do each time it has an opportunity to do some work. The constraints here are:

#. The :term:`client program` might have specified a limit on the
   acceptable length of the pause if the work is being done inside
   :c:func:`mps_arena_step`.

#. The collector needs to keep up with the :term:`client program`:
   that is, it has to collect garbage at least as fast as the client
   is producing it, otherwise the amount of garbage will grow without
   bound.

With perfect prediction, the collector's work should be smoothly
distributed, with a small maximum pause time. Getting the predicted
mortality wrong leads to "lumpy" distribution of collection work with
a longer maximum pause time. If the predicted mortality is too high,
the collector will start out by taking small time slices and then find
that it has to catch up later by taking larger time slices. If the
predicted mortality is too low, the collector will take larger time
slices up front and then find that it is idle later on.


.. index::
   single: garbage collection; start message
   single: message; garbage collection start

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


.. index::
   pair: garbage collection; message

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
      approximate size of the set of blocks that were in collected
      :term:`pools`, but were not condemned in the garbage
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

    The "live size" property is the total size of the set of blocks
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
    set of blocks that were in collected :term:`pools`, but
    were not in the :term:`condemned set` in the :term:`garbage
    collection` that generated the message.

    .. seealso::

        :ref:`topic-message`.
