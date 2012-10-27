.. sources:

    `<https://info.ravenbrook.com/project/mps/master/design/message-gc/>`_

.. _topic-collection:

Garbage collection
==================


Generation chains
-----------------

Each :term:`automatically managed <automatic memory management>`
:term:`pool` has an associated :term:`generation chain` which
describes the structure of the :term:`generations <generation>` in
that pool. You create a generation chain by preparing an array of
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
    :term:`kilobytes <kilobyte>`. When the size of the generation
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


.. _topic-collection-schedule:

Scheduling of collections
-------------------------

The first generation in a pool's chain is the :term:`nursery
generation`. When the nursery's size exceeds its capacity, the MPS
considers collecting the pool. (Whether it actually does so or not
depends on which other collections on other pools are in progress.)

.. note::

    You can affect the decision as to when to collect the nursery
    generation by using the :ref:`ramp allocation pattern
    <topic-pattern-ramp>`.

If the MPS decides to collect a pool at all, all generations are
collected below the first generation whose size is less than its
capacity.

For example, suppose that we have a pool with the following generation
structure:

+------------+--------------+----------------------+----------------+
| Generation | Current size | Capacity | Mortality | Predicted size |
+============+==============+==========+===========+================+
|          0 |          110 |      100 |       0.8 |              0 |
+------------+--------------+----------+-----------+----------------+
|          1 |          210 |      200 |       0.4 |             22 |
+------------+--------------+----------+-----------+----------------+
|          2 |          200 |      300 |       0.2 |            326 |
+------------+--------------+----------+-----------+----------------+

The nursery and generation 1 both have size that exceeds their
capacity, so these generations will be collected. Generation 2 will
not be collected this time. The last two columns give the predicted
sizes of each generation after the collection: the survivors from the
nursery will be promoted to generation 1 and the survivors from
generation 1 will be promoted to generation 2.

When the last generation in the chain is collected, the survivors are
promoted into an :term:`arena`\-wide "top" generation.

The predicted mortality is used to estimate how long the collection
will take, and this is used in turn to decide how much work the
collector will do each time it has an opportunity to do some work. The constraints here are:

1. The :term:`client program` might have specified a limit on the
   acceptable length of the pause if the work is being done inside
   :c:func:`mps_arena_step`.

2. The collector needs to keep up with the :term:`client program`:
   that is, it has to collect memory least as fast as the client
   program is allocating it, otherwise the amount of allocated memory
   will grow without bound.

With perfect prediction, the collector's work should be smoothly
distributed. Getting the predicted mortality wrong leads to "lumpy"
distribution of collection work. If the predicted mortality is too
high, the collector will bypass opportunities to perform work and then
find that it has to do more work to catch up later. If the predicted
mortality is too low, the collector will do extra work up front and
then find that it is idle later on.


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
