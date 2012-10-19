.. _topic-collection:

Garbage collection
==================


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


