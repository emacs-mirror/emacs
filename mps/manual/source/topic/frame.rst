.. Sources: 

    `<https://info.ravenbrook.com/project/mps/doc/2002-06-18/obsolete-mminfo/mmdoc/doc/mps/guide/stack-alloc/>`_
    `<https://info.ravenbrook.com/project/mps/master/design/alloc-frame/>`_

.. index::
   pair: allocation; frame
   single: allocation; stack-like
   single: stack; allocation

.. _topic-frame:

Allocation frames
=================

An allocation frame is a marker that can pushed onto an
:term:`allocation point` by calling :c:func:`mps_ap_frame_push`, and
then popped by calling :c:func:`mps_ap_frame_pop` to indicate that all
blocks allocated on the allocation point are :term:`dead` (in the case
of :term:`manual <manual memory management>` pools), or very likely
dead (in the case of :term:`automatic <automatic memory management>`
pools).

Allocation frames can be used by the :term:`client program` to
efficiently implement stack-like patterns of allocation.

.. note::

    The only :term:`pool class` in the MPS that supports allocation
    frames is :ref:`pool-snc`.


.. c:type:: mps_frame_t

    The type of :term:`allocation frames`.


.. c:function:: mps_res_t mps_ap_frame_push(mps_frame_t *frame_o, mps_ap_t ap)

    Declare a new :term:`allocation frame` and push it onto an
    :term:`allocation point's <allocation point>` frame stack.

    ``frame_o`` points to a location that will hold the new frame if the
    function is successful.

    ``ap`` is the allocation point in which the new frame is declared.

    Returns a :term:`result code`. The creation of new frames (which
    is implicit in the action of this function) can consume resources,
    so this function can fail because there are insufficient
    resources, or if the correct protocol is not followed by the
    :term:`client program`.


.. c:function:: mps_res_t mps_ap_frame_pop(mps_ap_t ap, mps_frame_t frame)

    Declare that a set of :term:`blocks` in a
    :term:`allocation frame` are :term:`dead` or likely to be dead,
    and pop the frame from the :term:`allocation point's <allocation
    point>` frame stack.

    ``ap`` is the allocation point in which ``frame`` was pushed.

    ``frame`` is the allocation frame whose blocks are likely to be
    dead.

    Returns a :term:`result code`.

    This function pops ``frame``, making its parent the current
    frame. Popping invalidates ``frame`` and all frames pushed since
    ``frame``. Popping ``frame`` also makes a declaration about the set of
    blocks which were allocated in ``frame`` and all frames which were
    pushed since ``frame``.

    The interpretation of this declaration depends on the :term:`pool`
    that the allocation point belongs to. Typically, :term:`manual
    <manual memory management>` pool classes use this declaration to
    mean that the blocks are dead and their space can be reclaimed
    immediately, whereas :term:`automatic <automatic memory
    management>` pool classes use this declaration to mean that the
    blocks are likely to be mostly dead, and may use this declaration
    to alter its collection decisions. See the documentation for the
    pool class.

    In general a frame other than the current frame can be popped (all
    frames pushed more recently will be invalidated as well, as
    described above), but a pool class may impose the restriction that
    only the current frame may be popped. This restriction means that
    every push must have a corresponding pop. See the documentation
    for the pool class.

    It is illegal to pop frames out of order (so the sequence "A =
    push; B = push; pop A; pop B" is illegal) or to pop the same frame
    twice (so the sequence "A = push, pop A, pop A" is illegal).
