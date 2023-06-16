.. index::
   single: transform; introduction

.. _topic-transform:

Transforms
==========

In a long-running interactive system, it may be desirable to change
the format of live objects. In some programming languages (notably
Smalltalk), when the programmer edits a class definition, objects
belonging to the class must be updated so that they are valid
instances of the redefined class. This may involve adding or removing
fields from each instance and so changing the size of the allocated
objects.

If the object has grown as a result of the redefinition, this
redefinition can't be done in-place, so what actually happens is that
for each instance of the old version of the class, a corresponding
instance of the new version of the class is created, and all
:term:`references` to the old instance are rewritten to refer to the new
instance. Discovering "all references" to an object is a task that falls
to the garbage collector.

*Transforms* are a general mechanism by which the client program
 requests the MPS to replace references to one set of objects (the
 *old* objects) with references to another (the *new* objects). The
 MPS performs this task by carrying out a complete garbage collection,
 in the course of which all references to old objects are discovered
 and substituted with references to the corresponding new object.


Cautions
--------

1. The arena must be :term:`parked <parked state>` (for example, by
   calling :c:func:`mps_arena_park`) before creating the transform and
   not :term:`unclamped <unclamped state>` before applying the
   transform.

2. A transform cannot be applied if there is an :term:`ambiguous
   reference` to any of the old objects. (Because the MPS cannot know
   whether or not the reference should be updated to point to the new
   object.)

.. warning::

    The second caution means that transforms may be unsuitable for
    client programs that treat the :term:`registers` and :term:`control
    stack` as a :term:`root`, by using :c:func:`mps_root_create_thread`
    and similar functions, unless the program can guarantee that none of
    the old references will be referenced by this root.

    An alternative and more robust approach is to segregate the
    :term:`formatted objects` that need to be updated into a suitable
    :term:`pool`, and iterate over them using the function
    :c:func:`mps_pool_walk`.


.. index::
   single: transform; interface

Interface
---------

::

    #include "mps.h"


.. c:type:: mps_transform_t

    The type of :term:`transforms`. A transform represents a mapping from *old*
    :term:`references` to *new* references.


.. c:function:: mps_res_t mps_transform_create(mps_transform_t *transform_o, mps_arena_t arena)

    Create an empty :term:`transform`.

    ``transform_o`` points to a location that will hold the address of
    the new transform.

    ``arena`` is the :term:`arena` in which to create the transform.

    :c:func:`mps_transform_create` returns :c:macro:`MPS_RES_OK` if
    successful. The MPS may exhaust some resource in the course of
    :c:func:`mps_transform_create` and will return an appropriate
    :term:`result code` if so.

    .. note::

        The arena must be :term:`parked <parked state>` (for example,
        by calling :c:func:`mps_arena_park`) before creating a
        transform, and if :c:func:`mps_transform_apply` is called on
        a transform, it must be called before the arena is
        :term:`unclamped <unclamped state>`.


.. c:function:: mps_res_t mps_transform_add_oldnew(mps_transform_t transform, mps_addr_t *old_array, mps_addr_t *new_array, size_t count)

    Add mappings from an old :term:`reference` to a new reference to a
    :term:`transform`.

    ``transform`` is the transform to which the mappings will be added.

    ``old_array`` points to an array of old references.

    ``new_array`` points to an array of corresponding new references.

    ``count`` is the number of references in both arrays.

    :c:func:`mps_transform_add_oldnew` returns :c:macro:`MPS_RES_OK`
    if successful. The MPS may exhaust some resource in the course of
    :c:func:`mps_transform_add_oldnew` and will return an appropriate
    :term:`result code` if so.

    .. note::

        Each old reference must be added at most once to a given
        transform.


.. c:function:: mps_res_t mps_transform_apply(mps_bool_t *applied_o, mps_transform_t transform)

    Attempt to apply a :term:`transform`.

    ``applied_o`` points to a location that will hold a Boolean
    indicating whether or not the transform was applied.

    ``transform`` is the transform to apply.

    If the :term:`arena` is currently incapable of applying the
    transform, then an appropriate :term:`result code` is returned, and
    the location pointed to by ``applied_o`` is not updated. Possible
    causes include (but are not limited to):

    - the arena not being in the :term:`parked state` (in which case
      the result code is :c:macro:`MPS_RES_LIMIT`)

    - a collection having taken place since ``transform`` was created
      (in which case the result code is :c:macro:`MPS_RES_PARAM`).

    If the arena is *capable* of applying the transform, then the MPS
    carries out a :term:`garbage collection`, the arena is left in the
    :term:`parked state`, :c:func:`mps_transform_apply` returns
    :c:macro:`MPS_RES_OK`, and the location pointed to by ``applied_o``
    is updated.

    If in the course of the application, an :term:`ambiguous reference`
    was discovered, then the transform is aborted and ``*applied_o`` is
    set to false. In this case, *no* references to the old objects are
    updated. (That is, either *all* of the transform is applied, or
    *none* of it.)

    If the transform was successfully applied, it is destroyed, as if
    :c:func:`mps_transform_destroy` had been called.


.. c:function:: void mps_transform_destroy(mps_transform_t transform)

    Destroy a :term:`transform`.

    ``transform`` is the transform to destroy.
