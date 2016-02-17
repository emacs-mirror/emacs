.. index::
   single: error handling; introduction
   single: result code

.. _topic-error:

Error handing
=============

Operations in the Memory Pool System that might fail return a
:term:`result code` of type :c:type:`mps_res_t`. Success is always
indicated by the result code :c:macro:`MPS_RES_OK`, which is defined
to be zero. Other result codes indicate failure, and are non-zero. The
MPS never uses a "special value" of some other type to indicate
failure (such as returning ``NULL`` for a pointer result, or −1 for a
size result).

.. note::

    The MPS does not throw or catch exceptions. (This is necessary
    for the MPS to be portable to systems that have only a
    :term:`freestanding` implementation of the C language.)

The modular nature of the MPS means that it is not usually possible
for a function description to list the possible error codes that it
might return. A function in the public interface typically calls
methods of an :term:`arena class` and one or more :term:`pool
classes`, any of which might fail. The MPS is extensible with new
arena and pool classes, which might fail in new and interesting ways,
so the only future-proof behaviour is for a :term:`client program` to
assume that any MPS function that returns a result code can return
*any* result code.


.. c:type:: mps_res_t

    The type of :term:`result codes`. It is a
    :term:`transparent alias <transparent type>` for ``int``, provided
    for convenience and clarity.

    A result code indicates the success or failure of an operation,
    along with the reason for failure. As with error numbers in Unix,
    the meaning of a result code depends on the call that returned it.
    Refer to the documentation of the function for the exact meaning
    of each result code.

    The result codes are:

    * :c:macro:`MPS_RES_OK`: operation succeeded.

    * :c:macro:`MPS_RES_FAIL`: operation failed.

    * :c:macro:`MPS_RES_IO`: an input/output error occurred.

    * :c:macro:`MPS_RES_LIMIT`: an internal limitation was exceeded.

    * :c:macro:`MPS_RES_MEMORY`: needed memory could not be obtained.

    * :c:macro:`MPS_RES_RESOURCE`: a needed resource could not be
      obtained.

    * :c:macro:`MPS_RES_UNIMPL`: operation is not implemented.

    * :c:macro:`MPS_RES_COMMIT_LIMIT`: the arena's :term:`commit
      limit` would be exceeded.

    * :c:macro:`MPS_RES_PARAM`: an invalid parameter was passed.



Result codes
------------

.. c:macro:: MPS_RES_COMMIT_LIMIT

    A :term:`result code` indicating that an operation could not be
    completed as requested without exceeding the :term:`commit limit`.

    You need to deallocate something or allow the :term:`garbage
    collector` to reclaim something to make more space, or increase
    the commit limit by calling :c:func:`mps_arena_commit_limit_set`.


.. c:macro:: MPS_RES_FAIL

    A :term:`result code` indicating that something went wrong that
    does not fall under the description of any other result code.


.. c:macro:: MPS_RES_IO

    A :term:`result code` indicating that an input/output error
    occurred in the :term:`telemetry` system.


.. c:macro:: MPS_RES_LIMIT

    A :term:`result code` indicating that an operation could not be
    completed as requested because of an internal limitation of the
    MPS.


.. c:macro:: MPS_RES_MEMORY

    A :term:`result code` indicating that an operation could not be
    completed because there wasn't enough memory available.

    You need to deallocate something or allow the :term:`garbage
    collector` to reclaim something to free enough memory, or extend
    the :term:`arena` (if you're using an arena for which that does
    not happen automatically).

    .. note::

        Failing to acquire enough memory because the :term:`commit
        limit` would have been exceeded is indicated by returning
        :c:macro:`MPS_RES_COMMIT_LIMIT`, not ``MPS_RES_MEMORY``.

        Running out of :term:`address space` (as might happen in
        :term:`virtual memory` systems) is indicated by returning
        :c:macro:`MPS_RES_RESOURCE`, not ``MPS_RES_MEMORY``.


.. c:macro:: MPS_RES_OK

    A :term:`result code` indicating that an operation succeeded.

    If a function takes an :term:`out parameter` or an :term:`in/out
    parameter`, this parameter will only be updated if
    :c:macro:`MPS_RES_OK` is returned. If any other result code is
    returned, the parameter will be left untouched by the function.

    :c:macro:`MPS_RES_OK` is zero.


.. c:macro:: MPS_RES_PARAM

    A :term:`result code` indicating that an operation could not be
    completed as requested because an invalid parameter was passed to
    the operation.


.. c:macro:: MPS_RES_RESOURCE

    A :term:`result code` indicating that an operation could not be
    completed as requested because the MPS could not obtain a needed
    resource. It can be returned when the MPS runs out of
    :term:`address space`. If this happens, you need to reclaim memory
    within your process (as for the result code
    :c:macro:`MPS_RES_MEMORY`).

    Two special cases have their own result codes: when the MPS runs
    out of committed memory, it returns :c:macro:`MPS_RES_MEMORY`, and
    when it cannot proceed without exceeding the :term:`commit limit`,
    it returns :c:macro:`MPS_RES_COMMIT_LIMIT`.


.. c:macro:: MPS_RES_UNIMPL

    A :term:`result code` indicating that an operation, or some vital
    part of it, is not implemented.

    This might be returned by functions that are no longer supported,
    or by operations that are included for future expansion, but not
    yet supported.


.. index::
   single: assertion
   single: error handling; assertion

.. _topic-error-assertion:

Assertions
----------

Bugs in the :term:`client program` may violate the invariants that the
MPS relies on. Most functions in the MPS (in most *varieties*; see
below) assert the correctness of their data structures, so these bugs
will often be discovered by an assertion failure in the MPS. The
section :ref:`topic-error-cause` below lists commonly encountered
assertions and explains the kinds of client program bugs that can
provoke these assertions.

It is very rare for an assertion to indicate a bug in the MPS rather
than the client program, but it is not unknown, so if you have made
every effort to track down the cause (see :ref:`guide-debug`) without
luck, :ref:`get in touch <contact>`.


.. index::
   single: assertion
   single: error handling; assertion; assertion handling

.. _topic-error-assertion-handling:

Assertion handling
..................

When the MPS detects an assertion failure, it calls the :term:`plinth`
function :c:func:`mps_lib_assert_fail`. Unless you have replaced the plinth, this behaves as follows:

- In the :term:`cool` :term:`variety`, print the assertion message to
  standard error and terminate the program by calling :c:func:`abort`.

- In the :term:`hot` and :term:`rash` varieties, print the assertion
  message to standard error and do *not* terminate the program.

You can change this behaviour by providing your own plinth, or using
:c:func:`mps_lib_assert_fail_install`.

In many applications, users don't want their program terminated when
the MPS detects an error, no matter how severe. A lot of MPS
assertions indicate that the program is going to crash very soon, but
there still may be a chance for a user to get some useful results or
save their work. This is why the default assertion handler only
terminates in the :term:`cool` :term:`variety`.


.. index::
   single: assertion; common causes

.. _topic-error-cause:

Common assertions and their causes
..................................

This section lists some commonly encountered assertions and suggests
likely causes. If you encounter an assertion not listed here (or an
assertion that is listed here but for which you discovered a different
cause), please :ref:`let us know <contact>` so that we can improve
this documentation.

``arg.c: MPS_KEY_...``

    A required :term:`keyword argument` was omitted from a call to
    :c:func:`mps_ap_create_k`, :c:func:`mps_arena_create_k`,
    :c:func:`mps_fmt_create_k`, or :c:func:`mps_pool_create_k`.


``buffer.c: BufferIsReady(buffer)``

    The client program called :c:func:`mps_reserve` twice on the same
    :term:`allocation point` without calling :c:func:`mps_commit`. See
    :ref:`topic-allocation-point-protocol`.


``dbgpool.c: fencepost check on free``

    The client program wrote to a location after the end, or before
    the beginning of an allocated block. See :ref:`topic-debugging`.


``dbgpool.c: free space corrupted on release``

    The client program used an object after it was reclaimed. See
    :ref:`topic-debugging`.


``format.c: SigCheck Format: format``

    The client program called :c:func:`mps_pool_create_k` for a
    :term:`pool class` like :ref:`pool-amc` that requires a
    :term:`object format`, but passed something other than a
    :c:type:`mps_fmt_t` for this argument.


``global.c: RingIsSingle(&arena->chainRing)``

    The client program called :c:func:`mps_arena_destroy` without
    destroying all the :term:`generation chains` belonging to the
    arena. It is necessary to call :c:func:`mps_chain_destroy` first.


``global.c: RingIsSingle(&arena->formatRing)``

    The client program called :c:func:`mps_arena_destroy` without
    destroying all the :term:`object formats` belonging to the arena.
    It is necessary to call :c:func:`mps_fmt_destroy` first.


``global.c: RingIsSingle(&arena->rootRing)``

    The client program called :c:func:`mps_arena_destroy` without
    destroying all the :term:`roots` belonging to the arena.
    It is necessary to call :c:func:`mps_root_destroy` first.


``global.c: RingIsSingle(&arena->threadRing)``

    The client program called :c:func:`mps_arena_destroy` without
    deregistering all the :term:`threads` belonging to the arena.
    It is necessary to call :c:func:`mps_thread_dereg` first.


``global.c: RingLength(&arenaGlobals->poolRing) == 5``

    The client program called :c:func:`mps_arena_destroy` without
    destroying all the :term:`pools` belonging to the arena.
    It is necessary to call :c:func:`mps_pool_destroy` first.


``global.c: PoolHasAttr(pool, AttrGC)``

    The client program called :c:func:`mps_finalize` on a reference
    that does not belong to an :term:`automatically managed <automatic
    memory management>` :term:`pool`.


``lockix.c: res == 0``

``lockw3.c: lock->claims == 0``

    The client program has made a re-entrant call into the MPS. Look
    at the backtrace to see what it was. Common culprits are signal
    handlers, assertion handlers, :term:`format methods`, and
    :term:`stepper functions`.


``locus.c: chain->activeTraces == TraceSetEMPTY``

    The client program called :c:func:`mps_chain_destroy`, but there
    was a garbage collection in progress on that chain. Park the arena
    before destroying the chain, by calling :c:func:`mps_arena_park`.


``mpsi.c: SizeIsAligned(size, BufferPool(buf)->alignment)``

    The client program reserved a block by calling
    :c:func:`mps_reserve` but neglected to round the size up to the
    alignment required by the pool's :term:`object format`.


``poolams.c: AMS_ALLOCED(seg, i)``

    The client program tried to :term:`fix` a :term:`reference` to a
    block in an :ref:`pool-ams` pool that died. This may mean that
    there was a previous collection in which a reference that should
    have kept the block alive failed to be scanned. Perhaps a
    :term:`formatted object` was updated in some way that has a race
    condition?


``poolsnc.c: foundSeg``

    The client program passed an incorrect ``frame`` argument to
    :c:func:`mps_ap_frame_pop`. This argument must be the result from
    a previous call to :c:func:`mps_ap_frame_push` on the same
    allocation point.


``seg.c: gcseg->buffer == NULL``

    The client program destroyed pool without first destroying all the
    allocation points created on that pool. The allocation points must
    be destroyed first.


``trace.c: ss->rank < RankEXACT``

    The client program destroyed a pool containing objects registered
    for finalization, and then continued to run the garbage collector.
    See :ref:`topic-finalization-cautions` under
    :ref:`topic-finalization`, which says, "You must destroy these
    pools by following the ‘safe tear-down’ procedure described under
    :c:func:`mps_pool_destroy`."


``trace.c: RefSetSub(ScanStateUnfixedSummary(ss), SegSummary(seg))``

    The client program's :term:`scan method` failed to update a
    reference to an object that moved. See
    :ref:`topic-scanning-protocol`, which says, "If :c:func:`MPS_FIX2`
    returns :c:macro:`MPS_RES_OK`, it may have updated the reference.
    Make sure that the updated reference is stored back to the region
    being scanned."


.. index::
   single: error handling; varieties
   single: variety

.. _topic-error-variety:
   
Varieties
---------

The MPS has three *varieties* which have different levels of internal
checking and :ref:`telemetry <topic-telemetry>`. The variety can be
selected at compile time, by defining one of the following
preprocessor constants. If none is specified then
:c:macro:`CONFIG_VAR_HOT` is the default.


.. index::
   single: cool variety
   single: variety; cool

.. c:macro:: CONFIG_VAR_COOL

    The *cool variety* is intended for development and testing.

    All functions check the consistency of their data structures and may
    assert, including functions on the :term:`critical path`.
    Furthermore, in the default ANSI Library the default assertion
    handler will terminate the program.  See
    :c:func:`mps_lib_assert_fail_install`.

    All events are sent to the :term:`telemetry stream`, including
    events on the :term:`critical path`.


.. index::
   single: hot variety
   single: variety; hot

.. c:macro:: CONFIG_VAR_HOT

    The *hot variety* is intended for production and deployment.

    Some functions check the consistency of their data structures and
    may assert, namely those not on the :term:`critical path`.  However,
    in the default ANSI Library, the default assertion handler will not
    terminate the program.  See :c:func:`mps_lib_assert_fail_install`.

    Some events are sent to the telemetry stream, namely those not on
    the :term:`critical path`.


.. index::
   single: rash variety
   single: variety; rash

.. c:macro:: CONFIG_VAR_RASH

    The *rash variety* is intended for mature integrations, or for
    developers who like living dangerously.

    No functions check the consistency of their data structures and
    consequently there are no assertions.

    No events are sent to the telemetry stream.
