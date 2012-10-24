.. _topic-error:

Error handing
=============

Operations in the Memory Pool System that might fail return a
:term:`result code` of type :c:type:`mps_res_t`, rather than a
"special value" of the return type.

Success is always indicated by the result code :c:macro:`MPS_RES_OK`,
which is defined to be zero. Other result codes indicate failure, and
are non-zero.

The modular nature of the MPS means that it is not usually possible
for a function description to list the possible error codes that it
might return. A function in the public interface typically calls
methods of an :term:`arena class` and one or more :term:`pool classes
<pool class>`, any of which might fail. The MPS is extensible with new
arena and pool classes, which might fail in new and interesting ways,
so the only future-proof behaviour is for a :term:`client program` to
assume that any MPS function that returns a result code can return
*any* result code.


.. c:type:: mps_res_t

    The type of :term:`result codes <result code>`. It is a
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

    You need to deallocate something to make more space, or increase
    the commit limit by calling :c:func:`mps_arena_commit_limit_set`.


.. c:macro:: MPS_RES_FAIL

    A :term:`result code` indicating that something went wrong that
    does not fall under the description of any other result code. The
    exact meaning depends on the function that returned this result
    code.


.. c:macro:: MPS_RES_IO

    A :term:`result code` indicating that an input/output error
    occurred. The exact meaning depends on the function that returned
    this result code.


.. c:macro:: MPS_RES_LIMIT

    A :term:`result code` indicating that an operation could not be
    completed as requested because of an internal limitation of the
    MPS. The exact meaning depends on the function that returned this
    result code.


.. c:macro:: MPS_RES_MEMORY

    A :term:`result code` indicating that an operation could not be
    completed because there wasn't enough memory available.

    You need to deallocate something or allow the :term:`garbage
    collector` to reclaim something to free enough memory, or expand
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
    the operation. The exact meaning depends on the function that
    returned this result code.


.. c:macro:: MPS_RES_RESOURCE

    A :term:`result code` indicating that an operation could not be
    completed as requested because the MPS could not obtain a needed
    resource. The resource in question depends on the operation.

    Two special cases have their own result codes: when the MPS runs
    out of committed memory, it returns :c:macro:`MPS_RES_MEMORY`, and
    when it cannot proceed without exceeding the :term:`commit limit`,
    it returns :c:macro:`MPS_RES_COMMIT_LIMIT`.

    This result code can be returned when the MPS runs out of
    :term:`virtual memory`. If this happens, you need to reclaim
    memory within your process (as for the result code
    :c:macro:`MPS_RES_MEMORY`), or terminate other processes running
    on the same machine.


.. c:macro:: MPS_RES_UNIMPL

    A :term:`result code` indicating that an operation, or some vital
    part of it, is not implemented.

    This might be returned by functions that are no longer supported,
    or by operations that are included for future expansion, but not
    yet supported.


Assertions
----------

Bugs in the :term:`client program` may violate the invariants that the
MPS relies on. Most functions in the MPS (in most *varieties*; see
below) assert the correctness of their data structures, so these bugs
will often be discovered by an assertion failure in the MPS.

For example, if a :term:`scan method` makes a re-entrant call into the
MPS, this may result in this assertion:

.. code-block:: none

    MPS ASSERTION FAILURE: res == 0
    ../../mps-kit-1.110.0/code/lockix.c
    125

Or if a client program fails to fix a reference in an :ref:`pool-amc`
pool, this may violate the :term:`tri-colour invariant` that the MPS
depends on for the correctness of its :term:`incremental garbage
collection`. Such a bug may result in this assertion:

.. code-block:: none

    MPS ASSERTION FAILURE: !AMS_IS_INVALID_COLOUR(seg, i)
    ../../mps-kit-1.110.0/code/poolams.c
    1323

It is very rare for an assertion to indicate a bug in the MPS rather
than the client program, but it is not unknown, so if you have made
every effort to track down the cause (see :ref:`guide-debug`) without
luck, :ref:`get in touch <contact>`.

If you are running your MPS-enabled program from Emacs via the
``compile`` command, you will probably want to recognize MPS
assertions automatically, by adding the following to your ``.emacs``:

.. code-block:: lisp

    (add-to-list 'compilation-error-regexp-alist
                 '("MPS ASSERTION FAILURE: .*\n\\(.*\\)\n\\([0-9]+\\)" 1 2))


Varieties
---------

The MPS has three behaviours with respect to internal checking and
:term:`telemetry`, which need to be selected at compile time, by
defining one of the following preprocessor constants. (If none is
specified then :c:macro:`CONFIG_VAR_HOT` is the default.)


.. c:macro:: CONFIG_VAR_COOL

    All functions check the consistency of their data structures and
    may assert, including function on the :term:`critical path`.

    All events are sent to the telemetry stream, including events on
    the :term:`critical path`


.. c:macro:: CONFIG_VAR_HOT

    Some functions check the consistency of their data structures and
    may assert, namely those not on the :term:`critical path`.

    No events are sent to the telemetry stream.


.. c:macro:: CONFIG_VAR_RASH

    No functions check the consistency of their data structures and
    consequently there are no assertions.

    No events are sent to the telemetry stream.
