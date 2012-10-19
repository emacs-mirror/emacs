.. _topic-interface:


Memory Pool System interface conventions
========================================

This document describes the C programming language interface
conventions used by the MPS.

It also covers our policy for support for the external symbols.


Interface
---------

.. c:type:: mps_addr_t

    The type of :term:`addresses <address>` managed by the MPS, and
    also the type of :term:`references <reference>`.

    It is used in the MPS interface for any pointer that is under the
    control of the MPS. In accordance with standard :term:`C`
    practice, null pointers of type :c:type:`mps_addr_t` will never be
    used to represent a reference to a block.

    .. topics::

        :ref:`topic-platform`.


.. c:type:: mps_align_t

    The type of an :term:`alignment`. It is an integral type
    equivalent to ``size_t``. An alignment must be a positive power of
    2.


.. c:type:: mps_bool_t

    The type of a Boolean value. It is an integral type equivalent to
    ``int``.

    When used as an input parameter to the MPS, a value of 0 means
    "false" and any other value means "true". As an output parameter
    or function return from the MPS, 0 means "false", and 1 means
    "true".


.. c:type:: mps_word_t
    
    An unsigned integral type that is the same size as an
    :term:`object pointer`, so that ``sizeof(mps_word_t) ==
    sizeof(void*)``.

    The exact identity of this type is
    :term:`platform`\-dependent. Typical identities are ``unsigned
    long`` and ``unsigned __int_64``.2
