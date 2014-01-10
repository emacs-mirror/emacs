.. Sources:

    `<https://info.ravenbrook.com/project/mps/doc/2002-06-18/obsolete-mminfo/mmdoc/protocol/mps/interface-types/>`_
    `<https://info.ravenbrook.com/project/mps/doc/2002-06-18/obsolete-mminfo/mmdoc/doc/mps/ref-man/if-conv/>`_
    `<https://info.ravenbrook.com/project/mps/master/design/interface-c/>`_

.. index::
   single: interface; introduction

.. _topic-interface:

Interface conventions
=====================

This document describes the conventions used in the programming
interface to the Memory Pool System. It also contains our :ref:`policy
for support for the public identifiers <topic-interface-support>` and
:ref:`definitions of general types <topic-interface-general>` that
appear throughout the interface.


.. index::
   pair: interface; support policy

.. _topic-interface-support:

Support policy
--------------

#.  We support the documented behaviour of public symbols in the MPS
    interface. We will only remove these symbols or change their
    behaviour in a new version, and not in a patch release. Normally
    we will give one version's notice before removing a symbol or
    changing a particular documented behaviour: that is, there will be
    a version in which the symbol (or reliance on some of its
    behaviour) is deprecated.

    .. note::

        If you are relying on a feature and you see that it's
        deprecated, please :ref:`contact us <contact>`. It makes a
        difference if we know that someone is using a feature.

#.  Behaviour that is not documented in the :ref:`guide`,
    :ref:`reference`, or :ref:`pool` is not supported and may change
    without notice in future releases. In particular, private
    identifiers may disappear or their behaviour be changed without
    notice in future releases.


.. index::
   pair: interface; language

Language
--------

#.  The MPS public interface conforms to :ref:`ANSI/ISO Standard C (IEC
    9899:1990) <C1990>`.


.. index::
   pair: interface; headers

Headers
-------

#.  The main interface is in the header ``mps.h``. This header
    contains all the core MPS interfaces. In practice, you always need
    at least one arena class and one pool class header file as well.

#.  We will always prefix public header file names with ``mps`` to
    avoid clashes. We reserve the right to create new headers
    with names beginning with ``mps`` in future versions.

#.  :term:`Pool class` headers have names beginning with ``mpsc``. For
    example, the header for :ref:`pool-amc` is ``mpscamc.h``.

#.  :term:`Arena class` headers have names beginning with ``mpsa``. For
    example, the header for the :term:`virtual memory arena` class is
    ``mpsavm.h``.


.. index::
   pair: interface; identifiers

Identifiers
-----------

#.  Identifiers are in lower case, except for preprocessor constants
    and macros that do not behave like functions, which are in upper
    case. Words are joined by underscores.

#.  All identifiers are either *public* or *private*.

#.  The names of public types, functions, variables, and macros start
    with ``mps_`` or ``MPS_``. The names of public structure members
    start with any letter.

#.  Private identifiers start with an underscore ``_``.

#.  Type names end with ``_t``, except for structure and union types.

#.  The names of structure types and tags end with ``_s``.

#.  The names of union types and tags end with ``_u``.


.. index::
   pair: interface; types

Types
-----

There are three kinds of types declared in the MPS interface:
*transparent types*, *opaque types*, and *derived types*.

#.  A *transparent type* is an alias defined using ``typedef``, and this
    is documented so that the :term:`client program` can rely on that
    fact. For example, :c:type:`mps_addr_t` is a transparent alias for
    ``void *``. Transparent types express intentions in the interface:
    in the case of :c:type:`mps_addr_t` it represents a pointer that
    is under the control of the MPS.

#.  An *opaque type* is a pointer to an incomplete structure type. The
    client program must not rely on details of its implementation. For
    example, the type :c:type:`mps_arena_t` is an alias for ``struct
    mps_arena_s *``, but the implementation of ``struct mps_arena_s``
    is not public.

    There are a few structure types that are declared in ``mps.h`` but
    whose implementation is not public. These only exist so that code
    can be inlined using macros. The most important of these is the
    :term:`scan state` structure ``mps_ss_s``, which is accessed by
    scanning macros such as :c:func:`MPS_SCAN_BEGIN` and
    :c:func:`MPS_FIX12`.

#.  A *derived type* is a structure or function type based on
    transparent and opaque types and on built-in C types. The degree
    to which you may or must depend upon the implementation of a
    derived type is covered by the documentation for the type. For
    example, the structure type :c:type:`mps_ap_s` has a mixture of
    public and private members.


.. index::
   single: interface; functions

Functions
---------

#.  Operations that might fail return a :term:`result code`, rather
    than a "special value" of the return type. See :ref:`topic-error`.

#.  A function that needs to return a value as well as a result code
    returns the value via an :term:`out parameter`, a parameter that
    points to a location to store the result.

#.  A function that stores a result in the location pointed to by an
    out parameter only does so if the function is successful (that is,
    if the function returns :c:macro:`MPS_RES_OK`).

#.  The value in the location pointed to by an out parameter is not
    read by the function.

#.  Out parameters have names ending with ``_o``.

#.  A function that both needs to read a value stored in a location and
    update the value does so via an :term:`in/out parameter`, which is
    the same as an out parameter except that the location it points to
    is read by the function. See for example :c:func:`MPS_FIX12`.

#.  In/out parameters have names ending with ``_io``.

#.  A function that takes optional arguments does so in the form of an
    array of keyword argument structures. These functions have names
    ending with ``_k``. See :ref:`topic-keyword`.


.. index::
   single: interface; type punning
   single: punning; type
   single: type punning

.. _topic-interface-pun:

Type punning
------------

It's tempting to use a type cast to change the type of an in/out or
out parameter, like this::

    /* allocate a struct foo */
    struct foo *fp;
    res = mps_alloc((mps_addr_t *)&fp, pool, sizeof(struct foo));

This is known as :term:`type punning`, and its behaviour is not
defined in ANSI/ISO Standard C. See :ref:`ISO/IEC 9899:1990 <ISO90>`
§6.3.2.3, which defines the conversion of a pointer from one type to
another: the behaviour of this cast is not covered by any of the cases
in the standard.

Instead, we recommend this approach::

    mps_addr_t p;
    struct foo *fp;
    res = mps_alloc(&p, pool, sizeof(struct foo));
    if(res) /* handle error case */;
    fp = (struct foo *)p;

This is portable because conversion from ``void *`` to any other
:term:`object pointer` type is defined by :ref:`ISO/IEC 9899:1990
<ISO90>` §6.3.2.3.1.


.. index::
   pair: interface; macros

Macros
------

#.  For function-like macros, the MPS follows the same convention as
    the Standard C library. To quote :ref:`ISO/IEC 9899:1990 <ISO90>`
    §7.1.7:

        Any function declared in a header may additionally be
        implemented as a macro defined in the header, so a library
        function should not be declared explicitly if its header is
        included. Any macro definition of a function can be suppressed
        locally be enclosing the name of the function in parentheses,
        because the name is then not followed by the left parenthesis
        that indicates expansion of a macro function name. [...] Any
        invocation of a library function that is implemented as a
        macro shall expand to code that evaluates each of its
        arguments exactly once, fully protected by parentheses where
        necessary, so it is generally safe to use arbitrary
        expressions as arguments.

#.  Some function-like macros evaluate an argument more than once, so
    it is not safe to have a side effect in an argument of such a
    method. These special cases are documented. For example,
    :c:func:`mps_reserve`.

#.  If you need the function rather than the macro, there are two
    approaches. You can undefine the macro::

          #undef mps_reserve
          res = mps_reserve(...);  /* calls function */

    Or you can put the name in parentheses::

          res = (mps_reserve)(...);  /* calls function */

#.  Statement-like macros have names in uppercase, for example
    :c:func:`MPS_RESERVE_BLOCK`. These macros behave like statements
    rather than expressions, so that you cannot write::

        (MPS_RESERVE_BLOCK(res, p, ap, size), 0)

#.  Details of the macro expansion, although visible in the header
    file, are not part of the MPS interface, and might change between
    releases. Don't rely on them, unless they are documented
    separately.


.. _topic-interface-general:

General types
-------------

.. c:type:: mps_addr_t

    The type of :term:`addresses` managed by the MPS, and also the
    type of :term:`references`.

    It is a :term:`transparent alias <transparent type>` for ``void *``.

    It is used in the MPS interface for any pointer that is under the
    control of the MPS. In accordance with standard :term:`C`
    practice, null pointers of type :c:type:`mps_addr_t` will never be
    used to represent a reference to a block.


.. c:type:: mps_align_t

    The type of an :term:`alignment`.

    It is a :term:`transparent alias <transparent type>` for ``size_t``.

    An alignment must be a positive power of 2.


.. c:type:: mps_bool_t

    The type of a Boolean value.

    It is a :term:`transparent alias <transparent type>` for ``int``.

    When used as an input parameter to the MPS, a value of 0 means
    "false" and any other value means "true". As an output parameter
    or function return from the MPS, 0 means "false", and 1 means
    "true".


.. c:type:: mps_clock_t

    The type of a processor time.

    It is a :term:`transparent alias <transparent type>` for
    :c:type:`mps_word_t`.

    This is the type returned by the plinth function
    :c:func:`mps_clock`.


.. c:type:: mps_fun_t

    The type of a generic function pointer.

    It is a :term:`transparent alias <transparent type>` for
    ``void (*)(void)``.


.. c:type:: mps_label_t

    The type of a :term:`telemetry label`.

    It is an unsigned integral type.


.. c:type:: mps_word_t
    
    An unsigned integral type that is the same size as an
    :term:`object pointer`, so that ``sizeof(mps_word_t) ==
    sizeof(void *)``.

    The exact identity of this type is
    :term:`platform`\-dependent. Typical identities are ``unsigned
    long`` and ``unsigned __int_64``.

    .. topics::

        :ref:`topic-platform`.
