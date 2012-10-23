.. _topic-interface:


Memory Pool System interface conventions
========================================

This document describes the C programming language interface
conventions used by the MPS. It also contains our :ref:`policy for
support for the public identifiers <topic-interface-support>` and
:ref:`definitions of general types <topic-interface-general>` that
appear throughout the interface.


Language
--------

1.  The MPS interface public interface conforms to ANSI/ISO Standard C
    (IEC 9899:1990).


Headers
-------

1.  The main interface is called ``mps.h``. This header contains all
    the core MPS interfaces. In practice, you always need at least one
    arena class and one pool class header file as well.

2.  We will always prefix public header file names with ``mps`` to
    avoid clashes. We reserve the right to create new headers
    with names beginning with ``mps`` in future versions.

3.  :term:`Pool class` headers have names beginning with ``mpsc``. For
    example, the header for :ref:`pool-amc` is ``mpscamc.h``.

4.  :term:`Arena class` headers have names beginning with ``mpsa``. For
    example, the header for the :term:`virtual memory arena` class is
    ``mpsavm.h``.


Identifiers
-----------

1.  Identifiers are in lower case, except for preprocessor constants
    and macros that do not behave like functions, which are in upper
    case. Words are joined by underscores.

2.  All identifiers are either *public* or *private*.

3.  The name of public types, functions, variables, and macros start
    with ``mps_`` or ``MPS_``. The name of public structure members
    start with any letter.

4.  Private identifiers start with an underscore ``_``.

5.  Type names end with ``_t``, except for structure and union types.

6.  Structure types and tags end with ``_s``.

7.  Union types and tags end with ``_u``.


Types
-----

1.  There are three kinds of types declared in the MPS interface:
    *transparent types*, *opaque types*, and *derived types*. See below.

2.  A *transparent type* is an alias defined using ``typedef``, and this
    is documented so that the :term:`client program` can rely on that
    fact. For example, :c:type:`mps_addr_t` is a transparent alias for
    ``void *``. Transparent types express intentions in the interface:
    in the case of :c:type:`mps_addr_t` it represents a pointer that
    is under the control of the MPS.

3.  An *opaque type* is a pointer to an incomplete structure type. The
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

4.  A *derived type* is a structure or function type based on
    transparent and opaque types and on built-in C types. The degree
    to which you may or must depend upon the implementation of a
    derived type is covered by the documentation for the type. For
    example, the structure type :c:type:`mps_ap_s` has a mixture of
    public and private members.


Functions
---------

1.  Operations that might fail return a :term:`result code`, rather
    than a "special value" of the return type. See :ref:`topic-error`.

2.  A function that needs to return a value as well as a result code
    returns the value via an :term:`out parameter`, a parameter that
    points to a location to store the result.

3.  A function that stores a result in the location pointed to by an
    out parameter only does so if the function is successful (that is,
    if the function returns :c:macro:`MPS_RES_OK`).

4.  The value in the location pointed to by an out parameter is not
    read by the function.

5.  Out parameters have names ending with ``_o``.

6.  A function that both needs to read a value stored in a location and
    update the value does so via an :term:`in/out parameter`, which is
    the same as an out parameter except that the location it points to
    is read by the function. See for example :c:func:`MPS_FIX12`.

7.  In/out parameters have names ending with ``_io``.

8.  It's tempting to use a type cast to change the type of a in/out or
    out parameter, like this::

        /* allocate a struct foo */
        struct foo *fp;
        res = mps_alloc((mps_addr_t *)&fp, pool, sizeof(struct foo));

    This is known as :term:`type punning`, and its behaviour is
    undefined in ANSI/ISO Standard C. (See §6.3.2.3, which defines the
    conversion of a pointer from one type to another: the behaviour of
    this cast is not covered by any of the cases in the standard.)
    Instead, we recommend this approach::

        mps_addr_t p;
        struct foo *fp;
        res = mps_alloc(&p, pool, sizeof(struct foo));
        if(res) /* handle error case */;
        fp = (struct foo *)p;

    This is portable because conversion from ``void *`` to any other
    :term:`object pointer` type is defined by §6.3.2.3.1.


Macros
------

1.  For function-like macros, the MPS follows the same convention as
    the Standard C library. To quote §7.1.7:

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

2.  Some function-like macros evaluate an argument more than once, so
    it is not safe to have a side effect in an argument of such a
    method. These special cases are documented. For example,
    :c:func:`mps_reserve`.

3.  If you need the function rather than the macro, there are two
    approaches. You can undefine the macro::

          #undef mps_reserve
          res = mps_reserve(...);  /* calls function */

    Or you can put the name in parentheses::

          res = (mps_reserve)(...);  /* calls function */

4.  Statement-like macros have named in uppercase, for example
    :c:func:`MPS_RESERVE_BLOCK`. These macros behave like statements
    rather than expressions, so that you cannot write::

        (MPS_RESERVE_BLOCK(res, p, ap, size), 0)

5.  Details of the macro expansion, although visible in the header
    file, are not part of the MPS interface, and might change between
    releases. Don't rely on them, unless they are documented
    separately.


.. _topic-interface-support:

Support policy
--------------

1.  We support the documented behaviour of public symbols in the MPS
    interface. We will only remove these symbols or change their
    behaviour in a new version, and not in a patch release. Normally
    we will give one version's notice before removing a symbol or
    changing a particular documented behaviour: that is, there will be
    a version in which the symbol (or reliance on some of its
    behaviour) is deprecated.

2.  Behaviour that is not documented in the :ref:`guide`,
    :ref:`reference`, or :ref:`pool` is not supported and may change
    without notice in future releases. In particular, private
    identifiers may disappear or their behaviour be changed without
    notice in future releases.

.. note::

    If you are relying on a feature and you see that it's deprecated,
    please :ref:`contact us <contact>`. It makes a difference if we
    know that someone is using a feature.


.. _topic-interface-general:

General types
-------------

.. c:type:: mps_addr_t

    The type of :term:`addresses <address>` managed by the MPS, and
    also the type of :term:`references <reference>`.

    It is a :term:`transparent alias <transparent type>` for ``void *``.

    It is used in the MPS interface for any pointer that is under the
    control of the MPS. In accordance with standard :term:`C`
    practice, null pointers of type :c:type:`mps_addr_t` will never be
    used to represent a reference to a block.

    .. topics::

        :ref:`topic-platform`.


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


.. c:type:: mps_word_t
    
    An unsigned integral type that is the same size as an
    :term:`object pointer`, so that ``sizeof(mps_word_t) ==
    sizeof(void *)``.

    The exact identity of this type is
    :term:`platform`\-dependent. Typical identities are ``unsigned
    long`` and ``unsigned __int_64``.
