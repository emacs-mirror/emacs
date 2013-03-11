.. sources:

    `<https://info.ravenbrook.com/project/mps/master/design/protocol/>`_

.. mps:prefix:: design.mps.protocol

Protocol inheritance
====================


Introduction
------------

.. mps:label:: intro

This document explains the design of the support for class
inheritance in MPS. It is not yet complete. It describes support for
single inheritance of classes. Future extensions will describe
multiple inheritance and the relationship between instances and
classes.

.. mps:label:: readership

This document is intended for any MPS developer.

.. mps:label:: hist.0

Written by Tony 1998-10-12


Purpose
-------

.. mps:label:: purpose.code-maintain

The purpose of the protocol inheritance design is to ensure that the
MPS code base can make use of the benefits of object-oriented class
inheritance to maximize code reuse, minimize code maintenance and
minimize the use of boilerplate code.

.. mps:label:: purpose.related

For related discussion, see mail.tony.1998-08-28.16-26(0),
mail.tony.1998-09-01.11-38(0), mail.tony.1998-10-06.11-03(0) and other
messages in the same threads.


Requirements
------------

.. mps:label:: req.implicit

The object system should provide a means for classes to inherit the
methods of their direct superclasses implicitly for all functions in
the protocol without having to write any explicit code for each
inherited function.

.. mps:label:: req.override

There must additionally be a way for classes to override the methods
of their superclasses.

.. mps:label:: req.next-method

As a result of :mps:label:`.req.implicit`, classes cannot make static
assumptions about methods used by direct superclasses. The object
system must provide a means for classes to extend (not just replace)
the behaviour of protocol functions, such as a mechanism for invoking
the "next-method".

.. mps:label:: req.ideal.extend

The object system must provide a standard way for classes to implement
the protocol supported by they superclass and additionally add new
methods of their own which can be specialized by subclasses.

.. mps:label:: req.ideal.multiple-inheritance

The object system should support multiple inheritance such that
sub-protocols can be "mixed in" with several classes which do not
themselves support identical protocols.


Overview
--------

.. mps:label:: overview.root

We start with the root of all conformant class hierarchies, which is
called :c:type:`ProtocolClass`. This is an "abstract" class (that is,
it has no direct instances, but it is intended to have subclasses). To
use Dylan terminology, instances of its subclasses are "general"
instances of ProtocolClass. They look like this::

     Instance Object                    Class Object

     --------------------              --------------------
     |     sig          |    |-------->|    sig           |
     --------------------    |         --------------------
     |     class        |----|         |    superclass    |
     --------------------              --------------------
     |     ...          |              |    coerceInst    |
     --------------------              --------------------
     |     ...          |              |    coerceClass   |
     --------------------              --------------------
     |                  |              |     ...          |

.. mps:label:: overview.inherit

Classes inherit the protocols supported by their superclasses. By
default they have the same methods as the class(es) from which they
inherit.

.. mps:label:: overview.inherit.specialize

Classes may specialize the behaviour of their superclass. They do this
by by overriding methods or other fields in the class object.

.. mps:label:: overview.extend

Classes may extend the protocols supported by their 
superclasses by adding new fields for methods or other data.

.. mps:label:: overview.sig.inherit

Classes will contain (possibly several) signatures. Classes must not
specialize (override) the signatures they inherit from their
superclasses.

.. mps:label:: overview.sig.extend

If a class definition extends a protocol, it is normal policy for the
class definition to include a new signature as the last field in the
class object.

.. mps:label:: overview.coerce-class

Each class contains a ``coerceClass`` field. This contains a method
which can find the part of the class object which implements the
protocols of a supplied superclass argument (if, indeed, the argument
*is* a superclass). This function may be used for testing
subclass/superclass relationships, and it also provides support for
multiple inheritance.

.. mps:label:: overview.coerce-inst

Each class contains a ``coerceInst`` field. This contains a method
which can find the part of an instance object which contains the
instance slots of a supplied superclass argument (if, indeed, the
argument *is* a superclass). This function may be used for testing
whether an object is an instance of a given class, and it also
provides support for multiple inheritance.

.. mps:label:: overview.superclass

Each class contains a ``superclass`` field. This enables classes to
call "next-method", as well as enabling the coercion functions.

.. mps:label:: overview.next-method

A specialized method in a class can make use of an overridden method
from a superclass by accessing the method from the appropriate field
in the superclass object and calling it. The superclass may be
accessed indirectly from the class's "Ensure" function when it is
statically known (see :mps:label:`.overview.access`). This permits
"next-method" calls, and is fully scalable in that it allows arbitrary
length method chains. The :c:func:`SUPERCLASS` macro helps with this
(see :mps:label:`.int.static-superclass`).

.. mps:label:: overview.next-method.naive

In some cases it is necessary to write a method which is designed to
specialize an inherited method, needs to call the next-method, and yet
the implementation doesn't have static knowledge of the superclass.
This might happen because the specialized method is designed to be
reusable by many class definitions. The specialized method can usually
locate the class object from one of the parameters passed to the
method. It can then access the superclass through the ``superclass``
field of the class, and hence call the next method. This technique has
some limitations and doesn't support longer method chains. It is also
dependent on none of the class definitions which use the method having
any subclasses.

.. mps:label:: overview.access

Classes must be initialized by calls to functions, since it is these
function calls which copy properties from superclasses. Each class
must provide an "Ensure" function, which returns the canonical
copy of the class. The canonical copy may reside in static storage,
but no MPS code may refer to that static storage by name.

.. mps:label:: overview.naming

There are some strict naming conventions which must be followed when
defining and using classes. The use is obligatory because it is
assumed by the macros which support the definition and inheritance
mechanism. For every class ``SomeClass``, we insist upon the following
naming conventions:-

``SomeClassStruct``

    names the type of the structure for the protocol class. This might
    be a ``typedef`` which aliases the type to the type of the
    superclass, but if the class has extended the protocols of the
    superclass the it will be a type which contains the new class
    fields.

``SomeClass``

    names the type ``*SomeClassStruct``. This might be a ``typedef``
    which aliases the type to the type of the superclass, but if the
    class has extended the protocols of the superclass then it will be
    a type which contains the new class fields.

``EnsureSomeClass()``

    names the function that returns the initialized class object.



Interface
---------

Class definition
................

.. mps:label:: int.define-class

Class definition is performed by the macro ``DEFINE_CLASS(className,
var)``. A call to the macro must be followed by a body of
initialization code in braces ``{}``. The parameter ``className`` is
used to name the class being defined. The parameter ``var`` is used to
name a local variable of type ``className``, which is defined by the
macro; it refers to the canonical storage for the class being defined.
This variable may be used in the initialization code. (The macro
doesn't just pick a name implicitly because of the danger of a name
clash with other names used by the programmer). A call to
``DEFINE_CLASS(SomeClass, var)`` defines the ``EnsureSomeClass()``
function, defines some static storage for the canonical class object,
and defines some other things to ensure the class gets initialized
exactly once.

.. mps:label:: int.define-alias-class

A convenience macro :c:func:`DEFINE_ALIAS_CLASS` is provided which
both performs the class definition and defines the types ``SomeClass``
and ``SomeClass struct`` as aliases for some other class types. This
is particularly useful for classes which simply inherit, and don't
extend protocols. The macro call ``DEFINE_ALIAS_CLASS(className,
superName, var)`` is exactly equivalent to the following::

     typedef superName className;
     typedef superNameStruct classNameStruct;
     DEFINE_CLASS(className, var)

.. mps:label:: int.define-special

If classes are particularly likely to be subclassed without extension,
the class implementor may choose to provide a convenience macro which
expands into :c:func:`DEFINE_ALIAS_CLASS` with an appropriate name for
the superclass. For example, there might be a macro for defining pool
classes such that the macro call ``DEFINE_POOL_CLASS(className, var)``
is exactly equivalent to the macro call
``DEFINE_ALIAS_CLASS(className, PoolClass, var)``. It may also be
convenient to define a static superclass accessor macro at the same
time (see :mps:label:`.int.static-superclass.special`).


Single inheritance
..................

.. mps:label:: int.inheritance

Class inheritance details must be provided in the class initialization
code (see :mps:label:`.int.define-class`). Inheritance is performed by
the macro ``INHERIT_CLASS(thisClassCoerced, parentClassName)``. A call
to this macro will make the class being defined a direct subclass of
``parentClassName`` by ensuring that all the fields of the parent
class are copied into ``thisClass``, and setting the superclass field
of ``thisClass`` to be the parent class object. The parameter
``thisClassCoerced`` must be of type ``parentClassName``. If the class
definition defines an alias class (see
:mps:label:`.int.define-alias-class`), then the variable named as the
second parameter to :c:func:`DEFINE_CLASS` will be appropriate to pass
to :c:func:`INHERIT_CLASS`.


Specialization
..............

.. mps:label:: int.specialize

Class specialization details must be given explicitly in the class
initialization code (see :mps:label:`.int.define-class`). This must
happen *after* the inheritance details are given (see
:mps:label:`.int.inheritance`).


Extension
.........

.. mps:label:: int.extend

To extend the protocol when defining a new class, a new type must be
defined for the class structure. This must embed the structure for the
primarily inherited class as the first field of the structure. Class
extension details must be given explicitly in the class initialization
code (see :mps:label:`.int.define-class`). This must happen *after*
the inheritance details are given (see :mps:label:`.int.inheritance`).


Introspection
.............

.. mps:label:: introspect.c-lang

The design includes a number of introspection functions for
dynamically examining class relationships. These functions are
polymorphic and accept arbitrary subclasses of
:c:type:`ProtocolClass`. C doesn't support such polymorphism. So
although these have the semantics of functions (and could be
implemented as functions in another language with compatible calling
conventions) they are actually implemented as macros. The macros are
named as method-style macros despite the fact that this arguably
contravenes :mps:label:`guide.impl.c.macro.method`. The justification
for this is that this design is intended to promote the use of
polymorphism, and it breaks the abstraction for the users to need to
be aware of what can and can't be expressed directly in C function
syntax. These functions all have names ending in ``Poly`` to identify
them as polymorphic functions.

.. mps:label:: int.superclass

``ProtocolClassSuperclassPoly(class)`` is an introspection function
which returns the direct superclass of class object class.

.. mps:label:: int.static-superclass

``SUPERCLASS(className)`` is an introspection macro which returns the
direct superclass given a class name, which must (obviously) be
statically known. The macro expands into a call to the ensure function
for the class name, so this must be in scope (which may require a
forward declaration). The macro is useful for next-method calls (see
:mps:label:`.overview.next-method`). The superclass is returned with
type :c:type:`ProtocolClass` so it may be necessary to cast it to the
type for the appropriate subclass.

.. mps:label:: int.static-superclass.special

Implementors of classes which are designed to be subclassed without
extension may choose to provide a convenience macro which expands into
a call to :c:func:`SUPERCLASS` along with a type cast. For example,
there might be a macro for finding pool superclasses such that the
macro call ``POOL_SUPERCLASS(className)`` is exactly equivalent to
``(PoolClass)SUPERCLASS(className)``. It's convenient to define these
macros alongside the convenience class definition macro (see
:mps:label:`.int.define-special`).

.. mps:label:: int.class

``ClassOfPoly(inst)`` is an introspection function which returns the
class of which inst is a direct instance.

.. mps:label:: int.subclass

``IsSubclassPoly(sub, super)`` is an introspection function which
returns a Boolean indicating whether sub is a subclass of super. That
is, it is a predicate for testing subclass relationships.


Multiple inheritance
....................

.. mps:label:: int.mult-inherit

Multiple inheritance involves an extension of the protocol (see
:mps:label:`.int.extend`) and also multiple uses of the single
inheritance mechanism (see :mps:label:`.int.inheritance`). It also
requires specialized methods for :c:func:`coerceClass` and
:c:func:`coerceInst` to be written (see
:mps:label:`.overview.coerce-class` and
:mps:label:`.overview.coerce-inst`). Documentation on support for
multiple inheritance is under construction. This facility is not
currently used. The basic idea is described in
mail.tony.1998-10-06.11-03(0).


Protocol guidelines
...................

.. mps:label:: guide.fail

When designing an extensible function which might fail, the design
must permit the correct implementation of the failure-case code.
Typically, a failure might occur in any method in the chain. Each
method is responsible for correctly propagating failure information
supplied by superclass methods and for managing it's own failures.

.. mps:label:: guide.fail.before-next

Dealing with a failure which is detected before any next-method call
is made is similar to a fail case in any non-extensible function. See
:mps:label:`.example.fail` below.

.. mps:label:: guide.fail.during-next

Dealing with a failure returned from a next-method call is also
similar to a fail case in any non-extensible function. See
:mps:label:`.example.fail` below.

.. mps:label:: guide.fail.after-next

Dealing with a failure which is detected after the next methods have
been successfully invoked is more complex. If this scenario is
possible, the design must include an "anti-function", and each class
must ensure that it provides a method for the anti-method which will
clean up any resources which are claimed after a successful invocation
of the main method for that class. Typically the anti-function would
exist anyway for clients of the protocol (for example, "finish" is an
anti-function for "init"). The effect of the next-method call can then
be cleaned up by calling the anti-method for the superclass. See
:mps:label:`.example.fail` below.


Example
.......

.. mps:label:: example.inheritance

The following example class definition shows both inheritance and
specialization. It shows the definition of the class
``EPDRPoolClass``, which inherits from ``EPDLPoolClass`` and has
specialized values of the ``name``, ``init``, and ``alloc`` fields.
The type ``EPDLPoolClass`` is an alias for :c:type:`PoolClass`. ::

    typedef EPDLPoolClass EPDRPoolClass;
    typedef EPDLPoolClassStruct EPDRPoolClassStruct;

    DEFINE_CLASS(EPDRPoolClass, this)
    {
        INHERIT_CLASS(this, EPDLPoolClass);
        this->name = "EPDR";
        this->init = EPDRInit;
        this->alloc = EPDRAlloc;
    }

.. mps:label:: example.extension

The following (hypothetical) example class definition shows
inheritance, specialization and also extension. It shows the
definition of the class ``EPDLDebugPoolClass``, which inherits from
``EPDLPoolClass``, but also implements a method for checking
properties of the pool. ::

    typedef struct EPDLDebugPoolClassStruct {
        EPDLPoolClassStruct epdl;
        DebugPoolCheckMethod check;
        Sig sig;
    } EPDLDebugPoolClassStruct;

    typedef EPDLDebugPoolClassStruct *EPDLDebugPoolClass;

    DEFINE_CLASS(EPDLDebugPoolClass, this)
    {
        EPDLPoolClass epdl = &this->epdl;
        INHERIT_CLASS(epdl, EPDLPoolClass);
        epdl->name = "EPDLDBG";
        this->check = EPDLDebugCheck;
        this->sig = EPDLDebugSig;
    }

.. mps:label:: example.fail

The following example shows the implementation of failure-case code
for an "init" method, making use of the "finish" anti-method::

    static Res mySegInit(Seg seg, Pool pool, Addr base, Size size, 
                         Bool reservoirPermit, va_list args)
    {
        SegClass super;
        MYSeg myseg;
        OBJ1 obj1;
        Res res;
        Arena arena;

        AVERT(Seg, seg);
        myseg = SegMYSeg(seg);
        AVERT(Pool, pool);
        arena = PoolArena(pool);

        /* Ensure the pool is ready for the segment */
        res = myNoteSeg(pool, seg);
        if(res != ResOK)
          goto failNoteSeg;

        /* Initialize the superclass fields first via next-method call */
        super = (SegClass)SUPERCLASS(MYSegClass);
        res = super->init(seg, pool, base, size, reservoirPermit, args);
        if(res != ResOK)
          goto failNextMethods;

        /* Create an object after the next-method call */
        res = ControlAlloc(&obj1, arena, sizeof(OBJ1Struct), reservoirPermit);
        if(res != ResOK)
          goto failObj1;

        myseg->obj1 = obj1
        return ResOK;

    failObj1:
        /* call the anti-method for the superclass */
        super->finish(seg);
    failNextMethods:
        /* reverse the effect of myNoteSeg */
        myUnnoteSeg(pool, seg);
    failNoteSeg:
        return res;
    }


Implementation
--------------

.. mps:label:: impl.derived-names

The :c:func:`DEFINE_CLASS` macro derives some additional names from
the class name as part of it's implementation. These should not appear
in the source code - but it may be useful to know about this for
debugging purposes. For each class definition for class ``SomeClass``,
the macro defines the following:

``extern SomeClass EnsureSomeClass(void);``

    The class accessor function. See :mps:label:`.overview.naming`.

``static Bool protocolSomeClassGuardian;``

    A Boolean which indicates whether the class has been initialzed yet.

``static void protocolEnsureSomeClass(SomeClass);``

    A function called by ``EnsureSomeClass``. All the class
    initialization code is actually in this function.

``static SomeClassStruct protocolSomeClassStruct;``

    Static storage for the canonical class object.

.. mps:label:: impl.init-once

Class objects only behave according to their definition after they
have been initialized, and class protocols may not be used before
initialization has happened. The only code which is allowed to see a
class object in a partially initialized state is the initialization
code itself -- and this must take care not to pass the object to any
other code which might assume it is initialized. Once a class has been
initialized, the class might have a client. The class must not be
initialized again when this has happened, because the state is not
necessarily consistent in the middle of an initialization function.
The initialization state for each class is stored in a Boolean
"guardian" variable whose name is derived from the class name (see
:mps:label:`.impl.derived-names`). This ensures the initialization
happens only once. The path through the ``EnsureSomeClass`` function
should be very fast for the common case when this variable is
``TRUE``, and the class has already been initialized, as the canonical
static storage can simply be returned in that case. However, when the
value of the guardian is ``FALSE``, the class is not initialized. In
this case, a call to ``EnsureSomeClass`` must first execute the
initialization code and then set the guardian to ``TRUE``. However,
this must happen atomically (see :mps:label:`.impl.init-lock`).

.. mps:label:: impl.init-lock

There would be the possibility of a race condition if
``EnsureSomeClass`` were called concurrently on separate threads
before ``SomeClass`` has been initialized. The class must not be
initialized more than once, so the sequence test-guard, init-class,
set-guard must be run as a critical region. It's not sufficient to use
the arena lock to protect the critical region, because the class
object might be shared between multiple arenas. The
:c:func:`DEFINE_CLASS` macro uses a global recursive lock instead. The
lock is only claimed after an initial unlocked access of the guard
variable shows that the class is not initialized. This avoids any
locking overhead for the common case where the class is already
initialized. This lock is provided by the lock module -- see
:mps:label:`design.mps.lock(0)`.

