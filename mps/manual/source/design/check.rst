.. sources:

    `<https://info.ravenbrook.com/project/mps/master/design/check/>`_

.. mps:prefix:: design.mps.check

Checking
========


Introduction
------------

This documents the design of structure checking within the MPS.


History
-------

:mps:tag:`hist.0` Incomplete design. Gavin Matthews, 1996-08-05.

:mps:tag:`hist.1` Converted from MMInfo database design document.
Richard Brooksby, 2002-06-07.

:mps:tag:`hist.2` Converted to reStructuredText. Gareth Rees,
2013-03-12.


Implementation
--------------

:mps:tag:`level` There are three levels of checking:

1. :mps:tag:`level.sig` The lowest level checks only that the
   structure has a valid :c:type:`Signature` (see
   :mps:ref:`design.mps.sig`).

2. :mps:tag:`level.shallow` Shallow checking checks all local fields
   (including signature) and also checks the signatures of any parent
   or child structures.

3. :mps:tag:`level.deep` Deep checking checks all local fields
   (including signatures), the signatures of any parent structures,
   and does full recursive checking on any child structures.

:mps:tag:`level.control` Control over the levels of checking is via
the definition of at most one of the macros
:c:macro:`TARGET_CHECK_SHALLOW` (which if defined gives
:mps:ref:`.level.shallow`), :c:macro:`TARGET_CHECK_DEEP` (which if
defined gives :mps:ref:`.level.deep`). If neither macro is defined
then :mps:ref:`.level.sig` is used. These macros are not intended to
be manipulated directly by developers, they should use the interface
in :mps:ref:`impl.h.target`.

:mps:tag:`order` Because deep checking (:mps:ref:`.level.deep`) uses
unchecked recursion, it is important that child relationships are
acyclic (:mps:ref:`.macro.down`).

:mps:tag:`fun` Every abstract data type which is a structure pointer
should have a function ``<type>Check`` which takes a pointer of type
``<type>`` and returns a :c:type:`Bool`. It should check all fields in
order, using one of the macros in :mps:ref:`.macro`, or document why
not.

:mps:tag:`fun.omit` The only fields which should be omitted from a
check function are those for which there is no meaningful check (for
example, an unlimited unsigned integer with no relation to other fields).

:mps:tag:`fun.return` Although the function returns a :c:type:`Bool`,
if the assert handler returns (or there is no assert handler), then
this is taken to mean "ignore and continue", and the check function
hence returns ``TRUE``.

:mps:tag:`macro` Checking is implemented by invoking four macros in :mps:ref:`impl.h.assert`:

* :mps:tag:`macro.sig` ``CHECKS(type, val)`` checks the signature
  only, and should be called precisely on ``type`` and the received
  object pointer.

* :mps:tag:`macro.local` ``CHECKL(cond)`` checks a local field
  (depending on level; see :mps:ref:`.level`), and should be called on
  each local field that is not an abstract data type structure pointer
  itself (apart from the signature), with an appropriate normally-true
  test condition.

* :mps:tag:`macro.up` ``CHECKU(type, val)`` checks a parent abstract
  data type structure pointer, performing at most signature checks
  (depending on level; see :mps:ref:`.level`). It should be called
  with the parent type and pointer.

* :mps:tag:`macro.down` ``CHECKD(type, val)`` checks a child abstract
  data type structure pointer, possibly invoking ``<type>Check``
  (depending on level; see :mps:ref:`.level`). It should be called
  with the child type and pointer.

:mps:tag:`full-type` ``CHECKS``, ``CHECKD``, ``CHECKU``, all operate
only on fully fledged types. This means the type has to provide a
function ``Bool TypeCheck(Type type)`` where ``Type`` is substituted
for the name of the type (for example, :c:func:`PoolCheck`), and the
expression ``obj->sig`` must be a valid value of type :c:type:`Sig`
whenever ``obj`` is a valid value of type ``Type``.

:mps:tag:`type.no-sig` This tag is to be referenced in implementations
whenever the form ``CHECKL(ThingCheck(thing))`` is used instead of
``CHECK{U,D}(Thing, thing)`` because ``Thing`` is not a fully fledged
type (:mps:ref:`.full-type`).
