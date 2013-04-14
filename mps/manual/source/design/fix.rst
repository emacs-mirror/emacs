.. sources:

    `<https://info.ravenbrook.com/project/mps/master/design/bt/>`_

.. mps:prefix:: design.mps.fix

The generic fix function
------------------------


Introduction
-------------

Fix is the interface through which the existence of references are
communicated from the MPS client to the MPS. The interface also allows
the value of such references to be changed (this is necessary in order
to implement a moving memory manager).


History
-------

:mps:tag:`hist.0` Incomplete design. Richard Brooksby, 1995-08-25.

:mps:tag:`hist.1` Converted from MMInfo database design document.
Richard Brooksby, 2002-06-07.

:mps:tag:`hist.2` Converted to reStructuredText. Gareth Rees,
2013-04-14.


Architecture
-------------

:mps:tag:`protocol.was-marked` The :c:type:`ScanState` has a
:c:type:`Bool` ``wasMarked`` field. This is used for finalization.

:mps:tag:`protocol.was-marked.set` All pool-specific fix methods must
set the ``wasMarked`` field in the :c:type:`ScanState` that they are
passed.

:mps:tag:`protocol.was-marked.meaning` If the pool-specific fix method
sets the ``wasMarked`` field to ``FALSE`` it is indicating the object
referred to by the ref (the one that it is supposed to be fixing) has
not previously been marked (ie, this is the first reference to this
object that has been fixed), and that the object was white (in
condemned space).

:mps:tag:`protocol.was-marked.conservative` It is always okay to set
the ``wasMarked`` field to ``TRUE``.

:mps:tag:`protocol.was-marked.finalizable` The MRG pool
(:mps:ref:`design.mps.poolmrg`) uses the value of the ``wasMarked``
field to determine whether an object is finalizable.


Implementation
---------------

:mps:tag:`fix.nailed` In a copying collection, a non-ambiguous fix to
a broken heart should be snapped out *even if* there is a
:c:macro:`RankAMBIG` ref to same object (that is, if the broken heart
is nailed); the :c:macro:`RankAMBIG` reference must either be stale
(no longer in existence) or bogus.
