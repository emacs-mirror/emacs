.. sources:

    `<https://info.ravenbrook.com/project/mps/master/design/version/>`_

.. mps:prefix:: design.mps.version

Software versions
=================


Introduction
------------

:mps:tag:`intro` This is the design of the support in the MPS for
describing and inspecting versions.


History
-------

:mps:tag:`hist.0` Incomplete document. David Jones, 1998-08-19.

:mps:tag:`hist.1` Converted from MMInfo database design document.
Richard Brooksby, 2002-06-07.

:mps:tag:`hist.2` Converted to reStructuredText. Gareth Rees,
2013-03-11.


Overview
--------

:mps:tag:`overview` There are three different sorts of version under
consideration:

1. versions of the (MPS) library used (linked with);

2. versions of the interface used (header files in C) when compiling
   the client's program; and

3. versions of the documentation used when the client was writing the
   program.

There are issues of programmatic and human access to these versions.

:mps:tag:`overview.split` The design is split accordingly. See
:mps:ref:`design.mps.version-library` for the design of a system for
determining the version of the library one is using. And other
non-existent documents for the others.
