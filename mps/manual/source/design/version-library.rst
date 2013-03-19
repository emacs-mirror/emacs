.. sources:

    `<https://info.ravenbrook.com/project/mps/master/design/version-library/>`_

.. mps:prefix:: design.mps.version-library

Library version mechanism
=========================


Introduction
------------

:mps:tag:`intro` This describes the design of a mechanism to be used
to determine the version (that is, product, version, and release) of
an MPS library.


History
-------

:mps:tag:`hist.0` Incomplete document. David Jones, 1998-08-19.

:mps:tag:`hist.1` Converted from MMInfo database design document.
Richard Brooksby, 2002-06-07.

:mps:tag:`hist.2` Converted to reStructuredText. Gareth Rees,
2013-03-11.


Readership
----------

:mps:tag:`readership` Any MPS developer.


Source
------

:mps:tag:`source` Various requirements demand such a mechanism. See
request.epcore.160021: There is no way to tell which version and
release of the MM one is using.


Overview
--------

:mps:tag:`overview` See :mps:ref:`design.mps.version` for discussion
and design of versions of other aspects of the software. This document
concentrates on a design for determining which version of the library
one has linked with. There are two aspects to the design, allowing
humans to determine the version of an MPS library, and allowing
programs to determine the version of an MPS library. Only the former
is currently designed (a method for humans to determine which version
of an MPS library is being used).

:mps:tag:`overview.impl` The overall design is to have a distinctive
string compiled into the library binary. Various programs and tools
will be able to extract the string and display it. The string will
identify the version of the MPS begin used.


Architecture
------------

:mps:tag:`arch.structure` The design consists of three components:

1. :mps:tag:`arch.string` A string embedded into any delivered library
   binaries (which will encode the necessary information).

2. :mps:tag:`arch.proc` A process by which the string is modified
   appropriately whenever releases are made.

3. :mps:tag:`arch.tool` A tool and its documentation (it is expected
   that standard tools can be used). The tool will be used to extract
   the version string from a delivered library or an executable linked
   with the library.

:mps:tag:`arch.not-here` Only the string component
(:mps:ref:`arch.string`) is directly described here. The other
components are described elsewhere. (where?)

The string will contain information to identify the following items:

1. :mps:tag:`arch.string.platform` the platform being used.

2. :mps:tag:`arch.string.product` the name of the product.

3. :mps:tag:`arch.string.variety` the variety of the product.

4. :mps:tag:`arch.string.version` the version and release of the product.


Implementation
--------------

:mps:tag:`impl.file` The string itself is a declared C object in the
file ``version.c`` (:mps:ref:`impl.c.version`). It consists of a
concatenation of various strings which are defined in other modules.

:mps:tag:`impl.variety` The string containing the name of the variety
is the expansion of the macro ``MPS_VARIETY_STRING`` defined by
``config.h`` (:mps:ref:`impl.h.config`).

:mps:tag:`impl.product` The string containing the name of the product
is the expansion of the macro ``MPS_PROD_STRING`` defined by
``config.h`` (:mps:ref:`impl.h.config`).

:mps:tag:`impl.platform` The string containing the name of the
platform is the expansion of the macro ``MPS_PF_STRING`` defined by
``mpstd.h`` (:mps:ref:`impl.h.mpstd`).

:mps:tag:`impl.date` The string contains the date and time of
compilation by using the ``__DATE__`` and ``__TIME__`` macros defined
by ISO C §6.8.8.

:mps:tag:`impl.version` The string contains the version and release of
the product. This is by the expansion of the macro ``MPS_RELEASE``
which is defined in this module (``version.c``).

:mps:tag:`impl.usage` To make a release, the ``MPS_RELEASE`` macro
(see :mps:ref:`impl.c.version.release`) is edited to contain the
release name (for example, ``"release.epcore.brisling"``), and then
changed back immediately after the release checkpoint is made.
