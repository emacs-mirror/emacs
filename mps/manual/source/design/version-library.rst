.. sources:

    `<https://info.ravenbrook.com/project/mps/master/design/version-library/>`_

.. mps:prefix:: design.mps.version-library

Library version mechanism
=========================


Introduction
------------

.. mps:label:: intro

This describes the design of a mechanism to be used to determine the 
version (that is, product, version, and release) of an MPS library.


Readership
----------

.. mps:label:: readership

Any MPS developer.


Source
------

.. mps:label:: source

Various requirements demand such a mechanism.  See 
request.epcore.160021: There is no way to tell which version and release of the 
MM one is using.


Overview
--------

.. mps:label:: overview

See :mps:label:`design.mps.version` for discussion and design of
versions of other aspects of the software. This document concentrates
on a design for determining which version of the library one has
linked with. There are two aspects to the design, allowing humans to
determine the version of an MPS library, and allowing programs to
determine the version of an MPS library. Only the former is currently
designed (a method for humans to determine which version of an MPS
library is being used).

.. mps:label:: overview.impl

The overall design is to have a distinctive string compiled into the
library binary. Various programs and tools will be able to extract the
string and display it. The string will identify the version of the MPS
begin used.


Architecture
------------

.. mps:label:: arch.structure

The design consists of three components:

1. .. mps:label:: arch.string

   A string embedded into any delivered library binaries (which will 
   encode the necessary information).

2. .. mps:label:: arch.proc

   A process by which the string is modified appropriately whenever 
   releases are made.

3. .. mps:label:: arch.tool

   A tool and its documentation (it is expected that standard tools 
   can be used).  The tool will be used to extract the version string from a 
   delivered library or an executable linked with the library.

.. mps:label:: arch.not-here

Only the string component (:mps:label:`arch.string`) is directly
described here. The other components are described elsewhere. (where?)

The string will contain information to identify the following items:

1. .. mps:label:: arch.string.platform

   the platform being used.

2. .. mps:label:: arch.string.product

   the name of the product.

3. .. mps:label:: arch.string.variety

   the variety of the product.

4. .. mps:label:: arch.string.version

   the version and release of the product.


Implementation
--------------

.. mps:label:: impl.file

The string itself is a declared C object in the file ``version.c``
(:mps:label:`impl.c.version`). It consists of a concatenation of
various strings which are defined in other modules.

.. mps:label:: impl.variety

The string containing the name of the variety is the expansion of the
macro ``MPS_VARIETY_STRING`` defined by ``config.h``
(:mps:label:`impl.h.config`).

.. mps:label:: impl.product

The string containing the name of the product is the expansion of the
macro ``MPS_PROD_STRING`` defined by ``config.h``
(:mps:label:`impl.h.config`).

.. mps:label:: impl.platform

The string containing the name of the platform is the expansion of the
macro ``MPS_PF_STRING`` defined by ``mpstd.h``
(:mps:label:`impl.h.mpstd`).

.. mps:label:: impl.date

The string contains the date and time of compilation by using the
``__DATE__`` and ``__TIME__`` macros defined by ISO C §6.8.8.

.. mps:label:: impl.version

The string contains the version and release of the product. This is by
the expansion of the macro ``MPS_RELEASE`` which is defined in this
module (``version.c``).

.. mps:label:: impl.usage

To make a release, the ``MPS_RELEASE`` macro (see
:mps:label:`impl.c.version.release`) is edited to contain the release
name (for example, ``"release.epcore.brisling"``), and then changed
back immediately after the release checkpoint is made.
