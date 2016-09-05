.. index::
   single: security issues

.. _topic-security:

Security issues
===============

This chapter describes security issues that may be present when using
the MPS.


.. index::
   pair: security issues; predictable address space layout
   single: address space; predictable layout

Predictable address space layout
--------------------------------

The MPS acquires :term:`address space` using the operating system's
:term:`virtual memory` interface (specifically, :c:func:`mmap` on Unix
platforms, and :c:func:`VirtualAlloc` on Windows). None of the
supported platforms randomize the allocated regions of address space,
which means that the :term:`addresses` of :term:`blocks` allocated by
the MPS are predictable: a :term:`client program` that makes an
identical series of calls to the MPS gets an identical series of
addresses back.

This means that if a program using the MPS has a buffer overflow, the
overflow is more easily exploitable by an attacker than if the program
had used :c:func:`malloc` (which has some randomization of the
allocated addresses), because it is easier for an attacker to
determine the address of allocated structures.

There is currently no workaround for this issue. If this affects you,
please :ref:`contact us <contact>`.


.. index::
   pair: security issues; telemetry

Telemetry
---------

In its :term:`hot` and :term:`cool` varieties, the MPS contains a
:term:`telemetry system` which can be configured to record a stream of
events for later analysis and debugging. When using the default
:term:`plinth`, the behaviour of the telemetry system is under the
control of the environment variable :envvar:`MPS_TELEMETRY_CONTROL`,
and the telemetry stream is written to the file named by the
environment variable :envvar:`MPS_TELEMETRY_FILENAME`.

This means that an attacker who can set arbitrary environment
variables when running a program that uses the MPS can cause that
program to write a telemetry stream to an arbitrary file. This
behaviour might be unexpected, and might enable a data overwriting
attack, or a denial-of-service attack, since telemetry streams are
typically very large.

If this is an issue for your program, then you can modify or replace
the :ref:`topic-plinth-io` in the :term:`plinth` so that it meets your
requirements, or distribute the :term:`rash` variety of the MPS, which
omits the :term:`telemetry system` entirely, and use the other
varieties only for development and testing.
