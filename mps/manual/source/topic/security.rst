.. index::
   single: security issues

.. _topic-security:

Security issues
===============

This chapter describes security issues that may be present when using
the MPS.


.. index::
   pair: security issues; predictable address space layout on FreeBSD
   single: address space; predictable layout on FreeBSD

Predictable address space layout on FreeBSD
-------------------------------------------

The MPS acquires :term:`address space` using the operating system's
:term:`virtual memory` interface (specifically, :c:func:`mmap` on
FreeBSD). As of version 10, FreeBSD does not randomize the allocated
regions of address space, which means that the :term:`addresses` of
:term:`blocks` allocated by the MPS are predictable: a :term:`client
program` that makes an identical series of calls to the MPS gets an
identical series of addresses back.

This means that if a program using the MPS has a buffer overflow, the
overflow is more easily exploitable by an attacker than if the program
had used :c:func:`malloc` (which has some randomization of the
allocated addresses), because it is easier for an attacker to
determine the address of allocated structures.

There is currently no workaround for this issue. If this affects you,
please :ref:`contact us <contact>`.

Other supported platforms are unaffected by this issue: Linux and OS X
randomize the addresses allocated by :c:func:`mmap`, and Windows
randomizes the addresses allocated by :c:func:`VirtualAlloc`.


.. index::
   pair: security issues; address disclosure

Address disclosure
------------------

The MPS supports :term:`semi-conservative garbage collection` in which
some memory locations are :term:`scanned <scan>` as :term:`ambiguous
references`. This may make it possible for a program to discover the
:term:`address` of an :term:`object`, even if the programming language
has no feature for obtaining the address of an object. Discovering the
addresses of objects makes it easier to exploit buffer overflow bugs.

The attack proceeds as follows: create a :term:`weak reference (1)` to
the object of interest (for example, via a :term:`weak-key hash
table`); guess a value for the address of the object; and arrange for
that value to be scanned as an ambiguous reference (for example, by
ensuring that it appears in :term:`registers` or on the :term:`control
stack` of a :term:`thread`). If the guess was correct, the MPS keeps
the object :term:`alive`; if incorrect, the object may :term:`die
<dead>`. The attacker can then determine which of these was the case
by examining the weak reference to see if it has been
:term:`splatted <splat>`.

The attack was pointed out by `Dionysus Blazakis in 2012
<https://github.com/justdionysus/gcwoah>`_ with respect to JavaScript
implementations, but it affects all :term:`conservative <conservative
garbage collection>` and :term:`semi-conservative <semi-conservative
garbage collection>` garbage collectors.


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
