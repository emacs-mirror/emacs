.. sources:

    `<https://info.ravenbrook.com/project/mps/master/design/config/>`_

.. mps:prefix:: design.mps.config

Configuration
=============


Introduction
------------

:mps:tag:`intro` This document describes how the MPS configuration is
parameterized so that it can target different architectures, operating
systems, build environments, varieties, and products.

:mps:tag:`bg` For background see [build system mail, configuration
mail, :mps:ref:`meeting.general.something`]


History
-------

:mps:tag:`hist.0` Initial draft created by Richard Brooksby on
1997-02-19 based on discussions of configuration at
:mps:ref:`meeting.general.1997-02-05`.

:mps:tag:`hist.1` Various improvements and clarifications to the draft
discussed between Richard and Nick Barnes at
:mps:ref:`meeting.general.1997-02-19`.

:mps:tag:`hist.2` Converted from MMInfo database design document.
Richard Brooksby, 2002-06-07.

:mps:tag:`hist.3` Updated for variety-reform branch, to remove untrue
things, though the document could do with a rewrite. Richard Brooksby,
2012-09-03.

:mps:tag:`hist.4` Converted to reStructuredText. Gareth Rees,
2013-03-19.


Requirements
------------

:mps:tag:`req.arch` Allow architecture specific configurations of the
MPS.

:mps:tag:`req.os` Allow operating system specific configurations of
the MPS.

:mps:tag:`req.builder` Allow build environment (compiler, etc.)
specific configurations of the MPS.

:mps:tag:`req.prod` Allow product specific configurations of the MPS.
[This requirement was retired on 2012-09-03. Client-specific
customisation of the MPS will be handled by configuration management,
while the MPS source remains generic, to reduce costs and increase
reliability.]

:mps:tag:`req.var` Allow configurations with different amounts of
instrumentation (assertions, metering, etc.).

:mps:tag:`req.impact` The configuration system should have a minimal
effect on maintainability of the implementation.

:mps:tag:`req.port` The system should be easy to port across operating
systems.

:mps:tag:`req.maint` Maintenance of the configuration and build system
should not consume much developer time.


Definitions
-----------

:mps:tag:`def.platform` A platform is a combination of an architecture
(:mps:ref:`.def.arch`), an operating system (:mps:ref:`.def.os`), and
a builder (:mps:ref:`.def.builder`). The set of supported platforms is
platform.*.

:mps:tag:`def.arch` An architecture is processor type with associated
calling conventions and other binary interface stuff.

:mps:tag:`def.os` An operating system is the interface to external
resources.

:mps:tag:`def.builder` A builder is the tools (C compiler, etc.) used
to make the target (:mps:ref:`.def.target`).

:mps:tag:`def.var` A variety is a combination of annotations such as
assertions, metering, etc.

:mps:tag:`def.prod` A product is the intended product into which the
MPS will fit, e.g. ScriptWorks, Dylan, etc.

:mps:tag:`def.target` The target is the result of the build.


Overview
--------

- No automatically generated code. Use only C compiler and linker.
- Simple build function (:mps:ref:`design.mps.buildsys`).
- Avoid conditional code spaghetti in implementations.
- Dependency on a particular configuration should be minimized and
  localized when developing code.


The build system
----------------

Abstract Build Function
.......................

:mps:tag:`build.fun` The MPS implementation assumes only a simple
"build function" which takes a set of sources, possibly in several
languages, compiles them with a set of predefined preprocessor
symbols, and links the result with a set of libraries to form the
target::

    target := build(<defs>, <srcs>, <libs>)

:mps:tag:`build.sep` Separate compilation and linkage can be seen as a
memoization of this function, and is not strictly necessary for the
build.

:mps:tag:`build.cc` A consequence of this approach is that it should
always be possible to build a complete target with a single UNIX
command line calling the compiler driver (usually ``cc`` or ``gcc``),
for example::

    cc -o main -DCONFIG_VAR_DF foo.c bar.c baz.s -lz

:mps:tag:`build.defs` The "defs" are the set of preprocessor macros
which are to be predefined when compiling the module sources, of the
form::

    CONFIG_VAR_<variety-code>

A variety-code is a code that appears after ``variety.`` in the tag of
the relevant variety document (see :mps:ref:`design.mps.variety`),
converted to upper case. Currently (2012-09-03) the valid variety
codes are ``RASH``, ``HOT``, ``COOL``, ``DIAG``, and ``TI``.

If no ``CONFIG_VAR_`` is present, ``HOT`` is assumed in ``config.h``.

:mps:tag:`build.srcs` The "srcs" are the set of sources that must be
compiled in order to build the target. The set of sources may vary
depending on the configuration. For example, different sets of sources
may be required to build on different architectures. [This is a
dependency between the makefile (or whatever) and the module
configuration in ``config.h``.]

:mps:tag:`build.libs` The "libs" are the set of libraries to which the
compiled sources must be linked in order to build the target. For
example, when building a test program, it might include the ANSI C
library and an operating system interface library.


File Structure
..............

:mps:tag:`file.dir` Each product consists of a single directory
(corresponding to a HOPE compound) containing all the sources for the
whole family of targets.

:mps:tag:`file.base` The names of sources must be unique in the first
eight characters in order to conform to FAT filesystem naming
restrictions.

:mps:tag:`.file.ext` The extension may be up to three characters and
directly indicates the source language.

[Where is the set of valid extensions and languages defined?]


Modules and Naming
..................

:mps:tag:`mod.unique` Each module has an identifier which is unique
within the MPS.

:mps:tag:`mod.impls` Each module has one or more implementations which
may be in any language supported by the relevant build environment.

:mps:tag:`mod.primary` The primary implementation of a module is
written in target-independent ANSI C in a source file with the same
name as the module (plus the ``an`` suffix if there are secondary
implementations: see :mps:ref:`.mod.secondary`).

:mps:tag:`.mod.secondary` The names of other implementations should
begin with the same prefix (the module identifier, or a shortened
version of it) and be suffixed with on or more target parameter codes
(defined below). In particular, the names of assembly language sources
must include the target parameter code for the relevant architecture.


Build System Rationale
......................

:mps:tag:`build.rat` This simple design makes it possible to build the
MPS using many different tools. Microsoft Visual C++, Metrowerks
Codewarrior, and other graphical development tools do not support much
in the way of generated sources, staged building, or other such stuff.
The Visual C and Metrowerks "project" files correspond closely to a
closure of the build function (:mps:ref:`.build.fun`). The simplicity
of the build function has also made it easy to set up builds using
NMAKE (DOS), MPW (Macintosh), and to get the MPS up and running on
other platforms such as FreeBSD and Linux in very little time. The
cost of maintaining the build systems on these various platforms is
also reduced to a minimum, allowing the MM Group to concentrate on
primary development. The source code is kept simple and
straightforward. When looking at MPS sources you can tell exactly what
is going to be generated with very little context. The sources are not
munged beyond the standard ANSI C preprocessor.

:mps:tag:`build.port` The portability requirement
(:mps:ref:`.req.port`) implies that the build system must use only
standard tools that will be available on all conceivable target
platforms. Experience of development environments on the Macintosh
(Metrowerks Codewarrior) and Windows NT (Visual C++) indicates that we
cannot assume much sophistication in the use of file structure by
development environments. The best that we can hope for is the ability
to combine a fixed list of source files, libraries, and predefined
preprocessor symbols into a single target.

:mps:tag:`build.maint` The maintainability requirement
(:mps:ref:`.req.maint`) implies that we don't spend time trying to
develop a set of tools to support anything more complicated than the
simple build function described above. The effort in constructing and
maintaining a portable system of this kind is considerable. Such
efforts have failed in EP.


Implementation
--------------

:mps:tag:`impl` The two implementation files :mps:ref:`impl.h.config`
and :mps:ref:`impl.h.mpstd` can be seen as preprocessor programs which
"accept" build parameters and "emit" configuration parameters
(:mps:ref:`.fig.impl`). The build parameters are defined either by the
builder (in the case of target detection) or by the build function (in
the case of selecting the variety).

===================  ============  =======================================
Build parameter →    Header →      Configuration parameters
===================  ============  =======================================
``CONFIG_VAR_COOL``  ``config.h``  ``AVER_AND_CHECK``, ``EVENT_ALL``, etc.
-------------------  ------------  ---------------------------------------
``_WIN32``           ``mpstd.h``   ``MPS_OS_W3``, ``MPS_BUILD_MV``, etc.
===================  ============  =======================================

:mps:tag:`impl.dep` No source code, other than the directives in
:mps:ref:`impl.h.config` and :mps:ref:`impl.h.mpstd`, should depend on
any build parameters. That is, identifiers beginning ``CONFIG_``
should only appear in :mps:ref:`impl.h.config`. Code may depend on
configuration parameters in certain, limited ways, as defined
below (:mps:ref:`.conf`).


Target Platform Detection
.........................

:mps:tag:`pf` The target platform is "detected" by the preprocessor
directives in :mps:ref:`impl.h.mpstd`.

:mps:tag:`pf.form` This file consists of sets of directives of the
form::

    #elif <conjunction of builder predefinitions>
    #define MPS_PF_<platform code>
    #define MPS_OS_<operating system code>
    #define MPS_ARCH_<architecture code>
    #define MPS_BUILD_<builder code>
    #define MPS_T_WORD      <word type>
    #define MPS_WORD_SHIFT  <word shift>
    #define MPS_PF_ALIGN    <minimum alignment>

:mps:tag:`pf.detect` The conjunction of builder predefinitions is a
constant expression which detects the target platform. It is a logical
conjunction of expressions which look for preprocessor symbols defined
by the build environment to indicate the target. These must be
accompanied by a reference to the build tool documentation from which
the symbols came. For example::

    /* Visual C++ 2.0, Books Online, C/C++ Book, Preprocessor Reference,
     * Chapter 1: The Preprocessor, Macros, Predefined Macros.
     */

    #elif defined(_MSC_VER) && defined(_WIN32) && defined(_M_IX86)

:mps:tag:`pf.codes` The declarations of the platform, operating
system, architecture, and builder codes define preprocessor macros
corresponding the the target detected (:mps:ref:`.pfm.detect`). For
example::

    #define MPS_PF_W3I3MV
    #define MPS_OS_W3
    #define MPS_ARCH_I3
    #define MPS_BUILD_MV

:mps:tag:`pf.word` The declaration of :c:macro:`MPS_T_WORD` defines
the unsigned integral type which corresponds, on the detected target,
to the machine word. It is used to defined the MPS :c:type:`Word` type
(:mps:ref:`design.mps.type.word`). For example::

    #define MPS_T_WORD unsigned long

:mps:tag:`pf.word-width` The declaration of :c:macro:`MPS_WORD_WIDTH`
defines the number of bits in the type defined by
:c:macro:`MPS_T_WORD` (:mps:ref:`.pf.word`) on the target. For
example::

    #define MPS_WORD_WIDTH 32

:mps:tag:`pf.word-shift` The declaration of :c:macro:`MPS_WORD_SHIFT`
defines the base-2 logarithm of :c:macro:`MPS_WORD_WIDTH`. For
example::

    #define MPS_WORD_SHIFT 5

:mps:tag:`pf.pf-align` The declaration of :c:macro:`MPS_PF_ALIGN`
defines the minimum alignment which must be used for a memory block to
permit any normal processor memory access. In other words, it is the
maximum alignment required by the processor for normal memory access.
For example::

    #define MPS_PF_ALIGN 4


Target varieties
................

:mps:tag:`var` The target variety is handled by preprocessor directives in 
:mps:ref:`impl.h.config`.

:mps:tag:`var.form` The file contains sets of directives of the form::

    #elif defined(CONFIG_VAR_DF)
    #define MPS_VAR_DF
    #define ASSERT_MPSI
    #define ASSERT_MPM
    etc.

:mps:tag:`var.detect` The configured variety is one of the variety
preprocessor definitions passed to the build function
(:mps:ref:`.build.defs`), for example, ``CONFIG_VAR_DF``. [These are
decoupled so that it's possible to tell the difference between
overridden settings etc. Explain.]

:mps:tag:`var.symbols` The directives should define whatever symbols
are necessary to control annotations. These symbols parameterize other
parts of the code, such as the declaration of assertions, etc. The
symbols should all begin with the prefix ``MPS_VAR_``.

[Tidy this up:] Note, anything which can be configured, is configured,
even if it's just configured to ``NONE`` meaning nothing. This makes
sure that you can't choose something by omission. Where these symbols
are used there will be an ``#error`` to catch the unused case.
Exception: To allow simple building of the MPS with ``cc -c mps.c`` we
choose :c:macro:`CONFIG_VAR_HOT` by default. [This is a general
principle which applies to other configuration stuff too.]


Source code configuration
-------------------------

:mps:tag:`conf` This section describes how the configuration may
affect the source code of the MPS.

:mps:tag:`conf.limit` The form of dependency allowed is carefully
limited to ensure that code remains maintainable and portable
(:mps:ref:`.req.impact`).

:mps:tag:`conf.min` The dependency of code on configuration parameters
should be kept to a minimum in order to keep the system maintainable
(:mps:ref:`.req.impact`).


Configuration parameters
........................

:mps:tag:`conf.params` The compilation of a module is parameterized
by::

    MPS_ARCH_<arch-code>
    MPS_OS_<os-code>
    MPS_BUILDER_<builder-code>
    MPS_PF_<platform-code>


Abstract and concrete module interfaces
.......................................

Basic principle: the caller must not be affected by configuration of a
module. This reduces complexity and dependency of configuration.

All callers use the same abstract interface. Caller code does not
change.

Abstract interface includes:

- method definitions (logical function prototypes which may be macro
  methods)
- names of types
- names of constants
- names of structures and fields which form part of the interface, and
  possibly their types, depending on the protocol defined
- the protocols

The abstract interface to a module may not be altered by a configuration 
parameter.  However, the concrete interface may vary.


Configuring module implementations
..................................

For example, this isn't allowed, because there is a change in the
interface::

    #if defined(PROT_FOO)
    void ProtSpong(Foo foo, Bar bar);
    #else
    int ProtSpong(Bar bar, Foo foo);
    #endif

This example is allowed::

    #ifdef PROTECTION
    void ProtSync(Space space);
    /* more decls. */
    #else /* PROTECTION not */
    #define ProtSync(space) NOOP
    /* more decls. */
    #endif /* PROTECTION */

And so is this::

    #if defined(PROT_FOO)
    typedef struct ProtStruct {
      int foo;
    } ProtStruct;
    #define ProtSpong(prot)  X((prot)->foo)
    #elif defined(PROT_BAR)
    typedef struct ProtStruct {
      float bar;
    } ProtStruct;
    #define ProtSpong(prot)  Y((prot)->bar)
    #else
    #error "No PROT_* configured."
    #endif

Configuration parameters may not be used to vary implementations in
``.c`` files. For example, this sort of thing is not allowed::

    int map(void *base, size_t size)
    {
    #if defined(MPS_OS_W3)
      VirtualAlloc(foo, bar, base, size);
    #elif defined(MPS_OS_SU)
      mmap(base, size, frob);
    #else
    #error "No implementation of map."
    #endif
    }

This leads to extreme code spaghetti. In effect, it's a "candy machine
interface" on source code. This kind of thing should be done by having
several implementations of the same interface in separate source
files. If this leads to duplication of code then that code should be
placed in a separate, common module.


Procedures
----------

[Adding an architecture, etc.]


Notes
-----

What about constants?
