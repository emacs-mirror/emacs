.. _lang:

Memory management in various languages
**************************************

.. glossary::

    ALGOL

        ALGOL, designed in 1958 for scientific computing, was the
        first block-structured language. It spawned a whole family of
        languages, and inspired many more, including :term:`Scheme`,
        :term:`Simula` and :term:`Pascal`.

        The block structure of ALGOL 60 induced a :term:`stack
        allocation` discipline. It had limited dynamic arrays, but no
        general :term:`heap allocation`. The substantially redesigned
        ALGOL 68 had both heap and stack allocation. It also had
        something like the modern :term:`pointer` type, and required
        :term:`garbage collection` for the heap. The new language was
        complex and difficult to implement, and it was never as
        successful as its predecessor.

        .. bibref:: [BL72]_.

    BASIC

        BASIC is a simple and easily-learned programming language
        developed by T. E. Kurtz and J. G. Kemeny in 1963–4. The
        motivation was to make computers easily accessible to
        undergraduate students in all disciplines.

        Most BASICs had quite powerful string handling operations that
        required a simple :term:`garbage collector`. In many
        implementations, the garbage collector could be forced to run
        by running the mysterious expression ``FRE("")``.

        BASIC is now old-fashioned, but survives as a scripting
        language, in particular in Visual BASIC, which is an
        application development environment with a BASIC-like
        scripting language. These descendants invariably have
        automatic memory management as well.

    C

        C is a systems programming language sometimes described as "a
        portable assembler" because it was intended to be sufficiently
        low-level to allow performance comparable to assembler or
        machine code, but sufficiently high-level to allow programs to
        be reused on other platforms with little or no modification.

        :term:`Memory management` is typically manual (the standard
        library functions for :term:`memory (2)` management in C,
        :term:`malloc` and :term:`free (2)`, have become almost
        synonymous with :term:`manual memory management`), although
        with the Boehm-Weiser :term:`collector (1)`, it is now
        possible to use :term:`garbage collection`.

        The language is notorious for fostering memory management
        bugs, including:

        1. Accessing arrays with indexes that are out of bounds;

        2. Using :term:`stack-allocated <stack allocation>` structures beyond their :term:`lifetimes <lifetime>` (see :term:`use after free`);

        3. Using :term:`heap-allocated <heap allocation>` structures after :term:`freeing <free (1)>` them (see :term:`use after free`);

        4. Neglecting to free heap-allocated objects when they are no longer required (see :term:`memory leak`);

        5. Failing to allocate memory for a :term:`pointer` before using it;

        6. Allocating insufficient memory for the intended contents;

        7. Loading from allocated memory before storing into it;

        8. Dereferencing non-pointers as if they were pointers.

        .. seealso:: :term:`automatic storage duration`, :term:`static storage duration`.

        .. bibref:: [BW88]_, [DACONTA93]_, [ZORN93]_.

        .. link::

            `Boehm-Weiser collector <http://www.hpl.hp.com/personal/Hans_Boehm/gc/>`_,
            `C standardization <http://www.open-std.org/jtc1/sc22/wg14/>`_, 
            `comp.lang.c Frequently Asked Questions <http://c-faq.com/>`_.

    COBOL

        COBOL was designed by the CODASYL committee in 1959–60 to be a
        business programming language, and has been extended many
        times since. It is still the most widely-used programming
        language (in terms of lines of code in use).

        Prior to 2002, COBOL had no :term:`heap allocation`, and did
        well in its application domain without it. COBOL 2002 has
        :term:`pointers <pointer>` and heap allocation through
        ``ALLOCATE`` and ``FREE``, mainly in order to be able to use
        C-style interfaces. It also supports a high level of
        abstraction through object-oriented programming and
        :term:`garbage collection` (including :term:`finalization`).

        .. link::

            `COBOL standardization <http://www.cobolstandard.info/wg4/wg4.html>`_.

    Common Lisp

        Common Lisp is the major dialect of the :term:`Lisp` family.
        In addition to the usual Lisp features, it has an advanced
        object system, data types from hash tables to complex numbers,
        and a rich standard library.

        Common Lisp is a :term:`garbage-collected <garbage
        collection>` language, and modern implementations, such as
        `LispWorks <http://www.lispworks.com/>`_ and `Allegro CL
        <http://www.franz.com/products/allegro-common-lisp/>`_,
        include advanced features, such as :term:`finalization` and
        :term:`weakness <weak reference (1)>`.

        .. link::

            `Common Lisp HyperSpec <http://www.lispworks.com/documentation/HyperSpec/Front/>`_.

    C++

        C++ is a (weakly) object-oriented language, extending the
        systems programming language :term:`C` with a
        multiple-inheritance class mechanism and simple method
        dispatch.

        The standard library functions for :term:`memory (2)`
        management in C++ are ``new`` and ``delete``. The higher
        abstraction level of C++ makes the bookkeeping required for
        :term:`manual memory management` even harder. Although the
        standard library provides only manual memory management, with
        the Boehm-Weiser :term:`collector (1)`, it is now possible to
        use :term:`garbage collection`. :term:`Smart pointers <smart
        pointer>` are another popular solution.

        The language is notorious for fostering memory management
        bugs, including:

        1. Using :term:`stack-allocated <stack allocation>` structures
           beyond their :term:`lifetimes <lifetime>` (see :term:`use
           after free`);

        2. Using :term:`heap-allocated <heap allocation>` structures
           after :term:`freeing <free (1)>` them (see :term:`use after
           free`);

        3. Neglecting to free heap-allocated objects when they are no
           longer required (see :term:`memory leak`);

        4. Excessive copying by copy :term:`constructors <constructor
           (1)>`;

        5. Unexpected sharing due to insufficient copying by copy
           constructors;

        6. Allocating insufficient memory for the intended contents;

        7. Accessing arrays with indexes that are out of bounds.

        .. historical::

            C++ was designed by Bjarne Stroustrup, as a minimal
            object-oriented extension to C. It has since grown to
            include some other modern programming language ideas. The
            first implementations were preprocessors that produced C
            code, but modern implementations are dedicated C++
            compilers.

        Ellis and Stroustrup write in *The Annotated C++ Reference
        Manual*:

            C programmers think memory management is too important to
            be left to the computer. Lisp programmers think memory
            management is too important to be left to the user.

        .. seealso:: :term:`constructor (2)`, :term:`destructor (2)`.

        .. bibref:: [ATTARDI94]_, [BARTLETT89]_, [BW88]_, [ZORN93]_, [EDELSON92]_, [ELLIS93]_.

        .. link::

            `comp.lang.c++ FAQ <http://www.parashift.com/c++-faq/>`_,
            `C++ standardization <http://www.open-std.org/jtc1/sc22/wg21/>`_.

    Dylan

        Dylan is a modern programming language invented by Apple
        around 1993 and developed by `Harlequin
        <http://en.wikipedia.org/wiki/Harlequin_(software_company)>`_
        and other partners. The language is a distillation of the best
        ideas in dynamic and object-oriented programming. Its
        ancestors include :term:`Lisp`, :term:`Smalltalk`, and
        :term:`C++`. Dylan is aimed at building modular component
        software and delivering safe, compact applications. It also
        facilitates the rapid development and incremental refinement
        of prototype programs.

        Dylan provides :term:`automatic memory management`. The
        generic allocation function is called ``make``. Most
        implementations provide :term:`finalization` and :term:`weak
        <weak reference (1)>` hash tables, although interfaces for
        these features have not yet been standardized. An object may
        be registered for finalization via the function
        ``finalize-when-unreachable``, in which case there will be a
        call to the ``finalize`` function once the :term:`garbage
        collector` has determined that the object is
        :term:`unreachable`. Weak hash tables may have either weak
        keys or values, depending on a parameter supplied at
        allocation time. A hash table entry will be deleted once the
        garbage collector has determined that there are no
        :term:`strong references <strong reference>` to the key or
        value of the entry, for weak key or value tables,
        respectively.

        .. link::

            `Open Dylan <http://opendylan.org/>`_.

    Emacs Lisp

        Emacs Lisp or elisp is a dialect of :term:`Lisp` used in the
        Emacs family of text editors, of which the most widely-used is
        `GNU Emacs <http://www.gnu.org/software/emacs/emacs.html>`_.

        Like most Lisps, Emacs Lisp requires :term:`garbage
        collection`. GNU Emacs has a simple :term:`mark-sweep`
        collector. It has been speculated that the
        non-:term:`incremental <incremental garbage collection>`
        nature of the Emacs collector, combined with the fact that,
        prior to version 19.31 (May 1996), it printed a message
        whenever it collected, gave garbage collection a bad name in
        programming circles.

        Erik Naggum reported at the time:

            I have run some tests at the U of Oslo with about 100
            users who generally agreed that Emacs had become faster in
            the latest Emacs pretest. All I had done was to remove the
            "Garbage collecting" message which people perceive as
            slowing Emacs down and tell them that it had been sped up.
            It is, somehow, permissible for a program to take a lot of
            time doing any other task than administrative duties like
            garbage collection.

        Emacs was originally written in Teco, not in Lisp, but it
        still had a garbage collector, though this was heuristic and
        conservative in nature. Teco-based Emacs was capable of
        running for weeks at a time in a 256 kB :term:`address space`.

        .. link::

            `GNU Emacs Lisp Reference Manual <http://www.gnu.org/software/emacs/manual/elisp.html>`_,
            `Entry on Garbage Collection <http://www.gnu.org/software/emacs/manual/html_node/elisp/Garbage-Collection.html>`_.

    Fortran

        Fortran, created in 1957, was one of the first languages
        qualifying as a high-level language. It is popular among
        scientists and has substantial support in the form of
        numerical libraries. For a long time, it had :term:`static
        allocation` only. The Fortran 90 standard added recursion with
        :term:`stack allocation` (automatic arrays). It also added
        :term:`dynamic allocation` using ``ALLOCATE`` with manual
        deallocation using ``DEALLOCATE``.

        .. link::

            `Fortran standardization <http://www.j3-fortran.org/>`_.

    Java

        A modern object-oriented language with a rich collection of
        useful features. The Java language started as an attempt by
        the Java group at Sun Microsystems to overcome software
        engineering problems introduced by :term:`C++`. Key reasons
        for the language's success were the security model and the
        portable execution environment, the Java Virtual Machine
        (JVM), which created a lot of interest for it as a platform
        for distributed computing on open networks.

        Java is :term:`garbage-collected <garbage collection>`, as
        this facilitates object-oriented programming and is essential
        for security (which :term:`use after free` would break). It
        had :term:`finalization` from version 1.0 and three kinds of
        :term:`weakness <weak reference (1)>` from version 1.2
        (confusingly, part of the Java 2 Platform).

        Early JVMs had simple collectors that didn't scale well for
        large programs, but the current crop is catching up to the
        state of the art.

        .. seealso:: :term:`reference object`, :term:`strong reference`, :term:`soft reference`, :term:`weak reference (2)`, :term:`phantom reference`, :term:`strongly reachable`, :term:`softly reachable`, :term:`weakly reachable`, :term:`phantom reachable`.

    JavaScript

        JavaScript is a scripting language used by web browsers. The
        loose type system resembles other scripting languages,
        although the syntax follows :term:`C`. There's a
        prototype-based object system. Note that JavaScript is not
        related to :term:`Java` in any way except name. There's a
        standard by `ECMA <http://www.ecma-international.org>`_, known
        as ECMAScript.

        Despite the :term:`C++`\-like syntax (with ``new`` and
        ``delete`` operators), JavaScript is :term:`garbage-collected
        <garbage collection>`.

        .. link::

            `Standard ECMA-262: ECMAScript Language Specification <http://www.ecma-international.org/publications/standards/Ecma-262.htm>`_.

    Lisp

        Lisp is a family of computer languages combining functional
        and procedural features with automatic memory management.

        Lisp was invented by John McCarthy around 1958 for the
        manipulation of symbolic expressions. As part of the original
        implementation of Lisp, he invented :term:`garbage
        collection`. He noted:

            This process, because it is entirely automatic, is more
            convenient for the programmer than a system in which he
            has to keep track of lists and erase unwanted lists.

        Modern Lisp implementations, such as `LispWorks
        <http://www.lispworks.com/>`_ and `Allegro CL
        <http://www.franz.com/products/allegro-common-lisp/>`_, have
        advanced :term:`garbage collectors <garbage collector>`.

        Lisp is now used for all kinds of symbolic programming and
        other advanced software development. Major dialects today are
        :term:`Emacs Lisp`, :term:`Common Lisp` and :term:`Scheme`.
        Most modern dialects and related languages, such as
        :term:`Dylan`, are object-oriented.

        .. seealso:: :term:`cons (1)`.

        .. bibref:: [MM59]_, [MCCARTHY60]_, [EDWARDS]_, [BAKER78]_, [MOON84]_, [SOBALVARRO88]_, [MOON87]_, [ZORN88]_, [MOON91]_.

        .. link::

            `The Association of Lisp Users <http://www.alu.org/>`_.

    Lisp Machine

        Of particular interest in the history of memory management are
        the *Lisp Machines*, early workstation computers built around
        a custom processor designed to improve the execution speed of
        Lisp by implementing primitive Lisp operations in microcode.
        The Lisp Machine :term:`garbage collector` is a generalization
        of the algorithm described in [BAKER78]_ and used a technique
        similar to that described in [UNGAR84]_, but utilizing
        hardware to improve performance.

        A description of the garbage collector of one particular model
        is in [MOON84]_. The features important for its performance
        were:

        1. Hardware support for data typing using :term:`tags <tag>`;

        2. Reference-based :term:`read barriers <read barrier>` for
           :term:`incremental <incremental garbage collection>`
           collecting;

        3. :term:`Write barriers <write barrier>` for
           :term:`remembered sets <remembered set>` and
           :term:`generational <generational garbage collection>`
           collecting;

        4. A tight integration with the :term:`virtual memory`
           system.

        The remembered sets were based on a :term:`BIBOP` division of
        the virtual :term:`address space`. The Lisp Machine
        :term:`page-table <page table>`, unlike virtually all modern
        virtual memory systems, was a flat, hash-based table
        (sometimes called an :term:`inverted page table`), and thus
        insensitive to sparsely-populated virtual address spaces
        associated with BIBOP schemes.

        These custom processors eventually lost out to rapidly
        advancing stock hardware. Many of the techniques pioneered on
        Lisp Machines are used in today's implementations, at a cost
        of a few more cycles.

    ML

        ML is a family of strongly-typed functional languages, of
        which the principal members are Standard ML and Caml.

        Like other functional languages, ML provides :term:`automatic
        memory management`. Modern ML implementations usually have
        advanced :term:`garbage collectors <garbage collector>`. The
        combination of clean functional semantics and strong typing
        allows advanced techniques, such as :term:`region inference`.

        The Standard ML of New Jersey (SML/NJ) system, which
        implements a slight variant of Standard ML, has been important
        to memory management research for three reasons. Firstly, the
        source code is publicly available and widely ported, allowing
        experimentation with both the :term:`collector (2)` and
        :term:`mutator`. Secondly, the compiler generates code that
        does not use a :term:`control stack`, but :term:`allocates
        <allocate>` function :term:`activation records <activation
        record>` on the :term:`heap` instead. This means that the
        allocation rate is very high (up to one byte per instruction),
        and also that the collector has a very small :term:`root set`.
        Thirdly, it uses a simple :term:`copying collector <copying
        garbage collection>` that is easy to modify.

        .. seealso:: :term:`immutable`.

        .. bibref:: [CNS92]_, [DOLIGEZ93]_, [TT97]_.

        .. link::

            `comp.lang.ml FAQ <http://www.faqs.org/faqs/meta-lang-faq/>`_.

    Modula-3

        An object-oriented descendant of :term:`Pascal`.

        Modula-3 is mostly :term:`garbage-collected <garbage
        collection>`, although it is possible to use :term:`manual
        memory management` in certain modules.

        .. link::

            `modula3.org <http://www.modula3.org/>`_,
            `Modula-3 language definition <http://www.hpl.hp.com/techreports/Compaq-DEC/SRC-RR-52.pdf>`_.

    Pascal

        An imperative language characterized by block structure and a
        relatively strong (for its time) static type system. Pascal
        was designed by Niklaus Wirth around 1970.

        Pascal was popular as a teaching language due to its small
        size, but it lacked many features needed for applications
        programming. Now it's been largely supplanted by its more
        feature-rich descendants Modula-2, :term:`Modula-3`, and
        Oberon, mainly surviving in the popular Delphi development
        tool.

        Pascal uses :term:`manual memory management` (with the
        operators ``NEW`` and ``DISPOSE``). The descendants mentioned
        all offer :term:`automatic memory management`.

        .. link::

            `Borland Delphi Home Page <http://www.borland.com/delphi/>`_,
            `Pascal standardization <http://www.open-std.org/JTC1/sc22/docs/oldwgs/wg2.html>`_.

    Perl

        Perl is a complex but powerful language that is an eclectic
        mixture of scripting languages and programming languages.

        Perl programmers can work with strings, arrays, and
        associative arrays without having to worry about :term:`manual
        memory management`. Perl is well-suited to complex text file
        manipulation, such as report generation, file format
        conversion, and web server CGI scripts. It is also useful for
        rapid prototyping, but large Perl scripts are often
        unmaintainable.

        Perl's :term:`memory management` is well-hidden, but is based
        on :term:`reference counts <reference counting>` and
        :term:`garbage collection`. It also has *mortal* variables,
        whose :term:`lifetimes <lifetime>` are limited to the current
        context. It is possible to :term:`free (1)` the :term:`memory
        (2)` assigned to variables (including arrays) explicitly, by
        ``undef``\-ing the only reference to them.

        .. link::

            `The Perl Programming Language <http://www.perl.org/>`_.

    PostScript

        The PostScript language is an interpretive language with
        powerful graphics features, widely used as a page description
        language for printers and typesetters.

        The Level 1 PostScript language has a simple
        :term:`stack`\-like memory management model, using ``save``
        and ``restore`` operators to :term:`recycle` memory. The Level
        2 PostScript language adds :term:`garbage collection` to this
        model.

        .. seealso:: :term:`VM (2)`, :term:`composite object`, :term:`simple object`.

        .. link::

            `Harlequin RIP <http://en.wikipedia.org/wiki/Harlequin_RIP>`_.

    Prolog

        A logic programming language invented by Alain Colmerauer
        around 1970, Prolog is popular in the AI and symbolic
        computation community. It is special because it deals directly
        with relationships and inference rather than functions or
        commands.

        Storage is usually managed using a :term:`garbage collector`,
        but the complex control flow places special requirements on
        the collector.

        .. link::

            `Prolog Standardization <http://people.sju.edu/~jhodgson/wg17/>`_,
            `Prolog Memory Management - Garbage Collection <http://www.informatik.uni-trier.de/%7Eley/db/prolog/gc.html>`_.

    Scheme

        A small functional language blending influences from
        :term:`Lisp` and :term:`Algol`.

        Key features of Scheme include symbol and list operations,
        :term:`heap allocation` and :term:`garbage collection`,
        lexical scoping with first-class function objects (implying
        :term:`closures <closure>`), reliable tail-call elimination
        (allowing iterative procedures to be described
        tail-recursively), the ability to dynamically obtain the
        current :term:`continuation` as a first-class object, and a
        language description that includes a formal semantics.

        Scheme has been gaining popularity as an extension language;
        Project GNU's extension package of choice, `Guile
        <http://www.gnu.org/software/guile/>`_, is a Scheme
        interpreter. :term:`Garbage collection` is an important part
        of the ease of use that is expected from an extension
        language.

        .. link::

            `Scheme Standards documents <http://www.cs.indiana.edu/scheme-repository/doc.standards.html>`_,
            `Scheme Requests for Implementation <http://srfi.schemers.org/>`_.

    Simula

        Simula was designed as a language for simulation, but it
        expanded into a full general-purpose programming language and
        the first object-oriented language.

        Simula I, designed in 1962–64 by Kristen Nygaard and Ole-Johan
        Dahl, was based on :term:`ALGOL` 60, but the :term:`stack
        allocation` discipline was replaced by a two-dimensional
        :term:`free-list <free list>`.

        It was Simula 67 that pioneered classes and inheritance to
        express behavior. This domain-oriented design was supported by
        :term:`garbage collection`.

        .. bibref:: [DAHL63]_.

    Smalltalk

        Smalltalk is an object-oriented language with single
        inheritance and message-passing.

        :term:`Automatic memory management` is an essential part of
        the Smalltalk philosophy. Many important techniques were first
        developed or implemented for Smalltalk.

        .. bibref:: [DB76]_, [UNGAR84]_, [UNGAR88]_.

        .. link::

            `Smalltalk standardization <http://www.smalltalk.org/versions/ANSIStandardSmalltalk.html>`_.
