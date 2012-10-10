.. _glossary:

========
Glossary
========

.. glossary::

    address

        ??

    address space

        ??

    alignment

        The address modulus to which all :term:`objects <object>` in
        an :term:`object format` must be aligned. That is, if an
        alignment of 4 is specified for a format, then the
        :term:`address` of any object in that format will always be 0,
        modulo 4. In the MPS, an alignment must be a positive power of
        2.

    allocation frame

        ??

    allocation pattern

        ??

    allocation point

        ??

    arena

        ??

    arena class

        ??

    automatic

        ??

    black

        ??

    block

        ??

    clamped state

        ??

    class method

        ??

    class structure

        ??

    client arena

        ??

    client program

        ??

    commit limit

        ??

    committed memory

        ??

    constant root

        ??

    copy method

        ??

    copying

        ??

    data object

        ??

    dead

        ??

    double free

        ??

    finalization

        ??

    finalized object

        ??

    format method

        ??

    formatted object

        ??

    formatted root

        ??

    forward method
    
        ??

    forwarded object

        ??

    forwarding marker

        ??

    fragmentation

        ??

    garbage collection

        ??

    garbage collector

        ??

    header

        ??

    is-forwarded method
    
        ??

    live

        ??

    manual

        ??

    memory

        ??

    memory leak

        ??

    message

        ??

    moving

        ??

    non-moving

        ??

    object

        A contiguous region of memory forming a single logical structure.

    object format

        ?? See the topic :ref:`topic-scanning`.

    object pointer

        ??

    padding method

        ??

    padding object

        ??

    page

        ??

    parked state

        ??

    plinth

        ??

    pointer

        ??

    pool

        ??

    pool class

        ??

    protectable root

        ??

    ramp pattern

        ??

    rank

        ??

    read barrier

        ??

    reference

        A link from one :term:`object` to another, usually in the form
        of a :term:`pointer`.

    remembered set

        ??

    reservoir

        ??

    result code

        A value returned from an MPS function, represented by the type
        :c:type:`mps_res_t`. The result code :c:macro:`MPS_RES_OK`
        indicates success; other values indicate errors. See the topic
        :ref:`topic-error`.

    root

        ??

    root mode

        ??

    scan method

        A function that examines a block of memory to find
        :term:`references <reference>` and indicate them to the MPS. A
        scan method forms part of an :term:`object format`. See
        the topic :ref:`topic-scanning`.

    scan state

        ?? See the topic :ref:`topic-scanning`.

    segment

        ??

    segregated allocation cache

        ??

    size

        ??

    size class

        ??

    skip method
    
        ??

    spare commit limit

        ??

    spare committed memory

        ??

    table root

        ??

    trace

        ??

    unclamped state

        ??

    virtual memory

        ??

    virtual memory arena

        ??

    white

        ??

    write barrier

        ??
