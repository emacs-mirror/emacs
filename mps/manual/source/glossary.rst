.. _glossary:

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

    arena commit limit

        ??

    block

        ??

    class structure

        ??

    client program

        ??

    constant root

        ??

    data object

        ??

    dead

        ??

    double free

        ??

    format method

        ??

    formatted object

        ??

    formatted root

        ??

    hardware write barrier

        ??

    live

        ??

    object

        A contiguous region of memory forming a single logical structure.

    object format

        ?? See the topic :ref:`topic-scanning`.

    object pointer

        ??

    pad object

        ??

    page

        ??

    parked

        ??

    pointer

        ??

    pool

        ??

    protectable root

        ??

    ramp pattern

        ??

    rank

        ??

    reference

        A link from one :term:`object` to another, usually in the form
        of a :term:`pointer`.

    reservoir

        ??

    result code

        A value returned from an MPS function, represented by the type
        :c:type:`mps_res_t`. The result code :c:macro:`MPS_RES_OK`
        indicates success; other values indicate errors. See the topic
        :ref:`topic-errors`.

    root

        ??

    root mode

        ??

    scan function

        A function that examines a block of memory to find
        :term:`references <reference>` and indicate them to the MPS. A
        scan function forms part of an :term:`object format`. See
        the topic :ref:`topic-scanning`.

    scan state

        ?? See the topic :ref:`topic-scanning`.

    segregated allocation cache

        ??

    size

        ??

    size class

        ??

    table root

        ??

    virtual memory

        ??
