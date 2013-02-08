.. _glossary-z:

=============================
Memory Management Glossary: Z
=============================

.. include:: alphabet.txt

.. glossary::

    ZCT

        .. see:: :term:`zero count table`.

    zero count table

        .. aka:: *ZCT*.

        A *zero count table* is used in :term:`deferred reference
        counting` to record :term:`objects` whose
        :term:`reference counts <reference counting>` have dropped to
        zero but which have not been processed to see if they can be
        :term:`reclaimed`.

