=====================================
Memory Pool System Product Procedures
=====================================

:author: Richard Brooksby
:organization: Ravenbrook Limited
:date: 2002-06-18
:revision: $Id$
:confidentiality: public
:copyright: See `C. Copyright and License`_


1. Introduction
---------------

This document lists the procedures which are applied to the product
sources of the Memory Pool System project. (Compare with the `procedures
which govern the *project* </project/mps/procedure/>`__.)

This document will be updated as new procedures are created.

The readership of this document is anyone developing or extending the
product sources.

This document is not confidential.


2. Procedures
-------------

=====================   ==================================================
`branch-merge`_         Branching and merging for development.
`pull-request-merge`_   Pull request merge procedure for GitHub.
`release-build`_        Build product releases from the sources.
`version-create`_       Create a new MPS version branch.
=====================   ==================================================

.. _branch-merge: branch-merge
.. _release-build: release-build
.. _version-create: version-create
.. _pull-request-merge: pull-request-merge

[These links work in the Ravenbrook infosys because Charlotte doesn't
expose extensions, as recommended by Tim Berners-Lee.  But they don't
work on GitHub.  We should probably make them work there.  RB
2023-01-07]


A. References
-------------


B. Document History
-------------------

==========    =======   ==================================================
2002-06-18    RB_       Created based on P4DTI document.
2005-09-30    RHSK_     Added version-create and release-configura.
2008-01-07    RHSK_     Added release-experimental.
2012-09-05    RB_       Removed release-experimental, no longer needed to work with Configura.
2012-09-13    RB_       Removed release-configura, now maintained on a custom mainline.
2014-01-13    GDR_      Added branch-merge.
2020-07-28    PNJ_      Updated licence text.
2023-01-07    RB_       Added pull-request-merge.
==========    =======   ==================================================

.. _GDR: mailto:gdr@ravenbrook.com
.. _RB: mailto:rb@ravenbrook.com
.. _RHSK: mailto:rhsk@ravenbrook.com
.. _PNJ:  mailto:pnj@ravenbrook.com

C. Copyright and License
------------------------

Copyright © 2002–2023 `Ravenbrook Limited <https://www.ravenbrook.com/>`_.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are
met:

1. Redistributions of source code must retain the above copyright
   notice, this list of conditions and the following disclaimer.

2. Redistributions in binary form must reproduce the above copyright
   notice, this list of conditions and the following disclaimer in the
   documentation and/or other materials provided with the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

.. checked with rst2html -v index.rst > /dev/null
.. end
