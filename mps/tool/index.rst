Memory Pool System Product Tools
================================
:author: Richard Brooksby
:organization: Ravenbrook Limited
:date: 2002-06-18
:revision: $Id$
:confidentiality: public
:copyright: See `C. Copyright and License`_


1. Introduction
---------------

This document lists the tools which have been written to develop the
Memory Pool System product.

This document will be updated as new tools are created.

The readership of this document is anyone developing or extending the
product sources.

This document is not confidential.


2. Tools
--------

=================  ==========================================================
`branch`_          Make a version or development branch.
`gcovfmt`_         Formats the output of the ``gcov`` coverage tool into a
                   summary table.
`noaslr.c`_        Run a program on macOS with address space layout
                   randomization (ASLR) disabled.
`p4-bisect`_       Find, by binary search, the Perforce change that introduced
                   a feature or bug.
`release`_         Make a product release.
`testaslr.c`_      Print addresses from different parts of address space in
                   order to detect whether address space layout
                   randomization (ASLR) is in force.
`testcases.txt`_   MPS test case database.
`testcoverage`_    Instrument the test suite for coverage, run it, and output
                   a coverage report.
`testopendylan`_   Download the latest version of Open Dylan and build it
                   against the MPS sources.
`testrun.bat`_     Test case runner on Windows.
`testrun.sh`_      Test case runner on Unix.
=================  ==========================================================

.. _branch: branch
.. _gcovfmt: gcovfmt
.. _noaslr.c: noaslr.c
.. _p4-bisect: p4-bisect
.. _release: release
.. _testaslr.c: testaslr.c
.. _testcases.txt: testcases.txt
.. _testcoverage: testcoverage
.. _testopendylan: testopendylan
.. _testrun.bat: testrun.bat
.. _testrun.sh: testrun.sh


A. References
-------------


B. Document History
-------------------

==========  ======  ========================================================
2002-06-18  RB_     Created based on P4DTI document.
2005-09-30  RHSK_   Added ``test-runner.py``.
2013-05-24  GDR_    Added ``gcovfmt.py`` and ``testrun.sh``.
                    ``test-runner.py`` is no longer used.
2014-01-13  GDR_    Converted to reStructuredText. Added ``testrun.bat``.
2014-03-22  GDR_    Add ``branch``, ``release``, ``testcoverage``, and
                    ``testopendylan``.
==========  ======  ========================================================

.. _GDR: mailto:gdr@ravenbrook.com
.. _RB: mailto:rb@ravenbrook.com
.. _RHSK: mailto:rhsk@ravenbrook.com


C. Copyright and License
------------------------

Copyright © 2002-2020 `Ravenbrook Limited <http://www.ravenbrook.com/>`_.

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
