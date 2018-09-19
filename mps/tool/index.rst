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

Copyright © 2002-2018 Ravenbrook Limited. All rights reserved.
<http://www.ravenbrook.com/> This is an open source license. Contact
Ravenbrook for commercial licensing options.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are
met:

#. Redistributions of source code must retain the above copyright
   notice, this list of conditions and the following disclaimer.
#. Redistributions in binary form must reproduce the above copyright
   notice, this list of conditions and the following disclaimer in the
   documentation and/or other materials provided with the distribution.
#. Redistributions in any form must be accompanied by information on how
   to obtain complete source code for the this software and any
   accompanying software that uses this software. The source code must
   either be included in the distribution or be available for no more
   than the cost of distribution plus a nominal fee, and must be freely
   redistributable under reasonable conditions. For an executable file,
   complete source code means the source code for all modules it
   contains. It does not include source code for modules or files that
   typically accompany the major components of the operating system on
   which the executable file runs.

**This software is provided by the copyright holders and contributors
"as is" and any express or implied warranties, including, but not
limited to, the implied warranties of merchantability, fitness for a
particular purpose, or non-infringement, are disclaimed. In no event
shall the copyright holders and contributors be liable for any direct,
indirect, incidental, special, exemplary, or consequential damages
(including, but not limited to, procurement of substitute goods or
services; loss of use, data, or profits; or business interruption)
however caused and on any theory of liability, whether in contract,
strict liability, or tort (including negligence or otherwise) arising in
any way out of the use of this software, even if advised of the
possibility of such damage.**
