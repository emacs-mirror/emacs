Memory Pool System branching and merging procedures
===================================================
:author: Gareth Rees
:organization: Ravenbrook Limited
:date: 2014-01-09
:confidentiality: public
:copyright: See `C. Copyright and License`_
:readership: MPS developers


Introduction
------------

This document contains procedures and checklists for branching and merging during the ordinary course of development. For making version branches, see `version-create`_, and for making releases, see `release-build`_.

.. _version-create: version-create
.. _release-build: release-build


Creating a development branch
-----------------------------

#. If you are creating a development branch to fix a problem, make
   sure that there's a job recording the problem.

#. Create a branch specification.

        $ p4 branch mps/branch/2013-08-21/lii6ll

   The specification should look something like this::

        Branch:	mps/branch/2013-08-21/lii6ll

        Description:
                Adding new supported platform lii6ll (job003596).

        View:
                //info.ravenbrook.com/project/mps/master/... //info.ravenbrook.com/project/mps/branch/2013-08-21/lii6ll/...

#. If you're working on a problem that's specific to customer, then
   map the mainline for that customer rather than the master sources::

        View:
                //info.ravenbrook.com/project/mps/custom/cet/main/... //info.ravenbrook.com/project/mps/branch/2013-11-04/cet-i6-stack-probe/...

#. If you're just going to work on a subset of the sources, then just
   map the files you need::

        View:
                //info.ravenbrook.com/project/mps/custom/cet/main/code/... //info.ravenbrook.com/project/mps/branch/2013-07-02/cet-ap-key/code/...

#. Edit the branch index::

        $ p4 edit //info.ravenbrook.com/project/mps/branch/index.html

   and add an entry to the "Active branches" section.


Pre-merge checklist
-------------------

#. Have you solved the problem?

#. Is there an automated test case?

#. Does the test suite pass?

#. If there are interface changes, is there documentation?

#. If this is work on a customer-specific branch, and you've added new
   customer-specific interfaces, have you ensured that these
   interfaces are separated from the public interfaces?

#. If this is work on a customer-specific branch, and you've added new
   files that should be merged into the mainline for that customer,
   but not into the master sources, have these files been excluded in
   the branch specification for that customer?

#. Has there been a code review?


Merging a development branch
----------------------------

#. Do a catch-up merge from the master sources (or the appropriate
   customer-specific mainline)::

         $ BRANCH=mps/branch/2013-08-21/lii6ll
         $ cd $BRANCH
         $ p4 integ -b $BRANCH ...
         $ p4 resolve -as ...
         $ p4 resolve

#. Check that the test suite passes on the branch.

#. Submit the merged files::

         $ p4 submit -d "Catch-up merge from the master sources to $BRANCH" ...

#. Update the branch on other platforms and check that the test suite
   passes.

#. Backward merge into the master sources (or the appropriate
   customer-specific mainline)::

         $ cd ../../../master
         $ p4 integ -r -b $BRANCH ...
         $ p4 resolve -as ...
         $ p4 resolve

   Note: don't cherry-pick individual lines from files, if at all
   possible. (It may not be possible in the case of makefiles.) Try to
   structure changes so that files can be included or excluded as a
   whole.

#. Check that the test suite passes on the master sources.

#. Submit the merged files::

         $ p4 submit -d "Merge $BRANCH into the master sources" ...

#. Create a fix record for the change you just submitted::

         $ p4 fix -c CHANGE JOB

#. Edit the branch index, moving the development branch from the
   "Active branches" to "Dormant branches" section and linking the
   change in which the branch was merged.

#. If the change is significant and user-visible, update the release
   notes (``master/manual/source/release.rst``).



A. References
-------------


B. Document History
-------------------

==========  =====  ==================================================
2014-01-09  GDR_   Created.
==========  =====  ==================================================

.. _GDR: mailto:gdr@ravenbrook.com


C. Copyright and License
------------------------

Copyright © 2014 Ravenbrook Limited. All rights reserved.
<http://www.ravenbrook.com/>. This is an open source license. Contact
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
“as is” and any express or implied warranties, including, but not
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

