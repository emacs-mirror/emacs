Memory Pool System Version Create Procedure
===========================================
:author: Richard Kistruck
:organization: Ravenbrook Limited
:date: 2008-10-29
:Revision: $Id$
:confidentiality: public
:copyright: See `C. Copyright and License`_
:readership: MPS developers


1. Introduction
---------------

This document tells you how to create a new version from the master
sources. (For example, if the last version of the MPS is 1.105, with
releases 1.105.0 and 1.105.1, this document tells you how to abandon the
1.105 lineage and take a new clone from the master sources to create
version 1.106).

Refer to "Product Quality Through Change Management" [RB_1999-05-20]
for background, terminology, rationale, and usage guidance. This tells
you what "a version" actually is.


2. Preamble
-----------

2.1. Do I need this procedure?
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

You might not need to create a new version. An alternative is to
create a further "point release" on the existing version. Refer to
[RB_1999-05-20]_ when deciding. (Summary: if changing the
specification, make a new version; if improving the product against an
unchanged specification, make a point release.)


2.2. What is a version?
~~~~~~~~~~~~~~~~~~~~~~~

A version is a clone of all the master sources, that has its own
evolution. A version has these parts:

#. The **version branch specification**, which defines the mapping used
   to integrate from the master sources to the version sources. By
   convention, the name of the branch specification is
   ``mps/version/A.BBB`` so that it exactly matches the path name of
   the version's sub-tree. For example::

        $ p4 branch -o mps/version/1.105
        ...
        View:
                //info.ravenbrook.com/project/mps/master/... //info.ravenbrook.com/project/mps/version/1.105/...
        ...

#. The **version sources**, in the ``version/A.BBB/...`` sub-tree.
   Usually these are clones of the master sources, and were created in
   a single change with ``p4 integrate -b BRANCH``. Some files may
   then be further modified.

#. The **version origin**, the last change on the master sources that
   made it into the version sources by virtue of the initial integrate
   command.

#. An entry in the `table of versions <https://info.ravenbrook.com/project/mps/version/>`_.


3. Procedure: How to make a new version
---------------------------------------


3.1. Pre-branch checklist
~~~~~~~~~~~~~~~~~~~~~~~~~

#. Are you an authenticated Git Fusion user? If not, follow the
   git-fusion_ procedure.

   .. _git-fusion: /procedure/git-fusion

#. Does ``code/version.c`` in the master sources contain the correct
   value for the ``MPS_RELEASE`` macro? It should be the name of the
   first release from the version you are about to create. If it is
   wrong, correct and submit it.

#. Does ``code/version.c`` in the master sources have the correct
   copyright dates for the ``MPSCopyrightNotice`` string? It should
   include the current year.  If it is wrong, correct and submit it.
   


3.2. Automated procedure
~~~~~~~~~~~~~~~~~~~~~~~~

Run the script ``tool/branch``, passing the options:

* ``-P mps`` — project name
* ``-p master`` — parent branch
* ``-C CHANGELEVEL`` — changelevel at which to make the branch
* ``-v`` — request a version branch
* ``-d "DESCRIPTION"`` — description of the branch (see below)

The branch description will be published in the version index and
should be a short summary of the improvements detailed in the release
notes.

If omitted, the project and parent branch are deduced from the current
directory, and the changelevel defaults to the most recent change on
the parent branch. A typical invocation looks like this::

    tool/branch -p master -v -d "Improved interface to generation chains."

Visually check the output of the script against `3.3. Manual
procedure`_, and when satisfied, repeat the invocation with the ``-y``
option.


3.3. Manual procedure
~~~~~~~~~~~~~~~~~~~~~

#. Create the version branch specification by running::

        VERSION=A.BBB
        BRANCH=mps/version/$VERSION
        p4 branch -i <<END
        Branch: $BRANCH
        Description: Branching master sources for version $VERSION.
        View: //info.ravenbrook.com/project/mps/master/... //info.ravenbrook.com/project/$BRANCH/...
        END

#. Create the branch itself by running::

        p4 populate -b $BRANCH -d "Branching master sources for version $VERSION."

#. Determine the origin of the new version::

        p4 changes -m 5 //info.ravenbrook.com/project/mps/master/...

   Note the latest change that was in before the populate.

#. Update the `table of versions <https://info.ravenbrook.com/project/mps/version/>`_.

#. Add the version to the “mps” and “mps-public” repos published by
   Git Fusion by editing ``//.git-fusion/repos/mps/p4gf_config`` and
   ``//.git-fusion/repos/mps-public/p4gf_config`` with entries similar
   to existing version branches.


3.4. Post-branch checklist
~~~~~~~~~~~~~~~~~~~~~~~~~~

Ensure that the branch appears correctly at:

#. the internal index at https://info.ravenbrook.com/project/mps/version

#. the external index at http://www.ravenbrook.com/project/mps/version

#. the GitHub mirror at https://github.com/Ravenbrook/mps/branches


A. References
-------------

.. [RB_1995-05-20] Richard Brooksby; "Product Quality Through Change
   Management"; Ravenbrook Limited; 1999-05-20;
   http://www.ravenbrook.com/doc/1999/05/20/pqtcm/


B. Document History
-------------------

==========  =====  ========================================================
2005-10-03  RHSK_  Created.
2006-12-27  RHSK_  Step 0: edit some files on master before making version branch
2007-07-05  RHSK_  Releasename now also in w3build.bat.  Make sure all submitted before integ.
2008-10-29  RHSK_  Convert from text to html.
2010-11-06  RHSK_  Correctly format example of p4 branch -o mps/version/1.105
2014-01-13  GDR_   Make procedure less error-prone by giving exact sequence of commands (where possible) based on experience of version 1.112.
2014-01-14  GDR_   Step for adding to Git Fusion.
2014-03-19  GDR_   Describe automated procedure.
2016-01-28  RB_    Git repository renamed from mps-temporary to mps.
2016-04-05  RB_    Bringing up to date in preparation for version 1.115.
==========  =====  ========================================================

.. _GDR: mailto:gdr@ravenbrook.com
.. _RHSK: mailto:rhsk@ravenbrook.com


C. Copyright and License
------------------------

Copyright © 2002-2016 Ravenbrook Limited. All rights reserved.
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
