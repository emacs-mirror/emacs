Memory Pool System Version Create Procedure
===========================================
:author: Richard Kistruck
:organization: Ravenbrook Limited
:date: 2008-10-29
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
[RB_1999-05-20] when deciding. (Summary: if changing the
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

#. Make sure that the sources for the version you are about to create,
   and for the table of versions, are mapped in your Perforce client.

#. Update the following files in the master sources that contain a
   version number, so that they refer to the version that you are
   about to create::

        code/version.c

   Submit these files to Perforce.

   (If there are other files that need updating, update this procedure
   and add them here. But it is better to parse the information out of
   ``code/version.c`` so that the version number is mentioned just
   once. See for example ``manual/source/conf.py``.)

#. Create the version branch specification by running::

        VERSION=A.BBB
        BRANCH=mps/version/$VERSION
        p4 branch -i <<END
        Branch: $BRANCH
        Description: Branching master sources for version $VERSION.
        View: //info.ravenbrook.com/project/mps/master/... //info.ravenbrook.com/project/$BRANCH/...
        END

#. Make sure you have no unsubmitted files::

        $ p4 opened
        File(s) not opened on this client.

   and then::

        p4 integrate -b $BRANCH
        p4 submit -d "Branching master sources for version $VERSION."

#. Determine the origin of the new version::

        p4 changes -m 5 //info.ravenbrook.com/project/mps/master/...

   Note the latest change that was in before the integrate.

#. Update the `table of versions <https://info.ravenbrook.com/project/mps/version/>`_.

#. Edit ``configure.ac`` on the version branch, replacing ``[master]``
   with ``[version A.BBB]``, and then submit it::

        p4 submit -d "Update 'master' to 'version $VERSION'"

   (If there are other files that need updating, update this procedure
   and add them here. But it is better to organize the sources so that
   this is not necessary.)

#. Do an empty integrate of this change back on to the masters, so
   Perforce knows that it's not wanted::

        p4 integrate -r -b $BRANCH
        p4 resolve -ay
        p4 submit -d "Ignoring update of 'master' to 'version $VERSION' from version branch"


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
==========  =====  ========================================================

.. _RHSK: mailto:rhsk@ravenbrook.com


C. Copyright and License
------------------------

Copyright © 2002-2014 Ravenbrook Limited. All rights reserved.
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
