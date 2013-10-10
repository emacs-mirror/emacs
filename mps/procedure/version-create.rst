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

Background: refer to PQTCM ("Product Quality Through Change Management",
`http://www.ravenbrook.com/doc/1999/05/20/pqtcm/ <http://www.ravenbrook.com/doc/1999/05/20/pqtcm/>`__)
for background, terminology, rationale, and usage guidance. This tells
you what "a version" actually is.


2. Preamble
-----------

Do I need this procedure?
~~~~~~~~~~~~~~~~~~~~~~~~~

You might not need to create a new version. An alternative is to create
a further 'point release' on the existing version. Refer to PQTCM when
deciding. (Summary: am I changing the specification?).

What is a version?
~~~~~~~~~~~~~~~~~~

A version is a ‘clone’ of all the master source files, that then has its
own evolution. A version has these parts:

-  Perforce branch, defining the mapping used to integrate from master
   to version. By convention, the name of the branch is
   "mps/version/A.BBB": note that we make the branch name exactly match
   the pathname of that version's sub-tree. For example::

      $ p4 branch -o mps/version/1.105
      ...
      View:
              //info.ravenbrook.com/project/mps/master/... //info.ravenbrook.com/project/mps/version/1.105/...
      ...

-  Cloned (integrated) and submitted files, in the ``version/A.BBB/...``
   sub-tree. Usually this is a clone of the entire ``master`` subtree
   and all its files. These were created in a single change with ``p4
   integrate -b <branchspec>``. This initial integrate is what
   populates the version branch with files; before that it was empty.
   For each of these files, Perforce reports the first action as
   "branch". Some files may then be further modified.

-  Origin: The point in time that the initial integrate was performed,
   expressed as its changelevel minus one, defines the ‘Origin’ of the
   version. The Origin is the last change on the master sources that
   also made it into the version sources by virtue of the initial
   integrate command.

-  Entry in the table at
   `http://info.ravenbrook.com/project/mps/version/ <http://info.ravenbrook.com/project/mps/version/>`__.

3. Procedure: How to make a new version
---------------------------------------

0. Some files contain an MPS version-name. What version-name do the
   *master* copies of these files contain? It depends. Some contain
   the pseudo-version-name ``master``: you will leave these files
   unchanged in the master source, and only update them on the version
   branch (see step 6 below). Others, even in the master sources,
   refer to the expected next version-name: you should update these
   files before making the branch. Make these files contain the
   expected new version-name, and/or information pertinent to the new
   version:

   - ``master/code/version.c``
   - ``master/manual/source/release.rst``

   Submit these files before you continue.

1. Make the branch by running ``p4 branch mps/version/A.BBB``. Specify::

       Description: Branching master sources for version A.BBB.

   Always branch the whole of the master sources::

       View:
               //info.ravenbrook.com/project/mps/master/... //info.ravenbrook.com/project/mps/version/A.BBB/...

2. Make sure you have no unsubmitted files, and then::

    p4 integrate -b mps/version/A.BBB

3. ``p4 submit``

4. Determine the Origin of the new version: do ``p4 changes -m 5`` on
   the master sources, and note the latest change that was in before
   the integrate.

   .. Note: it's better to do it this way -- do the integrate from the
      *implicit* tip of the master, and then check back to see what
      happened -- because it's hard to get wrong. Also, then the
      integrate has the changelevel origin+1.  Clashes with master
      submits could theoretically occur, and could be avoided by
      determining the origin first and specifying it to the initial
      integrate, but in practice this never happens.

5. Update the table at <http://info.ravenbrook.com/project/mps/version/>.

6. Edit Master->Version in documents that erroneously say "Master".
   Always edit version/A.BBB/index.html, e.g. (case-sensitive)::

       "of the Master version"  ->  "of Version A.BBB"
       "Master"  ->  "Version A.BBB"

   Less importantly, edit various other files. See change 30260.

7. Do an empty-integrate of this change back on to the masters, so P4
   thinks it's done and doesn't keep suggesting it::

       p4 integrate -r -b mps/version/A.BBB <Files Edited Master->Version>
       p4 resolve -ay <Files Edited Master->Version>


A. References
-------------


B. Document History
-------------------

- 2005-10-03  RHSK  Created.
- 2006-12-27  RHSK  Step 0: edit some files on master before making version branch
- 2007-07-05  RHSK  Releasename now also in w3build.bat.  Make sure all submitted before integ.
- 2008-10-29  RHSK  Convert from text to html.
- 2010-11-06  RHSK  Correctly format example of p4 branch -o mps/version/1.105


C. Copyright and License
------------------------

This document is copyright © 2002, 2005-2008, 2010 `Ravenbrook
Limited <http://www.ravenbrook.com/>`__. All rights reserved. This is an
open source license. Contact Ravenbrook for commercial licensing
options.

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
