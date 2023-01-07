===============================================
Memory Pool System pull request merge procedure
===============================================

:author: Richard Brooksby
:organization: Ravenbrook Limited
:date: 2023-01-07
:confidentiality: public
:copyright: See `C. Copyright and License`_
:readership: MPS developers, trainee integrators
:status: draft


1. Introduction
---------------

This document contains a procedure for merging a branch received via a
GitHub "pull request".

This document was created as a combination of the process improvement
steps from our Perforce-based "Memory Pool System branching and
merging procedures" [GDR_2014-01-09]_ with Gareth Rees' email
containing the working steps for git / GitHub merges, "Re: Possible
MPS Help" [GDR_2020-09-03]_ .

The document is still draft.  Some of the questions that need
resolving are noted in square brackets.

Ravenbrook is currently (2023-01) migrating the MPS project to git
(and GitHub) and this is likely to modify this procedure.  [Insert
reference to that project.  RB 2023-01-07]


2. Pre-merge checklist
----------------------

#. Is there a document (issue, job) recording the problem that is
   solved by the changes in the branch?

#. Does the branch solve the problem?

#. Is there an automated test case that demonstrates that the problem
   is solved?

#. If there are interface changes, is there documentation?

#. If the changes are significant and user-visible, have you updated
   the release notes (``manual/source/release.rst``)?

#. Has there been a code review?

#. Does the branch build and pass tests on all target platforms?
   [This may be validated by Travis CI.  Insert details here.  RB
   2023-01-07]

#. Does the branch merge cleanly into the masters?  If not, consider a
   catch-up merge from the masters.  [This may be validated by GitHub
   and/or Travis CI.  Insert details here.  RB 2023-01-07]

#. Does a merge of the branch with master build and pass tests on all
   target platforms?  [This may be validated by Travis CI.  Insert
   details here.  RB 2023-01-07]

[Checklist items for Customer-specific branches from branch-merge.rst
omitted for now.  RB 2023-01-07]


3. Prerequisite steps
---------------------

#. Ensure your public key is in Perforce at
   //.git-fusion/users/USER/keys/ in Perforce, submitting it if
   necessary.

#. Ensure your e-mail address is in Perforce at
   //.git-fusion/users/p4gf_usermap in Perforce and matches your
   Perforce user record, submitting it if necessary.

#. Clone the Ravenbrook MPS GitHub repository and name the remote
   "github", giving (inter alia) access to Travis CI to build and test
   the merge. ::

     git clone -o github git@github.com:Ravenbrook/mps.git

#. Add the Git Fusion mps-public repo, which is the interface to
   Ravenbrook's Perforce. ::

     git remote add ravenbrook ssh://git@perforce.ravenbrook.com:1622/mps-public

#. If the branch to be merged is in a third-party repo (such as a
   fork), then add that, e.g.::

     git remote add captain-contrib https://gitlab.com/captcontrib/mps.git


4. Merging a development branch
-------------------------------

1. Ensure that the contributor has not expressed any kind of variation
   on the licensing of their work from the material to which they are
   contributing.  (See `"Licensing" in "Contributing to the MPS"
   <../contributing.rst#licensing>`.)

   If they have, this procedure fails.  Talk to them or get someone to
   talk to them about the issue.

2. Fetch the branch that you are going to merge to a local branch
   using the MPS durable branch naming convention,
   "branch/DATE/TOPIC", e.g. ::

     git fetch captain-contrib mps-speed-hax:branch/2023-01-06/speed-hax

   Double check you've got this right.  Using the wrong branch naming
   `causes permanent pollution in the Ravenbrook Perforce repository
   <https://info.ravenbrook.com/mail/2023/01/07/15-06-41/0/>`_.

3. Merge master with the branch::

     git checkout branch/2023-01-06/speed-hax
     git merge ravenbrook/master

   You may need to resolve conflicts.  If you can't resolve conflicts
   yourself, you may need to involve the original author of the
   branch.  If you still can't resolve conflicts, this procedure
   fails.

   [What would be the default outcome from the GitHub interface, using
   the "merge" button?  [Chaser324_2017]_ claims it only works for
   fast-forwards, which we don't want, because they discard records of
   the merge happening, including who did it.  RB 2023-01-07]

4. Build and test the results locally.  For example::

     make -C code -f lii6gc.gmk testci testansi testpollnone testmmqa

   See `design.mps.tests <../design/tests.txt>`_ for details and other
   platforms.

   If tests do not pass, review your conflict resolution from step 4,
   and if that doesn't resolve things, the procedure fails, and you
   need to go back to the source of the branch, e.g. the pull request
   and its original author.  Something's wrong!

5. Push the branch to the Ravenbrook MPS GitHub repository to trigger
   building and testing on all target platforms using Travis CI. ::

     git push github branch/2023-01-06/speed-hax:branch/2023-01-06/speed-hax

   You will need to wait for results from Travis CI.  [Add details of
   how to see them.  RB 2023-07-01] [Some sort of
   ``--force-with-lease`` option may be required if we're pushing back
   to the same branch we started with, such as when it's a Ravenbrook
   development branch.  RB 2023-07-01]

   See step 5 about what to do if tests do not pass.

   Note: This potentially creates a branch in the GitHub repo ahead
   of Git Fusion doing so, but it will the same name, because of the
   Git Fusion mapping, and so the result is the same as if it had come
   in via Perforce.

6. Replace the master with your branch, effecting the merge::

     git checkout master
     git merge --ff-only branch/2023-01-06/speed-hax

   [There should not have been any further changes on master, and
   ``--ff-only`` checks for that.  The merge commit we want on master
   is made in step 4.  RB 2023-01-07]

7. Push master and the branch to Perforce via Git Fusion::

     git push ravenbrook master branch/2023-01-06/speed-hax

   If this fails because someone else has submitted changes to the
   master codeline since you started, pull those changes and go back
   to step 3 ::

     git pull ravenbrook master

8. After a bit [how long? RB 2023-01-07] check that gitpushbot has
   pushed the result to the Ravenbrook MPS repo on GitHub.  [And do
   what if it doesn't?  RB 2023-01-07]


A. References
-------------

.. [Chaser324_2017] "GitHub Standard Fork & Pull Request Workflow";
                    Chase Pettit; 2017;
                    <https://gist.github.com/Chaser324/ce0505fbed06b947d962#automatically-merging-a-pull-request>.

.. [GDR_2020-09-03] "Re: Possible MPS help"; Gareth Rees; 2020-09-03;
		    <https://info.ravenbrook.com/mail/2020/09/03/13-02-35/0/>.

.. [GDR_2014-01-09] "Memory Pool System branching and merging
		    procedures"; Gareth Rees; 2014-01-09;
		    <https://info.ravenbrook.com/project/mps/master/procedure/branch-merge>,
		    <https://github.com/Ravenbrook/mps/blob/e78c6e16735d7f16ef86a7f2f8356791a18c8a6e/procedure/branch-merge.rst>.


B. Document History
-------------------

==========  =====  ==================================================
2023-01-07  RB_    Created.
==========  =====  ==================================================

.. _RB: mailto:rb@ravenbrook.com


C. Copyright and License
------------------------

Copyright © 2014–2023 `Ravenbrook Limited <https://www.ravenbrook.com/>`_.

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

.. checked with rst2html -v pull-request-merge.rst > /dev/null
.. end
