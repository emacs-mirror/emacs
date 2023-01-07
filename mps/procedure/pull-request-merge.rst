===============================================
Memory Pool System pull request merge procedure
===============================================

:author: Richard Brooksby
:organization: Ravenbrook Limited
:date: 2023-01-07
:confidentiality: public
:copyright: See `C. Copyright and License`_
:readership: MPS developers
:status: draft


1. Introduction
---------------

This document contains a procedure for merging a branch received via a
GitHub "pull request".

This is a draft procedure based on:
- "Re: Possible MPS Help" [GDR_2014-01-09]_
- "Memory Pool System branching and merging procedures" [GDR_2020-09-03]_.

Some of the questions that need resolving are noted in square
brackets.

Ravenbrook is currently (2023-01) migrating the MPS project to git
(and GitHub) and this is likely to modify this procedure.


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

#. Ensure your public key is in //.git-fusion/users/USER/keys/ in
   Perforce, submitting it if necessary.

#. Ensure your e-mail address is in //.git-fusion/users/p4gf_usermap
   in Perforce and matches your Perforce user record, submitting it if
   necessary.

#. Clone the mps-public repo and name the remote “ravenbrook”::

     git clone -o ravenbrook ssh://git@perforce.ravenbrook.com:1622/mps-public

#. Add the GitHub repo as a new remote::

     git remote add github git@github.com:Ravenbrook/mps.git


4. Merging a development branch
-------------------------------

1. Ensure that the contributor has executed the appropriate assignment
   of copyright.

2. Fetch the branch that you are going to merge from GitHub, for
   example::

     BRANCH=branch/2020-08-23/walk-docs
     git fetch github "$BRANCH”

3. Make a local tracking branch::

     git checkout "$BRANCH”

4. Decide whether you are going to merge by fast-forward or not. This
   all depends on how you want the result of the merge to appear in
   Perforce.

   A fast-forward merge means that the individual commits from the
   branch are replayed on top of master and so each commit from the
   branch becomes a separate change in Perforce, with its own change
   description. This would be appropriate when each commit stands on
   its own. To make a fast-forward merge, rebase the branch on master,
   resolve any conflicts, and force-push it back to GitHub::

     git rebase ravenbrook/master
     # resolve conflicts here
     git push --force-with-lease github "$BRANCH:$BRANCH"

   A non-fast-forward merge makes a single merge commit with two
   parent commits (the heads of master and the branch
   respectively). This would be appropriate when the commits on the
   branch don’t stand on their own and you want to have a single
   change in Perforce.

   [This needs review, and in any case a decision procedure is needed
   here.  What would be the default outcome from the GitHub interface?
   RB 2023-01-07]

5. Merge the branch to master and resolve any conflicts::

     git checkout master
     # resolve conflicts here
     git merge "$BRANCH”

6. Run tests, for example::

     make -C code -f lii6gc.gmk testci testansi testpollnone testmmqa

   if you are on Linux.

7. Push master (and if non-fast-forward, the branch) to mps-public::

     git push ravenbrook master "$BRANCH”

8. Wait for gitpushbot to push the result to GitHub.


A. References
-------------

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

Copyright © 2014–2020 `Ravenbrook Limited <https://www.ravenbrook.com/>`_.

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

.. end
