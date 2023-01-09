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

Ravenbrook is currently (2023-01) `migrating the MPS project to git
(and GitHub) <https://github.com/Ravenbrook/mps/issues/98>`_ and this
is likely to modify and simplify this procedure.


2. Pre-merge checklist
----------------------

#. Is there a permanent visible document (issue, job), referenced by
   the branch, recording the problem that is solved by the changes in
   the branch?

#. Does the branch solve the problem?

#. Is there an automated test case that demonstrates the problem
   (without the branch) and that that the problem is solved (with the
   branch)?

#. If there are interface changes, are they documented?

#. If the changes are significant and user-visible, is there an update
   to the release notes (``manual/source/release.rst``)?

#. Has there been a code review?

#. Has the contributor licensed their work?

   By default, the work is licensed if the contributor has not
   expressed any kind of variation on the licensing of their work from
   the material to which they are contributing.  (See `"Licensing" in
   "Contributing to the MPS" <../contributing.rst#licensing>`_.)

   If they have, talk to them or get someone to talk to them about the
   matter.  Do not proceed.

#. Does the branch build and pass tests on all `target platforms
   <../readme.txt>`_?

   If the branch is in the Ravenbrook MPS repo on GitHub then Travis
   CI should have run builds.  Look for a successful build in the
   `Travis CI build history for the repo`_.  If there is a failed
   build you should not execute this procedure, but talk to the
   contributor about fixing the branch.

   If the branch is in the Ravenbrook MPS repo on GitHub and Travis
   builds are missing, inform sysadmins that Travis CI isn't
   functioning.

   If you have no build and test results, you can still execute this
   procedure, with caution.

#. Does the branch merge cleanly in to master and pass tests on all
   target platforms?

   Travis CI should have run builds of the pull request (i.e. `of a
   merge with master
   <https://docs.travis-ci.com/user/pull-requests/#how-pull-requests-are-built>`_).
   To check, either:

   - Look for "All checks have passed" in the pull request on GitHub.
     Expand "Show all checks", and look for build success messages
     from Travis CI.

   - Look for a successful build in the `Travis CI build history for
     the repo`_.

   Success by Travis CI is a strong indication that this procecure
   will be quick and successful.

   If Travis builds failed, you can still execute this procedure if
   you believe that the failure is due to merge conflicts that you are
   willing to resolve.

   If Travis builds are missing, inform sysadmins that Travis CI isn't
   functioning.

   If you have no build and test results for the merge, then you can
   still execute this procedure if:

   #. you believe there are only merge conflicts,
   #. you're willing to try to resolve those conflicts, and
   #. you're prepared to test on all target platforms.

[Checklist items for Customer-specific branches from branch-merge.rst
omitted for now.  RB 2023-01-07]

.. _Travis CI build history for the repo: https://app.travis-ci.com/github/Ravenbrook/mps/builds


3. Prerequisite steps
---------------------

These steps will only rarely need repeating.

#. Ensure your public key is submitted in Perforce at
   //.git-fusion/users/USER/keys/

#. Ensure your e-mail address is submitted in Perforce at
   //.git-fusion/users/p4gf_usermap and matches your Perforce user
   record.

#. Clone the Ravenbrook MPS GitHub repository and name the remote
   "github".  This will give you access to Travis CI to build and test
   the merge.  (If you're an MPS developer you can use your existing
   repo.)  ::

     git clone -o github git@github.com:Ravenbrook/mps.git

#. Add the Git Fusion mps-public repo, which is the interface to
   Ravenbrook's Perforce. ::

     git remote add perforce ssh://git@perforce.ravenbrook.com:1622/mps-public


4. Merging a development branch
-------------------------------

1. `Fetch the pull request branch`_ to a local branch using the MPS
   durable branch naming convention, "branch/DATE/TOPIC", e.g. ::

     git fetch github pull/93/head:branch/2022-12-23/hardened-runtime

   If the branch to be merged is in a third-party repo, such as a fork
   not on GitHub, you can fetch it usina a remote, e.g.::

     git remote add captain-contrib https://gitlab.com/captcontrib/mps.git
     git fetch captain-contrib mps-speed-hax:branch/2023-01-06/speed-hax

   Double check you've got the branch name right.  Using the wrong
   branch naming `causes permanent pollution in the Ravenbrook
   Perforce repository
   <https://info.ravenbrook.com/mail/2023/01/07/15-06-41/0/>`_.

2. Optionally, let other people know that you're working on a merge
   into master.  Negotiate to avoid racing them to push to the master
   codeline (step 7) because that will create extra merging work.

3. Merge master with the branch::

     git pull perforce master:master
     git checkout branch/2023-01-06/speed-hax
     git merge master

   You may need to resolve conflicts.  If you can't resolve conflicts
   yourself, you may need to involve the original author of the
   branch.  If you still can't resolve conflicts, this procedure
   fails.

4. Build and test the results locally.  For example::

     make -C code -f lii6gc.gmk testci testansi testpollnone testmmqa

   See `design.mps.tests <../design/tests.txt>`_ for details and other
   platforms.

   If tests do not pass, review your conflict resolution from the
   merge (step 3), and if that doesn't fix things, the procedure
   fails, and you need to go back to the source of the branch,
   e.g. the pull request and its original author.  Something's wrong!

5. Push the branch to the Ravenbrook MPS GitHub repository to trigger
   building and testing on all target platforms using Travis CI. ::

     git push github branch/2023-01-06/speed-hax

   You will need to wait for results from Travis CI.  [Add details of
   how to see them.  RB 2023-07-01]

   See build (step 4) about what to do if tests do not pass.

   Note: This potentially creates a branch in the GitHub repo ahead
   of Git Fusion doing so, but it will the same name, because of the
   Git Fusion mapping, and so the result is the same as if it had come
   in via Perforce.

6. Submit your merged branch to Perforce::

     git push Perforce branch/2023-01-06/speed-hax

7. Submit your merged branch to the Perforce master codeline::

     git push perforce branch/2023-01-06/speed-hax:master

   **Important**: Do *not* force this push.

   If this fails, someone has submitted changes to the master codeline
   since you started.  Go back to merging (step 3).

8. Optionally, if and *only if* the Perforce push (step 7) succeeded,
   you can also push to GitHub::

     git push github branch/2023-01-06/speed-hax:master

   If you don't do this, then within `30 minutes
   <https://info.ravenbrook.com/infosys/robots/gitpushbot/etc/crontab>`_
   check that the merge appears in `the commits in the Ravenbrook MPS
   repo on GitHub <https://github.com/Ravenbrook/mps/commits/master>`_.

   If they do not appear:

   1. Check email for error messages from gitpushbot and resolve them.

   2. Check (or ask a sysadmin to check) that gitpushbot is running
      on Berunda and restart it if necessary, or ask a sysadmin to do
      this.

.. _Fetch the pull request branch: https://docs.github.com/en/pull-requests/collaborating-with-pull-requests/reviewing-changes-in-pull-requests/checking-out-pull-requests-locally#modifying-an-inactive-pull-request-locally


5. Rationale
------------

This section explains why the procedure is like it is.  It's intended
for people who want to vary the procedure on the fly, or make
permanent changes to it.  In the latter case, update this section!

5.1. Why not press the GitHub merge button?
-------------------------------------------

GitHub provides a merge button on pull requests.  According to
[Chaser324_2017]_ it only works for branches that can fast-forward
master, and also only creates fast-forwards.

There are two reasons this is undesirable.

Firstly, it's quite likely that a pull request has a branch that isn't
at the tip of master and can't be fast-forwarded.  It's possible to
rebase such branches only if Perforce has never seen them, because
Perforce does not permit branch history to be rewritten.  We could
have a more complicated procedure involving making a new rebased
branch, but the result would be less good.

Secondly, we would like to avoid rewriting history and the destruction
of information on the grounds that it is bad software engineering, and
so want to discourage rebasing.

And it's for this reason we also want to avoid fast-forwards of
master.  A fast-forward means there is no commit that records the fact
that there has been a merge, by whom, from where, etc.  It discards
that information.  Therefore we want to discourage fast-forwards of
master in favour of merges.


5.2. Why the "durable" branch names?
------------------------------------

It's common in Git culture to delete branches once they've been
merged [Ardalis_2017]_ but this destroys information that has been
invaluable to MPS quality in the past.

It destroys the connection between the branch name and a series of
changes made together, intentionally, for a purpose.  That makes it
hard to identify those changes together.  It makes it hard to *refer*
to those changes from documents and code (referring to the hash of the
last commit is not as good).  It makes it hard to investigate the
intention of changes discovered by tools such as ``git blame`` or ``p4
annotate``.

Essentially, it throws away history and dissolves the branch into the
big global graph of git commits.  That's not good configuration
management.

The MPS has an ongoing policy of retaining all of its intentional
history, and that includes branch names.  Branch names in the MPS
repository are intended to last forever.  That is why they have
"durable" names.

This policy has persisted over decades through more than one SCM
system, and will persist when Git has been replaced by the next one.


A. References
-------------

.. [Ardalis_2017] "Why Delete Old Git Branches?"; Steve Ardalis;
		  2017-07-20;
		  <https://ardalis.com/why-delete-old-git-branches/>.

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
