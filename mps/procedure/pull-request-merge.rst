===============================================
Memory Pool System pull request merge procedure
===============================================

:author: Richard Brooksby
:organization: Ravenbrook Limited
:date: 2023-01-07
:confidentiality: public
:copyright: See `C. Copyright and License`_
:readership: MPS developers, trainee integrators


1. Introduction
---------------

This document contains a procedure for merging a branch received via a
GitHub "pull request".

Ravenbrook is currently (2023-01) `migrating the MPS project to git
(and GitHub) <https://github.com/Ravenbrook/mps/issues/98>`_ and that
will greatly simplify this procedure.

This procedure assumes a pull request has been received via GitHub,
but that dependency is light.  The procedure can be varied for other
sources, even where the "pull request" is received by email or some
other traceable document.

This document was created as a combination of the process improvement
steps from our Perforce-based "Memory Pool System branching and
merging procedures" [GDR_2014-01-09]_ with Gareth Rees' email
containing the working steps for git / GitHub merges, "Re: Possible
MPS Help" [GDR_2020-09-03]_ .


2. Pre-merge checklist
----------------------

The answers to these questions should be "yes".  If the answer to a
question isn't "yes", record that, and why, in response to the pull
request (and maybe suggest what to do about it).  When you finish the
checklist, decide whether to continue with the procedure.

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

   If they have expressed a variation, talk to them or get someone to
   talk to them about the matter.  *Do not proceed with merging*.

#. Does the branch build and pass tests on all `target platforms
   <../readme.txt>`_?

   If the branch is in the `Ravenbrook MPS repo on GitHub`_ then
   Travis CI should have run builds.  Look for a successful build in
   the `Travis CI build history for the repo`_.  If there is a failed
   build you should not execute this procedure, but talk to the
   contributor about fixing the branch.

   If the branch is in the `Ravenbrook MPS repo on GitHub`_ and Travis
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

.. _Travis CI build history for the repo: https://app.travis-ci.com/github/Ravenbrook/mps/builds


3. Prerequisite steps
---------------------

These steps will only rarely need repeating.

#. Ensure your public SSH key is submitted in Perforce at
   //.git-fusion/users/USER/keys/

#. Ensure your e-mail address is submitted in Perforce at
   //.git-fusion/users/p4gf_usermap and matches your Perforce user
   record.

#. Clone the Ravenbrook MPS GitHub repository and name the remote
   "github".  This will give you access to Travis CI to build and test
   the merge.  (If you're an MPS developer you can use your existing
   repo.)  ::

     git clone -o github git@github.com:Ravenbrook/mps.git
     cd mps

#. Set your e-mail address for commits to the repo to match the one in
   your Perforce user record. ::

     git config user.email spqr@ravenbrook.com

#. Add the Git Fusion mps-public repo, which is the interface to
   Ravenbrook's Perforce. ::

     git remote add perforce ssh://git@perforce.ravenbrook.com:1622/mps-public


4. Merging procedure
--------------------

1. `Fetch the pull request branch`_ to a local branch using the MPS
   durable branch naming convention, "branch/DATE/TOPIC", e.g. ::

     git fetch github pull/93/head:branch/2023-01-06/speed-hax

   If the pull request is from the `Ravenbrook MPS repo on GitHub`_,
   and its branch already has a conventional name, then use the
   existing name.

   If the branch to be merged is in a third-party repo, such as a fork
   not on GitHub, you can fetch it using a remote, e.g.::

     git remote add captain-contrib https://gitlab.com/captcontrib/mps.git
     git fetch captain-contrib mps-speed-hax:branch/2023-01-06/speed-hax

   Double check you've got the branch name right.  Using the wrong
   branch naming `causes permanent pollution in the Ravenbrook
   Perforce repository
   <https://info.ravenbrook.com/mail/2023/01/07/15-06-41/0/>`_.

2. Optionally, let other people know that you're working on a merge
   into master.  Negotiate to avoid racing them to push to the master
   codeline (step 7) because that will create extra merging work.

3. Ensure your local master is up to date with Perforce::

     git pull --ff-only perforce master

   If this doesn't succeed, then GitHub's master and Perforce's master
   are in out of sync, and this procedure fails.  [It may be possible
   to quickly fix that here and now and continue.  RB 2023-01-12]

4. Merge the branch in to your local master::

     git merge --no-ff branch/2023-01-06/speed-hax

   Edit the commit message to say something like::

     Merging branch/2023-01-06/speed-hax for GitHub pull request 93
     <https://github.com/Ravenbrook/mps/pull/93>.

   Do *not* just say "pull request 93" without a link, because that
   number is local to, and only valid on GitHub.  Bear this in mind
   for other references.  Do add any other links that would increase
   traceability.

   You may need to resolve conflicts.  If you can't resolve conflicts
   yourself, you may need to involve the original author of the
   branch.  If you still can't resolve conflicts, this procedure
   fails.

5. [This step is only necessary if the merge was non-trivial, there
   has been rebasing, or CI results are not available.  RB 2023-01-12]
   Build and test the results locally.  For example::

     make -C code -f lii6gc.gmk testci testansi testpollnone testmmqa

   See `design.mps.tests <../design/tests.txt>`_ for details and other
   platforms.

   If tests do not pass, review your conflict resolution from the
   merge (step 4), and if that doesn't fix things, the procedure
   fails, and you need to go back to the source of the branch,
   e.g. the pull request and its original author.  Something's wrong!

6. [This step is only necessary if the merge was non-trivial, there
   has been rebasing, or CI results are not available.  RB 2023-01-12]
   Push the merge to a fresh branch in the Ravenbrook MPS GitHub
   repository to trigger building and testing on all target platforms
   using Travis CI. ::

     git push github merge/2023-01-06/speed-hax

   You will need to wait for results from Travis CI.  [Add details of
   how to see them.  RB 2023-07-01]

   See build (step 5) about what to do if tests do not pass.

   Note: This potentially creates a branch in the GitHub repo ahead
   of Git Fusion doing so, but it will the same name, because of the
   Git Fusion mapping, and so the result is the same as if it had come
   in via Perforce.

7. Submit your merged master and the branch to Perforce::

     git push perforce master branch/2023-01-06/speed-hax

   **Important**: Do *not* force this push.

   If this fails, someone has submitted changes to the master codeline
   since you started.

   You can attempt to rebase your work on those changes::

     git pull --rebase perforce

   then go back to testing (step 5).

   Alternatively, you could undo your merging work::

     git reset --hard perforce/master

   then go back to merging (step 4).

8. Optionally, if and *only if* the Perforce push (step 7) succeeded,
   you can also push to GitHub::

     git push github master branch/2023-01-06/speed-hax

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


5.1. Why not rebase or squash merge?
------------------------------------

We would like to avoid rewriting history and the destruction of
information on the grounds that it destroys information that could be
important to the engineering of the MPS, such as tracking down
defects, comprehending the intention of changes.  So want to
discourage rebasing or squashing.

We want to avoid fast-forwards of master.  A fast-forward means there
is no commit that records the fact that there has been a merge, by
whom, from where, for what purpose, etc.  It discards that
information.  Therefore we want to discourage fast-forwards of master
in favour of merges.  (Annoyingly, GitHub only provides `branch
protection that enforces the opposite
<https://docs.github.com/en/repositories/configuring-branches-and-merges-in-your-repository/defining-the-mergeability-of-pull-requests/about-protected-branches#require-linear-history>`_!)
See also `5.3. Why the "durable" branch names?`_.

We also want to avoid `squash merges
<https://docs.github.com/en/pull-requests/collaborating-with-pull-requests/incorporating-changes-from-a-pull-request/about-pull-request-merges#squash-and-merge-your-commits>`_.
A squash merge compresses development history into a single commit,
destroying the record of what happened during development and the
connection to the branch.

The main motivation for fast-forwards and squashes appears to be to
simplify the branching history so that it's easier to understand.
Better tools and interfaces are no doubt required for analysing Git
history.  These will emerge.  And they will be able to analyse the
history that we are creating today.

There is also a strong tendency among developers to "correct" mistakes
and edit history to reflect "what should have happened" or "what I
meant to do", treating history like code.  But it's the function of
version control to protect software against well-intentioned mistakes.
Git is bad at remembering changes to history (it has no meta-history)
and so we should not edit it.


5.2. Why not press the GitHub merge button?
-------------------------------------------

We cannot use the GitHub pull request merge button because it would
put the GitHub master branch out of sync with (ahead of) Perforce.
Currently, Perforce is the authoritative home of the MPS, and the Git
repository is a mirror.

According to `GitHub's "About pull request merges"
<https://docs.github.com/en/pull-requests/collaborating-with-pull-requests/incorporating-changes-from-a-pull-request/about-pull-request-merges>`_:

  When you click the default Merge pull request option on a pull
  request on GitHub.com, all commits from the feature branch are added
  to the base branch in a merge commit.

`Travis CI builds and tests this merge in advance <https://docs.travis-ci.com/user/pull-requests/#how-pull-requests-are-built>`_:

  Rather than build the commits that have been pushed to the branch
  the pull request is from, we build the merge between the source
  branch and the upstream branch.

So, `once Git becomes the home
<https://github.com/Ravenbrook/mps/issues/98>`_ we will be able to use
the button to to replace sections 3 and 4, the procedure, but not
section 2, the pre-merge checklist.  We may be able to incorporate the
checklist into GitHub's interface using a `pull request template
<https://docs.github.com/en/communities/using-templates-to-encourage-useful-issues-and-pull-requests/creating-a-pull-request-template-for-your-repository>`_.


5.3. Why the "durable" branch names?
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

Note: `GitHub branch protection rules`_ are `enabled
<https://github.com/Ravenbrook/mps/settings/branches>`_ on the
`Ravenbrook MPS repo on GitHub`_ and should prevent deletion.

.. _Ravenbrook MPS repo on GitHub: https://github.com/Ravenbrook/mps

.. _GitHub branch protection rules: https://docs.github.com/en/repositories/configuring-branches-and-merges-in-your-repository/defining-the-mergeability-of-pull-requests/about-protected-branches#require-linear-history


A. References
-------------

.. [Ardalis_2017] "Why Delete Old Git Branches?"; Steve Ardalis;
		  2017-07-20;
		  <https://ardalis.com/why-delete-old-git-branches/>.

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
