Memory Pool System Release Build Procedure
==========================================
:author: Richard Brooksby
:organization: Ravenbrook Limited
:date: 2002-06-17
:revision: $Id$
:confidentiality: public
:copyright: See `C. Copyright and License`_


1. Introduction
---------------

This is the procedure for building a generic release of the Memory Pool
System (an “MPS Kit”) from the version sources.

The intended readership of this document is Ravenbrook development
staff. (If you are a user of the MPS, and want to build object code from
an MPS Kit, please see the ``readme.txt`` file in the kit.)

This document is not confidential.

All relative paths are relative to
``//info.ravenbrook.com/project/mps/``.


2. Prerequisites
----------------

#. Make sure you have a version branch from which to make the release.
   If not, follow the `version creation procedure <version-create>`_
   first.

   .. _version-create: version-create

#. Make sure that you have rights to push to the ``mps-temporary``
   repository on GitHub. If not, follow the `Becoming a Ravenbrook
   team member procedure <git-fusion>`_ first.

   .. _git-fusion: https://info.ravenbrook.com/procedure/git-fusion


3. Setting up for release
-------------------------

#. Choose a *RELEASE* name of the form *VERSION.N* (for example,
   1.111.0), where *VERSION* is the number of the version you’re
   releasing, and *N* is the first unused release number (starting at
   zero). Look in the index of releases (``release/index.html``) for
   existing release numbers for your version::

        VERSION=A.BBB
        RELEASE=$VERSION.N

#. Check that the macro ``MPS_RELEASE`` in ``code/version.c`` has the
   correct value.

#. Check that ``readme.txt`` contains an up-to-date description of the
   release you intend to build. For example, is the list of supported
   platforms still correct?

#. Check that ``manual/source/release.rst`` contains a section with an
   up-to-date description of significant user-visible changes since
   the previous release.

#. Determine the *CHANGELEVEL* at which you’re going to make the
   release. This will usually be the latest submitted changelevel on
   the branch from which you are making the release; to get it, use
   ``p4 changes -m 1``::

        CHANGELEVEL=$(p4 changes -m 1 ... | cut -d' ' -f2)


4. Pre-release testing
----------------------

#. Sync the version sources to precisely the *CHANGELEVEL* you
   determined above, with no extraneous files, by using the following
   procedure::

        p4 opened version/$VERSION/...
        # should output "version/$VERSION/... - file(s) not opened on this client."
        p4 revert version/$VERSION/...
        rm -rf version/$VERSION
        p4 sync -f version/$VERSION/...@$CHANGELEVEL

   Note that the ``revert`` and ``sync -f`` are necessary, otherwise
   opened files may be left in place, or writeable-on-client files may
   be omitted; see [RHSK_2008-10-16]_.

#. Run the test suite::

        (cd version/$VERSION && ./configure && make test)

   Check that the test suite passes.

#. Repeat for all supported platforms. On Windows the sequence of
   commands to run the test suite are::

        cd version\$VERSION\code
        nmake /f w3i6mv.nmk testrun

   On other platforms they are as shown above.


5. MPS Kit Tarball and Zip file
-------------------------------

.. note::

   If you are creating a customer-specific variant then vary the
   release name according to the variant, for example,
   ``mps-cet-1.110.0.zip``

On a Unix (including OS X) machine:

#. Create a fresh Perforce client workspace::

        CLIENT=mps-release-$RELEASE
        p4 client -i <<END
        Client: $CLIENT
        Root: /tmp/$CLIENT
        Description: Temporary client for making MPS Kit release $RELEASE
        LineEnd: local
        View:
                //info.ravenbrook.com/project/mps/version/$VERSION/... //$CLIENT/mps-kit-$RELEASE/...
                //info.ravenbrook.com/project/mps/release/$RELEASE/... //$CLIENT/release/$RELEASE/...
	END

#. Sync this client to *CHANGELEVEL*::

        p4 -c $CLIENT sync -f @$CHANGELEVEL

#. Create a tarball containing the MPS sources, and open it for add::

        pushd /tmp/$CLIENT
        mkdir -p release/$RELEASE
        tar czf release/$RELEASE/mps-kit-$RELEASE.tar.gz mps-kit-$RELEASE
        popd
        p4 -c $CLIENT add /tmp/$CLIENT/release/$RELEASE/mps-kit-$RELEASE.tar.gz

#. Switch the Perforce client workspace to Windows (CRLF) line
   endings::

        p4 -c $CLIENT client -o | sed "s/^LineEnd:.local/LineEnd: win/" | p4 client -i

#. Sync the version sources again::

        rm -rf /tmp/$CLIENT/version/$VERSION
	p4 -c $CLIENT sync -f @$CHANGELEVEL

#. Create a zip file containing the MPS sources, and open it for add::

        pushd /tmp/$CLIENT
        mkdir -p release/$RELEASE
        zip -r release/$RELEASE/mps-kit-$RELEASE.zip mps-kit-$RELEASE
        popd
        p4 -c $CLIENT add /tmp/$CLIENT/release/$RELEASE/mps-kit-$RELEASE.zip

#. Submit the release files to Perforce::

        p4 -c $CLIENT submit -d "MPS: adding the MPS Kit tarball and zip file for release $RELEASE."

#. Delete the temporary Perforce client::

        p4 -c $CLIENT client -d $CLIENT
        rm -rf /tmp/$CLIENT


6. Registering the release
--------------------------

#. Edit the index of releases (``release/index.html``) and add the
   release to the table, in a manner consistent with previous releases.

#. Edit the index of versions (``version/index.html``) and add the
   release to the list of releases for *VERSION*, in a manner consistent
   with previous releases.

#. Edit the main MPS Project index page (``index.rst``), updating the
   "Download the latest release" link.

#. Submit these changes to Perforce:

        p4 submit -d "MPS: registered release $RELEASE."

#. Integrate the changes you made on the version branch back to the
   master sources, ignoring changes that don't apply to the master
   sources::

        p4 integrate -r -b $BRANCH
        p4 resolve
        p4 submit -d "Merging updates preparatory to release $RELEASE."

#. Visit the `project
   updater <http://info.ravenbrook.com/infosys/cgi/data_update.cgi>`__,
   select “mps” from the dropdown, and hit “Find releases”.

#. Make a git tag for the release::

        git clone git-fusion@raven.ravenbrook.com:mps-version-$VERSION
        cd mps-version-$VERSION
        git tag -a release-$RELEASE -F - <<END
        Memory Pool System Kit release $RELEASE.
        See <http://www.ravenbrook.com/project/mps/release/>.
        END
        git push --tags git@github.com:Ravenbrook/mps-temporary.git

#. Inform the project manager and staff by e-mail to
   mps-staff@ravenbrook.com.

#. Announce the new release by e-mail to
   mps-discussion@ravenbrook.com. Include a summary of the release
   notes.


A. References
-------------

.. [RHSK_2008-10-16] Richard Kistruck; "revert ; rm ; sync -f";
   Ravenbrook Limited; 2008-10-16;
   http://info.ravenbrook.com/mail/2008/10/16/13-08-20/0.txt

.. [Sphinx] "Sphinx: Python document generator"; http://sphinx-doc.org/


B. Document History
-------------------

==========  =====  ==========================================================
2002-06-17  RB_    Created based on P4DTI procedure.
2002-06-19  NB_    Fixed up based on experience of release 1.100.0.
2004-03-03  RB_    Fixed the way we determine the release changelevel to avoid possible pending changelists.
2005-10-06  RHSK_  Clarify this procedure is for general MPS Kit releases; correct ``cp -r`` to ``-R``. Add: check ``version.c``.
2006-01-19  RHSK_  Correct readership statement, and direct MPS users to the mps-kit readme.
2006-02-16  RHSK_  Use Info-ZIP (free) for Windows archives, not WinZip.
2007-07-05  RHSK_  Releasename now also in ``w3build.bat``.
2008-01-07  RHSK_  Release changelevel was in ``issue.cgi``, now in ``data.py``.
2010‑10‑06  GDR_   Use the project updater to register new releases.
2012‑09‑13  RB_    Don’t copy the ``readme.txt`` to the release directory, since it no longer has that dual role; make the ZIP file on a Unix box with the zip utility, since compatibility has improved.
2013-03-08  GDR_   Add testing step.
2012‑09‑24  RB_    Make sure ZIP files contain files with Windows line endings. Use a fresh Perforce client to avoid any possibility of a clash with working files. Different archive name for custom variants.
2013-03-20  GDR_   Ensure that manual HTML is up to date before making a release.
2014-01-13  GDR_   Make procedure less error-prone by giving exact sequence of commands (where possible) based on experience of release 1.112.0.
==========  =====  ==========================================================

.. _RB: mailto:rb@ravenbrook.com
.. _NB: mailto:nb@ravenbrook.com
.. _RHSK: mailto:rhsk@ravenbrook.com
.. _GDR: mailto:gdr@ravenbrook.com


C. Copyright and License
------------------------

This document is copyright © 2002–2013 `Ravenbrook
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
