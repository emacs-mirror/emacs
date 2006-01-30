MEMORY POOL SYSTEM KIT README

Richard Brooksby, Ravenbrook Limited, updated 2006-01-30


CONTENTS

  1. Introduction
  2. Getting started
  3. The structure of the kit
  4. Developing using the kit
  5. Testing
  6. Building and testing a release
  7. Contributing your work
  8. Getting help
  9. Licensing
  A. References
  B. Document history
  C. Copyright and license


1. INTRODUCTION

This is the Memory Pool System Kit -- a compelete set of sources for
using, modifying, and adapting the MPS.

The Memory Pool System (MPS) is a very general, adaptable, flexible,
reliable, and efficient memory management system.  It permits the
flexible combination of memory management techniques, supporting manual
and automatic memory management, in-line allocation, finalization,
weakness, and multiple concurrent co-operating incremental generational
garbage collections.  It also includes a library of memory pool classes
implementing specialized memory management policies.

For a more detailed overview of the MPS, see "The Memory Pool System:
Thirty person-years of memory management development goes Open Source"
<http://www.ravenbrook.com/project/mps/doc/2002-01-30/ismm2002-paper/>
[RB 2002-01-30].

This kit is distributed under an open source license.  See under
"Licensing", below.

This document is a brief introduction to the kit.

The readership of this document is anyone who wants to modify, adapt, or
extend the MPS.

This document is not confidential.


2. GETTING STARTED

The MPS Kit is a complete set of sources and documentation to enable
third parties to use, modify, and adapt the MPS.

For Windows, the kit is distributed as the self-extracting archive
"mps-kit-1.106.1.exe", and also as the ZIP archive
"mps-kit-1.106.1.zip", which may be unpacked using WinZip.

For Unix and Mac OS X, the integration kit is distributed as the tarball
"mps-kit-1.106.1.tar.gz".  Unpack it using the command "gunzip -c
mps-kit-1.106.1.tar.gz | tar xvf -", or by dropping the file onto
StuffIt Expander under Mac OS X.

The top-level file "index.html" in the sources indexes many other files,
and is a good place to start.  You should read the user manuals and the
high level design documents.


3. THE STRUCTURE OF THE KIT

The Integration Kit is a copy of the sources used to develop and
maintain the MPS at Ravenbrook Limited <http://www.ravenbrook.com/>. The
"sources" are all the things that go to make up the product, not just
source code files.  For example, the sources for the manuals are here
too, because we maintain them in parallel with the code.

The sources at Ravenbrook are mirrored from our Perforce configuration
management server onto our internal web server at info.ravenbrook.com,
and are designed to be browsed on that server.  You will find HTML
documents which index many of the directories.  Read these, starting
with the top level "index.html".  You might find some links to
"info.ravenbrook.com" or "/project/mps/...".  Sorry about that -- it's
partly due to the nature of HTML links.  You will find most project
documents from the MPS Project page
<http://www.ravenbrook.com/project/mps/>, including many documents
which aren't "sources" but may still be useful to understand the history
of the project.

Some project documents are client confidential.  The MPS project has a
long history, and it will take some time to sort through the documents
to see which can be published.  If there's a particular document you
want to see, please write to us <mps-questions@ravenbrook.com> and we'll
see what we can do.

Many documents are referenced by tags, which look like
"design.mps.buffer" or "req.dylan".  These are references to the
obsolete Memory Management Information System [RB 2002-06-18].  We are
working on translating these documents to HTML.  There are a vast
number.  We've only been able to include a few key documents in the open
source release so far.


4. DEVELOPING USING THE KIT

The MPS can be built on a wide variety of platforms.  For Unix-like
platforms you will need the GNU Make tool.  Some platforms (such as
Linux and Mac OS X) have GNU Make as their default make tool.  For
others you will need to get and install it.  (It's available free from
<ftp://ftp.gnu.org/gnu/make/>.)  On Windows platforms the NMAKE tool is
used.  This comes with Microsoft Visual C++.

The MPS can also be built on Mac OS 7 through Mac OS 9 using MPW or
Metrowerks Codewarrior, but neither of these builds have been maintained
for some time, and probably no longer work.  We've included the
makefiles/project files as a starting point.

              Table 1. MPS makefiles and platforms

  Makefile        OS              Architecture    Compiler

  fri4gc.gmk      FreeBSD         Intel IA32      GCC
  iam4cc.gmk      Irix 6 N32      MIPS IV         CC
  lii3eg.gmk      Linux           Intel IA32      EGCS
  lii4gc.gmk      Linux           Intel IA32      GCC
  lippgc.gmk      Linux           POWER(32)       GCC
  o1alcc.gmk      OSF/1           Alpha           Digital C
  o1algc.gmk      OSF/1           Alpha           GCC
  sos8cx.gmk      Solaris         SPARC V8        CXREF
  sos8gc.gmk      Solaris         SPARC V8        GCC
  sos8gp.gmk      Solaris         SPARC V8        GCC with profiling
  sos9sc.gmk      Solaris         SPARC V9        SunPro C
  sus8gc.gmk      SunOS           SPARC V8        GCC
  xcppgc.gmk      Mac OS X        PowerPC         GCC

  w3almv.nmk      Windows         Alpha           Microsoft C
  w3i3mv.nmk      Windows         Intel IA32      Microsoft C
  w3ppmv.nmk      Windows         PowerPC         Microsoft C

  s7ppac/Makefile Mac OS 7-9      PowerPC         Apple Mr C
  s7ppmw.sit      Mac OS 7-9      PowerPC         Metrowerks Codewarrior

To build all MPS targets on Unix-like platforms, change to the "code"
directory and type:

  make -f <makefile>

where "make" is the command for GNU Make.  (Sometimes this will be
"gmake" or "gnumake".)

To build just one target, type:

  make -f <makefile> <target>

To build a restricted set of targets for just one variety, type:

  make -f <makefile> 'VARIETY=<variety>' <target>

For example, to build just the "cool internal" variety of the "amcss"
test on FreeBSD:

  gmake -f fri4gc.gmk 'VARIETY=ci' amcss

On Windows platforms you need to run "VCVARS32.BAT" to initialize your
environment variables.  Then type:

  nmake /f w3i3mv.nmk

To build just one target, type:

  nmake /f w3i3mv.nmk <target>

And so on.

The output of the build goes to a directory named after the platform
(e.g. "fri4gc") so that you can share the source tree across platforms
using NFS, for example.  Building generates "mps.a" or "mps.lib" or
equivalent, a library of object code which you can link with your
application, subject to the MPS licensing conditions (see under
"Licensing", below).  It also generates a number of test programs,
such as "amcss" (a stress test for the Automatic Mostly-Copying pool
class).


5. TESTING

There is an extensive regression test suite in the "test" directory, but
we have not yet had time to make it easy to run.


6. BUILDING A RELEASE

The procedure for building a release of the MPS is in
"procedure/release-build".  This will only be of interest if you are
intending to maintain and publish a variant of the MPS.  If you do want
to do this, please get in touch with us <mps-questions@ravenbrook.com>
to discuss it.


7. CONTRIBUTING YOUR WORK

We'd like to include improvements to the MPS in the distributions from
Ravenbrook Limited.  Please consider putting your work under a
compatible open source license and sending it to us.

It will help a great deal if your work is self-consistent.  This means
that the documentation is up to date (even if only with sketchy but
accurate notes) and the procedures have been maintained.


8. GETTING HELP

You can obtain expert professional support for the MPS from Ravenbrook
Limited <http://www.ravenbrook.com/>, the developers of the MPS, who
have many years of experience in commercial memory management systems.
Write to us <mps-questions@ravenbrook.com> for more information.

You might also want to join the MPS discussion mailing list.  To join,
send a message with the word "subscribe" in the body to
mps-discussion-request@ravenbrook.com or send the word "help" for
general information.


9. LICENSING

The MPS is distributed under an open source license (see the file
"license.txt").  The license is designed to make it possible for you to
use the MPS in your own projects, provided that you either don't
distribute your product, or your product is open source too.

If you are developing a closed source product and want to use the MPS
you must license under commercial terms from Ravenbrook Limited
<http://www.ravenbrook.com/>.  Please write to us
<mps-questions@ravenbrook.com> for more information.


A. REFERENCES

[RB 2002-01-30] "The Memory Pool System: Thirty person-years of memory
management development goes Open Source"; Richard Brooksby; Ravenbrook
Limited; 2002-01-30;
<http://www.ravenbrook.com/project/mps/doc/2002-01-30/ismm2002-paper/>.

[RB 2002-06-18] "The Obsolete Memory Management Information System";
Richard Brooksby; Ravenbrook Limited; 2002-06-18;
<http://www.ravenbrook.com/project/mps/doc/2002-06-18/obsolete-mminfo/>.


B. DOCUMENT HISTORY

2002-05-20  RB  Created based on template from P4DTI project.
2002-06-18  NB  Minor updates and corrections.
2002-06-18  RB  Removed obsolete requirement for MASM.
2002-06-19  NB  Added note on self-extracting archive
2006-01-30  RHSK  Update from "1.100.1" to "1.106.1".


C. COPYRIGHT AND LICENSE

Copyright (C) 2001-2002, 2006 Ravenbrook Limited 
<http://www.ravenbrook.com/>.  All rights reserved.  
This is an open source license.  Contact Ravenbrook 
for commercial licensing options.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are
met:

1. Redistributions of source code must retain the above copyright
notice, this list of conditions and the following disclaimer.

2. Redistributions in binary form must reproduce the above copyright
notice, this list of conditions and the following disclaimer in the
documentation and/or other materials provided with the distribution.

3. Redistributions in any form must be accompanied by information on how
to obtain complete source code for this software and any
accompanying software that uses this software.  The source code must
either be included in the distribution or be available for no more than
the cost of distribution plus a nominal fee, and must be freely
redistributable under reasonable conditions.  For an executable file,
complete source code means the source code for all modules it contains.
It does not include source code for modules or files that typically
accompany the major components of the operating system on which the
executable file runs.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR
PURPOSE, OR NON-INFRINGEMENT, ARE DISCLAIMED. IN NO EVENT SHALL THE
COPYRIGHT HOLDERS AND CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF
USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

$Id$
