MEMORY POOL SYSTEM KIT README

CONTENTS

  1. Introduction
  2. What's new; status
  3. Getting started
  4. The structure of the kit
  5. Developing using the kit
  6. Testing
  7. Building and testing a release
  8. Contributing your work
  9. Getting help
 10. Licensing
  A. References
  B. Document history
  C. Copyright and license


1. INTRODUCTION

This is the Memory Pool System Kit -- a complete set of sources for
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


2. WHAT'S NEW; STATUS

(In Master sources, this is a good place to accumulate a succinct
record of changes.  In a release of the MPS-Kit, this section becomes
the summary of what is new for that release.)

[
......Post 1.108.2 changes:

This is release A.BBB.C, made on YYYY-MM-DD.
Changes from release A.BBB.C-1:

Functional changes to MPS code:

Other changes:

]

This is release 1.108.2, made on 2008-05-01.
Changes from release 1.108.1:

Functional changes to MPS code:

<http://www.ravenbrook.com/project/mps/issue/job001784/>
Defect discovered:
  - when using an auto_header format (mps_fmt_create_auto_header) 
    with AMC pools (mps_class_amc), the MPS leaks a small amount of 
    memory on each collection.
Impact:
  - the leak is likely to be a few bytes per collection, and at most 
    one byte per page (typically 2^12 bytes) of the address-space 
    currently in use for objects in AMC pools;
  - the leak is of temporary memory that the MPS uses to process 
    ambiguous references (typically references on the stack and in 
    registers), so a larger stack when a collection starts will 
    tend to cause a larger leak;
  - the leaked bytes are widely-spaced single bytes which therefore 
    also cause fragmentation;
  - the leaked bytes are not reclaimed until the client calls 
    mps_arena_destroy().
Fixed: correctly release all of this temporary memory.

<http://www.ravenbrook.com/project/mps/issue/job001809/>
Defect discovered:
  - AMC pools (mps_class_amc) temporarily retain the memory that was
    used for a dead object, if there is an ambiguous reference (such 
    as a value on the stack) that happens to point at the interior of 
    the (dead) object.
Impact:
  - if the (dead) object was small- or medium-sized, this temporary 
    retention is unlikely to cause a problem;
  - if the (dead) object was very large, then this retention is more
    likely, and will retain a large amount of memory;
  - if many large objects are allocated, this retention can cause 
    memory to be exhausted when it should not be.
Fix:
  - it is usually possible for AMC pools to free the memory 
    immediately (that is, during the collection that identifies the 
    object as being dead), and AMC pools now do so;
Future work:
  - occasionally, there are adjacently located objects that are 
    ambiguously referenced and are not dead;
  - in this case it is not possible to free the memory immediately, 
    and so temporary retention still occurs (this is not expected to 
    be very common).
  - however, the MPS could prevent this by avoiding locating small 
    objects adjacent to very large objects, see:
      <http://www.ravenbrook.com/project/mps/issue/job001811/>

<http://www.ravenbrook.com/project/mps/issue/job001737/>
Further changes to arena growth (see notes below for version 1.108.1).
When the arena cannot grow by the desired increment, the MPS 
attempts successively smaller increments, but with a more fine-grained 
search than in version 1.108.1, thereby achieveing an increment that 
more closely matches the largest available chunk of remaining 
address-space.
New interface function mps_arena_vm_growth().  This function allows
the client more control over how a VM arena (mps_arena_class_vm) 
grows.  The interface is under development and is likely to change; 
please contact us if you would like further details.


[
Historical: changes in release 1.108.1 (2007-12-21).

Functional changes to MPS code:

<http://www.ravenbrook.com/project/mps/issue/job001737/>
Limitation:
  - if a VM arena (of mps_arena_class_vm) needed to be extended, 
    and the attempt to extend it was refused by the OS (usually 
    because the requested address-space was not available), MPS 
    would not try a smaller extension.
Fixed: MPS now tries to extend the arena by successively smaller 
    amounts, until the extension succeeds.

<http://www.ravenbrook.com/project/mps/issue/job001706/>
Defect discovered:
  - internal memory-protection state in AMC pools (mps_class_amc) 
    was incorrect for a brief period of time during a collection, 
    which could (theoretically) have caused an assert and failure.
However:
  - during this brief period, the MPS protection cache masks the 
    defect.  As a result, we believe this defect is unlikely to be 
    observed in practice (and we have no evidence that it has 
    ever occurred in practice).
Fixed: maintain memory-protection state correctly in AMC pools.

Other changes:

<http://www.ravenbrook.com/project/mps/issue/job001714/>
  - in the Mac OS X (PowerPC) build, remove outdated compiler flags.

The MPS diagnostic system -- which produces diagnostic output for the 
purpose of helping MPS programmers and client-code programmers -- is 
undergoing improvement.  Some early documentation is at
 <http://www.ravenbrook.com/project/mps/master/design/diag>
There is a new build variety "di" that emits diagnostics.

Note: for further details of this release (including a 'live' report 
of defects found after these release-notes were written), and details 
of earlier and later releases, please see:
  <http://www.ravenbrook.com/project/mps/release/>
]

[
Historical: changes in release 1.108.0 (2007-07-05).

Functional changes to MPS code:

<http://www.ravenbrook.com/project/mps/issue/job001548/>
Defect discovered:
  - an assert could, rarely, be incorrectly triggered when a pool 
    of mps_class_amc is in a constrained memory condition.
Fixed: in this rare case, calculate a certain value correctly, 
    such that the assert which checks it is not incorrectly 
    triggered.

<http://www.ravenbrook.com/project/mps/issue/job001658/>
Defect discovered:
  - finalization messages could suffer an unnecessary delay, of 
    several full collections (in the worst case), if there were 
    more than 1024 finalization-registered objects;
Fixed: the number of finalization-registered objects should 
    no longer cause such a delay, so more finalization messages 
    are likely to be produced by a single collection.

<http://www.ravenbrook.com/project/mps/issue/job001147/>
  - on Mac OS X (PowerPC and Intel), MPS now has memory-protection 
    code, so collections are much faster.

<http://www.ravenbrook.com/project/mps/issue/job001619/>
  - on Mac OS X (Intel), MPS now has stack-scanner code, so the 
    stack may be declared an ambiguous root.  (Note: there was 
    already a stack-scanner for Mac OS X PowerPC).

<http://www.ravenbrook.com/project/mps/issue/job001622/>
  - on Mac OS X (PowerPC and Intel), MPS now has locking code, so 
    multiple client threads may use the MPS.  (But note: the thread 
    module has not been implemented for Mac OS X, so threads may 
    not yet have their stacks as roots, I think.  See:
    <http://www.ravenbrook.com/project/mps/issue/job001621/>).
    
<http://www.ravenbrook.com/project/mps/issue/job001556/>
Defect discovered:
  - a macro used only in asserts was incorrect; this could have made 
    some checks ineffective.
Fixed: corrected the macro.

Other changes:

<http://www.ravenbrook.com/project/mps/issue/job001624/>
  - now builds on FreeBSD 5.5.

<http://www.ravenbrook.com/project/mps/issue/job001637/>
  - now builds on Linux.

<http://www.ravenbrook.com/project/mps/issue/job001617/>
  - on Mac OS X (Intel) default "all" build works (fix broken 
    compile of amsss stress test).
]

[
Historical: changes in release 1.107.0 (2006-12-13):

Functional changes to MPS code:

<http://www.ravenbrook.com/project/mps/issue/job001455/>
Defect discovered:
  - if there was a pool of pool-class LO (leaf-only objects,
    see mps_class_lo), the mps_arena_formatted_objects_walk() 
    call would fail.
Fixed: mps_arena_formatted_objects_walk() will now work.

<http://www.ravenbrook.com/project/mps/issue/job000666/>
  - when MPS starts a garbage collection, it now tells 
    the client by posting a message of the new type
    mps_message_type_gc_start().  The message includes 
    a textual explanation (for the client programmer) 
    of why this collection was triggered.

<http://www.ravenbrook.com/project/mps/issue/job001545/>
<http://www.ravenbrook.com/project/mps/issue/job001546/>
  - hot varieties contain asserts (AVERs and checks) now
  - hot varieties do not gather statistics

Other changes:

<http://www.ravenbrook.com/project/mps/issue/job001421/>
  - the default "all" build now builds mps library and plinth, on 
    all platforms.

<http://www.ravenbrook.com/project/mps/issue/job001530/>
  - Mac OS X on Intel now buildable; see build notes.

Some work-in-progress MPS documentation is available; see:
  manual/wiki/index.html
]

[
Historical: changes in release 1.106.2 (2006-04-11):

No functional changes to MPS implementation code.

Fixed <http://www.ravenbrook.com/project/mps/issue/job001352/>
  - the "mps.a" library does not (now) contain plinth files;
  - therefore, if your MPS client code needs the example plinth
    ("mpsplan.a"), you must link with it explicitly to obtain 
    _mps_lib_assert_fail, _mps_clock, etc;
  - the default "all" build now succeeds on Unix-like platforms
    (in 1.106.1 the "all" build only worked on Mac OS X).

Fixed <http://www.ravenbrook.com/project/mps/issue/job000605/>
Fixed <http://www.ravenbrook.com/project/mps/issue/job001366/>
  - the default Windows "all" build now works with Visual C++ 6.0.

Work on <http://www.ravenbrook.com/project/mps/issue/job001365/>
  - hopefully fixed some build problems with Visual C++ 7.0.

Fixed <http://www.ravenbrook.com/project/mps/issue/job001367/>
  - there is a "hello-world" example of using the MPS, in:
      example/hello-world/index.txt
]

For more information about the status and progress of the MPS project, 
consult the project home-page: <http://www.ravenbrook.com/project/mps/>.


3. GETTING STARTED

The MPS Kit is a complete set of sources and documentation to enable
third parties to use, modify, and adapt the MPS.

For Windows, the kit is distributed as the self-extracting archive
"mps-kit-1.108.0.exe", and also as the ZIP archive
"mps-kit-1.108.0.zip", which may be unpacked using WinZip.

For Unix and Mac OS X, the integration kit is distributed as the tarball
"mps-kit-1.108.0.tar.gz".  Unpack it using the command "gunzip -c
mps-kit-1.108.0.tar.gz | tar xvf -", or by dropping the file onto
StuffIt Expander under Mac OS X.

The top-level file "index.html" in the sources indexes many other files,
and is a good place to start.  You should read the user manuals and the
high level design documents.

A quick start to writing code that uses the MPS is the "hello-world" 
example:
    example/hello-world/index.txt


4. THE STRUCTURE OF THE KIT

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


5. DEVELOPING USING THE KIT

The MPS can be built on a wide variety of platforms.  For Unix-like
platforms you will need the GNU Make tool.  Some platforms (such as
Linux and Mac OS X) have GNU Make as their default make tool.  For
others you will need to get and install it.  (It's available free from
<ftp://ftp.gnu.org/gnu/make/>.)  On Windows platforms the NMAKE tool is
used.  This comes with Microsoft Visual C++.

The MPS uses a 6 character platform code to express a combination of
OS/CPU architecture/compiler toolchain.  Each 6 character code breaks
down into 3 groups of 2 characters, like this:

  OSARCT

Where OS denotes the operating system, AR denotes the CPU architecture,
and CT denotes compiler toolchain.  Table 1 lists the platforms that we
have regular access to and on which the MPS works well.


              Table 1. MPS makefiles and platforms

  Makefile        OS              Architecture    Compiler

  fri4gc.gmk      FreeBSD         Intel IA32      GCC
  lii4gc.gmk      Linux           Intel IA32      GCC
  xcppgc.gmk      Mac OS X        PowerPC         GCC
  w3i3mv.nmk      Windows         Intel IA32      Microsoft C

Historically the MPS has worked on a much wider variety of platforms and
still would if we had access to them.  The MPS has worked on various
combinations of operating systems, IRIX / OSF/1 (Tru64) / Solaris / SunOS /
Class Mac OS, CPU architectures, MIPS / PowerPC / ALPHA / SPARC v8 /
SPARC v9, and compiler toolchains, Metrowerks Codewarrior / SunPro C /
Digital C.  The full list of platforms that were supported historically
is available in manual/build-notes/ .

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


6. TESTING

There is an extensive regression test suite in the "test" directory, but
we have not yet had time to make it easy to run.


7. BUILDING A RELEASE

The procedure for building a release of the MPS is in
"procedure/release-build".  This will only be of interest if you are
intending to maintain and publish a variant of the MPS.  If you do want
to do this, please get in touch with us <mps-questions@ravenbrook.com>
to discuss it.


8. CONTRIBUTING YOUR WORK

We'd like to include improvements to the MPS in the distributions from
Ravenbrook Limited.  Please consider putting your work under a
compatible open source license and sending it to us.

It will help a great deal if your work is self-consistent.  This means
that the documentation is up to date (even if only with sketchy but
accurate notes) and the procedures have been maintained.


9. GETTING HELP

You can obtain expert professional support for the MPS from Ravenbrook
Limited <http://www.ravenbrook.com/>, the developers of the MPS, who
have many years of experience in commercial memory management systems.
Write to us <mps-questions@ravenbrook.com> for more information.

You might also want to join the MPS discussion mailing list.  To join,
send a message with the word "subscribe" in the body to
mps-discussion-request@ravenbrook.com or send the word "help" for
general information.


10. LICENSING

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

2002-05-20  RB    Original author: Richard Brooksby, Ravenbrook Limited.
2002-05-20  RB    Created based on template from P4DTI project.
2002-06-18  NB    Minor updates and corrections.
2002-06-18  RB    Removed obsolete requirement for MASM.
2002-06-19  NB    Added note on self-extracting archive
2006-01-30  RHSK  Update from "1.100.1" to "1.106.1".
2006-03-30  RHSK  Add section 2: What's new.
2006-04-11  RHSK  Update from "1.106.1" to "1.106.2".
2006-04-14  RHSK  Merge updates from version/1.106 back to master.
2006-06-29  RHSK  Note fixed job001421, job001455.
2006-12-13  RHSK  Release 1.107.0
2007-07-05  RHSK  Release 1.108.0
2007-12-21  RHSK  Release 1.108.1
2008-05-01  RHSK  Release 1.108.2


C. COPYRIGHT AND LICENSE

Copyright (C) 2001-2002, 2006-2007, 2008 Ravenbrook Limited.  
All rights reserved.  <http://www.ravenbrook.com/>.  
This is an open source license.  
Contact Ravenbrook for commercial licensing options.

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
