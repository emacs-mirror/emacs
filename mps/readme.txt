Memory Pool System Kit Readme
=============================
:author: Richard Brooksby
:organization: Ravenbrook Limited
:date: 2002-05-20
:revision: $Id$
:confidentiality: public


This is the Memory Pool System Kit -- a complete set of sources for
using, modifying, and adapting the MPS.  This document will give you a
very brief overview and tell you where to find more information.


Overview of the MPS
-------------------

The Memory Pool System (MPS) is a very general, adaptable, flexible,
reliable, and efficient memory management system.  It permits the
flexible combination of memory management techniques, supporting manual
and automatic memory management, in-line allocation, finalization,
weakness, and multiple concurrent co-operating incremental generational
garbage collections.  It also includes a library of memory pool classes
implementing specialized memory management policies.

The MPS has been in development since 1994 and deployed in successful
commercial products since 1997. Bugs are almost unknown in production.
It is under continuous development and support by `Ravenbrook
<http://www.ravenbrook.com/>`__.

The MPS is distributed under an open source license (see
`<license.txt>`_). The license is designed to make it possible for you
to use the MPS in your own projects, provided that you either don't
distribute your product, or your product is open source too.

If the licensing terms aren't suitable for you (for example, you're
developing a closed-source commercial product or a compiler run-time
system) you can easily license the MPS under different terms from
Ravenbrook. Please write to us at mps-questions@ravenbrook.com
for more information.


Getting started
---------------

The MPS Kit is a complete set of sources and documentation to enable
you to use, modify, and adapt the MPS: source code, manuals,
procedures, design documentation, and so on. See
`<manual/html/index.html>`_ for an index.

The MPS Kit is distributed in source form.  You need to build it before
using it.  The basic case is straightforward on supported platforms
(see below)::

    cd code
    cc -O2 -c mps.c     Unix / Mac OS X (with Xcode command line)
    cl /O2 /c mps.c     Windows (with Microsoft SDK or Visual Studio 2010)

This will produce an object file you can link with your project.  For
details of how to configure the MPS, build libraries and tests, use
IDEs, autoconf, etc. see `Building the MPS <manual/build.txt>`__.

For an example of using the MPS, see the `Scheme interpreter
example <example/scheme/>`_.

Then, to program and integrate the MPS you'll definitely need to read
the `manual <manual/html/index.html>`_.


Supported target platforms
--------------------------

The MPS is currently supported for deployment on:

- Windows XP or later on IA-32 and x86-64, using Microsoft Visual C/C++;

- Linux (Ubuntu 11 and RHEL 6.3 known good, otherwise YMMV) on IA-32 and
  x86-64, using GCC;

- FreeBSD 7 or later, on IA-32 and x86-64, using GCC;

- Mac OS X 10.4 or later, on IA-32 and x86-64 (single threaded only),
  using Clang/LLVM.

The MPS will *not* work in a multi-threaded 32-bit application on 64-bit
Windows 7.  This is due to a serious fault in Microsoft's WOW64 emulator
that we are powerless to correct.  It is due to be fixed in Windows 8.
(See `WOW64 bug: GetThreadContext() may return stale contents
<http://zachsaw.blogspot.co.uk/2010/11/wow64-bug-getthreadcontext-may-
return.html>`__.)

The MPS is highly portable and has run on many other processors and
operating systems in the past (see `Building the MPS
<manual/guide/build.html>`__). Most of the MPS is written in very pure
ANSI C and compiles without warnings on anything.


Getting help
------------

You can obtain expert professional support for the MPS from `Ravenbrook
Limited <http://www.ravenbrook.com/>`__, the developers of the MPS, who
have many years of experience in commercial memory management systems.
Write to us at mps-questions@ravenbrook.com for more information.

You might also want to join the MPS discussion mailing list.  To join,
visit http://mailman.ravenbrook.com/mailman/listinfo/mps-discussion .


Document History
----------------

- 2002-05-20  RB    Original author: Richard Brooksby, Ravenbrook Limited.
- 2002-05-20  RB    Created based on template from P4DTI project.
- 2002-06-18  NB    Minor updates and corrections.
- 2002-06-18  RB    Removed obsolete requirement for MASM.
- 2002-06-19  NB    Added note on self-extracting archive
- 2006-01-30  RHSK  Update from "1.100.1" to "1.106.1".
- 2006-03-30  RHSK  Add section 2: What's new.
- 2006-04-11  RHSK  Update from "1.106.1" to "1.106.2".
- 2006-04-14  RHSK  Merge updates from version/1.106 back to master.
- 2006-06-29  RHSK  Note fixed job001421, job001455.
- 2006-12-13  RHSK  Release 1.107.0
- 2007-07-05  RHSK  Release 1.108.0
- 2007-12-21  RHSK  Release 1.108.1
- 2008-05-01  RHSK  Release 1.108.2
- 2010-03-03  RHSK  Release 1.109.0
- 2012-08-14  RB    Updating build instructions for new platforms.
- 2012-09-05  RB    Considerably reduced ready for version 1.110.  Now
  brought to you in glorious reStructuredText.


Copyright and Licence
---------------------

Copyright (C) 2001-2013 Ravenbrook Limited. All rights reserved. 
<http://www.ravenbrook.com/>. This is an open source license. Contact
Ravenbrook for commercial licensing options.

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
