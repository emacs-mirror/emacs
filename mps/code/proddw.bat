@rem impl.bat.proddw
@rem Script that automates building and collating a dylan product
@rem $Id$
@rem Copyright (c) 2001 Ravenbrook Limited.

nmake /f w3i3mv.nmk VARIETY=ci mmdw.lib mpsplan.lib
nmake /f w3i3mv.nmk VARIETY=hi mmdw.lib mpsplan.lib
nmake /f w3i3mv.nmk VARIETY=he mmdw.lib mpsplan.lib
nmake /f w3i3mv.nmk VARIETY=wi mmdw.lib mpsplan.lib
rmdir /Q/S dylan
mkdir dylan
mkdir dylan\mps
mkdir dylan\mps\include
mkdir dylan\mps\lib
mkdir dylan\mps\lib\w3i3
mkdir dylan\mps\lib\w3i3\ci
mkdir dylan\mps\lib\w3i3\hi
mkdir dylan\mps\lib\w3i3\he
mkdir dylan\mps\lib\w3i3\wi
mkdir dylan\mps\src
copy mps.h dylan\mps\include
copy mpsavm.h dylan\mps\include
copy mpscamc.h dylan\mps\include
copy mpscawl.h dylan\mps\include
copy mpsclo.h dylan\mps\include
copy mpscsnc.h dylan\mps\include
copy mpscmv.h dylan\mps\include
copy mpsio.h dylan\mps\include
copy mpslib.h dylan\mps\include
copy mpstd.h dylan\mps\include
copy mpsw3.h dylan\mps\include
copy mpswin.h dylan\mps\include
copy w3i3mv\ci\mmdw.lib dylan\mps\lib\w3i3\ci
copy w3i3mv\hi\mmdw.lib dylan\mps\lib\w3i3\hi
copy w3i3mv\he\mmdw.lib dylan\mps\lib\w3i3\he
copy w3i3mv\wi\mmdw.lib dylan\mps\lib\w3i3\wi
copy w3i3mv\ci\mpsplan.lib dylan\mps\lib\w3i3\ci
copy w3i3mv\hi\mpsplan.lib dylan\mps\lib\w3i3\hi
copy w3i3mv\he\mpsplan.lib dylan\mps\lib\w3i3\he
copy w3i3mv\wi\mpsplan.lib dylan\mps\lib\w3i3\wi
copy mpsliban.c dylan\mps\src
copy mpsioan.c dylan\mps\src

@rem C. COPYRIGHT AND LICENSE
@rem
@rem Copyright (C) 2001-2002 Ravenbrook Limited <http://www.ravenbrook.com/>.
@rem All rights reserved.  This is an open source license.  Contact
@rem Ravenbrook for commercial licensing options.
@rem 
@rem Redistribution and use in source and binary forms, with or without
@rem modification, are permitted provided that the following conditions are
@rem met:
@rem 
@rem 1. Redistributions of source code must retain the above copyright
@rem notice, this list of conditions and the following disclaimer.
@rem 
@rem 2. Redistributions in binary form must reproduce the above copyright
@rem notice, this list of conditions and the following disclaimer in the
@rem documentation and/or other materials provided with the distribution.
@rem 
@rem 3. Redistributions in any form must be accompanied by information on how
@rem to obtain complete source code for this software and any accompanying
@rem software that uses this software.  The source code must either be
@rem included in the distribution or be available for no more than the cost
@rem of distribution plus a nominal fee, and must be freely redistributable
@rem under reasonable conditions.  For an executable file, complete source
@rem code means the source code for all modules it contains. It does not
@rem include source code for modules or files that typically accompany the
@rem major components of the operating system on which the executable file
@rem runs.
@rem 
@rem THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
@rem IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
@rem TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR
@rem PURPOSE, OR NON-INFRINGEMENT, ARE DISCLAIMED. IN NO EVENT SHALL THE
@rem COPYRIGHT HOLDERS AND CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
@rem INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
@rem NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF
@rem USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
@rem ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
@rem (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
@rem THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
