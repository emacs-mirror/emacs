@rem gathconf.bat: GATHERING A RELEASE FOR CONFIGURA
@rem
@rem $Id$
@rem Copyright (c) 2001 Ravenbrook Limited.  See end of file for license.

rmdir /s w3i3mv
nmake /f w3i3mv.nmk VARIETY=we mps_conf.lib mpsplan.lib
nmake /f w3i3mv.nmk VARIETY=wi mps_conf.lib mpsplan.lib
nmake /f w3i3mv.nmk VARIETY=ce mps_conf.lib mpsplan.lib
nmake /f w3i3mv.nmk VARIETY=ci mps_conf.lib mpsplan.lib
mkdir release
mkdir release\include
mkdir release\lib
mkdir release\lib\w3i3
mkdir release\lib\w3i3\release
mkdir release\lib\w3i3\debug
mkdir release\src
copy mps.h release\include
copy mpsavm.h release\include
copy mpsacl.h release\include
copy mpscamc.h release\include
copy mpscams.h release\include
copy mpsclo.h release\include
copy mpscmv.h release\include
copy mpscmvff.h release\include
copy mpscsnc.h release\include
copy mpsio.h release\include
copy mpslib.h release\include
copy mpstd.h release\include
copy mpsw3.h release\include
copy mpswin.h release\include
copy w3i3mv\we\mps_conf.lib release\lib\w3i3\release
copy w3i3mv\ce\mps_conf.lib release\lib\w3i3\debug
copy w3i3mv\we\mpsplan.lib release\lib\w3i3\release
copy w3i3mv\ce\mpsplan.lib release\lib\w3i3\debug
copy mpsliban.c release\src
copy mpsioan.c release\src

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
