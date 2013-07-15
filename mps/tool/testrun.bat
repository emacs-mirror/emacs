@rem $Id: //info.ravenbrook.com/project/mps/master/tool/testrun.sh#1 $
@rem Copyright (c) 2013 Ravenbrook Limited. See end of file for license.

@echo off

set PFM=%1
shift
set VARIETY=%1
shift

set TEST_COUNT=0
set FAIL_COUNT=0
set SEPARATOR=----------------------------------------
set LOGDIR=%TMP%\mps-%VARIETY%-log
echo Logging test output to %LOGDIR%
rmdir /q /s %LOGDIR%
mkdir %LOGDIR%

:loop
if "%1"=="" goto continue
    set /a TEST_COUNT=%TEST_COUNT%+1
    echo Running %1
    %PFM%\%VARIETY%\%1 > %LOGDIR%\%1
    if "%errorlevel%"=="0" goto success
        echo %SEPARATOR%%SEPARATOR%
        type %LOGDIR%\%1
        echo %SEPARATOR%%SEPARATOR%
        set /a FAIL_COUNT=%FAIL_COUNT%+1
    :success
shift
goto loop
:continue

if "%FAIL_COUNT%"=="0" goto allpass
    echo Tests: %TEST_COUNT%  Failures: %FAIL_COUNT%
    exit 1

:allpass
echo Tests: %TEST_COUNT%  All tests pass.


@rem C. COPYRIGHT AND LICENSE
@rem
@rem Copyright (C) 2013 Ravenbrook Limited <http://www.ravenbrook.com/>.
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
