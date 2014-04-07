@rem $Id$
@rem Copyright (c) 2013-2014 Ravenbrook Limited. See end of file for license.
@rem 
@rem This program runs a series of test cases, capturing the output of
@rem each one to a temporary file. In addition, the output of any test
@rem case that fails gets printed to standard output so that it is not
@rem lost when the test is running on a temporary build server (see
@rem job003489). Finally, it prints a summary of passes and failures, and
@rem if there were any failures, it exits with a non-zero status code.
@rem
@rem Usage::
@rem 
@rem     testrun.bat PLATFORM VARIETY [CASE1 CASE2 ...]

@echo off

set PFM=%1
shift
set VARIETY=%1
shift

set ALL_TEST_CASES=^
    abqtest.exe ^
    airtest.exe ^
    amcss.exe ^
    amcsshe.exe ^
    amsss.exe ^
    amssshe.exe ^
    apss.exe ^
    arenacv.exe ^
    awlut.exe ^
    awluthe.exe ^
    btcv.exe ^
    exposet0.exe ^
    expt825.exe ^
    fbmtest.exe ^
    finalcv.exe ^
    finaltest.exe ^
    fotest.exe ^
    locbwcss.exe ^
    lockcov.exe ^
    lockutw3.exe ^
    locusss.exe ^
    locv.exe ^
    messtest.exe ^
    mpmss.exe ^
    mpsicv.exe ^
    mv2test.exe ^
    nailboardtest.exe ^
    poolncv.exe ^
    qs.exe ^
    sacss.exe ^
    segsmss.exe ^
    steptest.exe ^
    walkt0.exe ^
    zmess.exe

@rem Ensure that test cases don't pop up dialog box on abort()
set MPS_TESTLIB_NOABORT=true
set TEST_COUNT=0
set PASS_COUNT=0
set FAIL_COUNT=0
set SEPARATOR=----------------------------------------
set LOGDIR=%TMP%\mps-%PFM%-%VARIETY%-log
echo Logging test output to %LOGDIR%
if exist %LOGDIR% rmdir /q /s %LOGDIR%
mkdir %LOGDIR%

if "%1"=="" call :run_tests %ALL_TEST_CASES%

if "%FAIL_COUNT%"=="0" (
    echo Tests: %TEST_COUNT%. All tests pass.
    exit 0
) else (
    echo Tests: %TEST_COUNT%. Passes: %PASS_COUNT%. Failures: %FAIL_COUNT%.
    exit 1
)

:run_tests
if "%1"=="" exit /b
set /a TEST_COUNT=%TEST_COUNT%+1
echo Running %1
%PFM%\%VARIETY%\%1 > %LOGDIR%\%1
if "%errorlevel%"=="0" (
    set /a PASS_COUNT=%PASS_COUNT%+1
) else (
    echo %SEPARATOR%%SEPARATOR%
    type %LOGDIR%\%1
    echo %SEPARATOR%%SEPARATOR%
    set /a FAIL_COUNT=%FAIL_COUNT%+1
)
shift
goto run_tests


@rem C. COPYRIGHT AND LICENSE
@rem
@rem Copyright (C) 2013-2014 Ravenbrook Limited <http://www.ravenbrook.com/>.
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
