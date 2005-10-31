#How to test a release before shipping it to Configura

#You have a candidate release.  This document tells you how to test it before shipping it -- quick tests.  
#Readership: all.  Confidential: no.  Status: rough notes.  
#Background: knowing the history of MPS development for Configura is an advantage.  
#Peer documents:
#  <http://info.ravenbrook.com/project/mps/master/procedure/release-build/>.
#  <http://info.ravenbrook.com/project/mps/master/procedure/release-configura/>.

#What do we test?
#Not much -- this procedure is for a quick test before shipping, principally to make sure that the build isn't crock in some way.  

#How to run the test:
#You need python (eg. 2.3).  Cd to where you would type w3build.bat.  
#Type ..\tool\test-runner.py
#Each test should report "Conclusion:  Failed to find any defects."
#Also check full log placed in file a1.txt.


import os
testout = "./a1.txt"

def runtest(test, variety, testout):
  # appends to testout
  os.system("echo .")
  os.system("echo .")
  os.system("echo .")
  os.system("echo --- %s (%s) ---" % (test, variety) )
  os.system("echo --- %s (%s) --- >>%s" % (test, variety, testout) )
  os.system("nmake /f w3i3mv.nmk VARIETY=%s %s.exe >>%s" % (variety, test, testout) )
  os.system(".\w3i3mv\%s\%s.exe >>%s" % (variety, test, testout) )

def runtestlist( lTest, lVariety, testout ):
  # clear testout
  os.system("echo . >%s" % testout)

  os.system("echo === Tests: (%s) (%s) ===" % (lTest, lVariety) )
  os.system("echo === Tests: (%s) (%s) === >>%s" % (lTest, lVariety, testout) )
  for test in lTest:
    for variety in lVariety:
      runtest(test, variety, testout)


runtestlist([
    "amcss",
    "finalcv",
#    "awlut",  # awlut.obj : error LNK2001: unresolved external symbol _dylan_weak_dependent
    "awluthe",
 ], ["ci", "we", "wi"], testout)

os.system("echo DONE")

"""
 * C. COPYRIGHT AND LICENSE
 *
 * Copyright (C) 2005 Ravenbrook Limited <http://www.ravenbrook.com/>.
 * All rights reserved.  This is an open source license.  Contact
 * Ravenbrook for commercial licensing options.
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 * 
 * 1. Redistributions of source code must retain the above copyright
 * notice, this list of conditions and the following disclaimer.
 * 
 * 2. Redistributions in binary form must reproduce the above copyright
 * notice, this list of conditions and the following disclaimer in the
 * documentation and/or other materials provided with the distribution.
 * 
 * 3. Redistributions in any form must be accompanied by information on how
 * to obtain complete source code for this software and any accompanying
 * software that uses this software.  The source code must either be
 * included in the distribution or be available for no more than the cost
 * of distribution plus a nominal fee, and must be freely redistributable
 * under reasonable conditions.  For an executable file, complete source
 * code means the source code for all modules it contains. It does not
 * include source code for modules or files that typically accompany the
 * major components of the operating system on which the executable file
 * runs.
 * 
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
 * IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 * TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR
 * PURPOSE, OR NON-INFRINGEMENT, ARE DISCLAIMED. IN NO EVENT SHALL THE
 * COPYRIGHT HOLDERS AND CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 * NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF
 * USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
 * ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 * THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
"""
# $Id$
