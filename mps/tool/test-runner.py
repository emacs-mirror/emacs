#!/usr/bin/python
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
import platform
import re

def mpsplatformcode() :
  """Makes a guess at the MPS platform code.  If it makes a full guess
     then returns the string, otherwise it returns the pair (None,guess)
     where guess is an MPS platform-like string with unknown parts set to
     '??'.
     """

  # Uses the platform module which appears to be in Python 2.3, but not
  # documented until Python 2.4.  See
  # http://www.python.org/doc/2.4/lib/module-platform.html

  os = '??' # operating system
  try :
    # 2007-07-03 DRJ : Darwin is tested, the other I have guessed at
    # from the documentation for platform.system()
    os = {'Darwin':'xc',
          'Linux':'li',
          'Windows':'w3',
         }[platform.system()]
  except :
    pass

  arch = '??' # CPU architecture
  try :
    # 2007-07-03 DRJ : 'Power Macintosh' and 'i386' are tested (but ppc
    # under Rosetta, so I'm not confident it's right).
    arch = {'Power Macintosh':'pp',
            'i386':'i3',
           }[platform.machine()]
  except :
    pass

  compiler = '??' # C compiler tool chain
  # There's no automagic way to determine this, some OS/Arch
  # combinations support more than one C compiler.  Sometimes it really
  # is up to the builder to choose what C compiler to use.
  # Here, we simplify and get it right for Windows and Macs.
  try :
    compiler = {'xc':'gc',
                'w3':'mv',
               }[os]
  except :
    pass

  plat = os + arch + compiler
  if re.search(r'\?', plat) :
    return None, plat
  return plat

mpsplatform = mpsplatformcode()

make = ''
if mpsplatform[4:6] == 'gc' :
  make = "make -r -f %s.gmk VARIETY=%%s %%s >> %%s" % mpsplatform
elif mpsplatform[4:6] == 'mv' :
  make = "nmake /f %s.nmk VARIETY=%%s %%s.exe >>%%s" % mpsplatform

run = ''
if mpsplatform[:2] == 'xc' :
  run = "./%s/%%s/%%s >> %%s" % mpsplatform
elif mpsplatform[:2] == 'w3' :
  run = r'.\%s\%%s\%%s.exe >>%%s' % mpsplatform

testout = "./a1.txt"

def runtest(test, variety, testout):
  """Appends to testout."""

  os.system("echo .")
  os.system("echo .")
  os.system("echo .")
  os.system("echo --- %s {%s} ---" % (test, variety) )
  os.system("echo --- %s {%s} --- >>%s" % (test, variety, testout) )
  os.system(make % (variety, test, testout) )
  os.system(run % (variety, test, testout) )

def runtestlist( lTest, lVariety, testout ):
  # clear testout
  os.system("echo . >%s" % testout)

  os.system("echo === Tests: {%s} {%s} ===" % (lTest, lVariety) )
  os.system("echo === Tests: {%s} {%s} === >>%s" % (lTest, lVariety, testout) )
  for test in lTest:
    for variety in lVariety:
      runtest(test, variety, testout)


runtestlist([
    "amcss",
    "amsss",
    "finalcv",
    "awlut",
    "awluthe",
    "mpsicv",
    "messtest",
 ], ["ci", "ce", "hi", "he", "we", "wi", "ti"], testout)

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
