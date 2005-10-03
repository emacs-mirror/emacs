#How to test a release before shipping it to Configura

#You have a candidate release.  This document tells you how to test it before shipping it -- quick tests.  
#Readership: all.  Confidential: no.  Status: rough notes.  
#Background: knowing the history of MPS development for Configura is an advantage.  
#Peer documents:
#  <http://info.ravenbrook.com/project/mps/master/procedure/release-build/>.
#  <http://info.ravenbrook.com/project/mps/master/procedure/release-configura/>.

#What do we test?
#Not much -- this procedure is for a quick test before shipping, principally to make sure that the build isn't crock in some way.  

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
  os.system("./w3i3mv/%s/%s.exe >>%s" % (variety, test, testout) )

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

# $Id$
