#!/bin/nawk -f
# idlench.awk: IDENTIFIER LENGTH CHECK
# $Id$
# Copyright (c) 2001 Ravenbrook Limited.  See end of file for license.
#
# READERSHIP
#
# .readership: Anyone prepared to read awk programs.
#
# SOURCE
#
# .language: This program is written in awk as specified in the Single
# UNIX Specification from the X/Open Group (aka XPG4 UNIX, aka UNIX98).
# See http://www.opengroup.org/onlinepubs/7908799/xcu/awk.html for
# their man page.
#
# DESIGN
#
# .design: See design.buildsys.idlench
#
# PURPOSE
#
# This program processes the output of cxref to find long
# identifiers.
#
# It outputs a list of functions that are used (ie those named appearing
# in the 3rd column: FUNCTION) whose names completely fill the column.
#
# A typical invocation might be:
#
# ./idlench.awk sos8cx/ci/*.o
#
# Not all awks are UNIX98 compliant; you need to find one that is.
# By default (if invoked as above) this script runs using /bin/nawk
# which on Solaris is a complant awk, but this isn't guaranteed for
# other systems.
#
# So the invocation might be something like:
#
# awk -f idlench.awk sos8cx/ci/*.o
#
# if there are problems with finding the right awk

# check for good awk
NR == 1 {
  if(FNR!=1) {
    print "error: bad version of awk, try nawk or /usr/xpg4/bin/awk ?"
    exit 1
  }
}
# skip 1st line
FNR == 1 {next}
# 2nd line contains column titles from which we determine widths
FNR == 2 {
  lastfunpos=index($0, "LINE")-2
  firstfunpos=index($0, "FUNCTION")
  if(lastfunpos<=0 || firstfunpos > lastfunpos) {
    print "error: malformed line 2 of file " FILENAME; exit 1}
  funlength = lastfunpos - firstfunpos + 1
  next
}
# for the rest of file, simply check last char in FUNCTION field
substr($0, lastfunpos, 1) != " " {
  fun = substr($0, firstfunpos, funlength)
  if(!dup[fun]) {
    print fun
    dup[fun] = 1
  }
}


# C. COPYRIGHT AND LICENSE
#
# Copyright (C) 2001-2002 Ravenbrook Limited <http://www.ravenbrook.com/>.
# All rights reserved.  This is an open source license.  Contact
# Ravenbrook for commercial licensing options.
# 
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are
# met:
# 
# 1. Redistributions of source code must retain the above copyright
# notice, this list of conditions and the following disclaimer.
# 
# 2. Redistributions in binary form must reproduce the above copyright
# notice, this list of conditions and the following disclaimer in the
# documentation and/or other materials provided with the distribution.
# 
# 3. Redistributions in any form must be accompanied by information on how
# to obtain complete source code for this software and any accompanying
# software that uses this software.  The source code must either be
# included in the distribution or be available for no more than the cost
# of distribution plus a nominal fee, and must be freely redistributable
# under reasonable conditions.  For an executable file, complete source
# code means the source code for all modules it contains. It does not
# include source code for modules or files that typically accompany the
# major components of the operating system on which the executable file
# runs.
# 
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
# IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
# TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR
# PURPOSE, OR NON-INFRINGEMENT, ARE DISCLAIMED. IN NO EVENT SHALL THE
# COPYRIGHT HOLDERS AND CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
# INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
# NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF
# USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
# ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
# (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
# THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
