#!/bin/nawk -f
# impl.awk.idlench: IDENTIFIER LENGTH CHECK
# $HopeName$
# Copyright (C) 1998.  Harlequin Group plc.  All rights reserved.
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
