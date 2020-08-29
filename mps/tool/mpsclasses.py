#!/usr/bin/python

# mpsclasses.py: show the MPS class hierarchy
# $Id$
# Copyright (c) 2007-2020 Ravenbrook Limited.  See end of file for license.

# This file parses MPS C source for MPS class definitions
# (DEFINE_CLASS() et al), and prints out the class hierarchy.
#
# USAGE:
#
# Invoke it with the list of MPS source files, like this:
#    cd code
#    ../tool/mpsclasses.py *.c
#
# Example output line (to stdout):
#  : : : : : AWLPoolClass  (POOL)  <<< AbstractCollectPoolClass +++ Pool:Format
#
# This means:
#   the MPS class "AWLPoolClass" is defined
#   using the DEFINE_POOL_CLASS macro (not DEFINE_CLASS)
#   its parent is AbstractCollectPoolClass
#   it uses the PoolClassMixInFormat mixin.


# VERSIONS
# -- first checkin --
# Version 06, Version 07
#  tidied and commented
#
# Version 05
#  sorted: shallowest first, then alphabetically
#
# Version 04
#  shows forest of classes, one class per line

import re
import fileinput

# examples:
"""
    DEFINE_CLASS(RankBufClass, class)
    {
      INHERIT_CLASS(class, SegBufClass);
      ...
    }

    DEFINE_POOL_CLASS(AWLPoolClass, this)
    {
      INHERIT_CLASS(this, AbstractCollectPoolClass);
      PoolClassMixInFormat(this);
      ...
    }
"""

#  Grammar:
#  C -> D I? M? E
#


class GramTerm(object):
  """a GramTerm holds information about a terminal in the grammar"""
  def __init__(self, name, patt, show):
    super(GramTerm, self).__init__()
    self.name = name
    self.patt = patt
    self.show = show


# ___D___ -- DEFINE_(<family>_)CLASS ( name
#
patt_D = re.compile( r"""
     DEFINE_
     (?P<family> [A-Z]*) [_]?   # family
     CLASS
 \s* [(]
 \s* (?P<name> [A-Za-z_][A-Za-z0-9_]* )  # name
""", re.VERBOSE)

def show_D(match):
   print ("%s  (%s)" % (match.group("name", "family")))

term_D = GramTerm("D", patt_D, show_D)


# ___I___ -- INHERIT_CLASS ( this , parentname
#
patt_I = re.compile( r"""
     INHERIT_CLASS
 \s* [(]
 \s* ( [A-Za-z_][A-Za-z0-9_]* )              # this
 \s* [,]
 \s* (?P<parentname> [A-Za-z_][A-Za-z0-9_]* )  # parentname
""", re.VERBOSE)

def show_I(match):
  print "  <<< %s" % (match.group("parentname"))

term_I = GramTerm("I", patt_I, show_I)


# ___M___ -- <family>ClassMixIn<mixin> ( this ) ;
#
patt_M = re.compile( r"""
     (?P<family> [A-Za-z_][A-Za-z0-9_]*)   # family
     ClassMixIn
     (?P<mixin> [A-Za-z0-9_]*)   # mixin
 \s* [(]
 \s* ( [A-Za-z_][A-Za-z0-9_]* )              # this
 \s* [)]
 \s* [;]
""", re.VERBOSE)

def show_M(match):
  print "  +++ %s [%s]" % (match.group("mixin", "family"))

term_M = GramTerm("M", patt_M, show_M)


# ___E___ -- }
#
patt_E = re.compile( r"""
     [}]
""", re.VERBOSE)

def show_E(match):
  print "..."

term_E = GramTerm("E", patt_E, show_E)


class NoMoreInput(Exception):
  """NoMoreInput"""

class MPSClass(object):
  """represents an MPS Class, as created by DEFINE_CLASS() et al"""
  def __init__(self, match_D):
    """init from match_D"""
    super(MPSClass, self).__init__()
    self.name, self.family = match_D.group("name", "family")
    self.parentname = None
    self.mixinnames = []
    self.descendants = 0

  def add_I(self, match_I):
    """add_I: parentname to inherit from"""
    assert(self.parentname == None)
    self.parentname = match_I.group("parentname")

  def add_M(self, match_M):
    """add_M: mixinname"""
    self.mixinnames.append("%s:%s" % match_M.group("family", "mixin"))

  def show(self, prefix):
    mix = ""
    sep = " +++ "
    for m in self.mixinnames:
      mix = mix + sep + m
      sep = ", "
    print ("%s%s  (%s)  <<< %s%s"
           % (prefix, self.name, self.family, self.parentname, mix))

def main():
  lines = fileinput.input()
  tops = []
  CChildListfromName = {}
  # (a cmp() function works even with very old Pythons)
  ChildrenSort = lambda C1, C2: cmp(
    [C1.descendants, C1.name.lower()],
    [C2.descendants, C2.name.lower()])

  def calc_descendants_tree(CT):
    for child in CChildListfromName[CT.name]:
      CT.descendants += calc_descendants_tree(child)
    return CT.descendants + 1

  def show_tree(CT, prefix):
    CT.show(prefix)
    CChildListfromName[CT.name].sort(ChildrenSort)
    for child in CChildListfromName[CT.name]:
      show_tree(child, prefix + ": ")

  try:
    while True:
      C = next_C(lines)

      # with C
      #C.show()

      if C.name not in CChildListfromName:
        CChildListfromName[C.name] = []
      if C.parentname == None:
        tops.append(C)
      else:
        if C.parentname not in CChildListfromName:
          CChildListfromName[C.parentname] = []
        CChildListfromName[C.parentname].append(C)

      C = None

  except NoMoreInput:
    assert(C == None)

  tot = 0
  for CT in tops:
    tot += calc_descendants_tree(CT)

  tops.sort(ChildrenSort)
  for CT in tops:
    show_tree(CT, "")
    print
  print "%d classes in total" % tot

def next_C(lines):
  """find the next class definition C"""

  # .DIME -- find next DEFINE_CLASS
  for l in lines:
    match_D = term_D.patt.search(l)
    if match_D != None:
      break
    for t in (term_I, term_M):
      match = t.patt.search(l)
      if match != None:
        print "ERROR parsing .DIME: got I or M: ", l
        raise
  else:
    raise NoMoreInput()
  # with match_D
  #term_D.show(match_D)
  C = MPSClass(match_D)

  # D.IME -- find next InheritFrom, Mixin, or End
  for l in lines:
    for t in (term_D, term_I, term_M, term_E):
      match = t.patt.search(l)
      if match != None:
        break
    else:
      continue
    # with t
    #t.show(match)
    if t == term_D:
      print "ERROR parsing D.IME: got (another) D; expected I, M, or E"
      raise
    elif t == term_I:
      C.add_I(match)
    elif t == term_M:
      C.add_M(match)
    elif t == term_E:
      return C
  else:
    raise NoMoreInput()

if __name__ == "__main__":
  main()


# C. COPYRIGHT AND LICENSE
#
# Copyright (C) 2007-2020 Ravenbrook Limited <https://www.ravenbrook.com/>.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are
# met:
#
# 1. Redistributions of source code must retain the above copyright
#    notice, this list of conditions and the following disclaimer.
#
# 2. Redistributions in binary form must reproduce the above copyright
#    notice, this list of conditions and the following disclaimer in the
#    documentation and/or other materials provided with the
#    distribution.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
# IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
# TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
# PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
# HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
# SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
# LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
# DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
# THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
# (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
# OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
#
