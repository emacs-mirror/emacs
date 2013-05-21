#!/usr/bin/python
# 
# $Id$
# Copyright (c) 2013 Ravenbrook Limited. See end of file for license.
# 
# This program takes the output of gcov on standard input and writes a
# human-readable table with a summary, to the file named on the
# command line (or standard output if none is given). The summary line
# is always written to standard output so that in the context of "make
# test" where the detailed test output is being directed to a test log
# file, the coverage summary can still be presented.
# 
# gcov output looks like this:
# 
#     File '/project/mps/master/code/mpsi.c'
#     Lines executed:85.12% of 921
#     /project/mps/master/code/mpsi.c:creating 'mpsi.c.gcov'
# 
# Note that we select only the .c files (there may also be output for
# system files like signal.h with inline function definitions, and we
# are not interested in covering them). The MPS has no inline function
# definitions in headers.

from sys import argv, stdin, stdout
from re import match

def coverage():
    """For each .c file with coverage data, generate a triple (percent
    coverage, file name, number of lines).

    """
    for line in stdin:
        m1 = match(r"File '.*/([^/]+\.c)'$", line)
        if not m1:
            continue
        m2 = match(r"Lines executed:(\d[0-9.]*)% of (\d+)$", next(stdin))
        if m2:
            yield float(m2.group(1)), m1.group(1), int(m2.group(2))

def main():
    if len(argv) >= 2:
        out = open(argv[1], 'a')
    else:
        out = stdout
    fmt1 = "{:<16s} {:<7s} {:<7s} {:<7s}\n"
    fmt2 = "{:<16s} {:7d} {:7d} {:6.2f}%\n"
    underlines = "---------------- ------- ------- -------".split()
    out.write(fmt1.format(*"File Lines Covered Percent".split()))
    out.write(fmt1.format(*underlines))
    total_lines, total_covered = 0, 0
    for percent, file, lines in sorted(coverage()):
        covered = int(round(lines * percent / 100))
        total_lines += lines
        total_covered += covered
        out.write(fmt2.format(file, lines, covered, percent))
    out.write(fmt1.format(*underlines))
    if total_lines == 0:
        total_percent = 100.0
    else:
        total_percent = 100.0 * total_covered / total_lines
    summary = fmt2.format("COVERAGE TOTAL", total_lines, total_covered,
                          total_percent)
    out.write(summary)
    if out != stdout:
        stdout.write(summary)

if __name__ == '__main__':
    main()


# C. COPYRIGHT AND LICENSE
#
# Copyright (C) 2013 Ravenbrook Limited <http://www.ravenbrook.com/>.
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
