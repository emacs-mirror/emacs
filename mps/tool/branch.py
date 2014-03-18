#!/usr/bin/env python
#
#                              Ravenbrook
#                     <http://www.ravenbrook.com/>
#
#             BRANCH.PY -- CREATE VERSION OR TASK BRANCH
#
#             Gareth Rees, Ravenbrook Limited, 2014-03-18
#
#
# 1. INTRODUCTION
#
# This script automates the process of branching the master sources
# (or customer mainline sources) of a project. It can create a version
# branch as described in [VERSION-CREATE], or a development (task)
# branch as described in [BRANCH-MERGE].


from __future__ import unicode_literals
import argparse
import datetime
import re
import subprocess
import sys

import p4

class Error(Exception): pass

ROOT = '//info.ravenbrook.com/project'
PROJECT_RE = r'[a-z][a-z0-9.-]*'
PROJECT_FILESPEC_RE = r'{}/({})/'.format(re.escape(ROOT), PROJECT_RE)
CUSTOMER_RE = r'[a-z][a-z0-9.-]*'
PARENT_RE = r'master|custom/({})/main'.format(CUSTOMER_RE)
PARENT_FILESPEC_RE = r'{}({})(?:/|$)'.format(PROJECT_FILESPEC_RE, PARENT_RE)
TASK_RE = r'[a-zA-Z][a-zA-Z0-9._-]*'
TASK_BRANCH_RE = r'branch/\d\d\d\d-\d\d-\d\d/{}'.format(TASK_RE)
VERSION_RE = r'\d+\.\d+'
VERSION_BRANCH_RE = r'(?:custom/{}/)?version/{}'.format(CUSTOMER_RE, VERSION_RE)
CHILD_RE = r'({}|{})$'.format(TASK_BRANCH_RE, VERSION_BRANCH_RE)

def main(argv):
    parser = argparse.ArgumentParser()
    parser.add_argument('-P', '--project',
                        help='Name of the project.')
    parser.add_argument('-p', '--parent',
                        help='Name of the parent branch.')
    parser.add_argument('-C', '--changelevel', type=int,
                        help='Changelevel at which to make the branch.')
    parser.add_argument('-d', '--description',
                        help='Description of the branch.')
    parser.add_argument('-y', '--commit', action='store_true',
                        help='Carry out the operation (by default, just '
                        'show a preview).')
    group = parser.add_mutually_exclusive_group(required=True)
    group.add_argument('-c', '--child',
                       help='Name of the child branch.')
    group.add_argument('-v', '--version', action='store_true',
                       help='Make the next version branch.')
    group.add_argument('-t', '--task',
                       help='Name of the task branch.')
    args = parser.parse_args(argv[1:])
    args.root = ROOT
    fmt = lambda s: s.format(**vars(args))

    if not args.project:
        # Deduce project from current directory.
        filespec = next(p4.run('dirs', '.'))['dir']
        m = re.match(PROJECT_FILESPEC_RE, filespec)
        if not m:
            raise Error("Can't deduce project from current directory.")
        args.project = m.group(1)
        print(fmt("project={project}"))

    if not any(p4.run('dirs', fmt('{root}/{project}'))):
        raise Error(fmt("No such project: {project}"))

    if not args.parent:
        # Deduce parent branch from current directory.
        filespec = next(p4.run('dirs', '.'))['dir']
        m = re.match(PARENT_FILESPEC_RE, filespec)
        if not m:
            raise Error("Can't deduce parent branch from {}".format(filespec))
        if args.project != m.group(1):
            raise Error("Specified project={} but current directory belongs "
                        "to project={}.".format(args.project, m.group(1)))
        args.parent = m.group(2)
        print(fmt("parent={parent}"))

    m = re.match(PARENT_RE, args.parent)
    if not m:
        raise Error("Invalid parent branch: must be master or custom/*/main.")
    customer = m.group(1)
    if not any(p4.run('dirs', fmt('{root}/{project}/{parent}'))):
        raise Error(fmt("No such branch: {parent}"))

    if not args.changelevel:
        cmd = p4.run('changes', '-m', '1', fmt('{root}/{project}/{parent}/...'))
        args.changelevel = int(next(cmd)['change'])
        print(fmt("changelevel={changelevel}"))

    if args.task:
        if not re.match(TASK_RE, args.task):
            raise Error(fmt("Invalid task: {task}"))
        args.child = fmt(datetime.date.today().strftime('branch/%Y-%m-%d/{task}'))
        print(fmt("child={child}"))
    elif args.version:
        # Deduce version number from code/version.c.
        f = fmt('{root}/{project}/{parent}/code/version.c@{changelevel}')
        m = re.search(r'^#define MPS_RELEASE "release/(\d+\.\d+)\.\d+"$',
                      p4.contents(f), re.M)
        if not m:
            raise Error("Failed to extract version from {}.".format(f))
        version = m.group(1)
        if args.parent == 'master':
            args.child = 'version/{}'.format(version)
        else:
            args.child = 'custom/{}/version/{}'.format(customer, version)
        print(fmt("child={child}"))

    if not re.match(CHILD_RE, args.child):
        raise Error(fmt("Invalid child: {child}"))

    if not args.description:
        args.description = fmt("Branching {parent} to {child}.")
        print(fmt("description={description}"))

    args.branch = fmt('{project}/{child}')
    branch_spec = dict(Branch=args.branch,
                       Description=args.description,
                       View0=fmt('{root}/{project}/{parent}/... '
                                 '{root}/{project}/{child}/...'))
    print("view={}".format(branch_spec['View0']))
    if any(p4.run('branches', '-E', args.branch)):
        print("Branch spec already exists: not creating.")
    else:
        p4.run('branch', '-i').send(branch_spec).done()

    if any(p4.run('dirs', fmt('{root}/{project}/{child}'))):
        print("Child branch already populated: not creating.")
    else:
        populate_args = ['populate', '-n', '-f',
                         '-b', args.branch,
                         '-d', args.description,
                         '-s', fmt('{root}/{project}/{parent}/...@{changelevel}')]
        if args.commit:
            populate_args.remove('-n')
        print("cmd={}".format(populate_args))
        for result in p4.run(*populate_args):
            print(result)

if __name__ == '__main__':
    main(sys.argv)


# A. REFERENCES
#
# [BRANCH-MERGE] Gareth Rees; "Memory Pool System branching and
# merging procedures"; Ravenbrook Limited; 2014-01-09.
# <https://info.ravenbrook.com/project/mps/master/procedure/branch-merge>
#
# [VERSION-CREATE] Richard Kistruck; "Memory Pool System Version
# Create Procedure"; Ravenbrook Limited; 2008-10-29.
# <https://info.ravenbrook.com/project/mps/master/procedure/version-create>
#
#
# B. DOCUMENT HISTORY
#
# 2014-03-18 GDR Created based on [BRANCH-MERGE] and [VERSION-CREATE].
#
#
# C. COPYRIGHT AND LICENCE
#
# Copyright (c) 2014 Ravenbrook Ltd.  All rights reserved.
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
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
# "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
# LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
# A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE REGENTS OR
# CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
# EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
# PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
# PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
# LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
# NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
# SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
#
#
# $Id$
