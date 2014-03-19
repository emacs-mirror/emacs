#!/usr/bin/env python
#
#                              Ravenbrook
#                     <http://www.ravenbrook.com/>
#
#                    RELEASE.PY -- MAKE A RELEASE
#
#             Gareth Rees, Ravenbrook Limited, 2014-03-18
#
#
# 1. INTRODUCTION
#
# This script automates the process of making a release, based on
# [RELEASE-BUILD].
#
# This script is idempotent: that is, you can run it repeatedly and it
# will skip steps that have already been performed.


from __future__ import unicode_literals
import argparse
from contextlib import contextmanager
import os
import re
import subprocess
import sys
import p4

class Error(Exception): pass

@contextmanager
def pushdir(dir):
    """Context manager that changes directory to dir for the body of the
    with statement.

    """
    cwd = os.getcwd()
    os.chdir(dir)
    yield
    os.chdir(cwd)

ROOT = '//info.ravenbrook.com/project'
PROJECT_RE = r'[a-z][a-z0-9.-]*'
PROJECT_FILESPEC_RE = r'{}/({})/'.format(re.escape(ROOT), PROJECT_RE)
VERSION_RE = r'\d+\.\d+'
CUSTOMER_RE = r'[a-z][a-z0-9.-]*'
BRANCH_RE = (r'master|(?:custom/({})/)?(?:main|version/({}))'
             .format(CUSTOMER_RE, VERSION_RE))
BRANCH_FILESPEC_RE = r'{}({})(?:/|$)'.format(PROJECT_FILESPEC_RE, BRANCH_RE)

def main(argv):
    parser = argparse.ArgumentParser()
    parser.add_argument('-P', '--project',
                        help='Name of the project.')
    parser.add_argument('-b', '--branch',
                        help='Name of the branch to make the release from.')
    parser.add_argument('-C', '--changelevel', type=int,
                        help='Changelevel at which to make the release.')
    parser.add_argument('-y', '--commit', action='store_true',
                        help='Carry out the operation (by default, just '
                        'show a preview).')
    args = parser.parse_args(argv[1:])
    args.root = ROOT
    fmt = lambda s: s.format_map(vars(args))

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

    if not args.branch:
        # Deduce branch from current directory.
        filespec = next(p4.run('dirs', '.'))['dir']
        m = re.match(BRANCH_FILESPEC_RE, filespec)
        if not m:
            raise Error("Can't deduce branch from {}".format(filespec))
        if args.project != m.group(1):
            raise Error("Specified project={} but current directory belongs "
                        "to project={}.".format(args.project, m.group(1)))
        args.branch = m.group(2)
        print(fmt("branch={branch}"))

    m = re.match(BRANCH_RE, args.branch)
    if not m:
        raise Error(fmt("Invalid branch {branch}"))
    args.customer = m.group(1)
    args.version = m.group(2)
    if args.customer:
        print(fmt("customer={customer}"))
    if args.version:
        print(fmt("version={version}"))

    if not any(p4.run('dirs', fmt('{root}/{project}/{branch}'))):
        raise Error(fmt("No such branch: {branch}"))

    if not args.changelevel:
        cmd = p4.run('changes', '-m', '1', fmt('{root}/{project}/{branch}/...'))
        args.changelevel = int(next(cmd)['change'])
        print(fmt("changelevel={changelevel}"))

    # Deduce release from code/version.c.
    f = fmt('{root}/{project}/{branch}/code/version.c@{changelevel}')
    m = re.search(r'^#define MPS_RELEASE "release/((\d+\.\d+)\.\d+)"$',
                  p4.contents(f), re.M)
    if not m:
        raise Error("Failed to extract release from {}.".format(f))
    args.release = m.group(1)
    print(fmt("release={release}"))
    if args.version and args.version != m.group(2):
        raise Error(fmt("Version {version} does not match release {release}"))
    if args.customer:
        args.reldir = fmt('{root}/{project}/custom/{customer}/release/{release}')
    else:
        args.reldir = fmt('{root}/{project}/release/{release}')

    args.kit = fmt('mps-kit-{release}')
    client_spec = dict(
        View0=fmt('{root}/{project}/{branch}/... //__CLIENT__/{kit}/...'),
        View1=fmt('{reldir}/... //__CLIENT__/release/{release}/...'))
    srcs = fmt('{root}/{project}/{branch}/...@{changelevel}')
    for line_end, args.ext, cmd in (('local', 'tar.gz', ['tar', 'czf']),
                                    ('win',   'zip',    ['zip', '-r'])):
        client_spec['LineEnd'] = line_end
        archive = fmt('release/{release}/{kit}.{ext}')
        with p4.temp_client(client_spec) as (conn, client_root):
            try:
                conn.do('files', fmt('{reldir}/{kit}.{ext}'))
            except p4.Error as e:
                print("Adding {}".format(archive))
                conn.do('sync', '-f', srcs)
                with pushdir(client_root):
                    os.makedirs(fmt('release/{release}'))
                    subprocess.check_call(cmd + [archive, args.kit],
                                          stdout=subprocess.DEVNULL)
                if not args.commit:
                    print("-y/--commit not specified: skipping.")
                else:
                    conn.do('add', os.path.join(client_root, archive))
                    desc = fmt("Adding the MPS Kit {ext} archive for "
                               "release {release}.")
                    conn.do('submit', '-d', desc)
            else:
                print("{} already exists: skipping.".format(archive))


if __name__ == '__main__':
    main(sys.argv)


# A. REFERENCES
#
# [RELEASE-BUILD] Richard Brooksby; "Memory Pool System Release Build
# Procedure"; Ravenbrook Limited; 2002-06-17.
# <https://info.ravenbrook.com/project/mps/master/procedure/release-build>
#
#
# B. DOCUMENT HISTORY
#
# 2014-03-18 GDR Created based on [RELEASE-BUILD].
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
# $Id: //info.ravenbrook.com/project/mps/master/tool/branch.py#1 $
