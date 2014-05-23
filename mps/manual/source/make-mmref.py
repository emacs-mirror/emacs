#!/usr/bin/env python
#
#                              Ravenbrook
#                     <http://www.ravenbrook.com/>
#
#                MAKE THE MEMORY MANAGEMENT REFERENCE
#
#             Gareth Rees, Ravenbrook Limited, 2014-05-23
#
#
# 1. INTRODUCTION
#
# This script builds the Memory Management Reference website from the
# Memory Pool System manual.
#
# The whole build procedure is as follows:
#
# 1. Sync //info.ravenbrook.com/project/mps/master/manual/...
# 2. make html MMREF=1
# 3. Run this script
#
#
# 2. DESIGN
#
# We build the Memory Management Reference out of the Memory Pool
# System manual because:
#
# 1. having a single set of sources makes it easier to work on;
# 2. the glossary is a vital tool in organizing the MPS manual;
# 3. cross-references from the MMRef to the MPS are an opportunity
#    for advertising the latter to readers of the former.
#
#
# 3. DEPENDENCIES
#
# html5lib <https://pypi.python.org/pypi/html5lib>
# six <https://pypi.python.org/pypi/six>

import html5lib
import html5lib.serializer
import html5lib.treewalkers
from io import open
import os
import re
from shutil import copyfile
import sys
from six.moves.urllib.parse import urljoin


# 4. CONFIGURATION

# Subdirectories of the MPS manual that belong in the MMRef.
mmref_dirs = ('glossary', 'mmref', '_images', '_static')

# Top-level files that belong in the MMRef.
mmref_files = ('index', 'copyright')

# Regular expression matching files to be included in the MMRef.
url_filter_re = re.compile(r'^/html/(?:(?:{})\.html)?(?:#.*)?$|/(?:{})/'.format(
    '|'.join(mmref_files), '|'.join(mmref_dirs)))

# Root URL for the MPS manual.
rewrite_url = 'http://www.ravenbrook.com/project/mps/master/manual/html/'


def rewrite_links(src, src_base, url_filter, rewrite_base,
                  url_attributes = (('a', 'href'),)):
    """Rewrite URLs in src and return the result.

    First, src is parsed as HTML. Second, all URLs found in the
    document are resolved relative to src_base and the result passed to
    the functions url_filter. If this returns False, the URL is resolved
    again, this time relative to rewrite_base, and the result stored
    back to the document. Finally, the updated document is serialized
    as HTML and returned.

    The keyword argument url_attributes is a sequence of (tag,
    attribute) pairs that contain URLs to be rewritten.

    """
    tree_builder = html5lib.treebuilders.getTreeBuilder('dom')
    parser = html5lib.html5parser.HTMLParser(tree = tree_builder)
    dom = parser.parse(src)

    for tag, attr in url_attributes:
        for e in dom.getElementsByTagName(tag):
            u = e.getAttribute(attr)
            if u and not url_filter(urljoin(src_base, u)):
                rewritten = urljoin(rewrite_base, u)
                if u != rewritten:
                    e.setAttribute(attr, rewritten)

    tree_walker = html5lib.treewalkers.getTreeWalker('dom')
    html_serializer = html5lib.serializer.htmlserializer.HTMLSerializer()
    return u''.join(html_serializer.serialize(tree_walker(dom)))

def newer(src, target):
    """Return True if src is newer (that is, modified more recently) than
    target, False otherwise.

    """
    return (not os.path.isfile(target)
            or os.path.getmtime(target) < os.path.getmtime(src))

def rewrite_file(src_dir, src_filename, target_path, rewrite_url):
    src_path = os.path.join(src_dir, src_filename)
    if not newer(src_path, target_path):
        return
    print("Rewriting links in {} -> {}".format(src_path, target_path))
    src = open(os.path.join(src_dir, src_filename), encoding='utf-8').read()
    src_base = '/{}/'.format(src_dir)
    url_filter = url_filter_re.search
    rewrite_base = urljoin(rewrite_url, src_dir)
    result = rewrite_links(src, src_base, url_filter, rewrite_base)
    open(target_path, 'w', encoding='utf-8').write(result)

def main(target_root='mmref'):
    src_root = 'html'
    for d in mmref_dirs:
        src_dir = os.path.join(src_root, d)
        target_dir = os.path.join(target_root, d)
        os.makedirs(target_dir, exist_ok=True)
        for f in os.listdir(src_dir):
            src_path = os.path.join(src_dir, f)
            target_path = os.path.join(target_dir, f)
            if os.path.splitext(f)[1] == '.html':
                rewrite_file(src_dir, f, target_path, rewrite_url)
            elif os.path.isfile(src_path):
                copyfile(src_path, target_path)
    for f in mmref_files:
        rewrite_file(src_root, 'mmref-{}.html'.format(f),
                     os.path.join(target_root, '{}.html'.format(f)),
                     rewrite_url)


if __name__ == '__main__':
    main(*sys.argv[1:])


# B. DOCUMENT HISTORY
#
# 2014-05-23 GDR Created.
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
# $Id: //info.ravenbrook.com/project/mps/master/tool/branch#9 $
