# designs.py -- Convert MPS design documents into sources for the MPS manual
#
# Copyright (c) 2013-2023 Ravenbrook Limited. See end of file for license.
#
# This code converts the MPS design documents (in plain
# reStructuredText) for use in the manual (in Sphinx extended
# reStructuredText) and so implements design.mps.doc.fmt.
#
# TODO: It's a shame that the regexps we convert from (near the start
# of the file) are a log way from the strings we convert to (in
# ``convert_file``).

from __future__ import unicode_literals

import glob
from itertools import chain
import os
import os.path
import re
import shutil
import sys

from sphinx.util import logging
from sphinx.util.console import bold

TYPES = '''

    AccessSet Accumulation Addr Align AllocFrame AllocPattern AP Arg
    Arena Attr Bool BootBlock BT Buffer BufferMode Byte Chain Chunk
    Clock Compare Count Epoch EventClock FindDelete Format Fun GenDesc
    Globals Index Land LD Lock LocusPref LocusPrefKind Message
    MessageType MutatorContext MutatorContextVar Page Pointer Pool
    PoolGen PThreadext Range Rank RankSet ReadonlyAddr Ref RefSet Res
    Ring Root RootMode RootVar ScanState Seg SegBuf Serial Shift Sig
    Size Space SplayNode SplayTree StackContext Thread Trace TraceId
    TraceSet TraceStartWhy TraceState ULongest VM Word ZoneSet

'''

# Macros that are improperly named (not all-caps).
MACROS = '''

    ClassOfPoly CouldBeA IsA IsSubclass Method MustBeA
    MustBeA_CRITICAL NextMethod SetClassOfPoly SuperclassPoly

'''

mode = re.compile(r'\.\. mode: .*\n')

# design.mps.doc.metadata.tag
prefix = re.compile(r'^:Tag: ([a-z][a-z.0-9-]*[a-z0-9])$', re.MULTILINE)

rst_tag = re.compile(r'^:(?:Author|Date|Status|Revision|Copyright|Organization|Format|Index terms|Readership):.*?$\n', re.MULTILINE | re.IGNORECASE)

# design.mps.doc.fmt.tag
mps_tag = re.compile(r'_`\.([a-z][A-Za-z.0-9_-]*[A-Za-z0-9])`:')

# design.mps.doc.fmt.ref
mps_ref = re.compile(r'`(\.[a-z][A-Za-z.0-9_-]*[A-Za-z0-9])`_(?:        )?')

# design.mps.fmt.function-decl
funcdef = re.compile(r'^``([^`]*\([^`]*\))``$', re.MULTILINE)

# design.mps.fmt.macro-decl
macrodef = re.compile(r'^``((?:[A-Z][A-Z0-9_]+|{})(?:\([^`]*\))?)``$'
                      .format('|'.join(map(re.escape, MACROS.split()))), re.MULTILINE)
# design.mps.fmt.macro
macro = re.compile(r'``([A-Z][A-Z0-9_]+)``(?:       )?')

# design.mps.fmt.type-def
typedef = re.compile(r'^``typedef ([^`]*)``$', re.MULTILINE) 

# design.mps.fmt.function-ref
func = re.compile(r'``([A-Za-z][A-Za-z0-9_]+\(\))``')

# design.mps.fmt.type-ref
typename = re.compile(r'``({0}|[A-Z][A-Za-z0-9_]*'
                      r'(?:Class|Function|Method|Struct|Union)|'
                      r'mps_[a-z_]+_[stu])``(?:      )?'
                      .format('|'.join(map(re.escape, TYPES.split()))))

# Tags referencing other design documents
design_ref = re.compile(r'^( *\.\. _design\.mps\.(?:[^:\n]+): (?:[^#:\n]+))$', re.MULTILINE)
design_frag_ref = re.compile(r'^( *\.\. _design\.mps\.([^:\n]+)\.([^:\n]+): (?:[^#:\n]+))#(.+)$', re.MULTILINE)

relative_link = re.compile(r'^( *\.\. _[\w\.]+: +\.\./)', re.MULTILINE)
history = re.compile(r'^Document History\n.*',
                     re.MULTILINE | re.IGNORECASE | re.DOTALL)

# Strip section numbering
secnum = re.compile(r'^(?:[0-9]+|[A-Z])\.\s+(.*)$\n(([=`:.\'"~^_*+#-])\3+)$',
                    re.MULTILINE)
def secnum_sub(m):
    return m.group(1) + '\n' + m.group(3) * len(m.group(1))

# Convert Ravenbrook style citations into MPS Manual style citations
# (design.mps.doc.fmt.citation)
#
# Example citations transformation, from:
#     .. [THVV_1995] "Structure Marking"; Tom Van Vleck; 1995;
#        <http://www.multicians.org/thvv/marking.html>.
# to:
#     .. [THVV_1995] Tom Van Vleck. 1995. "`Structure Marking <http://www.multicians.org/thvv/marking.html>`__".
citation = re.compile(
    r'''
        ^\.\.\s+(?P<ref>\[[^\n\]]+\])\s*
        "(?P<title>[^"]+?)"\s*
        ;\s*(?P<author>[^;]+?)\s*
        (?:;\s*(?P<organization>[^;]+?)\s*)?
        ;\s*(?P<date>[0-9-]+)\s*
        (?:;\s*<\s*(?P<url>[^>]*?)\s*>\s*)?
        \.
    ''',
    re.VERBOSE | re.MULTILINE | re.IGNORECASE | re.DOTALL
)
def citation_sub(m):
    groups = {k: re.sub(r'\s+', ' ', v) for k, v in m.groupdict().items() if v}
    fmt = '.. {ref} {author}.'
    if 'organization' in groups:
        fmt += ' {organization}.'
    fmt += ' {date}.'
    if 'url' in groups:
        fmt += ' "`{title} <{url}>`__".'
    else:
        fmt += ' "{title}".'
    return fmt.format(**groups)

index = re.compile(r'^:Index\s+terms:(.*$\n(?:[ \t]+.*$\n)*)',
                   re.MULTILINE | re.IGNORECASE)

# <http://sphinx-doc.org/markup/misc.html#directive-index>
index_term = re.compile(r'^\s*(\w+):\s*(.*?)\s*$', re.MULTILINE)

def index_sub(m):
    s = '\n.. index::\n'
    for term in index_term.finditer(m.group(1)):
        s += '   %s: %s\n' % (term.group(1), term.group(2))
    s += '\n'
    return s

def convert_file(name, source, dest):
    s = open(source, 'rb').read().decode('utf-8')
    # We want the index directive to go right at the start, so that it leads
    # to the whole document.
    m = index.search(s)
    if m:
        s = index_sub(m) + '.. _design-{0}:\n\n'.format(name) + s
    s = mode.sub(r'', s)
    s = prefix.sub(r'.. mps:prefix:: \1', s)
    s = rst_tag.sub(r'', s)
    s = mps_tag.sub(r':mps:tag:`\1`', s)
    s = mps_ref.sub(r':mps:ref:`\1`', s)
    s = typedef.sub(r'.. c:type:: \1', s)
    s = macrodef.sub(r'.. c:macro:: \1', s)
    s = funcdef.sub(r'.. c:function:: \1', s)
    s = typename.sub(r':c:type:`\1`', s)
    s = func.sub(r':c:func:`\1`', s)
    s = macro.sub(r':c:macro:`\1`', s)
    s = secnum.sub(secnum_sub, s)
    s = citation.sub(citation_sub, s)
    s = design_ref.sub(r'\1.html', s)
    s = design_frag_ref.sub(r'\1.html#design.mps.\2.\3', s)
    s = relative_link.sub(r'\1../../', s)
    s = history.sub('', s)
    # Don't try to format all the quoted code blocks as C.
    s = '.. highlight:: none\n\n' + s
    try:
        os.makedirs(os.path.dirname(dest))
    except:
        pass
    with open(dest, 'wb') as out:
        out.write(s.encode('utf-8'))

def newer(src, target):
    """Return True if src is newer (that is, modified more recently) than
    target, False otherwise.

    """
    return (not os.path.isfile(target)
            or os.path.getmtime(target) < os.path.getmtime(src)
            or os.path.getmtime(target) < os.path.getmtime(__file__))

logger = logging.getLogger(__name__)

# Mini-make
#
# This uses app.srcdir (the directory of the manual sources) to find
# the root of the MPS tree (two directories above) and so find the
# design documents to convert.

def convert_updated(app):
    logger.info(bold('converting MPS design documents'))
    for design in glob.iglob(os.path.join(app.srcdir, '../../design/*.txt')):
        name = os.path.splitext(os.path.basename(design))[0]
        if name == 'index': continue
        converted = os.path.join(app.srcdir, 'design/%s.rst' % name)
        if newer(design, converted):
            logger.info('converting design %s' % name)
            convert_file(name, design, converted)
    diagrams = chain(*[glob.iglob(os.path.join(app.srcdir, '../../design/*.' + ext))
                       for ext in 'png svg'.split()])
    for diagram in diagrams:
        target = os.path.join(app.srcdir, 'design/', os.path.basename(diagram))
        if newer(diagram, target):
            shutil.copyfile(diagram, target)


# A. REFERENCES
#
# [GDR-2018-09-18] "Documentation"; Gareth Rees; 2018-09-18; design.mps.doc.
#
#
# B. DOCUMENT HISTORY
#
# 2013-06-01 RB Created.
# 2023-02-15 RB Updated with copyright, licence, and document history.
#
#
# C. COPYRIGHT AND LICENSE
#
# Copyright (C) 2023 Ravenbrook Limited <https://www.ravenbrook.com/>.
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
#
# $Id$
