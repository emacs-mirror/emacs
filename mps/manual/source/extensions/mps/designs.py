import os
import os.path
import glob
import re
import sys

TYPES = '''

    AccessSet Accumulation Addr Align AP Arg Arena Attr Bool BT Buffer
    Byte Clock Compare Count Epoch Format Fun Index LD Lock Message
    Pointer Pool PThreadext Rank RankSet Ref Res Reservoir Ring Root
    RootVar ScanState Seg Serial Shift Sig Size Space SplayNode
    SplayTree Thread Trace TraceId TraceSet ULongest VM Word

'''

mode = re.compile(r'\.\. mode: .*\n')
prefix = re.compile(r'^:Tag: ([a-z][a-z.0-9-]*[a-z0-9])$', re.MULTILINE)
rst_tag = re.compile(r'^:(?:Author|Date|Status|Revision|Copyright|Organization|Format):.*?$\n', re.MULTILINE)
mps_tag = re.compile(r'_`\.([a-z][A-Za-z.0-9_-]*[A-Za-z0-9])`:')
mps_ref = re.compile(r'`(\.[a-z][A-Za-z.0-9_-]*[A-Za-z0-9])`_(?:        )?')
funcdef = re.compile(r'^``([^`]*\([^`]*\))``$', re.MULTILINE)
macrodef = re.compile(r'^``([A-Z][A-Z0-9_]+)``$', re.MULTILINE)
macro = re.compile(r'``([A-Z][A-Z0-9_]+)``(?:       )?')
typedef = re.compile(r'^``typedef ([^`]*)``$', re.MULTILINE) 
func = re.compile(r'``([A-Za-z][A-Za-z0-9_]+\(\))``')
typename = re.compile(r'``({0}|[A-Z][A-Za-z0-9_]*(?:Class|Struct|Method)|mps_[a-z_]+_[stu])``(?:      )?'
                      .format('|'.join(map(re.escape, TYPES.split()))))

# Example citations transformation, from:
#     .. [THVV_1995] "Structure Marking"; Tom Van Vleck; 1995;
#        <http://www.multicians.org/thvv/marking.html>.
# to:
#      .. [THVV_1995] Tom Van Vleck. 1995. "`Structure Marking <http://www.multicians.org/thvv/marking.html>`__".
citation = re.compile(
    r'''
        \.\.\s+(?P<ref>\[.*?\])\s*
        "(?P<title>.*?)"\s*
        ;\s*(?P<author>.*?)\s*
        (?:;\s*(?P<organization>.*?)\s*)?
        ;\s*(?P<date>[0-9-]+)\s*
        (?:;\s*<\s*(?P<url>.*?)\s*>\s*)?
        \.
    ''',
    re.VERBOSE | re.MULTILINE | re.IGNORECASE | re.DOTALL
)
def citation_sub(m):
    groups = m.groupdict()
    for key in groups:
        if groups[key]:
            groups[key] = re.sub(r'\s+', ' ', groups[key])
    result = '.. {ref} {author}.'.format(**groups)
    if groups.get('organization'):
        result += ' {organization}.'.format(**groups)
    result += ' {date}.'.format(**groups)
    if groups.get('url'):
        result += ' "`{title} <{url}>`__".'.format(**groups)
    else:
        result += ' "{title}".'.format(**groups)
    return result
history = re.compile(r'^Document History\n.*',
                     re.MULTILINE | re.IGNORECASE | re.DOTALL)

def convert_file(name, source, dest):
    s = open(source).read()
    s = mode.sub(r'', s)
    s = prefix.sub(r'.. mps:prefix:: \1', s)
    s = rst_tag.sub(r'', s)
    s = mps_tag.sub(r':mps:tag:`\1`', s)
    s = mps_ref.sub(r':mps:ref:`\1`', s)
    s = typedef.sub(r'.. c:type:: \1', s)
    s = funcdef.sub(r'.. c:function:: \1', s)
    s = macrodef.sub(r'.. c:macro:: \1', s)
    s = typename.sub(r':c:type:`\1`', s)
    s = func.sub(r':c:func:`\1`', s)
    s = macro.sub(r':c:macro:`\1`', s)
    s = citation.sub(citation_sub, s)
    s = history.sub('', s)
    try:
        os.makedirs(os.path.dirname(dest))
    except:
        pass
    with open(dest, 'w') as out:
        # out.write('.. _design-{0}:\n\n'.format(name))
        out.write(s)

def convert_updated():
    for design in glob.iglob('../design/*.txt'):
        name = os.path.splitext(os.path.basename(design))[0]
        converted = 'converted/%s.rst' % name
        if (not os.path.isfile(converted) or
            os.path.getmtime(converted) < os.path.getmtime(converted)):
            print 'Converting %s to %s' % (design, converted)
            convert_file(name, design, converted)
