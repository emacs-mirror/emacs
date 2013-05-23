#!/usr/bin/python

import fileinput
import os
import re
from sys import stdout

TYPES = '''

    AccessSet Accumulation Addr Align AP Arg Arena Attr Bool BT Buffer
    Byte Clock Compare Count Epoch Format Fun Index LD Lock Message
    Pointer Pool Rank RankSet Ref Res Reservoir Ring Root RootVar
    ScanState Seg Serial Shift Sig Size Space SplayNode SplayTree
    Thread Trace TraceId TraceSet ULongest VM Word

'''

def main():
    mode = re.compile(r'\.\. mode: .*\n')
    prefix = re.compile(r'^:Tag: ([a-z][a-z.0-9-]+[a-z0-9])$')
    rst_tag = re.compile(r'^:(?:Author|Date|Status|Revision|Copyright|Organization):.*\n')
    mps_tag = re.compile(r'_`\.([a-z][A-Za-z.0-9_-]+[A-Za-z0-9])`:')
    mps_ref = re.compile(r'`(\.[a-z][A-Za-z.0-9_-]+[A-Za-z0-9])`_(?:        )?')
    funcdef = re.compile(r'^``([^`]*\([^`]*\))``$')
    macrodef = re.compile(r'^``([A-Z][A-Z0-9_]+)``$')
    macro = re.compile(r'``([A-Z][A-Z0-9_]+)``')
    typedef = re.compile(r'^``typedef ([^`]*)``$') 
    func = re.compile(r'``([A-Za-z][A-Za-z0-9_]+\(\))``')
    typename = re.compile(r'``({0}|[A-Z][A-Za-z0-9_]+(?:Class|Struct|Method)|mps_[a-z_]+_[stu])``(?:      )?'
                          .format('|'.join(map(re.escape, TYPES.split()))))

    for s in fileinput.input():
        if fileinput.lineno() == 1:
            name, _ = os.path.splitext(fileinput.filename())
            stdout.write('.. _design-{0}:\n\n'.format(name))
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
        if s == 'Document History\n':
            fileinput.nextfile()
        else:
            stdout.write(s)

if __name__ == '__main__':
    main()
