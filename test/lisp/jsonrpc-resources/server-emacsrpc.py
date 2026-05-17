#!/usr/bin/env python3
"""General-purpose JSONRPC server for jsonrpc.el tests.

Handles arithmetic (+ - * /), sit-for, vconcat, append, and ignore,
mirroring the methods the in-process Emacs RPC server supports.
"""
import functools
import operator
import os
import sys
import time

sys.path.insert(0, os.path.dirname(__file__))
from common import log, read_msg, write_msg, harakiri

HANDLERS = {
    '+': lambda p: sum(p),
    '-': lambda p: functools.reduce(operator.sub, p),
    '*': lambda p: functools.reduce(operator.mul, p),
    '/': lambda p: functools.reduce(operator.truediv, p),
    'vconcat': lambda p: sum(p, []),
    'append': lambda p: sum(p, []),
    'sit-for': lambda p: time.sleep(p[0]),
    'ignore': lambda _: None,
}


def main():
    while True:
        msg = read_msg()
        if msg is None:
            break
        mid = msg.get('id')
        method = msg.get('method')
        log(f'<- {method or "(response)"} id={mid}')
        if harakiri(msg): break
        if method is None or mid is None:
            continue
        handler = HANDLERS.get(method)
        if handler is None:
            write_msg({'jsonrpc': '2.0', 'id': mid,
                       'error': {'code': -32601, 'message': 'Method not found'}})
        else:
            try:
                result = handler(msg.get('params', []))
                write_msg({'jsonrpc': '2.0', 'id': mid, 'result': result})
                log(f'-> (response {method}) id={mid}')
            except Exception as exc:
                write_msg({'jsonrpc': '2.0', 'id': mid,
                           'error': {'code': -32603, 'message': str(exc)}})


if __name__ == '__main__':
    main()
