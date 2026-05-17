#!/usr/bin/env python3
"""Test server for shutdown-clean test.

Waits for a 'harakiri' notification and then exits cleanly.
"""
import os, sys
sys.path.insert(0, os.path.dirname(__file__))
from common import read_msg, log


def main():
    while True:
        msg = read_msg()
        if msg is None:
            break
        method = msg.get('method')
        log(f'<- {method or "(response)"} id={msg.get("id")}')
        if method == 'harakiri':
            log('-> very clean harakiri')
            break


if __name__ == '__main__':
    main()
