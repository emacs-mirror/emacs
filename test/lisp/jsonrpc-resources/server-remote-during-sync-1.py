#!/usr/bin/env python3
"""Test server for scontrol-remote-during-sync-1.

Choreography (tests bug#80623):

  client -> server:  LR1            (id=1)
  server -> client:  RR1            (id=1000)
  server -> client:  response LR1   "lr1-ok"
  client -> server:  response RR1   "rr1-ok"
"""
import os
import sys
sys.path.insert(0, os.path.dirname(__file__))
from common import read_msg, write_msg, log


def main():
    while True:
        msg = read_msg()
        if msg is None:
            break
        mid = msg.get('id')
        method = msg.get('method')
        log(f'<- {method or "(response)"} id={mid}')
        if method == 'harakiri':
            log('-> very clean harakiri')
            break
        elif method == 'LR1':
            # Send RR1 request
            write_msg({'jsonrpc': '2.0', 'id': 1000,
                       'method': 'RR1', 'params': {}})
            log('-> RR1 id=1000')
            # Immediately reply to LR1
            write_msg({'jsonrpc': '2.0', 'id': mid, 'result': 'lr1-ok'})
            log(f'-> (response LR1) id={mid}')
            # Now wait for reply to RR1
            resp = read_msg()
            log(f'<- (response RR1) id={resp.get("id") if resp else None}')


if __name__ == '__main__':
    main()
