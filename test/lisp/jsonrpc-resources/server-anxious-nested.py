#!/usr/bin/env python3
"""Test server for scontrol-anxious-nested.

Choreography (exercises the anxious-continuation mechanism):

  client -> server:  LR1              (id=1)
  server -> client:  RR1              (id=1000)
  server -> client:  response LR1     "lr1-ok"
  client -> server:  LR2              (id=2)
  server -> client:  response LR2     "lr2-ok"
  client -> server:  response RR1     "lr2-ok"

LR2 should complete first, then RR1, then LR1.
"""
import os
import sys
sys.path.insert(0, os.path.dirname(__file__))
from common import read_msg, write_msg, log


def main():
    while True:
        lr1 = read_msg()
        if lr1 is None:
            break
        mid = lr1.get('id')
        method = lr1.get('method')
        log(f'<- {method or "(response)"} id={mid}')
        if method == 'harakiri':
            log('-> very clean harakiri')
            break
        elif method == 'LR1':
            # Send RR1, then immediately respond to LR1 without awaiting
            # anything.  The response-to-LR1 will be queued as anxious on the
            # client while its rdispatcher blocks waiting for LR2.
            write_msg({'jsonrpc': '2.0', 'id': 1000,
                       'method': 'RR1', 'params': {}})
            log('-> RR1 id=1000')
            write_msg({'jsonrpc': '2.0', 'id': mid, 'result': 'lr1-ok'})
            log(f'-> (response LR1) id={mid}')
            # LR2 arrives next: the client's rdispatcher for RR1
            # issues it as a nested sync request.
            lr2 = read_msg()
            fid = lr2.get('id') if lr2 else None
            log(f'<- LR2 id={fid}')
            write_msg({'jsonrpc': '2.0', 'id': fid, 'result': 'lr2-ok'})
            log(f'-> (response LR2) id={fid}')
            # Finally collect the RR1 response (rdispatcher return value).
            rr1_resp = read_msg()
            log(f'<- (response RR1) id={rr1_resp.get("id") if rr1_resp else None}')


if __name__ == '__main__':
    main()
