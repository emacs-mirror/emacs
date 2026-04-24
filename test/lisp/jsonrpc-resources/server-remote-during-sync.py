#!/usr/bin/env python3
"""Test server for scontrol-remote-during-sync.

Choreography (tests bug#80623):

  client -> server:  taskA            (id=1)
  server -> client:  showInfo         (id=1000)  <- before responding to taskA
  server -> client:  response taskA   "done"
  client -> server:  response showInfo            <- from rdispatcher

The (:remote 1000) entry on scontrol defers the response-to-taskA
continuation until showInfo dispatch completes.
"""
import os, sys
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
            log('-> harakiri: exiting cleanly')
            break
        elif method == 'taskA':
            # Send showInfo request BEFORE responding to taskA.
            write_msg({'jsonrpc': '2.0', 'id': 1000,
                       'method': 'showInfo', 'params': {}})
            log('-> showInfo id=1000')
            write_msg({'jsonrpc': '2.0', 'id': mid, 'result': 'done'})
            log(f'-> (response taskA) id={mid}')
            # Collect the client's response to showInfo.
            resp = read_msg()
            log(f'<- (response showInfo) id={resp.get("id") if resp else None}')


if __name__ == '__main__':
    main()
