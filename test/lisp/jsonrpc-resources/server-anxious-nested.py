#!/usr/bin/env python3
"""Test server for scontrol-anxious-nested.

Choreography (exercises the anxious-continuation mechanism):

  client -> server:  taskA              (id=1)
  server -> client:  callBack           (id=1000)
  server -> client:  response taskA     "done"   <- anxious: arrives while followUp pending
  client -> server:  followUp           (id=2)   <- sent by rdispatcher for callBack
  server -> client:  response followUp  "fw-ok"
  client -> server:  response callBack           <- rdispatcher return value

scontrol stack at deepest point:
  ((:local 2) (:remote 1000) (:local 1))

The response-to-taskA arrives while (:local followUp-id) is the head,
so it is queued as an anxious continuation.  When followUp completes,
the anxious entry is rescheduled via run-at-time and taskA resolves.
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
            # Send callBack, then immediately respond to taskA without awaiting
            # anything.  The response-to-taskA will be queued as anxious on the
            # client while its rdispatcher blocks waiting for followUp.
            write_msg({'jsonrpc': '2.0', 'id': 1000,
                       'method': 'callBack', 'params': {}})
            log('-> callBack id=1000')
            write_msg({'jsonrpc': '2.0', 'id': mid, 'result': 'done'})
            log(f'-> (response taskA) id={mid}')
            # followUp arrives next: the client's rdispatcher for callBack
            # issues it as a nested sync request.
            follow = read_msg()
            fid = follow.get('id') if follow else None
            log(f'<- followUp id={fid}')
            write_msg({'jsonrpc': '2.0', 'id': fid, 'result': 'fw-ok'})
            log(f'-> (response followUp) id={fid}')
            # Finally collect the callBack response (rdispatcher return value).
            cb_resp = read_msg()
            log(f'<- (response callBack) id={cb_resp.get("id") if cb_resp else None}')


if __name__ == '__main__':
    main()
