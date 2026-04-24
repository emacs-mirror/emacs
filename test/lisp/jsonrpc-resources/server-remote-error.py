#!/usr/bin/env python3
"""Test server for scontrol-remote-error.

Choreography (anxious continuation survives an rdispatcher error):

  client -> server:  taskA               (id=1)
  server -> client:  badMethod           (id=1000)  <- rdispatcher signals jsonrpc-error
  server -> client:  response taskA      "ok"       <- anxious during badMethod dispatch
  client -> server:  error response badMethod {code: -32601}

Even though the remote-request dispatch produces an error reply, the
anxious continuation for taskA must still fire and resolve to "ok".
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
            # Send badMethod BEFORE responding to taskA; the client rdispatcher
            # will signal a jsonrpc-error for it.
            write_msg({'jsonrpc': '2.0', 'id': 1000,
                       'method': 'badMethod', 'params': {}})
            log('-> badMethod id=1000')
            write_msg({'jsonrpc': '2.0', 'id': mid, 'result': 'ok'})
            log(f'-> (response taskA) id={mid}')
            # Collect the error response to badMethod.
            err = read_msg()
            log(f'<- (error response badMethod): {err}')


if __name__ == '__main__':
    main()
