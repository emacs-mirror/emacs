#!/usr/bin/env python3
"""Toy LSP server for testing Eglot's shutdown.

  --shutdown-delay N   Sleep N seconds before replying to 'shutdown'.
  --ignore-shutdown    Never reply to 'shutdown'.
  --ignore-exit        Don't exit on the 'exit' notification.
"""
import argparse
import os
import sys
import time

sys.path.insert(0, os.path.join(os.path.dirname(__file__),
                                '..', '..', 'jsonrpc-resources'))
from common import read_msg, log, reply, reply_error


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('--shutdown-delay', type=float, default=0)
    parser.add_argument('--ignore-shutdown', action='store_true')
    parser.add_argument('--ignore-exit', action='store_true')
    args = parser.parse_args()
    while (msg := read_msg()) is not None:
        method = msg.get('method')
        log(f'<- {method or "(response)"} id={msg.get("id")}')
        if method == 'initialize':
            reply(msg, {'capabilities': {}})
        elif method == 'shutdown':
            if args.ignore_shutdown:
                continue
            time.sleep(args.shutdown_delay)
            reply(msg, None)
        elif method == 'exit':
            if not args.ignore_exit:
                break
        elif 'id' in msg and method:
            reply_error(msg, -32601, f'{method} not supported')
    log('exiting')


if __name__ == '__main__':
    main()
