"""Common JSONRPC framing helpers for jsonrpc.el test servers."""

import json
import sys

def read_msg():
    """Read one Content-Length-framed JSON-RPC message from stdin."""
    headers = {}
    while True:
        line = sys.stdin.buffer.readline()
        if not line:
            return None
        text = line.decode('utf-8').rstrip('\r\n')
        if not text:
            break
        if ':' in text:
            k, _, v = text.partition(':')
            headers[k.strip()] = v.strip()
    n = int(headers.get('Content-Length', 0))
    return json.loads(sys.stdin.buffer.read(n).decode('utf-8')) if n else None

def write_msg(msg):
    """Write one Content-Length-framed JSON-RPC message to stdout."""
    body = json.dumps(msg, ensure_ascii=False).encode('utf-8')
    sys.stdout.buffer.write(
        f'Content-Length: {len(body)}\r\n\r\n'.encode('utf-8') + body
    )
    sys.stdout.buffer.flush()

def log(text):
    """Write a log line to stderr."""
    print(f'[test-server] {text}', file=sys.stderr, flush=True)
