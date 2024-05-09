import sys

try:
    from http.server import HTTPServer, SimpleHTTPRequestHandler
except ImportError:
    from BaseHTTPServer import HTTPServer
    from SimpleHTTPServer import SimpleHTTPRequestHandler


HandlerClass = SimpleHTTPRequestHandler
HandlerClass.protocol_version = "HTTP/1.0"
server_address = ("127.0.0.1", int(sys.argv[1]) if sys.argv[1:] else 0)
httpd = HTTPServer(server_address, HandlerClass)

ip, port = httpd.socket.getsockname()[0:2]
print("Server started, http://%s:%s/" % (ip, port))
# Flush in case we're in full buffering mode (instead of line
# buffering), this might happen if python is a cygwin program and we
# run it from a native w32 program.
sys.stdout.flush()
httpd.serve_forever()
