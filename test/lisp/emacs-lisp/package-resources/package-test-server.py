import sys
import BaseHTTPServer
from SimpleHTTPServer import SimpleHTTPRequestHandler


HandlerClass = SimpleHTTPRequestHandler
ServerClass  = BaseHTTPServer.HTTPServer
Protocol     = "HTTP/1.0"

if sys.argv[1:]:
    port = int(sys.argv[1])
else:
    port = 0
server_address = ('127.0.0.1', port)

HandlerClass.protocol_version = Protocol
httpd = ServerClass(server_address, HandlerClass)

ip, port = httpd.socket.getsockname()[0:2]
print ("Server started, http://%s:%s/" % (ip, port))
# Flush in case we're in full buffering mode (instead of line
# buffering), this might happen if python is a cygwin program and we
# run it from a native w32 program.
sys.stdout.flush()
httpd.serve_forever()
