#                              Ravenbrook
#                     <http://www.ravenbrook.com/>
#
#                P4.PY -- PYTHON INTERFACE TO PERFORCE
#
#             Gareth Rees, Ravenbrook Limited, 2001-03-07

from collections import Iterator
from contextlib import contextmanager
import marshal
import os
import re
from subprocess import Popen, PIPE

class Error(Exception):
    pass

class Command(Iterator):
    """A Perforce command and its output.

    You can iterate over the Command object to yield the outputs of
    the command as dictionaries. The keys and values in these output
    dictionaries are decoded according to the connection's encoding.

        >>> conn = Connection(encoding='utf8')
        >>> for change in conn.run('changes', '-m', '2'):
        ...     print("{change} {desc}".format(**change))
        10021 Explaining how to use the autom
        10020 Archiving new mail

    Iteration raises Error if the output indicates that an error
    occurred.

        >>> print(next(conn.run('help', 'xyzzy'))['data'])
        Traceback (most recent call last):
            ...
        p4.Error: No help for xyzzy.

    Call the send() method to send input to commands like client -i.
    The keys and values in these input dictionaries are encoded
    according to encoding.

        >>> cmd = conn.run('client', '-i')
        >>> cmd.send({'Client': 'abc', 'Root': '/'})
        >>> print(next(cmd)['data'])
        'Client abc saved.'

    Call the done() method to finish executing the Perforce command,
    closing the input, discarding any output, and raising Error if it
    failed. For example:

        >>> conn.run('edit', filespec).done()

    The send() method returns the Command object, to allow method calls to
    be chained:

        >>> conn.run('client', '-i').send(client).done()

    In the common case where run() is immediately followed by done(),
    use the do() method:

        >>> conn.do('submit', '-d', description)

    """
    def __init__(self, *args, **kwargs):
        self.encoding = kwargs.pop('encoding', 'utf8')
        self.pipe = Popen([a.encode('utf8') for a in args],
                          stdin = PIPE, stdout = PIPE)

    def __next__(self):
        # Ensure that stdin is closed before attempting to read any
        # output, to avoid deadlock.
        self.pipe.stdin.close()
        try:
            data = marshal.load(self.pipe.stdout)
        except EOFError:
            raise StopIteration
        def decode(s):
            if isinstance(s, bytes):
                return s.decode(self.encoding, 'replace')
            else:
                return s
        data = {decode(k): decode(v) for k, v in data.items()}
        if data.get('code') == 'error':
            raise Error(data.get('data', ''))
        return data

    next = __next__             # for compatibility with Python 2

    def send(self, data):
        def encode(s):
            if isinstance(s, type(u'')):
                return s.encode(self.encoding)
            else:
                return s
        data = {encode(k): encode(v) for k, v in data.items()}
        marshal.dump(data, self.pipe.stdin, 0)
        return self

    def done(self):
        for _ in self:
            pass

class Connection(object):
    """A connection to a Perforce server.

    The constructor takes these keyword arguments:

    p4 -- the Perforce client executable (default 'p4')
    port -- the server's listen address
    client -- the client name
    user -- the user name
    encoding -- the encoding to use for text (default 'utf8')

    """
    def __init__(self, p4='p4', port=None, client=None, user=None,
                 encoding='utf8'):
        self.args = [p4, '-G']
        if port: self.args.extend(['-p', port])
        if user: self.args.extend(['-u', user])
        if client: self.args.extend(['-c', client])
        self.encoding = encoding
        self.kwargs = dict(p4=p4, port=port, client=client, user=user,
                           encoding=encoding)

    def run(self, *args):
        """Run a Perforce command.

            >>> conn = Connection()
            >>> conn.run('edit', filespec)
            >>> conn.run('submit', '-d', description, filespec)

        Returns a Command object.

        """
        return Command(*(self.args + list(args)), encoding=self.encoding)

    def do(self, *args):
        """Run a Perforce command and consume its output."""
        self.run(*args).done()

    def contents(self, filespec):
        """Return the contents of the file whose specification is given as a
        string. If the file does not exist, raise p4.Error.

        """
        return ''.join(t['data'] for t in self.run('print', filespec)
                       if 'data' in t)

    @contextmanager
    def temp_client(self, client_spec):
        """Return a context manager that creates a temporary client workspace
        on entry and deletes it on exit.

        The client specification should omit the Client and Root keys:
        these are added automatically. The workspace views should use
        __CLIENT__ and this is replaced by the chosen client name.

        This context manager yields a tuple of the new Connection
        object, and the root directory of the client workspace.

        """
        import shutil
        import tempfile
        import uuid
        name = 'tmp-{}'.format(uuid.uuid4())
        root = tempfile.mkdtemp()
        spec = {k: re.sub(r'__CLIENT__', name, v)
                if re.match(r'View\d+$', k) else v
                for k, v in client_spec.items()}
        spec.update(Client=name, Root=root)
        try:
            conn = Connection(**dict(self.kwargs, client=name))
            conn.run('client', '-i').send(spec).done()
            try:
                yield conn, root
            finally:
                try:
                    conn.do('revert', '-k', '//...')
                except Error:
                    pass
                conn.do('client', '-d', name)
        finally:
            shutil.rmtree(root)


# Convenience interface for the default connection.
_conn = Connection()
run = _conn.run
do = _conn.do
contents = _conn.contents
temp_client = _conn.temp_client


# A. REFERENCES
#
# [SUBPROCESS] Python Standard Library: "subprocess -- Subprocess
# management"; <http://docs.python.org/library/subprocess.html>.
#
#
# B. DOCUMENT HISTORY
#
# 2001-05-20 GDR Created.
#
# 2003-02-14 NB Changed os.wait to os.waitpid for Python 2.2.
#
# 2010-10-04 GDR Rewritten to use [SUBPROCESS] instead of os.pipe.
#
# 2010-10-05 GDR Raise an exception if Perforce returns an error. New
# function 'contents' for getting the contents of a file.
#
# 2010-10-06 GDR Move p4client, p4path, p4port, and p4user to global
# variables, to make testing easier. New function 'pipe' makes it
# possible to run the 'client -i' command.
#
# 2013-12-09 GDR Merge pipe and run functions into a Run class that
# also handles encoding and decoding.
#
# 2014-03-18 GDR Refactor into classes: Connection (holding the
# client/server configuration) and Command (a single command and its
# output).
#
# 2014-03-19 GDR New methods Connection.temp_client for creating a
# temporary client workspace, and Connection.do for encapsulating
# run().done().
#
#
# C. COPYRIGHT AND LICENCE
#
# Copyright 2001-2014 Ravenbrook Ltd.  All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are
# met:
#
# 1. Redistributions of source code must retain the above copyright
#    notice, this list of conditions and the following disclaimer.
#
# 2. Redistributions in binary form must reproduce the above copyright
#    notice, this list of conditions and the following disclaimer in the
#    documentation and/or other materials provided with the
#    distribution.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
# "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
# LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
# A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE REGENTS OR
# CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
# EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
# PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
# PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
# LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
# NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
# SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
#
#
# $Id$
