"""Definitions used by commands sent to inferior Python in python.el."""

# Copyright (C) 2004, 2005 Free Software Foundation, Inc.
# Author: Dave Love <d.love@dl.ac.uk>

# This file is part of GNU Emacs.

# GNU Emacs is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2, or (at your option)
# any later version.

# GNU Emacs is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with GNU Emacs; see the file COPYING.  If not, write to the
# Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
# Boston, MA 02110-1301, USA.

import os, sys, traceback, inspect, rlcompleter, __main__

__all__ = ["eexecfile", "args", "complete", "ehelp", "eimport"]

def eexecfile (file):
    """Execute FILE and then remove it.
    If we get an exception, print a traceback with the top frame
    (oursleves) excluded."""
    try:
	try: execfile (file, globals (), globals ())
	except:
	    (type, value, tb) = sys.exc_info ()
	    # Lose the stack frame for this location.
	    tb = tb.tb_next
	    if tb is None:	# print_exception won't do it
		print "Traceback (most recent call last):"
	    traceback.print_exception (type, value, tb)
    finally:
	os.remove (file)

def eargs (name):
    "Get arglist of NAME for Eldoc &c."
    try:
	parts = name.split ('.')
	if len (parts) > 1:
	    exec 'import ' + parts[0] # might fail
	func = eval (name)
	if inspect.isbuiltin (func):
	    doc = func.__doc__
	    if doc.find (' ->') != -1:
		print '_emacs_out', doc.split (' ->')[0]
	    elif doc.find ('\n') != -1:
		print '_emacs_out', doc.split ('\n')[0]
	    return
	if inspect.ismethod (func):
	    func = func.im_func
	if not inspect.isfunction (func):
	    return
	(args, varargs, varkw, defaults) = inspect.getargspec (func)
	# No space between name and arglist for consistency with builtins.
	print '_emacs_out', \
	    func.__name__ + inspect.formatargspec (args, varargs, varkw,
						   defaults)
    except: pass

def complete (text, namespace = None):
    """Complete TEXT in NAMESPACE and print a Lisp list of completions.
    NAMESPACE is currently not used."""
    if namespace is None: namespace = __main__.__dict__
    c = rlcompleter.Completer (namespace)
    try:
        if '.' in text:
            matches = c.attr_matches (text)
        else:
            matches = c.global_matches (text)
	print '_emacs_out (',
        for elt in matches:
            print '"%s"' % elt,
        print ')'
    except:
        print '_emacs_out ()'

def ehelp (name, g, l):
    """Get help on string NAME using globals G and locals L.
    First try to eval name for, e.g. user definitions where we need
    the object.  Otherwise try the string form."""
    try: help (eval (name, g, l))
    except: help (name)

def eimport (mod, dir):
    """Import module MOD with directory DIR at the head of the search path.
    NB doesn't load from DIR if MOD shadows a system module."""
    path0 = sys.path[0]
    sys.path[0] = dir
    try:
	try:
	    if globals().has_key(mod) and inspect.ismodule (eval (mod)):
		reload(eval (mod))
	    else:
		globals ()[mod] = __import__ (mod)
	except:
	    (type, value, tb) = sys.exc_info ()
	    print "Traceback (most recent call last):"
	    traceback.print_exception (type, value, tb.tb_next)
    finally:
	sys.path[0] = path0

print '_emacs_ok'		# ready for input and can call continuation

# arch-tag: d90408f3-90e2-4de4-99c2-6eb9c7b9ca46
