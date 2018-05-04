#!/usr/bin/python
#
# NOTE: THIS PROGRAM DOES NOT WORK!
# It is intended as a regression test source for the Python support in etags.
# If you want a working version, you'll find it in the fetchmail distribution.
#

from Tkinter import *
from Dialog import *
import sys
import time
import os

#
# Define the data structures the GUIs will be tossing around
#
class Controls:
    def __init__(self):
	self.foreground = FALSE;	# Run in background
	self.daemon = 300		# Default to 5-minute timeout
	self.syslog = FALSE		# Use syslogd for logging?
	self.logfile = None		# No logfile, initially

    def __repr__(self):
	str = "";
	if self.syslog:
	   str = str + ("set syslog\n")
	elif self.logfile:
	    str = str + ("set logfile \"%s\"\n" % (self.logfile,));
	if not self.foreground and self.daemon:
	    str = str + ("set daemon %s\n" % (self.daemon,))
	return str + "\n"

    def __str__(self):
	return "[Server: " + repr(self) + "]"

class Server:
    def __init__(self):
	self.pollname = None		# Poll label
	self.via = None			# True name of host
	self.active = TRUE		# Poll status
	self.interval = 0		# Skip interval
	self.protocol = 'auto'		# Default to auto protocol
	self.port = 0			# Port number to use
	self.uidl = FALSE		# Don't use RFC1725 UIDLs by default
	self.auth = "password"		# Default to password authentication
	self.timeout = 300		# 5-minute timeout
	self.envelope = "Received"	# Envelope-address header
	self.aka = []			# List of DNS aka names
	self.dns = TRUE			# Enable DNS lookup on multidrop
	self.localdomains = []		# Domains to be considered local
	self.interface = None		# IP address and range
	self.monitor = None		# IP address and range
	self.userlist = []		# List of user entries for site
	self.typemap = (
	    ('pollname',  'String'),
	    ('via',       'String'),
	    ('active',    'Boolean'),
	    ('interval',  'Int'),
	    ('protocol',  'String'),
	    ('interval',  'Int'),
	    ('port',      'Int'),
	    ('uidl',      'Boolean'),
	    ('auth',      'String'),
	    ('timeout',   'Int'),
	    ('envelope',  'String'),
	    # leave aka out
	    ('dns',       'Boolean'),
	    # leave localdomains out
	    ('interface', 'String'),
	    ('monitor',   'String'))

    def dump(self, folded):
	str = ""
	if self.active:   str = str + "poll"
	else:             str = str + "skip"
	str = str + (" " + self.pollname)
	if self.via != self.pollname:
	    str = str + " via " + self.via
	if self.protocol != ServerDefaults.protocol:
	    str = str + " with proto " + self.protocol 
	if self.port != defaultports[self.protocol]:
	    str = str + " port " + `self.port`
	if self.timeout != ServerDefaults.timeout:
	    str = str + " timeout " + `self.timeout`
	if self.interval != ServerDefaults.interval: 
	    str = str + " interval " + `self.interval` 
	if self.envelope != ServerDefaults.envelope:
	    str = str + " envelope " + self.envelope
	if self.auth != ServerDefaults.auth:
	    str = str + " auth " + self.auth
	if self.dns != ServerDefaults.dns or self.uidl != ServerDefaults.uidl:
	    str = str + " and options"
	if self.dns != ServerDefaults.dns:
	    str = str + flag2str(self.dns, 'dns')
	if self.uidl != ServerDefaults.uidl:
	    str = str + flag2str(self.uidl, 'uidl')
	if folded:        str = str + "\n\t"
	else:             str = str + " "

	if self.aka:
	     str = str + "aka"
	     for x in self.aka:
		str = str + " " + x
	if self.aka and self.localdomains: str = str + " "
	if self.localdomains:
	     str = str + ("localdomains")
	     for x in self.localdomains:
		str = str + " " + x
        if (self.aka or self.localdomains):
	    if folded:
		str = str + "\n\t"
	    else:
		str = str + " "

	if self.interface: str = str + " interface " + self.interface
	if self.monitor: str = str + " monitor " + self.monitor
	if (self.interface or self.monitor):
	    if folded:
		str = str + "\n"

	if str[-1] == "\t": str = str[0:-1]
	return str;

    def __repr__(self):
	return self.dump(TRUE)

    def __str__(self):
	return "[Server: " + self.dump(FALSE) + "]"

class User:
    def __init__(self):
	self.username = ""		# Remote username
	self.localnames = None		# Local names
	self.password = ""		# Password for mail account access
	self.smpthost = 'localhost'	# Host to forward to
	self.mda = ""			# Mail Delivery Agent
	self.preconnect = ""		# Connection setup
	self.postconnect = ""		# Connection wrapup
	self.keep = FALSE		# Keep messages
	self.flush = FALSE		# Flush messages
	self.fetchall = FALSE		# Fetch old messages
	self.rewrite = TRUE		# Rewrite message headers
	self.forcecr = FALSE		# Force LF -> CR/LF
	self.stripcr = FALSE		# Strip CR
	self.pass8bits = FALSE		# Force BODY=7BIT
	self.dropstatus = FALSE		# Force BODY=7BIT
	self.limit = 0			# Message size limit
	self.fetchlimit = 0		# Max messages fetched per batch
	self.batchlimit = 0		# Max message forwarded per batch
	self.typemap = (
	    ('username',    'String')
	    ('folder',      'String')
	    # leave out localnames
	    ('password',    'String')
	    ('smtphost',    'String')
	    ('preconnect',  'String')
	    ('postconnect', 'String')
	    ('mda',         'String')
	    ('keep',        'Boolean')
	    ('flush',       'Boolean')
	    ('fetchall',    'Boolean')
	    ('rewrite',     'Boolean')
	    ('forcecr',     'Boolean')
	    ('stripcr',     'Boolean')
	    ('pass8bits',   'Boolean')
	    ('dropstatus',  'Boolean')
	    ('limit',       'Int')
	    ('fetchlimit',  'Int')
	    ('batchlimit',  'Int'))

    def __repr__(self):
	str = ""
	str = str + "user " + self.user;
	if self.password: str = str + "with password " + self.password
	if self.localnames:
	     str = str + "localnames"
	     for x in self.localnames:
		str = str + " " + x
	if (self.keep or self.flush or self.fetchall or self.rewrite or
          self.forcecr or self.stripcr or self.pass8bits or self.dropstatus):
	    str = str + " options"
	if self.keep != UserDefaults.keep:
	    str = str + flag2str(self.keep, 'keep')
	if self.flush != UserDefaults.flush:
	    str = str + flag2str(self.flush, 'flush')
	if self.fetchall != UserDefaults.fetchall:
	    str = str + flag2str(self.fetchall, 'fetchall')
	if self.rewrite != UserDefaults.rewrite:
	    str = str + flag2str(self.rewrite, 'rewrite')
	if self.forcecr != UserDefaults.forcecr:
	    str = str + flag2str(self.forcecr, 'forcecr')
	if self.stripcr != UserDefaults.stripcr:
	    str = str + flag2str(self.stripcr, 'stripcr')
	if self.pass8bits != UserDefaults.pass8bits:
	    str = str + flag2str(self.pass8bits, 'pass8bits')
	if self.dropstatus != UserDefaults.dropstatus:
	    str = str + flag2str(self.dropstatus, 'dropstatus')
	if self.limit != UserDefaults.limit:
	    str = str + " limit " + `self.limit`
	if self.fetchlimit != UserDefaults.fetchlimit:
	    str = str + " fetchlimit " + `self.fetchlimit`
	if self.batchlimit != UserDefaults.batchlimit:
	    str = str + " batchlimit " + `self.batchlimit`

    def __str__(self):
	return "[User: " + repr(self) + "]"
    
#
# Helper code
#

defaultports = {"auto":0,
    		"POP2":109, 
		"POP3":110, "APOP":110, "KPOP":1109, "IMAP":143,
		"IMAP-K4":143,
		"ETRN":25}

protolist = ("auto", "POP2", "POP3", "APOP", "KPOP", "IMAP", "IMAP-K4", "ETRN")

authlist = ("password", "kerberos")

def flag2str(value, string):
# make a string representation of a .fetchmailrc flag or negated flag
    str = ""
    if value != None:
	str = str + (" ")
	if value == FALSE: str = str + ("no ")
	str = str + string;
    return str

class LabeledEntry(Frame):
# widget consisting of entry field with caption to left
    def bind(self, key, action):
	self.E.bind(key, action)
    def focus_set(self):
	self.E.focus_set()
    def __init__(self, Master, text, textvar, width):
	Frame.__init__(self, Master)
	self.L = Label(self, {'text':text, 'width':width, 'anchor':'w'})
	self.E = Entry(self, {'textvar':textvar})
	self.L.pack({'side':'left'})
	self.E.pack({'side':'left', 'expand':'1', 'fill':'x'})

def ButtonBar(frame, legend, ref, alternatives, command):
# horizontal bar of radio buttons, caption to left, picking from a string list
    bar = Frame(frame)
    Label(bar, text=legend).pack(side=LEFT)
    for alt in alternatives:
	Radiobutton(bar,
		{'text':alt, 'variable':ref, 'value':alt, 'command':command}).pack(side=LEFT)
    bar.pack(side=TOP);
    return bar

def helpwin(helpdict):
# help message window with a self-destruct button
    helpwin = Toplevel()
    helpwin.title(helpdict['title']) 
    helpwin.iconname(helpdict['title'])
    Label(helpwin, text=helpdict['banner']).pack()
    textwin = Message(helpwin, text=helpdict['text'], width=600)
    textwin.pack()
    Button(helpwin, text='Done', 
	   command=lambda x=helpwin: Widget.destroy(x),
	   relief=SUNKEN, bd=2).pack()

class ListEdit(Frame):
# edit a list of values (duplicates not allowed) with a supplied editor hook 
    def __init__(self, newlegend, list, editor, master):
	self.editor = editor
	self.list = list

	# Set up a widget to accept new sites
	self.newval = StringVar(master)
	newwin = LabeledEntry(master, newlegend, self.newval, '16')
	newwin.bind('<Double-1>', self.handleNew)
	newwin.bind('<Return>', self.handleNew)
	newwin.pack(side=TOP, fill=X, anchor=E)

	# Create the sitelist for site-configuration selection
	listframe = Frame(master)
	scroll = Scrollbar(listframe)
	listwidget = Listbox(listframe, height=0, selectmode='browse')
	if list:
	    for dnsname in list:
		listwidget.insert('end', dnsname)
	listframe.pack(side=TOP, expand=YES, fill=BOTH)
	listwidget.config(yscrollcommand=scroll.set, relief=SUNKEN)
	listwidget.pack(side=LEFT, expand=YES, fill=BOTH)
	scroll.config(command=listwidget.yview, relief=SUNKEN)
	scroll.pack(side=RIGHT, fill=BOTH)
	listwidget.config(selectmode=SINGLE, setgrid=TRUE)
	listwidget.bind('<Double-1>', self.handleList);
	listwidget.bind('<Return>', self.handleList);
	self.listwidget = listwidget

	bf = Frame(master);
	if self.editor:
	    Button(bf, text='Edit',   command=self.editItem).pack(side=LEFT)
	Button(bf, text='Delete', command=self.deleteItem).pack(side=RIGHT)
	bf.pack(fill=X)

    def handleList(self, event):
	self.editItem();

    def handleNew(self, event):
	item = self.newval.get()
	entire = self.listwidget.get(0, self.listwidget.index('end'));
	if item and (not entire) or (not item in self.listwidget.get(0, self.listwidget.index('end'))):
	    self.listwidget.insert('end', item)
	    if self.list != None: self.list.append(item)
	self.newval.set('')

    def editItem(self):
	index = self.listwidget.curselection()[0]
	if index and self.editor:
	    label = self.listwidget.get(index);
	    apply(self.editor, (label,))

    def deleteItem(self):
	index = self.listwidget.curselection()[0]
	if index:
	    self.listwidget.delete(index)
	    if self.list != None: del self.list[index]

def ConfirmQuit(frame, context):
    ans = Dialog(frame, 
		 title = 'Quit?',
		 text = 'Really quit ' + context + ' without saving?',
		 bitmap = 'question',
		 strings = ('Yes', 'No'),
		 default = 1)
    return ans.num == 0
#
# First, code to set the global fetchmail run controls.
#

confighelp = {
    'title' : 'Fetchmail configurator help',
    'banner': 'Configurator help',
    'text' : """
In the `Configurator Controls' panel, you can:

Press `Save' to save the new fetchmail configuration you have created.
Press `Quit' to exit without saving.
Press `Help' to bring up this help message.

In the `Configurator Controls' panel, you can set the following options that
control how fetchmail runs:

Poll interval
        Number of seconds to wait between polls in the background.
        Ignored if the `Run in Foreground?' option is on.

Logfile
        If empty, emit progress and error messages to stderr.
        Otherwise this gives the name of the files to write to.
        This field is ignored if the "Log to syslog?" option is on.

In the `Remote Mail Configurations' panel, you can:

1. Enter the name of a new remote mail server you want fetchmail to query.

To do this, simply enter a label for the poll configuration in the
`New Server:' box.  The label should be a DNS name of the server (unless
you are using ssh or some other tunneling method and will fill in the `via'
option on the site configuration screen).

2. Change the configuration of an existing site.

To do this, find the site's label in the listbox and double-click it.
This will take you to a site configuration dialogue.
"""}

class ControlEdit(Frame):
    def PostControls(self):
	self.foreground = BooleanVar(self)
	self.foreground.set(self.controls.foreground)
	self.daemon = StringVar(self)
	self.daemon.set(`self.controls.daemon`)
	self.syslog = BooleanVar(self)
	self.syslog.set(self.controls.syslog);
	self.logfile = StringVar(self)
	if self.controls.logfile: self.logfile.set(self.controls.logfile);

	gf = Frame(self, relief=RAISED, bd = 5)

	Label(gf,
		text='Fetchmail Run Controls', 
		bd=2).pack(side=TOP, pady=10)

	df = Frame(gf, relief=RAISED, bd=2)

	# Run in foreground?
	Checkbutton(df,
		{'text':'Run in foreground?',
		'variable':self.foreground,
		'relief':GROOVE}).pack(side=LEFT,anchor=W)

	# Set the poll interval
	de = LabeledEntry(df, '     Poll interval:', self.daemon, '14')
	de.pack(side=RIGHT, anchor=E)

	df.pack();

	sf = Frame(gf, relief=RAISED, bd=2)

	# Use syslog for logging?
	Checkbutton(sf,
		{'text':'Log to syslog?',
		'variable':self.syslog,
		'relief':GROOVE}).pack(side=LEFT, anchor=W)

	# Set the logfile
	log = LabeledEntry(sf, '     Logfile:', self.logfile, '14')
	log.pack(side=RIGHT, anchor=E)

	sf.pack(fill=X)
	gf.pack(fill=X)

    def GatherControls(self):
	self.controls.daemon = self.daemon.get()
	self.controls.foreground = self.foreground.get()
	self.controls.logfile = self.logfile.get()
	self.controls.syslog = self.syslog.get()

#
# Server editing stuff.
#
serverhelp = {
    'title' : 'Server options help',
    'banner': 'Server Options',
    'text' : """
The server options screen controls fetchmail 
options that apply to one of your mailservers.

Once you have a mailserver configuration set
up as you like it, you can select `Save' to
store it in the server list maintained in
the main configuration window.

If you wish to discard changes to a server 
configuration, select `Quit'.
"""}

controlhelp = {
    'title' : 'Run Control help',
    'banner': 'Run Controls',
    'text' : """
If the `Poll normally' checkbox is on, the host is polled as part of
the normal operation of fetchmail when it is run with no arguments.
If it is off, fetchmail will only query this host when it is given as
a command-line argument.

The `True name of server' box should specify the actual DNS name
to query. By default this is the same as the poll name.

Normally each host described in the file is queried once each 
poll cycle. If `Cycles to skip between polls' is greater than 0,
that's the number of poll cycles that are skipped between the
times this post is actually polled.

The `Server timeout' is the number of seconds fetchmail will wait
for a reply from the mailserver before concluding it is hung and
giving up.
"""}

protohelp = {
    'title' : 'Protocol and Port help',
    'banner': 'Protocol and Port',
    'text' : """
These options control the remote-mail protocol
and TCP/IP service port used to query this
server.

The `Protocol' button bar offers you a choice of
all the different protocols available.  The `auto'
protocol is a special mode that probes the host
ports for POP3 and IMAP to see if either is
available.

Normally the TCP/IP service port to use is 
dictated by the protocol choice.  The `Port'
field lets you set a non-standard port.
"""}

sechelp = {
    'title' : 'Security option help',
    'banner': 'Security',
    'text' : """
These options control the security procedure used
to protect mail transfer

Normally the mail fetch is validated using an 
ordinary password logon.  If your server speaks
MIT Kerberos IV it is possible to pre-authenticate
the exxchange with a Kerberos ticket.

The `interface' and `monitor' options are available
only for Linux systems.  See the fetchmail manual page
for details on these.
"""}

multihelp = {
    'title' : 'Multidrop option help',
    'banner': 'Multidrop',
    'text' : """
These options are only useful with multidrop mode.
See the manual page for extended discussion.
"""}

class ServerEdit(Frame):
    def __init__(self, host, sitelist, master=None):
	Frame.__init__(self, master)
	Pack.config(self)
	self.master.title('Fetchmail host ' + host);
	self.master.iconname('Fetchmail host ' + host);
	self.server = Server()
	self.server.pollname = host
	self.server.via = host
	self.sitelist = sitelist
	self.post()
	self.createWidgets(host)

    def post(self):
	# we can't abstract this away, execs would happen in the wrong scope
	for x in self.server.typemap:
	    target = "self." + x[0]
	    source = "self.server." + x[0]
	    if x[1] == 'Boolean':
		exec target + " = BooleanVar(self)"
		if eval(source):
		    exec target + ".set(" + source + ")"
	    elif x[1] == 'String':
		exec target + " = StringVar(self)"
		if eval(source):
		    exec target + ".set(" + source + ")"
	    elif x[1] == 'Int':
		exec target + " = IntVar(self)"
		if eval(source):
		    exec target + ".set(" + source + ")"

    def gather(self):
	for x in self.server.typemap:
	    setattr(self.server, x[0], getattr(self, x[0]).get())

    def nosave(self):
	if ConfirmQuit(self, 'server option editing'):
	    Widget.destroy(self.master)

    def save(self):
	self.gather()
	self.sitelist.append(self.server) 
	Widget.destroy(self.master)

    def refreshPort(self):
	proto = self.protocol.get()
	self.port.set(defaultports[proto])
	if not proto in ("POP3", "APOP", "KPOP"): self.uidl = FALSE

    def createWidgets(self, host):
	topwin = Frame(self, relief=RAISED, bd=5)
	Label(topwin, text="Server options for " + host).pack(side=TOP,pady=10)
	Button(topwin, text='Save', fg='blue',
		command=self.save).pack(side=LEFT)
	Button(topwin, text='Quit', fg='blue',
		command=self.nosave).pack(side=LEFT)
	Button(topwin, text='Help', fg='blue',
	       command=lambda: helpwin(serverhelp)).pack(side=RIGHT)
	topwin.pack(fill=X)

	ctlwin = Frame(self, relief=RAISED, bd=5)
	Label(ctlwin, text="Run Controls").pack(side=TOP)
	Checkbutton(ctlwin, text='Poll ' + host + ' normally?', variable=self.active).pack(side=TOP)
	LabeledEntry(ctlwin, 'True name of ' + host + ':',
		      self.via, '30').pack(side=TOP, fill=X)
	LabeledEntry(ctlwin, 'Cycles to skip between polls:',
		      self.interval, '30').pack(side=TOP, fill=X)
	LabeledEntry(ctlwin, 'Server timeout (seconds):',
		      self.timeout, '30').pack(side=TOP, fill=X)
	Button(ctlwin, text='Help', fg='blue',
	       command=lambda: helpwin(controlhelp)).pack(side=RIGHT)
	ctlwin.pack(fill=X)

	protwin = Frame(self, relief=RAISED, bd=5)
	Label(protwin, text="Protocol and Port").pack(side=TOP)
	pb = ButtonBar(protwin, 'Protocol:', self.protocol, protolist, self.refreshPort) 
	LabeledEntry(protwin, 'TCP/IP service port to query:',
		      self.port, '30').pack(side=TOP, fill=X)
	Checkbutton(protwin,
		text="Track seen POP3 messages with client-side UIDL list?",
		variable=self.uidl).pack(side=TOP)   
	Button(protwin, text='Help', fg='blue',
	       command=lambda: helpwin(protohelp)).pack(side=RIGHT)
	protwin.pack(fill=X)

	secwin = Frame(self, relief=RAISED, bd=5)
	Label(secwin, text="Security").pack(side=TOP)
	ButtonBar(secwin, 'Authorization mode:',
		  self.auth, authlist, None).pack(side=TOP)

	if os.popen("uname").readlines()[0] == 'Linux\n':
	    LabeledEntry(secwin, 'Interface to check before polling:',
			 self.interface, '30').pack(side=TOP, fill=X)
	    LabeledEntry(secwin, 'IP addresses to watch for activity:',
			 self.monitor, '30').pack(side=TOP, fill=X)

	Button(secwin, text='Help', fg='blue',
	       command=lambda: helpwin(sechelp)).pack(side=RIGHT)
	secwin.pack(fill=X)

	mdropwin = Frame(self, relief=RAISED, bd=5)
	Label(mdropwin, text="Multidrop options").pack(side=TOP)
	LabeledEntry(mdropwin, 'Envelope address header:',
		      self.envelope, '30').pack(side=TOP, fill=X)
	Checkbutton(mdropwin, text="Enable multidrop DNS lookup?",
		    variable=self.dns).pack(side=TOP)
	Label(mdropwin, text="DNS aliases").pack(side=TOP)
	ListEdit("New site alias: ", self.server.aka, None, mdropwin)
	Label(mdropwin, text="Domains to be considered local").pack(side=TOP)
	ListEdit("New local domain: ", self.server.localdomains, None,mdropwin)
	Button(mdropwin, text='Help', fg='blue',
	       command=lambda: helpwin(multihelp)).pack(side=RIGHT)
	mdropwin.pack(fill=X)

	userwin = Frame(self, relief=RAISED, bd=5)
	Label(userwin, text="User entries for " + host).pack(side=TOP)
	ListEdit("New user: ", None, self.edituser, userwin)
	userwin.pack(fill=X)

    def edituser(self, user):
	UserEdit(user, self.server.userlist, Toplevel())

#
# User editing stuff
#

userhelp = {
    'title' : 'User option help',
    'banner': 'User options',
    'text' : """
FIXME
"""}

class UserEdit(Frame):
    def __init__(self, user, userlist, master=None):
	Frame.__init__(self, master)
	Pack.config(self)
	self.master.title('Fetchmail user ' + user);
	self.master.iconname('Fetchmail user ' + user);
	self.user = User()
	self.user.remote = user
	self.user.localnames = [user]
	self.userlist = userlist
	self.post()
	self.createWidgets(user)

    def post(self):
	# we can't abstract this away, execs would happen in the wrong scope
	for x in self.user.typemap:
	    target = "self." + x[0]
	    source = "self.user." + x[0]
	    if x[1] == 'Boolean':
		exec target + " = BooleanVar(self)"
		if eval(source):
		    exec target + ".set(" + source + ")"
	    elif x[1] == 'String':
		exec target + " = StringVar(self)"
		if eval(source):
		    exec target + ".set(" + source + ")"
	    elif x[1] == 'Int':
		exec target + " = IntVar(self)"
		if eval(source):
		    exec target + ".set(" + source + ")"

    def gather(self):
	for x in self.user.typemap:
	    setattr(self.user, x[0], getattr(self, x[0]).get())

    def nosave(self):
	if ConfirmQuit(self, 'user option editing'):
	    Widget.destroy(self.master)

    def save(self):
	self.gather()
	self.userlist.append(self.user) 
	Widget.destroy(self.master)

    def createWidgets(self):
	topwin = Frame(self, relief=RAISED, bd=5)
	Label(topwin, 
	      text="User options for " + self.user.remote).pack(side=TOP,pady=10)
	Button(topwin, text='Save', fg='blue',
		command=self.save).pack(side=LEFT)
	Button(topwin, text='Quit', fg='blue',
		command=self.nosave).pack(side=LEFT)
	Button(topwin, text='Help', fg='blue',
	       command=lambda: helpwin(userhelp)).pack(side=RIGHT)
	topwin.pack(fill=X)

	secwin = Frame(self, relief=RAISED, bd=5)
	Label(secwin, text="Authentication").pack(side=TOP)
	LabeledEntry(mdropwin, 'Password:',
		      self.password, '30').pack(side=TOP, fill=X)
	LabeledEntry(mdropwin, 'Remote folder:',
		     self.folder, '30').pack(side=TOP, fill=X)
	secwin.pack(fill=X)

	names = Frame(self, relief=RAISED, bd=5)
	Label(names, text="Local names").pack(side=TOP)
	ListEdit("New local name: ", self.localnames, None, names)
	names.pack(fill=X)

	targwin = Frame(self, relief=RAISED, bd=5)
	Label(targwin, text="Forwarding Options").pack(side=TOP)
	LabeledEntry(targwin, 'System to forward to:',
		     self.smtphost, '30').pack(side=TOP, fill=X)
	LabeledEntry(targwin, 'Connection setup command:',
		     self.preconnect, '30').pack(side=TOP, fill=X)
	LabeledEntry(targwin, 'Connection wrapup command:',
		     self.postconnect, '30').pack(side=TOP, fill=X)
	LabeledEntry(targwin, 'Local delivery agent:',
		     self.mda, '30').pack(side=TOP, fill=X)
	targwin.pack(fill=X)

	optwin = Frame(self, relief=RAISED, bd=5)
	Checkbutton(optwin, "Suppress deletion of messages after reading",
		    self.keep)
	Checkbutton(optwin, "Flush seen messages before retrieval", 
		    self.flush)
	Checkbutton(optwin, "Fetch old messages as well as new",
		    self.fetchall)
	Checkbutton(optwin, "Rewrite To/Cc/Bcc messages to enable reply", 
		    self.rewrite)
	Checkbutton(optwin, "Force CR/LF at end of each line",
		    self.forcecr)
	Checkbutton(optwin, "Strip CR from end of eacgh line",
		    self.stripcr)
	Checkbutton(optwin, "Pass 8 bits even theough SMTP says 7BIT",
		    self.pass8bits)
	Checkbutton(optwin, "Drop Status lines from forwarded messages", 
		    self.dropstatus)
	optwin.pack(fill=X)

	limwin = Frame(self, relief=RAISED, bd=5)
	Label(limwin, text="Resource Limits").pack(side=TOP)
	LabeledEntry(limwin, 'Message size limit:',
		      self.limit, '30').pack(side=TOP, fill=X)
	LabeledEntry(limwin, 'Maximum messages to fetch each poll:',
		      self.fetchlimit, '30').pack(side=TOP, fill=X)
	LabeledEntry(limwin, 'Maximum messages to forward each poll:',
		      self.batchlimit, '30').pack(side=TOP, fill=X)
	limwin.pack(fill=X)

#
# Configure drives the configuration dialogue.  It may call multiple
# instances of ServerEdit to do its job.
#

class Configure(Frame, ControlEdit):
    def __init__(self, master=None):
	Frame.__init__(self, master)
	self.master.title('fetchmail configurator');
	self.master.iconname('fetchmail configurator');
	Pack.config(self)
	self.MakeDispose()
	self.controls = Controls()
	self.PostControls()
	self.MakeSitelist(master)
	self.sites = []

    def MakeDispose(self):
	# Set the disposal of the given configuration
	dispose = Frame(self, relief=RAISED, bd=5);
	Label(dispose,
		text='Configurator Controls', 
		bd=2).pack(side=TOP, pady=10)
	Button(dispose, text='Save', fg='blue',
		command=self.save).pack(side=LEFT)
	Button(dispose, text='Quit', fg='blue',
		command=self.nosave).pack(side=LEFT)
	Button(dispose, text='Help', fg='blue',
	       command=lambda: helpwin(confighelp)).pack(side=RIGHT)
	dispose.pack(side=TOP, fill=X);

    def MakeSitelist(self, master):
	lf = Frame(master, relief=RAISED, bd=5)
	Label(lf,
	      text='Remote Mail Server Configurations', 
	      bd=2).pack(side=TOP, pady=10)
	ListEdit('New Server:', None, self.editsite, lf)
	lf.pack(fill=X)

    def editsite(self, site):
	ServerEdit(site, self.sites, Toplevel())

    def save(self):
	self.GatherControls()
	sys.stdout.write("# Configuration created %s\n" % time.ctime(time.time()))
	sys.stdout.write(`self.controls`)
	for site in self.sites:
	    sys.stdout.write(`site`)
	    for user in self.sites.userlist:
		sys.stdout.write(`user`)
	self.quit()

    def nosave(self):
	if ConfirmQuit(self, "configuration editor"):
	    self.quit()

if __name__ == '__main__': 
    ServerDefaults = Server()
    UserDefaults = User()
    Configure().mainloop()

# The following sets edit modes for GNU EMACS
# Local Variables:
# mode:python
# End:
