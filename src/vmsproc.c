/* Interfaces to subprocesses on VMS.
   Copyright (C) 1988, 1994 Free Software Foundation, Inc.

This file is part of GNU Emacs.

GNU Emacs is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs; see the file COPYING.  If not, write to
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */


/*
    Event flag and `select' emulation

    0 is never used
    1 is the terminal
    23 is the timer event flag
    24-31 are reserved by VMS
*/
#include <config.h>
#include	<ssdef.h>
#include	<iodef.h>
#include	<dvidef.h>
#include	<clidef.h>
#include	"vmsproc.h"
#include	"lisp.h"
#include	"buffer.h"
#include	<file.h>
#include	"process.h"
#include	"commands.h"
#include	<errno.h>
extern Lisp_Object call_process_cleanup ();


#define		KEYBOARD_EVENT_FLAG		1
#define		TIMER_EVENT_FLAG		23

static VMS_PROC_STUFF	procList[MAX_EVENT_FLAGS+1];

get_kbd_event_flag ()
{
  /*
    Return the first event flag for keyboard input.
    */
  VMS_PROC_STUFF	*vs = &procList[KEYBOARD_EVENT_FLAG];

  vs->busy = 1;
  vs->pid = 0;
  return (vs->eventFlag);
}

get_timer_event_flag ()
{
  /*
    Return the last event flag for use by timeouts
    */
  VMS_PROC_STUFF	*vs = &procList[TIMER_EVENT_FLAG];

  vs->busy = 1;
  vs->pid = 0;
  return (vs->eventFlag);
}

VMS_PROC_STUFF *
get_vms_process_stuff ()
{
  /*
    Return a process_stuff structure
    
    We use 1-23 as our event flags to simplify implementing
    a VMS `select' call. 
    */
  int i;
  VMS_PROC_STUFF *vs;

  for (i=1, vs = procList; i<MAX_EVENT_FLAGS; i++, vs++)
    {
      if (!vs->busy)
	{
	  vs->busy = 1;
	  vs->inputChan = 0;
	  vs->pid = 0;
	  sys$clref (vs->eventFlag);
	  return (vs);
	}
    }
  return ((VMS_PROC_STUFF *)0);
}

give_back_vms_process_stuff (vs)
     VMS_PROC_STUFF *vs;
{
  /*
    Return an event flag to our pool
    */
  vs->busy = 0;
  vs->inputChan = 0;
  vs->pid = 0;
}

VMS_PROC_STUFF *
get_vms_process_pointer (pid)
     int pid;
{
  /*
    Given a pid, return the VMS_STUFF pointer
    */
  int			i;
  VMS_PROC_STUFF	*vs;

  /* Don't search the last one */
  for (i=0, vs=procList; i<MAX_EVENT_FLAGS; i++, vs++)
    {
      if (vs->busy && vs->pid == pid)
	return (vs);
    }
  return ((VMS_PROC_STUFF *)0);
}

start_vms_process_read (vs)
     VMS_PROC_STUFF *vs;
{
  /*
    Start an asynchronous  read on a VMS process
    We will catch up with the output sooner or later
    */
  int			status;
  int			ProcAst ();

  status = sys$qio (vs->eventFlag, vs->outputChan, IO$_READVBLK,
		   vs->iosb, 0, vs,
		   vs->inputBuffer, sizeof (vs->inputBuffer), 0, 0, 0, 0);
  if (status != SS$_NORMAL)
    return (0);
  else
    return (1);
}

extern int	waiting_for_ast;		/* in sysdep.c */
extern int	timer_ef;
extern int	input_ef;

select (nDesc, rdsc, wdsc, edsc, timeOut)
     int nDesc;
     int *rdsc;
     int *wdsc;
     int *edsc;
     int *timeOut;
{
  /* Emulate a select call
     
     We know that we only use event flags 1-23
     
     timeout == 100000 & bit 0 set means wait on keyboard input until
     something shows up.  If timeout == 0, we just read the event
     flags and return what we find.  */

  int nfds = 0;
  int status;
  int time[2];
  int delta = -10000000;
  int zero = 0;
  int timeout = *timeOut;
  unsigned long	mask, readMask, waitMask;

  if (rdsc)
    readMask = *rdsc << 1;	/* Unix mask is shifted over 1 */
  else
    readMask = 0;		/* Must be a wait call */

  sys$clref (KEYBOARD_EVENT_FLAG);
  sys$setast (0);		/* Block interrupts */
  sys$readef (KEYBOARD_EVENT_FLAG, &mask); /* See what is set */
  mask &= readMask;		/* Just examine what we need */
  if (mask == 0)
    {		/* Nothing set, we must wait */
      if (timeout != 0)
	{	/* Not just inspecting... */
	  if (!(timeout == 100000 &&
		readMask == (1 << KEYBOARD_EVENT_FLAG)))
	    {
	      lib$emul (&timeout, &delta, &zero, time);
	      sys$setimr (TIMER_EVENT_FLAG, time, 0, 1);
	      waitMask = readMask | (1 << TIMER_EVENT_FLAG);
	    }
	  else
	    waitMask = readMask;
	  if (waitMask & (1 << KEYBOARD_EVENT_FLAG))
	    {
	      sys$clref (KEYBOARD_EVENT_FLAG);
	      waiting_for_ast = 1; /* Only if reading from 0 */
	    }
	  sys$setast (1);
	  sys$wflor (KEYBOARD_EVENT_FLAG, waitMask);
	  sys$cantim (1, 0);
	  sys$readef (KEYBOARD_EVENT_FLAG, &mask);
	  if (readMask & (1 << KEYBOARD_EVENT_FLAG))
	    waiting_for_ast = 0;
	}
    }
  sys$setast (1);

  /*
    Count number of descriptors that are ready
    */
  mask &= readMask;
  if (rdsc)
    *rdsc = (mask >> 1);	/* Back to Unix format */
  for (nfds = 0; mask; mask >>= 1)
    {
      if (mask & 1)
	nfds++;
    }
  return (nfds);
}

#define	MAX_BUFF	1024

write_to_vms_process (vs, buf, len)
     VMS_PROC_STUFF *vs;
     char *buf;
     int len;
{
  /*
    Write something to a VMS process.
    
    We have to map newlines to carriage returns for VMS.
    */
  char		ourBuff[MAX_BUFF];
  short		iosb[4];
  int			status;
  int			in, out;

  while (len > 0)
    {
      out = map_nl_to_cr (buf, ourBuff, len, MAX_BUFF);
      status = sys$qiow (0, vs->inputChan, IO$_WRITEVBLK|IO$M_NOFORMAT,
			iosb, 0, 0, ourBuff, out, 0, 0, 0, 0);
      if (status != SS$_NORMAL || (status = iosb[0]) != SS$_NORMAL)
	{
	  error ("Could not write to subprocess: %x", status);
	  return (0);
	}
      len -= out;
    }
  return (1);
}

static
map_nl_to_cr (in, out, maxIn, maxOut)
     char *in;
     char *out;
     int maxIn;
     int maxOut;
{
  /*
    Copy `in' to `out' remapping `\n' to `\r'
    */
  int			c;
  int			o;

  for (o=0; maxIn-- > 0 && o < maxOut; o++)
    {
      c = *in++;
      *out++ = (c == '\n') ? '\r' : c;
    }
  return (o);
}

clean_vms_buffer (buf, len)
     char *buf;
     int len;
{
  /*
    Sanitize output from a VMS subprocess
    Strip CR's and NULLs
    */
  char		*oBuf = buf;
  char		c;
  int			l = 0;

  while (len-- > 0)
    {
      c = *buf++;
      if (c == '\r' || c == '\0')
	;
      else
	{
	  *oBuf++ = c;
	  l++;
	}
    }
  return (l);
}

/*
    For the CMU PTY driver
*/
#define		PTYNAME		"PYA0:"

get_pty_channel (inDevName, outDevName, inChannel, outChannel)
     char *inDevName;
     char *outDevName;
     int *inChannel;
     int *outChannel;
{
  int			PartnerUnitNumber;
  int			status;
  struct {
    int	l;
    char	*a;
  } d;
  struct {
    short	BufLen;
    short	ItemCode;
    int	*BufAddress;
    int	*ItemLength;
  } g[2];
    
  d.l = strlen (PTYNAME);
  d.a = PTYNAME;
  *inChannel = 0;		/* Should be `short' on VMS */
  *outChannel = 0;
  *inDevName = *outDevName = '\0';
  status  = sys$assign (&d, inChannel, 0, 0);
  if (status == SS$_NORMAL)
    {
      *outChannel = *inChannel;
      g[0].BufLen = sizeof (PartnerUnitNumber);
      g[0].ItemCode = DVI$_UNIT;
      g[0].BufAddress = &PartnerUnitNumber;
      g[0].ItemLength = (int *)0;
      g[1].BufLen = g[1].ItemCode = 0;
      status = sys$getdviw (0, *inChannel, 0, &g, 0, 0, 0, 0);
      if (status == SS$_NORMAL)
	{
	  sprintf (inDevName, "_TPA%d:", PartnerUnitNumber);
	  strcpy (outDevName, inDevName);
	}
    }
  return (status);
}

VMSgetwd (buf)
     char *buf;
{
  /*
    Return the current directory
    */
  char curdir[256];
  char *getenv ();
  char *s;
  short len;
  int status;
  struct
    {
      int	l;
      char	*a;
    } d;

  s = getenv ("SYS$DISK");
  if (s)
    strcpy (buf, s);
  else
    *buf = '\0';

  d.l = 255;
  d.a = curdir;
  status = sys$setddir (0, &len, &d);
  if (status & 1)
    {
      curdir[len] = '\0';
      strcat (buf, curdir);
    }
}

static
call_process_ast (vs)
     VMS_PROC_STUFF *vs;
{
  sys$setef (vs->eventFlag);
}

void
child_setup (in, out, err, new_argv, env)
     int in, out, err;
     register char **new_argv;
     char **env;
{
  /* ??? I suspect that maybe this shouldn't be done on VMS.  */
#ifdef subprocesses
  /* Close Emacs's descriptors that this process should not have.  */
  close_process_descs ();
#endif

  if (STRINGP (current_buffer->directory))
    chdir (XSTRING (current_buffer->directory)->data);
}

DEFUN ("call-process", Fcall_process, Scall_process, 1, MANY, 0,
  "Call PROGRAM synchronously in a separate process.\n\
Program's input comes from file INFILE (nil means null device, `NLA0:').\n\
Insert output in BUFFER before point; t means current buffer;\n\
 nil for BUFFER means discard it; 0 means discard and don't wait.\n\
Fourth arg DISPLAY non-nil means redisplay buffer as output is inserted.\n\
Remaining arguments are strings passed as command arguments to PROGRAM.\n\
This function waits for PROGRAM to terminate, unless BUFFER is 0;\n\
if you quit, the process is killed.")
  (nargs, args)
     int nargs;
     register Lisp_Object *args;
{
  Lisp_Object display, buffer, path;
  char oldDir[512];
  int inchannel, outchannel;
  int len;
  int call_process_ast ();
  struct
    {
      int l;
      char *a;
    } dcmd, din, dout;
  char inDevName[65];
  char outDevName[65];
  short iosb[4];
  int status;
  int SpawnFlags = CLI$M_NOWAIT;
  VMS_PROC_STUFF *vs;
  VMS_PROC_STUFF *get_vms_process_stuff ();
  int fd[2];
  int filefd;
  register int pid;
  char buf[1024];
  int count = specpdl_ptr - specpdl;
  register unsigned char **new_argv;
  struct buffer *old = current_buffer;

  CHECK_STRING (args[0], 0);

  if (nargs <= 1 || NILP (args[1]))
    args[1] = build_string ("NLA0:");
  else
    args[1] = Fexpand_file_name (args[1], current_buffer->directory);

  CHECK_STRING (args[1], 1);

  {
    register Lisp_Object tem;
    buffer = tem = args[2];
    if (nargs <= 2)
      buffer = Qnil;
    else if (!(EQ (tem, Qnil) || EQ (tem, Qt)
	       || XFASTINT (tem) == 0))
      {
	buffer = Fget_buffer (tem);
	CHECK_BUFFER (buffer, 2);
      }
  }

  display = nargs >= 3 ? args[3] : Qnil;

  {
    /*
	if (args[0] == "*dcl*" then we need to skip pas the "-c",
	else args[0] is the program to run.
    */
    register int i;
    int arg0;
    int firstArg;

    if (strcmp (XSTRING (args[0])->data, "*dcl*") == 0)
      {
	arg0 = 5;
	firstArg = 6;
      }
    else
      {
	arg0 = 0;
	firstArg = 4;
      }
    len = XSTRING (args[arg0])->size + 1;
    for (i = firstArg; i < nargs; i++)
      {
	CHECK_STRING (args[i], i);
	len += XSTRING (args[i])->size + 1;
      }
    new_argv = alloca (len);
    strcpy (new_argv, XSTRING (args[arg0])->data);
    for (i = firstArg; i < nargs; i++)
      {
	strcat (new_argv, " ");
	strcat (new_argv, XSTRING (args[i])->data);
      }
    dcmd.l = len-1;
    dcmd.a = new_argv;
    
    status = get_pty_channel (inDevName, outDevName, &inchannel, &outchannel);
    if (!(status & 1))
      error ("Error getting PTY channel: %x", status);
    if (INTEGERP (buffer))
      {
	dout.l = strlen ("NLA0:");
	dout.a = "NLA0:";
      }
    else
      {
	dout.l = strlen (outDevName);
	dout.a = outDevName;
      }

    vs = get_vms_process_stuff ();
    if (!vs)
      {
	sys$dassgn (inchannel);
	sys$dassgn (outchannel);
	error ("Too many VMS processes");
      }
    vs->inputChan = inchannel;
    vs->outputChan = outchannel;
  }

  filefd = open (XSTRING (args[1])->data, O_RDONLY, 0);
  if (filefd < 0)
    {
      sys$dassgn (inchannel);
      sys$dassgn (outchannel);
      give_back_vms_process_stuff (vs);
      report_file_error ("Opening process input file", Fcons (args[1], Qnil));
    }
  else
    close (filefd);

  din.l = XSTRING (args[1])->size;
  din.a = XSTRING (args[1])->data;

  /*
      Start a read on the process channel
  */
  if (!INTEGERP (buffer))
    {
      start_vms_process_read (vs);
      SpawnFlags = CLI$M_NOWAIT;
    }
  else
    SpawnFlags = 0;

  /*
      On VMS we need to change the current directory
      of the parent process before forking so that
      the child inherit that directory.  We remember
      where we were before changing.
  */
  VMSgetwd (oldDir);
  child_setup (0, 0, 0, 0, 0);
  status = lib$spawn (&dcmd, &din, &dout, &SpawnFlags, 0, &vs->pid,
	      &vs->exitStatus, 0, call_process_ast, vs);
  chdir (oldDir);

  if (status != SS$_NORMAL)
    {
      sys$dassgn (inchannel);
      sys$dassgn (outchannel);
      give_back_vms_process_stuff (vs);
      error ("Error calling LIB$SPAWN: %x", status);
    }
  pid = vs->pid;

  if (INTEGERP (buffer))
    {
#ifndef subprocesses
      wait_without_blocking ();
#endif subprocesses
      return Qnil;
    }

  if (!NILP (display) && INTERACTIVE)
    prepare_menu_bars ();

  record_unwind_protect (call_process_cleanup,
			 Fcons (make_number (fd[0]), make_number (pid)));


  if (BUFFERP (buffer))
    Fset_buffer (buffer);

  immediate_quit = 1;
  QUIT;

  while (1)
    {
      sys$waitfr (vs->eventFlag);
      if (vs->iosb[0] & 1)
	{
	  immediate_quit = 0;
	  if (!NILP (buffer))
	    {
	      vs->iosb[1] = clean_vms_buffer (vs->inputBuffer, vs->iosb[1]);
	      InsCStr (vs->inputBuffer, vs->iosb[1]);
	    }
	  if (!NILP (display) && INTERACTIVE)
	  redisplay_preserve_echo_area ();
	  immediate_quit = 1;
	  QUIT;
	  if (!start_vms_process_read (vs))
	    break;		/* The other side went away */
	}
      else
	break;
    }

  sys$dassgn (inchannel);
  sys$dassgn (outchannel);
  give_back_vms_process_stuff (vs);

  /* Wait for it to terminate, unless it already has.  */
  wait_for_termination (pid);

  immediate_quit = 0;

  set_current_buffer (old);

  return unbind_to (count, Qnil);
}

create_process (process, new_argv)
     Lisp_Object process;
     char *new_argv;
{
  int pid, inchannel, outchannel, forkin, forkout;
  char old_dir[512];
  char in_dev_name[65];
  char out_dev_name[65];
  short iosb[4];
  int status;
  int spawn_flags = CLI$M_NOWAIT;
  int child_sig ();
  struct {
    int l;
    char *a;
  } din, dout, dprompt, dcmd;
  VMS_PROC_STUFF *vs;
  VMS_PROC_STUFF *get_vms_process_stuff ();
    
  status = get_pty_channel (in_dev_name, out_dev_name, &inchannel, &outchannel);
  if (!(status & 1))
    {
      remove_process (process);
      error ("Error getting PTY channel: %x", status);
    }
  dout.l = strlen (out_dev_name);
  dout.a = out_dev_name;
  dprompt.l = strlen (DCL_PROMPT);
  dprompt.a = DCL_PROMPT;

  if (strcmp (new_argv, "*dcl*") == 0)
    {
      din.l = strlen (in_dev_name);
      din.a = in_dev_name;
      dcmd.l = 0;
      dcmd.a = (char *)0;
    }
  else
    {
      din.l = strlen ("NLA0:");
      din.a = "NLA0:";
      dcmd.l = strlen (new_argv);
      dcmd.a = new_argv;
    }

  /* Delay interrupts until we have a chance to store
     the new fork's pid in its process structure */
  sys$setast (0);

  vs = get_vms_process_stuff ();
  if (vs == 0)
    {
      sys$setast (1);
      remove_process (process);
      error ("Too many VMS processes");
    }
  vs->inputChan = inchannel;
  vs->outputChan = outchannel;

  /* Start a read on the process channel */
  start_vms_process_read (vs);

  /* Switch current directory so that the child inherits it. */
  VMSgetwd (old_dir);
  child_setup (0, 0, 0, 0, 0);

  status = lib$spawn (&dcmd, &din, &dout, &spawn_flags, 0, &vs->pid,
		      &vs->exitStatus, 0, child_sig, vs, &dprompt);
  chdir (old_dir);

  if (status != SS$_NORMAL)
    {
      sys$setast (1);
      remove_process (process);
      error ("Error calling LIB$SPAWN: %x", status);
    }
  vs->pid &= 0xffff;		/* It needs to fit in a FASTINT,
				   we don't need the rest of the bits */
  pid = vs->pid;

  /*
    ON VMS process->infd holds the (event flag-1)
    that we use for doing I/O on that process.
    `input_wait_mask' is the cluster of event flags
    we can wait on.
    
    Event flags returned start at 1 for the keyboard.
    Since Unix expects descriptor 0 for the keyboard,
    we substract one from the event flag.
    */
  inchannel = vs->eventFlag-1;

  /* Record this as an active process, with its channels.
     As a result, child_setup will close Emacs's side of the pipes.  */
  chan_process[inchannel] = process;
  XSETFASTINT (XPROCESS (process)->infd, inchannel);
  XSETFASTINT (XPROCESS (process)->outfd, outchannel);
  XPROCESS (process)->status = Qrun

  /* Delay interrupts until we have a chance to store
     the new fork's pid in its process structure */

#define	NO_ECHO		"set term/noecho\r"
  sys$setast (0);
  /*
    Send a command to the process to not echo input
    
    The CMU PTY driver does not support SETMODEs.
    */
  write_to_vms_process (vs, NO_ECHO, strlen (NO_ECHO));

  XSETFASTINT (XPROCESS (process)->pid, pid);
  sys$setast (1);
}

child_sig (vs)
     VMS_PROC_STUFF *vs;
{
  register int pid;
  Lisp_Object tail, proc;
  register struct Lisp_Process *p;
  int old_errno = errno;

  pid = vs->pid;
  sys$setef (vs->eventFlag);

  for (tail = Vprocess_alist; XSYMBOL (tail) != XSYMBOL (Qnil); tail = XCONS (tail)->cdr)
    {
      proc = XCONS (XCONS (tail)->car)->cdr;
      p = XPROCESS (proc);
      if (EQ (p->childp, Qt) && XFASTINT (p->pid) == pid)
	break;
    }

  if (XSYMBOL (tail) == XSYMBOL (Qnil))
    return;

  p->status = Fcons (Qexit, Fcons (make_number (vs->exitStatus), Qnil))
}

syms_of_vmsproc ()
{
  defsubr (&Scall_process);
}

init_vmsproc ()
{
  char *malloc ();
  int i;
  VMS_PROC_STUFF *vs;

  for (vs=procList, i=0; i<MAX_EVENT_FLAGS+1; i++, vs++)
    {
      vs->busy = 0;
      vs->eventFlag = i;
      sys$clref (i);
      vs->inputChan = 0;
      vs->pid = 0;
    }
  procList[0].busy = 1;		/* Zero is reserved */
}
