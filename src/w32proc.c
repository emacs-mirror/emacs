/* Process support for Windows NT port of GNU EMACS.
   Copyright (C) 1992, 1995 Free Software Foundation, Inc.

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
the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.

   Drew Bliss                   Oct 14, 1993
     Adapted from alarm.c by Tim Fleehart
*/

#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <io.h>
#include <fcntl.h>
#include <signal.h>

/* must include CRT headers *before* config.h */
#include "config.h"
#undef signal
#undef wait
#undef spawnve
#undef select
#undef kill

#include <windows.h>

#include "lisp.h"
#include "nt.h"
#include "systime.h"
#include "syswait.h"
#include "process.h"

/* Control whether spawnve quotes arguments as necessary to ensure
   correct parsing by child process.  Because not all uses of spawnve
   are careful about constructing argv arrays, we make this behaviour
   conditional (off by default). */
Lisp_Object Vwin32_quote_process_args;

/* Time to sleep before reading from a subprocess output pipe - this
   avoids the inefficiency of frequently reading small amounts of data.
   This is primarily necessary for handling DOS processes on Windows 95,
   but is useful for Win32 processes on both Win95 and NT as well.  */
Lisp_Object Vwin32_pipe_read_delay;

/* Control conversion of upper case file names to lower case.
   nil means no, t means yes. */
Lisp_Object Vwin32_downcase_file_names;

/* Keep track of whether we have already started a DOS program. */
BOOL dos_process_running;

#ifndef SYS_SIGLIST_DECLARED
extern char *sys_siglist[];
#endif

#ifdef EMACSDEBUG
void _DebPrint (const char *fmt, ...)
{
  char buf[1024];
  va_list args;

  va_start (args, fmt);
  vsprintf (buf, fmt, args);
  va_end (args);
  OutputDebugString (buf);
}
#endif

typedef void (_CALLBACK_ *signal_handler)(int);

/* Signal handlers...SIG_DFL == 0 so this is initialized correctly.  */
static signal_handler sig_handlers[NSIG];

/* Fake signal implementation to record the SIGCHLD handler.  */
signal_handler 
sys_signal (int sig, signal_handler handler)
{
  signal_handler old;
  
  if (sig != SIGCHLD)
    {
      errno = EINVAL;
      return SIG_ERR;
    }
  old = sig_handlers[sig];
  sig_handlers[sig] = handler;
  return old;
}

/* Defined in <process.h> which conflicts with the local copy */
#define _P_NOWAIT 1

/* Child process management list.  */
int child_proc_count = 0;
child_process child_procs[ MAX_CHILDREN ];
child_process *dead_child = NULL;

DWORD WINAPI reader_thread (void *arg);

/* Find an unused process slot.  */
child_process *
new_child (void)
{
  child_process *cp;
  DWORD id;
  
  for (cp = child_procs+(child_proc_count-1); cp >= child_procs; cp--)
    if (!CHILD_ACTIVE (cp))
      goto Initialise;
  if (child_proc_count == MAX_CHILDREN)
    return NULL;
  cp = &child_procs[child_proc_count++];

 Initialise:
  memset (cp, 0, sizeof(*cp));
  cp->fd = -1;
  cp->pid = -1;
  cp->procinfo.hProcess = NULL;
  cp->status = STATUS_READ_ERROR;

  /* use manual reset event so that select() will function properly */
  cp->char_avail = CreateEvent (NULL, TRUE, FALSE, NULL);
  if (cp->char_avail)
    {
      cp->char_consumed = CreateEvent (NULL, FALSE, FALSE, NULL);
      if (cp->char_consumed)
        {
	  cp->thrd = CreateThread (NULL, 1024, reader_thread, cp, 0, &id);
	  if (cp->thrd)
	    return cp;
	}
    }
  delete_child (cp);
  return NULL;
}

void 
delete_child (child_process *cp)
{
  int i;

  /* Should not be deleting a child that is still needed. */
  for (i = 0; i < MAXDESC; i++)
    if (fd_info[i].cp == cp)
      abort ();

  if (!CHILD_ACTIVE (cp))
    return;

  /* reap thread if necessary */
  if (cp->thrd)
    {
      DWORD rc;

      if (GetExitCodeThread (cp->thrd, &rc) && rc == STILL_ACTIVE)
        {
	  /* let the thread exit cleanly if possible */
	  cp->status = STATUS_READ_ERROR;
	  SetEvent (cp->char_consumed);
	  if (WaitForSingleObject (cp->thrd, 1000) != WAIT_OBJECT_0)
	    {
	      DebPrint (("delete_child.WaitForSingleObject (thread) failed "
			 "with %lu for fd %ld\n", GetLastError (), cp->fd));
	      TerminateThread (cp->thrd, 0);
	    }
	}
      CloseHandle (cp->thrd);
      cp->thrd = NULL;
    }
  if (cp->char_avail)
    {
      CloseHandle (cp->char_avail);
      cp->char_avail = NULL;
    }
  if (cp->char_consumed)
    {
      CloseHandle (cp->char_consumed);
      cp->char_consumed = NULL;
    }

  /* update child_proc_count (highest numbered slot in use plus one) */
  if (cp == child_procs + child_proc_count - 1)
    {
      for (i = child_proc_count-1; i >= 0; i--)
	if (CHILD_ACTIVE (&child_procs[i]))
	  {
	    child_proc_count = i + 1;
	    break;
	  }
    }
  if (i < 0)
    child_proc_count = 0;
}

/* Find a child by pid.  */
static child_process *
find_child_pid (DWORD pid)
{
  child_process *cp;

  for (cp = child_procs+(child_proc_count-1); cp >= child_procs; cp--)
    if (CHILD_ACTIVE (cp) && pid == cp->pid)
      return cp;
  return NULL;
}


/* Thread proc for child process and socket reader threads. Each thread
   is normally blocked until woken by select() to check for input by
   reading one char.  When the read completes, char_avail is signalled
   to wake up the select emulator and the thread blocks itself again. */
DWORD WINAPI 
reader_thread (void *arg)
{
  child_process *cp;
  
  /* Our identity */
  cp = (child_process *)arg;
  
  /* We have to wait for the go-ahead before we can start */
  if (cp == NULL ||
      WaitForSingleObject (cp->char_consumed, INFINITE) != WAIT_OBJECT_0)
    return 1;

  for (;;)
    {
      int rc;

      rc = _sys_read_ahead (cp->fd);

      /* The name char_avail is a misnomer - it really just means the
	 read-ahead has completed, whether successfully or not. */
      if (!SetEvent (cp->char_avail))
        {
	  DebPrint (("reader_thread.SetEvent failed with %lu for fd %ld\n",
		     GetLastError (), cp->fd));
	  return 1;
	}

      if (rc == STATUS_READ_ERROR)
	return 1;
        
      /* If the read died, the child has died so let the thread die */
      if (rc == STATUS_READ_FAILED)
	break;
        
      /* Wait until our input is acknowledged before reading again */
      if (WaitForSingleObject (cp->char_consumed, INFINITE) != WAIT_OBJECT_0)
        {
	  DebPrint (("reader_thread.WaitForSingleObject failed with "
		     "%lu for fd %ld\n", GetLastError (), cp->fd));
	  break;
        }
    }
  return 0;
}

static BOOL 
create_child (char *exe, char *cmdline, char *env,
	      int * pPid, child_process *cp)
{
  STARTUPINFO start;
  SECURITY_ATTRIBUTES sec_attrs;
  SECURITY_DESCRIPTOR sec_desc;
  
  if (cp == NULL) abort ();
  
  memset (&start, 0, sizeof (start));
  start.cb = sizeof (start);
  
#ifdef HAVE_NTGUI
  start.dwFlags = STARTF_USESTDHANDLES | STARTF_USESHOWWINDOW;
  start.wShowWindow = SW_HIDE;

  start.hStdInput = GetStdHandle (STD_INPUT_HANDLE);
  start.hStdOutput = GetStdHandle (STD_OUTPUT_HANDLE);
  start.hStdError = GetStdHandle (STD_ERROR_HANDLE);
#endif /* HAVE_NTGUI */

  /* Explicitly specify no security */
  if (!InitializeSecurityDescriptor (&sec_desc, SECURITY_DESCRIPTOR_REVISION))
    goto EH_Fail;
  if (!SetSecurityDescriptorDacl (&sec_desc, TRUE, NULL, FALSE))
    goto EH_Fail;
  sec_attrs.nLength = sizeof (sec_attrs);
  sec_attrs.lpSecurityDescriptor = &sec_desc;
  sec_attrs.bInheritHandle = FALSE;
  
  if (!CreateProcess (exe, cmdline, &sec_attrs, NULL, TRUE,
		      CREATE_NEW_PROCESS_GROUP,
		      env, NULL,
		      &start, &cp->procinfo))
    goto EH_Fail;

  cp->pid = (int) cp->procinfo.dwProcessId;

  /* Hack for Windows 95, which assigns large (ie negative) pids */
  if (cp->pid < 0)
    cp->pid = -cp->pid;

  /* pid must fit in a Lisp_Int */
  cp->pid = (cp->pid & VALMASK);


  *pPid = cp->pid;
  
  return TRUE;
  
 EH_Fail:
  DebPrint (("create_child.CreateProcess failed: %ld\n", GetLastError()););
  return FALSE;
}

/* create_child doesn't know what emacs' file handle will be for waiting
   on output from the child, so we need to make this additional call
   to register the handle with the process
   This way the select emulator knows how to match file handles with
   entries in child_procs.  */
void 
register_child (int pid, int fd)
{
  child_process *cp;
  
  cp = find_child_pid (pid);
  if (cp == NULL)
    {
      DebPrint (("register_child unable to find pid %lu\n", pid));
      return;
    }
  
#ifdef FULL_DEBUG
  DebPrint (("register_child registered fd %d with pid %lu\n", fd, pid));
#endif
  
  cp->fd = fd;

  /* thread is initially blocked until select is called; set status so
     that select will release thread */
  cp->status = STATUS_READ_ACKNOWLEDGED;

  /* attach child_process to fd_info */
  if (fd_info[fd].cp != NULL)
    {
      DebPrint (("register_child: fd_info[%d] apparently in use!\n", fd));
      abort ();
    }

  fd_info[fd].cp = cp;
}

/* When a process dies its pipe will break so the reader thread will
   signal failure to the select emulator.
   The select emulator then calls this routine to clean up.
   Since the thread signaled failure we can assume it is exiting.  */
static void 
reap_subprocess (child_process *cp)
{
  if (cp->procinfo.hProcess)
    {
      /* Reap the process */
      if (WaitForSingleObject (cp->procinfo.hProcess, INFINITE) != WAIT_OBJECT_0)
	DebPrint (("reap_subprocess.WaitForSingleObject (process) failed "
		   "with %lu for fd %ld\n", GetLastError (), cp->fd));
      CloseHandle (cp->procinfo.hProcess);
      cp->procinfo.hProcess = NULL;
      CloseHandle (cp->procinfo.hThread);
      cp->procinfo.hThread = NULL;

      /* If this was a DOS process, indicate that it is now safe to
	 start a new one.  */
      if (cp->is_dos_process)
	dos_process_running = FALSE;
    }

  /* For asynchronous children, the child_proc resources will be freed
     when the last pipe read descriptor is closed; for synchronous
     children, we must explicitly free the resources now because
     register_child has not been called. */
  if (cp->fd == -1)
    delete_child (cp);
}

/* Wait for any of our existing child processes to die
   When it does, close its handle
   Return the pid and fill in the status if non-NULL.  */

int 
sys_wait (int *status)
{
  DWORD active, retval;
  int nh;
  int pid;
  child_process *cp, *cps[MAX_CHILDREN];
  HANDLE wait_hnd[MAX_CHILDREN];
  
  nh = 0;
  if (dead_child != NULL)
    {
      /* We want to wait for a specific child */
      wait_hnd[nh] = dead_child->procinfo.hProcess;
      cps[nh] = dead_child;
      if (!wait_hnd[nh]) abort ();
      nh++;
    }
  else
    {
      for (cp = child_procs+(child_proc_count-1); cp >= child_procs; cp--)
	/* some child_procs might be sockets; ignore them */
	if (CHILD_ACTIVE (cp) && cp->procinfo.hProcess)
	  {
	    wait_hnd[nh] = cp->procinfo.hProcess;
	    cps[nh] = cp;
	    if (!wait_hnd[nh]) abort ();
	    nh++;
	  }
    }
  
  if (nh == 0)
    {
      /* Nothing to wait on, so fail */
      errno = ECHILD;
      return -1;
    }
  
  active = WaitForMultipleObjects (nh, wait_hnd, FALSE, INFINITE);
  if (active == WAIT_FAILED)
    {
      errno = EBADF;
      return -1;
    }
  else if (active == WAIT_TIMEOUT)
    {
      /* Should never happen */
      errno = EINVAL;
      return -1;
    }
  else if (active >= WAIT_OBJECT_0 &&
	   active < WAIT_OBJECT_0+MAXIMUM_WAIT_OBJECTS)
    {
      active -= WAIT_OBJECT_0;
    }
  else if (active >= WAIT_ABANDONED_0 &&
	   active < WAIT_ABANDONED_0+MAXIMUM_WAIT_OBJECTS)
    {
      active -= WAIT_ABANDONED_0;
    }
  
  if (!GetExitCodeProcess (wait_hnd[active], &retval))
    {
      DebPrint (("Wait.GetExitCodeProcess failed with %lu\n",
		 GetLastError ()));
      retval = 1;
    }
  if (retval == STILL_ACTIVE)
    {
      /* Should never happen */
      DebPrint (("Wait.WaitForMultipleObjects returned an active process\n"));
      errno = EINVAL;
      return -1;
    }

  /* Massage the exit code from the process to match the format expected
     by the WIFSTOPPED et al macros in syswait.h.  Only WIFSIGNALED and
     WIFEXITED are supported; WIFSTOPPED doesn't make sense under NT.  */

  if (retval == STATUS_CONTROL_C_EXIT)
    retval = SIGINT;
  else
    retval <<= 8;
  
  cp = cps[active];
  pid = cp->pid;
#ifdef FULL_DEBUG
  DebPrint (("Wait signaled with process pid %d\n", cp->pid));
#endif

  if (status)
    {
      *status = retval;
    }
  else if (synch_process_alive)
    {
      synch_process_alive = 0;

      /* Report the status of the synchronous process.  */
      if (WIFEXITED (retval))
	synch_process_retcode = WRETCODE (retval);
      else if (WIFSIGNALED (retval))
	{
	  int code = WTERMSIG (retval);
	  char *signame = 0;
	  
	  if (code < NSIG)
	    {
	      /* Suppress warning if the table has const char *.  */
	      signame = (char *) sys_siglist[code];
	    }
	  if (signame == 0)
	    signame = "unknown";

	  synch_process_death = signame;
	}

      reap_subprocess (cp);
    }
  
  return pid;
}

int
win32_is_dos_binary (char * filename)
{
  IMAGE_DOS_HEADER dos_header;
  DWORD signature;
  int fd;
  int is_dos_binary = FALSE;

  fd = open (filename, O_RDONLY | O_BINARY, 0);
  if (fd >= 0)
    {
      char * p = strrchr (filename, '.');

      /* We can only identify DOS .com programs from the extension. */
      if (p && stricmp (p, ".com") == 0)
	is_dos_binary = TRUE;
      else if (p && stricmp (p, ".bat") == 0)
	{
	  /* A DOS shell script - it appears that CreateProcess is happy
	     to accept this (somewhat surprisingly); presumably it looks
	     at COMSPEC to determine what executable to actually invoke.
	     Therefore, we have to do the same here as well. */
	  p = getenv ("COMSPEC");
	  if (p)
	    is_dos_binary = win32_is_dos_binary (p);
	}
      else
	{
	  /* Look for DOS .exe signature - if found, we must also check
	     that it isn't really a 16- or 32-bit Windows exe, since
	     both formats start with a DOS program stub.  Note that
	     16-bit Windows executables use the OS/2 1.x format. */
	  if (read (fd, &dos_header, sizeof (dos_header)) == sizeof (dos_header)
	      && dos_header.e_magic == IMAGE_DOS_SIGNATURE
	      && lseek (fd, dos_header.e_lfanew, SEEK_SET) != -1)
	    {
	      if (read (fd, &signature, sizeof (signature)) != sizeof (signature)
		  || (signature != IMAGE_NT_SIGNATURE &&
		      LOWORD (signature) != IMAGE_OS2_SIGNATURE))
		is_dos_binary = TRUE;
	    }
	}
      close (fd);
    }

  return is_dos_binary;
}

/* We pass our process ID to our children by setting up an environment
   variable in their environment.  */
char ppid_env_var_buffer[64];

/* When a new child process is created we need to register it in our list,
   so intercept spawn requests.  */
int 
sys_spawnve (int mode, char *cmdname, char **argv, char **envp)
{
  Lisp_Object program, full;
  char *cmdline, *env, *parg, **targ;
  int arglen;
  int pid;
  child_process *cp;
  int is_dos_binary;
  
  /* We don't care about the other modes */
  if (mode != _P_NOWAIT)
    {
      errno = EINVAL;
      return -1;
    }

  /* Handle executable names without an executable suffix.  */
  program = make_string (cmdname, strlen (cmdname));
  if (NILP (Ffile_executable_p (program)))
    {
      struct gcpro gcpro1;
      
      full = Qnil;
      GCPRO1 (program);
      openp (Vexec_path, program, EXEC_SUFFIXES, &full, 1);
      UNGCPRO;
      if (NILP (full))
	{
	  errno = EINVAL;
	  return -1;
	}
      cmdname = XSTRING (full)->data;
      argv[0] = cmdname;
    }

  /* make sure cmdname is in DOS format */
  strcpy (cmdname = alloca (strlen (cmdname) + 1), argv[0]);
  unixtodos_filename (cmdname);
  argv[0] = cmdname;

  /* Check if program is a DOS executable, and if so whether we are
     allowed to start it. */
  is_dos_binary = win32_is_dos_binary (cmdname);
  if (is_dos_binary && dos_process_running)
    {
      errno = EAGAIN;
      return -1;
    }
  
  /* we have to do some conjuring here to put argv and envp into the
     form CreateProcess wants...  argv needs to be a space separated/null
     terminated list of parameters, and envp is a null
     separated/double-null terminated list of parameters.

     Additionally, zero-length args and args containing whitespace need
     to be wrapped in double quotes.  Args containing embedded double
     quotes (as opposed to enclosing quotes, which we leave alone) are
     usually illegal (most Win32 programs do not implement escaping of
     double quotes - sad but true, at least for programs compiled with
     MSVC), but we will escape quotes anyway for those programs that can
     handle it.  The Win32 gcc library from Cygnus doubles quotes to
     escape them, so we will use that convention.
   
     Since I have no idea how large argv and envp are likely to be
     we figure out list lengths on the fly and allocate them.  */
  
  /* do argv...  */
  arglen = 0;
  targ = argv;
  while (*targ)
    {
      char * p = *targ;
      int add_quotes = 0;

      if (*p == 0)
	add_quotes = 1;
      while (*p)
	if (*p++ == '"')
	  {
	    /* allow for embedded quotes to be doubled - we won't
	       actually double quotes that aren't embedded though */
	    arglen++;
	    add_quotes = 1;
	  }
      else if (*p == ' ' || *p == '\t')
	add_quotes = 1;
      if (add_quotes)
	arglen += 2;
      arglen += strlen (*targ++) + 1;
    }
  cmdline = alloca (arglen);
  targ = argv;
  parg = cmdline;
  while (*targ)
    {
      char * p = *targ;
      int add_quotes = 0;

      if (*p == 0)
	add_quotes = 1;

      if (!NILP (Vwin32_quote_process_args))
	{
	  /* This is conditional because it sometimes causes more
	     problems than it solves, since argv arrays are not always
	     carefully constructed.  M-x grep, for instance, passes the
	     whole command line as one argument, so it becomes
	     impossible to pass a regexp which contains spaces. */
	  for ( ; *p; p++)
	    if (*p == ' ' || *p == '\t' || *p == '"')
	      add_quotes = 1;
	}
      if (add_quotes)
	{
	  char * first;
	  char * last;

	  p = *targ;
	  first = p;
	  last = p + strlen (p) - 1;
	  *parg++ = '"';
	  while (*p)
	    {
	      if (*p == '"' && p > first && p < last)
		*parg++ = '"';	/* double up embedded quotes only */
	      *parg++ = *p++;
	    }
	  *parg++ = '"';
	}
      else
	{
	  strcpy (parg, *targ);
	  parg += strlen (*targ);
	}
      *parg++ = ' ';
      targ++;
    }
  *--parg = '\0';
  
  /* and envp...  */
  arglen = 1;
  targ = envp;
  while (*targ)
    {
      arglen += strlen (*targ++) + 1;
    }
  sprintf (ppid_env_var_buffer, "__PARENT_PROCESS_ID=%d", 
	   GetCurrentProcessId ());
  arglen += strlen (ppid_env_var_buffer) + 1;

  env = alloca (arglen);
  targ = envp;
  parg = env;
  while (*targ)
    {
      strcpy (parg, *targ);
      parg += strlen (*targ++);
      *parg++ = '\0';
    }
  strcpy (parg, ppid_env_var_buffer);
  parg += strlen (ppid_env_var_buffer);
  *parg++ = '\0';
  *parg = '\0';

  cp = new_child ();
  if (cp == NULL)
    {
      errno = EAGAIN;
      return -1;
    }
  
  /* Now create the process.  */
  if (!create_child (cmdname, cmdline, env, &pid, cp))
    {
      delete_child (cp);
      errno = ENOEXEC;
      return -1;
    }

  if (is_dos_binary)
    {
      cp->is_dos_process = TRUE;
      dos_process_running = TRUE;
    }
  
  return pid;
}

/* Emulate the select call
   Wait for available input on any of the given rfds, or timeout if
   a timeout is given and no input is detected
   wfds and efds are not supported and must be NULL.  */

/* From ntterm.c */
extern HANDLE keyboard_handle;
/* From process.c */
extern int proc_buffered_char[];

int 
sys_select (int nfds, SELECT_TYPE *rfds, SELECT_TYPE *wfds, SELECT_TYPE *efds,
	    EMACS_TIME *timeout)
{
  SELECT_TYPE orfds;
  DWORD timeout_ms;
  int i, nh, nr;
  DWORD active;
  child_process *cp;
  HANDLE wait_hnd[MAXDESC];
  int fdindex[MAXDESC];   /* mapping from wait handles back to descriptors */
  
  /* If the descriptor sets are NULL but timeout isn't, then just Sleep.  */
  if (rfds == NULL && wfds == NULL && efds == NULL && timeout != NULL) 
    {
      Sleep (timeout->tv_sec * 1000 + timeout->tv_usec / 1000);
      return 0;
    }

  /* Otherwise, we only handle rfds, so fail otherwise.  */
  if (rfds == NULL || wfds != NULL || efds != NULL)
    {
      errno = EINVAL;
      return -1;
    }
  
  orfds = *rfds;
  FD_ZERO (rfds);
  nr = 0;
  
  /* Build a list of handles to wait on.  */
  nh = 0;
  for (i = 0; i < nfds; i++)
    if (FD_ISSET (i, &orfds))
      {
	if (i == 0)
	  {
	    if (keyboard_handle)
	      {
		/* Handle stdin specially */
		wait_hnd[nh] = keyboard_handle;
		fdindex[nh] = i;
		nh++;
	      }

	    /* Check for any emacs-generated input in the queue since
	       it won't be detected in the wait */
	    if (detect_input_pending ())
	      {
		FD_SET (i, rfds);
		return 1;
	      }
	  }
	else
	  {
	    /* Child process and socket input */
	    cp = fd_info[i].cp;
	    if (cp)
	      {
		int current_status = cp->status;

		if (current_status == STATUS_READ_ACKNOWLEDGED)
		  {
		    /* Tell reader thread which file handle to use. */
		    cp->fd = i;
		    /* Wake up the reader thread for this process */
		    cp->status = STATUS_READ_READY;
		    if (!SetEvent (cp->char_consumed))
		      DebPrint (("nt_select.SetEvent failed with "
				 "%lu for fd %ld\n", GetLastError (), i));
		  }

#ifdef CHECK_INTERLOCK
		/* slightly crude cross-checking of interlock between threads */

		current_status = cp->status;
		if (WaitForSingleObject (cp->char_avail, 0) == WAIT_OBJECT_0)
		  {
		    /* char_avail has been signalled, so status (which may
		       have changed) should indicate read has completed
		       but has not been acknowledged. */
		    current_status = cp->status;
		    if (current_status != STATUS_READ_SUCCEEDED &&
			current_status != STATUS_READ_FAILED)
		      DebPrint (("char_avail set, but read not completed: status %d\n",
				 current_status));
		  }
		else
		  {
		    /* char_avail has not been signalled, so status should
		       indicate that read is in progress; small possibility
		       that read has completed but event wasn't yet signalled
		       when we tested it (because a context switch occurred
		       or if running on separate CPUs). */
		    if (current_status != STATUS_READ_READY &&
			current_status != STATUS_READ_IN_PROGRESS &&
			current_status != STATUS_READ_SUCCEEDED &&
			current_status != STATUS_READ_FAILED)
		      DebPrint (("char_avail reset, but read status is bad: %d\n",
				 current_status));
		  }
#endif
		wait_hnd[nh] = cp->char_avail;
		fdindex[nh] = i;
		if (!wait_hnd[nh]) abort ();
		nh++;
#ifdef FULL_DEBUG
		DebPrint (("select waiting on child %d fd %d\n",
			   cp-child_procs, i));
#endif
	      }
	    else
	      {
		/* Unable to find something to wait on for this fd, skip */
		DebPrint (("sys_select: fd %ld is invalid! ignoring\n", i));
		abort ();
	      }
	  }
      }
  
  /* Nothing to look for, so we didn't find anything */
  if (nh == 0) 
    {
      if (timeout)
	Sleep (timeout->tv_sec * 1000 + timeout->tv_usec / 1000);
      return 0;
    }
  
  /*
     Wait for input
     If a child process dies while this is waiting, its pipe will break
     so the reader thread will signal an error condition, thus, the wait
     will wake up
     */
  timeout_ms = timeout ? (timeout->tv_sec * 1000 + timeout->tv_usec / 1000) : INFINITE;

  active = WaitForMultipleObjects (nh, wait_hnd, FALSE, timeout_ms);

  if (active == WAIT_FAILED)
    {
      DebPrint (("select.WaitForMultipleObjects (%d, %lu) failed with %lu\n",
		 nh, timeout_ms, GetLastError ()));
      /* don't return EBADF - this causes wait_reading_process_input to
	 abort; WAIT_FAILED is returned when single-stepping under
	 Windows 95 after switching thread focus in debugger, and
	 possibly at other times. */
      errno = EINTR;
      return -1;
    }
  else if (active == WAIT_TIMEOUT)
    {
      return 0;
    }
  else if (active >= WAIT_OBJECT_0 &&
	   active < WAIT_OBJECT_0+MAXIMUM_WAIT_OBJECTS)
    {
      active -= WAIT_OBJECT_0;
    }
  else if (active >= WAIT_ABANDONED_0 &&
	   active < WAIT_ABANDONED_0+MAXIMUM_WAIT_OBJECTS)
    {
      active -= WAIT_ABANDONED_0;
    }

  /* Loop over all handles after active (now officially documented as
     being the first signalled handle in the array).  We do this to
     ensure fairness, so that all channels with data available will be
     processed - otherwise higher numbered channels could be starved. */
  do
    {
      if (fdindex[active] == 0)
	{
	  /* Keyboard input available */
	  FD_SET (0, rfds);
	  nr++;
	}
      else
	{
	  /* must be a socket or pipe */
	  int current_status;

	  cp = fd_info[ fdindex[active] ].cp;

	  /* Read ahead should have completed, either succeeding or failing. */
	  FD_SET (fdindex[active], rfds);
	  nr++;
	  current_status = cp->status;
	  if (current_status != STATUS_READ_SUCCEEDED)
	    {
	      if (current_status != STATUS_READ_FAILED)
		DebPrint (("internal error: subprocess pipe signalled "
			   "at the wrong time (status %d)\n!", current_status));

	      /* The child_process entry for a socket or pipe will be
		 freed when the last descriptor using it is closed; for
		 pipes, we call the SIGCHLD handler. */
	      if (fd_info[ fdindex[active] ].flags & FILE_PIPE)
		{
		  /* The SIGCHLD handler will do a Wait so we know it won't
		     return until the process is dead
		     We force Wait to only wait for this process to avoid it
		     picking up other children that happen to be dead but that
		     we haven't noticed yet
		     SIG_DFL for SIGCHLD is ignore? */
		  if (sig_handlers[SIGCHLD] != SIG_DFL &&
		      sig_handlers[SIGCHLD] != SIG_IGN)
		    {
#ifdef FULL_DEBUG
		      DebPrint (("select calling SIGCHLD handler for pid %d\n",
				 cp->pid));
#endif
		      dead_child = cp;
		      sig_handlers[SIGCHLD] (SIGCHLD);
		      dead_child = NULL;
		    }

		  /* Clean up the child process entry in the table */
		  reap_subprocess (cp);
		}
	    }
	}

      /* Test for input on remaining channels. */
      while (++active < nh)
	if (WaitForSingleObject (wait_hnd[active], 0) == WAIT_OBJECT_0)
	  break;
    } while (active < nh);

  return nr;
}

/* Substitute for certain kill () operations */
int 
sys_kill (int pid, int sig)
{
  child_process *cp;
  HANDLE proc_hand;
  int need_to_free = 0;
  int rc = 0;
  
  /* Only handle signals that will result in the process dying */
  if (sig != SIGINT && sig != SIGKILL && sig != SIGQUIT && sig != SIGHUP)
    {
      errno = EINVAL;
      return -1;
    }

  cp = find_child_pid (pid);
  if (cp == NULL)
    {
      proc_hand = OpenProcess (PROCESS_TERMINATE, 0, pid);
      if (proc_hand == NULL)
        {
	  errno = EPERM;
	  return -1;
	}
      need_to_free = 1;
    }
  else
    {
      proc_hand = cp->procinfo.hProcess;
      pid = cp->procinfo.dwProcessId;
    }
  
  if (sig == SIGINT)
    {
      /* Ctrl-Break is NT equivalent of SIGINT.  */
      if (!GenerateConsoleCtrlEvent (CTRL_BREAK_EVENT, pid))
        {
	  DebPrint (("sys_kill.GenerateConsoleCtrlEvent return %d "
		     "for pid %lu\n", GetLastError (), pid));
	  errno = EINVAL;
	  rc = -1;
        }
    }
  else
    {
      /* Kill the process.  On Win32 this doesn't kill child processes
	 so it doesn't work very well for shells which is why it's not
	 used in every case.  Also, don't try to terminate DOS processes
	 (on Win95), because this will hang Emacs. */
      if (!(cp && cp->is_dos_process)
	  && !TerminateProcess (proc_hand, 0xff))
        {
	  DebPrint (("sys_kill.TerminateProcess returned %d "
		     "for pid %lu\n", GetLastError (), pid));
	  errno = EINVAL;
	  rc = -1;
        }
    }

  if (need_to_free)
    CloseHandle (proc_hand);

  return rc;
}

extern int report_file_error (char *, Lisp_Object);

/* The following two routines are used to manipulate stdin, stdout, and
   stderr of our child processes.

   Assuming that in, out, and err are *not* inheritable, we make them
   stdin, stdout, and stderr of the child as follows:

   - Save the parent's current standard handles.
   - Set the std handles to inheritable duplicates of the ones being passed in.
     (Note that _get_osfhandle() is an io.h procedure that retrieves the
     NT file handle for a crt file descriptor.)
   - Spawn the child, which inherits in, out, and err as stdin,
     stdout, and stderr. (see Spawnve)
   - Close the std handles passed to the child.
   - Reset the parent's standard handles to the saved handles.
     (see reset_standard_handles)
   We assume that the caller closes in, out, and err after calling us.  */

void
prepare_standard_handles (int in, int out, int err, HANDLE handles[3])
{
  HANDLE parent;
  HANDLE newstdin, newstdout, newstderr;

  parent = GetCurrentProcess ();

  handles[0] = GetStdHandle (STD_INPUT_HANDLE);
  handles[1] = GetStdHandle (STD_OUTPUT_HANDLE);
  handles[2] = GetStdHandle (STD_ERROR_HANDLE);

  /* make inheritable copies of the new handles */
  if (!DuplicateHandle (parent, 
		       (HANDLE) _get_osfhandle (in),
		       parent,
		       &newstdin, 
		       0, 
		       TRUE, 
		       DUPLICATE_SAME_ACCESS))
    report_file_error ("Duplicating input handle for child", Qnil);
  
  if (!DuplicateHandle (parent,
		       (HANDLE) _get_osfhandle (out),
		       parent,
		       &newstdout,
		       0,
		       TRUE,
		       DUPLICATE_SAME_ACCESS))
    report_file_error ("Duplicating output handle for child", Qnil);
  
  if (!DuplicateHandle (parent,
		       (HANDLE) _get_osfhandle (err),
		       parent,
		       &newstderr,
		       0,
		       TRUE,
		       DUPLICATE_SAME_ACCESS))
    report_file_error ("Duplicating error handle for child", Qnil);

  /* and store them as our std handles */
  if (!SetStdHandle (STD_INPUT_HANDLE, newstdin))
    report_file_error ("Changing stdin handle", Qnil);
  
  if (!SetStdHandle (STD_OUTPUT_HANDLE, newstdout))
    report_file_error ("Changing stdout handle", Qnil);

  if (!SetStdHandle (STD_ERROR_HANDLE, newstderr))
    report_file_error ("Changing stderr handle", Qnil);
}

void
reset_standard_handles (int in, int out, int err, HANDLE handles[3])
{
  /* close the duplicated handles passed to the child */
  CloseHandle (GetStdHandle (STD_INPUT_HANDLE));
  CloseHandle (GetStdHandle (STD_OUTPUT_HANDLE));
  CloseHandle (GetStdHandle (STD_ERROR_HANDLE));

  /* now restore parent's saved std handles */
  SetStdHandle (STD_INPUT_HANDLE, handles[0]);
  SetStdHandle (STD_OUTPUT_HANDLE, handles[1]);
  SetStdHandle (STD_ERROR_HANDLE, handles[2]);
}

#ifdef HAVE_SOCKETS

/* To avoid problems with winsock implementations that work over dial-up
   connections causing or requiring a connection to exist while Emacs is
   running, Emacs no longer automatically loads winsock on startup if it
   is present.  Instead, it will be loaded when open-network-stream is
   first called.

   To allow full control over when winsock is loaded, we provide these
   two functions to dynamically load and unload winsock.  This allows
   dial-up users to only be connected when they actually need to use
   socket services.  */

/* From nt.c */
extern HANDLE winsock_lib;
extern BOOL term_winsock (void);
extern BOOL init_winsock (int load_now);

extern Lisp_Object Vsystem_name;

DEFUN ("win32-has-winsock", Fwin32_has_winsock, Swin32_has_winsock, 0, 1, 0,
  "Test for presence of the Windows socket library `winsock'.\n\
Returns non-nil if winsock support is present, nil otherwise.\n\
\n\
If the optional argument LOAD-NOW is non-nil, the winsock library is\n\
also loaded immediately if not already loaded.  If winsock is loaded,\n\
the winsock local hostname is returned (since this may be different from\n\
the value of `system-name' and should supplant it), otherwise t is\n\
returned to indicate winsock support is present.")
  (load_now)
     Lisp_Object load_now;
{
  int have_winsock;

  have_winsock = init_winsock (!NILP (load_now));
  if (have_winsock)
    {
      if (winsock_lib != NULL)
	{
	  /* Return new value for system-name.  The best way to do this
	     is to call init_system_name, saving and restoring the
	     original value to avoid side-effects.  */
	  Lisp_Object orig_hostname = Vsystem_name;
	  Lisp_Object hostname;

	  init_system_name ();
	  hostname = Vsystem_name;
	  Vsystem_name = orig_hostname;
	  return hostname;
	}
      return Qt;
    }
  return Qnil;
}

DEFUN ("win32-unload-winsock", Fwin32_unload_winsock, Swin32_unload_winsock,
       0, 0, 0,
  "Unload the Windows socket library `winsock' if loaded.\n\
This is provided to allow dial-up socket connections to be disconnected\n\
when no longer needed.  Returns nil without unloading winsock if any\n\
socket connections still exist.")
  ()
{
  return term_winsock () ? Qt : Qnil;
}

#endif /* HAVE_SOCKETS */


syms_of_ntproc ()
{
#ifdef HAVE_SOCKETS
  defsubr (&Swin32_has_winsock);
  defsubr (&Swin32_unload_winsock);
#endif

  DEFVAR_LISP ("win32-quote-process-args", &Vwin32_quote_process_args,
    "Non-nil enables quoting of process arguments to ensure correct parsing.\n\
Because Windows does not directly pass argv arrays to child processes,\n\
programs have to reconstruct the argv array by parsing the command\n\
line string.  For an argument to contain a space, it must be enclosed\n\
in double quotes or it will be parsed as multiple arguments.\n\
\n\
However, the argument list to call-process is not always correctly\n\
constructed (or arguments have already been quoted), so enabling this\n\
option may cause unexpected behavior.");
  Vwin32_quote_process_args = Qnil;

  DEFVAR_INT ("win32-pipe-read-delay", &Vwin32_pipe_read_delay,
    "Forced delay before reading subprocess output.\n\
This is done to improve the buffering of subprocess output, by\n\
avoiding the inefficiency of frequently reading small amounts of data.\n\
\n\
If positive, the value is the number of milliseconds to sleep before\n\
reading the subprocess output.  If negative, the magnitude is the number\n\
of time slices to wait (effectively boosting the priority of the child\n\
process temporarily).  A value of zero disables waiting entirely.");
  Vwin32_pipe_read_delay = 50;

  DEFVAR_LISP ("win32-downcase-file-names", &Vwin32_downcase_file_names,
    "Non-nil means convert all-upper case file names to lower case.\n\
This applies when performing completions and file name expansion.");
  Vwin32_downcase_file_names = Qnil;
}
/* end of ntproc.c */
