/* Process support for Windows NT port of GNU EMACS.
   Copyright (C) 1992, 1995 Free Software Foundation, Inc.

   This file is part of GNU Emacs.

   GNU Emacs is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by the
   Free Software Foundation; either version 2, or (at your option) any later
   version.

   GNU Emacs is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
   FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
   more details.

   You should have received a copy of the GNU General Public License along
   with GNU Emacs; see the file COPYING.  If not, write to the Free Software
   Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

   Drew Bliss                   Oct 14, 1993
     Adapted from alarm.c by Tim Fleehart
*/

#include <config.h>

#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <io.h>
#include <signal.h>

#include <windows.h>

#include "lisp.h"
#include "nt.h"
#include "systime.h"

/* #define FULL_DEBUG */

typedef void (_CALLBACK_ *signal_handler)(int);

/* Defined in process.h which conflicts with the local copy */
#define	_P_NOWAIT 1

typedef struct _child_process
{
  int fd;
  HANDLE char_avail;
  HANDLE char_consumed;
  char chr;
  BOOL status;
  HANDLE process;
  DWORD pid;
  HANDLE thrd;
} child_process;

#define MAX_CHILDREN MAXDESC

#ifdef EMACSDEBUG
void _CRTAPI1
_DebPrint (char *fmt, ...)
{
  char buf[256];
  va_list args;

  va_start (args, fmt);
  vsprintf (buf, fmt, args);
  va_end (args);
  OutputDebugString (buf);
}
#endif

/* Child process management list.  */
static int child_proc_count = 0;
static child_process child_procs[MAX_CHILDREN];
static child_process *dead_child = NULL;

#define CHILD_ACTIVE(cp) ((cp)->process != NULL)
#define DEACTIVATE_CHILD(cp) ((cp)->process = NULL)

/* Signal handlers...SIG_DFL == 0 so this is initialized correctly.  */
static signal_handler sig_handlers[NSIG];

/* Fake signal implementation to record the SIGCHLD handler.  */
signal_handler 
win32_signal (int sig, signal_handler handler)
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

/* Find an unused process slot.  */
static child_process *
new_child (void)
{
  child_process *cp;
  
  if (child_proc_count == MAX_CHILDREN)
    return NULL;
  
  for (cp = child_procs+(child_proc_count-1); cp >= child_procs; cp--)
    if (!CHILD_ACTIVE (cp))
      return cp;
  return &child_procs[child_proc_count++];
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

/* Find a child by fd.  */
static child_process *
find_child_fd (int fd)
{
  child_process *cp;
  
  for (cp = child_procs+(child_proc_count-1); cp >= child_procs; cp--)
    if (CHILD_ACTIVE (cp) && fd == cp->fd)
      return cp;
  return NULL;
}

/* Thread proc for child process reader threads
   The threads just sit in a loop waiting for input
   When they detect input, they signal the char_avail input to
   wake up the select emulator
   When the select emulator processes their input, it pulses
   char_consumed so that the reader thread goes back to reading.  */
DWORD WINAPI 
reader_thread (void *arg)
{
  child_process *cp;
  
  /* Our identity */
  cp = (child_process *)arg;
  
  /* We have to wait for the go-ahead before we can start */
  if (WaitForSingleObject (cp->char_consumed, INFINITE) != WAIT_OBJECT_0)
    return 0;
  /* If something went wrong, quit */
  if (!cp->status)
    return 0;
  
  for (;;)
    {
      /* Use read to get CRLF translation */
      if (read (cp->fd, &cp->chr, sizeof (char)) == sizeof (char))
        {
	  cp->status = TRUE;
        }
      else
        {
#ifdef FULL_DEBUG
	  DebPrint (("reader_thread.read failed with %lu for fd %ld\n",
		     GetLastError (), cp->fd));
#endif
	  cp->status = FALSE;
        }
        
      if (!SetEvent (cp->char_avail))
        {
	  DebPrint (("reader_thread.SetEvent failed with %lu for fd %ld\n",
		     GetLastError (), cp->fd));
	  break;
        }
        
      /* If the read died, the child has died so let the thread die */
      if (!cp->status)
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
	     PROCESS_INFORMATION *info)
{
  child_process *cp;
  DWORD id;
  STARTUPINFO start;
  SECURITY_ATTRIBUTES sec_attrs;
  SECURITY_DESCRIPTOR sec_desc;
  
  cp = new_child ();
  if (cp == NULL)
    goto EH_Fail;
  
  cp->fd = -1;
  
  cp->char_avail = CreateEvent (NULL, FALSE, FALSE, NULL);
  if (cp->char_avail == NULL)
    goto EH_Fail;
  
  cp->char_consumed = CreateEvent (NULL, FALSE, FALSE, NULL);
  if (cp->char_consumed == NULL)
    goto EH_char_avail;
  
  cp->thrd = CreateThread (NULL, 1024, reader_thread, cp, 0, &id);
  if (cp->thrd == NULL)
    goto EH_char_consumed;
  
  memset (&start, 0, sizeof (start));
  start.cb = sizeof (start);
  
  /* Explicitly specify no security */
  if (!InitializeSecurityDescriptor (&sec_desc, SECURITY_DESCRIPTOR_REVISION))
    goto EH_thrd;
  if (!SetSecurityDescriptorDacl (&sec_desc, TRUE, NULL, FALSE))
    goto EH_thrd;
  sec_attrs.nLength = sizeof (sec_attrs);
  sec_attrs.lpSecurityDescriptor = &sec_desc;
  sec_attrs.bInheritHandle = FALSE;
  
  if (!CreateProcess (exe, cmdline, &sec_attrs, NULL, TRUE,
		      CREATE_NEW_PROCESS_GROUP, env, NULL,
		      &start, info))
    goto EH_thrd;
  cp->process = info->hProcess;
  cp->pid = info->dwProcessId;
  
  return TRUE;
  
 EH_thrd:
  id = GetLastError ();
  
  cp->status = FALSE;
  SetEvent (cp->char_consumed);
 EH_char_consumed:
  CloseHandle (cp->char_consumed);
 EH_char_avail:
  CloseHandle (cp->char_avail);
 EH_Fail:
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
  cp->status = TRUE;

  /* Tell the reader thread to start */
  if (!SetEvent (cp->char_consumed))
    {
      DebPrint (("register_child.SetEvent failed with %lu for fd %ld\n",
		 GetLastError (), cp->fd));
    }
}

/* When a process dies its pipe will break so the reader thread will
   signal failure to the select emulator.
   The select emulator then calls this routine to clean up.
   Since the thread signaled failure we can assume it is exiting.  */
static void 
remove_child (child_process *cp)
{
  /* Reap the thread */
  if (WaitForSingleObject (cp->thrd, INFINITE) != WAIT_OBJECT_0)
    {
      DebPrint (("remove_child.WaitForSingleObject (thread) failed "
		 "with %lu for fd %ld\n", GetLastError (), cp->fd));
    }
  CloseHandle (cp->thrd);
  CloseHandle (cp->char_consumed);
  CloseHandle (cp->char_avail);
  
  /* Reap the process */
  if (WaitForSingleObject (cp->process, INFINITE) != WAIT_OBJECT_0)
    {
      DebPrint (("remove_child.WaitForSingleObject (process) failed "
		 "with %lu for fd %ld\n", GetLastError (), cp->fd));
    }
  CloseHandle (cp->process);
  
  DEACTIVATE_CHILD (cp);
}

/* Wait for any of our existing child processes to die
   When it does, close its handle
   Return the pid and fill in the status if non-NULL.  */

/* From callproc.c */
extern int synch_process_alive;
extern int synch_process_retcode;

int 
win32_wait (int *status)
{
  DWORD active, retval;
  int nh;
  child_process *cp, *cps[MAX_CHILDREN];
  HANDLE wait_hnd[MAX_CHILDREN];
  
  nh = 0;
  if (dead_child != NULL)
    {
      /* We want to wait for a specific child */
      wait_hnd[nh] = dead_child->process;
      cps[nh] = dead_child;
      nh++;
    }
  else
    {
      for (cp = child_procs+(child_proc_count-1); cp >= child_procs; cp--)
	if (CHILD_ACTIVE (cp))
	  {
	    wait_hnd[nh] = cp->process;
	    cps[nh] = cp;
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
  
  cp = cps[active];

  if (status)
    {
      *status = retval;
    }
  else if (synch_process_alive)
    {
      synch_process_alive = 0;
      synch_process_retcode = retval;

      TerminateThread (cp->thrd, 0);
      CloseHandle (cp->thrd);
      CloseHandle (cp->char_consumed);
      CloseHandle (cp->char_avail);
      CloseHandle (cp->process);
      DEACTIVATE_CHILD (cp);
    }
  
  return cp->pid;
}

/* We pass our process ID to our children by setting up an environment
   variable in their environment.  */
char ppid_env_var_buffer[64];

/* When a new child process is created we need to register it in our list,
   so intercept spawn requests.  */
int 
win32_spawnve (int mode, char *cmdname, char **argv, char **envp)
{
  char *cmdline, *env, *parg, **targ;
  int arglen;
  PROCESS_INFORMATION pi;
  
  if (child_proc_count == MAX_CHILDREN)
    {
      errno = EAGAIN;
      return -1;
    }
  
  /* We don't care about the other modes */
  if (mode != _P_NOWAIT)
    {
      errno = EINVAL;
      return -1;
    }
  
  /* we have to do some conjuring here to put argv and envp into the
     form CreateProcess wants...  argv needs to be a space separated/null
     terminated list of parameters, and envp is a null
     separated/double-null terminated list of parameters.
   
     Since I have no idea how large argv and envp are likely to be
     we figure out list lengths on the fly and allocate them.  */
  
  /* do argv...  */
  arglen = 0;
  targ = argv;
  while (*targ)
    {
      arglen += strlen (*targ++) + 1;
    }
  cmdline = malloc (arglen);
  if (cmdline == NULL)
    {
      errno = ENOMEM;
      goto EH_Fail;
    }
  targ = argv;
  parg = cmdline;
  while (*targ)
    {
      strcpy (parg, *targ);
      parg += strlen (*targ++);
      *parg++ = ' ';
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

  env = malloc (arglen);
  if (env == NULL)
    {
      errno = ENOMEM;
      goto EH_cmdline;
    }
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
  
  /* Now create the process.  */
  if (!create_child (cmdname, cmdline, env, &pi))
    {
      errno = ENOEXEC;
      goto EH_env;
    }
  
  return pi.dwProcessId;
  
 EH_env:
  free (env);
 EH_cmdline:
  free (cmdline);
 EH_Fail:
  return -1;
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
  child_process *cp, *cps[MAX_CHILDREN];
  HANDLE wait_hnd[MAX_CHILDREN];
  
  /* If the descriptor sets are NULL but timeout isn't, then just Sleep.  */
  if (rfds == NULL && wfds == NULL && efds == NULL && timeout != NULL) 
    {
#ifdef HAVE_TIMEVAL
      Sleep (timeout->tv_sec * 1000 + timeout->tv_usec / 1000);
#else
      Sleep ((*timeout) * 1000);
#endif
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
	    /* Handle stdin specially */
	    wait_hnd[nh] = keyboard_handle;
	    cps[nh] = NULL;
	    nh++;

	    /* Check for any emacs-generated input in the queue since
	       it won't be detected in the wait */
	    if (detect_input_pending ())
	      {
		FD_SET (i, rfds);
		nr++;
	      }
	  }
	else
	  {
	    /* Child process input */
	    cp = find_child_fd (i);
	    if (cp)
	      {
#ifdef FULL_DEBUG
		DebPrint (("select waiting on child %d fd %d\n",
			   cp-child_procs, i));
#endif
		wait_hnd[nh] = cp->char_avail;
		cps[nh] = cp;
		nh++;
	      }
	    else
	      {
		/* Unable to find something to wait on for this fd, fail */
		DebPrint (("select unable to find child process "
			   "for fd %ld\n", i));
		nh = 0;
		break;
	      }
	  }
      }
  
  /* Nothing to look for, so we didn't find anything */
  if (nh == 0) 
    {
      if (timeout)
#ifdef HAVE_TIMEVAL
	Sleep (timeout->tv_sec * 1000 + timeout->tv_usec / 1000);
#else
	Sleep ((*timeout) * 1000);
#endif
      return 0;
    }
  
  /* Check for immediate return without waiting */
  if (nr > 0)
    return nr;
  
  /*
     Wait for input
     If a child process dies while this is waiting, its pipe will break
     so the reader thread will signal an error condition, thus, the wait
     will wake up
     */
#ifdef HAVE_TIMEVAL
  timeout_ms = timeout ? (timeout->tv_sec * 1000 + timeout->tv_usec / 1000) : INFINITE;
#else
  timeout_ms = timeout ? *timeout*1000 : INFINITE;
#endif
  active = WaitForMultipleObjects (nh, wait_hnd, FALSE, timeout_ms);
  if (active == WAIT_FAILED)
    {
      DebPrint (("select.WaitForMultipleObjects (%d, %lu) failed with %lu\n",
		 nh, timeout_ms, GetLastError ()));
      /* Is there a better error? */
      errno = EBADF;
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
  
  if (cps[active] == NULL)
    {
      /* Keyboard input available */
      FD_SET (0, rfds);
      nr++;

      /* This shouldn't be necessary, but apparently just setting the input
	 fd is not good enough for emacs */
      read_input_waiting ();
    }
  else
    {
      /* Child process */
      cp = cps[active];

      /* If status is FALSE the read failed so don't report input */
      if (cp->status)
        {
	  FD_SET (cp->fd, rfds);
	  proc_buffered_char[cp->fd] = cp->chr;
	  nr++;
        }
      else
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
	      sig_handlers[SIGCHLD](SIGCHLD);
	      dead_child = NULL;
            }
            
	  /* Clean up the child process entry in the table */
	  remove_child (cp);
        }
    }
  return nr;
}

/*
   Substitute for certain kill () operations
   */
int 
win32_kill_process (int pid, int sig)
{
  child_process *cp;
  
  /* Only handle signals that will result in the process dying */
  if (sig != SIGINT && sig != SIGKILL && sig != SIGQUIT && sig != SIGHUP)
    {
      errno = EINVAL;
      return -1;
    }
  
  cp = find_child_pid (pid);
  if (cp == NULL)
    {
      DebPrint (("win32_kill_process didn't find a child with pid %lu\n", pid));
      errno = ECHILD;
      return -1;
    }
  
  if (sig == SIGINT)
    {
      /* Fake Ctrl-Break.  */
      if (!GenerateConsoleCtrlEvent (CTRL_BREAK_EVENT, pid))
        {
	  DebPrint (("win32_kill_process.GenerateConsoleCtrlEvent return %d "
		     "for pid %lu\n", GetLastError (), pid));
	  errno = EINVAL;
	  return -1;
        }
    }
  else
    {
      /* Kill the process.  On Win32 this doesn't kill child processes
	 so it doesn't work very well for shells which is why it's
	 not used in every case.  */
      if (!TerminateProcess (cp->process, 0xff))
        {
	  DebPrint (("win32_kill_process.TerminateProcess returned %d "
		     "for pid %lu\n", GetLastError (), pid));
	  errno = EINVAL;
	  return -1;
        }
    }
  return 0;
}

/* If the channel is a pipe this read might block since we don't
   know how many characters are available, so check and read only
   what's there
   We also need to wake up the reader thread once we've read our data.  */
int 
read_child_output (int fd, char *buf, int max)
{
  HANDLE h;
  int to_read, nchars;
  DWORD waiting;
  child_process *cp;
  
  h = (HANDLE)_get_osfhandle (fd);
  if (GetFileType (h) == FILE_TYPE_PIPE)
    {
      PeekNamedPipe (h, NULL, 0, NULL, &waiting, NULL);
      to_read = min (waiting, (DWORD)max);
    }
  else
    to_read = max;
  
  /* Use read to get CRLF translation */
  nchars = read (fd, buf, to_read);
  
  if (GetFileType (h) == FILE_TYPE_PIPE)
    {
      /* Wake up the reader thread
	 for this process */
      cp = find_child_fd (fd);
      if (cp)
        {
	  if (!SetEvent (cp->char_consumed))
	    DebPrint (("read_child_output.SetEvent failed with "
		       "%lu for fd %ld\n", GetLastError (), fd));
        }
      else
	DebPrint (("read_child_output couldn't find a child with fd %d\n",
		   fd));
    }
  
  return nchars;
}
