/* Session management module for systems which understand the X Session
   management protocol.
   Copyright (C) 2002, 2002 Free Software Foundation, Inc.

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
Boston, MA 02111-1307, USA.  */

#include <config.h>

#ifdef HAVE_X_SM

#include <X11/SM/SMlib.h>
#ifdef HAVE_STRING_H
#include <string.h>
#else
#ifdef HAVE_STRINGS_H
#include <strings.h>
#endif
#endif

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

#include <sys/param.h>
#include <stdio.h>

#include "systime.h"
#include "sysselect.h"
#include "lisp.h"
#include "termhooks.h"
#include "termopts.h"

#ifndef MAXPATHLEN
#define MAXPATHLEN 1024
#endif /* not MAXPATHLEN */


/* The user login name.  */

extern Lisp_Object Vuser_login_name;

/* This is the event used when save_session occurs */

static struct input_event emacs_event;

/* The descriptor that we use to check for data from the session manager. */

static int ice_fd = -1;

/* A flag that says if we are in shutdown interactions or not. */

static int doing_interact = False;

/* The session manager object for the session manager connection */

static SmcConn smc_conn;

/* The client session id for this session */
static char *client_id;

/* The full path name to the Emacs program */
static char *emacs_program;

/* The client session id for this session as a lisp object. */

Lisp_Object Vx_session_id;

/* The id we had the previous session.  This is only available if we
   have been started by the session manager with SMID_OPT. */

Lisp_Object Vx_session_previous_id;

/* The option we tell the session manager to start Emacs with when
   restarting Emacs.  The client_id is appended. */

#define SMID_OPT "--smid="


/* Handle any messages from the session manager.  If no connection is
   open to a session manager, just return 0.
   Otherwise returns the number of events stored in buffer BUFP,
   which can hold up to *NUMCHARS characters.  At most one event is
   stored, an save_session_event. */
int
x_session_check_input (bufp, numchars)
     struct input_event *bufp;
     int *numchars;
{
  SELECT_TYPE read_fds;
  EMACS_TIME tmout;
  
  if (ice_fd == -1) return 0;
  
  FD_ZERO (&read_fds);
  FD_SET (ice_fd, &read_fds);
      
  tmout.tv_sec = 0;
  tmout.tv_usec = 0;
  
  /* Reset this so wo can check kind after callbacks have been called by
     IceProcessMessages.  The smc_interact_CB sets the kind to
     save_session_event, but we don't know beforehand if that callback
     will be called. */
  emacs_event.kind = no_event;

  if (select (ice_fd+1, &read_fds,
              (SELECT_TYPE *)0, (SELECT_TYPE *)0, &tmout) < 0)
    {
      ice_fd = -1;
      return 0;
    }
  

  if (FD_ISSET (ice_fd, &read_fds))
    IceProcessMessages (SmcGetIceConnection (smc_conn),
                        (IceReplyWaitInfo *)0, (Bool *)0);

  
  /* Check if smc_interact_CB was called and we shall generate a
     save_session event. */
  if (*numchars > 0 && emacs_event.kind != no_event)
    {
      bcopy (&emacs_event, bufp, sizeof (struct input_event));
      bufp++;
      (*numchars)--;

      return 1;
    }

  return 0;
}

/* Return non-zero if we have a connection to a session manager.*/
int
x_session_have_connection ()
{
  return ice_fd != -1;
}

/* This is called when the session manager says it is OK to interact with the
   user.  Here we set the kind to save_session so an event is generated.
   Then lisp code can interact with the user. */
static void
smc_interact_CB (smcConn, clientData)
     SmcConn smcConn;
     SmPointer clientData;
{
  doing_interact = True;
  emacs_event.kind = save_session_event;
}

/* This is called when the session manager tells us to save ourself.
   We set the required properties so the session manager can restart us,
   plus the current working directory property (not mandatory) so we
   are started in the correct directory.

   If this is a shutdown and we can request to interact with the user,
   we do so, because we don't know what the lisp code might do. */
static void
smc_save_yourself_CB (smcConn,
                      clientData,
                      saveType,
                      shutdown,
                      interactStyle,
                      fast)
     SmcConn smcConn;
     SmPointer clientData;
     int saveType;
     Bool shutdown;
     int interactStyle;
     Bool fast;
{
#define NR_PROPS 5
  
  SmProp *props[NR_PROPS];
  SmProp prop_ptr[NR_PROPS];
  
  SmPropValue values[20];
  int val_idx = 0;
  int props_idx = 0;
  
  char cwd[MAXPATHLEN+1];
  char *smid_opt;

  /* How to start a new instance of Emacs */
  props[props_idx] = &prop_ptr[props_idx];
  props[props_idx]->name = SmCloneCommand;
  props[props_idx]->type = SmLISTofARRAY8;
  props[props_idx]->num_vals = 1;
  props[props_idx]->vals = &values[val_idx++];
  props[props_idx]->vals[0].length = strlen (emacs_program);
  props[props_idx]->vals[0].value = emacs_program;
  ++props_idx;

  /* The name of the program */
  props[props_idx] = &prop_ptr[props_idx];
  props[props_idx]->name = SmProgram;
  props[props_idx]->type = SmARRAY8;
  props[props_idx]->num_vals = 1;
  props[props_idx]->vals = &values[val_idx++];
  props[props_idx]->vals[0].length = strlen (XSTRING (Vinvocation_name)->data);
  props[props_idx]->vals[0].value = XSTRING (Vinvocation_name)->data;
  ++props_idx;
  
  /* How to restart Emacs (i.e.: /path/to/emacs --smid=xxxx). */
  props[props_idx] = &prop_ptr[props_idx];
  props[props_idx]->name = SmRestartCommand;
  props[props_idx]->type = SmLISTofARRAY8;
  props[props_idx]->num_vals = 2; /* 2 values: /path/to/emacs, --smid=xxx */
  props[props_idx]->vals = &values[val_idx];
  props[props_idx]->vals[0].length = strlen (emacs_program);
  props[props_idx]->vals[0].value = emacs_program;

  smid_opt = xmalloc (strlen (SMID_OPT) + strlen (client_id) + 1);
  strcpy (smid_opt, SMID_OPT);
  strcat (smid_opt, client_id);
  
  props[props_idx]->vals[1].length = strlen (smid_opt);
  props[props_idx]->vals[1].value = smid_opt;
  val_idx += 2;
  ++props_idx;

  /* User id */
  props[props_idx] = &prop_ptr[props_idx];
  props[props_idx]->name = SmUserID;
  props[props_idx]->type = SmARRAY8;
  props[props_idx]->num_vals = 1;
  props[props_idx]->vals = &values[val_idx++];
  props[props_idx]->vals[0].length = strlen (XSTRING (Vuser_login_name)->data);
  props[props_idx]->vals[0].value = XSTRING (Vuser_login_name)->data;
  ++props_idx;

  /* The current directory property, not mandatory */
#ifdef HAVE_GETCWD
  if (getcwd (cwd, MAXPATHLEN+1) != 0)
#else
  if (getwd (cwd) != 0)
#endif
    {
      props[props_idx] = &prop_ptr[props_idx];
      props[props_idx]->name = SmCurrentDirectory;
      props[props_idx]->type = SmARRAY8;
      props[props_idx]->num_vals = 1;
      props[props_idx]->vals = &values[val_idx++];
      props[props_idx]->vals[0].length = strlen (cwd);
      props[props_idx]->vals[0].value = cwd;
      ++props_idx;
    }
  
  
  SmcSetProperties (smcConn, props_idx, props);

  xfree (smid_opt);

  /* See if we maybe shall interact with the user. */
  if (interactStyle != SmInteractStyleAny
      || ! shutdown
      || saveType == SmSaveLocal
      || ! SmcInteractRequest (smcConn, SmDialogNormal, smc_interact_CB, 0))
    {
      /* No interaction, we are done saving ourself. */
      SmcSaveYourselfDone (smcConn, True);
    }
}

/* According to the SM specification, this shall close the connection */
static void
smc_die_CB (smcConn, clientData)
     SmcConn smcConn;
     SmPointer clientData;
{
  SmcCloseConnection (smcConn, 0, 0);
  ice_fd = -1;
}

/* We don't use the next two but they are mandatory, leave them empty.
   According to the SM specification, we should not interact with the
   user between smc_save_yourself_CB is called and until smc_save_complete_CB
   is called.  It seems like a lot of job to implement this and it doesn't
   even seem necessary. */
static void
smc_save_complete_CB (smcConn, clientData)
     SmcConn smcConn;
     SmPointer clientData;
{
  /* Empty */
}

static void
smc_shutdown_cancelled_CB (smcConn, clientData)
     SmcConn smcConn;
     SmPointer clientData;
{
  /* Empty */
}

/* Error handlers for SM and ICE.  We don't wan't to exit Emacs just
   because there is some error in the session management. */
static void
smc_error_handler (smcConn,
                   swap,
                   offendingMinorOpcode,
                   offendingSequence,
                   errorClass,
                   severity,
                   values)
     SmcConn smcConn;
     Bool swap;
     int offendingMinorOpcode;
     unsigned long offendingSequence;
     int errorClass;
     int severity;
     SmPointer values;
{
  /* Empty */
}

static void
ice_error_handler (iceConn,
                   swap,
                   offendingMinorOpcode,
                   offendingSequence,
                   errorClass,
                   severity,
                   values)
     IceConn iceConn;
     Bool swap;
     int offendingMinorOpcode;
     unsigned long offendingSequence;
     int errorClass;
     int severity;
     IcePointer values;
{
  /* Empty */
}


static void
ice_io_error_handler (iceConn)
     IceConn iceConn;
{
  /* Connection probably gone. */
  ice_fd = -1;
}

/* This is called when the ICE connection is created or closed.  The SM library
   uses ICE as it transport protocol. */
static void
ice_conn_watch_CB (iceConn, clientData, opening, watchData)
     IceConn iceConn;
     IcePointer clientData;
     Bool opening;
     IcePointer *watchData;
{
  if (! opening)
    {
      ice_fd = -1;
      return;
    }
  
  ice_fd = IceConnectionNumber (iceConn);
#ifndef F_SETOWN_BUG
#ifdef F_SETOWN
#ifdef F_SETOWN_SOCK_NEG
  /* stdin is a socket here */
  fcntl (ice_fd, F_SETOWN, -getpid ());
#else /* ! defined (F_SETOWN_SOCK_NEG) */
  fcntl (ice_fd, F_SETOWN, getpid ());
#endif /* ! defined (F_SETOWN_SOCK_NEG) */
#endif /* ! defined (F_SETOWN) */
#endif /* F_SETOWN_BUG */

#ifdef SIGIO
  if (interrupt_input)
    init_sigio (ice_fd);
#endif /* ! defined (SIGIO) */
}

/* Try to open a connection to the session manager. */
void
x_session_initialize ()
{
#define SM_ERRORSTRING_LEN 512
  char errorstring[SM_ERRORSTRING_LEN];
  char* previous_id = NULL;
  SmcCallbacks callbacks;
  int  name_len = 0;
  
  /* Check if we where started by the session manager.  If so, we will
     have a previous id. */
  if (! EQ (Vx_session_previous_id, Qnil) && STRINGP (Vx_session_previous_id))
    previous_id = XSTRING (Vx_session_previous_id)->data;

  /* Construct the path to the Emacs program. */
  if (! EQ (Vinvocation_directory, Qnil))
    name_len += strlen (XSTRING (Vinvocation_directory)->data);
  name_len += strlen (XSTRING (Vinvocation_name)->data);

  /* This malloc will not be freed, but it is only done once, and hopefully
     not very large  */
  emacs_program = xmalloc (name_len + 1);
  emacs_program[0] = '\0';

  if (! EQ (Vinvocation_directory, Qnil))
    strcpy (emacs_program, XSTRING (Vinvocation_directory)->data);
  strcat (emacs_program, XSTRING (Vinvocation_name)->data);
  
  /* The SM protocol says all callbacks are mandatory, so set up all
     here and in the mask passed to SmcOpenConnection */
  callbacks.save_yourself.callback = smc_save_yourself_CB;
  callbacks.save_yourself.client_data = 0;
  callbacks.die.callback = smc_die_CB;
  callbacks.die.client_data = 0;
  callbacks.save_complete.callback = smc_save_complete_CB;
  callbacks.save_complete.client_data = 0;
  callbacks.shutdown_cancelled.callback = smc_shutdown_cancelled_CB;
  callbacks.shutdown_cancelled.client_data = 0;

  /* Set error handlers. */
  SmcSetErrorHandler (smc_error_handler);
  IceSetErrorHandler (ice_error_handler);
  IceSetIOErrorHandler (ice_io_error_handler);

  /* Install callback for when connection status changes. */
  IceAddConnectionWatch (ice_conn_watch_CB, 0);

  /* Open the connection to the session manager.  A failure is not
     critical, it usualy means that no session manager is running.
     The errorstring is here for debugging. */
  smc_conn = SmcOpenConnection (NULL, NULL, 1, 0,
                                (SmcSaveYourselfProcMask|
                                 SmcDieProcMask|
                                 SmcSaveCompleteProcMask|
                                 SmcShutdownCancelledProcMask),
                                &callbacks,
                                previous_id,
                                &client_id,
                                SM_ERRORSTRING_LEN,
                                errorstring);

  if (smc_conn != 0)
    Vx_session_id = make_string (client_id, strlen (client_id));
}


DEFUN ("handle-save-session", Fhandle_save_session,
       Shandle_save_session, 1, 1, "e",
       doc: /* Handle the save_yourself event from a session manager.
A session manager can tell Emacs that the window system is shutting down 
by sending Emacs a save_yourself message.  Emacs executes this function when
such an event occurs.  This function then executes `emacs-session-save'.
After that, this function informs the session manager that it can continue
or abort shutting down the window system depending on the return value
from `emacs-session-save'  If the return value is non-nil the session manager
is told to abort the window system shutdown.

Do not call this function yourself. */)
     (event)
     Lisp_Object event;
{
  /* Check doing_interact so that we don't do anything if someone called
     this at the wrong time. */
  if (doing_interact)
    {
      Bool cancel_shutdown = False;

      cancel_shutdown = ! EQ (call0 (intern ("emacs-session-save")), Qnil);

      SmcInteractDone (smc_conn, cancel_shutdown);
      SmcSaveYourselfDone (smc_conn, True);

      doing_interact = False;
    }

  return Qnil;
}


/***********************************************************************
			    Initialization
 ***********************************************************************/
void
syms_of_xsmfns ()
{
  DEFVAR_LISP ("x-session-id", &Vx_session_id,
    doc: /* The session id Emacs got from the session manager for this session.
Changing the value does not change the session id used by Emacs.
The value is nil if no session manager is running.
See also `x-session-previous-id', `emacs-save-session-functions',
`emacs-session-save' and `emacs-session-restore'." */);
  Vx_session_id = Qnil;

  DEFVAR_LISP ("x-session-previous-id", &Vx_session_previous_id,
    doc: /* The previous session id Emacs got from session manager.
If Emacs is running on a window system that has a session manager, the 
session manager gives Emacs a session id.  It is feasible for Emacs lisp 
code to use the session id to save configuration in, for example, a file 
with a file name based on the session id.  If Emacs is running when the 
window system is shut down, the session manager remembers that Emacs was 
running and saves the session id Emacs had.

When the window system is started again, the session manager restarts 
Emacs and hands Emacs the session id it had the last time it was 
running.  This is now the previous session id and the value of this 
variable.  If configuration was saved in a file as stated above, the 
previous session id shall be used to reconstruct the file name.

The session id Emacs has while it is running is in the variable 
`x-session-id'.  The value of this variable and `x-session-id' may be the
same, depending on how the session manager works.

See also `emacs-save-session-functions', `emacs-session-save' and
`emacs-session-restore'." */);
  Vx_session_previous_id = Qnil;
  
  defsubr (&Shandle_save_session);
}

#endif /* HAVE_X_SM */
