/* GnuTLS glue for GNU Emacs.
   Copyright (C) 2010  Free Software Foundation, Inc.

This file is part of GNU Emacs.

GNU Emacs is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.  */

#include <config.h>
#include <errno.h>
#include <setjmp.h>

#include "lisp.h"
#include "process.h"

#ifdef HAVE_GNUTLS
#include <gnutls/gnutls.h>

Lisp_Object Qgnutls_code;
Lisp_Object Qgnutls_anon, Qgnutls_x509pki;
Lisp_Object Qgnutls_e_interrupted, Qgnutls_e_again,
  Qgnutls_e_invalid_session, Qgnutls_e_not_ready_for_handshake;
int global_initialized;

/* The following are for the property list of `gnutls-boot'.  */
Lisp_Object Qgnutls_bootprop_priority;
Lisp_Object Qgnutls_bootprop_trustfiles;
Lisp_Object Qgnutls_bootprop_keyfiles;
Lisp_Object Qgnutls_bootprop_callbacks;
Lisp_Object Qgnutls_bootprop_loglevel;

static void
emacs_gnutls_handshake (struct Lisp_Process *proc)
{
  gnutls_session_t state = proc->gnutls_state;
  int ret;

  if (proc->gnutls_initstage < GNUTLS_STAGE_HANDSHAKE_CANDO)
    return;

  if (proc->gnutls_initstage < GNUTLS_STAGE_TRANSPORT_POINTERS_SET)
    {
      /* This is how GnuTLS takes sockets: as file descriptors passed
         in.  For an Emacs process socket, infd and outfd are the
         same but we use this two-argument version for clarity.  */
      gnutls_transport_set_ptr2 (state,
				 (gnutls_transport_ptr_t) (long) proc->infd,
				 (gnutls_transport_ptr_t) (long) proc->outfd);

      proc->gnutls_initstage = GNUTLS_STAGE_TRANSPORT_POINTERS_SET;
    }

  ret = gnutls_handshake (state);
  proc->gnutls_initstage = GNUTLS_STAGE_HANDSHAKE_TRIED;

  if (ret == GNUTLS_E_SUCCESS)
    {
      /* here we're finally done.  */
      proc->gnutls_initstage = GNUTLS_STAGE_READY;
    }
}

int
emacs_gnutls_write (int fildes, struct Lisp_Process *proc, char *buf,
                    unsigned int nbyte)
{
  register int rtnval, bytes_written;
  gnutls_session_t state = proc->gnutls_state;

  if (proc->gnutls_initstage != GNUTLS_STAGE_READY) {
#ifdef EWOULDBLOCK
    errno = EWOULDBLOCK;
#endif
#ifdef EAGAIN
    errno = EAGAIN;
#endif
    return -1;
  }

  bytes_written = 0;

  while (nbyte > 0)
    {
      rtnval = gnutls_write (state, buf, nbyte);

      if (rtnval < 0)
        {
          if (rtnval == GNUTLS_E_AGAIN || rtnval == GNUTLS_E_INTERRUPTED)
            continue;
          else
            return (bytes_written ? bytes_written : -1);
        }

      buf += rtnval;
      nbyte -= rtnval;
      bytes_written += rtnval;
    }

  return (bytes_written);
}

int
emacs_gnutls_read (int fildes, struct Lisp_Process *proc, char *buf,
                   unsigned int nbyte)
{
  register int rtnval;
  gnutls_session_t state = proc->gnutls_state;

  if (proc->gnutls_initstage != GNUTLS_STAGE_READY)
    {
      emacs_gnutls_handshake (proc);
      return -1;
    }

  rtnval = gnutls_read (state, buf, nbyte);
  if (rtnval >= 0)
    return rtnval;
  else {
    if (rtnval == GNUTLS_E_AGAIN ||
	rtnval == GNUTLS_E_INTERRUPTED)
      return -1;
    else
      return 0;
  }
}

/* convert an integer error to a Lisp_Object; it will be either a
   known symbol like `gnutls_e_interrupted' and `gnutls_e_again' or
   simply the integer value of the error.  GNUTLS_E_SUCCESS is mapped
   to Qt.  */
static Lisp_Object
gnutls_make_error (int error)
{
  switch (error)
    {
    case GNUTLS_E_SUCCESS:
      return Qt;
    case GNUTLS_E_AGAIN:
      return Qgnutls_e_again;
    case GNUTLS_E_INTERRUPTED:
      return Qgnutls_e_interrupted;
    case GNUTLS_E_INVALID_SESSION:
      return Qgnutls_e_invalid_session;
    }

  return make_number (error);
}

DEFUN ("gnutls-get-initstage", Fgnutls_get_initstage, Sgnutls_get_initstage, 1, 1, 0,
       doc: /* Return the GnuTLS init stage of process PROC.
See also `gnutls-boot'.  */)
  (Lisp_Object proc)
{
  CHECK_PROCESS (proc);

  return make_number (GNUTLS_INITSTAGE (proc));
}

DEFUN ("gnutls-errorp", Fgnutls_errorp, Sgnutls_errorp, 1, 1, 0,
       doc: /* Return t if ERROR indicates a GnuTLS problem.
ERROR is an integer or a symbol with an integer `gnutls-code' property.
usage: (gnutls-errorp ERROR)  */)
  (Lisp_Object err)
{
  if (EQ (err, Qt)) return Qnil;

  return Qt;
}

DEFUN ("gnutls-error-fatalp", Fgnutls_error_fatalp, Sgnutls_error_fatalp, 1, 1, 0,
       doc: /* Check if ERROR is fatal.
ERROR is an integer or a symbol with an integer `gnutls-code' property.
usage: (gnutls-error-fatalp ERROR)  */)
  (Lisp_Object err)
{
  Lisp_Object code;

  if (EQ (err, Qt)) return Qnil;

  if (SYMBOLP (err))
    {
      code = Fget (err, Qgnutls_code);
      if (NUMBERP (code))
	{
	  err = code;
	}
      else
	{
	  error ("Symbol has no numeric gnutls-code property");
	}
    }

  if (!NUMBERP (err))
    error ("Not an error symbol or code");

  if (0 == gnutls_error_is_fatal (XINT (err)))
    return Qnil;

  return Qt;
}

DEFUN ("gnutls-error-string", Fgnutls_error_string, Sgnutls_error_string, 1, 1, 0,
       doc: /* Return a description of ERROR.
ERROR is an integer or a symbol with an integer `gnutls-code' property.
usage: (gnutls-error-string ERROR)  */)
  (Lisp_Object err)
{
  Lisp_Object code;

  if (EQ (err, Qt)) return build_string ("Not an error");

  if (SYMBOLP (err))
    {
      code = Fget (err, Qgnutls_code);
      if (NUMBERP (code))
	{
	  err = code;
	}
      else
	{
	  return build_string ("Symbol has no numeric gnutls-code property");
	}
    }

  if (!NUMBERP (err))
    return build_string ("Not an error symbol or code");

  return build_string (gnutls_strerror (XINT (err)));
}

DEFUN ("gnutls-deinit", Fgnutls_deinit, Sgnutls_deinit, 1, 1, 0,
       doc: /* Deallocate GnuTLS resources associated with process PROC.
See also `gnutls-init'.  */)
  (Lisp_Object proc)
{
  gnutls_session_t state;

  CHECK_PROCESS (proc);
  state = XPROCESS (proc)->gnutls_state;

  if (GNUTLS_INITSTAGE (proc) >= GNUTLS_STAGE_INIT)
    {
      gnutls_deinit (state);
      GNUTLS_INITSTAGE (proc) = GNUTLS_STAGE_INIT - 1;
    }

  return Qt;
}

/* Initializes global GnuTLS state to defaults.
Call `gnutls-global-deinit' when GnuTLS usage is no longer needed.
Returns zero on success.  */
static Lisp_Object
gnutls_emacs_global_init (void)
{
  int ret = GNUTLS_E_SUCCESS;

  if (!global_initialized)
    ret = gnutls_global_init ();

  global_initialized = 1;

  return gnutls_make_error (ret);
}

/* Deinitializes global GnuTLS state.
See also `gnutls-global-init'.  */
static Lisp_Object
gnutls_emacs_global_deinit (void)
{
  if (global_initialized)
    gnutls_global_deinit ();

  global_initialized = 0;

  return gnutls_make_error (GNUTLS_E_SUCCESS);
}

static void
gnutls_log_function (int level, const char* string)
{
  message ("gnutls.c: [%d] %s", level, string);
}

static void
gnutls_log_function2 (int level, const char* string, const char* extra)
{
  message ("gnutls.c: [%d] %s %s", level, string, extra);
}

DEFUN ("gnutls-boot", Fgnutls_boot, Sgnutls_boot, 3, 3, 0,
       doc: /* Initialize GnuTLS client for process PROC with TYPE+PROPLIST.
Currently only client mode is supported.  Returns a success/failure
value you can check with `gnutls-errorp'.

TYPE is a symbol, either `gnutls-anon' or `gnutls-x509pki'.
PROPLIST is a property list with the following keys:

:priority is a GnuTLS priority string, defaults to "NORMAL".
:trustfiles is a list of PEM-encoded trust files for `gnutls-x509pki'.
:keyfiles is a list of PEM-encoded key files for `gnutls-x509pki'.
:callbacks is an alist of callback functions (TODO).
:loglevel is the debug level requested from GnuTLS, try 4.

The debug level will be set for this process AND globally for GnuTLS.
So if you set it higher or lower at any point, it affects global
debugging.

Note that the priority is set on the client.  The server does not use
the protocols's priority except for disabling protocols that were not
specified.

Processes must be initialized with this function before other GnuTLS
functions are used.  This function allocates resources which can only
be deallocated by calling `gnutls-deinit' or by calling it again.

Each authentication type may need additional information in order to
work.  For X.509 PKI (`gnutls-x509pki'), you probably need at least
one trustfile (usually a CA bundle).  */)
  (Lisp_Object proc, Lisp_Object type, Lisp_Object proplist)
{
  int ret = GNUTLS_E_SUCCESS;

  int max_log_level = 0;

  /* TODO: GNUTLS_X509_FMT_DER is also an option.  */
  int file_format = GNUTLS_X509_FMT_PEM;

  gnutls_session_t state;
  gnutls_certificate_credentials_t x509_cred;
  gnutls_anon_client_credentials_t anon_cred;
  Lisp_Object global_init;
  char* priority_string_ptr = "NORMAL"; /* default priority string.  */
  Lisp_Object tail;

  /* Placeholders for the property list elements.  */
  Lisp_Object priority_string;
  Lisp_Object trustfiles;
  Lisp_Object keyfiles;
  Lisp_Object callbacks;
  Lisp_Object loglevel;

  CHECK_PROCESS (proc);
  CHECK_SYMBOL (type);
  CHECK_LIST (proplist);

  priority_string = Fplist_get (proplist, Qgnutls_bootprop_priority);
  trustfiles      = Fplist_get (proplist, Qgnutls_bootprop_trustfiles);
  keyfiles        = Fplist_get (proplist, Qgnutls_bootprop_keyfiles);
  callbacks       = Fplist_get (proplist, Qgnutls_bootprop_callbacks);
  loglevel        = Fplist_get (proplist, Qgnutls_bootprop_loglevel);

  state = XPROCESS (proc)->gnutls_state;
  XPROCESS (proc)->gnutls_p = 1;

  if (NUMBERP (loglevel))
    {
      gnutls_global_set_log_function (gnutls_log_function);
      gnutls_global_set_log_level (XINT (loglevel));
      max_log_level = XINT (loglevel);
      XPROCESS (proc)->gnutls_log_level = max_log_level;
    }

  /* always initialize globals.  */
  global_init = gnutls_emacs_global_init ();
  if (! NILP (Fgnutls_errorp (global_init)))
    return global_init;

  /* deinit and free resources.  */
  if (GNUTLS_INITSTAGE (proc) >= GNUTLS_STAGE_CRED_ALLOC)
    {
      GNUTLS_LOG (1, max_log_level, "deallocating credentials");

      if (EQ (type, Qgnutls_x509pki))
	{
          GNUTLS_LOG (2, max_log_level, "deallocating x509 credentials");
          x509_cred = XPROCESS (proc)->gnutls_x509_cred;
          gnutls_certificate_free_credentials (x509_cred);
	}
      else if (EQ (type, Qgnutls_anon))
	{
          GNUTLS_LOG (2, max_log_level, "deallocating anon credentials");
          anon_cred = XPROCESS (proc)->gnutls_anon_cred;
          gnutls_anon_free_client_credentials (anon_cred);
	}
      else
	{
          error ("unknown credential type");
          ret = GNUTLS_EMACS_ERROR_INVALID_TYPE;
	}

      if (GNUTLS_INITSTAGE (proc) >= GNUTLS_STAGE_INIT)
	{
          GNUTLS_LOG (1, max_log_level, "deallocating x509 credentials");
          Fgnutls_deinit (proc);
	}
    }

  GNUTLS_INITSTAGE (proc) = GNUTLS_STAGE_EMPTY;

  GNUTLS_LOG (1, max_log_level, "allocating credentials");

  if (EQ (type, Qgnutls_x509pki))
    {
      GNUTLS_LOG (2, max_log_level, "allocating x509 credentials");
      x509_cred = XPROCESS (proc)->gnutls_x509_cred;
      if (gnutls_certificate_allocate_credentials (&x509_cred) < 0)
        memory_full ();
    }
  else if (EQ (type, Qgnutls_anon))
    {
      GNUTLS_LOG (2, max_log_level, "allocating anon credentials");
      anon_cred = XPROCESS (proc)->gnutls_anon_cred;
      if (gnutls_anon_allocate_client_credentials (&anon_cred) < 0)
        memory_full ();
    }
  else
    {
      error ("unknown credential type");
      ret = GNUTLS_EMACS_ERROR_INVALID_TYPE;
    }

  if (ret < GNUTLS_E_SUCCESS)
    return gnutls_make_error (ret);

  GNUTLS_INITSTAGE (proc) = GNUTLS_STAGE_CRED_ALLOC;

  if (EQ (type, Qgnutls_x509pki))
    {
      for (tail = trustfiles; !NILP (tail); tail = Fcdr (tail))
	{
	  Lisp_Object trustfile = Fcar (tail);
          if (STRINGP (trustfile))
            {
              GNUTLS_LOG2 (1, max_log_level, "setting the trustfile: ",
                           SDATA (trustfile));
              ret = gnutls_certificate_set_x509_trust_file
                (x509_cred,
                 SDATA (trustfile),
                 file_format);
              
              if (ret < GNUTLS_E_SUCCESS)
                return gnutls_make_error (ret);
            }
          else
            {
              error ("Sorry, GnuTLS can't use non-string trustfile %s",
                     trustfile);
            }
        }

      for (tail = keyfiles; !NILP (tail); tail = Fcdr (tail))
	{
	  Lisp_Object keyfile = Fcar (tail);
          if (STRINGP (keyfile))
            {
              GNUTLS_LOG2 (1, max_log_level, "setting the keyfile: ",
                           SDATA (keyfile));
              ret = gnutls_certificate_set_x509_crl_file
                (x509_cred,
                 SDATA (keyfile),
                 file_format);
              
              if (ret < GNUTLS_E_SUCCESS)
                return gnutls_make_error (ret);
            }
          else
            {
              error ("Sorry, GnuTLS can't use non-string keyfile %s",
                     keyfile);
            }
        }
    }

  GNUTLS_INITSTAGE (proc) = GNUTLS_STAGE_FILES;

  GNUTLS_LOG (1, max_log_level, "gnutls_init");

  ret = gnutls_init (&state, GNUTLS_CLIENT);

  if (ret < GNUTLS_E_SUCCESS)
    return gnutls_make_error (ret);

  XPROCESS (proc)->gnutls_state = state;

  GNUTLS_INITSTAGE (proc) = GNUTLS_STAGE_INIT;

  if (STRINGP (priority_string))
    {
      priority_string_ptr = (char*) SDATA (priority_string);
      GNUTLS_LOG2 (1, max_log_level, "got non-default priority string:",
                   priority_string_ptr);
    }
  else
    {
      GNUTLS_LOG2 (1, max_log_level, "using default priority string:",
                   priority_string_ptr);
    }
  
  GNUTLS_LOG (1, max_log_level, "setting the priority string");

  ret = gnutls_priority_set_direct (state,
				    priority_string_ptr,
				    NULL);

  if (ret < GNUTLS_E_SUCCESS)
    return gnutls_make_error (ret);

  GNUTLS_INITSTAGE (proc) = GNUTLS_STAGE_PRIORITY;

  if (EQ (type, Qgnutls_x509pki))
    {
      ret = gnutls_cred_set (state, GNUTLS_CRD_CERTIFICATE, x509_cred);
    }
  else if (EQ (type, Qgnutls_anon))
    {
      ret = gnutls_cred_set (state, GNUTLS_CRD_ANON, anon_cred);
    }
  else
    {
      error ("unknown credential type");
      ret = GNUTLS_EMACS_ERROR_INVALID_TYPE;
    }

  if (ret < GNUTLS_E_SUCCESS)
    return gnutls_make_error (ret);

  XPROCESS (proc)->gnutls_anon_cred = anon_cred;
  XPROCESS (proc)->gnutls_x509_cred = x509_cred;
  XPROCESS (proc)->gnutls_cred_type = type;

  GNUTLS_INITSTAGE (proc) = GNUTLS_STAGE_CRED_SET;

  emacs_gnutls_handshake (XPROCESS (proc));

  return gnutls_make_error (GNUTLS_E_SUCCESS);
}

DEFUN ("gnutls-bye", Fgnutls_bye,
       Sgnutls_bye, 2, 2, 0,
       doc: /* Terminate current GnuTLS connection for process PROC.
The connection should have been initiated using `gnutls-handshake'.

If CONT is not nil the TLS connection gets terminated and further
receives and sends will be disallowed.  If the return value is zero you
may continue using the connection.  If CONT is nil, GnuTLS actually
sends an alert containing a close request and waits for the peer to
reply with the same message.  In order to reuse the connection you
should wait for an EOF from the peer.

This function may also return `gnutls-e-again', or
`gnutls-e-interrupted'.  */)
    (Lisp_Object proc, Lisp_Object cont)
{
  gnutls_session_t state;
  int ret;

  CHECK_PROCESS (proc);

  state = XPROCESS (proc)->gnutls_state;

  ret = gnutls_bye (state,
                    NILP (cont) ? GNUTLS_SHUT_RDWR : GNUTLS_SHUT_WR);

  return gnutls_make_error (ret);
}

void
syms_of_gnutls (void)
{
  global_initialized = 0;

  Qgnutls_code = intern_c_string ("gnutls-code");
  staticpro (&Qgnutls_code);

  Qgnutls_anon = intern_c_string ("gnutls-anon");
  staticpro (&Qgnutls_anon);

  Qgnutls_x509pki = intern_c_string ("gnutls-x509pki");
  staticpro (&Qgnutls_x509pki);

  Qgnutls_bootprop_priority = intern_c_string (":priority");
  staticpro (&Qgnutls_bootprop_priority);

  Qgnutls_bootprop_trustfiles = intern_c_string (":trustfiles");
  staticpro (&Qgnutls_bootprop_trustfiles);

  Qgnutls_bootprop_keyfiles = intern_c_string (":keyfiles");
  staticpro (&Qgnutls_bootprop_keyfiles);

  Qgnutls_bootprop_callbacks = intern_c_string (":callbacks");
  staticpro (&Qgnutls_bootprop_callbacks);

  Qgnutls_bootprop_loglevel = intern_c_string (":loglevel");
  staticpro (&Qgnutls_bootprop_loglevel);

  Qgnutls_e_interrupted = intern_c_string ("gnutls-e-interrupted");
  staticpro (&Qgnutls_e_interrupted);
  Fput (Qgnutls_e_interrupted, Qgnutls_code,
        make_number (GNUTLS_E_INTERRUPTED));

  Qgnutls_e_again = intern_c_string ("gnutls-e-again");
  staticpro (&Qgnutls_e_again);
  Fput (Qgnutls_e_again, Qgnutls_code,
        make_number (GNUTLS_E_AGAIN));

  Qgnutls_e_invalid_session = intern_c_string ("gnutls-e-invalid-session");
  staticpro (&Qgnutls_e_invalid_session);
  Fput (Qgnutls_e_invalid_session, Qgnutls_code,
        make_number (GNUTLS_E_INVALID_SESSION));

  Qgnutls_e_not_ready_for_handshake =
    intern_c_string ("gnutls-e-not-ready-for-handshake");
  staticpro (&Qgnutls_e_not_ready_for_handshake);
  Fput (Qgnutls_e_not_ready_for_handshake, Qgnutls_code,
        make_number (GNUTLS_E_APPLICATION_ERROR_MIN));

  defsubr (&Sgnutls_get_initstage);
  defsubr (&Sgnutls_errorp);
  defsubr (&Sgnutls_error_fatalp);
  defsubr (&Sgnutls_error_string);
  defsubr (&Sgnutls_boot);
  defsubr (&Sgnutls_deinit);
  defsubr (&Sgnutls_bye);
}
#endif
