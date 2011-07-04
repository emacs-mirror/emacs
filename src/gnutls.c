/* GnuTLS glue for GNU Emacs.
   Copyright (C) 2010-2011  Free Software Foundation, Inc.

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

#ifdef WINDOWSNT
#include <windows.h>
#include "w32.h"
#endif

static int
emacs_gnutls_handle_error (gnutls_session_t, int err);

static Lisp_Object Qgnutls_dll;
static Lisp_Object Qgnutls_log_level;
static Lisp_Object Qgnutls_code;
static Lisp_Object Qgnutls_anon, Qgnutls_x509pki;
static Lisp_Object Qgnutls_e_interrupted, Qgnutls_e_again,
  Qgnutls_e_invalid_session, Qgnutls_e_not_ready_for_handshake;
static int gnutls_global_initialized;

/* The following are for the property list of `gnutls-boot'.  */
static Lisp_Object Qgnutls_bootprop_priority;
static Lisp_Object Qgnutls_bootprop_trustfiles;
static Lisp_Object Qgnutls_bootprop_keylist;
static Lisp_Object Qgnutls_bootprop_crlfiles;
static Lisp_Object Qgnutls_bootprop_callbacks;
static Lisp_Object Qgnutls_bootprop_loglevel;
static Lisp_Object Qgnutls_bootprop_hostname;
static Lisp_Object Qgnutls_bootprop_verify_flags;
static Lisp_Object Qgnutls_bootprop_verify_hostname_error;

/* Callback keys for `gnutls-boot'.  Unused currently.  */
static Lisp_Object Qgnutls_bootprop_callbacks_verify;

static void gnutls_log_function (int, const char *);
static void gnutls_log_function2 (int, const char*, const char*);


#ifdef WINDOWSNT

/* Macro for defining functions that will be loaded from the GnuTLS DLL.  */
#define DEF_GNUTLS_FN(rettype,func,args) static rettype (FAR CDECL *fn_##func)args

/* Macro for loading GnuTLS functions from the library.  */
#define LOAD_GNUTLS_FN(lib,func) {					\
    fn_##func = (void *) GetProcAddress (lib, #func);			\
    if (!fn_##func) return 0;						\
  }

DEF_GNUTLS_FN (gnutls_alert_description_t, gnutls_alert_get,
               (gnutls_session_t));
DEF_GNUTLS_FN (const char *, gnutls_alert_get_name,
               (gnutls_alert_description_t));
DEF_GNUTLS_FN (int, gnutls_alert_send_appropriate, (gnutls_session_t, int));
DEF_GNUTLS_FN (int, gnutls_anon_allocate_client_credentials,
               (gnutls_anon_client_credentials_t *));
DEF_GNUTLS_FN (void, gnutls_anon_free_client_credentials,
               (gnutls_anon_client_credentials_t));
DEF_GNUTLS_FN (int, gnutls_bye, (gnutls_session_t, gnutls_close_request_t));
DEF_GNUTLS_FN (int, gnutls_certificate_allocate_credentials,
               (gnutls_certificate_credentials_t *));
DEF_GNUTLS_FN (void, gnutls_certificate_free_credentials,
               (gnutls_certificate_credentials_t));
DEF_GNUTLS_FN (const gnutls_datum_t *, gnutls_certificate_get_peers,
               (gnutls_session_t, unsigned int *));
DEF_GNUTLS_FN (void, gnutls_certificate_set_verify_flags,
               (gnutls_certificate_credentials_t, unsigned int));
DEF_GNUTLS_FN (int, gnutls_certificate_set_x509_crl_file,
               (gnutls_certificate_credentials_t, const char *,
                gnutls_x509_crt_fmt_t));
DEF_GNUTLS_FN (int, gnutls_certificate_set_x509_key_file,
               (gnutls_certificate_credentials_t, const char *, const char *,
                gnutls_x509_crt_fmt_t));
DEF_GNUTLS_FN (int, gnutls_certificate_set_x509_trust_file,
               (gnutls_certificate_credentials_t, const char *,
                gnutls_x509_crt_fmt_t));
DEF_GNUTLS_FN (gnutls_certificate_type_t, gnutls_certificate_type_get,
               (gnutls_session_t));
DEF_GNUTLS_FN (int, gnutls_certificate_verify_peers2,
               (gnutls_session_t, unsigned int *));
DEF_GNUTLS_FN (int, gnutls_credentials_set,
               (gnutls_session_t, gnutls_credentials_type_t, void *));
DEF_GNUTLS_FN (void, gnutls_deinit, (gnutls_session_t));
DEF_GNUTLS_FN (int, gnutls_error_is_fatal, (int));
DEF_GNUTLS_FN (int, gnutls_global_init, (void));
DEF_GNUTLS_FN (void, gnutls_global_set_log_function, (gnutls_log_func));
DEF_GNUTLS_FN (void, gnutls_global_set_log_level, (int));
DEF_GNUTLS_FN (void, gnutls_global_set_mem_functions,
	       (gnutls_alloc_function, gnutls_alloc_function,
		gnutls_is_secure_function, gnutls_realloc_function,
		gnutls_free_function));
DEF_GNUTLS_FN (int, gnutls_handshake, (gnutls_session_t));
DEF_GNUTLS_FN (int, gnutls_init, (gnutls_session_t *, gnutls_connection_end_t));
DEF_GNUTLS_FN (int, gnutls_priority_set_direct,
               (gnutls_session_t, const char *, const char **));
DEF_GNUTLS_FN (size_t, gnutls_record_check_pending, (gnutls_session_t));
DEF_GNUTLS_FN (ssize_t, gnutls_record_recv, (gnutls_session_t, void *, size_t));
DEF_GNUTLS_FN (ssize_t, gnutls_record_send,
               (gnutls_session_t, const void *, size_t));
DEF_GNUTLS_FN (const char *, gnutls_strerror, (int));
DEF_GNUTLS_FN (void, gnutls_transport_set_errno, (gnutls_session_t, int));
DEF_GNUTLS_FN (void, gnutls_transport_set_lowat, (gnutls_session_t, int));
DEF_GNUTLS_FN (void, gnutls_transport_set_ptr2,
               (gnutls_session_t, gnutls_transport_ptr_t,
                gnutls_transport_ptr_t));
DEF_GNUTLS_FN (void, gnutls_transport_set_pull_function,
               (gnutls_session_t, gnutls_pull_func));
DEF_GNUTLS_FN (void, gnutls_transport_set_push_function,
               (gnutls_session_t, gnutls_push_func));
DEF_GNUTLS_FN (int, gnutls_x509_crt_check_hostname,
               (gnutls_x509_crt_t, const char *));
DEF_GNUTLS_FN (void, gnutls_x509_crt_deinit, (gnutls_x509_crt_t));
DEF_GNUTLS_FN (int, gnutls_x509_crt_import,
               (gnutls_x509_crt_t, const gnutls_datum_t *,
                gnutls_x509_crt_fmt_t));
DEF_GNUTLS_FN (int, gnutls_x509_crt_init, (gnutls_x509_crt_t *));

static int
init_gnutls_functions (Lisp_Object libraries)
{
  HMODULE library;

  if (!(library = w32_delayed_load (libraries, Qgnutls_dll)))
    {
      GNUTLS_LOG (1, 1, "GnuTLS library not found");
      return 0;
    }

  LOAD_GNUTLS_FN (library, gnutls_alert_get);
  LOAD_GNUTLS_FN (library, gnutls_alert_get_name);
  LOAD_GNUTLS_FN (library, gnutls_alert_send_appropriate);
  LOAD_GNUTLS_FN (library, gnutls_anon_allocate_client_credentials);
  LOAD_GNUTLS_FN (library, gnutls_anon_free_client_credentials);
  LOAD_GNUTLS_FN (library, gnutls_bye);
  LOAD_GNUTLS_FN (library, gnutls_certificate_allocate_credentials);
  LOAD_GNUTLS_FN (library, gnutls_certificate_free_credentials);
  LOAD_GNUTLS_FN (library, gnutls_certificate_get_peers);
  LOAD_GNUTLS_FN (library, gnutls_certificate_set_verify_flags);
  LOAD_GNUTLS_FN (library, gnutls_certificate_set_x509_crl_file);
  LOAD_GNUTLS_FN (library, gnutls_certificate_set_x509_key_file);
  LOAD_GNUTLS_FN (library, gnutls_certificate_set_x509_trust_file);
  LOAD_GNUTLS_FN (library, gnutls_certificate_type_get);
  LOAD_GNUTLS_FN (library, gnutls_certificate_verify_peers2);
  LOAD_GNUTLS_FN (library, gnutls_credentials_set);
  LOAD_GNUTLS_FN (library, gnutls_deinit);
  LOAD_GNUTLS_FN (library, gnutls_error_is_fatal);
  LOAD_GNUTLS_FN (library, gnutls_global_init);
  LOAD_GNUTLS_FN (library, gnutls_global_set_log_function);
  LOAD_GNUTLS_FN (library, gnutls_global_set_log_level);
  LOAD_GNUTLS_FN (library, gnutls_global_set_mem_functions);
  LOAD_GNUTLS_FN (library, gnutls_handshake);
  LOAD_GNUTLS_FN (library, gnutls_init);
  LOAD_GNUTLS_FN (library, gnutls_priority_set_direct);
  LOAD_GNUTLS_FN (library, gnutls_record_check_pending);
  LOAD_GNUTLS_FN (library, gnutls_record_recv);
  LOAD_GNUTLS_FN (library, gnutls_record_send);
  LOAD_GNUTLS_FN (library, gnutls_strerror);
  LOAD_GNUTLS_FN (library, gnutls_transport_set_errno);
  LOAD_GNUTLS_FN (library, gnutls_transport_set_lowat);
  LOAD_GNUTLS_FN (library, gnutls_transport_set_ptr2);
  LOAD_GNUTLS_FN (library, gnutls_transport_set_pull_function);
  LOAD_GNUTLS_FN (library, gnutls_transport_set_push_function);
  LOAD_GNUTLS_FN (library, gnutls_x509_crt_check_hostname);
  LOAD_GNUTLS_FN (library, gnutls_x509_crt_deinit);
  LOAD_GNUTLS_FN (library, gnutls_x509_crt_import);
  LOAD_GNUTLS_FN (library, gnutls_x509_crt_init);

  GNUTLS_LOG2 (1, 1, "GnuTLS library loaded:",
               SDATA (Fget (Qgnutls_dll, QCloaded_from)));
  return 1;
}

#else /* !WINDOWSNT */

#define fn_gnutls_alert_get			gnutls_alert_get
#define fn_gnutls_alert_get_name		gnutls_alert_get_name
#define fn_gnutls_alert_send_appropriate	gnutls_alert_send_appropriate
#define fn_gnutls_anon_allocate_client_credentials gnutls_anon_allocate_client_credentials
#define fn_gnutls_anon_free_client_credentials	gnutls_anon_free_client_credentials
#define fn_gnutls_bye				gnutls_bye
#define fn_gnutls_certificate_allocate_credentials gnutls_certificate_allocate_credentials
#define fn_gnutls_certificate_free_credentials	gnutls_certificate_free_credentials
#define fn_gnutls_certificate_get_peers		gnutls_certificate_get_peers
#define fn_gnutls_certificate_set_verify_flags	gnutls_certificate_set_verify_flags
#define fn_gnutls_certificate_set_x509_crl_file	gnutls_certificate_set_x509_crl_file
#define fn_gnutls_certificate_set_x509_key_file gnutls_certificate_set_x509_key_file
#define fn_gnutls_certificate_set_x509_trust_file gnutls_certificate_set_x509_trust_file
#define fn_gnutls_certificate_type_get		gnutls_certificate_type_get
#define fn_gnutls_certificate_verify_peers2	gnutls_certificate_verify_peers2
#define fn_gnutls_credentials_set		gnutls_credentials_set
#define fn_gnutls_deinit			gnutls_deinit
#define fn_gnutls_error_is_fatal		gnutls_error_is_fatal
#define fn_gnutls_global_init			gnutls_global_init
#define fn_gnutls_global_set_log_function	gnutls_global_set_log_function
#define fn_gnutls_global_set_log_level		gnutls_global_set_log_level
#define fn_gnutls_global_set_mem_functions	gnutls_global_set_mem_functions
#define fn_gnutls_handshake			gnutls_handshake
#define fn_gnutls_init				gnutls_init
#define fn_gnutls_priority_set_direct		gnutls_priority_set_direct
#define fn_gnutls_record_check_pending		gnutls_record_check_pending
#define fn_gnutls_record_recv			gnutls_record_recv
#define fn_gnutls_record_send			gnutls_record_send
#define fn_gnutls_strerror			gnutls_strerror
#define fn_gnutls_transport_set_errno		gnutls_transport_set_errno
#define fn_gnutls_transport_set_ptr2		gnutls_transport_set_ptr2
#define fn_gnutls_x509_crt_check_hostname	gnutls_x509_crt_check_hostname
#define fn_gnutls_x509_crt_deinit		gnutls_x509_crt_deinit
#define fn_gnutls_x509_crt_import		gnutls_x509_crt_import
#define fn_gnutls_x509_crt_init			gnutls_x509_crt_init

#endif /* !WINDOWSNT */


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

static int
emacs_gnutls_handshake (struct Lisp_Process *proc)
{
  gnutls_session_t state = proc->gnutls_state;
  int ret;

  if (proc->gnutls_initstage < GNUTLS_STAGE_HANDSHAKE_CANDO)
    return -1;

  if (proc->gnutls_initstage < GNUTLS_STAGE_TRANSPORT_POINTERS_SET)
    {
#ifdef WINDOWSNT
      /* On W32 we cannot transfer socket handles between different runtime
         libraries, so we tell GnuTLS to use our special push/pull
         functions.  */
      fn_gnutls_transport_set_ptr2 (state,
                                    (gnutls_transport_ptr_t) proc,
                                    (gnutls_transport_ptr_t) proc);
      fn_gnutls_transport_set_push_function (state, &emacs_gnutls_push);
      fn_gnutls_transport_set_pull_function (state, &emacs_gnutls_pull);

      /* For non blocking sockets or other custom made pull/push
         functions the gnutls_transport_set_lowat must be called, with
         a zero low water mark value. (GnuTLS 2.10.4 documentation)

         (Note: this is probably not strictly necessary as the lowat
          value is only used when no custom pull/push functions are
          set.)  */
      fn_gnutls_transport_set_lowat (state, 0);
#else
      /* This is how GnuTLS takes sockets: as file descriptors passed
         in.  For an Emacs process socket, infd and outfd are the
         same but we use this two-argument version for clarity.  */
      fn_gnutls_transport_set_ptr2 (state,
                                    (gnutls_transport_ptr_t) (long) proc->infd,
                                    (gnutls_transport_ptr_t) (long) proc->outfd);
#endif

      proc->gnutls_initstage = GNUTLS_STAGE_TRANSPORT_POINTERS_SET;
    }

  do
    {
      ret = fn_gnutls_handshake (state);
      emacs_gnutls_handle_error (state, ret);
    }
  while (ret < 0 && fn_gnutls_error_is_fatal (ret) == 0);

  proc->gnutls_initstage = GNUTLS_STAGE_HANDSHAKE_TRIED;

  if (ret == GNUTLS_E_SUCCESS)
    {
      /* Here we're finally done.  */
      proc->gnutls_initstage = GNUTLS_STAGE_READY;
    }
  else
    {
      fn_gnutls_alert_send_appropriate (state, ret);
    }
  return ret;
}

int
emacs_gnutls_record_check_pending (gnutls_session_t state)
{
  return fn_gnutls_record_check_pending (state);
}

void
emacs_gnutls_transport_set_errno (gnutls_session_t state, int err)
{
  fn_gnutls_transport_set_errno (state, err);
}

EMACS_INT
emacs_gnutls_write (struct Lisp_Process *proc, const char *buf, EMACS_INT nbyte)
{
  ssize_t rtnval = 0;
  EMACS_INT bytes_written;
  gnutls_session_t state = proc->gnutls_state;

  if (proc->gnutls_initstage != GNUTLS_STAGE_READY) {
#ifdef EWOULDBLOCK
    errno = EWOULDBLOCK;
#endif
#ifdef EAGAIN
    errno = EAGAIN;
#endif
    return 0;
  }

  bytes_written = 0;

  while (nbyte > 0)
    {
      rtnval = fn_gnutls_record_send (state, buf, nbyte);

      if (rtnval < 0)
        {
          if (rtnval == GNUTLS_E_AGAIN || rtnval == GNUTLS_E_INTERRUPTED)
            continue;
          else
            break;
        }

      buf += rtnval;
      nbyte -= rtnval;
      bytes_written += rtnval;
    }

  emacs_gnutls_handle_error (state, rtnval);
  return (bytes_written);
}

EMACS_INT
emacs_gnutls_read (struct Lisp_Process *proc, char *buf, EMACS_INT nbyte)
{
  ssize_t rtnval;
  gnutls_session_t state = proc->gnutls_state;

  if (proc->gnutls_initstage != GNUTLS_STAGE_READY)
    {
      emacs_gnutls_handshake (proc);
      return -1;
    }
  rtnval = fn_gnutls_record_recv (state, buf, nbyte);
  if (rtnval >= 0)
    return rtnval;
  else if (emacs_gnutls_handle_error (state, rtnval) == 0)
    /* non-fatal error */
    return -1;
  else {
    /* a fatal error occured */
    return 0;
  }
}

/* report a GnuTLS error to the user.
   Returns zero if the error code was successfully handled. */
static int
emacs_gnutls_handle_error (gnutls_session_t session, int err)
{
  Lisp_Object gnutls_log_level = Fsymbol_value (Qgnutls_log_level);
  int max_log_level = 0;

  int ret;
  const char *str;

  /* TODO: use a Lisp_Object generated by gnutls_make_error?  */
  if (err >= 0)
    return 0;

  if (NUMBERP (gnutls_log_level))
    max_log_level = XINT (gnutls_log_level);

  /* TODO: use gnutls-error-fatalp and gnutls-error-string.  */

  str = fn_gnutls_strerror (err);
  if (!str)
    str = "unknown";

  if (fn_gnutls_error_is_fatal (err))
    {
      ret = err;
      GNUTLS_LOG2 (0, max_log_level, "fatal error:", str);
    }
  else
    {
      ret = 0;
      GNUTLS_LOG2 (1, max_log_level, "non-fatal error:", str);
      /* TODO: EAGAIN AKA Qgnutls_e_again should be level 2.  */
    }

  if (err == GNUTLS_E_WARNING_ALERT_RECEIVED
      || err == GNUTLS_E_FATAL_ALERT_RECEIVED)
    {
      int alert = fn_gnutls_alert_get (session);
      int level = (err == GNUTLS_E_FATAL_ALERT_RECEIVED) ? 0 : 1;
      str = fn_gnutls_alert_get_name (alert);
      if (!str)
	str = "unknown";

      GNUTLS_LOG2 (level, max_log_level, "Received alert: ", str);
    }
  return ret;
}

/* convert an integer error to a Lisp_Object; it will be either a
   known symbol like `gnutls_e_interrupted' and `gnutls_e_again' or
   simply the integer value of the error.  GNUTLS_E_SUCCESS is mapped
   to Qt.  */
static Lisp_Object
gnutls_make_error (int err)
{
  switch (err)
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

  return make_number (err);
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

  if (0 == fn_gnutls_error_is_fatal (XINT (err)))
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

  return build_string (fn_gnutls_strerror (XINT (err)));
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
      fn_gnutls_deinit (state);
      GNUTLS_INITSTAGE (proc) = GNUTLS_STAGE_INIT - 1;
    }

  return Qt;
}

DEFUN ("gnutls-available-p", Fgnutls_available_p, Sgnutls_available_p, 0, 0, 0,
       doc: /* Return t if GnuTLS is available in this instance of Emacs.  */)
     (void)
{
#ifdef WINDOWSNT
  Lisp_Object found = Fassq (Qgnutls_dll, Vlibrary_cache);
  if (CONSP (found))
    return XCDR (found);
  else
    {
      Lisp_Object status;
      status = init_gnutls_functions (Vdynamic_library_alist) ? Qt : Qnil;
      Vlibrary_cache = Fcons (Fcons (Qgnutls_dll, status), Vlibrary_cache);
      return status;
    }
#else
  return Qt;
#endif
}


/* Initializes global GnuTLS state to defaults.
Call `gnutls-global-deinit' when GnuTLS usage is no longer needed.
Returns zero on success.  */
static Lisp_Object
emacs_gnutls_global_init (void)
{
  int ret = GNUTLS_E_SUCCESS;

  if (!gnutls_global_initialized)
    {
      fn_gnutls_global_set_mem_functions (xmalloc, xmalloc, NULL,
					  xrealloc, xfree);
      ret = fn_gnutls_global_init ();
    }
  gnutls_global_initialized = 1;

  return gnutls_make_error (ret);
}

#if 0
/* Deinitializes global GnuTLS state.
See also `gnutls-global-init'.  */
static Lisp_Object
emacs_gnutls_global_deinit (void)
{
  if (gnutls_global_initialized)
    gnutls_global_deinit ();

  gnutls_global_initialized = 0;

  return gnutls_make_error (GNUTLS_E_SUCCESS);
}
#endif

DEFUN ("gnutls-boot", Fgnutls_boot, Sgnutls_boot, 3, 3, 0,
       doc: /* Initialize GnuTLS client for process PROC with TYPE+PROPLIST.
Currently only client mode is supported.  Returns a success/failure
value you can check with `gnutls-errorp'.

TYPE is a symbol, either `gnutls-anon' or `gnutls-x509pki'.
PROPLIST is a property list with the following keys:

:hostname is a string naming the remote host.

:priority is a GnuTLS priority string, defaults to "NORMAL".

:trustfiles is a list of PEM-encoded trust files for `gnutls-x509pki'.

:crlfiles is a list of PEM-encoded CRL lists for `gnutls-x509pki'.

:keylist is an alist of PEM-encoded key files and PEM-encoded
certificates for `gnutls-x509pki'.

:callbacks is an alist of callback functions, see below.

:loglevel is the debug level requested from GnuTLS, try 4.

:verify-flags is a bitset as per GnuTLS'
gnutls_certificate_set_verify_flags.

:verify-hostname-error, if non-nil, makes a hostname mismatch an
error.  Otherwise it will be just a warning.

The debug level will be set for this process AND globally for GnuTLS.
So if you set it higher or lower at any point, it affects global
debugging.

Note that the priority is set on the client.  The server does not use
the protocols's priority except for disabling protocols that were not
specified.

Processes must be initialized with this function before other GnuTLS
functions are used.  This function allocates resources which can only
be deallocated by calling `gnutls-deinit' or by calling it again.

The callbacks alist can have a `verify' key, associated with a
verification function (UNUSED).

Each authentication type may need additional information in order to
work.  For X.509 PKI (`gnutls-x509pki'), you probably need at least
one trustfile (usually a CA bundle).  */)
  (Lisp_Object proc, Lisp_Object type, Lisp_Object proplist)
{
  int ret = GNUTLS_E_SUCCESS;

  int max_log_level = 0;

  /* TODO: GNUTLS_X509_FMT_DER is also an option.  */
  int file_format = GNUTLS_X509_FMT_PEM;

  unsigned int gnutls_verify_flags = GNUTLS_VERIFY_ALLOW_X509_V1_CA_CRT;
  gnutls_x509_crt_t gnutls_verify_cert;
  unsigned int gnutls_verify_cert_list_size;
  const gnutls_datum_t *gnutls_verify_cert_list;

  gnutls_session_t state;
  gnutls_certificate_credentials_t x509_cred;
  gnutls_anon_client_credentials_t anon_cred;
  Lisp_Object global_init;
  char const *priority_string_ptr = "NORMAL"; /* default priority string.  */
  Lisp_Object tail;
  unsigned int peer_verification;
  char* c_hostname;

  /* Placeholders for the property list elements.  */
  Lisp_Object priority_string;
  Lisp_Object trustfiles;
  Lisp_Object crlfiles;
  Lisp_Object keylist;
  /* Lisp_Object callbacks; */
  Lisp_Object loglevel;
  Lisp_Object hostname;
  Lisp_Object verify_flags;
  /* Lisp_Object verify_error; */
  Lisp_Object verify_hostname_error;

  CHECK_PROCESS (proc);
  CHECK_SYMBOL (type);
  CHECK_LIST (proplist);

  if (NILP (Fgnutls_available_p ()))
    {
      error ("GnuTLS not available");
      return gnutls_make_error (GNUTLS_EMACS_ERROR_NOT_LOADED);
    }

  hostname              = Fplist_get (proplist, Qgnutls_bootprop_hostname);
  priority_string       = Fplist_get (proplist, Qgnutls_bootprop_priority);
  trustfiles            = Fplist_get (proplist, Qgnutls_bootprop_trustfiles);
  keylist               = Fplist_get (proplist, Qgnutls_bootprop_keylist);
  crlfiles              = Fplist_get (proplist, Qgnutls_bootprop_crlfiles);
  /* callbacks          = Fplist_get (proplist, Qgnutls_bootprop_callbacks); */
  loglevel              = Fplist_get (proplist, Qgnutls_bootprop_loglevel);
  verify_flags          = Fplist_get (proplist, Qgnutls_bootprop_verify_flags);
  /* verify_error       = Fplist_get (proplist, Qgnutls_bootprop_verify_error); */
  verify_hostname_error = Fplist_get (proplist, Qgnutls_bootprop_verify_hostname_error);

  if (!STRINGP (hostname))
    error ("gnutls-boot: invalid :hostname parameter");

  c_hostname = SSDATA (hostname);

  state = XPROCESS (proc)->gnutls_state;
  XPROCESS (proc)->gnutls_p = 1;

  if (NUMBERP (loglevel))
    {
      fn_gnutls_global_set_log_function (gnutls_log_function);
      fn_gnutls_global_set_log_level (XINT (loglevel));
      max_log_level = XINT (loglevel);
      XPROCESS (proc)->gnutls_log_level = max_log_level;
    }

  /* always initialize globals.  */
  global_init = emacs_gnutls_global_init ();
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
          fn_gnutls_certificate_free_credentials (x509_cred);
	}
      else if (EQ (type, Qgnutls_anon))
	{
          GNUTLS_LOG (2, max_log_level, "deallocating anon credentials");
          anon_cred = XPROCESS (proc)->gnutls_anon_cred;
          fn_gnutls_anon_free_client_credentials (anon_cred);
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
      fn_gnutls_certificate_allocate_credentials (&x509_cred);

      if (NUMBERP (verify_flags))
        {
          gnutls_verify_flags = XINT (verify_flags);
          GNUTLS_LOG (2, max_log_level, "setting verification flags");
        }
      else if (NILP (verify_flags))
        {
          /* The default is already GNUTLS_VERIFY_ALLOW_X509_V1_CA_CRT.  */
          GNUTLS_LOG (2, max_log_level, "using default verification flags");
        }
      else
        {
          /* The default is already GNUTLS_VERIFY_ALLOW_X509_V1_CA_CRT.  */
          GNUTLS_LOG (2, max_log_level, "ignoring invalid verify-flags");
        }
      fn_gnutls_certificate_set_verify_flags (x509_cred, gnutls_verify_flags);
    }
  else if (EQ (type, Qgnutls_anon))
    {
      GNUTLS_LOG (2, max_log_level, "allocating anon credentials");
      anon_cred = XPROCESS (proc)->gnutls_anon_cred;
      fn_gnutls_anon_allocate_client_credentials (&anon_cred);
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
                           SSDATA (trustfile));
              ret = fn_gnutls_certificate_set_x509_trust_file
                (x509_cred,
                 SSDATA (trustfile),
                 file_format);

              if (ret < GNUTLS_E_SUCCESS)
                return gnutls_make_error (ret);
            }
          else
            {
              error ("Sorry, GnuTLS can't use non-string trustfile %s",
                     SDATA (trustfile));
            }
        }

      for (tail = crlfiles; !NILP (tail); tail = Fcdr (tail))
	{
	  Lisp_Object crlfile = Fcar (tail);
          if (STRINGP (crlfile))
            {
              GNUTLS_LOG2 (1, max_log_level, "setting the CRL file: ",
                           SSDATA (crlfile));
              ret = fn_gnutls_certificate_set_x509_crl_file
                (x509_cred,
                 SSDATA (crlfile),
                 file_format);

              if (ret < GNUTLS_E_SUCCESS)
                return gnutls_make_error (ret);
            }
          else
            {
              error ("Sorry, GnuTLS can't use non-string CRL file %s",
                     SDATA (crlfile));
            }
        }

      for (tail = keylist; !NILP (tail); tail = Fcdr (tail))
	{
	  Lisp_Object keyfile = Fcar (Fcar (tail));
	  Lisp_Object certfile = Fcar (Fcdr (tail));
          if (STRINGP (keyfile) && STRINGP (certfile))
            {
              GNUTLS_LOG2 (1, max_log_level, "setting the client key file: ",
                           SSDATA (keyfile));
              GNUTLS_LOG2 (1, max_log_level, "setting the client cert file: ",
                           SSDATA (certfile));
              ret = fn_gnutls_certificate_set_x509_key_file
                (x509_cred,
                 SSDATA (certfile),
                 SSDATA (keyfile),
                 file_format);

              if (ret < GNUTLS_E_SUCCESS)
                return gnutls_make_error (ret);
            }
          else
            {
              if (STRINGP (keyfile))
                error ("Sorry, GnuTLS can't use non-string client cert file %s",
                       SDATA (certfile));
              else
                error ("Sorry, GnuTLS can't use non-string client key file %s",
                       SDATA (keyfile));
            }
        }
    }

  GNUTLS_INITSTAGE (proc) = GNUTLS_STAGE_FILES;

  GNUTLS_LOG (1, max_log_level, "gnutls callbacks");

  GNUTLS_INITSTAGE (proc) = GNUTLS_STAGE_CALLBACKS;

#ifdef HAVE_GNUTLS_CALLBACK_CERTIFICATE_VERIFY
#else
#endif

  GNUTLS_LOG (1, max_log_level, "gnutls_init");

  ret = fn_gnutls_init (&state, GNUTLS_CLIENT);

  if (ret < GNUTLS_E_SUCCESS)
    return gnutls_make_error (ret);

  XPROCESS (proc)->gnutls_state = state;

  GNUTLS_INITSTAGE (proc) = GNUTLS_STAGE_INIT;

  if (STRINGP (priority_string))
    {
      priority_string_ptr = SSDATA (priority_string);
      GNUTLS_LOG2 (1, max_log_level, "got non-default priority string:",
                   priority_string_ptr);
    }
  else
    {
      GNUTLS_LOG2 (1, max_log_level, "using default priority string:",
                   priority_string_ptr);
    }

  GNUTLS_LOG (1, max_log_level, "setting the priority string");

  ret = fn_gnutls_priority_set_direct (state,
                                       priority_string_ptr,
                                       NULL);

  if (ret < GNUTLS_E_SUCCESS)
    return gnutls_make_error (ret);

  GNUTLS_INITSTAGE (proc) = GNUTLS_STAGE_PRIORITY;

  if (EQ (type, Qgnutls_x509pki))
    {
      ret = fn_gnutls_credentials_set (state, GNUTLS_CRD_CERTIFICATE, x509_cred);
    }
  else if (EQ (type, Qgnutls_anon))
    {
      ret = fn_gnutls_credentials_set (state, GNUTLS_CRD_ANON, anon_cred);
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

  ret = emacs_gnutls_handshake (XPROCESS (proc));

  if (ret < GNUTLS_E_SUCCESS)
    return gnutls_make_error (ret);

  /* Now verify the peer, following
     http://www.gnu.org/software/gnutls/manual/html_node/Verifying-peer_0027s-certificate.html.
     The peer should present at least one certificate in the chain; do a
     check of the certificate's hostname with
     gnutls_x509_crt_check_hostname() against :hostname.  */

  ret = fn_gnutls_certificate_verify_peers2 (state, &peer_verification);

  if (ret < GNUTLS_E_SUCCESS)
    return gnutls_make_error (ret);

  if (XINT (loglevel) > 0 && peer_verification & GNUTLS_CERT_INVALID)
    message ("%s certificate could not be verified.",
             c_hostname);

 if (peer_verification & GNUTLS_CERT_REVOKED)
   GNUTLS_LOG2 (1, max_log_level, "certificate was revoked (CRL):",
                c_hostname);

 if (peer_verification & GNUTLS_CERT_SIGNER_NOT_FOUND)
   GNUTLS_LOG2 (1, max_log_level, "certificate signer was not found:",
                c_hostname);

 if (peer_verification & GNUTLS_CERT_SIGNER_NOT_CA)
   GNUTLS_LOG2 (1, max_log_level, "certificate signer is not a CA:",
                c_hostname);

 if (peer_verification & GNUTLS_CERT_INSECURE_ALGORITHM)
   GNUTLS_LOG2 (1, max_log_level,
                "certificate was signed with an insecure algorithm:",
                c_hostname);

 if (peer_verification & GNUTLS_CERT_NOT_ACTIVATED)
   GNUTLS_LOG2 (1, max_log_level, "certificate is not yet activated:",
                c_hostname);

 if (peer_verification & GNUTLS_CERT_EXPIRED)
   GNUTLS_LOG2 (1, max_log_level, "certificate has expired:",
                c_hostname);

 if (peer_verification != 0)
   {
     if (NILP (verify_hostname_error))
       {
         GNUTLS_LOG2 (1, max_log_level, "certificate validation failed:",
                      c_hostname);
       }
     else
       {
         error ("Certificate validation failed %s, verification code %d",
                c_hostname, peer_verification);
       }
   }

  /* Up to here the process is the same for X.509 certificates and
     OpenPGP keys.  From now on X.509 certificates are assumed.  This
     can be easily extended to work with openpgp keys as well.  */
  if (fn_gnutls_certificate_type_get (state) == GNUTLS_CRT_X509)
    {
      ret = fn_gnutls_x509_crt_init (&gnutls_verify_cert);

      if (ret < GNUTLS_E_SUCCESS)
        return gnutls_make_error (ret);

      gnutls_verify_cert_list =
        fn_gnutls_certificate_get_peers (state, &gnutls_verify_cert_list_size);

      if (NULL == gnutls_verify_cert_list)
        {
          error ("No x509 certificate was found!\n");
        }

      /* We only check the first certificate in the given chain.  */
      ret = fn_gnutls_x509_crt_import (gnutls_verify_cert,
                                       &gnutls_verify_cert_list[0],
                                       GNUTLS_X509_FMT_DER);

      if (ret < GNUTLS_E_SUCCESS)
        {
          fn_gnutls_x509_crt_deinit (gnutls_verify_cert);
          return gnutls_make_error (ret);
        }

      if (!fn_gnutls_x509_crt_check_hostname (gnutls_verify_cert, c_hostname))
        {
          if (NILP (verify_hostname_error))
            {
              GNUTLS_LOG2 (1, max_log_level, "x509 certificate does not match:",
                           c_hostname);
            }
          else
            {
              fn_gnutls_x509_crt_deinit (gnutls_verify_cert);
              error ("The x509 certificate does not match \"%s\"",
                     c_hostname);
            }
        }

      fn_gnutls_x509_crt_deinit (gnutls_verify_cert);
    }

  return gnutls_make_error (ret);
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

  ret = fn_gnutls_bye (state,
                       NILP (cont) ? GNUTLS_SHUT_RDWR : GNUTLS_SHUT_WR);

  return gnutls_make_error (ret);
}

void
syms_of_gnutls (void)
{
  gnutls_global_initialized = 0;

  DEFSYM (Qgnutls_dll, "gnutls");
  DEFSYM (Qgnutls_log_level, "gnutls-log-level");
  DEFSYM (Qgnutls_code, "gnutls-code");
  DEFSYM (Qgnutls_anon, "gnutls-anon");
  DEFSYM (Qgnutls_x509pki, "gnutls-x509pki");
  DEFSYM (Qgnutls_bootprop_hostname, ":hostname");
  DEFSYM (Qgnutls_bootprop_priority, ":priority");
  DEFSYM (Qgnutls_bootprop_trustfiles, ":trustfiles");
  DEFSYM (Qgnutls_bootprop_keylist, ":keylist");
  DEFSYM (Qgnutls_bootprop_crlfiles, ":crlfiles");
  DEFSYM (Qgnutls_bootprop_callbacks, ":callbacks");
  DEFSYM (Qgnutls_bootprop_callbacks_verify, "verify");
  DEFSYM (Qgnutls_bootprop_loglevel, ":loglevel");
  DEFSYM (Qgnutls_bootprop_verify_flags, ":verify-flags");
  DEFSYM (Qgnutls_bootprop_verify_hostname_error, ":verify-hostname-error");

  DEFSYM (Qgnutls_e_interrupted, "gnutls-e-interrupted");
  Fput (Qgnutls_e_interrupted, Qgnutls_code,
        make_number (GNUTLS_E_INTERRUPTED));

  DEFSYM (Qgnutls_e_again, "gnutls-e-again");
  Fput (Qgnutls_e_again, Qgnutls_code,
        make_number (GNUTLS_E_AGAIN));

  DEFSYM (Qgnutls_e_invalid_session, "gnutls-e-invalid-session");
  Fput (Qgnutls_e_invalid_session, Qgnutls_code,
        make_number (GNUTLS_E_INVALID_SESSION));

  DEFSYM (Qgnutls_e_not_ready_for_handshake, "gnutls-e-not-ready-for-handshake");
  Fput (Qgnutls_e_not_ready_for_handshake, Qgnutls_code,
        make_number (GNUTLS_E_APPLICATION_ERROR_MIN));

  defsubr (&Sgnutls_get_initstage);
  defsubr (&Sgnutls_errorp);
  defsubr (&Sgnutls_error_fatalp);
  defsubr (&Sgnutls_error_string);
  defsubr (&Sgnutls_boot);
  defsubr (&Sgnutls_deinit);
  defsubr (&Sgnutls_bye);
  defsubr (&Sgnutls_available_p);
}

#endif /* HAVE_GNUTLS */
