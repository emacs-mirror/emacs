/* GnuTLS glue for GNU Emacs.
   Copyright (C) 2010-2018 Free Software Foundation, Inc.

This file is part of GNU Emacs.

GNU Emacs is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or (at
your option) any later version.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.  */

#include <config.h>
#include <errno.h>
#include <stdio.h>

#include "lisp.h"
#include "process.h"
#include "gnutls.h"
#include "coding.h"
#include "buffer.h"

#if GNUTLS_VERSION_NUMBER >= 0x030014
# define HAVE_GNUTLS_X509_SYSTEM_TRUST
#endif

/* Although AEAD support started in GnuTLS 3.4.0 and works in 3.5.14,
   it was broken through at least GnuTLS 3.4.10; see:
   https://lists.gnu.org/r/emacs-devel/2017-07/msg00992.html
   The relevant fix seems to have been made in GnuTLS 3.5.1; see:
   https://gitlab.com/gnutls/gnutls/commit/568935848dd6b82b9315d8b6c529d00e2605e03d
   So, require 3.5.1.  */
#if GNUTLS_VERSION_NUMBER >= 0x030501
# define HAVE_GNUTLS_AEAD
#endif

/* gnutls_mac_get_nonce_size was added in GnuTLS 3.2.0, but was
   exported only since 3.3.0. */
#if GNUTLS_VERSION_NUMBER >= 0x030300
# define HAVE_GNUTLS_MAC_GET_NONCE_SIZE
#endif

#if GNUTLS_VERSION_NUMBER >= 0x030501
# define HAVE_GNUTLS_EXT_GET_NAME
#endif

#if GNUTLS_VERSION_NUMBER >= 0x030205
# define HAVE_GNUTLS_EXT__DUMBFW
#endif

#ifdef HAVE_GNUTLS

# ifdef WINDOWSNT
#  include <windows.h>
#  include "w32.h"
# endif

static bool emacs_gnutls_handle_error (gnutls_session_t, int);

static bool gnutls_global_initialized;

static void gnutls_log_function (int, const char *);
static void gnutls_log_function2 (int, const char *, const char *);
# ifdef HAVE_GNUTLS3
static void gnutls_audit_log_function (gnutls_session_t, const char *);
# endif

enum extra_peer_verification
{
    CERTIFICATE_NOT_MATCHING = 2
};


# ifdef WINDOWSNT

DEF_DLL_FN (gnutls_alert_description_t, gnutls_alert_get,
	    (gnutls_session_t));
DEF_DLL_FN (const char *, gnutls_alert_get_name,
	    (gnutls_alert_description_t));
DEF_DLL_FN (int, gnutls_anon_allocate_client_credentials,
	    (gnutls_anon_client_credentials_t *));
DEF_DLL_FN (void, gnutls_anon_free_client_credentials,
	    (gnutls_anon_client_credentials_t));
DEF_DLL_FN (int, gnutls_bye, (gnutls_session_t, gnutls_close_request_t));
DEF_DLL_FN (int, gnutls_certificate_allocate_credentials,
	    (gnutls_certificate_credentials_t *));
DEF_DLL_FN (void, gnutls_certificate_free_credentials,
	    (gnutls_certificate_credentials_t));
DEF_DLL_FN (const gnutls_datum_t *, gnutls_certificate_get_peers,
	    (gnutls_session_t, unsigned int *));
DEF_DLL_FN (void, gnutls_certificate_set_verify_flags,
	    (gnutls_certificate_credentials_t, unsigned int));
DEF_DLL_FN (int, gnutls_certificate_set_x509_crl_file,
	    (gnutls_certificate_credentials_t, const char *,
	     gnutls_x509_crt_fmt_t));
DEF_DLL_FN (int, gnutls_certificate_set_x509_key_file,
	    (gnutls_certificate_credentials_t, const char *, const char *,
	     gnutls_x509_crt_fmt_t));
#  ifdef HAVE_GNUTLS_X509_SYSTEM_TRUST
DEF_DLL_FN (int, gnutls_certificate_set_x509_system_trust,
	    (gnutls_certificate_credentials_t));
#  endif
DEF_DLL_FN (int, gnutls_certificate_set_x509_trust_file,
	    (gnutls_certificate_credentials_t, const char *,
	     gnutls_x509_crt_fmt_t));
DEF_DLL_FN (gnutls_certificate_type_t, gnutls_certificate_type_get,
	    (gnutls_session_t));
DEF_DLL_FN (int, gnutls_certificate_verify_peers2,
	    (gnutls_session_t, unsigned int *));
DEF_DLL_FN (int, gnutls_credentials_set,
	    (gnutls_session_t, gnutls_credentials_type_t, void *));
DEF_DLL_FN (void, gnutls_deinit, (gnutls_session_t));
DEF_DLL_FN (void, gnutls_dh_set_prime_bits,
	    (gnutls_session_t, unsigned int));
DEF_DLL_FN (int, gnutls_dh_get_prime_bits, (gnutls_session_t));
DEF_DLL_FN (int, gnutls_error_is_fatal, (int));
DEF_DLL_FN (int, gnutls_global_init, (void));
DEF_DLL_FN (void, gnutls_global_set_log_function, (gnutls_log_func));
#  ifdef HAVE_GNUTLS3
DEF_DLL_FN (void, gnutls_global_set_audit_log_function, (gnutls_audit_log_func));
#  endif
DEF_DLL_FN (void, gnutls_global_set_log_level, (int));
DEF_DLL_FN (int, gnutls_handshake, (gnutls_session_t));
DEF_DLL_FN (int, gnutls_init, (gnutls_session_t *, unsigned int));
DEF_DLL_FN (int, gnutls_priority_set_direct,
	    (gnutls_session_t, const char *, const char **));
DEF_DLL_FN (size_t, gnutls_record_check_pending, (gnutls_session_t));
DEF_DLL_FN (ssize_t, gnutls_record_recv, (gnutls_session_t, void *, size_t));
DEF_DLL_FN (ssize_t, gnutls_record_send,
	    (gnutls_session_t, const void *, size_t));
DEF_DLL_FN (const char *, gnutls_strerror, (int));
DEF_DLL_FN (void, gnutls_transport_set_errno, (gnutls_session_t, int));
DEF_DLL_FN (void, gnutls_transport_set_ptr2,
	    (gnutls_session_t, gnutls_transport_ptr_t,
	     gnutls_transport_ptr_t));
DEF_DLL_FN (void, gnutls_transport_set_pull_function,
	    (gnutls_session_t, gnutls_pull_func));
DEF_DLL_FN (void, gnutls_transport_set_push_function,
	    (gnutls_session_t, gnutls_push_func));
DEF_DLL_FN (int, gnutls_x509_crt_check_hostname,
	    (gnutls_x509_crt_t, const char *));
DEF_DLL_FN (int, gnutls_x509_crt_check_issuer,
              (gnutls_x509_crt_t, gnutls_x509_crt_t));
DEF_DLL_FN (void, gnutls_x509_crt_deinit, (gnutls_x509_crt_t));
DEF_DLL_FN (int, gnutls_x509_crt_import,
	    (gnutls_x509_crt_t, const gnutls_datum_t *,
	     gnutls_x509_crt_fmt_t));
DEF_DLL_FN (int, gnutls_x509_crt_init, (gnutls_x509_crt_t *));
DEF_DLL_FN (int, gnutls_x509_crt_get_fingerprint,
	    (gnutls_x509_crt_t,
	     gnutls_digest_algorithm_t, void *, size_t *));
DEF_DLL_FN (int, gnutls_x509_crt_get_version,
	    (gnutls_x509_crt_t));
DEF_DLL_FN (int, gnutls_x509_crt_get_serial,
	    (gnutls_x509_crt_t, void *, size_t *));
DEF_DLL_FN (int, gnutls_x509_crt_get_issuer_dn,
	    (gnutls_x509_crt_t, char *, size_t *));
DEF_DLL_FN (time_t, gnutls_x509_crt_get_activation_time,
	    (gnutls_x509_crt_t));
DEF_DLL_FN (time_t, gnutls_x509_crt_get_expiration_time,
	    (gnutls_x509_crt_t));
DEF_DLL_FN (int, gnutls_x509_crt_get_dn,
	    (gnutls_x509_crt_t, char *, size_t *));
DEF_DLL_FN (int, gnutls_x509_crt_get_pk_algorithm,
	    (gnutls_x509_crt_t, unsigned int *));
DEF_DLL_FN (const char *, gnutls_pk_algorithm_get_name,
	    (gnutls_pk_algorithm_t));
DEF_DLL_FN (int, gnutls_pk_bits_to_sec_param,
	    (gnutls_pk_algorithm_t, unsigned int));
DEF_DLL_FN (int, gnutls_x509_crt_get_issuer_unique_id,
	    (gnutls_x509_crt_t, char *, size_t *));
DEF_DLL_FN (int, gnutls_x509_crt_get_subject_unique_id,
	    (gnutls_x509_crt_t, char *, size_t *));
DEF_DLL_FN (int, gnutls_x509_crt_get_signature_algorithm,
	    (gnutls_x509_crt_t));
DEF_DLL_FN (int, gnutls_x509_crt_get_key_id,
	    (gnutls_x509_crt_t, unsigned int, unsigned char *, size_t *_size));
DEF_DLL_FN (const char *, gnutls_sec_param_get_name, (gnutls_sec_param_t));
DEF_DLL_FN (const char *, gnutls_sign_get_name, (gnutls_sign_algorithm_t));
DEF_DLL_FN (int, gnutls_server_name_set,
	    (gnutls_session_t, gnutls_server_name_type_t,
	     const void *, size_t));
DEF_DLL_FN (gnutls_kx_algorithm_t, gnutls_kx_get, (gnutls_session_t));
DEF_DLL_FN (const char *, gnutls_kx_get_name, (gnutls_kx_algorithm_t));
DEF_DLL_FN (gnutls_protocol_t, gnutls_protocol_get_version,
	    (gnutls_session_t));
DEF_DLL_FN (const char *, gnutls_protocol_get_name, (gnutls_protocol_t));
DEF_DLL_FN (gnutls_cipher_algorithm_t, gnutls_cipher_get,
	    (gnutls_session_t));
DEF_DLL_FN (const char *, gnutls_cipher_get_name,
	    (gnutls_cipher_algorithm_t));
DEF_DLL_FN (gnutls_mac_algorithm_t, gnutls_mac_get, (gnutls_session_t));
DEF_DLL_FN (const char *, gnutls_mac_get_name, (gnutls_mac_algorithm_t));

#  ifdef HAVE_GNUTLS3
DEF_DLL_FN (int, gnutls_rnd, (gnutls_rnd_level_t, void *, size_t));
DEF_DLL_FN (const gnutls_mac_algorithm_t *, gnutls_mac_list, (void));
#   ifdef HAVE_GNUTLS_MAC_GET_NONCE_SIZE
DEF_DLL_FN (size_t, gnutls_mac_get_nonce_size, (gnutls_mac_algorithm_t));
#   endif
DEF_DLL_FN (size_t, gnutls_mac_get_key_size, (gnutls_mac_algorithm_t));
DEF_DLL_FN (const gnutls_digest_algorithm_t *, gnutls_digest_list, (void));
DEF_DLL_FN (const char *, gnutls_digest_get_name, (gnutls_digest_algorithm_t));
DEF_DLL_FN (gnutls_cipher_algorithm_t *, gnutls_cipher_list, (void));
DEF_DLL_FN (int, gnutls_cipher_get_iv_size, (gnutls_cipher_algorithm_t));
DEF_DLL_FN (size_t, gnutls_cipher_get_key_size, (gnutls_cipher_algorithm_t));
DEF_DLL_FN (int, gnutls_cipher_get_block_size, (gnutls_cipher_algorithm_t));
DEF_DLL_FN (int, gnutls_cipher_get_tag_size, (gnutls_cipher_algorithm_t));
DEF_DLL_FN (int, gnutls_cipher_init,
	    (gnutls_cipher_hd_t *, gnutls_cipher_algorithm_t,
	     const gnutls_datum_t *, const gnutls_datum_t *));
DEF_DLL_FN (void, gnutls_cipher_set_iv, (gnutls_cipher_hd_t, void *, size_t));
DEF_DLL_FN (int, gnutls_cipher_encrypt2,
	    (gnutls_cipher_hd_t, const void *, size_t, void *, size_t));
DEF_DLL_FN (void, gnutls_cipher_deinit, (gnutls_cipher_hd_t));
DEF_DLL_FN (int, gnutls_cipher_decrypt2,
	    (gnutls_cipher_hd_t, const void *, size_t, void *, size_t));
#   ifdef HAVE_GNUTLS_AEAD
DEF_DLL_FN (int, gnutls_aead_cipher_init,
	    (gnutls_aead_cipher_hd_t *, gnutls_cipher_algorithm_t,
	     const gnutls_datum_t *));
DEF_DLL_FN (void, gnutls_aead_cipher_deinit, (gnutls_aead_cipher_hd_t));
DEF_DLL_FN (int, gnutls_aead_cipher_encrypt,
	    (gnutls_aead_cipher_hd_t, const void *, size_t, const void *,
	     size_t, size_t, const void *, size_t, void *, size_t *));
DEF_DLL_FN (int, gnutls_aead_cipher_decrypt,
	    (gnutls_aead_cipher_hd_t, const void *, size_t, const void *,
	     size_t, size_t, const void *, size_t, void *, size_t *));
#   endif
DEF_DLL_FN (int, gnutls_hmac_init,
	    (gnutls_hmac_hd_t *, gnutls_mac_algorithm_t, const void *, size_t));
DEF_DLL_FN (int, gnutls_hmac_get_len, (gnutls_mac_algorithm_t));
DEF_DLL_FN (int, gnutls_hmac, (gnutls_hmac_hd_t, const void *, size_t));
DEF_DLL_FN (void, gnutls_hmac_deinit, (gnutls_hmac_hd_t, void *));
DEF_DLL_FN (void, gnutls_hmac_output, (gnutls_hmac_hd_t, void *));
  DEF_DLL_FN (int, gnutls_hash_init,
	    (gnutls_hash_hd_t *, gnutls_digest_algorithm_t));
DEF_DLL_FN (int, gnutls_hash_get_len, (gnutls_digest_algorithm_t));
DEF_DLL_FN (int, gnutls_hash, (gnutls_hash_hd_t, const void *, size_t));
DEF_DLL_FN (void, gnutls_hash_deinit, (gnutls_hash_hd_t, void *));
DEF_DLL_FN (void, gnutls_hash_output, (gnutls_hash_hd_t, void *));
#   ifdef HAVE_GNUTLS_EXT_GET_NAME
DEF_DLL_FN (const char *, gnutls_ext_get_name, (unsigned int));
#   endif
#  endif	 /* HAVE_GNUTLS3 */


static bool
init_gnutls_functions (void)
{
  HMODULE library;
  int max_log_level = 1;

  if (!(library = w32_delayed_load (Qgnutls)))
    {
      GNUTLS_LOG (1, max_log_level, "GnuTLS library not found");
      return 0;
    }

  LOAD_DLL_FN (library, gnutls_alert_get);
  LOAD_DLL_FN (library, gnutls_alert_get_name);
  LOAD_DLL_FN (library, gnutls_anon_allocate_client_credentials);
  LOAD_DLL_FN (library, gnutls_anon_free_client_credentials);
  LOAD_DLL_FN (library, gnutls_bye);
  LOAD_DLL_FN (library, gnutls_certificate_allocate_credentials);
  LOAD_DLL_FN (library, gnutls_certificate_free_credentials);
  LOAD_DLL_FN (library, gnutls_certificate_get_peers);
  LOAD_DLL_FN (library, gnutls_certificate_set_verify_flags);
  LOAD_DLL_FN (library, gnutls_certificate_set_x509_crl_file);
  LOAD_DLL_FN (library, gnutls_certificate_set_x509_key_file);
#  ifdef HAVE_GNUTLS_X509_SYSTEM_TRUST
  LOAD_DLL_FN (library, gnutls_certificate_set_x509_system_trust);
#  endif
  LOAD_DLL_FN (library, gnutls_certificate_set_x509_trust_file);
  LOAD_DLL_FN (library, gnutls_certificate_type_get);
  LOAD_DLL_FN (library, gnutls_certificate_verify_peers2);
  LOAD_DLL_FN (library, gnutls_credentials_set);
  LOAD_DLL_FN (library, gnutls_deinit);
  LOAD_DLL_FN (library, gnutls_dh_set_prime_bits);
  LOAD_DLL_FN (library, gnutls_dh_get_prime_bits);
  LOAD_DLL_FN (library, gnutls_error_is_fatal);
  LOAD_DLL_FN (library, gnutls_global_init);
  LOAD_DLL_FN (library, gnutls_global_set_log_function);
#  ifdef HAVE_GNUTLS3
  LOAD_DLL_FN (library, gnutls_global_set_audit_log_function);
#  endif
  LOAD_DLL_FN (library, gnutls_global_set_log_level);
  LOAD_DLL_FN (library, gnutls_handshake);
  LOAD_DLL_FN (library, gnutls_init);
  LOAD_DLL_FN (library, gnutls_priority_set_direct);
  LOAD_DLL_FN (library, gnutls_record_check_pending);
  LOAD_DLL_FN (library, gnutls_record_recv);
  LOAD_DLL_FN (library, gnutls_record_send);
  LOAD_DLL_FN (library, gnutls_strerror);
  LOAD_DLL_FN (library, gnutls_transport_set_errno);
  LOAD_DLL_FN (library, gnutls_transport_set_ptr2);
  LOAD_DLL_FN (library, gnutls_transport_set_pull_function);
  LOAD_DLL_FN (library, gnutls_transport_set_push_function);
  LOAD_DLL_FN (library, gnutls_x509_crt_check_hostname);
  LOAD_DLL_FN (library, gnutls_x509_crt_check_issuer);
  LOAD_DLL_FN (library, gnutls_x509_crt_deinit);
  LOAD_DLL_FN (library, gnutls_x509_crt_import);
  LOAD_DLL_FN (library, gnutls_x509_crt_init);
  LOAD_DLL_FN (library, gnutls_x509_crt_get_fingerprint);
  LOAD_DLL_FN (library, gnutls_x509_crt_get_version);
  LOAD_DLL_FN (library, gnutls_x509_crt_get_serial);
  LOAD_DLL_FN (library, gnutls_x509_crt_get_issuer_dn);
  LOAD_DLL_FN (library, gnutls_x509_crt_get_activation_time);
  LOAD_DLL_FN (library, gnutls_x509_crt_get_expiration_time);
  LOAD_DLL_FN (library, gnutls_x509_crt_get_dn);
  LOAD_DLL_FN (library, gnutls_x509_crt_get_pk_algorithm);
  LOAD_DLL_FN (library, gnutls_pk_algorithm_get_name);
  LOAD_DLL_FN (library, gnutls_pk_bits_to_sec_param);
  LOAD_DLL_FN (library, gnutls_x509_crt_get_issuer_unique_id);
  LOAD_DLL_FN (library, gnutls_x509_crt_get_subject_unique_id);
  LOAD_DLL_FN (library, gnutls_x509_crt_get_signature_algorithm);
  LOAD_DLL_FN (library, gnutls_x509_crt_get_key_id);
  LOAD_DLL_FN (library, gnutls_sec_param_get_name);
  LOAD_DLL_FN (library, gnutls_sign_get_name);
  LOAD_DLL_FN (library, gnutls_server_name_set);
  LOAD_DLL_FN (library, gnutls_kx_get);
  LOAD_DLL_FN (library, gnutls_kx_get_name);
  LOAD_DLL_FN (library, gnutls_protocol_get_version);
  LOAD_DLL_FN (library, gnutls_protocol_get_name);
  LOAD_DLL_FN (library, gnutls_cipher_get);
  LOAD_DLL_FN (library, gnutls_cipher_get_name);
  LOAD_DLL_FN (library, gnutls_mac_get);
  LOAD_DLL_FN (library, gnutls_mac_get_name);
#  ifdef HAVE_GNUTLS3
  LOAD_DLL_FN (library, gnutls_rnd);
  LOAD_DLL_FN (library, gnutls_mac_list);
#   ifdef HAVE_GNUTLS_MAC_GET_NONCE_SIZE
  LOAD_DLL_FN (library, gnutls_mac_get_nonce_size);
#   endif
  LOAD_DLL_FN (library, gnutls_mac_get_key_size);
  LOAD_DLL_FN (library, gnutls_digest_list);
  LOAD_DLL_FN (library, gnutls_digest_get_name);
  LOAD_DLL_FN (library, gnutls_cipher_list);
  LOAD_DLL_FN (library, gnutls_cipher_get_iv_size);
  LOAD_DLL_FN (library, gnutls_cipher_get_key_size);
  LOAD_DLL_FN (library, gnutls_cipher_get_block_size);
  LOAD_DLL_FN (library, gnutls_cipher_get_tag_size);
  LOAD_DLL_FN (library, gnutls_cipher_init);
  LOAD_DLL_FN (library, gnutls_cipher_set_iv);
  LOAD_DLL_FN (library, gnutls_cipher_encrypt2);
  LOAD_DLL_FN (library, gnutls_cipher_deinit);
  LOAD_DLL_FN (library, gnutls_cipher_decrypt2);
#   ifdef HAVE_GNUTLS_AEAD
  LOAD_DLL_FN (library, gnutls_aead_cipher_init);
  LOAD_DLL_FN (library, gnutls_aead_cipher_deinit);
  LOAD_DLL_FN (library, gnutls_aead_cipher_encrypt);
  LOAD_DLL_FN (library, gnutls_aead_cipher_decrypt);
#   endif
  LOAD_DLL_FN (library, gnutls_hmac_init);
  LOAD_DLL_FN (library, gnutls_hmac_get_len);
  LOAD_DLL_FN (library, gnutls_hmac);
  LOAD_DLL_FN (library, gnutls_hmac_deinit);
  LOAD_DLL_FN (library, gnutls_hmac_output);
  LOAD_DLL_FN (library, gnutls_hash_init);
  LOAD_DLL_FN (library, gnutls_hash_get_len);
  LOAD_DLL_FN (library, gnutls_hash);
  LOAD_DLL_FN (library, gnutls_hash_deinit);
  LOAD_DLL_FN (library, gnutls_hash_output);
#   ifdef HAVE_GNUTLS_EXT_GET_NAME
  LOAD_DLL_FN (library, gnutls_ext_get_name);
#   endif
#  endif	 /* HAVE_GNUTLS3 */

  max_log_level = global_gnutls_log_level;

  {
    Lisp_Object name = CAR_SAFE (Fget (Qgnutls, QCloaded_from));
    GNUTLS_LOG2 (1, max_log_level, "GnuTLS library loaded:",
                 STRINGP (name) ? (const char *) SDATA (name) : "unknown");
  }

  return 1;
}

#  define gnutls_alert_get fn_gnutls_alert_get
#  define gnutls_alert_get_name fn_gnutls_alert_get_name
#  define gnutls_anon_allocate_client_credentials fn_gnutls_anon_allocate_client_credentials
#  define gnutls_anon_free_client_credentials fn_gnutls_anon_free_client_credentials
#  define gnutls_bye fn_gnutls_bye
#  define gnutls_certificate_allocate_credentials fn_gnutls_certificate_allocate_credentials
#  define gnutls_certificate_free_credentials fn_gnutls_certificate_free_credentials
#  define gnutls_certificate_get_peers fn_gnutls_certificate_get_peers
#  define gnutls_certificate_set_verify_flags fn_gnutls_certificate_set_verify_flags
#  define gnutls_certificate_set_x509_crl_file fn_gnutls_certificate_set_x509_crl_file
#  define gnutls_certificate_set_x509_key_file fn_gnutls_certificate_set_x509_key_file
#  define gnutls_certificate_set_x509_system_trust fn_gnutls_certificate_set_x509_system_trust
#  define gnutls_certificate_set_x509_trust_file fn_gnutls_certificate_set_x509_trust_file
#  define gnutls_certificate_type_get fn_gnutls_certificate_type_get
#  define gnutls_certificate_verify_peers2 fn_gnutls_certificate_verify_peers2
#  define gnutls_cipher_get fn_gnutls_cipher_get
#  define gnutls_cipher_get_name fn_gnutls_cipher_get_name
#  define gnutls_credentials_set fn_gnutls_credentials_set
#  define gnutls_deinit fn_gnutls_deinit
#  define gnutls_dh_get_prime_bits fn_gnutls_dh_get_prime_bits
#  define gnutls_dh_set_prime_bits fn_gnutls_dh_set_prime_bits
#  define gnutls_error_is_fatal fn_gnutls_error_is_fatal
#  define gnutls_global_init fn_gnutls_global_init
#  define gnutls_global_set_audit_log_function fn_gnutls_global_set_audit_log_function
#  define gnutls_global_set_log_function fn_gnutls_global_set_log_function
#  define gnutls_global_set_log_level fn_gnutls_global_set_log_level
#  define gnutls_handshake fn_gnutls_handshake
#  define gnutls_init fn_gnutls_init
#  define gnutls_kx_get fn_gnutls_kx_get
#  define gnutls_kx_get_name fn_gnutls_kx_get_name
#  define gnutls_mac_get fn_gnutls_mac_get
#  define gnutls_mac_get_name fn_gnutls_mac_get_name
#  define gnutls_pk_algorithm_get_name fn_gnutls_pk_algorithm_get_name
#  define gnutls_pk_bits_to_sec_param fn_gnutls_pk_bits_to_sec_param
#  define gnutls_priority_set_direct fn_gnutls_priority_set_direct
#  define gnutls_protocol_get_name fn_gnutls_protocol_get_name
#  define gnutls_protocol_get_version fn_gnutls_protocol_get_version
#  define gnutls_record_check_pending fn_gnutls_record_check_pending
#  define gnutls_record_recv fn_gnutls_record_recv
#  define gnutls_record_send fn_gnutls_record_send
#  define gnutls_sec_param_get_name fn_gnutls_sec_param_get_name
#  define gnutls_server_name_set fn_gnutls_server_name_set
#  define gnutls_sign_get_name fn_gnutls_sign_get_name
#  define gnutls_strerror fn_gnutls_strerror
#  define gnutls_transport_set_errno fn_gnutls_transport_set_errno
#  define gnutls_transport_set_ptr2 fn_gnutls_transport_set_ptr2
#  define gnutls_transport_set_pull_function fn_gnutls_transport_set_pull_function
#  define gnutls_transport_set_push_function fn_gnutls_transport_set_push_function
#  define gnutls_x509_crt_check_hostname fn_gnutls_x509_crt_check_hostname
#  define gnutls_x509_crt_check_issuer fn_gnutls_x509_crt_check_issuer
#  define gnutls_x509_crt_deinit fn_gnutls_x509_crt_deinit
#  define gnutls_x509_crt_get_activation_time fn_gnutls_x509_crt_get_activation_time
#  define gnutls_x509_crt_get_dn fn_gnutls_x509_crt_get_dn
#  define gnutls_x509_crt_get_expiration_time fn_gnutls_x509_crt_get_expiration_time
#  define gnutls_x509_crt_get_fingerprint fn_gnutls_x509_crt_get_fingerprint
#  define gnutls_x509_crt_get_issuer_dn fn_gnutls_x509_crt_get_issuer_dn
#  define gnutls_x509_crt_get_issuer_unique_id fn_gnutls_x509_crt_get_issuer_unique_id
#  define gnutls_x509_crt_get_key_id fn_gnutls_x509_crt_get_key_id
#  define gnutls_x509_crt_get_pk_algorithm fn_gnutls_x509_crt_get_pk_algorithm
#  define gnutls_x509_crt_get_serial fn_gnutls_x509_crt_get_serial
#  define gnutls_x509_crt_get_signature_algorithm fn_gnutls_x509_crt_get_signature_algorithm
#  define gnutls_x509_crt_get_subject_unique_id fn_gnutls_x509_crt_get_subject_unique_id
#  define gnutls_x509_crt_get_version fn_gnutls_x509_crt_get_version
#  define gnutls_x509_crt_import fn_gnutls_x509_crt_import
#  define gnutls_x509_crt_init fn_gnutls_x509_crt_init
#  ifdef HAVE_GNUTLS3
#  define gnutls_rnd fn_gnutls_rnd
#  define gnutls_mac_list fn_gnutls_mac_list
#   ifdef HAVE_GNUTLS_MAC_GET_NONCE_SIZE
#    define gnutls_mac_get_nonce_size fn_gnutls_mac_get_nonce_size
#   endif
#  define gnutls_mac_get_key_size fn_gnutls_mac_get_key_size
#  define gnutls_digest_list fn_gnutls_digest_list
#  define gnutls_digest_get_name fn_gnutls_digest_get_name
#  define gnutls_cipher_list fn_gnutls_cipher_list
#  define gnutls_cipher_get_iv_size fn_gnutls_cipher_get_iv_size
#  define gnutls_cipher_get_key_size fn_gnutls_cipher_get_key_size
#  define gnutls_cipher_get_block_size fn_gnutls_cipher_get_block_size
#  define gnutls_cipher_get_tag_size fn_gnutls_cipher_get_tag_size
#  define gnutls_cipher_init fn_gnutls_cipher_init
#  define gnutls_cipher_set_iv fn_gnutls_cipher_set_iv
#  define gnutls_cipher_encrypt2 fn_gnutls_cipher_encrypt2
#  define gnutls_cipher_decrypt2 fn_gnutls_cipher_decrypt2
#  define gnutls_cipher_deinit fn_gnutls_cipher_deinit
#   ifdef HAVE_GNUTLS_AEAD
#    define gnutls_aead_cipher_encrypt fn_gnutls_aead_cipher_encrypt
#    define gnutls_aead_cipher_decrypt fn_gnutls_aead_cipher_decrypt
#    define gnutls_aead_cipher_init fn_gnutls_aead_cipher_init
#    define gnutls_aead_cipher_deinit fn_gnutls_aead_cipher_deinit
#   endif
#  define gnutls_hmac_init fn_gnutls_hmac_init
#  define gnutls_hmac_get_len fn_gnutls_hmac_get_len
#  define gnutls_hmac fn_gnutls_hmac
#  define gnutls_hmac_deinit fn_gnutls_hmac_deinit
#  define gnutls_hmac_output fn_gnutls_hmac_output
#  define gnutls_hash_init fn_gnutls_hash_init
#  define gnutls_hash_get_len fn_gnutls_hash_get_len
#  define gnutls_hash fn_gnutls_hash
#  define gnutls_hash_deinit fn_gnutls_hash_deinit
#  define gnutls_hash_output fn_gnutls_hash_output
#   ifdef HAVE_GNUTLS_EXT_GET_NAME
#    define gnutls_ext_get_name fn_gnutls_ext_get_name
#   endif
#  endif	 /* HAVE_GNUTLS3 */


/* This wrapper is called from fns.c, which doesn't know about the
   LOAD_DLL_FN stuff above.  */
int
w32_gnutls_rnd (gnutls_rnd_level_t level, void *data, size_t len)
{
  return gnutls_rnd (level, data, len);
}

# endif	/* WINDOWSNT */


/* Report memory exhaustion if ERR is an out-of-memory indication.  */
static void
check_memory_full (int err)
{
  /* When GnuTLS exhausts memory, it doesn't say how much memory it
     asked for, so tell the Emacs allocator that GnuTLS asked for no
     bytes.  This isn't accurate, but it's good enough.  */
  if (err == GNUTLS_E_MEMORY_ERROR)
    memory_full (0);
}

# ifdef HAVE_GNUTLS3
/* Log a simple audit message.  */
static void
gnutls_audit_log_function (gnutls_session_t session, const char *string)
{
  if (global_gnutls_log_level >= 1)
    {
      message ("gnutls.c: [audit] %s", string);
    }
}
# endif

/* Log a simple message.  */
static void
gnutls_log_function (int level, const char *string)
{
  message ("gnutls.c: [%d] %s", level, string);
}

/* Log a message and a string.  */
static void
gnutls_log_function2 (int level, const char *string, const char *extra)
{
  message ("gnutls.c: [%d] %s %s", level, string, extra);
}

int
gnutls_try_handshake (struct Lisp_Process *proc)
{
  gnutls_session_t state = proc->gnutls_state;
  int ret;
  bool non_blocking = proc->is_non_blocking_client;

  if (proc->gnutls_complete_negotiation_p)
    non_blocking = false;

  if (non_blocking)
    proc->gnutls_p = true;

  do
    {
      ret = gnutls_handshake (state);
      emacs_gnutls_handle_error (state, ret);
      maybe_quit ();
    }
  while (ret < 0
	 && gnutls_error_is_fatal (ret) == 0
	 && ! non_blocking);

  proc->gnutls_initstage = GNUTLS_STAGE_HANDSHAKE_TRIED;

  if (ret == GNUTLS_E_SUCCESS)
    {
      /* Here we're finally done.  */
      proc->gnutls_initstage = GNUTLS_STAGE_READY;
    }
  else
    {
      /* check_memory_full (gnutls_alert_send_appropriate (state, ret));  */
    }
  return ret;
}

# ifndef WINDOWSNT
static int
emacs_gnutls_nonblock_errno (gnutls_transport_ptr_t ptr)
{
  int err = errno;

  switch (err)
    {
#  ifdef _AIX
      /* This is taken from the GnuTLS system_errno function circa 2016;
	 see <https://savannah.gnu.org/support/?107464>.  */
    case 0:
      errno = EAGAIN;
      /* Fall through.  */
#  endif
    case EINPROGRESS:
    case ENOTCONN:
      return EAGAIN;

    default:
      return err;
    }
}
# endif	/* !WINDOWSNT */

static int
emacs_gnutls_handshake (struct Lisp_Process *proc)
{
  gnutls_session_t state = proc->gnutls_state;

  if (proc->gnutls_initstage < GNUTLS_STAGE_HANDSHAKE_CANDO)
    return -1;

  if (proc->gnutls_initstage < GNUTLS_STAGE_TRANSPORT_POINTERS_SET)
    {
# ifdef WINDOWSNT
      /* On W32 we cannot transfer socket handles between different runtime
	 libraries, so we tell GnuTLS to use our special push/pull
	 functions.  */
      gnutls_transport_set_ptr2 (state,
				 (gnutls_transport_ptr_t) proc,
				 (gnutls_transport_ptr_t) proc);
      gnutls_transport_set_push_function (state, &emacs_gnutls_push);
      gnutls_transport_set_pull_function (state, &emacs_gnutls_pull);
# else
      /* This is how GnuTLS takes sockets: as file descriptors passed
	 in.  For an Emacs process socket, infd and outfd are the
	 same but we use this two-argument version for clarity.  */
      gnutls_transport_set_ptr2 (state,
				 (void *) (intptr_t) proc->infd,
				 (void *) (intptr_t) proc->outfd);
      if (proc->is_non_blocking_client)
	gnutls_transport_set_errno_function (state,
					     emacs_gnutls_nonblock_errno);
# endif

      proc->gnutls_initstage = GNUTLS_STAGE_TRANSPORT_POINTERS_SET;
    }

  return gnutls_try_handshake (proc);
}

ptrdiff_t
emacs_gnutls_record_check_pending (gnutls_session_t state)
{
  return gnutls_record_check_pending (state);
}

# ifdef WINDOWSNT
void
emacs_gnutls_transport_set_errno (gnutls_session_t state, int err)
{
  gnutls_transport_set_errno (state, err);
}
# endif

ptrdiff_t
emacs_gnutls_write (struct Lisp_Process *proc, const char *buf, ptrdiff_t nbyte)
{
  ssize_t rtnval = 0;
  ptrdiff_t bytes_written;
  gnutls_session_t state = proc->gnutls_state;

  if (proc->gnutls_initstage != GNUTLS_STAGE_READY)
    {
      errno = EAGAIN;
      return 0;
    }

  bytes_written = 0;

  while (nbyte > 0)
    {
      rtnval = gnutls_record_send (state, buf, nbyte);

      if (rtnval < 0)
	{
	  if (rtnval == GNUTLS_E_INTERRUPTED)
	    continue;
	  else
	    {
	      /* If we get GNUTLS_E_AGAIN, then set errno
		 appropriately so that send_process retries the
		 correct way instead of erroring out. */
	      if (rtnval == GNUTLS_E_AGAIN)
		errno = EAGAIN;
	      break;
	    }
	}

      buf += rtnval;
      nbyte -= rtnval;
      bytes_written += rtnval;
    }

  emacs_gnutls_handle_error (state, rtnval);
  return (bytes_written);
}

ptrdiff_t
emacs_gnutls_read (struct Lisp_Process *proc, char *buf, ptrdiff_t nbyte)
{
  ssize_t rtnval;
  gnutls_session_t state = proc->gnutls_state;

  if (proc->gnutls_initstage != GNUTLS_STAGE_READY)
    {
      errno = EAGAIN;
      return -1;
    }

  rtnval = gnutls_record_recv (state, buf, nbyte);
  if (rtnval >= 0)
    return rtnval;
  else if (rtnval == GNUTLS_E_UNEXPECTED_PACKET_LENGTH)
    /* The peer closed the connection. */
    return 0;
  else if (emacs_gnutls_handle_error (state, rtnval))
    /* non-fatal error */
    return -1;
  else {
    /* a fatal error occurred */
    return 0;
  }
}

static char const *
emacs_gnutls_strerror (int err)
{
  char const *str = gnutls_strerror (err);
  return str ? str : "unknown";
}

/* Report a GnuTLS error to the user.
   Return true if the error code was successfully handled.  */
static bool
emacs_gnutls_handle_error (gnutls_session_t session, int err)
{
  int max_log_level = 0;

  bool ret;

  /* TODO: use a Lisp_Object generated by gnutls_make_error?  */
  if (err >= 0)
    return 1;

  check_memory_full (err);

  max_log_level = global_gnutls_log_level;

  /* TODO: use gnutls-error-fatalp and gnutls-error-string.  */

  char const *str = emacs_gnutls_strerror (err);

  if (gnutls_error_is_fatal (err))
    {
      int level = 1;
      /* Mostly ignore "The TLS connection was non-properly
	 terminated" message which just means that the peer closed the
	 connection.  */
# ifdef HAVE_GNUTLS3
      if (err == GNUTLS_E_PREMATURE_TERMINATION)
	level = 3;
# endif

      GNUTLS_LOG2 (level, max_log_level, "fatal error:", str);
      ret = false;
    }
  else
    {
      ret = true;

      switch (err)
        {
        case GNUTLS_E_AGAIN:
          GNUTLS_LOG2 (3,
                       max_log_level,
                       "retry:",
                       str);
	  FALLTHROUGH;
        default:
          GNUTLS_LOG2 (1,
                       max_log_level,
                       "non-fatal error:",
                       str);
        }
    }

  if (err == GNUTLS_E_WARNING_ALERT_RECEIVED
      || err == GNUTLS_E_FATAL_ALERT_RECEIVED)
    {
      int alert = gnutls_alert_get (session);
      int level = (err == GNUTLS_E_FATAL_ALERT_RECEIVED) ? 0 : 1;
      str = gnutls_alert_get_name (alert);
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

  check_memory_full (err);
  return make_number (err);
}

Lisp_Object
emacs_gnutls_deinit (Lisp_Object proc)
{
  int log_level;

  CHECK_PROCESS (proc);

  if (! XPROCESS (proc)->gnutls_p)
    return Qnil;

  log_level = XPROCESS (proc)->gnutls_log_level;

  if (XPROCESS (proc)->gnutls_x509_cred)
    {
      GNUTLS_LOG (2, log_level, "Deallocating x509 credentials");
      gnutls_certificate_free_credentials (XPROCESS (proc)->gnutls_x509_cred);
      XPROCESS (proc)->gnutls_x509_cred = NULL;
    }

  if (XPROCESS (proc)->gnutls_anon_cred)
    {
      GNUTLS_LOG (2, log_level, "Deallocating anon credentials");
      gnutls_anon_free_client_credentials (XPROCESS (proc)->gnutls_anon_cred);
      XPROCESS (proc)->gnutls_anon_cred = NULL;
    }

  if (XPROCESS (proc)->gnutls_state)
    {
      gnutls_deinit (XPROCESS (proc)->gnutls_state);
      XPROCESS (proc)->gnutls_state = NULL;
      if (GNUTLS_INITSTAGE (proc) >= GNUTLS_STAGE_INIT)
	GNUTLS_INITSTAGE (proc) = GNUTLS_STAGE_INIT - 1;
    }

  XPROCESS (proc)->gnutls_p = false;
  return Qt;
}

DEFUN ("gnutls-asynchronous-parameters", Fgnutls_asynchronous_parameters,
       Sgnutls_asynchronous_parameters, 2, 2, 0,
       doc: /* Mark this process as being a pre-init GnuTLS process.
The second parameter is the list of parameters to feed to gnutls-boot
to finish setting up the connection. */)
  (Lisp_Object proc, Lisp_Object params)
{
  CHECK_PROCESS (proc);

  XPROCESS (proc)->gnutls_boot_parameters = params;
  return Qnil;
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
usage: (gnutls-errorp ERROR)  */
       attributes: const)
  (Lisp_Object err)
{
  if (EQ (err, Qt)
      || EQ (err, Qgnutls_e_again))
    return Qnil;

  return Qt;
}

DEFUN ("gnutls-error-fatalp", Fgnutls_error_fatalp, Sgnutls_error_fatalp, 1, 1, 0,
       doc: /* Return non-nil if ERROR is fatal.
ERROR is an integer or a symbol with an integer `gnutls-code' property.
Usage: (gnutls-error-fatalp ERROR)  */)
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

  if (! TYPE_RANGED_INTEGERP (int, err))
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

  if (! TYPE_RANGED_INTEGERP (int, err))
    return build_string ("Not an error symbol or code");

  return build_string (emacs_gnutls_strerror (XINT (err)));
}

DEFUN ("gnutls-deinit", Fgnutls_deinit, Sgnutls_deinit, 1, 1, 0,
       doc: /* Deallocate GnuTLS resources associated with process PROC.
See also `gnutls-init'.  */)
  (Lisp_Object proc)
{
  return emacs_gnutls_deinit (proc);
}

static Lisp_Object
gnutls_hex_string (unsigned char *buf, ptrdiff_t buf_size, const char *prefix)
{
  ptrdiff_t prefix_length = strlen (prefix);
  ptrdiff_t retlen;
  if (INT_MULTIPLY_WRAPV (buf_size, 3, &retlen)
      || INT_ADD_WRAPV (prefix_length - (buf_size != 0), retlen, &retlen))
    string_overflow ();
  Lisp_Object ret = make_uninit_string (retlen);
  char *string = SSDATA (ret);
  strcpy (string, prefix);

  for (ptrdiff_t i = 0; i < buf_size; i++)
    sprintf (string + i * 3 + prefix_length,
	     i == buf_size - 1 ? "%02x" : "%02x:",
	     buf[i]);

  return ret;
}

static Lisp_Object
gnutls_certificate_details (gnutls_x509_crt_t cert)
{
  Lisp_Object res = Qnil;
  int err;
  size_t buf_size;

  /* Version. */
  {
    int version = gnutls_x509_crt_get_version (cert);
    check_memory_full (version);
    if (version >= GNUTLS_E_SUCCESS)
      res = nconc2 (res, list2 (intern (":version"),
				make_number (version)));
  }

  /* Serial. */
  buf_size = 0;
  err = gnutls_x509_crt_get_serial (cert, NULL, &buf_size);
  check_memory_full (err);
  if (err == GNUTLS_E_SHORT_MEMORY_BUFFER)
    {
      void *serial = xmalloc (buf_size);
      err = gnutls_x509_crt_get_serial (cert, serial, &buf_size);
      check_memory_full (err);
      if (err >= GNUTLS_E_SUCCESS)
	res = nconc2 (res, list2 (intern (":serial-number"),
				  gnutls_hex_string (serial, buf_size, "")));
      xfree (serial);
    }

  /* Issuer. */
  buf_size = 0;
  err = gnutls_x509_crt_get_issuer_dn (cert, NULL, &buf_size);
  check_memory_full (err);
  if (err == GNUTLS_E_SHORT_MEMORY_BUFFER)
    {
      char *dn = xmalloc (buf_size);
      err = gnutls_x509_crt_get_issuer_dn (cert, dn, &buf_size);
      check_memory_full (err);
      if (err >= GNUTLS_E_SUCCESS)
	res = nconc2 (res, list2 (intern (":issuer"),
				  make_string (dn, buf_size)));
      xfree (dn);
    }

  /* Validity. */
  {
    /* Add 1 to the buffer size, since 1900 is added to tm_year and
       that might add 1 to the year length.  */
    char buf[INT_STRLEN_BOUND (int) + 1 + sizeof "-12-31"];
    struct tm t;
    time_t tim = gnutls_x509_crt_get_activation_time (cert);

    if (gmtime_r (&tim, &t) && strftime (buf, sizeof buf, "%Y-%m-%d", &t))
      res = nconc2 (res, list2 (intern (":valid-from"), build_string (buf)));

    tim = gnutls_x509_crt_get_expiration_time (cert);
    if (gmtime_r (&tim, &t) && strftime (buf, sizeof buf, "%Y-%m-%d", &t))
      res = nconc2 (res, list2 (intern (":valid-to"), build_string (buf)));
  }

  /* Subject. */
  buf_size = 0;
  err = gnutls_x509_crt_get_dn (cert, NULL, &buf_size);
  check_memory_full (err);
  if (err == GNUTLS_E_SHORT_MEMORY_BUFFER)
    {
      char *dn = xmalloc (buf_size);
      err = gnutls_x509_crt_get_dn (cert, dn, &buf_size);
      check_memory_full (err);
      if (err >= GNUTLS_E_SUCCESS)
	res = nconc2 (res, list2 (intern (":subject"),
				  make_string (dn, buf_size)));
      xfree (dn);
    }

  /* SubjectPublicKeyInfo. */
  {
    unsigned int bits;

    err = gnutls_x509_crt_get_pk_algorithm (cert, &bits);
    check_memory_full (err);
    if (err >= GNUTLS_E_SUCCESS)
      {
	const char *name = gnutls_pk_algorithm_get_name (err);
	if (name)
	  res = nconc2 (res, list2 (intern (":public-key-algorithm"),
				    build_string (name)));

	name = gnutls_sec_param_get_name (gnutls_pk_bits_to_sec_param
					  (err, bits));
	res = nconc2 (res, list2 (intern (":certificate-security-level"),
				  build_string (name)));
      }
  }

  /* Unique IDs. */
  buf_size = 0;
  err = gnutls_x509_crt_get_issuer_unique_id (cert, NULL, &buf_size);
  check_memory_full (err);
  if (err == GNUTLS_E_SHORT_MEMORY_BUFFER)
    {
      char *buf = xmalloc (buf_size);
      err = gnutls_x509_crt_get_issuer_unique_id (cert, buf, &buf_size);
      check_memory_full (err);
      if (err >= GNUTLS_E_SUCCESS)
	res = nconc2 (res, list2 (intern (":issuer-unique-id"),
				  make_string (buf, buf_size)));
      xfree (buf);
    }

  buf_size = 0;
  err = gnutls_x509_crt_get_subject_unique_id (cert, NULL, &buf_size);
  check_memory_full (err);
  if (err == GNUTLS_E_SHORT_MEMORY_BUFFER)
    {
      char *buf = xmalloc (buf_size);
      err = gnutls_x509_crt_get_subject_unique_id (cert, buf, &buf_size);
      check_memory_full (err);
      if (err >= GNUTLS_E_SUCCESS)
	res = nconc2 (res, list2 (intern (":subject-unique-id"),
				  make_string (buf, buf_size)));
      xfree (buf);
    }

  /* Signature. */
  err = gnutls_x509_crt_get_signature_algorithm (cert);
  check_memory_full (err);
  if (err >= GNUTLS_E_SUCCESS)
    {
      const char *name = gnutls_sign_get_name (err);
      if (name)
	res = nconc2 (res, list2 (intern (":signature-algorithm"),
				  build_string (name)));
    }

  /* Public key ID. */
  buf_size = 0;
  err = gnutls_x509_crt_get_key_id (cert, 0, NULL, &buf_size);
  check_memory_full (err);
  if (err == GNUTLS_E_SHORT_MEMORY_BUFFER)
    {
      void *buf = xmalloc (buf_size);
      err = gnutls_x509_crt_get_key_id (cert, 0, buf, &buf_size);
      check_memory_full (err);
      if (err >= GNUTLS_E_SUCCESS)
	res = nconc2 (res, list2 (intern (":public-key-id"),
				  gnutls_hex_string (buf, buf_size, "sha1:")));
      xfree (buf);
    }

  /* Certificate fingerprint. */
  buf_size = 0;
  err = gnutls_x509_crt_get_fingerprint (cert, GNUTLS_DIG_SHA1,
					 NULL, &buf_size);
  check_memory_full (err);
  if (err == GNUTLS_E_SHORT_MEMORY_BUFFER)
    {
      void *buf = xmalloc (buf_size);
      err = gnutls_x509_crt_get_fingerprint (cert, GNUTLS_DIG_SHA1,
					     buf, &buf_size);
      check_memory_full (err);
      if (err >= GNUTLS_E_SUCCESS)
	res = nconc2 (res, list2 (intern (":certificate-id"),
				  gnutls_hex_string (buf, buf_size, "sha1:")));
      xfree (buf);
    }

  return res;
}

DEFUN ("gnutls-peer-status-warning-describe", Fgnutls_peer_status_warning_describe, Sgnutls_peer_status_warning_describe, 1, 1, 0,
       doc: /* Describe the warning of a GnuTLS peer status from `gnutls-peer-status'.  */)
  (Lisp_Object status_symbol)
{
  CHECK_SYMBOL (status_symbol);

  if (EQ (status_symbol, intern (":invalid")))
    return build_string ("certificate could not be verified");

  if (EQ (status_symbol, intern (":revoked")))
    return build_string ("certificate was revoked (CRL)");

  if (EQ (status_symbol, intern (":self-signed")))
    return build_string ("certificate signer was not found (self-signed)");

  if (EQ (status_symbol, intern (":unknown-ca")))
    return build_string ("the certificate was signed by an unknown "
                         "and therefore untrusted authority");

  if (EQ (status_symbol, intern (":not-ca")))
    return build_string ("certificate signer is not a CA");

  if (EQ (status_symbol, intern (":insecure")))
    return build_string ("certificate was signed with an insecure algorithm");

  if (EQ (status_symbol, intern (":not-activated")))
    return build_string ("certificate is not yet activated");

  if (EQ (status_symbol, intern (":expired")))
    return build_string ("certificate has expired");

  if (EQ (status_symbol, intern (":no-host-match")))
    return build_string ("certificate host does not match hostname");

  return Qnil;
}

DEFUN ("gnutls-peer-status", Fgnutls_peer_status, Sgnutls_peer_status, 1, 1, 0,
       doc: /* Describe a GnuTLS PROC peer certificate and any warnings about it.
The return value is a property list with top-level keys :warnings and
:certificate.  The :warnings entry is a list of symbols you can describe with
`gnutls-peer-status-warning-describe'. */)
  (Lisp_Object proc)
{
  Lisp_Object warnings = Qnil, result = Qnil;
  unsigned int verification;
  gnutls_session_t state;

  CHECK_PROCESS (proc);

  if (GNUTLS_INITSTAGE (proc) != GNUTLS_STAGE_READY)
    return Qnil;

  /* Then collect any warnings already computed by the handshake. */
  verification = XPROCESS (proc)->gnutls_peer_verification;

  if (verification & GNUTLS_CERT_INVALID)
    warnings = Fcons (intern (":invalid"), warnings);

  if (verification & GNUTLS_CERT_REVOKED)
    warnings = Fcons (intern (":revoked"), warnings);

  if (verification & GNUTLS_CERT_SIGNER_NOT_FOUND)
    warnings = Fcons (intern (":unknown-ca"), warnings);

  if (verification & GNUTLS_CERT_SIGNER_NOT_CA)
    warnings = Fcons (intern (":not-ca"), warnings);

  if (verification & GNUTLS_CERT_INSECURE_ALGORITHM)
    warnings = Fcons (intern (":insecure"), warnings);

  if (verification & GNUTLS_CERT_NOT_ACTIVATED)
    warnings = Fcons (intern (":not-activated"), warnings);

  if (verification & GNUTLS_CERT_EXPIRED)
    warnings = Fcons (intern (":expired"), warnings);

  if (XPROCESS (proc)->gnutls_extra_peer_verification &
      CERTIFICATE_NOT_MATCHING)
    warnings = Fcons (intern (":no-host-match"), warnings);

  /* This could get called in the INIT stage, when the certificate is
     not yet set. */
  if (XPROCESS (proc)->gnutls_certificate != NULL &&
      gnutls_x509_crt_check_issuer(XPROCESS (proc)->gnutls_certificate,
                                   XPROCESS (proc)->gnutls_certificate))
    warnings = Fcons (intern (":self-signed"), warnings);

  if (!NILP (warnings))
    result = list2 (intern (":warnings"), warnings);

  /* This could get called in the INIT stage, when the certificate is
     not yet set. */
  if (XPROCESS (proc)->gnutls_certificate != NULL)
    result = nconc2 (result, list2
                     (intern (":certificate"),
                      gnutls_certificate_details (XPROCESS (proc)->gnutls_certificate)));

  state = XPROCESS (proc)->gnutls_state;

  /* Diffie-Hellman prime bits. */
  {
    int bits = gnutls_dh_get_prime_bits (state);
    check_memory_full (bits);
    if (bits > 0)
      result = nconc2 (result, list2 (intern (":diffie-hellman-prime-bits"),
				      make_number (bits)));
  }

  /* Key exchange. */
  result = nconc2
    (result, list2 (intern (":key-exchange"),
		    build_string (gnutls_kx_get_name
				  (gnutls_kx_get (state)))));

  /* Protocol name. */
  result = nconc2
    (result, list2 (intern (":protocol"),
		    build_string (gnutls_protocol_get_name
				  (gnutls_protocol_get_version (state)))));

  /* Cipher name. */
  result = nconc2
    (result, list2 (intern (":cipher"),
		    build_string (gnutls_cipher_get_name
				  (gnutls_cipher_get (state)))));

  /* MAC name. */
  result = nconc2
    (result, list2 (intern (":mac"),
		    build_string (gnutls_mac_get_name
				  (gnutls_mac_get (state)))));


  return result;
}

/* Initialize global GnuTLS state to defaults.
   Call `gnutls-global-deinit' when GnuTLS usage is no longer needed.
   Return zero on success.  */
Lisp_Object
emacs_gnutls_global_init (void)
{
  int ret = GNUTLS_E_SUCCESS;

  if (!gnutls_global_initialized)
    {
      ret = gnutls_global_init ();
      if (ret == GNUTLS_E_SUCCESS)
	gnutls_global_initialized = 1;
    }

  return gnutls_make_error (ret);
}

static bool
gnutls_ip_address_p (char *string)
{
  char c;

  while ((c = *string++) != 0)
    if (! ((c == '.' || c == ':' || (c >= '0' && c <= '9'))))
      return false;

  return true;
}

# if 0
/* Deinitialize global GnuTLS state.
   See also `gnutls-global-init'.  */
static Lisp_Object
emacs_gnutls_global_deinit (void)
{
  if (gnutls_global_initialized)
    gnutls_global_deinit ();

  gnutls_global_initialized = 0;

  return gnutls_make_error (GNUTLS_E_SUCCESS);
}
# endif

static void ATTRIBUTE_FORMAT_PRINTF (2, 3)
boot_error (struct Lisp_Process *p, const char *m, ...)
{
  va_list ap;
  va_start (ap, m);
  if (p->is_non_blocking_client)
    pset_status (p, list2 (Qfailed, vformat_string (m, ap)));
  else
    verror (m, ap);
  va_end (ap);
}

Lisp_Object
gnutls_verify_boot (Lisp_Object proc, Lisp_Object proplist)
{
  int ret;
  struct Lisp_Process *p = XPROCESS (proc);
  gnutls_session_t state = p->gnutls_state;
  unsigned int peer_verification;
  Lisp_Object warnings;
  int max_log_level = p->gnutls_log_level;
  Lisp_Object hostname, verify_error;
  bool verify_error_all = false;
  char *c_hostname;

  if (NILP (proplist))
    proplist = Fcdr (Fplist_get (p->childp, QCtls_parameters));

  verify_error = Fplist_get (proplist, QCverify_error);
  hostname = Fplist_get (proplist, QChostname);

  if (EQ (verify_error, Qt))
    verify_error_all = true;
  else if (NILP (Flistp (verify_error)))
    {
      boot_error (p,
		  "gnutls-boot: invalid :verify_error parameter (not a list)");
      return Qnil;
    }

  if (!STRINGP (hostname))
    {
      boot_error (p, "gnutls-boot: invalid :hostname parameter (not a string)");
      return Qnil;
    }
  c_hostname = SSDATA (hostname);

  /* Now verify the peer, following
     https://www.gnu.org/software/gnutls/manual/html_node/Verifying-peer_0027s-certificate.html.
     The peer should present at least one certificate in the chain; do a
     check of the certificate's hostname with
     gnutls_x509_crt_check_hostname against :hostname.  */

  ret = gnutls_certificate_verify_peers2 (state, &peer_verification);
  if (ret < GNUTLS_E_SUCCESS)
    return gnutls_make_error (ret);

  XPROCESS (proc)->gnutls_peer_verification = peer_verification;

  warnings = Fplist_get (Fgnutls_peer_status (proc), intern (":warnings"));
  if (!NILP (warnings))
    {
      for (Lisp_Object tail = warnings; CONSP (tail); tail = XCDR (tail))
        {
          Lisp_Object warning = XCAR (tail);
          Lisp_Object message = Fgnutls_peer_status_warning_describe (warning);
          if (!NILP (message))
            GNUTLS_LOG2 (1, max_log_level, "verification:", SSDATA (message));
        }
    }

  if (peer_verification != 0)
    {
      if (verify_error_all
          || !NILP (Fmember (QCtrustfiles, verify_error)))
        {
	  emacs_gnutls_deinit (proc);
	  boot_error (p,
		      "Certificate validation failed %s, verification code %x",
		      c_hostname, peer_verification);
	  return Qnil;
        }
      else
	{
          GNUTLS_LOG2 (1, max_log_level, "certificate validation failed:",
                       c_hostname);
	}
    }

  /* Up to here the process is the same for X.509 certificates and
     OpenPGP keys.  From now on X.509 certificates are assumed.  This
     can be easily extended to work with openpgp keys as well.  */
  if (gnutls_certificate_type_get (state) == GNUTLS_CRT_X509)
    {
      gnutls_x509_crt_t gnutls_verify_cert;
      const gnutls_datum_t *gnutls_verify_cert_list;
      unsigned int gnutls_verify_cert_list_size;

      ret = gnutls_x509_crt_init (&gnutls_verify_cert);
      if (ret < GNUTLS_E_SUCCESS)
	return gnutls_make_error (ret);

      gnutls_verify_cert_list
	= gnutls_certificate_get_peers (state, &gnutls_verify_cert_list_size);

      if (gnutls_verify_cert_list == NULL)
	{
	  gnutls_x509_crt_deinit (gnutls_verify_cert);
	  emacs_gnutls_deinit (proc);
	  boot_error (p, "No x509 certificate was found\n");
	  return Qnil;
	}

      /* Check only the first certificate in the given chain.  */
      ret = gnutls_x509_crt_import (gnutls_verify_cert,
				    &gnutls_verify_cert_list[0],
				    GNUTLS_X509_FMT_DER);

      if (ret < GNUTLS_E_SUCCESS)
	{
	  gnutls_x509_crt_deinit (gnutls_verify_cert);
	  return gnutls_make_error (ret);
	}

      XPROCESS (proc)->gnutls_certificate = gnutls_verify_cert;

      int err = gnutls_x509_crt_check_hostname (gnutls_verify_cert,
						c_hostname);
      check_memory_full (err);
      if (!err)
	{
	  XPROCESS (proc)->gnutls_extra_peer_verification
	    |= CERTIFICATE_NOT_MATCHING;
          if (verify_error_all
              || !NILP (Fmember (QChostname, verify_error)))
            {
	      gnutls_x509_crt_deinit (gnutls_verify_cert);
	      emacs_gnutls_deinit (proc);
	      boot_error (p, "The x509 certificate does not match \"%s\"",
			  c_hostname);
	      return Qnil;
            }
	  else
	    GNUTLS_LOG2 (1, max_log_level, "x509 certificate does not match:",
			 c_hostname);
	}
    }

  /* Set this flag only if the whole initialization succeeded.  */
  XPROCESS (proc)->gnutls_p = true;

  return gnutls_make_error (ret);
}

DEFUN ("gnutls-boot", Fgnutls_boot, Sgnutls_boot, 3, 3, 0,
       doc: /* Initialize GnuTLS client for process PROC with TYPE+PROPLIST.
Currently only client mode is supported.  Return a success/failure
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

:verify-hostname-error is ignored.  Pass :hostname in :verify-error
instead.

:verify-error is a list of symbols to express verification checks or
t to do all checks.  Currently it can contain `:trustfiles' and
`:hostname' to verify the certificate or the hostname respectively.

:min-prime-bits is the minimum accepted number of bits the client will
accept in Diffie-Hellman key exchange.

:complete-negotiation, if non-nil, will make negotiation complete
before returning even on non-blocking sockets.

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

  gnutls_session_t state;
  gnutls_certificate_credentials_t x509_cred = NULL;
  gnutls_anon_client_credentials_t anon_cred = NULL;
  Lisp_Object global_init;
  char const *priority_string_ptr = "NORMAL"; /* default priority string.  */
  char *c_hostname;

  /* Placeholders for the property list elements.  */
  Lisp_Object priority_string;
  Lisp_Object trustfiles;
  Lisp_Object crlfiles;
  Lisp_Object keylist;
  /* Lisp_Object callbacks; */
  Lisp_Object loglevel;
  Lisp_Object hostname;
  Lisp_Object prime_bits;
  struct Lisp_Process *p = XPROCESS (proc);

  CHECK_PROCESS (proc);
  CHECK_SYMBOL (type);
  CHECK_LIST (proplist);

  if (NILP (Fgnutls_available_p ()))
    {
      boot_error (p, "GnuTLS not available");
      return Qnil;
    }

  if (!EQ (type, Qgnutls_x509pki) && !EQ (type, Qgnutls_anon))
    {
      boot_error (p, "Invalid GnuTLS credential type");
      return Qnil;
    }

  hostname              = Fplist_get (proplist, QChostname);
  priority_string       = Fplist_get (proplist, QCpriority);
  trustfiles            = Fplist_get (proplist, QCtrustfiles);
  keylist               = Fplist_get (proplist, QCkeylist);
  crlfiles              = Fplist_get (proplist, QCcrlfiles);
  loglevel              = Fplist_get (proplist, QCloglevel);
  prime_bits            = Fplist_get (proplist, QCmin_prime_bits);

  if (!STRINGP (hostname))
    {
      boot_error (p, "gnutls-boot: invalid :hostname parameter (not a string)");
      return Qnil;
    }
  c_hostname = SSDATA (hostname);

  state = XPROCESS (proc)->gnutls_state;

  if (TYPE_RANGED_INTEGERP (int, loglevel))
    {
      gnutls_global_set_log_function (gnutls_log_function);
# ifdef HAVE_GNUTLS3
      gnutls_global_set_audit_log_function (gnutls_audit_log_function);
# endif
      gnutls_global_set_log_level (XINT (loglevel));
      max_log_level = XINT (loglevel);
      XPROCESS (proc)->gnutls_log_level = max_log_level;
    }

  GNUTLS_LOG2 (1, max_log_level, "connecting to host:", c_hostname);

  /* Always initialize globals.  */
  global_init = emacs_gnutls_global_init ();
  if (! NILP (Fgnutls_errorp (global_init)))
    return global_init;

  /* Before allocating new credentials, deallocate any credentials
     that PROC might already have.  */
  emacs_gnutls_deinit (proc);

  /* Mark PROC as a GnuTLS process.  */
  XPROCESS (proc)->gnutls_state = NULL;
  XPROCESS (proc)->gnutls_x509_cred = NULL;
  XPROCESS (proc)->gnutls_anon_cred = NULL;
  pset_gnutls_cred_type (XPROCESS (proc), type);
  GNUTLS_INITSTAGE (proc) = GNUTLS_STAGE_EMPTY;

  GNUTLS_LOG (1, max_log_level, "allocating credentials");
  if (EQ (type, Qgnutls_x509pki))
    {
      Lisp_Object verify_flags;
      unsigned int gnutls_verify_flags = GNUTLS_VERIFY_ALLOW_X509_V1_CA_CRT;

      GNUTLS_LOG (2, max_log_level, "allocating x509 credentials");
      check_memory_full (gnutls_certificate_allocate_credentials (&x509_cred));
      XPROCESS (proc)->gnutls_x509_cred = x509_cred;

      verify_flags = Fplist_get (proplist, QCverify_flags);
      if (TYPE_RANGED_INTEGERP (unsigned int, verify_flags))
	{
	  gnutls_verify_flags = XFASTINT (verify_flags);
	  GNUTLS_LOG (2, max_log_level, "setting verification flags");
	}
      else if (NILP (verify_flags))
	GNUTLS_LOG (2, max_log_level, "using default verification flags");
      else
	GNUTLS_LOG (2, max_log_level, "ignoring invalid verify-flags");

      gnutls_certificate_set_verify_flags (x509_cred, gnutls_verify_flags);
    }
  else /* Qgnutls_anon: */
    {
      GNUTLS_LOG (2, max_log_level, "allocating anon credentials");
      check_memory_full (gnutls_anon_allocate_client_credentials (&anon_cred));
      XPROCESS (proc)->gnutls_anon_cred = anon_cred;
    }

  GNUTLS_INITSTAGE (proc) = GNUTLS_STAGE_CRED_ALLOC;

  if (EQ (type, Qgnutls_x509pki))
    {
      /* TODO: GNUTLS_X509_FMT_DER is also an option.  */
      int file_format = GNUTLS_X509_FMT_PEM;
      Lisp_Object tail;

# ifdef HAVE_GNUTLS_X509_SYSTEM_TRUST
      ret = gnutls_certificate_set_x509_system_trust (x509_cred);
      if (ret < GNUTLS_E_SUCCESS)
	{
	  check_memory_full (ret);
	  GNUTLS_LOG2i (4, max_log_level,
			"setting system trust failed with code ", ret);
	}
# endif

      for (tail = trustfiles; CONSP (tail); tail = XCDR (tail))
	{
	  Lisp_Object trustfile = XCAR (tail);
	  if (STRINGP (trustfile))
	    {
	      GNUTLS_LOG2 (1, max_log_level, "setting the trustfile: ",
			   SSDATA (trustfile));
	      trustfile = ENCODE_FILE (trustfile);
# ifdef WINDOWSNT
	      /* Since GnuTLS doesn't support UTF-8 or UTF-16 encoded
		 file names on Windows, we need to re-encode the file
		 name using the current ANSI codepage.  */
	      trustfile = ansi_encode_filename (trustfile);
# endif
	      ret = gnutls_certificate_set_x509_trust_file
		(x509_cred,
		 SSDATA (trustfile),
		 file_format);

	      if (ret < GNUTLS_E_SUCCESS)
		return gnutls_make_error (ret);
	    }
	  else
	    {
	      emacs_gnutls_deinit (proc);
	      boot_error (p, "Invalid trustfile");
	      return Qnil;
	    }
	}

      for (tail = crlfiles; CONSP (tail); tail = XCDR (tail))
	{
	  Lisp_Object crlfile = XCAR (tail);
	  if (STRINGP (crlfile))
	    {
	      GNUTLS_LOG2 (1, max_log_level, "setting the CRL file: ",
			   SSDATA (crlfile));
	      crlfile = ENCODE_FILE (crlfile);
# ifdef WINDOWSNT
	      crlfile = ansi_encode_filename (crlfile);
# endif
	      ret = gnutls_certificate_set_x509_crl_file
		(x509_cred, SSDATA (crlfile), file_format);

	      if (ret < GNUTLS_E_SUCCESS)
		return gnutls_make_error (ret);
	    }
	  else
	    {
	      emacs_gnutls_deinit (proc);
	      boot_error (p, "Invalid CRL file");
	      return Qnil;
	    }
	}

      for (tail = keylist; CONSP (tail); tail = XCDR (tail))
	{
	  Lisp_Object keyfile = Fcar (XCAR (tail));
	  Lisp_Object certfile = Fcar (Fcdr (XCAR (tail)));
	  if (STRINGP (keyfile) && STRINGP (certfile))
	    {
	      GNUTLS_LOG2 (1, max_log_level, "setting the client key file: ",
			   SSDATA (keyfile));
	      GNUTLS_LOG2 (1, max_log_level, "setting the client cert file: ",
			   SSDATA (certfile));
	      keyfile = ENCODE_FILE (keyfile);
	      certfile = ENCODE_FILE (certfile);
# ifdef WINDOWSNT
	      keyfile = ansi_encode_filename (keyfile);
	      certfile = ansi_encode_filename (certfile);
# endif
	      ret = gnutls_certificate_set_x509_key_file
		(x509_cred, SSDATA (certfile), SSDATA (keyfile), file_format);

	      if (ret < GNUTLS_E_SUCCESS)
		return gnutls_make_error (ret);
	    }
	  else
	    {
	      emacs_gnutls_deinit (proc);
	      boot_error (p, STRINGP (keyfile) ? "Invalid client cert file"
			  : "Invalid client key file");
	      return Qnil;
	    }
	}
    }

  GNUTLS_INITSTAGE (proc) = GNUTLS_STAGE_FILES;
  GNUTLS_LOG (1, max_log_level, "gnutls callbacks");
  GNUTLS_INITSTAGE (proc) = GNUTLS_STAGE_CALLBACKS;

  /* Call gnutls_init here: */

  GNUTLS_LOG (1, max_log_level, "gnutls_init");
  int gnutls_flags = GNUTLS_CLIENT;
# ifdef GNUTLS_NONBLOCK
  if (XPROCESS (proc)->is_non_blocking_client)
    gnutls_flags |= GNUTLS_NONBLOCK;
# endif
  ret = gnutls_init (&state, gnutls_flags);
  XPROCESS (proc)->gnutls_state = state;
  if (ret < GNUTLS_E_SUCCESS)
    return gnutls_make_error (ret);
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
  ret = gnutls_priority_set_direct (state, priority_string_ptr, NULL);
  if (ret < GNUTLS_E_SUCCESS)
    return gnutls_make_error (ret);

  GNUTLS_INITSTAGE (proc) = GNUTLS_STAGE_PRIORITY;

  if (INTEGERP (prime_bits))
    gnutls_dh_set_prime_bits (state, XUINT (prime_bits));

  ret = EQ (type, Qgnutls_x509pki)
    ? gnutls_credentials_set (state, GNUTLS_CRD_CERTIFICATE, x509_cred)
    : gnutls_credentials_set (state, GNUTLS_CRD_ANON, anon_cred);
  if (ret < GNUTLS_E_SUCCESS)
    return gnutls_make_error (ret);

  if (!gnutls_ip_address_p (c_hostname))
    {
      ret = gnutls_server_name_set (state, GNUTLS_NAME_DNS, c_hostname,
				    strlen (c_hostname));
      if (ret < GNUTLS_E_SUCCESS)
	return gnutls_make_error (ret);
    }

  XPROCESS (proc)->gnutls_complete_negotiation_p =
    !NILP (Fplist_get (proplist, QCcomplete_negotiation));
  GNUTLS_INITSTAGE (proc) = GNUTLS_STAGE_CRED_SET;
  ret = emacs_gnutls_handshake (XPROCESS (proc));
  if (ret < GNUTLS_E_SUCCESS)
    return gnutls_make_error (ret);

  return gnutls_verify_boot (proc, proplist);
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

  gnutls_x509_crt_deinit (XPROCESS (proc)->gnutls_certificate);

  ret = gnutls_bye (state, NILP (cont) ? GNUTLS_SHUT_RDWR : GNUTLS_SHUT_WR);

  return gnutls_make_error (ret);
}

#endif	/* HAVE_GNUTLS */

#ifdef HAVE_GNUTLS3

DEFUN ("gnutls-ciphers", Fgnutls_ciphers, Sgnutls_ciphers, 0, 0, 0,
       doc: /* Return alist of GnuTLS symmetric cipher descriptions as plists.
The alist key is the cipher name. */)
  (void)
{
  Lisp_Object ciphers = Qnil;

  const gnutls_cipher_algorithm_t *gciphers = gnutls_cipher_list ();
  for (ptrdiff_t pos = 0; gciphers[pos] != 0; pos++)
    {
      gnutls_cipher_algorithm_t gca = gciphers[pos];
      if (gca == GNUTLS_CIPHER_NULL)
	continue;
      char const *cipher_name = gnutls_cipher_get_name (gca);
      if (!cipher_name)
	continue;

      /* A symbol representing the GnuTLS cipher.  */
      Lisp_Object cipher_symbol = intern (cipher_name);

      ptrdiff_t cipher_tag_size = gnutls_cipher_get_tag_size (gca);

      Lisp_Object cp
	= listn (CONSTYPE_HEAP, 15, cipher_symbol,
		 QCcipher_id, make_number (gca),
		 QCtype, Qgnutls_type_cipher,
		 QCcipher_aead_capable, cipher_tag_size == 0 ? Qnil : Qt,
		 QCcipher_tagsize, make_number (cipher_tag_size),

		 QCcipher_blocksize,
		 make_number (gnutls_cipher_get_block_size (gca)),

		 QCcipher_keysize,
		 make_number (gnutls_cipher_get_key_size (gca)),

		 QCcipher_ivsize,
		 make_number (gnutls_cipher_get_iv_size (gca)));

      ciphers = Fcons (cp, ciphers);
    }

  return ciphers;
}

static Lisp_Object
gnutls_symmetric_aead (bool encrypting, gnutls_cipher_algorithm_t gca,
                       Lisp_Object cipher,
		       const char *kdata, ptrdiff_t ksize,
		       const char *vdata, ptrdiff_t vsize,
		       const char *idata, ptrdiff_t isize,
                       Lisp_Object aead_auth)
{
# ifdef HAVE_GNUTLS_AEAD

  const char *desc = encrypting ? "encrypt" : "decrypt";
  Lisp_Object actual_iv = make_unibyte_string (vdata, vsize);

  gnutls_aead_cipher_hd_t acipher;
  gnutls_datum_t key_datum = { (unsigned char *) kdata, ksize };
  int ret = gnutls_aead_cipher_init (&acipher, gca, &key_datum);

  if (ret < GNUTLS_E_SUCCESS)
    error ("GnuTLS AEAD cipher %s/%s initialization failed: %s",
	   gnutls_cipher_get_name (gca), desc, emacs_gnutls_strerror (ret));

  ptrdiff_t cipher_tag_size = gnutls_cipher_get_tag_size (gca);
  ptrdiff_t tagged_size;
  if (INT_ADD_WRAPV (isize, cipher_tag_size, &tagged_size)
      || SIZE_MAX < tagged_size)
    memory_full (SIZE_MAX);
  size_t storage_length = tagged_size;
  USE_SAFE_ALLOCA;
  char *storage = SAFE_ALLOCA (storage_length);

  const char *aead_auth_data = NULL;
  ptrdiff_t aead_auth_size = 0;

  if (!NILP (aead_auth))
    {
      if (BUFFERP (aead_auth) || STRINGP (aead_auth))
        aead_auth = list1 (aead_auth);

      CHECK_CONS (aead_auth);

      ptrdiff_t astart_byte, aend_byte;
      const char *adata
	= extract_data_from_object (aead_auth, &astart_byte, &aend_byte);
      if (adata == NULL)
        error ("GnuTLS AEAD cipher auth extraction failed");

      aead_auth_data = adata;
      aead_auth_size = aend_byte - astart_byte;
    }

  ptrdiff_t expected_remainder = encrypting ? 0 : cipher_tag_size;
  ptrdiff_t cipher_block_size = gnutls_cipher_get_block_size (gca);

  if (isize < expected_remainder
      || (isize - expected_remainder) % cipher_block_size != 0)
    error (("GnuTLS AEAD cipher %s/%s input block length %"pD"d "
	    "is not %"pD"d greater than a multiple of the required %"pD"d"),
           gnutls_cipher_get_name (gca), desc,
	   isize, expected_remainder, cipher_block_size);

  ret = ((encrypting ? gnutls_aead_cipher_encrypt : gnutls_aead_cipher_decrypt)
	 (acipher, vdata, vsize, aead_auth_data, aead_auth_size,
	  cipher_tag_size, idata, isize, storage, &storage_length));

  Lisp_Object output;
  if (GNUTLS_E_SUCCESS <= ret)
    output = make_unibyte_string (storage, storage_length);
  explicit_bzero (storage, storage_length);
  gnutls_aead_cipher_deinit (acipher);

  if (ret < GNUTLS_E_SUCCESS)
    error ((encrypting
	    ? "GnuTLS AEAD cipher %s encryption failed: %s"
	    : "GnuTLS AEAD cipher %s decryption failed: %s"),
	   gnutls_cipher_get_name (gca), emacs_gnutls_strerror (ret));

  SAFE_FREE ();
  return list2 (output, actual_iv);
# else
  printmax_t print_gca = gca;
  error ("GnuTLS AEAD cipher %"pMd" is invalid or not found", print_gca);
# endif
}

static Lisp_Object
gnutls_symmetric (bool encrypting, Lisp_Object cipher,
                  Lisp_Object key, Lisp_Object iv,
                  Lisp_Object input, Lisp_Object aead_auth)
{
  if (BUFFERP (key) || STRINGP (key))
    key = list1 (key);

  CHECK_CONS (key);

  if (BUFFERP (input) || STRINGP (input))
    input = list1 (input);

  CHECK_CONS (input);

  if (BUFFERP (iv) || STRINGP (iv))
    iv = list1 (iv);

  CHECK_CONS (iv);


  const char *desc = encrypting ? "encrypt" : "decrypt";

  gnutls_cipher_algorithm_t gca = GNUTLS_CIPHER_UNKNOWN;

  Lisp_Object info = Qnil;
  if (STRINGP (cipher))
    cipher = intern (SSDATA (cipher));

  if (SYMBOLP (cipher))
    info = XCDR (Fassq (cipher, Fgnutls_ciphers ()));
  else if (TYPE_RANGED_INTEGERP (gnutls_cipher_algorithm_t, cipher))
    gca = XINT (cipher);
  else
    info = cipher;

  if (!NILP (info) && CONSP (info))
    {
      Lisp_Object v = Fplist_get (info, QCcipher_id);
      if (TYPE_RANGED_INTEGERP (gnutls_cipher_algorithm_t, v))
        gca = XINT (v);
    }

  ptrdiff_t key_size = gnutls_cipher_get_key_size (gca);
  if (key_size == 0)
    error ("GnuTLS cipher is invalid or not found");

  ptrdiff_t kstart_byte, kend_byte;
  const char *kdata = extract_data_from_object (key, &kstart_byte, &kend_byte);

  if (kdata == NULL)
    error ("GnuTLS cipher key extraction failed");

  if (kend_byte - kstart_byte != key_size)
    error (("GnuTLS cipher %s/%s key length %"pD"d is not equal to "
	    "the required %"pD"d"),
           gnutls_cipher_get_name (gca), desc,
	   kend_byte - kstart_byte, key_size);

  ptrdiff_t vstart_byte, vend_byte;
  char *vdata = extract_data_from_object (iv, &vstart_byte, &vend_byte);

  if (vdata == NULL)
    error ("GnuTLS cipher IV extraction failed");

  ptrdiff_t iv_size = gnutls_cipher_get_iv_size (gca);
  if (vend_byte - vstart_byte != iv_size)
    error (("GnuTLS cipher %s/%s IV length %"pD"d is not equal to "
	    "the required %"pD"d"),
           gnutls_cipher_get_name (gca), desc,
	   vend_byte - vstart_byte, iv_size);

  Lisp_Object actual_iv = make_unibyte_string (vdata, vend_byte - vstart_byte);

  ptrdiff_t istart_byte, iend_byte;
  const char *idata
    = extract_data_from_object (input, &istart_byte, &iend_byte);

  if (idata == NULL)
    error ("GnuTLS cipher input extraction failed");

  /* Is this an AEAD cipher? */
  if (gnutls_cipher_get_tag_size (gca) > 0)
    {
      Lisp_Object aead_output =
        gnutls_symmetric_aead (encrypting, gca, cipher,
                               kdata, kend_byte - kstart_byte,
                               vdata, vend_byte - vstart_byte,
                               idata, iend_byte - istart_byte,
                               aead_auth);
      if (STRINGP (XCAR (key)))
        Fclear_string (XCAR (key));
      return aead_output;
    }

  ptrdiff_t cipher_block_size = gnutls_cipher_get_block_size (gca);
  if ((iend_byte - istart_byte) % cipher_block_size != 0)
    error (("GnuTLS cipher %s/%s input block length %"pD"d is not a multiple "
	    "of the required %"pD"d"),
           gnutls_cipher_get_name (gca), desc,
	   iend_byte - istart_byte, cipher_block_size);

  gnutls_cipher_hd_t hcipher;
  gnutls_datum_t key_datum
    = { (unsigned char *) kdata, kend_byte - kstart_byte };

  int ret = gnutls_cipher_init (&hcipher, gca, &key_datum, NULL);

  if (ret < GNUTLS_E_SUCCESS)
    error ("GnuTLS cipher %s/%s initialization failed: %s",
	   gnutls_cipher_get_name (gca), desc, emacs_gnutls_strerror (ret));

  /* Note that this will not support streaming block mode. */
  gnutls_cipher_set_iv (hcipher, vdata, vend_byte - vstart_byte);

  /* GnuTLS docs: "For the supported ciphers the encrypted data length
     will equal the plaintext size."  */
  ptrdiff_t storage_length = iend_byte - istart_byte;
  Lisp_Object storage = make_uninit_string (storage_length);

  ret = ((encrypting ? gnutls_cipher_encrypt2 : gnutls_cipher_decrypt2)
	 (hcipher, idata, iend_byte - istart_byte,
	  SSDATA (storage), storage_length));

  if (STRINGP (XCAR (key)))
    Fclear_string (XCAR (key));

  if (ret < GNUTLS_E_SUCCESS)
    {
      gnutls_cipher_deinit (hcipher);
      if (encrypting)
	error ("GnuTLS cipher %s encryption failed: %s",
	       gnutls_cipher_get_name (gca), emacs_gnutls_strerror (ret));
      else
	error ("GnuTLS cipher %s decryption failed: %s",
	       gnutls_cipher_get_name (gca), emacs_gnutls_strerror (ret));
    }

  gnutls_cipher_deinit (hcipher);

  return list2 (storage, actual_iv);
}

DEFUN ("gnutls-symmetric-encrypt", Fgnutls_symmetric_encrypt,
       Sgnutls_symmetric_encrypt, 4, 5, 0,
       doc: /* Encrypt INPUT with symmetric CIPHER, KEY+AEAD_AUTH, and IV to a unibyte string.

Return nil on error.

The KEY can be specified as a buffer or string or in other ways (see
Info node `(elisp)Format of GnuTLS Cryptography Inputs').  The KEY
will be wiped after use if it's a string.

The IV and INPUT and the optional AEAD_AUTH can be specified as a
buffer or string or in other ways (see Info node `(elisp)Format of
GnuTLS Cryptography Inputs').

The alist of symmetric ciphers can be obtained with `gnutls-ciphers`.
The CIPHER may be a string or symbol matching a key in that alist, or
a plist with the :cipher-id numeric property, or the number itself.

AEAD ciphers: these ciphers will have a `gnutls-ciphers' entry with
:cipher-aead-capable set to t.  AEAD_AUTH can be supplied for
these AEAD ciphers, but it may still be omitted (nil) as well. */)
  (Lisp_Object cipher, Lisp_Object key, Lisp_Object iv,
   Lisp_Object input, Lisp_Object aead_auth)
{
  return gnutls_symmetric (true, cipher, key, iv, input, aead_auth);
}

DEFUN ("gnutls-symmetric-decrypt", Fgnutls_symmetric_decrypt,
       Sgnutls_symmetric_decrypt, 4, 5, 0,
       doc: /* Decrypt INPUT with symmetric CIPHER, KEY+AEAD_AUTH, and IV to a unibyte string.

Return nil on error.

The KEY can be specified as a buffer or string or in other ways (see
Info node `(elisp)Format of GnuTLS Cryptography Inputs').  The KEY
will be wiped after use if it's a string.

The IV and INPUT and the optional AEAD_AUTH can be specified as a
buffer or string or in other ways (see Info node `(elisp)Format of
GnuTLS Cryptography Inputs').

The alist of symmetric ciphers can be obtained with `gnutls-ciphers`.
The CIPHER may be a string or symbol matching a key in that alist, or
a plist with the `:cipher-id' numeric property, or the number itself.

AEAD ciphers: these ciphers will have a `gnutls-ciphers' entry with
:cipher-aead-capable set to t.  AEAD_AUTH can be supplied for
these AEAD ciphers, but it may still be omitted (nil) as well. */)
  (Lisp_Object cipher, Lisp_Object key, Lisp_Object iv,
   Lisp_Object input, Lisp_Object aead_auth)
{
  return gnutls_symmetric (false, cipher, key, iv, input, aead_auth);
}

DEFUN ("gnutls-macs", Fgnutls_macs, Sgnutls_macs, 0, 0, 0,
       doc: /* Return alist of GnuTLS mac-algorithm method descriptions as plists.

Use the value of the alist (extract it with `alist-get' for instance)
with `gnutls-hash-mac'.  The alist key is the mac-algorithm method
name. */)
  (void)
{
  Lisp_Object mac_algorithms = Qnil;
  const gnutls_mac_algorithm_t *macs = gnutls_mac_list ();
  for (ptrdiff_t pos = 0; macs[pos] != 0; pos++)
    {
      const gnutls_mac_algorithm_t gma = macs[pos];

      /* A symbol representing the GnuTLS MAC algorithm.  */
      Lisp_Object gma_symbol = intern (gnutls_mac_get_name (gma));

      size_t nonce_size = 0;
#ifdef HAVE_GNUTLS_MAC_GET_NONCE_SIZE
      nonce_size = gnutls_mac_get_nonce_size (gma);
#endif
      Lisp_Object mp = listn (CONSTYPE_HEAP, 11, gma_symbol,
			      QCmac_algorithm_id, make_number (gma),
			      QCtype, Qgnutls_type_mac_algorithm,

                              QCmac_algorithm_length,
                              make_number (gnutls_hmac_get_len (gma)),

                              QCmac_algorithm_keysize,
                              make_number (gnutls_mac_get_key_size (gma)),

                              QCmac_algorithm_noncesize,
			      make_number (nonce_size));
      mac_algorithms = Fcons (mp, mac_algorithms);
    }

  return mac_algorithms;
}

DEFUN ("gnutls-digests", Fgnutls_digests, Sgnutls_digests, 0, 0, 0,
       doc: /* Return alist of GnuTLS digest-algorithm method descriptions as plists.

Use the value of the alist (extract it with `alist-get' for instance)
with `gnutls-hash-digest'.  The alist key is the digest-algorithm
method name. */)
  (void)
{
  Lisp_Object digest_algorithms = Qnil;
  const gnutls_digest_algorithm_t *digests = gnutls_digest_list ();
  for (ptrdiff_t pos = 0; digests[pos] != 0; pos++)
    {
      const gnutls_digest_algorithm_t gda = digests[pos];

      /* A symbol representing the GnuTLS digest algorithm.  */
      Lisp_Object gda_symbol = intern (gnutls_digest_get_name (gda));

      Lisp_Object mp = listn (CONSTYPE_HEAP, 7, gda_symbol,
			      QCdigest_algorithm_id, make_number (gda),
			      QCtype, Qgnutls_type_digest_algorithm,

                              QCdigest_algorithm_length,
                              make_number (gnutls_hash_get_len (gda)));

      digest_algorithms = Fcons (mp, digest_algorithms);
    }

  return digest_algorithms;
}

DEFUN ("gnutls-hash-mac", Fgnutls_hash_mac, Sgnutls_hash_mac, 3, 3, 0,
       doc: /* Hash INPUT with HASH-METHOD and KEY into a unibyte string.

Return nil on error.

The KEY can be specified as a buffer or string or in other ways (see
Info node `(elisp)Format of GnuTLS Cryptography Inputs').  The KEY
will be wiped after use if it's a string.

The INPUT can be specified as a buffer or string or in other
ways (see Info node `(elisp)Format of GnuTLS Cryptography Inputs').

The alist of MAC algorithms can be obtained with `gnutls-macs`.  The
HASH-METHOD may be a string or symbol matching a key in that alist, or
a plist with the `:mac-algorithm-id' numeric property, or the number
itself. */)
  (Lisp_Object hash_method, Lisp_Object key, Lisp_Object input)
{
  if (BUFFERP (input) || STRINGP (input))
    input = list1 (input);

  CHECK_CONS (input);

  if (BUFFERP (key) || STRINGP (key))
    key = list1 (key);

  CHECK_CONS (key);

  gnutls_mac_algorithm_t gma = GNUTLS_MAC_UNKNOWN;

  Lisp_Object info = Qnil;
  if (STRINGP (hash_method))
    hash_method = intern (SSDATA (hash_method));

  if (SYMBOLP (hash_method))
    info = XCDR (Fassq (hash_method, Fgnutls_macs ()));
  else if (TYPE_RANGED_INTEGERP (gnutls_mac_algorithm_t, hash_method))
    gma = XINT (hash_method);
  else
    info = hash_method;

  if (!NILP (info) && CONSP (info))
    {
      Lisp_Object v = Fplist_get (info, QCmac_algorithm_id);
      if (TYPE_RANGED_INTEGERP (gnutls_mac_algorithm_t, v))
        gma = XINT (v);
    }

  ptrdiff_t digest_length = gnutls_hmac_get_len (gma);
  if (digest_length == 0)
    error ("GnuTLS MAC-method is invalid or not found");

  ptrdiff_t kstart_byte, kend_byte;
  const char *kdata = extract_data_from_object (key, &kstart_byte, &kend_byte);
  if (kdata == NULL)
    error ("GnuTLS MAC key extraction failed");

  gnutls_hmac_hd_t hmac;
  int ret = gnutls_hmac_init (&hmac, gma,
			      kdata + kstart_byte, kend_byte - kstart_byte);
  if (ret < GNUTLS_E_SUCCESS)
    error ("GnuTLS MAC %s initialization failed: %s",
	   gnutls_mac_get_name (gma), emacs_gnutls_strerror (ret));

  ptrdiff_t istart_byte, iend_byte;
  const char *idata
    = extract_data_from_object (input, &istart_byte, &iend_byte);
  if (idata == NULL)
    error ("GnuTLS MAC input extraction failed");

  Lisp_Object digest = make_uninit_string (digest_length);

  ret = gnutls_hmac (hmac, idata + istart_byte, iend_byte - istart_byte);

  if (STRINGP (XCAR (key)))
    Fclear_string (XCAR (key));

  if (ret < GNUTLS_E_SUCCESS)
    {
      gnutls_hmac_deinit (hmac, NULL);
      error ("GnuTLS MAC %s application failed: %s",
	     gnutls_mac_get_name (gma), emacs_gnutls_strerror (ret));
    }

  gnutls_hmac_output (hmac, SSDATA (digest));
  gnutls_hmac_deinit (hmac, NULL);

  return digest;
}

DEFUN ("gnutls-hash-digest", Fgnutls_hash_digest, Sgnutls_hash_digest, 2, 2, 0,
       doc: /* Digest INPUT with DIGEST-METHOD into a unibyte string.

Return nil on error.

The INPUT can be specified as a buffer or string or in other
ways (see Info node `(elisp)Format of GnuTLS Cryptography Inputs').

The alist of digest algorithms can be obtained with `gnutls-digests`.
The DIGEST-METHOD may be a string or symbol matching a key in that
alist, or a plist with the `:digest-algorithm-id' numeric property, or
the number itself. */)
  (Lisp_Object digest_method, Lisp_Object input)
{
  if (BUFFERP (input) || STRINGP (input))
    input = list1 (input);

  CHECK_CONS (input);

  gnutls_digest_algorithm_t gda = GNUTLS_DIG_UNKNOWN;

  Lisp_Object info = Qnil;
  if (STRINGP (digest_method))
    digest_method = intern (SSDATA (digest_method));

  if (SYMBOLP (digest_method))
    info = XCDR (Fassq (digest_method, Fgnutls_digests ()));
  else if (TYPE_RANGED_INTEGERP (gnutls_digest_algorithm_t, digest_method))
    gda = XINT (digest_method);
  else
    info = digest_method;

  if (!NILP (info) && CONSP (info))
    {
      Lisp_Object v = Fplist_get (info, QCdigest_algorithm_id);
      if (TYPE_RANGED_INTEGERP (gnutls_digest_algorithm_t, v))
        gda = XINT (v);
    }

  ptrdiff_t digest_length = gnutls_hash_get_len (gda);
  if (digest_length == 0)
    error ("GnuTLS digest-method is invalid or not found");

  gnutls_hash_hd_t hash;
  int ret = gnutls_hash_init (&hash, gda);

  if (ret < GNUTLS_E_SUCCESS)
    error ("GnuTLS digest initialization failed: %s",
	   emacs_gnutls_strerror (ret));

  Lisp_Object digest = make_uninit_string (digest_length);

  ptrdiff_t istart_byte, iend_byte;
  const char *idata
    = extract_data_from_object (input, &istart_byte, &iend_byte);
  if (idata == NULL)
    error ("GnuTLS digest input extraction failed");

  ret = gnutls_hash (hash, idata + istart_byte, iend_byte - istart_byte);

  if (ret < GNUTLS_E_SUCCESS)
    {
      gnutls_hash_deinit (hash, NULL);
      error ("GnuTLS digest application failed: %s",
	     emacs_gnutls_strerror (ret));
    }

  gnutls_hash_output (hash, SSDATA (digest));
  gnutls_hash_deinit (hash, NULL);

  return digest;
}

#endif	/* HAVE_GNUTLS3 */

DEFUN ("gnutls-available-p", Fgnutls_available_p, Sgnutls_available_p, 0, 0, 0,
       doc: /* Return list of capabilities if GnuTLS is available in this instance of Emacs.

...if supported         : then...
GnuTLS 3 or higher      : the list will contain `gnutls3'.
GnuTLS MACs             : the list will contain `macs'.
GnuTLS digests          : the list will contain `digests'.
GnuTLS symmetric ciphers: the list will contain `ciphers'.
GnuTLS AEAD ciphers     : the list will contain `AEAD-ciphers'.
%DUMBFW                 : the list will contain `ClientHello\ Padding'.
Any GnuTLS extension with ID up to 100
                        : the list will contain its name.  */)
  (void)
{
  Lisp_Object capabilities = Qnil;

#ifdef HAVE_GNUTLS

# ifdef WINDOWSNT
  Lisp_Object found = Fassq (Qgnutls, Vlibrary_cache);
  if (CONSP (found))
    return XCDR (found);

  /* Load the GnuTLS DLL and find exported functions.  The external
     library cache is updated after the capabilities have been
     determined.  */
  if (!init_gnutls_functions ())
    return Qnil;
# endif /* WINDOWSNT */

  capabilities = Fcons (intern("gnutls"), capabilities);

# ifdef HAVE_GNUTLS3
  capabilities = Fcons (intern("gnutls3"), capabilities);
  capabilities = Fcons (intern("digests"), capabilities);
  capabilities = Fcons (intern("ciphers"), capabilities);

#  ifdef HAVE_GNUTLS_AEAD
  capabilities = Fcons (intern("AEAD-ciphers"), capabilities);
#  endif

  capabilities = Fcons (intern("macs"), capabilities);

#  ifdef HAVE_GNUTLS_EXT_GET_NAME
  for (unsigned int ext=0; ext < 100; ext++)
    {
      const char* name = gnutls_ext_get_name(ext);
      if (name != NULL)
        {
          capabilities = Fcons (intern(name), capabilities);
        }
    }
#  endif
# endif	  /* HAVE_GNUTLS3 */

#  ifdef HAVE_GNUTLS_EXT__DUMBFW
  capabilities = Fcons (intern("ClientHello Padding"), capabilities);
#  endif

# ifdef WINDOWSNT
  Vlibrary_cache = Fcons (Fcons (Qgnutls, capabilities), Vlibrary_cache);
# endif /* WINDOWSNT */
#endif	/* HAVE_GNUTLS */

  return capabilities;
}

void
syms_of_gnutls (void)
{
  DEFSYM (Qlibgnutls_version, "libgnutls-version");
  Fset (Qlibgnutls_version,
#ifdef HAVE_GNUTLS
	make_number (GNUTLS_VERSION_MAJOR * 10000
		     + GNUTLS_VERSION_MINOR * 100
		     + GNUTLS_VERSION_PATCH)
#else
	make_number (-1)
#endif
        );
#ifdef HAVE_GNUTLS
  gnutls_global_initialized = 0;

  DEFSYM (Qgnutls_code, "gnutls-code");
  DEFSYM (Qgnutls_anon, "gnutls-anon");
  DEFSYM (Qgnutls_x509pki, "gnutls-x509pki");

  /* The following are for the property list of 'gnutls-boot'.  */
  DEFSYM (QChostname, ":hostname");
  DEFSYM (QCpriority, ":priority");
  DEFSYM (QCtrustfiles, ":trustfiles");
  DEFSYM (QCkeylist, ":keylist");
  DEFSYM (QCcrlfiles, ":crlfiles");
  DEFSYM (QCmin_prime_bits, ":min-prime-bits");
  DEFSYM (QCloglevel, ":loglevel");
  DEFSYM (QCcomplete_negotiation, ":complete-negotiation");
  DEFSYM (QCverify_flags, ":verify-flags");
  DEFSYM (QCverify_error, ":verify-error");

  DEFSYM (QCcipher_id, ":cipher-id");
  DEFSYM (QCcipher_aead_capable, ":cipher-aead-capable");
  DEFSYM (QCcipher_blocksize, ":cipher-blocksize");
  DEFSYM (QCcipher_keysize, ":cipher-keysize");
  DEFSYM (QCcipher_tagsize, ":cipher-tagsize");
  DEFSYM (QCcipher_ivsize, ":cipher-ivsize");

  DEFSYM (QCmac_algorithm_id, ":mac-algorithm-id");
  DEFSYM (QCmac_algorithm_noncesize, ":mac-algorithm-noncesize");
  DEFSYM (QCmac_algorithm_keysize, ":mac-algorithm-keysize");
  DEFSYM (QCmac_algorithm_length, ":mac-algorithm-length");

  DEFSYM (QCdigest_algorithm_id, ":digest-algorithm-id");
  DEFSYM (QCdigest_algorithm_length, ":digest-algorithm-length");

  DEFSYM (QCtype, ":type");
  DEFSYM (Qgnutls_type_cipher, "gnutls-symmetric-cipher");
  DEFSYM (Qgnutls_type_mac_algorithm, "gnutls-mac-algorithm");
  DEFSYM (Qgnutls_type_digest_algorithm, "gnutls-digest-algorithm");

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
  defsubr (&Sgnutls_asynchronous_parameters);
  defsubr (&Sgnutls_errorp);
  defsubr (&Sgnutls_error_fatalp);
  defsubr (&Sgnutls_error_string);
  defsubr (&Sgnutls_boot);
  defsubr (&Sgnutls_deinit);
  defsubr (&Sgnutls_bye);
  defsubr (&Sgnutls_peer_status);
  defsubr (&Sgnutls_peer_status_warning_describe);

#ifdef HAVE_GNUTLS3
  defsubr (&Sgnutls_ciphers);
  defsubr (&Sgnutls_macs);
  defsubr (&Sgnutls_digests);
  defsubr (&Sgnutls_hash_mac);
  defsubr (&Sgnutls_hash_digest);
  defsubr (&Sgnutls_symmetric_encrypt);
  defsubr (&Sgnutls_symmetric_decrypt);
#endif

  DEFVAR_INT ("gnutls-log-level", global_gnutls_log_level,
	      doc: /* Logging level used by the GnuTLS functions.
Set this larger than 0 to get debug output in the *Messages* buffer.
1 is for important messages, 2 is for debug data, and higher numbers
are as per the GnuTLS logging conventions.  */);
  global_gnutls_log_level = 0;

#endif	/* HAVE_GNUTLS */

  defsubr (&Sgnutls_available_p);
}
