;;; nsm.el --- Network Security Manager  -*- lexical-binding:t -*-

;; Copyright (C) 2014-2020 Free Software Foundation, Inc.

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
;; Keywords: encryption, security, network

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'cl-lib)
(require 'rmc)                       ; read-multiple-choice
(require 'subr-x)
(require 'seq)
(require 'map)

(defvar nsm-permanent-host-settings nil)
(defvar nsm-temporary-host-settings nil)

(defgroup nsm nil
  "Network Security Manager"
  :version "25.1"
  :group 'comm)

(defcustom network-security-level 'medium
  "How secure the network should be.
If a potential problem with the security of the network
connection is found, the user is asked to give input into how the
connection should be handled.

The following values are possible:

`low': No checks are performed: This is extremely insecure.
`medium': Default.  Suitable for most circumstances.
`high': Warns about additional issues not enabled in `medium' due to
compatibility concerns.
`paranoid': On this level, the user is queried for most new connections.

See the Emacs manual for a description of all things that are
checked and warned against."
  :version "25.1"
  :type '(choice (const :tag "Low" low)
                 (const :tag "Medium" medium)
                 (const :tag "High" high)
		 (const :tag "Paranoid" paranoid)))

(defcustom nsm-trust-local-network nil
  "Disable warnings when visiting trusted hosts on local networks.

The default suite of TLS checks in NSM is designed to follow the
most current security best practices.  Under some situations,
such as attempting to connect to an email server that do not
follow these practices inside a school or corporate network, NSM
may produce warnings for such occasions.  Setting this option to
a non-nil value, or a zero-argument function that returns non-nil
tells NSM to skip checking for potential TLS vulnerabilities when
connecting to hosts on a local network.

Make sure you know what you are doing before enabling this
option."
  :version "27.1"
  :type '(choice (const :tag "On" t)
                 (const :tag "Off" nil)
                 (function :tag "Custom function")))

(defcustom nsm-settings-file (expand-file-name "network-security.data"
						 user-emacs-directory)
  "The file the security manager settings will be stored in."
  :version "25.1"
  :type 'file)

(defcustom nsm-save-host-names nil
  "If non-nil, always save host names in the structures in `nsm-settings-file'.
By default, only hosts that have exceptions have their names
stored in plain text."
  :version "25.1"
  :type 'boolean)

(defvar nsm-noninteractive nil
  "If non-nil, the connection is opened in a non-interactive context.
This means that no queries should be performed.")

(declare-function gnutls-peer-status "gnutls.c" (proc))

(defun nsm-verify-connection (process host port &optional
				      save-fingerprint warn-unencrypted)
  "Verify the security status of PROCESS that's connected to HOST:PORT.
If PROCESS is a gnutls connection, the certificate validity will
be examined.  If it's a non-TLS connection, it may be compared
against previous connections.  If the function determines that
there is something odd about the connection, the user will be
queried about what to do about it.

The process is returned if everything is OK, and otherwise, the
process will be deleted and nil is returned.

If SAVE-FINGERPRINT, always save the fingerprint of the
server (if the connection is a TLS connection).  This is useful
to keep track of the TLS status of STARTTLS servers.

If WARN-UNENCRYPTED, query the user if the connection is
unencrypted."
  (let* ((status (gnutls-peer-status process))
         (id (nsm-id host port))
         (settings (nsm-host-settings id)))
    (cond
     ((not (process-live-p process))
      nil)
     ((not status)
      ;; This is a non-TLS connection.
      (nsm-check-plain-connection process host port settings
                                  warn-unencrypted))
     (t
      (let ((process
             (nsm-check-tls-connection process host port status settings)))
        (when (and process save-fingerprint
                   (null (nsm-host-settings id)))
          (nsm-save-host host port status 'fingerprint nil 'always))
        process)))))

(defcustom network-security-protocol-checks
  '(;; Old Known Weaknesses.
    (version                medium)
    (compression            medium)
    (renegotiation-info-ext medium)
    (verify-cert            medium)
    (same-cert              medium)
    (null-suite             medium)
    (export-kx              medium)
    (anon-kx                medium)
    (md5-sig                medium)
    (rc4-cipher             medium)
    ;; Weaknesses made known after 2013.
    (dhe-prime-kx           medium)
    (sha1-sig               medium)
    (ecdsa-cbc-cipher       medium)
    ;; Towards TLS 1.3
    (dhe-kx                 high)
    (rsa-kx                 high)
    (3des-cipher            high)
    (cbc-cipher             high))
  "This variable specifies what TLS connection checks to perform.
It's an alist where the key is the name of the check, and the
value is the minimum security level the check should begin.

Each check function is called with the parameters HOST PORT
STATUS SETTINGS.  HOST is the host domain, PORT is a TCP port
number, STATUS is the peer status returned by
`gnutls-peer-status', and SETTINGS is the persistent and session
settings for the host HOST.  Please refer to the contents of
`nsm-setting-file' for details.  If a problem is found, the check
function is required to return an error message, and nil
otherwise.

See also: `nsm-check-tls-connection', `nsm-save-host-names',
`nsm-settings-file'"
  :version "27.1"
  :type '(repeat (list (symbol :tag "Check function")
                       (choice :tag "Level"
                               :value medium
                               (const :tag "Low" low)
                               (const :tag "Medium" medium)
                               (const :tag "High" high)))))

(defun nsm-save-fingerprint-maybe (host port status &rest _)
  "Save the certificate's fingerprint.

In order to detect man-in-the-middle attacks, when
`network-security-level' is `high', this function will save the
fingerprint of the certificate for check functions to check."
  (when (>= (nsm-level network-security-level) (nsm-level 'high))
    ;; Save the host fingerprint so that we can check it the
    ;; next time we connect.
    (nsm-save-host host port status 'fingerprint nil 'always)))

(defvar nsm-tls-post-check-functions '(nsm-save-fingerprint-maybe)
  "Functions to run after checking a TLS session.

Each function will be run with the parameters HOST PORT STATUS
SETTINGS and RESULTS.  The parameters HOST PORT STATUS and
SETTINGS are the same as those supplied to each check function.
RESULTS is an alist where the keys are the checks run and the
values the results of the checks.")

(defun nsm-network-same-subnet (local-ip mask ip)
  "Return t if IP is in the same subnet as LOCAL-IP/MASK.
LOCAL-IP, MASK, and IP are specified as vectors of integers, and
are expected to have the same length.  Works for both IPv4 and
IPv6 addresses."
  (let ((matches t)
        (ip-length (length ip))
        (local-length (length local-ip)))
    (unless (and (memq ip-length '(4 5 8 9))
                 (memq local-length '(4 5 8 9)))
      (error "Unexpected length of IP address %S" local-ip))
    (if (/= ip-length local-length)
        nil
        (dotimes (i local-length)
          (setq matches (and matches
                             (=
                              (logand (aref local-ip i)
                                      (aref mask i))
                              (logand (aref ip i)
                                      (aref mask i))))))
        matches)))

(defun nsm-should-check (host)
  "Determine whether NSM should check for TLS problems for HOST.

If `nsm-trust-local-network' is or returns non-nil, and if the
host address is a localhost address, or in the same subnet as one
of the local interfaces, this function returns nil.  Non-nil
otherwise."
  (let ((addresses (network-lookup-address-info host))
        (network-interface-list (network-interface-list t))
        (off-net t))
    (when
     (or (and (functionp nsm-trust-local-network)
              (funcall nsm-trust-local-network))
         nsm-trust-local-network)
     (mapc
      (lambda (ip)
        (mapc
         (lambda (info)
           (let ((local-ip (nth 1 info))
                 (mask (nth 2 info)))
             (when
                 (nsm-network-same-subnet (substring local-ip 0 -1)
                                          (substring mask 0 -1)
                                          (substring ip 0 -1))
               (setq off-net nil))))
         network-interface-list))
      addresses))
     off-net))

(defun nsm-check-tls-connection (process host port status settings)
  "Check TLS connection against potential security problems.

This function runs each test defined in
`network-security-protocol-checks' in the order specified against
the TLS connection's peer status STATUS for the host HOST and
port PORT.

If one or more problems are found, this function will collect all
the error messages returned by the check functions, and confirm
with the user in interactive mode whether to continue with the
TLS session.

If the user declines to continue, or problem(s) are found under
non-interactive mode, the process PROCESS will be deleted, thus
terminating the connection.

This function returns the process PROCESS if no problems are
found, and nil otherwise.

See also: `network-security-protocol-checks' and `nsm-noninteractive'"
  (when (nsm-should-check host)
    (let* ((results
            (cl-loop
             for check in network-security-protocol-checks
             for type = (intern (format ":%s" (car check)))
             ;; Skip the check if the user has already said that this
             ;; host is OK for this type of "error".
             for result = (and (not (memq type
                                          (plist-get settings :conditions)))
                               (>= (nsm-level network-security-level)
                                   (nsm-level (cadr check)))
                               (funcall
                                (intern (format "nsm-protocol-check--%s"
                                                (car check)))
                                host port status settings))
             when result
             collect (cons type result)))
           (problems (nconc (plist-get status :warnings) (map-keys results))))

      ;; We haven't seen this before, and we're paranoid.
      (when (and (eq network-security-level 'paranoid)
	         (not (nsm-fingerprint-ok-p status settings)))
        (push '(:not-seen . "Certificate not seen before") results))

      (when (and results
                 (not (seq-set-equal-p (plist-get settings :conditions)
                                       problems))
                 (not (nsm-query host port status
                                 'conditions
                                 problems
                                 (format-message
		                  "The TLS connection to %s:%s is insecure\nfor the following reason%s:\n\n%s"
		                  host port
		                  (if (> (length problems) 1)
			              "s" "")
		                  (concat "* " (string-join
                                                (split-string
                                                 (string-join
                                                  (map-values results)
                                                  "\n")
                                                 "\n")
                                                "\n* ")))))
                 (delete-process process)
                 (setq process nil)))
      (run-hook-with-args 'nsm-tls-post-check-functions
                          host port status settings results)))
  process)



;; Certificate checks

(declare-function gnutls-peer-status-warning-describe "gnutls.c"
                  (status-symbol))

(defun nsm-protocol-check--verify-cert (_host _port status settings)
  "Check for warnings from the certificate verification status.

This is the most basic security check for a TLS connection.  If
 certificate verification fails, it means the server's identity
 cannot be verified by the credentials received."
  (let ((warnings (plist-get status :warnings)))
    (and warnings
         (not (nsm-warnings-ok-p status settings))
         (mapconcat #'gnutls-peer-status-warning-describe warnings "\n"))))

(defun nsm-protocol-check--same-cert (_host _port status settings)
  "Check for certificate fingerprint mismatch.

If the fingerprints saved do not match the fingerprint of the
certificate presented, the TLS session may be under a
man-in-the-middle attack."
  (and (not (nsm-fingerprint-ok-p status settings))
       (format-message
        "fingerprint has changed")))

;; Key exchange checks

(defun nsm-protocol-check--rsa-kx (_host _port status &optional _settings)
  "Check for static RSA key exchange.

Static RSA key exchange methods do not offer perfect forward
secrecy, therefore, the security of a TLS session is only as
secure as the server's private key.  Due to TLS' use of RSA key
exchange to create a session key (the key negotiated between the
client and the server to encrypt traffic), if the server's
private key had been compromised, the attacker will be able to
decrypt any past TLS session recorded, as opposed to just one TLS
session if the key exchange was conducted via a key exchange
method that offers perfect forward secrecy, such as ephemeral
Diffie-Hellman key exchange.

By default, this check is only enabled when
`network-security-level' is set to `high' for compatibility
reasons.

Reference:

Sheffer, Holz, Saint-Andre (May 2015).  \"Recommendations for Secure
Use of Transport Layer Security (TLS) and Datagram Transport Layer
Security (DTLS)\", \"(4.1.  General Guidelines)\"
`https://tools.ietf.org/html/rfc7525\#section-4.1'"
  (let ((kx (plist-get status :key-exchange)))
    (and (string-match "^\\bRSA\\b" kx)
         (format-message
          "RSA key exchange method (%s) does not offer perfect forward secrecy"
          kx))))

(defun nsm-protocol-check--dhe-prime-kx (_host _port status &optional _settings)
  "Check for the key strength of DH key exchange based on integer factorization.

This check is a response to Logjam[1].  Logjam is an attack that
allows an attacker with sufficient resource, and positioned
between the user and the server, to downgrade vulnerable TLS
connections to insecure 512-bit export grade cryptography.

The Logjam paper suggests using 1024-bit prime on the client to
mitigate some effects of this attack, and upgrade to 2048-bit as
soon as server configurations allow.  According to SSLLabs' SSL
Pulse tracker, only about 75% of server support 2048-bit key
exchange in June 2018[2].  To provide a balance between
compatibility and security, this function only checks for a
minimum key strength of 1024-bit.

See also: `nsm-protocol-check--dhe-kx'

Reference:

[1]: Adrian et al (2014).  \"Imperfect Forward Secrecy: How
Diffie-Hellman Fails in Practice\", `https://weakdh.org/'
[2]: SSL Pulse (June 03, 2018).  \"Key Exchange Strength\",
`https://www.ssllabs.com/ssl-pulse/'"
  (let ((prime-bits (plist-get status :diffie-hellman-prime-bits)))
    (if (and (string-match "^\\bDHE\\b" (plist-get status :key-exchange))
             (< prime-bits 1024))
        (format-message
         "Diffie-Hellman key strength (%s bits) too weak (%s bits)"
         prime-bits 1024))))

(defun nsm-protocol-check--dhe-kx (_host _port status &optional _settings)
  "Check for existence of DH key exchange based on integer factorization.

In the years since the discovery of Logjam, it was discovered
that there were rampant use of small subgroup prime or composite
number for DHE by many servers, and thus allowed themselves to be
vulnerable to backdoors[1].  Given the difficulty in validating
Diffie-Hellman parameters, major browser vendors had started to
remove DHE since 2016[2].  Emacs stops short of banning DHE and
terminating connection, but prompts the user instead.

References:

[1]: Dorey, Fong, and Essex (2016).  \"Indiscreet Logs: Persistent
Diffie-Hellman Backdoors in TLS.\",
`https://eprint.iacr.org/2016/999.pdf'
[2]: Chrome Platform Status (2017).  \"Remove DHE-based ciphers\",
`https://www.chromestatus.com/feature/5128908798164992'"
  (let ((kx (plist-get status :key-exchange)))
    (when (string-match "^\\bDHE\\b" kx)
      (format-message
       "unable to verify Diffie-Hellman key exchange method (%s) parameters"
       kx))))

(defun nsm-protocol-check--export-kx (_host _port status &optional _settings)
  "Check for RSA-EXPORT key exchange.

EXPORT cipher suites are a family of 40-bit and 56-bit effective
security algorithms legally exportable by the United States in
the early 90s[1].  They can be broken in seconds on 2018 hardware.

Prior to 3.2.0, GnuTLS had only supported RSA-EXPORT key
exchange.  Since 3.2.0, RSA-EXPORT had been removed, therefore,
this check has no effect on GnuTLS >= 3.2.0.

Reference:

[1]: Schneier, Bruce (1996). Applied Cryptography (Second ed.). John
Wiley & Sons. ISBN 0-471-11709-9.
[2]: N. Mavrogiannopoulos, FSF (Apr 2015).  \"GnuTLS NEWS -- History
of user-visible changes.\" Version 3.4.0,
`https://gitlab.com/gnutls/gnutls/blob/master/NEWS'"
  (when (< libgnutls-version 30200)
    (let ((kx (plist-get status :key-exchange)))
      (and (string-match "\\bEXPORT\\b" kx)
           (format-message
            "EXPORT level key exchange (%s) is insecure"
            kx)))))

(defun nsm-protocol-check--anon-kx (_host _port status &optional _settings)
  "Check for anonymous key exchange.

Anonymous key exchange exposes the connection to
man-in-the-middle attacks.

Reference:

GnuTLS authors (2018). \"GnuTLS Manual 4.3.3 Anonymous
authentication\",
`https://www.gnutls.org/manual/gnutls.html\#Anonymous-authentication'"
  (let ((kx (plist-get status :key-exchange)))
    (and (string-match "\\bANON\\b" kx)
         (format-message
          "anonymous key exchange method (%s) can be unsafe"
          kx))))

;; Cipher checks

(defun nsm-protocol-check--cbc-cipher (_host _port status &optional _settings)
  "Check for CBC mode ciphers.

CBC mode cipher in TLS versions earlier than 1.3 are problematic
because of MAC-then-encrypt.  This construction is vulnerable to
padding oracle attacks[1].

Since GnuTLS 3.4.0, the TLS encrypt-then-MAC extension[2] has
been enabled by default[3]. If encrypt-then-MAC is negotiated,
this check has no effect.

Reference:

[1]: Sullivan (Feb 2016).  \"Padding oracles and the decline of
CBC-mode cipher suites\",
`https://blog.cloudflare.com/padding-oracles-and-the-decline-of-cbc-mode-ciphersuites/'
[2]: P. Gutmann (Sept 2014).  \"Encrypt-then-MAC for Transport Layer
Security (TLS) and Datagram Transport Layer Security (DTLS)\",
`https://tools.ietf.org/html/rfc7366'
[3]: N. Mavrogiannopoulos (Nov 2015).  \"An overview of GnuTLS
3.4.x\",
`https://nikmav.blogspot.com/2015/11/an-overview-of-gnutls-34x.html'"
  (when (not (plist-get status :encrypt-then-mac))
    (let ((cipher (plist-get status :cipher)))
      (and (string-match "\\bCBC\\b" cipher)
           (format-message
            "CBC mode cipher (%s) can be insecure"
            cipher)))))

(defun nsm-protocol-check--ecdsa-cbc-cipher (_host _port status &optional _settings)
  "Check for CBC mode cipher usage under ECDSA key exchange.

CBC mode cipher in TLS versions earlier than 1.3 are problematic
because of MAC-then-encrypt.  This construction is vulnerable to
padding oracle attacks[1].

Due to current widespread use of CBC mode ciphers by servers,
this function only checks for CBC mode cipher usage in
combination with ECDSA key exchange, which is virtually
non-existent[2].

Since GnuTLS 3.4.0, the TLS encrypt-then-MAC extension[3] has
been enabled by default[4]. If encrypt-then-MAC is negotiated,
this check has no effect.

References:

[1]: Sullivan (Feb 2016).  \"Padding oracles and the decline of
CBC-mode cipher suites\",
`https://blog.cloudflare.com/padding-oracles-and-the-decline-of-cbc-mode-ciphersuites/'
[2]: Chrome Platform Status (2017). \"Remove CBC-mode ECDSA ciphers in
TLS\", `https://www.chromestatus.com/feature/5740978103123968'
[3]: P. Gutmann (Sept 2014).  \"Encrypt-then-MAC for Transport Layer
Security (TLS) and Datagram Transport Layer Security (DTLS)\",
`https://tools.ietf.org/html/rfc7366'
[4]: N. Mavrogiannopoulos (Nov 2015).  \"An overview of GnuTLS
3.4.x\",
`https://nikmav.blogspot.com/2015/11/an-overview-of-gnutls-34x.html'"
  (when (not (plist-get status :encrypt-then-mac))
    (let ((kx (plist-get status :key-exchange))
          (cipher (plist-get status :cipher)))
      (and (string-match "\\bECDSA\\b" kx)
           (string-match "\\bCBC\\b" cipher)
           (format-message
            "CBC mode cipher (%s) can be insecure"
            cipher)))))

(defun nsm-protocol-check--3des-cipher (_host _port status &optional _settings)
  "Check for 3DES ciphers.

Due to its use of 64-bit block size, it is known that a
ciphertext collision is highly likely when 2^32 blocks are
encrypted with the same key bundle under 3-key 3DES.  Practical
birthday attacks of this kind have been demonstrated by Sweet32[1].
As such, NIST is in the process of disallowing its use in TLS[2].

[1]: Bhargavan, Leurent (2016).  \"On the Practical (In-)Security of
64-bit Block Ciphers — Collision Attacks on HTTP over TLS and
OpenVPN\", `https://sweet32.info/'
[2]: NIST Information Technology Laboratory (Jul 2017).  \"Update to
Current Use and Deprecation of TDEA\",
`https://csrc.nist.gov/News/2017/Update-to-Current-Use-and-Deprecation-of-TDEA'"
  (let ((cipher (plist-get status :cipher)))
    (and (string-match "\\b3DES\\b" cipher)
         (format-message
          "3DES cipher (%s) is weak"
          cipher))))

(defun nsm-protocol-check--rc4-cipher (_host _port status &optional _settings)
  "Check for RC4 ciphers.

RC4 cipher has been prohibited by RFC 7465[1].

Since GnuTLS 3.4.0, RC4 is not enabled by default[2], but can be
enabled if requested.  This check is mainly provided to secure
Emacs built with older version of GnuTLS.

Reference:

[1]: Popov A (Feb 2015).  \"Prohibiting RC4 Cipher Suites\",
`https://tools.ietf.org/html/rfc7465'
[2]: N. Mavrogiannopoulos (Nov 2015).  \"An overview of GnuTLS
3.4.x\",
`https://nikmav.blogspot.com/2015/11/an-overview-of-gnutls-34x.html'"
  (let ((cipher (plist-get status :cipher)))
    (and (string-match "\\bARCFOUR\\b" cipher)
         (format-message
          "RC4 cipher (%s) is insecure"
          cipher))))

;; Signature checks

(defun nsm-protocol-check--sha1-sig (_host _port status &optional _settings)
  "Check for SHA1 signatures on certificates.

The first SHA1 collision was found in 2017[1], as a precaution
against the events following the discovery of cheap collisions in
MD5, major browsers[2][3][4][5] have removed the use of SHA1
signatures in certificates.

References:

[1]: Stevens M, Karpman P et al (2017).  \"The first collision for
full SHA-1\", `https://shattered.io/static/shattered.pdf'
[2]: Chromium Security Education TLS/SSL.  \"Deprecated and Removed
Features (SHA-1 Certificate Signatures)\",
`https://www.chromium.org/Home/chromium-security/education/tls\#TOC-SHA-1-Certificate-Signatures'
[3]: Jones J.C (2017).  \"The end of SHA-1 on the Public Web\",
`https://blog.mozilla.org/security/2017/02/23/the-end-of-sha-1-on-the-public-web/'
[4]: Apple Support (2017).  \"Move to SHA-256 signed certificates to
avoid connection failures\",
`https://support.apple.com/en-gb/HT207459'
[5]: Microsoft Security Advisory 4010323 (2017).  \"Deprecation of
SHA-1 for SSL/TLS Certificates in Microsoft Edge and Internet Explorer
11\",
`https://docs.microsoft.com/en-us/security-updates/securityadvisories/2017/4010323'"
  (cl-loop for certificate in (plist-get status :certificates)
           for algo = (plist-get certificate :signature-algorithm)
           ;; Don't check root certificates -- root is always trusted.
           if (and (not (equal (plist-get certificate :issuer)
                               (plist-get certificate :subject)))
                   (string-match "\\bSHA1\\b" algo))
           return (format-message
                   "SHA1 signature (%s) is prone to collisions"
                   algo)
           end))

(defun nsm-protocol-check--md5-sig (_host _port status &optional _settings)
  "Check for MD5 signatures on certificates.

In 2008, a group of researchers were able to forge an
intermediate CA certificate that appeared to be legitimate when
checked by MD5[1].  RFC 6151[2] has recommended against the usage
of MD5 for digital signatures, which includes TLS certificate
signatures.

Since GnuTLS 3.3.0, MD5 has been disabled by default, but can be
enabled if requested.

References:

[1]: Sotirov A, Stevens M et al (2008).  \"MD5 considered harmful today
- Creating a rogue CA certificate\",
`http://www.win.tue.nl/hashclash/rogue-ca/'
[2]: Turner S, Chen L (2011).  \"Updated Security Considerations for
the MD5 Message-Digest and the HMAC-MD5 Algorithms\",
`https://tools.ietf.org/html/rfc6151'"
  (cl-loop for certificate in (plist-get status :certificates)
           for algo = (plist-get certificate :signature-algorithm)
           ;; Don't check root certificates -- root is always trusted.
           if (and (not (equal (plist-get certificate :issuer)
                               (plist-get certificate :subject)))
                   (string-match "\\bMD5\\b" algo))
           return (format-message
                   "MD5 signature (%s) is very prone to collisions"
                   algo)
           end))

;; Extension checks

(defun nsm-protocol-check--renegotiation-info-ext (_host _port status
                                                  &optional _settings)
  "Check for renegotiation_info TLS extension status.

If this TLS extension is not used, the connection established is
vulnerable to an attack in which an impersonator can extract
sensitive information such as HTTP session ID cookies or login
passwords.  Renegotiation was removed in TLS1.3, so this is only
checked for earlier protocol versions.

Reference:

E. Rescorla, M. Ray, S. Dispensa, N. Oskov (Feb 2010).  \"Transport
Layer Security (TLS) Renegotiation Indication Extension\",
`https://tools.ietf.org/html/rfc5746'"
  (when (plist-member status :safe-renegotiation)
    (let ((unsafe-renegotiation (not (plist-get status :safe-renegotiation))))
      (and unsafe-renegotiation
           (format-message
            "safe renegotiation is not supported, connection not protected from impersonators")))))

;; Compression checks

(defun nsm-protocol-check--compression (_host _port status &optional _settings)
  "Check for TLS compression.

TLS compression attacks such as CRIME would allow an attacker to
decrypt ciphertext.  As a result, RFC 7525 has recommended its
disablement.

Reference:

Sheffer, Holz, Saint-Andre (May 2015).  \"Recommendations for Secure
Use of Transport Layer Security (TLS) and Datagram Transport Layer
Security (DTLS)\", `https://tools.ietf.org/html/rfc7525'"
  (let ((compression (plist-get status :compression)))
    (and compression
	 (string-match "^\\bDEFLATE\\b" compression)
         (format-message
          "compression method (%s) may lead to leakage of sensitive information"
          compression))))

;; Protocol version checks

(defun nsm-protocol-check--version (_host _port status &optional _settings)
  "Check for SSL/TLS protocol version.

This function guards against the usage of SSL3.0, which has been
deprecated by RFC7568[1], and TLS 1.0, which has been deprecated
by PCI DSS[2].

References:

[1]: Barnes, Thomson, Pironti, Langley (2015).  \"Deprecating Secure
Sockets Layer Version 3.0\", `https://tools.ietf.org/html/rfc7568'
[2]: PCI Security Standards Council (2016).  \"Migrating from SSL and
Early TLS\"
`https://www.pcisecuritystandards.org/documents/Migrating-from-SSL-Early-TLS-Info-Supp-v1_1.pdf'"
  (let ((protocol (plist-get status :protocol)))
    (and protocol
         (or (string-match "SSL" protocol)
             (and (string-match "TLS1.\\([0-9]+\\)" protocol)
                  (< (string-to-number (match-string 1 protocol)) 1)))
         (format-message
          "%s protocol is deprecated by standard bodies"
          protocol))))

;; Full suite checks

(defun nsm-protocol-check--null-suite (_host _port status &optional _settings)
  "Check for NULL cipher suites.

This function checks for NULL key exchange, cipher and message
authentication code key derivation function.  As the name
suggests, a NULL assigned for any of the above disables an
integral part of the security properties that makes up the TLS
protocol."
  (let ((suite (nsm-cipher-suite status)))
    (and (string-match "\\bNULL\\b" suite)
         (format-message
          "NULL cipher suite (%s) violates authenticity, integrity, or confidentiality guarantees"
          suite))))



(defun nsm-fingerprint (status)
  (plist-get (plist-get status :certificate) :public-key-id))

(defun nsm-fingerprint-ok-p (status settings)
  (let ((saved-fingerprints (plist-get settings :fingerprints)))
    ;; Haven't seen this host before or not pinning cert.
    (or (null saved-fingerprints)
        ;; Plain connection allowed.
        (memq :none saved-fingerprints)
        ;; We are pinning certs, and we have seen this host before,
        ;; but the credentials for this host differs from the last
        ;; times we saw it.
        (member (nsm-fingerprint status) saved-fingerprints))))

(defun nsm-check-plain-connection (process host port settings warn-unencrypted)
  (if (nsm-should-check host)
      ;; If this connection used to be TLS, but is now plain, then it's
      ;; possible that we're being Man-In-The-Middled by a proxy that's
      ;; stripping out STARTTLS announcements.
      (let ((fingerprints (plist-get settings :fingerprints)))
        (cond
         ((and fingerprints
	       (not (memq :none fingerprints))
	       (not
	        (nsm-query
	         host port nil 'conditions '(:unencrypted)
                 (format-message
	          "The connection to %s:%s used to be an encrypted connection, but is now unencrypted.  This might mean that there's a man-in-the-middle tapping this connection."
	          host port))))
          (delete-process process)
          nil)
         ((and warn-unencrypted
	       (not (memq :unencrypted (plist-get settings :conditions)))
	       (not (nsm-query
	             host port nil 'conditions '(:unencrypted)
                     (format-message
	              "The connection to %s:%s is unencrypted."
	              host port))))
          (delete-process process)
          nil)
         (t
          process)))
    process))

(defun nsm-query (host port status what problems message)
  ;; If there is no user to answer queries, then say `no' to everything.
  (if (or noninteractive
	  nsm-noninteractive)
      nil
    (let ((response
	   (condition-case nil
               (intern
                (car (split-string (nsm-query-user message status))))
	     ;; Make sure we manage to close the process if the user hits
	     ;; `C-g'.
	     (quit 'no)
	     (error 'no))))
      (if (eq response 'no)
          (progn
            (message "Aborting connection to %s:%s" host port)
            nil)
        (message (if (eq response 'session)
                     "Accepting certificate for %s:%s this session only"
                   "Permanently accepting certificate for %s:%s")
                 host port)
        (nsm-save-host host port status what problems response)
        t))))

(declare-function gnutls-format-certificate "gnutls.c" (cert))

(defun nsm-query-user (message status)
  (let ((buffer (get-buffer-create "*Network Security Manager*"))
        (cert-buffer (get-buffer-create "*Certificate Details*"))
        (certs (plist-get status :certificates))
        (accept-choices
         '((?a "always" "Accept this certificate this session and for all future sessions.")
           (?s "session only" "Accept this certificate this session only.")
           (?n "no" "Refuse to use this certificate, and close the connection.")
           (?d "details" "See certificate details")))
        (details-choices
         '((?b "backward page" "See previous page")
           (?f "forward page" "See next page")
           (?n "next" "Next certificate")
           (?p "previous" "Previous certificate")
           (?q "quit" "Quit details view")))
        (done nil))
    (save-window-excursion
      ;; First format the certificate and warnings.
      (pop-to-buffer buffer)
      (erase-buffer)
      (let ((inhibit-read-only t))
        (when status
          (insert (nsm-format-certificate status)))
        (insert message)
        (goto-char (point-min))
        ;; Fill the first line of the message, which usually
        ;; contains lots of explanatory text.
        (fill-region (point) (line-end-position))
        ;; If the window is too small, add navigation options.
        (when (> (line-number-at-pos (point-max)) (window-height))
          (setq accept-choices
                (append accept-choices
                        '((?b "backward page" "See previous page")
                          (?f "forward page" "See next page"))))))
      ;; Then ask the user what to do about it.
      (unwind-protect
          (let* ((pems (cl-loop for cert in certs
                                collect (gnutls-format-certificate
                                         (plist-get cert :pem))))
                 (cert-index 0)
                 show-details answer buf)
            (while (not done)
              (setq answer (if show-details
                               (read-multiple-choice "Viewing certificate:"
                                                     details-choices)
                             (read-multiple-choice "Continue connecting?"
                                                   accept-choices)))
              (setq buf (if show-details cert-buffer buffer))

              (cl-case (car answer)
                (?q
                 ;; Exit the details window.
                 (set-window-buffer (get-buffer-window cert-buffer) buffer)
                 (setq show-details nil))

                (?d
                 ;; Enter the details window.
                 (set-window-buffer (get-buffer-window buffer) cert-buffer)
                 (with-current-buffer cert-buffer
                   (read-only-mode -1)
                   (insert (nth cert-index pems))
                   (goto-char (point-min))
                   (read-only-mode))
                 (setq show-details t))

                (?b
                 ;; Scroll down.
                 (with-selected-window (get-buffer-window buf)
                   (with-current-buffer buf
                     (ignore-errors (scroll-down)))))

                (?f
                 ;; Scroll up.
                 (with-selected-window (get-buffer-window buf)
                   (with-current-buffer buf
                     (ignore-errors (scroll-up)))))

                (?n
                 ;; "No" or "next certificate".
                 (if show-details
                     (with-current-buffer cert-buffer
                       (read-only-mode -1)
                       (erase-buffer)
                       (setq cert-index (mod (1+ cert-index) (length pems)))
                       (insert (nth cert-index pems))
                       (goto-char (point-min))
                       (read-only-mode))
                   (setq done t)))

                (?a
                 ;; "Always"
                 (setq done t))

                (?s
                 ;; "Session only"
                 (setq done t))

                (?p
                 ;; Previous certificate.
                 (with-current-buffer cert-buffer
                   (read-only-mode -1)
                   (erase-buffer)
                   (setq cert-index (mod (1- cert-index) (length pems)))
                   (insert (nth cert-index pems))
                   (goto-char (point-min))
                   (read-only-mode)))))
            ;; Return the answer.
            (cadr answer))
        (kill-buffer cert-buffer)
        (kill-buffer buffer)))))

(defun nsm-save-host (host port status what problems permanency)
  (let* ((id (nsm-id host port))
         (saved-fingerprints (plist-get (nsm-host-settings id) :fingerprints))
         (fingerprints (cl-delete-duplicates
                        (append saved-fingerprints
                                (list (or (nsm-fingerprint status)
                                          ;; Plain connection.
                                          :none)))
                        :test #'string=))
         (saved (list :id id :fingerprints fingerprints)))
    (when (or (eq what 'conditions)
	      nsm-save-host-names)
      (nconc saved (list :host (format "%s:%s" host port))))
    ;; We either want to save/update the fingerprint or the conditions
    ;; of the certificate/unencrypted connection.
    (cond
     ((eq what 'conditions)
      (plist-put saved :conditions problems))
     ;; Make sure the conditions are not erased when we save a
     ;; fingerprint
     ((eq what 'fingerprint)
      ;; Store additional protocol settings.
      (let ((settings (nsm-host-settings id)))
        (when settings
          (setq saved settings))
        (if (plist-get saved :conditions)
            (plist-put saved :conditions
                       (cl-delete-duplicates
                        (nconc (plist-get saved :conditions) problems)))
          (plist-put saved :conditions problems)))))
    (if (eq permanency 'always)
	(progn
	  (nsm-remove-temporary-setting id)
	  (nsm-remove-permanent-setting id)
	  (push saved nsm-permanent-host-settings)
	  (nsm-write-settings))
      (nsm-remove-temporary-setting id)
      (push saved nsm-temporary-host-settings))))

(defun nsm-write-settings ()
  (with-temp-file nsm-settings-file
    (insert "(\n")
    (dolist (setting nsm-permanent-host-settings)
      (insert " ")
      (prin1 setting (current-buffer))
      (insert "\n"))
    (insert ")\n")))

(defun nsm-read-settings ()
  (setq nsm-permanent-host-settings
	(with-temp-buffer
	  (insert-file-contents nsm-settings-file)
	  (goto-char (point-min))
	  (ignore-errors (read (current-buffer))))))

(defun nsm-id (host port)
  (concat "sha1:" (sha1 (format "%s:%s" host port))))

(defun nsm-host-settings (id)
  (when (and (not nsm-permanent-host-settings)
	     (file-exists-p nsm-settings-file))
    (nsm-read-settings))
  (let ((result nil))
    (dolist (elem (append nsm-temporary-host-settings
			  nsm-permanent-host-settings))
      (when (and (not result)
		 (equal (plist-get elem :id) id))
	(setq result elem)))
    result))

(defun nsm-warnings-ok-p (status settings)
  (let ((ok t)
	(conditions (plist-get settings :conditions)))
    (dolist (warning (plist-get status :warnings))
      (unless (memq warning conditions)
	(setq ok nil)))
    ok))

(defun nsm-remove-permanent-setting (id)
  (setq nsm-permanent-host-settings
	(cl-delete-if
	 (lambda (elem)
	   (equal (plist-get elem :id) id))
	 nsm-permanent-host-settings)))

(defun nsm-remove-temporary-setting (id)
  (setq nsm-temporary-host-settings
	(cl-delete-if
	 (lambda (elem)
	   (equal (plist-get elem :id) id))
	 nsm-temporary-host-settings)))

(defun nsm-format-certificate (status)
  (let ((cert (plist-get status :certificate)))
    (when cert
      (with-temp-buffer
        (insert
	 (propertize "Certificate information" 'face 'underline) "\n"
	 "  Issued by:"
         (nsm-certificate-part (plist-get cert :issuer) "CN" t) "\n"
	 "  Issued to:"
	 (or (nsm-certificate-part (plist-get cert :subject) "O")
	     (nsm-certificate-part (plist-get cert :subject) "OU" t))
         "\n"
	 "  Hostname:"
	 (nsm-certificate-part (plist-get cert :subject) "CN" t) "\n")
	(when (and (plist-get cert :public-key-algorithm)
		   (plist-get cert :signature-algorithm))
	  (insert
	   "  Public key:" (plist-get cert :public-key-algorithm)
	   ", signature: " (plist-get cert :signature-algorithm) "\n"))
        (when (and (plist-get status :key-exchange)
		   (plist-get status :cipher)
		   (plist-get status :mac)
		   (plist-get status :protocol))
	  (insert
	   "  Session:" (plist-get status :protocol)
	   ", key: " (plist-get status :key-exchange)
	   ", cipher: " (plist-get status :cipher)
	   ", mac: " (plist-get status :mac) "\n"))
        (when (plist-get cert :certificate-security-level)
	  (insert
	   "  Security level:"
	   (propertize (plist-get cert :certificate-security-level)
		       'face 'bold)
	   "\n"))
	(insert
	 "  Valid:From " (plist-get cert :valid-from)
	 " to " (plist-get cert :valid-to) "\n")
        (insert "\n")
        (goto-char (point-min))
	(while (re-search-forward "^[^:]+:" nil t)
	  (insert (make-string (- 22 (current-column)) ? )))
	(buffer-string)))))

(defun nsm-level (symbol)
  "Return a numerical level for SYMBOL for easier comparison."
  (cond
   ((eq symbol 'low) 0)
   ((eq symbol 'medium) 1)
   (t 2)))

(defun nsm-cipher-suite (status)
  (format "%s-%s-%s"
          (plist-get status :key-exchange)
          (plist-get status :cipher)
          (plist-get status :mac)))

(defun nsm-certificate-part (string part &optional full)
  (let ((part (cadr (assoc part (nsm-parse-subject string)))))
    (cond
     (part part)
     (full string)
     (t nil))))

(defun nsm-parse-subject (string)
  (with-temp-buffer
    (insert string)
    (goto-char (point-min))
    (let ((start (point))
	  (result nil))
      (while (not (eobp))
	(push (replace-regexp-in-string
	       "[\\]\\(.\\)" "\\1"
	       (buffer-substring start
				 (if (re-search-forward "[^\\]," nil 'move)
				     (1- (point))
				   (point))))
	      result)
	(setq start (point)))
      (mapcar
       (lambda (elem)
	 (let ((pos (cl-position ?= elem)))
	   (if pos
	       (list (substring elem 0 pos)
		     (substring elem (1+ pos)))
	     elem)))
       (nreverse result)))))

(define-obsolete-function-alias 'nsm--encryption #'nsm-cipher-suite "27.1")

(provide 'nsm)

;;; nsm.el ends here
