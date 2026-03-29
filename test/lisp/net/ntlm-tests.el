;;; ntlm-tests.el --- tests for ntlm.el            -*- lexical-binding: t -*-

;; Copyright (C) 2020-2026 Free Software Foundation, Inc.

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

;; Run this with `NTLM_TESTS_VERBOSE=1' to get verbose debugging.

;;; Code:

(require 'ert)
(require 'ert-x)
(require 'ntlm)

(defsubst ntlm-tests-message (format-string &rest arguments)
  "Print a message conditional on an environment variable being set.
FORMAT-STRING and ARGUMENTS are passed to the message function."
  (when (getenv "NTLM_TESTS_VERBOSE")
    (apply #'message (concat "ntlm-tests: " format-string) arguments)))


;; This is the Lisp bignum implementation of `ntlm--time-to-timestamp',
;; for reference.

(defun ntlm-tests--time-to-timestamp (time)
  "Convert TIME to an NTLMv2 timestamp.
Return a unibyte string representing the number of tenths of a
microsecond since January 1, 1601 as a 64-bit little-endian
signed integer.  TIME must be on the form (HIGH LOW USEC PSEC)."
  (let* ((s (+ (ash (nth 0 time) 16) (nth 1 time)))
         (us (nth 2 time))
         (ps (nth 3 time))
         (tenths-of-us-since-jan-1-1601
          (+ (* s 10000000) (* us 10) (/ ps 100000)
	     ;; tenths of microseconds between 1601-01-01 and 1970-01-01
	     116444736000000000)))
    (apply #'unibyte-string
           (mapcar (lambda (i)
                     (logand (ash tenths-of-us-since-jan-1-1601 (* i -8))
                             #xff))
                   (number-sequence 0 7)))))

(ert-deftest ntlm-time-to-timestamp ()
  ;; Verify poor man's bignums in implementation that can run on Emacs < 27.1.
  (let ((time '(24471 63910 412962 0)))
    (should (equal (ntlm--time-to-timestamp time)
                   (ntlm-tests--time-to-timestamp time))))
  (let ((time '(397431 65535 999999 999999)))
    (should (equal (ntlm--time-to-timestamp time)
                   (ntlm-tests--time-to-timestamp time)))))

(defvar ntlm-tests--username-oem "ntlm"
  "The username for NTLM authentication tests, in OEM string encoding.")
(defvar ntlm-tests--username-unicode
  (ntlm-ascii2unicode ntlm-tests--username-oem
		      (length ntlm-tests--username-oem))
  "The username for NTLM authentication tests, in Unicode string encoding.")

(defvar ntlm-tests--password "ntlm"
  "The password used for NTLM authentication tests.")

(defvar ntlm-tests--client-supports-unicode nil
  "Non-nil if client supports Unicode strings.
If client only supports OEM strings, nil.")

(defvar ntlm-tests--challenge nil "The global random challenge.")

(defun ntlm-server-build-type-2 ()
  "Return an NTLM Type 2 message as a string.
This string will be returned from the NTLM server to the NTLM client."
  (let ((target (if ntlm-tests--client-supports-unicode
		    (ntlm-ascii2unicode "DOMAIN" (length "DOMAIN"))
		  "DOMAIN"))
	(target-information ntlm-tests--password)
	;; Flag byte 1 flags.
	(_negotiate-unicode 1)
	(negotiate-oem 2)
	(request-target 4)
	;; Flag byte 2 flags.
	(negotiate-ntlm 2)
	(_negotiate-local-call 4)
	(_negotiate-always-sign 8)
	;; Flag byte 3 flags.
	(_target-type-domain 1)
	(_target-type-server 2)
	(target-type-share 4)
	(_negotiate-ntlm2-key 8)
	(negotiate-target-information 128)
	;; Flag byte 4 flags, unused.
	(_negotiate-128 32)
	(_negotiate-56 128))
    (concat
     ;; Signature.
     "NTLMSSP" (unibyte-string 0)
     ;; Type 2.
     (unibyte-string 2 0 0 0)
     ;; Target length
     (unibyte-string (length target) 0)
     ;; Target allocated space.
     (unibyte-string (length target) 0)
     ;; Target offset.
     (unibyte-string 48 0 0 0)
     ;; Flags.
     ;; Flag byte 1.
     ;; Tell the client that this test server only supports OEM
     ;; strings.  This test server will handle Unicode strings
     ;; anyway though.
     (unibyte-string (logior negotiate-oem request-target))
     ;; Flag byte 2.
     (unibyte-string negotiate-ntlm)
     ;; Flag byte 3.
     (unibyte-string (logior negotiate-target-information target-type-share))
     ;; Flag byte 4.	Not sure what 2 means here.
     (unibyte-string 2)
     ;; Challenge.  Set this to (unibyte-string 1 2 3 4 5 6 7 8)
     ;; instead of (ntlm-generate-nonce) to hold constant for
     ;; debugging.
     (setq ntlm-tests--challenge (ntlm-generate-nonce))
     ;; Context.
     (make-string 8 0)
     (unibyte-string (length target-information) 0)
     (unibyte-string (length target-information) 0)
     (unibyte-string 54 0 0 0)
     target
     target-information)))

(defun ntlm-server-hash (challenge blob username password)
  "Hash CHALLENGE, BLOB, USERNAME and PASSWORD for a Type 3 check."
  (hmac-md5 (concat challenge blob)
	    (hmac-md5 (concat
		       (upcase
			;; This calculation always uses
			;; Unicode username, even when the
			;; server only supports OEM strings.
			(ntlm-ascii2unicode username (length username))) "")
		      (cadr (ntlm-get-password-hashes password)))))

(defun ntlm-server-check-authorization (authorization-string)
  "Return t if AUTHORIZATION-STRING correctly authenticates the user."
  (let* ((binary (base64-decode-string
		  (caddr (split-string authorization-string " "))))
	 (_lm-response-length (md4-unpack-int16 (substring binary 12 14)))
	 (_lm-response-offset
	  (cdr (md4-unpack-int32 (substring binary 16 20))))
	 (ntlm-response-length (md4-unpack-int16 (substring binary 20 22)))
	 (ntlm-response-offset
	  (cdr (md4-unpack-int32 (substring binary 24 28))))
	 (ntlm-hash
	  (substring binary ntlm-response-offset (+ ntlm-response-offset 16)))
	 (username-length (md4-unpack-int16 (substring binary 36 38)))
	 (username-offset (cdr (md4-unpack-int32 (substring binary 40 44))))
	 (username (substring binary username-offset
			      (+ username-offset username-length))))
    (if (equal ntlm-response-length 24)
	(let* ((expected
		(ntlm-smb-owf-encrypt
		 (cadr (ntlm-get-password-hashes ntlm-tests--password))
		 ntlm-tests--challenge))
	       (received (substring binary ntlm-response-offset
				    (+ ntlm-response-offset
				       ntlm-response-length))))
	  (ntlm-tests-message "Got NTLMv1 response:")
	  (ntlm-tests-message "Expected hash:     ===%S===" expected)
	  (ntlm-tests-message "Got      hash:     ===%S===" received)
	  (ntlm-tests-message "Expected username: ===%S==="
			      ntlm-tests--username-oem)
	  (ntlm-tests-message "Got      username: ===%S===" username)
	  (and (or (equal username ntlm-tests--username-oem)
		   (equal username ntlm-tests--username-unicode))
	       (equal expected received)))
      (let* ((ntlm-response-blob
	      (substring binary (+ ntlm-response-offset 16)
			 (+ (+ ntlm-response-offset 16)
			    (- ntlm-response-length 16))))
	     (_ntlm-timestamp (substring ntlm-response-blob 8 16))
	     (_ntlm-nonce (substring ntlm-response-blob 16 24))
	     (_target-length (md4-unpack-int16 (substring binary 28 30)))
	     (_target-offset
	      (cdr (md4-unpack-int32 (substring binary 32 36))))
	     (_workstation-length (md4-unpack-int16 (substring binary 44 46)))
	     (_workstation-offset
	      (cdr (md4-unpack-int32 (substring binary 48 52)))))
	(cond
	 ;; This test server claims to only support OEM strings,
	 ;; but also checks Unicode strings.
	 ((or (equal username ntlm-tests--username-oem)
	      (equal username ntlm-tests--username-unicode))
	  (let* ((password ntlm-tests--password)
		 (ntlm-hash-from-type-3 (ntlm-server-hash
					 ntlm-tests--challenge
					 ntlm-response-blob
					 ;; Always -oem since
					 ;; `ntlm-server-hash'
					 ;; always converts it to
					 ;; Unicode.
					 ntlm-tests--username-oem
					 password)))
	    (ntlm-tests-message "Got NTLMv2 response:")
	    (ntlm-tests-message "Expected hash: ==%S==" ntlm-hash)
	    (ntlm-tests-message "Got      hash: ==%S==" ntlm-hash-from-type-3)
	    (ntlm-tests-message "Expected username: ===%S==="
				ntlm-tests--username-oem)
	    (ntlm-tests-message "      or username: ===%S==="
		     ntlm-tests--username-unicode)
	    (ntlm-tests-message "Got      username: ===%S===" username)
	    (equal ntlm-hash ntlm-hash-from-type-3)))
	 (t
	  nil))))))

(require 'eieio)
(require 'cl-lib)

;; Silence some byte-compiler warnings that occur when
;; web-server/web-server.el is not found.
(eval-when-compile (cl-pushnew 'headers eieio--known-slot-names)
                   (cl-pushnew 'process eieio--known-slot-names))
(declare-function ws-send nil)
(declare-function ws-parse-request nil)
(declare-function ws-start nil)
(declare-function ws-stop-all nil)

(eval-and-compile
  (push (expand-file-name "../elpa/packages/web-server/" source-directory)
        load-path)
  (require 'web-server nil t)
  (push (expand-file-name "../elpa/packages/url-http-ntlm/" source-directory)
        load-path)
  (require 'url-http-ntlm nil t))

(defun ntlm-server-do-token (request _process)
  "Process an NTLM client's REQUEST.
PROCESS is unused."
  (with-slots (process headers) request
    (let* ((header-alist (cdr headers))
	   (authorization-header (assoc ':AUTHORIZATION header-alist))
	   (authorization-string (cdr authorization-header)))
      (if (and (stringp authorization-string)
	       (string-match "NTLM " authorization-string))
	  (let* ((challenge (substring authorization-string (match-end 0)))
		 (binary (base64-decode-string challenge))
		 (type (aref binary 8))
		 ;; Flag byte 1 flags.
		 (negotiate-unicode 1)
		 (negotiate-oem 2)
		 (flags-byte-1 (aref binary 12))
		 (client-supports-unicode
		  (not (zerop (logand flags-byte-1 negotiate-unicode))))
		 (client-supports-oem
		  (not (zerop (logand flags-byte-1 negotiate-oem))))
		 (connection-header (assoc ':CONNECTION header-alist))
		 (_keep-alive
		  (when connection-header (cdr connection-header)))
		 (response
		  (cl-case type
		    (1
		     ;; Return Type 2 message.
		     (when (and (not client-supports-unicode)
				(not client-supports-oem))
		       (warn (concat
			      "Weird client supports neither Unicode"
			      " nor OEM strings, using OEM.")))
		     (setq ntlm-tests--client-supports-unicode
			   client-supports-unicode)
		     (concat
		      "HTTP/1.1 401 Unauthorized\r\n"
		      "WWW-Authenticate: NTLM "
		      (base64-encode-string
		       (ntlm-server-build-type-2) t) "\r\n"
		      "WWW-Authenticate: Negotiate\r\n"
		      "WWW-Authenticate: Basic realm=\"domain\"\r\n"
		      "Content-Length: 0\r\n\r\n"))
		    (3
		     (if (ntlm-server-check-authorization
			  authorization-string)
			 "HTTP/1.1 200 OK\r\n\r\nAuthenticated.\r\n"
		       (progn
			 (if process
			     (set-process-filter process nil)
			   (error "Type 3 message found first?"))
			 (concat "HTTP/1.1 401 Unauthorized\r\n\r\n"
				 "Access Denied.\r\n")))))))
	    (if response
		(ws-send process response)
	      (when process
		(set-process-filter process nil)))
	    (when (equal type 3)
	      (set-process-filter process nil)
	      (process-send-eof process)))
	(progn
	  ;; Did not get NTLM anything.
	  (set-process-filter process nil)
	  (process-send-eof process)
	  (concat "HTTP/1.1 401 Unauthorized\r\n\r\n"
		  "Access Denied.\r\n"))))))

(defun ntlm-server-filter (process string)
  "Read from PROCESS a STRING and treat it as a request from an NTLM client."
  (let ((request (make-instance 'ws-request
				:process process :pending string)))
    (if (ws-parse-request request)
	(ntlm-server-do-token request process)
      (error "Failed to parse request"))))

(defun ntlm-server-handler (request)
  "Handle an HTTP REQUEST."
  (with-slots (process headers) request
    (let* ((header-alist (cdr headers))
	   (authorization-header (assoc ':AUTHORIZATION header-alist))
	   (connection-header (assoc ':CONNECTION header-alist))
	   (keep-alive (when connection-header (cdr connection-header)))
	   (response (concat
		      "HTTP/1.1 401 Unauthorized\r\n"
		      "WWW-Authenticate: Negotiate\r\n"
		      "WWW-Authenticate: NTLM\r\n"
		      "WWW-Authenticate: Basic realm=\"domain\"\r\n"
		      "Content-Length: 0\r\n\r\n")))
      (if (null authorization-header)
	  ;; Tell client to use NTLM.	 Firefox will create a new
	  ;; connection.
	  (progn
	    (process-send-string process response)
	    (process-send-eof process))
	(progn
	  (ntlm-server-do-token request nil)
	  (set-process-filter process #'ntlm-server-filter)
	  (if (equal (upcase keep-alive) "KEEP-ALIVE")
	      :keep-alive
	    (error "NTLM server expects keep-alive connection header")))))))

(defun ntlm-server-start ()
  "Start an NTLM server on port 8080 for testing."
  (ws-start 'ntlm-server-handler 8080))

(defun ntlm-server-stop ()
  "Stop the NTLM server."
  (ws-stop-all))

(defvar ntlm-tests--result-buffer nil "Final NTLM result buffer.")

(require 'url)

(defun ntlm-tests--url-retrieve-internal-around (original &rest arguments)
  "Save the result buffer from a `url-retrieve-internal' to a global variable.
ORIGINAL is the original `url-retrieve-internal' function and
ARGUMENTS are passed to it."
  (setq ntlm-tests--result-buffer (apply original arguments)))

(defun ntlm-tests--authenticate ()
  "Authenticate using credentials from the authinfo resource file."
  (setq ntlm-tests--result-buffer nil)
  (let ((auth-sources (list (ert-resource-file "authinfo")))
	(auth-source-do-cache nil)
	(auth-source-debug (when (getenv "NTLM_TESTS_VERBOSE") 'trivia)))
    (ntlm-tests-message "Using auth-sources: %S" auth-sources)
    (url-retrieve-synchronously "http://localhost:8080"))
  (sleep-for 0.1)
  (ntlm-tests-message "Results are in: %S" ntlm-tests--result-buffer)
  (with-current-buffer ntlm-tests--result-buffer
    (buffer-string)))

(defun ntlm-tests--start-server-authenticate-stop-server ()
  "Start an NTLM server, authenticate against it, then stop the server."
  (advice-add #'url-retrieve-internal
	      :around #'ntlm-tests--url-retrieve-internal-around)
  (ntlm-server-stop)
  (ntlm-server-start)
  (let ((result (ntlm-tests--authenticate)))
    (advice-remove #'url-retrieve-internal
		   #'ntlm-tests--url-retrieve-internal-around)
    (ntlm-server-stop)
    result))

(defvar ntlm-tests--successful-result
  (concat "HTTP/1.1 200 OK\n\nAuthenticated." (unibyte-string 13) "\n")
  "Expected result of successful NTLM authentication.")

(require 'find-func)
(defun ntlm-tests--ensure-ws-parse-ntlm-support ()
  "Ensure NTLM special-case in `ws-parse'."
  (let* ((hit (find-function-search-for-symbol
	       'ws-parse nil (locate-file "web-server.el" load-path)))
	 (buffer (car hit))
	 (position (cdr hit)))
    (with-current-buffer buffer
      (goto-char position)
      (search-forward-regexp
       ":NTLM" (save-excursion (forward-sexp) (point)) t))))

(require 'lisp-mnt)
(defvar ntlm-tests--dependencies-present
  (and (featurep 'url-http-ntlm)
       (version<= "2.0.4"
		  (lm-version (locate-file "url-http-ntlm.el" load-path)))
       (featurep 'web-server)
       (ntlm-tests--ensure-ws-parse-ntlm-support))
  "Non-nil if GNU ELPA test dependencies were loaded.")

(ert-deftest ntlm-authentication ()
  "Check ntlm.el's implementation of NTLM authentication over HTTP."
  (skip-unless ntlm-tests--dependencies-present)
  (should (equal (ntlm-tests--start-server-authenticate-stop-server)
		 ntlm-tests--successful-result)))

(ert-deftest ntlm-authentication-old-compatibility-level ()
  (skip-unless ntlm-tests--dependencies-present)
  (setq ntlm-compatibility-level 0)
  (should (equal (ntlm-tests--start-server-authenticate-stop-server)
		 ntlm-tests--successful-result)))

(provide 'ntlm-tests)

;;; ntlm-tests.el ends here
