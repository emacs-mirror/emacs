;;; socks-tests.el --- tests for SOCKS -*- coding: utf-8; lexical-binding: t; -*-

;; Copyright (C) 2021-2026 Free Software Foundation, Inc.

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

(require 'ert)
(require 'socks)
(require 'url-http)

(ert-deftest socks-tests-auth-registration-and-suite-offer ()
  (ert-info ("Default favors user/pass auth")
    (should (equal socks-authentication-methods
                   '((2 "Username/Password" . socks-username/password-auth)
                     (0 "No authentication" . identity))))
    (should (equal "\2\0\2" (socks-build-auth-list)))) ; length [offer ...]
  (let (socks-authentication-methods)
    (ert-info ("Empty selection/no methods offered")
      (should (equal "\0" (socks-build-auth-list))))
    (ert-info ("Simulate library defaults")
      (socks-register-authentication-method 0 "No authentication"
                                            'identity)
      (should (equal socks-authentication-methods
                     '((0 "No authentication" . identity))))
      (should (equal "\1\0" (socks-build-auth-list)))
      (socks-register-authentication-method 2 "Username/Password"
                                            'socks-username/password-auth)
      (should (equal socks-authentication-methods
                     '((2 "Username/Password" . socks-username/password-auth)
                       (0 "No authentication" . identity))))
      (should (equal "\2\0\2" (socks-build-auth-list))))
    (ert-info ("Removal")
      (socks-unregister-authentication-method 2)
      (should (equal socks-authentication-methods
                     '((0 "No authentication" . identity))))
      (should (equal "\1\0" (socks-build-auth-list)))
      (socks-unregister-authentication-method 0)
      (should-not socks-authentication-methods)
      (should (equal "\0" (socks-build-auth-list))))))

(ert-deftest socks-tests-filter-response-parsing-v4 ()
  "Ensure new chunks added on right (Bug#45162)."
  (let* ((buf (generate-new-buffer "*test-socks-filter*"))
         (proc (start-process "test-socks-filter" buf "sleep" "1")))
    (process-put proc 'socks t)
    (process-put proc 'socks-state socks-state-waiting)
    (process-put proc 'socks-server-protocol 4)
    (ert-info ("Receive initial incomplete segment")
      (socks-filter proc (unibyte-string 0 90 0 0 93 184 216))
      ;; From example.com: OK status       ^      ^ msg start
      (ert-info ("State still set to waiting")
        (should (eq (process-get proc 'socks-state) socks-state-waiting)))
      (ert-info ("Response field is nil because processing incomplete")
        (should-not (process-get proc 'socks-response)))
      (ert-info ("Scratch field holds stashed partial payload")
        (should (string= (unibyte-string 0 90 0 0 93 184 216)
                         (process-get proc 'socks-scratch)))))
    (ert-info ("Last part arrives")
      (socks-filter proc "\42") ; ?\" 34
      (ert-info ("State transitions to complete (length check passes)")
        (should (eq (process-get proc 'socks-state) socks-state-connected)))
      (ert-info ("Scratch and response fields hold stash w. last chunk")
        (should (string= (unibyte-string 0 90 0 0 93 184 216 34)
                         (process-get proc 'socks-response)))
        (should (string= (process-get proc 'socks-response)
                         (process-get proc 'socks-scratch)))))
    (delete-process proc)
    (kill-buffer buf)))

(ert-deftest socks-tests-filter-response-parsing-v5 ()
  "Ensure new chunks added on right (Bug#45162)."
  (let* ((buf (generate-new-buffer "*test-socks-filter*"))
         (proc (start-process "test-socks-filter" buf "sleep" "1")))
    (process-put proc 'socks t)
    (process-put proc 'socks-state socks-state-waiting)
    (process-put proc 'socks-server-protocol 5)
    (ert-info ("Receive initial incomplete segment")
      ;; From fedora.org: 2605:bc80:3010:600:dead:beef:cafe:fed9
      ;; 5004 ~~> Version Status (OK) NOOP Addr-Type (4 -> IPv6)
      (socks-filter proc "\5\0\0\4\x26\x05\xbc\x80\x30\x10\x00\x60")
      (ert-info ("State still waiting and response empty")
        (should (eq (process-get proc 'socks-state) socks-state-waiting))
        (should-not (process-get proc 'socks-response)))
      (ert-info ("Scratch field holds partial payload of pending msg")
        (should (string= "\5\0\0\4\x26\x05\xbc\x80\x30\x10\x00\x60"
                         (process-get proc 'socks-scratch)))))
    (ert-info ("Middle chunk arrives")
      (socks-filter proc "\xde\xad\xbe\xef\xca\xfe\xfe\xd9")
      (ert-info ("State and response fields still untouched")
        (should (eq (process-get proc 'socks-state) socks-state-waiting))
        (should-not (process-get proc 'socks-response)))
      (ert-info ("Scratch contains new arrival appended (on RHS)")
        (should (string=  (concat "\5\0\0\4"
                                  "\x26\x05\xbc\x80\x30\x10\x00\x60"
                                  "\xde\xad\xbe\xef\xca\xfe\xfe\xd9")
                          (process-get proc 'socks-scratch)))))
    (ert-info ("Final part arrives (port number)")
      (socks-filter proc "\0\0")
      (ert-info ("State transitions to complete")
        (should (eq (process-get proc 'socks-state) socks-state-connected)))
      (ert-info ("Scratch and response fields show last chunk appended")
        (should (string=  (concat "\5\0\0\4"
                                  "\x26\x05\xbc\x80\x30\x10\x00\x60"
                                  "\xde\xad\xbe\xef\xca\xfe\xfe\xd9"
                                  "\0\0")
                          (process-get proc 'socks-scratch)))
        (should (string= (process-get proc 'socks-response)
                         (process-get proc 'socks-scratch)))))
    (delete-process proc)
    (kill-buffer buf)))

(defvar socks-tests-canned-server-patterns nil
  "Alist containing request/response cons pairs to be tried in order.
Vectors must match verbatim.  Strings are considered regex patterns.")

(defun socks-tests-canned-server-create ()
  "Create and return a fake SOCKS server."
  (let* ((port (nth 2 socks-server))
         (name (format "socks-server:%s"
                       (if (numberp port) port (ert-test-name (ert-running-test)))))
         (pats socks-tests-canned-server-patterns)
         (filt (lambda (proc line)
                 (pcase-let ((`(,pat . ,resp) (pop pats)))
                   (unless (or (and (vectorp pat) (equal pat (vconcat line)))
                               (and (stringp pat) (string-match-p pat line)))
                     (error "Unknown request: %s" line))
                   (setq resp (apply #'unibyte-string (append resp nil)))
                   (let ((print-escape-control-characters t))
                     (message "[%s] <- %s" name (prin1-to-string line))
                     (message "[%s] -> %s" name (prin1-to-string resp)))
                   (process-send-string proc resp))))
         (serv (make-network-process :server 1
                                     :buffer (get-buffer-create name)
                                     :filter filt
                                     :name name
                                     :family 'ipv4
                                     :host 'local
                                     :coding 'binary
                                     :service (or port t))))
    (set-process-query-on-exit-flag serv nil)
    (unless (numberp (nth 2 socks-server))
      (setf (nth 2 socks-server) (process-contact serv :service)))
    serv))

(defvar socks-tests--hello-world-http-request-pattern
  (cons "^GET /" (concat "HTTP/1.1 200 OK\r\n"
                         "Content-Type: text/plain\r\n"
                         "Content-Length: 13\r\n\r\n"
                         "Hello World!\n")))

(defun socks-tests-perform-hello-world-http-request (&optional method)
  "Start canned server, validate hello-world response, and finalize."
  (let* ((url-gateway-method (or method 'socks))
         (url (url-generic-parse-url "http://example.com"))
         (server (socks-tests-canned-server-create))
         ;;
         done
         ;;
         (cb (lambda (&rest _r)
               (goto-char (point-min))
               (should (search-forward "Hello World" nil t))
               (setq done t)))
         (inhibit-message noninteractive)
         (buf (url-http url cb '(nil)))
         (proc (get-buffer-process buf))
         (attempts 10))
    (while (and (not done) (< 0 (decf attempts)))
      (sleep-for 0.1))
    (should done)
    (delete-process server)
    (delete-process proc) ; otherwise seems client proc is sometimes reused
    (kill-buffer (process-buffer server))
    (kill-buffer buf)
    (ignore url-gateway-method)))

;; Unlike curl, socks.el includes the ID field (but otherwise matches):
;; $ curl --proxy socks4://127.0.0.1:1080 example.com

(ert-deftest socks-tests-v4-basic ()
  "Show correct preparation of SOCKS4 connect command (Bug#46342)."
  (let ((socks-server '("server" "127.0.0.1" t 4))
        (url-user-agent "Test/4-basic")
        (socks-username "foo")
        (socks-tests-canned-server-patterns
         `(([4 1 0 80 93 184 216 34 ?f ?o ?o 0] . [0 90 0 0 0 0 0 0])
           ,socks-tests--hello-world-http-request-pattern))
        socks-nslookup-program)
    (ert-info ("Make HTTP request over SOCKS4")
      (cl-letf (((symbol-function 'socks-nslookup-host)
                 (lambda (host)
                   (should (equal host "example.com"))
                   (list 93 184 216 34))))
        (socks-tests-perform-hello-world-http-request)))))

(ert-deftest socks-tests-v4a-basic ()
  "Show correct preparation of SOCKS4a connect command."
  (let ((socks-server '("server" "127.0.0.1" t 4a))
        (socks-username "foo")
        (url-user-agent "Test/4a-basic")
        (socks-tests-canned-server-patterns
         `(([4 1 0 80 0 0 0 1 ?f ?o ?o 0 ?e ?x ?a ?m ?p ?l ?e ?. ?c ?o ?m 0]
            . [0 90 0 0 0 0 0 0])
           ,socks-tests--hello-world-http-request-pattern)))
    (ert-info ("Make HTTP request over SOCKS4A")
      (socks-tests-perform-hello-world-http-request))))

(ert-deftest socks-tests-v4a-error ()
  "Show error signaled when destination address rejected."
  (let ((socks-server '("server" "127.0.0.1" t 4a))
        (url-user-agent "Test/4a-basic")
        (socks-username "")
        (socks-tests-canned-server-patterns
         `(([4 1 0 80 0 0 0 1 0 ?e ?x ?a ?m ?p ?l ?e ?. ?c ?o ?m 0]
            . [0 91 0 0 0 0 0 0])
           ,socks-tests--hello-world-http-request-pattern)))
    (ert-info ("Make HTTP request over SOCKS4A")
      (let ((err (should-error
                  (socks-tests-perform-hello-world-http-request))))
        (should (equal err '(error "SOCKS: Rejected or failed")))))))

;; Replace first pattern below with ([5 3 0 1 2] . [5 2]) to validate
;; against curl 7.71 with the following options:
;; $ curl --verbose -U foo:bar --proxy socks5h://127.0.0.1:10080 example.com

(ert-deftest socks-tests-v5-auth-user-pass ()
  "Verify correct handling of SOCKS5 user/pass authentication."
  (should (assq 2 socks-authentication-methods))
  (let ((socks-server '("server" "127.0.0.1" t 5))
        (socks-username "foo")
        (socks-password "bar")
        (url-user-agent "Test/auth-user-pass")
        (socks-tests-canned-server-patterns
         `(([5 2 0 2] . [5 2])
           ([1 3 ?f ?o ?o 3 ?b ?a ?r] . [1 0])
           ([5 1 0 3 11 ?e ?x ?a ?m ?p ?l ?e ?. ?c ?o ?m 0 80]
            . [5 0 0 1 0 0 0 0 0 0])
           ,socks-tests--hello-world-http-request-pattern)))
    (ert-info ("Make HTTP request over SOCKS5 with USER/PASS auth method")
      (socks-tests-perform-hello-world-http-request))))

;; Services (like Tor) may be configured without auth but for some
;; reason still prefer the user/pass method over none when offered both.
;; Given this library's defaults, the scenario below is possible.
;;
;; FYI: RFC 1929 doesn't say that a username or password is required
;; but notes that the length of both fields should be at least one.
;; However, both socks.el and curl send zero-length fields (though
;; curl drops the user part too when the password is empty).
;;
;; From Tor's docs /socks-extensions.txt, 1.1 Extent of support:
;; > We allow username/password fields of this message to be empty ...
;; line 41 in blob 5fd1f828f3e9d014f7b65fa3bd1d33c39e4129e2
;; https://gitweb.torproject.org/torspec.git/tree/socks-extensions.txt
;;
;; To verify against curl 7.71, swap out the first two pattern pairs
;; with ([5 3 0 1 2] . [5 2]) and ([1 0 0] . [1 0]), then run:
;; $ curl verbose -U "foo:" --proxy socks5h://127.0.0.1:10081 example.com

(ert-deftest socks-tests-v5-auth-user-pass-blank ()
  "Verify correct SOCKS5 user/pass authentication with empty pass."
  (should (assq 2 socks-authentication-methods))
  (let ((socks-server '("server" "127.0.0.1" t 5))
        (socks-username "foo") ; defaults to (user-login-name)
        (socks-password "") ; simulate user hitting enter when prompted
        (url-user-agent "Test/auth-user-pass-blank")
        (socks-tests-canned-server-patterns
         `(([5 2 0 2] . [5 2])
           ([1 3 ?f ?o ?o 0] . [1 0])
           ([5 1 0 3 11 ?e ?x ?a ?m ?p ?l ?e ?. ?c ?o ?m 0 80]
            . [5 0 0 1 0 0 0 0 0 0])
           ,socks-tests--hello-world-http-request-pattern)))
    (ert-info ("Make HTTP request over SOCKS5 with USER/PASS auth method")
      (socks-tests-perform-hello-world-http-request))))

;; Swap out ([5 2 0 1] . [5 0]) with the first pattern below to validate
;; against curl 7.71 with the following options:
;; $ curl --verbose --proxy socks5h://127.0.0.1:10082 example.com

(defun socks-tests-v5-auth-none (method)
  "Verify correct handling of SOCKS5 when auth method 0 requested."
  (let ((socks-server '("server" "127.0.0.1" t 5))
        (socks-authentication-methods (append socks-authentication-methods
                                              nil))
        (url-user-agent "Test/auth-none")
        (socks-tests-canned-server-patterns
         `(([5 1 0] . [5 0])
           ([5 1 0 3 11 ?e ?x ?a ?m ?p ?l ?e ?. ?c ?o ?m 0 80]
            . [5 0 0 1 0 0 0 0 0 0])
           ,socks-tests--hello-world-http-request-pattern)))
    (socks-unregister-authentication-method 2)
    (should-not (assq 2 socks-authentication-methods))
    (ert-info ("Make HTTP request over SOCKS5 with no auth method")
      (socks-tests-perform-hello-world-http-request method)))
  (should (assq 2 socks-authentication-methods)))

(ert-deftest socks-tests-v5-auth-none ()
  (socks-tests-v5-auth-none 'socks))

;; This simulates the top-level advice around `open-network-stream'
;; that's applied when loading the library with a non-nil
;; `socks-override-functions'.
(ert-deftest socks-override-functions ()
  (should-not socks-override-functions)
  (should-not (advice-member-p #'socks--open-network-stream
                               'open-network-stream))
  (advice-add 'open-network-stream :around #'socks--open-network-stream)
  (unwind-protect (let ((socks-override-functions t))
                    (socks-tests-v5-auth-none 'native))
    (advice-remove 'open-network-stream #'socks--open-network-stream))
  (should-not (advice-member-p #'socks--open-network-stream
                               'open-network-stream)))

;;; socks-tests.el ends here
