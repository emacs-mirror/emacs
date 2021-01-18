;;; socks-tests.el --- tests for SOCKS -*- coding: utf-8; lexical-binding: t; -*-

;; Copyright (C) 2021 Free Software Foundation, Inc.

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

(require 'socks)
(require 'url-http)

(defvar socks-tests-canned-server-port nil)

(defun socks-tests-canned-server-create (verbatim patterns)
  "Create a fake SOCKS server and return the process.

`VERBATIM' and `PATTERNS' are dotted alists containing responses.
Requests are tried in order.  On failure, an error is raised."
  (let* ((buf (generate-new-buffer "*canned-socks-server*"))
         (filt (lambda (proc line)
                 (let ((resp (or (assoc-default line verbatim
                                                (lambda (k s) ; s is line
                                                  (string= (concat k) s)))
                                 (assoc-default line patterns
                                                (lambda (p s)
                                                  (string-match-p p s))))))
                   (unless resp
                     (error "Unknown request: %s" line))
                   (let ((print-escape-control-characters t))
                     (princ (format "<- %s\n" (prin1-to-string line)) buf)
                     (princ (format "-> %s\n" (prin1-to-string resp)) buf))
                   (process-send-string proc (concat resp)))))
         (srv (make-network-process :server 1
                                    :buffer buf
                                    :filter filt
                                    :name "server"
                                    :family 'ipv4
                                    :host 'local
                                    :service socks-tests-canned-server-port)))
    (set-process-query-on-exit-flag srv nil)
    (princ (format "[%s] Listening on localhost:10080\n" srv) buf)
    srv))

;; Add ([5 3 0 1 2] . [5 2]) to the `verbatim' list below to validate
;; against curl 7.71 with the following options:
;; $ curl --verbose -U foo:bar --proxy socks5h://127.0.0.1:10080 example.com
;;
;; If later implementing version 4a, try these:
;; [4 1 0 80 0 0 0 1 0 ?e ?x ?a ?m ?p ?l ?e ?. ?c ?o ?m 0] . [0 90 0 0 0 0 0 0]
;; $ curl --verbose --proxy socks4a://127.0.0.1:10080 example.com

(ert-deftest socks-tests-auth-filter-url-http ()
  "Verify correct handling of SOCKS5 user/pass authentication."
  (let* ((socks-server '("server" "127.0.0.1" 10080 5))
         (socks-username "foo")
         (socks-password "bar")
         (url-gateway-method 'socks)
         (url (url-generic-parse-url "http://example.com"))
         (verbatim '(([5 2 0 2] . [5 2])
                     ([1 3 ?f ?o ?o 3 ?b ?a ?r] . [1 0])
                     ([5 1 0 3 11 ?e ?x ?a ?m ?p ?l ?e ?. ?c ?o ?m 0 80]
                      . [5 0 0 1 0 0 0 0 0 0])))
         (patterns
          `(("^GET /" . ,(concat "HTTP/1.1 200 OK\r\n"
                                 "Content-Type: text/plain; charset=UTF-8\r\n"
                                 "Content-Length: 13\r\n\r\n"
                                 "Hello World!\n"))))
         (socks-tests-canned-server-port 10080)
         (server (socks-tests-canned-server-create verbatim patterns))
         (tries 10)
         ;;
         done
         ;;
         (cb (lambda (&rest _r)
               (goto-char (point-min))
               (should (search-forward "Hello World" nil t))
               (setq done t)))
         (buf (url-http url cb '(nil))))
    (ert-info ("Connect to HTTP endpoint over SOCKS5 with USER/PASS method")
      (while (and (not done) (< 0 (cl-decf tries))) ; cl-lib via url-http
        (sleep-for 0.1)))
    (should done)
    (delete-process server)
    (kill-buffer (process-buffer server))
    (kill-buffer buf)
    (ignore url-gateway-method)))

;;; socks-tests.el ends here
