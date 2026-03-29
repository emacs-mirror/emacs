;;; process-callout-tests.el --- Testing the process facilities -*- lexical-binding: t -*-

;; Copyright (C) 2022-2026 Free Software Foundation, Inc.

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

;;

;;; Code:

(require 'cl-lib)
(require 'ert)

;;; This test is here in test/manual instead of
;;; test/src/process-tests.el for two reasons: The test suite
;;; shouldn't "call home" automatically, because that's against our
;;; privacy principles, and as a practical matter, the server may have
;;; problems, and that shouldn't trigger a test error.

(ert-deftest process-async-https-with-delay ()
  "Bug#49449: asynchronous TLS connection with delayed completion."
  (skip-unless (and internet-is-working (gnutls-available-p)))
  (let* ((status nil)
         (buf (url-http
                 #s(url "https" nil nil "elpa.gnu.org" nil
                        "/packages/archive-contents" nil nil t silent t t)
                 (lambda (s) (setq status s))
                 '(nil) nil 'tls)))
    (unwind-protect
        (progn
          ;; Busy-wait for 1 s to allow for the TCP connection to complete.
          (let ((delay 1.0)
                (t0 (float-time)))
            (while (< (float-time) (+ t0 delay))))
          ;; Wait for the entire operation to finish.
          (let ((limit 4.0)
                (t0 (float-time)))
            (while (and (null status)
                        (< (float-time) (+ t0 limit)))
              (sit-for 0.1)))
          (should status)
          (should-not (plist-get status ':error))
          (should buf)
          (should (> (buffer-size buf) 0))
          )
      (when buf
        (kill-buffer buf)))))

;;; process-callout-tests.el ends here
