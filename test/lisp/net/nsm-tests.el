;;; network-stream-tests.el --- tests for network security manager -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2020 Free Software Foundation, Inc.

;; Author: Robert Pluim <rpluim@gmail.com>

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

(require 'nsm)
(eval-when-compile (require 'cl-lib))

(ert-deftest nsm-check-local-subnet-ipv4 ()
  "Check that nsm can be avoided for local subnets."
  (let ((local-ip '[172 26 128 160 0])
        (mask '[255 255 255 0 0])

        (wrong-length-mask '[255 255 255])
        (wrong-mask '[255 255 255 255 0])
        (remote-ip-yes '[172 26 128 161 0])
        (remote-ip-no '[172 26 129 161 0]))

    (should (eq t (nsm-network-same-subnet local-ip mask remote-ip-yes)))
    (should (eq nil (nsm-network-same-subnet local-ip mask remote-ip-no)))
    (should-error (nsm-network-same-subnet local-ip wrong-length-mask remote-ip-yes))
    (should (eq nil (nsm-network-same-subnet local-ip wrong-mask remote-ip-yes)))
    (should (eq t (nsm-should-check "google.com")))
    (should (eq t (nsm-should-check "127.0.0.1")))
    (should (eq t (nsm-should-check "localhost")))
    (let ((nsm-trust-local-network t))
      (should (eq t (nsm-should-check "google.com")))
      (should (eq nil (nsm-should-check "127.0.0.1")))
      (should (eq nil (nsm-should-check "localhost"))))))

(defun nsm-ipv6-is-available ()
  (and (featurep 'make-network-process '(:family ipv6))
       (cl-rassoc-if
        (lambda (elt)
          (eq 9 (length elt)))
        (network-interface-list))))

(ert-deftest nsm-check-local-subnet-ipv6 ()
  (skip-unless (nsm-ipv6-is-available))
  (let ((local-ip '[123 456 789 11 172 26 128 160 0])
        (mask '[255 255 255 255 255 255 255 0 0])

        (wrong-length-mask '[255 255 255 255 255 255 255])
        (wrong-mask '[255 255 255 255 255 255 255 255 0])
        (remote-ip-yes '[123 456 789 11 172 26 128 161 0])
        (remote-ip-no '[123 456 789 11 172 26 129 161 0]))
    (should (eq t (nsm-network-same-subnet local-ip mask remote-ip-yes)))
    (should (eq nil (nsm-network-same-subnet local-ip mask remote-ip-no)))
    (should-error (nsm-network-same-subnet local-ip wrong-length-mask remote-ip-yes))
    (should (eq nil (nsm-network-same-subnet local-ip wrong-mask remote-ip-yes))))
  (should (eq t (nsm-should-check "::1")))
  (let ((nsm-trust-local-network t))
    (should (eq nil (nsm-should-check "::1")))))


;;; nsm-tests.el ends here
