;;; erc-notify-tests.el --- Tests for erc-notify  -*- lexical-binding:t -*-

;; Copyright (C) 2024-2026 Free Software Foundation, Inc.

;; This file is part of GNU Emacs.
;;
;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation, either version 3 of the License,
;; or (at your option) any later version.
;;
;; GNU Emacs is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;; Code:
(require 'erc-notify)

(require 'ert-x)
(eval-and-compile
  (let ((load-path (cons (ert-resource-directory) load-path)))
    (require 'erc-tests-common)))


;;;; Module `querypoll'

(ert-deftest erc--querypoll-compute-period ()
  (should (equal (mapcar (lambda (i)
                           (/ (round (* 100 (erc--querypoll-compute-period i)))
                              100.0))
                         (number-sequence 0 10))
                 '(11.0 10.05 9.19 8.41 7.7 7.07 6.49 5.97 5.49 5.07 4.68))))

(declare-function ring-insert "ring" (ring item))

(ert-deftest erc--querypoll-target-in-chan-p ()
  (erc-tests-common-make-server-buf)
  (with-current-buffer (erc--open-target "#chan")
    (erc-update-current-channel-member "bob" "bob" 'addp))

  (with-current-buffer (erc--open-target "bob")
    (should (erc--querypoll-target-in-chan-p (current-buffer))))

  (with-current-buffer (erc--open-target "alice")
    (should-not (erc--querypoll-target-in-chan-p (current-buffer))))

  (when noninteractive
    (erc-tests-common-kill-buffers)))

(ert-deftest erc--querypoll-get-length ()
  (erc-tests-common-make-server-buf)
  (with-current-buffer (erc--open-target "#chan")
    (erc-update-current-channel-member "bob" "bob" 'addp))

  (let ((ring (make-ring 5)))
    (ring-insert ring (with-current-buffer (erc--open-target "bob")))
    (should (= 0 (erc--querypoll-get-length ring)))
    (ring-insert ring (with-current-buffer (erc--open-target "alice")))
    (should (= 1 (erc--querypoll-get-length ring))))

  (when noninteractive
    (erc-tests-common-kill-buffers)))

(ert-deftest erc--querypoll-get-next ()
  (erc-tests-common-make-server-buf)
  (with-current-buffer (erc--open-target "#chan")
    (erc-update-current-channel-member "bob" "bob" 'addp)
    (erc-update-current-channel-member "alice" "alice" 'addp))

  (let ((ring (make-ring 5)))
    (ring-insert ring (with-current-buffer (erc--open-target "bob")))
    (ring-insert ring (with-current-buffer (erc--open-target "dummy")))
    (ring-insert ring (with-current-buffer (erc--open-target "alice")))
    (ring-insert ring (with-current-buffer (erc--open-target "tester")))
    (kill-buffer (get-buffer "dummy"))

    (should (eq (get-buffer "tester") (erc--querypoll-get-next ring))))

  (when noninteractive
    (erc-tests-common-kill-buffers)))

;;; erc-notify-tests.el ends here
