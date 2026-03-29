;;; keyboard-tests.el --- Tests for keyboard.c -*- lexical-binding: t -*-

;; Copyright (C) 2017-2026 Free Software Foundation, Inc.

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

;;; Code:

(require 'ert)

(ert-deftest keyboard-unread-command-events ()
  "Test `unread-command-events'."
  ;; Avoid hang on Cygwin; see bug#65325.
  (skip-unless (or (not (eq system-type 'cygwin))
                   (featurep 'gfilenotify)
                   (featurep 'dbus)
                   (featurep 'threads)))
  (let ((unread-command-events nil))
    (should (equal (progn (push ?\C-a unread-command-events)
                          (read-event nil nil 1))
                   ?\C-a))
    (should (equal (progn (run-with-timer
                           1 nil
                           (lambda () (push '(t . ?\C-b) unread-command-events)))
                          (read-event nil nil 2))
                   ?\C-b))))

(ert-deftest keyboard-lossage-size ()
  "Test `lossage-size'."
  (let ((min-value 100)
        (lossage-orig (lossage-size)))
    (dolist (factor (list 1 3 4 5 10 7 3))
      (let ((new-lossage (* factor min-value)))
        (should (= new-lossage (lossage-size new-lossage)))))
    ;; Wrong type
    (should-error (lossage-size -5))
    (should-error (lossage-size "200"))
    ;; Less that minimum value
    (should-error (lossage-size (1- min-value)))
    (should (= lossage-orig (lossage-size lossage-orig)))))

;; FIXME: This test doesn't currently work :-(
;; (ert-deftest keyboard-tests--echo-keystrokes-bug15332 ()
;;   (let ((msgs '())
;;         (unread-command-events nil)
;;         (redisplay--interactive t)
;;         (echo-keystrokes 2))
;;     (setq unread-command-events '(?\C-u))
;;     (let* ((timer1
;; 	    (run-with-timer 3 1
;; 			    (lambda ()
;; 			      (setq unread-command-events '(?5)))))
;; 	   (timer2
;; 	    (run-with-timer 2.5 1
;; 			    (lambda ()
;; 			      (push (current-message) msgs)))))
;;       (run-with-timer 5 nil
;; 	              (lambda ()
;; 	                (cancel-timer timer1)
;; 	                (cancel-timer timer2)
;; 	                (throw 'exit msgs)))
;;       (recursive-edit)
;;       (should (equal msgs '("C-u 55-" "C-u 5-" "C-u-"))))))

(ert-deftest keyboard-inhibit-interaction ()
  (let ((inhibit-interaction t))
    (should-error (read-char "foo: "))
    (should-error (read-event "foo: "))
    (should-error (read-char-exclusive "foo: "))))

(provide 'keyboard-tests)
;;; keyboard-tests.el ends here
