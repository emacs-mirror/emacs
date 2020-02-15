;;; so-long-tests-helpers.el --- Test suite for so-long.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2020 Free Software Foundation, Inc.

;; Author: Phil Sainty <psainty@orcon.net.nz>
;; Keywords: convenience

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
(require 'so-long)

(defvar longlines-mode)
(declare-function longlines-mode "longlines")

(defvar so-long-tests-memory nil
  "Original values of minor modes and variables.")

(defun so-long-tests-assert-active (action)
  "Assert that ACTION is active."
  (cl-destructuring-bind (_key _label actionfunc revertfunc)
      (assq action so-long-action-alist)
    (should (eq so-long-function actionfunc))
    (should (eq so-long-revert-function revertfunc))
    (should (eq so-long-enabled t))
    (should (eq so-long--active t))
    ;; pcase fails here in Emacs 24.
    (cl-case action
      ('so-long-mode
       (should (eq major-mode 'so-long-mode))
       (so-long-tests-assert-overrides))
      ('so-long-minor-mode
       (should (eq so-long-minor-mode t))
       (so-long-tests-assert-overrides))
      ('longlines-mode
       (should (eq longlines-mode t))))))

(defun so-long-tests-assert-reverted (action)
  "Assert that ACTION has been reverted."
  (cl-destructuring-bind (_key _label actionfunc revertfunc)
      (assq action so-long-action-alist)
    (should (eq so-long-function actionfunc))
    (should (eq so-long-revert-function revertfunc))
    (should (eq so-long-enabled t))
    (should (eq so-long--active nil))
    ;; pcase fails here in Emacs 24.
    (cl-case action
      ('so-long-mode
       (should-not (eq major-mode 'so-long-mode))
       (so-long-tests-assert-overrides-reverted))
      ('so-long-minor-mode
       (should-not (eq so-long-minor-mode t))
       (so-long-tests-assert-overrides-reverted))
      ('longlines-mode
       (should-not (eq longlines-mode t))))))

(defun so-long-tests-assert-and-revert (action)
  "Assert ACTION, revert it, and then assert the revert."
  (so-long-tests-assert-active action)
  (so-long-revert)
  (so-long-tests-assert-reverted action))

(defun so-long-tests-assert-overrides ()
  "Assert that overridden modes and variables have their expected values."
  (dolist (ovar so-long-variable-overrides)
    (when (boundp (car ovar))
      (should (equal (symbol-value (car ovar)) (cdr ovar)))))
  (dolist (mode so-long-minor-modes)
    (when (boundp mode)
      (should (eq (symbol-value mode) nil)))))

(defun so-long-tests-assert-overrides-reverted ()
  "Assert that each remembered variable has its original value."
  (dolist (ovar so-long-tests-memory)
    (when (boundp (car ovar))
      (should (equal (symbol-value (car ovar)) (cdr ovar))))))

(defun so-long-tests-remember ()
  "Remember the original states of modes and variables.

Call this after setting up a buffer in the normal (not so-long)
state for its major mode, so that after triggering a so-long
action we can call `so-long-revert' and compare the reverted
state against this remembered state."
  (setq so-long-tests-memory nil)
  (push (cons 'major-mode major-mode)
        so-long-tests-memory)
  (dolist (ovar so-long-variable-overrides)
    (when (boundp (car ovar))
      (push (cons (car ovar) (symbol-value (car ovar)))
            so-long-tests-memory)))
  (dolist (mode so-long-minor-modes)
    (when (boundp mode)
      (push (cons mode (symbol-value mode))
            so-long-tests-memory))))

(provide 'so-long-tests-helpers)
;;; so-long-tests-helpers.el ends here
