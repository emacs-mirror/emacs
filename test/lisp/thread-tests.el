;;; thread-tests.el --- Test suite for thread.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2018-2020 Free Software Foundation, Inc.

;; Author: Gemini Lasswell <gazally@runbox.com>
;; Keywords: threads

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
(require 'thread)

;; Declare the functions used here in case Emacs has been configured
;; --without-threads.
(declare-function make-mutex "thread.c" (&optional name))
(declare-function mutex-lock "thread.c" (mutex))
(declare-function mutex-unlock "thread.c" (mutex))
(declare-function make-thread "thread.c" (function &optional name))
(declare-function thread-join "thread.c" (thread))
(declare-function thread-yield "thread.c" ())

(defvar thread-tests-flag)
(defvar thread-tests-mutex (when (featurep 'threads) (make-mutex "mutex1")))

(defun thread-tests--thread-function ()
  (setq thread-tests-flag t)
  (with-mutex thread-tests-mutex
    (sleep-for 0.01)))

(ert-deftest thread-tests-thread-list-send-error ()
  "A thread can be sent an error signal from the *Thread List* buffer."
  (skip-unless (featurep 'threads))
  (cl-letf (((symbol-function 'y-or-n-p) (lambda (_prompt) t)))
    (with-mutex thread-tests-mutex
      (setq thread-tests-flag nil)
      (let ((thread (make-thread #'thread-tests--thread-function
                                 "thread-tests-wait")))
        (while (not thread-tests-flag)
          (thread-yield))
        (list-threads)
        (goto-char (point-min))
        (re-search-forward
         "^thread-tests.+[[:blank:]]+Blocked[[:blank:]]+.+mutex1.+?")
        (thread-list-send-error-signal)
        (should-error (thread-join thread))
        (list-threads)
        (goto-char (point-min))
        (should-error (re-search-forward "thread-tests"))))))

(ert-deftest thread-tests-thread-list-show-backtrace ()
  "Show a backtrace for another thread from the *Thread List* buffer."
  (skip-unless (featurep 'threads))
  (let (thread)
    (with-mutex thread-tests-mutex
      (setq thread-tests-flag nil)
      (setq thread
            (make-thread #'thread-tests--thread-function "thread-tests-back"))
      (while (not thread-tests-flag)
        (thread-yield))
      (list-threads)
      (goto-char (point-min))
      (re-search-forward
       "^thread-tests.+[[:blank:]]+Blocked[[:blank:]]+.+mutex1.+?")
      (thread-list-pop-to-backtrace)
      (goto-char (point-min))
      (re-search-forward "thread-tests-back")
      (re-search-forward "mutex-lock")
      (re-search-forward "thread-tests--thread-function"))
    (thread-join thread)))

(ert-deftest thread-tests-list-threads-error-when-not-configured ()
  "Signal an error running `list-threads' if threads are not configured."
  (skip-unless (not (featurep 'threads)))
  (should-error (list-threads)))

(provide 'thread-tests)

;;; thread-tests.el ends here
