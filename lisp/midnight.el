;;; midnight.el --- run something every midnight, e.g., kill old buffers.

;;; Copyright (C) 1998 Free Software Foundation, Inc.

;;; Author: Sam Steingold <sds@usa.net>
;;; Maintainer: Sam Steingold <sds@usa.net>
;;; Created: 1998-05-18
;;; Keywords: utilities

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; To use the file, put (require 'midnight) into your .emacs.  Then, at
;; midnight, Emacs will run the normal hook `midnight-hook'.  You can
;; put whatever you like there, say, `calendar'; by default there is
;; only one function there - `clean-buffer-list'. It will kill the
;; buffers matching `clean-buffer-list-kill-buffer-names' and
;; `clean-buffer-list-kill-regexps' and the buffers which where last
;; displayed more than `clean-buffer-list-delay-general' days ago,
;; keeping `clean-buffer-list-kill-never-buffer-names' and
;; `clean-buffer-list-kill-never-regexps'.

(eval-when-compile (require 'cl))

(defgroup midnight nil
  "Run something every day at midnight."
  :group 'calendar)

(defcustom midnight-mode t
  "*Non-nil means run `midnight-hook' at midnight.
Setting this variable outside customize has no effect;
call `cancel-timer' or `timer-activate' on `midnight-timer' instead."
  :type 'boolean
  :group 'midnight
  :require 'midnight
  :version "20.3"
  :set (lambda (symb val)
         (set symb val) (require 'midnight)
         (if val (timer-activate midnight-timer)
             (cancel-timer midnight-timer))))

;;; time conversion

(defun float-time (&optional tm)
  "Convert `current-time' to a float number of seconds."
  (multiple-value-bind (s0 s1 s2) (or tm (current-time))
    (+ (* (float (ash 1 16)) s0) (float s1) (* 0.0000001 s2))))

(defun time-float (num)
  "Convert the float number of seconds since epoch to the list of 3 integers."
  (let* ((div (ash 1 16)) (1st (floor num div)))
    (list 1st (floor (- num (* (float div) 1st)))
          (round (* 10000000 (mod num 1))))))

(defun buffer-display-time (&optional buf)
  "Return the time-stamp of the given buffer, or current buffer, as float."
  (save-excursion
    (set-buffer (or buf (current-buffer)))
    (when buffer-display-time (float-time buffer-display-time))))

;;; clean-buffer-list stuff

(defcustom clean-buffer-list-delay-general 3
  "*The number of days before any buffer becomes eligible for autokilling.
The autokilling is done by `clean-buffer-list' when is it in `midnight-hook'.
Currently displayed and/or modified (unsaved) buffers, as well as buffers
matching `clean-buffer-list-kill-never-buffer-names' and
`clean-buffer-list-kill-never-regexps' are excluded."
  :type 'integer
  :group 'midnight
  :version "20.3")

(defcustom clean-buffer-list-delay-special 3600
  "*The number of seconds before some buffers become eligible for autokilling.
Buffers matched by `clean-buffer-list-kill-regexps' and
`clean-buffer-list-kill-buffer-names' are killed if they were last
displayed more than this many seconds ago."
  :type 'integer
  :group 'midnight
  :version "20.3")

(defcustom clean-buffer-list-kill-regexps nil
  "*List of regexps saying which buffers will be killed at midnight.
If buffer name matches a regexp in the list and the buffer was not displayed
in the last `clean-buffer-list-delay-special' seconds, it is killed by
`clean-buffer-list' when is it in `midnight-hook'.
If a member of the list is a cons, it's `car' is the regexp and its `cdr' is
the number of seconds to use instead of `clean-buffer-list-delay-special'.
See also `clean-buffer-list-kill-buffer-names',
`clean-buffer-list-kill-never-regexps' and
`clean-buffer-list-kill-never-buffer-names'."
  :type 'list
  :group 'midnight
  :version "20.3")

(defcustom clean-buffer-list-kill-buffer-names
    '("*Help*" "*Apropos*" "*Man " "*Buffer List*" "*Compile-Log*" "*info*")
  "*List of strings saying which buffers will be killed at midnight.
Buffers with names in this list, which were not displayed in the last
`clean-buffer-list-delay-special' seconds, are killed by `clean-buffer-list'
when is it in `midnight-hook'.
If a member of the list is a cons, it's `car' is the name and its `cdr' is
the number of seconds to use instead of `clean-buffer-list-delay-special'.
See also `clean-buffer-list-kill-regexps',
`clean-buffer-list-kill-never-regexps' and
`clean-buffer-list-kill-never-buffer-names'."
  :type 'list
  :group 'midnight
  :version "20.3")

(defcustom clean-buffer-list-kill-never-buffer-names
    '("*scratch*" "*Messages*")
  "*List of buffer names which will never be killed by `clean-buffer-list'.
See also `clean-buffer-list-kill-never-regexps'.
Note that this does override `clean-buffer-list-kill-regexps' and
`clean-buffer-list-kill-buffer-names' so a buffer matching any of these
two lists will NOT be killed if it is also present in this list."
  :type 'list
  :group 'midnight
  :version "20.3")

(defcustom clean-buffer-list-kill-never-regexps '("^ \*Minibuf-.*\*$")
  "*List of regexp saying which buffers will never be killed at midnight.
See also `clean-buffer-list-kill-never-buffer-names'.
Killing is done by `clean-buffer-list'.
Note that this does override `clean-buffer-list-kill-regexps' and
`clean-buffer-list-kill-buffer-names' so a buffer matching any of these
two lists will NOT be killed if it also matches anything in this list."
  :type 'list
  :group 'midnight
  :version "20.3")

(defun midnight-find (el ls test &optional key)
  "A stopgap solution to the absence of `find' in ELisp."
  (if (fboundp 'find)
      (find el ls :test test :key (or key 'eql))
      (loop for rr in ls when (funcall test el (if key (funcall key rr) rr))
            return rr)))

(defun clean-buffer-list-delay (bn)
  "Return the delay, in seconds, before this buffer name is auto-killed.
Uses `clean-buffer-list-kill-buffer-names', `clean-buffer-list-kill-regexps'
`clean-buffer-list-delay-general' and `clean-buffer-list-delay-special'.
Autokilling is done by `clean-buffer-list'."
  (flet ((ff (ls ts)
           (let ((zz (midnight-find
                      bn ls ts (lambda (xx) (if (consp xx) (car xx) xx)))))
             (cond ((consp zz) (cdr zz))
                   ((null zz) nil)
                   (clean-buffer-list-delay-special)))))
    (or (ff clean-buffer-list-kill-buffer-names 'string=)
        (ff clean-buffer-list-kill-regexps 'string-match)
        (* clean-buffer-list-delay-general 24 60 60))))

(defun clean-buffer-list ()
  "Kill old buffers.
The relevant vartiables are `clean-buffer-list-delay-general',
`clean-buffer-list-delay-special', `clean-buffer-list-kill-buffer-names',
`clean-buffer-list-kill-never-buffer-names',
`clean-buffer-list-kill-regexps' and `clean-buffer-list-kill-never-regexps'."
  (interactive)
  (let ((tm (float-time)) bts (ts (format-time-string "%Y-%m-%d %T")) bn)
    (dolist (buf (buffer-list))
      (message "[%s] processing `%s'..." ts buf)
      (setq bts (buffer-display-time buf) bn (buffer-name buf))
      (unless (or ;; (string-match clean-buffer-list-kill-never bn)
                  (midnight-find bn clean-buffer-list-kill-never-regexps
                                 'string-match)
                  (midnight-find bn clean-buffer-list-kill-never-buffer-names
                                 'string-equal)
                  (buffer-modified-p buf) (get-buffer-window buf 'visible)
                  (null bts) (< (- tm bts) (clean-buffer-list-delay bn)))
        (message "[%s] killing `%s'" ts bn)
        (kill-buffer buf)))))

;;; midnight hook

(defvar midnight-period (* 24 60 60)
  "The number of seconds in a day - the delta for `midnight-timer'.")

(defcustom midnight-hook 'clean-buffer-list
  "The hook run `midnight-delay' seconds after midnight every day.
The default value is `clean-buffer-list'."
  :type 'hook
  :group 'midnight
  :version "20.3")

(defun midnight-next ()
  "Return the number of seconds till the next midnight."
  (multiple-value-bind (sec min hrs) (decode-time)
    (- (* 24 60 60) (* 60 60 hrs) (* 60 min) sec)))

(defvar midnight-timer nil
  "Timer running the `midnight-hook' `midnight-delay' seconds after midnight.
Use `cancel-timer' to stop it and `midnight-delay-set' to change
the time when it is run.")

;;;###autoload
(defun midnight-delay-set (symb tm)
  "Modify `midnight-timer' according to `midnight-delay'.
Sets the first argument (which must be symbol `midnight-delay')
to its second argument."
  (unless (eq symb 'midnight-delay)
    (error "Illegal argument to `midnight-delay-set': `%s'" symb))
  (set symb tm)
  (when (timerp midnight-timer) (cancel-timer midnight-timer))
  (setq midnight-timer
        (run-at-time (if (numberp tm) (+ (midnight-next) tm) tm)
                     midnight-period 'midnight-timer-function)))

(defun midnight-timer-function ()
  "This is the function run by the `midnight-mode' timer once each day."
  (when midnight-mode
    (run-hooks 'midnight-hook)))

(defcustom midnight-delay 3600
  "*The number of seconds after the midnight when the `midnight-timer' is run.
You should set this variable before loading midnight.el, or
set it by calling `midnight-delay-set', or use `custom'.
If you wish, you can use a string instead, it will be passed as the
first argument to `run-at-time'."
  :type 'sexp
  :set 'midnight-delay-set
  :group 'midnight
  :version "20.3")

(provide 'midnight)

;;; midnight.el ends here
