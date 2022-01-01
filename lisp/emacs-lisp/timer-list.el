;;; timer-list.el --- list active timers in a buffer  -*- lexical-binding:t -*-

;; Copyright (C) 2016-2022 Free Software Foundation, Inc.

;; Maintainer: emacs-devel@gnu.org
;; Package: emacs

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

(defvar cl-print-compiled)
(defvar cl-print-compiled-button)

;;;###autoload
(defun list-timers (&optional _ignore-auto _nonconfirm)
  "List all timers in a buffer."
  (interactive)
  (pop-to-buffer-same-window (get-buffer-create "*timer-list*"))
  (timer-list-mode)
  (tabulated-list-init-header)
  (setq tabulated-list-entries
        (mapcar
         (lambda (timer)
           (list
            nil
            `[ ;; Idle.
              ,(propertize
                (if (aref timer 7) "   *" " ")
                'help-echo "* marks idle timers"
                'timer timer)
              ;; Next time.
              ,(propertize
                (let ((time (list (aref timer 1)
				  (aref timer 2)
				  (aref timer 3))))
                  (format "%12s"
                          (format-seconds "%dd %hh %mm %z%,1ss"
			                  (float-time
			                   (if (aref timer 7)
			                       time
			                     (time-subtract time nil))))))
                'help-echo "Time until next invocation")
              ;; Repeat.
              ,(let ((repeat (aref timer 4)))
                 (cond
                  ((numberp repeat)
                   (propertize
                    (format "%12s" (format-seconds
                                    "%dd %hh %mm %z%,1ss" repeat))
                    'help-echo "Repeat interval"))
                  ((null repeat)
                   (propertize "           -" 'help-echo "Runs once"))
                  (t
                   (format "%12s" repeat))))
              ;; Function.
              ,(propertize
                (let ((cl-print-compiled 'static)
                      (cl-print-compiled-button nil)
                      (print-escape-newlines t))
                  (cl-prin1-to-string (aref timer 5)))
                'help-echo "Function called by timer")]))
         (append timer-list timer-idle-list)))
  (tabulated-list-print))
;; This command can be destructive if they don't know what they are
;; doing.  Kids, don't try this at home!
;;;###autoload (put 'list-timers 'disabled "Beware: manually canceling timers can ruin your Emacs session.")

(defvar timer-list-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "c" 'timer-list-cancel)
    (easy-menu-define nil map ""
      '("Timers"
	["Cancel" timer-list-cancel t]))
    map))

(define-derived-mode timer-list-mode tabulated-list-mode "Timer-List"
  "Mode for listing and controlling timers."
  (buffer-disable-undo)
  (setq-local revert-buffer-function #'list-timers)
  (setq tabulated-list-format
        '[("Idle" 6 timer-list--idle-predicate)
          ("Next" 12 timer-list--next-predicate :right-align t :pad-right 1)
          ("Repeat" 12 timer-list--repeat-predicate :right-align t :pad-right 1)
          ("Function" 10 timer-list--function-predicate)]))

(defun timer-list--idle-predicate (A B)
  "Predicate to sort Timer-List by the Idle column."
  (let ((iA (aref (cadr A) 0))
        (iB (aref (cadr B) 0)))
    (cond ((string= iA iB)
           (timer-list--next-predicate A B))
          ((string= iA "   *") nil)
          (t t))))

(defun timer-list--next-predicate (A B)
  "Predicate to sort Timer-List by the Next column."
  (let ((nA (string-to-number (aref (cadr A) 1)))
        (nB (string-to-number (aref (cadr B) 1))))
    (< nA nB)))

(defun timer-list--repeat-predicate (A B)
  "Predicate to sort Timer-List by the Repeat column."
  (let ((rA (aref (cadr A) 2))
        (rB (aref (cadr B) 2)))
    (string< rA rB)))

(defun timer-list--function-predicate (A B)
  "Predicate to sort Timer-List by the Function column."
  (let ((fA (aref (cadr A) 3))
        (fB (aref (cadr B) 3)))
    (string< fA fB)))

(defun timer-list-cancel ()
  "Cancel the timer on the line under point."
  (interactive)
  (let ((timer (get-text-property (line-beginning-position) 'timer))
        (inhibit-read-only t))
    (unless timer
      (error "No timer on the current line"))
    (cancel-timer timer)
    (delete-region (line-beginning-position)
                   (line-beginning-position 2))))

(provide 'timer-list)

;;; timer-list.el ends here
