;;; timer-list.el --- list active timers in a buffer

;; Copyright (C) 2016-2017 Free Software Foundation, Inc.

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

;;;###autoload
(defun list-timers (&optional _ignore-auto _nonconfirm)
  "List all timers in a buffer."
  (interactive)
  (pop-to-buffer-same-window (get-buffer-create "*timer-list*"))
  (let ((inhibit-read-only t))
    (erase-buffer)
    (timer-list-mode)
    (dolist (timer (append timer-list timer-idle-list))
      (insert (format "%4s %10s %8s %s"
                      ;; Idle.
                      (if (aref timer 7) "*" " ")
                      ;; Next time.
                      (let ((time (float-time (list (aref timer 1)
                                                    (aref timer 2)
                                                    (aref timer 3)))))
                        (format "%.2f"
                                (if (aref timer 7)
                                    time
                                  (- (float-time (list (aref timer 1)
                                                       (aref timer 2)
                                                       (aref timer 3)))
                                     (float-time)))))
                      ;; Repeat.
                      (let ((repeat (aref timer 4)))
                        (cond
                         ((numberp repeat)
                          (format "%.2f" (/ repeat 60)))
                         ((null repeat)
                          "-")
                         (t
                          (format "%s" repeat))))
                      ;; Function.
                      (let ((cl-print-compiled 'static)
                            (cl-print-compiled-button nil))
                        (cl-prin1-to-string (aref timer 5)))))
      (put-text-property (line-beginning-position)
                         (1+ (line-beginning-position))
                         'timer timer)
      (insert "\n")))
  (goto-char (point-min)))
;; This command can be destructive if they don't know what they are
;; doing.  Kids, don't try this at home!
;;;###autoload (put 'list-timers 'disabled "Beware: manually canceling timers can ruin your Emacs session.")

(defvar timer-list-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "c" 'timer-list-cancel)
    (define-key map "n" 'next-line)
    (define-key map "p" 'previous-line)
    (easy-menu-define nil map ""
      '("Timers"
	["Cancel" timer-list-cancel t]))
    map))

(define-derived-mode timer-list-mode special-mode "Timer-List"
  "Mode for listing and controlling timers."
  (setq bidi-paragraph-direction 'left-to-right)
  (setq truncate-lines t)
  (buffer-disable-undo)
  (setq-local revert-buffer-function #'list-timers)
  (setq buffer-read-only t)
  (setq header-line-format
        (format "%4s %10s %8s %s"
                "Idle" "Next" "Repeat" "Function")))

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
