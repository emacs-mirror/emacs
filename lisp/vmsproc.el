;; -*- no-byte-compile: t -*-
;;; vmsproc.el --- run asynchronous VMS subprocesses under Emacs

;; Copyright (C) 1986, 2001, 2002, 2003, 2004, 2005,
;;   2006, 2007, 2008 Free Software Foundation, Inc.

;; Author: Mukesh Prasad
;; Maintainer: FSF
;; Keywords: vms

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
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(defvar display-subprocess-window nil
  "If non-nil, the subprocess window is displayed whenever input is received.")

(defvar command-prefix-string "$ "
  "String to insert to distinguish commands entered by user.")

(defvar subprocess-running nil)
(defvar subprocess-buf nil)

(defvar command-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-m" 'command-send-input)
    (define-key map "\C-u" 'command-kill-line)
    map))

(defun subprocess-input (name str)
  "Handle input from a subprocess.  Called by Emacs."
  (if display-subprocess-window
      (display-buffer subprocess-buf))
  (with-current-buffer subprocess-buf
    (goto-char (point-max))
    (insert str ?\n)))

(defun subprocess-exit (name)
  "Called by Emacs upon subprocess exit."
  (setq subprocess-running nil))

(defun start-subprocess ()
  "Spawn an asynchronous subprocess with output redirected to
the buffer *COMMAND*.  Within this buffer, use C-m to send
the last line to the subprocess or to bring another line to
the end."
  (if subprocess-running
      (return t))
  (setq subprocess-buf (get-buffer-create "*COMMAND*"))
  (with-current-buffer subprocess-buf
    (use-local-map command-mode-map))
  (setq subprocess-running (spawn-subprocess 1 'subprocess-input
					     'subprocess-exit))
  ;; Initialize subprocess so it doesn't panic and die upon
  ;; encountering the first error.
  (and subprocess-running
       (send-command-to-subprocess 1 "ON SEVERE_ERROR THEN CONTINUE")))

(defun subprocess-command-to-buffer (command buffer)
  "Execute COMMAND and redirect output into BUFFER."
  (let (cmd args)
    (setq cmd (substring command 0 (string-match " " command)))
    (setq args (substring command (string-match " " command)))
    (call-process cmd nil buffer nil "*dcl*" args)))
  ;; BUGS: only the output up to the end of the first image activation is trapped.
  ;; (if (not subprocess-running)
  ;;     (start-subprocess))
  ;; (with-current-buffer buffer
  ;;   (let ((output-filename (concat "SYS$SCRATCH:OUTPUT-FOR-"
  ;;       			   (getenv "USER") ".LISTING")))
  ;;     (while (file-exists-p output-filename)
  ;;       (delete-file output-filename))
  ;;     (define-logical-name "SYS$OUTPUT" (concat output-filename "-NEW"))
  ;;     (send-command-to-subprocess 1 command)
  ;;     (send-command-to-subprocess 1 (concat
  ;;       			     "RENAME " output-filename
  ;;       			     "-NEW " output-filename))
  ;;     (while (not (file-exists-p output-filename))
  ;;       (sleep-for 1))
  ;;     (define-logical-name "SYS$OUTPUT" nil)
  ;;     (insert-file output-filename)
  ;;     (delete-file output-filename))))

(defun subprocess-command ()
  "Start asynchronous subprocess if not running and switch to its window."
  (interactive)
  (if (not subprocess-running)
      (start-subprocess))
  (and subprocess-running
       (progn (pop-to-buffer subprocess-buf) (goto-char (point-max)))))

(defun command-send-input ()
  "If at last line of buffer, send the current line to
the spawned subprocess.  Otherwise bring back current
line to the last line for resubmission."
  (interactive)
  (beginning-of-line)
  (let ((current-line (buffer-substring (point) (line-end-position))))
    (if (eobp)
	(progn
	  (if (not subprocess-running)
	      (start-subprocess))
	  (if subprocess-running
	      (progn
		(beginning-of-line)
		(send-command-to-subprocess 1 current-line)
		(if command-prefix-string
		    (progn (beginning-of-line) (insert command-prefix-string)))
		(forward-line 1))))
      ;; else -- if not at last line in buffer
      (goto-char (point-max))
      (backward-char)
      (forward-line 1)
      (insert
       (if (compare-strings command-prefix-string nil nil
                            current-line 0 (length command-prefix-string))
           (substring current-line (length command-prefix-string))
         current-line)))))

(defun command-kill-line ()
  "Kill the current line.  Used in command mode."
  (interactive)
  (beginning-of-line)
  (kill-line))

(define-key esc-map "$" 'subprocess-command)

(provide 'vmsproc)

;; arch-tag: 600b2512-f903-4887-bcd2-e76b306f5b66
;;; vmsproc.el ends here
