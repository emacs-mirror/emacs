;;; telnet.el --- run a telnet session from within an Emacs buffer

;;; Copyright (C) 1985, 1988, 1992, 1994 Free Software Foundation, Inc.

;; Author: William F. Schelter
;; Maintainer: FSF

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
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; This mode is intended to be used for telnet or rsh to a remode host;
;; `telnet' and `rsh' are the two entry points.  Multiple telnet or rsh
;; sessions are supported.
;;
;; Normally, input is sent to the remote telnet/rsh line-by-line, as you
;; type RET or LFD.  C-c C-c sends a C-c to the remote immediately; 
;; C-c C-z sends C-z immediately.  C-c C-q followed by any character
;; sends that character immediately.
;;
;; All RET characters are filtered out of the output coming back from the
;; remote system.  The mode tries to do other useful translations based
;; on what it sees coming back from the other system before the password
;; query.  It knows about UNIX, ITS, TOPS-20 and Explorer systems.

;;; Code:

;; to do fix software types for lispm:
;; to eval current expression.  Also to try to send escape keys correctly.
;; essentially we'll want the rubout-handler off.

;; filter is simplistic but should be okay for typical shell usage.
;; needs hacking if it is going to deal with asynchronous output in a sane
;; manner

(require 'comint)

(defvar telnet-new-line "\r")
(defvar telnet-mode-map nil)
(defvar telnet-prompt-pattern "^[^#$%>\n]*[#$%>] *")
(defvar telnet-replace-c-g nil)
(make-variable-buffer-local
 (defvar telnet-remote-echoes t
   "True if the telnet process will echo input."))
(make-variable-buffer-local
 (defvar telnet-interrupt-string "\C-c" "String sent by C-c."))

(defvar telnet-count 0
  "Number of output strings from telnet process while looking for password.")
(make-variable-buffer-local 'telnet-count)

(defvar telnet-program "telnet"
  "Program to run to open a telnet connection.")

(defvar telnet-initial-count -50
  "Initial value of `telnet-count'.  Should be set to the negative of the
number of terminal writes telnet will make setting up the host connection.")

(defvar telnet-maximum-count 4
  "Maximum value `telnet-count' can have.
After this many passes, we stop looking for initial setup data.
Should be set to the number of terminal writes telnet will make
rejecting one login and prompting again for a username and password.")

(defun telnet-interrupt-subjob ()
  (interactive)
  "Interrupt the program running through telnet on the remote host."
  (send-string nil telnet-interrupt-string))

(defun telnet-c-z ()
  (interactive)
  (send-string nil "\C-z"))

(defun send-process-next-char ()
  (interactive)
  (send-string nil
	       (char-to-string
		(let ((inhibit-quit t))
		  (prog1 (read-char)
		    (setq quit-flag nil))))))

; initialization on first load.
(if telnet-mode-map
    nil
  (setq telnet-mode-map (nconc (make-sparse-keymap) comint-mode-map))
  (define-key telnet-mode-map "\C-m" 'telnet-send-input)
;  (define-key telnet-mode-map "\C-j" 'telnet-send-input)
  (define-key telnet-mode-map "\C-c\C-q" 'send-process-next-char)
  (define-key telnet-mode-map "\C-c\C-c" 'telnet-interrupt-subjob) 
  (define-key telnet-mode-map "\C-c\C-z" 'telnet-c-z))

;;maybe should have a flag for when have found type
(defun telnet-check-software-type-initialize (string)
  "Tries to put correct initializations in.  Needs work."
  (let ((case-fold-search t))
    (cond ((string-match "unix" string)
	 (setq telnet-prompt-pattern comint-prompt-regexp)
	 (setq telnet-new-line "\n"))
	((string-match "tops-20" string) ;;maybe add telnet-replace-c-g
	 (setq telnet-prompt-pattern  "[@>]*"))
	((string-match "its" string)
	 (setq telnet-prompt-pattern  "^[^*>\n]*[*>] *"))
	((string-match "explorer" string)  ;;explorer telnet needs work
	 (setq telnet-replace-c-g ?\n))))
  (setq comint-prompt-regexp telnet-prompt-pattern))

(defun telnet-initial-filter (proc string)
  ;For reading up to and including password; also will get machine type.
  (cond ((string-match "No such host" string)
	 (kill-buffer (process-buffer proc))
	 (error "No such host."))
	((string-match "passw" string)
	 (telnet-filter proc string)
	 (setq telnet-count 0)
	 (send-string proc (concat (comint-read-noecho "Password: " t)
				   telnet-new-line)))
	(t (telnet-check-software-type-initialize string)
	   (telnet-filter proc string)
	   (cond ((> telnet-count telnet-maximum-count)
		  (set-process-filter proc 'telnet-filter))
		 (t (setq telnet-count (1+ telnet-count)))))))

;; Identical to comint-simple-send, except that it sends telnet-new-line
;; instead of "\n".
(defun telnet-simple-send (proc string)
  (comint-send-string proc string)
  (comint-send-string proc telnet-new-line))

(defun telnet-filter (proc string)
  (save-excursion
    (set-buffer (process-buffer proc))
    (let* ((last-insertion (marker-position (process-mark proc)))
	   (delta (- (point) last-insertion))
	   (ie (and comint-last-input-end
		    (marker-position comint-last-input-end)))
	   (w (get-buffer-window (current-buffer)))
	   (ws (and w (window-start w))))
      (goto-char last-insertion)
      (insert-before-markers string)
      (set-marker (process-mark proc) (point))
      (if ws (set-window-start w ws t))
      (if ie (set-marker comint-last-input-end ie))
      (while (progn (skip-chars-backward "^\C-m" last-insertion)
		    (> (point) last-insertion))
	(delete-region (1- (point)) (point)))
      (goto-char (process-mark proc))
      (and telnet-replace-c-g
	   (subst-char-in-region last-insertion (point) ?\C-g
				 telnet-replace-c-g t))
      ;; If point is after the insertion place, move it
      ;; along with the text.
      (if (> delta 0)
	  (goto-char (+ (process-mark proc) delta))))))

(defun telnet-send-input ()
  (interactive)
;  (comint-send-input telnet-new-line telnet-remote-echoes)
  (comint-send-input)
  (if telnet-remote-echoes
      (delete-region comint-last-input-start
		     comint-last-input-end)))

;;;###autoload
(defun telnet (host)
  "Open a network login connection to host named HOST (a string).
Communication with HOST is recorded in a buffer `*telnet-HOST*'.
Normally input is edited in Emacs and sent a line at a time."
  (interactive "sOpen telnet connection to host: ")
  (let* ((comint-delimiter-argument-list '(?\  ?\t))
         (name (concat "telnet-" (comint-arguments host 0 nil) ))
	 (buffer (get-buffer (concat "*" name "*")))
	 process)
    (if (and buffer (get-buffer-process buffer))
	(switch-to-buffer (concat "*" name "*"))
      (switch-to-buffer (make-comint name telnet-program))
      (setq process (get-buffer-process (current-buffer)))
      (set-process-filter process 'telnet-initial-filter)
      ;; Don't send the `open' cmd till telnet is ready for it.
      (accept-process-output process)
      (erase-buffer)
      (send-string process (concat "open " host "\n"))
      (telnet-mode)
      (setq comint-input-sender 'telnet-simple-send)
      (setq telnet-count telnet-initial-count))))

(defun telnet-mode ()
  "This mode is for using telnet (or rsh) from a buffer to another host.
It has most of the same commands as comint-mode.
There is a variable ``telnet-interrupt-string'' which is the character
sent to try to stop execution of a job on the remote host.
Data is sent to the remote host when RET is typed.

\\{telnet-mode-map}
"
  (interactive)
  (comint-mode)
  (setq major-mode 'telnet-mode
	mode-name "Telnet"
	comint-prompt-regexp telnet-prompt-pattern)
  (use-local-map telnet-mode-map)
  (run-hooks 'telnet-mode-hook))

;;;###autoload
(defun rsh (host)
  "Open a network login connection to host named HOST (a string).
Communication with HOST is recorded in a buffer *HOST-rsh*.
Normally input is edited in Emacs and sent a line at a time."
  (interactive "sOpen rsh connection to host: ")
  (require 'shell)
  (let ((name (concat host "-rsh" )))
    (switch-to-buffer (make-comint name remote-shell-program nil host))
    (set-process-filter (get-process name) 'telnet-initial-filter)
    (telnet-mode)
    (setq telnet-count -16)))

(provide 'telnet)

;;; telnet.el ends here
