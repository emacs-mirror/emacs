;;; rlogin.el --- remote login interface

;; Author: Noah Friedman
;; Maintainer: Noah Friedman <friedman@prep.ai.mit.edu>
;; Keywords: unix, comm

;; Copyright (C) 1992, 1993 Free Software Foundation, Inc.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to: The Free Software Foundation,
;; Inc.; 675 Massachusetts Avenue.; Cambridge, MA 02139, USA.

;;; Commentary:

;; Support for remote logins using `rlogin'.
;;
;; Todo: add directory tracking using ange-ftp style patchnames for the cwd.

;;; Code:

(require 'comint)

;;;###autoload
(defvar rlogin-program "rlogin"
  "*Name of program to invoke rlogin")

;;;###autoload
(defvar rlogin-explicit-args nil
  "*List of arguments to pass to rlogin on the command line.")

;;;###autoload
(defvar rlogin-mode-hook nil
  "*Hooks to run after setting current buffer to rlogin-mode.")

;;;###autoload
(defvar rlogin-process-connection-type nil
  "*If non-`nil', use a pty for the local rlogin process.  
If `nil', use a pipe (if pipes are supported on the local system).  

Generally it is better not to waste ptys on systems which have a static
number of them.  On the other hand, some implementations of `rlogin' assume
a pty is being used, and errors will result from using a pipe instead.")

;; Leave this nil because it makes rlogin-filter a tiny bit faster.  Plus
;; you can still call rlogin-password by hand.
;;;###autoload
(defvar rlogin-password-paranoia nil
  "*If non-`nil', query user for a password in the minibuffer when a Password: prompt appears.
It's also possible to selectively enter passwords without echoing them in
the minibuffer using the command `rlogin-password' explicitly.")

;; Initialize rlogin mode map.
;;;###autoload
(defvar rlogin-mode-map '())
(cond ((not rlogin-mode-map)
       (setq rlogin-mode-map (full-copy-sparse-keymap comint-mode-map))
       ;(define-key rlogin-mode-map "\M-\t" 'comint-dynamic-complete)
       ;(define-key rlogin-mode-map "\M-?"  'comint-dynamic-list-completions)
       (define-key rlogin-mode-map "\C-c\C-c" 'rlogin-send-Ctrl-C)
       (define-key rlogin-mode-map "\C-c\C-z" 'rlogin-send-Ctrl-Z)
       (define-key rlogin-mode-map "\C-c\C-\\" 'rlogin-send-Ctrl-backslash)
       (define-key rlogin-mode-map "\C-d" 'rlogin-delchar-or-send-Ctrl-D)))

;;;###autoload
(defun rlogin (&optional prefix host)
  "Open a network login connection to HOST via the `rlogin' program.
Input is sent line-at-a-time to the remote connection.

Communication with HOST is recorded in a buffer *rlogin-HOST*.
If a prefix argument is given and the buffer *rlogin-HOST* already exists,
a new buffer with a different connection will be made. 

The variable `rlogin-program' contains the name of the actual program to
run.  It can be a relative or absolute path. 

The variable `rlogin-explicit-args' is a list of arguments to give to
the rlogin when starting."
  (interactive (list current-prefix-arg
                     (read-from-minibuffer "Open rlogin connection to host: ")))
  (let* ((process-connection-type rlogin-process-connection-type)
         (buffer-name (format "*rlogin-%s*" host))
         (args (if (and rlogin-explicit-args (listp rlogin-explicit-args))
                   (cons host rlogin-explicit-args)
                 (list host)))
	 proc)
    (and prefix (setq buffer-name 
                      (buffer-name (generate-new-buffer buffer-name))))
    (switch-to-buffer buffer-name)
    (or (comint-check-proc buffer-name)
        (progn
          (comint-mode)
          (comint-exec (current-buffer) buffer-name rlogin-program nil args)
          (setq proc (get-process buffer-name))
          ;; Set process-mark to point-max in case there is text in the
          ;; buffer from a previous exited process.
          (set-marker (process-mark proc) (point-max))
          (set-process-filter proc 'rlogin-filter)
          (rlogin-mode)))))

;;;###autoload
(defun rlogin-with-args (host args)
  "Open a new rlogin connection to HOST, even if one already exists. 
String ARGS is given as arguments to the `rlogin' program, overriding the
value of `rlogin-explicit-args'."
  (interactive (list (read-from-minibuffer "Open rlogin connection to host: ")
                     (read-from-minibuffer "with arguments: ")))
  (let ((old-match-data (match-data))
        (rlogin-explicit-args nil))
    (unwind-protect
        (progn
          (while (string-match "[ \t]*\\([^ \t]+\\)$" args)
            (setq rlogin-explicit-args 
                  (cons (substring args 
                                   (match-beginning 1)
                                   (match-end 1))
                        rlogin-explicit-args)
                  args (substring args 0 (match-beginning 0)))))
      (store-match-data old-match-data))
    (rlogin 1 host)))

;;;###autoload
(defun rlogin-password (&optional proc)
  "Read a password and send it to an rlogin session.
For each character typed, a `*' is echoed in the minibuffer.
End with RET, LFD, or ESC.  DEL or C-h rubs out.  C-u kills line.
C-g aborts attempt to read and send password. 

Optional argument PROC is the process to which the password should be sent.
If not provided, send to the process in the current buffer.  This argument
is intended primarily for use by `rlogin-filter'."
  (interactive)
  (or proc (setq proc (get-buffer-process (current-buffer))))
  (let* ((buffer-name (buffer-name (process-buffer proc)))
         (pass (comint-read-noecho (format "Password for buffer \"%s\": " 
                                           buffer-name)
                                   'stars)))
    (and pass
         (save-excursion
           (set-buffer buffer-name)
           (insert-before-markers "\n")
           (comint-send-string proc (format "%s\n" pass))))))

;;;###autoload
(defun rlogin-mode ()
  "Set major-mode for rlogin sessions. 
If `rlogin-mode-hook' is set, run it."
  (interactive)
  (kill-all-local-variables)
  (comint-mode)
  (setq comint-prompt-regexp shell-prompt-pattern)
  (setq major-mode 'rlogin-mode)
  (setq mode-name "rlogin")
  (use-local-map rlogin-mode-map)
  (run-hooks 'rlogin-mode-hook))


(defun rlogin-filter (proc string)
  (save-excursion
    (set-buffer (process-buffer proc))
    (let ((proc-mark (process-mark proc))
          (region-begin (point)))
      (goto-char proc-mark)
      (insert-before-markers string)
      (goto-char region-begin)
      (while (search-forward "\C-m" proc-mark t)
        (delete-char -1))))
  ;; Kludgy workaround for scroll-step bug in emacs.  If point is at the
  ;; top of the window, scroll step is nonzero, and you call
  ;; insert-before-markers, the text is inserted off-screen.  If
  ;; scroll-step is 0, this doesn't happen. 
  (and (/= scroll-step 0)
       (eq (process-buffer proc) (window-buffer (selected-window)))
       (eq (point) (window-start))
       (set-window-start (selected-window) 
                         (save-excursion
                           (beginning-of-line)
                           (point)) 
                         'noforce))
  (and rlogin-password-paranoia 
       (string= "Password:" string)
       (rlogin-password proc)))

;;;###autoload
(defun rlogin-send-Ctrl-C ()
  (interactive)
  (send-string nil "\C-c"))

;;;###autoload
(defun rlogin-send-Ctrl-Z ()
  (interactive)
  (send-string nil "\C-z"))

;;;###autoload
(defun rlogin-send-Ctrl-backslash ()
  (interactive)
  (send-string nil "\C-\\"))

;;;###autoload
(defun rlogin-delchar-or-send-Ctrl-D (arg)
  "Delete ARG characters forward, or send a C-d to process if at end of
buffer."  
  (interactive "p") 
  (if (eobp)
      (send-string nil "\C-d")
    (delete-char arg)))

;;; rlogin.el ends here
