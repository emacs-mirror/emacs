;;; rlogin.el --- remote login interface

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

(defvar rlogin-program "rlogin"
  "*Name of program to invoke rlogin")

(defvar rlogin-explicit-args nil
  "*List of arguments to pass to rlogin on the command line.")

(defvar rlogin-mode-hook nil
  "*Hooks to run after setting current buffer to rlogin-mode.")

;; I think this is so obnoxious I refuse to enable it by default. 
;; In any case, there is a bug with regards to generating a quit while
;; reading keyboard input in a process filter, so until that's fixed it's
;; not safe to enable this anyway. 
(defvar rlogin-password-paranoia nil
  "*If non-`nil', query user for a password in the minibuffer when a
Password: prompt appears.  Stars will echo as characters are type. 

It's also possible to selectively enter passwords without echoing them in
the minibuffer using the function `rlogin-password'.")

(defvar rlogin-last-input-line nil nil)

;; Initialize rlogin mode map.
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
  (let* ((buffer-name (format "*rlogin-%s*" host))
         (args (if (and rlogin-explicit-args (listp rlogin-explicit-args))
                   (cons host rlogin-explicit-args)
                 (list host))))
    (and prefix (setq buffer-name 
                      (buffer-name (generate-new-buffer buffer-name))))
    (switch-to-buffer buffer-name)
    (or (comint-check-proc buffer-name)
        (progn
          (comint-mode)
          (comint-exec (current-buffer) buffer-name rlogin-program nil args)
          (setq proc (get-process buffer-name))
          (set-marker (process-mark proc) (point-min))
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
          (while (string-match "[ \t]*\\([^ \t]\\)+$" args)
            (setq rlogin-explicit-args 
                  (cons (substring args 
                                   (match-beginning 1)
                                   (match-end 1))
                        rlogin-explicit-args)
                  args (substring args 0 (match-beginning 0)))))
      (store-match-data old-match-data))
    (rlogin 1 host)))

;;;###autoload
(defun rlogin-password ()
  "Play the paranoia game by not echoing entered password in buffer 
(stars will echo in the minibuffer instead."
  (interactive)
  (let ((input (comint-read-noecho "Enter password: " 'stars)))
    (insert-before-markers "\n")
    (comint-send-string (get-buffer-process (current-buffer))
                        (format "%s\n" input))))

;;;###autoload
(defun rlogin-mode ()
  (interactive)
  (kill-all-local-variables)
  (comint-mode)
  (setq comint-prompt-regexp shell-prompt-pattern)
  (setq major-mode 'rlogin-mode)
  (setq mode-name "rlogin")
  (use-local-map rlogin-mode-map)
  (run-hooks 'rlogin-mode-hook))

(defun rlogin-filter (proc string)
  (let ((old-buffer (current-buffer))
        (old-match-data (match-data))
        at-max-pos
        moving)
    (unwind-protect
        (progn
          (set-buffer (process-buffer proc))
          (setq moving (= (point) (process-mark proc)))
          (save-excursion
            (goto-char (process-mark proc))
            (save-restriction
              (let ((beg (point)))
                (insert-before-markers string)
                (narrow-to-region beg (point))
                (goto-char (point-min))
                (while (search-forward "\C-m" nil t)
                  (delete-char -1))
                (and rlogin-password-paranoia
                     (setq string (buffer-substring (point-min) (point-max))))
                (goto-char (point-max))))
            (set-marker (process-mark proc) (point)))
          (and moving 
               (goto-char (process-mark proc))))
      (set-buffer old-buffer)
      (store-match-data old-match-data)))
  (and rlogin-password-paranoia 
       (string= "Password:" string)
       (let ((input (comint-read-noecho "Enter password: " 'stars)))
         (and input
              (progn
                (insert-before-markers "\n")
                (comint-send-string proc (format "%s\n" input)))))))

(defun rlogin-send-Ctrl-C ()
  (interactive)
  (send-string nil "\C-c"))

(defun rlogin-send-Ctrl-Z ()
  (interactive)
  (send-string nil "\C-z"))

(defun rlogin-send-Ctrl-backslash ()
  (interactive)
  (send-string nil "\C-\\"))

(defun rlogin-delchar-or-send-Ctrl-D (arg)
  "Delete ARG characters forward, or send a C-d to process if at end of
buffer."  
  (interactive "p") 
  (if (eobp)
      (send-string nil "\C-d")
    (delete-char arg)))

;;; rlogin.el ends here
