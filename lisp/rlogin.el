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
;; along with this program; if not, you can either send email to this
;; program's author (see below) or write to:
;;
;;              The Free Software Foundation, Inc.
;;              675 Massachusetts Avenue.
;;              Cambridge, MA 02139, USA. 
;;

;;; Commentary:

;; Support for remote logins using `rlogin'.
;;
;; Todo: add directory tracking using ange-ftp style patchnames for the cwd.

;;; Code:

(require 'comint)

(defvar rlogin-program "rlogin"
  "*Name of program to invoke rlogin")

(defvar rlogin-mode-hook nil
  "*Hooks to run after setting current buffer to rlogin-mode.")

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
(defun rlogin (host)
  (interactive "sOpen rlogin connection to host: ")
  (let* ((buffer-name (concat "rlogin-" host))
         (*buffer-name* (concat "*" buffer-name "*")))
    (cond ((not (comint-check-proc *buffer-name*))
           (let* ((xargs-name (intern-soft "explicit-rlogin-args"))
                  (xargs (and xargs-name (boundp xargs-name) (symbol-value xargs-name)))
                  (process-connection-type nil)
                  proc)
             (if xargs
                 (setq xargs (append xargs host))
               (setq xargs (list host)))
             (set-buffer (apply 'make-comint buffer-name rlogin-program nil xargs))
             (setq proc (get-process buffer-name))
             (set-marker (process-mark proc) (point-min))
             (set-process-filter proc 'rlogin-filter)
             (rlogin-mode))))
    (switch-to-buffer *buffer-name*)))

;;;###autoload
(defun rlogin-mode ()
  (interactive)
  (comint-mode)
  (setq comint-prompt-regexp shell-prompt-pattern)
  (setq major-mode 'rlogin-mode)
  (setq mode-name "Rlogin")
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
                (setq string (buffer-substring (point-min) (point-max)))
                (goto-char (point-max))))
            (set-marker (process-mark proc) (point)))
          (and moving 
               (goto-char (process-mark proc))))
      (set-buffer old-buffer)
      (store-match-data old-match-data))))

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

