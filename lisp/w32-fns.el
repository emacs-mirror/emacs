;;; w32-fns.el --- Lisp routines for Windows NT.

;; Copyright (C) 1994 Free Software Foundation, Inc.

;; Author: Geoff Voelker (voelker@cs.washington.edu)

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

;; (August 12, 1993)
;; Created.

;; (November 21, 1994)
;; [C-M-backspace] defined.
;; mode-line-format defined to show buffer file type.
;; audio bell initialized.

;;; Code:

;; Map delete and backspace
(define-key function-key-map [backspace] "\177")
(define-key function-key-map [delete] "\C-d")
(define-key function-key-map [M-backspace] [?\M-\177])
(define-key function-key-map [C-M-backspace] [\C-\M-delete])

;; Ignore case on file-name completion
(setq completion-ignore-case t)

;; The cmd.exe shell uses the "/c" switch instead of the "-c" switch
;; for executing its command line argument (from simple.el).
(setq shell-command-switch "/c")

;; For appending suffixes to directories and files in shell completions.
(add-hook 'shell-mode-hook 
	  '(lambda () (setq comint-completion-addsuffix '("\\" . " "))))

;;; Avoid creating auto-save file names containing invalid characters
;;; (primarily "*", eg. for the *mail* buffer).
(fset 'original-make-auto-save-file-name
      (symbol-function 'make-auto-save-file-name))

(defun make-auto-save-file-name ()
  "Return file name to use for auto-saves of current buffer.
Does not consider `auto-save-visited-file-name' as that variable is checked
before calling this function.  You can redefine this for customization.
See also `auto-save-file-name-p'."
  (let ((name (original-make-auto-save-file-name))
	(start 0))
    ;; destructively replace occurences of * or ? with $
    (while (string-match "[?*]" name start)
      (aset name (match-beginning 0) ?$)
      (setq start (1+ (match-end 0))))
    name))

;;; Fix interface to (X-specific) mouse.el
(defun x-set-selection (type data)
  (or type (setq type 'PRIMARY))
  (put 'x-selections type data))

(defun x-get-selection (&optional type data-type)
  (or type (setq type 'PRIMARY))
  (get 'x-selections type))

(fmakunbound 'font-menu-add-default)
(global-unset-key [C-down-mouse-1])
(global-unset-key [C-down-mouse-2])
(global-unset-key [C-down-mouse-3])

;;; Set to a system sound if you want a fancy bell.
(set-message-beep nil)

;;; w32-fns.el ends here
