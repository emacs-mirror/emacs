;; winnt.el --- Lisp routines for Windows NT.
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
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

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

;; Show file type (text or binary) on modeline
(setq-default mode-line-format
  (list (purecopy "")
   'mode-line-modified
   'mode-line-buffer-identification
   (purecopy "   ")
   'global-mode-string
   (purecopy "   %[(")
   (purecopy "%t:")
   'mode-name 'mode-line-process 'minor-mode-alist
   (purecopy "%n")
   (purecopy ")%]--")
   (purecopy '(line-number-mode "L%l--"))
   (purecopy '(-3 . "%p"))
   (purecopy "-%-")))

;; Ignore case on file-name completion
(setq completion-ignore-case t)

;; The cmd.exe shell uses the "/c" switch instead of the "-c" switch
;; for executing its command line argument (from simple.el).
(setq shell-command-switch "/c")

;; Use ";" instead of ":" as a path separator (from files.el).
(setq path-separator ";")

;; Taken from dos-fn.el ... don't want all that's in the file, maybe
;; separate it out someday.

(defvar file-name-buffer-file-type-alist
  '(
    ("[:/].*config.sys$" . nil)		; config.sys text
    ("\\.elc$" . t)			; emacs stuff
    ("\\.\\(obj\\|exe\\|com\\|lib\\|sys\\|chk\\|out\\|bin\\|ico\\|pif\\)$" . t)
					; MS-Dos stuff
    ("\\.\\(arc\\|zip\\|pak\\|lzh\\|zoo\\)$" . t)
					; Packers
    ("\\.\\(a\\|o\\|tar\\|z\\|gz\\|taz\\)$" . t)
					; Unix stuff
    ("\\.tp[ulpw]$" . t)
					; Borland Pascal stuff
    ("[:/]tags$" . t)
					; Emacs TAGS file
    )
  "*Alist for distinguishing text files from binary files.
Each element has the form (REGEXP . TYPE), where REGEXP is matched
against the file name, and TYPE is nil for text, t for binary.")

(defun find-buffer-file-type (filename)
  (let ((alist file-name-buffer-file-type-alist)
	(found nil)
	(code nil))
    (let ((case-fold-search t))
      (setq filename (file-name-sans-versions filename))
      (while (and (not found) alist)
	(if (string-match (car (car alist)) filename)
	    (setq code (cdr (car alist))
		  found t))
	(setq alist (cdr alist))))
    (if found
	(cond((memq code '(nil t)) code)
	     ((and (symbolp code) (fboundp code))
	      (funcall code filename)))
      default-buffer-file-type)))

(defun find-file-binary (filename) 
  "Visit file FILENAME and treat it as binary."
  (interactive "FFind file binary: ")
  (let ((file-name-buffer-file-type-alist '(("" . t))))
    (find-file filename)))

(defun find-file-text (filename) 
  "Visit file FILENAME and treat it as a text file."
  (interactive "FFind file text: ")
  (let ((file-name-buffer-file-type-alist '(("" . nil))))
    (find-file filename)))

(defun find-file-not-found-set-buffer-file-type ()
  (save-excursion
    (set-buffer (current-buffer))
    (setq buffer-file-type (find-buffer-file-type (buffer-file-name))))
  nil)

;;; To set the default file type on new files.
(add-hook 'find-file-not-found-hooks 'find-file-not-found-set-buffer-file-type)

;;; Fix interface to (X-specific) mouse.el
(defalias 'window-frame 'ignore)
(defalias 'x-set-selection 'ignore)
(fset 'x-get-selection '(lambda (&rest rest) ""))
(fmakunbound 'font-menu-add-default)
(global-unset-key [C-down-mouse-1])
(global-unset-key [C-down-mouse-2])
(global-unset-key [C-down-mouse-3])

;;; Set to a system sound if you want a fancy bell.
(set-message-beep nil)

;;; winnt.el ends here
