;;; winnt.el --- Lisp routines for Windows NT.

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
   (purecopy '(column-number-mode "C%c--"))
   (purecopy '(-3 . "%p"))
   (purecopy "-%-")))

;; Ignore case on file-name completion
(setq completion-ignore-case t)

;; The cmd.exe shell uses the "/c" switch instead of the "-c" switch
;; for executing its command line argument (from simple.el).
(setq shell-command-switch "/c")

;; For appending suffixes to directories and files in shell completions.
(add-hook 'shell-mode-hook 
	  '(lambda () (setq comint-completion-addsuffix '("\\" . " "))))

;; Use ";" instead of ":" as a path separator (from files.el).
(setq path-separator ";")

;; Set the null device (for compile.el).
(setq grep-null-device "NUL")

;; Set the grep regexp to match entries with drive letters.
(setq grep-regexp-alist
  '(("^\\(\\([a-zA-Z]:\\)?[^:( \t\n]+\\)[:( \t]+\\([0-9]+\\)[:) \t]" 1 3)))

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

;;; For using attached Unix filesystems.
(defun save-to-unix-hook ()
  (save-excursion
    (setq buffer-file-type t))
  nil)

(defun revert-from-unix-hook ()
  (save-excursion
    (setq buffer-file-type (find-buffer-file-type (buffer-file-name))))
  nil)

;; Really should provide this capability at the drive letter granularity.
(defun using-unix-filesystems (flag)
  "Read and write files without CR/LF translation, if FLAG is non-nil.
This is in effect assuming the files are on a remote Unix file system.
If FLAG is nil, resume using CR/LF translation as usual."
  (if flag
      (progn
	(add-hook 'write-file-hooks 'save-to-unix-hook)
	(add-hook 'after-save-hook 'revert-from-unix-hook))
    (progn
      (remove-hook 'write-file-hooks 'save-to-unix-hook)
      (remove-hook 'after-save-hook 'revert-from-unix-hook))))

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

;;; winnt.el ends here
