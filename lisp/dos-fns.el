;;; dos-fns.el --- MS-Dos specific functions.

;; Copyright (C) 1991, 1993 Free Software Foundation, Inc.

;; Maintainer: Morten Welinder (terra@diku.dk)
;; Keywords: internal

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

;; Part of this code is taken from (or derived from) demacs.

;;; Code:

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

;; Use ";" instead of ":" as a path separator (from files.el).
(setq path-separator ";")

;; Set the null device (for compile.el).
(setq grep-null-device "NUL")

;; Set the grep regexp to match entries with drive letters.
(setq grep-regexp-alist
  '(("^\\(\\([a-zA-Z]:\\)?[^:( \t\n]+\\)[:( \t]+\\([0-9]+\\)[:) \t]" 1 3)))

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

(defvar msdos-shells '("command.com" "4dos.com" "ndos.com")
  "*List of shells that use `/c' instead of `-c' and a backslashed command.")

(defconst register-name-alist
  '((ax . 0) (bx . 1) (cx . 2) (dx . 3) (si . 4) (di . 5)
    (cflag . 6) (flags . 7)
    (al . (0 . 0)) (bl . (1 . 0)) (cl . (2 . 0)) (dl . (3 . 0))
    (ah . (0 . 1)) (bh . (1 . 1)) (ch . (2 . 1)) (dh . (3 . 1))))

(defun make-register ()
  (make-vector 8 0))

(defun register-value (regs name)
  (let ((where (cdr (assoc name register-name-alist))))
    (cond ((consp where)
	   (let ((tem (aref regs (car where))))
	     (if (zerop (cdr where))
		 (% tem 256)
	       (/ tem 256))))
	  ((numberp where)
	   (aref regs where))
	  (t nil))))

(defun set-register-value (regs name value)
  (and (numberp value)
       (>= value 0)
       (let ((where (cdr (assoc name register-name-alist))))
	 (cond ((consp where)
		(let ((tem (aref regs (car where)))
		      (value (logand value 255)))
		  (aset regs
			(car where)
			(if (zerop (cdr where))
			    (logior (logand tem 65280) value)
			  (logior (logand tem 255) (lsh value 8))))))
	       ((numberp where)
		(aset regs where (logand value 65535))))))
  regs)

(defsubst intdos (regs)
  (int86 33 regs))

;; Extra stub to functions in src/frame.c
;; Emacs aborts during dump if the following don't have a doc string.
(defun window-frame (window)
  "Return the frame that WINDOW resides on."
  (selected-frame))
(defun raise-frame (frame)
  "Raise FRAME to the top of the desktop."
  nil)
(defun select-frame (frame &optional no-enter)
  "Select FRAME for input events."
  (selected-frame))
