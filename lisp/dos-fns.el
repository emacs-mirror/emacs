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
   'mode-name 'minor-mode-alist "%n" 'mode-line-process
   (purecopy ")%]--")
   (purecopy '(line-number-mode "L%l--"))
   (purecopy '(-3 . "%p"))
   (purecopy "-%-")))

;;
;; buffer-file-type   (0 "text") (1 "binary")
;;
(defvar file-name-buffer-file-type-alist
  '(
    ("[:/].*config.sys$" . 0)		; config.sys text
    ("\\.elc$" . 1)			; emacs stuff
    ("\\.\\(obj\\|exe\\|com\\|lib\\|sys\\|chk\\|out\\|bin\\|ico\\|pif\\)$" . 1)
					; MS-Dos stuff
    ("\\.\\(arc\\|zip\\|pak\\|lzh\\|zoo\\)$" . 1)
					; Packers
    ("\\.\\(a\\|o\\|tar\\|z\\|gz\\|taz\\)$" . 1)
					; Unix stuff
    ("\\.tp[ulpw]$" . 1)
					; Borland Pascal stuff
    ("[:/]tags$" . 1 ) 
					; Emacs TAGS file
    ))

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
    (if code
	(cond((numberp code) code)
	     ((and (symbolp code) (fboundp code))
	      (funcall code filename)))
      default-buffer-file-type)))

(defun find-file-binary (filename) 
  "Like find-file but always load the file as binary."
  (interactive "FFind file binary: ")
  (let ((file-name-buffer-file-type-alist '(("" . 1))))
    (find-file filename)))

(defun find-file-text (filename) 
  "Like find-file but always load the file as text."
  (interactive "FFind file text: ")
  (let ((file-name-buffer-file-type-alist '(("" . 0))))
    (find-file filename)))

(defun find-file-not-found-set-buffer-file-type ()
  (save-excursion
    (set-buffer (current-buffer))
    (setq buffer-file-type (find-buffer-file-type (buffer-file-name))))
  nil)

;;; To set the default file type on new files.
(add-hook 'find-file-not-found-hooks 'find-file-not-found-set-buffer-file-type)

;;; We use the Emacs directory, not /usr/local
(setq Info-default-directory-list (list "c:/emacs/info"))

(defvar msdos-shells '("command.com" "4dos.com" "ndos.com")
  "*List of shells that use `/c' instead of `-c' and a backslashed command.")

(defconst register-name-by-word-alist
  '((ax . 0) (bx . 1) (cx . 2) (dx . 3) (si . 4) (di . 5)
    (cflag . 6) (flags . 7)))

(defconst register-name-by-byte-alist
  '((al . (0 . 0)) (ah . (0 . 1))
    (bl . (1 . 0)) (bh . (1 . 1))
    (cl . (2 . 0)) (ch . (2 . 1))
    (dl . (3 . 0)) (dh . (3 . 1))))

(defun make-register ()
  (make-vector 8 0))

(defun register-value (regs name)
  (let ((where (or (cdr (assoc name register-name-by-word-alist))
		   (cdr (assoc name register-name-by-byte-alist)))))
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
       (> value 0)
       (let ((where (or (cdr (assoc name register-name-by-word-alist))
			(cdr (assoc name register-name-by-byte-alist)))))
	 (cond ((consp where)
		(setq value (% value 256))	; 0x100
		(let* ((tem (aref regs (car where)))
		       (l (% tem 256))
		       (h (/ tem 256)))
		  (if (zerop (cdr where))
		      (aset regs (car where) (+ (* h 256) value))
		    (aset regs (car where) (+ (* value 256) h)))))
	       ((numberp where)
		(setq value (% value 65536))	; 0x10000
		(aset regs where value)))))
  regs)

(defsubst intdos (regs)
  (int86 33 regs))

;;; Fix interface to (X-specific) mouse.el
(defalias 'window-frame 'ignore)
(defalias 'x-set-selection 'ignore)
(fset 'x-get-selection '(lambda (&rest rest) ""))
(fset 'frame-parameters 'ignore)
(fmakunbound 'font-menu-add-default)
(global-unset-key [C-down-mouse-1])
(global-unset-key [C-down-mouse-2])
(global-unset-key [C-down-mouse-3])
