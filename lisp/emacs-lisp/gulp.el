;;; gulp.el --- Ask for updates for Lisp packages

;; Copyright (C) 1996 Free Software Foundation, Inc.

;; Author: Sam Shteingold <shteingd@math.ucla.edu>
;; Maintainer: FSF
;; Keywords: maintenance

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

;; Search the emacs/{version}/lisp directory for *.el files, extract the
;; name of the author or maintainer and send him e-mail requesting
;; update.

;;; Code:

(defvar gulp-discard "^;+ *Maintainer: *FSF *$"
  "*The regexp matching the packages not requiring the request for updates.")

(defvar gulp-tmp-buffer " *gulp*" "The name of the temporary buffer.")

(defvar gulp-max-len 2000
  "*Distance into a Lisp source file to scan for keywords.")

(defvar gulp-request-header
  "This message was created automatically.
Apparently, you are the maintainer of the following package(s):\n\n"
  "*Text to use at the start of a message sent to request updates.")

(defvar gulp-request-end
  "\nIf your copy is newer than mine, please email me the patches ASAP.\n\n"
  "*Text to add at the end of a message sent to request updates.")

(defun gulp-send-requests (dir)
  "Send requests for updates to the authors of Lisp packages in directory DIR.
The prepared message consists of `gulp-request-header', followed by the
list of packages with modification times, concluded with `gulp-request-end'.
You can't edit the message, but you can confirm whether to send it.
The list of rejected addresses will be put into `gulp-tmp-buffer'."
  (interactive "DRequest updates for Lisp directory: ")
  (let ((m-p-alist (gulp-create-m-p-alist
		    (directory-files dir nil "\\.el$" t)))
	mail-setup-hook msg node)
    (while (setq node (car m-p-alist))
      (setq msg (gulp-create-message (cdr node)))
      (setq mail-setup-hook '(lambda () (goto-char (point-max)) (insert msg)))
      (mail nil (car node))
      (if (y-or-n-p "Send? ") (mail-send)
	(kill-this-buffer)
	(set-buffer gulp-tmp-buffer)
	(insert (format "%s\n\n" node)))
      (setq m-p-alist (cdr m-p-alist)))))

(defun gulp-create-message (rec)
  "Return the message string for REC, which is a list like (FILE TIME)."
  (let (node (str gulp-request-header))
    (while (setq node (car rec))
      (setq str (concat str "\t" (car node) "\tLast modified:\t" (cdr node) "\n"))
      (setq rec (cdr rec)))
    (concat str gulp-request-end)))

(defun gulp-create-m-p-alist (flist)
  "Create the maintainer/package alist for files in FLIST.
List of elements (MAINTAINER . (LIST of PACKAGES))"
  (let (mplist filen node fl-tm)
    (get-buffer-create gulp-tmp-buffer)
    (while flist
      (setq fl-tm (gulp-maintainer (setq filen (car flist))))
      (if (setq mnt (car fl-tm));; there is a definite maintainer
	  (if (setq node (assoc mnt mplist));; this is not a new maintainer
	      (setq mplist (cons (cons (car node)
				       (cons (cons filen (cdr fl-tm))
					     (cdr node)))
				 (delete node mplist)))
	    (setq mplist (cons (list mnt (cons filen (cdr fl-tm))) mplist))))
      (message "%s -- %s" filen fl-tm)
      (setq flist (cdr flist)))
    (set-buffer gulp-tmp-buffer)
    (erase-buffer)
    mplist))

(defun gulp-maintainer (filenm)
  "Return a list (MAINTAINER TIMESTAMP) for the package FILENM."
  (save-excursion
    (let* ((fl (concat gulp-search-path filenm)) mnt
	   (timest (format-time-string "%Y-%m-%d %a %T %Z"
				       (elt (file-attributes fl) 5))))
      (set-buffer gulp-tmp-buffer)
      (erase-buffer)
      (insert-file-contents fl nil 0 gulp-max-len)
      (goto-char 1)
      (if (re-search-forward gulp-discard nil t)
	  (setq mnt nil) ;; do nothing, return nil
	(goto-char 1)
	(if (and (re-search-forward "^;+ *Maintainer: \\(.*\\)$" nil t)
		 (> (length (setq mnt (match-string 1))) 0))
	    () ;; found!
	  (goto-char 1)
	  (if (re-search-forward "^;+ *Author: \\(.*\\)$" nil t)
	      (setq mnt (match-string 1))))
	(if (= (length mnt) 0) (setq mnt nil))) ;; "^;; Author: $" --> nil
      (cons mnt timest))))

;;; gulp.el ends here
