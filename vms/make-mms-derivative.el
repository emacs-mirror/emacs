;;; make-mms-derivative.el --- framework to do horrible things for VMS support

;; Copyright (C) 2003 Free Software Foundation, Inc.

;; Author: Thien-Thi Nguyen <ttn@gnu.org>
;; Keywords: maint build vms mms makefile levitte autoconf war-is-a-lose
;; Favorite-TV-Game-Show: L'Eredità

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

;; Under OpenVMS the standard make-like program is called MMS, which
;; looks for an input file in the default directory named DESCRIP.MMS
;; and runs the DCL command rules therein.  As of 2003, the build
;; process requires a hand translation of the Makefile.in and related
;; Emacs-specific methodology to DCL and TPU commands, so to alleviate
;; this pain, we provide `make-mms-derivative', which given a source
;; FILENAME (under `make-mms-derivative-root-dir'), inserts the file
;; contents in a new buffer and loads FILENAME-2mms.  The elisp in the
;; -2mms file can (do whatever -- it's emacs -- and) arrange to write
;; out the modified buffer after FILENAME-2mms loading by using:
;;
;;  (make-mms-derivative-data 'write-under-root RELATIVE-FILENAME)
;;
;; where RELATIVE-FILENAME is something like "src/descrip.mms_in_in".
;; Over the long run, the convenience procedures provided (see source)
;; will be augmented by factoring maximally the -2mms files, squeezing
;; as much algorithm out of those nasty heuristics as possible.  What
;; makes them nasty is not that they rely on the conventions of the
;; Emacs makefiles; that's no big deal.  What makes them nasty is that
;; they rely on the conventions of separately maintained tools (namely
;; Autoconf 1.11 under OpenVMS and the rest of GNU), and the separation
;; of conventions is how people drift apart, dragging their software
;; behind mercilessly.
;;
;; In general, codified thought w/o self-synchronization is doomed.
;; That a generation would eat its young (most discriminatingly, even)
;; is no reason GNU cannot build around such woe.

;;; Code:

(defvar make-mms-derivative-root-dir "AXPA:[TTN.EMACS.EMACS212_3]"
  "Source tree root directory.")

(defvar make-mms-derivative-data nil
  "Alist of data specific to `make-mms-derivative'.")

(defun make-mms-derivative-data (key &optional newval)
  (if newval
      (setq make-mms-derivative-data
            (cons (cons key newval) make-mms-derivative-data))
    (cdr (assq key make-mms-derivative-data))))

(defun make-mms-derivative-write-under-root (rel-filename)
  (write-file (expand-file-name rel-filename make-mms-derivative-root-dir)))

(defmacro make-mms-derivative-progn (msg &rest body)
  `(progn
     (message "(%s) %s" (point) ,msg)
     ,@body))

(put 'make-mms-derivative-progn 'lisp-indent-function 1)

(defun make-mms-derivative-load-edits-file (name)
  (make-mms-derivative-data 'edits-filename name)
  (let (raw-data
	(cur (current-buffer))
	(wbuf (get-buffer-create "*make-mms-derivative-load-edits-file work")))
    (set-buffer wbuf)
    (insert-file-contents name)
    (keep-lines "^;;;[0-9]+;;")
    (goto-char (point-max))
    (while (re-search-backward "^;;;\\([0-9]+\\);;\\(.*\\)$" (point-min) t)
      (let* ((i (string-to-number (match-string 1)))
	     (line (match-string 2))
	     (look (assq i raw-data)))
	(if look
	    (setcdr look (cons line (cdr look)))
	  (setq raw-data (cons (list i line) raw-data)))))
    (kill-buffer wbuf)
    (set-buffer cur)
    (mapcar '(lambda (ent)
	       (setcdr ent (mapconcat '(lambda (line)
					 (concat line "\n"))
				      (cdr ent)
				      "")))
	    raw-data)
    (make-mms-derivative-data 'raw-data raw-data))
  (load name))

(defun make-mms-derivative-insert-raw-data (n)
  (insert (cdr (assq n (make-mms-derivative-data 'raw-data)))))

(defun make-mms-derivative (file)
  (interactive "fSource File: ")
  (let ((root (expand-file-name make-mms-derivative-root-dir))
        (file (expand-file-name file)))
    (when (file-name-absolute-p (file-relative-name file root))
      (error "Not under root (%s)" root))
    (let ((edits-filename (concat file "-2mms")))
      (unless (file-exists-p edits-filename)
        (error "Could not find %s" edits-filename))
      (let ((buf (get-buffer-create
		  (format "*mms-derivative: %s"
			  (file-relative-name file root)))))
        (message "Munging ...")
        (switch-to-buffer buf)
        (erase-buffer)
        (make-variable-buffer-local 'make-mms-derivative-data)
        (insert-file file)
        (make-mms-derivative-load-edits-file edits-filename)
        (let ((out (make-mms-derivative-data 'write-under-root)))
          (when out (make-mms-derivative-write-under-root out))
          (kill-buffer buf)
          (unless out (message "Munging ... done")))))))

(provide 'make-mms-derivative)

;;; arch-tag: a5b08625-3952-4053-be16-296220e27bb0
;;; make-mms-derivative.el ends here
