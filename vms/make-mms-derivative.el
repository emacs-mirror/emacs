;;; make-mms-derivative.el --- framework to do horrible things for VMS support

;; Copyright (C) 2003, 2004, 2005, 2006, 2007, 2008
;;   Free Software Foundation, Inc.

;; Author: Thien-Thi Nguyen <ttn@gnu.org>
;; Keywords: maint build vms mms makefile levitte autoconf war-is-a-lose

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Under VMS the standard make-like program is called MMS, which looks
;; for an input file in the default directory named DESCRIP.MMS and runs
;; the DCL command rules therein.  As of 2005, the build process
;; requires a hand translation of the Makefile.in and Emacs-specific
;; methodology to DCL and TPU commands, so to alleviate this pain, we
;; provide `make-mms-derivative', which given a source FILENAME, inserts
;; the file contents in a new buffer and loads FILENAME-2mms.  The lisp
;; code in the -2mms file can (do whatever -- it's emacs -- and), as
;; long as it arranges to write out the modified buffer after loading by
;; specifying, on a line of its own, the directive:
;;
;;  :output RELATIVE-OUTPUT
;;
;; where RELATIVE-OUTPUT is a filename (a string) relative to FILENAME's
;; directory, typically something simple like "descrip.mms_in_in".  Only
;; the first :output directive is recognized.
;;
;; The only other special directive at this time has the form:
;;
;;  :gigo NAME
;;  ;;blah blah blah
;;  ;;(more text here)
;;
;; NAME is anything distinguishable w/ `eq' (number, symbol or keyword).
;; This associates NAME with the block of text starting immediately below
;; the :gigo directive and ending at the first line that does not begin
;; with two semicolons (which are stripped from each line in the block).
;; To insert this block of text, pass NAME to `make-mms-derivative-gigo'.
;;
;; Directives are scanned before normal evaluation, so their placement
;; in the file is not important.  During loading, plain strings are
;; displayed in the echo area, prefixed with the current line number.
;;
;; Over the long run, the convenience functions provided (see source)
;; will be augmented by factoring maximally the -2mms files, squeezing
;; as much algorithm out of those nasty heuristics as possible.  What
;; makes them nasty is not that they rely on the conventions of the
;; Emacs makefiles; that's no big deal.  What makes them nasty is that
;; they rely on the conventions of separately maintained tools (namely
;; Autoconf for VMS and GNU Autoconf), and the separation of conventions
;; is how people drift apart, dragging their software behind
;; mercilessly.
;;
;; In general, codified thought w/o self-synchronization is doomed.
;; That a generation would eat its young (most discriminatingly, even)
;; is no reason GNU cannot build around such woe.

;;; Code:

(defvar make-mms-derivative-data nil
  "Plist of data specific to `make-mms-derivative'.")

(defun make-mms-derivative-data (key &optional newval)
  (if newval (setq make-mms-derivative-data
		   (plist-put make-mms-derivative-data key newval))
    (plist-get make-mms-derivative-data key)))

(defun make-mms-derivative-gigo (name)
  "Insert the text associated with :gigo NAME."
  (insert (cdr (assq name (make-mms-derivative-data :gigo)))))

(defun make-mms-derivative (filename)
  "Take FILENAME contents, load FILENAME-2mms, and write out the result.
The output file is specified by the :output directive in FILENAME-2mms.
See commentary of make-mms-derivative.el for full documentation."
  (interactive "fSource File: ")
  (let* ((todo (let ((fn (concat filename "-2mms")))
		 (unless (file-exists-p fn)
		   (error "Could not find %s" fn))
		 (set-buffer (get-buffer-create " *make-mms-derivative todo*"))
		 (insert-file-contents fn)
		 (current-buffer)))
	 (deriv (get-buffer-create (format "*mms-derivative: %s"
					   (file-relative-name filename))))
	 output gigo form)
    (set-buffer todo)
    (re-search-forward "^:output")
    (setq output (expand-file-name (read (current-buffer))
				   (file-name-directory filename)))
    (goto-char (point-min))
    (while (re-search-forward "^:gigo" (point-max) t)
      (let ((name (read (current-buffer)))
	    (p (progn (forward-line 1) (point))))
	(while (looking-at ";;")
	  (delete-char 2)
	  (forward-line 1))
	(setq gigo (cons (cons name (buffer-substring p (point))) gigo))
	(delete-region p (point))))
    (message "Munging...")
    (switch-to-buffer deriv)
    (erase-buffer)
    (insert-file-contents filename)
    (set (make-local-variable 'make-mms-derivative-data)
	 (list :gigo gigo))
    (set-buffer todo)
    (goto-char (point-min))
    (while (condition-case nil
	       (setq form (read (current-buffer)))
	     (end-of-file nil))
      (if (stringp form)
	  (message "%d: %s" (count-lines (point-min) (point)) form)
	(save-excursion
	  (set-buffer deriv)
	  (eval form))))
    (set-buffer deriv)
    (message "Munging...done")
    (write-file output)
    (kill-buffer todo)
    (kill-buffer deriv)))

(provide 'make-mms-derivative)

;; arch-tag: a5b08625-3952-4053-be16-296220e27bb0
;;; make-mms-derivative.el ends here
