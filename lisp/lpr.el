;;; lpr.el --- print Emacs buffer on line printer.

;; Copyright (C) 1985, 1988, 1992, 1994 Free Software Foundation, Inc.

;; Maintainer: FSF
;; Keywords: unix

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

;; Commands to send the region or a buffer your printer.  Entry points
;; are `lpr-buffer', `print-buffer', lpr-region', or `print-region'; option
;; variables include `lpr-switches' and `lpr-command'.

;;; Code:

;;;###autoload
(defvar lpr-switches nil 
  "*List of strings to pass as extra switch args to `lpr' when it is invoked.")

(defvar lpr-add-options (eq system-type 'berkeley-unix)
  "*Non-nil means construct -T and -J options for the `lpr'.")

;;;###autoload
(defvar lpr-command
  (if (memq system-type '(usg-unix-v dgux hpux irix))
      "lp" "lpr")
  "*Name of program for printing a file.")

;; Default is nil, because that enables us to use pr -f
;; which is more reliable than pr with no args, which is what lpr -p does.
(defvar lpr-headers-switches nil
  "*List of strings to use as options for `lpr' to request page headings.
If nil, we run `lpr-page-header-program' to make page headings
and print the result.")

(defvar print-region-function nil
  "Function to call to print the region on a printer.
See definition of `print-region-1' for calling conventions.")

(defvar lpr-page-header-program "pr"
  "*Name of program for adding page headers to a file.")

(defvar lpr-page-header-switches '("-f")
  "*List of strings to use as options for `lpr-page-header-program'.")

;;;###autoload
(defun lpr-buffer ()
  "Print buffer contents as with Unix command `lpr'.
`lpr-switches' is a list of extra switches (strings) to pass to lpr."
  (interactive)
  (print-region-1 (point-min) (point-max) lpr-switches nil))

;;;###autoload
(defun print-buffer ()
  "Print buffer contents as with Unix command `lpr -p'.
`lpr-switches' is a list of extra switches (strings) to pass to lpr."
  (interactive)
  (print-region-1 (point-min) (point-max) lpr-switches t))

;;;###autoload
(defun lpr-region (start end)
  "Print region contents as with Unix command `lpr'.
`lpr-switches' is a list of extra switches (strings) to pass to lpr."
  (interactive "r")
  (print-region-1 start end lpr-switches nil))

;;;###autoload
(defun print-region (start end)
  "Print region contents as with Unix command `lpr -p'.
`lpr-switches' is a list of extra switches (strings) to pass to lpr."
  (interactive "r")
  (print-region-1 start end lpr-switches t))

(defun print-region-1 (start end switches page-headers)
  ;; On some MIPS system, having a space in the job name
  ;; crashes the printer demon.  But using dashes looks ugly
  ;; and it seems to annoying to do for that MIPS system.
  (let ((name (concat (buffer-name) " Emacs buffer"))
	(title (concat (buffer-name) " Emacs buffer"))
	(width tab-width))
    (save-excursion
      (message "Spooling...")
      (if (/= tab-width 8)
	  (progn
	    (print-region-new-buffer start end)
	    (setq tab-width width)
	    (save-excursion
	      (goto-char end)
	      (setq end (point-marker)))
	    (untabify (point-min) (point-max))))
      (if page-headers
	  (if lpr-headers-switches
	      ;; It is possible to use an lpr option
	      ;; to get page headers.
	      (setq switches (append (if (stringp lpr-headers-switches)
					 (list lpr-headers-switches)
				        lpr-headers-switches)
				     switches))
	    ;; Run a separate program to get page headers.
	    (print-region-new-buffer start end)
	    (call-process-region start end lpr-page-header-program
				 t t lpr-page-header-switches)
	    (setq start (point-min) end (point-max))))
      (apply (or print-region-function 'call-process-region)
	     (nconc (list start end lpr-command
			  nil nil nil)
		    (nconc (and lpr-add-options
				(list "-J" name "-T" title))
			   switches)))
      (if (markerp end)
	  (set-marker end nil))
      (message "Spooling...done"))))

;; This function copies the text between start and end
;; into a new buffer, makes that buffer current,
;; and sets start and end to the buffer bounds.
;; start and end are used free.
(defun print-region-new-buffer (ostart oend)
  (or (string= (buffer-name) " *spool temp*")
      (let ((oldbuf (current-buffer)))
	(set-buffer (get-buffer-create " *spool temp*"))
	(widen) (erase-buffer)
	(insert-buffer-substring oldbuf ostart oend)
	(setq start (point-min) end (point-max)))))

(defun printify-region (begin end)
  "Turn nonprinting characters (other than TAB, LF, SPC, RET, and FF)
in the current buffer into printable representations as control or
hexadecimal escapes."
  (interactive "r")
  (save-excursion
    (goto-char begin)
    (let (c)
      (while (re-search-forward "[\^@-\^h\^k\^n-\^_\177-\377]" end t)
	(setq c (preceding-char))
	(delete-backward-char 1)
	(insert 
	 (if (< c ?\ )
	     (format "\\^%c" (+ c ?@))
	   (format "\\%02x" c)))))))

;;; lpr.el ends here
