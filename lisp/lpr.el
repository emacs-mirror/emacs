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
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Commands to send the region or a buffer your printer.  Entry points
;; are `lpr-buffer', `print-buffer', lpr-region', or `print-region'; option
;; variables include `printer-name', `lpr-switches' and `lpr-command'.

;;; Code:

(defgroup lpr nil
  "Print Emacs buffer on line printer"
  :group 'wp)

;;;###autoload
(defcustom printer-name
  (if (memq system-type '(ms-dos windows-nt)) "PRN")
  "*The name of a local printer to which data is sent for printing.
\(Note that PostScript files are sent to `ps-printer-name', which see.\)

On Unix-like systems, a string value should be a name understood by
lpr's -P option; otherwise the value should be nil.

On MS-DOS and MS-Windows systems, a string value is taken as the name of
a printer device or port, provided `lpr-command' is set to \"\".
Typical non-default settings would be \"LPT1\" to \"LPT3\" for parallel
printers, or \"COM1\" to \"COM4\" or \"AUX\" for serial printers, or
\"//hostname/printer\" for a shared network printer.  You can also set
it to the name of a file, in which case the output gets appended to that
file.  If you want to discard the printed output, set this to \"NUL\"."
  :type '(choice ; could use string but then we lose completion for files.
		 (file :tag "Name")
		 (const :tag "Default" nil))
  :group 'lpr)

;;;###autoload
(defcustom lpr-switches nil 
  "*List of strings to pass as extra options for the printer program.
It is recommended to set `printer-name' instead of including an explicit
switch on this list.
See `lpr-command'."
  :type '(repeat (string :tag "Argument"))
  :group 'lpr)

(defcustom lpr-add-switches (eq system-type 'berkeley-unix)
  "*Non-nil means construct -T and -J options for the printer program.
These are made assuming that the program is `lpr';
if you are using some other incompatible printer program,
this variable should be nil."
  :type 'boolean
  :group 'lpr)

;;;###autoload
(defcustom lpr-command
  (cond
   ((memq system-type '(ms-dos windows-nt))
    "")
   ((memq system-type '(usg-unix-v dgux hpux irix))
    "lp")
   (t
    "lpr"))
  "*Name of program for printing a file.

On MS-DOS and MS-Windows systems, if the value is an empty string then
Emacs will write directly to the printer port named by `printer-name'.
The programs `print' and `nprint' (the standard print programs on
Windows NT and Novell Netware respectively) are handled specially, using
`printer-name' as the destination for output; any other program is
treated like `lpr' except that an explicit filename is given as the last
argument."
  :type 'string
  :group 'lpr)

;; Default is nil, because that enables us to use pr -f
;; which is more reliable than pr with no args, which is what lpr -p does.
(defcustom lpr-headers-switches nil
  "*List of strings of options to request page headings in the printer program.
If nil, we run `lpr-page-header-program' to make page headings
and print the result."
  :type '(repeat (string :tag "Argument"))
  :group 'lpr)

(defcustom print-region-function nil
  "Function to call to print the region on a printer.
See definition of `print-region-1' for calling conventions."
  :type 'function
  :group 'lpr)

(defcustom lpr-page-header-program "pr"
  "*Name of program for adding page headers to a file."
  :type 'string
  :group 'lpr)

;; Berkeley systems support -F, and GNU pr supports both -f and -F,
;; So it looks like -F is a better default.
(defcustom lpr-page-header-switches '("-F")
  "*List of strings to use as options for the page-header-generating program.
The variable `lpr-page-header-program' specifies the program to use."
  :type '(repeat string)
  :group 'lpr)

;;;###autoload
(defun lpr-buffer ()
  "Print buffer contents as with Unix command `lpr'.
`lpr-switches' is a list of extra switches (strings) to pass to lpr."
  (interactive)
  (print-region-1 (point-min) (point-max) lpr-switches nil))

;;;###autoload
(defun print-buffer ()
  "Paginate and print buffer contents.
Normally invokes `pr' for pagination, but see the variable
`lpr-page-header-program'.  Printing is normally done with `lpr'
or `lp'; the variable `lpr-command' changes this.

Also see the variables `lpr-switches' and `lpr-page-header-switches'
for further customization of the commands used."
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
	;; Make pipes use the same coding system as
	;; writing the buffer to a file would.
	(coding-system-for-write
	 (or coding-system-for-write buffer-file-coding-system))
	(coding-system-for-read
	 (or coding-system-for-read buffer-file-coding-system))
	(width tab-width)
	switch-string)
    (save-excursion
      (if page-headers
	  (if lpr-headers-switches
	      ;; It is possible to use an lpr option
	      ;; to get page headers.
	      (setq switches (append (if (stringp lpr-headers-switches)
					 (list lpr-headers-switches)
				        lpr-headers-switches)
				     switches))))
      (setq switch-string
	    (if switches (concat " with options "
				 (mapconcat 'identity switches " "))
	      ""))
      (message "Spooling%s..." switch-string)
      (if (/= tab-width 8)
	  (let ((new-coords (print-region-new-buffer start end)))
	    (setq start (car new-coords) end (cdr new-coords))
	    (setq tab-width width)
	    (save-excursion
	      (goto-char end)
	      (setq end (point-marker)))
	    (untabify (point-min) (point-max))))
      (if page-headers
	  (if lpr-headers-switches
	      ;; We handled this above by modifying SWITCHES.
	      nil
	    ;; Run a separate program to get page headers.
	    (let ((new-coords (print-region-new-buffer start end)))
	      (setq start (car new-coords) end (cdr new-coords)))
	    (apply 'call-process-region start end lpr-page-header-program
				 t t nil
				 (nconc (list "-h" title)
					lpr-page-header-switches))
	    (setq start (point-min) end (point-max))))
      (apply (or print-region-function 'call-process-region)
	     (nconc (list start end lpr-command
			  nil nil nil)
		    (nconc (and lpr-add-switches
				(list "-J" name))
			   ;; These belong in pr if we are using that.
			   (and lpr-add-switches lpr-headers-switches
				(list "-T" title))
			   (and (stringp printer-name)
				(list (concat "-P" printer-name)))
			   switches)))
      (if (markerp end)
	  (set-marker end nil))
      (message "Spooling%s...done" switch-string))))

;; This function copies the text between start and end
;; into a new buffer, makes that buffer current.
;; It returns the new range to print from the new current buffer
;; as (START . END).

(defun print-region-new-buffer (ostart oend)
  (if (string= (buffer-name) " *spool temp*")
      (cons ostart oend)
    (let ((oldbuf (current-buffer)))
      (set-buffer (get-buffer-create " *spool temp*"))
      (widen) (erase-buffer)
      (insert-buffer-substring oldbuf ostart oend)
      (cons (point-min) (point-max)))))

(defun printify-region (begin end)
  "Replace nonprinting characters in region with printable representations.
The printable representations use ^ (for ASCII control characters) or hex.
The characters tab, linefeed, space, return and formfeed are not affected."
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

(provide 'lpr)

;;; lpr.el ends here
