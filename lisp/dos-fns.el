;;; dos-fns.el --- MS-Dos specific functions.

;; Copyright (C) 1991, 1993, 1995, 1996 Free Software Foundation, Inc.

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
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Part of this code is taken from (or derived from) demacs.

;;; Code:

;;; Add %t: into the mode line format just after the open-paren.
(let ((tail (member "   %[(" mode-line-format)))
  (setcdr tail (cons (purecopy "%t:")
		     (cdr tail))))

;; Use ";" instead of ":" as a path separator (from files.el).
(setq path-separator ";")

;; Set the null device (for compile.el).
(setq grep-null-device "NUL")

;; Set the grep regexp to match entries with drive letters.
(setq grep-regexp-alist
  '(("^\\(\\([a-zA-Z]:\\)?[^:( \t\n]+\\)[:( \t]+\\([0-9]+\\)[:) \t]" 1 3)))

;; This overrides a trivial definition in files.el.
(defun convert-standard-filename (filename)
  "Convert a standard file's name to something suitable for the current OS.
This function's standard definition is trivial; it just returns the argument.
However, on some systems, the function is redefined
with a definition that really does change some file names."
  (if (or (msdos-long-file-names)
	  (not (stringp filename))
	  (member (file-name-nondirectory filename) '("" "." "..")))
      filename
    (let* ((dir (file-name-directory filename))
	   (string (copy-sequence (file-name-nondirectory filename)))
	   (lastchar (aref string (1- (length string))))
	   i firstdot)
      ;; Change a leading period to a leading underscore.
      (if (= (aref string 0) ?.)
	  (aset string 0 ?_))
      ;; Get rid of invalid characters.
      (while (setq i (string-match
		      "[^-a-zA-Z0-9_.%~^$!#&{}@`'()\200-\376]"
		      string))
	(aset string i ?_))
      ;; If we don't have a period,
      ;; and we have a dash or underscore that isn't the first char,
      ;; change that to a period.
      (if (and (not (string-match "\\." string))
	       (setq i (string-match "[-_]" string 1)))
	  (aset string i ?\.))
      ;; If we don't have a period in the first 8 chars, insert one.
      (if (> (or (string-match "\\." string)
		 (length string))
	     8)
	  (setq string
		(concat (substring string 0 8)
			"."
			(substring string 8))))
      (setq firstdot (or (string-match "\\." string) (1- (length string))))
      ;; Truncate to 3 chars after the first period.
      (if (> (length string) (+ firstdot 4))
	  (setq string (substring string 0 (+ firstdot 4))))
      ;; Change all periods except the first one into underscores.
      (while (string-match "\\." string (1+ firstdot))
	(setq i (string-match "\\." string (1+ firstdot)))
	(aset string i ?_))
      ;; If the last character of the original filename was `~',
      ;; make sure the munged name ends with it also.
      (if (equal lastchar ?~)
	  (aset string (1- (length string)) lastchar))
      (concat dir string))))

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

;; Support for printing under MS-DOS, see lpr.el and ps-print.el.
(defvar dos-printer "PRN"
  "*The name of a local MS-DOS device to which data is sent for printing.
\(Note that PostScript files are sent to `dos-ps-printer', which see.\)

Typical non-default settings would be \"LPT1\" to \"LPT3\" for
parallel printers, or \"COM1\" to \"COM4\" or \"AUX\" for serial
printers.  You can also set it to a name of a file, in which
case the output gets appended to that file.
If you want to discard the printed output, set this to \"NUL\".")

(defun dos-print-region-function (start end
					&optional lpr-prog
					delete-text buf display rest)
  "MS-DOS-specific function to print the region on a printer.
Writes the region to the device or file which is a value of
`dos-printer' \(which see\).  Ignores any arguments beyond
START and END."

  (write-region start end dos-printer t 0)
  ;; Make each print-out start on a new page, but don't waste
  ;; paper if there was a form-feed at the end of this file.
  (if (not (char-equal (char-after (1- end)) ?\C-l))
      (write-region "\f" nil dos-printer t 0)))

;; Set this to nil if you have a port of the `lpr' program and
;; you want to use it for printing.  If the default setting is
;; in effect, `lpr-command' and its switches are ignored when
;; printing with `lpr-xxx' and `print-xxx'.
(setq print-region-function 'dos-print-region-function)

;; Set this to nil if you have a port of the `pr' program
;; (e.g., from GNU Textutils), or if you have an `lpr'
;; program (see above) that can print page headers.
;; If `lpr-headers-switches' is non-nil (the default) and
;; `print-region-function' is set to `dos-print-region-function',
;; then requests to print page headers will be silently
;; ignored, and `print-buffer' and `print-region' produce
;; the same output as `lpr-buffer' and `lpr-region', accordingly.
(setq lpr-headers-switches "(page headers are not supported)")

(defvar dos-ps-printer "PRN"
  "*Method for printing PostScript files under MS-DOS.

If the value is a string, then it is taken as the name of the
device to which PostScript files are written.  By default it
is the default printer device; typical non-default settings
would be \"LPT1\" to \"LPT3\" for parallel printers, or \"COM1\"
to \"COM4\" or \"AUX\" for serial printers.  You can also set it
to a name of a file, in which case the output gets appended
to that file.  \(Note that `ps-print' package already has
facilities for printing to a file, so you might as well use
them instead of changing the setting of this variable.\)  If
you want to silently discard the printed output, set this to \"NUL\".

If the value is anything but a string, PostScript files will be
piped to the program given by `ps-lpr-command', with switches
given by `ps-lpr-switches', which see.")

(setq ps-lpr-command "gs")

(setq ps-lpr-switches '("-q" "-dNOPAUSE" "-sDEVICE=epson" "-r240x60"
			  "-sOutputFile=LPT1" "-"))

;; Backward compatibility for obsolescent functions which
;; set screen size.

(defun mode25 ()
  "Changes the number of screen rows to 25."
  (interactive)
  (set-frame-size (selected-frame) 80 25))

(defun mode4350 ()
  "Changes the number of rows to 43 or 50.
Emacs always tries to set the screen height to 50 rows first.
If this fails, it will try to set it to 43 rows, on the assumption
that your video hardware might not support 50-line mode."
  (interactive)
  (set-frame-size (selected-frame) 80 50)
  (if (eq (frame-height (selected-frame)) 50)
      nil  ; the original built-in function returned nil
    (set-frame-size (selected-frame) 80 43)))

(provide 'dos-fns)

; dos-fns.el ends here
