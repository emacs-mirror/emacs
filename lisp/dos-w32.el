;; dos-w32.el --- Functions shared among MS-DOS and W32 (NT/95) platforms

;; Copyright (C) 1996 Free Software Foundation, Inc.

;; Maintainer: Geoff Voelker <voelker@cs.washington.edu>
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
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Parts of this code are duplicated functions taken from dos-fns.el
;; and winnt.el.

;;; Code:

;; Use ";" instead of ":" as a path separator (from files.el).
(setq path-separator ";")

(setq minibuffer-history-case-insensitive-variables
      (cons 'file-name-history minibuffer-history-case-insensitive-variables))

;; Set the null device (for compile.el).
(setq null-device "NUL")

;; For distinguishing file types based upon suffixes.
(defvar file-name-buffer-file-type-alist
  '(
    ("[:/].*config.sys$" . nil)		; config.sys text
    ("\\.\\(obj\\|exe\\|com\\|lib\\|sys\\|bin\\|ico\\|pif\\|class\\)$" . t)
					; MS-Dos stuff
    ("\\.\\(dll\\|drv\\|386\\|vxd\\|fon\\|fnt\\|fot\\|ttf\\|grp\\)$" . t)
					; Windows stuff
    ("\\.\\(bmp\\|wav\\|avi\\|mpg\\|jpg\\|tif\\|mov\\|au\\)$" . t)
					; known binary data files
    ("\\.\\(arc\\|zip\\|pak\\|lzh\\|zoo\\)$" . t)
					; Packers
    ("\\.\\(a\\|o\\|tar\\|z\\|gz\\|taz\\|jar\\)$" . t)
					; Unix stuff
    ("\\.sx[dmicw]$" . t)		; OpenOffice.org
    ("\\.tp[ulpw]$" . t)		; borland Pascal stuff
    ("[:/]tags$" . nil)			; emacs TAGS file
    )
  "*Alist for distinguishing text files from binary files.
Each element has the form (REGEXP . TYPE), where REGEXP is matched
against the file name, and TYPE is nil for text, t for binary.")

;; Return the pair matching filename on file-name-buffer-file-type-alist,
;; or nil otherwise.
(defun find-buffer-file-type-match (filename)
  (let ((alist file-name-buffer-file-type-alist)
	(found nil))
    (let ((case-fold-search t))
      (setq filename (file-name-sans-versions filename))
      (while (and (not found) alist)
	(if (string-match (car (car alist)) filename)
	    (setq found (car alist)))
	(setq alist (cdr alist)))
      found)))

;; Don't check for untranslated file systems here.
(defun find-buffer-file-type (filename)
  (let ((match (find-buffer-file-type-match filename))
	(code))
    (if (not match)
	default-buffer-file-type
      (setq code (cdr match))
      (cond ((memq code '(nil t)) code)
	    ((and (symbolp code) (fboundp code))
	     (funcall code filename))))))

(setq-default buffer-file-coding-system 'undecided-dos)

(defun find-buffer-file-type-coding-system (command)
  "Choose a coding system for a file operation.
If COMMAND is `insert-file-contents', the coding system is chosen based
upon the filename, the contents of `untranslated-filesystem-list' and
`file-name-buffer-file-type-alist', and whether the file exists:

  If it matches in `untranslated-filesystem-list':
    If the file exists:					`undecided'
    If the file does not exist:				`undecided-unix'
  If it matches in `file-name-buffer-file-type-alist':
    If the match is t (for binary):			`no-conversion'
    If the match is nil (for dos-text):			`undecided-dos'
  Otherwise:
    If the file exists:					`undecided'
    If the file does not exist:	       default-buffer-file-coding-system

If COMMAND is `write-region', the coding system is chosen based upon
the value of `buffer-file-coding-system' and `buffer-file-type'. If
`buffer-file-coding-system' is non-nil, its value is used.  If it is
nil and `buffer-file-type' is t, the coding system is `no-conversion'.
Otherwise, it is `undecided-dos'.

The two most common situations are when DOS and Unix files are read
and written, and their names do not match in
`untranslated-filesystem-list' and `file-name-buffer-file-type-alist'.
In these cases, the coding system initially will be `undecided'.  As
the file is read in the DOS case, the coding system will be changed to
`undecided-dos' as CR/LFs are detected.  As the file is read in the
Unix case, the coding system will be changed to `undecided-unix' as
LFs are detected.  In both cases, `buffer-file-coding-system' will be
set to the appropriate coding system, and the value of
`buffer-file-coding-system' will be used when writing the file."

  (let ((op (nth 0 command))
	(target)
	(binary nil) (text nil)
	(undecided nil) (undecided-unix nil))
    (cond ((eq op 'insert-file-contents)
	   (setq target (nth 1 command))
	   ;; First check for a file name that indicates
	   ;; it is truly binary.
	   (setq binary (find-buffer-file-type target))
	   (cond (binary)
		 ;; Next check for files that MUST use DOS eol conversion.
		 ((find-buffer-file-type-match target)
		  (setq text t))
		 ;; For any other existing file, decide based on contents.
		 ((file-exists-p target)
		  (setq undecided t))
		 ;; Next check for a non-DOS file system.
		 ((untranslated-file-p target)
		  (setq undecided-unix t)))
	   (cond (binary '(no-conversion . no-conversion))
		 (text '(undecided-dos . undecided-dos))
		 (undecided-unix '(undecided-unix . undecided-unix))
		 (undecided '(undecided . undecided))
		 (t (cons default-buffer-file-coding-system
			  default-buffer-file-coding-system))))
	  ((eq op 'write-region)
	   (if buffer-file-coding-system
	       (cons buffer-file-coding-system
		     buffer-file-coding-system)
	     ;; Normally this is used only in a non-file-visiting
	     ;; buffer, because normally buffer-file-coding-system is non-nil
	     ;; in a file-visiting buffer.
	     (if buffer-file-type
		 '(no-conversion . no-conversion)
	       '(undecided-dos . undecided-dos)))))))

(modify-coding-system-alist 'file "" 'find-buffer-file-type-coding-system)

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

(defun find-file-not-found-set-buffer-file-coding-system ()
  (save-excursion
    (set-buffer (current-buffer))
    (let ((coding buffer-file-coding-system))
      ;; buffer-file-coding-system is already set by
      ;; find-operation-coding-system, which was called from
      ;; insert-file-contents.  All that's left is to change
      ;; the EOL conversion, if required by the user.
      (when (and (null coding-system-for-read)
		 (or inhibit-eol-conversion
		     (untranslated-file-p (buffer-file-name))))
	(setq coding (coding-system-change-eol-conversion coding 0))
	(setq buffer-file-coding-system coding))
      (setq buffer-file-type (eq buffer-file-coding-system 'no-conversion)))))

;;; To set the default coding system on new files.
(add-hook 'find-file-not-found-hooks
	  'find-file-not-found-set-buffer-file-coding-system)

;;; To accomodate filesystems that do not require CR/LF translation.
(defvar untranslated-filesystem-list nil
  "List of filesystems that require no CR/LF translation when reading
and writing files.  Each filesystem in the list is a string naming
the directory prefix corresponding to the filesystem.")

(defun untranslated-canonical-name (filename)
  "Return FILENAME in a canonicalized form for use with the functions
dealing with untranslated filesystems."
  (if (memq system-type '(ms-dos windows-nt cygwin))
      ;; The canonical form for DOS/W32 is with A-Z downcased and all
      ;; directory separators changed to directory-sep-char.
      (let ((name nil))
	(setq name (mapconcat
		    '(lambda (char)
		       (if (and (<= ?A char) (<= char ?Z))
			   (char-to-string (+ (- char ?A) ?a))
			 (char-to-string char)))
		    filename nil))
	;; Use expand-file-name to canonicalize directory separators, except
	;; with bare drive letters (which would have the cwd appended).
	;; Avoid expanding names that could trigger ange-ftp to prompt
	;; for passwords, though.
	(if (or (string-match "^.:$" name)
		(string-match "^/[^/:]+:" name))
	    name
	  (expand-file-name name)))
    filename))

(defun untranslated-file-p (filename)
  "Return t if FILENAME is on a filesystem that does not require
CR/LF translation, and nil otherwise."
  (let ((fs (untranslated-canonical-name filename))
	(ufs-list untranslated-filesystem-list)
	(found nil))
    (while (and (not found) ufs-list)
      (if (string-match (concat "^" (car ufs-list)) fs)
	  (setq found t)
	(setq ufs-list (cdr ufs-list))))
    found))

(defun add-untranslated-filesystem (filesystem)
  "Add FILESYSTEM to the list of filesystems that do not require
CR/LF translation.  FILESYSTEM is a string containing the directory
prefix corresponding to the filesystem.  For example, for a Unix
filesystem mounted on drive Z:, FILESYSTEM could be \"Z:\"."
  ;; We use "D", not "f", to avoid confusing the user: "f" prompts
  ;; with a directory, but RET returns the current buffer's file, not
  ;; its directory.
  (interactive "DUntranslated file system: ")
  (let ((fs (untranslated-canonical-name filesystem)))
    (if (member fs untranslated-filesystem-list)
	untranslated-filesystem-list
      (setq untranslated-filesystem-list
	    (cons fs untranslated-filesystem-list)))))

(defun remove-untranslated-filesystem (filesystem)
  "Remove FILESYSTEM from the list of filesystems that do not require
CR/LF translation.  FILESYSTEM is a string containing the directory
prefix corresponding to the filesystem.  For example, for a Unix
filesystem mounted on drive Z:, FILESYSTEM could be \"Z:\"."
  (interactive "fUntranslated file system: ")
  (setq untranslated-filesystem-list
	(delete (untranslated-canonical-name filesystem)
		untranslated-filesystem-list)))

;;; Support for printing under DOS/Windows, see lpr.el and ps-print.el.

(defvar direct-print-region-use-command-dot-com t
  "*Control whether command.com is used to print on Windows 9x.")

;; Function to actually send data to the printer port.
;; Supports writing directly, and using various programs.
(defun direct-print-region-helper (printer
				   start end
				   lpr-prog
				   delete-text buf display
				   rest)
  (let* (;; Ignore case when matching known external program names.
	 (case-fold-search t)
	 ;; Convert / to \ in printer name, for sake of external programs.
	 (printer
	  (if (stringp printer)
	      (subst-char-in-string ?/ ?\\ printer)
	    printer))
	 ;; Find a directory that is local, to work-around Windows bug.
	 (safe-dir
	  (let ((safe-dirs (list "c:/" (getenv "windir") (getenv "TMPDIR"))))
	    (while (not (file-attributes (car safe-dirs)))
	      (setq safe-dirs (cdr safe-dirs)))
	    (car safe-dirs)))
	 (tempfile
	  (subst-char-in-string
	   ?/ ?\\
	   (make-temp-name
	    (expand-file-name "EP" temporary-file-directory))))
	 ;; capture output for diagnosis
	 (errbuf (list (get-buffer-create " *print-region-helper*") t)))
    ;; It seems that we must be careful about the directory name that
    ;; gets added to the printer port name by write-region when using
    ;; the standard "PRN" or "LPTx" ports, because the write can fail if
    ;; the directory is on a network drive.  The same is true when
    ;; asking command.com to copy the file.
    ;; No action is needed for UNC printer names, which is just as well
    ;; because `expand-file-name' doesn't support UNC names on MS-DOS.
    (if (and (stringp printer) (not (string-match "^\\\\" printer)))
	(setq printer
	      (subst-char-in-string ?/ ?\\ (expand-file-name printer safe-dir))))
    ;; Handle known programs specially where necessary.
    (unwind-protect
	(cond
	 ;; nprint.exe is the standard print command on Netware
	 ((string-match "^nprint\\(\\.exe\\)?$" (file-name-nondirectory lpr-prog))
	  (write-region start end tempfile nil 0)
	  (call-process lpr-prog nil errbuf nil
			tempfile (concat "P=" printer)))
	 ;; print.exe is a standard command on NT
	 ((string-match "^print\\(\\.exe\\)?$" (file-name-nondirectory lpr-prog))
	  ;; Be careful not to invoke print.exe on MS-DOS or Windows 9x
	  ;; though, because it is a TSR program there (hangs Emacs).
	  (or (and (eq system-type 'windows-nt)
		   (null (getenv "winbootdir")))
	      (error "Printing via print.exe is not supported on MS-DOS or Windows 9x"))
	  ;; It seems that print.exe always appends a form-feed so we
	  ;; should make sure to omit the last FF in the data.
	  (if (and (> end start)
		   (char-equal (char-before end) ?\C-l))
	      (setq end (1- end)))
	  ;; cancel out annotate function for non-PS case
	  (let ((write-region-annotate-functions nil))
	    (write-region start end tempfile nil 0))
	  (call-process lpr-prog nil errbuf nil
			(concat "/D:" printer) tempfile))
	 ;; support lpr and similar programs for convenience, but
	 ;; supply an explicit filename because the NT version of lpr
	 ;; can't read from stdin.
	 ((> (length lpr-prog) 0)
	  (write-region start end tempfile nil 0)
	  (setq rest (append rest (list tempfile)))
	  (apply 'call-process lpr-prog nil errbuf nil rest))
	 ;; Run command.com to access printer port on Windows 9x, unless
	 ;; we are supposed to append to an existing (non-empty) file,
	 ;; to work around a bug in Windows 9x that prevents Win32
	 ;; programs from accessing LPT ports reliably.
	 ((and (eq system-type 'windows-nt)
	       (getenv "winbootdir")
	       ;; Allow cop-out so command.com isn't invoked
	       direct-print-region-use-command-dot-com
	       ;; file-attributes fails on LPT ports on Windows 9x but
	       ;; not on NT, so handle both cases for safety.
	       (eq (or (nth 7 (file-attributes printer)) 0) 0))
	  (write-region start end tempfile nil 0)
	  (let ((w32-quote-process-args nil))
	    (call-process "command.com" nil errbuf nil "/c"
			  (format "copy /b %s %s" tempfile printer))))
	 ;; write directly to the printer port
	 (t
	  (write-region start end printer t 0)))
      ;; ensure we remove the tempfile if created
      (if (file-exists-p tempfile)
	  (delete-file tempfile)))))

(defvar printer-name)

(defun direct-print-region-function (start end
					   &optional lpr-prog
					   delete-text buf display
					   &rest rest)
  "DOS/Windows-specific function to print the region on a printer.
Writes the region to the device or file which is a value of
`printer-name' \(which see\), unless the value of `lpr-command'
indicates a specific program should be invoked."

  ;; DOS printers need the lines to end with CR-LF pairs, so make
  ;; sure it always happens that way, unless the buffer is binary.
  (let* ((coding coding-system-for-write)
	 (coding-base
	  (if (null coding) 'undecided (coding-system-base coding)))
	 (eol-type (coding-system-eol-type coding-base))
	 ;; Make each print-out eject the final page, but don't waste
	 ;; paper if the file ends with a form-feed already.
	 (write-region-annotate-functions
	  (cons
	   (lambda (start end)
	     (if (not (char-equal (char-before end) ?\C-l))
		 `((,end . "\f"))))
	   write-region-annotate-functions))
	 (printer (or (and (boundp 'dos-printer)
			   (stringp (symbol-value 'dos-printer))
			   (symbol-value 'dos-printer))
		      printer-name
		      (default-printer-name))))
    (or (eq coding-system-for-write 'no-conversion)
	(setq coding-system-for-write
	      (aref eol-type 1)))	; force conversion to DOS EOLs
    (direct-print-region-helper printer start end lpr-prog
				delete-text buf display rest)))

(setq print-region-function 'direct-print-region-function)

;; Set this to nil if you have a port of the `pr' program
;; (e.g., from GNU Textutils), or if you have an `lpr'
;; program (see above) that can print page headers.
;; If `lpr-headers-switches' is non-nil (the default) and
;; `print-region-function' is set to `dos-print-region-function',
;; then requests to print page headers will be silently
;; ignored, and `print-buffer' and `print-region' produce
;; the same output as `lpr-buffer' and `lpr-region', accordingly.
(setq lpr-headers-switches "(page headers are not supported)")

(defvar ps-printer-name)

(defun direct-ps-print-region-function (start end
					      &optional lpr-prog
					      delete-text buf display
					      &rest rest)
  "DOS/Windows-specific function to print the region on a PostScript printer.
Writes the region to the device or file which is a value of
`ps-printer-name' \(which see\), unless the value of `ps-lpr-command'
indicates a specific program should be invoked."

  (let ((printer (or (and (boundp 'dos-ps-printer)
			  (stringp (symbol-value 'dos-ps-printer))
			  (symbol-value 'dos-ps-printer))
		     ps-printer-name
		     (default-printer-name))))
    (direct-print-region-helper printer start end lpr-prog
				delete-text buf display rest)))

(setq ps-print-region-function 'direct-ps-print-region-function)

;(setq ps-lpr-command "gs")

;(setq ps-lpr-switches '("-q" "-dNOPAUSE" "-sDEVICE=epson" "-r240x60"
;			  "-sOutputFile=LPT1"))

(provide 'dos-w32)

;;; arch-tag: dcfefdd2-362f-4fbc-9141-9634f5f4d6a7
;;; dos-w32.el ends here
