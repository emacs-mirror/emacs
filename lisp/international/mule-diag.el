;;; mule-diag.el --- Show diagnosis of multilingual environment (MULE)

;; Copyright (C) 1995 Free Software Foundation, Inc.
;; Copyright (C) 1995 Electrotechnical Laboratory, JAPAN.

;; Keywords: multilingual, charset, coding system, fontset, diagnosis

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

;;; General utility function

;; Print all arguments with single space separator in one line.
(defun print-list (&rest args)
  (while (cdr args)
    (if (car args)
	(progn (princ (car args)) (princ " ")))
    (setq args (cdr args)))
  (princ (car args))
  (princ "\n"))

;;; CHARSET

;;;###autoload
(defun list-character-sets ()
  "Display a list of all charsets."
  (interactive)
  (with-output-to-temp-buffer "*Help*"
    (print-character-sets)
    (save-excursion
      (set-buffer standard-output)
      (help-mode))))

(defvar charset-other-info-func nil)
  
(defun print-character-sets ()
  "Print information on all charsets in a machine readable format."
  (princ "\
#########################
## LIST OF CHARSETS
## Each line corresponds to one charset.
## The following attributes are listed in this order
## separated by a colon `:' in one line.
##	CHARSET-SYMBOL-NAME,
##	CHARSET-ID,
##	DIMENSION (1 or 2)
##	CHARS (94 or 96)
##	BYTES (of multibyte form: 1, 2, 3, or 4),
##	WIDTH (occupied column numbers: 1 or 2),
##	DIRECTION (0:left-to-right, 1:right-to-left),
##	ISO-FINAL-CHAR (character code of ISO-2022's final character)
##	ISO-GRAPHIC-PLANE (ISO-2022's graphic plane, 0:GL, 1:GR)
##	DESCRIPTION (describing string of the charset)
")
  (let ((charsets charset-list)
	charset)
    (while charsets
      (setq charset (car charsets))
      (princ (format "%s:%03d:%d:%d:%d:%d:%d:%d:%d:%s\n" 
		     charset
		     (charset-id charset)
		     (charset-dimension charset)
		     (charset-chars charset)
		     (charset-bytes charset)
		     (charset-width charset)
		     (charset-direction charset)
		     (charset-iso-final-char charset)
		     (charset-iso-graphic-plane charset)
		     (charset-description charset)))
      (setq charsets (cdr charsets)))))


;;; CODING-SYSTEM

;; Print information of designation of each graphic register in FLAGS
;; in human readable format.  See the documentation of
;; `make-coding-system' for the meaning of FLAGS.
(defun print-designation (flags)
  (let ((graphic-register 0)
	charset)
    (while (< graphic-register 4)
      (setq charset (aref flags graphic-register))
      (princ (format
	      "  G%d -- %s\n"
	      graphic-register
	      (cond ((null charset)
		     "never used")
		    ((eq charset t)
		     "no initial designation, and used by any charsets")
		    ((symbolp charset)
		     (format "%s:%s"
			     charset (charset-description charset)))
		    ((listp charset)
		     (if (charsetp (car charset))
			 (format "%s:%s, and also used by the followings:"
				 (car charset)
				 (charset-description (car charset)))
		       "no initial designation, and used by the followings:"))
		    (t
		     "invalid designation information"))))
      (if (listp charset)
	  (progn
	    (setq charset (cdr charset))
	    (while charset
	      (cond ((eq (car charset) t)
		     (princ "\tany other charsets\n"))
		    ((charsetp (car charset))
		     (princ (format "\t%s:%s\n"
				    (car charset)
				    (charset-description (car charset)))))
		    (t
		     "invalid designation information"))		   
	      (setq charset (cdr charset)))))
      (setq graphic-register (1+ graphic-register)))))

;;;###autoload
(defun describe-coding-system (coding-system)
  "Display information of CODING-SYSTEM."
  (interactive "zDescribe coding system (default, current choices): ")
  (if (null coding-system)
      (describe-current-coding-system)
    (with-output-to-temp-buffer "*Help*"
      (print-coding-system-briefly coding-system 'doc-string)
      (let ((coding-spec (coding-system-spec coding-system)))
	(princ "Type: ")
	(let ((type (coding-system-type coding-system))
	      (flags (coding-system-flags coding-system)))
	  (princ type)
	  (cond ((eq type nil)
		 (princ " (do no conversion)"))
		((eq type t)
		 (princ " (do automatic conversion)"))
		((eq type 0)
		 (princ " (Emacs internal multibyte form)"))
		((eq type 1)
		 (princ " (Shift-JIS, MS-KANJI)"))
		((eq type 2)
		 (princ " (variant of ISO-2022)\n")
		 (princ "Initial designations:\n")
		 (print-designation flags)
		 (princ "Other Form: \n  ")
		 (princ (if (aref flags 4) "short-form" "long-form"))
		 (if (aref flags 5) (princ ", ASCII@EOL"))
		 (if (aref flags 6) (princ ", ASCII@CNTL"))
		 (princ (if (aref flags 7) ", 7-bit" ", 8-bit"))
		 (if (aref flags 8) (princ ", use-locking-shift"))
		 (if (aref flags 9) (princ ", use-single-shift"))
		 (if (aref flags 10) (princ ", use-roman"))
		 (if (aref flags 10) (princ ", use-old-jis"))
		 (if (aref flags 11) (princ ", no-ISO6429"))
		 (princ "."))
		((eq type 3)
		 (princ " (Big5)"))
		((eq type 4)
		 (princ " (do conversion by CCL program)"))
		(t (princ "invalid coding-system."))))
	(princ "\nEOL type:\n  ")
	(let ((eol-type (coding-system-eol-type coding-system)))
	  (cond ((vectorp eol-type)
		 (princ "Automatic selection from:\n\t")
		 (princ eol-type)
		 (princ "\n"))
		((or (null eol-type) (eq eol-type 0)) (princ "LF\n"))
		((eq eol-type 1) (princ "CRLF\n"))
		((eq eol-type 2) (princ "CR\n"))
		(t (princ "invalid\n")))))
      (save-excursion
	(set-buffer standard-output)
	(help-mode)))))

;;;###autoload
(defun describe-current-coding-system-briefly ()
  "Display coding systems currently used in a brief format in echo area.

The format is \"F[..],K[..],T[..],P>[..],P<[..], default F[..],P<[..],P<[..]\",
where mnemonics of the following coding systems come in this order
at the place of `..':
  buffer-file-coding-system (of the current buffer)
  eol-type of buffer-file-coding-system (of the current buffer)
  (keyboard-coding-system)
  eol-type of (keyboard-coding-system)
  (terminal-coding-system)
  eol-type of (terminal-coding-system)
  process-coding-system for read (of the current buffer, if any)
  eol-type of process-coding-system for read (of the current buffer, if any)
  process-coding-system for write (of the current buffer, if any)
  eol-type of process-coding-system for write (of the current buffer, if any)
  default-buffer-file-coding-system
  eol-type of default-buffer-file-coding-system
  default-process-coding-system for read
  eol-type of default-process-coding-system for read
  default-process-coding-system for write
  eol-type of default-process-coding-system"
  (interactive)
  (let* ((proc (get-buffer-process (current-buffer)))
	 (process-coding-systems (if proc (process-coding-system proc))))
    (message
     "F[%c%c],K[%c%c],T[%c%c],P>[%c%c],P<[%c%c], default F[%c%c],P>[%c%c],P<[%c%c]"
     (coding-system-mnemonic buffer-file-coding-system)
     (coding-system-eol-type-mnemonic buffer-file-coding-system)
     (coding-system-mnemonic (keyboard-coding-system))
     (coding-system-eol-type-mnemonic (keyboard-coding-system))
     (coding-system-mnemonic (terminal-coding-system))
     (coding-system-eol-type-mnemonic (terminal-coding-system))
     (coding-system-mnemonic (car process-coding-systems))
     (coding-system-eol-type-mnemonic (car process-coding-systems))
     (coding-system-mnemonic (cdr process-coding-systems))
     (coding-system-eol-type-mnemonic (cdr process-coding-systems))
     (coding-system-mnemonic default-buffer-file-coding-system)
     (coding-system-eol-type-mnemonic default-buffer-file-coding-system)
     (coding-system-mnemonic (car default-process-coding-system))
     (coding-system-eol-type-mnemonic (car default-process-coding-system))
     (coding-system-mnemonic (cdr default-process-coding-system))
     (coding-system-eol-type-mnemonic (cdr default-process-coding-system))
     )))

;; Print symbol name and mnemonic letter of CODING-SYSTEM by `princ'.
(defun print-coding-system-briefly (coding-system &optional doc-string)
  (if (not coding-system)
      (princ "nil\n")
    (princ (format "%c -- %s"
		   (coding-system-mnemonic coding-system)
		   coding-system))
    (let ((parent (coding-system-parent coding-system)))
      (if parent
	  (princ (format " (alias of %s)" parent))))
    (let ((aliases (get coding-system 'alias-coding-systems)))
      (if aliases
	  (princ (format " %S" (cons 'alias: aliases)))))
    (princ "\n")
    (if (and doc-string
	     (setq doc-string (coding-system-doc-string coding-system)))
	(princ (format "  %s\n" doc-string)))))

;;;###autoload
(defun describe-current-coding-system ()
  "Display coding systems currently used in a detailed format."
  (interactive)
  (with-output-to-temp-buffer "*Help*"
    (let* ((proc (get-buffer-process (current-buffer)))
	   (process-coding-systems (if proc (process-coding-system proc))))
      (princ "Current buffer file: buffer-file-coding-system\n  ")
      (if (local-variable-p 'buffer-file-coding-system)
	  (print-coding-system-briefly buffer-file-coding-system)
	(princ "Not set locally, use the following default.\n"))
      (princ "Default buffer file: default-buffer-file-coding-system\n  ")
      (print-coding-system-briefly default-buffer-file-coding-system)
      (princ "Keyboard: (keyboard-coding-system)\n  ")
      (print-coding-system-briefly (keyboard-coding-system))
      (princ "Terminal: (display-coding-system)\n  ")
      (print-coding-system-briefly (terminal-coding-system))
      (princ "Current buffer process: (process-coding-system)\n")
      (if (not process-coding-systems)
	  (princ "  No process.\n")
	(princ "  decoding: ")
	(print-coding-system-briefly (car process-coding-systems))
	(princ "  encoding: ")
	(print-coding-system-briefly (cdr process-coding-systems)))
      (princ "Default process: default-process-coding-system\n")
      (princ "  decoding: ")
      (print-coding-system-briefly (car default-process-coding-system))
      (princ "  encoding: ")
      (print-coding-system-briefly (cdr default-process-coding-system)))

    (save-excursion
      (set-buffer standard-output)

      (princ "\nPriority order of coding systems:\n")
      (let ((l coding-category-list)
	    (i 1)
	    coding aliases)
	(while l
	  (setq coding (symbol-value (car l)))
	  (princ (format "  %d. %s" i coding))
	  (if (setq aliases (get coding 'alias-coding-systems))
	      (progn
		(princ " ")
		(princ (cons 'alias: aliases))))
	  (terpri)
	  (setq l (cdr l) i (1+ i))))
      (princ "\n  Other coding systems cannot be distinguished automatically
  from these, and therefore cannot be recognized automatically
  with the present coding system priorities.\n\n")

      (let ((categories '(coding-category-iso-7 coding-category-iso-else))
	    coding-system codings)
	(while categories
	  (setq coding-system (symbol-value (car categories)))
	  (mapcar
	   (function
	    (lambda (x)
	      (if (and (not (eq x coding-system))
		       (get x 'no-initial-designation)
		       (let ((flags (coding-system-flags x)))
			 (not (or (aref flags 10) (aref flags 11)))))
		  (setq codings (cons x codings)))))
	   (get (car categories) 'coding-systems))
	  (if codings
	      (let ((max-col (frame-width))
		    pos)
		(princ (format "  The followings are decoded correctly but recognized as %s:\n   " coding-system))
		(while codings
		  (setq pos (point))
		  (insert (format " %s" (car codings)))
		  (if (> (current-column) max-col)
		      (progn
		       (goto-char pos)
		       (insert "\n   ")
		       (goto-char (point-max))))
		  (setq codings (cdr codings)))
		(insert "\n\n")))
	  (setq categories (cdr categories))))

      (princ "Look up tables for finding a coding system on I/O operations:\n")
      (terpri)
      (princ "  OPERATION\tTARGET PATTERN\t\tCODING SYSTEM(s)\n")
      (princ "  ---------\t--------------\t\t----------------\n")
      (let ((func (lambda (operation alist)
		    (princ "  ")
		    (princ operation)
		    (if (not alist)
			(princ "\tnothing specified\n")
		      (while alist
			(indent-to 16)
			(prin1 (car (car alist)))
			(indent-to 40)
			(princ (cdr (car alist)))
			(princ "\n")
			(setq alist (cdr alist)))))))
	(funcall func "File I/O" file-coding-system-alist)
	(funcall func "Process I/O" process-coding-system-alist)
	(funcall func "Network I/O" network-coding-system-alist))
      (help-mode))))

;; Print detailed information on CODING-SYSTEM.
(defun print-coding-system (coding-system &optional aliases)
  (let ((type (coding-system-type coding-system))
	(eol-type (coding-system-eol-type coding-system))
	(flags (coding-system-flags coding-system))
	(base (coding-system-base coding-system)))
    (if (not (eq base coding-system))
	(princ (format "%s (alias of %s)\n" coding-system base))
      (princ coding-system)
      (while aliases
	(progn
	  (princ ",")
	  (princ (car aliases))
	  (setq aliases (cdr aliases))))
      (princ (format ":%s:%c:%d:"
		     type
		     (coding-system-mnemonic coding-system)
		     (if (integerp eol-type) eol-type 3)))
      (cond ((eq type 2)		; ISO-2022
	     (let ((idx 0)
		   charset)
	       (while (< idx 4)
		 (setq charset (aref flags idx))
		 (cond ((null charset)
			(princ -1))
		       ((eq charset t)
			(princ -2))
		       ((charsetp charset)
			(princ charset))
		       ((listp charset)
			(princ "(")
			(princ (car charset))
			(setq charset (cdr charset))
			(while charset
			  (princ ",")
			  (princ (car charset))
			  (setq charset (cdr charset)))
			(princ ")")))
		 (princ ",")
		 (setq idx (1+ idx)))
	       (while (< idx 12)
		 (princ (if (aref flags idx) 1 0))
		 (princ ",")
		 (setq idx (1+ idx)))
	       (princ (if (aref flags idx) 1 0))))
	    ((eq type 4)		; CCL
	     (let (i len)
	       (setq i 0 len (length (car flags)))
	       (while (< i len)
		 (princ (format " %x" (aref (car flags) i)))
		 (setq i (1+ i)))
	       (princ ",")
	       (setq i 0 len (length (cdr flags)))
	       (while (< i len)
		 (princ (format " %x" (aref (cdr flags) i)))
		 (setq i (1+ i)))))
	    (t (princ 0)))
      (princ ":")
      (princ (coding-system-doc-string coding-system))
      (princ "\n"))))

;;;###autoload
(defun list-coding-systems ()
  "Print information of all base coding systems.
If called interactive, it prints name, mnemonic letter, and doc-string
of each coding system.
If not, it prints whole information of each coding system
with the format which is more suitable for being read by a machine,
in addition, it prints list of coding category ordered by priority."
  (interactive)
  (with-output-to-temp-buffer "*Help*"
    (if (interactive-p)
	(princ "\
###############################################
# List of coding systems in the following format:
# MNEMONIC-LETTER -- CODING-SYSTEM-NAME
#	DOC-STRING
")
      (princ "\
#########################
## LIST OF CODING SYSTEMS
## Each line corresponds to one coding system
## Format of a line is:
##   NAME[,ALIAS...]:TYPE:MNEMONIC:EOL:FLAGS:POST-READ-CONVERSION
##	:PRE-WRITE-CONVERSION:DOC-STRING,
## where
##  NAME = coding system name
##  ALIAS = alias of the coding system
##  TYPE = nil (no conversion), t (undecided or automatic detection),
##         0 (EMACS-MULE), 1 (SJIS), 2 (ISO2022), 3 (BIG5), or 4 (CCL)
##  EOL = 0 (LF), 1 (CRLF), 2 (CR), or 3 (Automatic detection)
##  FLAGS =
##    if TYPE = 2 then
##      comma (`,') separated data of the followings:
##        G0, G1, G2, G3, SHORT-FORM, ASCII-EOL, ASCII-CNTL, SEVEN,
##        LOCKING-SHIFT, SINGLE-SHIFT, USE-ROMAN, USE-OLDJIS, NO-ISO6429
##    else if TYPE = 4 then
##      comma (`,') separated CCL programs for read and write
##    else
##      0
##  POST-READ-CONVERSION, PRE-WRITE-CONVERSION = function name to be called
##
"))
    (let ((bases (coding-system-list 'base-only))
	  coding-system)
      (while bases
	(setq coding-system (car bases))
	(if (interactive-p)
	    (print-coding-system-briefly coding-system 'doc-string)
	  (print-coding-system coding-system))
	(setq bases (cdr bases))))
    (if (interactive-p)
	nil
      (princ "\
############################
## LIST OF CODING CATEGORIES (ordered by priority)
## CATEGORY:CODING-SYSTEM
##
")
      (let ((l coding-category-list))
	(while l
	  (princ (format "%s:%s\n" (car l) (symbol-value (car l))))
	  (setq l (cdr l)))))
    ))

;;; FONT

;; Print information of a font in FONTINFO.
(defun describe-font-internal (font-info &optional verbose)
  (print-list "name (opened by):" (aref font-info 0))
  (print-list "       full name:" (aref font-info 1))
  (let ((charset (aref font-info 2)))
    (print-list "   charset:"
		(format "%s (%s)" charset (charset-description charset))))
  (print-list "            size:" (format "%d" (aref font-info 3)))
  (print-list "          height:" (format "%d" (aref font-info 4)))
  (print-list " baseline-offset:" (format "%d" (aref font-info 5)))
  (print-list "relative-compose:" (format "%d" (aref font-info 6))))

;;;###autoload
(defun describe-font (fontname)
  "Display information about fonts which partially match FONTNAME."
  (interactive "sFontname: ")
  (or window-system
      (error "No window system being used"))
  (let ((font-info (font-info fontname)))
    (if (null font-info)
	(message "No matching font")
      (with-output-to-temp-buffer "*Help*"
	(describe-font-internal font-info 'verbose)))))

;; Print information in FONTINFO of a fontset named FONTSET.
(defun describe-fontset-internal (fontset fontset-info)
  (print-list "Fontset:" fontset)
  (let ((size (aref fontset-info 0)))
    (print-list "  size:" (format "%d" size)
		(if (= size 0) "... which means not yet used" "")))
  (print-list "  height:" (format "%d" (aref fontset-info 1)))
  (print-list "  fonts: (charset : font name)")
  (let* ((fonts (aref fontset-info 2))
	 elt charset requested opened)
    (while fonts
      (setq elt (car fonts)
	    charset (car elt)
	    requested (nth 1 elt)
	    opened (nth 2 elt))
      (print-list "   " charset ":" requested)
      (if (stringp opened)
	  (print-list "      Opened as: " opened)
	(if (null opened) "      -- open failed --"))
      (setq fonts (cdr fonts)))))

;;;###autoload
(defun describe-fontset (fontset)
  "Display information about FONTSET."
  (interactive
   (if (not window-system)
       (error "No window system being used")
     (let ((fontset-list (mapcar '(lambda (x) (list x)) (fontset-list))))
       (list (completing-read "Fontset: " fontset-list)))))
  (setq fontset (query-fontset fontset))
  (if (null fontset)
      (error "No matching fontset")
    (let ((fontset-info (fontset-info fontset)))
      (with-output-to-temp-buffer "*Help*"
	(describe-fontset-internal fontset fontset-info)))))

;;;###autoload
(defun list-input-methods ()
  "Print information of all input methods."
  (interactive)
  (with-output-to-temp-buffer "*Help*"
    (princ "LANGUAGE\n  NAME (`TITLE' in mode line)\n")
    (princ "    SHORT-DESCRIPTION\n------------------------------\n")
    (setq input-method-alist
	  (sort input-method-alist
		(function (lambda (x y) (string< (nth 1 x) (nth 1 y))))))
    (let ((l input-method-alist)
	  language elt)
      (while l
	(setq elt (car l) l (cdr l))
	(if (not (equal language (nth 1 elt)))
	    (progn
	      (setq language (nth 1 elt))
	      (princ language)
	      (terpri)))
	(princ (format "  %s (`%s' in mode line)\n    %s\n"
		       (car elt) (nth 3 elt)
		       (let ((title (nth 4 elt)))
			 (string-match ".*" title)
			 (match-string 0 title))))))))

;;; DIAGNOSIS

(defun insert-list (args)
  (while (cdr args)
    (insert (or (car args) "nil") " ")
    (setq args (cdr args)))
  (if args (insert (or (car args) "nil")))
  (insert "\n"))

(defun insert-section (sec title)
  (insert "########################################\n"
	  "# Section " (format "%d" sec) ".  " title "\n"
	  "########################################\n\n"))

;;;###autoload
(defun mule-diag ()
  "Show diagnosis of the running Mule."
  (interactive)
  (let ((buf (get-buffer-create "*Diagnosis*")))
    (save-excursion
      (set-buffer buf)
      (erase-buffer)
      (insert "\t###############################\n"
	      "\t### Diagnosis of your Emacs ###\n"
	      "\t###############################\n\n"
	      "CONTENTS: Section 1.  General Information\n"
	      "          Section 2.  Display\n"
	      "          Section 3.  Input methods\n"
	      "          Section 4.  Coding systems\n"
	      "          Section 5.  Charsets\n")
      (if window-system
	  (insert "          Section 6.  Fontset list\n"))
      (insert "\n")

      (insert-section 1 "General Information")
      (insert "Version of this emacs:\n  " (emacs-version) "\n"
	      "Primary language:\n  " primary-language "\n\n")

      (insert-section 2 "Display")
      (if window-system
	  (insert "Window-system: "
		  (symbol-name window-system)
		  (format "%s" window-system-version))
	(insert "Terminal: " (getenv "TERM")))
      (insert "\n\n")

      (if (eq window-system 'x)
	  (let ((font (cdr (assq 'font (frame-parameters)))))
	    (insert "The selected frame is using the "
		    (if (query-fontset font) "fontset" "font")
		    ":\n\t" font))
	(insert "Coding system of the terminal: "
		(symbol-name (terminal-coding-system))))
      (insert "\n\n")

      (insert-section 3 "Input methods")
      (save-excursion (list-input-methods))
      (insert-buffer "*Help*")
      (goto-char (point-max))
      (insert "\n")
      (if default-input-method
	  (insert "Default input method: %s\n" default-input-method)
	(insert "No default input method is specified.\n"))

      (insert-section 4 "Coding systems")
      (save-excursion (list-coding-systems))
      (insert-buffer "*Help*")
      (goto-char (point-max))
      (insert "\n")

      (insert-section 5 "Charsets")
      (save-excursion (list-character-sets))
      (insert-buffer "*Help*")
      (goto-char (point-max))
      (insert "\n")

      (if window-system
	  (let ((fontsets (fontset-list)))
	    (insert-section 6 "Fontset list")
	    (while fontsets
	      (describe-fontset (car fontsets))
	      (insert-buffer "*Help*")
	      (setq fontsets (cdr fontsets)))))

      (set-buffer-modified-p nil)
      )
    (let ((win (display-buffer buf)))
      (set-window-point win 1)
      (set-window-start win 1))
    ))


;;; DUMP DATA FILE

;;;###autoload
(defun dump-charsets ()
  "Dump information of all charsets into the file \"charsets.dat\"."
  (list-character-sets)
  (set-buffer (get-buffer "*Help*"))
  (let (make-backup-files)
    (write-region (point-min) (point-max) "charsets.dat"))
  (kill-emacs))

;;;###autoload
(defun dump-codings ()
  "Dump information of all coding systems into the file \"codings.dat\"."
  (list-coding-systems)
  (set-buffer (get-buffer "*Help*"))
  (let (make-backup-files)
    (write-region (point-min) (point-max) "codings.dat"))
  (kill-emacs))

;;; mule-diag.el ends here
