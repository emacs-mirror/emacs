;;; mule-diag.el --- Show diagnosis of multilingual environment (Mule)

;; Copyright (C) 1995 Electrotechnical Laboratory, JAPAN.
;; Licensed to the Free Software Foundation.

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
    (when (car args)
      (princ (car args))
      (princ " "))
    (setq args (cdr args)))
  (princ (car args))
  (princ "\n"))

;; Re-order the elements of charset-list.
(defun sort-charset-list ()
  (setq charset-list
	(sort charset-list
	      (function (lambda (x y) (< (charset-id x) (charset-id y)))))))

;;; CHARSET

;;;###autoload
(defun list-character-sets (&optional arg)
  "Display a list of all character sets.

The ID column contains a charset identification number for internal Emacs use.
The B column contains a number of bytes occupied in a buffer
  by any character in this character set.
The W column contains a number of columns occupied on the screen
  by any character in this character set.

With prefix arg, the output format gets more cryptic,
but still shows the full information."
  (interactive "P")
  (sort-charset-list)
  (with-output-to-temp-buffer "*Help*"
    (save-excursion
      (set-buffer standard-output)
      (list-character-sets-1 arg)
      (help-mode)
      (setq truncate-lines t))))

(defun list-character-sets-1 (arg)
  (let ((l charset-list)
	charset)
    (if (null arg)
	(progn
	  (insert "ID  Name		    B W Description\n")
	  (insert "--  ----		    - - -----------\n")
	  (while l
	    (setq charset (car l) l (cdr l))
	    (insert (format "%03d %s" (charset-id charset) charset))
	    (indent-to 28)
	    (insert (format "%d %d %s\n"
			    (charset-bytes charset)
			    (charset-width charset)
			    (charset-description charset)))))
      (insert "\
#########################
## LIST OF CHARSETS
## Each line corresponds to one charset.
## The following attributes are listed in this order
## separated by a colon `:' in one line.
##	CHARSET-ID,
##	CHARSET-SYMBOL-NAME,
##	DIMENSION (1 or 2)
##	CHARS (94 or 96)
##	BYTES (of multibyte form: 1, 2, 3, or 4),
##	WIDTH (occupied column numbers: 1 or 2),
##	DIRECTION (0:left-to-right, 1:right-to-left),
##	ISO-FINAL-CHAR (character code of ISO-2022's final character)
##	ISO-GRAPHIC-PLANE (ISO-2022's graphic plane, 0:GL, 1:GR)
##	DESCRIPTION (describing string of the charset)
")
      (while l
	(setq charset (car l) l (cdr l))
	(princ (format "%03d:%s:%d:%d:%d:%d:%d:%d:%d:%s\n" 
		       (charset-id charset)
		       charset
		       (charset-dimension charset)
		       (charset-chars charset)
		       (charset-bytes charset)
		       (charset-width charset)
		       (charset-direction charset)
		       (charset-iso-final-char charset)
		       (charset-iso-graphic-plane charset)
		       (charset-description charset)))))))

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
      (when (listp charset)
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
	  (setq charset (cdr charset))))
      (setq graphic-register (1+ graphic-register)))))

;;;###autoload
(defun describe-coding-system (coding-system)
  "Display information about CODING-SYSTEM."
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
		 (if (aref flags 11) (princ ", use-old-jis"))
		 (if (aref flags 12) (princ ", no-ISO6429"))
		 (if (aref flags 13) (princ ", init-bol"))
		 (if (aref flags 14) (princ ", designation-bol"))
		 (if (aref flags 15) (princ ", convert-unsafe"))
		 (if (aref flags 16) (princ ", accept-latin-extra-code"))
		 (princ "."))
		((eq type 3)
		 (princ " (Big5)"))
		((eq type 4)
		 (princ " (do conversion by CCL program)"))
		((eq type 5)
		 (princ " (text with random binary characters)"))
		(t (princ ": invalid coding-system."))))
	(princ "\nEOL type: ")
	(let ((eol-type (coding-system-eol-type coding-system)))
	  (cond ((vectorp eol-type)
		 (princ "Automatic selection from:\n\t")
		 (princ eol-type)
		 (princ "\n"))
		((or (null eol-type) (eq eol-type 0)) (princ "LF\n"))
		((eq eol-type 1) (princ "CRLF\n"))
		((eq eol-type 2) (princ "CR\n"))
		(t (princ "invalid\n")))))
      (let ((postread (coding-system-get coding-system 'post-read-conversion)))
	(when postread
	  (princ "After decoding a text normally,")
	  (princ " perform post-conversion by the function: ")
	  (princ "\n  ")
	  (princ postread)
	  (princ "\n")))
      (let ((prewrite (coding-system-get coding-system 'pre-write-conversion)))
	(when prewrite
	  (princ "Before encoding a text normally,")
	  (princ " perform pre-conversion by the function: ")
	  (princ "\n  ")
	  (princ prewrite)
	  (princ "\n")))
      (let ((charsets (coding-system-get coding-system 'safe-charsets)))
	(when charsets
	  (if (eq charsets t)
	      (princ "This coding system can encode all charsets.\n")
	    (princ "This coding system encode the following charsets:\n")
	    (princ " ")
	    (while charsets
	      (princ " ")
	      (princ (car charsets))
	      (setq charsets (cdr charsets))))))
      (save-excursion
	(set-buffer standard-output)
	(help-mode)))))

;;;###autoload
(defun describe-current-coding-system-briefly ()
  "Display coding systems currently used in a brief format in echo area.

The format is \"F[..],K[..],T[..],P>[..],P<[..], default F[..],P<[..],P<[..]\",
where mnemonics of the following coding systems come in this order
at the place of `..':
  `buffer-file-coding-system` (of the current buffer)
  eol-type of buffer-file-coding-system (of the current buffer)
  Value returned by `keyboard-coding-system'
  eol-type of (keyboard-coding-system)
  Value returned by `terminal-coding-system.
  eol-type of (terminal-coding-system)
  `process-coding-system' for read (of the current buffer, if any)
  eol-type of process-coding-system for read (of the current buffer, if any)
  `process-coding-system' for write (of the current buffer, if any)
  eol-type of process-coding-system for write (of the current buffer, if any)
  `default-buffer-file-coding-system'
  eol-type of default-buffer-file-coding-system
  `default-process-coding-system' for read
  eol-type of default-process-coding-system for read
  `default-process-coding-system' for write
  eol-type of default-process-coding-system"
  (interactive)
  (let* ((proc (get-buffer-process (current-buffer)))
	 (process-coding-systems (if proc (process-coding-system proc))))
    (message
     "F[%c%s],K[%c%s],T[%c%s],P>[%c%s],P<[%c%s], default F[%c%s],P>[%c%s],P<[%c%s]"
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

;; Print symbol name and mnemonic letter of CODING-SYSTEM with `princ'.
(defun print-coding-system-briefly (coding-system &optional doc-string)
  (if (not coding-system)
      (princ "nil\n")
    (princ (format "%c -- %s"
		   (coding-system-mnemonic coding-system)
		   coding-system))
    (let ((aliases (coding-system-get coding-system 'alias-coding-systems)))
      (if (eq coding-system (car aliases))
	  (if (cdr aliases)
	      (princ (format " %S" (cons 'alias: (cdr aliases)))))
	(if (memq coding-system aliases)
	    (princ (format " (alias of %s)" (car aliases))))))
    (princ "\n")
    (if (and doc-string
	     (setq doc-string (coding-system-doc-string coding-system)))
	(princ (format "  %s\n" doc-string)))))

;;;###autoload
(defun describe-current-coding-system ()
  "Display coding systems currently used, in detail."
  (interactive)
  (with-output-to-temp-buffer "*Help*"
    (let* ((proc (get-buffer-process (current-buffer)))
	   (process-coding-systems (if proc (process-coding-system proc))))
      (princ "Coding system for saving this buffer:\n  ")
      (if (local-variable-p 'buffer-file-coding-system)
	  (print-coding-system-briefly buffer-file-coding-system)
	(princ "Not set locally, use the default.\n"))
      (princ "Default coding system (for new files):\n  ")
      (print-coding-system-briefly default-buffer-file-coding-system)
      (princ "Coding system for keyboard input:\n  ")
      (print-coding-system-briefly (keyboard-coding-system))
      (princ "Coding system for terminal output:\n  ")
      (print-coding-system-briefly (terminal-coding-system))
      (when (get-buffer-process (current-buffer))
	(princ "Coding systems for process I/O:\n")
	(princ "  encoding input to the process: ")
	(print-coding-system-briefly (cdr process-coding-systems))
	(princ "  decoding output from the process: ")
	(print-coding-system-briefly (car process-coding-systems)))
      (princ "Defaults for subprocess I/O:\n")
      (princ "  decoding: ")
      (print-coding-system-briefly (car default-process-coding-system))
      (princ "  encoding: ")
      (print-coding-system-briefly (cdr default-process-coding-system)))

    (save-excursion
      (set-buffer standard-output)

      (princ "\nPriority order for recognizing coding systems when reading files:\n")
      (let ((l coding-category-list)
	    (i 1)
	    (coding-list nil)
	    coding aliases)
	(while l
	  (setq coding (symbol-value (car l)))
	  ;; Do not list up the same coding system twice.
	  (when (and coding (not (memq coding coding-list)))
	    (setq coding-list (cons coding coding-list))
	    (princ (format "  %d. %s " i coding))
	    (setq aliases (coding-system-get coding 'alias-coding-systems))
	    (if (eq coding (car aliases))
		(if (cdr aliases)
		    (princ (cons 'alias: (cdr aliases))))
	      (if (memq coding aliases)
		  (princ (list 'alias 'of (car aliases)))))
	    (terpri)
	    (setq i (1+ i)))
	  (setq l (cdr l))))

      (princ "\n  Other coding systems cannot be distinguished automatically
  from these, and therefore cannot be recognized automatically
  with the present coding system priorities.\n\n")

      (let ((categories '(coding-category-iso-7 coding-category-iso-7-else))
	    coding-system codings)
	(while categories
	  (setq coding-system (symbol-value (car categories)))
	  (mapcar
	   (function
	    (lambda (x)
	      (if (and (not (eq x coding-system))
		       (coding-system-get x 'no-initial-designation)
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
		  (when (> (current-column) max-col)
		    (goto-char pos)
		    (insert "\n   ")
		    (goto-char (point-max)))
		  (setq codings (cdr codings)))
		(insert "\n\n")))
	  (setq categories (cdr categories))))

      (princ "Particular coding systems specified for certain file names:\n")
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
			(if (>= (current-column) 40)
			    (newline))
			(indent-to 40)
			(princ (cdr (car alist)))
			(princ "\n")
			(setq alist (cdr alist)))))))
	(funcall func "File I/O" file-coding-system-alist)
	(funcall func "Process I/O" process-coding-system-alist)
	(funcall func "Network I/O" network-coding-system-alist))
      (help-mode))))

;; Print detailed information on CODING-SYSTEM.
(defun print-coding-system (coding-system)
  (let ((type (coding-system-type coding-system))
	(eol-type (coding-system-eol-type coding-system))
	(flags (coding-system-flags coding-system))
	(aliases (coding-system-get coding-system 'alias-coding-systems)))
    (if (not (eq (car aliases) coding-system))
	(princ (format "%s (alias of %s)\n" coding-system (car aliases)))
      (princ coding-system)
      (setq aliases (cdr aliases))
      (while aliases
	(princ ",")
	(princ (car aliases))
	(setq aliases (cdr aliases)))
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
	       (if (symbolp (car flags))
		   (princ (format " %s" (car flags)))
		 (setq i 0 len (length (car flags)))
		 (while (< i len)
		   (princ (format " %x" (aref (car flags) i)))
		   (setq i (1+ i))))
	       (princ ",")
	       (if (symbolp (cdr flags))
		   (princ (format "%s" (cdr flags)))
		 (setq i 0 len (length (cdr flags)))
		 (while (< i len)
		   (princ (format " %x" (aref (cdr flags) i)))
		   (setq i (1+ i))))))
	    (t (princ 0)))
      (princ ":")
      (princ (coding-system-doc-string coding-system))
      (princ "\n"))))

;;;###autoload
(defun list-coding-systems (&optional arg)
  "Display a list of all coding systems.
This shows the mnemonic letter, name, and description of each coding system.

With prefix arg, the output format gets more cryptic,
but still contains full information about each coding system."
  (interactive "P")
  (with-output-to-temp-buffer "*Help*"
    (list-coding-systems-1 arg)))

(defun list-coding-systems-1 (arg)
  (if (null arg)
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
      (if (null arg)
	  (print-coding-system-briefly coding-system 'doc-string)
	(print-coding-system coding-system))
      (setq bases (cdr bases)))))

;;;###automatic
(defun list-coding-categories ()
  "Display a list of all coding categories."
  (with-output-to-temp-buffer "*Help*"
    (princ "\
############################
## LIST OF CODING CATEGORIES (ordered by priority)
## CATEGORY:CODING-SYSTEM
##
")
    (let ((l coding-category-list))
      (while l
	(princ (format "%s:%s\n" (car l) (symbol-value (car l))))
	(setq l (cdr l))))))

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
  (interactive "sFontname (default, current choice for ASCII chars): ")
  (or (and window-system (boundp 'global-fontset-alist))
      (error "No fontsets being used"))
  (when (or (not fontname) (= (length fontname) 0))
    (setq fontname (cdr (assq 'font (frame-parameters))))
    (if (query-fontset fontname)
	(setq fontname
	      (nth 2 (assq 'ascii (aref (fontset-info fontname) 2))))))
  (let ((font-info (font-info fontname)))
    (if (null font-info)
	(message "No matching font")
      (with-output-to-temp-buffer "*Help*"
	(describe-font-internal font-info 'verbose)))))

;; Print information of FONTSET.  If optional arg PRINT-FONTS is
;; non-nil, print also names of all fonts in FONTSET.  This function
;; actually INSERT such information in the current buffer.
(defun print-fontset (fontset &optional print-fonts)
  (let* ((fontset-info (fontset-info fontset))
	 (size (aref fontset-info 0))
	 (height (aref fontset-info 1))
	 (fonts (and print-fonts (aref fontset-info 2)))
	 (xlfd-fields (x-decompose-font-name fontset))
	 style)
    (if xlfd-fields
	(let ((weight (aref xlfd-fields xlfd-regexp-weight-subnum))
	      (slant  (aref xlfd-fields xlfd-regexp-slant-subnum)))
	  (if (string-match "^bold$\\|^demibold$" weight)
	      (setq style (concat weight " "))
	    (setq style "medium "))
	  (cond ((string-match "^i$" slant)
		 (setq style (concat style "italic")))
		((string-match "^o$" slant)
		 (setq style (concat style "slant")))
		((string-match "^ri$" slant)
		 (setq style (concat style "reverse italic")))
		((string-match "^ro$" slant)
		 (setq style (concat style "reverse slant")))))
      (setq style " ? "))
    (beginning-of-line)
    (insert fontset)
    (indent-to 58)
    (insert (if (> size 0) (format "%2dx%d" size height) "  -"))
    (indent-to 64)
    (insert style "\n")
    (when print-fonts
      (insert "  O Charset / Fontname\n"
	      "  - ------------------\n")
      (sort-charset-list)
      (let ((l charset-list)
	    charset font-info opened fontname)
	(while l
	  (setq charset (car l) l (cdr l))
	  (setq font-info (assq charset fonts))
	  (if (null font-info)
	      (setq opened ?? fontname "not specified")
	    (if (nth 2 font-info)
		(if (stringp (nth 2 font-info))
		    (setq opened ?o fontname (nth 2 font-info))
		  (setq opened ?- fontname (nth 1 font-info)))
	      (setq opened ?x fontname (nth 1 font-info))))
	  (insert (format "  %c %s\n    %s\n"
			  opened charset fontname)))))))

;;;###autoload
(defun describe-fontset (fontset)
  "Display information of FONTSET.
This shows the name, size, and style of FONTSET, and the list of fonts
contained in FONTSET.

The column WDxHT contains width and height (pixels) of each fontset
\(i.e. those of ASCII font in the fontset).  The letter `-' in this
column means that the corresponding fontset is not yet used in any
frame.

The O column for each font contains one of the following letters:
 o -- font already opened
 - -- font not yet opened
 x -- font can't be opened
 ? -- no font specified

The Charset column for each font contains a name of character set
displayed (for this fontset) using that font."
  (interactive
   (if (not (and window-system (boundp 'global-fontset-alist)))
       (error "No fontsets being used")
     (let ((fontset-list (mapcar '(lambda (x) (list x)) (fontset-list)))
	   (completion-ignore-case t))
       (list (completing-read
	      "Fontset (default, used by the current frame): "
	      fontset-list nil t)))))
  (if (= (length fontset) 0)
      (setq fontset (cdr (assq 'font (frame-parameters)))))
  (if (not (query-fontset fontset))
      (error "Current frame is using font, not fontset"))
  (let ((fontset-info (fontset-info fontset)))
    (with-output-to-temp-buffer "*Help*"
      (save-excursion
	(set-buffer standard-output)
	(insert "Fontset-Name\t\t\t\t\t\t  WDxHT Style\n")
	(insert "------------\t\t\t\t\t\t  ----- -----\n")
	(print-fontset fontset t)))))

;;;###autoload
(defun list-fontsets (arg)
  "Display a list of all fontsets.
This shows the name, size, and style of each fontset.
With prefix arg, it also list the fonts contained in each fontset;
see the function `describe-fontset' for the format of the list."
  (interactive "P")
  (if (not (and window-system (boundp 'global-fontset-alist)))
      (error "No fontsets being used")
    (with-output-to-temp-buffer "*Help*"
      (save-excursion
	;; This code is duplicated near the end of mule-diag.
	(set-buffer standard-output)
	(insert "Fontset-Name\t\t\t\t\t\t  WDxHT Style\n")
	(insert "------------\t\t\t\t\t\t  ----- -----\n")
	(let ((fontsets
	       (sort (fontset-list)
		     (function (lambda (x y)
				 (string< (fontset-plain-name x)
					  (fontset-plain-name y)))))))
	  (while fontsets
	    (print-fontset (car fontsets) arg)
	    (setq fontsets (cdr fontsets))))))))

;;;###autoload
(defun list-input-methods ()
  "Display information about all input methods."
  (interactive)
  (with-output-to-temp-buffer "*Help*"
    (list-input-methods-1)))

(defun list-input-methods-1 ()
  (if (not input-method-alist)
      (progn
	(princ "
No input method is available, perhaps because you have not yet
installed LEIM (Libraries of Emacs Input Method).

LEIM is available from the same ftp directory as Emacs.  For instance,
if there exists an archive file `emacs-20.N.tar.gz', there should also
be a file `leim-20.N.tar.gz'.  When you extract this file, LEIM files
are put under the subdirectory `emacs-20.N/leim'.  When you install
Emacs again, you should be able to use various input methods."))
    (princ "LANGUAGE\n  NAME (`TITLE' in mode line)\n")
    (princ "    SHORT-DESCRIPTION\n------------------------------\n")
    (setq input-method-alist
	  (sort input-method-alist
		(function (lambda (x y) (string< (nth 1 x) (nth 1 y))))))
    (let ((l input-method-alist)
	  language elt)
      (while l
	(setq elt (car l) l (cdr l))
	(when (not (equal language (nth 1 elt)))
	  (setq language (nth 1 elt))
	  (princ language)
	  (terpri))
	(princ (format "  %s (`%s' in mode line)\n    %s\n"
		       (car elt)
		       (let ((title (nth 3 elt)))
			 (if (and (consp title) (stringp (car title)))
			     (car title)
			   title))
		       (let ((description (nth 4 elt)))
			 (string-match ".*" description)
			 (match-string 0 description))))))))

;;; DIAGNOSIS

;; Insert a header of a section with SECTION-NUMBER and TITLE.
(defun insert-section (section-number title)
  (insert "########################################\n"
	  "# Section " (format "%d" section-number) ".  " title "\n"
	  "########################################\n\n"))

;;;###autoload
(defun mule-diag ()
  "Display diagnosis of the multilingual environment (Mule).

This shows various information related to the current multilingual
environment, including lists of input methods, coding systems,
character sets, and fontsets (if Emacs is running under a window
system which uses fontsets)."
  (interactive)
  (with-output-to-temp-buffer "*Mule-Diagnosis*"
    (save-excursion
      (set-buffer standard-output)
      (insert "###############################################\n"
	      "### Current Status of Multilingual Features ###\n"
	      "###############################################\n\n"
	      "CONTENTS: Section 1.  General Information\n"
	      "          Section 2.  Display\n"
	      "          Section 3.  Input methods\n"
	      "          Section 4.  Coding systems\n"
	      "          Section 5.  Character sets\n")
      (if (and window-system (boundp 'global-fontset-alist))
	  (insert "          Section 6.  Fontsets\n"))
      (insert "\n")

      (insert-section 1 "General Information")
      (insert "Version of this emacs:\n  " (emacs-version) "\n\n")

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
      (list-input-methods-1)
      (insert "\n")
      (if default-input-method
	  (insert (format "Default input method: %s\n" default-input-method))
	(insert "No default input method is specified\n"))

      (insert-section 4 "Coding systems")
      (list-coding-systems-1 t)
      (princ "\
############################
## LIST OF CODING CATEGORIES (ordered by priority)
## CATEGORY:CODING-SYSTEM
##
")
      (let ((l coding-category-list))
	(while l
	  (princ (format "%s:%s\n" (car l) (symbol-value (car l))))
	  (setq l (cdr l))))
      (insert "\n")

      (insert-section 5 "Character sets")
      (list-character-sets-1 t)
      (insert "\n")

      (when (and window-system (boundp 'global-fontset-alist))
	;; This code duplicates most of list-fontsets.
	(insert-section 6 "Fontsets")
	(insert "Fontset-Name\t\t\t\t\t\t  WDxHT Style\n")
	(insert "------------\t\t\t\t\t\t  ----- -----\n")
	(let ((fontsets (fontset-list)))
	  (while fontsets
	    (print-fontset (car fontsets) t)
	    (setq fontsets (cdr fontsets)))))
      (print-help-return-message))))


;;; DUMP DATA FILE

;;;###autoload
(defun dump-charsets ()
  "Dump information about all charsets into the file `CHARSETS'.
The file is saved in the directory `data-directory'."
  (let ((file (expand-file-name "CHARSETS" data-directory))
	buf)
    (or (file-writable-p file)
	(error "Can't write to file %s" file))
    (setq buf (find-file-noselect file))
    (save-window-excursion
      (save-excursion
	(set-buffer buf)
	(setq buffer-read-only nil)
	(erase-buffer)
	(list-character-sets t)
	(insert-buffer-substring "*Help*")
	(let (make-backup-files
	      coding-system-for-write)
	  (save-buffer))))
    (kill-buffer buf))
  (if noninteractive
      (kill-emacs)))

;;;###autoload
(defun dump-codings ()
  "Dump information about all coding systems into the file `CODINGS'.
The file is saved in the directory `data-directory'."
  (let ((file (expand-file-name "CODINGS" data-directory))
	buf)
    (or (file-writable-p file)
	(error "Can't write to file %s" file))
    (setq buf (find-file-noselect file))
    (save-window-excursion
      (save-excursion
	(set-buffer buf)
	(setq buffer-read-only nil)
	(erase-buffer)
	(list-coding-systems t)
	(insert-buffer-substring "*Help*")
	(list-coding-categories)
	(insert-buffer-substring "*Help*")
	(let (make-backup-files
	      coding-system-for-write)
	  (save-buffer))))
    (kill-buffer buf))
  (if noninteractive
      (kill-emacs)))

;;; mule-diag.el ends here
