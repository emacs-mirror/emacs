;;; titdic-cnv.el --- convert cxterm dictionary (TIT format) to Quail package

;; Copyright (C) 1995 Electrotechnical Laboratory, JAPAN.
;; Licensed to the Free Software Foundation.

;; Keywords: Quail, TIT, cxterm

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

;;; Comments:

;; Convert cxterm dictionary (of TIT format) to quail-package.
;;
;; Usage (within Emacs):
;;	M-x titdic-convert<CR>CXTERM-DICTIONARY-NAME<CR>
;; Usage (from shell):
;;	% emacs -batch -l titdic-cnv -f batch-titdic-convert\
;;		[-dir DIR] [DIR | FILE] ...
;;
;; When you run titdic-convert within Emacs, you have a chance to
;; modify arguments of `quail-define-package' before saving the
;; converted file.  For instance, you are likely to modify TITLE,
;; DOCSTRING, and KEY-BINDINGS.

;; Cxterm dictionary file (*.tit) is a line-oriented text (English,
;; Chinese, Japanese, and Korean) file.  The whole file contains of
;; two parts, the definition part (`header' here after) followed by
;; the dictionary part (`body' here after).  All lines begin with
;; leading '#' are ignored.
;;
;; Each line in the header part has two fields, KEY and VALUE.  These
;; fields are separated by one or more white characters.
;;
;; Each line in the body part has two fields, KEYSEQ and TRANSLATIONS.
;; These fields are separated by one or more white characters.
;;
;; See the manual page of `tit2cit' of cxterm distribution for more
;; detail.

;;; Code:

(require 'quail)

;; List of values of key "ENCODE:" and the corresponding Emacs
;; coding-system and language environment name.
(defvar tit-encode-list
  '(("GB" euc-china "Chinese-GB")
    ("BIG5" cn-big5 "Chinese-BIG5")
    ("JIS" euc-japan "Japanese")
    ("KS" euc-kr "Korean")))

;; List of package names and the corresponding titles.
(defvar quail-cxterm-package-title-alist
  '(("chinese-4corner" . "$(0(?-F(B")
    ("chinese-array30" . "$(0#R#O(B")
    ("chinese-ccdospy" . "$AKuF4(B")
    ("chinese-ctlau" . "$AAuTA(B")
    ("chinese-ctlaub" . "$(0N,Gn(B")
    ("chinese-ecdict" . "$(05CKH(B")
    ("chinese-etzy" . "$(06/0D(B")
    ("chinese-punct-b5" . "$(0O:(BB")
    ("chinese-punct" . "$A1j(BG")
    ("chinese-py-b5" . "$(03<(BB")
    ("chinese-py" . "$AF4(BG")
    ("chinese-qj-b5" . "$(0)A(BB")
    ("chinese-qj" . "$AH+(BG")
    ("chinese-sw" . "$AJWN2(B")
    ("chinese-tonepy" . "$A5wF4(B")
    ("chinese-ziranma" . "$AK+F4(B")
    ("chinese-zozy" . "$(0I\0D(B")))

;; Return a value of the key in the current line.
(defsubst tit-read-key-value ()
  (if (looking-at "[^ \t\n]+")
      (car (read-from-string (concat "\"" (match-string 0) "\"")))))

;; Return an appropriate quail-package filename from FILENAME (TIT
;; dictionary filename).  For instance, ".../ZOZY.tit" -> "ZOZY.el".
(defun tit-make-quail-package-file-name (filename &optional dirname)
  (expand-file-name
   (concat (file-name-nondirectory (substring filename 0 -4)) ".el")
   dirname))

;; This value is nil if we are processing phrase dictionary.
(defconst tit-dictionary t)
(defvar tit-encode nil)
(defvar tit-default-encode "GB")

;; Generate elements of KEY-BINDINGS arg for `quail-define-package' so
;; that each characters in KEYS invokes FUNCTION-SYMBOL.
(defun tit-generate-key-bindings (keys function-symbol)
  (let ((len (length keys))
	(i 0)
	(first t)
	key)
    (while (< i len)
      (or first (princ "\n   "))
      (setq key (aref keys i))
      (if (if (< key ?\ )
	      (eq (lookup-key quail-translation-keymap
			      (char-to-string key))
		  'quail-execute-non-quail-command)
	    (<= key 127))
	  (progn
	    (princ (cons (cond ((< key ?\ ) (format "\"\\C-%c\"" (+ key ?@)))
			       ((< key 127) (format "\"%c\"" key))
			       (t "\"\\C-?\""))
			 function-symbol))
	    (setq first nil)))
      (setq i (1+ i)))))

;; Analyze header part of TIT dictionary and generate an appropriate
;; `quail-define-package' function call.
(defun tit-process-header (filename)
  (message "Processing header part...")
  (goto-char (point-min))

  ;; At first, generate header part of the Quail package while
  ;; collecting information from the original header.
  (let ((package (concat
		  "chinese-"
		  (substring (downcase (file-name-nondirectory filename))
			     0 -4)))
	;; TIT keywords and the corresponding default values.
	(tit-multichoice t)
	(tit-prompt "")
	(tit-comments nil)
	(tit-backspace "\010\177")
	(tit-deleteall "\015\025")
	(tit-moveright ".>")
	(tit-moveleft ",<")
	(tit-keyprompt nil))

    (princ ";; Quail package `")
    (princ package)
    (princ "' generated by the command `titdic-convert'\n;;\tDate: ")
    (princ (current-time-string))
    (princ "\n;;\tOriginal TIT dictionary file: ")
    (princ (file-name-nondirectory filename))
    (princ "\n\n;;; Comment:\n\n")
    (princ ";; Byte-compile this file again after any modification.\n\n")
    (princ ";;; Start of the header of original TIT dictionary.\n\n")

    (while (not (eobp))
      (let ((ch (following-char))
	    (pos (point)))
	(cond ((= ch ?C)		; COMMENT
	       (cond ((looking-at "COMMENT")
		      (let ((pos (match-end 0)))
			(end-of-line)
			(setq tit-comments (cons (buffer-substring pos (point))
						 tit-comments))))))
	      ((= ch ?M)		; MULTICHOICE, MOVERIGHT, MOVELEFT
	       (cond ((looking-at "MULTICHOICE:[ \t]*")
		      (goto-char (match-end 0))
		      (setq tit-multichoice (looking-at "YES")))
		     ((looking-at "MOVERIGHT:[ \t]*")
		      (goto-char (match-end 0))
		      (setq tit-moveright (tit-read-key-value)))
		     ((looking-at "MOVELEFT:[ \t]*")
		      (goto-char (match-end 0))
		      (setq tit-moveleft (tit-read-key-value)))))
	      ((= ch ?P)		; PROMPT
	       (cond ((looking-at "PROMPT:[ \t]*")
		      (goto-char (match-end 0))
		      (setq tit-prompt (tit-read-key-value)))))
	      ((= ch ?B)		; BACKSPACE, BEGINDICTIONARY,
					; BEGINPHRASE
	       (cond ((looking-at "BACKSPACE:[ \t]*")
		      (goto-char (match-end 0))
		      (setq tit-backspace (tit-read-key-value)))
		     ((looking-at "BEGINDICTIONARY")
		      (setq tit-dictionary t))
		     ((looking-at "BEGINPHRASE")
		      (setq tit-dictionary nil))))
	      ((= ch ?K)		; KEYPROMPT
	       (cond ((looking-at "KEYPROMPT(\\(.*\\)):[ \t]*")
		      (let ((key-char (match-string 1)))
			(goto-char (match-end 0))
			(if (string-match "\\\\[0-9]+" key-char)
			    (setq key-char
				  (car (read-from-string (format "\"%s\""
								 key-char)))))
			(setq tit-keyprompt
			      (cons (cons key-char (tit-read-key-value))
				    tit-keyprompt)))))))
	(end-of-line)
	(princ ";; ")
	(princ (buffer-substring pos (point)))
	(princ "\n")
	(forward-line 1)))
  
    (princ "\n;;; End of the header of original TIT dictionary.\n\n")
    (princ ";;; Code:\n\n(require 'quail)\n\n")

    (princ "(quail-define-package ")
    ;; Args NAME, LANGUAGE, TITLE
    (let ((title (cdr (assoc package quail-cxterm-package-title-alist))))
      (princ "\"")
      (princ package)
      (princ "\" \"")
      (princ (nth 2 (assoc tit-encode tit-encode-list)))
      (princ "\" \"")
      (princ (or title
		 (if (string-match "[:$A!K$(0!(!J(B]+\\([^:$A!K$(0!(!K(B]+\\)" tit-prompt)
		     (substring tit-prompt (match-beginning 1) (match-end 1))
		   tit-prompt)))
      (princ "\"\n"))

    ;; Arg GUIDANCE
    (if tit-keyprompt
	(progn
	  (princ " '(")
	  (while tit-keyprompt
	    (princ "   ")
	    (princ (format "(%d . \"%s\")\n"
			   (string-to-char (car (car tit-keyprompt)))
			   (cdr (car tit-keyprompt))))
	    (setq tit-keyprompt (cdr tit-keyprompt)))
	  (princ ")"))
      (princ " t\n"))

    ;; Arg DOCSTRING
    (prin1
     (mapconcat 'identity (cons tit-prompt (nreverse tit-comments)) "\n"))
    (terpri)

    ;; Arg KEY-BINDINGS
    (princ " '(")
    (tit-generate-key-bindings tit-backspace 'quail-delete-last-char)
    (princ "\n   ")
    (tit-generate-key-bindings tit-deleteall 'quail-abort-translation)
    (princ "\n   ")
    (tit-generate-key-bindings tit-moveright 'quail-next-translation)
    (princ "\n   ")
    (tit-generate-key-bindings tit-moveleft 'quail-prev-translation)
    (princ ")\n")

    ;; Args FORGET-TRANSLATION, DETERMINISTIC, KBD-TRANSLATE, SHOW-LAYOUT.
    ;; The remaining args are all nil.
    (princ " nil")
    (princ (if tit-multichoice " nil" " t"))
    (princ (if tit-keyprompt " t t)\n\n" " nil nil)\n\n"))))

(defsubst tit-flush-translations (key translations)
  (if (string-match "\\\\[0-9][0-9][0-9]" key)
      (let ((newkey (concat (substring key 0 (match-beginning 0))
			    (car (read-from-string
				  (concat "\"" (match-string 0 key) "\"")))))
	    (idx (match-end 0)))
	(while (string-match "\\\\[0-9][0-9][0-9]" key idx)
	  (setq newkey (concat
			newkey
			(substring key idx (match-beginning 0))
			(car (read-from-string
			      (concat "\"" (match-string 0 key) "\"")))))
	  (setq idx (match-end 0)))
	(setq key (concat newkey (substring key idx)))))
  (prin1 (list key (if tit-dictionary translations
		     (vconcat (nreverse translations)))))
  (princ "\n"))

;; Convert body part of TIT dictionary into `quail-define-rules'
;; function call.
(defun tit-process-body ()
  (message "Formatting translation rules...")
  (let* ((template (list nil nil))
	 (second (cdr template))
	 (prev-key "")
	 ch key translations pos)
    (princ "(quail-define-rules\n")
    (while (null (eobp))
      (setq ch (following-char))
      (if (or (= ch ?#) (= ch ?\n))
	  (forward-line 1)
	(setq pos (point))
	(skip-chars-forward "^ \t\n")
	(setq key (buffer-substring pos (point)))
	(skip-chars-forward " \t")
	(setq ch (following-char))
	(if (or (= ch ?#) (= ch ?\n))
	    ;; This entry contains no translations.  Let's ignore it.
	    (forward-line 1)
	  (or (string= key prev-key)
	      (progn
		(if translations
		    (tit-flush-translations prev-key translations))
		(setq translations nil
		      prev-key key)))
	  (if tit-dictionary
	      (progn
		(setq pos (point))
		(skip-chars-forward "^ \t#\n")
		(setq translations
		      (if translations
			  (concat translations
				  (buffer-substring pos (point)))
			(buffer-substring pos (point)))))
	    (while (not (eolp))
	      (setq pos (point))
	      (skip-chars-forward "^ \t\n")
	      (setq translations (cons (buffer-substring pos (point))
				       translations))
	      (skip-chars-forward " \t")
	      (setq ch (following-char))
	      (if (= ch ?#) (end-of-line))))
	  (forward-line 1))))

    (if translations
	(tit-flush-translations prev-key translations))
    (princ ")\n")))

;;;###autoload
(defun titdic-convert (filename &optional dirname)
  "Convert a TIT dictionary of FILENAME into a Quail package.
Optional argument DIRNAME if specified is the directory name under which
the generated Quail package is saved."
  (interactive "FTIT dictionary file: ")
  (with-temp-file  (tit-make-quail-package-file-name filename dirname)
    (set-buffer-file-coding-system 'iso-2022-7bit)
    (let ((standard-output (current-buffer)))
      (with-temp-buffer
	(let ((coding-system-for-read 'no-conversion))
	  (insert-file-contents (expand-file-name filename)))
	(set-buffer-multibyte t)
	
	;; Decode the buffer contents from the encoding specified by a
	;; value of the key "ENCODE:".
	(if (not (search-forward "\nBEGIN" nil t))
	    (error "TIT dictionary doesn't have body part"))
	(let ((limit (point))
	      coding-system slot)
	  (goto-char (point-min))
	  (if (re-search-forward "^ENCODE:[ \t]*" limit t)
	      (progn
		(goto-char (match-end 0))
		(setq tit-encode (tit-read-key-value)))
	    (setq tit-encode tit-default-encode))
	  (setq slot (assoc tit-encode tit-encode-list))
	  (if (not slot)
	      (error "Invalid ENCODE: value in TIT dictionary"))
	  (setq coding-system (nth 1 slot))
	  (message "Decoding with coding system %s..." coding-system)
	  (goto-char (point-min))
	  (decode-coding-region (point-min) (point-max) coding-system))

	;; Set point the starting position of the body part.
	(goto-char (point-min))
	(if (not (search-forward "\nBEGIN" nil t))
	    (error "TIT dictionary can't be decoded correctly"))

	;; Process the header part.
	(forward-line 1)
	(narrow-to-region (point-min) (point))
	(tit-process-header filename)
	(widen)

	;; Process the body part.  For speed, we turn off multibyte facility.
	(with-current-buffer standard-output
	  (set-buffer-multibyte nil))
	(set-buffer-multibyte nil)
	(tit-process-body)))))

;;;###autoload
(defun batch-titdic-convert (&optional force)
  "Run `titdic-convert' on the files remaining on the command line.
Use this from the command line, with `-batch';
it won't work in an interactive Emacs.
For example, invoke \"emacs -batch -f batch-titdic-convert XXX.tit\" to
 generate Quail package file \"xxx.el\" from TIT dictionary file \"XXX.tit\".
To get complete usage, invoke \"emacs -batch -f batch-titdic-convert -h\"."
  (defvar command-line-args-left)	; Avoid compiler warning.
  (if (not noninteractive)
      (error "`batch-titdic-convert' should be used only with -batch"))
  (if (string= (car command-line-args-left) "-h")
      (progn
	(message "To convert XXX.tit and YYY.tit into xxx.el and yyy.el:")
	(message "  %% emacs -batch -l titdic-cnv -f batch-titdic-convert XXX.tit YYY.tit")
	(message "To convert XXX.tit into DIR/xxx.el:")
	(message "  %% emacs -batch -l titdic-cnv -f batch-titdic-convert -dir DIR XXX.tit"))
    (let (targetdir filename files file)
      (if (string= (car command-line-args-left) "-dir")
	  (progn
	    (setq command-line-args-left (cdr command-line-args-left))
	    (setq targetdir (car command-line-args-left))
	    (setq command-line-args-left (cdr command-line-args-left))))
      (while command-line-args-left
	(setq filename (expand-file-name (car command-line-args-left)))
	(if (file-directory-p filename)
	    (progn
	      (message "Converting all tit files in the directory %s" filename)
	      (setq files (directory-files filename t "\\.tit$")))
	  (setq files (list filename)))
	(while files
	  (setq file (expand-file-name (car files)))
	  (when (or force
		    (file-newer-than-file-p
		     file (tit-make-quail-package-file-name file targetdir)))
	    (message "Converting %s to quail-package..." file)
	    (titdic-convert file targetdir))
	  (setq files (cdr files)))
	(setq command-line-args-left (cdr command-line-args-left)))
      (message "Byte-compile the created files by:")
      (message "  %% emacs -batch -f batch-byte-compile XXX.el")))
  (kill-emacs 0))

;;; titdic-cnv.el ends here
