;;; titdic-cnv.el --- convert TIT dictionary to Quail package

;; Copyright (C) 1995 Free Software Foundation, Inc.
;; Copyright (C) 1995 Electrotechnical Laboratory, JAPAN.

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

;; Convert TIT format dictionary (of cxterm) to quail-package.
;;
;; Usage (within Emacs):
;;	M-x titdic-convert<CR>TIT-FILE-NAME<CR>
;; Usage (from shell):
;;	% emacs -batch -l titdic-convert -f batch-titdic-convert\
;;		[-dir DIR] [DIR | FILE] ...
;;
;; When you run titdic-convert within Emacs, you have a chance to
;; modify arguments of `quail-define-package' before saving the
;; converted file.  For instance, you are likely to modify TITLE,
;; DOCSTRING, and KEY-BINDINGS.

;; TIT dictionary file (*.tit) is a line-oriented text (English,
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

;; List of values of key "ENCODE:" and the corresponding Emacs'
;; coding-system and language environment name.
(defvar tit-encode-list
  '(("GB" coding-system-euc-china "Chinese-GB")
    ("BIG5" coding-system-big5 "Chinese-BIG5")
    ("JIS" coding-system-euc-japan "Japanese")
    ("KS" coding-system-euc-korea "Korean")))

;; Return a value of the key in the current line.
(defsubst tit-read-key-value ()
  (if (looking-at "[^ \t\n]+")
      (car (read-from-string (concat "\"" (match-string 0) "\"")))))

;; Return an appropriate quail-package filename from FILENAME (TIT
;; dictionary filename).  For instance, ".../ZOZY.tit" -> "zozy.el".
(defun tit-make-quail-package-name (filename &optional dirname)
  (expand-file-name
   (concat (downcase (file-name-nondirectory (substring filename 0 -4))) ".el")
   dirname))

;; This value is t if we are processing phrase dictionary.
(defvar tit-phrase nil)
(defvar tit-encode nil)
(defvar tit-default-encode "GB")

;; Generate elements of KEY-BINDINGS arg for `quail-define-package' so
;; that each characters in KEYS invokes FUNCTION-SYMBOL.
(defun tit-generate-key-bindings (keys function-symbol)
  (let ((len (length keys))
	(i 0)
	key)
    (while (< i len)
      (setq key (aref keys i))
      (indent-to 3)
      (if (< key ?\ )
	  (if (eq (lookup-key quail-translation-keymap (char-to-string key))
		  'quail-execute-non-quail-command)
	      (insert (format "(\"\\C-%c\" . %s)\n"
			      (+ key ?@) function-symbol)))
	(if (< key 127)
	    (insert (format "(\"%c\" . %s)\n" key function-symbol))
	  (insert (format "(\"\\C-?\" . %s)\n" function-symbol))))
      (setq i (1+ i)))))

;; Analyze header part of TIT dictionary and generate an appropriate
;; `quail-define-package' function call.
(defun tit-process-header (filename)
  (message "Processing header part...")
  (goto-char (point-min))

  (let (;; TIT keywords and the corresponding default values.
	(tit-multichoice t)
	(tit-prompt "")
	(tit-comments nil)
	(tit-backspace "\010\177")
	(tit-deleteall "\015\025")
	(tit-moveright ".>")
	(tit-moveleft ",<")
	(tit-keyprompt nil))
    ;; At first, collect information from the header.
    (while (not (eobp))
      (insert ";; ")
      (let ((ch (following-char)))
	(cond ((= ch ?C)		; COMMENT
	       (cond ((looking-at "COMMENT")
		      (let ((pos (match-end 0)))
			(end-of-line)
			(while (re-search-backward "[\"\\]" pos t)
			  (insert "\\")
			  (forward-char -1))
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
		      (setq tit-phrase nil))
		     ((looking-at "BEGINPHRASE")
		      (setq tit-phrase t))))
	      ((= ch ?K)		; KEYPROMPT
	       (cond ((looking-at "KEYPROMPT(\\(.*\\)):[ \t]*")
		      (let ((key-char (match-string 1)))
			(goto-char (match-end 0))
			(setq tit-keyprompt
			      (cons (cons key-char (tit-read-key-value))
				    tit-keyprompt))))))))
      (forward-line 1))
  
    ;; Then, generate header part of the Quail package.
    (goto-char (point-min))
    (insert ";; Quail package `"
	    (substring (file-name-nondirectory buffer-file-name) 0 -3)
	    "' generated by the command `titdic-convert'\n"
	    ";;\tDate: " (current-time-string) "\n"
	    ";;\tOriginal TIT dictionary file: "
	    (file-name-nondirectory filename)
	    "\n\n"
	    ";;; Comment:\n\n"
	    ";; Do byte-compile this file again after any modification.\n\n"
	    ";;; Start of the header of original TIT dictionary.\n\n")

    (goto-char (point-max))
    (insert "\n"
	    ";;; End of the header of original TIT dictionary.\n\n"
	    ";;; Code:\n\n"
	    "(require 'quail)\n\n")

    (insert "(quail-define-package ")
    ;; Args NAME, LANGUAGE, TITLE
    (insert
     "\""
     (concat "quail-"
	     (substring (file-name-nondirectory buffer-file-name) 0 -3))
     "\" \"" (nth 2 (assoc tit-encode tit-encode-list))
     "\" \""
     (if (string-match "[:$A!K$(0!(!J(B]+\\([^:$A!K$(0!(!K(B]+\\)" tit-prompt)
	 (substring tit-prompt (match-beginning 1) (match-end 1))
       tit-prompt)
     "\"\n")

    ;; Arg GUIDANCE
    (if tit-keyprompt
	(progn
	  (insert " '(")
	  (while tit-keyprompt
	    (indent-to 3)
	    (insert (format "(%d . \"%s\")\n"
			    (string-to-char (car (car tit-keyprompt)))
			    (cdr (car tit-keyprompt))))
	    (setq tit-keyprompt (cdr tit-keyprompt)))
	  (forward-char -1)
	  (insert ")")
	  (forward-char 1))
      (insert " t\n"))

    ;; Arg DOCSTRING
    (insert "\"" tit-prompt "\n")
    (let ((l (nreverse tit-comments)))
      (while l
	(insert (format "%s\n" (car l)))
	(setq l (cdr l))))
    (insert "\"\n")

    ;; Arg KEY-BINDINGS
    (insert " '(")
    (tit-generate-key-bindings tit-backspace 'quail-delete-last-char)
    (tit-generate-key-bindings tit-deleteall 'quail-abort-translation)
    (tit-generate-key-bindings tit-moveright 'quail-next-translation)
    (tit-generate-key-bindings tit-moveleft 'quail-prev-translation)
    (forward-char -1)
    (insert ")")
    (forward-char 1)

    ;; Args FORGET-TRANSLATION, DETERMINISTIC, KBD-TRANSLATE, SHOW-LAYOUT.
    ;; The remaining args are all nil.
    (insert " nil"
	    (if tit-multichoice " nil" " t")
	    (if tit-keyprompt " t t)\n\n" " nil nil)\n\n")))

  ;; Return the position of end of the header.
  (point-max))

;; Convert body part of TIT dictionary into `quail-define-rules'
;; function call.
(defun tit-process-body ()
  (message "Formatting translation rules...")
  (let ((enable-multibyte-characters nil)
	(keyseq "\000")
	pos)
    (insert "(quail-define-rules\n")
    (while (null (eobp))
      (if (or (= (following-char) ?#) (= (following-char) ?\n))
	  (insert ";; ")
	(insert "(\"")
	(setq pos (point))
	(skip-chars-forward "^ \t")
	(setq keyseq
	      (concat (regexp-quote (buffer-substring pos (point))) "[ \t]+"))
	(save-excursion
	  (while (re-search-backward "[\\\"]" pos t)
	    (insert "\\")
	    (forward-char -1)))
	(insert "\"")
	(skip-chars-forward " \t")

	;; Now point is at the start of translations.  Remember it in
	;; POS and combine lines of the same key sequence while
	;; deleting trailing white spaces and  comments (start with
	;; '#').  POS doesn't has to be a marker because we never
	;; modify region before POS.
	(setq pos (point))
	(if (looking-at "[^ \t]*\\([ \t]*#.*\\)")
	    (delete-region (match-beginning 1) (match-end 1)))
	(while (and (= (forward-line 1) 0)
		    (looking-at keyseq))
	  (let ((p (match-end 0)))
	    (skip-chars-backward " \t\n")
	    (delete-region (point) p)
	    (if tit-phrase (insert " "))
	    (if (looking-at "[^ \t]*\\([ \t]*#.*\\)")
		(delete-region (match-beginning 1) (match-end 1)))
	    ))

	;; Modify the current line to meet the syntax of Quail package.
	(goto-char pos)
	(if tit-phrase
	    (progn
	      ;; PHRASE1 PHRASE2 ... => ["PHRASE1" "PHRASE2" ...]
	      (insert "[\"")
	      (skip-chars-forward "^ \t\n")
	      (while (not (eolp))
		(insert "\"")
		(forward-char 1)
		(insert "\"")
		(skip-chars-forward "^ \t\n"))
	      (insert "\"])"))
	  ;; TRANSLATIONS => "TRANSLATIONS"
	  (insert "\"")
	  (end-of-line)
	  (insert "\")")))
      (forward-line 1))
    (insert ")\n")))

;;;###autoload
(defun titdic-convert (filename &optional dirname)
  "Convert a TIT dictionary of FILENAME into a Quail package.
Optional argument DIRNAME if specified is the directory name under which
the generated Quail package is saved."
  (interactive "FTIT dictionary file: ")
  (let ((buf (get-buffer-create "*tit-work*")))
    (save-excursion
      ;; Setup the buffer.
      (set-buffer buf)
      (erase-buffer)
      (let ((coding-system-for-read 'no-conversion))
	(insert-file-contents (expand-file-name filename)))
      (set-visited-file-name (tit-make-quail-package-name filename dirname) t)
      (set-buffer-file-coding-system 'coding-system-iso-2022-7)

      ;; Decode the buffer contents from the encoding specified by a
      ;; value of the key "ENCODE:".
      (let (coding-system)
	(save-excursion
	  (if (search-forward "\nBEGIN" nil t)
	      (let ((limit (point))
		    slot)
		(goto-char 1)
		(if (re-search-forward "^ENCODE:[ \t]*" limit t)
		    (progn
		      (goto-char (match-end 0))
		      (setq tit-encode (tit-read-key-value)))
		  (setq tit-encode tit-default-encode))
		(setq slot (assoc tit-encode tit-encode-list))
		(if slot
		    (setq coding-system (nth 1 slot))
		  (error "Invalid ENCODE: value in TIT dictionary")))
	    (error "TIT dictionary doesn't have body part")))
	(message "Decoding %s..." coding-system)
	(goto-char 1)
	(decode-coding-region 1 (point-max) coding-system))

      ;; Set point the starting position of the body part.
      (goto-char 1)
      (if (search-forward "\nBEGIN" nil t)
	  (forward-line 1)
	(error "TIT dictionary can't be decoded correctly"))

      ;; Now process the header and body parts.
      (goto-char
       (save-excursion
	 (save-restriction
	   (narrow-to-region 1 (point))
	   (tit-process-header filename))))
      (tit-process-body))

    (if noninteractive
	;; Save the Quail package file.
	(save-excursion
	  (set-buffer buf)
	  (save-buffer 0))
      ;; Show the Quail package just generated.
      (switch-to-buffer buf)
      (goto-char 1)
      (message "Save this buffer after you make any modification"))))

;;;###autoload
(defun batch-titdic-convert ()
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
	  (if (file-newer-than-file-p
	       file (tit-make-quail-package-name file targetdir))
	      (progn
		(message "Converting %s to quail-package..." file)
		(titdic-convert file targetdir)))
	  (setq files (cdr files)))
	(setq command-line-args-left (cdr command-line-args-left)))
      (message "Do byte-compile the created files by:")
      (message "  %% emacs -batch -f batch-byte-compile XXX.el")))
  (kill-emacs 0))

;;; titdic-cnv.el ends here
