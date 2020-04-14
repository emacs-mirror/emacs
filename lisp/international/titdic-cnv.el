;;; titdic-cnv.el --- convert cxterm dictionary (TIT format) to Quail package -*- coding: utf-8-emacs; lexical-binding:t -*-

;; Copyright (C) 1997-1998, 2000-2020 Free Software Foundation, Inc.
;; Copyright (C) 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004,
;;   2005, 2006, 2007, 2008, 2009, 2010, 2011
;;   National Institute of Advanced Industrial Science and Technology (AIST)
;;   Registration Number H14PRO021
;; Copyright (C) 2003
;;   National Institute of Advanced Industrial Science and Technology (AIST)
;;   Registration Number H13PRO009

;; Keywords: Quail, TIT, cxterm

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
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

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
;;
;; Near the end of this file, we also have a few other tools to convert
;; miscellaneous dictionaries.

;;; Code:

(require 'quail)

;; List of values of key "ENCODE:" and the corresponding Emacs
;; coding-system and language environment name.
(defvar tit-encode-list
  '(("GB" euc-china "Chinese-GB")
    ("BIG5" cn-big5 "Chinese-BIG5")
    ("JIS" euc-japan "Japanese")
    ("KS" euc-kr "Korean")))

;; Alist of input method names and the corresponding title and extra
;; docstring.  For each of input method generated from TIT dictionary,
;; a docstring is automatically generated from the comments in the
;; dictionary.  The extra docstring in this alist is to add more
;; information.
;; The command describe-input-method shows the automatically generated
;; docstring, then an extra docstring while replacing the form \<VAR>
;; by the value of variable VAR.  For instance, the form
;; \<quail-translation-docstring> is replaced by a description about
;; how to select a translation from a list of candidates.

(defvar quail-cxterm-package-ext-info
  '(("chinese-4corner" "四角")
    ("chinese-array30" "３０")
    ("chinese-ccdospy" "缩拼"
     "Pinyin base input method for Chinese charset GB2312 (`chinese-gb2312').

Pinyin is the standard Roman transliteration method for Chinese.
For the detail of Pinyin system, see the documentation of the input
method `chinese-py'.

This input method works almost the same way as `chinese-py'.  The
difference is that you type a single key for these Pinyin spelling.
    Pinyin:  zh  en  eng ang ch  an  ao  ai  ong sh  ing  yu(ü)
    keyseq:   a   f   g   h   i   j   k   l   s   u   y   v
For example:
    Chinese:  啊    果    中    文    光    玉    全
    Pinyin:   a    guo   zhong  wen  guang  yu   quan
    Keyseq:   a1   guo4   as1   wf4  guh1  yu..6 qvj6

\\<quail-translation-docstring>

For double-width GB2312 characters corresponding to ASCII, use the
input method `chinese-qj'.")

    ("chinese-ecdict" "英漢"
"In this input method, you enter a Chinese (Big5) character or word
by typing the corresponding English word.  For example, if you type
\"computer\", \"電腦\" is input.

\\<quail-translation-docstring>")

    ("chinese-etzy" "倚注"
"Zhuyin base input method for Chinese Big5 characters (`chinese-big5-1',
`chinese-big5-2').

Zhuyin is a kind of phonetic symbol.  One to three Zhuyin symbols
compose one Chinese character.

In this input method, you enter a Chinese character by first typing
keys corresponding to Zhuyin symbols (see the above table) followed by
SPC, 1, 2, 3, or 4 specifying a tone (SPC:陰平, 1:輕聲, 2:陽平, 3: 上聲,
4:去聲).

\\<quail-translation-docstring>")

    ("chinese-punct-b5" "標B"
     "Input method for Chinese punctuation and symbols of Big5
\(`chinese-big5-1' and `chinese-big5-2').")

    ("chinese-punct" "标G"
     "Input method for Chinese punctuation and symbols of GB2312
\(`chinese-gb2312').")

    ("chinese-py-b5" "拼B"
     "Pinyin base input method for Chinese Big5 characters
\(`chinese-big5-1', `chinese-big5-2').

This input method works almost the same way as `chinese-py' (which
see).

This input method supports only Han characters.  The more convenient
method is `chinese-py-punct-b5', which is the combination of this
method and `chinese-punct-b5' and which supports both Han characters
and punctuation/symbols.

For double-width Big5 characters corresponding to ASCII, use the input
method `chinese-qj-b5'.

The input method `chinese-py' and `chinese-tonepy' are also Pinyin
based, but for the character set GB2312 (`chinese-gb2312').")

    ("chinese-qj-b5" "全B")

    ("chinese-qj" "全G")

    ("chinese-sw" "首尾"
"Radical base input method for Chinese charset GB2312 (`chinese-gb2312').

In this input method, you enter a Chinese character by typing two
keys.  The first key corresponds to the first (首) radical, the second
key corresponds to the last (尾) radical.  The correspondence of keys
and radicals is as below:

 first radical:
 a  b  c  d  e  f  g  h  i  j  k  l  m  n  o  p  q  r  s  t  u  v  w  x  y  z
 心 冖 尸 丶 火 口 扌 氵 讠 艹 亻 木 礻 饣 月 纟 石 王 八 丿 日 辶 犭 竹 一 人
 last radical:
 a  b  c  d  e  f  g  h  i  j  k  l  m  n  o  p  q  r  s  t  u  v  w  x  y  z
 又 山 土 刀 阝 口 衣 疋 大 丁 厶 灬 十 歹 冂 门 今 丨 女 乙 囗 小 厂 虫 弋 卜

\\<quail-translation-docstring>")

    ("chinese-tonepy" "调拼"
     "Pinyin base input method for Chinese charset GB2312 (`chinese-gb2312').

Pinyin is the standard roman transliteration method for Chinese.
For the details of Pinyin system, see the documentation of the input
method `chinese-py'.

This input method works almost the same way as `chinese-py'.  The
difference is that you must type 1..5 after each Pinyin spelling to
specify a tone (1:阴平, 2:阳平, 3:上声, 4下声, 5:轻声).

\\<quail-translation-docstring>

For instance, to input 你, you type \"n i 3 3\", the first \"n i\" is
a Pinyin, the next \"3\" specifies tone, and the last \"3\" selects
the third character from the candidate list.

For double-width GB2312 characters corresponding to ASCII, use the
input method `chinese-qj'.")

    ("chinese-zozy" "零注"
"Zhuyin base input method for Chinese Big5 characters (`chinese-big5-1',
`chinese-big5-2').

Zhuyin is a kind of a phonetic symbol.  One to three Zhuyin symbols
compose a Chinese character.

In this input method, you enter a Chinese character by first typing
keys corresponding to Zhuyin symbols (see the above table) followed by
SPC, 6, 3, 4, or 7 specifying a tone (SPC:陰平, 6:陽平, 3:上聲, 4:去聲,
7:輕聲).

\\<quail-translation-docstring>")))

;; Return a value of the key in the current line.
(defsubst tit-read-key-value ()
  (if (looking-at "[^ \t\r\n]+")
      (car (read-from-string (concat "\"" (match-string 0) "\"")))))

;; Return an appropriate quail-package filename from FILENAME (TIT
;; dictionary filename).  For instance, ".../ZOZY.tit" -> "ZOZY.el".
(defun tit-make-quail-package-file-name (filename &optional dirname)
  (expand-file-name
   (concat (file-name-nondirectory (substring filename 0 -4)) ".el")
   dirname))

;; This value is nil if we are processing phrase dictionary.
(defvar tit-dictionary t)
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
    (princ "\n")
    (princ (substitute-command-keys
	    ";;   Generated by the command `titdic-convert'\n"))
    (princ ";;\tOriginal TIT dictionary file: ")
    (princ (file-name-nondirectory filename))
    (princ "\n\n")

    (while (not (eobp))
      (let ((ch (following-char))
	    (pos (point)))
	(cond ((= ch ?C)		; COMMENT
	       (cond ((looking-at "COMMENT")
		      (let ((pos (match-end 0))
			    (to (progn (end-of-line) (point))))
			(goto-char pos)
			(while (re-search-forward "[\\\"]" to t)
			  (replace-match "\\\\\\&"))
			(goto-char pos)
			(while (re-search-forward "['`]" to t)
			  (replace-match "\\\\\\\\=\\&"))
			(end-of-line)
			(setq tit-comments
			      (cons (buffer-substring-no-properties pos (point))
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
		      (setq tit-prompt (tit-read-key-value))
		      ;; Some TIT dictionaries that are encoded by
		      ;; euc-china contains invalid character at the tail.
		      (let* ((last (aref tit-prompt (1- (length tit-prompt))))
			     (split (split-char last)))
			(if (or (eq (nth 1 split) 32)
				(eq (nth 2 split) 32))
			    (setq tit-prompt (substring tit-prompt 0 -1)))))))
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
	(princ (buffer-substring-no-properties pos (point)))
	(princ "\n")
	(forward-line 1)))

    (princ "\n;;; End of the header of original TIT dictionary.\n\n")
    (princ ";;; Code:\n\n(require 'quail)\n\n")

    (princ "(quail-define-package ")
    ;; Args NAME, LANGUAGE, TITLE
    (let ((title (nth 1 (assoc package quail-cxterm-package-ext-info))))
      (princ "\"")
      (princ package)
      (princ "\" \"")
      (princ (nth 2 (assoc tit-encode tit-encode-list)))
      (princ "\" \"")
      (princ (or title
		 (if (string-match "[:∷：【]+\\([^:∷：】]+\\)" tit-prompt)
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
    (let ((doc (concat tit-prompt "\n"))
	  (comments (if tit-comments
			(mapconcat 'identity (nreverse tit-comments) "\n")))
	  (doc-ext (nth 2 (assoc package quail-cxterm-package-ext-info))))
      (if comments
	  (setq doc (concat doc "\n" comments "\n")))
      (if doc-ext
	  (setq doc (concat doc "\n" doc-ext "\n")))
      (prin1 doc)
      (terpri))

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
  (let* ((prev-key "")
	 ch key translations pos)
    (princ "(quail-define-rules\n")
    (while (null (eobp))
      (setq ch (following-char))
      (if (or (= ch ?#) (= ch ?\n))
	  (forward-line 1)
	(setq pos (point))
	(skip-chars-forward "^ \t\n")
	(setq key (buffer-substring-no-properties pos (point)))
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
				  (buffer-substring-no-properties pos (point)))
			(buffer-substring-no-properties pos (point)))))
	    (while (not (eolp))
	      (setq pos (point))
	      (skip-chars-forward "^ \t\n")
	      (setq translations (cons (buffer-substring-no-properties
					pos (point))
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
  (let ((coding-system-for-write nil))
    (with-temp-file  (tit-make-quail-package-file-name filename dirname)
      (let ((standard-output (current-buffer)))
	(with-temp-buffer
	  (set-buffer-multibyte nil)
	  ;; Here we must use `raw-text' instead of `no-conversion' to
	  ;; enable auto-decoding of eol format (CRLF->LF).
	  (let ((coding-system-for-read 'raw-text))
	    (insert-file-contents (expand-file-name filename)))

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
	    (goto-char (point-min))
	    (decode-coding-region (point-min) (point-max) coding-system)
	    ;; Explicitly set eol format to `unix'.
	    (setq coding-system-for-write 'utf-8-unix)
	    (remove-text-properties (point-min) (point-max) '(charset nil)))

	  (set-buffer-multibyte t)
	  ;; Set point the starting position of the body part.
	  (goto-char (point-min))
	  (if (not (search-forward "\nBEGIN" nil t))
	      (error "TIT dictionary can't be decoded correctly"))

	  ;; Process the header part.
	  (forward-line 1)
	  (narrow-to-region (point-min) (point))
	  (tit-process-header filename)
	  (widen)

	  ;; Process the body part
	  (tit-process-body)

	  (princ ";; Local Variables:\n")
	  (princ ";; version-control: never\n")
	  (princ ";; no-update-autoloads: t\n")
	  (princ ";; End:\n"))))))

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
	      (setq files (directory-files filename t "\\.tit\\'")))
	  (setq files (list filename)))
	(while files
	  (setq file (expand-file-name (car files)))
	  (when (or force
		    (file-newer-than-file-p
		     file (tit-make-quail-package-file-name file targetdir)))
	    (titdic-convert file targetdir))
	  (setq files (cdr files)))
	(setq command-line-args-left (cdr command-line-args-left)))))
  (kill-emacs 0))


;;; Converter of miscellaneous dictionaries other than TIT format.

;; Alist of input method names and the corresponding information.
;; Each element has this form:
;;   (INPUT-METHOD-NAME		;; Name of the input method.
;;    INPUT-METHOD-TITLE	;; Title string of the input method
;;    DICFILE			;; Name of the source dictionary file.
;;    CODING			;; Coding system of the dictionary file.
;;    QUAILFILE			;; Name of the Quail package file.
;;    CONVERTER			;; Function to generate the Quail package.
;;    COPYRIGHT-NOTICE		;; Copyright notice of the source dictionary.
;;    )

(defvar quail-misc-package-ext-info
  '(("chinese-b5-tsangchi" "倉B"
     "cangjie-table.b5" big5 "tsang-b5.el"
     tsang-b5-converter
     "\
;; # Copyright 2001 Christian Wittern <wittern@iis.sinica.edu.tw>
;; #
;; # Permission to copy and distribute both modified and
;; # unmodified versions is granted without royalty provided
;; # this notice is preserved.")

    ("chinese-b5-quick" "簡B"
     "cangjie-table.b5" big5 "quick-b5.el"
     quick-b5-converter
     "\
;; # Copyright 2001 Christian Wittern <wittern@iis.sinica.edu.tw>
;; #
;; # Permission to copy and distribute both modified and
;; # unmodified versions is granted without royalty provided
;; # this notice is preserved.")

    ("chinese-cns-tsangchi" "倉C"
     "cangjie-table.cns" iso-2022-cn-ext "tsang-cns.el"
     tsang-cns-converter
     "\
;; # Copyright 2001 Christian Wittern <wittern@iis.sinica.edu.tw>
;; #
;; # Permission to copy and distribute both modified and
;; # unmodified versions is granted without royalty provided
;; # this notice is preserved.")

    ("chinese-cns-quick" "簡C"
     "cangjie-table.cns" iso-2022-cn-ext "quick-cns.el"
     quick-cns-converter
     "\
;; # Copyright 2001 Christian Wittern <wittern@iis.sinica.edu.tw>
;; #
;; # Permission to copy and distribute both modified and
;; # unmodified versions is granted without royalty provided
;; # this notice is preserved.")

    ("chinese-py" "拼G"
     "pinyin.map" cn-gb-2312 "PY.el"
     py-converter
     "\
;; \"pinyin.map\" is included in a free package called CCE.  It is
;; available at:
;;	http://ftp.debian.org/debian/dists/potato/main
;;		/source/utils/cce_0.36.orig.tar.gz
;; This package contains the following copyright notice.
;;
;;
;;             Copyright (C) 1999, Rui He, herui@cs.duke.edu
;;
;;
;;                  CCE(Console Chinese Environment) 0.32
;;
;; CCE is free software; you can redistribute it and/or modify it under the
;; terms of the GNU General Public License as published by the Free Software
;; Foundation; either version 1, or (at your option) any later version.
;;
;; CCE is distributed in the hope that it will be useful, but WITHOUT ANY
;; WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;; details.
;;
;; You should have received a copy of the GNU General Public License along with
;; CCE.  If not, see <https://www.gnu.org/licenses/>.")

    ("chinese-ziranma" "自然"
     "ziranma.cin" cn-gb-2312 "ZIRANMA.el"
     ziranma-converter
     "\
;; \"ziranma.cin\" is included in a free package called CCE.  It is
;; available at:
;;	http://ftp.debian.org/debian/dists/potato/main
;;		/source/utils/cce_0.36.orig.tar.gz
;; This package contains the following copyright notice.
;;
;;
;;             Copyright (C) 1999, Rui He, herui@cs.duke.edu
;;
;;
;;                  CCE(Console Chinese Environment) 0.32
;;
;; CCE is free software; you can redistribute it and/or modify it under the
;; terms of the GNU General Public License as published by the Free Software
;; Foundation; either version 1, or (at your option) any later version.
;;
;; CCE is distributed in the hope that it will be useful, but WITHOUT ANY
;; WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;; details.
;;
;; You should have received a copy of the GNU General Public License along with
;; CCE.  If not, see <https://www.gnu.org/licenses/>.")

    ("chinese-ctlau" "刘粤"
     "CTLau.html" cn-gb-2312 "CTLau.el"
     ctlau-gb-converter
     "\
;; \"CTLau.html\" is available at:
;;
;;   http://umunhum.stanford.edu/~lee/chicomp/CTLau.html
;;
;; It contains the following copyright notice:
;;
;; # Copyright (C) 1988-2001  Fung Fung Lee (lee@umunhum.stanford.edu)
;; #
;; # This program is free software; you can redistribute it and/or
;; # modify it under the terms of the GNU General Public License
;; # as published by the Free Software Foundation; either version 2
;; # of the License, or any later version.
;; #
;; # This program is distributed in the hope that it will be useful,
;; # but WITHOUT ANY WARRANTY; without even the implied warranty of
;; # MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; # GNU General Public License for more details.
;; #
;; # You should have received a copy of the GNU General Public License
;; # along with this program.  If not, see <https://www.gnu.org/licenses/>.")

    ("chinese-ctlaub" "劉粵"
     "CTLau-b5.html" big5 "CTLau-b5.el"
     ctlau-b5-converter
     "\
;; \"CTLau-b5.html\" is available at:
;;
;;   http://umunhum.stanford.edu/~lee/chicomp/CTLau-b5.html
;;
;; It contains the following copyright notice:
;;
;; # Copyright (C) 1988-2001  Fung Fung Lee (lee@umunhum.stanford.edu)
;; #
;; # This program is free software; you can redistribute it and/or
;; # modify it under the terms of the GNU General Public License
;; # as published by the Free Software Foundation; either version 2
;; # of the License, or any later version.
;; #
;; # This program is distributed in the hope that it will be useful,
;; # but WITHOUT ANY WARRANTY; without even the implied warranty of
;; # MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; # GNU General Public License for more details.
;; #
;; # You should have received a copy of the GNU General Public License
;; # along with this program.  If not, see <https://www.gnu.org/licenses/>.")
    ))

;; Generate a code of a Quail package in the current buffer from Tsang
;; dictionary in the buffer DICBUF.  The input method name of the
;; Quail package is NAME, and the title string is TITLE.

;; TSANG-P is non-nil, generate 倉頡 input method.  Otherwise
;; generate 簡易 (simple version of 倉頡).  If BIG5-P is non-nil, the
;; input method is for inputting Big5 characters.  Otherwise the input
;; method is for inputting CNS characters.

(defun tsang-quick-converter (dicbuf tsang-p big5-p)
  (let ((fulltitle (if tsang-p (if big5-p "倉頡" "倉頡")
		     (if big5-p "簡易" "簡易")))
	dic)
    (goto-char (point-max))
    (if big5-p
	(insert (format "\"中文輸入【%s】BIG5

	漢語%s輸入鍵盤

   [Q 手] [W 田] [E 水] [R 口] [T 廿] [Y 卜] [U 山] [I 戈] [O 人] [P 心]

    [A 日] [S 尸] [D 木] [F 火] [G 土] [H 竹] [J 十] [L 中]

      [Z  ] [X 難] [C 金] [V 女] [B 月] [N 弓] [M 一]

\\\\<quail-translation-docstring>\"\n"
			fulltitle fulltitle))
      (insert (format "\"中文輸入【%s】CNS

	漢語%s輸入鍵盤

   [Q 手] [W 田] [E 水] [R 口] [T 廿] [Y 卜] [U 山] [I 戈] [O 人] [P 心]

    [A 日] [S 尸] [D 木] [F 火] [G 土] [H 竹] [J 十] [L 中]

      [Z  ] [X 難] [C 金] [V 女] [B 月] [N 弓] [M 一]

\\\\<quail-translation-docstring>\"\n"
		      fulltitle fulltitle)))
    (insert "  '((\".\" . quail-next-translation-block)
   (\",\" . quail-prev-translation-block))
  nil nil)\n\n")
    (insert "(quail-define-rules\n")
    (with-current-buffer dicbuf
      ;; Handle double CR line ends, which result when checking out of
      ;; CVS on MS-Windows.
      (goto-char (point-min))
      (search-forward "A440")
      (beginning-of-line)
      (let ((table (make-hash-table :test 'equal))
	    val)
	(while (not (eobp))
	  (forward-char 5)
	  (let ((trans (char-to-string (following-char)))
		key)
	    (re-search-forward "\\([A-Z]+\\)\r*$" nil t)
	    (setq key (downcase
		       (if (or tsang-p
			       (<= (- (match-end 1) (match-beginning 1)) 1))
			   (match-string 1)
			 (string (char-after (match-beginning 1))
				 (char-after (1- (match-end 1)))))))
	    (setq val (gethash key table))
	    (if val (setq trans (concat val trans)))
	    (puthash key trans table)
	    (forward-line 1)))
	(maphash #'(lambda (key val) (setq dic (cons (cons key val) dic)))
		 table)))
    (setq dic (sort dic (function (lambda (x y) (string< (car x ) (car y))))))
    (dolist (elt dic)
      (insert (format "(%S\t%S)\n" (car elt) (cdr elt))))
    (let ((punctuation '((";" "；﹔，、﹐﹑" "；﹔，、﹐﹑")
			 (":" "：︰﹕．。‧﹒·" "：︰﹕．。・﹒·")
			 ("'" "’‘" "’‘")
			 ("\"" "”“〝〞〃" "”“〝〞〃")
			 ("\\" "＼﹨╲" "＼﹨╲")
			 ("|" "｜︱︳∣" "︱︲����｜")
			 ("/" "／∕╱" "／∕╱")
			 ("?" "？﹖" "？﹖")
			 ("<" "〈＜﹤︿∠" "〈＜﹤︿∠")
			 (">" "〉＞﹥﹀" "〉＞﹦﹀")
			 ("[" "〔【﹝︹︻「『﹁﹃" "〔【﹝︹︻「『﹁﹃")
			 ("]" "〕】﹞︺︼」』﹂﹄" "〕】﹞︺︼」』﹂﹄")
			 ("{" "｛﹛︷ " "｛﹛︷ ")
			 ("}" "｝﹜︸" "｝﹜︸")
			 ("`" "‵′" "′‵")
			 ("~" "～﹋﹌︴﹏" "∼﹋﹌��������")
			 ("!" "！﹗" "！﹗")
			 ("@" "＠﹫" "＠﹫")
			 ("#" "＃﹟" "＃﹟")
			 ("$" "＄﹩" "＄﹩")
			 ("%" "％﹪" "％﹪")
			 ("&" "＆﹠" "＆﹠")
			 ("*" "＊﹡※☆★" "＊﹡※☆★")
			 ("(" "（﹙︵" "（﹙︵")
			 (")" "）﹚︶" "）﹚︶")
			 ("-" "–—¯￣－﹣" "—–‾����－﹣")
			 ("_" "＿ˍ" "＿����")
			 ("=" "＝﹦" "＝﹥")
			 ("+" "＋﹢" "＋﹢"))))
    (dolist (elt punctuation)
      (insert (format "(%S %S)\n" (concat "z" (car elt))
		      (if big5-p (nth 1 elt) (nth 2 elt))))))
    (insert ")\n")))

(defun tsang-b5-converter (dicbuf)
  (tsang-quick-converter dicbuf t t))

(defun quick-b5-converter (dicbuf)
  (tsang-quick-converter dicbuf nil t))

(defun tsang-cns-converter (dicbuf)
  (tsang-quick-converter dicbuf t nil))

(defun quick-cns-converter (dicbuf)
  (tsang-quick-converter dicbuf nil nil))

;; Generate a code of a Quail package in the current buffer from
;; Pinyin dictionary in the buffer DICBUF.  The input method name of
;; the Quail package is NAME, and the title string is TITLE.

(defun py-converter (dicbuf)
  (goto-char (point-max))
  (insert (format "%S\n" "汉字输入∷拼音∷

	拼音方案

 小写英文字母代表「拼音」符号， \"u(yu) 则用 u: 表示∶

Pinyin base input method for Chinese charset GB2312 (`chinese-gb2312').

Pinyin is the standard roman transliteration method for Chinese.
Pinyin uses a sequence of Latin alphabetic characters for each Chinese
character.  The sequence is made by the combination of the initials
\(the beginning sounds) and finals (the ending sounds).

  initials: b p m f d t n l z c s zh ch sh r j q x g k h
  finals: a o e i er ai ei oa ou an en ang eng ong i ia iao ie iu ian in
          iang ing iong u ua uo uai ui uan un uan ueng yu yue yuan yun

  (Note: In the correct Pinyin writing, the sequence \"yu\" in the last
   four finals should be written by the character u-umlaut `ü'.)

With this input method, you enter a Chinese character by first
entering its pinyin spelling.

\\<quail-translation-docstring>

For instance, to input 你, you type \"n i C-n 3\".  The first \"n i\"
is a Pinyin, \"C-n\" selects the next group of candidates (each group
contains at most 10 characters), \"3\" select the third character in
that group.

This input method supports only Han characters.  The related input
method `chinese-py-punct' is the combination of this method and
`chinese-punct'; it supports both Han characters and punctuation
characters.

For double-width GB2312 characters corresponding to ASCII, use the
input method `chinese-qj'.

The correct Pinyin system specifies tones by diacritical marks, but
this input method doesn't use them, which results in easy (you don't
have to know the exact tones), but verbose (many characters are assigned
to the same key sequence) input.  You may also want to try the input
method `chinese-tonepy' with which you must specify tones by digits
\(1..5)."))
  (insert "  '((\"\C-?\" . quail-delete-last-char)
   (\".\" . quail-next-translation)
   (\">\" . quail-next-translation)
   (\",\" . quail-prev-translation)
   (\"<\" . quail-prev-translation))
  nil nil nil nil)\n\n")
  (insert "(quail-define-rules\n")
  (let ((pos (point)))
    (insert-buffer-substring-no-properties dicbuf)
    (goto-char pos)
    (re-search-forward "^[a-z]")
    (beginning-of-line)
    (delete-region pos (point))
    (while (not (eobp))
      (insert "(\"")
      (skip-chars-forward "a-z")
      (insert "\" \"")
      (delete-char 1)
      (end-of-line)
      (while (= (preceding-char) ?\r)
	(delete-char -1))
      (insert "\")")
      (forward-line 1)))
  (insert ")\n"))

;; Generate a code of a Quail package in the current buffer from
;; Ziranma dictionary in the buffer DICBUF.  The input method name of
;; the Quail package is NAME, and the title string is TITLE.

(defun ziranma-converter (dicbuf)
  (let (dic)
    (with-current-buffer dicbuf
      (goto-char (point-min))
      (search-forward "\n%keyname end")
      (forward-line 1)
      (let ((table (make-hash-table :test 'equal))
	    pos key trans val)
	(while (not (eobp))
	  (setq pos (point))
	  (skip-chars-forward "^ \t")
	  (setq key (buffer-substring-no-properties pos (point)))
	  (skip-chars-forward " \t")
	  (setq pos (point))
	  (skip-chars-forward "^\r\n")
	  (setq trans (vector (buffer-substring-no-properties pos (point))))
	  (setq val (gethash key table))
	  (if val (setq trans (vconcat val trans)))
	  (puthash key trans table)
	  (forward-line 1))
	(maphash #'(lambda (key trans)
		     (let ((len (length trans))
			   i)
		       (if (and (= len 1) (= (length (aref trans 0)) 1))
			   (setq trans (aref trans 0))
			 (setq i 0)
			 (while (and (< i len)
				     (= (length (aref trans i)) 1))
			   (setq i (1+ i)))
			 (if (= i len)
			     (setq trans (mapconcat 'identity trans "")))))
		     (setq dic (cons (cons key trans) dic)))
		 table)))
    (setq dic (sort dic (function (lambda (x y) (string< (car x) (car y))))))
    (goto-char (point-max))
    (insert (format "%S\n" "汉字输入∷【自然】∷

                            键盘对照表:
 ┏━━┳━━┳━━┳━━┳━━┳━━┳━━┳━━┳━━┳━━┓
 ┃Ｑ  ┃Ｗ  ┃Ｅ  ┃Ｒ  ┃Ｔ  ┃Ｙ  ┃Ｕsh┃Ｉch┃Ｏ  ┃Ｐ  ┃
 ┃  iu┃  ua┃   e┃ uan┃  ue┃ uai┃   u┃   i┃   o┃  un┃
 ┃    ┃  ia┃    ┃ van┃  ve┃ ing┃    ┃    ┃  uo┃  vn┃
 ┗┳━┻┳━┻┳━┻┳━┻┳━┻┳━┻┳━┻┳━┻┳━┻┳━┛
   ┃Ａ  ┃Ｓ  ┃Ｄ  ┃Ｆ  ┃Ｇ  ┃Ｈ  ┃Ｊ  ┃Ｋ  ┃Ｌ  ┃
   ┃   a┃iong┃uang┃  en┃ eng┃ ang┃  an┃  ao┃  ai┃
   ┃    ┃ ong┃iang┃    ┃  ng┃    ┃    ┃    ┃    ┃
   ┗┳━┻┳━┻┳━┻┳━┻┳━┻┳━┻┳━┻┳━┻┳━┻┳━━┓
     ┃Ｚ  ┃Ｘ  ┃Ｃ  ┃Ｖzh┃Ｂ  ┃Ｎ  ┃Ｍ  ┃，  ┃．  ┃ ／ ┃
     ┃  ei┃  ie┃ iao┃  ui┃  ou┃  in┃ ian┃前页┃后页┃符号┃
     ┃    ┃    ┃    ┃   v┃    ┃    ┃    ┃    ┃    ┃    ┃
     ┗━━┻━━┻━━┻━━┻━━┻━━┻━━┻━━┻━━┻━━┛


Pinyin base input method for Chinese GB2312 characters (`chinese-gb2312').

Pinyin is the standard roman transliteration method for Chinese.
For the details of Pinyin system, see the documentation of the input
method `chinese-py'.

Unlike the standard spelling of Pinyin, in this input method all
initials and finals are assigned to single keys (see the above table).
For instance, the initial \"ch\" is assigned to the key `i', the final
\"iu\" is assigned to the key `q', and tones 1, 2, 3, 4, and 轻声 are
assigned to the keys `q', `w', `e', `r', `t' respectively.

\\<quail-translation-docstring>

To input one-letter words, you type 4 keys, the first two for the
Pinyin of the letter, next one for tone, and the last one is always a
quote (').  For instance, \"vsq'\" input 中.  Exceptions are these
letters.  You can input them just by typing a single key.

	Character: 按 不 次 的 二 发 个 和 出 及 可 了 没
	Key:	   a  b  c  d  e  f  g  h  i  j  k  l  m
	Character: 你 欧 片 七 人 三 他 是 着 我 小 一 在
	Key:	   n  o  p  q  r  s  t  u  v  w  x  y  z

To input two-letter words, you have two ways.  One way is to type 4
keys, two for the first Pinyin, two for the second Pinyin.  For
instance, \"vsgo\" inputs 中国.  Another way is to type 3 keys: 2
initials of two letters, and quote (').  For instance, \"vg'\" also
inputs 中国.

To input three-letter words, you type 4 keys: initials of three
letters, and the last is quote (').  For instance, \"bjy'2\" inputs 北
京鸭 (the last `2' is to select one of the candidates).

To input words of more than three letters, you type 4 keys, initials
of the first three letters and the last letter.  For instance,
\"bjdt\" inputs 北京电视台.

To input symbols and punctuation, type `/' followed by one of `a' to
`z', then select one of the candidates."))
    (insert "  '((\"\C-?\" . quail-delete-last-char)
   (\".\" . quail-next-translation)
   (\"[\" . quail-next-translation)
   (\",\" . quail-prev-translation)
   (\"]\" . quail-prev-translation))
  nil nil nil nil)\n\n")
    (insert "(quail-define-rules\n")
    (dolist (elt dic)
      (insert (format "(%S %S)\n" (car elt) (cdr elt))))
    (insert ")\n")))

;; Generate the code for a Quail package in the current buffer from a
;; CTLau or CTLau-b5 dictionary in the buffer DICBUF.  The input
;; method name of the Quail package is NAME, and the title string is
;; TITLE.  DESCRIPTION is the string shown by describe-input-method.

(defun ctlau-converter (dicbuf description)
  (goto-char (point-max))
  (insert (format "%S\n" description))
  (insert "  '((\"\C-?\" . quail-delete-last-char)
   (\".\" . quail-next-translation)
   (\">\" . quail-next-translation)
   (\",\" . quail-prev-translation)
   (\"<\" . quail-prev-translation))
  nil nil nil nil)\n\n")
  (insert "(quail-define-rules\n")
  (let (dicbuf-start dicbuf-end key-start (pos (point)))
    ;; Find the dictionary, which starts below a horizontal rule and
    ;; ends at the second to last line in the HTML file.
    (with-current-buffer dicbuf
      (goto-char (point-min))
      (re-search-forward "^#<hr>")
      (forward-line 1)
      (setq dicbuf-start (point))
      (goto-char (point-max))
      (re-search-backward "^<hr>")
      (setq dicbuf-end (point)))
    (insert-buffer-substring-no-properties dicbuf dicbuf-start dicbuf-end)
    ;; CTLau-b5.html contains characters (0xa1 0xbc) which show up as
    ;; hollow boxes when the original characters in CTLau.html from
    ;; which the file is converted have no Big5 equivalent.  Go
    ;; through and delete them.
    (goto-char pos)
    (while (search-forward "□" nil t)
      (delete-char -1))
    ;; Uppercase keys in dictionary need to be downcased.  Backslashes
    ;; at the beginning of keys need to be turned into double
    ;; backslashes.
    (goto-char pos)
    (while (not (eobp))
      (insert "(\"")
      (if (char-equal (following-char) ?\\)
	  (insert "\\"))
      (setq key-start (point))
      (skip-chars-forward "\\\\A-Z")
      (downcase-region key-start (point))
      (insert "\" \"")
      (delete-char 1)
      (end-of-line)
      (while (= (preceding-char) ?\r)
	(delete-char -1))
      (insert "\")")
      (forward-line 1)))
  (insert ")\n"))

(defun ctlau-gb-converter (dicbuf)
  (ctlau-converter dicbuf
"汉字输入∷刘锡祥式粤音∷

 刘锡祥式粤语注音方案
 Sidney Lau's Cantonese transcription scheme as described in his book
 \"Elementary Cantonese\", The Government Printer, Hong Kong, 1972.
 This file was prepared by Fung Fung Lee (李枫峰).
 Originally converted from CTCPS3.tit
 Last modified: June 2, 1993.

 Some infrequent GB characters are accessed by typing \\, followed by
 the Cantonese romanization of the respective radical (部首)."))

(defun ctlau-b5-converter (dicbuf)
  (ctlau-converter dicbuf
"漢字輸入：劉錫祥式粵音：

 劉錫祥式粵語注音方案
 Sidney Lau's Cantonese transcription scheme as described in his book
 \"Elementary Cantonese\", The Government Printer, Hong Kong, 1972.
 This file was prepared by Fung Fung Lee (李楓峰).
 Originally converted from CTCPS3.tit
 Last modified: June 2, 1993.

 Some infrequent characters are accessed by typing \\, followed by
 the Cantonese romanization of the respective radical (部首)."))

(declare-function dos-8+3-filename "dos-fns.el" (filename))

(defun miscdic-convert (filename &optional dirname)
  "Convert a dictionary file FILENAME into a Quail package.
Optional argument DIRNAME if specified is the directory name under which
the generated Quail package is saved."
  (interactive "FInput method dictionary file: ")
  (or (file-readable-p filename)
      (error "%s does not exist" filename))
  (let ((tail quail-misc-package-ext-info)
	coding-system-for-write
	slot
	name title dicfile coding quailfile converter copyright)
    (while tail
      (setq slot (car tail)
	    dicfile (nth 2 slot)
	    quailfile (nth 4 slot))
      (when (and (or (string-match dicfile filename)
		     ;; MS-DOS filesystem truncates file names to 8+3
		     ;; limits, so "cangjie-table.cns" becomes
		     ;; "cangjie-.cns", and the above string-match
		     ;; fails.  Give DOS users a chance...
		     (and (fboundp 'msdos-long-file-names)
			  (not (msdos-long-file-names))
			  (string-match (dos-8+3-filename dicfile) filename)))
		 (if (file-newer-than-file-p
		      filename (expand-file-name quailfile dirname))
		     t
		   (message "%s is up to date" quailfile)
		   nil))
	(setq name (car slot)
	      title (nth 1 slot)
	      coding (nth 3 slot)
	      converter (nth 5 slot)
	      copyright (nth 6 slot))
	;; Explicitly set eol format to `unix'.
	(setq coding-system-for-write 'utf-8-unix)
	(with-temp-file (expand-file-name quailfile dirname)
	  (insert (format-message ";; Quail package `%s'\n" name))
	  (insert (format-message
		   ";;   Generated by the command `miscdic-convert'\n"))
	  (insert ";;   Source dictionary file: " dicfile "\n")
	  (insert ";;   Copyright notice of the source file\n")
	  (insert ";;------------------------------------------------------\n")
	  (insert copyright "\n")
	  (insert ";;------------------------------------------------------\n")
	  (insert "\n")
	  (insert ";;; Code:\n\n")
	  (insert "(require 'quail)\n")
	  (insert "(quail-define-package \"" name "\" \""
		  (if (eq coding 'big5) "Chinese-BIG5"
		    (if (eq coding 'iso-2022-cn-ext) "Chinese-CNS"
		      "Chinese-GB"))
		  "\" \"" title "\" t\n")
          (let ((coding-system-for-read
                 (coding-system-change-eol-conversion coding 'unix))
                (dstbuf (current-buffer)))
            (with-temp-buffer
              (insert-file-contents filename)
              (let ((dicbuf (current-buffer)))
                (with-current-buffer dstbuf
                  (funcall converter dicbuf)))))
	  (insert ";; Local Variables:\n"
		  ";; version-control: never\n"
		  ";; no-update-autoloads: t\n"
		  ";; End:\n\n"
		  ";;; " quailfile " ends here\n")))
      (setq tail (cdr tail)))))

(defun batch-miscdic-convert ()
  "Run `miscdic-convert' on the files remaining on the command line.
Use this from the command line, with `-batch';
it won't work in an interactive Emacs.
If there's an argument \"-dir\", the next argument specifies a directory
to store generated Quail packages."
  (defvar command-line-args-left)	; Avoid compiler warning.
  (if (not noninteractive)
      (error "`batch-miscdic-convert' should be used only with -batch"))
  (let ((dir default-directory)
	filename)
    (while command-line-args-left
      (if (string= (car command-line-args-left) "-dir")
	  (progn
	    (setq command-line-args-left (cdr command-line-args-left))
	    (setq dir (car command-line-args-left))
	    (setq command-line-args-left (cdr command-line-args-left))))
      (setq filename (car command-line-args-left)
	    command-line-args-left (cdr command-line-args-left))
      (if (file-directory-p filename)
	  (dolist (file (directory-files filename t nil t))
	    (or (file-directory-p file)
		(miscdic-convert file dir)))
	(miscdic-convert filename dir))))
  (kill-emacs 0))

(defun pinyin-convert ()
  "Convert text file pinyin.map into an elisp library.
The library is named pinyin.el, and contains the constant
`pinyin-character-map'."
  (let ((src-file (car command-line-args-left))
        (dst-file (cadr command-line-args-left))
        (coding-system-for-write 'utf-8-unix))
    (with-temp-file dst-file
      (insert ";; This file is automatically generated from pinyin.map,\
 by the\n;; function pinyin-convert.\n\n")
      (insert "(defconst pinyin-character-map\n'(")
      (let ((pos (point)))
        (insert-file-contents src-file)
        (goto-char pos)
        (re-search-forward "^[a-z]")
        (beginning-of-line)
        (delete-region pos (point))
        (while (not (eobp))
          (insert "(\"")
          (skip-chars-forward "a-z")
          (insert "\" . \"")
          (delete-char 1)
          (end-of-line)
          (while (= (preceding-char) ?\r)
	    (delete-char -1))
          (insert "\")")
          (forward-line 1)))
      (insert ")\n\"An alist holding correspondences between pinyin syllables\
 and\nChinese characters.\")\n\n")
      (insert "(provide 'pinyin)\n"))
    (kill-emacs 0)))

;; Prevent "Local Variables" above confusing Emacs.


;;; titdic-cnv.el ends here
