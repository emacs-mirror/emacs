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

;; Alist of input method names and the corresponding title and extra
;; docstring.  For each of input method generated from TIT dictionary,
;; a docstring is automatically generated from the comments in the
;; dictionary.  The extra docstring in this alist is to add more
;; information.
;; The command describe-input-method shows the automatically generated
;; docstring, then an extra docstrings while replacing the form \<VAR>
;; by the value of variable VAR.  For instance, the form
;; \<quail-translation-docstring> is replaced by a description about
;; how to select a translation from a list of candidates.

(defvar quail-cxterm-package-ext-info
  '(("chinese-4corner" "$(0(?-F(B")
    ("chinese-array30" "$(0#R#O(B")
    ("chinese-ccdospy" "$AKuF4(B"
     "Pinyin base input method for Chinese charset GB2312 \(`chinese-gb2312').

Pinyin is the standared roman transliteration method for Chinese.
For the detail of Pinyin system, see the documentation of the input
method `chinese-py'.

This input method works almost the same way as `chinese-py'.  The
difference is that you type a single key for these Pinyin spelling.
    Pinyin:  zh  en  eng ang ch  an  ao  ai  ong sh  ing  yu($A(9(B)
    keyseq:   a   f   g   h   i   j   k   l   s   u   y   v
For expample:
    Chinese:  $A0!(B    $A9{(B    $AVP(B    $AND(B    $A9b(B    $ASq(B    $AH+(B
    Pinyin:   a    guo   zhong  wen  guang  yu   quan
    Keyseq:   a1   guo4   as1   wf4  guh1  yu..6 qvj6

\\<quail-translation-docstring>

For double-width GB2312 characters correponding to ASCII, use the
input method `chinese-qj'.")

    ("chinese-ctlau" "$AAuTA(B")

    ("chinese-ctlaub" "$(0N,Gn(B")

    ("chinese-ecdict" "$(05CKH(B"
"In this input method, you enter a Chinese (Big5) charactere or word
by typing the corresponding English word.  For example, if you type
\"computer\", \"$(0IZH+(B\" is input.

\\<quail-translation-docstring>")

    ("chinese-etzy" "$(06/0D(B"
"Zhuyin base input method for Chinese Big5 characters (`chinese-big5-1',
`chinese-big5-2').

Zhuyin is a kind of phonetic symbol.  One to three Zhuyin symbols
compose one Chinese character.

In this input method, you enter a Chinese character by first typing
keys corresponding to Zhuyin symbols (see the above table) followed by
SPC, 1, 2, 3, or 4 specifing a tone (SPC:$(0?v(N(B, 1:$(0M=Vy(B, 2:$(0Dm(N(B, 3: $(0&9Vy(B,
4:$(0(+Vy(B).

\\<quail-translation-docstring>")

    ("chinese-punct-b5" "$(0O:(BB"
     "Input method for Chinese punctuations and symbols of Big5
\(`chinese-big5-1' and `chinese-big5-2').")

    ("chinese-punct" "$A1j(BG"
     "Input method for Chinese punctuations and symbols of GB2312
\(`chinese-gb2312').")

    ("chinese-py-b5" "$(03<(BB"
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

    ("chinese-py" "$AF4(BG"
     "Pinyin base input method for Chinese charset GB2312
\(`chinese-gb2312').

Pinyin is the standared roman transliteration method for Chinese.
Pinyin uses a sequence of Latin alphabetic characters for each Chinese
character.  The sequence is made by the combination of the initials
\(the beginning sounds) and finals \(the ending sounds).

  initials: b p m f d t n l z c s zh ch sh r j q x g k h
  finals: a o e i er ai ei oa ou an en ang eng ong i ia iao ie iu ian in
          iang ing iong u ua uo uai ui uan un uan ueng yu yue yuan yun

  (Note: In the correct Pinyin writing, the sequence \"yu\" in the last
   four finals should be written by the character u-umlaut `$A(9(B'.)

With this input method, you enter a Chinese character by first
entering its pinyin spelling.

\\<quail-translation-docstring>

For instance, to input $ADc(B, you type \"n i C-n 3\".  The first \"n i\"
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
\(1..5).")

    ("chinese-qj-b5" "$(0)A(BB")

    ("chinese-qj" "$AH+(BG")

    ("chinese-sw" "$AJWN2(B"
"Radical base input method for Chinese charset GB2312 (`chinese-gb2312').

In this input method, you enter a Chinese character by typing two
keys.  The first key corresponds to the first ($AJW(B) radical, the second
key corresponds to the last ($AN2(B) radical.  The correspondence of keys
and radicals is as below:

 first radical:
 a  b  c  d  e  f  g  h  i  j  k  l  m  n  o  p  q  r  s  t  u  v  w  x  y  z
 $APD(B $AZ"(B $AJ,(B $AX<(B $A;p(B $A?Z(B $A^P(B $Ac_(B $AZ%(B $A\3(B $AXi(B $AD>(B $Alj(B $Ab;(B $ATB(B $Afy(B $AJ/(B $AMu(B $A0K(B $AX/(B $AHU(B $AeA(B $Aak(B $AVq(B $AR;(B $AHK(B 
 last radical:
 a  b  c  d  e  f  g  h  i  j  k  l  m  n  o  p  q  r  s  t  u  v  w  x  y  z
 $ASV(B $AI=(B $AMA(B $A56(B $AZb(B $A?Z(B $ARB(B $Aqb(B $A4s(B $A6!(B $A[L(B $Ala(B $AJ.(B $A4u(B $AXg(B $ACE(B $A=q(B $AX-(B $AE.(B $ARR(B $A`m(B $AP!(B $A3'(B $A3f(B $A_.(B $A27(B 

\<quail-translation-docstring>")

    ("chinese-tonepy" "$A5wF4(B"
     "Pinyin base input method for Chinese charset GB2312 (`chinese-gb2312').

Pinyin is the standared roman transliteration method for Chinese.
For the details of Pinyin system, see the documentation of the input
method `chinese-py'.

This input method works almost the same way as `chinese-py'.  The
difference is that you must type 1..5 after each Pinyin spelling to
specify a tone (1:$ARuF=(B, 2:$AQtF=(B, 3:$AIOIy(B, 4$AOBIy(B, 5:$AGaIy(B).

\<quail-translation-docstring>

For instance, to input $ADc(B, you type \"n i 3 3\", the first \"n i\" is
a Pinyin, the next \"3\" specifies tone, and the last \"3\" selects
the third character from the candidate list.

For double-width GB2312 characters correponding to ASCII, use the
input method `chinese-qj'.")

    ("chinese-ziranma" "$AK+F4(B"
"Pinyin base input method for Chinese GB2312 characters (`chinese-gb2312').

Pinyin is the standard roman transliteration method for Chinese.
For the details of Pinyin system, see the documentation of the input
method `chinese-py'.

unlike the standard spelling of Pinyin, in this input method all
initials and finals are assigned to single keys (see the above table).
For instance, the initial \"ch\" is assigned to the key `i', the final
\"iu\" is assigned to the key `q', and tones 1, 2, 3, 4, and $AGaIy(B are
assigned to the keys `q', `w', `e', `r', `t' respectively.

\<quail-translation-docstring>

To input one-letter words, you type 4 keys, the first two for the
Pinyin of the letter, next one for tone, and the last one is always a
quote (').  For instance, \"vsq'1\" input $AVP(B.  Exceptions are these
letters.  You can input them just by typing a single key.

	Character: $A04(B $A2;(B $A4N(B $A5D(B $A6~(B $A7"(B $A8v(B $A:M(B $A3v(B $A<0(B $A?I(B $AAK(B $AC;(B
	Key:	   a  b  c  d  e  f  g  h  i  j  k  l  m
	Character: $ADc(B $AE7(B $AF,(B $AF_(B $AHK(B $AH}(B $AK{(B $AJG(B $AWE(B $ANR(B $AP!(B $AR;(B $ATZ(B
	Key:	   n  o  p  q  r  s  t  u  v  w  x  y  z

To input two-letter words, you have two ways.  One way is to type 4
keys, two for the first Pinyin, two for the second Pinyin.  For
instance, \"vsgo\" inputs $AVP9z(B.  Another way is to type 3 keys: 2
initials of two letters, and quote (').  For instance, \"vg'\" also
inputs $AVP9z(B.

To input three-letter words, you type 4 keys: initials of three
letters, and the last is quote (').  For instance, \"bjy'2\" inputs $A11(B
$A>)Q<(B (the last `2' is to select one of the candidates).

To input words of more than three letters, you type 4 keys, initials
of the first three letters and the last letter.  For instance,
\"bjdt\" inputs $A11>)5gJSL((B.

To input symbols and punctuations, type `/' followed by one of `a' to
`z', then select one of the candidates.

")

    ("chinese-zozy" "$(0I\0D(B"
"Zhuyin base input method for Chinese Big5 characters (`chinese-big5-1',
`chinese-big5-2').

Zhuyin is a kind of a phonetic symbol.  One to three Zhuyin symbols
compose a Chinese character.

In this input method, you enter a Chinese character by first typing
keys corresponding to Zhuyin symbols (see the above table) followed by
SPC, 6, 3, 4, or 7 specifing a tone (SPC:$(0?v(N(B, 6:$(0Dm(N(B, 3:$(0&9Vy(B, 4:$(0(+Vy(B,
7:$(0M=Vy(B).

\<quail-translation-docstring>")))

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
		      (setq tit-prompt (tit-read-key-value))
		      ;; Some TIT dictionaies that are encoded by
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
	(princ (buffer-substring pos (point)))
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
