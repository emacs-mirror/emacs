;;; mm-util.el --- Utility functions for Mule and low level things
;; Copyright (C) 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005
;;   Free Software Foundation, Inc.

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
;;	MORIOKA Tomohiko <morioka@jaist.ac.jp>
;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;;; Code:

(eval-when-compile (require 'cl))
(require 'mail-prsvr)

(eval-and-compile
  (mapcar
   (lambda (elem)
     (let ((nfunc (intern (format "mm-%s" (car elem)))))
       (if (fboundp (car elem))
	   (defalias nfunc (car elem))
	 (defalias nfunc (cdr elem)))))
   '((decode-coding-string . (lambda (s a) s))
     (encode-coding-string . (lambda (s a) s))
     (encode-coding-region . ignore)
     (coding-system-list . ignore)
     (decode-coding-region . ignore)
     (char-int . identity)
     (coding-system-equal . equal)
     (annotationp . ignore)
     (set-buffer-file-coding-system . ignore)
     (make-char
      . (lambda (charset int)
	  (int-to-char int)))
     (read-charset
      . (lambda (prompt)
	  "Return a charset."
	  (intern
	   (completing-read
	    prompt
	    (mapcar (lambda (e) (list (symbol-name (car e))))
		    mm-mime-mule-charset-alist)
	    nil t))))
     (subst-char-in-string
      . (lambda (from to string &optional inplace)
	  ;; stolen (and renamed) from nnheader.el
	  "Replace characters in STRING from FROM to TO.
	  Unless optional argument INPLACE is non-nil, return a new string."
	  (let ((string (if inplace string (copy-sequence string)))
		(len (length string))
		(idx 0))
	    ;; Replace all occurrences of FROM with TO.
	    (while (< idx len)
	      (when (= (aref string idx) from)
		(aset string idx to))
	      (setq idx (1+ idx)))
	    string)))
     (string-as-unibyte . identity)
     (string-make-unibyte . identity)
     ;; string-as-multibyte often doesn't really do what you think it does.
     ;; Example:
     ;;    (aref (string-as-multibyte "\201") 0) -> 129 (aka ?\201)
     ;;    (aref (string-as-multibyte "\300") 0) -> 192 (aka ?\300)
     ;;    (aref (string-as-multibyte "\300\201") 0) -> 192 (aka ?\300)
     ;;    (aref (string-as-multibyte "\300\201") 1) -> 129 (aka ?\201)
     ;; but
     ;;    (aref (string-as-multibyte "\201\300") 0) -> 2240
     ;;    (aref (string-as-multibyte "\201\300") 1) -> <error>
     ;; Better use string-to-multibyte or encode-coding-string.
     ;; If you really need string-as-multibyte somewhere it's usually
     ;; because you're using the internal emacs-mule representation (maybe
     ;; because you're using string-as-unibyte somewhere), which is
     ;; generally a problem in itself.
     ;; Here is an approximate equivalence table to help think about it:
     ;; (string-as-multibyte s)   ~= (decode-coding-string s 'emacs-mule)
     ;; (string-to-multibyte s)   ~= (decode-coding-string s 'binary)
     ;; (string-make-multibyte s) ~= (decode-coding-string s locale-coding-system)
     (string-as-multibyte . identity)
     (string-to-multibyte
      . (lambda (string)
	  "Return a multibyte string with the same individual chars as string."
	  (mapconcat
	   (lambda (ch) (mm-string-as-multibyte (char-to-string ch)))
	   string "")))
     (multibyte-string-p . ignore)
     ;; It is not a MIME function, but some MIME functions use it.
     (make-temp-file . (lambda (prefix &optional dir-flag)
			 (let ((file (expand-file-name
				      (make-temp-name prefix)
				      (if (fboundp 'temp-directory)
					  (temp-directory)
					temporary-file-directory))))
			   (if dir-flag
			       (make-directory file))
			   file)))
     (insert-byte . insert-char)
     (multibyte-char-to-unibyte . identity))))

(eval-and-compile
  (cond
   ((fboundp 'replace-in-string)
    (defalias 'mm-replace-in-string 'replace-in-string))
   ((fboundp 'replace-regexp-in-string)
    (defun mm-replace-in-string (string regexp newtext &optional literal)
      "Replace all matches for REGEXP with NEWTEXT in STRING.
If LITERAL is non-nil, insert NEWTEXT literally.  Return a new
string containing the replacements.

This is a compatibility function for different Emacsen."
      (replace-regexp-in-string regexp newtext string nil literal)))
   (t
    (defun mm-replace-in-string (string regexp newtext &optional literal)
      "Replace all matches for REGEXP with NEWTEXT in STRING.
If LITERAL is non-nil, insert NEWTEXT literally.  Return a new
string containing the replacements.

This is a compatibility function for different Emacsen."
      (let ((start 0) tail)
	(while (string-match regexp string start)
	  (setq tail (- (length string) (match-end 0)))
	  (setq string (replace-match newtext nil literal string))
	  (setq start (- (length string) tail))))
      string))))

(eval-and-compile
  (defalias 'mm-char-or-char-int-p
    (cond
     ((fboundp 'char-or-char-int-p) 'char-or-char-int-p)
     ((fboundp 'char-valid-p) 'char-valid-p)
     (t 'identity))))

;; Fixme:  This seems always to be used to read a MIME charset, so it
;; should be re-named and fixed (in Emacs) to offer completion only on
;; proper charset names (base coding systems which have a
;; mime-charset defined).  XEmacs doesn't believe in mime-charset;
;; test with
;;   `(or (coding-system-get 'iso-8859-1 'mime-charset)
;;        (coding-system-get 'iso-8859-1 :mime-charset))'
;; Actually, there should be an `mm-coding-system-mime-charset'.
(eval-and-compile
  (defalias 'mm-read-coding-system
    (cond
     ((fboundp 'read-coding-system)
      (if (and (featurep 'xemacs)
	       (<= (string-to-number emacs-version) 21.1))
	  (lambda (prompt &optional default-coding-system)
	    (read-coding-system prompt))
	'read-coding-system))
     (t (lambda (prompt &optional default-coding-system)
	  "Prompt the user for a coding system."
	  (completing-read
	   prompt (mapcar (lambda (s) (list (symbol-name (car s))))
			  mm-mime-mule-charset-alist)))))))

(defvar mm-coding-system-list nil)
(defun mm-get-coding-system-list ()
  "Get the coding system list."
  (or mm-coding-system-list
      (setq mm-coding-system-list (mm-coding-system-list))))

(defun mm-coding-system-p (cs)
  "Return non-nil if CS is a symbol naming a coding system.
In XEmacs, also return non-nil if CS is a coding system object.
If CS is available, return CS itself in Emacs, and return a coding
system object in XEmacs."
  (if (fboundp 'find-coding-system)
      (and cs (find-coding-system cs))
    (if (fboundp 'coding-system-p)
	(when (coding-system-p cs)
	  cs)
      ;; Is this branch ever actually useful?
      (car (memq cs (mm-get-coding-system-list))))))

(defvar mm-charset-synonym-alist
  `(
    ;; Not in XEmacs, but it's not a proper MIME charset anyhow.
    ,@(unless (mm-coding-system-p 'x-ctext)
       '((x-ctext . ctext)))
    ;; ISO-8859-15 is very similar to ISO-8859-1.  But it's _different_!
    ,@(unless (mm-coding-system-p 'iso-8859-15)
       '((iso-8859-15 . iso-8859-1)))
    ;; BIG-5HKSCS is similar to, but different than, BIG-5.
    ,@(unless (mm-coding-system-p 'big5-hkscs)
	'((big5-hkscs . big5)))
    ;; Windows-1252 is actually a superset of Latin-1.  See also
    ;; `gnus-article-dumbquotes-map'.
    ,@(unless (mm-coding-system-p 'windows-1252)
       (if (mm-coding-system-p 'cp1252)
	   '((windows-1252 . cp1252))
	 '((windows-1252 . iso-8859-1))))
    ;; Windows-1250 is a variant of Latin-2 heavily used by Microsoft
    ;; Outlook users in Czech republic. Use this to allow reading of their
    ;; e-mails. cp1250 should be defined by M-x codepage-setup.
    ,@(if (and (not (mm-coding-system-p 'windows-1250))
	       (mm-coding-system-p 'cp1250))
	  '((windows-1250 . cp1250)))
    )
  "A mapping from invalid charset names to the real charset names.")

(defvar mm-binary-coding-system
  (cond
   ((mm-coding-system-p 'binary) 'binary)
   ((mm-coding-system-p 'no-conversion) 'no-conversion)
   (t nil))
  "100% binary coding system.")

(defvar mm-text-coding-system
  (or (if (memq system-type '(windows-nt ms-dos ms-windows))
	  (and (mm-coding-system-p 'raw-text-dos) 'raw-text-dos)
	(and (mm-coding-system-p 'raw-text) 'raw-text))
      mm-binary-coding-system)
  "Text-safe coding system (For removing ^M).")

(defvar mm-text-coding-system-for-write nil
  "Text coding system for write.")

(defvar mm-auto-save-coding-system
  (cond
   ((mm-coding-system-p 'utf-8-emacs)	; Mule 7
    (if (memq system-type '(windows-nt ms-dos ms-windows))
	(if (mm-coding-system-p 'utf-8-emacs-dos)
	    'utf-8-emacs-dos mm-binary-coding-system)
      'utf-8-emacs))
   ((mm-coding-system-p 'emacs-mule)
    (if (memq system-type '(windows-nt ms-dos ms-windows))
	(if (mm-coding-system-p 'emacs-mule-dos)
	    'emacs-mule-dos mm-binary-coding-system)
      'emacs-mule))
   ((mm-coding-system-p 'escape-quoted) 'escape-quoted)
   (t mm-binary-coding-system))
  "Coding system of auto save file.")

(defvar mm-universal-coding-system mm-auto-save-coding-system
  "The universal coding system.")

;; Fixme: some of the cars here aren't valid MIME charsets.  That
;; should only matter with XEmacs, though.
(defvar mm-mime-mule-charset-alist
  `((us-ascii ascii)
    (iso-8859-1 latin-iso8859-1)
    (iso-8859-2 latin-iso8859-2)
    (iso-8859-3 latin-iso8859-3)
    (iso-8859-4 latin-iso8859-4)
    (iso-8859-5 cyrillic-iso8859-5)
    ;; Non-mule (X)Emacs uses the last mule-charset for 8bit characters.
    ;; The fake mule-charset, gnus-koi8-r, tells Gnus that the default
    ;; charset is koi8-r, not iso-8859-5.
    (koi8-r cyrillic-iso8859-5 gnus-koi8-r)
    (iso-8859-6 arabic-iso8859-6)
    (iso-8859-7 greek-iso8859-7)
    (iso-8859-8 hebrew-iso8859-8)
    (iso-8859-9 latin-iso8859-9)
    (iso-8859-14 latin-iso8859-14)
    (iso-8859-15 latin-iso8859-15)
    (viscii vietnamese-viscii-lower)
    (iso-2022-jp latin-jisx0201 japanese-jisx0208 japanese-jisx0208-1978)
    (euc-kr korean-ksc5601)
    (gb2312 chinese-gb2312)
    (big5 chinese-big5-1 chinese-big5-2)
    (tibetan tibetan)
    (thai-tis620 thai-tis620)
    (windows-1251 cyrillic-iso8859-5)
    (iso-2022-7bit ethiopic arabic-1-column arabic-2-column)
    (iso-2022-jp-2 latin-iso8859-1 greek-iso8859-7
		   latin-jisx0201 japanese-jisx0208-1978
		   chinese-gb2312 japanese-jisx0208
		   korean-ksc5601 japanese-jisx0212)
    (iso-2022-int-1 latin-iso8859-1 greek-iso8859-7
		    latin-jisx0201 japanese-jisx0208-1978
		    chinese-gb2312 japanese-jisx0208
		    korean-ksc5601 japanese-jisx0212
		    chinese-cns11643-1 chinese-cns11643-2)
    (iso-2022-int-1 latin-iso8859-1 latin-iso8859-2
		    cyrillic-iso8859-5 greek-iso8859-7
		    latin-jisx0201 japanese-jisx0208-1978
		    chinese-gb2312 japanese-jisx0208
		    korean-ksc5601 japanese-jisx0212
		    chinese-cns11643-1 chinese-cns11643-2
		    chinese-cns11643-3 chinese-cns11643-4
		    chinese-cns11643-5 chinese-cns11643-6
		    chinese-cns11643-7)
    (iso-2022-jp-3 latin-jisx0201 japanese-jisx0208-1978 japanese-jisx0208
		   japanese-jisx0213-1 japanese-jisx0213-2)
    (shift_jis latin-jisx0201 katakana-jisx0201 japanese-jisx0208)
    ,(if (or (not (fboundp 'charsetp)) ;; non-Mule case
	     (charsetp 'unicode-a)
	     (not (mm-coding-system-p 'mule-utf-8)))
	 '(utf-8 unicode-a unicode-b unicode-c unicode-d unicode-e)
       ;; If we have utf-8 we're in Mule 5+.
       (append '(utf-8)
	       (delete 'ascii
		       (coding-system-get 'mule-utf-8 'safe-charsets)))))
  "Alist of MIME-charset/MULE-charsets.")

(defun mm-enrich-utf-8-by-mule-ucs ()
  "Make the `utf-8' MIME charset usable by the Mule-UCS package.
This function will run when the `un-define' module is loaded under
XEmacs, and fill the `utf-8' entry in `mm-mime-mule-charset-alist'
with Mule charsets.  It is completely useless for Emacs."
  (unless (cdr (delete '(mm-enrich-utf-8-by-mule-ucs)
		       (assoc "un-define" after-load-alist)))
    (setq after-load-alist
	  (delete '("un-define") after-load-alist)))
  (when (boundp 'unicode-basic-translation-charset-order-list)
    (condition-case nil
	(let ((val (delq
		    'ascii
		    (copy-sequence
		     (symbol-value
		      'unicode-basic-translation-charset-order-list))))
	      (elem (assq 'utf-8 mm-mime-mule-charset-alist)))
	  (if elem
	      (setcdr elem val)
	    (setq mm-mime-mule-charset-alist
		  (nconc mm-mime-mule-charset-alist
			 (list (cons 'utf-8 val))))))
      (error))))

;; Correct by construction, but should be unnecessary for Emacs:
(if (featurep 'xemacs)
    (eval-after-load "un-define" '(mm-enrich-utf-8-by-mule-ucs))
  (when (and (fboundp 'coding-system-list)
	     (fboundp 'sort-coding-systems))
    (let ((css (sort-coding-systems (coding-system-list 'base-only)))
	  cs mime mule alist)
      (while css
	(setq cs (pop css)
	      mime (or (coding-system-get cs :mime-charset) ; Emacs 22
		       (coding-system-get cs 'mime-charset)))
	(when (and mime
		   (not (eq t (setq mule
				    (coding-system-get cs 'safe-charsets))))
		   (not (assq mime alist)))
	  (push (cons mime (delq 'ascii mule)) alist)))
      (setq mm-mime-mule-charset-alist (nreverse alist)))))

(defvar mm-hack-charsets '(iso-8859-15 iso-2022-jp-2)
  "A list of special charsets.
Valid elements include:
`iso-8859-15'    convert ISO-8859-1, -9 to ISO-8859-15 if ISO-8859-15 exists.
`iso-2022-jp-2'  convert ISO-2022-jp to ISO-2022-jp-2 if ISO-2022-jp-2 exists."
)

(defvar mm-iso-8859-15-compatible
  '((iso-8859-1 "\xA4\xA6\xA8\xB4\xB8\xBC\xBD\xBE")
    (iso-8859-9 "\xA4\xA6\xA8\xB4\xB8\xBC\xBD\xBE\xD0\xDD\xDE\xF0\xFD\xFE"))
  "ISO-8859-15 exchangeable coding systems and inconvertible characters.")

(defvar mm-iso-8859-x-to-15-table
  (and (fboundp 'coding-system-p)
       (mm-coding-system-p 'iso-8859-15)
       (mapcar
	(lambda (cs)
	  (if (mm-coding-system-p (car cs))
	      (let ((c (string-to-char
			(decode-coding-string "\341" (car cs)))))
		(cons (char-charset c)
		      (cons
		       (- (string-to-char
			   (decode-coding-string "\341" 'iso-8859-15)) c)
		       (string-to-list (decode-coding-string (car (cdr cs))
							     (car cs))))))
	    '(gnus-charset 0)))
	mm-iso-8859-15-compatible))
  "A table of the difference character between ISO-8859-X and ISO-8859-15.")

(defcustom mm-coding-system-priorities
  (if (boundp 'current-language-environment)
      (let ((lang (symbol-value 'current-language-environment)))
	(cond ((string= lang "Japanese")
	       ;; Japanese users prefer iso-2022-jp to euc-japan or
	       ;; shift_jis, however iso-8859-1 should be used when
	       ;; there are only ASCII text and Latin-1 characters.
	       '(iso-8859-1 iso-2022-jp iso-2022-jp-2 shift_jis utf-8)))))
  "Preferred coding systems for encoding outgoing messages.

More than one suitable coding system may be found for some text.
By default, the coding system with the highest priority is used
to encode outgoing messages (see `sort-coding-systems').  If this
variable is set, it overrides the default priority."
  :version "21.2"
  :type '(repeat (symbol :tag "Coding system"))
  :group 'mime)

;; ??
(defvar mm-use-find-coding-systems-region
  (fboundp 'find-coding-systems-region)
  "Use `find-coding-systems-region' to find proper coding systems.

Setting it to nil is useful on Emacsen supporting Unicode if sending
mail with multiple parts is preferred to sending a Unicode one.")

;;; Internal variables:

;;; Functions:

(defun mm-mule-charset-to-mime-charset (charset)
  "Return the MIME charset corresponding to the given Mule CHARSET."
  (if (and (fboundp 'find-coding-systems-for-charsets)
	   (fboundp 'sort-coding-systems))
      (let ((css (sort (sort-coding-systems
			(find-coding-systems-for-charsets (list charset)))
		       'mm-sort-coding-systems-predicate))
	    cs mime)
	(while (and (not mime)
		    css)
	  (when (setq cs (pop css))
	    (setq mime (or (coding-system-get cs :mime-charset)
			   (coding-system-get cs 'mime-charset)))))
	mime)
    (let ((alist (mapcar (lambda (cs)
			   (assq cs mm-mime-mule-charset-alist))
			 (sort (mapcar 'car mm-mime-mule-charset-alist)
			       'mm-sort-coding-systems-predicate)))
	  out)
      (while alist
	(when (memq charset (cdar alist))
	  (setq out (caar alist)
		alist nil))
	(pop alist))
      out)))

(defun mm-charset-to-coding-system (charset &optional lbt)
  "Return coding-system corresponding to CHARSET.
CHARSET is a symbol naming a MIME charset.
If optional argument LBT (`unix', `dos' or `mac') is specified, it is
used as the line break code type of the coding system."
  (when (stringp charset)
    (setq charset (intern (downcase charset))))
  (when lbt
    (setq charset (intern (format "%s-%s" charset lbt))))
  (cond
   ((null charset)
    charset)
   ;; Running in a non-MULE environment.
   ((or (null (mm-get-coding-system-list))
	(not (fboundp 'coding-system-get)))
    charset)
   ;; ascii
   ((eq charset 'us-ascii)
    'ascii)
   ;; Check to see whether we can handle this charset.  (This depends
   ;; on there being some coding system matching each `mime-charset'
   ;; property defined, as there should be.)
   ((and (mm-coding-system-p charset)
;;; Doing this would potentially weed out incorrect charsets.
;;; 	 charset
;;; 	 (eq charset (coding-system-get charset 'mime-charset))
	 )
    charset)
   ;; Translate invalid charsets.
   ((let ((cs (cdr (assq charset mm-charset-synonym-alist))))
      (and cs (mm-coding-system-p cs) cs)))
   ;; Last resort: search the coding system list for entries which
   ;; have the right mime-charset in case the canonical name isn't
   ;; defined (though it should be).
   ((let (cs)
      ;; mm-get-coding-system-list returns a list of cs without lbt.
      ;; Do we need -lbt?
      (dolist (c (mm-get-coding-system-list))
	(if (and (null cs)
		 (eq charset (or (coding-system-get c :mime-charset)
				 (coding-system-get c 'mime-charset))))
	    (setq cs c)))
      cs))))

(defsubst mm-replace-chars-in-string (string from to)
  (mm-subst-char-in-string from to string))

(eval-and-compile
  (defvar mm-emacs-mule (and (not (featurep 'xemacs))
			     (boundp 'default-enable-multibyte-characters)
			     default-enable-multibyte-characters
			     (fboundp 'set-buffer-multibyte))
    "True in Emacs with Mule.")

  (if mm-emacs-mule
      (defun mm-enable-multibyte ()
	"Set the multibyte flag of the current buffer.
Only do this if the default value of `enable-multibyte-characters' is
non-nil.  This is a no-op in XEmacs."
	(set-buffer-multibyte 'to))
    (defalias 'mm-enable-multibyte 'ignore))

  (if mm-emacs-mule
      (defun mm-disable-multibyte ()
	"Unset the multibyte flag of in the current buffer.
This is a no-op in XEmacs."
	(set-buffer-multibyte nil))
    (defalias 'mm-disable-multibyte 'ignore)))

(defun mm-preferred-coding-system (charset)
  ;; A typo in some Emacs versions.
  (or (get-charset-property charset 'preferred-coding-system)
      (get-charset-property charset 'prefered-coding-system)))

;; Mule charsets shouldn't be used.
(defsubst mm-guess-charset ()
  "Guess Mule charset from the language environment."
  (or
   mail-parse-mule-charset ;; cached mule-charset
   (progn
     (setq mail-parse-mule-charset
	   (and (boundp 'current-language-environment)
		(car (last
		      (assq 'charset
			    (assoc current-language-environment
				   language-info-alist))))))
     (if (or (not mail-parse-mule-charset)
	     (eq mail-parse-mule-charset 'ascii))
	 (setq mail-parse-mule-charset
	       (or (car (last (assq mail-parse-charset
				    mm-mime-mule-charset-alist)))
		   ;; default
		   'latin-iso8859-1)))
     mail-parse-mule-charset)))

(defun mm-charset-after (&optional pos)
  "Return charset of a character in current buffer at position POS.
If POS is nil, it defauls to the current point.
If POS is out of range, the value is nil.
If the charset is `composition', return the actual one."
  (let ((char (char-after pos)) charset)
    (if (< (mm-char-int char) 128)
	(setq charset 'ascii)
      ;; charset-after is fake in some Emacsen.
      (setq charset (and (fboundp 'char-charset) (char-charset char)))
      (if (eq charset 'composition)	; Mule 4
	  (let ((p (or pos (point))))
	    (cadr (find-charset-region p (1+ p))))
	(if (and charset (not (memq charset '(ascii eight-bit-control
						    eight-bit-graphic))))
	    charset
	  (mm-guess-charset))))))

(defun mm-mime-charset (charset)
  "Return the MIME charset corresponding to the given Mule CHARSET."
  (if (eq charset 'unknown)
      (error "The message contains non-printable characters, please use attachment"))
  (if (and (fboundp 'coding-system-get) (fboundp 'get-charset-property))
      ;; This exists in Emacs 20.
      (or
       (and (mm-preferred-coding-system charset)
	    (or (coding-system-get
		 (mm-preferred-coding-system charset) :mime-charset)
		(coding-system-get
		 (mm-preferred-coding-system charset) 'mime-charset)))
       (and (eq charset 'ascii)
	    'us-ascii)
       (mm-preferred-coding-system charset)
       (mm-mule-charset-to-mime-charset charset))
    ;; This is for XEmacs.
    (mm-mule-charset-to-mime-charset charset)))

(defun mm-delete-duplicates (list)
  "Simple substitute for CL `delete-duplicates', testing with `equal'."
  (let (result head)
    (while list
      (setq head (car list))
      (setq list (delete head list))
      (setq result (cons head result)))
    (nreverse result)))

;; Fixme:  This is used in places when it should be testing the
;; default multibyteness.  See mm-default-multibyte-p.
(eval-and-compile
  (if (and (not (featurep 'xemacs))
	   (boundp 'enable-multibyte-characters))
      (defun mm-multibyte-p ()
	"Non-nil if multibyte is enabled in the current buffer."
	enable-multibyte-characters)
    (defun mm-multibyte-p () (featurep 'mule))))

(defun mm-default-multibyte-p ()
  "Return non-nil if the session is multibyte.
This affects whether coding conversion should be attempted generally."
  (if (featurep 'mule)
      (if (boundp 'default-enable-multibyte-characters)
	  default-enable-multibyte-characters
	t)))

(defun mm-iso-8859-x-to-15-region (&optional b e)
  (if (fboundp 'char-charset)
      (let (charset item c inconvertible)
	(save-restriction
	  (if e (narrow-to-region b e))
	  (goto-char (point-min))
	  (skip-chars-forward "\0-\177")
	  (while (not (eobp))
	    (cond
	     ((not (setq item (assq (char-charset (setq c (char-after)))
				    mm-iso-8859-x-to-15-table)))
	      (forward-char))
	     ((memq c (cdr (cdr item)))
	      (setq inconvertible t)
	      (forward-char))
	     (t
	      (insert-before-markers (prog1 (+ c (car (cdr item)))
				       (delete-char 1)))))
	    (skip-chars-forward "\0-\177")))
	(not inconvertible))))

(defun mm-sort-coding-systems-predicate (a b)
  (let ((priorities
	 (mapcar (lambda (cs)
		   ;; Note: invalid entries are dropped silently
		   (and (setq cs (mm-coding-system-p cs))
			(coding-system-base cs)))
		 mm-coding-system-priorities)))
    (and (setq a (mm-coding-system-p a))
	 (if (setq b (mm-coding-system-p b))
	     (> (length (memq (coding-system-base a) priorities))
		(length (memq (coding-system-base b) priorities)))
	   t))))

(eval-when-compile
  (autoload 'latin-unity-massage-name "latin-unity")
  (autoload 'latin-unity-maybe-remap "latin-unity")
  (autoload 'latin-unity-representations-feasible-region "latin-unity")
  (autoload 'latin-unity-representations-present-region "latin-unity")
  (defvar latin-unity-coding-systems)
  (defvar latin-unity-ucs-list))

(defun mm-xemacs-find-mime-charset-1 (begin end)
  "Determine which MIME charset to use to send region as message.
This uses the XEmacs-specific latin-unity package to better handle the
case where identical characters from diverse ISO-8859-? character sets
can be encoded using a single one of the corresponding coding systems.

It treats `mm-coding-system-priorities' as the list of preferred
coding systems; a useful example setting for this list in Western
Europe would be '(iso-8859-1 iso-8859-15 utf-8), which would default
to the very standard Latin 1 coding system, and only move to coding
systems that are less supported as is necessary to encode the
characters that exist in the buffer.

Latin Unity doesn't know about those non-ASCII Roman characters that
are available in various East Asian character sets.  As such, its
behavior if you have a JIS 0212 LATIN SMALL LETTER A WITH ACUTE in a
buffer and it can otherwise be encoded as Latin 1, won't be ideal.
But this is very much a corner case, so don't worry about it."
  (let ((systems mm-coding-system-priorities) csets psets curset)

    ;; Load the Latin Unity library, if available.
    (when (and (not (featurep 'latin-unity)) (locate-library "latin-unity"))
      (ignore-errors (require 'latin-unity)))

    ;; Now, can we use it?
    (if (featurep 'latin-unity)
	(progn
	  (setq csets (latin-unity-representations-feasible-region begin end)
		psets (latin-unity-representations-present-region begin end))

	  (catch 'done

	    ;; Pass back the first coding system in the preferred list
	    ;; that can encode the whole region.
	    (dolist (curset systems)
	      (setq curset (latin-unity-massage-name 'buffer-default curset))

	      ;; If the coding system is a universal coding system, then
	      ;; it can certainly encode all the characters in the region.
	      (if (memq curset latin-unity-ucs-list)
		  (throw 'done (list curset)))

	      ;; If a coding system isn't universal, and isn't in
	      ;; the list that latin unity knows about, we can't
	      ;; decide whether to use it here. Leave that until later
	      ;; in `mm-find-mime-charset-region' function, whence we
	      ;; have been called.
	      (unless (memq curset latin-unity-coding-systems)
		(throw 'done nil))

	      ;; Right, we know about this coding system, and it may
	      ;; conceivably be able to encode all the characters in
	      ;; the region.
	      (if (latin-unity-maybe-remap begin end curset csets psets t)
		  (throw 'done (list curset))))

	    ;; Can't encode using anything from the
	    ;; `mm-coding-system-priorities' list.
	    ;; Leave `mm-find-mime-charset' to do most of the work.
	    nil))

      ;; Right, latin unity isn't available; let `mm-find-charset-region'
      ;; take its default action, which equally applies to GNU Emacs.
      nil)))

(defmacro mm-xemacs-find-mime-charset (begin end)
  (when (featurep 'xemacs)
    `(and (featurep 'mule) (mm-xemacs-find-mime-charset-1 ,begin ,end))))

(defun mm-find-mime-charset-region (b e &optional hack-charsets)
  "Return the MIME charsets needed to encode the region between B and E.
nil means ASCII, a single-element list represents an appropriate MIME
charset, and a longer list means no appropriate charset."
  (let (charsets)
    ;; The return possibilities of this function are a mess...
    (or (and (mm-multibyte-p)
	     mm-use-find-coding-systems-region
	     ;; Find the mime-charset of the most preferred coding
	     ;; system that has one.
	     (let ((systems (find-coding-systems-region b e)))
	       (when mm-coding-system-priorities
		 (setq systems
		       (sort systems 'mm-sort-coding-systems-predicate)))
	       (setq systems (delq 'compound-text systems))
	       (unless (equal systems '(undecided))
		 (while systems
		   (let* ((head (pop systems))
			  (cs (or (coding-system-get head :mime-charset)
				  (coding-system-get head 'mime-charset))))
		     ;; The mime-charset (`x-ctext') of
		     ;; `compound-text' is not in the IANA list.  We
		     ;; shouldn't normally use anything here with a
		     ;; mime-charset having an `x-' prefix.
		     ;; Fixme:  Allow this to be overridden, since
		     ;; there is existing use of x-ctext.
		     ;; Also people apparently need the coding system
		     ;; `iso-2022-jp-3' (which Mule-UCS defines with
		     ;; mime-charset, though it's not valid).
		     (if (and cs
			      (not (string-match "^[Xx]-" (symbol-name cs)))
			      ;; UTF-16 of any variety is invalid for
			      ;; text parts and, unfortunately, has
			      ;; mime-charset defined both in Mule-UCS
			      ;; and versions of Emacs.  (The name
			      ;; might be `mule-utf-16...'  or
			      ;; `utf-16...'.)
			      (not (string-match "utf-16" (symbol-name cs))))
			 (setq systems nil
			       charsets (list cs))))))
	       charsets))
	;; If we're XEmacs, and some coding system is appropriate,
	;; mm-xemacs-find-mime-charset will return an appropriate list.
	;; Otherwise, we'll get nil, and the next setq will get invoked.
	(setq charsets (mm-xemacs-find-mime-charset b e))

	;; We're not multibyte, or a single coding system won't cover it.
	(setq charsets
	      (mm-delete-duplicates
	       (mapcar 'mm-mime-charset
		       (delq 'ascii
			     (mm-find-charset-region b e))))))
    (if (and (> (length charsets) 1)
	     (memq 'iso-8859-15 charsets)
	     (memq 'iso-8859-15 hack-charsets)
	     (save-excursion (mm-iso-8859-x-to-15-region b e)))
	(mapcar (lambda (x) (setq charsets (delq (car x) charsets)))
		mm-iso-8859-15-compatible))
    (if (and (memq 'iso-2022-jp-2 charsets)
	     (memq 'iso-2022-jp-2 hack-charsets))
	(setq charsets (delq 'iso-2022-jp charsets)))
    charsets))

(defmacro mm-with-unibyte-buffer (&rest forms)
  "Create a temporary buffer, and evaluate FORMS there like `progn'.
Use unibyte mode for this."
  `(let (default-enable-multibyte-characters)
     (with-temp-buffer ,@forms)))
(put 'mm-with-unibyte-buffer 'lisp-indent-function 0)
(put 'mm-with-unibyte-buffer 'edebug-form-spec '(body))

(defmacro mm-with-multibyte-buffer (&rest forms)
  "Create a temporary buffer, and evaluate FORMS there like `progn'.
Use multibyte mode for this."
  `(let ((default-enable-multibyte-characters t))
     (with-temp-buffer ,@forms)))
(put 'mm-with-multibyte-buffer 'lisp-indent-function 0)
(put 'mm-with-multibyte-buffer 'edebug-form-spec '(body))

(defmacro mm-with-unibyte-current-buffer (&rest forms)
  "Evaluate FORMS with current buffer temporarily made unibyte.
Also bind `default-enable-multibyte-characters' to nil.
Equivalent to `progn' in XEmacs"
  (let ((multibyte (make-symbol "multibyte"))
	(buffer (make-symbol "buffer")))
    `(if mm-emacs-mule
 	 (let ((,multibyte enable-multibyte-characters)
	       (,buffer (current-buffer)))
	   (unwind-protect
	       (let (default-enable-multibyte-characters)
		 (set-buffer-multibyte nil)
		 ,@forms)
	     (set-buffer ,buffer)
	     (set-buffer-multibyte ,multibyte)))
       (let (default-enable-multibyte-characters)
	 ,@forms))))
(put 'mm-with-unibyte-current-buffer 'lisp-indent-function 0)
(put 'mm-with-unibyte-current-buffer 'edebug-form-spec '(body))

(defmacro mm-with-unibyte (&rest forms)
  "Eval the FORMS with the default value of `enable-multibyte-characters' nil."
  `(let (default-enable-multibyte-characters)
     ,@forms))
(put 'mm-with-unibyte 'lisp-indent-function 0)
(put 'mm-with-unibyte 'edebug-form-spec '(body))

(defmacro mm-with-multibyte (&rest forms)
  "Eval the FORMS with the default value of `enable-multibyte-characters' t."
  `(let ((default-enable-multibyte-characters t))
     ,@forms))
(put 'mm-with-multibyte 'lisp-indent-function 0)
(put 'mm-with-multibyte 'edebug-form-spec '(body))

(defun mm-find-charset-region (b e)
  "Return a list of Emacs charsets in the region B to E."
  (cond
   ((and (mm-multibyte-p)
	 (fboundp 'find-charset-region))
    ;; Remove composition since the base charsets have been included.
    ;; Remove eight-bit-*, treat them as ascii.
    (let ((css (find-charset-region b e)))
      (mapcar (lambda (cs) (setq css (delq cs css)))
	      '(composition eight-bit-control eight-bit-graphic
			    control-1))
      css))
   (t
    ;; We are in a unibyte buffer or XEmacs non-mule, so we futz around a bit.
    (save-excursion
      (save-restriction
	(narrow-to-region b e)
	(goto-char (point-min))
	(skip-chars-forward "\0-\177")
	(if (eobp)
	    '(ascii)
	  (let (charset)
	    (setq charset
		  (and (boundp 'current-language-environment)
		       (car (last (assq 'charset
					(assoc current-language-environment
					       language-info-alist))))))
	    (if (eq charset 'ascii) (setq charset nil))
	    (or charset
		(setq charset
		      (car (last (assq mail-parse-charset
				       mm-mime-mule-charset-alist)))))
	    (list 'ascii (or charset 'latin-iso8859-1)))))))))

(if (fboundp 'shell-quote-argument)
    (defalias 'mm-quote-arg 'shell-quote-argument)
  (defun mm-quote-arg (arg)
    "Return a version of ARG that is safe to evaluate in a shell."
    (let ((pos 0) new-pos accum)
      ;; *** bug: we don't handle newline characters properly
      (while (setq new-pos (string-match "[]*[;!'`\"$\\& \t{} |()<>]" arg pos))
	(push (substring arg pos new-pos) accum)
	(push "\\" accum)
	(push (list (aref arg new-pos)) accum)
	(setq pos (1+ new-pos)))
      (if (= pos 0)
	  arg
	(apply 'concat (nconc (nreverse accum) (list (substring arg pos))))))))

(defun mm-auto-mode-alist ()
  "Return an `auto-mode-alist' with only the .gz (etc) thingies."
  (let ((alist auto-mode-alist)
	out)
    (while alist
      (when (listp (cdar alist))
	(push (car alist) out))
      (pop alist))
    (nreverse out)))

(defvar mm-inhibit-file-name-handlers
  '(jka-compr-handler image-file-handler)
  "A list of handlers doing (un)compression (etc) thingies.")

(defun mm-insert-file-contents (filename &optional visit beg end replace
					 inhibit)
  "Like `insert-file-contents', but only reads in the file.
A buffer may be modified in several ways after reading into the buffer due
to advanced Emacs features, such as file-name-handlers, format decoding,
`find-file-hooks', etc.
If INHIBIT is non-nil, inhibit `mm-inhibit-file-name-handlers'.
  This function ensures that none of these modifications will take place."
  (let* ((format-alist nil)
	 (auto-mode-alist (if inhibit nil (mm-auto-mode-alist)))
	 (default-major-mode 'fundamental-mode)
	 (enable-local-variables nil)
	 (after-insert-file-functions nil)
	 (enable-local-eval nil)
	 (inhibit-file-name-operation (if inhibit
					  'insert-file-contents
					inhibit-file-name-operation))
	 (inhibit-file-name-handlers
	  (if inhibit
	      (append mm-inhibit-file-name-handlers
		      inhibit-file-name-handlers)
	    inhibit-file-name-handlers))
	 (ffh (if (boundp 'find-file-hook)
		  'find-file-hook
		'find-file-hooks))
	 (val (symbol-value ffh)))
    (set ffh nil)
    (unwind-protect
	(insert-file-contents filename visit beg end replace)
      (set ffh val))))

(defun mm-append-to-file (start end filename &optional codesys inhibit)
  "Append the contents of the region to the end of file FILENAME.
When called from a function, expects three arguments,
START, END and FILENAME.  START and END are buffer positions
saying what text to write.
Optional fourth argument specifies the coding system to use when
encoding the file.
If INHIBIT is non-nil, inhibit `mm-inhibit-file-name-handlers'."
  (let ((coding-system-for-write
	 (or codesys mm-text-coding-system-for-write
	     mm-text-coding-system))
	(inhibit-file-name-operation (if inhibit
					 'append-to-file
				       inhibit-file-name-operation))
	(inhibit-file-name-handlers
	 (if inhibit
	     (append mm-inhibit-file-name-handlers
		     inhibit-file-name-handlers)
	   inhibit-file-name-handlers)))
    (write-region start end filename t 'no-message)
    (message "Appended to %s" filename)))

(defun mm-write-region (start end filename &optional append visit lockname
			      coding-system inhibit)

  "Like `write-region'.
If INHIBIT is non-nil, inhibit `mm-inhibit-file-name-handlers'."
  (let ((coding-system-for-write
	 (or coding-system mm-text-coding-system-for-write
	     mm-text-coding-system))
	(inhibit-file-name-operation (if inhibit
					 'write-region
				       inhibit-file-name-operation))
	(inhibit-file-name-handlers
	 (if inhibit
	     (append mm-inhibit-file-name-handlers
		     inhibit-file-name-handlers)
	   inhibit-file-name-handlers)))
    (write-region start end filename append visit lockname)))

(defun mm-image-load-path (&optional package)
  (let (dir result)
    (dolist (path load-path (nreverse result))
      (when (and path
		 (file-directory-p
		  (setq dir (concat (file-name-directory
				     (directory-file-name path))
				    "etc/images/" (or package "gnus/")))))
	(push dir result))
      (push path result))))

;; Fixme: This doesn't look useful where it's used.
(if (fboundp 'detect-coding-region)
    (defun mm-detect-coding-region (start end)
      "Like `detect-coding-region' except returning the best one."
      (let ((coding-systems
	     (detect-coding-region start end)))
	(or (car-safe coding-systems)
	    coding-systems)))
  (defun mm-detect-coding-region (start end)
    (let ((point (point)))
      (goto-char start)
      (skip-chars-forward "\0-\177" end)
      (prog1
	  (if (eq (point) end) 'ascii (mm-guess-charset))
	(goto-char point)))))

(if (fboundp 'coding-system-get)
    (defun mm-detect-mime-charset-region (start end)
      "Detect MIME charset of the text in the region between START and END."
      (let ((cs (mm-detect-coding-region start end)))
	(coding-system-get cs 'mime-charset)))
  (defun mm-detect-mime-charset-region (start end)
    "Detect MIME charset of the text in the region between START and END."
    (let ((cs (mm-detect-coding-region start end)))
      cs)))


(provide 'mm-util)

;; arch-tag: 94dc5388-825d-4fd1-bfa5-2100aa351238
;;; mm-util.el ends here
