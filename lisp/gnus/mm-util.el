;;; mm-util.el --- Utility functions for Mule and low level things
;; Copyright (C) 1998, 1999, 2000, 2001, 2002 Free Software Foundation, Inc.

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
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

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
     (device-type . ignore)
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
      . (lambda (from to string) ;; stolen (and renamed) from nnheader.el
	  "Replace characters in STRING from FROM to TO."
	  (let ((string (substring string 0)) ;Copy string.
		(len (length string))
		(idx 0))
	    ;; Replace all occurrences of FROM with TO.
	    (while (< idx len)
	      (when (= (aref string idx) from)
		(aset string idx to))
	      (setq idx (1+ idx)))
	    string)))
     (string-as-unibyte . identity)
     (string-as-multibyte . identity)
     (multibyte-string-p . ignore))))

(eval-and-compile
  (defalias 'mm-char-or-char-int-p
    (cond
     ((fboundp 'char-or-char-int-p) 'char-or-char-int-p)
     ((fboundp 'char-valid-p) 'char-valid-p)
     (t 'identity))))

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

(defun mm-coding-system-p (sym)
  "Return non-nil if SYM is a coding system."
  (or (and (fboundp 'coding-system-p) (coding-system-p sym))
      (memq sym (mm-get-coding-system-list))))

(defvar mm-charset-synonym-alist
  `(
    ;; Perfectly fine?  A valid MIME name, anyhow.
    ,@(unless (mm-coding-system-p 'big5)
       '((big5 . cn-big5)))
    ;; Not in XEmacs, but it's not a proper MIME charset anyhow.
    ,@(unless (mm-coding-system-p 'x-ctext)
       '((x-ctext . ctext)))
    ;; Apparently not defined in Emacs 20, but is a valid MIME name.
    ,@(unless (mm-coding-system-p 'gb2312)
       '((gb2312 . cn-gb-2312)))
    ;; ISO-8859-15 is very similar to ISO-8859-1.
    ,@(unless (mm-coding-system-p 'iso-8859-15) ; Emacs 21 defines it.
       '((iso-8859-15 . iso-8859-1)))
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
    (iso-2022-7bit ethiopic arabic-1-column arabic-2-column)
    (iso-2022-jp-2 latin-iso8859-1 greek-iso8859-7
		   latin-jisx0201 japanese-jisx0208-1978
		   chinese-gb2312 japanese-jisx0208
		   korean-ksc5601 japanese-jisx0212
		   katakana-jisx0201)
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
    ,(if (or (not (fboundp 'charsetp)) ;; non-Mule case
	     (charsetp 'unicode-a)
	     (not (mm-coding-system-p 'mule-utf-8)))
	 '(utf-8 unicode-a unicode-b unicode-c unicode-d unicode-e)
       ;; If we have utf-8 we're in Mule 5+.
       (append '(utf-8)
	       (delete 'ascii
		       (coding-system-get 'mule-utf-8 'safe-charsets)))))
  "Alist of MIME-charset/MULE-charsets.")

;; Correct by construction, but should be unnecessary:
;; XEmacs hates it.
(when (and (not (featurep 'xemacs))
	   (fboundp 'coding-system-list)
	   (fboundp 'sort-coding-systems))
  (setq mm-mime-mule-charset-alist
	(apply
	 'nconc
	 (mapcar
	  (lambda (cs)
	    (when (and (coding-system-get cs 'mime-charset)
		       (not (eq t (coding-system-get cs 'safe-charsets))))
	      (list (cons (coding-system-get cs 'mime-charset)
			  (delq 'ascii
				(coding-system-get cs 'safe-charsets))))))
	  (sort-coding-systems (coding-system-list 'base-only))))))

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

(defvar mm-coding-system-priorities nil
  "Preferred coding systems for encoding outgoing mails.

More than one suitable coding systems may be found for some texts.  By
default, a coding system with the highest priority is used to encode
outgoing mails (see `sort-coding-systems').  If this variable is set,
it overrides the default priority.  For example, Japanese users may
prefer iso-2022-jp to japanese-shift-jis:

\(setq mm-coding-system-priorities
  '(iso-2022-jp iso-2022-jp-2 japanese-shift-jis utf-8))
")

(defvar mm-use-find-coding-systems-region
  (fboundp 'find-coding-systems-region)
  "Use `find-coding-systems-region' to find proper coding systems.")

;;; Internal variables:

;;; Functions:

(defun mm-mule-charset-to-mime-charset (charset)
  "Return the MIME charset corresponding to the given Mule CHARSET."
  (if (fboundp 'find-coding-systems-for-charsets)
      (let (mime)
	(dolist (cs (find-coding-systems-for-charsets (list charset)))
	  (unless mime
	    (when cs
	      (setq mime (coding-system-get cs 'mime-charset)))))
	mime)
    (let ((alist mm-mime-mule-charset-alist)
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
   ((null (mm-get-coding-system-list))
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
      (and cs (mm-coding-system-p charset) cs)))
   ;; Last resort: search the coding system list for entries which
   ;; have the right mime-charset in case the canonical name isn't
   ;; defined (though it should be).
   ((let (cs)
      ;; mm-get-coding-system-list returns a list of cs without lbt.
      ;; Do we need -lbt?
      (dolist (c (mm-get-coding-system-list))
	(if (and (null cs)
		 (eq charset (coding-system-get c 'mime-charset)))
	    (setq cs c)))
      cs))))

(defsubst mm-replace-chars-in-string (string from to)
  (mm-subst-char-in-string from to string))

(eval-and-compile
  (defvar mm-emacs-mule (and (not (featurep 'xemacs))
			     (boundp 'default-enable-multibyte-characters)
			     default-enable-multibyte-characters
			     (fboundp 'set-buffer-multibyte))
    "Emacs mule.")
  
  (defvar mm-mule4-p (and mm-emacs-mule
			  (fboundp 'charsetp)
			  (not (charsetp 'eight-bit-control)))
    "Mule version 4.")

  (if mm-emacs-mule
      (defun mm-enable-multibyte ()
	"Set the multibyte flag of the current buffer.
Only do this if the default value of `enable-multibyte-characters' is
non-nil.  This is a no-op in XEmacs."
	(set-buffer-multibyte t))
    (defalias 'mm-enable-multibyte 'ignore))

  (if mm-emacs-mule
      (defun mm-disable-multibyte ()
	"Unset the multibyte flag of in the current buffer.
This is a no-op in XEmacs."
	(set-buffer-multibyte nil))
    (defalias 'mm-disable-multibyte 'ignore))

  (if mm-mule4-p
      (defun mm-enable-multibyte-mule4  ()
	"Enable multibyte in the current buffer.
Only used in Emacs Mule 4."
	(set-buffer-multibyte t))
    (defalias 'mm-enable-multibyte-mule4 'ignore))
  
  (if mm-mule4-p
      (defun mm-disable-multibyte-mule4 ()
	"Disable multibyte in the current buffer.
Only used in Emacs Mule 4."
	(set-buffer-multibyte nil))
    (defalias 'mm-disable-multibyte-mule4 'ignore)))

(defun mm-preferred-coding-system (charset)
  ;; A typo in some Emacs versions.
  (or (get-charset-property charset 'preferred-coding-system)
      (get-charset-property charset 'prefered-coding-system)))

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
      (if (eq charset 'composition)
	  (let ((p (or pos (point))))
	    (cadr (find-charset-region p (1+ p))))
	(if (and charset (not (memq charset '(ascii eight-bit-control
						    eight-bit-graphic))))
	    charset
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
			   ;; Fixme: don't fix that!
			   'latin-iso8859-1)))
	     mail-parse-mule-charset)))))))

(defun mm-mime-charset (charset)
  "Return the MIME charset corresponding to the given Mule CHARSET."
  (if (eq charset 'unknown)
      (error "The message contains non-printable characters, please use attachment"))
  (if (and (fboundp 'coding-system-get) (fboundp 'get-charset-property))
      ;; This exists in Emacs 20.
      (or
       (and (mm-preferred-coding-system charset)
	    (coding-system-get
	     (mm-preferred-coding-system charset) 'mime-charset))
       (and (eq charset 'ascii)
	    'us-ascii)
       (mm-preferred-coding-system charset)
       (mm-mule-charset-to-mime-charset charset))
    ;; This is for XEmacs.
    (mm-mule-charset-to-mime-charset charset)))

(defun mm-delete-duplicates (list)
  "Simple  substitute for CL `delete-duplicates', testing with `equal'."
  (let (result head)
    (while list
      (setq head (car list))
      (setq list (delete head list))
      (setq result (cons head result)))
    (nreverse result)))

;; It's not clear whether this is supposed to mean the global or local
;; setting.  I think it's used inconsistently.  -- fx
(defsubst mm-multibyte-p ()
  "Say whether multibyte is enabled."
  (if (and (not (featurep 'xemacs))
	   (boundp 'enable-multibyte-characters))
      enable-multibyte-characters
    (featurep 'mule)))

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
	      (insert (prog1 (+ c (car (cdr item))) (delete-char 1))))
	    (skip-chars-forward "\0-\177"))))
	(not inconvertible))))

(defun mm-sort-coding-systems-predicate (a b)
  (> (length (memq a mm-coding-system-priorities))
     (length (memq b mm-coding-system-priorities))))

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
	       ;; Fixme: The `mime-charset' (`x-ctext') of `compound-text'
	       ;; is not in the IANA list.
	       (setq systems (delq 'compound-text systems))
	       (unless (equal systems '(undecided))
		 (while systems
		   (let ((cs (coding-system-get (pop systems) 'mime-charset)))
		     (if cs
			 (setq systems nil
			       charsets (list cs))))))
	       charsets))
	;; Otherwise we're not multibyte, XEmacs or a single coding
	;; system won't cover it.
	(setq charsets 
	      (mm-delete-duplicates
	       (mapcar 'mm-mime-charset
		       (delq 'ascii
			     (mm-find-charset-region b e))))))
    (if (and (memq 'iso-8859-15 charsets)
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

(defmacro mm-with-unibyte-current-buffer (&rest forms)
  "Evaluate FORMS with current current buffer temporarily made unibyte.
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

(defmacro mm-with-unibyte-current-buffer-mule4 (&rest forms)
  "Evaluate FORMS there like `progn' in current buffer.
Mule4 only."
  (let ((multibyte (make-symbol "multibyte"))
	(buffer (make-symbol "buffer")))
    `(if mm-mule4-p
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
(put 'mm-with-unibyte-current-buffer-mule4 'lisp-indent-function 0)
(put 'mm-with-unibyte-current-buffer-mule4 'edebug-form-spec '(body))

(defmacro mm-with-unibyte (&rest forms)
  "Eval the FORMS with the default value of `enable-multibyte-characters' nil, ."
  `(let (default-enable-multibyte-characters)
     ,@forms))
(put 'mm-with-unibyte 'lisp-indent-function 0)
(put 'mm-with-unibyte 'edebug-form-spec '(body))

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
  "Like `insert-file-contents', q.v., but only reads in the file.
A buffer may be modified in several ways after reading into the buffer due
to advanced Emacs features, such as file-name-handlers, format decoding,
find-file-hooks, etc.
If INHIBIT is non-nil, inhibit mm-inhibit-file-name-handlers.
  This function ensures that none of these modifications will take place."
  (let ((format-alist nil)
	(auto-mode-alist (if inhibit nil (mm-auto-mode-alist)))
	(default-major-mode 'fundamental-mode)
	(enable-local-variables nil)
	(after-insert-file-functions nil)
	(enable-local-eval nil)
	(find-file-hooks nil)
	(inhibit-file-name-operation (if inhibit
					 'insert-file-contents
				       inhibit-file-name-operation))
	(inhibit-file-name-handlers
	 (if inhibit
	     (append mm-inhibit-file-name-handlers
		     inhibit-file-name-handlers)
	   inhibit-file-name-handlers)))
    (insert-file-contents filename visit beg end replace)))

(defun mm-append-to-file (start end filename &optional codesys inhibit)
  "Append the contents of the region to the end of file FILENAME.
When called from a function, expects three arguments,
START, END and FILENAME.  START and END are buffer positions
saying what text to write.
Optional fourth argument specifies the coding system to use when
encoding the file.
If INHIBIT is non-nil, inhibit mm-inhibit-file-name-handlers."
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
    (append-to-file start end filename)))

(defun mm-write-region (start end filename &optional append visit lockname
			      coding-system inhibit)

  "Like `write-region'.
If INHIBIT is non-nil, inhibit mm-inhibit-file-name-handlers."
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
      (if (file-directory-p
	   (setq dir (concat (file-name-directory
			      (directory-file-name path))
			     "etc/" (or package "gnus/"))))
	  (push dir result))
      (push path result))))

;; It is not a MIME function, but some MIME functions use it.
(defalias 'mm-make-temp-file
  (if (fboundp 'make-temp-file)
      'make-temp-file
    (lambda (prefix &optional dir-flag)
      (let ((file (expand-file-name
		   (make-temp-name prefix)
		   (if (fboundp 'temp-directory)
		       (temp-directory)
		     temporary-file-directory))))
	(if dir-flag
	    (make-directory file))
	file))))

(provide 'mm-util)

;;; mm-util.el ends here
