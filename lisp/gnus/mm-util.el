;;; mm-util.el --- Utility functions for Mule and low level things

;; Copyright (C) 1998, 1999, 2000, 2001, 2002, 2003, 2004,
;;   2005, 2006, 2007, 2008 Free Software Foundation, Inc.

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
;;	MORIOKA Tomohiko <morioka@jaist.ac.jp>
;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
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

;; For Emacs < 22.2.
(eval-and-compile
  (unless (fboundp 'declare-function) (defmacro declare-function (&rest r))))

(eval-when-compile (require 'cl))
(require 'mail-prsvr)

(eval-and-compile
  (if (featurep 'xemacs)
      (unless (ignore-errors
		(require 'timer-funcs))
	(require 'timer))
    (require 'timer)))

(defvar mm-mime-mule-charset-alist )

(eval-and-compile
  (mapc
   (lambda (elem)
     (let ((nfunc (intern (format "mm-%s" (car elem)))))
       (if (fboundp (car elem))
	   (defalias nfunc (car elem))
	 (defalias nfunc (cdr elem)))))
   '((coding-system-list . ignore)
     (char-int . identity)
     (coding-system-equal . equal)
     (annotationp . ignore)
     (set-buffer-file-coding-system . ignore)
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
     (replace-in-string
      . (lambda (string regexp rep &optional literal)
	  "See `replace-regexp-in-string', only the order of args differs."
	  (replace-regexp-in-string regexp rep string nil literal)))
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
     (multibyte-string-p . ignore)
     (insert-byte . insert-char)
     (multibyte-char-to-unibyte . identity)
     (special-display-p
      . (lambda (buffer-name)
	  "Returns non-nil if a buffer named BUFFER-NAME gets a special frame."
	  (and special-display-function
	       (or (and (member buffer-name special-display-buffer-names) t)
		   (cdr (assoc buffer-name special-display-buffer-names))
		   (catch 'return
		     (dolist (elem special-display-regexps)
		       (and (stringp elem)
			    (string-match elem buffer-name)
			    (throw 'return t))
		       (and (consp elem)
			    (stringp (car elem))
			    (string-match (car elem) buffer-name)
			    (throw 'return (cdr elem))))))))))))

(eval-and-compile
  (if (featurep 'xemacs)
      (if (featurep 'file-coding)
	  ;; Don't modify string if CODING-SYSTEM is nil.
	  (progn
	    (defun mm-decode-coding-string (str coding-system)
	      (if coding-system
		  (decode-coding-string str coding-system)
		str))
	    (defun mm-encode-coding-string (str coding-system)
	      (if coding-system
		  (encode-coding-string str coding-system)
		str))
	    (defun mm-decode-coding-region (start end coding-system)
	      (if coding-system
		  (decode-coding-region start end coding-system)))
	    (defun mm-encode-coding-region (start end coding-system)
	      (if coding-system
		  (encode-coding-region start end coding-system))))
	(defun mm-decode-coding-string (str coding-system) str)
	(defun mm-encode-coding-string (str coding-system) str)
	(defalias 'mm-decode-coding-region 'ignore)
	(defalias 'mm-encode-coding-region 'ignore))
    (defalias 'mm-decode-coding-string 'decode-coding-string)
    (defalias 'mm-encode-coding-string 'encode-coding-string)
    (defalias 'mm-decode-coding-region 'decode-coding-region)
    (defalias 'mm-encode-coding-region 'encode-coding-region)))

(defalias 'mm-string-to-multibyte
  (cond
   ((featurep 'xemacs)
    'identity)
   ((fboundp 'string-to-multibyte)
    'string-to-multibyte)
   (t
    (lambda (string)
      "Return a multibyte string with the same individual chars as string."
      (mapconcat
       (lambda (ch) (mm-string-as-multibyte (char-to-string ch)))
       string "")))))

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
      ;; no-MULE XEmacs:
      (car (memq cs (mm-get-coding-system-list))))))

(defun mm-codepage-setup (number &optional alias)
  "Create a coding system cpNUMBER.
The coding system is created using `codepage-setup'.  If ALIAS is
non-nil, an alias is created and added to
`mm-charset-synonym-alist'.  If ALIAS is a string, it's used as
the alias.  Else windows-NUMBER is used."
  (interactive
   (let ((completion-ignore-case t)
	 (candidates (cp-supported-codepages)))
     (list (completing-read "Setup DOS Codepage: (default 437) " candidates
			    nil t nil nil "437"))))
  (when alias
    (setq alias (if (stringp alias)
		    (intern alias)
		  (intern (format "windows-%s" number)))))
  (let* ((cp (intern (format "cp%s" number))))
    (unless (mm-coding-system-p cp)
      (codepage-setup number))
    (when (and alias
	       ;; Don't add alias if setup of cp failed.
	       (mm-coding-system-p cp))
      (add-to-list 'mm-charset-synonym-alist (cons alias cp)))))

(defvar mm-charset-synonym-alist
  `(
    ;; Not in XEmacs, but it's not a proper MIME charset anyhow.
    ,@(unless (mm-coding-system-p 'x-ctext)
	'((x-ctext . ctext)))
    ;; ISO-8859-15 is very similar to ISO-8859-1.  But it's _different_ in 8
    ;; positions!
    ,@(unless (mm-coding-system-p 'iso-8859-15)
	'((iso-8859-15 . iso-8859-1)))
    ;; BIG-5HKSCS is similar to, but different than, BIG-5.
    ,@(unless (mm-coding-system-p 'big5-hkscs)
	'((big5-hkscs . big5)))
    ;; A Microsoft misunderstanding.
    ,@(when (and (not (mm-coding-system-p 'unicode))
		 (mm-coding-system-p 'utf-16-le))
	'((unicode . utf-16-le)))
    ;; A Microsoft misunderstanding.
    ,@(unless (mm-coding-system-p 'ks_c_5601-1987)
	(if (mm-coding-system-p 'cp949)
	    '((ks_c_5601-1987 . cp949))
	  '((ks_c_5601-1987 . euc-kr))))
    ;; Windows-31J is Windows Codepage 932.
    ,@(when (and (not (mm-coding-system-p 'windows-31j))
		 (mm-coding-system-p 'cp932))
	'((windows-31j . cp932)))
    ;; Charset name: GBK, Charset aliases: CP936, MS936, windows-936
    ;; http://www.iana.org/assignments/charset-reg/GBK
    ;; Emacs 22.1 has cp936, but not gbk, so we alias it:
    ,@(when (and (not (mm-coding-system-p 'gbk))
		 (mm-coding-system-p 'cp936))
	'((gbk . cp936)))
    ;; ISO8859-1 is a bogus name for ISO-8859-1
    ,@(when (and (not (mm-coding-system-p 'iso8859-1))
		 (mm-coding-system-p 'iso-8859-1))
	'((iso8859-1 . iso-8859-1)))
    )
  "A mapping from unknown or invalid charset names to the real charset names.

See `mm-codepage-iso-8859-list' and `mm-codepage-ibm-list'.")

(defcustom mm-codepage-iso-8859-list
  (list 1250 ;; Windows-1250 is a variant of Latin-2 heavily used by Microsoft
	;; Outlook users in Czech republic.  Use this to allow reading of
	;; their e-mails.  cp1250 should be defined by M-x codepage-setup
	;; (Emacs 21).
	'(1252 . 1) ;; Windows-1252 is a superset of iso-8859-1 (West
	            ;; Europe).  See also `gnus-article-dumbquotes-map'.
	'(1254 . 9) ;; Windows-1254 is a superset of iso-8859-9 (Turkish).
	'(1255 . 8));; Windows-1255 is a superset of iso-8859-8 (Hebrew).
  "A list of Windows codepage numbers and iso-8859 charset numbers.

If an element is a number corresponding to a supported windows
codepage, appropriate entries to `mm-charset-synonym-alist' are
added by `mm-setup-codepage-iso-8859'.  An element may also be a
cons cell where the car is a codepage number and the cdr is the
corresponding number of an iso-8859 charset."
  :type '(list (set :inline t
		    (const 1250 :tag "Central and East European")
		    (const (1252 . 1) :tag "West European")
		    (const (1254 . 9) :tag "Turkish")
		    (const (1255 . 8) :tag "Hebrew"))
	       (repeat :inline t
		       :tag "Other options"
		       (choice
			(integer :tag "Windows codepage number")
			(cons (integer :tag "Windows codepage number")
			      (integer :tag "iso-8859 charset  number")))))
  :version "22.1" ;; Gnus 5.10.9
  :group 'mime)

(defcustom mm-codepage-ibm-list
  (list 437 ;; (US etc.)
	860 ;; (Portugal)
	861 ;; (Iceland)
	862 ;; (Israel)
	863 ;; (Canadian French)
	865 ;; (Nordic)
	852 ;;
	850 ;; (Latin 1)
	855 ;; (Cyrillic)
	866 ;; (Cyrillic - Russian)
	857 ;; (Turkish)
	864 ;; (Arabic)
	869 ;; (Greek)
	874);; (Thai)
  ;; In Emacs 23 (unicode), cp... and ibm... are aliases.
  ;; Cf. http://thread.gmane.org/v9lkng5nwy.fsf@marauder.physik.uni-ulm.de
  "List of IBM codepage numbers.

The codepage mappings slighly differ between IBM and other vendors.
See \"ftp://ftp.unicode.org/Public/MAPPINGS/VENDORS/IBM/README.TXT\".

If an element is a number corresponding to a supported windows
codepage, appropriate entries to `mm-charset-synonym-alist' are
added by `mm-setup-codepage-ibm'."
  :type '(list (set :inline t
		    (const 437 :tag "US etc.")
		    (const 860 :tag "Portugal")
		    (const 861 :tag "Iceland")
		    (const 862 :tag "Israel")
		    (const 863 :tag "Canadian French")
		    (const 865 :tag "Nordic")
		    (const 852)
		    (const 850 :tag "Latin 1")
		    (const 855 :tag "Cyrillic")
		    (const 866 :tag "Cyrillic - Russian")
		    (const 857 :tag "Turkish")
		    (const 864 :tag "Arabic")
		    (const 869 :tag "Greek")
		    (const 874 :tag "Thai"))
	       (repeat :inline t
		       :tag "Other options"
		       (integer :tag "Codepage number")))
  :version "22.1" ;; Gnus 5.10.9
  :group 'mime)

(defun mm-setup-codepage-iso-8859 (&optional list)
  "Add appropriate entries to `mm-charset-synonym-alist'.
Unless LIST is given, `mm-codepage-iso-8859-list' is used."
  (unless list
    (setq list mm-codepage-iso-8859-list))
  (dolist (i list)
    (let (cp windows iso)
      (if (consp i)
	  (setq cp (intern (format "cp%d" (car i)))
		windows (intern (format "windows-%d" (car i)))
		iso (intern (format "iso-8859-%d" (cdr i))))
	(setq cp (intern (format "cp%d" i))
	      windows (intern (format "windows-%d" i))))
      (unless (mm-coding-system-p windows)
	(if (mm-coding-system-p cp)
	    (add-to-list 'mm-charset-synonym-alist (cons windows cp))
	  (add-to-list 'mm-charset-synonym-alist (cons windows iso)))))))

(defun mm-setup-codepage-ibm (&optional list)
  "Add appropriate entries to `mm-charset-synonym-alist'.
Unless LIST is given, `mm-codepage-ibm-list' is used."
  (unless list
    (setq list mm-codepage-ibm-list))
  (dolist (number list)
    (let ((ibm (intern (format "ibm%d" number)))
	  (cp  (intern (format "cp%d" number))))
      (when (and (not (mm-coding-system-p ibm))
		 (mm-coding-system-p cp))
	(add-to-list 'mm-charset-synonym-alist (cons ibm cp))))))

;; Initialize:
(mm-setup-codepage-iso-8859)
(mm-setup-codepage-ibm)

(defcustom mm-charset-override-alist
  '((iso-8859-1 . windows-1252)
    (iso-8859-8 . windows-1255)
    (iso-8859-9 . windows-1254))
  "A mapping from undesired charset names to their replacement.

You may add pairs like (iso-8859-1 . windows-1252) here,
i.e. treat iso-8859-1 as windows-1252.  windows-1252 is a
superset of iso-8859-1."
  :type '(list (set :inline t
		    (const (iso-8859-1 . windows-1252))
		    (const (iso-8859-8 . windows-1255))
		    (const (iso-8859-9 . windows-1254))
		    (const (undecided  . windows-1252)))
	       (repeat :inline t
		       :tag "Other options"
		       (cons (symbol :tag "From charset")
			     (symbol :tag "To charset"))))
  :version "22.1" ;; Gnus 5.10.9
  :group 'mime)

(defcustom mm-charset-eval-alist
  (if (featurep 'xemacs)
      nil ;; I don't know what would be useful for XEmacs.
    '(;; Emacs 21 offers 1250 1251 1253 1257.  Emacs 22 provides autoloads for
      ;; 1250-1258 (i.e. `mm-codepage-setup' does nothing).
      (windows-1250 . (mm-codepage-setup 1250 t))
      (windows-1251 . (mm-codepage-setup 1251 t))
      (windows-1253 . (mm-codepage-setup 1253 t))
      (windows-1257 . (mm-codepage-setup 1257 t))))
  "An alist of (CHARSET . FORM) pairs.
If an article is encoded in an unknown CHARSET, FORM is
evaluated.  This allows to load additional libraries providing
charsets on demand.  If supported by your Emacs version, you
could use `autoload-coding-system' here."
  :version "22.1" ;; Gnus 5.10.9
  :type '(list (set :inline t
		    (const (windows-1250 . (mm-codepage-setup 1250 t)))
		    (const (windows-1251 . (mm-codepage-setup 1251 t)))
		    (const (windows-1253 . (mm-codepage-setup 1253 t)))
		    (const (windows-1257 . (mm-codepage-setup 1257 t)))
		    (const (cp850 . (mm-codepage-setup 850 nil))))
	       (repeat :inline t
		       :tag "Other options"
		       (cons (symbol :tag "charset")
			     (symbol :tag "form"))))
  :group 'mime)
(put 'mm-charset-eval-alist 'risky-local-variable t)

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
    (gbk chinese-gbk)
    (gb18030 gb18030-2-byte
	     gb18030-4-byte-bmp gb18030-4-byte-smp
	     gb18030-4-byte-ext-1 gb18030-4-byte-ext-2)
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
    ,(cond ((fboundp 'unicode-precedence-list)
	    (cons 'utf-8 (delq 'ascii (mapcar 'charset-name
					      (unicode-precedence-list)))))
	   ((or (not (fboundp 'charsetp)) ;; non-Mule case
		(charsetp 'unicode-a)
		(not (mm-coding-system-p 'mule-utf-8)))
	    '(utf-8 unicode-a unicode-b unicode-c unicode-d unicode-e))
	   (t ;; If we have utf-8 we're in Mule 5+.
	    (append '(utf-8)
		    (delete 'ascii
			    (coding-system-get 'mule-utf-8 'safe-charsets))))))
  "Alist of MIME-charset/MULE-charsets.")

(defun mm-enrich-utf-8-by-mule-ucs ()
  "Make the `utf-8' MIME charset usable by the Mule-UCS package.
This function will run when the `un-define' module is loaded under
XEmacs, and fill the `utf-8' entry in `mm-mime-mule-charset-alist'
with Mule charsets.  It is completely useless for Emacs."
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
	      mime (or (coding-system-get cs :mime-charset); Emacs 23 (unicode)
		       (coding-system-get cs 'mime-charset)))
	(when (and mime
		   (not (eq t (setq mule
				    (coding-system-get cs 'safe-charsets))))
		   (not (assq mime alist)))
	  (push (cons mime (delq 'ascii mule)) alist)))
      (setq mm-mime-mule-charset-alist (nreverse alist)))))

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

(defun mm-charset-to-coding-system (charset &optional lbt
					    allow-override)
  "Return coding-system corresponding to CHARSET.
CHARSET is a symbol naming a MIME charset.
If optional argument LBT (`unix', `dos' or `mac') is specified, it is
used as the line break code type of the coding system.

If ALLOW-OVERRIDE is given, use `mm-charset-override-alist' to
map undesired charset names to their replacement.  This should
only be used for decoding, not for encoding."
  ;; OVERRIDE is used (only) in `mm-decode-body' and `mm-decode-string'.
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
   ;; Check override list quite early.  Should only used for decoding, not for
   ;; encoding!
   ((and allow-override
	 (let ((cs (cdr (assq charset mm-charset-override-alist))))
	   (and cs (mm-coding-system-p cs) cs))))
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
   ;; Eval expressions from `mm-charset-eval-alist'
   ((let* ((el (assq charset mm-charset-eval-alist))
	   (cs (car el))
	   (form (cdr el)))
      (and cs
	   form
	   (prog2
	       ;; Avoid errors...
	       (condition-case nil (eval form) (error nil))
	       ;; (message "Failed to eval `%s'" form))
	       (mm-coding-system-p cs)
	     (message "Added charset `%s' via `mm-charset-eval-alist'" cs))
	   cs)))
   ;; Translate invalid charsets.
   ((let ((cs (cdr (assq charset mm-charset-synonym-alist))))
      (and cs
	   (mm-coding-system-p cs)
	   ;; (message
	   ;;  "Using synonym `%s' from `mm-charset-synonym-alist' for `%s'"
	   ;;  cs charset)
	   cs)))
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
      (unless cs
	;; Warn the user about unknown charset:
	(if (fboundp 'gnus-message)
	    (gnus-message 7 "Unknown charset: %s" charset)
	  (message "Unknown charset: %s" charset)))
      cs))))

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

(if (fboundp 'delete-dups)
    (defalias 'mm-delete-duplicates 'delete-dups)
  (defun mm-delete-duplicates (list)
    "Destructively remove `equal' duplicates from LIST.
Store the result in LIST and return it.  LIST must be a proper list.
Of several `equal' occurrences of an element in LIST, the first
one is kept.

This is a compatibility function for Emacsen without `delete-dups'."
    ;; Code from `subr.el' in Emacs 22:
    (let ((tail list))
      (while tail
	(setcdr tail (delete (car tail) (cdr tail)))
	(setq tail (cdr tail))))
    list))

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
  (autoload 'latin-unity-representations-present-region "latin-unity"))

(defvar latin-unity-coding-systems)
(defvar latin-unity-ucs-list)

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
      (require 'latin-unity))

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

(declare-function mm-delete-duplicates "mm-util" (list))

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

	;; Fixme: won't work for unibyte Emacs 23:

	;; We're not multibyte, or a single coding system won't cover it.
	(setq charsets
	      (mm-delete-duplicates
	       (mapcar 'mm-mime-charset
		       (delq 'ascii
			     (mm-find-charset-region b e))))))
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
Equivalent to `progn' in XEmacs

NOTE: Use this macro with caution in multibyte buffers (it is not
worth using this macro in unibyte buffers of course).  Use of
`(set-buffer-multibyte t)', which is run finally, is generally
harmful since it is likely to modify existing data in the buffer.
For instance, it converts \"\\300\\255\" into \"\\255\" in
Emacs 23 (unicode)."
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
      (dolist (cs
	       '(composition eight-bit-control eight-bit-graphic control-1)
	       css)
	(setq css (delq cs css)))))
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
  '(jka-compr-handler image-file-handler epa-file-handler)
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

(autoload 'gmm-write-region "gmm-utils")

;; It is not a MIME function, but some MIME functions use it.
(if (and (fboundp 'make-temp-file)
	 (ignore-errors
	   (let ((def (symbol-function 'make-temp-file)))
	     (and (byte-code-function-p def)
		  (setq def (if (fboundp 'compiled-function-arglist)
				;; XEmacs
				(eval (list 'compiled-function-arglist def))
			      (aref def 0)))
		  (>= (length def) 4)
		  (eq (nth 3 def) 'suffix)))))
    (defalias 'mm-make-temp-file 'make-temp-file)
  ;; Stolen (and modified for XEmacs) from Emacs 22.
  (defun mm-make-temp-file (prefix &optional dir-flag suffix)
    "Create a temporary file.
The returned file name (created by appending some random characters at the end
of PREFIX, and expanding against `temporary-file-directory' if necessary),
is guaranteed to point to a newly created empty file.
You can then use `write-region' to write new data into the file.

If DIR-FLAG is non-nil, create a new empty directory instead of a file.

If SUFFIX is non-nil, add that at the end of the file name."
    (let ((umask (default-file-modes))
	  file)
      (unwind-protect
	  (progn
	    ;; Create temp files with strict access rights.  It's easy to
	    ;; loosen them later, whereas it's impossible to close the
	    ;; time-window of loose permissions otherwise.
	    (set-default-file-modes 448)
	    (while (condition-case err
		       (progn
			 (setq file
			       (make-temp-name
				(expand-file-name
				 prefix
				 (if (fboundp 'temp-directory)
				     ;; XEmacs
				     (temp-directory)
				   temporary-file-directory))))
			 (if suffix
			     (setq file (concat file suffix)))
			 (if dir-flag
			     (make-directory file)
			   ;; NOTE: This is unsafe if Emacs 20
			   ;; users and XEmacs users don't use
			   ;; a secure temp directory.
			   (gmm-write-region "" nil file nil 'silent
					     nil 'excl))
			 nil)
		     (file-already-exists t)
		     ;; The XEmacs version of `make-directory' issues
		     ;; `file-error'.
		     (file-error (or (and (featurep 'xemacs)
					  (file-exists-p file))
				     (signal (car err) (cdr err)))))
	      ;; the file was somehow created by someone else between
	      ;; `make-temp-name' and `write-region', let's try again.
	      nil)
	    file)
	;; Reset the umask.
	(set-default-file-modes umask)))))

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

(declare-function mm-detect-coding-region "mm-util" (start end))

(if (fboundp 'coding-system-get)
    (defun mm-detect-mime-charset-region (start end)
      "Detect MIME charset of the text in the region between START and END."
      (let ((cs (mm-detect-coding-region start end)))
	(or (coding-system-get cs :mime-charset)
	    (coding-system-get cs 'mime-charset))))
  (defun mm-detect-mime-charset-region (start end)
    "Detect MIME charset of the text in the region between START and END."
    (let ((cs (mm-detect-coding-region start end)))
      cs)))

(eval-when-compile
  (unless (fboundp 'coding-system-to-mime-charset)
    (defalias 'coding-system-to-mime-charset 'ignore)))

(defun mm-coding-system-to-mime-charset (coding-system)
  "Return the MIME charset corresponding to CODING-SYSTEM.
To make this function work with XEmacs, the APEL package is required."
  (when coding-system
    (or (and (fboundp 'coding-system-get)
	     (or (coding-system-get coding-system :mime-charset)
		 (coding-system-get coding-system 'mime-charset)))
	(and (featurep 'xemacs)
	     (or (and (fboundp 'coding-system-to-mime-charset)
		      (not (eq (symbol-function 'coding-system-to-mime-charset)
			       'ignore)))
		 (and (condition-case nil
			  (require 'mcharset)
			(error nil))
		      (fboundp 'coding-system-to-mime-charset)))
	     (coding-system-to-mime-charset coding-system)))))

(eval-when-compile
  (require 'jka-compr))

(defun mm-decompress-buffer (filename &optional inplace force)
  "Decompress buffer's contents, depending on jka-compr.
Only when FORCE is t or `auto-compression-mode' is enabled and FILENAME
agrees with `jka-compr-compression-info-list', decompression is done.
Signal an error if FORCE is neither nil nor t and compressed data are
not decompressed because `auto-compression-mode' is disabled.
If INPLACE is nil, return decompressed data or nil without modifying
the buffer.  Otherwise, replace the buffer's contents with the
decompressed data.  The buffer's multibyteness must be turned off."
  (when (and filename
	     (if force
		 (prog1 t (require 'jka-compr))
	       (and (fboundp 'jka-compr-installed-p)
		    (jka-compr-installed-p))))
    (let ((info (jka-compr-get-compression-info filename)))
      (when info
	(unless (or (memq force (list nil t))
		    (jka-compr-installed-p))
	  (error ""))
	(let ((prog (jka-compr-info-uncompress-program info))
	      (args (jka-compr-info-uncompress-args info))
	      (msg (format "%s %s..."
			   (jka-compr-info-uncompress-message info)
			   filename))
	      (err-file (jka-compr-make-temp-name))
	      (cur (current-buffer))
	      (coding-system-for-read mm-binary-coding-system)
	      (coding-system-for-write mm-binary-coding-system)
	      retval err-msg)
	  (message "%s" msg)
	  (mm-with-unibyte-buffer
	    (insert-buffer-substring cur)
	    (condition-case err
		(progn
		  (unless (memq (apply 'call-process-region
				       (point-min) (point-max)
				       prog t (list t err-file) nil args)
				jka-compr-acceptable-retval-list)
		    (erase-buffer)
		    (insert (mapconcat
			     'identity
			     (delete "" (split-string
					 (prog2
					     (insert-file-contents err-file)
					     (buffer-string)
					   (erase-buffer))))
			     " ")
			    "\n")
		    (setq err-msg
			  (format "Error while executing \"%s %s < %s\""
				  prog (mapconcat 'identity args " ")
				  filename)))
		  (setq retval (buffer-string)))
	      (error
	       (setq err-msg (error-message-string err)))))
	  (when (file-exists-p err-file)
	    (ignore-errors (jka-compr-delete-temp-file err-file)))
	  (when inplace
	    (unless err-msg
	      (delete-region (point-min) (point-max))
	      (insert retval))
	    (setq retval nil))
	  (message "%s" (or err-msg (concat msg "done")))
	  retval)))))

(eval-when-compile
  (unless (fboundp 'coding-system-name)
    (defalias 'coding-system-name 'ignore))
  (unless (fboundp 'find-file-coding-system-for-read-from-filename)
    (defalias 'find-file-coding-system-for-read-from-filename 'ignore))
  (unless (fboundp 'find-operation-coding-system)
    (defalias 'find-operation-coding-system 'ignore)))

(defun mm-find-buffer-file-coding-system (&optional filename)
  "Find coding system used to decode the contents of the current buffer.
This function looks for the coding system magic cookie or examines the
coding system specified by `file-coding-system-alist' being associated
with FILENAME which defaults to `buffer-file-name'.  Data compressed by
gzip, bzip2, etc. are allowed."
  (unless filename
    (setq filename buffer-file-name))
  (save-excursion
    (let ((decomp (unless ;; No worth to examine charset of tar files.
		      (and filename
			   (string-match
			    "\\.\\(?:tar\\.[^.]+\\|tbz\\|tgz\\)\\'"
			    filename))
		    (mm-decompress-buffer filename nil t))))
      (when decomp
	(set-buffer (let (default-enable-multibyte-characters)
		      (generate-new-buffer " *temp*")))
	(insert decomp)
	(setq filename (file-name-sans-extension filename)))
      (goto-char (point-min))
      (prog1
	  (cond
	   ((boundp 'set-auto-coding-function) ;; Emacs
	    (if filename
		(or (funcall (symbol-value 'set-auto-coding-function)
			     filename (- (point-max) (point-min)))
		    (car (find-operation-coding-system 'insert-file-contents
						       filename)))
	      (let (auto-coding-alist)
		(condition-case nil
		    (funcall (symbol-value 'set-auto-coding-function)
			     nil (- (point-max) (point-min)))
		  (error nil)))))
	   ((and (featurep 'xemacs) (featurep 'file-coding)) ;; XEmacs
	    (let ((case-fold-search t)
		  (end (point-at-eol))
		  codesys start)
	      (or
	       (and (re-search-forward "-\\*-+[\t ]*" end t)
		    (progn
		      (setq start (match-end 0))
		      (re-search-forward "[\t ]*-+\\*-" end t))
		    (progn
		      (setq end (match-beginning 0))
		      (goto-char start)
		      (or (looking-at "coding:[\t ]*\\([^\t ;]+\\)")
			  (re-search-forward
			   "[\t ;]+coding:[\t ]*\\([^\t ;]+\\)"
			   end t)))
		    (find-coding-system (setq codesys
					      (intern (match-string 1))))
		    codesys)
	       (and (re-search-forward "^[\t ]*;+[\t ]*Local[\t ]+Variables:"
				       nil t)
		    (progn
		      (setq start (match-end 0))
		      (re-search-forward "^[\t ]*;+[\t ]*End:" nil t))
		    (progn
		      (setq end (match-beginning 0))
		      (goto-char start)
		      (re-search-forward
		       "^[\t ]*;+[\t ]*coding:[\t ]*\\([^\t\n\r ]+\\)"
		       end t))
		    (find-coding-system (setq codesys
					      (intern (match-string 1))))
		    codesys)
	       (and (progn
		      (goto-char (point-min))
		      (setq case-fold-search nil)
		      (re-search-forward "^;;;coding system: "
					 ;;(+ (point-min) 3000) t))
					 nil t))
		    (looking-at "[^\t\n\r ]+")
		    (find-coding-system
		     (setq codesys (intern (match-string 0))))
		    codesys)
	       (and filename
		    (setq codesys
			  (find-file-coding-system-for-read-from-filename
			   filename))
		    (coding-system-name (coding-system-base codesys)))))))
	(when decomp
	  (kill-buffer (current-buffer)))))))

(provide 'mm-util)

;; arch-tag: 94dc5388-825d-4fd1-bfa5-2100aa351238
;;; mm-util.el ends here
