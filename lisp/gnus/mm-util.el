;;; mm-util.el --- Utility functions for MIME things
;; Copyright (C) 1998, 1999, 2000 Free Software Foundation, Inc.

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
;;	MORIOKA Tomohiko <morioka@jaist.ac.jp>
;; Maintainer: bugs@gnus.org
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

(defvar mm-mime-mule-charset-alist
  '((us-ascii ascii)
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
    (viscii vietnamese-viscii-lower)
    (iso-2022-jp latin-jisx0201 japanese-jisx0208 japanese-jisx0208-1978)
    (euc-kr korean-ksc5601)
    (cn-gb-2312 chinese-gb2312)
    (cn-big5 chinese-big5-1 chinese-big5-2)
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
    (utf-8 unicode-a unicode-b unicode-c unicode-d unicode-e))
  "Alist of MIME-charset/MULE-charsets.")

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
     (read-coding-system
      . (lambda (prompt)
	  "Prompt the user for a coding system."
	  (completing-read
	   prompt (mapcar (lambda (s) (list (symbol-name (car s))))
			  mm-mime-mule-charset-alist))))
     (read-charset
      . (lambda (prompt)
	  "Return a charset."
	  (intern
	   (completing-read
	    prompt
	    (mapcar (lambda (e) (list (symbol-name (car e))))
		    mm-mime-mule-charset-alist)
	    nil t)))))))

(eval-and-compile
  (defalias 'mm-char-or-char-int-p
    (cond 
     ((fboundp 'char-or-char-int-p) 'char-or-char-int-p)
     ((fboundp 'char-valid-p) 'char-valid-p) 
     (t 'identity))))

(defvar mm-coding-system-list nil)
(defun mm-get-coding-system-list ()
  "Get the coding system list."
  (or mm-coding-system-list
      (setq mm-coding-system-list (mm-coding-system-list))))

(defvar mm-charset-synonym-alist
  `((big5 . cn-big5)
    (gb2312 . cn-gb-2312)
    ;; Windows-1252 is actually a superset of Latin-1.  See also
    ;; `gnus-article-dumbquotes-map'.
    (unless (mm-coding-system-p 'windows-1252) ; should be defined eventually
      (windows-1252 . iso-8859-1))
    (x-ctext . ctext))
  "A mapping from invalid charset names to the real charset names.")

(defun mm-coding-system-p (sym)
  "Return non-nil if SYM is a coding system."
  (or (and (fboundp 'coding-system-p) (coding-system-p sym))
      (memq sym (mm-get-coding-system-list))))

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

;;; Internal variables:

;;; Functions:

(defun mm-mule-charset-to-mime-charset (charset)
  "Return the MIME charset corresponding to MULE CHARSET."
  (let ((alist mm-mime-mule-charset-alist)
	out)
    (while alist
      (when (memq charset (cdar alist))
	(setq out (caar alist)
	      alist nil))
      (pop alist))
    out))

(defun mm-charset-to-coding-system (charset &optional lbt)
  "Return coding-system corresponding to CHARSET.
CHARSET is a symbol naming a MIME charset.
If optional argument LBT (`unix', `dos' or `mac') is specified, it is
used as the line break code type of the coding system."
  (when (stringp charset)
    (setq charset (intern (downcase charset))))
  (setq charset
	(or (cdr (assq charset mm-charset-synonym-alist))
	    charset))
  (when lbt
    (setq charset (intern (format "%s-%s" charset lbt))))
  (cond
   ;; Running in a non-MULE environment.
   ((null (mm-get-coding-system-list))
    charset)
   ;; ascii
   ((eq charset 'us-ascii)
    'ascii)
   ;; Check to see whether we can handle this charset.
   ((memq charset (mm-get-coding-system-list))
    charset)
   ;; Nope.
   (t
    nil)))

(if (fboundp 'subst-char-in-string)
    (defsubst mm-replace-chars-in-string (string from to)
      (subst-char-in-string from to string))
  (defun mm-replace-chars-in-string (string from to)
    "Replace characters in STRING from FROM to TO."
    (let ((string (substring string 0))	;Copy string.
	  (len (length string))
	  (idx 0))
      ;; Replace all occurrences of FROM with TO.
      (while (< idx len)
	(when (= (aref string idx) from)
	  (aset string idx to))
	(setq idx (1+ idx)))
      string)))

(defsubst mm-enable-multibyte ()
  "Enable multibyte in the current buffer."
  (when (and (fboundp 'set-buffer-multibyte)
             (boundp 'enable-multibyte-characters)
	     (default-value 'enable-multibyte-characters))
    (set-buffer-multibyte t)))

(defsubst mm-disable-multibyte ()
  "Disable multibyte in the current buffer."
  (when (fboundp 'set-buffer-multibyte)
    (set-buffer-multibyte nil)))

(defsubst mm-enable-multibyte-mule4 ()
  "Enable multibyte in the current buffer.
Only used in Emacs Mule 4."
  (when (and (fboundp 'set-buffer-multibyte)
             (boundp 'enable-multibyte-characters)
	     (default-value 'enable-multibyte-characters)
	     (not (charsetp 'eight-bit-control)))
    (set-buffer-multibyte t)))

(defsubst mm-disable-multibyte-mule4 ()
  "Disable multibyte in the current buffer.
Only used in Emacs Mule 4."
  (when (and (fboundp 'set-buffer-multibyte)
	     (not (charsetp 'eight-bit-control)))
    (set-buffer-multibyte nil)))

(defun mm-preferred-coding-system (charset)
  ;; A typo in some Emacs versions.
  (or (get-charset-property charset 'prefered-coding-system)
      (get-charset-property charset 'preferred-coding-system)))

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
			   'latin-iso8859-1)))
	     mail-parse-mule-charset)))))))

(defun mm-mime-charset (charset)
  "Return the MIME charset corresponding to the MULE CHARSET."
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

(defun mm-find-mime-charset-region (b e)
  "Return the MIME charsets needed to encode the region between B and E."
  (let ((charsets (mapcar 'mm-mime-charset
			  (delq 'ascii
				(mm-find-charset-region b e)))))
    (when (memq 'iso-2022-jp-2 charsets)
      (setq charsets (delq 'iso-2022-jp charsets)))
    (setq charsets (mm-delete-duplicates charsets))
    (if (and (> (length charsets) 1)
	     (fboundp 'find-coding-systems-region)
	     (memq 'utf-8 (find-coding-systems-region b e)))
	'(utf-8)
      charsets)))

(defsubst mm-multibyte-p ()
  "Say whether multibyte is enabled."
  (if (and (not (featurep 'xemacs))
	   (boundp 'enable-multibyte-characters))
      enable-multibyte-characters
    (featurep 'mule)))

(defmacro mm-with-unibyte-buffer (&rest forms)
  "Create a temporary buffer, and evaluate FORMS there like `progn'.
See also `with-temp-file' and `with-output-to-string'."
  (let ((temp-buffer (make-symbol "temp-buffer"))
	(multibyte (make-symbol "multibyte")))
    `(if (or (featurep 'xemacs)
	     (not (boundp 'enable-multibyte-characters)))
	 (with-temp-buffer ,@forms)
       (let ((,multibyte (default-value 'enable-multibyte-characters))
	     ,temp-buffer)
	 (unwind-protect
	     (progn
	       (setq-default enable-multibyte-characters nil)
	       (setq ,temp-buffer
		     (get-buffer-create (generate-new-buffer-name " *temp*")))
	       (unwind-protect
		   (with-current-buffer ,temp-buffer
		     (let ((buffer-file-coding-system mm-binary-coding-system)
			   (coding-system-for-read mm-binary-coding-system)
			   (coding-system-for-write mm-binary-coding-system))
		       ,@forms))
		 (and (buffer-name ,temp-buffer)
		      (kill-buffer ,temp-buffer))))
	   (setq-default enable-multibyte-characters ,multibyte))))))
(put 'mm-with-unibyte-buffer 'lisp-indent-function 0)
(put 'mm-with-unibyte-buffer 'edebug-form-spec '(body))

(defmacro mm-with-unibyte-current-buffer (&rest forms)
  "Evaluate FORMS there like `progn' in current buffer."
  (let ((multibyte (make-symbol "multibyte")))
    `(if (or (featurep 'xemacs)
	     (not (fboundp 'set-buffer-multibyte)))
	 (progn
	   ,@forms)
       (let ((,multibyte (default-value 'enable-multibyte-characters)))
	 (unwind-protect
	     (let ((buffer-file-coding-system mm-binary-coding-system)
		   (coding-system-for-read mm-binary-coding-system)
		   (coding-system-for-write mm-binary-coding-system))
	       (set-buffer-multibyte nil)
	       (setq-default enable-multibyte-characters nil)
	       ,@forms)
	   (setq-default enable-multibyte-characters ,multibyte)
	   (set-buffer-multibyte ,multibyte))))))
(put 'mm-with-unibyte-current-buffer 'lisp-indent-function 0)
(put 'mm-with-unibyte-current-buffer 'edebug-form-spec '(body))

(defmacro mm-with-unibyte-current-buffer-mule4 (&rest forms)
  "Evaluate FORMS there like `progn' in current buffer.
Mule4 only."
  (let ((multibyte (make-symbol "multibyte")))
    `(if (or (featurep 'xemacs)
	     (not (fboundp 'set-buffer-multibyte))
	     (charsetp 'eight-bit-control)) ;; For Emacs Mule 4 only.
	 (progn
	   ,@forms)
       (let ((,multibyte (default-value 'enable-multibyte-characters)))
	 (unwind-protect
	     (let ((buffer-file-coding-system mm-binary-coding-system)
		   (coding-system-for-read mm-binary-coding-system)
		   (coding-system-for-write mm-binary-coding-system))
	       (set-buffer-multibyte nil)
	       (setq-default enable-multibyte-characters nil)
	       ,@forms)
	   (setq-default enable-multibyte-characters ,multibyte)
	   (set-buffer-multibyte ,multibyte))))))
(put 'mm-with-unibyte-current-buffer-mule4 'lisp-indent-function 0)
(put 'mm-with-unibyte-current-buffer-mule4 'edebug-form-spec '(body))

(defmacro mm-with-unibyte (&rest forms)
  "Set default `enable-multibyte-characters' to `nil', eval the FORMS."
  (let ((multibyte (make-symbol "multibyte")))
    `(if (or (featurep 'xemacs)
	     (not (boundp 'enable-multibyte-characters)))
	 (progn ,@forms)
       (let ((,multibyte (default-value 'enable-multibyte-characters)))
	 (unwind-protect
	     (progn
	       (setq-default enable-multibyte-characters nil)
	       ,@forms)
	   (setq-default enable-multibyte-characters ,multibyte))))))
(put 'mm-with-unibyte 'lisp-indent-function 0)
(put 'mm-with-unibyte 'edebug-form-spec '(body))

(defun mm-find-charset-region (b e)
  "Return a list of charsets in the region."
  (cond
   ((and (mm-multibyte-p)
 	 (fboundp 'find-charset-region))
    ;; Remove composition since the base charsets have been included.
    (delq 'composition (find-charset-region b e)))
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
  '(jka-compr-handler)
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

(provide 'mm-util)

;;; mm-util.el ends here
