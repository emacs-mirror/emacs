;;; rfc2047.el --- functions for encoding and decoding rfc2047 messages
;; Copyright (C) 1998, 1999, 2000 Free Software Foundation, Inc.

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

;; RFC 2047 is "MIME (Multipurpose Internet Mail Extensions) Part
;; Three:  Message Header Extensions for Non-ASCII Text".

;;; Code:

(eval-when-compile (require 'cl))

(require 'qp)
(require 'mm-util)
(require 'ietf-drums)
(require 'mail-prsvr)
(require 'base64)
;; Fixme: Avoid this (for gnus-point-at-...) mm dependence on gnus.
(require 'gnus-util)
(autoload 'mm-body-7-or-8 "mm-bodies")

(defvar rfc2047-header-encoding-alist
  '(("Newsgroups" . nil)
    ("Message-ID" . nil)
    (t . mime))
  "*Header/encoding method alist.
The list is traversed sequentially.  The keys can either be
header regexps or t.

The values can be:

1) nil, in which case no encoding is done;
2) `mime', in which case the header will be encoded according to RFC2047;
3) a charset, in which case it will be encoded as that charset;
4) `default', in which case the field will be encoded as the rest
   of the article.")

(defvar rfc2047-charset-encoding-alist
  '((us-ascii . nil)
    (iso-8859-1 . Q)
    (iso-8859-2 . Q)
    (iso-8859-3 . Q)
    (iso-8859-4 . Q)
    (iso-8859-5 . B)
    (koi8-r . B)
    (iso-8859-7 . Q)
    (iso-8859-8 . Q)
    (iso-8859-9 . Q)
    (iso-8859-14 . Q)
    (iso-8859-15 . Q)
    (iso-2022-jp . B)
    (iso-2022-kr . B)
    (gb2312 . B)
    (big5 . B)
    (cn-big5 . B)
    (cn-gb . B)
    (cn-gb-2312 . B)
    (euc-kr . B)
    (iso-2022-jp-2 . B)
    (iso-2022-int-1 . B))
  "Alist of MIME charsets to RFC2047 encodings.
Valid encodings are nil, `Q' and `B'.")

(defvar rfc2047-encoding-function-alist
  '((Q . rfc2047-q-encode-region)
    (B . rfc2047-b-encode-region)
    (nil . ignore))
  "Alist of RFC2047 encodings to encoding functions.")

(defvar rfc2047-q-encoding-alist
  '(("\\(From\\|Cc\\|To\\|Bcc\||Reply-To\\):" . "-A-Za-z0-9!*+/")
    ;; = (\075), _ (\137), ? (\077) are used in the encoded word.
    ;; Avoid using 8bit characters.
    ;; Equivalent to "^\000-\007\011\013\015-\037\200-\377=_?"
    ("." . "\010\012\014\040-\074\076\100-\136\140-\177"))
  "Alist of header regexps and valid Q characters.")

;;;
;;; Functions for encoding RFC2047 messages
;;;

(defun rfc2047-narrow-to-field ()
  "Narrow the buffer to the header on the current line."
  (beginning-of-line)
  (narrow-to-region
   (point)
   (progn
     (forward-line 1)
     (if (re-search-forward "^[^ \n\t]" nil t)
	 (progn
	   (beginning-of-line)
	   (point))
       (point-max))))
  (goto-char (point-min)))

(defun rfc2047-encode-message-header ()
  "Encode the message header according to `rfc2047-header-encoding-alist'.
Should be called narrowed to the head of the message."
  (interactive "*")
  (save-excursion
    (goto-char (point-min))
    (let (alist elem method)
      (while (not (eobp))
	(save-restriction
	  (rfc2047-narrow-to-field)
	  (if (not (rfc2047-encodable-p))
	      (if (and (eq (mm-body-7-or-8) '8bit)
		       (mm-multibyte-p)
		       (mm-coding-system-p
			(car message-posting-charset)))
		       ;; 8 bit must be decoded.
		       ;; Is message-posting-charset a coding system?
		       (mm-encode-coding-region
			(point-min) (point-max)
			(car message-posting-charset)))
	    ;; We found something that may perhaps be encoded.
	    (setq method nil
		  alist rfc2047-header-encoding-alist)
	    (while (setq elem (pop alist))
	      (when (or (and (stringp (car elem))
			     (looking-at (car elem)))
			(eq (car elem) t))
		(setq alist nil
		      method (cdr elem))))
	    (cond
	     ((eq method 'mime)
	      (rfc2047-encode-region (point-min) (point-max)))
	     ((eq method 'default)
	      (if (and (featurep 'mule)
		       (if (boundp 'default-enable-multibyte-characters)
			   default-enable-multibyte-characters)
		       mail-parse-charset)
		  (mm-encode-coding-region (point-min) (point-max)
					   mail-parse-charset)))
	     ((mm-coding-system-p method)
	      (if (and (featurep 'mule)
		       (if (boundp 'default-enable-multibyte-characters)
			   default-enable-multibyte-characters))
		  (mm-encode-coding-region (point-min) (point-max) method)))
	     ;; Hm.
	     (t)))
	  (goto-char (point-max)))))))

;; Fixme: This, and the require below may not be the Right Thing, but
;; should be safe just before release.  -- fx 2001-02-08
(eval-when-compile (defvar message-posting-charset))

(defun rfc2047-encodable-p ()
  "Return non-nil if any characters in current buffer need encoding in headers.
The buffer may be narrowed."
  (require 'message)			; for message-posting-charset
  (let ((charsets
	 (mapcar
	  'mm-mime-charset
	  (mm-find-charset-region (point-min) (point-max))))
	(cs (list 'us-ascii (car message-posting-charset)))
	found)
    (while charsets
      (unless (memq (pop charsets) cs)
	(setq found t)))
    found))

(defun rfc2047-dissect-region (b e)
  "Dissect the region between B and E into words."
  (let ((word-chars "-A-Za-z0-9!*+/")
	;; Not using ietf-drums-specials-token makes life simple.
	mail-parse-mule-charset
	words point current
	result word)
    (save-restriction
      (narrow-to-region b e)
      (goto-char (point-min))
      (skip-chars-forward "\000-\177")
      (while (not (eobp))
	(setq point (point))
	(skip-chars-backward word-chars b)
	(unless (eq b (point))
	  (push (cons (buffer-substring b (point)) nil) words))
	(setq b (point))
	(goto-char point)
	(setq current (mm-charset-after))
	(forward-char 1)
	(skip-chars-forward word-chars)
	(while (and (not (eobp))
		    (eq (mm-charset-after) current))
	  (forward-char 1)
	  (skip-chars-forward word-chars))
	(unless (eq b (point))
	  (push (cons (buffer-substring b (point)) current) words))
	(setq b (point))
	(skip-chars-forward "\000-\177"))
      (unless (eq b (point))
	(push (cons (buffer-substring b (point)) nil) words)))
    ;; merge adjacent words
    (setq word (pop words))
    (while word
      (if (and (cdr word)
	       (caar words)
	       (not (cdar words))
	       (not (string-match "[^ \t]" (caar words))))
	  (if (eq (cdr (nth 1 words)) (cdr word))
	      (progn
		(setq word (cons (concat
				  (car (nth 1 words)) (caar words)
				  (car word))
				 (cdr word)))
		(pop words)
		(pop words))
	    (push (cons (concat (caar words) (car word)) (cdr word))
		  result)
	    (pop words)
	    (setq word (pop words)))
	(push word result)
	(setq word (pop words))))
    result))

(defun rfc2047-encode-region (b e)
  "Encode all encodable words in region B to E."
  (let ((words (rfc2047-dissect-region b e)) word)
    (save-restriction
      (narrow-to-region b e)
      (delete-region (point-min) (point-max))
      (while (setq word (pop words))
	(if (not (cdr word))
	    (insert (car word))
	  (rfc2047-fold-region (gnus-point-at-bol) (point))
	  (goto-char (point-max))
	  (if (> (- (point) (save-restriction
			      (widen)
			      (gnus-point-at-bol))) 76)
	      (insert "\n "))
	  ;; Insert blank between encoded words
	  (if (eq (char-before) ?=) (insert " "))
	  (rfc2047-encode (point)
			  (progn (insert (car word)) (point))
			  (cdr word))))
      (rfc2047-fold-region (point-min) (point-max)))))

(defun rfc2047-encode-string (string)
  "Encode words in STRING."
  (with-temp-buffer
    (insert string)
    (rfc2047-encode-region (point-min) (point-max))
    (buffer-string)))

(defun rfc2047-encode (b e charset)
  "Encode the word in the region B to E with CHARSET."
  (let* ((mime-charset (mm-mime-charset charset))
	 (cs (mm-charset-to-coding-system mime-charset))
	 (encoding (or (cdr (assq mime-charset
				  rfc2047-charset-encoding-alist))
		       'B))
	 (start (concat
		 "=?" (downcase (symbol-name mime-charset)) "?"
		 (downcase (symbol-name encoding)) "?"))
	 (first t))
    (save-restriction
      (narrow-to-region b e)
      (when (eq encoding 'B)
	;; break into lines before encoding
	(goto-char (point-min))
	(while (not (eobp))
	  (goto-char (min (point-max) (+ 15 (point))))
	  (unless (eobp)
	    (insert "\n"))))
      (if (and (mm-multibyte-p)
	       (mm-coding-system-p cs))
	  (mm-encode-coding-region (point-min) (point-max) cs))
      (funcall (cdr (assq encoding rfc2047-encoding-function-alist))
	       (point-min) (point-max))
      (goto-char (point-min))
      (while (not (eobp))
	(unless first
	  (insert " "))
	(setq first nil)
	(insert start)
	(end-of-line)
	(insert "?=")
	(forward-line 1)))))

(defun rfc2047-fold-region (b e)
  "Fold long lines in region B to E."
  (save-restriction
    (narrow-to-region b e)
    (goto-char (point-min))
    (let ((break nil)
	  (qword-break nil)
	  (bol (save-restriction
		 (widen)
		 (gnus-point-at-bol))))
      (while (not (eobp))
	(when (and (or break qword-break) (> (- (point) bol) 76))
	  (goto-char (or break qword-break))
	  (setq break nil
		qword-break nil)
	  (if (looking-at " \t")
	      (insert "\n")
	    (insert "\n "))
	  (setq bol (1- (point)))
	  ;; Don't break before the first non-LWSP characters.
	  (skip-chars-forward " \t")
	  (unless (eobp) (forward-char 1)))
	(cond
	 ((eq (char-after) ?\n)
	  (forward-char 1)
	  (setq bol (point)
		break nil
		qword-break nil)
	  (skip-chars-forward " \t")
	  (unless (or (eobp) (eq (char-after) ?\n))
	    (forward-char 1)))
	 ((eq (char-after) ?\r)
	  (forward-char 1))
	 ((memq (char-after) '(?  ?\t))
	  (skip-chars-forward " \t")
	  (setq break (1- (point))))
	 ((not break)
	  (if (not (looking-at "=\\?[^=]"))
	      (if (eq (char-after) ?=)
		  (forward-char 1)
		(skip-chars-forward "^ \t\n\r="))
	    (setq qword-break (point))
	    (skip-chars-forward "^ \t\n\r")))
	 (t
	  (skip-chars-forward "^ \t\n\r"))))
      (when (and (or break qword-break) (> (- (point) bol) 76))
	(goto-char (or break qword-break))
	(setq break nil
	      qword-break nil)
	  (if (looking-at " \t")
	      (insert "\n")
	    (insert "\n "))
	(setq bol (1- (point)))
	;; Don't break before the first non-LWSP characters.
	(skip-chars-forward " \t")
	(unless (eobp) (forward-char 1))))))

(defun rfc2047-unfold-region (b e)
  "Unfold lines in region B to E."
  (save-restriction
    (narrow-to-region b e)
    (goto-char (point-min))
    (let ((bol (save-restriction
		 (widen)
		 (gnus-point-at-bol)))
	  (eol (gnus-point-at-eol))
	  leading)
      (forward-line 1)
      (while (not (eobp))
	(looking-at "[ \t]*")
	(setq leading (- (match-end 0) (match-beginning 0)))
	(if (< (- (gnus-point-at-eol) bol leading) 76)
	    (progn
	      (goto-char eol)
	      (delete-region eol (progn
				   (skip-chars-forward "[ \t\n\r]+")
				   (1- (point)))))
	  (setq bol (gnus-point-at-bol)))
	(setq eol (gnus-point-at-eol))
	(forward-line 1)))))

(defun rfc2047-b-encode-region (b e)
  "Base64-encode the header contained in region B to E."
  (save-restriction
    (narrow-to-region (goto-char b) e)
    (while (not (eobp))
      (base64-encode-region (point) (progn (end-of-line) (point)) t)
      (if (and (bolp) (eolp))
	  (delete-backward-char 1))
      (forward-line))))

(defun rfc2047-q-encode-region (b e)
  "Quoted-printable-encode the header in region B to E."
  (save-excursion
    (save-restriction
      (narrow-to-region (goto-char b) e)
      (let ((alist rfc2047-q-encoding-alist)
	    (bol (save-restriction
		   (widen)
		   (gnus-point-at-bol))))
	(while alist
	  (when (looking-at (caar alist))
	    (quoted-printable-encode-region b e nil (cdar alist))
	    (subst-char-in-region (point-min) (point-max) ?  ?_)
	    (setq alist nil))
	  (pop alist))
	;; The size of QP encapsulation is about 20, so set limit to
	;; 56=76-20.
	(unless (< (- (point-max) (point-min)) 56)
	  ;; Don't break if it could fit in one line.
	  ;; Let rfc2047-encode-region break it later.
	  (goto-char (1+ (point-min)))
	  (while (and (not (bobp)) (not (eobp)))
	    (goto-char (min (point-max) (+ 56 bol)))
	    (search-backward "=" (- (point) 2) t)
	    (unless (or (bobp) (eobp))
	      (insert "\n")
	      (setq bol (point)))))))))

;;;
;;; Functions for decoding RFC2047 messages
;;;

(defvar rfc2047-encoded-word-regexp
  "=\\?\\([^][\000-\040()<>@,\;:\\\"/?.=]+\\)\\?\\(B\\|Q\\)\\?\\([!->@-~ +]+\\)\\?=")

(defun rfc2047-decode-region (start end)
  "Decode MIME-encoded words in region between START and END."
  (interactive "r")
  (let ((case-fold-search t)
	b e)
    (save-excursion
      (save-restriction
	(narrow-to-region start end)
	(goto-char (point-min))
	;; Remove whitespace between encoded words.
	(while (re-search-forward
		(concat "\\(" rfc2047-encoded-word-regexp "\\)"
			"\\(\n?[ \t]\\)+"
			"\\(" rfc2047-encoded-word-regexp "\\)")
		nil t)
	  (delete-region (goto-char (match-end 1)) (match-beginning 6)))
	;; Decode the encoded words.
	(setq b (goto-char (point-min)))
	(while (re-search-forward rfc2047-encoded-word-regexp nil t)
	  (setq e (match-beginning 0))
	  (insert (rfc2047-parse-and-decode
		   (prog1
		       (match-string 0)
		     (delete-region (match-beginning 0) (match-end 0)))))
	  (when (and (mm-multibyte-p)
		     mail-parse-charset
		     (not (eq mail-parse-charset 'gnus-decoded)))
	    (mm-decode-coding-region b e mail-parse-charset))
	  (setq b (point)))
	(when (and (mm-multibyte-p)
		   mail-parse-charset
		   (not (eq mail-parse-charset 'us-ascii))
		   (not (eq mail-parse-charset 'gnus-decoded)))
	  (mm-decode-coding-region b (point-max) mail-parse-charset))
	(rfc2047-unfold-region (point-min) (point-max))))))

(defun rfc2047-decode-string (string)
  "Decode the quoted-printable-encoded STRING and return the results."
  (let ((m (mm-multibyte-p)))
    (with-temp-buffer
      (when m
	(mm-enable-multibyte))
      (insert string)
      (inline
	(rfc2047-decode-region (point-min) (point-max)))
      (buffer-string))))

(defun rfc2047-parse-and-decode (word)
  "Decode WORD and return it if it is an encoded word.
Return WORD if not."
  (if (not (string-match rfc2047-encoded-word-regexp word))
      word
    (or
     (condition-case nil
	 (rfc2047-decode
	  (match-string 1 word)
	  (upcase (match-string 2 word))
	  (match-string 3 word))
       (error word))
     word)))

(defun rfc2047-decode (charset encoding string)
  "Decode STRING from the given MIME CHARSET in the given ENCODING.
Valid ENCODINGs are \"B\" and \"Q\".
If your Emacs implementation can't decode CHARSET, return nil."
  (if (stringp charset)
      (setq charset (intern (downcase charset))))
  (if (or (not charset)
	  (eq 'gnus-all mail-parse-ignored-charsets)
	  (memq 'gnus-all mail-parse-ignored-charsets)
	  (memq charset mail-parse-ignored-charsets))
      (setq charset mail-parse-charset))
  (let ((cs (mm-charset-to-coding-system charset)))
    (if (and (not cs) charset
	     (listp mail-parse-ignored-charsets)
	     (memq 'gnus-unknown mail-parse-ignored-charsets))
	(setq cs (mm-charset-to-coding-system mail-parse-charset)))
    (when cs
      (when (and (eq cs 'ascii)
		 mail-parse-charset)
	(setq cs mail-parse-charset))
      ;; Ensure unibyte result in Emacs 20.
      (let (default-enable-multibyte-characters)
	(with-temp-buffer
	  (mm-decode-coding-string
	   (cond
	    ((equal "B" encoding)
	     (base64-decode-string string))
	    ((equal "Q" encoding)
	     (quoted-printable-decode-string
	      (mm-replace-chars-in-string string ?_ ? )))
	    (t (error "Invalid encoding: %s" encoding)))
	   cs))))))

(provide 'rfc2047)

;;; rfc2047.el ends here
