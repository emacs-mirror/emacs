;;; rfc2047.el --- Functions for encoding and decoding rfc2047 messages
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

;;; Code:

(eval-and-compile
  (eval
   '(unless (fboundp 'base64-decode-string)
      (require 'base64))))

(require 'qp)
(require 'mm-util)
(require 'ietf-drums)
(require 'mail-prsvr)

(defvar rfc2047-header-encoding-alist
  '(("Newsgroups" . nil)
    ("Message-ID" . nil)
    (t . mime))
  "*Header/encoding method alist.
The list is traversed sequentially.  The keys can either be
header regexps or `t'.

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
    (iso-2022-jp . B)
    (iso-2022-kr . B)
    (gb2312 . B)
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
    ;; Avoid using 8bit characters. Some versions of Emacs has bug!
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
	      (rfc2047-encode-region (point-min) (point-max))
	      (rfc2047-fold-region (point-min) (point-max)))
	     ((eq method 'default)
	      (if (and (featurep 'mule)
		       mail-parse-charset)
		  (mm-encode-coding-region (point-min) (point-max) 
					   mail-parse-charset)))
	     ((mm-coding-system-p method)
	      (if (featurep 'mule)
		  (mm-encode-coding-region (point-min) (point-max) method)))
	     ;; Hm.
	     (t)))
	  (goto-char (point-max)))))))

(defun rfc2047-encodable-p (&optional header)
  "Say whether the current (narrowed) buffer contains characters that need encoding in headers."
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
  (let ((all-specials (concat ietf-drums-tspecials " \t\n\r"))
	(special-list (mapcar 'identity ietf-drums-tspecials))
	(blank-list '(?  ?\t ?\n ?\r))
	words current cs state mail-parse-mule-charset)
    (save-restriction
      (narrow-to-region b e)
      (goto-char (point-min))
      (skip-chars-forward all-specials)
      (setq b (point))
      (while (not (eobp))
	(cond
	 ((not state)
	  (setq state 'word)
	  (if (not (eq (setq cs (mm-charset-after)) 'ascii))
	      (setq current cs))
	  (setq b (point)))
	 ((eq state 'blank)
	  (cond 
	   ((memq (char-after) special-list)
	    (setq state nil))
	   ((memq (char-after) blank-list))
	   (t
	    (setq state 'word)
	    (unless b
		(setq b (point)))
	    (if (not (eq (setq cs (mm-charset-after)) 'ascii))
		(setq current cs)))))
	 ((eq state 'word)
	  (cond 
	   ((memq (char-after) special-list)
	    (setq state nil)
	    (push (list b (point) current) words)
	    (setq current nil))
	   ((memq (char-after) blank-list)
	    (setq state 'blank)
	    (if (not current)
		(setq b nil)
	      (push (list b (point) current) words)
	      (setq b (point))
	      (setq current nil)))
	   ((or (eq (setq cs (mm-charset-after)) 'ascii)
		(if current
		    (eq current cs)
		  (setq current cs))))
	   (t
	    (push (list b (point) current) words)
	    (setq current cs)
	    (setq b (point))))))
	(if state
	    (forward-char)
	  (skip-chars-forward all-specials)))
      (if (eq state 'word)
	  (push (list b (point) current) words)))
    words))

(defun rfc2047-encode-region (b e)
  "Encode all encodable words in REGION."
  (let ((words (rfc2047-dissect-region b e))
	beg end current word)
    (while (setq word (pop words))
      (if (equal (nth 2 word) current)
	  (setq beg (nth 0 word))
	(when current
	  (if (and (eq beg (nth 1 word)) (nth 2 word))
	      (progn
		;; There might be a bug in Emacs Mule.
		;; A space must be inserted before encoding.
		(goto-char beg)
		(insert " ")
		(rfc2047-encode (1+ beg) (1+ end) current))
	    (rfc2047-encode beg end current)))
	(setq current (nth 2 word)
	      beg (nth 0 word)
	      end (nth 1 word))))
    (when current
      (rfc2047-encode beg end current))))

(defun rfc2047-encode-string (string)
  "Encode words in STRING."
  (with-temp-buffer
    (insert string)
    (rfc2047-encode-region (point-min) (point-max))
    (buffer-string)))

(defun rfc2047-encode (b e charset)
  "Encode the word in the region with CHARSET."
  (let* ((mime-charset (mm-mime-charset charset))
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
	       (mm-coding-system-p mime-charset))
	  (mm-encode-coding-region (point-min) (point-max) mime-charset))
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
  "Fold the long lines in the region."
  (save-restriction
    (narrow-to-region b e)
    (goto-char (point-min))
    (let ((break nil))
      (while (not (eobp))
	(cond
	 ((memq (char-after) '(?  ?\t))
	  (setq break (point)))
	 ((and (not break)
	       (looking-at "=\\?"))
	  (setq break (point)))
	 ((and break
	       (looking-at "\\?=")
	       (> (- (point) (save-excursion (beginning-of-line) (point))) 76))
	  (goto-char break)
	  (setq break nil)
	  (insert "\n ")))
	(unless (eobp)
	  (forward-char 1))))))

(defun rfc2047-b-encode-region (b e)
  "Encode the header contained in REGION with the B encoding."
  (save-restriction
    (narrow-to-region (goto-char b) e)
    (while (not (eobp))
      (base64-encode-region (point) (progn (end-of-line) (point)) t)
      (if (and (bolp) (eolp))
	  (delete-backward-char 1))
      (forward-line))))

(defun rfc2047-q-encode-region (b e)
  "Encode the header contained in REGION with the Q encoding."
  (save-excursion
    (save-restriction
      (narrow-to-region (goto-char b) e)
      (let ((alist rfc2047-q-encoding-alist))
	(while alist
	  (when (looking-at (caar alist))
	    (quoted-printable-encode-region b e nil (cdar alist))
	    (subst-char-in-region (point-min) (point-max) ?  ?_)
	    (setq alist nil))
	  (pop alist))
	(goto-char (point-min))
	(while (not (eobp))
	  (goto-char (min (point-max) (+ 64 (point))))
	  (search-backward "=" (- (point) 2) t)
	  (unless (eobp)
	    (insert "\n")))))))

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
	  (mm-decode-coding-region b (point-max) mail-parse-charset))))))

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
  "Decode STRING that uses CHARSET with ENCODING.
Valid ENCODINGs are \"B\" and \"Q\".
If your Emacs implementation can't decode CHARSET, it returns nil."
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
      (mm-decode-coding-string
       (cond
	((equal "B" encoding)
	 (base64-decode-string string))
	((equal "Q" encoding)
	 (quoted-printable-decode-string
	  (mm-replace-chars-in-string string ?_ ? )))
	(t (error "Invalid encoding: %s" encoding)))
       cs))))

(provide 'rfc2047)

;;; rfc2047.el ends here
