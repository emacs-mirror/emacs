;;; nndoc.el --- single file access for Gnus

;; Copyright (C) 1995 Free Software Foundation, Inc.

;; Author: Lars Magne Ingebrigtsen <larsi@ifi.uio.no>
;; 	Masanobu UMEDA <umerin@flab.flab.fujitsu.junet>
;; Keywords: news

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

;;; Commentary:

;;; Code:

(require 'nnheader)
(require 'rmail)
(require 'nnmail)

(defvar nndoc-article-type 'mbox
  "*Type of the file - one of `mbox', `babyl' or `digest'.")

(defvar nndoc-digest-type 'traditional
  "Type of the last digest.  Auto-detected from the article header.
Possible values:
  `traditional' -- the \"lots of dashes\" (30+) rules used;
                   we currently also do unconditional RFC 934 unquoting.
  `rfc1341' -- RFC 1341 digest (MIME, unique boundary, no quoting).")

(defconst nndoc-type-to-regexp
  (list (list 'mbox 
	      (concat "^" rmail-unix-mail-delimiter)
	      (concat "^" rmail-unix-mail-delimiter)
	      nil "^$" nil nil nil)
	(list 'babyl "\^_\^L *\n" "\^_" "^[0-9].*\n" "^$" nil nil
	      "\\*\\*\\* EOOH \\*\\*\\*\n\\(^.+\n\\)*")
	(list 'digest
	      "^------------------------------*[\n \t]+"
	      "^------------------------------*[\n \t]+"
	      nil "^ ?$"   
	      "^------------------------------*[\n \t]+"
	      "^End of" nil))
  "Regular expressions for articles of the various types.")



(defvar nndoc-article-begin nil)
(defvar nndoc-article-end nil)
(defvar nndoc-head-begin nil)
(defvar nndoc-head-end nil)
(defvar nndoc-first-article nil)
(defvar nndoc-end-of-file nil)
(defvar nndoc-body-begin nil)

(defvar nndoc-current-server nil)
(defvar nndoc-server-alist nil)
(defvar nndoc-server-variables
  (list
   (list 'nndoc-article-type nndoc-article-type)
   '(nndoc-article-begin nil)
   '(nndoc-article-end nil)
   '(nndoc-head-begin nil)
   '(nndoc-head-end nil)
   '(nndoc-first-article nil)
   '(nndoc-current-buffer nil)
   '(nndoc-group-alist nil)
   '(nndoc-end-of-file nil)
   '(nndoc-body-begin nil)
   '(nndoc-address nil)))

(defconst nndoc-version "nndoc 1.0"
  "nndoc version.")

(defvar nndoc-current-buffer nil
  "Current nndoc news buffer.")

(defvar nndoc-address nil)



(defvar nndoc-status-string "")

(defvar nndoc-group-alist nil)

;;; Interface functions

(defun nndoc-retrieve-headers (sequence &optional newsgroup server)
  (save-excursion
    (set-buffer nntp-server-buffer)
    (erase-buffer)
    (let ((prev 2)
	  article p beg lines)
      (nndoc-possibly-change-buffer newsgroup server)
      (if (stringp (car sequence))
	  'headers
	(set-buffer nndoc-current-buffer)
	(widen)
	(goto-char (point-min))
	(re-search-forward (or nndoc-first-article 
			       nndoc-article-begin) nil t)
	(or (not nndoc-head-begin)
	    (re-search-forward nndoc-head-begin nil t))
	(re-search-forward nndoc-head-end nil t)
	(while sequence
	  (setq article (car sequence))
	  (set-buffer nndoc-current-buffer)
	  (if (not (nndoc-forward-article (max 0 (- article prev))))
	      ()
	    (setq p (point))
	    (setq beg (or (and
			   (re-search-backward nndoc-article-begin nil t)
			   (match-end 0))
			  (point-min)))
	    (goto-char p)
	    (setq lines (count-lines 
			 (point)
			 (or
			  (and (re-search-forward nndoc-article-end nil t)
			       (goto-char (match-beginning 0)))
			  (goto-char (point-max)))))

	    (set-buffer nntp-server-buffer)
	    (insert (format "221 %d Article retrieved.\n" article))
	    (insert-buffer-substring nndoc-current-buffer beg p)
	    (goto-char (point-max))
	    (or (= (char-after (1- (point))) ?\n) (insert "\n"))
	    (insert (format "Lines: %d\n" lines))
	    (insert ".\n"))

	  (setq prev article
		sequence (cdr sequence)))

	;; Fold continuation lines.
	(set-buffer nntp-server-buffer)
	(goto-char (point-min))
	(while (re-search-forward "\\(\r?\n[ \t]+\\)+" nil t)
	  (replace-match " " t t))
	'headers))))

(defun nndoc-open-server (server &optional defs)
  (nnheader-init-server-buffer)
  (if (equal server nndoc-current-server)
      t
    (if nndoc-current-server
	(setq nndoc-server-alist 
	      (cons (list nndoc-current-server
			  (nnheader-save-variables nndoc-server-variables))
		    nndoc-server-alist)))
    (let ((state (assoc server nndoc-server-alist)))
      (if state 
	  (progn
	    (nnheader-restore-variables (nth 1 state))
	    (setq nndoc-server-alist (delq state nndoc-server-alist)))
	(nnheader-set-init-variables nndoc-server-variables defs)))
    (setq nndoc-current-server server)
    (let ((defs (cdr (assq nndoc-article-type nndoc-type-to-regexp))))
      (setq nndoc-article-begin (nth 0 defs))
      (setq nndoc-article-end (nth 1 defs))
      (setq nndoc-head-begin (nth 2 defs))
      (setq nndoc-head-end (nth 3 defs))
      (setq nndoc-first-article (nth 4 defs))
      (setq nndoc-end-of-file (nth 5 defs))
      (setq nndoc-body-begin (nth 6 defs)))
    t))

(defun nndoc-close-server (&optional server)
  t)

(defun nndoc-server-opened (&optional server)
  (and (equal server nndoc-current-server)
       nntp-server-buffer
       (buffer-name nntp-server-buffer)))

(defun nndoc-status-message (&optional server)
  nndoc-status-string)

(defun nndoc-request-article (article &optional newsgroup server buffer)
  (nndoc-possibly-change-buffer newsgroup server)
  (save-excursion
    (let ((buffer (or buffer nntp-server-buffer)))
      (set-buffer buffer)
      (erase-buffer)
      (if (stringp article)
	  nil
	(nndoc-insert-article article)
	;; Unquote quoted non-separators in digests.
	(if (and (eq nndoc-article-type 'digest)
		 (eq nndoc-digest-type 'traditional))
	    (progn
	      (goto-char (point-min))
	      (while (re-search-forward "^- -"nil t)
		(replace-match "-" t t))))
	;; Some assholish digests do not have a blank line after the
	;; headers. Aargh!
	(goto-char (point-min))
	(if (search-forward "\n\n" nil t)
	    ()				; We let this one pass.
	  (if (re-search-forward "^[ \t]+$" nil t)
	      (replace-match "" t t)	; We nix out a line of blanks.
	    (while (and (looking-at "[^ ]+:")
			(zerop (forward-line 1))))
	    ;; We just insert a couple of lines. If you read digests
	    ;; that are so badly formatted, you don't deserve any
	    ;; better. Blphphpht!
	    (insert "\n\n")))
	t))))

(defun nndoc-request-group (group &optional server dont-check)
  "Select news GROUP."
  (save-excursion
    (if (not (nndoc-possibly-change-buffer group server))
	(progn
	  (setq nndoc-status-string "No such file or buffer")
	  nil)
      (nndoc-set-header-dependent-regexps) ; hack for MIME digests
      (if dont-check
	  t
	(save-excursion
	  (set-buffer nntp-server-buffer)
	  (erase-buffer)
	  (let ((number (nndoc-number-of-articles)))
	    (if (zerop number)
		(progn
		  (nndoc-close-group group)
		  nil)
	      (insert (format "211 %d %d %d %s\n" number 1 number group))
	      t)))))))

(defun nndoc-close-group (group &optional server)
  (nndoc-possibly-change-buffer group server)
  (kill-buffer nndoc-current-buffer)
  (setq nndoc-group-alist (delq (assoc group nndoc-group-alist)
				nndoc-group-alist))
  (setq nndoc-current-buffer nil)
  (setq nndoc-current-server nil)
  t)

(defun nndoc-request-list (&optional server)
  nil)

(defun nndoc-request-newgroups (date &optional server)
  nil)

(defun nndoc-request-list-newsgroups (&optional server)
  nil)

(defalias 'nndoc-request-post 'nnmail-request-post)
(defalias 'nndoc-request-post-buffer 'nnmail-request-post-buffer)


;;; Internal functions.

(defun nndoc-possibly-change-buffer (group source)
  (let (buf)
    (cond 
     ;; The current buffer is this group's buffer.
     ((and nndoc-current-buffer
	   (eq nndoc-current-buffer 
	       (setq buf (cdr (assoc group nndoc-group-alist))))))
     ;; We change buffers by taking an old from the group alist.
     ;; `source' is either a string (a file name) or a buffer object. 
     (buf
      (setq nndoc-current-buffer buf))
     ;; It's a totally new group.    
     ((or (and (bufferp nndoc-address)
	       (buffer-name nndoc-address))
	  (and (stringp nndoc-address)
	       (file-exists-p nndoc-address)
	       (not (file-directory-p nndoc-address))))
      (setq nndoc-group-alist 
	    (cons (cons group (setq nndoc-current-buffer 
				    (get-buffer-create 
				     (concat " *nndoc " group "*"))))
		  nndoc-group-alist))
      (save-excursion
	(set-buffer nndoc-current-buffer)
	(buffer-disable-undo (current-buffer))
	(erase-buffer)
	(if (stringp nndoc-address)
	    (insert-file-contents nndoc-address)
	  (save-excursion
	    (set-buffer nndoc-address)
	    (widen))
	  (insert-buffer-substring nndoc-address))
	t)))))

;; MIME (RFC 1341) digest hack by Ulrik Dickow <dickow@nbi.dk>.
(defun nndoc-set-header-dependent-regexps ()
  (if (not (eq nndoc-article-type 'digest))
      ()
    (let ((case-fold-search t)		; We match a bit too much, keep it simple.
	  (boundary-id) (b-delimiter))
      (save-excursion
	(set-buffer nndoc-current-buffer)
	(goto-char (point-min))
	(if (and
	     (re-search-forward
	      (concat "\n\n\\|^Content-Type: *multipart/digest;[ \t\n]*[ \t]"
		      "boundary=\"\\([^\"\n]*[^\" \t\n]\\)\"")
	      nil t)
	     (match-beginning 1))
	    (setq nndoc-digest-type 'rfc1341
		  boundary-id (format "%s"
				      (buffer-substring
				       (match-beginning 1) (match-end 1)))
		  b-delimiter       (concat "\n--" boundary-id "[\n \t]+")
		  nndoc-article-begin b-delimiter ; Too strict: "[ \t]*$"
		  nndoc-article-end (concat "\n--" boundary-id
					    "\\(--\\)?[\n \t]+")
		  nndoc-first-article b-delimiter ; ^eof ends article too.
		  nndoc-end-of-file (concat "\n--" boundary-id "--[ \t]*$"))
	  (setq nndoc-digest-type 'traditional))))))

(defun nndoc-forward-article (n)
  (while (and (> n 0)
	      (re-search-forward nndoc-article-begin nil t)
	      (or (not nndoc-head-begin)
		  (re-search-forward nndoc-head-begin nil t))
	      (re-search-forward nndoc-head-end nil t))
    (setq n (1- n)))
  (zerop n))

(defun nndoc-number-of-articles ()
  (save-excursion
    (set-buffer nndoc-current-buffer)
    (widen)
    (goto-char (point-min))
    (let ((num 0))
      (if (re-search-forward (or nndoc-first-article
				 nndoc-article-begin) nil t)
	  (progn
	    (setq num 1)
	    (while (and (re-search-forward nndoc-article-begin nil t)
			(or (not nndoc-end-of-file)
			    (not (looking-at nndoc-end-of-file)))
			(or (not nndoc-head-begin)
			    (re-search-forward nndoc-head-begin nil t))
			(re-search-forward nndoc-head-end nil t))
	      (setq num (1+ num)))))
      num)))

(defun nndoc-narrow-to-article (article)
  (save-excursion
    (set-buffer nndoc-current-buffer)
    (widen)
    (goto-char (point-min))
    (while (and (re-search-forward nndoc-article-begin nil t)
		(not (zerop (setq article (1- article))))))
    (if (not (zerop article))
	()
      (narrow-to-region 
       (match-end 0)
       (or (and (re-search-forward nndoc-article-end nil t)
		(match-beginning 0))
	   (point-max)))
      t)))

;; Insert article ARTICLE in the current buffer.
(defun nndoc-insert-article (article)
  (let ((ibuf (current-buffer)))
    (save-excursion
      (set-buffer nndoc-current-buffer)
      (widen)
      (goto-char (point-min))
      (while (and (re-search-forward nndoc-article-begin nil t)
		  (not (zerop (setq article (1- article))))))
      (if (not (zerop article))
	  ()
	(narrow-to-region 
	 (match-end 0)
	 (or (and (re-search-forward nndoc-article-end nil t)
		  (match-beginning 0))
	     (point-max)))
	(goto-char (point-min))
	(and nndoc-head-begin
	     (re-search-forward nndoc-head-begin nil t)
	     (narrow-to-region (point) (point-max)))
	(or (re-search-forward nndoc-head-end nil t)
	    (goto-char (point-max)))
	(append-to-buffer ibuf (point-min) (point))
	(and nndoc-body-begin 
	     (re-search-forward nndoc-body-begin nil t))
	(append-to-buffer ibuf (point) (point-max))
	t))))

(provide 'nndoc)

;;; nndoc.el ends here
