;;; nndoc.el --- single file access for Gnus
;; Copyright (C) 1995,96,97,98 Free Software Foundation, Inc.

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
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
(require 'message)
(require 'nnmail)
(require 'nnoo)
(require 'gnus-util)
(eval-when-compile (require 'cl))

(nnoo-declare nndoc)

(defvoo nndoc-article-type 'guess
  "*Type of the file.
One of `mbox', `babyl', `digest', `news', `rnews', `mmdf', `forward',
`rfc934', `rfc822-forward', `mime-digest', `mime-parts', `standard-digest',
`slack-digest', `clari-briefs' or `guess'.")

(defvoo nndoc-post-type 'mail
  "*Whether the nndoc group is `mail' or `post'.")

(defvoo nndoc-open-document-hook 'nnheader-ms-strip-cr
  "Hook run after opening a document.
The default function removes all trailing carriage returns
from the document.")  

(defvar nndoc-type-alist
  `((mmdf
     (article-begin .  "^\^A\^A\^A\^A\n")
     (body-end .  "^\^A\^A\^A\^A\n"))
    (news
     (article-begin . "^Path:"))
    (rnews
     (article-begin . "^#! *rnews +\\([0-9]+\\) *\n")
     (body-end-function . nndoc-rnews-body-end))
    (mbox
     (article-begin-function . nndoc-mbox-article-begin)
     (body-end-function . nndoc-mbox-body-end))
    (babyl
     (article-begin . "\^_\^L *\n")
     (body-end . "\^_")
     (body-begin-function . nndoc-babyl-body-begin)
     (head-begin-function . nndoc-babyl-head-begin))
    (forward
     (article-begin . "^-+ Start of forwarded message -+\n+")
     (body-end . "^-+ End of forwarded message -+$")
     (prepare-body-function . nndoc-unquote-dashes))
    (rfc934
     (article-begin . "^--.*\n+")
     (body-end . "^--.*$")
     (prepare-body-function . nndoc-unquote-dashes))
    (clari-briefs
     (article-begin . "^ \\*")
     (body-end . "^\t------*[ \t]^*\n^ \\*")
     (body-begin . "^\t")
     (head-end . "^\t")
     (generate-head-function . nndoc-generate-clari-briefs-head)
     (article-transform-function . nndoc-transform-clari-briefs))
    (mime-digest
     (article-begin . "")
     (head-end . "^ ?$")
     (body-end . "")
     (file-end . "")
     (subtype digest guess))
    (mime-parts
     (generate-head-function . nndoc-generate-mime-parts-head)
     (article-transform-function . nndoc-transform-mime-parts))
    (standard-digest
     (first-article . ,(concat "^" (make-string 70 ?-) "\n *\n+"))
     (article-begin . ,(concat "^\n" (make-string 30 ?-) "\n *\n+"))
     (prepare-body-function . nndoc-unquote-dashes)
     (body-end-function . nndoc-digest-body-end)
     (head-end . "^ *$")
     (body-begin . "^ *\n")
     (file-end . "^End of .*digest.*[0-9].*\n\\*\\*\\|^End of.*Digest *$")
     (subtype digest guess))
    (slack-digest
     (article-begin . "^------------------------------*[\n \t]+")
     (head-end . "^ ?$")
     (body-end-function . nndoc-digest-body-end)
     (body-begin . "^ ?$")
     (file-end . "^End of")
     (prepare-body-function . nndoc-unquote-dashes)
     (subtype digest guess))
    (lanl-gov-announce
     (article-begin . "^\\\\\\\\\n")
     (head-begin . "^Paper.*:")
     (head-end   . "\\(^\\\\\\\\.*\n\\|-----------------\\)")
     (body-begin . "")
     (body-end   . "-------------------------------------------------")
     (file-end   . "^Title: Recent Seminal")
     (generate-head-function . nndoc-generate-lanl-gov-head)
     (article-transform-function . nndoc-transform-lanl-gov-announce)
     (subtype preprints guess))
    (rfc822-forward
     (article-begin . "^\n")
     (body-end-function . nndoc-rfc822-forward-body-end-function))
    (guess
     (guess . t)
     (subtype nil))
    (digest
     (guess . t)
     (subtype nil))
    (preprints
     (guess . t)
     (subtype nil))))


(defvoo nndoc-file-begin nil)
(defvoo nndoc-first-article nil)
(defvoo nndoc-article-begin nil)
(defvoo nndoc-head-begin nil)
(defvoo nndoc-head-end nil)
(defvoo nndoc-file-end nil)
(defvoo nndoc-body-begin nil)
(defvoo nndoc-body-end-function nil)
(defvoo nndoc-body-begin-function nil)
(defvoo nndoc-head-begin-function nil)
(defvoo nndoc-body-end nil)
;; nndoc-dissection-alist is a list of sublists.  Each sublist holds the
;; following items.  ARTICLE is an ordinal starting at 1.  HEAD-BEGIN,
;; HEAD-END, BODY-BEGIN and BODY-END are positions in the `nndoc' buffer.
;; LINE-COUNT is a count of lines in the body.  SUBJECT, MESSAGE-ID and
;; REFERENCES, only present for MIME dissections, are field values.
(defvoo nndoc-dissection-alist nil)
(defvoo nndoc-prepare-body-function nil)
(defvoo nndoc-generate-head-function nil)
(defvoo nndoc-article-transform-function nil)
(defvoo nndoc-article-begin-function nil)

(defvoo nndoc-status-string "")
(defvoo nndoc-group-alist nil)
(defvoo nndoc-current-buffer nil
  "Current nndoc news buffer.")
(defvoo nndoc-address nil)
(defvoo nndoc-mime-header nil)
(defvoo nndoc-mime-subject nil)

(defconst nndoc-version "nndoc 1.0"
  "nndoc version.")



;;; Interface functions

(nnoo-define-basics nndoc)

(deffoo nndoc-retrieve-headers (articles &optional newsgroup server fetch-old)
  (when (nndoc-possibly-change-buffer newsgroup server)
    (save-excursion
      (set-buffer nntp-server-buffer)
      (erase-buffer)
      (let (article entry)
	(if (stringp (car articles))
	    'headers
	  (while articles
	    (when (setq entry (cdr (assq (setq article (pop articles))
					 nndoc-dissection-alist)))
	      (insert (format "221 %d Article retrieved.\n" article))
	      (if nndoc-generate-head-function
		  (funcall nndoc-generate-head-function article)
		(insert-buffer-substring
		 nndoc-current-buffer (car entry) (nth 1 entry)))
	      (goto-char (point-max))
	      (unless (= (char-after (1- (point))) ?\n)
		(insert "\n"))
	      (insert (format "Lines: %d\n" (nth 4 entry)))
	      (insert ".\n")))

	  (nnheader-fold-continuation-lines)
	  'headers)))))

(deffoo nndoc-request-article (article &optional newsgroup server buffer)
  (nndoc-possibly-change-buffer newsgroup server)
  (save-excursion
    (let ((buffer (or buffer nntp-server-buffer))
	  (entry (cdr (assq article nndoc-dissection-alist)))
	  beg)
      (set-buffer buffer)
      (erase-buffer)
      (when entry
	(if (stringp article)
	    nil
	  (insert-buffer-substring
	   nndoc-current-buffer (car entry) (nth 1 entry))
	  (insert "\n")
	  (setq beg (point))
	  (insert-buffer-substring
	   nndoc-current-buffer (nth 2 entry) (nth 3 entry))
	  (goto-char beg)
	  (when nndoc-prepare-body-function
	    (funcall nndoc-prepare-body-function))
	  (when nndoc-article-transform-function
	    (funcall nndoc-article-transform-function article))
	  t)))))

(deffoo nndoc-request-group (group &optional server dont-check)
  "Select news GROUP."
  (let (number)
    (cond
     ((not (nndoc-possibly-change-buffer group server))
      (nnheader-report 'nndoc "No such file or buffer: %s"
		       nndoc-address))
     (dont-check
      (nnheader-report 'nndoc "Selected group %s" group)
      t)
     ((zerop (setq number (length nndoc-dissection-alist)))
      (nndoc-close-group group)
      (nnheader-report 'nndoc "No articles in group %s" group))
     (t
      (nnheader-insert "211 %d %d %d %s\n" number 1 number group)))))

(deffoo nndoc-request-type (group &optional article)
  (cond ((not article) 'unknown)
        (nndoc-post-type nndoc-post-type)
        (t 'unknown)))

(deffoo nndoc-close-group (group &optional server)
  (nndoc-possibly-change-buffer group server)
  (and nndoc-current-buffer
       (buffer-name nndoc-current-buffer)
       (kill-buffer nndoc-current-buffer))
  (setq nndoc-group-alist (delq (assoc group nndoc-group-alist)
				nndoc-group-alist))
  (setq nndoc-current-buffer nil)
  (nnoo-close-server 'nndoc server)
  (setq nndoc-dissection-alist nil)
  t)

(deffoo nndoc-request-list (&optional server)
  nil)

(deffoo nndoc-request-newgroups (date &optional server)
  nil)

(deffoo nndoc-request-list-newsgroups (&optional server)
  nil)


;;; Internal functions.

(defun nndoc-possibly-change-buffer (group source)
  (let (buf)
    (cond
     ;; The current buffer is this group's buffer.
     ((and nndoc-current-buffer
	   (buffer-name nndoc-current-buffer)
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
      (push (cons group (setq nndoc-current-buffer
			      (get-buffer-create
			       (concat " *nndoc " group "*"))))
	    nndoc-group-alist)
      (setq nndoc-dissection-alist nil)
      (save-excursion
	(set-buffer nndoc-current-buffer)
	(buffer-disable-undo (current-buffer))
	(erase-buffer)
	(if (stringp nndoc-address)
	    (nnheader-insert-file-contents nndoc-address)
	  (insert-buffer-substring nndoc-address))
	(run-hooks 'nndoc-open-document-hook))))
    ;; Initialize the nndoc structures according to this new document.
    (when (and nndoc-current-buffer
	       (not nndoc-dissection-alist))
      (save-excursion
	(set-buffer nndoc-current-buffer)
	(nndoc-set-delims)
	(if (eq nndoc-article-type 'mime-parts)
	    (nndoc-dissect-mime-parts)
	  (nndoc-dissect-buffer))))
    (unless nndoc-current-buffer
      (nndoc-close-server))
    ;; Return whether we managed to select a file.
    nndoc-current-buffer))

;;;
;;; Deciding what document type we have
;;;

(defun nndoc-set-delims ()
  "Set the nndoc delimiter variables according to the type of the document."
  (let ((vars '(nndoc-file-begin
		nndoc-first-article
		nndoc-article-begin-function
		nndoc-head-begin nndoc-head-end
		nndoc-file-end nndoc-article-begin
		nndoc-body-begin nndoc-body-end-function nndoc-body-end
		nndoc-prepare-body-function nndoc-article-transform-function
		nndoc-generate-head-function nndoc-body-begin-function
		nndoc-head-begin-function)))
    (while vars
      (set (pop vars) nil)))
  (let (defs)
    ;; Guess away until we find the real file type.
    (while (assq 'guess (setq defs (cdr (assq nndoc-article-type
					      nndoc-type-alist))))
      (setq nndoc-article-type (nndoc-guess-type nndoc-article-type)))
    ;; Set the nndoc variables.
    (while defs
      (set (intern (format "nndoc-%s" (caar defs)))
	   (cdr (pop defs))))))

(defun nndoc-guess-type (subtype)
  (let ((alist nndoc-type-alist)
	results result entry)
    (while (and (not result)
		(setq entry (pop alist)))
      (when (memq subtype (or (cdr (assq 'subtype entry)) '(guess)))
	(goto-char (point-min))
	(when (numberp (setq result (funcall (intern
					      (format "nndoc-%s-type-p"
						      (car entry))))))
	  (push (cons result entry) results)
	  (setq result nil))))
    (unless (or result results)
      (error "Document is not of any recognized type"))
    (if result
	(car entry)
      (cadar (sort results 'car-less-than-car)))))

;;;
;;; Built-in type predicates and functions
;;;

(defun nndoc-mbox-type-p ()
  (when (looking-at message-unix-mail-delimiter)
    t))

(defun nndoc-mbox-article-begin ()
  (when (re-search-forward (concat "^" message-unix-mail-delimiter) nil t)
    (goto-char (match-beginning 0))))

(defun nndoc-mbox-body-end ()
  (let ((beg (point))
	len end)
    (when
	(save-excursion
	  (and (re-search-backward
		(concat "^" message-unix-mail-delimiter) nil t)
	       (setq end (point))
	       (search-forward "\n\n" beg t)
	       (re-search-backward
		"^Content-Length:[ \t]*\\([0-9]+\\) *$" end t)
	       (setq len (string-to-int (match-string 1)))
	       (search-forward "\n\n" beg t)
	       (unless (= (setq len (+ (point) len)) (point-max))
		 (and (< len (point-max))
		      (goto-char len)
		      (looking-at message-unix-mail-delimiter)))))
      (goto-char len))))

(defun nndoc-mmdf-type-p ()
  (when (looking-at "\^A\^A\^A\^A$")
    t))

(defun nndoc-news-type-p ()
  (when (looking-at "^Path:.*\n")
    t))

(defun nndoc-rnews-type-p ()
  (when (looking-at "#! *rnews")
    t))

(defun nndoc-rnews-body-end ()
  (and (re-search-backward nndoc-article-begin nil t)
       (forward-line 1)
       (goto-char (+ (point) (string-to-int (match-string 1))))))

(defun nndoc-babyl-type-p ()
  (when (re-search-forward "\^_\^L *\n" nil t)
    t))

(defun nndoc-babyl-body-begin ()
  (re-search-forward "^\n" nil t)
  (when (looking-at "\\*\\*\\* EOOH \\*\\*\\*")
    (let ((next (or (save-excursion
		      (re-search-forward nndoc-article-begin nil t))
		    (point-max))))
      (unless (re-search-forward "^\n" next t)
	(goto-char next)
	(forward-line -1)
	(insert "\n")
	(forward-line -1)))))

(defun nndoc-babyl-head-begin ()
  (when (re-search-forward "^[0-9].*\n" nil t)
    (when (looking-at "\\*\\*\\* EOOH \\*\\*\\*")
      (forward-line 1))
    t))

(defun nndoc-forward-type-p ()
  (when (and (re-search-forward "^-+ Start of forwarded message -+\n+" nil t)
	     (not (re-search-forward "^Subject:.*digest" nil t))
	     (not (re-search-backward "^From:" nil t 2))
	     (not (re-search-forward "^From:" nil t 2)))
    t))

(defun nndoc-rfc934-type-p ()
  (when (and (re-search-forward "^-+ Start of forwarded.*\n+" nil t)
	     (not (re-search-forward "^Subject:.*digest" nil t))
	     (not (re-search-backward "^From:" nil t 2))
	     (not (re-search-forward "^From:" nil t 2)))
    t))

(defun nndoc-rfc822-forward-type-p ()
  (save-restriction
    (message-narrow-to-head)
    (when (re-search-forward "^Content-Type: *message/rfc822" nil t)
      t)))

(defun nndoc-rfc822-forward-body-end-function ()
  (goto-char (point-max)))

(defun nndoc-mime-parts-type-p ()
  (let ((case-fold-search t)
	(limit (search-forward "\n\n" nil t)))
    (goto-char (point-min))
    (when (and limit
		(re-search-forward
		 (concat "\
^Content-Type:[ \t]*multipart/[a-z]+;\\(.*;\\)*"
			 "[ \t\n]*[ \t]boundary=\"?[^\"\n]*[^\" \t\n]")
	   limit t))
      t)))

(defun nndoc-transform-mime-parts (article)
  (unless (= article 1)
    ;; Ensure some MIME-Version.
    (goto-char (point-min))
    (search-forward "\n\n")
    (let ((case-fold-search nil)
	  (limit (point)))
      (goto-char (point-min))
      (or (save-excursion (re-search-forward "^MIME-Version:" limit t))
	  (insert "Mime-Version: 1.0\n")))
    ;; Generate default header before entity fields.
    (goto-char (point-min))
    (nndoc-generate-mime-parts-head article t)))

(defun nndoc-generate-mime-parts-head (article &optional body-present)
  (let ((entry (cdr (assq (if body-present 1 article) nndoc-dissection-alist))))
    (let ((subject (if body-present
		       nndoc-mime-subject
		     (concat "<" (nth 5 entry) ">")))
	  (message-id (nth 6 entry))
	  (references (nth 7 entry)))
      (insert nndoc-mime-header)
      (and subject (insert "Subject: " subject "\n"))
      (and message-id (insert "Message-ID: " message-id "\n"))
      (and references (insert "References: " references "\n")))))

(defun nndoc-clari-briefs-type-p ()
  (when (let ((case-fold-search nil))
	  (re-search-forward "^\t[^a-z]+ ([^a-z]+) --" nil t))
    t))

(defun nndoc-transform-clari-briefs (article)
  (goto-char (point-min))
  (when (looking-at " *\\*\\(.*\\)\n")
    (replace-match "" t t))
  (nndoc-generate-clari-briefs-head article))

(defun nndoc-generate-clari-briefs-head (article)
  (let ((entry (cdr (assq article nndoc-dissection-alist)))
	subject from)
    (save-excursion
      (set-buffer nndoc-current-buffer)
      (save-restriction
	(narrow-to-region (car entry) (nth 3 entry))
	(goto-char (point-min))
	(when (looking-at " *\\*\\(.*\\)$")
	  (setq subject (match-string 1))
	  (when (string-match "[ \t]+$" subject)
	    (setq subject (substring subject 0 (match-beginning 0)))))
	(when
	    (let ((case-fold-search nil))
	      (re-search-forward
	       "^\t\\([^a-z]+\\(,[^(]+\\)? ([^a-z]+)\\) --" nil t))
	  (setq from (match-string 1)))))
    (insert "From: " "clari@clari.net (" (or from "unknown") ")"
	    "\nSubject: " (or subject "(no subject)") "\n")))

(defun nndoc-mime-digest-type-p ()
  (let ((case-fold-search t)
	boundary-id b-delimiter entry)
    (when (and
	   (re-search-forward
	    (concat "^Content-Type: *multipart/digest;[ \t\n]*[ \t]"
		    "boundary=\"?\\([^\"\n]*[^\" \t\n]\\)")
	    nil t)
	   (match-beginning 1))
      (setq boundary-id (match-string 1)
	    b-delimiter (concat "\n--" boundary-id "[\n \t]+"))
      (setq entry (assq 'mime-digest nndoc-type-alist))
      (setcdr entry
	      (list
	       (cons 'head-end "^ ?$")
	       (cons 'body-begin "^ ?\n")
	       (cons 'article-begin b-delimiter)
	       (cons 'body-end-function 'nndoc-digest-body-end)
	       (cons 'file-end (concat "\n--" boundary-id "--[ \t]*$"))))
      t)))

(defun nndoc-standard-digest-type-p ()
  (when (and (re-search-forward (concat "^" (make-string 70 ?-) "\n\n") nil t)
	     (re-search-forward
	      (concat "\n\n" (make-string 30 ?-) "\n\n") nil t))
    t))

(defun nndoc-digest-body-end ()
  (and (re-search-forward nndoc-article-begin nil t)
       (goto-char (match-beginning 0))))

(defun nndoc-slack-digest-type-p ()
  0)

(defun nndoc-lanl-gov-announce-type-p ()
  (when (let ((case-fold-search nil))
	  (re-search-forward "^\\\\\\\\\nPaper: [a-z-]+/[0-9]+" nil t))
    t))

(defun nndoc-transform-lanl-gov-announce (article)
  (goto-char (point-max))
  (when (re-search-backward "^\\\\\\\\ +(\\([^ ]*\\) , *\\([^ ]*\\))" nil t)
    (replace-match "\n\nGet it at \\1 (\\2)" t nil))
  ;;  (when (re-search-backward "^\\\\\\\\$" nil t)
  ;;    (replace-match "" t t))
  )

(defun nndoc-generate-lanl-gov-head (article)
  (let ((entry (cdr (assq article nndoc-dissection-alist)))
 	(e-mail "no address given")
 	subject from)
    (save-excursion
      (set-buffer nndoc-current-buffer)
      (save-restriction
 	(narrow-to-region (car entry) (nth 1 entry))
 	(goto-char (point-min))
 	(when (looking-at "^Paper.*: \\([a-z-]+/[0-9]+\\)")
 	  (setq subject (concat " (" (match-string 1) ")"))
 	  (when (re-search-forward "^From: \\([^ ]+\\)" nil t)
 	    (setq e-mail (match-string 1)))
 	  (when (re-search-forward "^Title: \\([^\f]*\\)\nAuthors?: \\(.*\\)"
 				   nil t)
 	    (setq subject (concat (match-string 1) subject))
 	    (setq from (concat (match-string 2) " <" e-mail ">"))))
 	))
    (while (and from (string-match "(\[^)\]*)" from))
      (setq from (replace-match "" t t from)))
    (insert "From: "  (or from "unknown")
 	    "\nSubject: " (or subject "(no subject)") "\n")))

(deffoo nndoc-request-accept-article (group &optional server last)
  nil)



;;;
;;; Functions for dissecting the documents
;;;

(defun nndoc-search (regexp)
  (prog1
      (re-search-forward regexp nil t)
    (beginning-of-line)))

(defun nndoc-dissect-buffer ()
  "Go through the document and partition it into heads/bodies/articles."
  (let ((i 0)
	(first t)
	head-begin head-end body-begin body-end)
    (setq nndoc-dissection-alist nil)
    (save-excursion
      (set-buffer nndoc-current-buffer)
      (goto-char (point-min))
      ;; Find the beginning of the file.
      (when nndoc-file-begin
	(nndoc-search nndoc-file-begin))
      ;; Go through the file.
      (while (if (and first nndoc-first-article)
		 (nndoc-search nndoc-first-article)
	       (nndoc-article-begin))
	(setq first nil)
	(cond (nndoc-head-begin-function
	       (funcall nndoc-head-begin-function))
	      (nndoc-head-begin
	       (nndoc-search nndoc-head-begin)))
 	(if (or (eobp)
		(and nndoc-file-end
		     (looking-at nndoc-file-end)))
	    (goto-char (point-max))
	  (setq head-begin (point))
	  (nndoc-search (or nndoc-head-end "^$"))
	  (setq head-end (point))
	  (if nndoc-body-begin-function
	      (funcall nndoc-body-begin-function)
	    (nndoc-search (or nndoc-body-begin "^\n")))
	  (setq body-begin (point))
	  (or (and nndoc-body-end-function
		   (funcall nndoc-body-end-function))
	      (and nndoc-body-end
		   (nndoc-search nndoc-body-end))
	      (nndoc-article-begin)
	      (progn
		(goto-char (point-max))
		(when nndoc-file-end
		  (and (re-search-backward nndoc-file-end nil t)
		       (beginning-of-line)))))
	  (setq body-end (point))
	  (push (list (incf i) head-begin head-end body-begin body-end
		      (count-lines body-begin body-end))
		nndoc-dissection-alist))))))

(defun nndoc-article-begin ()
  (if nndoc-article-begin-function
      (funcall nndoc-article-begin-function)
    (ignore-errors
      (nndoc-search nndoc-article-begin))))

(defun nndoc-unquote-dashes ()
  "Unquote quoted non-separators in digests."
  (while (re-search-forward "^- -"nil t)
    (replace-match "-" t t)))

;; Against compiler warnings.
(defvar nndoc-mime-split-ordinal)

(defun nndoc-dissect-mime-parts ()
  "Go through a MIME composite article and partition it into sub-articles.
When a MIME entity contains sub-entities, dissection produces one article for
the header of this entity, and one article per sub-entity."
  (setq nndoc-dissection-alist nil
	nndoc-mime-split-ordinal 0)
  (save-excursion
    (set-buffer nndoc-current-buffer)
    (message-narrow-to-head)
    (let ((case-fold-search t)
	  (message-id (message-fetch-field "Message-ID"))
	  (references (message-fetch-field "References")))
      (setq nndoc-mime-header (buffer-substring (point-min) (point-max))
	    nndoc-mime-subject (message-fetch-field "Subject"))
      (while (string-match "\
^\\(Subject\\|Message-ID\\|References\\|Lines\\|\
MIME-Version\\|Content-Type\\|Content-Transfer-Encoding\\|\
\\):.*\n\\([ \t].*\n\\)*"
			   nndoc-mime-header)
	(setq nndoc-mime-header (replace-match "" t t nndoc-mime-header)))
      (widen)
      (nndoc-dissect-mime-parts-sub (point-min) (point-max)
				    nil message-id references))))

(defun nndoc-dissect-mime-parts-sub (begin end position message-id references)
  "Dissect an entity within a composite MIME message.
The article, which corresponds to a MIME entity, extends from BEGIN to END.
The string POSITION holds a dotted decimal representation of the article
position in the hierarchical structure, it is nil for the outer entity.
The generated article should use MESSAGE-ID and REFERENCES field values."
  ;; Note: `case-fold-search' is already `t' from the calling function.
  (let ((head-begin begin)
	(body-end end)
	head-end body-begin type subtype composite comment)
    (save-excursion
      ;; Gracefully handle a missing body.
      (goto-char head-begin)
      (if (search-forward "\n\n" body-end t)
	  (setq head-end (1- (point))
		body-begin (point))
	(setq head-end end
	      body-begin end))
      ;; Save MIME attributes.
      (goto-char head-begin)
      (if (re-search-forward "\
^Content-Type: *\\([^ \t\n/;]+\\)/\\([^ \t\n/;]+\\)"
			     head-end t)
	  (setq type (downcase (match-string 1))
		subtype (downcase (match-string 2)))
	(setq type "text"
	      subtype "plain"))
      (setq composite (string= type "multipart")
	    comment (concat position
			    (when (and position composite) ".")
			    (when composite "*")
			    (when (or position composite) " ")
			    (cond ((string= subtype "plain") type)
				  ((string= subtype "basic") type)
				  (t subtype))))
      ;; Generate dissection information for this entity.
      (push (list (incf nndoc-mime-split-ordinal)
		  head-begin head-end body-begin body-end
		  (count-lines body-begin body-end)
		  comment message-id references)
	    nndoc-dissection-alist)
      ;; Recurse for all sub-entities, if any.
      (goto-char head-begin)
      (when (re-search-forward
	     (concat "\
^Content-Type: *multipart/\\([a-z]+\\);\\(.*;\\)*"
		     "[ \t\n]*[ \t]boundary=\"?\\([^\"\n]*[^\" \t\n]\\)")
	   head-end t)
	(let ((boundary (concat "\n--" (match-string 3) "\\(--\\)?[ \t]*\n"))
	      (part-counter 0)
	      begin end eof-flag)
	  (goto-char head-end)
	  (setq eof-flag (not (re-search-forward boundary body-end t)))
	  (while (not eof-flag)
	    (setq begin (point))
	    (cond ((re-search-forward boundary body-end t)
		   (or (not (match-string 1))
		       (string= (match-string 1) "")
		       (setq eof-flag t))
		   (forward-line -1)
		   (setq end (point))
		   (forward-line 1))
		  (t (setq end body-end
			   eof-flag t)))
	    (nndoc-dissect-mime-parts-sub begin end
					  (concat position (when position ".")
						  (format "%d"
							  (incf part-counter)))
					  (nnmail-message-id)
					  message-id)))))))

;;;###autoload
(defun nndoc-add-type (definition &optional position)
  "Add document DEFINITION to the list of nndoc document definitions.
If POSITION is nil or `last', the definition will be added
as the last checked definition, if t or `first', add as the
first definition, and if any other symbol, add after that
symbol in the alist."
  ;; First remove any old instances.
  (gnus-pull (car definition) nndoc-type-alist)
  ;; Then enter the new definition in the proper place.
  (cond
   ((or (null position) (eq position 'last))
    (setq nndoc-type-alist (nconc nndoc-type-alist (list definition))))
   ((or (eq position t) (eq position 'first))
    (push definition nndoc-type-alist))
   (t
    (let ((list (memq (assq position nndoc-type-alist)
		      nndoc-type-alist)))
      (unless list
	(error "No such position: %s" position))
      (setcdr list (cons definition (cdr list)))))))

(provide 'nndoc)

;;; nndoc.el ends here
