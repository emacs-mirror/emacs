;;; mhspool.el --- MH folder access using NNTP for GNU Emacs

;; Copyright (C) 1988, 1989, 1990, 1993 Free Software Foundation, Inc.

;; Author: Masanobu UMEDA <umerin@flab.flab.fujitsu.junet>
;; Maintainer: FSF
;; Keywords: mail, news

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
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; This package enables you to read mail or articles in MH folders, or
;; articles saved by GNUS. In any case, the file names of mail or
;; articles must consist of only numeric letters.

;; Before using this package, you have to create a server specific
;; startup file according to the directory which you want to read. For
;; example, if you want to read mail under the directory named
;; `~/Mail', the file must be a file named `.newsrc-:Mail'. (There is
;; no way to specify hierarchical directory now.) In this case, the
;; name of the NNTP server passed to GNUS must be `:Mail'.

;;; Code:

(require 'nntp)

(defvar mhspool-list-folders-method
  (function mhspool-list-folders-using-sh)
  "*Function to list files in folders.
The function should accept a directory as its argument, and fill the
current buffer with file and directory names.  The output format must
be the same as that of 'ls -R1'.  Two functions
mhspool-list-folders-using-ls and mhspool-list-folders-using-sh are
provided now.  I suppose the later is faster.")

(defvar mhspool-list-directory-switches '("-R")
  "*Switches for mhspool-list-folders-using-ls to pass to `ls' for getting file lists.
One entry should appear on one line. You may need to add `-1' option.")



(defconst mhspool-version "MHSPOOL 1.8"
  "Version numbers of this version of MHSPOOL.")

(defvar mhspool-spool-directory "~/Mail"
  "Private mail directory.")

(defvar mhspool-current-directory nil
  "Current news group directory.")

;;;
;;; Replacement of Extended Command for retrieving many headers.
;;;

(defun mhspool-retrieve-headers (sequence)
  "Return list of article headers specified by SEQUENCE of article id.
The format of list is
 `([NUMBER SUBJECT FROM XREF LINES DATE MESSAGE-ID REFERENCES] ...)'.
If there is no References: field, In-Reply-To: field is used instead.
Reader macros for the vector are defined as `nntp-header-FIELD'.
Writer macros for the vector are defined as `nntp-set-header-FIELD'.
Newsgroup must be selected before calling this."
  (save-excursion
    (set-buffer nntp-server-buffer)
    ;;(erase-buffer)
    (let ((file nil)
	  (number (length sequence))
	  (count 0)
	  (headers nil)			;Result list.
	  (article 0)
	  (subject nil)
	  (message-id nil)
	  (from nil)
	  (xref nil)
	  (lines 0)
	  (date nil)
	  (references nil))
      (while sequence
	;;(nntp-send-strings-to-server "HEAD" (car sequence))
	(setq article (car sequence))
	(setq file
	      (concat mhspool-current-directory (prin1-to-string article)))
	(if (and (file-exists-p file)
		 (not (file-directory-p file)))
	    (progn
	      (erase-buffer)
	      (insert-file-contents file)
	      ;; Make message body invisible.
	      (goto-char (point-min))
	      (search-forward "\n\n" nil 'move)
	      (narrow-to-region (point-min) (point))
	      ;; Fold continuation lines.
	      (goto-char (point-min))
	      (while (re-search-forward "\\(\r?\n[ \t]+\\)+" nil t)
		(replace-match " " t t))
	      ;; Make it possible to search for `\nFIELD'.
	      (goto-char (point-min))
	      (insert "\n")
	      ;; Extract From:
	      (goto-char (point-min))
	      (if (search-forward "\nFrom: " nil t)
		  (setq from (buffer-substring
			      (point)
			      (save-excursion (end-of-line) (point))))
		(setq from "(Unknown User)"))
	      ;; Extract Subject:
	      (goto-char (point-min))
	      (if (search-forward "\nSubject: " nil t)
		  (setq subject (buffer-substring
				 (point)
				 (save-excursion (end-of-line) (point))))
		(setq subject "(None)"))
	      ;; Extract Message-ID:
	      (goto-char (point-min))
	      (if (search-forward "\nMessage-ID: " nil t)
		  (setq message-id (buffer-substring
				    (point)
				    (save-excursion (end-of-line) (point))))
		(setq message-id nil))
	      ;; Extract Date:
	      (goto-char (point-min))
	      (if (search-forward "\nDate: " nil t)
		  (setq date (buffer-substring
			      (point)
			      (save-excursion (end-of-line) (point))))
		(setq date nil))
	      ;; Extract Lines:
	      (goto-char (point-min))
	      (if (search-forward "\nLines: " nil t)
		  (setq lines (string-to-int
			       (buffer-substring
				(point)
				(save-excursion (end-of-line) (point)))))
		;; Count lines since there is no lines field in most cases.
		(setq lines
		      (save-restriction
			(goto-char (point-max))
			(widen)
			(count-lines (point) (point-max)))))
	      ;; Extract Xref:
	      (goto-char (point-min))
	      (if (search-forward "\nXref: " nil t)
		  (setq xref (buffer-substring
			      (point)
			      (save-excursion (end-of-line) (point))))
		(setq xref nil))
	      ;; Extract References:
	      ;; If no References: field, use In-Reply-To: field instead.
	      ;; Suggested by tanaka@flab.fujitsu.co.jp (Hiroshi TANAKA).
	      (goto-char (point-min))
	      (if (or (search-forward "\nReferences: " nil t)
		      (search-forward "\nIn-Reply-To: " nil t))
		  (setq references (buffer-substring
				    (point)
				    (save-excursion (end-of-line) (point))))
		(setq references nil))
	      ;; Collect valid article only.
	      (and article
		   message-id
		   (setq headers
			 (cons (vector article subject from
				       xref lines date
				       message-id references) headers)))
	      ))
	(setq sequence (cdr sequence))
	(setq count (1+ count))
	(and (numberp nntp-large-newsgroup)
	     (> number nntp-large-newsgroup)
	     (zerop (% count 20))
	     (message "MHSPOOL: Receiving headers... %d%%"
		      (/ (* count 100) number)))
	)
      (and (numberp nntp-large-newsgroup)
	   (> number nntp-large-newsgroup)
	   (message "MHSPOOL: Receiving headers... done"))
      (nreverse headers)
      )))


;;;
;;; Replacement of NNTP Raw Interface.
;;;

(defun mhspool-open-server (host &optional service)
  "Open news server on HOST.
If HOST is nil, use value of environment variable `NNTPSERVER'.
If optional argument SERVICE is non-nil, open by the service name."
  (let ((host (or host (getenv "NNTPSERVER")))
	(status nil))
    ;; Get directory name from HOST name.
    (if (string-match ":\\(.+\\)$" host)
	(progn
	  (setq mhspool-spool-directory
		(file-name-as-directory
		 (expand-file-name
		  (substring host (match-beginning 1) (match-end 1))
		  (expand-file-name "~/" nil))))
	  (setq host (system-name)))
      (setq mhspool-spool-directory nil))
    (setq nntp-status-string "")
    (cond ((and (stringp host)
		(stringp mhspool-spool-directory)
		(file-directory-p mhspool-spool-directory)
		(string-equal host (system-name)))
	   (setq status (mhspool-open-server-internal host service)))
	  ((string-equal host (system-name))
	   (setq nntp-status-string
		 (format "No such directory: %s.  Goodbye."
			 mhspool-spool-directory)))
	  ((null host)
	   (setq nntp-status-string "NNTP server is not specified."))
	  (t
	   (setq nntp-status-string
		 (format "MHSPOOL: cannot talk to %s." host)))
	  )
    status
    ))

(defun mhspool-close-server ()
  "Close news server."
  (mhspool-close-server-internal))

(fset 'mhspool-request-quit (symbol-function 'mhspool-close-server))

(defun mhspool-server-opened ()
  "Return server process status, T or NIL.
If the stream is opened, return T, otherwise return NIL."
  (and nntp-server-buffer
       (get-buffer nntp-server-buffer)))

(defun mhspool-status-message ()
  "Return server status response as string."
  nntp-status-string
  )

(defun mhspool-request-article (id)
  "Select article by message ID (or number)."
  (let ((file (concat mhspool-current-directory (prin1-to-string id))))
    (if (and (stringp file)
	     (file-exists-p file)
	     (not (file-directory-p file)))
	(save-excursion
	  (mhspool-find-file file)))
    ))

(defun mhspool-request-body (id)
  "Select article body by message ID (or number)."
  (if (mhspool-request-article id)
      (save-excursion
	(set-buffer nntp-server-buffer)
	(goto-char (point-min))
	(if (search-forward "\n\n" nil t)
	    (delete-region (point-min) (point)))
	t
	)
    ))

(defun mhspool-request-head (id)
  "Select article head by message ID (or number)."
  (if (mhspool-request-article id)
      (save-excursion
	(set-buffer nntp-server-buffer)
	(goto-char (point-min))
	(if (search-forward "\n\n" nil t)
	    (delete-region (1- (point)) (point-max)))
	t
	)
    ))

(defun mhspool-request-stat (id)
  "Select article by message ID (or number)."
  (setq nntp-status-string "MHSPOOL: STAT is not implemented.")
  nil
  )

(defun mhspool-request-group (group)
  "Select news GROUP."
  (cond ((file-directory-p
	  (mhspool-article-pathname group))
	 ;; Mail/NEWS.GROUP/N
	 (setq mhspool-current-directory
	       (mhspool-article-pathname group)))
	((file-directory-p
	  (mhspool-article-pathname
	   (mhspool-replace-chars-in-string group ?. ?/)))
	 ;; Mail/NEWS/GROUP/N
	 (setq mhspool-current-directory
	       (mhspool-article-pathname
		(mhspool-replace-chars-in-string group ?. ?/))))
	))

(defun mhspool-request-list ()
  "List active newsgoups."
  (save-excursion
    (let* ((newsgroup nil)
	   (articles nil)
	   (directory (file-name-as-directory
		       (expand-file-name mhspool-spool-directory nil)))
	   (folder-regexp (concat "^" (regexp-quote directory) "\\(.+\\):$"))
	   (buffer (get-buffer-create " *MHSPOOL File List*")))
      (set-buffer nntp-server-buffer)
      (erase-buffer)
      (set-buffer buffer)
      (erase-buffer)
;;      (apply 'call-process
;;	     "ls" nil t nil
;;	     (append mhspool-list-directory-switches (list directory)))
      (funcall mhspool-list-folders-method directory)
      (goto-char (point-min))
      (while (re-search-forward folder-regexp nil t)
	(setq newsgroup
	      (mhspool-replace-chars-in-string
	       (buffer-substring (match-beginning 1) (match-end 1)) ?/ ?.))
	(setq articles nil)
	(forward-line 1)		;(beginning-of-line)
	;; Thank nobu@flab.fujitsu.junet for his bug fixes.
	(while (and (not (eobp))
		    (not (looking-at "^$")))
	  (if (looking-at "^[0-9]+$")
	      (setq articles
		    (cons (string-to-int
			   (buffer-substring
			    (match-beginning 0) (match-end 0)))
			  articles)))
	  (forward-line 1))
	(if articles
	    (princ (format "%s %d %d n\n" newsgroup
			   (apply (function max) articles)
			   (apply (function min) articles))
		   nntp-server-buffer))
	)
      (kill-buffer buffer)
      (set-buffer nntp-server-buffer)
      (buffer-size)
      )))

(defun mhspool-request-list-newsgroups ()
  "List newsgoups (defined in NNTP2)."
  (setq nntp-status-string "MHSPOOL: LIST NEWSGROUPS is not implemented.")
  nil
  )

(defun mhspool-request-list-distributions ()
  "List distributions (defined in NNTP2)."
  (setq nntp-status-string "MHSPOOL: LIST DISTRIBUTIONS is not implemented.")
  nil
  )

(defun mhspool-request-last ()
  "Set current article pointer to the previous article
in the current news group."
  (setq nntp-status-string "MHSPOOL: LAST is not implemented.")
  nil
  )

(defun mhspool-request-next ()
  "Advance current article pointer."
  (setq nntp-status-string "MHSPOOL: NEXT is not implemented.")
  nil
  )

(defun mhspool-request-post ()
  "Post a new news in current buffer."
  (setq nntp-status-string "MHSPOOL: POST: what do you mean?")
  nil
  )


;;;
;;; Replacement of Low-Level Interface to NNTP Server.
;;; 

(defun mhspool-open-server-internal (host &optional service)
  "Open connection to news server on HOST by SERVICE (default is nntp)."
  (save-excursion
    (if (not (string-equal host (system-name)))
	(error "MHSPOOL: cannot talk to %s." host))
    ;; Initialize communication buffer.
    (setq nntp-server-buffer (get-buffer-create " *nntpd*"))
    (set-buffer nntp-server-buffer)
    (buffer-flush-undo (current-buffer))
    (erase-buffer)
    (kill-all-local-variables)
    (setq case-fold-search t)		;Should ignore case.
    (setq nntp-server-process nil)
    (setq nntp-server-name host)
    ;; It is possible to change kanji-fileio-code in this hook.
    (run-hooks 'nntp-server-hook)
    t
    ))

(defun mhspool-close-server-internal ()
  "Close connection to news server."
  (if nntp-server-buffer
      (kill-buffer nntp-server-buffer))
  (setq nntp-server-buffer nil)
  (setq nntp-server-process nil))

(defun mhspool-find-file (file)
  "Insert FILE in server buffer safely."
  (set-buffer nntp-server-buffer)
  (erase-buffer)
  (condition-case ()
      (progn
	(insert-file-contents file)
	(goto-char (point-min))
	;; If there is no body, `^L' appears at end of file. Special
	;; hack for MH folder.
	(and (search-forward "\n\n" nil t)
	     (string-equal (buffer-substring (point) (point-max)) "\^L")
	     (delete-char 1))
	t
	)
    (file-error nil)
    ))

(defun mhspool-article-pathname (group)
  "Make pathname for GROUP."
  (concat (file-name-as-directory mhspool-spool-directory) group "/"))

(defun mhspool-replace-chars-in-string (string from to)
  "Replace characters in STRING from FROM to TO."
  (let ((string (substring string 0))	;Copy string.
	(len (length string))
	(idx 0))
    ;; Replace all occurrences of FROM with TO.
    (while (< idx len)
      (if (= (aref string idx) from)
	  (aset string idx to))
      (setq idx (1+ idx)))
    string
    ))


;; Methods for listing files in folders.

(defun mhspool-list-folders-using-ls (directory)
  "List files in folders under DIRECTORY using 'ls'."
  (apply 'call-process
	 "ls" nil t nil
	 (append mhspool-list-directory-switches (list directory))))

;; Basic ideas by tanaka@flab.fujitsu.co.jp (Hiroshi TANAKA)

(defun mhspool-list-folders-using-sh (directory)
  "List files in folders under DIRECTORY using '/bin/sh'."
  (let ((buffer (current-buffer))
	(script (get-buffer-create " *MHSPOOL Shell Script Buffer*")))
    (save-excursion
      (save-restriction
	(set-buffer script)
	(erase-buffer)
	;; /bin/sh script which does 'ls -R'.
	(insert
	 "PS2=
          ffind() {
		cd $1; echo $1:
		ls -1
		echo
		for j in `echo *[a-zA-Z]*`
		do
		  if [ -d $1/$j ]; then
			ffind $1/$j
		  fi
		done
	  }
	  cd " directory "; ffind `pwd`; exit 0\n")
	(call-process-region (point-min) (point-max) "sh" nil buffer nil)
	))
    (kill-buffer script)
    ))

(provide 'mhspool)

;;; mhspool.el ends here
