;;; nnspool.el --- spool access using NNTP for GNU Emacs

;; Copyright (C) 1988, 1989 Fujitsu Laboratories LTD.
;; Copyright (C) 1988, 1989, 1990 Masanobu UMEDA

;; Author: Masanobu UMEDA <umerin@flab.flab.fujitsu.junet>
;; Keywords: news

;; $Header: nnspool.el,v 1.10 90/03/23 13:25:25 umerin Locked $

;; This file is part of GNU Emacs.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY.  No author or distributor
;; accepts responsibility to anyone for the consequences of using it
;; or for whether it serves any particular purpose or works at all,
;; unless he says so in writing.  Refer to the GNU Emacs General Public
;; License for full details.

;; Everyone is granted permission to copy, modify and redistribute
;; GNU Emacs, but only under the conditions described in the
;; GNU Emacs General Public License.   A copy of this license is
;; supposed to have been given to you along with GNU Emacs so you
;; can know your rights and responsibilities.  It should be in a
;; file named COPYING.  Among other things, the copyright notice
;; and this notice must be preserved on all copies.

;;; Code:

(require 'nntp)

(defvar nnspool-inews-program news-inews-program
  "*Program to post news.")

(defvar nnspool-inews-switches '("-h")
  "*Switches for nnspool-request-post to pass to `inews' for posting news.")

(defvar nnspool-spool-directory news-path
  "*Local news spool directory.")

(defvar nnspool-active-file "/usr/lib/news/active"
  "*Local news active file.")

(defvar nnspool-history-file "/usr/lib/news/history"
  "*Local news history file.")



(defconst nnspool-version "NNSPOOL 1.10"
  "Version numbers of this version of NNSPOOL.")

(defvar nnspool-current-directory nil
  "Current news group directory.")

;;;
;;; Replacement of Extended Command for retrieving many headers.
;;;

(defun nnspool-retrieve-headers (sequence)
  "Return list of article headers specified by SEQUENCE of article id.
The format of list is
 `([NUMBER SUBJECT FROM XREF LINES DATE MESSAGE-ID REFERENCES] ...)'.
Reader macros for the vector are defined as `nntp-header-FIELD'.
Writer macros for the vector are defined as `nntp-set-header-FIELD'.
News group must be selected before calling me."
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
	      (concat nnspool-current-directory (prin1-to-string article)))
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
		(setq lines 0))
	      ;; Extract Xref:
	      (goto-char (point-min))
	      (if (search-forward "\nXref: " nil t)
		  (setq xref (buffer-substring
			      (point)
			      (save-excursion (end-of-line) (point))))
		(setq xref nil))
	      ;; Extract References:
	      (goto-char (point-min))
	      (if (search-forward "\nReferences: " nil t)
		  (setq references (buffer-substring
				    (point)
				    (save-excursion (end-of-line) (point))))
		(setq references nil))
	      (setq headers
		    (cons (vector article subject from
				  xref lines date
				  message-id references) headers))
	      ))
	(setq sequence (cdr sequence))
	(setq count (1+ count))
	(and (numberp nntp-large-newsgroup)
	     (> number nntp-large-newsgroup)
	     (zerop (% count 20))
	     (message "NNSPOOL: %d%% of headers received."
		      (/ (* count 100) number)))
	)
      (and (numberp nntp-large-newsgroup)
	   (> number nntp-large-newsgroup)
	   (message "NNSPOOL: 100%% of headers received."))
      (nreverse headers)
      )))


;;;
;;; Replacement of NNTP Raw Interface.
;;;

(defun nnspool-open-server (host &optional service)
  "Open news server on HOST.
If HOST is nil, use value of environment variable `NNTPSERVER'.
If optional argument SERVICE is non-nil, open by the service name."
  (let ((host (or host (getenv "NNTPSERVER")))
	(status nil))
    (setq nntp-status-message-string "")
    (cond ((and (file-directory-p nnspool-spool-directory)
		(file-exists-p nnspool-active-file)
		(string-equal host (system-name)))
	   (setq status (nnspool-open-server-internal host service)))
	  ((string-equal host (system-name))
	   (setq nntp-status-message-string
		 (format "%s has no news spool.  Goodbye." host)))
	  ((null host)
	   (setq nntp-status-message-string "NNTP server is not specified."))
	  (t
	   (setq nntp-status-message-string
		 (format "NNSPOOL: cannot talk to %s." host)))
	  )
    status
    ))

(defun nnspool-close-server ()
  "Close news server."
  (nnspool-close-server-internal))

(fset 'nnspool-request-quit (symbol-function 'nnspool-close-server))

(defun nnspool-server-opened ()
  "Return server process status, T or NIL.
If the stream is opened, return T, otherwise return NIL."
  (and nntp-server-buffer
       (get-buffer nntp-server-buffer)))

(defun nnspool-status-message ()
  "Return server status response as string."
  nntp-status-message-string
  )

(defun nnspool-request-article (id)
  "Select article by message ID (or number)."
  (let ((file (if (stringp id)
		  (nnspool-find-article-by-message-id id)
		(concat nnspool-current-directory (prin1-to-string id)))))
    (if (and (stringp file)
	     (file-exists-p file)
	     (not (file-directory-p file)))
	(save-excursion
	  (nnspool-find-file file)))
    ))

(defun nnspool-request-body (id)
  "Select article body by message ID (or number)."
  (if (nnspool-request-article id)
      (save-excursion
	(set-buffer nntp-server-buffer)
	(goto-char (point-min))
	(if (search-forward "\n\n" nil t)
	    (delete-region (point-min) (point)))
	t
	)
    ))

(defun nnspool-request-head (id)
  "Select article head by message ID (or number)."
  (if (nnspool-request-article id)
      (save-excursion
	(set-buffer nntp-server-buffer)
	(goto-char (point-min))
	(if (search-forward "\n\n" nil t)
	    (delete-region (1- (point)) (point-max)))
	t
	)
    ))

(defun nnspool-request-stat (id)
  "Select article by message ID (or number)."
  (error "NNSPOOL: STAT is not implemented."))

(defun nnspool-request-group (group)
  "Select news GROUP."
  (let ((pathname (nnspool-article-pathname
		   (nnspool-replace-chars-in-string group ?. ?/))))
    (if (file-directory-p pathname)
	(setq nnspool-current-directory pathname))
    ))

(defun nnspool-request-list ()
  "List valid newsgoups."
  (save-excursion
    (nnspool-find-file nnspool-active-file)))

(defun nnspool-request-last ()
  "Set current article pointer to the previous article in the current news group."
  (error "NNSPOOL: LAST is not implemented."))

(defun nnspool-request-next ()
  "Advance current article pointer."
  (error "NNSPOOL: NEXT is not implemented."))

(defun nnspool-request-post ()
  "Post a new news in current buffer."
  (save-excursion
    ;; We have to work in the server buffer because of NEmacs hack.
    (copy-to-buffer nntp-server-buffer (point-min) (point-max))
    (set-buffer nntp-server-buffer)
    (apply 'call-process-region
	   (point-min) (point-max)
	   nnspool-inews-program 'delete t nil nnspool-inews-switches)
    (prog1
	(or (zerop (buffer-size))
	    ;; If inews returns strings, it must be error message 
	    ;;  unless SPOOLNEWS is defined.  
	    ;; This condition is very weak, but there is no good rule 
	    ;;  identifying errors when SPOOLNEWS is defined.  
	    ;; Suggested by ohm@kaba.junet.
	    (string-match "spooled" (buffer-string)))
      ;; Make status message by unfolding lines.
      (subst-char-in-region (point-min) (point-max) ?\n ?\\ 'noundo)
      (setq nntp-status-message-string (buffer-string))
      (erase-buffer))
    ))


;;;
;;; Replacement of Low-Level Interface to NNTP Server.
;;; 

(defun nnspool-open-server-internal (host &optional service)
  "Open connection to news server on HOST by SERVICE (default is nntp)."
  (save-excursion
    (if (not (string-equal host (system-name)))
	(error "NNSPOOL: cannot talk to %s." host))
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

(defun nnspool-close-server-internal ()
  "Close connection to news server."
  (if (get-file-buffer nnspool-history-file)
      (kill-buffer (get-file-buffer nnspool-history-file)))
  (if nntp-server-buffer
      (kill-buffer nntp-server-buffer))
  (setq nntp-server-buffer nil)
  (setq nntp-server-process nil))

(defun nnspool-find-article-by-message-id (id)
  "Return full pathname of an article identified by message-ID."
  (save-excursion
    (let ((buffer (get-file-buffer nnspool-history-file)))
      (if buffer
	  (set-buffer buffer)
	;; Finding history file may take lots of time.
	(message "Reading history file...")
	(set-buffer (find-file-noselect nnspool-history-file))
	(message "Reading history file... done")))
    ;; Search from end of the file. I think this is much faster than
    ;; do from the beginning of the file.
    (goto-char (point-max))
    (if (re-search-backward
	 (concat "^" (regexp-quote id)
		 "[ \t].*[ \t]\\([^ \t/]+\\)/\\([0-9]+\\)[ \t]*$") nil t)
	(let ((group (buffer-substring (match-beginning 1) (match-end 1)))
	      (number (buffer-substring (match-beginning 2) (match-end 2))))
	  (concat (nnspool-article-pathname
		   (nnspool-replace-chars-in-string group ?. ?/))
		  number))
      )))

(defun nnspool-find-file (file)
  "Insert FILE in server buffer safely."
  (set-buffer nntp-server-buffer)
  (erase-buffer)
  (condition-case ()
      (progn (insert-file-contents file) t)
    (file-error nil)
    ))

(defun nnspool-article-pathname (group)
  "Make pathname for GROUP."
  (concat (file-name-as-directory nnspool-spool-directory) group "/"))

(defun nnspool-replace-chars-in-string (string from to)
  "Replace characters in STRING from FROM to TO."
  (let ((string (substring string 0))	;Copy string.
	(len (length string))
	(idx 0))
    ;; Replace all occurence of FROM with TO.
    (while (< idx len)
      (if (= (aref string idx) from)
	  (aset string idx to))
      (setq idx (1+ idx)))
    string
    ))

(provide 'nnspool)

;;; nnspool.el ends here
