;;; gnus-int.el --- backend interface functions for Gnus
;; Copyright (C) 1996,97 Free Software Foundation, Inc.

;; Author: Lars Magne Ingebrigtsen <larsi@ifi.uio.no>
;; Keywords: news

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

(require 'gnus)

(defcustom gnus-open-server-hook nil
  "Hook called just before opening connection to the news server."
  :group 'gnus-start
  :type 'hook)

;;;
;;; Server Communication
;;;

(defun gnus-start-news-server (&optional confirm)
  "Open a method for getting news.
If CONFIRM is non-nil, the user will be asked for an NNTP server."
  (let (how)
    (if gnus-current-select-method
	;; Stream is already opened.
	nil
      ;; Open NNTP server.
      (unless gnus-nntp-service
	(setq gnus-nntp-server nil))
      (when confirm
	;; Read server name with completion.
	(setq gnus-nntp-server
	      (completing-read "NNTP server: "
			       (mapcar (lambda (server) (list server))
				       (cons (list gnus-nntp-server)
					     gnus-secondary-servers))
			       nil nil gnus-nntp-server)))

      (when (and gnus-nntp-server
		 (stringp gnus-nntp-server)
		 (not (string= gnus-nntp-server "")))
	(setq gnus-select-method
	      (cond ((or (string= gnus-nntp-server "")
			 (string= gnus-nntp-server "::"))
		     (list 'nnspool (system-name)))
		    ((string-match "^:" gnus-nntp-server)
		     (list 'nnmh gnus-nntp-server
			   (list 'nnmh-directory
				 (file-name-as-directory
				  (expand-file-name
				   (concat "~/" (substring
						 gnus-nntp-server 1)))))
			   (list 'nnmh-get-new-mail nil)))
		    (t
		     (list 'nntp gnus-nntp-server)))))

      (setq how (car gnus-select-method))
      (cond
       ((eq how 'nnspool)
	(require 'nnspool)
	(gnus-message 5 "Looking up local news spool..."))
       ((eq how 'nnmh)
	(require 'nnmh)
	(gnus-message 5 "Looking up mh spool..."))
       (t
	(require 'nntp)))
      (setq gnus-current-select-method gnus-select-method)
      (run-hooks 'gnus-open-server-hook)
      (or
       ;; gnus-open-server-hook might have opened it
       (gnus-server-opened gnus-select-method)
       (gnus-open-server gnus-select-method)
       (gnus-y-or-n-p
	(format
	 "%s (%s) open error: '%s'.  Continue? "
	 (car gnus-select-method) (cadr gnus-select-method)
	 (gnus-status-message gnus-select-method)))
       (gnus-error 1 "Couldn't open server on %s"
		   (nth 1 gnus-select-method))))))

(defun gnus-check-group (group)
  "Try to make sure that the server where GROUP exists is alive."
  (let ((method (gnus-find-method-for-group group)))
    (or (gnus-server-opened method)
	(gnus-open-server method))))

(defun gnus-check-server (&optional method silent)
  "Check whether the connection to METHOD is down.
If METHOD is nil, use `gnus-select-method'.
If it is down, start it up (again)."
  (let ((method (or method gnus-select-method)))
    ;; Transform virtual server names into select methods.
    (when (stringp method)
      (setq method (gnus-server-to-method method)))
    (if (gnus-server-opened method)
	;; The stream is already opened.
	t
      ;; Open the server.
      (unless silent
	(gnus-message 5 "Opening %s server%s..." (car method)
		      (if (equal (nth 1 method) "") ""
			(format " on %s" (nth 1 method)))))
      (run-hooks 'gnus-open-server-hook)
      (prog1
	  (gnus-open-server method)
	(unless silent
	  (message ""))))))

(defun gnus-get-function (method function &optional noerror)
  "Return a function symbol based on METHOD and FUNCTION."
  ;; Translate server names into methods.
  (unless method
    (error "Attempted use of a nil select method"))
  (when (stringp method)
    (setq method (gnus-server-to-method method)))
  (let ((func (intern (format "%s-%s" (car method) function))))
    ;; If the functions isn't bound, we require the backend in
    ;; question.
    (unless (fboundp func)
      (require (car method))
      (when (and (not (fboundp func))
		 (not noerror))
	;; This backend doesn't implement this function.
	(error "No such function: %s" func)))
    func))


;;;
;;; Interface functions to the backends.
;;;

(defun gnus-open-server (method)
  "Open a connection to METHOD."
  (when (stringp method)
    (setq method (gnus-server-to-method method)))
  (let ((elem (assoc method gnus-opened-servers)))
    ;; If this method was previously denied, we just return nil.
    (if (eq (nth 1 elem) 'denied)
	(progn
	  (gnus-message 1 "Denied server")
	  nil)
      ;; Open the server.
      (let ((result
	     (funcall (gnus-get-function method 'open-server)
		      (nth 1 method) (nthcdr 2 method))))
	;; If this hasn't been opened before, we add it to the list.
	(unless elem
	  (setq elem (list method nil)
		gnus-opened-servers (cons elem gnus-opened-servers)))
	;; Set the status of this server.
	(setcar (cdr elem) (if result 'ok 'denied))
	;; Return the result from the "open" call.
	result))))

(defun gnus-close-server (method)
  "Close the connection to METHOD."
  (when (stringp method)
    (setq method (gnus-server-to-method method)))
  (funcall (gnus-get-function method 'close-server) (nth 1 method)))

(defun gnus-request-list (method)
  "Request the active file from METHOD."
  (when (stringp method)
    (setq method (gnus-server-to-method method)))
  (funcall (gnus-get-function method 'request-list) (nth 1 method)))

(defun gnus-request-list-newsgroups (method)
  "Request the newsgroups file from METHOD."
  (when (stringp method)
    (setq method (gnus-server-to-method method)))
  (funcall (gnus-get-function method 'request-list-newsgroups) (nth 1 method)))

(defun gnus-request-newgroups (date method)
  "Request all new groups since DATE from METHOD."
  (when (stringp method)
    (setq method (gnus-server-to-method method)))
  (let ((func (gnus-get-function method 'request-newgroups t)))
    (when func
      (funcall func date (nth 1 method)))))

(defun gnus-server-opened (method)
  "Check whether a connection to METHOD has been opened."
  (when (stringp method)
    (setq method (gnus-server-to-method method)))
  (funcall (inline (gnus-get-function method 'server-opened)) (nth 1 method)))

(defun gnus-status-message (method)
  "Return the status message from METHOD.
If METHOD is a string, it is interpreted as a group name.   The method
this group uses will be queried."
  (let ((method (if (stringp method) (gnus-find-method-for-group method)
		  method)))
    (funcall (gnus-get-function method 'status-message) (nth 1 method))))

(defun gnus-request-regenerate (method)
  "Request a data generation from METHOD."
  (when (stringp method)
    (setq method (gnus-server-to-method method)))
  (funcall (gnus-get-function method 'request-regenerate) (nth 1 method)))

(defun gnus-request-group (group &optional dont-check method)
  "Request GROUP.  If DONT-CHECK, no information is required."
  (let ((method (or method (inline (gnus-find-method-for-group group)))))
    (when (stringp method)
      (setq method (inline (gnus-server-to-method method))))
    (funcall (inline (gnus-get-function method 'request-group))
	     (gnus-group-real-name group) (nth 1 method) dont-check)))

(defun gnus-list-active-group (group)
  "Request active information on GROUP."
  (let ((method (gnus-find-method-for-group group))
	(func 'list-active-group))
    (when (gnus-check-backend-function func group)
      (funcall (gnus-get-function method func)
	       (gnus-group-real-name group) (nth 1 method)))))

(defun gnus-request-group-description (group)
  "Request a description of GROUP."
  (let ((method (gnus-find-method-for-group group))
	(func 'request-group-description))
    (when (gnus-check-backend-function func group)
      (funcall (gnus-get-function method func)
	       (gnus-group-real-name group) (nth 1 method)))))

(defun gnus-close-group (group)
  "Request the GROUP be closed."
  (let ((method (inline (gnus-find-method-for-group group))))
    (funcall (gnus-get-function method 'close-group)
	     (gnus-group-real-name group) (nth 1 method))))

(defun gnus-retrieve-headers (articles group &optional fetch-old)
  "Request headers for ARTICLES in GROUP.
If FETCH-OLD, retrieve all headers (or some subset thereof) in the group."
  (let ((method (gnus-find-method-for-group group)))
    (if (and gnus-use-cache (numberp (car articles)))
	(gnus-cache-retrieve-headers articles group fetch-old)
      (funcall (gnus-get-function method 'retrieve-headers)
	       articles (gnus-group-real-name group) (nth 1 method)
	       fetch-old))))

(defun gnus-retrieve-groups (groups method)
  "Request active information on GROUPS from METHOD."
  (when (stringp method)
    (setq method (gnus-server-to-method method)))
  (funcall (gnus-get-function method 'retrieve-groups) groups (nth 1 method)))

(defun gnus-request-type (group &optional article)
  "Return the type (`post' or `mail') of GROUP (and ARTICLE)."
  (let ((method (gnus-find-method-for-group group)))
    (if (not (gnus-check-backend-function 'request-type (car method)))
	'unknown
      (funcall (gnus-get-function method 'request-type)
	       (gnus-group-real-name group) article))))

(defun gnus-request-update-mark (group article mark)
  "Return the type (`post' or `mail') of GROUP (and ARTICLE)."
  (let ((method (gnus-find-method-for-group group)))
    (if (not (gnus-check-backend-function 'request-update-mark (car method)))
	mark
      (funcall (gnus-get-function method 'request-update-mark)
	       (gnus-group-real-name group) article mark))))

(defun gnus-request-article (article group &optional buffer)
  "Request the ARTICLE in GROUP.
ARTICLE can either be an article number or an article Message-ID.
If BUFFER, insert the article in that group."
  (let ((method (gnus-find-method-for-group group)))
    (funcall (gnus-get-function method 'request-article)
	     article (gnus-group-real-name group) (nth 1 method) buffer)))

(defun gnus-request-head (article group)
  "Request the head of ARTICLE in GROUP."
  (let* ((method (gnus-find-method-for-group group))
	 (head (gnus-get-function method 'request-head t))
	 res clean-up)
    (cond
     ;; Check the cache.
     ((and gnus-use-cache
	   (numberp article)
	   (gnus-cache-request-article article group))
      (setq res (cons group article)
	    clean-up t))
     ;; Use `head' function.
     ((fboundp head)
      (setq res (funcall head article (gnus-group-real-name group)
			 (nth 1 method))))
     ;; Use `article' function.
     (t
      (setq res (gnus-request-article article group)
	    clean-up t)))
    (when clean-up
      (save-excursion
	(set-buffer nntp-server-buffer)
	(goto-char (point-min))
	(when (search-forward "\n\n" nil t)
	  (delete-region (1- (point)) (point-max)))
	(nnheader-fold-continuation-lines)))
    res))

(defun gnus-request-body (article group)
  "Request the body of ARTICLE in GROUP."
  (let ((method (gnus-find-method-for-group group)))
    (funcall (gnus-get-function method 'request-body)
	     article (gnus-group-real-name group) (nth 1 method))))

(defun gnus-request-post (method)
  "Post the current buffer using METHOD."
  (when (stringp method)
    (setq method (gnus-server-to-method method)))
  (funcall (gnus-get-function method 'request-post) (nth 1 method)))

(defun gnus-request-scan (group method)
  "Request a SCAN being performed in GROUP from METHOD.
If GROUP is nil, all groups on METHOD are scanned."
  (let ((method (if group (gnus-find-method-for-group group) method))
	(gnus-inhibit-demon t))
    (funcall (gnus-get-function method 'request-scan)
	     (and group (gnus-group-real-name group)) (nth 1 method))))

(defsubst gnus-request-update-info (info method)
  "Request that METHOD update INFO."
  (when (stringp method)
    (setq method (gnus-server-to-method method)))
  (when (gnus-check-backend-function 'request-update-info (car method))
    (funcall (gnus-get-function method 'request-update-info)
	     (gnus-group-real-name (gnus-info-group info))
	     info (nth 1 method))))

(defun gnus-request-expire-articles (articles group &optional force)
  (let ((method (gnus-find-method-for-group group)))
    (funcall (gnus-get-function method 'request-expire-articles)
	     articles (gnus-group-real-name group) (nth 1 method)
	     force)))

(defun gnus-request-move-article
  (article group server accept-function &optional last)
  (let ((method (gnus-find-method-for-group group)))
    (funcall (gnus-get-function method 'request-move-article)
	     article (gnus-group-real-name group)
	     (nth 1 method) accept-function last)))

(defun gnus-request-accept-article (group method &optional last)
  ;; Make sure there's a newline at the end of the article.
  (when (stringp method)
    (setq method (gnus-server-to-method method)))
  (when (and (not method)
	     (stringp group))
    (setq method (gnus-group-name-to-method group)))
  (goto-char (point-max))
  (unless (bolp)
    (insert "\n"))
  (let ((func (car (or method (gnus-find-method-for-group group)))))
    (funcall (intern (format "%s-request-accept-article" func))
	     (if (stringp group) (gnus-group-real-name group) group)
	     (cadr method)
	     last)))

(defun gnus-request-replace-article (article group buffer)
  (let ((func (car (gnus-find-method-for-group group))))
    (funcall (intern (format "%s-request-replace-article" func))
	     article (gnus-group-real-name group) buffer)))

(defun gnus-request-associate-buffer (group)
  (let ((method (gnus-find-method-for-group group)))
    (funcall (gnus-get-function method 'request-associate-buffer)
	     (gnus-group-real-name group))))

(defun gnus-request-restore-buffer (article group)
  "Request a new buffer restored to the state of ARTICLE."
  (let ((method (gnus-find-method-for-group group)))
    (funcall (gnus-get-function method 'request-restore-buffer)
	     article (gnus-group-real-name group) (nth 1 method))))

(defun gnus-request-create-group (group &optional method args)
  (when (stringp method)
    (setq method (gnus-server-to-method method)))
  (let ((method (or method (gnus-find-method-for-group group))))
    (funcall (gnus-get-function method 'request-create-group)
	     (gnus-group-real-name group) (nth 1 method) args)))

(defun gnus-request-delete-group (group &optional force)
  (let ((method (gnus-find-method-for-group group)))
    (funcall (gnus-get-function method 'request-delete-group)
	     (gnus-group-real-name group) force (nth 1 method))))

(defun gnus-request-rename-group (group new-name)
  (let ((method (gnus-find-method-for-group group)))
    (funcall (gnus-get-function method 'request-rename-group)
	     (gnus-group-real-name group)
	     (gnus-group-real-name new-name) (nth 1 method))))

(defun gnus-close-backends ()
  ;; Send a close request to all backends that support such a request.
  (let ((methods gnus-valid-select-methods)
	(gnus-inhibit-demon t)
	func method)
    (while (setq method (pop methods))
      (when (fboundp (setq func (intern
				 (concat (car method) "-request-close"))))
	(funcall func)))))

(defun gnus-asynchronous-p (method)
  (let ((func (gnus-get-function method 'asynchronous-p t)))
    (when (fboundp func)
      (funcall func))))

(defun gnus-remove-denial (method)
  (when (stringp method)
    (setq method (gnus-server-to-method method)))
  (let* ((elem (assoc method gnus-opened-servers))
	 (status (cadr elem)))
    ;; If this hasn't been opened before, we add it to the list.
    (when (eq status 'denied)
      ;; Set the status of this server.
      (setcar (cdr elem) 'closed))))

(provide 'gnus-int)

;;; gnus-int.el ends here
