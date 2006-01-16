;;; gnus-int.el --- backend interface functions for Gnus

;; Copyright (C) 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004
;;   2005 Free Software Foundation, Inc.

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
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
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;;; Code:

(eval-when-compile (require 'cl))

(require 'gnus)
(require 'message)
(require 'gnus-range)

(autoload 'gnus-agent-expire "gnus-agent")
(autoload 'gnus-agent-regenerate-group "gnus-agent")
(autoload 'gnus-agent-read-servers-validate-native "gnus-agent")

(defcustom gnus-open-server-hook nil
  "Hook called just before opening connection to the news server."
  :group 'gnus-start
  :type 'hook)

(defcustom gnus-server-unopen-status nil
  "The default status if the server is not able to open.
If the server is covered by Gnus agent, the possible values are
`denied', set the server denied; `offline', set the server offline;
nil, ask user.  If the server is not covered by Gnus agent, set the
server denied."
  :version "22.1"
  :group 'gnus-start
  :type '(choice (const :tag "Ask" nil)
		 (const :tag "Deny server" denied)
		 (const :tag "Unplug Agent" offline)))

(defvar gnus-internal-registry-spool-current-method nil
  "The current method, for the registry.")

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
				   (substring gnus-nntp-server 1) "~/")))
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
      (gnus-run-hooks 'gnus-open-server-hook)

      ;; Partially validate agent covered methods now that the
      ;; gnus-select-method is known.

      (if gnus-agent
          ;; NOTE: This is here for one purpose only.  By validating
          ;; the current select method, it converts the old 5.10.3,
          ;; and earlier, format to the current format.  That enables
          ;; the agent code within gnus-open-server to function
          ;; correctly.
          (gnus-agent-read-servers-validate-native gnus-select-method))

      (or
       ;; gnus-open-server-hook might have opened it
       (gnus-server-opened gnus-select-method)
       (gnus-open-server gnus-select-method)
       gnus-batch-mode
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
  (let ((method (or method gnus-select-method))
	result)
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
      (gnus-run-hooks 'gnus-open-server-hook)
      (prog1
	  (condition-case ()
	      (setq result (gnus-open-server method))
	    (quit (message "Quit gnus-check-server")
		  nil))
	(unless silent
	  (gnus-message 5 "Opening %s server%s...%s" (car method)
			(if (equal (nth 1 method) "") ""
			  (format " on %s" (nth 1 method)))
			(if result "done" "failed")))))))

(defun gnus-get-function (method function &optional noerror)
  "Return a function symbol based on METHOD and FUNCTION."
  ;; Translate server names into methods.
  (unless method
    (error "Attempted use of a nil select method"))
  (when (stringp method)
    (setq method (gnus-server-to-method method)))
  ;; Check cache of constructed names.
  (let* ((method-sym (if gnus-agent
			 (inline (gnus-agent-get-function method))
		       (car method)))
	 (method-fns (get method-sym 'gnus-method-functions))
	 (func (let ((method-fnlist-elt (assq function method-fns)))
		 (unless method-fnlist-elt
		   (setq method-fnlist-elt
			 (cons function
			       (intern (format "%s-%s" method-sym function))))
		   (put method-sym 'gnus-method-functions
			(cons method-fnlist-elt method-fns)))
		 (cdr method-fnlist-elt))))
    ;; Maybe complain if there is no function.
    (unless (fboundp func)
      (unless (car method)
	(error "Trying to require a method that doesn't exist"))
      (require (car method))
      (when (not (fboundp func))
	(if noerror
	    (setq func nil)
	  (error "No such function: %s" func))))
    func))


;;;
;;; Interface functions to the backends.
;;;

(defun gnus-open-server (gnus-command-method)
  "Open a connection to GNUS-COMMAND-METHOD."
  (when (stringp gnus-command-method)
    (setq gnus-command-method (gnus-server-to-method gnus-command-method)))
  (let ((elem (assoc gnus-command-method gnus-opened-servers)))
    ;; If this method was previously denied, we just return nil.
    (if (eq (nth 1 elem) 'denied)
	(progn
	  (gnus-message 1 "Denied server")
	  nil)
      ;; Open the server.
      (let* ((open-server-function (gnus-get-function gnus-command-method 'open-server))
             (result
             (condition-case err
                 (funcall open-server-function
                          (nth 1 gnus-command-method)
                          (nthcdr 2 gnus-command-method))
               (error
                (gnus-message 1 (format
                                 "Unable to open server due to: %s"
                                 (error-message-string err)))
                nil)
               (quit
                (gnus-message 1 "Quit trying to open server")
                nil)))
            open-offline)
	;; If this hasn't been opened before, we add it to the list.
	(unless elem
	  (setq elem (list gnus-command-method nil)
		gnus-opened-servers (cons elem gnus-opened-servers)))
	;; Set the status of this server.
        (setcar (cdr elem)
                (cond (result
                       (if (eq open-server-function #'nnagent-open-server)
                           ;; The agent's backend has a "special" status
                           'offline
                         'ok))
                      ((and gnus-agent
                            (gnus-agent-method-p gnus-command-method))
                       (cond (gnus-server-unopen-status
                              ;; Set the server's status to the unopen
                              ;; status.  If that status is offline,
                              ;; recurse to open the agent's backend.
                              (setq open-offline (eq gnus-server-unopen-status 'offline))
                              gnus-server-unopen-status)
                             ((gnus-y-or-n-p
                               (format "Unable to open %s:%s, go offline? "
                                       (car gnus-command-method)
                                       (cadr gnus-command-method)))
                              (setq open-offline t)
                              'offline)
                             (t
                              ;; This agentized server was still denied
                              'denied)))
                      (t
                       ;; This unagentized server must be denied
                       'denied)))

        ;; NOTE: I MUST set the server's status to offline before this
        ;; recursive call as this status will drive the
        ;; gnus-get-function (called above) to return the agent's
        ;; backend.
        (if open-offline
            ;; Recursively open this offline server to perform the
            ;; open-server function of the agent's backend.
            (let ((gnus-server-unopen-status 'denied))
              ;; Bind gnus-server-unopen-status to avoid recursively
              ;; prompting with "go offline?".  This is only a concern
              ;; when the agent's backend fails to open the server.
              (gnus-open-server gnus-command-method))
          result)))))

(defun gnus-close-server (gnus-command-method)
  "Close the connection to GNUS-COMMAND-METHOD."
  (when (stringp gnus-command-method)
    (setq gnus-command-method (gnus-server-to-method gnus-command-method)))
  (funcall (gnus-get-function gnus-command-method 'close-server)
	   (nth 1 gnus-command-method)))

(defun gnus-request-list (gnus-command-method)
  "Request the active file from GNUS-COMMAND-METHOD."
  (when (stringp gnus-command-method)
    (setq gnus-command-method (gnus-server-to-method gnus-command-method)))
  (funcall (gnus-get-function gnus-command-method 'request-list)
	   (nth 1 gnus-command-method)))

(defun gnus-request-list-newsgroups (gnus-command-method)
  "Request the newsgroups file from GNUS-COMMAND-METHOD."
  (when (stringp gnus-command-method)
    (setq gnus-command-method (gnus-server-to-method gnus-command-method)))
  (funcall (gnus-get-function gnus-command-method 'request-list-newsgroups)
	   (nth 1 gnus-command-method)))

(defun gnus-request-newgroups (date gnus-command-method)
  "Request all new groups since DATE from GNUS-COMMAND-METHOD."
  (when (stringp gnus-command-method)
    (setq gnus-command-method (gnus-server-to-method gnus-command-method)))
  (let ((func (gnus-get-function gnus-command-method 'request-newgroups t)))
    (when func
      (funcall func date (nth 1 gnus-command-method)))))

(defun gnus-server-opened (gnus-command-method)
  "Check whether a connection to GNUS-COMMAND-METHOD has been opened."
  (unless (eq (gnus-server-status gnus-command-method)
	      'denied)
    (when (stringp gnus-command-method)
      (setq gnus-command-method (gnus-server-to-method gnus-command-method)))
    (funcall (inline (gnus-get-function gnus-command-method 'server-opened))
	     (nth 1 gnus-command-method))))

(defun gnus-status-message (gnus-command-method)
  "Return the status message from GNUS-COMMAND-METHOD.
If GNUS-COMMAND-METHOD is a string, it is interpreted as a group
name.  The method this group uses will be queried."
  (let ((gnus-command-method
	 (if (stringp gnus-command-method)
	     (gnus-find-method-for-group gnus-command-method)
	   gnus-command-method)))
    (funcall (gnus-get-function gnus-command-method 'status-message)
	     (nth 1 gnus-command-method))))

(defun gnus-request-regenerate (gnus-command-method)
  "Request a data generation from GNUS-COMMAND-METHOD."
  (when (stringp gnus-command-method)
    (setq gnus-command-method (gnus-server-to-method gnus-command-method)))
  (funcall (gnus-get-function gnus-command-method 'request-regenerate)
	   (nth 1 gnus-command-method)))

(defun gnus-request-group (group &optional dont-check gnus-command-method)
  "Request GROUP.  If DONT-CHECK, no information is required."
  (let ((gnus-command-method
	 (or gnus-command-method (inline (gnus-find-method-for-group group)))))
    (when (stringp gnus-command-method)
      (setq gnus-command-method
	    (inline (gnus-server-to-method gnus-command-method))))
    (funcall (inline (gnus-get-function gnus-command-method 'request-group))
	     (gnus-group-real-name group) (nth 1 gnus-command-method)
	     dont-check)))

(defun gnus-list-active-group (group)
  "Request active information on GROUP."
  (let ((gnus-command-method (gnus-find-method-for-group group))
	(func 'list-active-group))
    (when (gnus-check-backend-function func group)
      (funcall (gnus-get-function gnus-command-method func)
	       (gnus-group-real-name group) (nth 1 gnus-command-method)))))

(defun gnus-request-group-description (group)
  "Request a description of GROUP."
  (let ((gnus-command-method (gnus-find-method-for-group group))
	(func 'request-group-description))
    (when (gnus-check-backend-function func group)
      (funcall (gnus-get-function gnus-command-method func)
	       (gnus-group-real-name group) (nth 1 gnus-command-method)))))

(defun gnus-request-group-articles (group)
  "Request a list of existing articles in GROUP."
  (let ((gnus-command-method (gnus-find-method-for-group group))
	(func 'request-group-articles))
    (when (gnus-check-backend-function func group)
      (funcall (gnus-get-function gnus-command-method func)
	       (gnus-group-real-name group) (nth 1 gnus-command-method)))))

(defun gnus-close-group (group)
  "Request the GROUP be closed."
  (let ((gnus-command-method (inline (gnus-find-method-for-group group))))
    (funcall (gnus-get-function gnus-command-method 'close-group)
	     (gnus-group-real-name group) (nth 1 gnus-command-method))))

(defun gnus-retrieve-headers (articles group &optional fetch-old)
  "Request headers for ARTICLES in GROUP.
If FETCH-OLD, retrieve all headers (or some subset thereof) in the group."
  (let ((gnus-command-method (gnus-find-method-for-group group)))
    (cond
     ((and gnus-use-cache (numberp (car articles)))
      (gnus-cache-retrieve-headers articles group fetch-old))
     ((and gnus-agent (gnus-online gnus-command-method)
	   (gnus-agent-method-p gnus-command-method))
      (gnus-agent-retrieve-headers articles group fetch-old))
     (t
      (funcall (gnus-get-function gnus-command-method 'retrieve-headers)
	       articles (gnus-group-real-name group)
	       (nth 1 gnus-command-method) fetch-old)))))

(defun gnus-retrieve-articles (articles group)
  "Request ARTICLES in GROUP."
  (let ((gnus-command-method (gnus-find-method-for-group group)))
    (funcall (gnus-get-function gnus-command-method 'retrieve-articles)
	     articles (gnus-group-real-name group)
	     (nth 1 gnus-command-method))))

(defun gnus-retrieve-groups (groups gnus-command-method)
  "Request active information on GROUPS from GNUS-COMMAND-METHOD."
  (when (stringp gnus-command-method)
    (setq gnus-command-method (gnus-server-to-method gnus-command-method)))
  (funcall (gnus-get-function gnus-command-method 'retrieve-groups)
	   groups (nth 1 gnus-command-method)))

(defun gnus-request-type (group &optional article)
  "Return the type (`post' or `mail') of GROUP (and ARTICLE)."
  (let ((gnus-command-method (gnus-find-method-for-group group)))
    (if (not (gnus-check-backend-function
	      'request-type (car gnus-command-method)))
	'unknown
      (funcall (gnus-get-function gnus-command-method 'request-type)
	       (gnus-group-real-name group) article))))

(defun gnus-request-set-mark (group action)
  "Set marks on articles in the back end."
  (let ((gnus-command-method (gnus-find-method-for-group group)))
    (if (not (gnus-check-backend-function
	      'request-set-mark (car gnus-command-method)))
	action
      (funcall (gnus-get-function gnus-command-method 'request-set-mark)
	       (gnus-group-real-name group) action
	       (nth 1 gnus-command-method)))))

(defun gnus-request-update-mark (group article mark)
  "Allow the back end to change the mark the user tries to put on an article."
  (let ((gnus-command-method (gnus-find-method-for-group group)))
    (if (not (gnus-check-backend-function
	      'request-update-mark (car gnus-command-method)))
	mark
      (funcall (gnus-get-function gnus-command-method 'request-update-mark)
	       (gnus-group-real-name group) article mark))))

(defun gnus-request-article (article group &optional buffer)
  "Request the ARTICLE in GROUP.
ARTICLE can either be an article number or an article Message-ID.
If BUFFER, insert the article in that group."
  (let ((gnus-command-method (gnus-find-method-for-group group)))
    (funcall (gnus-get-function gnus-command-method 'request-article)
	     article (gnus-group-real-name group)
	     (nth 1 gnus-command-method) buffer)))

(defun gnus-request-head (article group)
  "Request the head of ARTICLE in GROUP."
  (let* ((gnus-command-method (gnus-find-method-for-group group))
	 (head (gnus-get-function gnus-command-method 'request-head t))
	 res clean-up)
    (cond
     ;; Check the cache.
     ((and gnus-use-cache
	   (numberp article)
	   (gnus-cache-request-article article group))
      (setq res (cons group article)
	    clean-up t))
     ;; Check the agent cache.
     ((gnus-agent-request-article article group)
      (setq res (cons group article)
	    clean-up t))
     ;; Use `head' function.
     ((fboundp head)
      (setq res (funcall head article (gnus-group-real-name group)
			 (nth 1 gnus-command-method))))
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
  (let* ((gnus-command-method (gnus-find-method-for-group group))
	 (head (gnus-get-function gnus-command-method 'request-body t))
	 res clean-up)
    (cond
     ;; Check the cache.
     ((and gnus-use-cache
	   (numberp article)
	   (gnus-cache-request-article article group))
      (setq res (cons group article)
	    clean-up t))
     ;; Check the agent cache.
     ((gnus-agent-request-article article group)
      (setq res (cons group article)
	    clean-up t))
     ;; Use `head' function.
     ((fboundp head)
      (setq res (funcall head article (gnus-group-real-name group)
			 (nth 1 gnus-command-method))))
     ;; Use `article' function.
     (t
      (setq res (gnus-request-article article group)
	    clean-up t)))
    (when clean-up
      (save-excursion
	(set-buffer nntp-server-buffer)
	(goto-char (point-min))
	(when (search-forward "\n\n" nil t)
	  (delete-region (point-min) (1- (point))))))
    res))

(defun gnus-request-post (gnus-command-method)
  "Post the current buffer using GNUS-COMMAND-METHOD."
  (when (stringp gnus-command-method)
    (setq gnus-command-method (gnus-server-to-method gnus-command-method)))
  (funcall (gnus-get-function gnus-command-method 'request-post)
	   (nth 1 gnus-command-method)))

(defun gnus-request-scan (group gnus-command-method)
  "Request a SCAN being performed in GROUP from GNUS-COMMAND-METHOD.
If GROUP is nil, all groups on GNUS-COMMAND-METHOD are scanned."
  (let ((gnus-command-method
	 (if group (gnus-find-method-for-group group) gnus-command-method))
	(gnus-inhibit-demon t)
	(mail-source-plugged gnus-plugged))
    (if (or gnus-plugged (not (gnus-agent-method-p gnus-command-method)))
	(progn
	  (setq gnus-internal-registry-spool-current-method gnus-command-method)
	  (funcall (gnus-get-function gnus-command-method 'request-scan)
		   (and group (gnus-group-real-name group))
		   (nth 1 gnus-command-method))))))

(defsubst gnus-request-update-info (info gnus-command-method)
  "Request that GNUS-COMMAND-METHOD update INFO."
  (when (stringp gnus-command-method)
    (setq gnus-command-method (gnus-server-to-method gnus-command-method)))
  (when (gnus-check-backend-function
	 'request-update-info (car gnus-command-method))
    (let ((group (gnus-info-group info)))
      (and (funcall (gnus-get-function gnus-command-method
				       'request-update-info)
		    (gnus-group-real-name group)
		    info (nth 1 gnus-command-method))
	   ;; If the minimum article number is greater than 1, then all
	   ;; smaller article numbers are known not to exist; we'll
	   ;; artificially add those to the 'read range.
	   (let* ((active (gnus-active group))
		  (min (car active)))
	     (when (> min 1)
	       (let* ((range (if (= min 2) 1 (cons 1 (1- min))))
		      (read (gnus-info-read info))
		      (new-read (gnus-range-add read (list range))))
		 (gnus-info-set-read info new-read)))
	     info)))))

(defun gnus-request-expire-articles (articles group &optional force)
  (let* ((gnus-command-method (gnus-find-method-for-group group))
	 (not-deleted
	  (funcall
	   (gnus-get-function gnus-command-method 'request-expire-articles)
	   articles (gnus-group-real-name group) (nth 1 gnus-command-method)
	   force)))
    (when (and gnus-agent
	       (gnus-agent-method-p gnus-command-method))
      (let ((expired-articles (gnus-sorted-difference articles not-deleted)))
        (when expired-articles
          (gnus-agent-expire expired-articles group 'force))))
    not-deleted))

(defun gnus-request-move-article (article group server accept-function
					  &optional last)
  (let* ((gnus-command-method (gnus-find-method-for-group group))
	 (result (funcall (gnus-get-function gnus-command-method
					     'request-move-article)
			  article (gnus-group-real-name group)
			  (nth 1 gnus-command-method) accept-function last)))
    (when (and result gnus-agent
	       (gnus-agent-method-p gnus-command-method))
      (gnus-agent-unfetch-articles group (list article)))
    result))

(defun gnus-request-accept-article (group &optional gnus-command-method last
					  no-encode)
  ;; Make sure there's a newline at the end of the article.
  (when (stringp gnus-command-method)
    (setq gnus-command-method (gnus-server-to-method gnus-command-method)))
  (when (and (not gnus-command-method)
	     (stringp group))
    (setq gnus-command-method (or (gnus-find-method-for-group group)
                                  (gnus-group-name-to-method group))))
  (goto-char (point-max))
  (unless (bolp)
    (insert "\n"))
  (unless no-encode
    (let ((message-options message-options))
      (message-options-set-recipient)
      (save-restriction
	(message-narrow-to-head)
	(let ((mail-parse-charset message-default-charset))
	  (mail-encode-encoded-word-buffer)))
      (message-encode-message-body)))
(let ((gnus-command-method (or gnus-command-method
				 (gnus-find-method-for-group group)))
	(result
	 (funcall
	  (gnus-get-function gnus-command-method 'request-accept-article)
	  (if (stringp group) (gnus-group-real-name group) group)
	  (cadr gnus-command-method)
	  last)))
    (when (and gnus-agent (gnus-agent-method-p gnus-command-method))
      (gnus-agent-regenerate-group group (list (cdr result))))
    result))

(defun gnus-request-replace-article (article group buffer &optional no-encode)
  (unless no-encode
    (let ((message-options message-options))
      (message-options-set-recipient)
      (save-restriction
	(message-narrow-to-head)
	(let ((mail-parse-charset message-default-charset))
	  (mail-encode-encoded-word-buffer)))
      (message-encode-message-body)))
  (let* ((func (car (gnus-group-name-to-method group)))
         (result (funcall (intern (format "%s-request-replace-article" func))
			  article (gnus-group-real-name group) buffer)))
    (when (and gnus-agent (gnus-agent-method-p gnus-command-method))
      (gnus-agent-regenerate-group group (list article)))
    result))

(defun gnus-request-associate-buffer (group)
  (let ((gnus-command-method (gnus-find-method-for-group group)))
    (funcall (gnus-get-function gnus-command-method 'request-associate-buffer)
	     (gnus-group-real-name group))))

(defun gnus-request-restore-buffer (article group)
  "Request a new buffer restored to the state of ARTICLE."
  (let ((gnus-command-method (gnus-find-method-for-group group)))
    (funcall (gnus-get-function gnus-command-method 'request-restore-buffer)
	     article (gnus-group-real-name group)
	     (nth 1 gnus-command-method))))

(defun gnus-request-create-group (group &optional gnus-command-method args)
  (when (stringp gnus-command-method)
    (setq gnus-command-method (gnus-server-to-method gnus-command-method)))
  (let ((gnus-command-method
	 (or gnus-command-method (gnus-find-method-for-group group))))
    (funcall (gnus-get-function gnus-command-method 'request-create-group)
	     (gnus-group-real-name group) (nth 1 gnus-command-method) args)))

(defun gnus-request-delete-group (group &optional force)
  (let* ((gnus-command-method (gnus-find-method-for-group group))
	 (result
	  (funcall (gnus-get-function gnus-command-method 'request-delete-group)
		   (gnus-group-real-name group) force (nth 1 gnus-command-method))))
    (when result
      (gnus-cache-delete-group group)
      (gnus-agent-delete-group group))
    result))

(defun gnus-request-rename-group (group new-name)
  (let* ((gnus-command-method (gnus-find-method-for-group group))
	 (result
	  (funcall (gnus-get-function gnus-command-method 'request-rename-group)
		   (gnus-group-real-name group)
		   (gnus-group-real-name new-name) (nth 1 gnus-command-method))))
    (when result
      (gnus-cache-rename-group group new-name)
      (gnus-agent-rename-group group new-name))
    result))

(defun gnus-close-backends ()
  ;; Send a close request to all backends that support such a request.
  (let ((methods gnus-valid-select-methods)
	(gnus-inhibit-demon t)
	func gnus-command-method)
    (while (setq gnus-command-method (pop methods))
      (when (fboundp (setq func (intern
				 (concat (car gnus-command-method)
					 "-request-close"))))
	(funcall func)))))

(defun gnus-asynchronous-p (gnus-command-method)
  (let ((func (gnus-get-function gnus-command-method 'asynchronous-p t)))
    (when (fboundp func)
      (funcall func))))

(defun gnus-remove-denial (gnus-command-method)
  (when (stringp gnus-command-method)
    (setq gnus-command-method (gnus-server-to-method gnus-command-method)))
  (let* ((elem (assoc gnus-command-method gnus-opened-servers))
	 (status (cadr elem)))
    ;; If this hasn't been opened before, we add it to the list.
    (when (eq status 'denied)
      ;; Set the status of this server.
      (setcar (cdr elem) 'closed))))

(provide 'gnus-int)

;;; arch-tag: bbc90087-9b7f-4017-a92c-3abf180ac86d
;;; gnus-int.el ends here
