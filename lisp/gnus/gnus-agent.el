;;; gnus-agent.el --- unplugged support for Gnus
;; Copyright (C) 1997, 1998, 1999, 2000 Free Software Foundation, Inc.

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
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

(require 'gnus)
(require 'gnus-cache)
(require 'nnvirtual)
(require 'gnus-sum)
(eval-when-compile
  (require 'timer)
  (require 'cl)
  (require 'gnus-score))

(defcustom gnus-agent-directory (nnheader-concat gnus-directory "agent/")
  "Where the Gnus agent will store its files."
  :group 'gnus-agent
  :type 'directory)

(defcustom gnus-agent-plugged-hook nil
  "Hook run when plugging into the network."
  :group 'gnus-agent
  :type 'hook)

(defcustom gnus-agent-unplugged-hook nil
  "Hook run when unplugging from the network."
  :group 'gnus-agent
  :type 'hook)

(defcustom gnus-agent-handle-level gnus-level-subscribed
  "Groups on levels higher than this variable will be ignored by the Agent."
  :group 'gnus-agent
  :type 'integer)

(defcustom gnus-agent-expire-days 7
  "Read articles older than this will be expired."
  :group 'gnus-agent
  :type 'integer)

(defcustom gnus-agent-expire-all nil
  "If non-nil, also expire unread, ticked and dormant articles.
If nil, only read articles will be expired."
  :group 'gnus-agent
  :type 'boolean)

(defcustom gnus-agent-group-mode-hook nil
  "Hook run in Agent group minor modes."
  :group 'gnus-agent
  :type 'hook)

(defcustom gnus-agent-summary-mode-hook nil
  "Hook run in Agent summary minor modes."
  :group 'gnus-agent
  :type 'hook)

(defcustom gnus-agent-server-mode-hook nil
  "Hook run in Agent summary minor modes."
  :group 'gnus-agent
  :type 'hook)

(defcustom gnus-agent-confirmation-function 'y-or-n-p
  "Function to confirm when error happens."
  :group 'gnus-agent
  :type 'function)

;;; Internal variables

(defvar gnus-agent-history-buffers nil)
(defvar gnus-agent-buffer-alist nil)
(defvar gnus-agent-article-alist nil)
(defvar gnus-agent-group-alist nil)
(defvar gnus-agent-covered-methods nil)
(defvar gnus-category-alist nil)
(defvar gnus-agent-current-history nil)
(defvar gnus-agent-overview-buffer nil)
(defvar gnus-category-predicate-cache nil)
(defvar gnus-category-group-cache nil)
(defvar gnus-agent-spam-hashtb nil)
(defvar gnus-agent-file-name nil)
(defvar gnus-agent-send-mail-function nil)
(defvar gnus-agent-file-coding-system 'raw-text)

(defconst gnus-agent-scoreable-headers
  '("subject" "from" "date" "message-id" "references" "chars" "lines" "xref")
  "Headers that are considered when scoring articles for download via the Agent.")

;; Dynamic variables
(defvar gnus-headers)
(defvar gnus-score)

;;;
;;; Setup
;;;

(defun gnus-open-agent ()
  (setq gnus-agent t)
  (gnus-agent-read-servers)
  (gnus-category-read)
  (gnus-agent-create-buffer)
  (add-hook 'gnus-group-mode-hook 'gnus-agent-mode)
  (add-hook 'gnus-summary-mode-hook 'gnus-agent-mode)
  (add-hook 'gnus-server-mode-hook 'gnus-agent-mode))

(defun gnus-agent-create-buffer ()
  (if (gnus-buffer-live-p gnus-agent-overview-buffer)
      t
    (setq gnus-agent-overview-buffer
	  (gnus-get-buffer-create " *Gnus agent overview*"))
    (with-current-buffer gnus-agent-overview-buffer
      (mm-enable-multibyte))
    nil))

(gnus-add-shutdown 'gnus-close-agent 'gnus)

(defun gnus-close-agent ()
  (setq gnus-agent-covered-methods nil
	gnus-category-predicate-cache nil
	gnus-category-group-cache nil
	gnus-agent-spam-hashtb nil)
  (gnus-kill-buffer gnus-agent-overview-buffer))

;;;
;;; Utility functions
;;;

(defun gnus-agent-read-file (file)
  "Load FILE and do a `read' there."
  (with-temp-buffer
    (ignore-errors
      (nnheader-insert-file-contents file)
      (goto-char (point-min))
      (read (current-buffer)))))

(defsubst gnus-agent-method ()
  (concat (symbol-name (car gnus-command-method)) "/"
	  (if (equal (cadr gnus-command-method) "")
	      "unnamed"
	    (cadr gnus-command-method))))

(defsubst gnus-agent-directory ()
  "Path of the Gnus agent directory."
  (nnheader-concat gnus-agent-directory
		   (nnheader-translate-file-chars (gnus-agent-method)) "/"))

(defun gnus-agent-lib-file (file)
  "The full path of the Gnus agent library FILE."
  (concat (gnus-agent-directory) "agent.lib/" file))

;;; Fetching setup functions.

(defun gnus-agent-start-fetch ()
  "Initialize data structures for efficient fetching."
  (gnus-agent-open-history)
  (setq gnus-agent-current-history (gnus-agent-history-buffer))
  (gnus-agent-create-buffer))

(defun gnus-agent-stop-fetch ()
  "Save all data structures and clean up."
  (gnus-agent-save-history)
  (gnus-agent-close-history)
  (setq gnus-agent-spam-hashtb nil)
  (save-excursion
    (set-buffer nntp-server-buffer)
    (widen)))

(defmacro gnus-agent-with-fetch (&rest forms)
  "Do FORMS safely."
  `(unwind-protect
       (progn
	 (gnus-agent-start-fetch)
	 ,@forms)
     (gnus-agent-stop-fetch)))

(put 'gnus-agent-with-fetch 'lisp-indent-function 0)
(put 'gnus-agent-with-fetch 'edebug-form-spec '(body))

;;;
;;; Mode infestation
;;;

(defvar gnus-agent-mode-hook nil
  "Hook run when installing agent mode.")

(defvar gnus-agent-mode nil)
(defvar gnus-agent-mode-status '(gnus-agent-mode " Plugged"))

(defun gnus-agent-mode ()
  "Minor mode for providing a agent support in Gnus buffers."
  (let* ((buffer (progn (string-match "^gnus-\\(.*\\)-mode$"
				      (symbol-name major-mode))
			(match-string 1 (symbol-name major-mode))))
	 (mode (intern (format "gnus-agent-%s-mode" buffer))))
    (set (make-local-variable 'gnus-agent-mode) t)
    (set mode nil)
    (set (make-local-variable mode) t)
    ;; Set up the menu.
    (when (gnus-visual-p 'agent-menu 'menu)
      (funcall (intern (format "gnus-agent-%s-make-menu-bar" buffer))))
    (unless (assq 'gnus-agent-mode minor-mode-alist)
      (push gnus-agent-mode-status minor-mode-alist))
    (unless (assq mode minor-mode-map-alist)
      (push (cons mode (symbol-value (intern (format "gnus-agent-%s-mode-map"
						     buffer))))
	    minor-mode-map-alist))
    (when (eq major-mode 'gnus-group-mode)
      (gnus-agent-toggle-plugged gnus-plugged))
    (gnus-run-hooks 'gnus-agent-mode-hook
		    (intern (format "gnus-agent-%s-mode-hook" buffer)))))

(defvar gnus-agent-group-mode-map (make-sparse-keymap))
(gnus-define-keys gnus-agent-group-mode-map
  "Ju" gnus-agent-fetch-groups
  "Jc" gnus-enter-category-buffer
  "Jj" gnus-agent-toggle-plugged
  "Js" gnus-agent-fetch-session
  "JY" gnus-agent-synchronize
  "JS" gnus-group-send-drafts
  "Ja" gnus-agent-add-group
  "Jr" gnus-agent-remove-group)

(defun gnus-agent-group-make-menu-bar ()
  (unless (boundp 'gnus-agent-group-menu)
    (easy-menu-define
     gnus-agent-group-menu gnus-agent-group-mode-map ""
     '("Agent"
       ["Toggle plugged" gnus-agent-toggle-plugged t]
       ["List categories" gnus-enter-category-buffer t]
       ["Send drafts" gnus-group-send-drafts gnus-plugged]
       ("Fetch"
	["All" gnus-agent-fetch-session gnus-plugged]
	["Group" gnus-agent-fetch-group gnus-plugged])))))

(defvar gnus-agent-summary-mode-map (make-sparse-keymap))
(gnus-define-keys gnus-agent-summary-mode-map
  "Jj" gnus-agent-toggle-plugged
  "J#" gnus-agent-mark-article
  "J\M-#" gnus-agent-unmark-article
  "@" gnus-agent-toggle-mark
  "Jc" gnus-agent-catchup)

(defun gnus-agent-summary-make-menu-bar ()
  (unless (boundp 'gnus-agent-summary-menu)
    (easy-menu-define
     gnus-agent-summary-menu gnus-agent-summary-mode-map ""
     '("Agent"
       ["Toggle plugged" gnus-agent-toggle-plugged t]
       ["Mark as downloadable" gnus-agent-mark-article t]
       ["Unmark as downloadable" gnus-agent-unmark-article t]
       ["Toggle mark" gnus-agent-toggle-mark t]
       ["Catchup undownloaded" gnus-agent-catchup t]))))

(defvar gnus-agent-server-mode-map (make-sparse-keymap))
(gnus-define-keys gnus-agent-server-mode-map
  "Jj" gnus-agent-toggle-plugged
  "Ja" gnus-agent-add-server
  "Jr" gnus-agent-remove-server)

(defun gnus-agent-server-make-menu-bar ()
  (unless (boundp 'gnus-agent-server-menu)
    (easy-menu-define
     gnus-agent-server-menu gnus-agent-server-mode-map ""
     '("Agent"
       ["Toggle plugged" gnus-agent-toggle-plugged t]
       ["Add" gnus-agent-add-server t]
       ["Remove" gnus-agent-remove-server t]))))

(defun gnus-agent-toggle-plugged (plugged)
  "Toggle whether Gnus is unplugged or not."
  (interactive (list (not gnus-plugged)))
  (if plugged
      (progn
	(setq gnus-plugged plugged)
	(gnus-run-hooks 'gnus-agent-plugged-hook)
	(setcar (cdr gnus-agent-mode-status) " Plugged"))
    (gnus-agent-close-connections)
    (setq gnus-plugged plugged)
    (gnus-run-hooks 'gnus-agent-unplugged-hook)
    (setcar (cdr gnus-agent-mode-status) " Unplugged"))
  (set-buffer-modified-p t))

(defun gnus-agent-close-connections ()
  "Close all methods covered by the Gnus agent."
  (let ((methods gnus-agent-covered-methods))
    (while methods
      (gnus-close-server (pop methods)))))

;;;###autoload
(defun gnus-unplugged ()
  "Start Gnus unplugged."
  (interactive)
  (setq gnus-plugged nil)
  (gnus))

;;;###autoload
(defun gnus-plugged ()
  "Start Gnus plugged."
  (interactive)
  (setq gnus-plugged t)
  (gnus))

;;;###autoload
(defun gnus-agentize ()
  "Allow Gnus to be an offline newsreader.
The normal usage of this command is to put the following as the
last form in your `.gnus.el' file:

\(gnus-agentize)

This will modify the `gnus-before-startup-hook', `gnus-post-method',
and `message-send-mail-function' variables, and install the Gnus
agent minor mode in all Gnus buffers."
  (interactive)
  (gnus-open-agent)
  (add-hook 'gnus-setup-news-hook 'gnus-agent-queue-setup)
  (unless gnus-agent-send-mail-function
    (setq gnus-agent-send-mail-function message-send-mail-function
	  message-send-mail-function 'gnus-agent-send-mail))
  (unless gnus-agent-covered-methods
    (setq gnus-agent-covered-methods (list gnus-select-method))))

(defun gnus-agent-queue-setup ()
  "Make sure the queue group exists."
  (unless (gnus-gethash "nndraft:queue" gnus-newsrc-hashtb)
    (gnus-request-create-group "queue" '(nndraft ""))
    (let ((gnus-level-default-subscribed 1))
      (gnus-subscribe-group "nndraft:queue" nil '(nndraft "")))
    (gnus-group-set-parameter
     "nndraft:queue" 'gnus-dummy '((gnus-draft-mode)))))

(defun gnus-agent-send-mail ()
  (if gnus-plugged
      (funcall gnus-agent-send-mail-function)
    (goto-char (point-min))
    (re-search-forward
     (concat "^" (regexp-quote mail-header-separator) "\n"))
    (replace-match "\n")
    (gnus-agent-insert-meta-information 'mail)
    (gnus-request-accept-article "nndraft:queue" nil t t)))

(defun gnus-agent-insert-meta-information (type &optional method)
  "Insert meta-information into the message that says how it's to be posted.
TYPE can be either `mail' or `news'.  If the latter METHOD can
be a select method."
  (save-excursion
    (message-remove-header gnus-agent-meta-information-header)
    (goto-char (point-min))
    (insert gnus-agent-meta-information-header ": "
	    (symbol-name type) " " (format "%S" method)
	    "\n")
    (forward-char -1)
    (while (search-backward "\n" nil t)
      (replace-match "\\n" t t))))

;;;
;;; Group mode commands
;;;

(defun gnus-agent-fetch-groups (n)
  "Put all new articles in the current groups into the Agent."
  (interactive "P")
  (unless gnus-plugged
    (error "Groups can't be fetched when Gnus is unplugged"))
  (gnus-group-iterate n 'gnus-agent-fetch-group))

(defun gnus-agent-fetch-group (group)
  "Put all new articles in GROUP into the Agent."
  (interactive (list (gnus-group-group-name)))
  (unless gnus-plugged
    (error "Groups can't be fetched when Gnus is unplugged"))
  (unless group
    (error "No group on the current line"))
  (let ((gnus-command-method (gnus-find-method-for-group group)))
    (gnus-agent-with-fetch
      (gnus-agent-fetch-group-1 group gnus-command-method)
      (gnus-message 5 "Fetching %s...done" group))))

(defun gnus-agent-add-group (category arg)
  "Add the current group to an agent category."
  (interactive
   (list
    (intern
     (completing-read
      "Add to category: "
      (mapcar (lambda (cat) (list (symbol-name (car cat))))
	      gnus-category-alist)
      nil t))
    current-prefix-arg))
  (let ((cat (assq category gnus-category-alist))
	c groups)
    (gnus-group-iterate arg
      (lambda (group)
	(when (cadddr (setq c (gnus-group-category group)))
	  (setf (cadddr c) (delete group (cadddr c))))
	(push group groups)))
    (setf (cadddr cat) (nconc (cadddr cat) groups))
    (gnus-category-write)))

(defun gnus-agent-remove-group (arg)
  "Remove the current group from its agent category, if any."
  (interactive "P")
  (let (c)
    (gnus-group-iterate arg
      (lambda (group)
	(when (cadddr (setq c (gnus-group-category group)))
	  (setf (cadddr c) (delete group (cadddr c))))))
    (gnus-category-write)))

(defun gnus-agent-synchronize ()
  "Synchronize local, unplugged, data with backend.
Currently sends flag setting requests, if any."
  (interactive)
  (save-excursion
    (dolist (gnus-command-method gnus-agent-covered-methods)
      (when (file-exists-p (gnus-agent-lib-file "flags"))
	(set-buffer (get-buffer-create " *Gnus Agent flag synchronize*"))
	(erase-buffer)
	(nnheader-insert-file-contents (gnus-agent-lib-file "flags"))
	(if (null (gnus-check-server gnus-command-method))
	    (message "Couldn't open server %s" (nth 1 gnus-command-method))
	  (while (not (eobp))
	    (if (null (eval (read (current-buffer))))
		(progn (forward-line)
		       (kill-line -1))
	      (write-file (gnus-agent-lib-file "flags"))
	      (error "Couldn't set flags from file %s"
		     (gnus-agent-lib-file "flags"))))
	  (write-file (gnus-agent-lib-file "flags")))
        (kill-buffer nil)))))

;;;
;;; Server mode commands
;;;

(defun gnus-agent-add-server (server)
  "Enroll SERVER in the agent program."
  (interactive (list (gnus-server-server-name)))
  (unless server
    (error "No server on the current line"))
  (let ((method (gnus-server-get-method nil (gnus-server-server-name))))
    (when (member method gnus-agent-covered-methods)
      (error "Server already in the agent program"))
    (push method gnus-agent-covered-methods)
    (gnus-agent-write-servers)
    (message "Entered %s into the Agent" server)))

(defun gnus-agent-remove-server (server)
  "Remove SERVER from the agent program."
  (interactive (list (gnus-server-server-name)))
  (unless server
    (error "No server on the current line"))
  (let ((method (gnus-server-get-method nil (gnus-server-server-name))))
    (unless (member method gnus-agent-covered-methods)
      (error "Server not in the agent program"))
    (setq gnus-agent-covered-methods
	  (delete method gnus-agent-covered-methods))
    (gnus-agent-write-servers)
    (message "Removed %s from the agent" server)))

(defun gnus-agent-read-servers ()
  "Read the alist of covered servers."
  (setq gnus-agent-covered-methods
	(gnus-agent-read-file
	 (nnheader-concat gnus-agent-directory "lib/servers"))))

(defun gnus-agent-write-servers ()
  "Write the alist of covered servers."
  (gnus-make-directory (nnheader-concat gnus-agent-directory "lib"))
  (let ((coding-system-for-write nnheader-file-coding-system)
	(file-name-coding-system nnmail-pathname-coding-system))
    (with-temp-file (nnheader-concat gnus-agent-directory "lib/servers")
      (prin1 gnus-agent-covered-methods (current-buffer)))))

;;;
;;; Summary commands
;;;

(defun gnus-agent-mark-article (n &optional unmark)
  "Mark the next N articles as downloadable.
If N is negative, mark backward instead.  If UNMARK is non-nil, remove
the mark instead.  The difference between N and the actual number of
articles marked is returned."
  (interactive "p")
  (let ((backward (< n 0))
	(n (abs n)))
    (while (and
	    (> n 0)
	    (progn
	      (gnus-summary-set-agent-mark
	       (gnus-summary-article-number) unmark)
	      (zerop (gnus-summary-next-subject (if backward -1 1) nil t))))
      (setq n (1- n)))
    (when (/= 0 n)
      (gnus-message 7 "No more articles"))
    (gnus-summary-recenter)
    (gnus-summary-position-point)
    n))

(defun gnus-agent-unmark-article (n)
  "Remove the downloadable mark from the next N articles.
If N is negative, unmark backward instead.  The difference between N and
the actual number of articles unmarked is returned."
  (interactive "p")
  (gnus-agent-mark-article n t))

(defun gnus-agent-toggle-mark (n)
  "Toggle the downloadable mark from the next N articles.
If N is negative, toggle backward instead.  The difference between N and
the actual number of articles toggled is returned."
  (interactive "p")
  (gnus-agent-mark-article n 'toggle))

(defun gnus-summary-set-agent-mark (article &optional unmark)
  "Mark ARTICLE as downloadable."
  (let ((unmark (if (and (not (null unmark)) (not (eq t unmark)))
		    (memq article gnus-newsgroup-downloadable)
		  unmark)))
    (if unmark
	(progn
	  (setq gnus-newsgroup-downloadable
		(delq article gnus-newsgroup-downloadable))
	  (push article gnus-newsgroup-undownloaded))
      (setq gnus-newsgroup-undownloaded
	    (delq article gnus-newsgroup-undownloaded))
      (push article gnus-newsgroup-downloadable))
    (gnus-summary-update-mark
     (if unmark gnus-undownloaded-mark gnus-downloadable-mark)
     'unread)))

(defun gnus-agent-get-undownloaded-list ()
  "Mark all unfetched articles as read."
  (let ((gnus-command-method (gnus-find-method-for-group gnus-newsgroup-name)))
    (when (and (not gnus-plugged)
	       (gnus-agent-method-p gnus-command-method))
      (gnus-agent-load-alist gnus-newsgroup-name)
      ;; First mark all undownloaded articles as undownloaded.
      (let ((articles (append gnus-newsgroup-unreads
			      gnus-newsgroup-marked
			      gnus-newsgroup-dormant))
	    article)
	(while (setq article (pop articles))
	  (unless (or (cdr (assq article gnus-agent-article-alist))
		      (memq article gnus-newsgroup-downloadable)
		      (memq article gnus-newsgroup-cached))
	    (push article gnus-newsgroup-undownloaded))))
      ;; Then mark downloaded downloadable as not-downloadable,
      ;; if you get my drift.
      (let ((articles gnus-newsgroup-downloadable)
	    article)
	(while (setq article (pop articles))
	  (when (cdr (assq article gnus-agent-article-alist))
	    (setq gnus-newsgroup-downloadable
		  (delq article gnus-newsgroup-downloadable))))))))

(defun gnus-agent-catchup ()
  "Mark all undownloaded articles as read."
  (interactive)
  (save-excursion
    (while gnus-newsgroup-undownloaded
      (gnus-summary-mark-article
       (pop gnus-newsgroup-undownloaded) gnus-catchup-mark)))
  (gnus-summary-position-point))

;;;
;;; Internal functions
;;;

(defun gnus-agent-save-active (method)
  (gnus-agent-save-active-1 method 'gnus-active-to-gnus-format))

(defun gnus-agent-save-active-1 (method function)
  (when (gnus-agent-method-p method)
    (let* ((gnus-command-method method)
	   (new (gnus-make-hashtable (count-lines (point-min) (point-max))))
	   (file (gnus-agent-lib-file "active")))
      (funcall function nil new)
      (gnus-agent-write-active file new)
      (erase-buffer)
      (nnheader-insert-file-contents file))))

(defun gnus-agent-write-active (file new)
  (let ((orig (gnus-make-hashtable (count-lines (point-min) (point-max))))
	(file (gnus-agent-lib-file "active"))
	elem osym)
    (when (file-exists-p file)
      (with-temp-buffer
	(nnheader-insert-file-contents file)
	(gnus-active-to-gnus-format nil orig))
      (mapatoms
       (lambda (sym)
	 (when (and sym (boundp sym))
	   (if (and (boundp (setq osym (intern (symbol-name sym) orig)))
		    (setq elem (symbol-value osym)))
	       (setcdr elem (cdr (symbol-value sym)))
	     (set (intern (symbol-name sym) orig) (symbol-value sym)))))
       new))
    (gnus-make-directory (file-name-directory file))
    (let ((coding-system-for-write gnus-agent-file-coding-system))
      ;; The hashtable contains real names of groups,  no more prefix
      ;; removing, so set `full' to `t'.
      (gnus-write-active-file file orig t))))

(defun gnus-agent-save-groups (method)
  (gnus-agent-save-active-1 method 'gnus-groups-to-gnus-format))

(defun gnus-agent-save-group-info (method group active)
  (when (gnus-agent-method-p method)
    (let* ((gnus-command-method method)
	   (coding-system-for-write nnheader-file-coding-system)
	   (file-name-coding-system nnmail-pathname-coding-system)
	   (file (gnus-agent-lib-file "active"))
	   oactive)
      (gnus-make-directory (file-name-directory file))
      (with-temp-file file
	;; Emacs got problem to match non-ASCII group in multibyte buffer.
	(mm-disable-multibyte) 
	(when (file-exists-p file)
	  (nnheader-insert-file-contents file))
	(goto-char (point-min))
	(when (re-search-forward
	       (concat "^" (regexp-quote group) " ") nil t)
	  (save-excursion
	    (save-restriction
	      (narrow-to-region (match-beginning 0)
				(progn
				  (forward-line 1)
				  (point)))
	      (setq oactive (car (nnmail-parse-active)))))
	  (gnus-delete-line))
	(insert (format "%S %d %d y\n" (intern group)
			(cdr active)
			(or (car oactive) (car active))))
	(goto-char (point-max))
	(while (search-backward "\\." nil t)
	  (delete-char 1))))))

(defun gnus-agent-group-path (group)
  "Translate GROUP into a path."
  (if nnmail-use-long-file-names
      (gnus-group-real-name group)
    (nnheader-translate-file-chars
     (nnheader-replace-chars-in-string
      (nnheader-replace-duplicate-chars-in-string
       (nnheader-replace-chars-in-string 
	(gnus-group-real-name group)
	?/ ?_)
       ?. ?_)
      ?. ?/))))



(defun gnus-agent-method-p (method)
  "Say whether METHOD is covered by the agent."
  (member method gnus-agent-covered-methods))

(defun gnus-agent-get-function (method)
  (if (and (not gnus-plugged)
	   (gnus-agent-method-p method))
      (progn
	(require 'nnagent)
	'nnagent)
    (car method)))

;;; History functions

(defun gnus-agent-history-buffer ()
  (cdr (assoc (gnus-agent-method) gnus-agent-history-buffers)))

(defun gnus-agent-open-history ()
  (save-excursion
    (push (cons (gnus-agent-method)
		(set-buffer (gnus-get-buffer-create
			     (format " *Gnus agent %s history*"
				     (gnus-agent-method)))))
	  gnus-agent-history-buffers)
    (mm-disable-multibyte) ;; everything is binary
    (erase-buffer)
    (insert "\n")
    (let ((file (gnus-agent-lib-file "history")))
      (when (file-exists-p file)
	(nnheader-insert-file-contents file))
      (set (make-local-variable 'gnus-agent-file-name) file))))

(defun gnus-agent-save-history ()
  (save-excursion
    (set-buffer gnus-agent-current-history)
    (gnus-make-directory (file-name-directory gnus-agent-file-name))
    (let ((coding-system-for-write gnus-agent-file-coding-system))
      (write-region (1+ (point-min)) (point-max)
		    gnus-agent-file-name nil 'silent))))

(defun gnus-agent-close-history ()
  (when (gnus-buffer-live-p gnus-agent-current-history)
    (kill-buffer gnus-agent-current-history)
    (setq gnus-agent-history-buffers
	  (delq (assoc (gnus-agent-method) gnus-agent-history-buffers)
		gnus-agent-history-buffers))))

(defun gnus-agent-enter-history (id group-arts date)
  (save-excursion
    (set-buffer gnus-agent-current-history)
    (goto-char (point-max))
    (let ((p (point)))
      (insert id "\t" (number-to-string date) "\t")
      (while group-arts
	(insert (format "%S" (intern (caar group-arts)))
		" " (number-to-string (cdr (pop group-arts)))
		" "))
      (insert "\n")
      (while (search-backward "\\." p t)
	(delete-char 1)))))

(defun gnus-agent-article-in-history-p (id)
  (save-excursion
    (set-buffer (gnus-agent-history-buffer))
    (goto-char (point-min))
    (search-forward (concat "\n" id "\t") nil t)))

(defun gnus-agent-history-path (id)
  (save-excursion
    (set-buffer (gnus-agent-history-buffer))
    (goto-char (point-min))
    (when (search-forward (concat "\n" id "\t") nil t)
      (let ((method (gnus-agent-method)))
	(let (paths group)
	  (while (not (numberp (setq group (read (current-buffer)))))
	    (push (concat method "/" group) paths))
	  (nreverse paths))))))

;;;
;;; Fetching
;;;

(defun gnus-agent-fetch-articles (group articles)
  "Fetch ARTICLES from GROUP and put them into the Agent."
  (when articles
    ;; Prune off articles that we have already fetched.
    (while (and articles
		(cdr (assq (car articles) gnus-agent-article-alist)))
      (pop articles))
    (let ((arts articles))
      (while (cdr arts)
	(if (cdr (assq (cadr arts) gnus-agent-article-alist))
	    (setcdr arts (cddr arts))
	  (setq arts (cdr arts)))))
    (when articles
      (let ((dir (concat
		  (gnus-agent-directory)
		  (gnus-agent-group-path group) "/"))
	    (date (time-to-days (current-time)))
	    (case-fold-search t)
	    pos crosses id elem)
	(gnus-make-directory dir)
	(gnus-message 7 "Fetching articles for %s..." group)
	;; Fetch the articles from the backend.
	(if (gnus-check-backend-function 'retrieve-articles group)
	    (setq pos (gnus-retrieve-articles articles group))
	  (with-temp-buffer
	    (let (article)
	      (while (setq article (pop articles))
		(when (or 
		       (gnus-backlog-request-article group article 
						     nntp-server-buffer)
		       (gnus-request-article article group))
		  (goto-char (point-max))
		  (push (cons article (point)) pos)
		  (insert-buffer-substring nntp-server-buffer)))
	      (copy-to-buffer nntp-server-buffer (point-min) (point-max))
	      (setq pos (nreverse pos)))))
	;; Then save these articles into the Agent.
	(save-excursion
	  (set-buffer nntp-server-buffer)
	  (while pos
	    (narrow-to-region (cdar pos) (or (cdadr pos) (point-max)))
	    (goto-char (point-min))
	    (when (search-forward "\n\n" nil t)
	      (when (search-backward "\nXrefs: " nil t)
		;; Handle crossposting.
		(skip-chars-forward "^ ")
		(skip-chars-forward " ")
		(setq crosses nil)
		(while (looking-at "\\([^: \n]+\\):\\([0-9]+\\) +")
		  (push (cons (buffer-substring (match-beginning 1)
						(match-end 1))
			      (buffer-substring (match-beginning 2)
						(match-end 2)))
			crosses)
		  (goto-char (match-end 0)))
		(gnus-agent-crosspost crosses (caar pos))))
	    (goto-char (point-min))
	    (if (not (re-search-forward "^Message-ID: *<\\([^>\n]+\\)>" nil t))
		(setq id "No-Message-ID-in-article")
	      (setq id (buffer-substring (match-beginning 1) (match-end 1))))
	    (let ((coding-system-for-write
		   gnus-agent-file-coding-system))
	      (write-region (point-min) (point-max)
			    (concat dir (number-to-string (caar pos)))
			    nil 'silent))
	    (when (setq elem (assq (caar pos) gnus-agent-article-alist))
	      (setcdr elem t))
	    (gnus-agent-enter-history
	     id (or crosses (list (cons group (caar pos)))) date)
	    (widen)
	    (pop pos)))
	(gnus-agent-save-alist group)))))

(defun gnus-agent-crosspost (crosses article)
  (let (gnus-agent-article-alist group alist beg end)
    (save-excursion
      (set-buffer gnus-agent-overview-buffer)
      (when (nnheader-find-nov-line article)
	(forward-word 1)
	(setq beg (point))
	(setq end (progn (forward-line 1) (point)))))
    (while crosses
      (setq group (caar crosses))
      (unless (setq alist (assoc group gnus-agent-group-alist))
	(push (setq alist (list group (gnus-agent-load-alist (caar crosses))))
	      gnus-agent-group-alist))
      (setcdr alist (cons (cons (cdar crosses) t) (cdr alist)))
      (save-excursion
	(set-buffer (gnus-get-buffer-create (format " *Gnus agent overview %s*"
						    group)))
	(when (= (point-max) (point-min))
	  (push (cons group (current-buffer)) gnus-agent-buffer-alist)
	  (ignore-errors
	    (nnheader-insert-file-contents
	     (gnus-agent-article-name ".overview" group))))
	(nnheader-find-nov-line (string-to-number (cdar crosses)))
	(insert (string-to-number (cdar crosses)))
	(insert-buffer-substring gnus-agent-overview-buffer beg end))
      (pop crosses))))

(defun gnus-agent-flush-cache ()
  (save-excursion
    (while gnus-agent-buffer-alist
      (set-buffer (cdar gnus-agent-buffer-alist))
      (let ((coding-system-for-write
	     gnus-agent-file-coding-system))
	(write-region (point-min) (point-max)
		      (gnus-agent-article-name ".overview"
					       (caar gnus-agent-buffer-alist))
		      nil 'silent))
      (pop gnus-agent-buffer-alist))
    (while gnus-agent-group-alist
      (with-temp-file (caar gnus-agent-group-alist)
	(princ (cdar gnus-agent-group-alist))
	(insert "\n"))
      (pop gnus-agent-group-alist))))

(if (fboundp 'union)
    (defalias 'gnus-agent-union 'union)
  (defun gnus-agent-union (l1 l2)
    "Set union of lists L1 and L2."
    (cond ((null l1) l2)
	  ((null l2) l1)
	  ((equal l1 l2) l1)
	  (t
	   (or (>= (length l1) (length l2))
	       (setq l1 (prog1 l2 (setq l2 l1))))
	   (while l2
	     (or (memq (car l2) l1)
		 (push (car l2) l1))
	     (pop l2))
	   l1))))

(defun gnus-agent-fetch-headers (group &optional force)
  (let ((articles (gnus-list-of-unread-articles group))
	(gnus-decode-encoded-word-function 'identity)
	(file (gnus-agent-article-name ".overview" group)))
    ;; Add article with marks to list of article headers we want to fetch.
    (dolist (arts (gnus-info-marks (gnus-get-info group)))
      (setq articles (gnus-agent-union (gnus-uncompress-sequence (cdr arts))
			    articles)))
    (setq articles (sort articles '<))
    ;; Remove known articles.
    (when (gnus-agent-load-alist group)
      (setq articles (gnus-sorted-intersection
		      articles
		      (gnus-uncompress-range
		       (cons (1+ (caar (last gnus-agent-article-alist)))
			     (cdr (gnus-active group)))))))
    ;; Fetch them.
    (gnus-make-directory (nnheader-translate-file-chars
			  (file-name-directory file) t))
    (when articles
      (gnus-message 7 "Fetching headers for %s..." group)
      (save-excursion
 	(set-buffer nntp-server-buffer)
 	(unless (eq 'nov (gnus-retrieve-headers articles group))
 	  (nnvirtual-convert-headers))
 	;; Save these headers for later processing.
 	(copy-to-buffer gnus-agent-overview-buffer (point-min) (point-max))
	(when (file-exists-p file)
	  (gnus-agent-braid-nov group articles file))
	(let ((coding-system-for-write
	       gnus-agent-file-coding-system))
	  (write-region (point-min) (point-max) file nil 'silent))
	(gnus-agent-save-alist group articles nil)
	(gnus-agent-enter-history
	 "last-header-fetched-for-session"
	 (list (cons group (nth (- (length  articles) 1) articles)))
	 (time-to-days (current-time)))
	articles))))

(defsubst gnus-agent-copy-nov-line (article)
  (let (b e)
    (set-buffer gnus-agent-overview-buffer)
    (setq b (point))
    (if (eq article (read (current-buffer)))
	(setq e (progn (forward-line 1) (point)))
      (progn
	(beginning-of-line)
	(setq e b)))
    (set-buffer nntp-server-buffer)
    (insert-buffer-substring gnus-agent-overview-buffer b e)))

(defun gnus-agent-braid-nov (group articles file)
  (set-buffer gnus-agent-overview-buffer)
  (goto-char (point-min))
  (set-buffer nntp-server-buffer)
  (erase-buffer)
  (nnheader-insert-file-contents file)
  (goto-char (point-max))
  (if (or (= (point-min) (point-max))
	  (progn
	    (forward-line -1)
	    (< (read (current-buffer)) (car articles))))
      ;; We have only headers that are after the older headers,
      ;; so we just append them.
      (progn
	(goto-char (point-max))
	(insert-buffer-substring gnus-agent-overview-buffer))
    ;; We do it the hard way.
    (nnheader-find-nov-line (car articles))
    (gnus-agent-copy-nov-line (car articles))
    (pop articles)
    (while (and articles
		(not (eobp)))
      (while (and (not (eobp))
		  (< (read (current-buffer)) (car articles)))
	(forward-line 1))
      (beginning-of-line)
      (unless (eobp)
	(gnus-agent-copy-nov-line (car articles))
	(setq articles (cdr articles))))
    (when articles
      (let (b e)
	(set-buffer gnus-agent-overview-buffer)
	(setq b (point)
	      e (point-max))
	(set-buffer nntp-server-buffer)
	(insert-buffer-substring gnus-agent-overview-buffer b e)))))

(defun gnus-agent-load-alist (group &optional dir)
  "Load the article-state alist for GROUP."
  (setq gnus-agent-article-alist
	(gnus-agent-read-file
	 (if dir
	     (concat dir ".agentview")
	   (gnus-agent-article-name ".agentview" group)))))

(defun gnus-agent-save-alist (group &optional articles state dir)
  "Save the article-state alist for GROUP."
  (let ((file-name-coding-system nnmail-pathname-coding-system))
      (with-temp-file (if dir
			  (concat dir ".agentview")
			(gnus-agent-article-name ".agentview" group))
	(princ (setq gnus-agent-article-alist
		     (nconc gnus-agent-article-alist
			    (mapcar (lambda (article) (cons article state))
				    articles)))
	       (current-buffer))
	(insert "\n"))))

(defun gnus-agent-article-name (article group)
  (concat (gnus-agent-directory) (gnus-agent-group-path group) "/"
	  (if (stringp article) article (string-to-number article))))

(defun gnus-agent-batch-confirmation (msg)
  "Show error message and return t."
  (gnus-message 1 msg)
  t)

;;;###autoload
(defun gnus-agent-batch-fetch ()
  "Start Gnus and fetch session."
  (interactive)
  (gnus)
  (let ((gnus-agent-confirmation-function 'gnus-agent-batch-confirmation))
    (gnus-agent-fetch-session))
  (gnus-group-exit))

(defun gnus-agent-fetch-session ()
  "Fetch all articles and headers that are eligible for fetching."
  (interactive)
  (unless gnus-agent-covered-methods
    (error "No servers are covered by the Gnus agent"))
  (unless gnus-plugged
    (error "Can't fetch articles while Gnus is unplugged"))
  (let ((methods gnus-agent-covered-methods)
	groups group gnus-command-method)
    (save-excursion
      (while methods
	(condition-case err
	    (progn
	      (setq gnus-command-method (car methods))
	      (when (or (gnus-server-opened gnus-command-method)
			(gnus-open-server gnus-command-method))
		(setq groups (gnus-groups-from-server (car methods)))
		(gnus-agent-with-fetch
		  (while (setq group (pop groups))
		    (when (<= (gnus-group-level group) gnus-agent-handle-level)
		      (gnus-agent-fetch-group-1 group gnus-command-method))))))
	  (error 
	   (unless (funcall gnus-agent-confirmation-function
			    (format "Error (%s).  Continue? " err))
	     (error "Cannot fetch articles into the Gnus agent."))))
	(pop methods))
      (gnus-message 6 "Finished fetching articles into the Gnus agent"))))

(defun gnus-agent-fetch-group-1 (group method)
  "Fetch GROUP."
  (let ((gnus-command-method method)
	(gnus-newsgroup-name group)
	gnus-newsgroup-dependencies gnus-newsgroup-headers
	gnus-newsgroup-scored gnus-headers gnus-score
	gnus-use-cache articles arts
	category predicate info marks score-param
	(gnus-summary-expunge-below gnus-summary-expunge-below)
	(gnus-summary-mark-below gnus-summary-mark-below)
	(gnus-orphan-score gnus-orphan-score)
	;; Maybe some other gnus-summary local variables should also
	;; be put here.
	)
    (unless (gnus-check-group group)
      (error "Can't open server for %s" group))
    ;; Fetch headers.
    (when (and (or (gnus-active group) (gnus-activate-group group))
	       (setq articles (gnus-agent-fetch-headers group))
	       (progn
		 ;; Parse them and see which articles we want to fetch.
		 (setq gnus-newsgroup-dependencies
		       (make-vector (length articles) 0))
		 ;; No need to call `gnus-get-newsgroup-headers-xover' with
		 ;; the entire .overview for group as we still have the just
		 ;; downloaded headers in `gnus-agent-overview-buffer'.
		 (let ((nntp-server-buffer gnus-agent-overview-buffer))
		   (setq gnus-newsgroup-headers
			 (gnus-get-newsgroup-headers-xover articles nil nil 
							   group)))
		 ;; `gnus-agent-overview-buffer' may be killed for
		 ;; timeout reason.  If so, recreate it.
		 (gnus-agent-create-buffer)))
      (setq category (gnus-group-category group))
      (setq predicate
	    (gnus-get-predicate
	     (or (gnus-group-find-parameter group 'agent-predicate t)
		 (cadr category))))
      ;; Do we want to download everything, or nothing?
      (if (or (eq (caaddr predicate) 'gnus-agent-true)
	      (eq (caaddr predicate) 'gnus-agent-false))
	  ;; Yes.
	  (setq arts (symbol-value
		      (cadr (assoc (caaddr predicate)
				   '((gnus-agent-true articles)
				     (gnus-agent-false nil))))))
	;; No, we need to decide what we want.
	(setq score-param
	      (let ((score-method
		     (or
		      (gnus-group-find-parameter group 'agent-score t)
		      (caddr category))))
		(when score-method
		  (require 'gnus-score)
		  (if (eq score-method 'file)
		      (let ((entries
			     (gnus-score-load-files
			      (gnus-all-score-files group)))
			    list score-file)
			(while (setq list (car entries))
			  (push (car list) score-file)
			  (setq list (cdr list))
			  (while list
			    (when (member (caar list)
					  gnus-agent-scoreable-headers)
			      (push (car list) score-file))
			    (setq list (cdr list)))
			  (setq score-param
				(append score-param (list (nreverse score-file)))
				score-file nil entries (cdr entries)))
			(list score-param))
		    (if (stringp (car score-method))
			score-method
		      (list (list score-method)))))))
	(when score-param
	  (gnus-score-headers score-param))
	(setq arts nil)
	(while (setq gnus-headers (pop gnus-newsgroup-headers))
	  (setq gnus-score
		(or (cdr (assq (mail-header-number gnus-headers)
			       gnus-newsgroup-scored))
		    gnus-summary-default-score))
	  (when (funcall predicate)
	    (push (mail-header-number gnus-headers)
		  arts))))
      ;; Fetch the articles.
      (when arts
	(gnus-agent-fetch-articles group arts)))
    ;; Perhaps we have some additional articles to fetch.
    (setq arts (assq 'download (gnus-info-marks
				(setq info (gnus-get-info group)))))
    (when (cdr arts)
      (gnus-agent-fetch-articles
       group (gnus-uncompress-range (cdr arts)))
      (setq marks (delq arts (gnus-info-marks info)))
      (gnus-info-set-marks info marks)
      (gnus-dribble-enter
       (concat "(gnus-group-set-info '"
	       (gnus-prin1-to-string info)
	       ")")))))

;;;
;;; Agent Category Mode
;;;

(defvar gnus-category-mode-hook nil
  "Hook run in `gnus-category-mode' buffers.")

(defvar gnus-category-line-format "     %(%20c%): %g\n"
  "Format of category lines.")

(defvar gnus-category-mode-line-format "Gnus: %%b"
  "The format specification for the category mode line.")

(defvar gnus-agent-short-article 100
  "Articles that have fewer lines than this are short.")

(defvar gnus-agent-long-article 200
  "Articles that have more lines than this are long.")

(defvar gnus-agent-low-score 0
  "Articles that have a score lower than this have a low score.")

(defvar gnus-agent-high-score 0
  "Articles that have a score higher than this have a high score.")


;;; Internal variables.

(defvar gnus-category-buffer "*Agent Category*")

(defvar gnus-category-line-format-alist
  `((?c gnus-tmp-name ?s)
    (?g gnus-tmp-groups ?d)))

(defvar gnus-category-mode-line-format-alist
  `((?u user-defined ?s)))

(defvar gnus-category-line-format-spec nil)
(defvar gnus-category-mode-line-format-spec nil)

(defvar gnus-category-mode-map nil)
(put 'gnus-category-mode 'mode-class 'special)

(unless gnus-category-mode-map
  (setq gnus-category-mode-map (make-sparse-keymap))
  (suppress-keymap gnus-category-mode-map)

  (gnus-define-keys gnus-category-mode-map
    "q" gnus-category-exit
    "k" gnus-category-kill
    "c" gnus-category-copy
    "a" gnus-category-add
    "p" gnus-category-edit-predicate
    "g" gnus-category-edit-groups
    "s" gnus-category-edit-score
    "l" gnus-category-list

    "\C-c\C-i" gnus-info-find-node
    "\C-c\C-b" gnus-bug))

(defvar gnus-category-menu-hook nil
  "*Hook run after the creation of the menu.")

(defun gnus-category-make-menu-bar ()
  (gnus-turn-off-edit-menu 'category)
  (unless (boundp 'gnus-category-menu)
    (easy-menu-define
     gnus-category-menu gnus-category-mode-map ""
     '("Categories"
       ["Add" gnus-category-add t]
       ["Kill" gnus-category-kill t]
       ["Copy" gnus-category-copy t]
       ["Edit predicate" gnus-category-edit-predicate t]
       ["Edit score" gnus-category-edit-score t]
       ["Edit groups" gnus-category-edit-groups t]
       ["Exit" gnus-category-exit t]))

    (gnus-run-hooks 'gnus-category-menu-hook)))

(defun gnus-category-mode ()
  "Major mode for listing and editing agent categories.

All normal editing commands are switched off.
\\<gnus-category-mode-map>
For more in-depth information on this mode, read the manual
(`\\[gnus-info-find-node]').

The following commands are available:

\\{gnus-category-mode-map}"
  (interactive)
  (when (gnus-visual-p 'category-menu 'menu)
    (gnus-category-make-menu-bar))
  (kill-all-local-variables)
  (gnus-simplify-mode-line)
  (setq major-mode 'gnus-category-mode)
  (setq mode-name "Category")
  (gnus-set-default-directory)
  (setq mode-line-process nil)
  (use-local-map gnus-category-mode-map)
  (buffer-disable-undo)
  (setq truncate-lines t)
  (setq buffer-read-only t)
  (gnus-run-hooks 'gnus-category-mode-hook))

(defalias 'gnus-category-position-point 'gnus-goto-colon)

(defun gnus-category-insert-line (category)
  (let* ((gnus-tmp-name (car category))
	 (gnus-tmp-groups (length (cadddr category))))
    (beginning-of-line)
    (gnus-add-text-properties
     (point)
     (prog1 (1+ (point))
       ;; Insert the text.
       (eval gnus-category-line-format-spec))
     (list 'gnus-category gnus-tmp-name))))

(defun gnus-enter-category-buffer ()
  "Go to the Category buffer."
  (interactive)
  (gnus-category-setup-buffer)
  (gnus-configure-windows 'category)
  (gnus-category-prepare))

(defun gnus-category-setup-buffer ()
  (unless (get-buffer gnus-category-buffer)
    (save-excursion
      (set-buffer (gnus-get-buffer-create gnus-category-buffer))
      (gnus-category-mode))))

(defun gnus-category-prepare ()
  (gnus-set-format 'category-mode)
  (gnus-set-format 'category t)
  (let ((alist gnus-category-alist)
	(buffer-read-only nil))
    (erase-buffer)
    (while alist
      (gnus-category-insert-line (pop alist)))
    (goto-char (point-min))
    (gnus-category-position-point)))

(defun gnus-category-name ()
  (or (get-text-property (gnus-point-at-bol) 'gnus-category)
      (error "No category on the current line")))

(defun gnus-category-read ()
  "Read the category alist."
  (setq gnus-category-alist
	(or (gnus-agent-read-file
	     (nnheader-concat gnus-agent-directory "lib/categories"))
	    (list (list 'default 'short nil nil)))))

(defun gnus-category-write ()
  "Write the category alist."
  (setq gnus-category-predicate-cache nil
	gnus-category-group-cache nil)
  (gnus-make-directory (nnheader-concat gnus-agent-directory "lib"))
  (with-temp-file (nnheader-concat gnus-agent-directory "lib/categories")
    (prin1 gnus-category-alist (current-buffer))))

(defun gnus-category-edit-predicate (category)
  "Edit the predicate for CATEGORY."
  (interactive (list (gnus-category-name)))
  (let ((info (assq category gnus-category-alist)))
    (gnus-edit-form
     (cadr info) (format "Editing the predicate for category %s" category)
     `(lambda (predicate)
	(setcar (cdr (assq ',category gnus-category-alist)) predicate)
	(gnus-category-write)
	(gnus-category-list)))))

(defun gnus-category-edit-score (category)
  "Edit the score expression for CATEGORY."
  (interactive (list (gnus-category-name)))
  (let ((info (assq category gnus-category-alist)))
    (gnus-edit-form
     (caddr info)
     (format "Editing the score expression for category %s" category)
     `(lambda (groups)
	(setcar (cddr (assq ',category gnus-category-alist)) groups)
	(gnus-category-write)
	(gnus-category-list)))))

(defun gnus-category-edit-groups (category)
  "Edit the group list for CATEGORY."
  (interactive (list (gnus-category-name)))
  (let ((info (assq category gnus-category-alist)))
    (gnus-edit-form
     (cadddr info) (format "Editing the group list for category %s" category)
     `(lambda (groups)
	(setcar (nthcdr 3 (assq ',category gnus-category-alist)) groups)
	(gnus-category-write)
	(gnus-category-list)))))

(defun gnus-category-kill (category)
  "Kill the current category."
  (interactive (list (gnus-category-name)))
  (let ((info (assq category gnus-category-alist))
	(buffer-read-only nil))
    (gnus-delete-line)
    (setq gnus-category-alist (delq info gnus-category-alist))
    (gnus-category-write)))

(defun gnus-category-copy (category to)
  "Copy the current category."
  (interactive (list (gnus-category-name) (intern (read-string "New name: "))))
  (let ((info (assq category gnus-category-alist)))
    (push (list to (gnus-copy-sequence (cadr info))
		(gnus-copy-sequence (caddr info)) nil)
	  gnus-category-alist)
    (gnus-category-write)
    (gnus-category-list)))

(defun gnus-category-add (category)
  "Create a new category."
  (interactive "SCategory name: ")
  (when (assq category gnus-category-alist)
    (error "Category %s already exists" category))
  (push (list category 'false nil nil)
	gnus-category-alist)
  (gnus-category-write)
  (gnus-category-list))

(defun gnus-category-list ()
  "List all categories."
  (interactive)
  (gnus-category-prepare))

(defun gnus-category-exit ()
  "Return to the group buffer."
  (interactive)
  (kill-buffer (current-buffer))
  (gnus-configure-windows 'group t))

;; To avoid having 8-bit characters in the source file.
(defvar gnus-category-not (list '! 'not (intern (format "%c" 172))))

(defvar gnus-category-predicate-alist
  '((spam . gnus-agent-spam-p)
    (short . gnus-agent-short-p)
    (long . gnus-agent-long-p)
    (low . gnus-agent-low-scored-p)
    (high . gnus-agent-high-scored-p)
    (true . gnus-agent-true)
    (false . gnus-agent-false))
  "Mapping from short score predicate symbols to predicate functions.")

(defun gnus-agent-spam-p ()
  "Say whether an article is spam or not."
  (unless gnus-agent-spam-hashtb
    (setq gnus-agent-spam-hashtb (gnus-make-hashtable 1000)))
  (if (not (equal (mail-header-references gnus-headers) ""))
      nil
    (let ((string (gnus-simplify-subject (mail-header-subject gnus-headers))))
      (prog1
	  (gnus-gethash string gnus-agent-spam-hashtb)
	(gnus-sethash string t gnus-agent-spam-hashtb)))))

(defun gnus-agent-short-p ()
  "Say whether an article is short or not."
  (< (mail-header-lines gnus-headers) gnus-agent-short-article))

(defun gnus-agent-long-p ()
  "Say whether an article is long or not."
  (> (mail-header-lines gnus-headers) gnus-agent-long-article))

(defun gnus-agent-low-scored-p ()
  "Say whether an article has a low score or not."
  (< gnus-score gnus-agent-low-score))

(defun gnus-agent-high-scored-p ()
  "Say whether an article has a high score or not."
  (> gnus-score gnus-agent-high-score))

(defun gnus-category-make-function (cat)
  "Make a function from category CAT."
  `(lambda () ,(gnus-category-make-function-1 cat)))

(defun gnus-agent-true ()
  "Return t."
  t)

(defun gnus-agent-false ()
  "Return nil."
  nil)

(defun gnus-category-make-function-1 (cat)
  "Make a function from category CAT."
  (cond
   ;; Functions are just returned as is.
   ((or (symbolp cat)
	(gnus-functionp cat))
    `(,(or (cdr (assq cat gnus-category-predicate-alist))
	   cat)))
   ;; More complex category.
   ((consp cat)
    `(,(cond
	((memq (car cat) '(& and))
	 'and)
	((memq (car cat) '(| or))
	 'or)
	((memq (car cat) gnus-category-not)
	 'not))
      ,@(mapcar 'gnus-category-make-function-1 (cdr cat))))
   (t
    (error "Unknown category type: %s" cat))))

(defun gnus-get-predicate (predicate)
  "Return the predicate for CATEGORY."
  (or (cdr (assoc predicate gnus-category-predicate-cache))
      (cdar (push (cons predicate
			(gnus-category-make-function predicate))
		  gnus-category-predicate-cache))))

(defun gnus-group-category (group)
  "Return the category GROUP belongs to."
  (unless gnus-category-group-cache
    (setq gnus-category-group-cache (gnus-make-hashtable 1000))
    (let ((cs gnus-category-alist)
	  groups cat)
      (while (setq cat (pop cs))
	(setq groups (cadddr cat))
	(while groups
	  (gnus-sethash (pop groups) cat gnus-category-group-cache)))))
  (or (gnus-gethash group gnus-category-group-cache)
      (assq 'default gnus-category-alist)))

(defun gnus-agent-expire ()
  "Expire all old articles."
  (interactive)
  (let ((methods gnus-agent-covered-methods)
	(day (- (time-to-days (current-time)) gnus-agent-expire-days))
	gnus-command-method sym group articles
	history overview file histories elem art nov-file low info
	unreads marked article orig lowest highest)
    (save-excursion
      (setq overview (gnus-get-buffer-create " *expire overview*"))
      (while (setq gnus-command-method (pop methods))
	(when (file-exists-p (gnus-agent-lib-file "active"))
	  (with-temp-buffer
	    (nnheader-insert-file-contents (gnus-agent-lib-file "active"))
	    (gnus-active-to-gnus-format 
	     gnus-command-method
	     (setq orig (gnus-make-hashtable
			 (count-lines (point-min) (point-max))))))
	  (let ((expiry-hashtb (gnus-make-hashtable 1023)))
	    (gnus-agent-open-history)
	    (set-buffer
	     (setq gnus-agent-current-history
		   (setq history (gnus-agent-history-buffer))))
	    (goto-char (point-min))
	    (when (> (buffer-size) 1)
	      (goto-char (point-min))
	      (while (not (eobp))
		(skip-chars-forward "^\t")
		(if (> (read (current-buffer)) day)
		    ;; New article; we don't expire it.
		    (forward-line 1)
		  ;; Old article.  Schedule it for possible nuking.
		  (while (not (eolp))
		    (setq sym (let ((obarray expiry-hashtb) s)
				(setq s (read (current-buffer)))
				(if (stringp s) (intern s) s)))
		    (if (boundp sym)
			(set sym (cons (cons (read (current-buffer)) (point))
				       (symbol-value sym)))
		      (set sym (list (cons (read (current-buffer)) (point)))))
		    (skip-chars-forward " "))
		  (forward-line 1)))
	      ;; We now have all articles that can possibly be expired.
	      (mapatoms
	       (lambda (sym)
		 (setq group (symbol-name sym)
		       articles (sort (symbol-value sym) 'car-less-than-car)
		       low (car (gnus-active group))
		       info (gnus-get-info group)
		       unreads (ignore-errors
				 (gnus-list-of-unread-articles group))
		       marked (nconc
			       (gnus-uncompress-range
				(cdr (assq 'tick (gnus-info-marks info))))
			       (gnus-uncompress-range
				(cdr (assq 'dormant
					   (gnus-info-marks info)))))
		       nov-file (gnus-agent-article-name ".overview" group)
		       lowest nil
		       highest nil)
		 (gnus-agent-load-alist group)
		 (gnus-message 5 "Expiring articles in %s" group)
		 (set-buffer overview)
		 (erase-buffer)
		 (when (file-exists-p nov-file)
		   (nnheader-insert-file-contents nov-file))
		 (goto-char (point-min))
		 (setq article 0)
		 (while (setq elem (pop articles))
		   (setq article (car elem))
		   (when (or (null low)
			     (< article low)
			     gnus-agent-expire-all
			     (and (not (memq article unreads))
				  (not (memq article marked))))
		     ;; Find and nuke the NOV line.
		     (while (and (not (eobp))
				 (or (not (numberp
					   (setq art (read (current-buffer)))))
				     (< art article)))
		       (if (and (numberp art) 
				(file-exists-p
				 (gnus-agent-article-name
				  (number-to-string art) group)))
			   (progn
			     (unless lowest
			       (setq lowest art))
			     (setq highest art)
			     (forward-line 1))
			 ;; Remove old NOV lines that have no articles.
			 (gnus-delete-line)))
		     (if (or (eobp)
			     (/= art article))
			 (beginning-of-line)
		       (gnus-delete-line))
		     ;; Nuke the article.
		     (when (file-exists-p
			    (setq file (gnus-agent-article-name
					(number-to-string article)
					group)))
		       (delete-file file))
		     ;; Schedule the history line for nuking.
		     (push (cdr elem) histories)))
		 (gnus-make-directory (file-name-directory nov-file))
		 (let ((coding-system-for-write
			gnus-agent-file-coding-system))
		   (write-region (point-min) (point-max) nov-file nil 'silent))
		 ;; Delete the unwanted entries in the alist.
		 (setq gnus-agent-article-alist
		       (sort gnus-agent-article-alist 'car-less-than-car))
		 (let* ((alist gnus-agent-article-alist)
			(prev (cons nil alist))
			(first prev)
			expired)
		   (while (and alist
			       (<= (caar alist) article))
		     (if (or (not (cdar alist))
			     (not (file-exists-p
				   (gnus-agent-article-name
				    (number-to-string
				     (caar alist))
				    group))))
			 (progn
			   (push (caar alist) expired)
			   (setcdr prev (setq alist (cdr alist))))
		       (setq prev alist
			     alist (cdr alist))))
		   (setq gnus-agent-article-alist (cdr first))
		   (gnus-agent-save-alist group)
		   ;; Mark all articles up to the first article
		   ;; in `gnus-article-alist' as read.
		   (when (and info (caar gnus-agent-article-alist))
		     (setcar (nthcdr 2 info)
			     (gnus-range-add
			      (nth 2 info)
			      (cons 1 (- (caar gnus-agent-article-alist) 1)))))
		   ;; Maybe everything has been expired from `gnus-article-alist'
		   ;; and so the above marking as read could not be conducted,
		   ;; or there are expired article within the range of the alist.
		   (when (and info
			      expired
			      (or (not (caar gnus-agent-article-alist))
				  (> (car expired)
				     (caar gnus-agent-article-alist))))
		     (setcar (nthcdr 2 info)
			     (gnus-add-to-range
			      (nth 2 info)
			      (nreverse expired))))
		   (gnus-dribble-enter
		    (concat "(gnus-group-set-info '"
			    (gnus-prin1-to-string info)
			    ")")))
		 (when lowest
		   (if (gnus-gethash group orig)
		       (setcar (gnus-gethash group orig) lowest)
		     (gnus-sethash group (cons lowest highest) orig))))
	       expiry-hashtb)
	      (set-buffer history)
	      (setq histories (nreverse (sort histories '<)))
	      (while histories
		(goto-char (pop histories))
		(gnus-delete-line))
	      (gnus-agent-save-history)
	      (gnus-agent-close-history)
	      (gnus-write-active-file
	       (gnus-agent-lib-file "active") orig))
	    (gnus-message 4 "Expiry...done")))))))

;;;###autoload
(defun gnus-agent-batch ()
  (interactive)
  (let ((init-file-user "")
	(gnus-always-read-dribble-file t))
    (gnus))
  (gnus-group-send-drafts)
  (gnus-agent-fetch-session))

(provide 'gnus-agent)

;;; gnus-agent.el ends here
