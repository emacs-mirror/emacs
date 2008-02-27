;;; nnmairix.el --- Mairix back end for Gnus, the Emacs newsreader

;; Copyright (C) 2007, 2008  Free Software Foundation, Inc.

;; Author: David Engster <dengste@eml.cc>
;; Keywords: mail searching
;; Version: 0.5

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; THIS IS BETA SOFTWARE! This back end should not mess up or
;; even delete your mails, but having a backup is always a good idea.

;; This is a back end for using the mairix search engine with
;; Gnus.  Mairix is a tool for searching words in locally stored
;; mail.  Mairix is very fast which allows using it efficiently for
;; "smart folders", e.g. folders which are associated with search
;; queries.  Of course, you can also use this back end just for
;; calling mairix with some search query.
;;
;; Mairix is written by Richard Curnow.  More information can be found at
;; http://www.rpcurnow.force9.co.uk/mairix/
;;
;; For details about setting up mairix&Gnus&nnmairix.el, look at the
;; emacswiki:
;;
;; http://www.emacswiki.org/cgi-bin/wiki/GnusMairix
;;
;; The newest version of nnmairix.el can be found at
;; 
;; http://www.emacswiki.org/cgi-bin/emacs/nnmairix.el

;; For impatient people, here's the setup in a nutshell:
;;
;; This back end requires an installed mairix binary which is
;; configured to index your mail folder.  You don't have to specify a
;; search folder (but it does no harm, either).  Visit the man page of
;; mairix and mairixrc for details.
;;
;; Put nnmairix.el into your search path and "(require 'nnmarix)" into
;; your .gnus.  Then call nnmairix-create-default-group (or 'G b
;; c'). This function will ask for all necessary information to create
;; a mairix server in Gnus with the default search folder.  This
;; default search folder will be used for all temporary searches: call
;; nnmairix-search ('G b s') and enter a mairix query (like
;; f:test@example.com). To create a mairix group for one specific
;; search query, use 'G b g'.  See the emacswiki or the source for more
;; information.

;; Commentary on the code: nnmairix sits between Gnus and the "real"
;; back end which handles the mail (currently nnml, nnimap and
;; nnmaildir were tested). I know this is all a bit hacky, but so far
;; it works for me.  This is the first back end I've written for Gnus,
;; so I'd appreciate any comments, suggestions, bug reports (and, of
;; course, patches) for improving nnmairix.

;; nnmairix does not use an active file, since I wanted to contain the
;; back end "inside Gnus" as much as possible without the need of an
;; external file.  It stores the query/folder information in the group
;; parameters instead.  This also implies that once you kill a mairix
;; group, it's gone for good.  I don't think that this is really
;; problematic, since I don't see the need in unsubscribing and
;; re-subscribing search groups

;; Every mairix server is "responsible" for one mairix installation,
;; i.e. you can have several mairix servers for different mairix
;; configurations.  Not that I think anyone will actually do this, but
;; I thought it would be a "nice to have feature"...

;; KNOWN BUGS:
;; * When using Maildir: path and filename of a mail can change due to
;; reading/replying/etc.  This can lead to dangling symlinks in
;; nnmairix groups and it depends on the back end how well it deals
;; with that (some IMAP servers out there may not be amused).  Update the
;; database ('G b u') and the group to fix it.
;; * Mairix does only support us-ascii characters.

;; TODO/MISSING FEATURES:
;; * Possibility to propagate flags like seen, replied, ticked
;;   to original message
;; * Support of more back ends (nnmh, nnfolder, nnmbox...)?
;; * Maybe use an active file instead of group parameters?
;; * Use "-a" when updating groups which are not newly created
 
;;; Changelog:
;;
;; 02/06/2008 - version 0.5
;; 
;;    * New function: nnmairix-goto-original-article. Uses the
;;      registry or the mail file path for determining original group.
;; 
;;    * Deal with empty Xref header
;;
;;    * Changed summary mode keybindings since the old ones were
;;      already taken
;;
;;   (Thanks to Tassilo Horn and Ted Zlatanov for their help)
;;
;; 01/07/2008 - version 0.4
;;
;;    * New/fixed doc strings and code cleanup.
;;
;; 18/11/2007 - version 0.3
;;
;;    * Fixed bugs when dealing with nnml and native servers
;;     
;;    * Make variables customizable
;;
;; 10/10/2007 - version 0.2
;;
;;    * Use nnml-directory/directory server variables for nnml and
;;    nnmaildir backends as path for search folders. This way it
;;    becomes independent of 'base' setting in .mairixirc (but not for
;;    nnimap).
;;
;;    * As a result: Changed nnmairix-backend-to-server so that user
;;    is asked when more than one nnmairix server exists and we do not
;;    know which one is responsible for current backend.
;;
;;    * Rename files when using nnml backends so that there are no
;;    holes in article numbers. This should fix all problems regarding
;;    wrong article counts with nnml.
;;
;;    * More commands for creating queries (using widgets or the
;;    minibuffer).
;;
;;    * Fixed bug in nnmairix-create-search-group-from-message
;;
;;    * Changed copyright to FSF
;;
;;      (Thanks to Georg C. F. Greve and Bastien for suggestions and
;;      ideas!)
;;
;; 10/03/2007 - version 0.1 - first release


;;; Code:

(require 'nnoo)
(require 'gnus-group)
(require 'gnus-sum)
(require 'message)
(require 'nnml)
(require 'widget)

(nnoo-declare nnmairix)

;;; === Keymaps

;; Group mode
(defun nnmairix-group-mode-hook ()
  "Nnmairix group mode keymap."
  (define-key gnus-group-mode-map
    (kbd "G b") (make-sparse-keymap))
  (define-key gnus-group-mode-map
    (kbd "G b g") 'nnmairix-create-search-group)
  (define-key gnus-group-mode-map
    (kbd "G b c") 'nnmairix-create-server-and-default-group)
  (define-key gnus-group-mode-map
    (kbd "G b q") 'nnmairix-group-change-query-this-group)
  (define-key gnus-group-mode-map
    (kbd "G b t") 'nnmairix-group-toggle-threads-this-group)
  (define-key gnus-group-mode-map
    (kbd "G b u") 'nnmairix-update-database)
  (define-key gnus-group-mode-map
    (kbd "G b s") 'nnmairix-search)
  (define-key gnus-group-mode-map
    (kbd "G b i") 'nnmairix-search-interactive)
  (define-key gnus-group-mode-map
    (kbd "G b m") 'nnmairix-widget-search))

;; Summary mode
(defun nnmairix-summary-mode-hook ()
  "Nnmairix summary mode keymap."
  (define-key gnus-summary-mode-map
    (kbd "$ t") 'nnmairix-search-thread-this-article)
  (define-key gnus-summary-mode-map
    (kbd "$ f") 'nnmairix-search-from-this-article)
  (define-key gnus-summary-mode-map
    (kbd "$ m") 'nnmairix-widget-search-from-this-article)
  (define-key gnus-summary-mode-map
    (kbd "$ g") 'nnmairix-create-search-group-from-message)
  (define-key gnus-summary-mode-map
    (kbd "$ o") 'nnmairix-goto-original-article))

(add-hook 'gnus-group-mode-hook 'nnmairix-group-mode-hook)
(add-hook 'gnus-summary-mode-hook 'nnmairix-summary-mode-hook)


;; Customizable stuff

(defgroup nnmairix nil
  "Backend for the Mairix mail search engine."
  :group 'gnus)

(defcustom nnmairix-group-prefix "zz_mairix"
  "Prefix for mairix search groups on back end server.
nnmairix will create these groups automatically on the back end
server for each nnmairix search group.  The name on the back end
server will be this prefix plus a random number.  You can delete
unused nnmairix groups on the back end using
`nnmairix-purge-old-groups'."
  :version "23.0"
  :type 'string
  :group 'nnmairix)

(defcustom nnmairix-mairix-output-buffer "*mairix output*"
  "Buffer used for mairix output."
  :version "23.0"
  :type 'string
  :group 'nnmairix)

(defcustom nnmairix-customize-query-buffer "*mairix query*"
  "Name of the buffer for customizing Mairix queries."
  :version "23.0"
  :type 'string
  :group 'nnmairix)

(defcustom nnmairix-mairix-update-options '("-F" "-Q")
  "Options when calling mairix for updating the database.
The default is '-F' and '-Q' for making updates faster.  You
should call mairix without these options from time to
time (e.g. via cron job)."
  :version "23.0"
  :type '(repeat string)
  :group 'nnmairix)

(defcustom nnmairix-mairix-synchronous-update nil
  "Set this to t if you want Emacs to wait for mairix updating the database."
  :version "23.0"
  :type 'boolean
  :group 'nnmairix)

(defcustom nnmairix-rename-files-for-nnml t
  "Rename nnml mail files so that they are consecutively numbered.
When using nnml as backend, mairix might produce holes in the
article numbers which will produce wrong article counts by
Gnus.  This option controls whether nnmairix should rename the
files consecutively."
  :version "23.0"
  :type 'boolean
  :group 'nnmairix)

(defcustom nnmairix-widget-fields-list
  '(("from" "f" "From") ("to" "t" "To") ("cc" "c" "Cc")
    ("subject" "s" "Subject")  ("to" "tc" "To or Cc")
    ("from" "a" "Address") (nil "b" "Body") (nil "n" "Attachment")
    ("Message-ID" "m" "Message ID") (nil "s" "Size") (nil "d" "Date"))
  "Fields that should be editable during interactive query customization.

Header, corresponding mairix command and description for editable
fields in interactive query customization.  The header specifies
which header contents should be inserted into the editable field
when creating a Mairix query based on the current message (can be
nil for disabling this)."
  :version "23.0"
  :type '(repeat (list
		  (choice :tag "Field"
			  (const :tag "none" nil)
			  (const :tag "From" "from")
			  (const :tag "To" "to")
			  (const :tag "Cc" "cc")
			  (const :tag "Subject" "subject")
			  (const :tag "Message ID" "Message-ID"))
		  (string :tag "Command")
		  (string :tag "Description")))
  :group 'nnmairix)

(defcustom nnmairix-widget-select-window-function
  (lambda () (select-window (get-largest-window)))
  "Function for selecting the window for customizing the mairix query.
The default chooses the largest window in the current frame."
  :version "23.0"
  :type 'function
  :group 'nnmairix)

;; ==== Other variables

(defvar nnmairix-widget-other
  '(threads flags)
  "Other editable mairix commands when using customization widgets.
Currently there are 'threads and 'flags.")

(defvar nnmairix-interactive-query-parameters
  '((?f "from" "f" "From") (?t "to" "t" "To") (?c "to" "tc" "To or Cc")
    (?a "from" "a" "Address") (?s "subject" "s" "Subject") (?b nil "b" "Body")
    (?d nil "d" "Date") (?n nil "n" "Attachment"))
  "Things that should be editable during interactive query generation.
Every list element consists of the following entries: Keystroke,
message field (if any), mairix command and description.")

(defvar nnmairix-delete-and-create-on-change '(nnimap nnmaildir nnml)
  "Controls on which backends groups should be deleted and re-created.
This variable is a list of back ends where the search group should
be completely deleted and re-created when the query or thread
parameter changes.  I know this is rather \"brute force\" and maybe
even dangerous (you have backups, right?), but it should be used at
least for nnimap since some IMAP servers are really not amused when
mailbox content changes behind their back.  It usually also corrects
the problem of \"holes\" in the article numbers which often lead to a
wrong count of total articles shown by Gnus.")

;;; === Server variables

(defvoo nnmairix-backend  nil
  "Backend where mairix stores its searches.")

(defvoo nnmairix-backend-server nil
  "Name of the server where mairix stores its searches.")

(defvoo nnmairix-mairix-command "mairix"
  "Command to call mairix for this nnmairix server.")

(defvoo nnmairix-hidden-folders nil
  "Set this to t if the back end server uses hidden directories for
its maildir mail folders (e.g. the Dovecot IMAP server or mutt).")

(defvoo nnmairix-default-group nil
  "Default search group. This is the group which is used for all
temporary searches, e.g. nnmairix-search.")

;;; === Internal variables

;; Regexp for mairix groups on back end
(setq nnmairix-group-regexp (format "%s-\\(.*\\)-[0-9]+" nnmairix-group-prefix))

;; Back ends (hopefully...) supported by nnmairix.
;; Other backends might or might not work.
(setq nnmairix-valid-backends '(nnimap nnml nnmaildir))

;; Last chosen server
(setq nnmairix-last-server nil)

;; Current server
(setq nnmairix-current-server nil)

;;; === Gnus backend functions
  
(nnoo-define-basics nnmairix)

(gnus-declare-backend "nnmairix" 'mail 'address)

(deffoo nnmairix-open-server (server &optional definitions)
  ;; just set server variables
  (setq nnmairix-current-server server)
  (nnoo-change-server 'nnmairix server definitions))

(deffoo nnmairix-request-group (group &optional server fast)
  ;; Call mairix and request group on back end server
  (when server (nnmairix-open-server server))
  (let* ((qualgroup (if server
			(gnus-group-prefixed-name group (list 'nnmairix server))
		      group))
	 (query (gnus-group-get-parameter qualgroup 'query t))
	(folder (gnus-group-get-parameter qualgroup 'folder))
	(threads (gnus-group-get-parameter qualgroup 'threads))
	(backendmethod (gnus-server-to-method
			(format "%s:%s" (symbol-name nnmairix-backend)
				nnmairix-backend-server)))
	rval mfolder folderpath)
    (cond
     ((not folder)
      ;; No folder parameter -> error
      (nnheader-report 'nnmairix "Check folder parameter for group %s" group)
      nil)
     ((not query)
      ;; No query -> return empty group
      (save-excursion
	(set-buffer nntp-server-buffer)
	(erase-buffer)
	(insert (concat "211 0 1 0 " group))
	t))
     (t
      ;; For maildir++ folders: create a hidden directory (prepend dot)
      (setq mfolder (if (and nnmairix-hidden-folders
			     (not (string-match "^\\." folder)))
			(concat "." folder)
		      folder))
      ;; For nnml and nnmaildir, precede mfolder with directory where mail
      ;; is actually stored so that it's independent of 'base' setting
      ;; in .mairixrc.
      (when (eq nnmairix-backend 'nnml)
	(setq folderpath (cadr (assoc 'nnml-directory backendmethod)))
	;; if nnml-directory is not explicitly set, use global value
	(when (not folderpath)
	  (setq folderpath nnml-directory)))
      (when (eq nnmairix-backend 'nnmaildir)
	(setq folderpath
	      (cadr (assoc 'directory backendmethod))))
      (when folderpath
	(setq mfolder
	      (concat
	       (file-name-as-directory
		(expand-file-name
		 folderpath))
	       mfolder)))
      ;; If (not fast), call Mairix binary
      (setq rval
	    (if fast 0
	      (nnmairix-call-mairix-binary
	       (split-string nnmairix-mairix-command)
	       mfolder query threads)))
      ;; Check return value
      (cond
       ((zerop rval)			; call was succesful
	(nnmairix-call-backend
	 "open-server" nnmairix-backend-server)
	;; If we're dealing with nnml, rename files
	;; consecutively and make new active file for this
	;; group
	(when (eq nnmairix-backend 'nnml)
	  (when nnmairix-rename-files-for-nnml
	    (nnmairix-rename-files-consecutively mfolder))
	  (nnml-generate-nov-databases-directory mfolder))
	(nnmairix-call-backend
	 "request-scan" folder nnmairix-backend-server)
	(if fast
	    t
	  (nnmairix-request-group-with-article-number-correction folder qualgroup)))
       ((and (= rval 1)
	     (save-excursion (set-buffer nnmairix-mairix-output-buffer)
			     (goto-char (point-min))
			     (looking-at "^Matched 0 messages")))
	;; No messages found -> return empty group
	(nnheader-message 5 "Mairix: No matches found.")
	(set-buffer nntp-server-buffer)
	(erase-buffer)
	(insert (concat "211 0 1 0 " group))
	t)
       ;; Everything else is an error
       (t
	(nnheader-report
	 'nnmairix "Error running marix. See buffer %s for details"
	 nnmairix-mairix-output-buffer)
	nil))))))


(deffoo nnmairix-request-create-group (group &optional server args)
  (let ((qualgroup (if server (gnus-group-prefixed-name group (list 'nnmairix server))
		     group))
	(exist t)
	(count 0)
	groupname info)
    (when server (nnmairix-open-server server))
    (gnus-group-add-parameter qualgroup '(query . nil))
    (gnus-group-add-parameter qualgroup '(threads . nil))
    (while exist
      (setq count (1+ count))
      (setq groupname (format "%s-%s-%s" nnmairix-group-prefix group
			      (number-to-string count)))
      (setq exist (nnmairix-call-backend
		   "request-group" groupname nnmairix-backend-server)))
    (nnmairix-call-backend
     "request-create-group" groupname nnmairix-backend-server)
    (gnus-group-add-parameter qualgroup '(folder . nil))
    (gnus-group-set-parameter qualgroup 'folder groupname))
  t)


(deffoo nnmairix-retrieve-headers (articles group &optional server fetch-old)
  (when server (nnmairix-open-server server))
  (let* ((folder (nnmairix-get-backend-folder group server))
	 (corr (nnmairix-get-numcorr group server))
	 (numcorr 0)
	 rval)
    (when (and corr
	       (not (zerop (cadr corr)))
	       (numberp (car articles)))
      (setq numcorr (cadr corr))
      (setq articles
	    (mapcar
	     (lambda (arg) (- arg numcorr))
	     articles)))
    (setq rval (nnmairix-call-backend
		"retrieve-headers" articles folder nnmairix-backend-server fetch-old))
    (when (eq rval 'nov)
      (nnmairix-replace-group-and-numbers articles folder group numcorr)
      rval)))

(deffoo nnmairix-request-article (article &optional group server to-buffer)
  (when server (nnmairix-open-server server))
  (let ((folder (nnmairix-get-backend-folder group server))
	(corr (nnmairix-get-numcorr group server)))
    (when (and
	   (numberp article)
	   corr
	   (not (zerop (cadr corr))))
      (setq article (- article (cadr corr))))
    (nnmairix-call-backend
     "request-article" article folder nnmairix-backend-server to-buffer))
  t)

(deffoo nnmairix-close-group (group &optional server)
  ;; Should we do something here?
  nil)


(deffoo nnmairix-request-list (&optional server)
  (when server (nnmairix-open-server server))
  (if (nnmairix-call-backend "request-list" nnmairix-backend-server)
      (let (cpoint cur qualgroup folder)
	(save-excursion
	  (set-buffer nntp-server-buffer)
	  (goto-char (point-min))
	  (setq cpoint (point))
	  (while (re-search-forward nnmairix-group-regexp (point-max) t)
	    (setq cur (match-string 1)
		  qualgroup (gnus-group-prefixed-name cur
						      (list 'nnmairix server)))
	    (if (and (gnus-group-entry qualgroup)
		     (string= (match-string 0)
			      (gnus-group-get-parameter qualgroup 'folder)))
		(progn
		  (replace-match cur)
		  (delete-region cpoint (point-at-bol))
		  (forward-line)
		  (setq cpoint (point)))
	      (forward-line)))
	  (delete-region cpoint (point-max)))
	t)
    nil))

	         
(nnoo-define-skeleton nnmairix)


;;; === Interactive functions

(defun nnmairix-create-search-group (server group query threads)
  "Create on SERVER nnmairix search group GROUP with QUERY.
If THREADS is t, include whole threads from found messages.  If
called interactively, user will be asked for parameters."
  (interactive
   (list
    (gnus-server-to-method (car (nnmairix-get-server)))
    (read-string "Group name: ")
    (read-string "Query: ")
    (y-or-n-p "Include threads? ")))
  (when (and (stringp query)
	     (string-match "\\s-" query))
    (setq query (split-string query)))
  (when (not (listp query))
    (setq query (list query)))
  (when (and server group query)
    (save-excursion
      (let ((groupname (gnus-group-prefixed-name group server))
	    info)
	(set-buffer gnus-group-buffer)
	(gnus-group-make-group group server)
	(gnus-group-set-parameter groupname 'query  query)
	(gnus-group-set-parameter groupname 'threads threads)
	(nnmairix-update-and-clear-marks groupname)))))

(defun nnmairix-search-interactive ()
  "Create mairix search interactively with the minibuffer."
  (interactive)
  (let ((char-header nnmairix-interactive-query-parameters)
	header finished query achar)
    (while (not finished)
      (while (not achar)
	(message "Query (%s): " (nnmairix-create-message-line-for-search))
	  (setq achar (read-char))
	  (when (not (assoc achar char-header))
	    (setq achar nil)))
      (setq header (read-string
		    (concat "Match " (nth 3 (assoc achar char-header)) " on: ")))
	(push  (concat (nth 2 (assoc achar char-header)) ":" header) query)
	(setq finished (not (y-or-n-p "Add another search query? "))
	      achar nil))
    (nnmairix-search
     (mapconcat 'identity query " ")
     (car (nnmairix-get-server))
     (y-or-n-p "Include whole threads? "))))

(defun nnmairix-create-search-group-from-message ()
  "Interactively create search group with query based on current message."
  (interactive)
  (let ((char-header nnmairix-interactive-query-parameters)
	(server (nnmairix-backend-to-server gnus-current-select-method))
	 query achar header finished group threads cq)
    (when (or (not (gnus-buffer-live-p gnus-article-buffer))
	      (not (gnus-buffer-live-p gnus-summary-buffer)))
      (error "No article or summary buffer"))
    (when (not server)
      (error "No nnmairix server found for back end %s:%s"
	     (symbol-name (car gnus-current-select-method))
	     (nth 1 gnus-current-select-method)))
    (while (not finished)
      (save-excursion
	(gnus-summary-toggle-header 1)
	(while (not achar)
	  (message "Query (%s): " (nnmairix-create-message-line-for-search))
	  (setq achar (read-char))
	  (when (not (assoc achar char-header))
	    (setq achar nil)))
	(set-buffer gnus-article-buffer)
	(setq header nil)
	(when (setq cq (nth 1 (assoc achar char-header)))
	  (setq header
		(nnmairix-replace-illegal-chars
		 (gnus-fetch-field (nth 1 (assoc achar char-header))))))
	(setq header (read-string
		      (concat "Match " (nth 3 (assoc achar char-header)) " on: ")
		      header))
	(push  (concat (nth 2 (assoc achar char-header)) ":" header) query)
	(setq finished (not (y-or-n-p "Add another search query? "))
	      achar nil)))
    (setq threads (y-or-n-p "Include whole threads? "))
    (setq group (read-string "Group name: "))
    (set-buffer gnus-summary-buffer)
    (message "Creating group %s on server %s with query %s." group
	     (gnus-method-to-server server) (mapconcat 'identity query " "))
    (nnmairix-create-search-group server group query threads)))

(defun nnmairix-create-server-and-default-group ()
  "Interactively create new nnmairix server with default search group.
All necessary information will be queried from the user."
  (interactive)
  (let* ((name (read-string "Name of the mairix server: "))
	(server (completing-read "Back end server (TAB for completion): "
				 (nnmairix-get-valid-servers)))
	(mairix (read-string "Command to call mairix: " "mairix"))
	(defaultgroup (read-string "Default search group: "))
	(backend (symbol-name (car (gnus-server-to-method server))))
	(servername (nth 1 (gnus-server-to-method server)))
	(hidden (and (string-match "^nn\\(imap\\|maildir\\)$" backend)
		     (y-or-n-p
		      "Does the back end server work with maildir++ (i.e. hidden directories)? ")))
	create)

    (apply (intern (format "%s-%s" backend "open-server"))
	   (list servername))

    (when (and hidden
	       (string-match "^\\." defaultgroup))
      (setq defaultgroup (substring defaultgroup 1)))
    ;; Create default search group
    (gnus-group-make-group
     defaultgroup (list 'nnmairix name  (list 'nnmairix-backend (intern backend))
			(list 'nnmairix-backend-server servername)
			(list 'nnmairix-mairix-command mairix)
			(list 'nnmairix-hidden-folders hidden)
			(list 'nnmairix-default-group defaultgroup)))))


(defun nnmairix-group-change-query-this-group (&optional query)
  "Set QUERY for group under cursor."
  (interactive)
  (let* ((group (gnus-group-group-name))
	 (method (gnus-find-method-for-group group))
	 (oldquery (gnus-group-get-parameter group 'query t)))
    (if (eq (car method) 'nnmairix)
	(progn
	  (when (listp oldquery)
	    (setq oldquery (mapconcat 'identity oldquery " ")))
	  (setq query (or query
			  (read-string "New query: " oldquery)))
	  (when (stringp query)
	    (setq query (split-string query)))
	  (when query
	    (gnus-group-set-parameter group 'query query)
	    (nnmairix-update-and-clear-marks group)))
      (error "This is no nnmairix group"))))
		  

(defun nnmairix-group-toggle-threads-this-group (&optional threads)
  "Toggle threads parameter for this group.
If THREADS is a positive number, set threads parameter to t.
If THREADS is a negative number, set it to nil."
  (interactive)
  (let* ((group (gnus-group-group-name))
	 (method (gnus-find-method-for-group group))
	 (getthreads (or threads
			(not (gnus-group-get-parameter group 'threads)))))
    (if (eq (car method) 'nnmairix)
	(progn
	  (when (numberp getthreads)
	    (setq getthreads (> getthreads 0)))
	  (gnus-group-set-parameter group 'threads getthreads)
	  (if getthreads
	      (message "Threads activated for group %s" group)
	    (message "Threads deacitavted for group %s" group))
	  (nnmairix-update-and-clear-marks group))
      (error "This is no nnmairix group"))))
        

(defun nnmairix-search (query &optional server threads)
  "Sends QUERY to nnmairix backend SERVER, using default its search group.

Default search group is automatically entered and results are shown.
If THREADS is t, enable threads.
If THREADS is a negative number, disable threads.
Otherwise, leave threads parameter as it is."
  (interactive (list (read-string "Query: ")))
  (when (not server)
    (setq server (car (nnmairix-get-server))))
  (if (not server)
      (error "No opened nnmairix server found")
    (setq server (gnus-server-to-method server)))
  (nnmairix-open-server (nth 1 server))
  (let* ((qualgroup (gnus-group-prefixed-name nnmairix-default-group
					      (list 'nnmairix (nth 1 server)))))
    (set-buffer gnus-group-buffer)
    (when (stringp query)
      (setq query (split-string query)))
    (gnus-group-set-parameter qualgroup 'query query)
    (if (symbolp threads)
	(when (eq threads 't)
	  (gnus-group-set-parameter qualgroup 'threads t))
      (when (< threads 0)
	(gnus-group-set-parameter qualgroup 'threads nil)))
    (nnmairix-update-and-clear-marks qualgroup)
    (when (not (zerop (gnus-group-unread qualgroup)))
      (gnus-group-read-group nil t qualgroup))))

(defun nnmairix-search-thread-this-article ()
  "Search thread for the current article.
This is effectively a shortcut for calling `nnmairix-search'
with m:msgid of the current article and enabled threads."
  (interactive)
  (let* ((server
	  (nnmairix-backend-to-server gnus-current-select-method))
	 mid)
    (if server
	(if (gnus-buffer-live-p gnus-article-buffer)
	    (progn
	      (save-excursion
		(set-buffer gnus-article-buffer)
		(gnus-summary-toggle-header 1)
		(setq mid (message-fetch-field "Message-ID")))
	      (while (string-match "[<>]" mid)
		(setq mid (replace-match "" t t mid)))
	      (nnmairix-search (concat "m:" mid) server t))
	  (message "No article buffer."))
      (error "No nnmairix server found for back end %s:%s"
	     (symbol-name (car gnus-current-select-method))
	     (nth 1 gnus-current-select-method)))))

(defun nnmairix-search-from-this-article ()
  "Search messages from sender of the current article.
This is effectively a shortcut for calling `nnmairix-search' with
f:current_from."
  (interactive)
  (let* ((server
	  (nnmairix-backend-to-server gnus-current-select-method))
	 from)
    (if server
	(if (gnus-buffer-live-p gnus-article-buffer)
	    (progn
	      (save-excursion
		(set-buffer gnus-article-buffer)
		(gnus-summary-toggle-header 1)
		(setq from (cadr (gnus-extract-address-components
				  (gnus-fetch-field "From"))))
		(nnmairix-search (concat "f:" from) server -1)))
	  (message "No article buffer."))
      (error "No nnmairix server found for back end %s:%s"
	     (symbol-name (car gnus-current-select-method))
	     (nth 1 gnus-current-select-method)))))


(defun nnmairix-purge-old-groups (&optional dontask server)
  "Delete mairix search groups which are no longer used.

You may want to call this from time to time if you are creating
and deleting lots of nnmairix groups.  If DONTASK is t, do not ask
before deleting a group on the back end.  SERVER specifies nnmairix server."
  (interactive)
  (let ((server (or server
		    (gnus-server-to-method (car (nnmairix-get-server))))))
    (if (nnmairix-open-server (nth 1 server))
	(when (nnmairix-call-backend
	       "request-list" nnmairix-backend-server)
	  (let (cur qualgroup folder)
	    (save-excursion
	      (set-buffer nntp-server-buffer)
	      (goto-char (point-min))
	      (while (re-search-forward nnmairix-group-regexp (point-max) t)
		(setq cur (match-string 0)
		      qualgroup (gnus-group-prefixed-name
				 (match-string 1) server))
		(when (not (and (gnus-group-entry qualgroup)
				(string= cur
					 (gnus-group-get-parameter
					  qualgroup 'folder))))
		  (when (or dontask
			    (y-or-n-p
			     (concat "Delete group " cur
				     " on server " nnmairix-backend-server "? ")))
		    (nnmairix-call-backend
		     "request-delete-group" cur t nnmairix-backend-server)))))))
      (message "Couldn't open server %s" (nth 1 server)))))


(defun nnmairix-update-database (&optional servers)
  "Call mairix for updating the database for SERVERS.

If SERVERS is nil, do update for all nnmairix servers.  Mairix
will be called asynchronously unless
`nnmairix-mairix-synchronous-update' is t.  Mairix will be called
with `nnmairix-mairix-update-options'."
  (interactive)
  (let ((servers (or servers
		     (nnmairix-get-nnmairix-servers)))
	args cur commandsplit)
    (while servers
      (setq cur (car (pop servers)))
      (nnmairix-open-server
       (nth 1 (gnus-server-to-method cur)))
      (setq commandsplit (split-string nnmairix-mairix-command))
      (nnheader-message 7 "Updating mairix database for %s..." cur)
      (if nnmairix-mairix-synchronous-update
	  (progn
	    (setq args (append (list (car commandsplit) nil
				     (get-buffer nnmairix-mairix-output-buffer)
				     nil)))
	    (if (> (length commandsplit) 1)
		(setq args (append args (cdr commandsplit) nnmairix-mairix-update-options))
	      (setq args (append args nnmairix-mairix-update-options)))
	    (apply 'call-process args)
	    (nnheader-message 7 "Updating mairix database for %s... done" cur))
	(progn
	  (setq args (append (list cur (get-buffer nnmairix-mairix-output-buffer)
				   (car commandsplit))))
	  (if (> (length commandsplit) 1)
	      (setq args (append args (cdr commandsplit) nnmairix-mairix-update-options))
	    (setq args (append args nnmairix-mairix-update-options)))
	  (set-process-sentinel (apply 'start-process args)
				'nnmairix-sentinel-mairix-update-finished))))))

(defun nnmairix-goto-original-article (&optional no-registry)
  "Jump to the original group and display article.
The original group of the article is first determined with the
registry (if enabled). If the registry is not enabled or did not
find the article or the prefix NO-REGISTRY is non-nil, this
function will try to determine the original group form the path
of the mail file. The path is obtained through another mairix
search in raw mode."
  (interactive "P")
  (when (not (eq (car gnus-current-select-method) 'nnmairix))
    (let ((method (gnus-find-method-for-group gnus-newsgroup-name)))
      (if (eq (car method) 'nnmairix)
	  (nnmairix-open-server (nth 1 method))
	(error "Not in a nnmairix group"))))
  (when (not (gnus-buffer-live-p gnus-article-buffer))
    (error "No article buffer available"))
  (let ((server (nth 1 gnus-current-select-method))
	mid rval group allgroups)
    ;; get message id
    (save-excursion
      (set-buffer gnus-article-buffer)
      (gnus-summary-toggle-header 1)
      (setq mid (message-fetch-field "Message-ID"))
      ;; first check the registry (if available)
      (when (and (boundp 'gnus-registry-install)
		 gnus-registry-install
		 (not no-registry))
	(setq group (gnus-registry-fetch-group mid)))
      (while (string-match "[<>]" mid)
	(setq mid (replace-match "" t t mid)))
      (unless group
	;; registry was not available or did not find article
	;; so we search again with mairix in raw mode to get filename
	(nnmairix-open-server server)
	(setq rval 
	      (nnmairix-call-mairix-binary-raw 
	       (split-string nnmairix-mairix-command) 
	       (list (concat "m:" mid))))
	(if (zerop rval)
	    ;; determine original group(s) from filename
	    (save-excursion
	      (set-buffer nnmairix-mairix-output-buffer)
	      (goto-char (point-min))
	      (while (looking-at "/")
		(push (nnmairix-determine-original-group)
		      allgroups)
		(forward-line 1))
	      (if (> (length allgroups) 1)
		  (setq group 
			(completing-read 
			 "Message exists in more than one group. Choose: " 
			 allgroups nil t))
		(setq group (car allgroups))))
	  (error "Mairix could not find original article. See buffer %s for details" 
		 nnmairix-mairix-output-buffer))))
    (if group
	;; show article in summary buffer
	(nnmairix-show-original-article group mid)
      (message "Couldn't find original article"))))

(defun nnmairix-determine-original-group ()
  "Try to determine to original group from the file path."
  (let (path filename serverbase group maildirflag allgroups)
    (re-search-forward "^\\(.*\\)/\\(.*?\\)$")
    (setq path (expand-file-name (match-string 1)))
    (setq filename (match-string 2))
    ;; when we deal with maildir, remove cur/new/tmp from path
    (setq maildirflag (string-match ".+\\..+\\..+" filename))
    (when maildirflag
      (setq path
	    (replace-regexp-in-string 
	     ".*\\(/cur\\|/new\\|/tmp\\)$" "" path t t 1)))
    ;; we first check nnml and nnmaildir servers
    (setq 
     group
     (catch 'found
       (dolist (cur gnus-opened-servers)
	 (when (or (and (not maildirflag)
			(eq (caar cur) 'nnml))
		   (and maildirflag
			(eq (caar cur) 'nnmaildir)))
	   ;; get base path from server
	   (if maildirflag 
	       (setq serverbase (cadr (assoc 'directory (car cur))))		   
	     (setq serverbase (cadr (assoc 'nnml-directory (car cur))))
	     (when (not serverbase)
	       (setq serverbase nnml-directory)))
	   (setq serverbase (file-name-as-directory 
			     (expand-file-name serverbase)))
	   (when (string-match (concat serverbase "\\(.*\\)") path)
	     ;; looks good - rest of the path should be the group
	     (setq group (match-string 1 path))
	     (when (string-match "/$" group)
	       (setq group (replace-match "" t t group)))
	     (when (not maildirflag)
	       ;; for nnml: convert slashes to dots
	       (while (string-match "/" group)
		 (setq group (replace-match "." t t group))))
	     (setq group (gnus-group-prefixed-name group (car cur)))
	     ;; check whether this group actually exists
	     (when (gnus-group-entry group)
	       (throw 'found group)))))))
    (unless group
      ;; we haven't found it yet --> look for nnimap groups
      ;; assume last element of the path is the group
      (string-match "^.*/\\.?\\(.*\\)$" path)
      (setq group (match-string 1 path))
      ;; convert dots to slashes (nested group)
      (while (string-match "\\." group)
	(setq group (replace-match "/" t t group)))
      (dolist (cur gnus-opened-servers)
	(when (eq (caar cur) 'nnimap)
	  (when (gnus-group-entry 
		 (gnus-group-prefixed-name group (car cur)))
	    (push 
	     (gnus-group-prefixed-name group (car cur))
	     allgroups))))
      (if (> (length allgroups) 1)
	  (setq group (completing-read 
		       "Group %s exists on more than one IMAP server. Choose: " 
		       allgroups nil t))
	(setq group (car allgroups))))
    group))


;;; ==== Helper functions

(defun nnmairix-request-group-with-article-number-correction (folder qualgroup)
  "Request FOLDER on backend for nnmairix QUALGROUP and article number correction."
  (save-excursion
    (nnmairix-call-backend
     "request-group" folder nnmairix-backend-server fast)
    (set-buffer nnmairix-mairix-output-buffer)
    (goto-char (point-min))
    (re-search-forward "^Matched.*messages")
    (nnheader-message 7 (match-string 0))
    (set-buffer nntp-server-buffer)
    (goto-char (point-min))
    (let ((status (read (current-buffer)))
	  (total (read (current-buffer)))
	  (low (read (current-buffer)))
	  (high (read (current-buffer)))
	  (corr (gnus-group-get-parameter qualgroup 'numcorr t)))
      (if (= status 211)
	  (progn
	    ;; Article number correction
	    (if (and corr
		     (> (+ (car (cddr corr)) high) 0))
		(progn
		  (when (car corr) ;Group has changed
		    (setq corr
			  (list nil
				(car (cddr corr))
				(+ (car (cddr corr)) high)))
		    (gnus-group-set-parameter
		     qualgroup 'numcorr corr))
		  (setq low (+ low (cadr corr))
			high (+ high (cadr corr))))
	      (when (member nnmairix-backend
			    nnmairix-delete-and-create-on-change)
		(gnus-group-set-parameter
		 qualgroup 'numcorr (list nil 0 high))))
	    (erase-buffer)
	    (insert (format "%d %d %d %d %s" status total low high group))
	    t)
	(progn
	  (nnheader-report
	   'nnmairix "Error calling back end on group %s" folder)
	  nil)))))

(defun nnmairix-call-mairix-binary (command folder query threads)
  "Call mairix binary with COMMAND, using FOLDER and QUERY.
If THREADS is non-nil, enable full threads."
  (let ((args (cons (car command) '(nil t nil))))
    (save-excursion
      (set-buffer
       (get-buffer-create nnmairix-mairix-output-buffer))
      (erase-buffer)
      (when (> (length command) 1)
	(setq args (append args (cdr command))))
      (when threads
	(setq args (append args '("-t"))))
      (apply 'call-process
	     (append args (list "-o" folder) query)))))

(defun nnmairix-call-mairix-binary-raw (command query)
  "Call mairix binary with COMMAND and QUERY in raw mode."
  (let ((args (cons (car command) '(nil t nil))))
    (save-excursion
      (set-buffer
       (get-buffer-create nnmairix-mairix-output-buffer))
      (erase-buffer)
      (when (> (length command) 1)
        (setq args (append args (cdr command))))
      (setq args (append args '("-r")))
      (apply 'call-process
             (append args query)))))

(defun nnmairix-get-server ()
  "If there exists just one nnmairix server, return its value.
Otherwise, ask user for server."
  (let ((openedserver (nnmairix-get-nnmairix-servers)))
    (when (not openedserver)
      (error "No opened nnmairix server found"))
    (if (> (length openedserver) 1)
	(progn
	  (while
	      (equal '("")
		  (setq nnmairix-last-server
			(list (completing-read "Server: " openedserver nil 1
					       (or nnmairix-last-server
						   "nnmairix:"))))))
	  nnmairix-last-server)
      (car openedserver))))

(defun nnmairix-get-nnmairix-servers (&optional all)
  "Return available nnmairix servers.
If ALL is t, return also the unopened/failed ones."
  (let ((alist gnus-opened-servers)
	server openedserver)
    (while alist
      (setq server (pop alist))
      (when (and server
		 (or all
		     (eq (cadr server) 'ok))
		 (eq (caar server) 'nnmairix)
		 (not (member (car server) gnus-ephemeral-servers)))
	(setq server
	      (concat (symbol-name (caar server)) ":" (nth 1 (car server))))
	(push (list server) openedserver)))
    openedserver))


(defun nnmairix-get-valid-servers ()
  "Return list of valid backend servers for nnmairix groups."
  (let ((alist gnus-opened-servers)
	(mairixservers (nnmairix-get-nnmairix-servers t))
	server mserver openedserver occ cur)
    ;; Get list of all nnmairix backends (i.e. backends which are
    ;; already occupied)
    (dolist (cur mairixservers)
      (push
       (concat
	(symbol-name
	 (cadr (assoc 'nnmairix-backend
		      (gnus-server-to-method (car cur)))))
	 ":"
	 (cadr (assoc 'nnmairix-backend-server
		      (gnus-server-to-method (car cur)))))
	occ))
    (while alist
      (setq server (pop alist))
      (setq mserver (gnus-method-to-server (car server)))
      ;; If this is the native server, convert it to the real server
      ;; name to avoid confusion
      (when (string= mserver "native")
	(setq mserver (format "%s:%s"
			      (caar server)
			      (nth 1 (car server)))))
      (when (and server
		 (eq (cadr server) 'ok)
		 (member (caar server) nnmairix-valid-backends)
		 (not (member (car server) gnus-ephemeral-servers))
		 (not (member (gnus-method-to-server (car server)) occ)))
	(push
	 (list mserver)
	 openedserver)))
    openedserver))

(defun nnmairix-call-backend (func &rest args)
  "Call a function FUNC on backend with ARGS."
  (apply (intern (format "%s-%s" (symbol-name nnmairix-backend) func)) args))

(defun nnmairix-get-backend-folder (group &optional server)
  "Return back end GROUP from nnmairix group on SERVER."
  (let* ((qualgroup (if server
			(gnus-group-prefixed-name group (list 'nnmairix server))
		      group))
	 (folder (gnus-group-get-parameter qualgroup 'folder)))
    folder))

(defun nnmairix-get-numcorr (group &optional server)
  "Return values for article number correction nnmairix GROUP on SERVER."
  (let* ((qualgroup (if server
			(gnus-group-prefixed-name group (list 'nnmairix server))
		      group))
	 (corr (gnus-group-get-parameter qualgroup 'numcorr t)))
    corr))


(defun nnmairix-rename-files-consecutively (path)
  "Rename all nnml mail files in PATH so that they have consecutive numbers.
This should correct problems of wrong article counts when using
nnmairix with nnml backends."
  (let* ((files
	 (sort
	  (mapcar 'string-to-number
		  (directory-files path nil "[0-9]+" t))
	  '<))
	 (lastplusone (car files))
	 (path (file-name-as-directory path)))
    (dolist (cur files)
      (when (not (= cur lastplusone))
	(rename-file (concat path
			     (number-to-string cur))
		     (concat path
			     (number-to-string lastplusone)))
	(setq cur lastplusone))
      (setq lastplusone (1+ cur)))))

(defun nnmairix-replace-group-and-numbers (articles backendgroup mairixgroup numc)
  "Replace folder names in Xref header and correct article numbers.
Do this for all ARTICLES on BACKENDGROUP.  Replace using
MAIRIXGROUP.  NUMC contains values for article number correction."
  (let ((buf (get-buffer-create " *nnmairix buffer*"))
	(corr (not (zerop numc)))
	(name (buffer-name nntp-server-buffer))
	header cur xref)
    (save-excursion
      (set-buffer buf)
      (erase-buffer)
      (set-buffer nntp-server-buffer)
      (goto-char (point-min))
      (nnheader-message 7 "nnmairix: Rewriting headers...")
      (mapcar
       (function
	(lambda (article)
	  (when (or (looking-at (number-to-string article))
		    (nnheader-find-nov-line article))
	    (setq cur (nnheader-parse-nov))
	    (when corr
		  (setq article (+ (mail-header-number cur) numc))
		  (mail-header-set-number cur article))
	    (setq xref (mail-header-xref cur))
	    (when (and (stringp xref)
		       (string-match (format "[ \t]%s:[0-9]+" backendgroup) xref))
	      (setq xref (replace-match (format " %s:%d" mairixgroup article) t nil xref))
	      (mail-header-set-xref cur xref))
	    (set-buffer buf)
	    (nnheader-insert-nov cur)
	    (set-buffer nntp-server-buffer)
	    (when (not (eobp))
	      (forward-line 1)))))
       articles)
      (nnheader-message 7 "nnmairix: Rewriting headers... done")
      (kill-buffer nntp-server-buffer)
      (set-buffer buf)
      (rename-buffer name)
      (setq nntp-server-buffer buf))))

(defun nnmairix-backend-to-server (server)
  "Return nnmairix server most probably responsible for back end SERVER.
User will be asked if this cannot be determined.  Result is saved in
parameter 'indexed-servers of corresponding default search
group."
  (let ((allservers (nnmairix-get-nnmairix-servers))
	mairixserver found defaultgroup)
    (if (> (length allservers) 1)
	(progn
	  ;; If there is more than one nnmairix server, we go through them
	  (while (and allservers (not found))
	    (setq mairixserver (gnus-server-to-method (car (pop allservers))))
	    ;; First we look if SERVER is the backend of current nnmairix server
	    (setq found (and (eq (cadr (assoc 'nnmairix-backend mairixserver))
				 (car server))
			     (string= (cadr (assoc 'nnmairix-backend-server mairixserver))
				      (nth 1 server))))
	    ;; If that's not the case, we look at 'indexed-servers
	    ;; variable in default search group
	    (when (not found)
	      (setq defaultgroup (cadr (assoc 'nnmairix-default-group mairixserver)))
	      (setq found (member (gnus-method-to-server server)
				  (gnus-group-get-parameter
				   (gnus-group-prefixed-name defaultgroup
							     mairixserver)
				   'indexed-servers t)))))
	  ;; If still not found, we ask user
	  (when (not found)
	    (setq mairixserver
		  (gnus-server-to-method
		   (completing-read
		    (format "Cannot determine which nnmairix server indexes %s. Please specify: "
			    (gnus-method-to-server server))
		    (nnmairix-get-nnmairix-servers) nil nil "nnmairix:")))
	    ;; Save result in parameter of default search group so that
	    ;; we don't have to ask again
	    (setq defaultgroup (gnus-group-prefixed-name
				(cadr (assoc 'nnmairix-default-group mairixserver)) mairixserver))
	    (gnus-group-set-parameter
	     defaultgroup
	     'indexed-servers
	     (append (gnus-group-get-parameter defaultgroup 'indexed-servers t)
		     (list (gnus-method-to-server server)))))
	  mairixserver)
      ;; If there is just one (or none) nnmairix server:
      (gnus-server-to-method (caar allservers)))))

(defun nnmairix-update-and-clear-marks (group &optional method)
  "Update group and clear all marks from GROUP using METHOD."
  (when method
    (setq group (gnus-group-prefixed-name group method)))
  (let ((method (or method
		    (gnus-find-method-for-group group)))
	(folder (gnus-group-get-parameter group 'folder))
	(corr (gnus-group-get-parameter group 'numcorr t))
	info)
    (if (eq (nth 0 method) 'nnmairix)
	(save-excursion
	  (nnmairix-open-server (nth 1 method))
	  (set-buffer gnus-group-buffer)
	  (setq info (gnus-get-info group))
	  ;; Clear active and info
	  (gnus-set-active group nil)
	  (gnus-info-clear-data info)
	  ;; Delete and re-create group if needed
	  (when (member nnmairix-backend nnmairix-delete-and-create-on-change)
	    (if (string-match nnmairix-group-regexp folder)
		(progn
		  (nnmairix-call-backend "open-server"
					 nnmairix-backend-server)
		  (nnmairix-call-backend "request-delete-group"
					 folder t nnmairix-backend-server)
		  (nnmairix-call-backend "request-create-group"
					 folder nnmairix-backend-server)
		  ;; set flag that group has changed for article number correction
		  (when corr
		    (setcar corr t)
		    (gnus-group-set-parameter group 'numcorr corr)))
	      (error "Nnmairix-update-and-clear-marks - delete/create with\
 non-mairix group!! - check folder parameter")))
	  (when (gnus-group-jump-to-group group)
	    (gnus-group-get-new-news-this-group)))
      (error "Nnmairix-update-and-clear-marks - Called with non-nnmairix group"))))
  

(defun nnmairix-sentinel-mairix-update-finished (proc status)
  "Sentinel for mairix update process PROC with STATUS."
  (if (equal status "finished\n")
      (nnheader-message 7 "Updating mairix database for %s... done" proc)
    (error "There was an error updating the mairix database for server %s.  \
See %s for details" proc nnmairix-mairix-output-buffer)))

(defun nnmairix-create-message-line-for-search ()
  "Create message line for interactive query in minibuffer."
  (mapconcat
   (function
    (lambda (cur)
      (format "%c=%s" (car cur) (nth 3 cur))))
   nnmairix-interactive-query-parameters ","))

(defun nnmairix-replace-illegal-chars (header)
  "Replace illegal characters in HEADER for mairix query."
  (when header
    (if (> emacs-major-version 20)
	(while (string-match "[^-.@/,& [:alnum:]]" header)
	  (setq header (replace-match "" t t header)))
      (while (string-match "[[]{}:<>]" header)
	(setq header (replace-match "" t t header))))
    (while (string-match "[-& ]" header)
      (setq header (replace-match "," t t header)))
  header))

(defun nnmairix-show-original-article (group mid)
  "Switch to GROUP and display Article with message-id MID."
  (when (string-match "Summary" (buffer-name (current-buffer)))
    (gnus-summary-exit))
  (pop-to-buffer gnus-group-buffer)
  (gnus-group-jump-to-group group)
  (gnus-summary-read-group group 1 t)	
  (gnus-summary-refer-article mid)
  (gnus-summary-limit-to-headers (format "message-id: <%s>" mid))
  (gnus-summary-select-article)
  ;; Force redisplay
  (gnus-summary-show-article)
  (nnheader-message 5 "Switched to group %s." group))


;; ==== Widget stuff

(defvar nnmairix-widgets)
(defvar nnmairix-widgets-values nil)

(defun nnmairix-widget-search-from-this-article ()
  "Create mairix query based on current article using graphical widgets."
  (interactive)
  (nnmairix-widget-search
   (nnmairix-widget-get-values)))


(defun nnmairix-widget-get-values ()
  "Create values for editable fields from current article."
  (if (not (gnus-buffer-live-p gnus-article-buffer))
      (error "No article buffer available")
    (save-excursion
      (gnus-summary-toggle-header 1)
      (set-buffer gnus-article-buffer)
      (mapcar
       (function
	(lambda (field)
	  (list (caddr field)
		(if (car field)
		    (nnmairix-replace-illegal-chars
		     (gnus-fetch-field (car field)))
		  nil))))
       nnmairix-widget-fields-list))))
  

(defun nnmairix-widget-search (&optional mvalues)
  "Create mairix query interactively using graphical widgets.
MVALUES may contain values from current article."
  (interactive)
  ;; Select window for mairix customization
  (funcall nnmairix-widget-select-window-function)
  ;; generate widgets
  (nnmairix-widget-create-query mvalues)
  ;; generate Buttons
  (widget-create 'push-button
		 :notify
		 (if mvalues
		     (lambda (&rest ignore)
		       (nnmairix-widget-send-query nnmairix-widgets
						   t))
		   (lambda (&rest ignore)
		     (nnmairix-widget-send-query nnmairix-widgets
						 nil)))
		 "Send Query")
  (widget-insert "   ")
  (widget-create 'push-button
		 :notify
		 (if mvalues
		     (lambda (&rest ignore)
		       (nnmairix-widget-create-group nnmairix-widgets
						     t))
		   (lambda (&rest ignore)
		     (nnmairix-widget-create-group nnmairix-widgets
						   nil)))
		 "Create permanent group")
  (widget-insert "   ")
  (widget-create 'push-button
		 :notify (lambda (&rest ignore)
			   (kill-buffer nnmairix-customize-query-buffer))
		 "Cancel")
  (use-local-map widget-keymap)
  (widget-setup)
  (goto-char (point-min)))

(defun nnmairix-widget-send-query (widgets &optional withvalues)
  "Send query from WIDGETS to mairix binary.
If WITHVALUES is t, query is based on current article."
  (nnmairix-search
   (nnmairix-widget-make-query-from-widgets widgets)
   (if withvalues
       (gnus-method-to-server
	(nnmairix-backend-to-server gnus-current-select-method))
     (car (nnmairix-get-server)))
   (if (widget-value (cadr (assoc "Threads" widgets)))
       t
     -1))
  (kill-buffer nnmairix-customize-query-buffer))

(defun nnmairix-widget-create-group (widgets &optional withvalues)
  "Create nnmairix group based on current widget values WIDGETS.
If WITHVALUES is t, query is based on current article."
  (let ((group (read-string "Name of the group: ")))
    (when (not (zerop (length group)))
      (nnmairix-create-search-group
       (if withvalues
	   (gnus-method-to-server
	    (nnmairix-backend-to-server gnus-current-select-method))
	 (car (nnmairix-get-server)))
       group
       (nnmairix-widget-make-query-from-widgets widgets)
       (widget-value (cadr (assoc "Threads" widgets))))))
  (kill-buffer nnmairix-customize-query-buffer))


(defun nnmairix-widget-make-query-from-widgets (widgets)
  "Create mairix query from widget values WIDGETS."
  (let (query temp flag)
    ;; first we do the editable fields
    (dolist (cur nnmairix-widget-fields-list)
      ;; See if checkbox is checked
      (when (widget-value
	     (cadr (assoc (concat "c" (caddr cur)) widgets)))
	;; create query for the field
	(push
	 (concat
	  (nth 1 cur)
	  ":"
	  (nnmairix-replace-illegal-chars
	   (widget-value
	   (cadr (assoc (concat "e" (caddr cur)) widgets)))))
	 query)))
    ;; Flags
    (when (member 'flags nnmairix-widget-other)
      (setq flag
	    (mapconcat
	     (function
	      (lambda (flag)
		(setq temp
		      (widget-value (cadr (assoc (car flag) nnmairix-widgets))))
		(if (string= "yes" temp)
		    (cadr flag)
		  (if (string= "no" temp)
		      (concat "-" (cadr flag))))))
	     '(("seen" "s") ("replied" "r") ("flagged" "f")) ""))
      (when (not (zerop (length flag)))
	(push (concat "F:" flag) query)))
    ;; return query string
    (mapconcat 'identity query " ")))


(defun nnmairix-widget-create-query (&optional values)
  "Create widgets for creating mairix queries.
Fill in VALUES if based on an article."
  (let (allwidgets)
    (when (get-buffer nnmairix-customize-query-buffer)
      (kill-buffer nnmairix-customize-query-buffer))
    (switch-to-buffer nnmairix-customize-query-buffer)
    (kill-all-local-variables)
    (erase-buffer)
    (widget-insert "Specify your query for Mairix (check boxes for activating fields):\n\n")
    (widget-insert "(Whitespaces will be converted to ',' (i.e. AND). Use '/' for OR.)\n\n")
;    (make-local-variable 'nnmairix-widgets)
    (setq nnmairix-widgets (nnmairix-widget-build-editable-fields values))
    (when (member 'flags nnmairix-widget-other)
      (widget-insert "\nFlags:\n      Seen:     ")
      (nnmairix-widget-add "seen"
			   'menu-choice
			   :value "ignore"
			   '(item "yes") '(item "no") '(item "ignore"))
      (widget-insert "      Replied:  ")
      (nnmairix-widget-add "replied"
			   'menu-choice
			   :value "ignore"
			   '(item "yes") '(item "no") '(item "ignore"))
      (widget-insert "      Ticked:   ")
      (nnmairix-widget-add "flagged"
			   'menu-choice
			   :value "ignore"
			   '(item "yes") '(item "no") '(item "ignore")))
    (when (member 'threads nnmairix-widget-other)
      (widget-insert "\n")
      (nnmairix-widget-add "Threads" 'checkbox nil))
      (widget-insert " Show full threads\n\n")))


(defun nnmairix-widget-build-editable-fields (values)
  "Build editable field widgets in `nnmairix-widget-fields-list'.
VALUES may contain values for editable fields from current article."
  ;; how can this be done less ugly?
  (let ((ret))
    (mapc
     (function
      (lambda (field)
	(setq field (caddr field))
	(setq ret
	      (nconc
	       (list
		(list
		 (concat "c" field)
		 (widget-create 'checkbox
				:tag field
				:notify (lambda (widget &rest ignore)
					  (nnmairix-widget-toggle-activate widget))
				nil)))
	       (list
		(list
		 (concat "e" field)
		 (widget-create 'editable-field
				:size 60
				:format (concat " " field ":"
						(make-string (- 11 (length field)) ?\ )
						"%v")
				:value (or (cadr (assoc field values)) ""))))
	       ret))
	(widget-insert "\n")
	;; Deactivate editable field
	(widget-apply (cadr (nth 1 ret)) :deactivate)))
     nnmairix-widget-fields-list)
    ret))

(defun nnmairix-widget-add (name &rest args)
  "Add a widget NAME with optional ARGS."
  (push
   (list name
	 (apply 'widget-create args))
   nnmairix-widgets))

(defun nnmairix-widget-toggle-activate (widget)
  "Toggle activation status of WIDGET dependent on corresponding checkbox value."
  (let ((field (widget-get widget :tag)))
    (if (widget-value widget)
	(widget-apply
	 (cadr (assoc (concat "e" field) nnmairix-widgets))
	 :activate)
      (widget-apply
       (cadr (assoc (concat "e" field) nnmairix-widgets))
       :deactivate)))
  (widget-setup))

(provide 'nnmairix)

;; arch-tag: bb187498-b229-4a55-8c07-6d3f80713e94
;;; nnmairix.el ends here
