;;; gnus-srvr.el --- virtual server support for Gnus
;; Copyright (C) 1995,96,97,98 Free Software Foundation, Inc.

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
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

(eval-when-compile (require 'cl))

(eval-when-compile (require 'cl))

(require 'gnus)
(require 'gnus-spec)
(require 'gnus-group)
(require 'gnus-int)
(require 'gnus-range)

(defvar gnus-server-mode-hook nil
  "Hook run in `gnus-server-mode' buffers.")

(defconst gnus-server-line-format "     {%(%h:%w%)} %s\n"
  "Format of server lines.
It works along the same lines as a normal formatting string,
with some simple extensions.

The following specs are understood:

%h backend
%n name
%w address
%s status")

(defvar gnus-server-mode-line-format "Gnus: %%b"
  "The format specification for the server mode line.")

(defvar gnus-server-exit-hook nil
  "*Hook run when exiting the server buffer.")

;;; Internal variables.

(defvar gnus-inserted-opened-servers nil)

(defvar gnus-server-line-format-alist
  `((?h gnus-tmp-how ?s)
    (?n gnus-tmp-name ?s)
    (?w gnus-tmp-where ?s)
    (?s gnus-tmp-status ?s)))

(defvar gnus-server-mode-line-format-alist
  `((?S gnus-tmp-news-server ?s)
    (?M gnus-tmp-news-method ?s)
    (?u gnus-tmp-user-defined ?s)))

(defvar gnus-server-line-format-spec nil)
(defvar gnus-server-mode-line-format-spec nil)
(defvar gnus-server-killed-servers nil)

(defvar gnus-server-mode-map)

(defvar gnus-server-menu-hook nil
  "*Hook run after the creation of the server mode menu.")

(defun gnus-server-make-menu-bar ()
  (gnus-turn-off-edit-menu 'server)
  (unless (boundp 'gnus-server-server-menu)
    (easy-menu-define
     gnus-server-server-menu gnus-server-mode-map ""
     '("Server"
       ["Add" gnus-server-add-server t]
       ["Browse" gnus-server-read-server t]
       ["Scan" gnus-server-scan-server t]
       ["List" gnus-server-list-servers t]
       ["Kill" gnus-server-kill-server t]
       ["Yank" gnus-server-yank-server t]
       ["Copy" gnus-server-copy-server t]
       ["Edit" gnus-server-edit-server t]
       ["Regenerate" gnus-server-regenerate-server t]
       ["Exit" gnus-server-exit t]))

    (easy-menu-define
     gnus-server-connections-menu gnus-server-mode-map ""
     '("Connections"
       ["Open" gnus-server-open-server t]
       ["Close" gnus-server-close-server t]
       ["Deny" gnus-server-deny-server t]
       "---"
       ["Open All" gnus-server-open-all-servers t]
       ["Close All" gnus-server-close-all-servers t]
       ["Reset All" gnus-server-remove-denials t]))

    (gnus-run-hooks 'gnus-server-menu-hook)))

(defvar gnus-server-mode-map nil)
(put 'gnus-server-mode 'mode-class 'special)

(unless gnus-server-mode-map
  (setq gnus-server-mode-map (make-sparse-keymap))
  (suppress-keymap gnus-server-mode-map)

  (gnus-define-keys gnus-server-mode-map
    " " gnus-server-read-server
    "\r" gnus-server-read-server
    gnus-mouse-2 gnus-server-pick-server
    "q" gnus-server-exit
    "l" gnus-server-list-servers
    "k" gnus-server-kill-server
    "y" gnus-server-yank-server
    "c" gnus-server-copy-server
    "a" gnus-server-add-server
    "e" gnus-server-edit-server
    "s" gnus-server-scan-server

    "O" gnus-server-open-server
    "\M-o" gnus-server-open-all-servers
    "C" gnus-server-close-server
    "\M-c" gnus-server-close-all-servers
    "D" gnus-server-deny-server
    "R" gnus-server-remove-denials

    "g" gnus-server-regenerate-server

    "\C-c\C-i" gnus-info-find-node
    "\C-c\C-b" gnus-bug))

(defun gnus-server-mode ()
  "Major mode for listing and editing servers.

All normal editing commands are switched off.
\\<gnus-server-mode-map>
For more in-depth information on this mode, read the manual
(`\\[gnus-info-find-node]').

The following commands are available:

\\{gnus-server-mode-map}"
  (interactive)
  (when (gnus-visual-p 'server-menu 'menu)
    (gnus-server-make-menu-bar))
  (kill-all-local-variables)
  (gnus-simplify-mode-line)
  (setq major-mode 'gnus-server-mode)
  (setq mode-name "Server")
  (gnus-set-default-directory)
  (setq mode-line-process nil)
  (use-local-map gnus-server-mode-map)
  (buffer-disable-undo (current-buffer))
  (setq truncate-lines t)
  (setq buffer-read-only t)
  (gnus-run-hooks 'gnus-server-mode-hook))

(defun gnus-server-insert-server-line (gnus-tmp-name method)
  (let* ((gnus-tmp-how (car method))
	 (gnus-tmp-where (nth 1 method))
	 (elem (assoc method gnus-opened-servers))
	 (gnus-tmp-status (cond ((eq (nth 1 elem) 'denied)
			"(denied)")
		       ((or (gnus-server-opened method)
			    (eq (nth 1 elem) 'ok))
			"(opened)")
		       (t
			"(closed)"))))
    (beginning-of-line)
    (gnus-add-text-properties
     (point)
     (prog1 (1+ (point))
       ;; Insert the text.
       (eval gnus-server-line-format-spec))
     (list 'gnus-server (intern gnus-tmp-name)))))

(defun gnus-enter-server-buffer ()
  "Set up the server buffer."
  (gnus-server-setup-buffer)
  (gnus-configure-windows 'server)
  (gnus-server-prepare))

(defun gnus-server-setup-buffer ()
  "Initialize the server buffer."
  (unless (get-buffer gnus-server-buffer)
    (save-excursion
      (set-buffer (gnus-get-buffer-create gnus-server-buffer))
      (gnus-server-mode)
      (when gnus-carpal
	(gnus-carpal-setup-buffer 'server)))))

(defun gnus-server-prepare ()
  (gnus-set-format 'server-mode)
  (gnus-set-format 'server t)
  (let ((alist gnus-server-alist)
	(buffer-read-only nil)
	(opened gnus-opened-servers)
	done server op-ser)
    (erase-buffer)
    (setq gnus-inserted-opened-servers nil)
    ;; First we do the real list of servers.
    (while alist
      (unless (member (cdar alist) done)
	(push (cdar alist) done)
	(cdr (setq server (pop alist)))
	(when (and server (car server) (cdr server))
	  (gnus-server-insert-server-line (car server) (cdr server))))
      (when (member (cdar alist) done)
	(pop alist)))
    ;; Then we insert the list of servers that have been opened in
    ;; this session.
    (while opened
      (when (and (not (member (caar opened) done))
		 ;; Just ignore ephemeral servers.
		 (not (member (caar opened) gnus-ephemeral-servers)))
	(push (caar opened) done)
	(gnus-server-insert-server-line
	 (setq op-ser (format "%s:%s" (caaar opened) (nth 1 (caar opened))))
	 (caar opened))
	(push (list op-ser (caar opened)) gnus-inserted-opened-servers))
      (setq opened (cdr opened))))
  (goto-char (point-min))
  (gnus-server-position-point))

(defun gnus-server-server-name ()
  (let ((server (get-text-property (gnus-point-at-bol) 'gnus-server)))
    (and server (symbol-name server))))

(defalias 'gnus-server-position-point 'gnus-goto-colon)

(defconst gnus-server-edit-buffer "*Gnus edit server*")

(defun gnus-server-update-server (server)
  (save-excursion
    (set-buffer gnus-server-buffer)
    (let* ((buffer-read-only nil)
	   (entry (assoc server gnus-server-alist))
	   (oentry (assoc (gnus-server-to-method server)
			  gnus-opened-servers)))
      (when entry
	(gnus-dribble-enter
	 (concat "(gnus-server-set-info \"" server "\" '"
		 (prin1-to-string (cdr entry)) ")\n")))
      (when (or entry oentry)
	;; Buffer may be narrowed.
	(save-restriction
	  (widen)
	  (when (gnus-server-goto-server server)
	    (gnus-delete-line))
	  (if entry
	      (gnus-server-insert-server-line (car entry) (cdr entry))
	    (gnus-server-insert-server-line
	     (format "%s:%s" (caar oentry) (nth 1 (car oentry)))
	     (car oentry)))
	  (gnus-server-position-point))))))

(defun gnus-server-set-info (server info)
  ;; Enter a select method into the virtual server alist.
  (when (and server info)
    (gnus-dribble-enter
     (concat "(gnus-server-set-info \"" server "\" '"
	     (prin1-to-string info) ")"))
    (let* ((server (nth 1 info))
	   (entry (assoc server gnus-server-alist)))
      (if entry (setcdr entry info)
	(setq gnus-server-alist
	      (nconc gnus-server-alist (list (cons server info))))))))

;;; Interactive server functions.

(defun gnus-server-kill-server (server)
  "Kill the server on the current line."
  (interactive (list (gnus-server-server-name)))
  (unless (gnus-server-goto-server server)
    (if server (error "No such server: %s" server)
      (error "No server on the current line")))
  (unless (assoc server gnus-server-alist)
    (error "Read-only server %s" server))
  (gnus-dribble-touch)
  (let ((buffer-read-only nil))
    (gnus-delete-line))
  (push (assoc server gnus-server-alist) gnus-server-killed-servers)
  (setq gnus-server-alist (delq (car gnus-server-killed-servers)
				gnus-server-alist))
  (gnus-server-position-point))

(defun gnus-server-yank-server ()
  "Yank the previously killed server."
  (interactive)
  (unless gnus-server-killed-servers
    (error "No killed servers to be yanked"))
  (let ((alist gnus-server-alist)
	(server (gnus-server-server-name))
	(killed (car gnus-server-killed-servers)))
    (if (not server)
	(setq gnus-server-alist (nconc gnus-server-alist (list killed)))
      (if (string= server (caar gnus-server-alist))
	  (push killed gnus-server-alist)
	(while (and (cdr alist)
		    (not (string= server (caadr alist))))
	  (setq alist (cdr alist)))
	(if alist
	    (setcdr alist (cons killed (cdr alist)))
 	  (setq gnus-server-alist (list killed)))))
    (gnus-server-update-server (car killed))
    (setq gnus-server-killed-servers (cdr gnus-server-killed-servers))
    (gnus-server-position-point)))

(defun gnus-server-exit ()
  "Return to the group buffer."
  (interactive)
  (gnus-run-hooks 'gnus-server-exit-hook)
  (kill-buffer (current-buffer))
  (gnus-configure-windows 'group t))

(defun gnus-server-list-servers ()
  "List all available servers."
  (interactive)
  (let ((cur (gnus-server-server-name)))
    (gnus-server-prepare)
    (if cur (gnus-server-goto-server cur)
      (goto-char (point-max))
      (forward-line -1))
    (gnus-server-position-point)))

(defun gnus-server-set-status (method status)
  "Make METHOD have STATUS."
  (let ((entry (assoc method gnus-opened-servers)))
    (if entry
	(setcar (cdr entry) status)
      (push (list method status) gnus-opened-servers))))

(defun gnus-opened-servers-remove (method)
  "Remove METHOD from the list of opened servers."
  (setq gnus-opened-servers (delq (assoc method gnus-opened-servers)
				  gnus-opened-servers)))

(defun gnus-server-open-server (server)
  "Force an open of SERVER."
  (interactive (list (gnus-server-server-name)))
  (let ((method (gnus-server-to-method server)))
    (unless method
      (error "No such server: %s" server))
    (gnus-server-set-status method 'ok)
    (prog1
	(or (gnus-open-server method)
	    (progn (message "Couldn't open %s" server) nil))
      (gnus-server-update-server server)
      (gnus-server-position-point))))

(defun gnus-server-open-all-servers ()
  "Open all servers."
  (interactive)
  (let ((servers gnus-inserted-opened-servers))
    (while servers
      (gnus-server-open-server (car (pop servers))))))

(defun gnus-server-close-server (server)
  "Close SERVER."
  (interactive (list (gnus-server-server-name)))
  (let ((method (gnus-server-to-method server)))
    (unless method
      (error "No such server: %s" server))
    (gnus-server-set-status method 'closed)
    (prog1
	(gnus-close-server method)
      (gnus-server-update-server server)
      (gnus-server-position-point))))

(defun gnus-server-close-all-servers ()
  "Close all servers."
  (interactive)
  (let ((servers gnus-inserted-opened-servers))
    (while servers
      (gnus-server-close-server (car (pop servers))))))

(defun gnus-server-deny-server (server)
  "Make sure SERVER will never be attempted opened."
  (interactive (list (gnus-server-server-name)))
  (let ((method (gnus-server-to-method server)))
    (unless method
      (error "No such server: %s" server))
    (gnus-server-set-status method 'denied))
  (gnus-server-update-server server)
  (gnus-server-position-point)
  t)

(defun gnus-server-remove-denials ()
  "Make all denied servers into closed servers."
  (interactive)
  (let ((servers gnus-opened-servers))
    (while servers
      (when (eq (nth 1 (car servers)) 'denied)
	(setcar (nthcdr 1 (car servers)) 'closed))
      (setq servers (cdr servers))))
  (gnus-server-list-servers))

(defun gnus-server-copy-server (from to)
  (interactive
   (list
    (or (gnus-server-server-name)
	(error "No server on the current line"))
    (read-string "Copy to: ")))
  (unless from
    (error "No server on current line"))
  (unless (and to (not (string= to "")))
    (error "No name to copy to"))
  (when (assoc to gnus-server-alist)
    (error "%s already exists" to))
  (unless (gnus-server-to-method from)
    (error "%s: no such server" from))
  (let ((to-entry (cons from (gnus-copy-sequence
			      (gnus-server-to-method from)))))
    (setcar to-entry to)
    (setcar (nthcdr 2 to-entry) to)
    (push to-entry gnus-server-killed-servers)
    (gnus-server-yank-server)))

(defun gnus-server-add-server (how where)
  (interactive
   (list (intern (completing-read "Server method: "
				  gnus-valid-select-methods nil t))
	 (read-string "Server name: ")))
  (when (assq where gnus-server-alist)
    (error "Server with that name already defined"))
  (push (list where how where) gnus-server-killed-servers)
  (gnus-server-yank-server))

(defun gnus-server-goto-server (server)
  "Jump to a server line."
  (interactive
   (list (completing-read "Goto server: " gnus-server-alist nil t)))
  (let ((to (text-property-any (point-min) (point-max)
			       'gnus-server (intern server))))
    (when to
      (goto-char to)
      (gnus-server-position-point))))

(defun gnus-server-edit-server (server)
  "Edit the server on the current line."
  (interactive (list (gnus-server-server-name)))
  (unless server
    (error "No server on current line"))
  (unless (assoc server gnus-server-alist)
    (error "This server can't be edited"))
  (let ((info (cdr (assoc server gnus-server-alist))))
    (gnus-close-server info)
    (gnus-edit-form
     info "Editing the server."
     `(lambda (form)
	(gnus-server-set-info ,server form)
	(gnus-server-list-servers)
	(gnus-server-position-point)))))

(defun gnus-server-scan-server (server)
  "Request a scan from the current server."
  (interactive (list (gnus-server-server-name)))
  (let ((method (gnus-server-to-method server)))
    (if (not (gnus-get-function method 'request-scan))
	(error "Server %s can't scan" (car method))
      (gnus-message 3 "Scanning %s..." server)
      (gnus-request-scan nil method)
      (gnus-message 3 "Scanning %s...done" server))))

(defun gnus-server-read-server (server)
  "Browse a server."
  (interactive (list (gnus-server-server-name)))
  (let ((buf (current-buffer)))
    (prog1
	(gnus-browse-foreign-server server buf)
      (save-excursion
	(set-buffer buf)
	(gnus-server-update-server (gnus-server-server-name))
	(gnus-server-position-point)))))

(defun gnus-server-pick-server (e)
  (interactive "e")
  (mouse-set-point e)
  (gnus-server-read-server (gnus-server-server-name)))


;;;
;;; Browse Server Mode
;;;

(defvar gnus-browse-menu-hook nil
  "*Hook run after the creation of the browse mode menu.")

(defvar gnus-browse-mode-hook nil)
(defvar gnus-browse-mode-map nil)
(put 'gnus-browse-mode 'mode-class 'special)

(unless gnus-browse-mode-map
  (setq gnus-browse-mode-map (make-keymap))
  (suppress-keymap gnus-browse-mode-map)

  (gnus-define-keys
   gnus-browse-mode-map
   " " gnus-browse-read-group
   "=" gnus-browse-select-group
   "n" gnus-browse-next-group
   "p" gnus-browse-prev-group
   "\177" gnus-browse-prev-group
   [delete] gnus-browse-prev-group
   "N" gnus-browse-next-group
   "P" gnus-browse-prev-group
   "\M-n" gnus-browse-next-group
   "\M-p" gnus-browse-prev-group
   "\r" gnus-browse-select-group
   "u" gnus-browse-unsubscribe-current-group
   "l" gnus-browse-exit
   "L" gnus-browse-exit
   "q" gnus-browse-exit
   "Q" gnus-browse-exit
   "\C-c\C-c" gnus-browse-exit
   "?" gnus-browse-describe-briefly

   "\C-c\C-i" gnus-info-find-node
   "\C-c\C-b" gnus-bug))

(defun gnus-browse-make-menu-bar ()
  (gnus-turn-off-edit-menu 'browse)
  (unless (boundp 'gnus-browse-menu)
    (easy-menu-define
     gnus-browse-menu gnus-browse-mode-map ""
     '("Browse"
       ["Subscribe" gnus-browse-unsubscribe-current-group t]
       ["Read" gnus-browse-read-group t]
       ["Select" gnus-browse-select-group t]
       ["Next" gnus-browse-next-group t]
       ["Prev" gnus-browse-next-group t]
       ["Exit" gnus-browse-exit t]))
    (gnus-run-hooks 'gnus-browse-menu-hook)))

(defvar gnus-browse-current-method nil)
(defvar gnus-browse-return-buffer nil)

(defvar gnus-browse-buffer "*Gnus Browse Server*")

(defun gnus-browse-foreign-server (server &optional return-buffer)
  "Browse the server SERVER."
  (setq gnus-browse-current-method server)
  (setq gnus-browse-return-buffer return-buffer)
  (let* ((method (gnus-server-to-method server))
	 (gnus-select-method method)
	 groups group)
    (gnus-message 5 "Connecting to %s..." (nth 1 method))
    (cond
     ((not (gnus-check-server method))
      (gnus-message
       1 "Unable to contact server %s: %s" (nth 1 method)
       (gnus-status-message method))
      nil)
     ((not
       (prog2
	   (gnus-message 6 "Reading active file...")
	   (gnus-request-list method)
	 (gnus-message 6 "Reading active file...done")))
      (gnus-message
       1 "Couldn't request list: %s" (gnus-status-message method))
      nil)
     (t
      (gnus-get-buffer-create gnus-browse-buffer)
      (when gnus-carpal
	(gnus-carpal-setup-buffer 'browse))
      (gnus-configure-windows 'browse)
      (buffer-disable-undo (current-buffer))
      (let ((buffer-read-only nil))
	(erase-buffer))
      (gnus-browse-mode)
      (setq mode-line-buffer-identification
	    (list
	     (format
	      "Gnus: %%b {%s:%s}" (car method) (cadr method))))
      (save-excursion
	(set-buffer nntp-server-buffer)
	(let ((cur (current-buffer)))
	  (goto-char (point-min))
	  (unless (string= gnus-ignored-newsgroups "")
	    (delete-matching-lines gnus-ignored-newsgroups))
	  (while (re-search-forward
		  "\\(^[^ \t]+\\)[ \t]+[0-9]+[ \t]+[0-9]+" nil t)
	    (goto-char (match-end 1))
	    (condition-case ()
		(push (cons (match-string 1)
			    (max 0 (- (1+ (read cur)) (read cur))))
		      groups)
	      (error nil)))))
      (setq groups (sort groups
			 (lambda (l1 l2)
			   (string< (car l1) (car l2)))))
      (let ((buffer-read-only nil))
	(while groups
	  (setq group (car groups))
	  (insert
	   (format "K%7d: %s\n" (cdr group) (car group)))
	  (setq groups (cdr groups))))
      (switch-to-buffer (current-buffer))
      (goto-char (point-min))
      (gnus-group-position-point)
      (gnus-message 5 "Connecting to %s...done" (nth 1 method))
      t))))

(defun gnus-browse-mode ()
  "Major mode for browsing a foreign server.

All normal editing commands are switched off.

\\<gnus-browse-mode-map>
The only things you can do in this buffer is

1) `\\[gnus-browse-unsubscribe-current-group]' to subscribe to a group.
The group will be inserted into the group buffer upon exit from this
buffer.

2) `\\[gnus-browse-read-group]' to read a group ephemerally.

3) `\\[gnus-browse-exit]' to return to the group buffer."
  (interactive)
  (kill-all-local-variables)
  (when (gnus-visual-p 'browse-menu 'menu)
    (gnus-browse-make-menu-bar))
  (gnus-simplify-mode-line)
  (setq major-mode 'gnus-browse-mode)
  (setq mode-name "Browse Server")
  (setq mode-line-process nil)
  (use-local-map gnus-browse-mode-map)
  (buffer-disable-undo (current-buffer))
  (setq truncate-lines t)
  (gnus-set-default-directory)
  (setq buffer-read-only t)
  (gnus-run-hooks 'gnus-browse-mode-hook))

(defun gnus-browse-read-group (&optional no-article)
  "Enter the group at the current line."
  (interactive)
  (let ((group (gnus-browse-group-name)))
    (if (or (not (gnus-get-info group))
	    (gnus-ephemeral-group-p group))
	(unless (gnus-group-read-ephemeral-group
		 group gnus-browse-current-method nil
		 (cons (current-buffer) 'browse))
	  (error "Couldn't enter %s" group))
      (unless (gnus-group-read-group nil no-article group)
	(error "Couldn't enter %s" group)))))
      
(defun gnus-browse-select-group ()
  "Select the current group."
  (interactive)
  (gnus-browse-read-group 'no))

(defun gnus-browse-next-group (n)
  "Go to the next group."
  (interactive "p")
  (prog1
      (forward-line n)
    (gnus-group-position-point)))

(defun gnus-browse-prev-group (n)
  "Go to the next group."
  (interactive "p")
  (gnus-browse-next-group (- n)))

(defun gnus-browse-unsubscribe-current-group (arg)
  "(Un)subscribe to the next ARG groups."
  (interactive "p")
  (when (eobp)
    (error "No group at current line"))
  (let ((ward (if (< arg 0) -1 1))
	(arg (abs arg)))
    (while (and (> arg 0)
		(not (eobp))
		(gnus-browse-unsubscribe-group)
		(zerop (gnus-browse-next-group ward)))
      (decf arg))
    (gnus-group-position-point)
    (when (/= 0 arg)
      (gnus-message 7 "No more newsgroups"))
    arg))

(defun gnus-browse-group-name ()
  (save-excursion
    (beginning-of-line)
    (when (re-search-forward ": \\(.*\\)$" (gnus-point-at-eol) t)
      (gnus-group-prefixed-name
       ;; Remove text props.
       (format "%s" (match-string 1))
       gnus-browse-current-method))))

(defun gnus-browse-unsubscribe-group ()
  "Toggle subscription of the current group in the browse buffer."
  (let ((sub nil)
	(buffer-read-only nil)
	group)
    (save-excursion
      (beginning-of-line)
      ;; If this group it killed, then we want to subscribe it.
      (when (= (following-char) ?K)
	(setq sub t))
      (setq group (gnus-browse-group-name))
      (when (and sub
		 (cadr (gnus-gethash group gnus-newsrc-hashtb)))
	(error "Group already subscribed"))
      (delete-char 1)
      (if sub
	  (progn
	    ;; Make sure the group has been properly removed before we
	    ;; subscribe to it.
	    (gnus-kill-ephemeral-group group)
	    (gnus-group-change-level
	     (list t group gnus-level-default-subscribed
		   nil nil (if (gnus-server-equal
				gnus-browse-current-method "native")
			       nil
			     gnus-browse-current-method))
	     gnus-level-default-subscribed gnus-level-killed
	     (and (car (nth 1 gnus-newsrc-alist))
		  (gnus-gethash (car (nth 1 gnus-newsrc-alist))
				gnus-newsrc-hashtb))
	     t)
	    (insert ? ))
	(gnus-group-change-level
	 group gnus-level-killed gnus-level-default-subscribed)
	(insert ?K)))
    t))

(defun gnus-browse-exit ()
  "Quit browsing and return to the group buffer."
  (interactive)
  (when (eq major-mode 'gnus-browse-mode)
    (kill-buffer (current-buffer)))
  ;; Insert the newly subscribed groups in the group buffer.
  (save-excursion
    (set-buffer gnus-group-buffer)
    (gnus-group-list-groups nil))
  (if gnus-browse-return-buffer
      (gnus-configure-windows 'server 'force)
    (gnus-configure-windows 'group 'force)))

(defun gnus-browse-describe-briefly ()
  "Give a one line description of the group mode commands."
  (interactive)
  (gnus-message 6
		(substitute-command-keys "\\<gnus-browse-mode-map>\\[gnus-group-next-group]:Forward  \\[gnus-group-prev-group]:Backward  \\[gnus-browse-exit]:Exit  \\[gnus-info-find-node]:Run Info  \\[gnus-browse-describe-briefly]:This help")))

(defun gnus-server-regenerate-server ()
  "Issue a command to the server to regenerate all its data structures."
  (interactive)
  (let ((server (gnus-server-server-name)))
    (unless server
      (error "No server on the current line"))
    (if (not (gnus-check-backend-function
	      'request-regenerate (car (gnus-server-to-method server))))
	(error "This backend doesn't support regeneration")
      (gnus-message 5 "Requesting regeneration of %s..." server)
      (unless (gnus-open-server server)
	(error "Couldn't open server"))
      (if (gnus-request-regenerate server)
	  (gnus-message 5 "Requesting regeneration of %s...done" server)
	(gnus-message 5 "Couldn't regenerate %s" server)))))

(provide 'gnus-srvr)

;;; gnus-srvr.el ends here.
