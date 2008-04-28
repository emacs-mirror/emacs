;;; org-archive.el --- Archiving for Org-mode

;; Copyright (C) 2004, 2005, 2006, 2007, 2008 Free Software Foundation, Inc.

;; Author: Carsten Dominik <carsten at orgmode dot org>
;; Keywords: outlines, hypermedia, calendar, wp
;; Homepage: http://orgmode.org
;; Version: 6.02b
;;
;; This file is part of GNU Emacs.
;;
;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:

;; This file contains the face definitons for Org.

;;; Code:

(require 'org)

(defcustom org-archive-sibling-heading "Archive"
  "Name of the local archive sibling that is used to archive entries locally.
Locally means: in the tree, under a sibling.
See `org-archive-to-archive-sibling' for more information."
  :group 'org-archive
  :type 'string)

(defcustom org-archive-mark-done t
  "Non-nil means, mark entries as DONE when they are moved to the archive file.
This can be a string to set the keyword to use.  When t, Org-mode will
use the first keyword in its list that means done."
  :group 'org-archive
  :type '(choice
	  (const :tag "No" nil)
	  (const :tag "Yes" t)
	  (string :tag "Use this keyword")))

(defcustom org-archive-stamp-time t
  "Non-nil means, add a time stamp to entries moved to an archive file.
This variable is obsolete and has no effect anymore, instead add ot remove
`time' from the variablle `org-archive-save-context-info'."
  :group 'org-archive
  :type 'boolean)

(defcustom org-archive-save-context-info '(time file olpath category todo itags)
  "Parts of context info that should be stored as properties when archiving.
When a subtree is moved to an archive file, it looses information given by
context, like inherited tags, the category, and possibly also the TODO
state (depending on the variable `org-archive-mark-done').
This variable can be a list of any of the following symbols:

time       The time of archiving.
file       The file where the entry originates.
itags      The local tags, in the headline of the subtree.
ltags      The tags the subtree inherits from further up the hierarchy.
todo       The pre-archive TODO state.
category   The category, taken from file name or #+CATEGORY lines.
olpath     The outline path to the item.  These are all headlines above
           the current item, separated by /, like a file path.

For each symbol present in the list, a property will be created in
the archived entry, with a prefix \"PRE_ARCHIVE_\", to remember this
information."
  :group 'org-archive
  :type '(set :greedy t
	  (const :tag "Time" time)
	  (const :tag "File" file)
	  (const :tag "Category" category)
	  (const :tag "TODO state" todo)
	  (const :tag "TODO state" priority)
	  (const :tag "Inherited tags" itags)
	  (const :tag "Outline path" olpath)
	  (const :tag "Local tags" ltags)))

(defun org-get-local-archive-location ()
  "Get the archive location applicable at point."
  (let ((re "^#\\+ARCHIVE:[ \t]+\\(\\S-.*\\S-\\)[ \t]*$")
	prop)
    (save-excursion
      (save-restriction
	(widen)
	(setq prop (org-entry-get nil "ARCHIVE" 'inherit))
	(cond
	 ((and prop (string-match "\\S-" prop))
	  prop)
	 ((or (re-search-backward re nil t)
	      (re-search-forward re nil t))
	  (match-string 1))
	 (t org-archive-location (match-string 1)))))))

(defun org-add-archive-files (files)
  "Splice the archive files into the list f files.
This implies visiting all these files and finding out what the
archive file is."
  (apply
   'append
   (mapcar
    (lambda (f)
      (if (not (file-exists-p f))
	  nil
	(with-current-buffer (org-get-agenda-file-buffer f)
	  (cons f (org-all-archive-files)))))
    files)))

(defun org-all-archive-files ()
  "Get a list of all archive files used in the current buffer."
  (let (file files)
    (save-excursion
      (save-restriction
	(goto-char (point-min))
	(while (re-search-forward
		"^\\(#\\+\\|[ \t]*:\\)ARCHIVE:[ \t]+\\(.*\\)"
		nil t)
	  (setq file (org-extract-archive-file
		      (org-match-string-no-properties 2)))
	  (and file (> (length file) 0) (file-exists-p file)
	       (add-to-list 'files file)))))
    (setq files (nreverse files))
    (setq file (org-extract-archive-file))
    (and file (> (length file) 0) (file-exists-p file)
	 (add-to-list 'files file))
    files))

(defun org-extract-archive-file (&optional location)
  (setq location (or location org-archive-location))
  (if (string-match "\\(.*\\)::\\(.*\\)" location)
      (if (= (match-beginning 1) (match-end 1))
	  (buffer-file-name)
	(expand-file-name
	 (format (match-string 1 location) buffer-file-name)))))

(defun org-extract-archive-heading (&optional location)
  (setq location (or location org-archive-location))
  (if (string-match "\\(.*\\)::\\(.*\\)" location)
      (match-string 2 location)))

(defun org-archive-subtree (&optional find-done)
  "Move the current subtree to the archive.
The archive can be a certain top-level heading in the current file, or in
a different file.  The tree will be moved to that location, the subtree
heading be marked DONE, and the current time will be added.

When called with prefix argument FIND-DONE, find whole trees without any
open TODO items and archive them (after getting confirmation from the user).
If the cursor is not at a headline when this comand is called, try all level
1 trees.  If the cursor is on a headline, only try the direct children of
this heading."
  (interactive "P")
  (if find-done
      (org-archive-all-done)
    ;; Save all relevant TODO keyword-relatex variables

    (let ((tr-org-todo-line-regexp org-todo-line-regexp) ; keep despite compiler
	  (tr-org-todo-keywords-1 org-todo-keywords-1)
	  (tr-org-todo-kwd-alist org-todo-kwd-alist)
	  (tr-org-done-keywords org-done-keywords)
	  (tr-org-todo-regexp org-todo-regexp)
	  (tr-org-todo-line-regexp org-todo-line-regexp)
	  (tr-org-odd-levels-only org-odd-levels-only)
	  (this-buffer (current-buffer))
          ;; start of variables that will be used for saving context
	  ;; The compiler complains about them - keep them anyway!
	  (file (abbreviate-file-name (buffer-file-name)))
	  (olpath (mapconcat 'identity (org-get-outline-path) "/"))
	  (time (format-time-string
		 (substring (cdr org-time-stamp-formats) 1 -1)
		 (current-time)))
	  category todo priority ltags itags
          ;; end of variables that will be used for saving context
	  location afile heading buffer level newfile-p)

      ;; Find the local archive location
      (setq location (org-get-local-archive-location)
	    afile (org-extract-archive-file location)
	    heading (org-extract-archive-heading location))
      (unless afile
	(error "Invalid `org-archive-location'"))

      (if (> (length afile) 0)
	  (setq newfile-p (not (file-exists-p afile))
		buffer (find-file-noselect afile))
	(setq buffer (current-buffer)))
      (unless buffer
	(error "Cannot access file \"%s\"" afile))
      (if (and (> (length heading) 0)
	       (string-match "^\\*+" heading))
	  (setq level (match-end 0))
	(setq heading nil level 0))
      (save-excursion
	(org-back-to-heading t)
	;; Get context information that will be lost by moving the tree
	(org-refresh-category-properties)
	(setq category (org-get-category)
	      todo (and (looking-at org-todo-line-regexp)
			(match-string 2))
	      priority (org-get-priority
			(if (match-end 3) (match-string 3) ""))
	      ltags (org-get-tags)
	      itags (org-delete-all ltags (org-get-tags-at)))
	(setq ltags (mapconcat 'identity ltags " ")
	      itags (mapconcat 'identity itags " "))
	;; We first only copy, in case something goes wrong
	;; we need to protect this-command, to avoid kill-region sets it,
	;; which would lead to duplication of subtrees
	(let (this-command) (org-copy-subtree))
	(set-buffer buffer)
	;; Enforce org-mode for the archive buffer
	(if (not (org-mode-p))
	    ;; Force the mode for future visits.
	    (let ((org-insert-mode-line-in-empty-file t)
		  (org-inhibit-startup t))
	      (call-interactively 'org-mode)))
	(when newfile-p
	  (goto-char (point-max))
	  (insert (format "\nArchived entries from file %s\n\n"
			  (buffer-file-name this-buffer))))
	;; Force the TODO keywords of the original buffer
	(let ((org-todo-line-regexp tr-org-todo-line-regexp)
	      (org-todo-keywords-1 tr-org-todo-keywords-1)
	      (org-todo-kwd-alist tr-org-todo-kwd-alist)
	      (org-done-keywords tr-org-done-keywords)
	      (org-todo-regexp tr-org-todo-regexp)
	      (org-todo-line-regexp tr-org-todo-line-regexp)
	      (org-odd-levels-only
	       (if (local-variable-p 'org-odd-levels-only (current-buffer))
		   org-odd-levels-only
		 tr-org-odd-levels-only)))
	  (goto-char (point-min))
	  (show-all)
	  (if heading
	      (progn
		(if (re-search-forward
		     (concat "^" (regexp-quote heading)
			     (org-re "[ \t]*\\(:[[:alnum:]_@:]+:\\)?[ \t]*\\($\\|\r\\)"))
		     nil t)
		    (goto-char (match-end 0))
		  ;; Heading not found, just insert it at the end
		  (goto-char (point-max))
		  (or (bolp) (insert "\n"))
		  (insert "\n" heading "\n")
		  (end-of-line 0))
		;; Make the subtree visible
		(show-subtree)
		(org-end-of-subtree t)
		(skip-chars-backward " \t\r\n")
		(and (looking-at "[ \t\r\n]*")
		     (replace-match "\n\n")))
	    ;; No specific heading, just go to end of file.
	    (goto-char (point-max)) (insert "\n"))
	  ;; Paste
	  (org-paste-subtree (org-get-valid-level level 1))

	  ;; Mark the entry as done
	  (when (and org-archive-mark-done
		     (looking-at org-todo-line-regexp)
		     (or (not (match-end 2))
			 (not (member (match-string 2) org-done-keywords))))
	    (let (org-log-done org-todo-log-states)
	      (org-todo
	       (car (or (member org-archive-mark-done org-done-keywords)
			org-done-keywords)))))

	  ;; Add the context info
	  (when org-archive-save-context-info
	    (let ((l org-archive-save-context-info) e n v)
	      (while (setq e (pop l))
		(when (and (setq v (symbol-value e))
			   (stringp v) (string-match "\\S-" v))
		  (setq n (concat "ARCHIVE_" (upcase (symbol-name e))))
		  (org-entry-put (point) n v)))))

	  ;; Save and kill the buffer, if it is not the same buffer.
	  (if (not (eq this-buffer buffer))
	      (progn (save-buffer) (kill-buffer buffer)))))
      ;; Here we are back in the original buffer.  Everything seems to have
      ;; worked.  So now cut the tree and finish up.
      (let (this-command) (org-cut-subtree))
      (if (and (not (eobp)) (looking-at "[ \t]*$")) (kill-line))
      (message "Subtree archived %s"
	       (if (eq this-buffer buffer)
		   (concat "under heading: " heading)
		 (concat "in file: " (abbreviate-file-name afile)))))))

(defun org-archive-to-archive-sibling ()
  "Archive the current heading by moving it under the archive sibling.
The archive sibling is a sibling of the heading with the heading name
`org-archive-sibling-heading' and an `org-archive-tag' tag.  If this
sibling does not exist, it will be created at the end of the subtree."
  (interactive)
  (save-restriction
    (widen)
    (let (b e pos leader level)
      (org-back-to-heading t)
      (looking-at outline-regexp)
      (setq leader (match-string 0)
	    level (funcall outline-level))
      (setq pos (point))
      (condition-case nil
	  (outline-up-heading 1 t)
	(error (goto-char (point-min))))
      (setq b (point))
      (condition-case nil
	  (org-end-of-subtree t t)
	(error (goto-char (point-max))))
      (setq e (point))
      (goto-char b)
      (unless (re-search-forward
	       (concat "^" (regexp-quote leader)
		       "[ \t]*"
		       org-archive-sibling-heading
		       "[ \t]*:"
		       org-archive-tag ":") e t)
	(goto-char e)
	(or (bolp) (newline))
	(insert leader org-archive-sibling-heading "\n")
	(beginning-of-line 0)
	(org-toggle-tag org-archive-tag 'on))
      (beginning-of-line 1)
      (org-end-of-subtree t t)
      (save-excursion
	(goto-char pos)
	(org-cut-subtree))
      (org-paste-subtree (org-get-valid-level level 1))
      (org-set-property
       "ARCHIVE_TIME"
       (format-time-string
	(substring (cdr org-time-stamp-formats) 1 -1)
	(current-time)))
      (outline-up-heading 1 t)
      (hide-subtree)
      (goto-char pos))))

(defun org-archive-all-done (&optional tag)
  "Archive sublevels of the current tree without open TODO items.
If the cursor is not on a headline, try all level 1 trees.  If
it is on a headline, try all direct children.
When TAG is non-nil, don't move trees, but mark them with the ARCHIVE tag."
  (let ((re (concat "^\\*+ +" org-not-done-regexp)) re1
	(rea (concat ".*:" org-archive-tag ":"))
	(begm (make-marker))
	(endm (make-marker))
	(question (if tag "Set ARCHIVE tag (no open TODO items)? "
		    "Move subtree to archive (no open TODO items)? "))
	beg end (cntarch 0))
    (if (org-on-heading-p)
	(progn
	  (setq re1 (concat "^" (regexp-quote
				 (make-string
				  (1+ (- (match-end 0) (match-beginning 0) 1))
				  ?*))
			    " "))
	  (move-marker begm (point))
	  (move-marker endm (org-end-of-subtree t)))
      (setq re1 "^* ")
      (move-marker begm (point-min))
      (move-marker endm (point-max)))
    (save-excursion
      (goto-char begm)
      (while (re-search-forward re1 endm t)
	(setq beg (match-beginning 0)
	      end (save-excursion (org-end-of-subtree t) (point)))
	(goto-char beg)
	(if (re-search-forward re end t)
	    (goto-char end)
	  (goto-char beg)
	  (if (and (or (not tag) (not (looking-at rea)))
		   (y-or-n-p question))
	      (progn
		(if tag
		    (org-toggle-tag org-archive-tag 'on)
		  (org-archive-subtree))
		(setq cntarch (1+ cntarch)))
	    (goto-char end)))))
    (message "%d trees archived" cntarch)))

(defun org-toggle-archive-tag (&optional find-done)
  "Toggle the archive tag for the current headline.
With prefix ARG, check all children of current headline and offer tagging
the children that do not contain any open TODO items."
  (interactive "P")
  (if find-done
      (org-archive-all-done 'tag)
    (let (set)
      (save-excursion
	(org-back-to-heading t)
	(setq set (org-toggle-tag org-archive-tag))
	(when set (hide-subtree)))
      (and set (beginning-of-line 1))
      (message "Subtree %s" (if set "archived" "unarchived")))))

(provide 'org-archive)

;; arch-tag: 0837f601-9699-43c3-8b90-631572ae6c85
;;; org-archive.el ends here
