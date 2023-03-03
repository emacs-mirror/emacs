;;; org-archive.el --- Archiving for Org             -*- lexical-binding: t; -*-

;; Copyright (C) 2004-2023 Free Software Foundation, Inc.

;; Author: Carsten Dominik <carsten.dominik@gmail.com>
;; Keywords: outlines, hypermedia, calendar, wp
;; URL: https://orgmode.org
;;
;; This file is part of GNU Emacs.
;;
;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:

;; This file contains the archive functionality for Org.

;;; Code:

(require 'org-macs)
(org-assert-version)

(require 'org)
(require 'cl-lib)

(declare-function org-element-type "org-element" (element))
(declare-function org-datetree-find-date-create "org-datetree" (date &optional keep-restriction))
(declare-function org-inlinetask-remove-END-maybe "org-inlinetask" ())

;; From org-element.el
(defvar org-element--cache-avoid-synchronous-headline-re-parsing)

(defcustom org-archive-default-command 'org-archive-subtree
  "The default archiving command."
  :group 'org-archive
  :type '(choice
	  (const org-archive-subtree)
	  (const org-archive-to-archive-sibling)
	  (const org-archive-set-tag)))

(defcustom org-archive-reversed-order nil
  "Non-nil means make the tree first child under the archive heading, not last."
  :group 'org-archive
  :version "24.1"
  :type 'boolean)

(defcustom org-archive-sibling-heading "Archive"
  "Name of the local archive sibling that is used to archive entries locally.
Locally means: in the tree, under a sibling.
See `org-archive-to-archive-sibling' for more information."
  :group 'org-archive
  :type 'string)

(defcustom org-archive-mark-done nil
  "Non-nil means mark entries as DONE when they are moved to the archive file.
This can be a string to set the keyword to use.  When non-nil, Org will
use the first keyword in its list that means done."
  :group 'org-archive
  :type '(choice
	  (const :tag "No" nil)
	  (const :tag "Yes" t)
	  (string :tag "Use this keyword")))

(defcustom org-archive-stamp-time t
  "Non-nil means add a time stamp to entries moved to an archive file.
This variable is obsolete and has no effect anymore, instead add or remove
`time' from the variable `org-archive-save-context-info'."
  :group 'org-archive
  :type 'boolean)

(defcustom org-archive-file-header-format "\nArchived entries from file %s\n\n"
  "The header format string for newly created archive files.
When nil, no header will be inserted.
When a string, a %s formatter will be replaced by the file name."
  :group 'org-archive
  :version "24.4"
  :package-version '(Org . "8.0")
  :type 'string)

(defcustom org-archive-subtree-add-inherited-tags 'infile
  "Non-nil means append inherited tags when archiving a subtree."
  :group 'org-archive
  :version "24.1"
  :type '(choice
	  (const :tag "Never" nil)
	  (const :tag "When archiving a subtree to the same file" infile)
	  (const :tag "Always" t)))

(defcustom org-archive-subtree-save-file-p 'from-org
  "Conditionally save the archive file after archiving a subtree.
This variable can be any of the following symbols:

t              saves in all cases.
`from-org'     prevents saving from an agenda-view.
`from-agenda'  saves only when the archive is initiated from an agenda-view.
nil            prevents saving in all cases.

Note that, regardless of this value, the archive buffer is never
saved when archiving into a location in the current buffer."
  :group 'org-archive
  :package-version '(Org . "9.4")
  :type '(choice
	  (const :tag "Save archive buffer" t)
	  (const :tag "Save when archiving from agenda" from-agenda)
	  (const :tag "Save when archiving from an Org buffer" from-org)
	  (const :tag "Do not save")))

(defcustom org-archive-save-context-info '(time file olpath category todo itags)
  "Parts of context info that should be stored as properties when archiving.
When a subtree is moved to an archive file, it loses information given by
context, like inherited tags, the category, and possibly also the TODO
state (depending on the variable `org-archive-mark-done').
This variable can be a list of any of the following symbols:

time       The time of archiving.
file       The file where the entry originates.
ltags      The local tags, in the headline of the subtree.
itags      The tags the subtree inherits from further up the hierarchy.
todo       The pre-archive TODO state.
category   The category, taken from file name or #+CATEGORY lines.
olpath     The outline path to the item.  These are all headlines above
           the current item, separated by /, like a file path.

For each symbol present in the list, a property will be created in
the archived entry, with a prefix \"ARCHIVE_\", to remember this
information."
  :group 'org-archive
  :type '(set :greedy t
	      (const :tag "Time" time)
	      (const :tag "File" file)
	      (const :tag "Category" category)
	      (const :tag "TODO state" todo)
	      (const :tag "Priority" priority)
	      (const :tag "Inherited tags" itags)
	      (const :tag "Outline path" olpath)
	      (const :tag "Local tags" ltags)))

(defvar org-archive-hook nil
  "Hook run after successfully archiving a subtree.
Hook functions are called with point on the subtree in the
original file.  At this stage, the subtree has been added to the
archive location, but not yet deleted from the original file.")

;;;###autoload
(defun org-add-archive-files (files)
  "Splice the archive files into the list of files.
This implies visiting all these files and finding out what the
archive file is."
  (org-uniquify
   (apply
    'append
    (mapcar
     (lambda (f)
       (if (not (file-exists-p f))
	   nil
	 (with-current-buffer (org-get-agenda-file-buffer f)
	   (cons f (org-all-archive-files)))))
     files))))

(defun org-all-archive-files ()
  "List of all archive files used in the current buffer."
  (let* ((case-fold-search t)
	 (files `(,(car (org-archive--compute-location org-archive-location)))))
    (org-with-point-at 1
      (while (re-search-forward "^[ \t]*:ARCHIVE:" nil t)
	(when (org-at-property-p)
	  (pcase (org-archive--compute-location (match-string 3))
	    (`(,file . ,_)
	     (when (org-string-nw-p file)
	       (cl-pushnew file files :test #'file-equal-p))))))
      (cl-remove-if-not #'file-exists-p (nreverse files)))))

(defun org-archive--compute-location (location)
  "Extract and expand the location from archive LOCATION.
Return a pair (FILE . HEADING) where FILE is the file name and
HEADING the heading of the archive location, as strings.  Raise
an error if LOCATION is not a valid archive location."
  (unless (string-match "::" location)
    (error "Invalid archive location: %S" location))
  (let ((current-file (buffer-file-name (buffer-base-buffer)))
	(file-fmt (substring location 0 (match-beginning 0)))
	(heading-fmt (substring location (match-end 0))))
    (cons
     ;; File part.
     (if (org-string-nw-p file-fmt)
	 (expand-file-name
	  (format file-fmt (file-name-nondirectory current-file)))
       current-file)
     ;; Heading part.
     (format heading-fmt (file-name-nondirectory current-file)))))

;;;###autoload
(defun org-archive-subtree (&optional find-done)
  "Move the current subtree to the archive.
The archive can be a certain top-level heading in the current
file, or in a different file.  The tree will be moved to that
location, the subtree heading be marked DONE, and the current
time will be added.

When called with a single prefix argument FIND-DONE, find whole
trees without any open TODO items and archive them (after getting
confirmation from the user).  When called with a double prefix
argument, find whole trees with timestamps before today and
archive them (after getting confirmation from the user).  If the
cursor is not at a headline when these commands are called, try
all level 1 trees.  If the cursor is on a headline, only try the
direct children of this heading."
  (interactive "P")
  (if (and (org-region-active-p) org-loop-over-headlines-in-active-region)
      (let ((cl (if (eq org-loop-over-headlines-in-active-region 'start-level)
		    'region-start-level 'region))
	    org-loop-over-headlines-in-active-region)
	(org-map-entries
	 `(progn (setq org-map-continue-from (progn (org-back-to-heading) (point)))
		 (org-archive-subtree ,find-done))
	 org-loop-over-headlines-in-active-region
	 cl (if (org-invisible-p) (org-end-of-subtree nil t))))
    (cond
     ((equal find-done '(4))  (org-archive-all-done))
     ((equal find-done '(16)) (org-archive-all-old))
     (t
      ;; Save all relevant TODO keyword-related variables.
      (let* ((tr-org-todo-keywords-1 org-todo-keywords-1)
	     (tr-org-todo-kwd-alist org-todo-kwd-alist)
	     (tr-org-done-keywords org-done-keywords)
	     (tr-org-todo-regexp org-todo-regexp)
	     (tr-org-todo-line-regexp org-todo-line-regexp)
	     (tr-org-odd-levels-only org-odd-levels-only)
	     (this-buffer (current-buffer))
	     (time (format-time-string
                    (org-time-stamp-format 'with-time 'no-brackets)))
	     (file (abbreviate-file-name
		    (or (buffer-file-name (buffer-base-buffer))
			(error "No file associated to buffer"))))
	     (location (org-archive--compute-location
			(or (org-entry-get nil "ARCHIVE" 'inherit)
			    org-archive-location)))
	     (afile (car location))
	     (heading (cdr location))
	     (infile-p (equal file (abbreviate-file-name (or afile ""))))
	     (newfile-p (and (org-string-nw-p afile)
			     (not (file-exists-p afile))))
	     (buffer (cond ((not (org-string-nw-p afile)) this-buffer)
			   ((find-buffer-visiting afile))
			   ((find-file-noselect afile))
			   (t (error "Cannot access file \"%s\"" afile))))
	     (org-odd-levels-only
	      (if (local-variable-p 'org-odd-levels-only (current-buffer))
		  org-odd-levels-only
		tr-org-odd-levels-only))
	     level datetree-date datetree-subheading-p
             ;; Suppress on-the-fly headline updates.
             (org-element--cache-avoid-synchronous-headline-re-parsing t))
	(when (string-match "\\`datetree/\\(\\**\\)" heading)
	  ;; "datetree/" corresponds to 3 levels of headings.
	  (let ((nsub (length (match-string 1 heading))))
	    (setq heading (concat (make-string
				   (+ (if org-odd-levels-only 5 3)
				      (* (org-level-increment) nsub))
				   ?*)
				  (substring heading (match-end 0))))
	    (setq datetree-subheading-p (> nsub 0)))
	  (setq datetree-date (org-date-to-gregorian
			       (or (org-entry-get nil "CLOSED" t) time))))
	(if (and (> (length heading) 0)
		 (string-match "^\\*+" heading))
	    (setq level (match-end 0))
	  (setq heading nil level 0))
	(save-excursion
	  (org-back-to-heading t)
	  ;; Get context information that will be lost by moving the
	  ;; tree.  See `org-archive-save-context-info'.
	  (let* ((all-tags (org-get-tags))
		 (local-tags
		  (cl-remove-if (lambda (tag)
				  (get-text-property 0 'inherited tag))
				all-tags))
		 (inherited-tags
		  (cl-remove-if-not (lambda (tag)
				      (get-text-property 0 'inherited tag))
				    all-tags))
		 (context
		  `((category . ,(org-get-category nil 'force-refresh))
		    (file . ,file)
		    (itags . ,(mapconcat #'identity inherited-tags " "))
		    (ltags . ,(mapconcat #'identity local-tags " "))
		    (olpath . ,(mapconcat #'identity
					  (org-get-outline-path)
					  "/"))
		    (time . ,time)
		    (todo . ,(org-entry-get (point) "TODO")))))
	    ;; We first only copy, in case something goes wrong
	    ;; we need to protect `this-command', to avoid kill-region sets it,
	    ;; which would lead to duplication of subtrees
	    (let (this-command) (org-copy-subtree 1 nil t))
	    (set-buffer buffer)
	    ;; Enforce Org mode for the archive buffer
	    (if (not (derived-mode-p 'org-mode))
		;; Force the mode for future visits.
		(let ((org-insert-mode-line-in-empty-file t)
		      (org-inhibit-startup t))
		  (call-interactively 'org-mode)))
	    (when (and newfile-p org-archive-file-header-format)
	      (goto-char (point-max))
	      (insert (format org-archive-file-header-format
			      (buffer-file-name this-buffer))))
	    (when datetree-date
	      (require 'org-datetree)
	      (org-datetree-find-date-create datetree-date)
	      (org-narrow-to-subtree))
	    ;; Force the TODO keywords of the original buffer
	    (let ((org-todo-line-regexp tr-org-todo-line-regexp)
		  (org-todo-keywords-1 tr-org-todo-keywords-1)
		  (org-todo-kwd-alist tr-org-todo-kwd-alist)
		  (org-done-keywords tr-org-done-keywords)
		  (org-todo-regexp tr-org-todo-regexp)
		  (org-todo-line-regexp tr-org-todo-line-regexp))
	      (goto-char (point-min))
	      (org-fold-show-all '(headings blocks))
	      (if (and heading (not (and datetree-date (not datetree-subheading-p))))
		  (progn
		    (if (re-search-forward
			 (concat "^" (regexp-quote heading)
				 "\\([ \t]+:\\(" org-tag-re ":\\)+\\)?[ \t]*$")
			 nil t)
			(goto-char (match-end 0))
		      ;; Heading not found, just insert it at the end
		      (goto-char (point-max))
		      (or (bolp) (insert "\n"))
		      ;; datetrees don't need too much spacing
		      (insert (if datetree-date "" "\n") heading "\n")
		      (end-of-line 0))
		    ;; Make the subtree visible
		    (org-fold-show-subtree)
		    (if org-archive-reversed-order
			(progn
			  (org-back-to-heading t)
			  (outline-next-heading))
		      (org-end-of-subtree t))
		    (skip-chars-backward " \t\r\n")
		    (and (looking-at "[ \t\r\n]*")
			 ;; datetree archives don't need so much spacing.
			 (replace-match (if datetree-date "\n" "\n\n"))))
		;; No specific heading, just go to end of file, or to the
		;; beginning, depending on `org-archive-reversed-order'.
		(if org-archive-reversed-order
		    (progn
		      (goto-char (point-min))
		      (unless (org-at-heading-p) (outline-next-heading)))
		  (goto-char (point-max))
		  ;; Subtree narrowing can let the buffer end on
		  ;; a headline.  `org-paste-subtree' then deletes it.
		  ;; To prevent this, make sure visible part of buffer
		  ;; always terminates on a new line, while limiting
		  ;; number of blank lines in a date tree.
		  (unless (and datetree-date (bolp)) (insert "\n"))))
	      ;; Paste
	      (org-paste-subtree (org-get-valid-level level (and heading 1)))
	      ;; Shall we append inherited tags?
	      (and inherited-tags
		   (or (and (eq org-archive-subtree-add-inherited-tags 'infile)
			    infile-p)
		       (eq org-archive-subtree-add-inherited-tags t))
		   (org-set-tags all-tags))
	      ;; Mark the entry as done
	      (when (and org-archive-mark-done
			 (let ((case-fold-search nil))
			   (looking-at org-todo-line-regexp))
			 (or (not (match-end 2))
			     (not (member (match-string 2) org-done-keywords))))
		(let (org-log-done org-todo-log-states)
		  (org-todo
		   (car (or (member org-archive-mark-done org-done-keywords)
			    org-done-keywords)))))

	      ;; Add the context info.
	      (dolist (item org-archive-save-context-info)
		(let ((value (cdr (assq item context))))
		  (when (org-string-nw-p value)
		    (org-entry-put
		     (point)
		     (concat "ARCHIVE_" (upcase (symbol-name item)))
		     value))))
	      ;; Save the buffer, if it is not the same buffer and
	      ;; depending on `org-archive-subtree-save-file-p'.
	      (unless (eq this-buffer buffer)
		(when (or (eq org-archive-subtree-save-file-p t)
			  (eq org-archive-subtree-save-file-p
			      (if (boundp 'org-archive-from-agenda)
				  'from-agenda
				'from-org)))
		  (save-buffer)))
	      (widen))))
	;; Here we are back in the original buffer.  Everything seems
	;; to have worked.  So now run hooks, cut the tree and finish
	;; up.
	(run-hooks 'org-archive-hook)
	(let (this-command) (org-cut-subtree))
	(when (featurep 'org-inlinetask)
	  (org-inlinetask-remove-END-maybe))
	(setq org-markers-to-move nil)
	(when org-provide-todo-statistics
	  (save-excursion
	    ;; Go to parent, even if no children exist.
	    (org-up-heading-safe)
	    ;; Update cookie of parent.
	    (org-update-statistics-cookies nil)))
	(message "Subtree archived %s"
		 (if (eq this-buffer buffer)
		     (concat "under heading: " heading)
		   (concat "in file: " (abbreviate-file-name afile)))))))
    (org-fold-reveal)
    (if (looking-at "^[ \t]*$")
	(outline-next-visible-heading 1))))

;;;###autoload
(defun org-archive-to-archive-sibling ()
  "Archive the current heading by moving it under the archive sibling.

The archive sibling is a sibling of the heading with the heading name
`org-archive-sibling-heading' and an `org-archive-tag' tag.  If this
sibling does not exist, it will be created at the end of the subtree.

Archiving time is retained in the ARCHIVE_TIME node property."
  (interactive)
  (if (and (org-region-active-p) org-loop-over-headlines-in-active-region)
      (let ((cl (when (eq org-loop-over-headlines-in-active-region 'start-level)
		  'region-start-level 'region))
	    org-loop-over-headlines-in-active-region)
	(org-map-entries
	 '(progn (setq org-map-continue-from
		       (progn (org-back-to-heading)
			      (if (looking-at (concat "^.*:" org-archive-tag ":.*$"))
				  (org-end-of-subtree t)
				(point))))
		 (when (org-at-heading-p)
		   (org-archive-to-archive-sibling)))
	 org-loop-over-headlines-in-active-region
	 cl (if (org-invisible-p) (org-end-of-subtree nil t))))
    (save-restriction
      (widen)
      (let (b e pos leader level)
	(org-back-to-heading t)
	(looking-at org-outline-regexp)
	(setq leader (match-string 0)
	      level (funcall outline-level))
	(setq pos (point-marker))
        ;; Advance POS upon insertion in front of it.
        (set-marker-insertion-type pos t)
	(condition-case nil
	    (outline-up-heading 1 t)
	  (error (setq e (point-max)) (goto-char (point-min))))
	(setq b (point))
	(unless e
	  (condition-case nil
	      (org-end-of-subtree t t)
	    (error (goto-char (point-max))))
	  (setq e (point)))
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
	(if org-archive-reversed-order
	    (outline-next-heading)
	  (org-end-of-subtree t t))
	(save-excursion
	  (goto-char pos)
	  (let ((this-command this-command)) (org-cut-subtree)))
	(org-paste-subtree (org-get-valid-level level 1))
	(org-set-property
	 "ARCHIVE_TIME"
	 (format-time-string
          (org-time-stamp-format 'with-time 'no-brackets)))
	(outline-up-heading 1 t)
	(org-fold-subtree t)
	(org-cycle-show-empty-lines 'folded)
	(when org-provide-todo-statistics
	  ;; Update TODO statistics of parent.
	  (org-update-parent-todo-statistics))
	(goto-char pos)))
    (org-fold-reveal)
    (if (looking-at "^[ \t]*$")
	(outline-next-visible-heading 1))))

(defun org-archive-all-done (&optional tag)
  "Archive sublevels of the current tree without open TODO items.
If the cursor is not on a headline, try all level 1 trees.  If
it is on a headline, try all direct children.
When TAG is non-nil, don't move trees, but mark them with the ARCHIVE tag."
  (org-archive-all-matches
   (lambda (_beg end)
     (let ((case-fold-search nil))
       (unless (re-search-forward org-not-done-heading-regexp end t)
	 "no open TODO items")))
   tag))

(defun org-archive-all-old (&optional tag)
  "Archive sublevels of the current tree with timestamps prior to today.
If the cursor is not on a headline, try all level 1 trees.  If
it is on a headline, try all direct children.
When TAG is non-nil, don't move trees, but mark them with the ARCHIVE tag."
  (org-archive-all-matches
   (lambda (_beg end)
     (let (ts)
       (and (re-search-forward org-ts-regexp end t)
	    (setq ts (match-string 0))
	    (< (org-time-stamp-to-now ts) 0)
	    (if (not (looking-at
		      (concat "--\\(" org-ts-regexp "\\)")))
		(concat "old timestamp " ts)
	      (setq ts (concat "old timestamp " ts (match-string 0)))
	      (and (< (org-time-stamp-to-now (match-string 1)) 0)
		   ts)))))
   tag))

(defun org-archive-all-matches (predicate &optional tag)
  "Archive sublevels of the current tree that match PREDICATE.

PREDICATE is a function of two arguments, BEG and END, which
specify the beginning and end of the headline being considered.
It is called with point positioned at BEG.  The headline will be
archived if PREDICATE returns non-nil.  If the return value of
PREDICATE is a string, it should describe the reason for
archiving the heading.

If the cursor is not on a headline, try all level 1 trees.  If it
is on a headline, try all direct children.  When TAG is non-nil,
don't move trees, but mark them with the ARCHIVE tag."
  (let ((rea (concat ".*:" org-archive-tag ":")) re1
	(begm (make-marker))
	(endm (make-marker))
	(question (if tag "Set ARCHIVE tag? "
		    "Move subtree to archive? "))
	reason beg end (cntarch 0))
    (if (org-at-heading-p)
	(progn
	  (setq re1 (concat "^" (regexp-quote
				 (make-string
				  (+ (- (match-end 0) (match-beginning 0) 1)
				     (if org-odd-levels-only 2 1))
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
	(if (not (setq reason (funcall predicate beg end)))
	    (goto-char end)
	  (goto-char beg)
	  (if (and (or (not tag) (not (looking-at rea)))
		   (y-or-n-p
		    (if (stringp reason)
			(concat question "(" reason ")")
		      question)))
	      (progn
		(if tag
		    (org-toggle-tag org-archive-tag 'on)
		  (org-archive-subtree))
		(setq cntarch (1+ cntarch)))
	    (goto-char end)))))
    (message "%d trees archived" cntarch)))

;;;###autoload
(defun org-toggle-archive-tag (&optional find-done)
  "Toggle the archive tag for the current headline.
With prefix ARG, check all children of current headline and offer tagging
the children that do not contain any open TODO items."
  (interactive "P")
  (if (and (org-region-active-p) org-loop-over-headlines-in-active-region)
      (let ((cl (if (eq org-loop-over-headlines-in-active-region 'start-level)
		    'region-start-level 'region))
	    org-loop-over-headlines-in-active-region)
	(org-map-entries
	 `(org-toggle-archive-tag ,find-done)
	 org-loop-over-headlines-in-active-region
	 cl (if (org-invisible-p) (org-end-of-subtree nil t))))
    (if find-done
	(org-archive-all-done 'tag)
      (let (set)
	(save-excursion
	  (org-back-to-heading t)
	  (setq set (org-toggle-tag org-archive-tag))
	  (when set (org-fold-subtree t)))
	(and set (beginning-of-line 1))
	(message "Subtree %s" (if set "archived" "unarchived"))))))

(defun org-archive-set-tag ()
  "Set the ARCHIVE tag."
  (interactive)
  (if (and (org-region-active-p) org-loop-over-headlines-in-active-region)
      (let ((cl (if (eq org-loop-over-headlines-in-active-region 'start-level)
		    'region-start-level 'region))
	    org-loop-over-headlines-in-active-region)
	(org-map-entries
	 'org-archive-set-tag
	 org-loop-over-headlines-in-active-region
	 cl (if (org-invisible-p) (org-end-of-subtree nil t))))
    (org-toggle-tag org-archive-tag 'on)))

;;;###autoload
(defun org-archive-subtree-default ()
  "Archive the current subtree with the default command.
This command is set with the variable `org-archive-default-command'."
  (interactive)
  (call-interactively org-archive-default-command))

;;;###autoload
(defun org-archive-subtree-default-with-confirmation ()
  "Archive the current subtree with the default command.
This command is set with the variable `org-archive-default-command'."
  (interactive)
  (if (y-or-n-p "Archive this subtree or entry? ")
      (call-interactively org-archive-default-command)
    (error "Abort")))

(provide 'org-archive)

;; Local variables:
;; generated-autoload-file: "org-loaddefs.el"
;; End:

;;; org-archive.el ends here
