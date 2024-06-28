;;; org-refile.el --- Refile Org Subtrees             -*- lexical-binding: t; -*-

;; Copyright (C) 2010-2024 Free Software Foundation, Inc.

;; Author: Carsten Dominik <carsten.dominik@gmail.com>
;; Keywords: outlines, hypermedia, calendar, text
;;
;; This file is part of GNU Emacs.

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

;;; Commentary:

;; Org Refile allows you to refile subtrees to various locations.

;;; Code:
(require 'org-macs)
(org-assert-version)

(require 'org)

(declare-function org-inlinetask-remove-END-maybe "org-inlinetask" ())

(defgroup org-refile nil
  "Options concerning refiling entries in Org mode."
  :tag "Org Refile"
  :group 'org)

(defcustom org-log-refile nil
  "Information to record when a task is refiled.

Possible values are:

nil     Don't add anything
time    Add a time stamp to the task
note    Prompt for a note and add it with template `org-log-note-headings'

This option can also be set with on a per-file-basis with

   #+STARTUP: nologrefile
   #+STARTUP: logrefile
   #+STARTUP: lognoterefile

You can have local logging settings for a subtree by setting the LOGGING
property to one or more of these keywords.

When bulk-refiling, e.g., from the agenda, the value `note' is
forbidden and will temporarily be changed to `time'."
  :group 'org-refile
  :group 'org-progress
  :version "24.1"
  :type '(choice
	  (const :tag "No logging" nil)
	  (const :tag "Record timestamp" time)
	  (const :tag "Record timestamp with note." note)))

(defcustom org-refile-targets nil
  "Targets for refiling entries with `\\[org-refile]'.
This is a list of cons cells.  Each cell contains:
- a specification of the files to be considered, either a list of files,
  or a symbol whose function or variable value will be used to retrieve
  a file name or a list of file names.  If you use `org-agenda-files' for
  that, all agenda files will be scanned for targets.  Nil means consider
  headings in the current buffer.
- A specification of how to find candidate refile targets.  This may be
  any of:
  - a cons cell (:tag . \"TAG\") to identify refile targets by a tag.
    This tag has to be present in all target headlines, inheritance will
    not be considered.
  - a cons cell (:todo . \"KEYWORD\") to identify refile targets by
    todo keyword.
  - a cons cell (:regexp . \"REGEXP\") with a regular expression matching
    headlines that are refiling targets.
  - a cons cell (:level . N).  Any headline of level N is considered a target.
    Note that, when `org-odd-levels-only' is set, level corresponds to
    order in hierarchy, not to the number of stars.
  - a cons cell (:maxlevel . N).  Any headline with level <= N is a target.
    Note that, when `org-odd-levels-only' is set, level corresponds to
    order in hierarchy, not to the number of stars.

Each element of this list generates a set of possible targets.
The union of these sets is presented (with completion) to
the user by `org-refile'.

You can set the variable `org-refile-target-verify-function' to a function
to verify each headline found by the simple criteria above.

When this variable is nil, all top-level headlines in the current buffer
are used, equivalent to the value `((nil . (:level . 1)))'."
  :group 'org-refile
  :type '(repeat
	  (cons
	   (choice :value org-agenda-files
		   (const :tag "All agenda files" org-agenda-files)
		   (const :tag "Current buffer" nil)
		   (function) (variable) (file))
	   (choice :tag "Identify target headline by"
		   (cons :tag "Specific tag" (const :value :tag) (string))
		   (cons :tag "TODO keyword" (const :value :todo) (string))
		   (cons :tag "Regular expression" (const :value :regexp) (regexp))
		   (cons :tag "Level number" (const :value :level) (integer))
		   (cons :tag "Max Level number" (const :value :maxlevel) (integer))))))

(defcustom org-refile-target-verify-function nil
  "Function to verify if the headline at point should be a refile target.
The function will be called without arguments, with point at the
beginning of the headline.  It should return t and leave point
where it is if the headline is a valid target for refiling.

If the target should not be selected, the function must return nil.
In addition to this, it may move point to a place from where the search
should be continued.  For example, the function may decide that the entire
subtree of the current entry should be excluded and move point to the end
of the subtree."
  :group 'org-refile
  :type '(choice
	  (const nil)
	  (function)))

(defcustom org-refile-use-cache nil
  "Non-nil means cache refile targets to speed up the process.
\\<org-mode-map>\
The cache for a particular file will be updated automatically when
the buffer has been killed, or when any of the marker used for flagging
refile targets no longer points at a live buffer.
If you have added new entries to a buffer that might themselves be targets,
you need to clear the cache manually by pressing `C-0 \\[org-refile]' or,
if you find that easier, \
`\\[universal-argument] \\[universal-argument] \\[universal-argument] \
\\[org-refile]'."
  :group 'org-refile
  :version "24.1"
  :type 'boolean)

(defcustom org-refile-use-outline-path nil
  "Non-nil means provide refile targets as paths.
So a level 3 headline will be available as level1/level2/level3.

When the value is `file', also include the file name (without directory)
into the path.  In this case, you can also stop the completion after
the file name, to get entries inserted as top level in the file.

When `full-file-path', include the full file path.

When `buffer-name', use the buffer name."
  :group 'org-refile
  :package-version '(Org . "9.6")
  :type '(choice
	  (const :tag "Not" nil)
	  (const :tag "Yes" t)
	  (const :tag "Start with file name" file)
	  (const :tag "Start with full file path" full-file-path)
	  (const :tag "Start with buffer name" buffer-name)
	  (const :tag "Start with document title" title)))

(defcustom org-outline-path-complete-in-steps t
  "Non-nil means complete the outline path in hierarchical steps.
When Org uses the refile interface to select an outline path (see
`org-refile-use-outline-path'), the completion of the path can be
done in a single go, or it can be done in steps down the headline
hierarchy.  Going in steps is probably the best if you do not use
a special completion package like `ido' or `icicles'.  However,
when using these packages, going in one step can be very fast,
while still showing the whole path to the entry."
  :group 'org-refile
  :type 'boolean)

(defcustom org-refile-allow-creating-parent-nodes nil
  "Non-nil means allow the creation of new nodes as refile targets.
New nodes are then created by adding \"/new node name\" to the completion
of an existing node.  When the value of this variable is `confirm',
new node creation must be confirmed by the user (recommended).
When nil, the completion must match an existing entry.

Note that, if the new heading is not seen by the criteria
listed in `org-refile-targets', multiple instances of the same
heading would be created by trying again to file under the new
heading."
  :group 'org-refile
  :type '(choice
	  (const :tag "Never" nil)
	  (const :tag "Always" t)
	  (const :tag "Prompt for confirmation" confirm)))

(defcustom org-refile-active-region-within-subtree nil
  "Non-nil means also refile active region within a subtree.

By default `org-refile' doesn't allow refiling regions if they
don't contain a set of subtrees, but it might be convenient to
do so sometimes: in that case, the first line of the region is
converted to a headline before refiling."
  :group 'org-refile
  :version "24.1"
  :type 'boolean)

(defvar org-refile-target-table nil
  "The list of refile targets, created by `org-refile'.")

(defvar org-refile-cache nil
  "Cache for refile targets.")

(defvar org-refile-markers nil
  "All the markers used for caching refile locations.")

;; Add org refile commands to the main org menu
(mapc (lambda (i) (easy-menu-add-item
		   org-org-menu
		   '("Edit Structure") i))
      '(["Refile Subtree" org-refile (org-in-subtree-not-table-p)]
	["Refile and copy Subtree" org-refile-copy (org-in-subtree-not-table-p)]))

(defun org-refile-marker (pos)
  "Return a new refile marker at POS, but only if caching is in use.
When `org-refile-use-cache' is nil, just return POS."
  (if (not org-refile-use-cache)
      pos
    (let ((m (make-marker)))
      (move-marker m pos)
      (push m org-refile-markers)
      m)))

(defun org-refile-cache-clear ()
  "Clear the refile cache and disable all the markers."
  (dolist (m org-refile-markers) (move-marker m nil))
  (setq org-refile-markers nil)
  (setq org-refile-cache nil)
  (message "Refile cache has been cleared"))

(defun org-refile-cache-check-set (set)
  "Check if all the markers in the cache still have live buffers."
  (let (marker)
    (catch 'exit
      (while (and set (setq marker (nth 3 (pop set))))
	;; If `org-refile-use-outline-path' is 'file, marker may be nil
	(when (and marker (null (marker-buffer marker)))
	  (message "Please regenerate the refile cache with `C-0 C-c C-w'")
	  (sit-for 3)
	  (throw 'exit nil)))
      t)))

(defun org-refile-cache-put (set &rest identifiers)
  "Push the refile targets SET into the cache, under IDENTIFIERS."
  (let* ((key (sha1 (prin1-to-string identifiers)))
	 (entry (assoc key org-refile-cache)))
    (if entry
	(setcdr entry set)
      (push (cons key set) org-refile-cache))))

(defun org-refile-cache-get (&rest identifiers)
  "Retrieve the cached value for refile targets given by IDENTIFIERS."
  (cond
   ((not org-refile-cache) nil)
   ((not org-refile-use-cache) (org-refile-cache-clear) nil)
   (t
    (let ((set (cdr (assoc (sha1 (prin1-to-string identifiers))
			   org-refile-cache))))
      (and set (org-refile-cache-check-set set) set)))))

(defun org-refile-get-targets (&optional default-buffer)
  "Produce a table with refile targets."
  (let ((case-fold-search nil)
	;; otherwise org confuses "TODO" as a kw and "Todo" as a word
	(entries (or org-refile-targets '((nil . (:level . 1)))))
	targets tgs files desc descre)
    (message "Getting targets...")
    (cl-assert (listp entries) t "`org-refile-targets' must be a list of targets")
    (with-current-buffer (or default-buffer (current-buffer))
      (dolist (entry entries)
        (cl-assert (consp entry) t "Refile target must be a cons cell (FILES . SPECIFICATION)")
	(setq files (car entry) desc (cdr entry))
	(cond
	 ((null files) (setq files (list (current-buffer))))
	 ((eq files 'org-agenda-files)
	  (setq files (org-agenda-files 'unrestricted)))
	 ((and (symbolp files) (fboundp files))
	  (setq files (funcall files)))
	 ((and (symbolp files) (boundp files))
	  (setq files (symbol-value files))))
	(when (stringp files) (setq files (list files)))
        ;; Allow commonly used (FILE :maxlevel N) and similar values.
        (when (and (listp (cdr desc)) (null (cddr desc)))
          (setq desc (cons (car desc) (cadr desc))))
        (condition-case err
	    (cond
	     ((eq (car desc) :tag)
	      (setq descre (concat "^\\*+[ \t]+.*?:" (regexp-quote (cdr desc)) ":")))
	     ((eq (car desc) :todo)
	      (setq descre (concat "^\\*+[ \t]+" (regexp-quote (cdr desc)) "[ \t]")))
	     ((eq (car desc) :regexp)
	      (setq descre (cdr desc)))
	     ((eq (car desc) :level)
	      (setq descre (concat "^\\*\\{" (number-to-string
					    (if org-odd-levels-only
					        (1- (* 2 (cdr desc)))
					      (cdr desc)))
			           "\\}[ \t]")))
	     ((eq (car desc) :maxlevel)
	      (setq descre (concat "^\\*\\{1," (number-to-string
					      (if org-odd-levels-only
					          (1- (* 2 (cdr desc)))
					        (cdr desc)))
			           "\\}[ \t]")))
	     (t (error "Bad refiling target description %s" desc)))
          (error
           (error "Error parsing refiling target description: %s"
                  (error-message-string err))))
	(dolist (f files)
	  (with-current-buffer (if (bufferp f) f (org-get-agenda-file-buffer f))
            (unless (derived-mode-p 'org-mode)
              (error "Major mode in refile target buffer \"%s\" must be `org-mode'" f))
	    (or
	     (setq tgs (org-refile-cache-get (buffer-file-name) descre))
	     (progn
	       (when (bufferp f)
		 (setq f (buffer-file-name (buffer-base-buffer f))))
	       (setq f (and f (expand-file-name f)))
	       (when (eq org-refile-use-outline-path 'file)
		 (push (list (and f (file-name-nondirectory f)) f nil nil) tgs))
	       (when (eq org-refile-use-outline-path 'buffer-name)
		 (push (list (buffer-name (buffer-base-buffer)) f nil nil) tgs))
	       (when (eq org-refile-use-outline-path 'full-file-path)
		 (push (list (and (buffer-file-name (buffer-base-buffer))
                                  (file-truename (buffer-file-name (buffer-base-buffer))))
                             f nil nil) tgs))
               (when (eq org-refile-use-outline-path 'title)
                 (push (list (or (org-get-title)
                                 (and f (file-name-nondirectory f)))
                             f nil nil)
                       tgs))
	       (org-with-wide-buffer
		(goto-char (point-min))
		(setq org-outline-path-cache nil)
		(while (re-search-forward descre nil t)
		  (forward-line 0)
		  (let ((case-fold-search nil))
		    (looking-at org-complex-heading-regexp))
		  (let ((begin (point))
			(heading (match-string-no-properties 4)))
		    (unless (or (and
				 org-refile-target-verify-function
				 (not
				  (funcall org-refile-target-verify-function)))
				(not heading))
		      (let ((re (format org-complex-heading-regexp-format
					(regexp-quote heading)))
			    (target
			     (if (not org-refile-use-outline-path) heading
			       (mapconcat
				#'identity
				(append
				 (pcase org-refile-use-outline-path
				   (`file (list
                                           (and (buffer-file-name (buffer-base-buffer))
                                                (file-name-nondirectory
                                                 (buffer-file-name (buffer-base-buffer))))))
                                   (`title (list
                                            (or (org-get-title)
                                                (and (buffer-file-name (buffer-base-buffer))
                                                     (file-name-nondirectory
                                                      (buffer-file-name (buffer-base-buffer)))))))
                                   (`full-file-path
				    (list (buffer-file-name
					   (buffer-base-buffer))))
				   (`buffer-name
				    (list (buffer-name
					   (buffer-base-buffer))))
				   (_ nil))
				 (mapcar (lambda (s) (replace-regexp-in-string
						 "/" "\\/" s nil t))
					 (org-get-outline-path t t)))
				"/"))))
			(push (list target f re (org-refile-marker (point)))
			      tgs)))
		    (when (= (point) begin)
		      ;; Verification function has not moved point.
		      (end-of-line)))))))
	    (when org-refile-use-cache
	      (org-refile-cache-put tgs (buffer-file-name) descre))
	    (setq targets (append tgs targets))))))
    (message "Getting targets...done")
    (delete-dups (nreverse targets))))

(defvar org-refile-history nil
  "History for refiling operations.")

(defvar org-after-refile-insert-hook nil
  "Hook run after `org-refile' has inserted its stuff at the new location.
Note that this is still *before* the stuff will be removed from
the *old* location.")

(defvar org-refile-keep nil
  "Non-nil means `org-refile' will copy instead of refile.")

;;;###autoload
(defun org-refile-copy ()
  "Like `org-refile', but preserve the refiled subtree."
  (interactive)
  (let ((org-refile-keep t))
    (org-refile nil nil nil "Copy")))

;;;###autoload
(defun org-refile-reverse (&optional arg default-buffer rfloc msg)
  "Refile while temporarily toggling `org-reverse-note-order'.
So if `org-refile' would append the entry as the last entry under
the target heading, `org-refile-reverse' will prepend it as the
first entry, and vice-versa."
  (interactive "P")
  (let ((org-reverse-note-order (not (org-notes-order-reversed-p))))
    (org-refile arg default-buffer rfloc msg)))

(defvar org-capture-last-stored-marker)


;;;###autoload
(defun org-refile (&optional arg default-buffer rfloc msg)
  "Move the entry or entries at point to another heading.

The list of target headings is compiled using the information in
`org-refile-targets', which see.

At the target location, the entry is filed as a subitem of the
target heading.  Depending on `org-reverse-note-order', the new
subitem will either be the first or the last subitem.

If there is an active region, all entries in that region will be
refiled.  However, the region must fulfill the requirement that
the first heading sets the top-level of the moved text.

With a `\\[universal-argument]' ARG, the command will only visit the target \
location
and not actually move anything.

With a prefix `\\[universal-argument] \\[universal-argument]', go to the \
location where the last
refiling operation has put the subtree.

With a numeric prefix argument of `2', refile to the running clock.

With a numeric prefix argument of `3', emulate `org-refile-keep'
being set to t and copy to the target location, don't move it.
Beware that keeping refiled entries may result in duplicated ID
properties.

RFLOC can be a refile location obtained in a different way.  It
should be a list with the following 4 elements:

1. Name - an identifier for the refile location, typically the
headline text
2. File - the file the refile location is in
3. nil - used for generating refile location candidates, not
needed when passing RFLOC
4. Position - the position in the specified file of the
headline to refile under

MSG is a string to replace \"Refile\" in the default prompt with
another verb.  E.g. `org-refile-copy' sets this parameter to \"Copy\".

See also `org-refile-use-outline-path'.

If you are using target caching (see `org-refile-use-cache'), you
have to clear the target cache in order to find new targets.
This can be done with a `0' prefix (`C-0 C-c C-w') or a triple
prefix argument (`C-u C-u C-u C-c C-w')."
  (interactive "P")
  (if (member arg '(0 (64)))
      (org-refile-cache-clear)
    (let* ((actionmsg (cond (msg msg)
			    ((equal arg 3) "Refile (and keep)")
			    (t "Refile")))
	   (regionp (org-region-active-p))
	   (region-start (and regionp (region-beginning)))
	   (region-end (and regionp (region-end)))
	   (org-refile-keep (if (equal arg 3) t org-refile-keep))
	   pos it nbuf file level reversed)
      (setq last-command nil)
      (when regionp
	(goto-char region-start)
	(forward-line 0)
	(setq region-start (point))
	(unless (or (org-kill-is-subtree-p
		     (buffer-substring region-start region-end))
		    (prog1 org-refile-active-region-within-subtree
                      (let ((s (line-end-position)))
			(org-toggle-heading)
                        (setq region-end (+ (- (line-end-position) s) region-end)))))
	  (user-error "The region is not a (sequence of) subtree(s)")))
      (if (equal arg '(16))
	  (org-refile-goto-last-stored)
	(when (or
	       (and (equal arg 2)
		    org-clock-hd-marker (marker-buffer org-clock-hd-marker)
		    (prog1
			(setq it (list (or org-clock-heading "running clock")
				       (buffer-file-name
					(marker-buffer org-clock-hd-marker))
				       ""
				       (marker-position org-clock-hd-marker)))
		      (setq arg nil)))
	       (setq it
		     (or rfloc
			 (let (heading-text)
			   (save-excursion
			     (unless (and arg (listp arg))
			       (org-back-to-heading t)
			       (setq heading-text
				     (replace-regexp-in-string
				      org-link-bracket-re
				      "\\2"
				      (or (nth 4 (org-heading-components))
					  ""))))
			     (org-refile-get-location
			      (cond ((and arg (listp arg)) "Goto")
				    (regionp (concat actionmsg " region to"))
				    (t (concat actionmsg " subtree \""
					       heading-text "\" to")))
			      default-buffer
			      (and (not (equal '(4) arg))
				   org-refile-allow-creating-parent-nodes)))))))
	  (setq file (nth 1 it)
		pos (nth 3 it))
	  (when (and (not arg)
		     pos
		     (equal (buffer-file-name) file)
		     (if regionp
			 (and (>= pos region-start)
			      (<= pos region-end))
		       (and (>= pos (save-excursion
                                     (org-back-to-heading t)
                                     (point)))
			    (< pos (save-excursion
				     (org-end-of-subtree t t))))))
	    (error "Cannot refile to position inside the tree or region"))
	  (setq nbuf (find-file-noselect file 'nowarn))
	  (if (and arg (not (equal arg 3)))
	      (progn
		(pop-to-buffer-same-window nbuf)
		(goto-char (cond (pos)
				 ((org-notes-order-reversed-p) (point-min))
				 (t (point-max))))
		(org-fold-show-context 'org-goto))
	    (if regionp
		(progn
		  (org-kill-new (buffer-substring region-start region-end))
		  (org-save-markers-in-region region-start region-end))
	      (org-copy-subtree 1 nil t))
            (let ((origin (point-marker)))
              ;; Handle special case when we refile to exactly same
              ;; location with tree promotion/demotion.  Point marker
              ;; saved by `org-width-wide-buffer' (`save-excursion')
              ;; will then remain before the inserted subtree in
              ;; unexpected location.
              (set-marker-insertion-type origin t)
	      (with-current-buffer (setq nbuf (find-file-noselect file 'nowarn))
	        (setq reversed (org-notes-order-reversed-p))
	        (org-with-wide-buffer
	         (if pos
		     (progn
		       (goto-char pos)
		       (setq level (org-get-valid-level (funcall outline-level) 1))
		       (goto-char
		        (if reversed
			    (or (outline-next-heading) (point-max))
			  (or (save-excursion (org-get-next-sibling))
			      (org-end-of-subtree t t)
			      (point-max)))))
		   (setq level 1)
		   (if (not reversed)
		       (goto-char (point-max))
		     (goto-char (point-min))
		     (or (outline-next-heading) (goto-char (point-max)))))
	         (unless (bolp) (newline))
	         (org-paste-subtree level nil nil t)
	         ;; Record information, according to `org-log-refile'.
	         ;; Do not prompt for a note when refiling multiple
	         ;; headlines, however.  Simply add a time stamp.
	         (cond
		  ((not org-log-refile))
		  (regionp
		   (org-map-region
		    (lambda () (org-add-log-setup 'refile nil nil 'time))
		    (point)
		    (+ (point) (- region-end region-start))))
		  (t
		   (org-add-log-setup 'refile nil nil org-log-refile)))
	         (and org-auto-align-tags
		      (let ((org-loop-over-headlines-in-active-region nil))
		        (org-align-tags)))
	         (let ((bookmark-name (plist-get org-bookmark-names-plist
					         :last-refile)))
		   (when bookmark-name
                     (condition-case err
	                 (bookmark-set bookmark-name)
                       (error
                        (message (format "Bookmark set error: %S" err))))))
	         ;; If we are refiling for capture, make sure that the
	         ;; last-capture pointers point here
	         (when (bound-and-true-p org-capture-is-refiling)
		   (let ((bookmark-name (plist-get org-bookmark-names-plist
						   :last-capture-marker)))
		     (when bookmark-name
                       (condition-case err
	                   (bookmark-set bookmark-name)
                         (error
                          (message (format "Bookmark set error: %S" err))))))
		   (move-marker org-capture-last-stored-marker (point)))
                 (deactivate-mark)
	         (run-hooks 'org-after-refile-insert-hook)))
              ;; Go back to ORIGIN.
              (goto-char origin))
	    (unless org-refile-keep
	      (if regionp
		  (delete-region (point) (+ (point) (- region-end region-start)))
		(org-preserve-local-variables
		 (delete-region
		  (and (org-back-to-heading t) (point))
		  (min (1+ (buffer-size)) (org-end-of-subtree t t) (point))))))
	    (when (featurep 'org-inlinetask)
	      (org-inlinetask-remove-END-maybe))
	    (setq org-markers-to-move nil)
	    (message "%s to \"%s\" in file %s: done" actionmsg
		     (car it) file)))))))

(defun org-refile-goto-last-stored ()
  "Go to the location where the last refile was stored."
  (interactive)
  (bookmark-jump (plist-get org-bookmark-names-plist :last-refile))
  (message "This is the location of the last refile"))

(defun org-refile--get-location (refloc tbl)
  "When user refile to REFLOC, find the associated target in TBL.
Also check `org-refile-target-table'."
  (car (delq
	nil
	(mapcar
	 (lambda (r) (or (assoc r tbl)
			 (assoc r org-refile-target-table)))
	 (list (replace-regexp-in-string "/$" "" refloc)
	       (replace-regexp-in-string "\\([^/]\\)$" "\\1/" refloc))))))

(defun org-refile-get-location (&optional prompt default-buffer new-nodes)
  "Prompt the user for a refile location, using PROMPT.
PROMPT should not be suffixed with a colon and a space, because
this function appends the default value from
`org-refile-history' automatically, if that is not empty."
  (let ((org-refile-targets org-refile-targets)
	(org-refile-use-outline-path org-refile-use-outline-path))
    (setq org-refile-target-table (org-refile-get-targets default-buffer)))
  (unless org-refile-target-table
    (user-error "No refile targets"))
  (let* ((cbuf (current-buffer))
	 (cfn (buffer-file-name (buffer-base-buffer cbuf)))
	 (cfunc (if (and org-refile-use-outline-path
			 org-outline-path-complete-in-steps)
		    #'org-olpath-completing-read
		  #'completing-read))
	 (extra (if org-refile-use-outline-path "/" ""))
	 (cbnex (concat (buffer-name) extra))
	 (filename (and cfn (file-truename cfn)))
	 (tbl (mapcar
	       (lambda (x)
		 (if (and (not (member org-refile-use-outline-path
				       '(file full-file-path title)))
			  (not (equal filename (file-truename (nth 1 x)))))
		     (cons (concat (car x) extra " ("
				   (file-name-nondirectory (nth 1 x)) ")")
			   (cdr x))
		   (cons (concat (car x) extra) (cdr x))))
	       org-refile-target-table))
	 (completion-ignore-case t)
	 cdef
         (prompt (let ((default (or (car org-refile-history)
                                    (and (assoc cbnex tbl) (setq cdef cbnex)
                                         cbnex))))
                   (org-format-prompt prompt default)))
	 pa answ parent-target child parent old-hist)
    (setq old-hist org-refile-history)
    (setq answ (funcall cfunc prompt tbl nil (not new-nodes)
			nil 'org-refile-history
			(or cdef (car org-refile-history))))
    (if (setq pa (org-refile--get-location answ tbl))
	(let ((last-refile-loc (car org-refile-history)))
	  (org-refile-check-position pa)
	  (when (or (not org-refile-history)
		    (not (eq old-hist org-refile-history))
		    (not (equal (car pa) last-refile-loc)))
	    (setq org-refile-history
		  (cons (car pa) (if (assoc last-refile-loc tbl)
				     org-refile-history
				   (cdr org-refile-history))))
	    (when (equal last-refile-loc (nth 1 org-refile-history))
	      (pop org-refile-history)))
	  pa)
      (if (string-match "\\`\\(.*\\)/\\([^/]+\\)\\'" answ)
	  (progn
	    (setq parent (match-string 1 answ)
		  child (match-string 2 answ))
	    (setq parent-target (org-refile--get-location parent tbl))
	    (when (and parent-target
		       (or (eq new-nodes t)
			   (and (eq new-nodes 'confirm)
				(y-or-n-p (format "Create new node \"%s\"? "
						  child)))))
	      (org-refile-new-child parent-target child)))
	(user-error "Invalid target location")))))

(defun org-refile-check-position (refile-pointer)
  "Check if the refile pointer matches the headline to which it points."
  (let* ((file (nth 1 refile-pointer))
	 (re (nth 2 refile-pointer))
	 (pos (nth 3 refile-pointer))
	 buffer)
    (if (and (not (markerp pos)) (not file))
	(user-error "Please indicate a target file in the refile path")
      (when (org-string-nw-p re)
	(setq buffer (if (markerp pos)
			 (marker-buffer pos)
		       (find-file-noselect file 'nowarn)))
	(with-current-buffer buffer
	  (org-with-wide-buffer
	   (goto-char pos)
	   (forward-line 0)
	   (unless (looking-at-p re)
	     (user-error "Invalid refile position, please clear the cache with `C-0 C-c C-w' before refiling"))))))))

(defun org-refile-new-child (parent-target child)
  "Use refile target PARENT-TARGET to add new CHILD below it."
  (unless parent-target
    (error "Cannot find parent for new node"))
  (let ((file (nth 1 parent-target))
	(pos (nth 3 parent-target))
	level)
    (with-current-buffer (find-file-noselect file 'nowarn)
      (org-with-wide-buffer
       (if pos
	   (goto-char pos)
	 (goto-char (point-max))
	 (unless (bolp) (newline)))
       (when (looking-at org-outline-regexp)
	 (setq level (funcall outline-level))
	 (org-end-of-subtree t t))
       (org-back-over-empty-lines)
       (insert "\n" (make-string
		     (if pos (org-get-valid-level level 1) 1) ?*)
	       " " child "\n")
       (forward-line -1)
       (list (concat (car parent-target) "/" child) file "" (point))))))

(defun org-olpath-completing-read (prompt collection &rest args)
  "Read an outline path like a file name."
  (let ((thetable collection))
    (apply #'completing-read
	   prompt
	   (lambda (string predicate &optional flag)
	     (cond
	      ((eq flag nil) (try-completion string thetable))
	      ((eq flag t)
	       (let ((l (length string)))
		 (mapcar (lambda (x)
			   (let ((r (substring x l))
				 (f (if (string-match " ([^)]*)$" x)
					(match-string 0 x)
				      "")))
			     (if (string-match "/" r)
				 (concat string (substring r 0 (match-end 0)) f)
			       x)))
			 (all-completions string thetable predicate))))
              ((eq (car-safe flag) 'boundaries)
               ;; See `completion-file-name-table'.
               (let ((start (or (and (string-match "/" string)
                                     (match-beginning 0))
                                (length string)))
                     (end (and (string-match "/" (cdr flag))
                               (match-beginning 0))))
                 `(boundaries ,start . ,end)))
	      ;; Exact match?
	      ((eq flag 'lambda) (assoc string thetable))))
	   args)))

(provide 'org-refile)

;; Local variables:
;; generated-autoload-file: "org-loaddefs.el"
;; End:

;;; org-refile.el ends here
