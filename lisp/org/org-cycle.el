;;; org-cycle.el --- Visibility cycling of Org entries -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020-2024 Free Software Foundation, Inc.
;;
;; Maintainer: Ihor Radchenko <yantar92 at posteo dot net>
;; Keywords: folding, visibility cycling, invisible text
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

;; This file contains code controlling global folding state in buffer
;; and TAB-cycling.

;;; Code:

(require 'org-macs)
(org-assert-version)

(require 'org-macs)
(require 'org-fold)

(declare-function org-element-type-p "org-element-ast" (node types))
(declare-function org-element-property "org-element-ast" (property node))
(declare-function org-element-post-affiliated "org-element" (node))
(declare-function org-element-lineage "org-element-ast" (datum &optional types with-self))
(declare-function org-element-at-point "org-element" (&optional pom cached-only))
(declare-function org-display-inline-images "org" (&optional include-linked refresh beg end))
(declare-function org-get-tags "org" (&optional pos local fontify))
(declare-function org-subtree-end-visible-p "org" ())
(declare-function org-narrow-to-subtree "org" (&optional element))
(declare-function org-next-visible-heading "org" (arg))
(declare-function org-at-property-p "org" ())
(declare-function org-re-property "org" (property &optional literal allow-null value))
(declare-function org-remove-inline-images "org" (&optional beg end))
(declare-function org-item-beginning-re "org" ())
(declare-function org-at-heading-p "org" (&optional invisible-not-ok))
(declare-function org-at-item-p "org" ())
(declare-function org-before-first-heading-p "org" ())
(declare-function org-back-to-heading "org" (&optional invisible-ok))
(declare-function org-end-of-subtree "org" (&optional invisible-ok to-heading))
(declare-function org-entry-end-position "org" ())
(declare-function org-try-cdlatex-tab "org" ())
(declare-function org-cycle-level "org" ())
(declare-function org-table-next-field "org-table" ())
(declare-function org-table-justify-field-maybe "org-table" (&optional new))
(declare-function org-inlinetask-at-task-p "org-inlinetask" ())
(declare-function org-inlinetask-toggle-visibility "org-inlinetask" ())
(declare-function org-list-get-all-items "org-list" (item struct prevs))
(declare-function org-list-get-bottom-point "org-list" (struct))
(declare-function org-list-prevs-alist "org-list" (struct))
(declare-function org-list-set-item-visibility "org-list" (item struct view))
(declare-function org-list-search-forward "org-list" (regexp &optional bound noerror))
(declare-function org-list-has-child-p "org-list" (item struct))
(declare-function org-list-get-item-end-before-blank "org-list" (item struct))
(declare-function org-list-struct "org-list" ())
(declare-function org-cycle-item-indentation "org-list" ())

(declare-function outline-previous-heading "outline" ())
(declare-function outline-next-heading "outline" ())
(declare-function outline-end-of-heading "outline" ())
(declare-function outline-up-heading "outline" (arg &optional invisible-ok))

(defvar org-drawer-regexp)
(defvar org-odd-levels-only)
(defvar org-startup-folded)
(defvar org-archive-tag)
(defvar org-cycle-include-plain-lists)
(defvar org-outline-regexp-bol)

(defvar-local org-cycle-global-status nil)
(put 'org-cycle-global-status 'org-state t)
(defvar-local org-cycle-subtree-status nil)
(put 'org-cycle-subtree-status 'org-state t)

;;;; Customization:


(defgroup org-cycle nil
  "Options concerning visibility cycling in Org mode."
  :tag "Org Cycle"
  :group 'org-structure)

(defcustom org-cycle-skip-children-state-if-no-children t
  "Non-nil means skip CHILDREN state in entries that don't have any."
  :group 'org-cycle
  :type 'boolean)

(defcustom org-cycle-max-level nil
  "Maximum level which should still be subject to visibility cycling.
Levels higher than this will, for cycling, be treated as text, not a headline.
When `org-odd-levels-only' is set, a value of N in this variable actually
means 2N-1 stars as the limiting headline.
When nil, cycle all levels.
Note that the limiting level of cycling is also influenced by
`org-inlinetask-min-level'.  When `org-cycle-max-level' is not set but
`org-inlinetask-min-level' is, cycling will be limited to levels one less
than its value."
  :group 'org-cycle
  :type '(choice
	  (const :tag "No limit" nil)
	  (integer :tag "Maximum level")))

(defvaralias 'org-hide-block-startup 'org-cycle-hide-block-startup)
(defcustom org-cycle-hide-block-startup nil
  "Non-nil means entering Org mode will fold all blocks.
This can also be set in on a per-file basis with

#+STARTUP: hideblocks
#+STARTUP: nohideblocks"
  :group 'org-startup
  :group 'org-cycle
  :type 'boolean)

(defvaralias 'org-hide-drawer-startup 'org-cycle-hide-drawer-startup)
(defcustom org-cycle-hide-drawer-startup t
  "Non-nil means entering Org mode will fold all drawers.
This can also be set in on a per-file basis with

#+STARTUP: hidedrawers
#+STARTUP: nohidedrawers"
  :group 'org-startup
  :group 'org-cycle
  :type 'boolean)

(defcustom org-cycle-global-at-bob nil
  "Cycle globally if cursor is at beginning of buffer and not at a headline.

This makes it possible to do global cycling without having to use `S-TAB'
or `\\[universal-argument] TAB'.  For this special case to work, the first \
line of the buffer
must not be a headline -- it may be empty or some other text.

When used in this way, `org-cycle-hook' is disabled temporarily to make
sure the cursor stays at the beginning of the buffer.

When this option is nil, don't do anything special at the beginning of
the buffer."
  :group 'org-cycle
  :type 'boolean)

(defcustom org-cycle-level-after-item/entry-creation t
  "Non-nil means cycle entry level or item indentation in new empty entries.

When the cursor is at the end of an empty headline, i.e., with only stars
and maybe a TODO keyword, TAB will then switch the entry to become a child,
and then all possible ancestor states, before returning to the original state.
This makes data entry extremely fast:  M-RET to create a new headline,
on TAB to make it a child, two or more tabs to make it a (grand-)uncle.

When the cursor is at the end of an empty plain list item, one TAB will
make it a subitem, two or more tabs will back up to make this an item
higher up in the item hierarchy."
  :group 'org-cycle
  :type 'boolean)

(defcustom org-cycle-emulate-tab t
  "Where should `org-cycle' emulate TAB.
nil         Never
white       Only in completely white lines
whitestart  Only at the beginning of lines, before the first non-white char
t           Everywhere except in headlines
exc-hl-bol  Everywhere except at the start of a headline
If TAB is used in a place where it does not emulate TAB, the current subtree
visibility is cycled."
  :group 'org-cycle
  :type '(choice (const :tag "Never" nil)
		 (const :tag "Only in completely white lines" white)
		 (const :tag "Before first char in a line" whitestart)
		 (const :tag "Everywhere except in headlines" t)
		 (const :tag "Everywhere except at bol in headlines" exc-hl-bol)))

(defcustom org-cycle-separator-lines 2
  "Number of empty lines needed to keep an empty line between collapsed trees.
If you leave an empty line between the end of a subtree and the following
headline, this empty line is hidden when the subtree is folded.
Org mode will leave (exactly) one empty line visible if the number of
empty lines is equal or larger to the number given in this variable.
So the default 2 means at least 2 empty lines after the end of a subtree
are needed to produce free space between a collapsed subtree and the
following headline.

If the number is negative, and the number of empty lines is at least -N,
all empty lines are shown.

Special case: when 0, never leave empty lines in collapsed view."
  :group 'org-cycle
  :type 'integer)
(put 'org-cycle-separator-lines 'safe-local-variable 'integerp)

(defvaralias 'org-pre-cycle-hook 'org-cycle-pre-hook)
(defcustom org-cycle-pre-hook nil
  "Hook that is run before visibility cycling is happening.
The function(s) in this hook must accept a single argument which indicates
the new state that will be set right after running this hook.  The
argument is a symbol.  Before a global state change, it can have the values
`overview', `content', or `all'.  Before a local state change, it can have
the values `folded', `children', or `subtree'."
  :group 'org-cycle
  :type 'hook)

(defcustom org-cycle-hook '(org-cycle-hide-archived-subtrees
                            org-cycle-show-empty-lines
                            org-cycle-optimize-window-after-visibility-change
                            org-cycle-display-inline-images)
  "Hook that is run after `org-cycle' has changed the buffer visibility.
The function(s) in this hook must accept a single argument which indicates
the new state that was set by the most recent `org-cycle' command.  The
argument is a symbol.  After a global state change, it can have the values
`overview', `contents', or `all'.  After a local state change, it can have
the values `folded', `children', or `subtree'."
  :group 'org-cycle
  :package-version '(Org . "9.4")
  :type 'hook)

(defcustom org-cycle-open-archived-trees nil
  "Non-nil means `org-cycle' will open archived trees.
An archived tree is a tree marked with the tag ARCHIVE.
When nil, archived trees will stay folded.  You can still open them with
normal outline commands like `show-all', but not with the cycling commands."
  :group 'org-archive
  :group 'org-cycle
  :type 'boolean)

(defcustom org-cycle-inline-images-display nil
  "Non-nil means auto display inline images under subtree when cycling."
  :group 'org-startup
  :group 'org-cycle
  :package-version '(Org . "9.6")
  :type 'boolean)

(defvaralias 'org-tab-first-hook 'org-cycle-tab-first-hook)
(defvar org-cycle-tab-first-hook nil
  "Hook for functions to attach themselves to TAB.
See `org-ctrl-c-ctrl-c-hook' for more information.
This hook runs as the first action when TAB is pressed, even before
`org-cycle' messes around with the `outline-regexp' to cater for
inline tasks and plain list item folding.
If any function in this hook returns t, any other actions that
would have been caused by TAB (such as table field motion or visibility
cycling) will not occur.")

;;;; Implementation:

(defun org-cycle-hide-drawers (state)
  "Re-hide all drawers after a visibility state change.
STATE should be one of the symbols listed in the docstring of
`org-cycle-hook'."
  (when (derived-mode-p 'org-mode)
    (cond ((not (memq state '(overview folded contents)))
           (let* ((global? (eq state 'all))
                  (beg (if global? (point-min) (line-beginning-position)))
                  (end (cond (global? (point-max))
                             ((eq state 'children) (org-entry-end-position))
                             (t (save-excursion (org-end-of-subtree t t))))))
             (org-fold--hide-drawers beg end)))
          ((memq state '(overview contents))
           ;; Hide drawers before first heading.
           (let ((beg (point-min))
                 (end (save-excursion
                        (goto-char (point-min))
                        (if (org-before-first-heading-p)
                            (org-entry-end-position)
                          (point-min)))))
             (when (< beg end)
               (org-fold--hide-drawers beg end)))))))

;;;###autoload
(defun org-cycle (&optional arg)
  "TAB-action and visibility cycling for Org mode.

This is the command invoked in Org mode by the `TAB' key.  Its main
purpose is outline visibility cycling, but it also invokes other actions
in special contexts.

When this function is called with a `\\[universal-argument]' prefix, rotate \
the entire
buffer through 3 states (global cycling)
  1. OVERVIEW: Show only top-level headlines.
  2. CONTENTS: Show all headlines of all levels, but no body text.
  3. SHOW ALL: Show everything.

With a `\\[universal-argument] \\[universal-argument]' prefix argument, \
switch to the startup visibility,
determined by the variable `org-startup-folded', and by any VISIBILITY
properties in the buffer.

With a `\\[universal-argument] \\[universal-argument] \
\\[universal-argument]' prefix argument, show the entire buffer, including
any drawers.

When inside a table, re-align the table and move to the next field.

When point is at the beginning of a headline, rotate the subtree started
by this line through 3 different states (local cycling)
  1. FOLDED:   Only the main headline is shown.
  2. CHILDREN: The main headline and the direct children are shown.
               From this state, you can move to one of the children
               and zoom in further.
  3. SUBTREE:  Show the entire subtree, including body text.
If there is no subtree, switch directly from CHILDREN to FOLDED.

When point is at the beginning of an empty headline and the variable
`org-cycle-level-after-item/entry-creation' is set, cycle the level
of the headline by demoting and promoting it to likely levels.  This
speeds up creation document structure by pressing `TAB' once or several
times right after creating a new headline.

When there is a numeric prefix, go up to a heading with level ARG, do
a `show-subtree' and return to the previous cursor position.  If ARG
is negative, go up that many levels.

When point is not at the beginning of a headline, execute the global
binding for `TAB', which is re-indenting the line.  See the option
`org-cycle-emulate-tab' for details.

As a special case, if point is at the very beginning of the buffer, if
there is no headline there, and if the variable `org-cycle-global-at-bob'
is non-nil, this function acts as if called with prefix argument \
\(`\\[universal-argument] TAB',
same as `S-TAB') also when called without prefix argument."
  (interactive "P")
  (org-load-modules-maybe)
  (unless (or (run-hook-with-args-until-success 'org-cycle-tab-first-hook)
	      (and org-cycle-level-after-item/entry-creation
		   (or (org-cycle-level)
		       (org-cycle-item-indentation))))
    (when (and org-cycle-max-level
               (or (not (integerp org-cycle-max-level))
                   (< org-cycle-max-level 1)))
      (user-error "`org-cycle-max-level' must be a positive integer"))
    (let* ((limit-level
	    (or org-cycle-max-level
		(and (boundp 'org-inlinetask-min-level)
		     org-inlinetask-min-level
		     (1- org-inlinetask-min-level))))
	   (nstars
	    (and limit-level
		 (if org-odd-levels-only
		     (1- (* 2 limit-level))
		   limit-level)))
	   (org-outline-regexp
	    (format "\\*%s " (if nstars (format "\\{1,%d\\}" nstars) "+"))))
      (cond
       ((equal arg '(16))
	(setq last-command 'dummy)
	(org-cycle-set-startup-visibility)
	(org-unlogged-message "Startup visibility, plus VISIBILITY properties"))
       ((equal arg '(64))
	(org-fold-show-all)
	(org-unlogged-message "Entire buffer visible, including drawers"))
       ((equal arg '(4)) (org-cycle-internal-global))
       ;; Show-subtree, ARG levels up from here.
       ((integerp arg)
	(save-excursion
	  (org-back-to-heading)
	  (outline-up-heading (if (< arg 0) (- arg)
				(- (funcall outline-level) arg)))
	  (org-fold-show-subtree)))
       ;; Global cycling at BOB: delegate to `org-cycle-internal-global'.
       ((and org-cycle-global-at-bob
	     (bobp)
	     (not (looking-at org-outline-regexp)))
	(let ((org-cycle-hook
	       (remq 'org-cycle-optimize-window-after-visibility-change
		     org-cycle-hook)))
	  (org-cycle-internal-global)))
       ;; Try CDLaTeX TAB completion.
       ((org-try-cdlatex-tab))
       ;; Inline task: delegate to `org-inlinetask-toggle-visibility'.
       ((and (featurep 'org-inlinetask)
	     (org-inlinetask-at-task-p)
	     (or (bolp) (not (eq org-cycle-emulate-tab 'exc-hl-bol))))
	(org-inlinetask-toggle-visibility))
       (t
	(let ((pos (point))
	      (element (org-element-at-point)))
	  (cond
	   ;; Try toggling visibility for block at point.
	   ((org-fold-hide-block-toggle nil t element))
	   ;; Try toggling visibility for drawer at point.
	   ((org-fold-hide-drawer-toggle nil t element))
	   ;; Table: enter it or move to the next field.
	   ((and (org-match-line "[ \t]*[|+]")
		 (org-element-lineage element 'table t))
	    (if (and (org-element-type-p element 'table)
		     (eq 'table.el (org-element-property :type element)))
		(message (substitute-command-keys "\\<org-mode-map>\
Use `\\[org-edit-special]' to edit table.el tables"))
	      (org-table-justify-field-maybe)
	      (call-interactively #'org-table-next-field)))
	   ((run-hook-with-args-until-success
	     'org-tab-after-check-for-table-hook))
	   ;; At an item/headline: delegate to `org-cycle-internal-local'.
	   ((and (or (and org-cycle-include-plain-lists
			  (let ((item (org-element-lineage element
							   '(item plain-list)
							   t)))
			    (and item
				 (= (line-beginning-position)
				    (org-element-post-affiliated
				     item)))))
		     (org-match-line org-outline-regexp))
		 (or (bolp) (not (eq org-cycle-emulate-tab 'exc-hl-bol))))
	    (org-cycle-internal-local))
	   ;; From there: TAB emulation and template completion.
	   (buffer-read-only (org-back-to-heading))
	   ((run-hook-with-args-until-success
	     'org-tab-after-check-for-cycling-hook))
	   ((run-hook-with-args-until-success
	     'org-tab-before-tab-emulation-hook))
	   ((and (eq org-cycle-emulate-tab 'exc-hl-bol)
		 (or (not (bolp))
		     (not (looking-at org-outline-regexp))))
	    (call-interactively (global-key-binding (kbd "TAB"))))
	   ((or (eq org-cycle-emulate-tab t)
		(and (memq org-cycle-emulate-tab '(white whitestart))
		     (save-excursion (forward-line 0) (looking-at "[ \t]*"))
		     (or (and (eq org-cycle-emulate-tab 'white)
			      (= (match-end 0) (line-end-position)))
			 (and (eq org-cycle-emulate-tab 'whitestart)
			      (>= (match-end 0) pos)))))
	    (call-interactively (global-key-binding (kbd "TAB"))))
	   (t
	    (save-excursion
	      (org-back-to-heading)
	      (org-cycle))))))))))

(defun org-cycle-force-archived ()
  "Cycle subtree even if it is archived."
  (interactive)
  (setq this-command 'org-cycle)
  (let ((org-cycle-open-archived-trees t))
    (call-interactively 'org-cycle)))

(defun org-cycle-internal-global ()
  "Do the global cycling action."
  ;; Hack to avoid display of messages for .org  attachments in Gnus
  (let ((ga (string-match-p "\\*fontification" (buffer-name))))
    (cond
     ((and (eq last-command this-command)
	   (eq org-cycle-global-status 'overview))
      ;; We just created the overview - now do table of contents
      ;; This can be slow in very large buffers, so indicate action
      (run-hook-with-args 'org-cycle-pre-hook 'contents)
      (unless ga (org-unlogged-message "CONTENTS..."))
      (org-cycle-content)
      (unless ga (org-unlogged-message "CONTENTS...done"))
      (setq org-cycle-global-status 'contents)
      (run-hook-with-args 'org-cycle-hook 'contents))

     ((and (eq last-command this-command)
	   (eq org-cycle-global-status 'contents))
      ;; We just showed the table of contents - now show everything
      (run-hook-with-args 'org-cycle-pre-hook 'all)
      (org-fold-show-all '(headings blocks))
      (unless ga (org-unlogged-message "SHOW ALL"))
      (setq org-cycle-global-status 'all)
      (run-hook-with-args 'org-cycle-hook 'all))

     (t
      ;; Default action: go to overview
      (run-hook-with-args 'org-cycle-pre-hook 'overview)
      (org-cycle-overview)
      (unless ga (org-unlogged-message "OVERVIEW"))
      (setq org-cycle-global-status 'overview)
      (run-hook-with-args 'org-cycle-hook 'overview)))))

(defun org-cycle-internal-local ()
  "Do the local cycling action."
  (let ((goal-column 0) eoh eol eos has-children children-skipped struct)
    ;; First, determine end of headline (EOH), end of subtree or item
    ;; (EOS), and if item or heading has children (HAS-CHILDREN).
    (save-excursion
      (if (org-at-item-p)
	  (progn
	    (forward-line 0)
	    (setq struct (org-list-struct))
	    (setq eoh (line-end-position))
	    (setq eos (org-list-get-item-end-before-blank (point) struct))
	    (setq has-children (org-list-has-child-p (point) struct)))
	(org-back-to-heading)
	(setq eoh (save-excursion (outline-end-of-heading) (point)))
	(setq eos (save-excursion
		    (org-end-of-subtree t t)
		    (unless (eobp) (forward-char -1))
		    (point)))
	(setq has-children
	      (or
	       (save-excursion
		 (let ((level (funcall outline-level)))
		   (outline-next-heading)
		   (and (org-at-heading-p)
			(> (funcall outline-level) level))))
	       (and (eq org-cycle-include-plain-lists 'integrate)
		    (save-excursion
		      (org-list-search-forward (org-item-beginning-re) eos t))))))
      ;; Determine end invisible part of buffer (EOL)
      (forward-line 1)
      (if (eq org-fold-core-style 'text-properties)
          (while (and (not (eobp))		;this is like `next-line'
		      (org-fold-folded-p (1- (point))))
	    (goto-char (org-fold-next-visibility-change nil nil t))
	    (and (eolp) (forward-line 1)))
        (while (and (not (eobp))		;this is like `next-line'
		    (get-char-property (1- (point)) 'invisible))
	  (goto-char (next-single-char-property-change (point) 'invisible))
	  (and (eolp) (forward-line 1))))
      (setq eol (point)))
    ;; Find out what to do next and set `this-command'
    (cond
     ((= eos eoh)
      ;; Nothing is hidden behind this heading
      (unless (org-before-first-heading-p)
	(run-hook-with-args 'org-cycle-pre-hook 'empty))
      (org-unlogged-message "EMPTY ENTRY")
      (setq org-cycle-subtree-status nil)
      (save-excursion
	(goto-char eos)
        (org-with-limited-levels
	 (outline-next-heading))
	(when (org-invisible-p) (org-fold-heading nil))))
     ((and (or (>= eol eos)
	       (save-excursion (goto-char eol) (skip-chars-forward "[:space:]" eos) (= (point) eos)))
	   (or has-children
	       (not (setq children-skipped
			org-cycle-skip-children-state-if-no-children))))
      ;; Entire subtree is hidden in one line: children view
      (unless (org-before-first-heading-p)
        (org-with-limited-levels
	 (run-hook-with-args 'org-cycle-pre-hook 'children)))
      (if (org-at-item-p)
	  (org-list-set-item-visibility (line-beginning-position) struct 'children)
	(org-fold-show-entry)
	(org-with-limited-levels (org-fold-show-children))
	(org-fold-show-set-visibility 'tree)
	;; Fold every list in subtree to top-level items.
	(when (eq org-cycle-include-plain-lists 'integrate)
	  (save-excursion
	    (org-back-to-heading)
	    (while (org-list-search-forward (org-item-beginning-re) eos t)
	      (forward-line 0)
	      (let* ((struct (org-list-struct))
		     (prevs (org-list-prevs-alist struct))
		     (end (org-list-get-bottom-point struct)))
		(dolist (e (org-list-get-all-items (point) struct prevs))
		  (org-list-set-item-visibility e struct 'folded))
		(goto-char (if (< end eos) end eos)))))))
      (org-unlogged-message "CHILDREN")
      (save-excursion
	(goto-char eos)
        (org-with-limited-levels
	 (outline-next-heading))
	(when (and
               ;; Subtree does not end at the end of visible section of the
               ;; buffer.
               (< (point) (point-max))
               (org-invisible-p))
          ;; Reveal the following heading line.
          (org-fold-heading nil)))
      (setq org-cycle-subtree-status 'children)
      (unless (org-before-first-heading-p)
	(run-hook-with-args 'org-cycle-hook 'children)))
     ((or children-skipped
	  (and (eq last-command this-command)
	       (eq org-cycle-subtree-status 'children)))
      ;; We just showed the children, or no children are there,
      ;; now show everything.
      (unless (org-before-first-heading-p)
	(run-hook-with-args 'org-pre-cycle-hook 'subtree))
      (org-fold-region eoh eos nil 'outline)
      (org-unlogged-message
       (if children-skipped "SUBTREE (NO CHILDREN)" "SUBTREE"))
      (setq org-cycle-subtree-status 'subtree)
      (unless (org-before-first-heading-p)
	(run-hook-with-args 'org-cycle-hook 'subtree)))
     (t
      ;; Default action: hide the subtree.
      (run-hook-with-args 'org-cycle-pre-hook 'folded)
      (org-fold-region eoh eos t 'outline)
      (org-unlogged-message "FOLDED")
      (setq org-cycle-subtree-status 'folded)
      (unless (org-before-first-heading-p)
	(run-hook-with-args 'org-cycle-hook 'folded))))))

;;;###autoload
(defun org-cycle-global (&optional arg)
  "Cycle the global visibility.  For details see `org-cycle'.
With `\\[universal-argument]' prefix ARG, switch to startup visibility.
With a numeric prefix, show all headlines up to that level."
  (interactive "P")
  (cond
   ((integerp arg)
    (org-cycle-content arg)
    (setq org-cycle-global-status 'contents))
   ((equal arg '(4))
    (org-cycle-set-startup-visibility)
    (org-unlogged-message "Startup visibility, plus VISIBILITY properties."))
   (t
    (org-cycle '(4)))))

(defun org-cycle-set-startup-visibility ()
  "Set the visibility required by startup options and properties."
  (cond
   ;; `fold' is technically not allowed value, but it is often
   ;; intuitively tried by users by analogy with #+STARTUP: fold.
   ((memq org-startup-folded '(t fold overview))
    (org-cycle-overview))
   ((eq org-startup-folded 'content)
    (org-cycle-content))
   ((eq org-startup-folded 'show2levels)
    (org-cycle-content 2))
   ((eq org-startup-folded 'show3levels)
    (org-cycle-content 3))
   ((eq org-startup-folded 'show4levels)
    (org-cycle-content 4))
   ((eq org-startup-folded 'show5levels)
    (org-cycle-content 5))
   ;; `nofold' and `showall' are technically not allowed values, but
   ;; they are often intuitively tried by users by analogy with
   ;; #+STARTUP: nofold or #STARUP: showall.
   ((memq org-startup-folded '(showeverything nil nofold showall))
    (org-fold-show-all)))
  (unless (eq org-startup-folded 'showeverything)
    (when org-cycle-hide-block-startup (org-fold-hide-block-all))
    (org-cycle-set-visibility-according-to-property)
    (org-cycle-hide-archived-subtrees 'all)
    (when org-cycle-hide-drawer-startup (org-cycle-hide-drawers 'all))
    (org-cycle-show-empty-lines t)))

(defun org-cycle-set-visibility-according-to-property ()
  "Switch subtree visibility according to VISIBILITY property."
  (interactive)
  (let ((regexp (org-re-property "VISIBILITY")))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward regexp nil t)
        (let ((state (match-string 3)))
	  (if (not (org-at-property-p)) (outline-next-heading)
	    (save-excursion
	      (org-back-to-heading t)
	      (org-fold-subtree t)
	      (pcase state
	        ("folded"
		 (org-fold-subtree t))
	        ("children"
		 (org-fold-show-hidden-entry)
		 (org-fold-show-children))
	        ("content"
                 ;; Newline before heading will be outside the
                 ;; narrowing.  Make sure that it is revealed.
                 (org-fold-heading nil)
		 (save-excursion
		   (save-restriction
		     (org-narrow-to-subtree)
		     (org-cycle-content))))
	        ((or "all" "showall")
		 (org-fold-show-subtree))
	        (_ nil)))))))))

(defun org-cycle-overview ()
  "Switch to overview mode, showing only top-level headlines."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    ;; Hide top-level drawer.
    (save-restriction
      (narrow-to-region (point-min) (or (re-search-forward org-outline-regexp-bol nil t) (point-max)))
      (org-fold-hide-drawer-all))
    (goto-char (point-min))
    (when (re-search-forward org-outline-regexp-bol nil t)
      (let* ((last (line-end-position))
             (level (- (match-end 0) (match-beginning 0) 1))
             (regexp (format "^\\*\\{1,%d\\} " level)))
        (while (re-search-forward regexp nil :move)
          (org-fold-region last (line-end-position 0) t 'outline)
          (setq last (line-end-position))
          (setq level (- (match-end 0) (match-beginning 0) 1))
          (setq regexp (format "^\\*\\{1,%d\\} " level)))
        (org-fold-region last (point) t 'outline)))))

(defun org-cycle-content (&optional arg)
  "Show all headlines in the buffer, like a table of contents.
With numerical argument ARG, show content up to level ARG."
  (interactive "p")
  (org-fold-show-all '(headings))
  (save-excursion
    (goto-char (point-min))
    ;; Hide top-level drawer.
    (save-restriction
      (narrow-to-region (point-min) (or (re-search-forward org-outline-regexp-bol nil t) (point-max)))
      (org-fold-hide-drawer-all))
    (goto-char (point-max))
    (let ((regexp (if (and (wholenump arg) (> arg 0))
                      (format "^\\*\\{1,%d\\} " arg)
                    "^\\*+ "))
          (last (point)))
      (while (re-search-backward regexp nil t)
        (org-fold-region (line-end-position) last t 'outline)
        (setq last (line-end-position 0))))))

(defvar org-cycle-scroll-position-to-restore nil
  "Temporarily store scroll position to restore.")
(defun org-cycle-optimize-window-after-visibility-change (state)
  "Adjust the window after a change in outline visibility.
This function is the default value of the hook `org-cycle-hook'.
STATE is the current outline visibility state.  It should be one of
symbols `content', `all', `folded', `children', or `subtree'."
  (when (get-buffer-window (current-buffer))
    (let ((repeat (eq last-command this-command)))
      (unless repeat
	(setq org-cycle-scroll-position-to-restore nil))
      (cond
       ((eq state 'content)  nil)
       ((eq state 'all)      nil)
       ((and org-cycle-scroll-position-to-restore repeat
	     (eq state 'folded))
	(set-window-start nil org-cycle-scroll-position-to-restore))
       ((eq state 'folded) nil)
       ((eq state 'children)
	(setq org-cycle-scroll-position-to-restore (window-start))
	(or (org-subtree-end-visible-p) (recenter 1)))
       ((eq state 'subtree)
        (unless repeat
	  (setq org-cycle-scroll-position-to-restore (window-start)))
        (or (org-subtree-end-visible-p) (recenter 1)))))))

(defun org-cycle-show-empty-lines (state)
  "Show empty lines above all visible headlines.
The region to be covered depends on STATE when called through
`org-cycle-hook'.  Lisp program can use t for STATE to get the
entire buffer covered.  Note that an empty line is only shown if there
are at least `org-cycle-separator-lines' empty lines before the headline."
  (when (/= org-cycle-separator-lines 0)
    (save-excursion
      (let* ((n (abs org-cycle-separator-lines))
             (re (cond
                  ((= n 1) "\\(\n[ \t]*\n\\*+\\) ")
                  ((= n 2) "^[ \t]*\\(\n[ \t]*\n\\*+\\) ")
                  (t (let ((ns (number-to-string (- n 2))))
                       (concat "^\\(?:[ \t]*\n\\)\\{" ns "," ns "\\}"
                               "[ \t]*\\(\n[ \t]*\n\\*+\\) ")))))
             beg end)
        (cond
         ((memq state '(overview contents t))
          (setq beg (point-min) end (point-max)))
         ((memq state '(children folded))
          (setq beg (point)
                end (progn (org-end-of-subtree t t)
                           (line-beginning-position 2)))))
        (when beg
          (goto-char beg)
          (while (re-search-forward re end t)
            (unless (org-invisible-p (match-end 1))
              (let ((e (match-end 1))
                    (b (if (>= org-cycle-separator-lines 0)
                           (match-beginning 1)
                         (save-excursion
                           (goto-char (match-beginning 0))
                           (skip-chars-backward " \t\n")
                           (line-end-position)))))
                (org-fold-region b e nil 'outline))))))))
  ;; Never hide empty lines at the end of the file.
  (save-excursion
    (goto-char (point-max))
    (outline-previous-heading)
    (outline-end-of-heading)
    (when (and (looking-at "[ \t\n]+")
               (= (match-end 0) (point-max)))
      (org-fold-region (point) (match-end 0) nil 'outline))))

(defun org-cycle-hide-archived-subtrees (state)
  "Re-hide all archived subtrees after a visibility state change.
STATE should be one of the symbols listed in the docstring of
`org-cycle-hook'."
  (when (and (not org-cycle-open-archived-trees)
             (not (memq state '(overview folded))))
    (let ((globalp (memq state '(contents all))))
      (if globalp
          (org-fold-hide-archived-subtrees (point-min) (point-max))
        (org-fold-hide-archived-subtrees
         (point)
         (save-excursion
           (org-end-of-subtree t))))
      (when (and (not globalp)
                 (member org-archive-tag
                         (org-get-tags nil 'local)))
	(message "%s" (substitute-command-keys
		       "Subtree is archived and stays closed.  Use \
`\\[org-cycle-force-archived]' to cycle it anyway."))))))

(defun org-cycle-display-inline-images (state)
  "Auto display inline images under subtree when cycling.
It works when `org-cycle-inline-images-display' is non-nil.
STATE is the current outline visibility state.  It should be one of
symbols `content', `all', `folded', `children', or `subtree'."
  (when org-cycle-inline-images-display
    (pcase state
      ('children
       (org-with-wide-buffer
        (org-narrow-to-subtree)
        ;; If has nested headlines, beg,end only from parent headline
        ;; to first child headline which reference to upper
        ;; let-binding `org-next-visible-heading'.
        (org-display-inline-images
         nil nil
         (point-min) (progn (org-next-visible-heading 1) (point)))))
      ('subtree
       (org-with-wide-buffer
        (org-narrow-to-subtree)
        ;; If has nested headlines, also inline display images under all sub-headlines.
        (org-display-inline-images nil nil (point-min) (point-max))))
      ('folded
       (org-with-wide-buffer
        (org-narrow-to-subtree)
        (if (numberp (point-max))
            (org-remove-inline-images (point-min) (point-max))
          (ignore)))))))

(provide 'org-cycle)

;;; org-cycle.el ends here
