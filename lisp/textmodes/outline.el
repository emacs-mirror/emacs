;;; outline.el --- outline mode commands for Emacs

;; Copyright (C) 1986, 93, 94, 95, 97, 2000, 2001
;;   Free Software Foundation, Inc.

;; Maintainer: FSF
;; Keywords: outlines

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

;; This package is a major mode for editing outline-format documents.
;; An outline can be `abstracted' to show headers at any given level,
;; with all stuff below hidden.  See the Emacs manual for details.

;;; Todo:

;; - subtree-terminators
;; - better handle comments before function bodies (i.e. heading)
;; - don't bother hiding whitespace

;;; Code:

(defgroup outlines nil
  "Support for hierarchical outlining"
  :prefix "outline-"
  :group 'editing)

(defcustom outline-regexp "[*\^L]+"
  "*Regular expression to match the beginning of a heading.
Any line whose beginning matches this regexp is considered to start a heading.
Note that Outline mode only checks this regexp at the start of a line,
so the regexp need not (and usually does not) start with `^'.
The recommended way to set this is with a Local Variables: list
in the file it applies to.  See also `outline-heading-end-regexp'."
  :type '(choice regexp (const nil))
  :group 'outlines)

(defcustom outline-heading-end-regexp "\n"
  "*Regular expression to match the end of a heading line.
You can assume that point is at the beginning of a heading when this
regexp is searched for.  The heading ends at the end of the match.
The recommended way to set this is with a `Local Variables:' list
in the file it applies to."
  :type 'regexp
  :group 'outlines)

(defvar outline-mode-prefix-map
  (let ((map (make-sparse-keymap)))
    (define-key map "@" 'outline-mark-subtree)
    (define-key map "\C-n" 'outline-next-visible-heading)
    (define-key map "\C-p" 'outline-previous-visible-heading)
    (define-key map "\C-i" 'show-children)
    (define-key map "\C-s" 'show-subtree)
    (define-key map "\C-d" 'hide-subtree)
    (define-key map "\C-u" 'outline-up-heading)
    (define-key map "\C-f" 'outline-forward-same-level)
    (define-key map "\C-b" 'outline-backward-same-level)
    (define-key map "\C-t" 'hide-body)
    (define-key map "\C-a" 'show-all)
    (define-key map "\C-c" 'hide-entry)
    (define-key map "\C-e" 'show-entry)
    (define-key map "\C-l" 'hide-leaves)
    (define-key map "\C-k" 'show-branches)
    (define-key map "\C-q" 'hide-sublevels)
    (define-key map "\C-o" 'hide-other)
    (define-key map "\C-^" 'outline-promote)
    (define-key map "\C-v" 'outline-demote)
    ;; Where to bind toggle and insert-heading ?
    map))

(defvar outline-mode-menu-bar-map
  (let ((map (make-sparse-keymap)))

    (define-key map [hide] (cons "Hide" (make-sparse-keymap "Hide")))

    (define-key map [hide hide-other] '("Hide Other" . hide-other))
    (define-key map [hide hide-sublevels] '("Hide Sublevels" . hide-sublevels))
    (define-key map [hide hide-subtree] '("Hide Subtree" . hide-subtree))
    (define-key map [hide hide-entry] '("Hide Entry" . hide-entry))
    (define-key map [hide hide-body] '("Hide Body" . hide-body))
    (define-key map [hide hide-leaves] '("Hide Leaves" . hide-leaves))

    (define-key map [show] (cons "Show" (make-sparse-keymap "Show")))

    (define-key map [show show-subtree] '("Show Subtree" . show-subtree))
    (define-key map [show show-children] '("Show Children" . show-children))
    (define-key map [show show-branches] '("Show Branches" . show-branches))
    (define-key map [show show-entry] '("Show Entry" . show-entry))
    (define-key map [show show-all] '("Show All" . show-all))

    (define-key map [headings]
      (cons "Headings" (make-sparse-keymap "Headings")))

    (define-key map [headings copy]
      '(menu-item "Copy to kill ring" outline-headers-as-kill
	:enable mark-active))
    (define-key map [headings outline-backward-same-level]
      '("Previous Same Level" . outline-backward-same-level))
    (define-key map [headings outline-forward-same-level]
      '("Next Same Level" . outline-forward-same-level))
    (define-key map [headings outline-previous-visible-heading]
      '("Previous" . outline-previous-visible-heading))
    (define-key map [headings outline-next-visible-heading]
      '("Next" . outline-next-visible-heading))
    (define-key map [headings outline-up-heading]
      '("Up" . outline-up-heading))
    map))

(defvar outline-minor-mode-menu-bar-map
  (let ((map (make-sparse-keymap)))
    (define-key map [outline]
      (cons "Outline"
	    (nconc (make-sparse-keymap "Outline")
		   ;; Remove extra separator
		   (cdr
		    ;; Flatten the major mode's menus into a single menu.
		    (apply 'append
			   (mapcar (lambda (x)
				     (if (consp x)
					 ;; Add a separator between each
					 ;; part of the unified menu.
					 (cons '(--- "---") (cdr x))))
				   outline-mode-menu-bar-map))))))
    map))
	      

(defvar outline-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c" outline-mode-prefix-map)
    (define-key map [menu-bar] outline-mode-menu-bar-map)
    map))

(defvar outline-font-lock-keywords
  '(;;
    ;; Highlight headings according to the level.
    (eval . (list (concat "^" outline-regexp ".+")
		  0 '(or (cdr (assq (outline-font-lock-level)
				    ;; FIXME: this is silly!
				    '((1 . font-lock-function-name-face)
				      (2 . font-lock-variable-name-face)
				      (3 . font-lock-keyword-face)
				      (4 . font-lock-builtin-face)
				      (5 . font-lock-comment-face)
				      (6 . font-lock-constant-face)
				      (7 . font-lock-type-face)
				      (8 . font-lock-string-face))))
			 font-lock-warning-face)
		  nil t)))
  "Additional expressions to highlight in Outline mode.")

(defun outline-font-lock-level ()
  (let ((count 1))
    (save-excursion
      (outline-back-to-heading t)
      (while (and (not (bobp))
		  (not (eq (funcall outline-level) 1)))
	(outline-up-heading 1 t)
	(setq count (1+ count)))
      count)))

(defvar outline-view-change-hook nil
  "Normal hook to be run after outline visibility changes.")

;;;###autoload
(define-derived-mode outline-mode text-mode "Outline"
  "Set major mode for editing outlines with selective display.
Headings are lines which start with asterisks: one for major headings,
two for subheadings, etc.  Lines not starting with asterisks are body lines.

Body text or subheadings under a heading can be made temporarily
invisible, or visible again.  Invisible lines are attached to the end
of the heading, so they move with it, if the line is killed and yanked
back.  A heading with text hidden under it is marked with an ellipsis (...).

Commands:\\<outline-mode-map>
\\[outline-next-visible-heading]   outline-next-visible-heading      move by visible headings
\\[outline-previous-visible-heading]   outline-previous-visible-heading
\\[outline-forward-same-level]   outline-forward-same-level        similar but skip subheadings
\\[outline-backward-same-level]   outline-backward-same-level
\\[outline-up-heading]   outline-up-heading		    move from subheading to heading

\\[hide-body]	make all text invisible (not headings).
\\[show-all]	make everything in buffer visible.

The remaining commands are used when point is on a heading line.
They apply to some of the body or subheadings of that heading.
\\[hide-subtree]   hide-subtree	make body and subheadings invisible.
\\[show-subtree]   show-subtree	make body and subheadings visible.
\\[show-children]   show-children	make direct subheadings visible.
		 No effect on body, or subheadings 2 or more levels down.
		 With arg N, affects subheadings N levels down.
\\[hide-entry]	   make immediately following body invisible.
\\[show-entry]	   make it visible.
\\[hide-leaves]	   make body under heading and under its subheadings invisible.
		     The subheadings remain visible.
\\[show-branches]  make all subheadings at all levels visible.

The variable `outline-regexp' can be changed to control what is a heading.
A line is a heading if `outline-regexp' matches something at the
beginning of the line.  The longer the match, the deeper the level.

Turning on outline mode calls the value of `text-mode-hook' and then of
`outline-mode-hook', if they are non-nil."
  (make-local-variable 'line-move-ignore-invisible)
  (setq line-move-ignore-invisible t)
  ;; Cause use of ellipses for invisible text.
  (add-to-invisibility-spec '(outline . t))
  (set (make-local-variable 'paragraph-start)
       (concat paragraph-start "\\|\\(" outline-regexp "\\)"))
  ;; Inhibit auto-filling of header lines.
  (set (make-local-variable 'auto-fill-inhibit-regexp) outline-regexp)
  (set (make-local-variable 'paragraph-separate)
       (concat paragraph-separate "\\|\\(" outline-regexp "\\)"))
  (set (make-local-variable 'font-lock-defaults)
       '(outline-font-lock-keywords t nil nil backward-paragraph))
  (setq imenu-generic-expression
	(list (list nil (concat "^\\(?:" outline-regexp "\\).*$") 0)))
  (add-hook 'change-major-mode-hook 'show-all nil t))

(defcustom outline-minor-mode-prefix "\C-c@"
  "*Prefix key to use for Outline commands in Outline minor mode.
The value of this variable is checked as part of loading Outline mode.
After that, changing the prefix key requires manipulating keymaps."
  :type 'string
  :group 'outlines)

;;;###autoload
(define-minor-mode outline-minor-mode
  "Toggle Outline minor mode.
With arg, turn Outline minor mode on if arg is positive, off otherwise.
See the command `outline-mode' for more information on this mode."
  nil " Outl" (list (cons [menu-bar] outline-minor-mode-menu-bar-map)
		    (cons outline-minor-mode-prefix outline-mode-prefix-map))
  (if outline-minor-mode
      (progn
	;; Turn off this mode if we change major modes.
	(add-hook 'change-major-mode-hook
		  (lambda () (outline-minor-mode -1))
		  nil t)
	(set (make-local-variable 'line-move-ignore-invisible) t)
	;; Cause use of ellipses for invisible text.
	(add-to-invisibility-spec '(outline . t)))
    (setq line-move-ignore-invisible nil)
    ;; Cause use of ellipses for invisible text.
    (remove-from-invisibility-spec '(outline . t))
    ;; When turning off outline mode, get rid of any outline hiding.
    (show-all)))

(defcustom outline-level 'outline-level
  "*Function of no args to compute a header's nesting level in an outline.
It can assume point is at the beginning of a header line."
  :type 'function
  :group 'outlines)

(defvar outline-heading-alist ()
  "Alist associating a heading for every possible level.
Each entry is of the form (HEADING . LEVEL).
This alist is used both to find the heading corresponding to
a given level and to find the level of a given heading.")
(make-variable-buffer-local 'outline-heading-alist)

;; This used to count columns rather than characters, but that made ^L
;; appear to be at level 2 instead of 1.  Columns would be better for
;; tab handling, but the default regexp doesn't use tabs, and anyone
;; who changes the regexp can also redefine the outline-level variable
;; as appropriate.
(defun outline-level ()
  "Return the depth to which a statement is nested in the outline.
Point must be at the beginning of a header line.
This is actually either the level specified in `outline-heading-alist'
or else the number of characters matched by `outline-regexp'."
  (save-excursion
    (if (not (looking-at outline-regexp))
	;; This should never happen
	1000
      (or (cdr (assoc (match-string 0) outline-heading-alist))
	  (- (match-end 0) (match-beginning 0))))))

(defun outline-next-preface ()
  "Skip forward to just before the next heading line.
If there's no following heading line, stop before the newline
at the end of the buffer."
  (if (re-search-forward (concat "\n\\(" outline-regexp "\\)")
			 nil 'move)
      (goto-char (match-beginning 0)))
  (if (and (bolp) (not (bobp)))
      (forward-char -1)))

(defun outline-next-heading ()
  "Move to the next (possibly invisible) heading line."
  (interactive)
  (if (re-search-forward (concat "\n\\(" outline-regexp "\\)")
			 nil 'move)
      (goto-char (1+ (match-beginning 0)))))

(defun outline-previous-heading ()
  "Move to the previous (possibly invisible) heading line."
  (interactive)
  (re-search-backward (concat "^\\(" outline-regexp "\\)")
		      nil 'move))

(defsubst outline-invisible-p ()
  "Non-nil if the character after point is invisible."
  (get-char-property (point) 'invisible))
(defun outline-visible ()
  "Obsolete.  Use `outline-invisible-p'."
  (not (outline-invisible-p)))

(defun outline-back-to-heading (&optional invisible-ok)
  "Move to previous heading line, or beg of this line if it's a heading.
Only visible heading lines are considered, unless INVISIBLE-OK is non-nil."
  (beginning-of-line)
  (or (outline-on-heading-p invisible-ok)
      (let (found)
	(save-excursion
	  (while (not found)
	    (or (re-search-backward (concat "^\\(" outline-regexp "\\)")
				    nil t)
		(error "before first heading"))
	    (setq found (and (or invisible-ok (not (outline-invisible-p)))
			     (point)))))
	(goto-char found)
	found)))

(defun outline-on-heading-p (&optional invisible-ok)
  "Return t if point is on a (visible) heading line.
If INVISIBLE-OK is non-nil, an invisible heading line is ok too."
  (save-excursion
    (beginning-of-line)
    (and (bolp) (or invisible-ok (not (outline-invisible-p)))
	 (looking-at outline-regexp))))

(defun outline-insert-heading ()
  "Insert a new heading at same depth at point."
  (interactive)
  (let ((head (save-excursion
		(condition-case nil
		    (outline-back-to-heading)
		  (error (outline-next-heading)))
		(if (eobp)
		    (or (caar outline-heading-alist) "")
		  (match-string 0)))))
    (unless (or (string-match "[ \t]\\'" head)
		(not (string-match outline-regexp (concat head " "))))
      (setq head (concat head " ")))
    (unless (bolp) (end-of-line) (newline))
    (insert head)
    (unless (eolp)
      (save-excursion (newline-and-indent)))
    (run-hooks 'outline-insert-heading-hook)))

(defun outline-promote (&optional children)
  "Promote the current heading higher up the tree.
If prefix argument CHILDREN is given, promote also all the children."
  (interactive "P")
  (outline-back-to-heading)
  (let* ((head (match-string 0))
	 (level (save-match-data (funcall outline-level)))
	 (up-head (or (car (rassoc (1- level) outline-heading-alist))
		      (save-excursion
			(save-match-data
			  (outline-up-heading 1 t)
			  (match-string 0))))))
    
    (unless (rassoc level outline-heading-alist)
      (push (cons head level) outline-heading-alist))

    (replace-match up-head nil t)
    (when children
      (outline-map-tree 'outline-promote level))))

(defun outline-demote (&optional children)
  "Demote the current heading lower down the tree.
If prefix argument CHILDREN is given, demote also all the children."
  (interactive "P")
  (outline-back-to-heading)
  (let* ((head (match-string 0))
	 (level (save-match-data (funcall outline-level)))
	 (down-head
	  (or (car (rassoc (1+ level) outline-heading-alist))
	      (save-excursion
		(save-match-data
		  (while (and (not (eobp))
			      (progn
				(outline-next-heading)
				(<= (funcall outline-level) level))))
		  (when (eobp)
		    ;; Try again from the beginning of the buffer.
		    (goto-char (point-min))
		    (while (and (not (eobp))
				(progn
				  (outline-next-heading)
				  (<= (funcall outline-level) level)))))
		  (unless (eobp) (match-string 0))))
	      (save-match-data
		;; Bummer!! There is no lower heading in the buffer.
		;; Let's try to invent one by repeating the first char.
		(let ((new-head (concat (substring head 0 1) head)))
		  (if (string-match (concat "\\`" outline-regexp) new-head)
		      ;; Why bother checking that it is indeed of lower level ?
		      new-head
		    ;; Didn't work: keep it as is so it's still a heading.
		    head))))))

    (unless (rassoc level outline-heading-alist)
      (push (cons head level) outline-heading-alist))
    
    (replace-match down-head nil t)
    (when children
      (outline-map-tree 'outline-demote level))))

(defun outline-map-tree (fun level)
  "Call FUN for every heading underneath the current one."
  (save-excursion
    (while (and (progn
		  (outline-next-heading)
		  (> (funcall outline-level) level))
		(not (eobp)))
      (funcall fun))))

(defun outline-end-of-heading ()
  (if (re-search-forward outline-heading-end-regexp nil 'move)
      (forward-char -1)))

(defun outline-next-visible-heading (arg)
  "Move to the next visible heading line.
With argument, repeats or can move backward if negative.
A heading line is one that starts with a `*' (or that
`outline-regexp' matches)."
  (interactive "p")
  (if (< arg 0)
      (beginning-of-line)
    (end-of-line))
  (while (and (not (bobp)) (< arg 0))
    (while (and (not (bobp))
		(re-search-backward (concat "^\\(" outline-regexp "\\)")
				    nil 'move)
		(outline-invisible-p)))
    (setq arg (1+ arg)))
  (while (and (not (eobp)) (> arg 0))
    (while (and (not (eobp))
		(re-search-forward (concat "^\\(" outline-regexp "\\)")
				   nil 'move)
		(outline-invisible-p)))
    (setq arg (1- arg)))
  (beginning-of-line))

(defun outline-previous-visible-heading (arg)
  "Move to the previous heading line.
With argument, repeats or can move forward if negative.
A heading line is one that starts with a `*' (or that
`outline-regexp' matches)."
  (interactive "p")
  (outline-next-visible-heading (- arg)))

(defun outline-mark-subtree ()
  "Mark the current subtree in an outlined document.
This puts point at the start of the current subtree, and mark at the end."
  (interactive)
  (let ((beg))
    (if (outline-on-heading-p)
	;; we are already looking at a heading
	(beginning-of-line)
      ;; else go back to previous heading
      (outline-previous-visible-heading 1))
    (setq beg (point))
    (outline-end-of-subtree)
    (push-mark (point))
    (goto-char beg)))


(put 'outline 'reveal-toggle-invisible 'outline-reveal-toggle-invisible)
(defun outline-flag-region (from to flag)
  "Hide or show lines from FROM to TO, according to FLAG.
If FLAG is nil then text is shown, while if FLAG is t the text is hidden."
  (remove-overlays from to 'invisible 'outline)
  (when flag
    (let ((o (make-overlay from to)))
      (overlay-put o 'invisible 'outline)
      (overlay-put o 'isearch-open-invisible 'outline-isearch-open-invisible)))
  ;; Seems only used by lazy-lock.  I.e. obsolete.
  (run-hooks 'outline-view-change-hook))

(defun outline-reveal-toggle-invisible (o hidep)
  (save-excursion
    (goto-char (overlay-start o))
    (if hidep
	;; When hiding the area again, we could just clean it up and let
	;; reveal do the rest, by simply doing:
	;; (remove-overlays (overlay-start o) (overlay-end o)
	;;                  'invisible 'outline)
	;; 
	;; That works fine as long as everything is in sync, but if the
	;; structure of the document is changed while revealing parts of it,
	;; the resulting behavior can be ugly.  I.e. we need to make
	;; sure that we hide exactly a subtree.
	(progn
	  (let ((end (overlay-end o)))
	    (delete-overlay o)
	    (while (progn
		     (hide-subtree)
		     (outline-next-visible-heading 1)
		     (and (not (eobp)) (< (point) end))))))

      ;; When revealing, we just need to reveal sublevels.  If point is
      ;; inside one of the sublevels, reveal will call us again.
      ;; But we need to preserve the original overlay.
      (let ((o1 (copy-overlay o)))
	(overlay-put o 'invisible nil)	;Show (most of) the text.
	(while (progn
		 (show-entry)
		 (show-children)
		 ;; Normally just the above is needed.
		 ;; But in odd cases, the above might fail to show anything.
		 ;; To avoid an infinite loop, we have to make sure that
		 ;; *something* gets shown.
		 (and (equal (overlay-start o) (overlay-start o1))
		      (< (point) (overlay-end o))
		      (= 0 (forward-line 1)))))
	;; If still nothing was shown, just kill the damn thing.
	(when (equal (overlay-start o) (overlay-start o1))
	  ;; I've seen it happen at the end of buffer.
	  (delete-overlay o1))))))

;; Function to be set as an outline-isearch-open-invisible' property
;; to the overlay that makes the outline invisible (see
;; `outline-flag-region').
(defun outline-isearch-open-invisible (overlay)
  ;; We rely on the fact that isearch places point on the matched text.
  (show-entry))

(defun hide-entry ()
  "Hide the body directly following this heading."
  (interactive)
  (outline-back-to-heading)
  (outline-end-of-heading)
  (save-excursion
    (outline-flag-region (point) (progn (outline-next-preface) (point)) t)))

(defun show-entry ()
  "Show the body directly following this heading.
Show the heading too, if it is currently invisible."
  (interactive)
  (save-excursion
    (outline-back-to-heading t)
    (outline-flag-region (1- (point))
			 (progn (outline-next-preface) (point)) nil)))

(defun hide-body ()
  "Hide all of buffer except headings."
  (interactive)
  (hide-region-body (point-min) (point-max)))

(defun hide-region-body (start end)
  "Hide all body lines in the region, but not headings."
  ;; Nullify the hook to avoid repeated calls to `outline-flag-region'
  ;; wasting lots of time running `lazy-lock-fontify-after-outline'
  ;; and run the hook finally.
  (let (outline-view-change-hook)
    (save-excursion
      (save-restriction
	(narrow-to-region start end)
	(goto-char (point-min))
	(if (outline-on-heading-p)
	    (outline-end-of-heading))
	(while (not (eobp))
	  (outline-flag-region (point)
			       (progn (outline-next-preface) (point)) t)
	  (unless (eobp)
	    (forward-char (if (looking-at "\n\n") 2 1))
	    (outline-end-of-heading))))))
  (run-hooks 'outline-view-change-hook))

(defun show-all ()
  "Show all of the text in the buffer."
  (interactive)
  (outline-flag-region (point-min) (point-max) nil))

(defun hide-subtree ()
  "Hide everything after this heading at deeper levels."
  (interactive)
  (outline-flag-subtree t))

(defun hide-leaves ()
  "Hide all body after this heading at deeper levels."
  (interactive)
  (outline-back-to-heading)
  (save-excursion
    (outline-end-of-heading)
    (hide-region-body (point) (progn (outline-end-of-subtree) (point)))))

(defun show-subtree ()
  "Show everything after this heading at deeper levels."
  (interactive)
  (outline-flag-subtree nil))

(defun hide-sublevels (levels)
  "Hide everything but the top LEVELS levels of headers, in whole buffer."
  (interactive "p")
  (if (< levels 1)
      (error "Must keep at least one level of headers"))
  (setq levels (1- levels))
  (let (outline-view-change-hook)
    (save-excursion
      (goto-char (point-min))
      ;; Keep advancing to the next top-level heading.
      (while (or (and (bobp) (outline-on-heading-p))
		 (outline-next-heading))
	(let ((end (save-excursion (outline-end-of-subtree) (point))))
	  ;; Hide everything under that.
	  (outline-end-of-heading)
	  (outline-flag-region (point) end t)
	  ;; Show the first LEVELS levels under that.
	  (if (> levels 0)
	      (show-children levels))
	  ;; Move to the next, since we already found it.
	  (goto-char end)))))
  (run-hooks 'outline-view-change-hook))

(defun hide-other ()
  "Hide everything except current body and parent and top-level headings."
  (interactive)
  (hide-sublevels 1)
  (let (outline-view-change-hook)
    (save-excursion
      (outline-back-to-heading t)
      (show-entry)
      (while (condition-case nil (progn (outline-up-heading 1) (not (bobp)))
	       (error nil))
	(outline-flag-region (1- (point))
			     (save-excursion (forward-line 1) (point))
			     nil))))
  (run-hooks 'outline-view-change-hook))

(defun outline-toggle-children ()
  "Show or hide the current subtree depending on its current state."
  (interactive)
  (outline-back-to-heading)
  (if (save-excursion
	(end-of-line)
	(not (outline-invisible-p)))
      (hide-subtree)
    (show-children)
    (show-entry)))

(defun outline-flag-subtree (flag)
  (save-excursion
    (outline-back-to-heading)
    (outline-end-of-heading)
    (outline-flag-region (point)
			  (progn (outline-end-of-subtree) (point))
			  flag)))

(defun outline-end-of-subtree ()
  (outline-back-to-heading)
  (let ((opoint (point))
	(first t)
	(level (funcall outline-level)))
    (while (and (not (eobp))
		(or first (> (funcall outline-level) level)))
      (setq first nil)
      (outline-next-heading))
    (if (bolp)
	(progn
	  ;; Go to end of line before heading
	  (forward-char -1)
	  (if (bolp)
	      ;; leave blank line before heading
	      (forward-char -1))))))

(defun show-branches ()
  "Show all subheadings of this heading, but not their bodies."
  (interactive)
  (show-children 1000))

(defun show-children (&optional level)
  "Show all direct subheadings of this heading.
Prefix arg LEVEL is how many levels below the current level should be shown.
Default is enough to cause the following heading to appear."
  (interactive "P")
  (setq level
	(if level (prefix-numeric-value level)
	  (save-excursion
	    (outline-back-to-heading)
	    (let ((start-level (funcall outline-level)))
	      (outline-next-heading)
	      (if (eobp)
		  1
		(max 1 (- (funcall outline-level) start-level)))))))
  (let (outline-view-change-hook)
    (save-excursion
      (save-restriction
	(outline-back-to-heading)
	(setq level (+ level (funcall outline-level)))
	(narrow-to-region (point)
			  (progn (outline-end-of-subtree)
				 (if (eobp) (point-max) (1+ (point)))))
	(goto-char (point-min))
	(while (and (not (eobp))
		    (progn
		      (outline-next-heading)
		      (not (eobp))))
	  (if (<= (funcall outline-level) level)
	      (save-excursion
		(outline-flag-region (save-excursion
				       (forward-char -1)
				       (if (bolp)
					   (forward-char -1))
				       (point))
				     (progn (outline-end-of-heading) (point))
				     nil)))))))
  (run-hooks 'outline-view-change-hook))



(defun outline-up-heading (arg &optional invisible-ok)
  "Move to the visible heading line of which the present line is a subheading.
With argument, move up ARG levels.
If INVISIBLE-OK is non-nil, also consider invisible lines."
  (interactive "p")
  (outline-back-to-heading invisible-ok)
  (if (eq (funcall outline-level) 1)
      (error "Already at top level of the outline"))
  (while (and (> (funcall outline-level) 1)
	      (> arg 0)
	      (not (bobp)))
    (let ((present-level (funcall outline-level)))
      (while (and (not (< (funcall outline-level) present-level))
		  (not (bobp)))
	(if invisible-ok
	    (outline-previous-heading)
	  (outline-previous-visible-heading 1)))
      (setq arg (- arg 1)))))

(defun outline-forward-same-level (arg)
  "Move forward to the ARG'th subheading at same level as this one.
Stop at the first and last subheadings of a superior heading."
  (interactive "p")
  (outline-back-to-heading)
  (while (> arg 0)
    (let ((point-to-move-to (save-excursion
			      (outline-get-next-sibling))))
      (if point-to-move-to
	  (progn
	    (goto-char point-to-move-to)
	    (setq arg (1- arg)))
	(progn
	  (setq arg 0)
	  (error "No following same-level heading"))))))

(defun outline-get-next-sibling ()
  "Move to next heading of the same level, and return point or nil if none."
  (let ((level (funcall outline-level)))
    (outline-next-visible-heading 1)
    (while (and (> (funcall outline-level) level)
		(not (eobp)))
      (outline-next-visible-heading 1))
    (if (< (funcall outline-level) level)
	nil
      (point))))

(defun outline-backward-same-level (arg)
  "Move backward to the ARG'th subheading at same level as this one.
Stop at the first and last subheadings of a superior heading."
  (interactive "p")
  (outline-back-to-heading)
  (while (> arg 0)
    (let ((point-to-move-to (save-excursion
			      (outline-get-last-sibling))))
      (if point-to-move-to
	  (progn
	    (goto-char point-to-move-to)
	    (setq arg (1- arg)))
	(progn
	  (setq arg 0)
	  (error "No previous same-level heading"))))))

(defun outline-get-last-sibling ()
  "Move to previous heading of the same level, and return point or nil if none."
  (let ((level (funcall outline-level)))
    (outline-previous-visible-heading 1)
    (while (and (> (funcall outline-level) level)
		(not (bobp)))
      (outline-previous-visible-heading 1))
    (if (< (funcall outline-level) level)
	nil
        (point))))

(defun outline-headers-as-kill (beg end)
  "Save the visible outline headers in region at the start of the kill ring.

Text shown between the headers isn't copied.  Two newlines are
inserted between saved headers.  Yanking the result may be a
convenient way to make a table of contents of the buffer."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (let ((buffer (current-buffer))
	    start end)
	(with-temp-buffer
	  (with-current-buffer buffer
	    ;; Boundary condition: starting on heading:
	    (when (outline-on-heading-p)
	      (outline-back-to-heading)
	      (setq start (point)
		    end (progn (outline-end-of-heading)
			       (point)))
	      (insert-buffer-substring buffer start end)
	      (insert "\n\n")))
	  (let ((temp-buffer (current-buffer)))
	    (with-current-buffer buffer
	      (while (outline-next-heading)
		(unless (outline-invisible-p)
		  (setq start (point)
			end (progn (outline-end-of-heading) (point)))
		  (with-current-buffer temp-buffer
		    (insert-buffer-substring buffer start end)
		    (insert "\n\n"))))))
	  (kill-new (buffer-string)))))))

(provide 'outline)
(provide 'noutline)

;;; outline.el ends here
