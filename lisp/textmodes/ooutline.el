;;; outline.el --- outline mode commands for Emacs

;; Copyright (C) 1986, 1993 Free Software Foundation, Inc.

;; Maintainer: FSF

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
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; This package is a major mode for editing outline-format documents.
;; An outline can be `abstracted' to show headers at any given level,
;; with all stuff below hidden.  See the Emacs manual for details.

;;; Code:

;; Jan '86, Some new features added by Peter Desnoyers and rewritten by RMS.
  
(defvar outline-regexp "[*\^l]+"
  "*Regular expression to match the beginning of a heading.
Any line whose beginning matches this regexp is considered to start a heading.
The recommended way to set this is with a Local Variables: list
in the file it applies to.  See also outline-heading-end-regexp.")
  
(defvar outline-heading-end-regexp "[\n\^M]"
  "*Regular expression to match the end of a heading line.
You can assume that point is at the beginning of a heading when this
regexp is searched for.  The heading ends at the end of the match.
The recommended way to set this is with a \"Local Variables:\" list
in the file it applies to.")

(defvar outline-mode-map nil "")

(if outline-mode-map
    nil
  (setq outline-mode-map (nconc (make-sparse-keymap) text-mode-map))
  (define-key outline-mode-map "\C-c\C-n" 'outline-next-visible-heading)
  (define-key outline-mode-map "\C-c\C-p" 'outline-previous-visible-heading)
  (define-key outline-mode-map "\C-c\C-i" 'show-children)
  (define-key outline-mode-map "\C-c\C-s" 'show-subtree)
  (define-key outline-mode-map "\C-c\C-h" 'hide-subtree)
  (define-key outline-mode-map "\C-c\C-u" 'outline-up-heading)
  (define-key outline-mode-map "\C-c\C-f" 'outline-forward-same-level)
  (define-key outline-mode-map "\C-c\C-b" 'outline-backward-same-level)

  (define-key outline-mode-map [menu-bar hide]
    (cons "Hide" (make-sparse-keymap "Hide")))

  (define-key outline-mode-map [menu-bar hide hide-subtree]
    '("Hide Subtree" . hide-subtree))
  (define-key outline-mode-map [menu-bar hide hide-entry]
    '("Hide Entry" . hide-entry))
  (define-key outline-mode-map [menu-bar hide hide-body]
    '("Hide Body" . hide-body))
  (define-key outline-mode-map [menu-bar hide hide-leaves]
    '("Hide Leaves" . hide-leaves))

  (define-key outline-mode-map [menu-bar show]
    (cons "Show" (make-sparse-keymap "Show")))

  (define-key outline-mode-map [menu-bar show show-subtree]
    '("Show Subtree" . show-subtree))
  (define-key outline-mode-map [menu-bar show show-children]
    '("Show Children" . show-children))
  (define-key outline-mode-map [menu-bar show show-branches]
    '("Show Branches" . show-branches))
  (define-key outline-mode-map [menu-bar show show-entry]
    '("Show Entry" . show-entry))
  (define-key outline-mode-map [menu-bar show show-all]
    '("Show All" . show-all))

  (define-key outline-mode-map [menu-bar headings]
    (cons "Headings" (make-sparse-keymap "Headings")))

  (define-key outline-mode-map [menu-bar headings outline-backward-same-level]
    '("Previous Same Level" . outline-backward-same-level))
  (define-key outline-mode-map [menu-bar headings outline-forward-same-level]
    '("Next Same Level" . outline-forward-same-level))
  (define-key outline-mode-map [menu-bar headings outline-previous-visible-heading]
    '("Previous" . outline-previous-visible-heading))
  (define-key outline-mode-map [menu-bar headings outline-next-visible-heading]
    '("Next" . outline-next-visible-heading))
  (define-key outline-mode-map [menu-bar headings outline-up-heading]
    '("Up" . outline-up-heading)))

(defvar outline-minor-mode nil
  "Non-nil if using Outline mode as a minor mode of some other mode.")
(make-variable-buffer-local 'outline-minor-mode)
(put 'outline-minor-mode 'permanent-local t)
(setq minor-mode-alist (append minor-mode-alist
			       (list '(outline-minor-mode " Outl"))))

;;;###autoload
(defun outline-mode ()
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

M-x hide-body	make all text invisible (not headings).
M-x show-all	make everything in buffer visible.

The remaining commands are used when point is on a heading line.
They apply to some of the body or subheadings of that heading.
\\[hide-subtree]   hide-subtree	make body and subheadings invisible.
\\[show-subtree]   show-subtree	make body and subheadings visible.
\\[show-children]   show-children	make direct subheadings visible.
		 No effect on body, or subheadings 2 or more levels down.
		 With arg N, affects subheadings N levels down.
M-x hide-entry	   make immediately following body invisible.
M-x show-entry	   make it visible.
M-x hide-leaves	   make body under heading and under its subheadings invisible.
		     The subheadings remain visible.
M-x show-branches  make all subheadings at all levels visible.

The variable `outline-regexp' can be changed to control what is a heading.
A line is a heading if `outline-regexp' matches something at the
beginning of the line.  The longer the match, the deeper the level.

Turning on outline mode calls the value of `text-mode-hook' and then of
`outline-mode-hook', if they are non-nil."
  (interactive)
  (kill-all-local-variables)
  (setq selective-display t)
  (use-local-map outline-mode-map)
  (setq mode-name "Outline")
  (setq major-mode 'outline-mode)
  (define-abbrev-table 'text-mode-abbrev-table ())
  (setq local-abbrev-table text-mode-abbrev-table)
  (set-syntax-table text-mode-syntax-table)
  (make-local-variable 'paragraph-start)
  (setq paragraph-start (concat paragraph-start "\\|^\\("
				outline-regexp "\\)"))
  ;; Inhibit auto-filling of header lines.
  (make-local-variable 'auto-fill-inhibit-regexp)
  (setq auto-fill-inhibit-regexp outline-regexp)
  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate (concat paragraph-separate "\\|^\\("
				   outline-regexp "\\)"))
  (run-hooks 'text-mode-hook 'outline-mode-hook))

(defvar outline-minor-mode-prefix "\C-c"
  "*Prefix key to use for Outline commands in Outline minor mode.")

(defvar outline-minor-mode-map nil)
(if outline-minor-mode-map
    nil
  (setq outline-minor-mode-map (make-sparse-keymap))
  (define-key outline-minor-mode-map [menu-bar]
    (lookup-key outline-mode-map [menu-bar]))
  (define-key outline-minor-mode-map outline-minor-mode-prefix
    (lookup-key outline-mode-map "\C-c")))

(or (assq 'outline-minor-mode minor-mode-map-alist)
    (setq minor-mode-map-alist
	  (cons (cons 'outline-minor-mode outline-minor-mode-map)
		minor-mode-map-alist)))

;;;###autoload
(defun outline-minor-mode (&optional arg)
  "Toggle Outline minor mode.
With arg, turn Outline minor mode on if arg is positive, off otherwise.
See the command `outline-mode' for more information on this mode."
  (interactive "P")
  (setq outline-minor-mode
	(if (null arg) (not outline-minor-mode)
	  (> (prefix-numeric-value arg) 0)))
  (if outline-minor-mode
      (progn
	(setq selective-display t)
	(run-hooks 'outline-minor-mode-hook))
    (setq selective-display nil))
  (set-buffer-modified-p (buffer-modified-p)))

(defvar outline-level 'outline-level
  "Function of no args to compute a header's nesting level in an outline.
It can assume point is at the beginning of a header line.")

(defun outline-level ()
  "Return the depth to which a statement is nested in the outline.
Point must be at the beginning of a header line.  This is actually
the column number of the end of what `outline-regexp matches'."
  (save-excursion
    (looking-at outline-regexp)
    (save-excursion (goto-char (match-end 0)) (current-column))))

(defun outline-next-preface ()
  "Skip forward to just before the next heading line."
  (if (re-search-forward (concat "[\n\^M]\\(" outline-regexp "\\)")
			 nil 'move)
      (goto-char (match-beginning 0)))
  (if (memq (preceding-char) '(?\n ?\^M))
      (forward-char -1)))

(defun outline-next-heading ()
  "Move to the next (possibly invisible) heading line."
  (interactive)
  (if (re-search-forward (concat "[\n\^M]\\(" outline-regexp "\\)")
			 nil 'move)
      (goto-char (1+ (match-beginning 0)))))

(defun outline-back-to-heading ()
  "Move to previous (possibly invisible) heading line,
or to the beginning of this line if it is a heading line."
  (beginning-of-line)
  (or (outline-on-heading-p)
      (re-search-backward (concat "^\\(" outline-regexp "\\)") nil 'move)))

(defun outline-on-heading-p ()
  "Return T if point is on a header line."
  (save-excursion
    (beginning-of-line)
    (and (eq (preceding-char) ?\n)
	 (looking-at outline-regexp))))

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
  (re-search-forward (concat "^\\(" outline-regexp "\\)") nil nil arg)
  (beginning-of-line))

(defun outline-previous-visible-heading (arg)
  "Move to the previous heading line.
With argument, repeats or can move forward if negative.
A heading line is one that starts with a `*' (or that
`outline-regexp' matches)."
  (interactive "p")
  (outline-next-visible-heading (- arg)))

(defun outline-flag-region (from to flag)
  "Hides or shows lines from FROM to TO, according to FLAG.
If FLAG is `\\n' (newline character) then text is shown,
while if FLAG is `\\^M' (control-M) the text is hidden."
  (let (buffer-read-only)
    (subst-char-in-region from to
			  (if (= flag ?\n) ?\^M ?\n)
			  flag t)))

(defun hide-entry ()
  "Hide the body directly following this heading."
  (interactive)
  (outline-back-to-heading)
  (outline-end-of-heading)
  (save-excursion
   (outline-flag-region (point) (progn (outline-next-preface) (point)) ?\^M)))

(defun show-entry ()
  "Show the body directly following this heading."
  (interactive)
  (save-excursion
   (outline-flag-region (point) (progn (outline-next-preface) (point)) ?\n)))

(defun hide-body ()
  "Hide all of buffer except headings."
  (interactive)
  (hide-region-body (point-min) (point-max)))

(defun hide-region-body (start end)
  "Hide all body lines in the region, but not headings."
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (goto-char (point-min))
      (if (outline-on-heading-p)
	  (outline-end-of-heading))
      (while (not (eobp))
	(outline-flag-region (point)
			     (progn (outline-next-preface) (point)) ?\^M)
	(if (not (eobp))
	    (progn
	      (forward-char
	       (if (looking-at "[\n\^M][\n\^M]")
		   2 1))
	      (outline-end-of-heading)))))))

(defun show-all ()
  "Show all of the text in the buffer."
  (interactive)
  (outline-flag-region (point-min) (point-max) ?\n))

(defun hide-subtree ()
  "Hide everything after this heading at deeper levels."
  (interactive)
  (outline-flag-subtree ?\^M))

(defun hide-leaves ()
  "Hide all body after this heading at deeper levels."
  (interactive)
  (outline-back-to-heading)
  (outline-end-of-heading)
  (hide-region-body (point) (progn (outline-end-of-subtree) (point))))

(defun show-subtree ()
  "Show everything after this heading at deeper levels."
  (interactive)
  (outline-flag-subtree ?\n))

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
    (forward-char -1)
    (if (memq (preceding-char) '(?\n ?\^M))
	(forward-char -1))))

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
	    (beginning-of-line)
	    (let ((start-level (funcall outline-level)))
	      (outline-next-heading)
	      (max 1 (- (funcall outline-level) start-level))))))
  (save-excursion
   (save-restriction
    (beginning-of-line)
    (setq level (+ level (funcall outline-level)))
    (narrow-to-region (point)
		      (progn (outline-end-of-subtree) (1+ (point))))
    (goto-char (point-min))
    (while (and (not (eobp))
		(progn
		 (outline-next-heading)
		 (not (eobp))))
      (if (<= (funcall outline-level) level)
	  (save-excursion
	    (outline-flag-region (save-excursion
				   (forward-char -1)
				   (if (memq (preceding-char) '(?\n ?\^M))
				       (forward-char -1))
				   (point))
				 (progn (outline-end-of-heading) (point))
				 ?\n)))))))

(defun outline-up-heading (arg)
  "Move to the heading line of which the present line is a subheading.
With argument, move up ARG levels."
  (interactive "p")
  (outline-back-to-heading)
  (if (eq (funcall outline-level) 1)
      (error ""))
    (while (and (> (funcall outline-level) 1)
		(> arg 0)
		(not (bobp)))
      (let ((present-level (funcall outline-level)))
	(while (not (< (funcall outline-level) present-level))
	  (outline-previous-visible-heading 1))
	(setq arg (- arg 1)))))

(defun outline-forward-same-level (arg)
  "Move forward to the ARG'th subheading from here of the same level as the
present one. It stops at the first and last subheadings of a superior heading."
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
	  (error ""))))))

(defun outline-get-next-sibling ()
  "Position the point at the next heading of the same level, 
and return that position or nil if it cannot be found."
  (let ((level (funcall outline-level)))
    (outline-next-visible-heading 1)
    (while (and (> (funcall outline-level) level)
		(not (eobp)))
      (outline-next-visible-heading 1))
    (if (< (funcall outline-level) level)
	nil
      (point))))
	
(defun outline-backward-same-level (arg)
  "Move backward to the ARG'th subheading from here of the same level as the
present one. It stops at the first and last subheadings of a superior heading."
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
	  (error ""))))))

(defun outline-get-last-sibling ()
  "Position the point at the previous heading of the same level, 
and return that position or nil if it cannot be found."
  (let ((level (funcall outline-level)))
    (outline-previous-visible-heading 1)
    (while (and (> (funcall outline-level) level)
		(not (bobp)))
      (outline-previous-visible-heading 1))
    (if (< (funcall outline-level) level)
	nil
        (point))))

(provide 'outline)

;;; outline.el ends here
