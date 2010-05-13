;;; org-list.el --- Plain lists for Org-mode
;;
;; Copyright (C) 2004, 2005, 2006, 2007, 2008, 2009, 2010
;;   Free Software Foundation, Inc.
;;
;; Author: Carsten Dominik <carsten at orgmode dot org>
;;	   Bastien Guerry <bzg AT altern DOT org>
;; Keywords: outlines, hypermedia, calendar, wp
;; Homepage: http://orgmode.org
;; Version: 6.35i
;;
;; This file is part of GNU Emacs.
;;
;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:

;; This file contains the code dealing with plain lists in Org-mode.

;;; Code:

(require 'org-macs)
(require 'org-compat)

(defvar org-blank-before-new-entry)
(defvar org-M-RET-may-split-line)
(defvar org-complex-heading-regexp)
(defvar org-odd-levels-only)

(declare-function org-invisible-p "org" ())
(declare-function org-on-heading-p "org" (&optional invisible-ok))
(declare-function outline-next-heading "outline" ())
(declare-function org-back-to-heading "org" (&optional invisible-ok))
(declare-function org-back-over-empty-lines "org" ())
(declare-function org-skip-whitespace "org" ())
(declare-function org-trim "org" (s))
(declare-function org-get-indentation "org" (&optional line))
(declare-function org-timer-item "org-timer" (&optional arg))
(declare-function org-combine-plists "org" (&rest plists))
(declare-function org-entry-get "org" (pom property &optional inherit))
(declare-function org-narrow-to-subtree "org" ())
(declare-function org-show-subtree "org" ())

(defgroup org-plain-lists nil
  "Options concerning plain lists in Org-mode."
  :tag "Org Plain lists"
  :group 'org-structure)

(defcustom org-cycle-include-plain-lists t
  "When t, make TAB cycle visibility on plain list items.

Cycling plain lists works only when the cursor is on a plain list
item.  When the cursor is on an outline heading, plain lists are
treated as text.  This is the most stable way of handling this,
which is why it is the default.

When this is the symbol `integrate', then during cycling, plain
list items will *temporarily* be interpreted as outline headlines
with a level given by 1000+i where i is the indentation of the
bullet.  This setting can lead to strange effects when switching
visibility to `children', because the first \"child\" in a
subtree decides what children should be listed.  If that first
\"child\" is a plain list item with an implied large level
number, all true children and grand children of the outline
heading will be exposed in a children' view."
  :group 'org-plain-lists
  :type '(choice
	  (const :tag "Never" nil)
	  (const :tag "With cursor in plain list (recommended)" t)
	  (const :tag "As children of outline headings" integrate)))

(defcustom org-list-demote-modify-bullet nil
  "Default bullet type installed when demoting an item.
This is an association list, for each bullet type, this alist will point
to the bulled that should be used when this item is demoted."
  :group 'org-plain-lists
  :type '(repeat
	  (cons
	   (choice :tag "If the current bullet is  "
		   (const "-")
		   (const "+")
		   (const "*")
		   (const "1.")
		   (const "1)"))
	   (choice :tag "demotion will change it to"
		   (const "-")
		   (const "+")
		   (const "*")
		   (const "1.")
		   (const "1)")))))

(defcustom org-plain-list-ordered-item-terminator t
  "The character that makes a line with leading number an ordered list item.
Valid values are ?. and ?\).  To get both terminators, use t.  While
?. may look nicer, it creates the danger that a line with leading
number may be incorrectly interpreted as an item.  ?\) therefore is
the safe choice."
  :group 'org-plain-lists
  :type '(choice (const :tag "dot like in \"2.\"" ?.)
		 (const :tag "paren like in \"2)\"" ?\))
		 (const :tab "both" t)))

(defcustom org-list-two-spaces-after-bullet-regexp nil
  "A regular expression matching bullets that should have 2 spaces after them.
When nil, no bullet will have two spaces after them.
When a string, it will be used as a regular expression.	 When the bullet
type of a list is changed, the new bullet type will be matched against this
regexp.	 If it matches, there will be two spaces instead of one after
the bullet in each item of he list."
  :group 'org-plain-list
  :type '(choice
	  (const :tag "never" nil)
	  (regexp)))

(defcustom org-empty-line-terminates-plain-lists nil
  "Non-nil means an empty line ends all plain list levels.
This is currently effective only during export.  It should also have
an effect for indentation and plain list folding, but it does not.
When nil, empty lines are part of the preceding item."
  :group 'org-plain-lists
  :type 'boolean)

(defcustom org-auto-renumber-ordered-lists t
  "Non-nil means automatically renumber ordered plain lists.
Renumbering happens when the sequence have been changed with
\\[org-shiftmetaup] or \\[org-shiftmetadown].  After other editing commands,
use \\[org-ctrl-c-ctrl-c] to trigger renumbering."
  :group 'org-plain-lists
  :type 'boolean)

(defcustom org-provide-checkbox-statistics t
  "Non-nil means update checkbox statistics after insert and toggle.
When this is set, checkbox statistics is updated each time you
either insert a new checkbox with \\[org-insert-todo-heading] or
toggle a checkbox with \\[org-ctrl-c-ctrl-c]."
  :group 'org-plain-lists
  :type 'boolean)

(defcustom org-hierarchical-checkbox-statistics t
  "Non-nil means checkbox statistics counts only the state of direct children.
When nil, all boxes below the cookie are counted.
This can be set to nil on a per-node basis using a COOKIE_DATA property
with the word \"recursive\" in the value."
  :group 'org-plain-lists
  :type 'boolean)

(defcustom org-description-max-indent 20
  "Maximum indentation for the second line of a description list.
When the indentation would be larger than this, it will become
5 characters instead."
  :group 'org-plain-lists
  :type 'integer)

(defvar org-list-beginning-re
  "^\\([ \t]*\\)\\([-+]\\|[0-9]+[.)]\\) +\\(.*\\)$")

(defcustom org-list-radio-list-templates
  '((latex-mode "% BEGIN RECEIVE ORGLST %n
% END RECEIVE ORGLST %n
\\begin{comment}
#+ORGLST: SEND %n org-list-to-latex
| | |
\\end{comment}\n")
    (texinfo-mode "@c BEGIN RECEIVE ORGLST %n
@c END RECEIVE ORGLST %n
@ignore
#+ORGLST: SEND %n org-list-to-texinfo
| | |
@end ignore\n")
    (html-mode "<!-- BEGIN RECEIVE ORGLST %n -->
<!-- END RECEIVE ORGLST %n -->
<!--
#+ORGLST: SEND %n org-list-to-html
| | |
-->\n"))
  "Templates for radio lists in different major modes.
All occurrences of %n in a template will be replaced with the name of the
list, obtained by prompting the user."
  :group 'org-plain-lists
  :type '(repeat
	  (list (symbol :tag "Major mode")
		(string :tag "Format"))))

;;;; Plain list items, including checkboxes

;;; Plain list items

(defun org-at-item-p ()
  "Is point in a line starting a hand-formatted item?"
  (let ((llt org-plain-list-ordered-item-terminator))
    (save-excursion
      (goto-char (point-at-bol))
      (looking-at
       (cond
	((eq llt t)  "\\([ \t]*\\([-+]\\|\\([0-9]+[.)]\\)\\)\\|[ \t]+\\*\\)\\( \\|$\\)")
	((= llt ?.)  "\\([ \t]*\\([-+]\\|\\([0-9]+\\.\\)\\)\\|[ \t]+\\*\\)\\( \\|$\\)")
	((= llt ?\)) "\\([ \t]*\\([-+]\\|\\([0-9]+)\\)\\)\\|[ \t]+\\*\\)\\( \\|$\\)")
	(t (error "Invalid value of `org-plain-list-ordered-item-terminator'")))))))

(defun org-at-item-bullet-p ()
  "Is point at the bullet of a plain list item?"
  (and (org-at-item-p)
       (not (member (char-after) '(?\  ?\t)))
       (< (point) (match-end 0))))

(defun org-in-item-p ()
  "It the cursor inside a plain list item.
Does not have to be the first line."
  (save-excursion
    (condition-case nil
	(progn
	  (org-beginning-of-item)
	  (org-at-item-p)
	  t)
      (error nil))))

(defun org-insert-item (&optional checkbox)
  "Insert a new item at the current level.
Return t when things worked, nil when we are not in an item."
  (when (save-excursion
	  (condition-case nil
	      (progn
		(org-beginning-of-item)
		(org-at-item-p)
		(if (org-invisible-p) (error "Invisible item"))
		t)
	    (error nil)))
    (let* ((bul (match-string 0))
	   (descp (save-excursion (goto-char (match-beginning 0))
				  (beginning-of-line 1)
				  (save-match-data
				    (and (looking-at "[ \t]*\\(.*?\\) ::")
					 (match-string 1)))))
	   (empty-line-p (save-excursion
			   (goto-char (match-beginning 0))
			   (and (not (bobp))
				(or (beginning-of-line 0) t)
				(save-match-data
				  (looking-at "[ \t]*$")))))
	   (timerp (and descp
			(save-match-data
			  (string-match "^[-+*][ \t]+[0-9]+:[0-9]+:[0-9]+$"
					descp))))
	   (eow (save-excursion (beginning-of-line 1) (looking-at "[ \t]*")
				(match-end 0)))
	   (blank-a (if org-empty-line-terminates-plain-lists
			nil
		      (cdr (assq 'plain-list-item org-blank-before-new-entry))))
	   (blank (if (eq blank-a 'auto) empty-line-p blank-a))
	   pos)
      (if descp (setq checkbox nil))
      (if timerp
	  (progn (org-timer-item) t)
	(cond
	 ((and (org-at-item-p) (<= (point) eow))
	  ;; before the bullet
	  (beginning-of-line 1)
	  (open-line (if blank 2 1)))
	 ((<= (point) eow)
	  (beginning-of-line 1))
	 (t
	  (unless (org-get-alist-option org-M-RET-may-split-line 'item)
	    (end-of-line 1)
	    (delete-horizontal-space))
	  (newline (if blank 2 1))))
	(insert bul
		(if checkbox "[ ]" "")
		(if descp (concat (if checkbox " " "")
				  (read-string "Term: ") " :: ") ""))
	(just-one-space)
	(setq pos (point))
	(end-of-line 1)
	(unless (= (point) pos) (just-one-space) (backward-delete-char 1)))
      (org-maybe-renumber-ordered-list)
      (and checkbox (org-update-checkbox-count-maybe))
      t)))

;;; Checkboxes

(defun org-at-item-checkbox-p ()
  "Is point at a line starting a plain-list item with a checklet?"
  (and (org-at-item-p)
       (save-excursion
	 (goto-char (match-end 0))
	 (skip-chars-forward " \t")
	 (looking-at "\\[[- X]\\]"))))

(defun org-toggle-checkbox (&optional toggle-presence)
  "Toggle the checkbox in the current line.
With prefix arg TOGGLE-PRESENCE, add or remove checkboxes.
With double prefix, set checkbox to [-].
When there is an active region, toggle status or presence of the checkbox
in the first line, and make every item in the region have the same
status or presence, respectively.
If the cursor is in a headline, apply this to all checkbox items in the
text below the heading."
  (interactive "P")
  (catch 'exit
    (let (beg end status first-present first-status blocked)
      (cond
       ((org-region-active-p)
	(setq beg (region-beginning) end (region-end)))
       ((org-on-heading-p)
	(setq beg (point) end (save-excursion (outline-next-heading) (point))))
       ((org-at-item-checkbox-p)
	(save-excursion
	  (if (equal toggle-presence '(4))
	      (progn
		(replace-match "")
		(goto-char (match-beginning 0))
		(just-one-space))
	    (when (setq blocked (org-checkbox-blocked-p))
	      (error "Checkbox blocked because of unchecked box in line %d"
		     blocked))
	    (replace-match
	     (cond ((equal toggle-presence '(16)) "[-]")
		   ((member (match-string 0) '("[ ]" "[-]")) "[X]")
		   (t "[ ]"))
	     t t)))
	(throw 'exit t))
       ((org-at-item-p)
	;; add a checkbox
	(save-excursion
	  (goto-char (match-end 0))
	  (insert "[ ] "))
	(throw 'exit t))
       (t (error "Not at a checkbox or heading, and no active region")))
      (setq end (move-marker (make-marker) end))
      (save-excursion
	(goto-char beg)
	(setq first-present (org-at-item-checkbox-p)
	      first-status
	      (save-excursion
		(and (re-search-forward "[ \t]\\(\\[[ X]\\]\\)" end t)
		     (equal (match-string 1) "[X]"))))
	(while (< (point) end)
	  (if toggle-presence
	      (cond
	       ((and first-present (org-at-item-checkbox-p))
		(save-excursion
		  (replace-match "")
		  (goto-char (match-beginning 0))
		  (just-one-space)))
	       ((and (not first-present) (not (org-at-item-checkbox-p))
		     (org-at-item-p))
		(save-excursion
		  (goto-char (match-end 0))
		  (insert "[ ] "))))
	    (when (org-at-item-checkbox-p)
	      (setq status (equal (match-string 0) "[X]"))
	      (replace-match
	       (if first-status "[ ]" "[X]") t t)))
	  (beginning-of-line 2)))))
  (org-update-checkbox-count-maybe))

(defun org-reset-checkbox-state-subtree ()
  "Reset all checkboxes in an entry subtree."
  (interactive "*")
  (save-restriction
    (save-excursion
      (org-narrow-to-subtree)
      (org-show-subtree)
      (goto-char (point-min))
      (let ((end (point-max)))
	(while (< (point) end)
	  (when (org-at-item-checkbox-p)
	    (replace-match "[ ]" t t))
	  (beginning-of-line 2))))
    (org-update-checkbox-count-maybe)))

(defun org-checkbox-blocked-p ()
  "Is the current checkbox blocked from for being checked now?
A checkbox is blocked if all of the following conditions are fulfilled:

1. The checkbox is not checked already.
2. The current entry has the ORDERED property set.
3. There is an unchecked checkbox in this entry before the current line."
  (catch 'exit
    (save-match-data
      (save-excursion
	(unless (org-at-item-checkbox-p) (throw 'exit nil))
	(when (equal (match-string 0) "[X]")
	  ;; the box is already checked!
	  (throw 'exit nil))
	(let ((end (point-at-bol)))
	  (condition-case nil (org-back-to-heading t)
	    (error (throw 'exit nil)))
	  (unless (org-entry-get nil "ORDERED") (throw 'exit nil))
	  (if (re-search-forward "^[ \t]*[-+*0-9.)] \\[[- ]\\]" end t)
	      (org-current-line)
	    nil))))))

(defvar org-checkbox-statistics-hook nil
  "Hook that is run whenever Org thinks checkbox statistics should be updated.
This hook runs even if `org-provide-checkbox-statistics' is nil, to it can
be used to implement alternative ways of collecting statistics information.")

(defun org-update-checkbox-count-maybe ()
  "Update checkbox statistics unless turned off by user."
  (when org-provide-checkbox-statistics
    (org-update-checkbox-count))
  (run-hooks 'org-checkbox-statistics-hook))

(defun org-update-checkbox-count (&optional all)
 "Update the checkbox statistics in the current section.
This will find all statistic cookies like [57%] and [6/12] and update them
with the current numbers.  With optional prefix argument ALL, do this for
the whole buffer."
 (interactive "P")
 (save-excursion
   (let* ((buffer-invisibility-spec (org-inhibit-invisibility)) ; Emacs 21
	  (beg (condition-case nil
		   (progn (org-back-to-heading) (point))
		 (error (point-min))))
	  (end (move-marker (make-marker)
			    (progn (outline-next-heading) (point))))
	  (re "\\(\\(\\[[0-9]*%\\]\\)\\|\\(\\[[0-9]*/[0-9]*\\]\\)\\)")
	  (re-box "^[ \t]*\\([-+*]\\|[0-9]+[.)]\\) +\\(\\[[- X]\\]\\)")
	  (re-find (concat re "\\|" re-box))
	  beg-cookie end-cookie is-percent c-on c-off lim new
	  eline curr-ind next-ind continue-from startsearch
	  (recursive
	   (or (not org-hierarchical-checkbox-statistics)
	       (string-match "\\<recursive\\>"
			     (or (ignore-errors
				   (org-entry-get nil "COOKIE_DATA"))
				 ""))))
	  (cstat 0)
	  )
     (when all
       (goto-char (point-min))
       (outline-next-heading)
       (setq beg (point) end (point-max)))
     (goto-char end)
     ;; find each statistics cookie
     (while (and (re-search-backward re-find beg t)
		 (not (save-match-data
			(and (org-on-heading-p)
			     (string-match "\\<todo\\>"
					   (downcase
					    (or (org-entry-get
						 nil "COOKIE_DATA")
						"")))))))
       (setq beg-cookie (match-beginning 1)
	     end-cookie (match-end 1)
	     cstat (+ cstat (if end-cookie 1 0))
	     startsearch (point-at-eol)
	     continue-from (match-beginning 0)
	     is-percent (match-beginning 2)
	     lim (cond
		  ((org-on-heading-p) (outline-next-heading) (point))
		  ((org-at-item-p) (org-end-of-item) (point))
		  (t nil))
	     c-on 0
	     c-off 0)
       (when lim
	 ;; find first checkbox for this cookie and gather
	 ;; statistics from all that are at this indentation level
	 (goto-char startsearch)
	 (if (re-search-forward re-box lim t)
	     (progn
	       (org-beginning-of-item)
	       (setq curr-ind (org-get-indentation))
	       (setq next-ind curr-ind)
	       (while (and (bolp) (org-at-item-p)
			   (if recursive
			       (<= curr-ind next-ind)
			     (= curr-ind next-ind)))
		 (save-excursion (end-of-line) (setq eline (point)))
		 (if (re-search-forward re-box eline t)
		     (if (member (match-string 2) '("[ ]" "[-]"))
			 (setq c-off (1+ c-off))
		       (setq c-on (1+ c-on))))
		 (if (not recursive)
		     (org-end-of-item)
		   (end-of-line)
		   (when (re-search-forward org-list-beginning-re lim t)
		     (beginning-of-line)))
		 (setq next-ind (org-get-indentation)))))
	 (goto-char continue-from)
	 ;; update cookie
	 (when end-cookie
	   (setq new (if is-percent
			 (format "[%d%%]" (/ (* 100 c-on) (max 1 (+ c-on c-off))))
		       (format "[%d/%d]" c-on (+ c-on c-off))))
	   (goto-char beg-cookie)
	   (insert new)
	   (delete-region (point) (+ (point) (- end-cookie beg-cookie))))
	 ;; update items checkbox if it has one
	 (when (org-at-item-p)
	   (org-beginning-of-item)
	   (when (and (> (+ c-on c-off) 0)
		      (re-search-forward re-box (point-at-eol) t))
	     (setq beg-cookie (match-beginning 2)
		   end-cookie (match-end       2))
	     (delete-region beg-cookie end-cookie)
	     (goto-char beg-cookie)
	     (cond ((= c-off 0) (insert "[X]"))
		   ((= c-on  0) (insert "[ ]"))
		   (t		(insert "[-]")))
	     )))
       (goto-char continue-from))
     (when (interactive-p)
       (message "Checkbox statistics updated %s (%d places)"
		(if all "in entire file" "in current outline entry") cstat)))))

(defun org-get-checkbox-statistics-face ()
  "Select the face for checkbox statistics.
The face will be `org-done' when all relevant boxes are checked.  Otherwise
it will be `org-todo'."
  (if (match-end 1)
      (if (equal (match-string 1) "100%")
	  'org-checkbox-statistics-done
	'org-checkbox-statistics-todo)
    (if (and (> (match-end 2) (match-beginning 2))
	     (equal (match-string 2) (match-string 3)))
	'org-checkbox-statistics-done
      'org-checkbox-statistics-todo)))

(defun org-beginning-of-item ()
  "Go to the beginning of the current hand-formatted item.
If the cursor is not in an item, throw an error."
  (interactive)
  (let ((pos (point))
	(limit (save-excursion
		 (condition-case nil
		     (progn
		       (org-back-to-heading)
		       (beginning-of-line 2) (point))
		   (error (point-min)))))
	(ind-empty (if org-empty-line-terminates-plain-lists 0 10000))
	ind ind1)
    (if (org-at-item-p)
	(beginning-of-line 1)
      (beginning-of-line 1)
      (skip-chars-forward " \t")
      (setq ind (current-column))
      (if (catch 'exit
	    (while t
	      (beginning-of-line 0)
	      (if (or (bobp) (< (point) limit)) (throw 'exit nil))

	      (if (looking-at "[ \t]*$")
		  (setq ind1 ind-empty)
		(skip-chars-forward " \t")
		(setq ind1 (current-column)))
	      (if (< ind1 ind)
		  (progn (beginning-of-line 1) (throw 'exit (org-at-item-p))))))
	  nil
	(goto-char pos)
	(error "Not in an item")))))

(defun org-end-of-item ()
  "Go to the end of the current hand-formatted item.
If the cursor is not in an item, throw an error."
  (interactive)
  (let* ((pos (point))
	 ind1
	 (ind-empty (if org-empty-line-terminates-plain-lists 0 10000))
	 (limit (save-excursion (outline-next-heading) (point)))
	 (ind (save-excursion
		(org-beginning-of-item)
		(skip-chars-forward " \t")
		(current-column)))
	 (end (catch 'exit
		(while t
		  (beginning-of-line 2)
		  (if (eobp) (throw 'exit (point)))
		  (if (>= (point) limit) (throw 'exit (point-at-bol)))
		  (if (looking-at "[ \t]*$")
		      (setq ind1 ind-empty)
		    (skip-chars-forward " \t")
		    (setq ind1 (current-column)))
		  (if (<= ind1 ind)
		      (throw 'exit (point-at-bol)))))))
    (if end
	(goto-char end)
      (goto-char pos)
      (error "Not in an item"))))

(defun org-next-item ()
  "Move to the beginning of the next item in the current plain list.
Error if not at a plain list, or if this is the last item in the list."
  (interactive)
  (let (ind ind1 (pos (point)))
    (org-beginning-of-item)
    (setq ind (org-get-indentation))
    (org-end-of-item)
    (setq ind1 (org-get-indentation))
    (unless (and (org-at-item-p) (= ind ind1))
      (goto-char pos)
      (error "On last item"))))

(defun org-previous-item ()
  "Move to the beginning of the previous item in the current plain list.
Error if not at a plain list, or if this is the first item in the list."
  (interactive)
  (let (beg ind ind1 (pos (point)))
    (org-beginning-of-item)
    (setq beg (point))
    (setq ind (org-get-indentation))
    (goto-char beg)
    (catch 'exit
      (while t
	(beginning-of-line 0)
	(if (looking-at "[ \t]*$")
	    nil
	  (if (<= (setq ind1 (org-get-indentation)) ind)
	      (throw 'exit t)))
	(if (bobp) (throw 'exit t))))
    (condition-case nil
	(if (or (not (org-at-item-p))
		(< ind1 (1- ind)))
	    (error "")
	  (org-beginning-of-item))
      (error (goto-char pos)
	     (error "On first item")))))

(defun org-first-list-item-p ()
  "Is this heading the first item in a plain list?"
  (unless (org-at-item-p)
    (error "Not at a plain list item"))
  (save-excursion
    (org-beginning-of-item)
    (= (point) (save-excursion (org-beginning-of-item-list)))))

(defun org-move-item-down ()
  "Move the plain list item at point down, i.e. swap with following item.
Subitems (items with larger indentation) are considered part of the item,
so this really moves item trees."
  (interactive)
  (let ((col (current-column))
	(pos (point))
	beg beg0 end end0 ind ind1  txt ne-end ne-beg)
    (org-beginning-of-item)
    (setq beg0 (point))
    (save-excursion
      (setq ne-beg (org-back-over-empty-lines))
      (setq beg (point)))
    (goto-char beg0)
    (setq ind (org-get-indentation))
    (org-end-of-item)
    (setq end0 (point))
    (setq ind1 (org-get-indentation))
    (setq ne-end (org-back-over-empty-lines))
    (setq end (point))
    (goto-char beg0)
    (when (and (org-first-list-item-p) (< ne-end ne-beg))
      ;; include less whitespace
      (save-excursion
	(goto-char beg)
	(forward-line (- ne-beg ne-end))
	(setq beg (point))))
    (goto-char end0)
    (if (and (org-at-item-p) (= ind ind1))
	(progn
	  (org-end-of-item)
	  (org-back-over-empty-lines)
	  (setq txt (buffer-substring beg end))
	  (save-excursion
	    (delete-region beg end))
	  (setq pos (point))
	  (insert txt)
	  (goto-char pos) (org-skip-whitespace)
	  (org-maybe-renumber-ordered-list)
	  (move-to-column col))
      (goto-char pos)
      (move-to-column col)
      (error "Cannot move this item further down"))))

(defun org-move-item-up (arg)
  "Move the plain list item at point up, i.e. swap with previous item.
Subitems (items with larger indentation) are considered part of the item,
so this really moves item trees."
  (interactive "p")
  (let ((col (current-column)) (pos (point))
	beg beg0 end ind ind1  txt
	ne-beg ne-ins ins-end)
    (org-beginning-of-item)
    (setq beg0 (point))
    (setq ind (org-get-indentation))
    (save-excursion
      (setq ne-beg (org-back-over-empty-lines))
      (setq beg (point)))
    (goto-char beg0)
    (org-end-of-item)
    (org-back-over-empty-lines)
    (setq end (point))
    (goto-char beg0)
    (catch 'exit
      (while t
	(beginning-of-line 0)
	(if (looking-at "[ \t]*$")
	    (if org-empty-line-terminates-plain-lists
		(progn
		  (goto-char pos)
		  (error "Cannot move this item further up"))
	      nil)
	  (if (<= (setq ind1 (org-get-indentation)) ind)
	      (throw 'exit t)))))
    (condition-case nil
	(org-beginning-of-item)
      (error (goto-char beg0)
	     (move-to-column col)
	     (error "Cannot move this item further up")))
    (setq ind1 (org-get-indentation))
    (if (and (org-at-item-p) (= ind ind1))
	(progn
	  (setq ne-ins (org-back-over-empty-lines))
	  (setq txt (buffer-substring beg end))
	  (save-excursion
	    (delete-region beg end))
	  (setq pos (point))
	  (insert txt)
	  (setq ins-end (point))
	  (goto-char pos) (org-skip-whitespace)

	  (when (and (org-first-list-item-p) (> ne-ins ne-beg))
	    ;; Move whitespace back to beginning
	    (save-excursion
	      (goto-char ins-end)
	      (let ((kill-whole-line t))
		(kill-line (- ne-ins ne-beg)) (point)))
	    (insert (make-string (- ne-ins ne-beg) ?\n)))

	  (org-maybe-renumber-ordered-list)
	  (move-to-column col))
      (goto-char pos)
      (move-to-column col)
      (error "Cannot move this item further up"))))

(defun org-maybe-renumber-ordered-list ()
  "Renumber the ordered list at point if setup allows it.
This tests the user option `org-auto-renumber-ordered-lists' before
doing the renumbering."
  (interactive)
  (when (and org-auto-renumber-ordered-lists
	     (org-at-item-p))
    (if (match-beginning 3)
	(org-renumber-ordered-list 1)
      (org-fix-bullet-type))))

(defun org-maybe-renumber-ordered-list-safe ()
  (condition-case nil
      (save-excursion
	(org-maybe-renumber-ordered-list))
    (error nil)))

(defun org-cycle-list-bullet (&optional which)
  "Cycle through the different itemize/enumerate bullets.
This cycle the entire list level through the sequence:

   `-'	->  `+'	 ->  `*'  ->  `1.'  ->	`1)'

If WHICH is a string, use that as the new bullet.  If WHICH is an integer,
0 means `-', 1 means `+' etc."
  (interactive "P")
  (org-preserve-lc
   (org-beginning-of-item-list)
   (org-at-item-p)
   (beginning-of-line 1)
   (let ((current (match-string 0))
	 (prevp (eq which 'previous))
	 new old)
     (setq new (cond
		((and (numberp which)
		      (nth (1- which) '("-" "+" "*" "1." "1)"))))
		((string-match "-" current) (if prevp "1)" "+"))
		((string-match "\\+" current)
		 (if prevp "-" (if (looking-at "\\S-") "1." "*")))
		((string-match "\\*" current) (if prevp "+" "1."))
		((string-match "\\." current)
		 (if prevp (if (looking-at "\\S-") "+" "*") "1)"))
		((string-match ")" current) (if prevp "1." "-"))
		(t (error "This should not happen"))))
     (and (looking-at "\\([ \t]*\\)\\(\\S-+\\)")
	  (setq old (match-string 2))
	  (replace-match (concat "\\1" new)))
     (org-shift-item-indentation (- (length new) (length old)))
     (org-fix-bullet-type)
     (org-maybe-renumber-ordered-list))))

(defun org-get-string-indentation (s)
  "What indentation has S due to SPACE and TAB at the beginning of the string?"
  (let ((n -1) (i 0) (w tab-width) c)
    (catch 'exit
      (while (< (setq n (1+ n)) (length s))
	(setq c (aref s n))
	(cond ((= c ?\ ) (setq i (1+ i)))
	      ((= c ?\t) (setq i (* (/ (+ w i) w) w)))
	      (t (throw 'exit t)))))
    i))

(defun org-renumber-ordered-list (arg)
  "Renumber an ordered plain list.
Cursor needs to be in the first line of an item, the line that starts
with something like \"1.\" or \"2)\"."
  (interactive "p")
  (unless (and (org-at-item-p)
	       (match-beginning 3))
    (error "This is not an ordered list"))
  (let ((line (org-current-line))
	(col (current-column))
	(ind (org-get-string-indentation
	      (buffer-substring (point-at-bol) (match-beginning 3))))
	;; (term (substring (match-string 3) -1))
	ind1 (n (1- arg))
	fmt bobp old new delta)
    ;; find where this list begins
    (org-beginning-of-item-list)
    (setq bobp (bobp))
    (looking-at "[ \t]*[0-9]+\\([.)]\\)")
    (setq fmt (concat "%d" (or (match-string 1) ".")))
    (beginning-of-line 0)
    ;; walk forward and replace these numbers
    (catch 'exit
      (while t
	(catch 'next
	  (if bobp (setq bobp nil) (beginning-of-line 2))
	  (if (eobp) (throw 'exit nil))
	  (if (looking-at "[ \t]*$") (throw 'next nil))
	  (skip-chars-forward " \t") (setq ind1 (current-column))
	  (if (> ind1 ind) (throw 'next t))
	  (if (< ind1 ind) (throw 'exit t))
	  (if (not (org-at-item-p)) (throw 'exit nil))
	  (setq old (match-string 2))
	  (delete-region (match-beginning 2) (match-end 2))
	  (goto-char (match-beginning 2))
	  (insert (setq new (format fmt (setq n (1+ n)))))
	  (setq delta (- (length new) (length old)))
	  (org-shift-item-indentation delta)
	  (if (= (org-current-line) line) (setq col (+ col delta))))))
    (org-goto-line line)
    (org-move-to-column col)))

(defvar org-suppress-item-indentation) ; dynamically scoped parameter
(defun org-fix-bullet-type (&optional force-bullet)
  "Make sure all items in this list have the same bullet as the first item.
Also, fix the indentation."
  (interactive)
  (unless (org-at-item-p) (error "This is not a list"))
  (let ((line (org-current-line))
	(chars-from-eol (- (point-at-eol) (point)))
	(ind (current-indentation))
	ind1 bullet oldbullet)
    ;; find where this list begins
    (org-beginning-of-item-list)
    (beginning-of-line 1)
    ;; find out what the bullet type is
    (looking-at "[ \t]*\\(\\S-+\\)")
    (setq bullet (concat (or force-bullet (match-string 1)) " "))
    (if (and org-list-two-spaces-after-bullet-regexp
	     (string-match org-list-two-spaces-after-bullet-regexp bullet))
	(setq bullet (concat bullet " ")))
    ;; walk forward and replace these numbers
    (beginning-of-line 0)
    (catch 'exit
      (while t
	(catch 'next
	  (beginning-of-line 2)
	  (if (eobp) (throw 'exit nil))
	  (if (looking-at "[ \t]*$") (throw 'next nil))
	  (skip-chars-forward " \t") (setq ind1 (current-column))
	  (if (> ind1 ind) (throw 'next t))
	  (if (< ind1 ind) (throw 'exit t))
	  (if (not (org-at-item-p)) (throw 'exit nil))
	  (skip-chars-forward " \t")
	  (looking-at "\\S-+ *")
	  (setq oldbullet (match-string 0))
	  (unless (equal bullet oldbullet) (replace-match bullet))
	  (org-shift-item-indentation (- (length bullet)
					 (length oldbullet))))))
    (org-goto-line line)
    (goto-char (max (point-at-bol) (- (point-at-eol) chars-from-eol)))
    (if (string-match "[0-9]" bullet)
	(org-renumber-ordered-list 1))))

(defun org-shift-item-indentation (delta)
  "Shift the indentation in current item by DELTA."
  (unless (org-bound-and-true-p org-suppress-item-indentation)
    (save-excursion
      (let ((beg (point-at-bol))
	    (end (progn (org-end-of-item) (point)))
	    i)
	(goto-char end)
	(beginning-of-line 0)
	(while (> (point) beg)
	  (when (looking-at "[ \t]*\\S-")
	    ;; this is not an empty line
	    (setq i (org-get-indentation))
	    (if (and (> i 0) (> (setq i (+ i delta)) 0))
		(indent-line-to i)))
	  (beginning-of-line 0))))))

(defun org-beginning-of-item-list ()
  "Go to the beginning of the current item list.
I.e. to the first item in this list."
  (interactive)
  (org-beginning-of-item)
  (let ((pos (point-at-bol))
	(ind (org-get-indentation))
	ind1)
    ;; find where this list begins
    (catch 'exit
      (while t
	(catch 'next
	  (beginning-of-line 0)
	  (if (looking-at "[ \t]*$")
	      (throw (if (bobp) 'exit 'next) t))
	  (skip-chars-forward " \t") (setq ind1 (current-column))
	  (if (or (< ind1 ind)
		  (and (= ind1 ind)
		       (not (org-at-item-p)))
		  (and (= (point-at-bol) (point-min))
		       (setq pos (point-min))))
	      (throw 'exit t)
	    (when (org-at-item-p) (setq pos (point-at-bol)))))))
    (goto-char pos)))

(defun org-end-of-item-list ()
  "Go to the end of the current item list.
I.e. to the text after the last item."
  (interactive)
  (org-beginning-of-item)
  (let ((pos (point-at-bol))
	(ind (org-get-indentation))
	ind1)
    ;; find where this list begins
    (catch 'exit
      (while t
	(catch 'next
	  (beginning-of-line 2)
	  (if (looking-at "[ \t]*$")
	      (if (eobp)
		  (progn (setq pos (point)) (throw 'exit t))
		(throw 'next t)))
	  (skip-chars-forward " \t") (setq ind1 (current-column))
	  (if (or (< ind1 ind)
		  (and (= ind1 ind)
		       (not (org-at-item-p)))
		  (eobp))
	      (progn
		(setq pos (point-at-bol))
		(throw 'exit t))))))
    (goto-char pos)))


(defvar org-last-indent-begin-marker (make-marker))
(defvar org-last-indent-end-marker (make-marker))

(defun org-outdent-item (arg)
  "Outdent a local list item."
  (interactive "p")
  (org-indent-item (- arg)))

(defun org-indent-item (arg)
  "Indent a local list item."
  (interactive "p")
  (and (org-region-active-p) (org-cursor-to-region-beginning))
  (unless (org-at-item-p)
    (error "Not on an item"))
  (let (beg end ind ind1 ind-bul delta ind-down ind-up firstp)
    (setq firstp (org-first-list-item-p))
    (save-excursion
      (setq end (and (org-region-active-p) (region-end)))
      (if (memq last-command '(org-shiftmetaright org-shiftmetaleft))
	  (setq beg org-last-indent-begin-marker
		end org-last-indent-end-marker)
	(org-beginning-of-item)
	(setq beg (move-marker org-last-indent-begin-marker (point)))
	(org-end-of-item)
	(setq end (move-marker org-last-indent-end-marker (or end (point)))))
      (goto-char beg)
      (setq ind-bul (org-item-indent-positions)
	    ind (caar ind-bul)
	    ind-down (car (nth 2 ind-bul))
	    ind-up (car (nth 1 ind-bul))
	    delta (if (> arg 0)
		      (if ind-down (- ind-down ind) 2)
		    (if ind-up (- ind-up ind) -2)))
      (if (< (+ delta ind) 0) (error "Cannot outdent beyond margin"))
      (while (< (point) end)
	(beginning-of-line 1)
	(skip-chars-forward " \t") (setq ind1 (current-column))
	(delete-region (point-at-bol) (point))
	(or (eolp) (org-indent-to-column (+ ind1 delta)))
	(beginning-of-line 2)))
    (org-fix-bullet-type
     (and (> arg 0)
	  (not firstp)
	  (cdr (assoc (cdr (nth 0 ind-bul)) org-list-demote-modify-bullet))))
    (org-maybe-renumber-ordered-list-safe)
    (save-excursion
      (beginning-of-line 0)
      (condition-case nil (org-beginning-of-item) (error nil))
      (org-maybe-renumber-ordered-list-safe))))

(defun org-item-indent-positions ()
  "Return indentation for plain list items.
This returns a list with three values:	The current indentation, the
parent indentation and the indentation a child should have.
Assumes cursor in item line."
  (let* ((bolpos (point-at-bol))
	 (ind (org-get-indentation))
	 (bullet (org-get-bullet))
	 ind-down ind-up bullet-up bullet-down pos)
    (save-excursion
      (org-beginning-of-item-list)
      (skip-chars-backward "\n\r \t")
      (when (org-in-item-p)
	(org-beginning-of-item)
	(setq ind-up (org-get-indentation))
	(setq bullet-up (org-get-bullet))))
    (setq pos (point))
    (save-excursion
      (cond
       ((and (condition-case nil (progn (org-previous-item) t)
	       (error nil))
	     (or (forward-char 1) t)
	     (re-search-forward "^\\([ \t]*\\([-+]\\|\\([0-9]+[.)]\\)\\)\\|[ \t]+\\*\\)\\( \\|$\\)" bolpos t))
	(setq ind-down (org-get-indentation)
	      bullet-down (org-get-bullet)))
       ((and (goto-char pos)
	     (org-at-item-p))
	(goto-char (match-end 0))
	(skip-chars-forward " \t")
	(setq ind-down (current-column)
	      bullet-down (org-get-bullet)))))
    (if (and bullet-down (string-match "\\`[0-9]+\\(\\.\\|)\\)\\'" bullet-down))
	(setq bullet-down (concat "1" (match-string 1 bullet-down))))
    (if (and bullet-up (string-match "\\`[0-9]+\\(\\.\\|)\\)\\'" bullet-up))
	(setq bullet-up (concat "1" (match-string 1 bullet-up))))
    (if (and bullet (string-match "\\`[0-9]+\\(\\.\\|)\\)\\'" bullet))
	(setq bullet (concat "1" (match-string 1 bullet))))
    (list (cons ind bullet)
	  (cons ind-up bullet-up)
	  (cons ind-down bullet-down))))

(defvar org-tab-ind-state) ; defined in org.el
(defun org-cycle-item-indentation ()
  (let ((org-suppress-item-indentation t)
	(org-adapt-indentation nil))
    (cond
     ((and (looking-at "[ \t]*$")
	   (org-looking-back "^\\([ \t]*\\)\\([-+*]\\|[0-9]+[).]\\)[ \t]+"))
      (setq this-command 'org-cycle-item-indentation)
      (if (eq last-command 'org-cycle-item-indentation)
	  (condition-case nil
	      (progn (org-outdent-item 1)
		     (if (equal org-tab-ind-state (org-get-indentation))
			 (org-outdent-item 1))
		     (end-of-line 1))
	    (error
	     (progn
	       (while (< (org-get-indentation) org-tab-ind-state)
		 (progn (org-indent-item 1) (end-of-line 1)))
	       (setq this-command 'org-cycle))))
	(setq org-tab-ind-state (org-get-indentation))
	(org-indent-item 1))
      t))))

(defun org-get-bullet ()
  (save-excursion
    (goto-char (point-at-bol))
    (and (looking-at
	  "^\\([ \t]*\\([-+]\\|\\([0-9]+[.)]\\)\\)\\|[ \t]+\\(\\*\\)\\)\\( \\|$\\)")
	 (or (match-string 2) (match-string 4)))))

;;; Send and receive lists

(defun org-list-parse-list (&optional delete)
  "Parse the list at point and maybe DELETE it.
Return a list containing first level items as strings and
sublevels as a list of strings."
  (let* ((item-beginning (org-list-item-beginning))
	 (start (car item-beginning))
	 (end (save-excursion
		(goto-char (org-list-end (cdr item-beginning)))
		(org-back-over-empty-lines)
		(point)))
	 output itemsep ltype)
    (while (re-search-forward org-list-beginning-re end t)
      (goto-char (match-beginning 3))
      (save-match-data
	(cond ((string-match "[0-9]" (match-string 2))
	       (setq itemsep "[0-9]+\\(?:\\.\\|)\\)"
		     ltype 'ordered))
	      ((string-match "^.*::" (match-string 0))
	       (setq itemsep "[-+]" ltype 'descriptive))
	      (t (setq itemsep "[-+]" ltype 'unordered))))
      (let* ((indent1 (match-string 1))
	     (nextitem (save-excursion
			 (save-match-data
			   (or (and (re-search-forward
				     (concat "^" indent1 itemsep " *?") end t)
				    (match-beginning 0)) end))))
	     (item (buffer-substring
		    (point)
		    (or (and (re-search-forward
			      org-list-beginning-re end t)
			     (goto-char (match-beginning 0)))
			(goto-char end))))
	     (nextindent (match-string 1))
	     (item (org-trim item))
	     (item (if (string-match "^\\[\\([xX ]\\)\\]" item)
		       (replace-match (if (equal (match-string 1 item) " ")
					  "[CBOFF]"
					"[CBON]")
				      t nil item)
		     item)))
	(push item output)
	(when (> (length nextindent)
		 (length indent1))
	  (narrow-to-region (point) nextitem)
	  (push (org-list-parse-list) output)
	  (widen))))
    (when delete (delete-region start end))
    (setq output (nreverse output))
    (push ltype output)))

(defun org-list-item-beginning ()
  "Find the beginning of the list item.
Return a cons which car is the beginning position of the item and
cdr is the indentation string."
  (save-excursion
    (if (not (or (looking-at org-list-beginning-re)
		 (re-search-backward
		  org-list-beginning-re nil t)))
	(progn (goto-char (point-min)) (point))
      (cons (match-beginning 0) (match-string 1)))))

(defun org-list-goto-true-beginning ()
  "Go to the beginning of the list at point."
  (beginning-of-line 1)
  (while (looking-at org-list-beginning-re)
    (beginning-of-line 0))
  (progn
    (re-search-forward org-list-beginning-re nil t)
    (goto-char (match-beginning 0))))

(defun org-list-make-subtree ()
  "Convert the plain list at point into a subtree."
  (interactive)
  (org-list-goto-true-beginning)
  (let ((list (org-list-parse-list t)) nstars)
    (save-excursion
      (if (condition-case nil
	      (org-back-to-heading)
	    (error nil))
	  (progn (re-search-forward org-complex-heading-regexp nil t)
		 (setq nstars (length (match-string 1))))
	(setq nstars 0)))
    (org-list-make-subtrees list (1+ nstars))))

(defun org-list-make-subtrees (list level)
  "Convert LIST into subtrees starting at LEVEL."
  (if (symbolp (car list))
      (org-list-make-subtrees (cdr list) level)
    (mapcar (lambda (item)
	      (if (stringp item)
		  (insert (make-string
			   (if org-odd-levels-only
			       (1- (* 2 level)) level) ?*) " " item "\n")
		(org-list-make-subtrees item (1+ level))))
	    list)))

(defun org-list-end (indent)
  "Return the position of the end of the list.
INDENT is the indentation of the list, as a string."
  (save-excursion
    (catch 'exit
      (while (or (looking-at org-list-beginning-re)
		 (looking-at (concat "^" indent "[ \t]+\\|^$"))
		 (> (or (get-text-property (point) 'original-indentation) -1)
		     (length indent)))
	(if (eq (point) (point-max))
	    (throw 'exit (point-max)))
	(forward-line 1)))
    (point)))

(defun org-list-insert-radio-list ()
  "Insert a radio list template appropriate for this major mode."
  (interactive)
  (let* ((e (assq major-mode org-list-radio-list-templates))
	 (txt (nth 1 e))
	 name pos)
    (unless e (error "No radio list setup defined for %s" major-mode))
    (setq name (read-string "List name: "))
    (while (string-match "%n" txt)
      (setq txt (replace-match name t t txt)))
    (or (bolp) (insert "\n"))
    (setq pos (point))
    (insert txt)
    (goto-char pos)))

(defun org-list-send-list (&optional maybe)
  "Send a transformed version of this list to the receiver position.
With argument MAYBE, fail quietly if no transformation is defined for
this list."
  (interactive)
  (catch 'exit
    (unless (org-at-item-p) (error "Not at a list"))
    (save-excursion
      (org-list-goto-true-beginning)
      (beginning-of-line 0)
      (unless (looking-at "#\\+ORGLST: *SEND +\\([a-zA-Z0-9_]+\\) +\\([^ \t\r\n]+\\)\\( +.*\\)?")
	(if maybe
	    (throw 'exit nil)
	  (error "Don't know how to transform this list"))))
    (let* ((name (match-string 1))
	   (transform (intern (match-string 2)))
	   (item-beginning (org-list-item-beginning))
	   (txt (buffer-substring-no-properties
		 (car item-beginning)
		 (org-list-end (cdr item-beginning))))
	   (list (org-list-parse-list))
	   beg)
      (unless (fboundp transform)
	(error "No such transformation function %s" transform))
      (setq txt (funcall transform list))
      ;; Find the insertion place
      (save-excursion
	(goto-char (point-min))
	(unless (re-search-forward
		 (concat "BEGIN RECEIVE ORGLST +" name "\\([ \t]\\|$\\)") nil t)
	  (error "Don't know where to insert translated list"))
	(goto-char (match-beginning 0))
	(beginning-of-line 2)
	(setq beg (point))
	(unless (re-search-forward (concat "END RECEIVE ORGLST +" name) nil t)
	  (error "Cannot find end of insertion region"))
	(beginning-of-line 1)
	(delete-region beg (point))
	(goto-char beg)
	(insert txt "\n"))
      (message "List converted and installed at receiver location"))))

(defun org-list-to-generic (list params)
  "Convert a LIST parsed through `org-list-parse-list' to other formats.

Valid parameters PARAMS are

:ustart	    String to start an unordered list
:uend	    String to end an unordered list

:ostart	    String to start an ordered list
:oend	    String to end an ordered list

:dstart	    String to start a descriptive list
:dend	    String to end a descriptive list
:dtstart    String to start a descriptive term
:dtend	    String to end a descriptive term
:ddstart    String to start a description
:ddend	    String to end a description

:splice	    When set to t, return only list body lines, don't wrap
	    them into :[u/o]start and :[u/o]end.  Default is nil.

:istart	    String to start a list item
:iend	    String to end a list item
:isep	    String to separate items
:lsep	    String to separate sublists

:cboff      String to insert for an unchecked checkbox
:cbon       String to insert for a checked checkbox"
  (interactive)
  (let* ((p params) sublist
	 (splicep (plist-get p :splice))
	 (ostart  (plist-get p :ostart))
	 (oend	(plist-get p :oend))
	 (ustart  (plist-get p :ustart))
	 (uend	(plist-get p :uend))
	 (dstart  (plist-get p :dstart))
	 (dend	(plist-get p :dend))
	 (dtstart  (plist-get p :dtstart))
	 (dtend	 (plist-get p :dtend))
	 (ddstart  (plist-get p :ddstart))
	 (ddend	 (plist-get p :ddend))
	 (istart  (plist-get p :istart))
	 (iend	(plist-get p :iend))
	 (isep	(plist-get p :isep))
	 (lsep	(plist-get p :lsep))
	 (cbon	(plist-get p :cbon))
	 (cboff (plist-get p :cboff)))
    (let ((wrapper
	   (cond ((eq (car list) 'ordered)
		  (concat ostart "\n%s" oend "\n"))
		 ((eq (car list) 'unordered)
		  (concat ustart "\n%s" uend "\n"))
		 ((eq (car list) 'descriptive)
		  (concat dstart "\n%s" dend "\n"))))
	  rtn term defstart defend)
      (while (setq sublist (pop list))
	(cond ((symbolp sublist) nil)
	      ((stringp sublist)
	       (when (string-match "^\\(.*\\) ::" sublist)
		 (setq term (org-trim (format (concat dtstart "%s" dtend)
					      (match-string 1 sublist))))
		 (setq sublist (substring sublist (1+ (length term)))))
	       (if (string-match "\\[CBON\\]" sublist)
		   (setq sublist (replace-match cbon t t sublist)))
	       (if (string-match "\\[CBOFF\\]" sublist)
		   (setq sublist (replace-match cboff t t sublist)))
	       (if (string-match "\\[-\\]" sublist)
		   (setq sublist (replace-match "$\\boxminus$" t t sublist)))
	       (setq rtn (concat rtn istart term ddstart
				 sublist ddend iend isep)))
	      (t (setq rtn (concat rtn	 ;; previous list
				   lsep	 ;; list separator
				   (org-list-to-generic sublist p)
				   lsep	 ;; list separator
				   )))))
      (format wrapper rtn))))

(defun org-list-to-latex (list &optional params)
  "Convert LIST into a LaTeX list.
LIST is as returnd by `org-list-parse-list'.  PARAMS is a property list
with overruling parameters for `org-list-to-generic'."
  (org-list-to-generic
   list
   (org-combine-plists
    '(:splicep nil :ostart "\\begin{enumerate}" :oend "\\end{enumerate}"
	       :ustart "\\begin{itemize}" :uend "\\end{itemize}"
	       :dstart "\\begin{description}" :dend "\\end{description}"
	       :dtstart "[" :dtend "]"
	       :ddstart "" :ddend ""
	       :istart "\\item " :iend ""
	       :isep "\n" :lsep "\n"
	       :cbon "\\texttt{[X]}" :cboff "\\texttt{[ ]}")
    params)))

(defun org-list-to-html (list &optional params)
  "Convert LIST into a HTML list.
LIST is as returnd by `org-list-parse-list'.  PARAMS is a property list
with overruling parameters for `org-list-to-generic'."
  (org-list-to-generic
   list
   (org-combine-plists
    '(:splicep nil :ostart "<ol>" :oend "</ol>"
	       :ustart "<ul>" :uend "</ul>"
	       :dstart "<dl>" :dend "</dl>"
	       :dtstart "<dt>" :dtend "</dt>"
	       :ddstart "<dd>" :ddend "</dd>"
	       :istart "<li>" :iend "</li>"
	       :isep "\n" :lsep "\n"
	       :cbon "<code>[X]</code>" :cboff "<code>[ ]</code>")
    params)))

(defun org-list-to-texinfo (list &optional params)
  "Convert LIST into a Texinfo list.
LIST is as returnd by `org-list-parse-list'.  PARAMS is a property list
with overruling parameters for `org-list-to-generic'."
  (org-list-to-generic
   list
   (org-combine-plists
    '(:splicep nil :ostart "@itemize @minus" :oend "@end itemize"
	       :ustart "@enumerate" :uend "@end enumerate"
	       :dstart "@table" :dend "@end table"
	       :dtstart "@item " :dtend "\n"
	       :ddstart "" :ddend ""
	       :istart "@item\n" :iend ""
	       :isep "\n" :lsep "\n"
	       :cbon "@code{[X]}" :cboff "@code{[ ]}")
    params)))

(provide 'org-list)

;; arch-tag: 73cf50c1-200f-4d1d-8a53-4e842a5b11c8
;;; org-list.el ends here
