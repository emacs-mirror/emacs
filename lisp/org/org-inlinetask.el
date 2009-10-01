;;; org-inlinetask.el --- Tasks independent of outline hierarchy

;; Copyright (C) 2009 Free Software Foundation, Inc.
;;
;; Author: Carsten Dominik <carsten at orgmode dot org>
;; Keywords: outlines, hypermedia, calendar, wp
;; Homepage: http://orgmode.org
;; Version: 6.31a

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
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; This module implements inline tasks in Org-mode.  Inline tasks are
;; tasks that have all the properties of normal outline nodes, including
;; the ability to store meta data like scheduling dates, TODO state, tags
;; and properties.  However, these nodes are treated specially by the
;; visibility cycling and export commands.
;;
;; Visibility cycling exempts these nodes from cycling. So whenever their
;; parent is opened, so are these tasks.  This will only work with
;; `org-cycle', so if you are also using orther commands to show/hide
;; entries, you will occasionally find these tasks to behave like
;; all other outline nodes, seemingly splitting the text of the parent
;; into children.
;;
;; Export commands do not treat these nodes as part of the sectioning
;; structure, but as a special inline text that is either removed, or
;; formatted in some special way.
;;
;; Special fontification of inline tasks, so that they can be immediately
;; recognized.  From the stars of the headline, only the first and the
;; last two will be visible, the others will be hidden using the
;; `org-hide' face.
;;
;; An inline task is identified solely by a minimum outline level, given
;; by the variable `org-inlinetask-min-level', default 15.
;;
;; Inline tasks are normally assumed to contain at most a time planning
;; line (DEADLINE etc) after it, and then any number of drawers, for
;; example LOGBOOK of PROPERTIES.  No empty lines are allowed.
;; If you need to have normal text as part of an inline task, you
;; can do so by adding an "END" headline with the same number of stars,
;; for example
;;
;;    **************** TODO some small task
;;                     DEADLINE: <2009-03-30 Mon>
;;                     :PROPERTIES:
;;                       :SOMETHING: or other
;;                     :END:
;;                     And here is some extra text
;;    **************** END
;;
;; Also, if you want to use refiling and archiving for inline tasks,
;; The END line must be present to make things work properly.
;;
;; This package installs one new comand:
;;
;; C-c C-x t      Insert a new inline task with END line


;;; Code

(require 'org)

(defgroup org-inlinetask nil
  "Options concerning inline tasks in Org mode."
  :tag "Org Inline Tasks"
  :group 'org-structure)

(defcustom org-inlinetask-min-level 15
  "Minimum level a headline must have before it is treated as an inline task.
It is strongly recommended that you set `org-cycle-max-level' not at all,
or to a number smaller than this one.  In fact, when `org-cycle-max-level' is
not set, it will be assumed to be one less than the value of smaller than
the value of this variable."
  :group 'org-inlinetask
  :type 'boolean)

(defvar org-odd-levels-only)
(defvar org-keyword-time-regexp)
(defvar org-drawer-regexp)
(defvar org-complex-heading-regexp)
(defvar org-property-end-re)

(defun org-inlinetask-insert-task ()
  "Insert an inline task."
  (interactive)
  (or (bolp) (newline))
  (insert (make-string org-inlinetask-min-level ?*) " \n"
	  (make-string org-inlinetask-min-level ?*) " END\n")
  (end-of-line -1))
(define-key org-mode-map "\C-c\C-xt" 'org-inlinetask-insert-task)

(defvar htmlp)  ; dynamically scoped into the next function
(defvar latexp) ; dynamically scoped into the next function
(defun org-inlinetask-export-handler ()
  "Handle headlines with level larger or equal to `org-inlinetask-min-level'.
Either remove headline and meta data, or do special formatting."
  (goto-char (point-min))
  (let* ((nstars (if org-odd-levels-only
		     (1- (* 2 (or org-inlinetask-min-level 200)))
		   (or org-inlinetask-min-level 200)))
	 (re1 (format "^\\(\\*\\{%d,\\}\\) .*\n" nstars))
	 (re2 (concat "^[ \t]*" org-keyword-time-regexp))
	 headline beg end stars content indent)
    (while (re-search-forward re1 nil t)
      (setq headline (match-string 0)
	    stars (match-string 1)
	    content nil)
      (replace-match "")
      (while (looking-at re2)
	(delete-region (point) (1+ (point-at-eol))))
      (while (looking-at org-drawer-regexp)
	(setq beg (point))
	(if (re-search-forward org-property-end-re nil t)
	    (delete-region beg (1+ (match-end 0)))))
      (setq beg (point))
      (when (and (re-search-forward "^\\(\\*+\\) " nil t)
		 (= (length (match-string 1)) (length stars))
		 (progn (goto-char (match-end 0))
			(looking-at "END[ \t]*$")))
	(setq content (buffer-substring beg (1- (point-at-bol))))
	(delete-region beg (1+ (match-end 0))))
      (goto-char beg)
      (when (string-match org-complex-heading-regexp headline)
	(setq headline (concat
			(if (match-end 2)
			    (concat (match-string 2 headline) " ") "")
			(match-string 4 headline)))
	(if (not (string-match "\\S-" content))
	    (setq content nil)
	  (if (string-match "[ \t\n]+\\'" content)
	      (setq content (substring content 0 (match-beginning 0))))
	  (setq content (org-remove-indentation content))
	  (if latexp (setq content (concat "\\quad \\\\\n" content))))
	(insert "- ")
	(setq indent (make-string (current-column) ?\ ))
	(insert headline " ::")
	(when content
	  (insert (if htmlp " " (concat "\n" indent))
		  (mapconcat 'identity (org-split-string content "\n")
			     (concat "\n" indent)) "\n"))))))

(defun org-inlinetask-fontify (limit)
  "Fontify the inline tasks."
  (let* ((nstars (if org-odd-levels-only
		     (1- (* 2 (or org-inlinetask-min-level 200)))
		   (or org-inlinetask-min-level 200)))
	 (re (concat "^\\(\\*\\)\\(\\*\\{"
		    (format "%d" (- nstars 3))
		    ",\\}\\)\\(\\*\\* .*\\)")))
    (while (re-search-forward re limit t)
      (add-text-properties (match-beginning 1) (match-end 1)
			   '(face org-warning font-lock-fontified t))
      (add-text-properties (match-beginning 2) (match-end 2)
			   '(face org-hide font-lock-fontified t))
      (add-text-properties (match-beginning 3) (match-end 3)
			   '(face shadow font-lock-fontified t)))))

(defun org-inlinetask-remove-END-maybe ()
  "Remove an END line when present."
  (when (looking-at (format "\\([ \t]*\n\\)*\\*\\{%d,\\}[ \t]+END[ \t]*$"
			    org-inlinetask-min-level))
    (replace-match "")))

(eval-after-load "org-exp"
  '(add-hook 'org-export-preprocess-after-tree-selection-hook
	     'org-inlinetask-export-handler))
(eval-after-load "org"
  '(add-hook 'org-font-lock-hook 'org-inlinetask-fontify))

(provide 'org-inlinetask)

;;; org-inlinetask.el ends here
