;;; org-inlinetask.el --- Tasks independent of outline hierarchy

;; Copyright (C) 2009, 2010 Free Software Foundation, Inc.
;;
;; Author: Carsten Dominik <carsten at orgmode dot org>
;; Keywords: outlines, hypermedia, calendar, wp
;; Homepage: http://orgmode.org
;; Version: 7.4

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
;; Visibility cycling exempts these nodes from cycling.  So whenever their
;; parent is opened, so are these tasks.  This will only work with
;; `org-cycle', so if you are also using other commands to show/hide
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
;; This package installs one new command:
;;
;; C-c C-x t      Insert a new inline task with END line

;;; Code:

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
  :type '(choice
	  (const :tag "Off" nil)
	  (integer)))

(defcustom org-inlinetask-export t
  "Non-nil means export inline tasks.
When nil, they will not be exported."
  :group 'org-inlinetask
  :type 'boolean)

(defvar org-inlinetask-export-templates
  '((html "<pre class=\"inlinetask\"><b>%s%s</b><br>%s</pre>"
	  '((unless (eq todo "")
	      (format "<span class=\"%s %s\">%s%s</span> "
		      class todo todo priority))
	    heading content))
    (latex "\\begin\{description\}\\item[%s%s]%s\\end\{description\}"
	   '((unless (eq todo "") (format "\\textsc\{%s%s\} " todo priority))
	     heading content))
    (ascii "     -- %s%s%s"
	   '((unless (eq todo "") (format "%s%s " todo priority))
	     heading
	     (unless (eq content "")
	       (format "\n         ¦ %s"
		       (mapconcat 'identity (org-split-string content "\n")
				  "\n         ¦ ")))))
    (docbook "<variablelist>
<varlistentry>
<term>%s%s</term>
<listitem><para>%s</para></listitem>
</varlistentry>
</variablelist>"
	     '((unless (eq todo "") (format "%s%s " todo priority))
	       heading content)))
  "Templates for inline tasks in various exporters.

This variable is an alist in the shape of (BACKEND STRING OBJECTS).

BACKEND is the name of the backend for the template (ascii, html...).

STRING is a format control string.

OBJECTS is a list of elements to be substituted into the format
string.  They can be of any type, from a string to a form
returning a value (thus allowing conditional insertion).  A nil
object will be substituted as the empty string.  Obviously, there
must be at least as many objects as %-sequences in the format
string.

Moreover, the following special keywords are provided: `todo',
`priority', `heading', `content', `tags'.  If some of them are not
defined in an inline task, their value is the empty string.

As an example, valid associations are:

(html \"<ul><li>%s <p>%s</p></li></ul>\" (heading content))

or, with the additional package \"todonotes\" for LaTeX,

(latex \"\\todo[inline]{\\textbf{\\textsf{%s %s}}\\linebreak{} %s}\"
       '((unless (eq todo \"\")
	   (format \"\\textsc{%s%s}\" todo priority))
	 heading content)))")

(defvar org-odd-levels-only)
(defvar org-keyword-time-regexp)
(defvar org-drawer-regexp)
(defvar org-complex-heading-regexp)
(defvar org-property-end-re)

(defcustom org-inlinetask-default-state nil
  "Non-nil means make inline tasks have a TODO keyword initially.
This should be the state `org-inlinetask-insert-task' should use by
default, or nil of no state should be assigned."
  :group 'org-inlinetask
  :type '(choice
	  (const :tag "No state" nil)
	  (string :tag "Specific state")))

(defun org-inlinetask-insert-task (&optional no-state)
  "Insert an inline task.
If prefix arg NO-STATE is set, ignore `org-inlinetask-default-state'."
  (interactive "P")
  (or (bolp) (newline))
  (let ((indent org-inlinetask-min-level))
    (if org-odd-levels-only
        (setq indent (- (* 2 indent) 1)))
    (insert (make-string indent ?*)
            (if (or no-state (not org-inlinetask-default-state))
		" \n"
	      (concat " " org-inlinetask-default-state " \n"))
            (make-string indent ?*) " END\n"))
  (end-of-line -1))
(define-key org-mode-map "\C-c\C-xt" 'org-inlinetask-insert-task)

(defun org-inlinetask-outline-regexp ()
  "Return string matching an inline task heading.
The number of levels is controlled by `org-inlinetask-min-level'."
  (let ((nstars (if org-odd-levels-only
		    (1- (* org-inlinetask-min-level 2))
		  org-inlinetask-min-level)))
    (format "^\\(\\*\\{%d,\\}\\)[ \t]+" nstars)))

(defun org-inlinetask-in-task-p ()
  "Return true if point is inside an inline task."
  (save-excursion
    (let* ((stars-re (org-inlinetask-outline-regexp))
	   (task-beg-re (concat stars-re "\\(?:.*\\)"))
	   (task-end-re (concat stars-re "\\(?:END\\|end\\)[ \t]*$")))
      (beginning-of-line)
      (or (looking-at task-beg-re)
	  (and (re-search-forward "^\\*+[ \t]+" nil t)
	       (progn (beginning-of-line) (looking-at task-end-re)))))))

(defun org-inlinetask-goto-beginning ()
  "Go to the beginning of the inline task at point."
  (end-of-line)
  (re-search-backward (org-inlinetask-outline-regexp) nil t)
  (when (org-looking-at-p (concat (org-inlinetask-outline-regexp) "END[ \t]*$"))
    (re-search-backward (org-inlinetask-outline-regexp) nil t)))

(defun org-inlinetask-goto-end ()
  "Go to the end of the inline task at point."
  (beginning-of-line)
  (cond
   ((org-looking-at-p (concat (org-inlinetask-outline-regexp) "END[ \t]*$"))
    (forward-line 1))
   ((org-looking-at-p (org-inlinetask-outline-regexp))
    (forward-line 1)
    (when (org-inlinetask-in-task-p)
      (re-search-forward (org-inlinetask-outline-regexp) nil t)
      (forward-line 1)))
   (t
    (re-search-forward (org-inlinetask-outline-regexp) nil t)
    (forward-line 1))))

(defun org-inlinetask-get-task-level ()
  "Get the level of the inline task around.
This assumes the point is inside an inline task."
  (save-excursion
    (end-of-line)
    (re-search-backward (org-inlinetask-outline-regexp) nil t)
    (- (match-end 1) (match-beginning 1))))

(defvar backend) ; dynamically scoped into the next function
(defun org-inlinetask-export-handler ()
  "Handle headlines with level larger or equal to `org-inlinetask-min-level'.
Either remove headline and meta data, or do special formatting."
  (goto-char (point-min))
  (let* ((nstars (if org-odd-levels-only
		     (1- (* 2 (or org-inlinetask-min-level 200)))
		   (or org-inlinetask-min-level 200)))
	 (re1 (format "^\\(\\*\\{%d,\\}\\) .*\n" nstars))
	 (re2 (concat "^[ \t]*" org-keyword-time-regexp))
	 headline beg end stars content)
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
      (when org-inlinetask-export
	;; content formatting
	(when content
	    (if (not (string-match "\\S-" content))
		(setq content nil)
	      (if (string-match "[ \t\n]+\\'" content)
		  (setq content (substring content 0 (match-beginning 0))))
	      (setq content (org-remove-indentation content))))
	(setq content (or content ""))
	;; grab elements to export
	(when (string-match org-complex-heading-regexp headline)
	  (let* ((todo (or (match-string 2 headline) ""))
		 (class (or (and (eq "" todo) "")
			    (if (member todo org-done-keywords) "done" "todo")))
		 (priority (or (match-string 3 headline) ""))
		 (heading (or (match-string 4 headline) ""))
		 (tags (or (match-string 5 headline) ""))
		 (backend-spec (assq backend org-inlinetask-export-templates))
		 (format-str (nth 1 backend-spec))
		 (tokens (cadr (nth 2 backend-spec)))
		 ;; change nil arguments into empty strings
		 (nil-to-str (lambda (el) (or (eval el) "")))
		 ;; build and protect export string
		 (export-str (org-add-props
				 (eval (append '(format format-str)
					       (mapcar nil-to-str tokens)))
				 nil 'org-protected t)))
	    ;; eventually insert it
	    (insert export-str "\n")))))))

(defun org-inlinetask-get-current-indentation ()
  "Get the indentation of the last non-while line above this one."
  (save-excursion
    (beginning-of-line 1)
    (skip-chars-backward " \t\n")
    (beginning-of-line 1)
    (or (org-at-item-p)
	(looking-at "[ \t]*"))
    (goto-char (match-end 0))
    (current-column)))

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
