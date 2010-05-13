;;; org-footnote.el --- Footnote support in Org and elsewhere
;;
;; Copyright (C) 2009, 2010 Free Software Foundation, Inc.
;;
;; Author: Carsten Dominik <carsten at orgmode dot org>
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

;; This file contains the code dealing with footnotes in Org-mode.
;; The code can also be used in arbitrary text modes to provide
;; footnotes.  Compared to Steven L Baur's footnote.el it provides
;; better support for resuming editing.  It is less configurable than
;; Steve's code, though.

;;; Code:

(eval-when-compile
  (require 'cl))
(require 'org-macs)
(require 'org-compat)

(declare-function org-in-commented-line "org" ())
(declare-function org-in-regexp "org" (re &optional nlines visually))
(declare-function org-mark-ring-push "org" (&optional pos buffer))
(declare-function outline-next-heading "outline")
(declare-function org-trim "org" (s))
(declare-function org-show-context "org" (&optional key))
(declare-function org-back-to-heading "org" (&optional invisible-ok))
(declare-function org-end-of-subtree "org"  (&optional invisible-ok to-heading))
(declare-function org-in-verbatim-emphasis "org" ())
(declare-function org-inside-latex-macro-p "org" ())
(defvar org-odd-levels-only) ;; defined in org.el

(defconst org-footnote-re
  (concat "[^][\n]"   ; to make sure it is not at the beginning of a line
	  "\\["
	  "\\(?:"
	  "\\([0-9]+\\)"
	  "\\|"
	  (org-re "\\(fn:\\([-_[:word:]]+?\\)?\\)\\(?::\\([^\]]*?\\)\\)?")
	  "\\)"
	  "\\]")
  "Regular expression for matching footnotes.")

(defconst org-footnote-definition-re
  (org-re "^\\(\\[\\([0-9]+\\|fn:[-_[:word:]]+\\)\\]\\)")
  "Regular expression matching the definition of a footnote.")

(defcustom org-footnote-section "Footnotes"
  "Outline heading containing footnote definitions before export.
This can be nil, to place footnotes locally at the end of the current
outline node.  If can also be the name of a special outline heading
under which footnotes should be put.
This variable defines the place where Org puts the definition
automatically, i.e. when creating the footnote, and when sorting the notes.
However, by hand you may place definitions *anywhere*.
If this is a string, during export, all subtrees starting with this
heading will be removed after extracting footnote definitions."
  :group 'org-footnotes
  :type '(choice
	  (string :tag "Collect footnotes under heading")
	  (const :tag "Define footnotes locally" nil)))

(defcustom org-footnote-tag-for-non-org-mode-files "Footnotes:"
  "Tag marking the beginning of footnote section.
The Org-mode footnote engine can be used in arbitrary text files as well
as in Org-mode.  Outside Org-mode, new footnotes are always placed at
the end of the file.  When you normalize the notes, any line containing
only this tag will be removed, a new one will be inserted at the end
of the file, followed by the collected and normalized footnotes."
  :group 'org-footnotes
  :type 'string)

(defcustom org-footnote-define-inline nil
  "Non-nil means define footnotes inline, at reference location.
When nil, footnotes will be defined in a special section near
the end of the document.  When t, the [fn:label:definition] notation
will be used to define the footnote at the reference position."
  :group 'org-footnote
  :type 'boolean)

(defcustom org-footnote-auto-label t
  "Non-nil means define automatically new labels for footnotes.
Possible values are:

nil        prompt the user for each label
t          create unique labels of the form [fn:1], [fn:2], ...
confirm    like t, but let the user edit the created value.  In particular,
           the label can be removed from the minibuffer, to create
           an anonymous footnote.
plain      Automatically create plain number labels like [1]"
  :group 'org-footnote
  :type '(choice
	  (const :tag "Prompt for label" nil)
	  (const :tag "Create automatic [fn:N]" t)
	  (const :tag "Offer automatic [fn:N] for editing" confirm)
	  (const :tag "Create automatic [N]" plain)))

(defcustom org-footnote-auto-adjust nil
  "Non-nil means automatically adjust footnotes after insert/delete.
When this is t, after each insertion or deletion of a footnote,
simple fn:N footnotes will be renumbered, and all footnotes will be sorted.
If you want to have just sorting or just renumbering, set this variable
to `sort' or `renumber'.

The main values of this variable can be set with in-buffer options:

#+STARTUP: fnadjust
#+STARTUP: nofnadjust"
  :group 'org-footnote
  :type '(choice
	  (const :tag "Renumber" renumber)
	  (const :tag "Sort" sort)
	  (const :tag "Renumber and Sort" t)))

(defcustom org-footnote-fill-after-inline-note-extraction nil
  "Non-nil means fill paragraphs after extracting footnotes.
When extracting inline footnotes, the lengths of lines can change a lot.
When this option is set, paragraphs from which an inline footnote has been
extracted will be filled again."
  :group 'org-footnote
  :type 'boolean)

(defun org-footnote-at-reference-p ()
  "Is the cursor at a footnote reference?
If yes, return the beginning position, the label, and the definition, if local."
  (when (org-in-regexp org-footnote-re 15)
    (list (match-beginning 0)
	  (or (match-string 1)
	      (if (equal (match-string 2) "fn:") nil (match-string 2)))
	  (match-string 4))))

(defun org-footnote-at-definition-p ()
  "Is the cursor at a footnote definition.
This matches only pure definitions like [1] or [fn:name] at the beginning
of a line.  It does not a references like [fn:name:definition], where the
footnote text is included and defined locally.
The return value will be nil if not at a footnote definition, and a list
with start and label of the footnote if there is a definition at point."
  (save-excursion
    (end-of-line 1)
    (let ((lim (save-excursion (re-search-backward "^\\*+ \\|^[ \t]*$" nil t))))
      (when (re-search-backward org-footnote-definition-re lim t)
	(list (match-beginning 0) (match-string 2))))))

(defun org-footnote-goto-definition (label)
  "Find the definition of the footnote with label LABEL."
  (interactive "sLabel: ")
  (org-mark-ring-push)
  (setq label (org-footnote-normalize-label label))
  (let ((re (format "^\\[%s\\]\\|.\\[%s:" label label))
	pos)
    (save-excursion
      (setq pos (or (re-search-forward re nil t)
		    (and (goto-char (point-min))
			 (re-search-forward re nil t))
		    (and (progn (widen) t)
			 (goto-char (point-min))
			 (re-search-forward re nil t)))))
    (if (not pos)
	(error "Cannot find definition of footnote %s" label)
      (goto-char pos)
      (org-show-context 'link-search)
      (message "Edit definition and go back with `C-c &' or, if unique, with `C-c C-c'."))))

(defun org-footnote-goto-next-reference (label)
  "Find the next reference of the footnote with label LABEL."
  (interactive "sLabel: ")
  (org-mark-ring-push)
  (setq label (org-footnote-normalize-label label))
  (let ((re (format ".\\[%s[]:]" label))
	(p0 (point)) pos)
    (save-excursion
      (setq pos (or (re-search-forward re nil t)
		    (and (goto-char (point-min))
			 (re-search-forward re nil t))
		    (and (progn (widen) t)
			 (goto-char p0)
			 (re-search-forward re nil t))
		    (and (goto-char (point-min))
			 (re-search-forward re nil t)))))
    (if pos
	(progn
	  (goto-char pos)
	  (org-show-context 'link-search))
      (error "Cannot find reference of footnote %s" label))))

(defun org-footnote-normalize-label (label)
  (if (numberp label) (setq label (number-to-string label)))
  (if (not (string-match "^[0-9]+$\\|^$\\|^fn:" label))
      (setq label (concat "fn:" label)))
  label)

(defun org-footnote-all-labels ()
  "Return list with all defined foot labels used in the buffer."
  (let (rtn l)
    (save-excursion
      (save-restriction
	(widen)
	(goto-char (point-min))
	(while (re-search-forward org-footnote-definition-re nil t)
	  (setq l (org-match-string-no-properties 2))
	  (and l (add-to-list 'rtn l)))
	(goto-char (point-min))
	(while (re-search-forward org-footnote-re nil t)
	  (setq l (or (org-match-string-no-properties 1)
		      (org-match-string-no-properties 2)))
	  (and l (not (equal l "fn:")) (add-to-list 'rtn l)))))
    rtn))

(defun org-footnote-unique-label (&optional current)
  "Return a new unique footnote label.
The returns the firsts fn:N labels that is currently not used."
  (unless current (setq current (org-footnote-all-labels)))
  (let ((fmt (if (eq org-footnote-auto-label 'plain) "%d" "fn:%d"))
	(cnt 1))
    (while (member (format fmt cnt) current)
      (incf cnt))
    (format fmt cnt)))

(defvar org-footnote-label-history nil
  "History of footnote labels entered in current buffer.")
(make-variable-buffer-local 'org-footnote-label-history)

(defun org-footnote-new ()
  "Insert a new footnote.
This command prompts for a label.  If this is a label referencing an
existing label, only insert the label.  If the footnote label is empty
or new, let the user edit the definition of the footnote."
  (interactive)
  (let* ((labels (org-footnote-all-labels))
	 (propose (org-footnote-unique-label labels))
	 (label
	  (if (member org-footnote-auto-label '(t plain))
	      propose
	    (completing-read
	     "Label (leave empty for anonymous): "
	     (mapcar 'list labels) nil nil
	     (if (eq org-footnote-auto-label 'confirm) propose nil)
	     'org-footnote-label-history))))
    (setq label (org-footnote-normalize-label label))
    (cond
     ((equal label "")
      (insert "[fn:: ]")
      (backward-char 1))
     ((member label labels)
      (insert "[" label "]")
      (message "New reference to existing note"))
     (org-footnote-define-inline
      (insert "[" label ": ]")
      (backward-char 1)
      (org-footnote-auto-adjust-maybe))
     (t
      (insert "[" label "]")
      (org-footnote-create-definition label)
      (org-footnote-auto-adjust-maybe)))))

(defun org-footnote-create-definition (label)
  "Start the definition of a footnote with label LABEL."
  (interactive "sLabel: ")
  (setq label (org-footnote-normalize-label label))
  (let (re)
    (cond
     ((org-mode-p)
      (if (not org-footnote-section)
	  ;; No section, put footnote into the current outline node
	  nil
	;; Try to find or make the special node
	(setq re (concat "^\\*+[ \t]+" org-footnote-section "[ \t]*$"))
	(unless (or (re-search-forward re nil t)
		    (and (progn (widen) t)
			 (re-search-forward re nil t)))
	  (goto-char (point-max))
	  (insert "\n\n* " org-footnote-section "\n")))
      ;; Now go to the end of this entry and insert there.
      (org-footnote-goto-local-insertion-point)
      (org-show-context 'link-search))
     (t
      (setq re (concat "^" org-footnote-tag-for-non-org-mode-files "[ \t]*$"))
      (unless (re-search-forward re nil t)
	(goto-char (point-max))
	(skip-chars-backward " \t\r\n")
	(insert "\n\n")
	(delete-region (point) (point-max))
	(insert org-footnote-tag-for-non-org-mode-files "\n"))
      (goto-char (point-max))
      (skip-chars-backward " \t\r\n")))
    (insert "\n\n")
    (insert "[" label "] ")
    (message "Edit definition and go back with `C-c &' or, if unique, with `C-c C-c'.")))

;;;###autoload
(defun org-footnote-action (&optional special)
  "Do the right thing for footnotes.
When at a footnote reference, jump to the definition.  When at a definition,
jump to the references.  When neither at definition or reference,
create a new footnote, interactively.
With prefix arg SPECIAL, offer additional commands in a menu."
  (interactive "P")
  (let (tmp c)
    (cond
     (special
      (message "Footnotes: [s]ort  |  [r]enumber fn:N  |  [S]=r+s |->[n]umeric  |  [d]elete")
      (setq c (read-char-exclusive))
      (cond
       ((equal c ?s)
	(org-footnote-normalize 'sort))
       ((equal c ?r)
	(org-footnote-renumber-fn:N))
       ((equal c ?S)
	(org-footnote-renumber-fn:N)
	(org-footnote-normalize 'sort))
       ((equal c ?n)
	(org-footnote-normalize))
       ((equal c ?d)
	(org-footnote-delete))
       (t (error "No such footnote command %c" c))))
     ((setq tmp (org-footnote-at-reference-p))
      (if (nth 1 tmp)
	  (org-footnote-goto-definition (nth 1 tmp))
	(goto-char (match-beginning 4))))
     ((setq tmp (org-footnote-at-definition-p))
      (org-footnote-goto-next-reference (nth 1 tmp)))
     (t (org-footnote-new)))))

;;;###autoload
(defun org-footnote-normalize (&optional sort-only for-preprocessor)
  "Collect the footnotes in various formats and normalize them.
This finds the different sorts of footnotes allowed in Org, and
normalizes them to the usual [N] format that is understood by the
Org-mode exporters.
When SORT-ONLY is set, only sort the footnote definitions into the
referenced sequence."
  ;; This is based on Paul's function, but rewritten.
  (let* ((limit-level
	  (and (boundp 'org-inlinetask-min-level)
	       org-inlinetask-min-level
	       (1- org-inlinetask-min-level)))
	 (nstars (and limit-level
		      (if org-odd-levels-only
			  (and limit-level (1- (* limit-level 2)))
			limit-level)))
	 (outline-regexp
	  (concat "\\*" (if nstars (format "\\{1,%d\\} " nstars) "+ ")))
	 (count 0)
	 ref def idef ref-table beg beg1 marker a before ins-point)
     (save-excursion
      ;; Now find footnote references, and extract the definitions
      (goto-char (point-min))
      (while (re-search-forward org-footnote-re nil t)
	(unless (or (org-in-commented-line) (org-in-verbatim-emphasis)
		    (org-inside-latex-macro-p))
	  (org-if-unprotected
	   (setq def (match-string 4)
		 idef def
		 ref (or (match-string 1) (match-string 2))
		 before (char-to-string (char-after (match-beginning 0))))
	   (if (equal ref "fn:") (setq ref nil))
	   (if (and ref (setq a (assoc ref ref-table)))
	       (progn
		 (setq marker (nth 1 a))
		 (unless (nth 2 a) (setf (caddr a) def)))
	     (setq marker (number-to-string (incf count))))
	   (save-match-data
	     (if def
		 (setq def (org-trim def))
	       (save-excursion
		 (goto-char (point-min))
		 (if (not (re-search-forward (concat "^\\[" (regexp-quote ref)
						     "\\]") nil t))
		     (setq def nil)
		   (setq beg (match-beginning 0))
		   (setq beg1 (match-end 0))
		   (re-search-forward
		    (org-re "^[ \t]*$\\|^\\*+ \\|^\\[\\([0-9]+\\|fn:[-_[:word:]]+\\)\\]")
		    nil 'move)
		   (setq def (buffer-substring beg1 (or (match-beginning 0)
							(point-max))))
		   (goto-char beg)
		   (skip-chars-backward " \t\n\t")
		   (delete-region (1+ (point)) (match-beginning 0))))))
	   (unless sort-only
	     (replace-match (concat before "[" marker "]") t t)
	     (and idef
		  org-footnote-fill-after-inline-note-extraction
		  (fill-paragraph)))
	   (if (not a) (push (list ref marker def (if idef t nil))
			     ref-table)))))

      ;; First find and remove the footnote section
      (goto-char (point-min))
      (cond
       ((org-mode-p)
	(if (and org-footnote-section
		 (re-search-forward
		  (concat "^\\*[ \t]+" (regexp-quote org-footnote-section)
			  "[ \t]*$")
		  nil t))
	    (if (or for-preprocessor (not org-footnote-section))
		(replace-match "")
	      (org-back-to-heading t)
	      (forward-line 1)
	      (setq ins-point (point))
	      (delete-region (point) (org-end-of-subtree t)))
	  (goto-char (point-max))
	  (unless for-preprocessor
	    (when org-footnote-section
	      (or (bolp) (insert "\n"))
	      (insert "* " org-footnote-section "\n")
	      (setq ins-point (point))))))
       (t
	(if (re-search-forward
	     (concat "^"
		     (regexp-quote org-footnote-tag-for-non-org-mode-files)
		     "[ \t]*$")
	     nil t)
	    (replace-match ""))
	(goto-char (point-max))
	(skip-chars-backward " \t\n\r")
	(delete-region (point) (point-max))
	(insert "\n\n" org-footnote-tag-for-non-org-mode-files "\n")
	(setq ins-point (point))))

      ;; Insert the footnotes again
      (goto-char (or ins-point (point-max)))
      (setq ref-table (reverse ref-table))
      (when sort-only
	;; remove anonymous and inline footnotes from the list
	(setq ref-table
	      (delq nil (mapcar
			 (lambda (x) (and (car x)
					  (not (equal (car x) "fn:"))
					  (not (nth 3 x))
					  x))
			 ref-table))))
      ;; Make sure each footnote has a description, or an error message.
      (setq ref-table
	    (mapcar
	     (lambda (x)
	       (if (not (nth 2 x))
		   (setcar (cddr x)
			   (format "FOOTNOTE DEFINITION NOT FOUND: %s" (car x)))
		 (setcar (cddr x) (org-trim (nth 2 x))))
	       x)
	     ref-table))

      (if (or (not (org-mode-p))     ; not an Org file
	      org-footnote-section   ; we do not use a footnote section
	      (not sort-only)	     ; this is normalization
	      for-preprocessor)       ; the is the preprocessor
	  ;; Insert the footnotes together in one place
	  (progn
	    (setq def
		  (mapconcat
		   (lambda (x)
		     (format "[%s] %s" (nth (if sort-only 0 1) x)
			     (org-trim (nth 2 x))))
		   ref-table "\n\n"))
	    (if ref-table (insert "\n" def "\n\n")))
	;; Insert each footnote near the first reference
	;; Happens only in Org files with no special footnote section,
	;; and only when doing sorting
	(mapc 'org-insert-footnote-reference-near-definition
	      ref-table)))))

(defun org-insert-footnote-reference-near-definition (entry)
  "Find first reference of footnote ENTRY and insert the definition there.
ENTRY is (fn-label num-mark definition)."
  (when (car entry)
    (goto-char (point-min))
    (when (re-search-forward (format ".\\[%s[]:]" (regexp-quote (car entry)))
			     nil t)
      (org-footnote-goto-local-insertion-point)
      (insert (format "\n\n[%s] %s" (car entry) (nth 2 entry))))))

(defun org-footnote-goto-local-insertion-point ()
  "Find insertion point for footnote, just before next outline heading."
  (org-with-limited-levels (outline-next-heading))
  (or (bolp) (newline))
  (beginning-of-line 0)
  (while (and (not (bobp)) (= (char-after) ?#))
    (beginning-of-line 0))
  (if (looking-at "[ \t]*#\\+TBLFM:") (beginning-of-line 2))
  (end-of-line 1)
  (skip-chars-backward "\n\r\t "))

(defun org-footnote-delete (&optional label)
  "Delete the footnote at point.
This will remove the definition (even multiple definitions if they exist)
and all references of a footnote label."
  (catch 'done
    (let (x label l beg def-re (nref 0) (ndef 0))
      (unless label
	(when (setq x (org-footnote-at-reference-p))
	  (setq label (nth 1 x))
	  (when (or (not label) (equal "fn:" label))
	    (delete-region (1+ (match-beginning 0)) (match-end 0))
	    (message "Anonymous footnote removed")
	    (throw 'done t)))
	(when (and (not label) (setq x (org-footnote-at-definition-p)))
	  (setq label (nth 1 x)))
	(unless label (error "Don't know which footnote to remove")))
      (save-excursion
	(save-restriction
	  (goto-char (point-min))
	  (while (re-search-forward org-footnote-re nil t)
	    (setq l (or (match-string 1) (match-string 2)))
	    (when (equal l label)
	      (delete-region (1+ (match-beginning 0)) (match-end 0))
	      (incf nref)))
	  (goto-char (point-min))
	  (setq def-re (concat "^\\[" (regexp-quote label) "\\]"))
	  (while (re-search-forward def-re nil t)
	    (setq beg (match-beginning 0))
	    (if (re-search-forward "^\\[\\|^[ \t]*$\\|^\\*+ " nil t)
		(goto-char (match-beginning 0))
	      (goto-char (point-max)))
	    (delete-region beg (point))
	    (incf ndef))))
      (org-footnote-auto-adjust-maybe)
      (message "%d definition(s) of and %d reference(s) of footnote %s removed"
	       ndef nref label))))

(defun org-footnote-renumber-fn:N ()
  "Renumber the simple footnotes like fn:17 into a sequence in the document."
  (interactive)
  (let (map i (n 0))
    (save-excursion
      (save-restriction
	(widen)
	(goto-char (point-min))
	(while (re-search-forward "\\[fn:\\([0-9]+\\)[]:]" nil t)
	  (setq i (string-to-number (match-string 1)))
	  (when (and (string-match "\\S-" (buffer-substring
					   (point-at-bol) (match-beginning 0)))
		     (not (assq i map)))
	    (push (cons i (number-to-string (incf n))) map)))
	(goto-char (point-min))
	(while (re-search-forward "\\(\\[fn:\\)\\([0-9]+\\)\\([]:]\\)" nil t)
	  (replace-match (concat "\\1" (cdr (assq (string-to-number (match-string 2)) map)) "\\3")))))))

(defun org-footnote-auto-adjust-maybe ()
  "Renumber and/or sort footnotes according to user settings."
  (when (memq org-footnote-auto-adjust '(t renumber))
    (org-footnote-renumber-fn:N))
  (when (memq org-footnote-auto-adjust '(t sort))
    (let ((label (nth 1 (org-footnote-at-definition-p))))
      (org-footnote-normalize 'sort)
      (when label
	(goto-char (point-min))
	(and (re-search-forward (concat "^\\[" (regexp-quote label) "\\]")
				nil t)
	     (progn (insert " ")
		    (just-one-space)))))))

(provide 'org-footnote)

;; arch-tag: 1b5954df-fb5d-4da5-8709-78d944dbfc37

;;; org-footnote.el ends here
