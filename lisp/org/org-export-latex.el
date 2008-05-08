;;; org-export-latex.el --- LaTeX exporter for org-mode
;;
;; Copyright (C) 2007, 2008 Free Software Foundation, Inc.
;;
;; Emacs Lisp Archive Entry
;; Filename: org-export-latex.el
;; Version: 6.02b
;; Author: Bastien Guerry <bzg AT altern DOT org>
;; Maintainer: Bastien Guerry <bzg AT altern DOT org>
;; Keywords: org, wp, tex
;; Description: Converts an org-mode buffer into LaTeX
;; URL: http://www.cognition.ens.fr/~guerry/u/org-export-latex.el

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

;;; Commentary:
;;
;; This library implements a LaTeX exporter for org-mode.
;;
;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'org-export-latex)
;;
;; The interactive functions are similar to those of the HTML exporter:
;;
;; M-x `org-export-as-latex'
;; M-x `org-export-as-latex-batch'
;; M-x `org-export-as-latex-to-buffer'
;; M-x `org-export-region-as-latex'
;; M-x `org-replace-region-by-latex'
;;
;;; Code:

(eval-when-compile
  (require 'cl))

(require 'footnote)
(require 'org)
(require 'org-exp)

;;; Variables:
(defvar org-export-latex-class nil)
(defvar org-export-latex-header nil)
(defvar org-export-latex-append-header nil)
(defvar org-export-latex-options-plist nil)
(defvar org-export-latex-todo-keywords-1 nil)
(defvar org-export-latex-all-targets-re nil)
(defvar org-export-latex-add-level 0)
(defvar org-export-latex-sectioning "")
(defvar org-export-latex-sectioning-depth 0)
(defvar org-export-latex-list-beginning-re
  "^\\([ \t]*\\)\\([-+*]\\|[0-9]+[.)]\\) +?")

(defvar org-export-latex-special-string-regexps
  '(org-ts-regexp
    org-scheduled-string
    org-deadline-string
    org-clock-string)
  "A list of regexps to convert as special keywords.")

(defvar latexp)    ; dynamically scoped from org.el
(defvar re-quote)  ; dynamically scoped from org.el
(defvar commentsp) ; dynamically scoped from org.el

;;; User variables:

(defcustom org-export-latex-default-class "article"
  "The default LaTeX class."
  :group 'org-export-latex
  :type '(string :tag "LaTeX class"))

(defcustom org-export-latex-classes
  '(("article"
     "\\documentclass[11pt,a4paper]{article}
\\usepackage[utf8]{inputenc}
\\usepackage[T1]{fontenc}
\\usepackage{hyperref}"
     ("\\section{%s}" . "\\section*{%s}")
     ("\\subsection{%s}" . "\\subsection*{%s}")
     ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
     ("\\paragraph{%s}" . "\\paragraph*{%s}")
     ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
    ("report"
     "\\documentclass[11pt,a4paper]{report}
\\usepackage[utf8]{inputenc}
\\usepackage[T1]{fontenc}
\\usepackage{hyperref}"
     ("\\part{%s}" . "\\part*{%s}")
     ("\\chapter{%s}" . "\\chapter*{%s}")
     ("\\section{%s}" . "\\section*{%s}")
     ("\\subsection{%s}" . "\\subsection*{%s}")
     ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
    ("book"
     "\\documentclass[11pt,a4paper]{book}
\\usepackage[utf8]{inputenc}
\\usepackage[T1]{fontenc}
\\usepackage{hyperref}"
     ("\\part{%s}" . "\\part*{%s}")
     ("\\chapter{%s}" . "\\chapter*{%s}")
     ("\\section{%s}" . "\\section*{%s}")
     ("\\subsection{%s}" . "\\subsection*{%s}")
     ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))
  "Alist of LaTeX classes and associated header and structure.
If #+LaTeX_CLASS is set in the buffer, use its value and the
associated information.  Here is the structure of each cell:

  \(class-name
    header-string
    (unnumbered-section numbered-section\)
    ...\)

A %s formatter is mandatory in each section string and will be
replaced by the title of the section."
  :group 'org-export-latex
  :type '(repeat
	  (list (string :tag "LaTeX class")
		(string :tag "LaTeX header")
		(cons :tag "Level 1"
		      (string :tag "Numbered")
		      (string :tag "Unnumbered"))
		(cons :tag "Level 2"
		      (string :tag "Numbered")
		      (string :tag "Unnumbered"))
		(cons :tag "Level 3"
		      (string :tag "Numbered")
		      (string :tag "Unnumbered"))
		(cons :tag "Level 4"
		      (string :tag "Numbered")
		      (string :tag "Unnumbered"))
		(cons :tag "Level 5"
		      (string :tag "Numbered")
		      (string :tag "Unnumbered")))))

(defcustom org-export-latex-emphasis-alist
  '(("*" "\\textbf{%s}" nil)
    ("/" "\\emph{%s}" nil)
    ("_" "\\underline{%s}" nil)
    ("+" "\\texttt{%s}" nil)
    ("=" "\\texttt{%s}" nil)
    ("~" "\\texttt{%s}" t))
  "Alist of LaTeX expressions to convert emphasis fontifiers.
Each element of the list is a list of three elements.
The first element is the character used as a marker for fontification.
The second element is a formatting string to wrap fontified text with.
The third element decides whether to protect converted text from other
conversions."
  :group 'org-export-latex
  :type 'alist)

(defcustom org-export-latex-title-command "\\maketitle"
  "The command used to insert the title just after \\begin{document}.
If this string contains the formatting specification \"%s\" then
it will be used as a formatting string, passing the title as an
argument."
  :group 'org-export-latex
  :type 'string)

(defcustom org-export-latex-date-format
  "%d %B %Y"
  "Format string for \\date{...}."
  :group 'org-export-latex
  :type 'string)

(defcustom org-export-latex-tables-verbatim nil
  "When non-nil, export tables as verbatim."
  :group 'org-export-latex
  :type 'boolean)

(defcustom org-export-latex-tables-column-borders nil
  "When non-nil, group of columns are surrounded with borders,
XSeven if these borders are the outside borders of the table."
  :group 'org-export-latex
  :type 'boolean)

(defcustom org-export-latex-packages-alist nil
  "Alist of packages to be inserted in the header.
Each cell is of the forma \( \"option\" . \"package\" \)."
  :group 'org-export-latex
  :type 'alist)

(defcustom org-export-latex-low-levels 'description
  "How to convert sections below the current level of sectioning,
as specified by `org-export-headline-levels' or the value of \"H:\"
in Org's #+OPTION line.

This can be either nil (skip the sections), 'description (convert
the sections as descriptive lists) or a string to be used instead
of \\section{%s}. In this latter case, the %s stands here for the
inserted headline and is mandatory."
  :group 'org-export-latex
  :type '(choice (const :tag "Ignore" nil)
		 (symbol :tag "Convert as descriptive list" description)
		 (string :tag "Use a section string" :value "\\subparagraph{%s}")))

(defcustom org-export-latex-remove-from-headlines
  '(:todo t :priority t :tags t)
  "A plist of keywords to remove from headlines.
Non-nil means remove this keyword type from the headline.

Don't remove the keys, just change their values."
  :type 'plist
  :group 'org-export-latex)

(defcustom org-export-latex-image-default-option "width=10em"
  "Default option for images."
  :group 'org-export-latex
  :type 'string)

(defcustom org-export-latex-coding-system nil
  "Coding system for the exported LaTex file."
  :group 'org-export-latex
  :type 'coding-system)

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

;;; Autoload functions:

;;;###autoload
(defun org-export-as-latex-batch ()
  "Call `org-export-as-latex', may be used in batch processing as
emacs 	--batch
	--load=$HOME/lib/emacs/org.el
	--eval \"(setq org-export-headline-levels 2)\"
	--visit=MyFile --funcall org-export-as-latex-batch"
  (org-export-as-latex org-export-headline-levels 'hidden))

;;;###autoload
(defun org-export-as-latex-to-buffer (arg)
  "Call `org-exort-as-latex` with output to a temporary buffer.
No file is created.  The prefix ARG is passed through to `org-export-as-latex'."
  (interactive "P")
  (org-export-as-latex arg nil nil "*Org LaTeX Export*")
  (switch-to-buffer-other-window "*Org LaTeX Export*"))

;;;###autoload
(defun org-replace-region-by-latex (beg end)
  "Replace the region from BEG to END with its LaTeX export.
It assumes the region has `org-mode' syntax, and then convert it to
LaTeX.  This can be used in any buffer.  For example, you could
write an itemized list in `org-mode' syntax in an LaTeX buffer and
then use this command to convert it."
  (interactive "r")
  (let (reg latex buf)
    (save-window-excursion
      (if (org-mode-p)
	  (setq latex (org-export-region-as-latex
		       beg end t 'string))
	(setq reg (buffer-substring beg end)
	      buf (get-buffer-create "*Org tmp*"))
	(save-excursion
	  (set-buffer buf)
	  (erase-buffer)
	  (insert reg)
	  (org-mode)
	  (setq latex (org-export-region-as-latex
		       (point-min) (point-max) t 'string)))
	(kill-buffer buf)))
    (delete-region beg end)
    (insert latex)))

;;;###autoload
(defun org-export-region-as-latex (beg end &optional body-only buffer)
  "Convert region from BEG to END in `org-mode' buffer to LaTeX.
If prefix arg BODY-ONLY is set, omit file header, footer, and table of
contents, and only produce the region of converted text, useful for
cut-and-paste operations.
If BUFFER is a buffer or a string, use/create that buffer as a target
of the converted LaTeX.  If BUFFER is the symbol `string', return the
produced LaTeX as a string and leave not buffer behind.  For example,
a Lisp program could call this function in the following way:

  (setq latex (org-export-region-as-latex beg end t 'string))

When called interactively, the output buffer is selected, and shown
in a window.  A non-interactive call will only retunr the buffer."
  (interactive "r\nP")
  (when (interactive-p)
    (setq buffer "*Org LaTeX Export*"))
  (let ((transient-mark-mode t) (zmacs-regions t)
	rtn)
    (goto-char end)
    (set-mark (point)) ;; to activate the region
    (goto-char beg)
    (setq rtn (org-export-as-latex
	       nil nil nil
	       buffer body-only))
    (if (fboundp 'deactivate-mark) (deactivate-mark))
    (if (and (interactive-p) (bufferp rtn))
	(switch-to-buffer-other-window rtn)
      rtn)))

;;;###autoload
(defun org-export-as-latex (arg &optional hidden ext-plist
				to-buffer body-only pub-dir)
  "Export current buffer to a LaTeX file.
If there is an active region, export only the region.  The prefix
ARG specifies how many levels of the outline should become
headlines.  The default is 3.  Lower levels will be exported
depending on `org-export-latex-low-levels'.  The default is to
convert them as description lists.  When HIDDEN is non-nil, don't
display the LaTeX buffer.  EXT-PLIST is a property list with
external parameters overriding org-mode's default settings, but
still inferior to file-local settings.  When TO-BUFFER is
non-nil, create a buffer with that name and export to that
buffer.  If TO-BUFFER is the symbol `string', don't leave any
buffer behind but just return the resulting LaTeX as a string.
When BODY-ONLY is set, don't produce the file header and footer,
simply return the content of \begin{document}...\end{document},
without even the \begin{document} and \end{document} commands.
when PUB-DIR is set, use this as the publishing directory."
  (interactive "P")
  ;; Make sure we have a file name when we need it.
  (when (and (not (or to-buffer body-only))
	     (not buffer-file-name))
    (if (buffer-base-buffer)
	(org-set-local 'buffer-file-name
		       (with-current-buffer (buffer-base-buffer)
			 buffer-file-name))
      (error "Need a file name to be able to export")))

  (message "Exporting to LaTeX...")
  (org-update-radio-target-regexp)
  (org-export-latex-set-initial-vars ext-plist arg)
  (let* ((wcf (current-window-configuration))
	 (opt-plist org-export-latex-options-plist)
	 (region-p (org-region-active-p))
	 (subtree-p
	  (when region-p
	    (save-excursion
	      (goto-char (region-beginning))
	      (and (org-at-heading-p)
		   (>= (org-end-of-subtree t t) (region-end))))))
	 (title (or (and subtree-p (org-export-get-title-from-subtree))
		    (plist-get opt-plist :title)
		    (and (not
			  (plist-get opt-plist :skip-before-1st-heading))
			 (org-export-grab-title-from-buffer))
		    (file-name-sans-extension
		     (file-name-nondirectory buffer-file-name))))
	 (filename (concat (file-name-as-directory
			    (or pub-dir
				(org-export-directory :LaTeX ext-plist)))
			   (file-name-sans-extension
			    (file-name-nondirectory ;sans-extension
			     buffer-file-name)) ".tex"))
	 (filename (if (equal (file-truename filename)
			      (file-truename buffer-file-name))
		       (concat filename ".tex")
		     filename))
	 (buffer (if to-buffer
		     (cond
		      ((eq to-buffer 'string) (get-buffer-create
					       "*Org LaTeX Export*"))
		      (t (get-buffer-create to-buffer)))
		   (find-file-noselect filename)))
	 (odd org-odd-levels-only)
	 (header (org-export-latex-make-header title opt-plist))
	 (skip (cond (subtree-p nil)
		     (region-p t)
		 ;; never skip first lines when exporting a subtree
		     (t (plist-get opt-plist :skip-before-1st-heading))))
	 (text (plist-get opt-plist :text))
	 (first-lines (if skip "" (org-export-latex-first-lines)))
	 (coding-system (and (boundp 'buffer-file-coding-system)
			     buffer-file-coding-system))
	 (coding-system-for-write (or org-export-latex-coding-system
				      coding-system))
	 (save-buffer-coding-system (or org-export-latex-coding-system
					coding-system))
         (region (buffer-substring
		  (if region-p (region-beginning) (point-min))
		  (if region-p (region-end) (point-max))))
	 (string-for-export
	  (org-export-preprocess-string
	   region :emph-multiline t
		  :for-LaTeX t
		  :comments nil
		  :add-text (if (eq to-buffer 'string) nil text)
		  :skip-before-1st-heading skip
		  :LaTeX-fragments nil)))

    (set-buffer buffer)
    (erase-buffer)

    (and (fboundp 'set-buffer-file-coding-system)
	 (set-buffer-file-coding-system coding-system-for-write))

    ;; insert the header and initial document commands
    (unless (or (eq to-buffer 'string) body-only)
      (insert header))

    ;; insert text found in #+TEXT
    (when (and text (not (eq to-buffer 'string)))
      (insert (org-export-latex-content
	       text '(lists tables fixed-width keywords))
	       "\n\n"))

    ;; insert lines before the first headline
    (unless (or skip (eq to-buffer 'string))
      (insert first-lines))

    ;; handle the case where the region does not begin with a section
    (when region-p
      (insert (with-temp-buffer
		(insert string-for-export)
		(org-export-latex-first-lines))))

    ;; export the content of headlines
    (org-export-latex-global
     (with-temp-buffer
       (insert string-for-export)
       (goto-char (point-min))
       (when (re-search-forward "^\\(\\*+\\) " nil t)
	 (let* ((asters (length (match-string 1)))
		(level (if odd (- asters 2) (- asters 1))))
	   (setq org-export-latex-add-level
		 (if odd (1- (/ (1+ asters) 2)) (1- asters)))
	   (org-export-latex-parse-global level odd)))))

    ;; finalization
    (unless body-only (insert "\n\\end{document}"))
    (or to-buffer (save-buffer))
    (goto-char (point-min))
    (message "Exporting to LaTeX...done")
    (prog1
	(if (eq to-buffer 'string)
	    (prog1 (buffer-substring (point-min) (point-max))
	      (kill-buffer (current-buffer)))
	  (current-buffer))
      (set-window-configuration wcf))))

;;; Parsing functions:

(defun org-export-latex-parse-global (level odd)
  "Parse the current buffer recursively, starting at LEVEL.
If ODD is non-nil, assume the buffer only contains odd sections.
Return a list reflecting the document structure."
  (save-excursion
    (goto-char (point-min))
    (let* ((cnt 0) output
	   (depth org-export-latex-sectioning-depth))
      (while (re-search-forward
	      (concat "^\\(\\(?:\\*\\)\\{"
		      (number-to-string (+ (if odd 2 1) level))
		      "\\}\\) \\(.*\\)$")
	      ;; make sure that there is no upper heading
	      (when (> level 0)
		(save-excursion
		  (save-match-data
		    (re-search-forward
		     (concat "^\\(\\(?:\\*\\)\\{"
			     (number-to-string level)
			     "\\}\\) \\(.*\\)$") nil t)))) t)
	(setq cnt (1+ cnt))
	(let* ((pos (match-beginning 0))
	       (heading (match-string 2))
	       (nlevel (if odd (/ (+ 3 level) 2) (1+ level))))
	  (save-excursion
	    (narrow-to-region
	     (point)
	     (save-match-data
	       (if (re-search-forward
		    (concat "^\\(\\(?:\\*\\)\\{"
			    (number-to-string (+ (if odd 2 1) level))
			    "\\}\\) \\(.*\\)$") nil t)
		   (match-beginning 0)
		 (point-max))))
	    (goto-char (point-min))
	    (setq output
		  (append output
			  (list
			   (list
			    `(pos . ,pos)
			    `(level . ,nlevel)
			    `(occur . ,cnt)
			    `(heading . ,heading)
			    `(content . ,(org-export-latex-parse-content))
			    `(subcontent . ,(org-export-latex-parse-subcontent
					     level odd)))))))
	  (widen)))
      (list output))))

(defun org-export-latex-parse-content ()
  "Extract the content of a section."
  (let ((beg (point))
	(end (if (re-search-forward "^\\(\\*\\)+ .*$" nil t)
		 (progn (beginning-of-line) (point))
	       (point-max))))
    (buffer-substring beg end)))

(defun org-export-latex-parse-subcontent (level odd)
  "Extract the subcontent of a section at LEVEL.
If ODD Is non-nil, assume subcontent only contains odd sections."
  (if (not (re-search-forward
	    (concat "^\\(\\(?:\\*\\)\\{"
		    (number-to-string (+ (if odd 4 2) level))
		    "\\}\\) \\(.*\\)$")
	    nil t))
      nil ; subcontent is nil
    (org-export-latex-parse-global (+ (if odd 2 1) level) odd)))

;;; Rendering functions:
(defun org-export-latex-global (content)
  "Export CONTENT to LaTeX.
CONTENT is an element of the list produced by
`org-export-latex-parse-global'."
  (if (eq (car content) 'subcontent)
      (mapc 'org-export-latex-sub (cdr content))
    (org-export-latex-sub (car content))))

(defun org-export-latex-sub (subcontent)
  "Export the list SUBCONTENT to LaTeX.
SUBCONTENT is an alist containing information about the headline
and its content."
  (let ((num (plist-get org-export-latex-options-plist :section-numbers)))
    (mapc (lambda(x) (org-export-latex-subcontent x num)) subcontent)))

(defun org-export-latex-subcontent (subcontent num)
  "Export each cell of SUBCONTENT to LaTeX."
  (let ((heading (org-export-latex-fontify-headline
		  (cdr (assoc 'heading subcontent))))
	(level (- (cdr (assoc 'level subcontent))
		  org-export-latex-add-level))
	(occur (number-to-string (cdr (assoc 'occur subcontent))))
	(content (cdr (assoc 'content subcontent)))
	(subcontent (cadr (assoc 'subcontent subcontent))))
    (cond
     ;; Normal conversion
     ((<= level org-export-latex-sectioning-depth)
      (let ((sec (nth (1- level) org-export-latex-sectioning)))
	(insert (format (if num (car sec) (cdr sec)) heading) "\n"))
      (insert (org-export-latex-content content))
      (cond ((stringp subcontent) (insert subcontent))
	    ((listp subcontent) (org-export-latex-sub subcontent))))
     ;; At a level under the hl option: we can drop this subsection
     ((> level org-export-latex-sectioning-depth)
      (cond ((eq org-export-latex-low-levels 'description)
	     (insert (format "\\begin{description}\n\n\\item[%s]\n\n" heading))
	     (insert (org-export-latex-content content))
	     (cond ((stringp subcontent) (insert subcontent))
		   ((listp subcontent) (org-export-latex-sub subcontent)))
	     (insert "\\end{description}\n"))
	    ((stringp org-export-latex-low-levels)
	     (insert (format org-export-latex-low-levels heading) "\n")
	     (insert (org-export-latex-content content))
	     (cond ((stringp subcontent) (insert subcontent))
		   ((listp subcontent) (org-export-latex-sub subcontent)))))))))

;;; Exporting internals:
(defun org-export-latex-set-initial-vars (ext-plist level)
  "Store org local variables required for LaTeX export.
EXT-PLIST is an optional additional plist.
LEVEL indicates the default depth for export."
  (setq org-export-latex-todo-keywords-1 org-todo-keywords-1
	org-export-latex-all-targets-re
	(org-make-target-link-regexp (org-all-targets))
	org-export-latex-options-plist
	(org-combine-plists (org-default-export-plist) ext-plist
			    (org-infile-export-plist))
	org-export-latex-class
	(save-excursion
	  (goto-char (point-min))
	  (if (and (re-search-forward "^#\\+LaTeX_CLASS:[ \t]*\\([a-zA-Z]+\\)" nil t)
		   (assoc (match-string 1) org-export-latex-classes))
	      (match-string 1)
	    org-export-latex-default-class))
	org-export-latex-header
	(cadr (assoc org-export-latex-class org-export-latex-classes))
	org-export-latex-sectioning
	(cddr (assoc org-export-latex-class org-export-latex-classes))
	org-export-latex-sectioning-depth
	(or level
	    (let ((hl-levels
		   (plist-get org-export-latex-options-plist :headline-levels))
		  (sec-depth (length org-export-latex-sectioning)))
	      (if (> hl-levels sec-depth) sec-depth hl-levels)))))

(defun org-export-latex-make-header (title opt-plist)
  "Make the LaTeX header and return it as a string.
TITLE is the current title from the buffer or region.
OPT-PLIST is the options plist for current buffer."
  (let ((toc (plist-get opt-plist :table-of-contents))
	(author (plist-get opt-plist :author)))
    (concat
     (if (plist-get opt-plist :time-stamp-file)
	 (format-time-string "% Created %Y-%m-%d %a %H:%M\n"))
     ;; insert LaTeX custom header
     org-export-latex-header
     "\n"
     ;; insert information on LaTeX packages
     (when org-export-latex-packages-alist
       (mapconcat (lambda(p)
		    (if (equal "" (car p))
			(format "\\usepackage{%s}" (cadr p))
		      (format "\\usepackage[%s]{%s}"
			      (car p) (cadr p))))
		  org-export-latex-packages-alist "\n"))
     ;; insert additional commands in the header
     org-export-latex-append-header
     ;; insert the title
     (format
      "\n\n\\title{%s}\n"
      ;; convert the title
      (org-export-latex-content
       title '(lists tables fixed-width keywords)))
     ;; insert author info
     (if (plist-get opt-plist :author-info)
	 (format "\\author{%s}\n"
		 (or author user-full-name))
       (format "%%\\author{%s}\n"
	       (or author user-full-name)))
     ;; insert the date
     (format "\\date{%s}\n"
	     (format-time-string
	      (or (plist-get opt-plist :date)
		  org-export-latex-date-format)))
     ;; beginning of the document
     "\n\\begin{document}\n\n"
     ;; insert the title command
     (if (string-match "%s" org-export-latex-title-command)
	 (format org-export-latex-title-command title)
       org-export-latex-title-command)
     "\n\n"
     ;; table of contents
     (when (and org-export-with-toc
		(plist-get opt-plist :section-numbers))
       (cond ((numberp toc)
	      (format "\\setcounter{tocdepth}{%s}\n\\tableofcontents\n\n"
		      (min toc (plist-get opt-plist :headline-levels))))
	     (toc (format "\\setcounter{tocdepth}{%s}\n\\tableofcontents\n\n"
			  (plist-get opt-plist :headline-levels))))))))

(defun org-export-latex-first-lines (&optional comments)
  "Export the first lines before first headline.
COMMENTS is either nil to replace them with the empty string or a
formatting string like %%%%s if we want to comment them out."
  (save-excursion
    (goto-char (point-min))
    (if (org-at-heading-p) (beginning-of-line 2))
    (let* ((pt (point))
	   (end (if (and (re-search-forward "^\\* " nil t)
			 (not (eq pt (match-beginning 0))))
		    (goto-char (match-beginning 0))
		  (goto-char (point-max)))))
      (org-export-latex-content
       (org-export-preprocess-string
	(buffer-substring (point-min) end)
	:for-LaTeX t
	:emph-multiline t
	:add-text nil
	:comments nil
	:skip-before-1st-heading nil
	:LaTeX-fragments nil)))))

(defun org-export-latex-content (content &optional exclude-list)
  "Convert CONTENT string to LaTeX.
Don't perform conversions that are in EXCLUDE-LIST.  Recognized
conversion types are: quotation-marks, emphasis, sub-superscript,
links, keywords, lists, tables, fixed-width"
  (with-temp-buffer
   (insert content)
   (unless (memq 'quotation-marks exclude-list)
     (org-export-latex-quotation-marks))
   (unless (memq 'emphasis exclude-list)
     (when (plist-get org-export-latex-options-plist :emphasize)
       (org-export-latex-fontify)))
   (unless (memq 'sub-superscript exclude-list)
     (org-export-latex-special-chars
      (plist-get org-export-latex-options-plist :sub-superscript)))
   (unless (memq 'links exclude-list)
     (org-export-latex-links))
   (unless (memq 'keywords exclude-list)
     (org-export-latex-keywords
      (plist-get org-export-latex-options-plist :timestamps)))
   (unless (memq 'lists exclude-list)
     (org-export-latex-lists))
   (unless (memq 'tables exclude-list)
     (org-export-latex-tables
      (plist-get org-export-latex-options-plist :tables)))
   (unless (memq 'fixed-width exclude-list)
     (org-export-latex-fixed-width
      (plist-get org-export-latex-options-plist :fixed-width)))
   ;; return string
   (buffer-substring (point-min) (point-max))))

(defun org-export-latex-protect-string (s)
  "Prevent further conversion for string S by adding the
org-protect property."
  (add-text-properties 0 (length s) '(org-protected t) s) s)

(defun org-export-latex-protect-char-in-string (char-list string)
  "Add org-protected text-property to char from CHAR-LIST in STRING."
  (with-temp-buffer
    (save-match-data
      (insert string)
      (goto-char (point-min))
      (while (re-search-forward (regexp-opt char-list) nil t)
	(add-text-properties (match-beginning 0)
			     (match-end 0) '(org-protected t)))
      (buffer-string))))

(defun org-export-latex-keywords-maybe (remove-list)
  "Maybe remove keywords depending on rules in REMOVE-LIST."
  (goto-char (point-min))
  (let ((re-todo (mapconcat 'identity org-export-latex-todo-keywords-1 "\\|"))
	(case-fold-search nil))
    ;; convert TODO keywords
    (when (re-search-forward (concat "^\\(" re-todo "\\)") nil t)
      (if (plist-get remove-list :todo)
	  (replace-match "")
	(replace-match (format "\\texttt{%s}" (match-string 1)) t t)))
    ;; convert priority string
    (when (re-search-forward "\\[\\\\#.\\]" nil t)
      (if (plist-get remove-list :priority)
	  (replace-match "")
	(replace-match (format "\\texttt{%s}" (match-string 0)) t t)))
    ;; convert tags
    (when (re-search-forward "\\(:[a-zA-Z0-9]+\\)+:" nil t)
      (if (or (not org-export-with-tags)
	      (plist-get remove-list :tags))
	  (replace-match "")
	(replace-match (format "\\texttt{%s}" (match-string 0)) t t)))))

(defun org-export-latex-fontify-headline (string)
  "Fontify special words in string."
  (with-temp-buffer
    ;; FIXME: org-inside-LaTeX-fragment-p doesn't work when the $...$ is at
    ;; the beginning of the buffer - inserting "\n" is safe here though.
    (insert "\n" string)
    (goto-char (point-min))
    (when (plist-get org-export-latex-options-plist :emphasize)
      (org-export-latex-fontify))
    (org-export-latex-special-chars
     (plist-get org-export-latex-options-plist :sub-superscript))
    (org-export-latex-keywords-maybe
     org-export-latex-remove-from-headlines)
    (org-export-latex-links)
    (org-trim (buffer-substring-no-properties (point-min) (point-max)))))

(defun org-export-latex-quotation-marks ()
  "Export question marks depending on language conventions."
  (let* ((lang (plist-get org-export-latex-options-plist :language))
	 (quote-rpl (if (equal lang "fr")
			'(("\\(\\s-\\)\"" "«~")
			  ("\\(\\S-\\)\"" "~»")
			  ("\\(\\s-\\)'" "`"))
		      '(("\\(\\s-\\)\"" "``")
			("\\(\\S-\\)\"" "''")
			("\\(\\s-\\)'" "`")))))
    (mapc (lambda(l) (goto-char (point-min))
	    (while (re-search-forward (car l) nil t)
	      (let ((rpl (concat (match-string 1) (cadr l))))
		(org-export-latex-protect-string rpl)
		(org-if-unprotected
		 (replace-match rpl t t))))) quote-rpl)))

(defun org-export-latex-special-chars (sub-superscript)
  "Export special characters to LaTeX.
If SUB-SUPERSCRIPT is non-nil, convert \\ and ^.
See the `org-export-latex.el' code for a complete conversion table."
  (goto-char (point-min))
  (mapc (lambda(c)
	  (goto-char (point-min))
	  (while (re-search-forward c nil t)
	    ;; Put the point where to check for org-protected
	    (unless (or (get-text-property (match-beginning 2) 'org-protected)
			(org-at-table-p))
	      (cond ((member (match-string 2) '("\\$" "$"))
		     (if (equal (match-string 2) "\\$")
			 (replace-match (concat (match-string 1) "$"
						(match-string 3)) t t)
		       (replace-match (concat (match-string 1) "\\$"
					      (match-string 3)) t t)))
		    ((member (match-string 2) '("&" "%" "#"))
		     (if (equal (match-string 1) "\\")
			 (replace-match (match-string 2) t t)
		       (replace-match (concat (match-string 1) "\\"
					      (match-string 2)) t t)))
		    ((equal (match-string 2) "...")
		     (replace-match
		      (concat (match-string 1)
			      (org-export-latex-protect-string "\\ldots{}")) t t))
		    ((equal (match-string 2) "~")
		     (cond ((equal (match-string 1) "\\") nil)
			   ((eq 'org-link (get-text-property 0 'face (match-string 2)))
			    (replace-match (concat (match-string 1) "\\~") t t))
			   (t (replace-match
			       (org-export-latex-protect-string
				(concat (match-string 1) "\\~{}")) t t))))
		    ((member (match-string 2) '("{" "}"))
		     (unless (save-match-data (org-inside-LaTeX-fragment-p))
		       (if (equal (match-string 1) "\\")
			   (replace-match (match-string 2) t t)
			 (replace-match (concat (match-string 1) "\\"
						(match-string 2)) t t)))))
	      (unless (save-match-data (org-inside-LaTeX-fragment-p))
		(cond ((equal (match-string 2) "\\")
		       (replace-match (or (save-match-data
					    (org-export-latex-treat-backslash-char
					     (match-string 1)
					     (match-string 3))) "") t t))
		      ((member (match-string 2) '("_" "^"))
		       (replace-match (or (save-match-data
					    (org-export-latex-treat-sub-super-char
					     sub-superscript
					     (match-string 1)
					     (match-string 2)
					     (match-string 3))) "") t t)))))))
	'("^\\([^\n$]*?\\|^\\)\\(\\\\?\\$\\)\\([^\n$]*\\)$"
 	  "\\([a-za-z0-9]+\\|[ \t\n]\\|\\b\\|\\\\\\)\\(_\\|\\^\\)\\([a-za-z0-9]+\\|[ \t\n]\\|[:punct:]\\|{[a-za-z0-9]+}\\|([a-za-z0-9]+)\\)"
	  "\\(.\\|^\\)\\(\\\\\\)\\([ \t\n]\\|[a-zA-Z&#%{}\"]+\\)"
	  "\\(.\\|^\\)\\(&\\)"
 	  "\\(.\\|^\\)\\(#\\)"
 	  "\\(.\\|^\\)\\(%\\)"
 	  "\\(.\\|^\\)\\({\\)"
	  "\\(.\\|^\\)\\(}\\)"
	  "\\(.\\|^\\)\\(~\\)"
	  "\\(.\\|^\\)\\(\\.\\.\\.\\)"
	  ;; (?\< . "\\textless{}")
	  ;; (?\> . "\\textgreater{}")
	  )))

(defun org-export-latex-treat-sub-super-char
  (subsup string-before char string-after)
  "Convert the \"_\" and \"^\" characters to LaTeX.
SUBSUP corresponds to the ^: option in the #+OPTIONS line.
Convert CHAR depending on STRING-BEFORE and STRING-AFTER."
  (cond ((equal string-before "\\")
	 (concat string-before char string-after))
	;; this is part of a math formula
	((and (string-match "\\S-+" string-before)
	      (string-match "\\S-+" string-after))
	 (cond ((eq 'org-link (get-text-property 0 'face char))
		(concat string-before "\\" char string-after))
	       ((save-match-data (org-inside-LaTeX-fragment-p))
		(if subsup
		    (cond ((eq 1 (length string-after))
			   (concat string-before char string-after))
			  ((string-match "[({]?\\([^)}]+\\)[)}]?" string-after)
			   (format "%s%s{%s}" string-before char
				   (match-string 1 string-after))))))
	       ((and subsup
		     (> (length string-after) 1)
		     (string-match "[({]?\\([^)}]+\\)[)}]?" string-after))
		(format "$%s%s{%s}$" string-before char
			(match-string 1 string-after)))
	       (subsup (concat "$" string-before char string-after "$"))
	       (t (org-export-latex-protect-string
		   (concat string-before "\\" char "{}" string-after)))))
	(t (org-export-latex-protect-string
	    (concat string-before "\\" char "{}" string-after)))))

(defun org-export-latex-treat-backslash-char (string-before string-after)
  "Convert the \"$\" special character to LaTeX.
The conversion is made depending of STRING-BEFORE and STRING-AFTER."
  (cond ((member (list string-after) org-html-entities)
	 ;; backslash is part of a special entity (like "\alpha")
	 (concat string-before "$\\"
		 (or (cdar (member (list string-after) org-html-entities))
		     string-after) "$"))
	((and (not (string-match "^[ \n\t]" string-after))
	      (not (string-match "[ \t]\\'\\|^" string-before)))
	 ;; backslash is inside a word
	 (org-export-latex-protect-string
	  (concat string-before "\\textbackslash{}" string-after)))
	((not (or (equal string-after "")
		  (string-match "^[ \t\n]" string-after)))
	 ;; backslash might escape a character (like \#) or a user TeX
	 ;; macro (like \setcounter)
	 (org-export-latex-protect-string
	  (concat string-before "\\" string-after)))
	((and (string-match "^[ \t\n]" string-after)
	      (string-match "[ \t\n]\\'" string-before))
	 ;; backslash is alone, convert it to $\backslash$
	 (org-export-latex-protect-string
	  (concat string-before "\\textbackslash{}" string-after)))
	(t (org-export-latex-protect-string
	    (concat string-before "\\textbackslash{}" string-after)))))

(defun org-export-latex-keywords (timestamps)
  "Convert special keywords to LaTeX.
Regexps are those from `org-export-latex-special-string-regexps'."
  (let ((rg org-export-latex-special-string-regexps) r)
    (while (setq r (pop rg))
      (goto-char (point-min))
      (while (re-search-forward (eval r) nil t)
	(if (not timestamps)
	    (replace-match (format "\\\\texttt{%s}" (match-string 0)) t)
	  (replace-match ""))))))

(defun org-export-latex-fixed-width (opt)
  "When OPT is non-nil convert fixed-width sections to LaTeX."
  (goto-char (point-min))
  (while (re-search-forward "^[ \t]*:" nil t)
    (if opt
	(progn (goto-char (match-beginning 0))
	       (insert "\\begin{verbatim}\n")
	       (while (looking-at "^\\([ \t]*\\):\\(.*\\)$")
		 (replace-match (concat (match-string 1)
					(match-string 2)) t t)
		 (forward-line))
	       (insert "\\end{verbatim}\n\n"))
      (progn (goto-char (match-beginning 0))
	     (while (looking-at "^\\([ \t]*\\):\\(.*\\)$")
	       (replace-match (concat "%" (match-string 1)
				      (match-string 2)) t t)
	       (forward-line))))))


(defvar org-table-last-alignment) ; defined in org-table.el
(declare-function orgtbl-to-latex "org-table" (table params) t)
(defun org-export-latex-tables (insert)
  "Convert tables to LaTeX and INSERT it."
  (goto-char (point-min))
  (while (re-search-forward "^\\([ \t]*\\)|" nil t)
    ;; FIXME really need to save-excursion?
    (save-excursion (org-table-align))
    (let* ((beg (org-table-begin))
	   (end (org-table-end))
	   (raw-table (buffer-substring-no-properties beg end))
	   fnum fields line lines olines gr colgropen line-fmt align)
      (if org-export-latex-tables-verbatim
	  (let* ((tbl (concat "\\begin{verbatim}\n" raw-table
			      "\\end{verbatim}\n")))
	    (apply 'delete-region (list beg end))
	    (insert tbl))
	(progn
	  (setq lines (split-string raw-table "\n" t))
	  (apply 'delete-region (list beg end))
 	  (when org-export-table-remove-special-lines
 	    (setq lines (org-table-clean-before-export lines)))
	  ;; make a formatting string to reflect aligment
	  (setq olines lines)
	  (while (and (not line-fmt) (setq line (pop olines)))
	    (unless (string-match "^[ \t]*|-" line)
	      (setq fields (org-split-string line "[ \t]*|[ \t]*"))
	      (setq fnum (make-vector (length fields) 0))
	      (setq line-fmt
		    (mapconcat
		     (lambda (x)
		       (setq gr (pop org-table-colgroup-info))
		       (format "%s%%s%s"
			       (cond ((eq gr ':start)
				      (prog1 (if colgropen "|" "")
					(setq colgropen t)))
				     ((eq gr ':startend)
				      (prog1 (if colgropen "|" "|")
					(setq colgropen nil)))
				     (t ""))
			       (if (memq gr '(:end :startend))
				   (progn (setq colgropen nil) "|")
				 "")))
		     fnum ""))))
	  ;; fix double || in line-fmt
	  (setq line-fmt (replace-regexp-in-string "||" "|" line-fmt))
	  ;; maybe remove the first and last "|"
	  (when (and (not org-export-latex-tables-column-borders)
		     (string-match "^\\(|\\)?\\(.+\\)|$" line-fmt))
	    (setq line-fmt (match-string 2 line-fmt)))
	  ;; format alignment
	  (setq align (apply 'format
			     (cons line-fmt
				   (mapcar (lambda (x) (if x "r" "l"))
					   org-table-last-alignment))))
	  ;; prepare the table to send to orgtbl-to-latex
	  (setq lines
		(mapcar
		 (lambda(elem)
		   (or (and (string-match "[ \t]*|-+" elem) 'hline)
		       (split-string (org-trim elem) "|" t)))
		 lines))
    	  (when insert
	    (insert (orgtbl-to-latex
		     lines `(:tstart ,(concat "\\begin{tabular}{" align "}")))
		    "\n\n")))))))

(defun org-export-latex-fontify ()
  "Convert fontification to LaTeX."
  (goto-char (point-min))
  (while (re-search-forward org-emph-re nil t)
    ;; The match goes one char after the *string*
    (let ((emph (assoc (match-string 3)
		       org-export-latex-emphasis-alist))
	  rpl)
      (unless (get-text-property (1- (point)) 'org-protected)
	(setq rpl (concat (match-string 1)
			  (format (org-export-latex-protect-char-in-string
				   '("\\" "{" "}") (cadr emph))
				  (match-string 4))
			  (match-string 5)))
	(if (caddr emph)
	    (setq rpl (org-export-latex-protect-string rpl)))
	(replace-match rpl t t)))
    (backward-char)))

(defun org-export-latex-links ()
  ;; Make sure to use the LaTeX hyperref and graphicx package
  ;; or send some warnings.
  "Convert links to LaTeX."
  (goto-char (point-min))
  (while (re-search-forward org-bracket-link-analytic-regexp nil t)
    (org-if-unprotected
     (goto-char (match-beginning 0))
     (let* ((re-radio org-export-latex-all-targets-re)
	    (remove (list (match-beginning 0) (match-end 0)))
	    (type (match-string 2))
	    (raw-path (match-string 3))
	    (full-raw-path (concat (match-string 1) raw-path))
	    (desc (match-string 5))
	    imgp radiop
	    ;; define the path of the link
	    (path (cond
		   ((member type '("http" "https" "ftp"))
		    (concat type ":" raw-path))
		   ((and re-radio (string-match re-radio raw-path))
 		    (setq radiop t))
		   ((equal type "mailto")
		    (concat type ":" raw-path))
		   ((equal type "file")
		    (if (and (or (org-file-image-p (expand-file-name raw-path))
				 (string-match "\\.eps$" raw-path))
			     (equal desc full-raw-path))
			(setq imgp t)
		      (progn (when (string-match "\\(.+\\)::.+" raw-path)
			       (setq raw-path (match-string 1 raw-path)))
			     (if (file-exists-p raw-path)
				 (concat type "://" (expand-file-name raw-path))
			       (concat type "://" (org-export-directory
						   :LaTeX org-export-latex-options-plist)
				       raw-path))))))))
       ;; process with link inserting
       (apply 'delete-region remove)
       (cond ((and imgp (plist-get org-export-latex-options-plist :inline-images))
	      (insert (format "\\includegraphics[%s]{%s}"
			      ;; image option should be set be a comment line
			      org-export-latex-image-default-option
			      (expand-file-name raw-path))))
	     (radiop (insert (format "\\hyperref[%s]{%s}" raw-path desc)))
	     (path (insert (format "\\href{%s}{%s}" path desc)))
	     (t (insert "\\texttt{" desc "}")))))))

(defvar org-latex-entities)   ; defined below

(defun org-export-latex-preprocess ()
  "Clean stuff in the LaTeX export."

  ;; Preserve line breaks
  (goto-char (point-min))
  (while (re-search-forward "\\\\\\\\" nil t)
    (add-text-properties (match-beginning 0) (match-end 0)
			 '(org-protected t)))

  ;; Convert LaTeX to \LaTeX{}
  (goto-char (point-min))
  (let ((case-fold-search nil) rpl)
    (while (re-search-forward "\\([^+_]\\)LaTeX" nil t)
    (replace-match (org-export-latex-protect-string
		    (concat (match-string 1) "\\LaTeX{}")) t t)))

  ;; Convert horizontal rules
  (goto-char (point-min))
  (while (re-search-forward "^----+.$" nil t)
    (replace-match (org-export-latex-protect-string "\\hrule") t t))

  ;; Protect LaTeX commands like \commad[...]{...} or \command{...}
  (goto-char (point-min))
  (while (re-search-forward "\\\\[a-zA-Z]+\\(?:\\[.*\\]\\)?{.*}" nil t)
    (add-text-properties (match-beginning 0) (match-end 0)
			 '(org-protected t)))

  ;; Protect LaTeX entities
  (goto-char (point-min))
  (while (re-search-forward (regexp-opt org-latex-entities) nil t)
    (add-text-properties (match-beginning 0) (match-end 0)
			 '(org-protected t)))

  ;; Replace radio links
  (goto-char (point-min))
  (while (re-search-forward
	  (concat "<<<?" org-export-latex-all-targets-re
		  ">>>?\\((INVISIBLE)\\)?") nil t)
    (replace-match
     (org-export-latex-protect-string
      (format "\\label{%s}%s"(match-string 1)
	      (if (match-string 2) "" (match-string 1)))) t t))

  ;; Delete @<...> constructs
  ;; Thanks to Daniel Clemente for this regexp
  (goto-char (point-min))
  (while (re-search-forward "@<\\(?:[^\"\n]\\|\".*\"\\)*?>" nil t)
    (replace-match ""))

  ;; When converting to LaTeX, replace footnotes
  ;; FIXME: don't protect footnotes from conversion
  (when (plist-get org-export-latex-options-plist :footnotes)
    (goto-char (point-min))
    (while (re-search-forward "\\[[0-9]+\\]" nil t)
      (when (save-match-data
	      (save-excursion (beginning-of-line)
			      (looking-at "[^:|#]")))
	(let ((foot-beg (match-beginning 0))
	      (foot-end (match-end 0))
	      (foot-prefix (match-string 0))
	      footnote footnote-rpl)
	  (save-excursion
	    (when (search-forward foot-prefix nil t)
	      (replace-match "")
	      (let ((end (save-excursion
			   (if (re-search-forward "^$\\|^#.*$\\|\\[[0-9]+\\]" nil t)
			       (match-beginning 0) (point-max)))))
		(setq footnote (concat (org-trim (buffer-substring (point) end))
				       " ")) ; prevent last } being part of a link
		(delete-region (point) end))
	      (goto-char foot-beg)
	    (delete-region foot-beg foot-end)
	    (unless (null footnote)
	      (setq footnote-rpl (format "\\footnote{%s}" footnote))
	      (add-text-properties 0 10 '(org-protected t) footnote-rpl)
	      (add-text-properties (1- (length footnote-rpl))
				   (length footnote-rpl)
				   '(org-protected t) footnote-rpl)
	      (insert footnote-rpl)))))))

    ;; Replace footnote section tag for LaTeX
    (goto-char (point-min))
    (while (re-search-forward
	    (concat "^" footnote-section-tag-regexp) nil t)
      (replace-match ""))))

;;; List handling:

(defun org-export-latex-lists ()
  "Replace plain text lists in current buffer into LaTeX lists."
  "Convert lists to LaTeX."
  (goto-char (point-min))
  (while (re-search-forward org-export-latex-list-beginning-re nil t)
    (beginning-of-line)
    (insert (org-list-to-latex (org-list-parse-list t)) "\n")))

(defun org-list-parse-list (&optional delete)
  "Parse the list at point.
Return a list containing first level items as strings and
sublevels as a list of strings."
  (let ((start (org-list-item-begin))
	(end (org-list-end))
	output itemsep)
    (while (re-search-forward org-export-latex-list-beginning-re end t)
      (setq itemsep (if (save-match-data
			  (string-match "^[0-9]" (match-string 2)))
			"[0-9]+\\(?:\\.\\|)\\)" "[-+]"))
      (let* ((indent1 (match-string 1))
	     (nextitem (save-excursion
			 (save-match-data
			   (or (and (re-search-forward
				     (concat "^" indent1 itemsep " *?") end t)
				    (match-beginning 0)) end))))
	     (item (buffer-substring
		    (point)
		    (or (and (re-search-forward
			      org-export-latex-list-beginning-re end t)
			     (goto-char (match-beginning 0)))
			(goto-char end))))
	     (nextindent (match-string 1))
	     (item (org-trim item))
	     (item (if (string-match "^\\[.+\\]" item)
		       (replace-match "\\\\texttt{\\&}"
				      t nil item) item)))
	(push item output)
	(when (> (length nextindent)
		 (length indent1))
	  (narrow-to-region (point) nextitem)
	  (push (org-list-parse-list) output)
	  (widen))))
    (when delete (delete-region start end))
    (setq output (nreverse output))
    (push (if (string-match "^\\[0" itemsep)
	      'ordered 'unordered) output)))

(defun org-list-item-begin ()
  "Find the beginning of the list item and return its position."
  (save-excursion
    (if (not (or (looking-at org-export-latex-list-beginning-re)
		 (re-search-backward
		  org-export-latex-list-beginning-re nil t)))
	(progn (goto-char (point-min)) (point))
      (match-beginning 0))))

(defun org-list-end ()
  "Find the end of the list and return its position."
  (save-excursion
    (catch 'exit
      (while (or (looking-at org-export-latex-list-beginning-re)
		 (looking-at "^[ \t]+\\|^$"))
	(if (eq (point) (point-max))
	    (throw 'exit (point-max)))
	(forward-line 1))) (point)))

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
  "Send a tranformed version of this list to the receiver position.
With argument MAYBE, fail quietly if no transformation is defined for
this list."
  (interactive)
  (catch 'exit
    (unless (org-at-item-p) (error "Not at a list"))
    (save-excursion
      (goto-char (org-list-item-begin))
      (beginning-of-line 0)
      (unless (looking-at "#\\+ORGLST: *SEND +\\([a-zA-Z0-9_]+\\) +\\([^ \t\r\n]+\\)\\( +.*\\)?")
	(if maybe
	    (throw 'exit nil)
	  (error "Don't know how to transform this list"))))
    (let* ((name (match-string 1))
	   beg
	   (transform (intern (match-string 2)))
	   (txt (buffer-substring-no-properties
		 (org-list-item-begin)
		 (org-list-end)))
	   (list (org-list-parse-list)))
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

Valid parameters are

:ustart     String to start an unordered list
:uend       String to end an unordered list

:ostart     String to start an ordered list
:oend       String to end an ordered list

:splice     When set to t, return only list body lines, don't wrap
            them into :[u/o]start and :[u/o]end.  Default is nil.

:istart     String to start a list item
:iend       String to end a list item
:isep       String to separate items
:lsep       String to separate sublists"
  (interactive)
  (let* ((p params) sublist
	 (splicep (plist-get p :splice))
	 (ostart  (plist-get p :ostart))
	 (oend  (plist-get p :oend))
	 (ustart  (plist-get p :ustart))
	 (uend  (plist-get p :uend))
	 (istart  (plist-get p :istart))
	 (iend  (plist-get p :iend))
	 (isep  (plist-get p :isep))
	 (lsep  (plist-get p :lsep)))
    (let ((wrapper
	   (cond ((eq (car list) 'ordered)
		  (concat ostart "\n%s" oend "\n"))
		 ((eq (car list) 'unordered)
		  (concat ustart "\n%s" uend "\n"))))
	  rtn)
      (while (setq sublist (pop list))
	(cond ((symbolp sublist) nil)
	      ((stringp sublist)
	       (setq rtn (concat rtn istart sublist iend isep)))
	      (t
	       (setq rtn (concat rtn   ;; previous list
				 lsep  ;; list separator
				 (org-list-to-generic sublist p)
				 lsep  ;; list separator
				 )))))
      (format wrapper rtn))))

(defun org-list-to-latex (list)
  "Convert LIST into a LaTeX list."
  (org-list-to-generic
   list '(:splicep nil :ostart "\\begin{enumerate}" :oend "\\end{enumerate}"
                       :ustart "\\begin{itemize}" :uend "\\end{itemize}"
                       :istart "\\item " :iend ""
                       :isep "\n" :lsep "\n")))

(defun org-list-to-html (list)
  "Convert LIST into a HTML list."
  (org-list-to-generic
   list '(:splicep nil :ostart "<ol>" :oend "</ol>"
                       :ustart "<ul>" :uend "</ul>"
                       :istart "<li>" :iend "</li>"
                       :isep "\n" :lsep "\n")))

(defun org-list-to-texinfo (list)
  "Convert LIST into a Texinfo list."
  (org-list-to-generic
   list '(:splicep nil :ostart "@itemize @minus" :oend "@end itemize"
                       :ustart "@enumerate" :uend "@end enumerate"
                       :istart "@item\n" :iend ""
                       :isep "\n" :lsep "\n")))

(defconst org-latex-entities
 '("\\!"
   "\\'"
   "\\+"
   "\\,"
   "\\-"
   "\\:"
   "\\;"
   "\\<"
   "\\="
   "\\>"
   "\\Huge"
   "\\LARGE"
   "\\Large"
   "\\Styles"
   "\\\\"
   "\\`"
   "\\addcontentsline"
   "\\address"
   "\\addtocontents"
   "\\addtocounter"
   "\\addtolength"
   "\\addvspace"
   "\\alph"
   "\\appendix"
   "\\arabic"
   "\\author"
   "\\begin{array}"
   "\\begin{center}"
   "\\begin{description}"
   "\\begin{enumerate}"
   "\\begin{eqnarray}"
   "\\begin{equation}"
   "\\begin{figure}"
   "\\begin{flushleft}"
   "\\begin{flushright}"
   "\\begin{itemize}"
   "\\begin{list}"
   "\\begin{minipage}"
   "\\begin{picture}"
   "\\begin{quotation}"
   "\\begin{quote}"
   "\\begin{tabbing}"
   "\\begin{table}"
   "\\begin{tabular}"
   "\\begin{thebibliography}"
   "\\begin{theorem}"
   "\\begin{titlepage}"
   "\\begin{verbatim}"
   "\\begin{verse}"
   "\\bf"
   "\\bf"
   "\\bibitem"
   "\\bigskip"
   "\\cdots"
   "\\centering"
   "\\circle"
   "\\cite"
   "\\cleardoublepage"
   "\\clearpage"
   "\\cline"
   "\\closing"
   "\\dashbox"
   "\\date"
   "\\ddots"
   "\\dotfill"
   "\\em"
   "\\fbox"
   "\\flushbottom"
   "\\fnsymbol"
   "\\footnote"
   "\\footnotemark"
   "\\footnotesize"
   "\\footnotetext"
   "\\frac"
   "\\frame"
   "\\framebox"
   "\\hfill"
   "\\hline"
   "\\hrulespace"
   "\\hspace"
   "\\huge"
   "\\hyphenation"
   "\\include"
   "\\includeonly"
   "\\indent"
   "\\input"
   "\\it"
   "\\kill"
   "\\label"
   "\\large"
   "\\ldots"
   "\\line"
   "\\linebreak"
   "\\linethickness"
   "\\listoffigures"
   "\\listoftables"
   "\\location"
   "\\makebox"
   "\\maketitle"
   "\\mark"
   "\\mbox"
   "\\medskip"
   "\\multicolumn"
   "\\multiput"
   "\\newcommand"
   "\\newcounter"
   "\\newenvironment"
   "\\newfont"
   "\\newlength"
   "\\newline"
   "\\newpage"
   "\\newsavebox"
   "\\newtheorem"
   "\\nocite"
   "\\nofiles"
   "\\noindent"
   "\\nolinebreak"
   "\\nopagebreak"
   "\\normalsize"
   "\\onecolumn"
   "\\opening"
   "\\oval"
   "\\overbrace"
   "\\overline"
   "\\pagebreak"
   "\\pagenumbering"
   "\\pageref"
   "\\pagestyle"
   "\\par"
   "\\parbox"
   "\\put"
   "\\raggedbottom"
   "\\raggedleft"
   "\\raggedright"
   "\\raisebox"
   "\\ref"
   "\\rm"
   "\\roman"
   "\\rule"
   "\\savebox"
   "\\sc"
   "\\scriptsize"
   "\\setcounter"
   "\\setlength"
   "\\settowidth"
   "\\sf"
   "\\shortstack"
   "\\signature"
   "\\sl"
   "\\small"
   "\\smallskip"
   "\\sqrt"
   "\\tableofcontents"
   "\\telephone"
   "\\thanks"
   "\\thispagestyle"
   "\\tiny"
   "\\title"
   "\\tt"
   "\\twocolumn"
   "\\typein"
   "\\typeout"
   "\\underbrace"
   "\\underline"
   "\\usebox"
   "\\usecounter"
   "\\value"
   "\\vdots"
   "\\vector"
   "\\verb"
   "\\vfill"
   "\\vline"
   "\\vspace")
 "A list of LaTeX commands to be protected when performing conversion.")

(provide 'org-export-latex)

;; arch-tag: 23c2b87d-da04-4c2d-ad2d-1eb6487bc3ad
;;; org-export-latex.el ends here
