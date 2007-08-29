;;; org-export-latex.el --- LaTeX exporter for Org-mode
;; Copyright (C) 2007 Free Software Foundation, Inc.
;;
;; Author: Bastien Guerry <bzg AT altern DOT org>
;; Keywords: org organizer latex export convert
;; X-URL: <http://www.cognition.ens.fr/~guerry/u/org-export-latex.el>
;;
;; This file is part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; This library is a LaTeX exporter for org-mode.
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

;;; History:
;; 
;; I started this piece of code in may 2007. Special thanks to Carsten
;; Dominik for helping me on this.
;; 

;;; Code:

(require 'org)
(require 'footnote)

(defvar org-latex-options-plist nil)
(defvar org-latex-todo-keywords-1 nil)
(defvar org-latex-all-targets-regexp nil)
(defvar org-latex-add-level 0)
(defvar org-latex-sectioning-depth 0)

(defvar org-latex-special-string-regexps
  '(org-ts-regexp
    org-scheduled-string
    org-deadline-string
    org-clock-string)
  "A list of regexps to convert as special keywords.")

(defcustom org-export-latex-sectioning-alist
  '((1 "\\section{%s}" "\\section*{%s}")
    (2 "\\subsection{%s}" "\\subsection*{%s}")
    (3 "\\subsubsection{%s}" "\\subsubsection*{%s}")
    (4 "\\paragraph{%s}" "\\paragraph*{%s}")
    (5 "\\subparagraph{%s}" "\\subparagraph*{%s}"))
  "Alist of LaTeX commands for inserting sections.
Here is the structure of each cell:

  \(level unnumbered-section numbered-section\)

The %s formatter will be replaced by the title of the section."
  :group 'org-export-latex
  :type 'alist)

(defcustom org-export-latex-emphasis-alist
  '(("*" "\\textbf{%s}")
    ("/" "\\emph{%s}")
    ("_" "\\underline{%s}")
    ("+" "\\texttt{%s}")
    ("=" "\\texttt{%s}"))
  "Alist of LaTeX expressions to convert emphasis fontifiers."
  :group 'org-export-latex
  :type 'alist)

(defcustom org-export-latex-preamble
  "\\documentclass[11pt,a4paper]{article}
\\usepackage[utf8]{inputenc}
\\usepackage[T1]{fontenc}
\\usepackage{hyperref}"
  "Preamble to be inserted at the very beginning of the LaTeX export."
  :group 'org-export-latex
  :type 'string)

(defcustom org-export-latex-date-format nil
  "Format string for \\date{...}."
  :group 'org-export-latex
  :type 'string)

(defcustom org-export-latex-packages-alist nil
  "Alist of packages to be inserted in the preamble.
Each cell is of the forma \( option . package \).

For example:

\(setq org-export-latex-packages-alist
      '((\"french\" \"babel\"))"
  :group 'org-export-latex
  :type 'alist)

(defcustom org-export-latex-low-levels 'description
  "Choice for converting sections that are below the current
admitted level of sectioning. This can be either nil (ignore the
sections), 'description (convert them as description lists) or a
string to be used instead of \\section{%s} (a %s for inserted the
headline is mandatory)."
  :group 'org-export-latex
  :type '(choice (const :tag "Ignore" nil)
		 (symbol :tag "Convert as descriptive list" description)
		 (string :tag "Use a section string" :value "\\subparagraph{%s}")))

(defcustom org-export-latex-remove-from-headines
  '(:todo t :priority t :tags t)
  "A plist of keywords to remove from headlines.
Non-nil means remove this keyword type from the headline.

Don't remove the keys, just change their values."
  :type 'plist
  :group 'org-export-latex)

(defcustom org-export-latex-quotation-marks-convention "en"
  "Convention for conversion of the quotation marks.
This value is overriden by any infile language setup."
  :group 'org-export-latex
  :type '(choice (string :tag "english" "en")
		 (string :tag "french" "fr")))

(defcustom org-export-latex-image-default-option "width=10em"
  "Default option for images."
  :group 'org-export-latex
  :type '(string))

(defcustom org-export-latex-coding-system nil
  "Coding system for the exported LaTex file."
  :group 'org-export-latex
  :type 'coding-system)

;; FIXME Do we want this one?
;; (defun org-export-as-latex-and-open (arg) ...)

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
LaTeX. This can be used in any buffer. For example, you could
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
				to-buffer body-only)
  "Export current buffer to a LaTeX file."
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
  (org-export-latex-set-initial-vars ext-plist)
  (let* ((opt-plist org-latex-options-plist)
	 (filename (concat (file-name-as-directory
			    (org-export-directory :LaTeX ext-plist))
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
	 (region-p (org-region-active-p))
	 (odd org-odd-levels-only)
	 (preamble (org-export-latex-make-preamble opt-plist))
	 (skip (plist-get opt-plist :skip-before-1st-heading))
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
	  ;; FIXME Use org-cleaned-string-for-export instead, only when
	  ;; everyone uses Org >5.04
	  (org-latex-cleaned-string-for-export
	   region :for-html nil
		  :comments nil
		  :for-LaTeX t
		  :skip-before-1st-heading nil
		  :LaTeX-fragments nil)))
    (set-buffer buffer)
    (erase-buffer)
    (unless body-only (insert preamble))
    (when text (insert (org-export-latex-content text) "\n\n"))
    (unless skip (insert first-lines))

    ;; handle the case where the region does not begin with a section
    (when region-p
      (insert (with-temp-buffer
		(insert string-for-export)
		(org-export-latex-first-lines))))

    (org-export-latex-global
     (with-temp-buffer
       (insert string-for-export)
       (goto-char (point-min))
       (re-search-forward "^\\(\\*+\\) " nil t)
       (let* ((asters (length (match-string 1)))
	      (level (if odd (- asters 2) (- asters 1))))
	 (setq org-latex-add-level
	       (if odd (1- (/ (1+ asters) 2)) (1- asters)))
	 (org-export-latex-parse-global level odd))))

    (unless body-only (insert "\n\\end{document}"))
    (or to-buffer (write-file filename))
    (goto-char (point-min))
    (message "Exporting to LaTeX...done")
    (if (eq to-buffer 'string)
	(prog1 (buffer-substring (point-min) (point-max))
	  (kill-buffer (current-buffer)))
      (current-buffer))))

(defun org-export-latex-set-initial-vars (ext-plist)
  "Store org local variables required for LaTeX export.
EXT-PLIST is an optional additional plist."
  (setq org-latex-todo-keywords-1 org-todo-keywords-1
	org-latex-all-targets-regexp
	(org-make-target-link-regexp (org-all-targets))
	org-latex-options-plist
	(org-combine-plists (org-default-export-plist) ext-plist
			    (org-infile-export-plist))
	org-latex-sectioning-depth
	(let ((hl-levels (plist-get org-latex-options-plist :headline-levels))
	      (sec-depth (length org-export-latex-sectioning-alist)))
	      ;; Fall back on org-export-latex-sectioning-alist length if
	      ;; headline-levels goes beyond it
	  (if (> hl-levels sec-depth) sec-depth hl-levels))))

(defun org-export-latex-make-preamble (opt-plist)
  "Make the LaTeX preamble and return it as a string.
Argument OPT-PLIST is the options plist for current buffer."
  (let ((toc (plist-get opt-plist :table-of-contents)))
    (format (concat org-export-latex-preamble
     "
%s

\\begin{document}

\\title{%s}
%s
%s
\\maketitle
%s
%s
")
	    (if org-export-latex-packages-alist
		(mapconcat (lambda(p)
			     (if (equal "" (car p))
				 (format "\\usepackage{%s}" (cadr p))
			       (format "\\usepackage[%s]{%s}"
				       (car p) (cadr p))))
			   org-export-latex-packages-alist "\n") "")
	    (or (plist-get opt-plist :title)
		(and (not
		      (plist-get opt-plist :skip-before-1st-heading))
		     (org-export-grab-title-from-buffer))
		(and buffer-file-name
		     (file-name-sans-extension
		      (file-name-nondirectory buffer-file-name)))
		"UNTITLED")
	    (if (plist-get opt-plist :author-info)
		(format "\\author{%s}" 
			(or (plist-get opt-plist :author) user-full-name))
	      (format "%%\\author{%s}"
		      (or (plist-get opt-plist :author) user-full-name)))
	    (if (plist-get opt-plist :timestamps)
		(format "\\date{%s}"
			(format-time-string (or org-export-latex-date-format
						(car org-time-stamp-formats))))
	      "%\\date{}")
	    (if (and (plist-get opt-plist :section-numbers) toc)
		(format "\\setcounter{tocdepth}{%s}"
			(plist-get opt-plist :headline-levels)) "")
	    (if (and (plist-get opt-plist :section-numbers) toc)
		"\\tableofcontents" ""))))

(defun org-export-latex-first-lines (&optional comments)
  "Export the first lines before first headline.
COMMENTS is either nil to replace them with the empty string or a
formatting string like %%%%s if we want to comment them out."
  (save-excursion
    (goto-char (point-min))
    (let* ((end (if (re-search-forward "^\\*" nil t)
		    (goto-char (match-beginning 0))
		  (goto-char (point-max)))))
      (org-export-latex-content
       (org-latex-cleaned-string-for-export
	(buffer-substring (point-min) end)
	:for-html nil
	:for-LaTeX t
	:comments nil
	:skip-before-1st-heading nil
	:LaTeX-fragments nil)))))

(defun org-export-latex-parse-global (level odd)
  "Parse the current buffer recursively, starting at LEVEL.
If ODD is non-nil, assume the buffer only contains odd sections.
Return A list reflecting the document structure."
  (save-excursion
    (goto-char (point-min))
    (let* ((cnt 0) output
	   (depth org-latex-sectioning-depth))
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
  (mapc (lambda(x) (org-export-latex-subcontent x)) subcontent))

(defun org-export-latex-subcontent (subcontent)
  "Export each cell of SUBCONTENT to LaTeX."
  (let ((heading (org-export-latex-fontify-headline
		  (cdr (assoc 'heading subcontent))))
	(level (- (cdr (assoc 'level subcontent))
		  org-latex-add-level))
	(occur (number-to-string (cdr (assoc 'occur subcontent))))
	(content (cdr (assoc 'content subcontent)))
	(subcontent (cadr (assoc 'subcontent subcontent)))
	(num (plist-get org-latex-options-plist :section-numbers)))
    (cond 
     ;; Normal conversion
     ((<= level org-latex-sectioning-depth)
      (let ((sec (assoc level org-export-latex-sectioning-alist)))
	(insert (format (if num (cadr sec) (caddr sec)) heading) "\n"))
      (insert (org-export-latex-content content))
      (cond ((stringp subcontent) (insert subcontent))
	    ((listp subcontent) (org-export-latex-sub subcontent))))
     ;; At a level under the hl option: we can drop this subsection
     ((> level org-latex-sectioning-depth)
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

(defun org-export-latex-special-keywords-maybe (remove-list)
  "Maybe remove keywords depending on rules in REMOVE-LIST."
  (goto-char (point-min))
  (let ((re-todo (mapconcat 'identity org-latex-todo-keywords-1 "\\|")))
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
      (if (plist-get remove-list :tags)
	  (replace-match "")
	(replace-match (format "\\texttt{%s}" (match-string 0)) t t)))))

(defun org-export-latex-fontify-headline (headline)
  "Fontify special words in a HEADLINE."
  (with-temp-buffer
    ;; FIXME: org-inside-LaTeX-fragment-p doesn't work when the $...$ is at
    ;; the beginning of the buffer - inserting "\n" is safe here though.
    (insert "\n" headline)
    (goto-char (point-min))
    (org-export-latex-fontify)
    (org-export-latex-special-chars
     (plist-get org-latex-options-plist :sub-superscript))
    (org-export-latex-special-keywords-maybe
     org-export-latex-remove-from-headines)
    (org-export-latex-links)
    (org-trim (buffer-substring-no-properties (point-min) (point-max)))))

(defun org-export-latex-content (content)
  "Convert CONTENT string to LaTeX."
  (with-temp-buffer
   (insert content)
   (org-export-latex-quotation-marks)
   (when (plist-get org-latex-options-plist :emphasize)
     (org-export-latex-fontify))
   (org-export-latex-special-chars
    (plist-get org-latex-options-plist :sub-superscript))
   (org-export-latex-links)
   (org-export-latex-special-keywords)
   (org-export-latex-itemize)
   (org-export-latex-enumerate)
   (org-export-latex-tables
    (plist-get org-latex-options-plist :tables))
   (org-export-latex-fixed-width
    (plist-get org-latex-options-plist :fixed-width))
   (org-export-fix-invisible-strings)
   (buffer-substring (point-min) (point-max))))

(defun org-export-fix-invisible-strings ()
  "Comment out (INVISIBLE) warnings."
  (goto-char (point-min))
  (while (re-search-forward "(INVISIBLE)" nil t)
    (replace-match "%\\&")))

(defun org-export-latex-quotation-marks ()
  "Export question marks depending on language conventions.
Local definition of the language overrides
`org-export-latex-quotation-marks-convention' which overrides
`org-export-default-language'."
  (let* ((lang (or (plist-get org-latex-options-plist :language)
		    org-export-latex-quotation-marks-convention))
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
		(org-latex-protect rpl)
		(org-if-unprotected
		 (replace-match rpl t t))))) quote-rpl)))

;; | chars/string in Org   | normal environment    | math environment      |
;; |-----------------------+-----------------------+-----------------------|
;; | & # % $               | \& \# \% \$           | \& \# \% \$           |
;; | { } _ ^ \             | \ { \ } \_ \^ \\      | {  }  _  ^ \          |
;; |-----------------------+-----------------------+-----------------------|
;; | a_b and a^b           | $a_b$ and $a^b$       | a_b and a^b           |
;; | a_abc and a_{abc}     | $a_a$bc and $a_{abc}$ | a_abc and a_{abc}     |
;; | \tau and \mu          | $\tau$ and $\mu$      | \tau and \mu          |
;; |-----------------------+-----------------------+-----------------------|
;; | \_ \^                 | \_  \^                | \_  \^                |
;; | \(a=\mu\mbox{m}\)     | \(a=\mu\mbox{m}\)     | \(a=\mu\mbox{m}\)     |
;; | \[\beta^2-a=0\]       | \[\beta^2-a=0\]       | \[\beta^2-a=0\]       |
;; | $x=22\tau$            | $x=22\tau$            | $x=22\tau$            |
;; | $$\alpha=\sqrt{a^3}$$ | $$\alpha=\sqrt{a^3}$$ | $$\alpha=\sqrt{a^3}$$ |

(defun org-export-latex-special-chars (sub-superscript)
  "Export special characters to LaTeX.
If SUB-SUPERSCRIPT is non-nil, convert \\ and ^.
See the `org-export-latex.el' code for a complete conversion table."
  (goto-char (point-min))
  (mapc (lambda(c)
	  (goto-char (point-min))
	  (while (re-search-forward c nil t)
	    ;; Put the point where to check for org-protected
	    (unless (get-text-property (match-beginning 2) 'org-protected)
	      (cond ((member (match-string 2) '("\\$" "$"))
		    (if (equal (match-string 2) "\\$")
			(replace-match (concat (match-string 1) "$"
					       (match-string 3)) t t)
		      (replace-match (concat (match-string 1) "\\$"
					     (match-string 3)) t t)))
		   ((member (match-string 2) '("&" "#" "%"))
		    (if (equal (match-string 1) "\\")
			(replace-match (match-string 2) t t)
		      (replace-match (concat (match-string 1) "\\"
					     (match-string 2)) t t)))
		   ((equal (match-string 2) "~")
		    (unless (get-text-property 0 'org-protected (match-string 2))
		      (if (equal (match-string 1) "\\") nil
			(replace-match 
			 (org-latex-protect
			  (concat (match-string 1) "\\textasciitilde{}")) t t))))
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
 	  "\\([a-za-z0-9]+\\|[ \t\n]\\|\\\\\\)\\(_\\|\\^\\)\\([a-za-z0-9]+\\|[ \t\n]\\|[:punct:]\\|{[a-za-z0-9]+}\\|([a-za-z0-9]+)\\)"
	  "\\(.\\|^\\)\\(\\\\\\)\\([ \t\n]\\|[a-za-z&#%{}]+\\)"
	  "\\(.\\|^\\)\\(&\\)"
 	  "\\(.\\|^\\)\\(#\\)"
 	  "\\(.\\|^\\)\\(%\\)"
 	  "\\(.\\|^\\)\\({\\)"
	  "\\(.\\|^\\)\\(}\\)"
	  "\\(.\\|^\\)\\(~\\)")))

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
	 (cond ((get-text-property 0 'org-protected char)
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
	  (t (concat string-before char string-after))))
	(t (concat string-before "\\" char string-after))))

(defun org-export-latex-treat-backslash-char (string-before string-after)
  "Convert the \"$\" special character to LaTeX.
The conversion is made depending of STRING-BEFORE and STRING-AFTER."
  (cond ((member (list string-after) org-html-entities)
	 ;; backslash is part of a special entity (like "\alpha")
	 (concat string-before "$\\"
		 (or (cdar (member (list string-after) org-html-entities))
		     string-after) "$"))
	((and (not (string-match "^[ \n\t]" string-after))
	      (not (string-match "[ \n\t]\\'" string-before)))
	 ;; backslash is inside a word
	 (concat string-before "$\\backslash$" string-after))
	((not (or (equal string-after "")
		  (string-match "^[ \t\n]" string-after)))
	 ;; backslash might escape a character (like \#) or a user TeX
	 ;; macro (like \setcounter)
	 (concat string-before "\\" string-after))
	((and (string-match "^[ \t\n]" string-after)
	      (string-match "[ \t\n]\\'" string-before))
	 ;; backslash is alone, convert it to $\backslash$
	 (concat string-before "$\\backslash$" string-after))
	(t (concat string-before "$\\backslash$" string-after))))

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

(defun org-export-latex-tables (opt)
  "When OPT is non-nil convert tables to LaTeX."
  (goto-char (point-min))
  (while (re-search-forward "^\\([ \t]*\\)|" nil t)
    ;; Re-align the table to update org-table-last-alignment
    (save-excursion (save-match-data (org-table-align)))
    (let (tbl-list
	  (beg (match-beginning 0))
	  (end (save-excursion
		 (re-search-forward
		  (concat "^" (regexp-quote (match-string 1))
			  "[^|]\\|\\'") nil t) (match-beginning 0))))
      (beginning-of-line)
      (while (not (eq end (point)))
	(if (looking-at "[ \t]*|\\([^-|].+\\)|[ \t]*$")
	    (push (split-string (org-trim (match-string 1)) "|") tbl-list)
	  (push 'hline tbl-list))
	(forward-line))
      ;; comment region out instead of deleting it ?
      (apply 'delete-region (list beg end))
      (when opt (insert (orgtbl-to-latex (nreverse tbl-list) 
					 nil) "\n\n")))))

(defun org-export-latex-special-keywords ()
  "Convert special keywords to LaTeX.
Regexps are those from `org-latex-special-string-regexps'."
  (let ((rg org-latex-special-string-regexps) r)
    (while (setq r (pop rg))
      (goto-char (point-min))
      (while (re-search-forward (eval r) nil t)
	(replace-match (format "\\\\texttt{%s}" (match-string 0)) t)))))

;; FIXME - we need better implementation for nested lists
(defun org-export-latex-list (srch0 srch1 srch2 rpl0 rpl1)
  "Convert lists to LaTeX."
  (goto-char (point-min))
  (while (re-search-forward srch0 nil t)
    (let* ((beg (match-beginning 0))
	   (prefix (regexp-quote (match-string 1)))
	   (end-string (when (re-search-forward srch1 nil t)
			 (match-string 0))))
      (goto-char beg) (insert rpl0)
      (while (re-search-forward
	      (concat "^" prefix srch2)
	      (if (not end-string)
		  (point-max)
		(save-match-data
		  (save-excursion
		    (re-search-forward
		     (regexp-quote end-string) nil t)))) t)
	(replace-match 
	 (concat "\\item " 
		 (if (match-string 1) 
		     (format "\\texttt{%s}" (match-string 1))))
	 t t))
      (goto-char (if end-string
		     (progn (re-search-forward
			     (regexp-quote end-string) nil t)
			    (match-beginning 0))
		   (point-max)))
      (skip-chars-backward "\n") (forward-line 2)
      (insert rpl1))))

(defun org-export-latex-itemize ()
  "Convert item list to LaTeX."
  (org-export-latex-list
   "^\\([ \t]*\\)-"
   "^[^ \n\t-]+.*$"
   "- ?\\(\\[.+\\]\\)?"
   "\\begin{itemize}\n"
   "\\end{itemize}\n"))

(defun org-export-latex-enumerate ()
  "Convert numeric list to LaTeX."
  (org-export-latex-list
   "^\\([ \t]*\\)[0-9]+[\.)] \\(\\[.+\\]\\)? ?"
   "^[^ \n\t0-9]+.*$"
   "[0-9]+[\.)] ?\\(\\[.+\\]\\)?"
   "\\begin{enumerate}\n"
   "\\end{enumerate}\n"))

(defun org-export-latex-fontify ()
  "Convert fontification to LaTeX."
  (goto-char (point-min))
  (while (re-search-forward org-emph-re nil t)
    ;; The match goes one char after the *string*
    (unless (get-text-property (1- (point)) 'org-protected)
      (replace-match
       (concat (match-string 1)
	       (format
		(org-export-latex-protect-char-in-string
		 '("\\" "{" "}")
		 (cadr (assoc (match-string 3)
			      org-export-latex-emphasis-alist)))
		(match-string 4))
	       (match-string 5)) t t)
      (backward-char))))

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

(defun org-export-latex-links ()
  ;; Make sure to use the LaTeX hyperref and graphicx package
  ;; or send some warnings.
  "Convert links to LaTeX."
  (goto-char (point-min))
  (while (re-search-forward org-bracket-link-analytic-regexp nil t)
    (org-if-unprotected
     (goto-char (match-beginning 0))
     (let* ((re-radio org-latex-all-targets-regexp)
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
						   :LaTeX org-latex-options-plist)
				       raw-path))))))))
       ;; process with link inserting
       (apply 'delete-region remove)
       (cond ((and imgp (plist-get org-latex-options-plist :inline-images))
	      (insert (format "\\includegraphics[%s]{%s}"
			      ;; image option should be set be a comment line
			      org-export-latex-image-default-option
			      (expand-file-name raw-path))))
	     ;; FIXME: what about caption? image properties?
	     (radiop (insert (format "\\hyperref[%s]{%s}" raw-path desc)))
	     (path (insert (format "\\href{%s}{%s}" path desc)))
	     (t (insert "\\texttt{" desc "}")))))))


(defun org-latex-cleaned-string-for-export (string &rest parameters)
  "Cleanup a buffer STRING so that links can be created safely."
  (interactive)
  (let* ((re-radio (and org-target-link-regexp
			(concat "\\([^<]\\)\\(" org-target-link-regexp "\\)")))
	 (re-plain-link (concat "\\([^[<]\\)" org-plain-link-re))
	 (re-angle-link (concat "\\([^[]\\)" org-angle-link-re))
	 (re-archive (concat ":" org-archive-tag ":"))
	 (re-quote (concat "^\\*+[ \t]+" org-quote-string "\\>"))
	 (htmlp (plist-get parameters :for-html))
	 (latexp (plist-get parameters :for-LaTeX))
	 (commentsp (plist-get parameters :comments))
	 (inhibit-read-only t)
	 (outline-regexp "\\*+ ")
	 a b xx
	 rtn p)
    (save-excursion
      (set-buffer (get-buffer-create " org-mode-tmp"))
      (erase-buffer)
      (insert string)
      ;; Remove license-to-kill stuff
      (while (setq p (text-property-any (point-min) (point-max)
					:org-license-to-kill t))
	(delete-region p (next-single-property-change p :org-license-to-kill)))

      (let ((org-inhibit-startup t)) (org-mode))
      (untabify (point-min) (point-max))

      ;; Get the correct stuff before the first headline
      (when (plist-get parameters :skip-before-1st-heading)
	(goto-char (point-min))
	(when (re-search-forward "^\\*+[ \t]" nil t)
	  (delete-region (point-min) (match-beginning 0))
	  (goto-char (point-min))
	  (insert "\n")))
      (when (plist-get parameters :add-text)
	(goto-char (point-min))
	(insert (plist-get parameters :add-text) "\n"))

      ;; Get rid of archived trees
      (when (not (eq org-export-with-archived-trees t))
	(goto-char (point-min))
	(while (re-search-forward re-archive nil t)
	  (if (not (org-on-heading-p t))
	      (org-end-of-subtree t)
	    (beginning-of-line 1)
	    (setq a (if org-export-with-archived-trees
			(1+ (point-at-eol)) (point))
		  b (org-end-of-subtree t))
	    (if (> b a) (delete-region a b)))))

      ;; Get rid of property drawers
      (unless org-export-with-property-drawer
	(goto-char (point-min))
	(while (re-search-forward "^[ \t]*:PROPERTIES:[ \t]*\n\\([^@]*?\n\\)?[ \t]*:END:[ \t]*\n" nil t)
	  (replace-match "")))

      ;; Find targets in comments and move them out of comments,
      ;; but mark them as targets that should be invisible
      (goto-char (point-min))
      (while (re-search-forward "^#.*?\\(<<<?[^>\r\n]+>>>?\\).*" nil t)
	(replace-match "\\1(INVISIBLE)"))

      ;; Specific LaTeX cleaning
      (when latexp
	(require 'org-export-latex nil t)
	(org-export-latex-cleaned-string))

      ;; Protect stuff from HTML processing
      (goto-char (point-min))
      (let ((formatters `((,htmlp "HTML" "BEGIN_HTML" "END_HTML"))) fmt)
	(while (re-search-forward "^[ \t]*:.*\\(\n[ \t]*:.*\\)*" nil t)
	  (add-text-properties (match-beginning 0) (match-end 0)
			       '(org-protected t)))
	(while formatters
	  (setq fmt (pop formatters))
	  (when (car fmt)
	    (goto-char (point-min))
	    (while (re-search-forward (concat "^#\\+" (cadr fmt) 
					      ":[ \t]*\\(.*\\)") nil t)
	      (replace-match "\\1" t)
	      (add-text-properties
	       (point-at-bol) (min (1+ (point-at-eol)) (point-max))
	       '(org-protected t))))
	  (goto-char (point-min))
	  (while (re-search-forward
		  (concat "^#\\+" 
			  (caddr fmt) "\\>.*\\(\\(\n.*\\)*?\n\\)#\\+"
			  (cadddr fmt) "\\>.*\n?") nil t)
	    (if (car fmt)
		(add-text-properties (match-beginning 1) (1+ (match-end 1))
				     '(org-protected t))
	      (delete-region (match-beginning 0) (match-end 0))))
	  (goto-char (point-min))
	  (while (re-search-forward re-quote nil t)
	    (goto-char (match-beginning 0))
	    (end-of-line 1)
	    (add-text-properties (point) (org-end-of-subtree t)
				 '(org-protected t)))))

      ;; Remove or replace comments
      ;; If :comments is set, use this char for commenting out comments and
      ;; protect them. otherwise delete them
      (goto-char (point-min))
      (while (re-search-forward "^#\\(.*\n?\\)" nil t)
	(if commentsp
	    (progn (add-text-properties
		    (match-beginning 0) (match-end 0) '(org-protected t))
		   (replace-match (format commentsp (match-string 1)) t t))
	  (replace-match "")))

      ;; Find matches for radio targets and turn them into internal links
      (goto-char (point-min))
      (when re-radio
	(while (re-search-forward re-radio nil t)
	  (org-if-unprotected
	   (replace-match "\\1[[\\2]]"))))

      ;; Find all links that contain a newline and put them into a single line
      (goto-char (point-min))
      (while (re-search-forward "\\(\\(\\[\\|\\]\\)\\[[^]]*?\\)[ \t]*\n[ \t]*\\([^]]*\\]\\(\\[\\|\\]\\)\\)" nil t)
	(org-if-unprotected
	 (replace-match "\\1 \\3")
	 (goto-char (match-beginning 0))))

      ;; Convert LaTeX fragments to images
      (when (plist-get parameters :LaTeX-fragments)
	(org-format-latex
	 (concat "ltxpng/" (file-name-sans-extension
			    (file-name-nondirectory
			     org-current-export-file)))
	 org-current-export-dir nil "Creating LaTeX image %s"))
      (message "Exporting...")

      ;; Normalize links: Convert angle and plain links into bracket links
      ;; Expand link abbreviations
      (goto-char (point-min))
      (while (re-search-forward re-plain-link nil t)
	(goto-char (1- (match-end 0)))
	(org-if-unprotected
	 (let* ((s (concat (match-string 1) "[[" (match-string 2)
			   ":" (match-string 3) "]]")))
	   ;; added 'org-protected property to links
	   (add-text-properties 0 (length s) '(org-protected t) s)
	   (replace-match s t t))))
      (goto-char (point-min))
      (while (re-search-forward re-angle-link nil t)
	(goto-char (1- (match-end 0)))
	(org-if-unprotected
	 (let* ((s (concat (match-string 1) "[[" (match-string 2)
			   ":" (match-string 3) "]]")))
	   (add-text-properties 0 (length s) '(org-protected t) s)
	   (replace-match s t t))))
      (goto-char (point-min))
      (while (re-search-forward org-bracket-link-regexp nil t)
	(org-if-unprotected
	 (let* ((s (concat "[[" (setq xx (save-match-data
					   (org-link-expand-abbrev (match-string 1))))
			   "]"
			   (if (match-end 3)
			       (match-string 2)
			     (concat "[" xx "]"))
			   "]")))
	   (add-text-properties 0 (length s) '(org-protected t) s)
	   (replace-match s t t))))

      ;; Find multiline emphasis and put them into single line
      (when (plist-get  parameters :emph-multiline)
	(goto-char (point-min))
	(while (re-search-forward org-emph-re nil t)
	  (if (not (= (char-after (match-beginning 3))
		      (char-after (match-beginning 4))))
	      (org-if-unprotected
	       (subst-char-in-region (match-beginning 0) (match-end 0)
				     ?\n ?\  t)
	       (goto-char (1- (match-end 0))))
	    (goto-char (1+ (match-beginning 0))))))

      (setq rtn (buffer-string)))
    (kill-buffer " org-mode-tmp")
    rtn))

(defsubst org-latex-protect (string)
  (add-text-properties 0 (length string) '(org-protected t) string)
  string)

(defun org-export-latex-cleaned-string ()
  "Clean stuff in the LaTeX export."

  ;; preserve line breaks
  (goto-char (point-min))
  (while (re-search-forward "\\\\\\\\" nil t)
    (add-text-properties (match-beginning 0) (match-end 0)
			 '(org-protected t)))

  ;; convert LaTeX to @LaTeX{}
  (goto-char (point-min))
  (let ((case-fold-search nil) rpl)
    (while (re-search-forward "\\([^+_]\\)LaTeX" nil t)
    (replace-match (org-latex-protect 
		    (concat (match-string 1) "\\LaTeX{}")) t t)))

  ;; convert horizontal rules
  (goto-char (point-min))
  (while (re-search-forward "^----+.$" nil t)
    (replace-match (org-latex-protect "\\hrule") t t))

  ;; Remove COMMENT subtrees
  ;; What about QUOTE subtrees?
  (goto-char (point-min))
  (while (re-search-forward 
	  (concat "^\\*+ \\(" org-comment-string "\\)") 
	  nil t)
    (beginning-of-line)
    (org-cut-subtree))
  
  ;; protect LaTeX \commands{...}
  (goto-char (point-min))
  (while (re-search-forward "\\\\[a-z]+{.+}" nil t)
    (add-text-properties (match-beginning 0) (match-end 0)
			 '(org-protected t)))
  
  ;; Replace radio links
  (goto-char (point-min))
  (let ((search (concat "<<<?" org-latex-all-targets-regexp ">?>>")))
    (while (re-search-forward search nil t)
      (replace-match 
       (org-latex-protect (format "\\label{%s}" (match-string 1))) t t)))
  
  ;; delete @<br /> cookies
  (goto-char (point-min))
  (while (re-search-forward "@<[^<>\n]*>" nil t)
    (replace-match ""))
  
  ;; add #+BEGIN_LaTeX before any \begin{...}
  (goto-char (point-min))
  (while (re-search-forward "^ *\\\\begin{" nil t)
    (replace-match "#+BEGIN_LaTeX:\n\\&" t))
  
  ;; add #+END_LaTeX after any \end{...}
  (goto-char (point-min))
  (while (re-search-forward "^ *\\\\end{.+}.*$" nil t)
    (replace-match "\\&\n#+END_LaTeX" t))
  
  ;; When converting to LaTeX, replace footnotes
  ;; FIXME: don't protect footnotes from conversion
  (when (plist-get org-latex-options-plist :footnotes)
    (goto-char (point-min))
    (while (re-search-forward "\\[[0-9]+\\]" nil t)
      (when (save-match-data
	      (save-excursion (beginning-of-line)
			      (looking-at "[^:|]")))
	(let ((foot-beg (match-beginning 0))
	      (foot-end (match-end 0))
	      (foot-prefix (match-string 0))
	      footnote footnote-rpl)
	  (when (and (re-search-forward (regexp-quote foot-prefix) nil t))
	    (replace-match "")
	    (let ((end (save-excursion
			 (if (re-search-forward "^$\\|\\[[0-9]+\\]" nil t)
			     (match-beginning 0) (point-max)))))
	      (setq footnote (concat 
			      (org-trim (buffer-substring (point) end)) 
			      ;; FIXME stupid workaround for cases where 
			      ;; `org-bracket-link-analytic-regexp' matches
			      ;; }. as part of the link.
			      " "))
	      (delete-region (point) end)))
	  (goto-char foot-beg)
	  (delete-region foot-beg foot-end)
	  (setq footnote-rpl (format "\\footnote{%s}" footnote))
	  (add-text-properties 0 1 '(org-protected t) footnote-rpl)
	  (add-text-properties 9 10 '(org-protected t) footnote-rpl)
	  (add-text-properties (1- (length footnote-rpl))
			       (length footnote-rpl)
			       '(org-protected t) footnote-rpl)
	  (insert footnote-rpl))))
    
    ;; Replace footnote section tag for LaTeX
    (goto-char (point-min))
    (while (re-search-forward 
	    (concat "^" footnote-section-tag-regexp) nil t)
      (replace-match "")))
  
  ;; Protect stuff from LaTeX processing. 
  ;; We will get rid on this once org.el integrate org-export-latex.el
  ;; FIXME: #+LaTeX should be aware of the preceeding indentation in lists
  (goto-char (point-min))
  (let ((formatters `((,latexp "LaTeX" "BEGIN_LaTeX" "END_LaTeX"))) fmt)
    (while (re-search-forward "^[ \t]*:.*\\(\n[ \t]*:.*\\)*" nil t)
      (add-text-properties (match-beginning 0) (match-end 0)
			   '(org-protected t)))
    (while formatters
      (setq fmt (pop formatters))
      (when (car fmt)
	(goto-char (point-min))
	(while (re-search-forward (concat "^#\\+" (cadr fmt) 
					  ":[ \t]*\\(.*\\)") nil t)
	  (replace-match "\\1" t)
	  (add-text-properties
	   (point-at-bol) (min (1+ (point-at-eol)) (point-max))
	   '(org-protected t))))
      (goto-char (point-min))
      (while (re-search-forward
	      (concat "^#\\+" 
		      (caddr fmt) "\\>.*\\(\\(\n.*\\)*?\n\\)#\\+"
		      (cadddr fmt) "\\>.*\n?") nil t)
	(if (car fmt)
	    (add-text-properties (match-beginning 1) (1+ (match-end 1))
				 '(org-protected t))
	  (delete-region (match-beginning 0) (match-end 0))))
      (goto-char (point-min))
      (while (re-search-forward re-quote nil t)
	(goto-char (match-beginning 0))
	(end-of-line 1)
	(add-text-properties (point) (org-end-of-subtree t)
			     '(org-protected t))))))

(provide 'org-export-latex)

;; arch-tag: 23c2b87d-da04-4c2d-ad2d-1eb6487bc3ad
;;; org-export-latex.el ends here
