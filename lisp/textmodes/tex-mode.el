;;; tex-mode.el --- TeX, LaTeX, and SliTeX mode commands.

;; Copyright (C) 1985, 1986, 1989, 1992, 1994 Free Software Foundation, Inc.

;; Maintainer: FSF
;; Keywords: tex

;; Contributions over the years by William F. Schelter, Dick King,
;; Stephen Gildea, Michael Prange, Jacob Gore, and Edward M. Reingold.

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

;;; Code:

(require 'shell)
(require 'compile)

;;;###autoload
(defvar tex-shell-file-name nil
  "*If non-nil, the shell file name to run in the subshell used to run TeX.")

;;;###autoload
(defvar tex-directory "."
  "*Directory in which temporary files are left.
You can make this `/tmp' if your TEXINPUTS has no relative directories in it
and you don't try to apply \\[tex-region] or \\[tex-buffer] when there are
`\\input' commands with relative directories.")

;;;###autoload
(defvar tex-offer-save t
  "*If non-nil, ask about saving modified buffers before \\[tex-file] is run.")

;;;###autoload
(defvar tex-run-command "tex"
  "*Command used to run TeX subjob.
If this string contains an asterisk (`*'), that is replaced by the file name;
otherwise, the file name, preceded by blank, is added at the end.")

;;;###autoload
(defvar latex-run-command "latex"
  "*Command used to run LaTeX subjob.
If this string contains an asterisk (`*'), that is replaced by the file name;
otherwise, the file name, preceded by blank, is added at the end.")

(defvar standard-latex-block-names
      '("abstract"         "array"            "center"       "description"
        "displaymath"      "document"         "enumerate"    "eqnarray"
        "eqnarray*"        "equation"         "figure"       "figure*"
        "flushleft"        "flushright"       "itemize"      "letter"
        "list"             "minipage"         "picture"      "quotation"
        "quote"            "slide"            "sloppypar"     "tabbing"
        "table"            "table*"           "tabular"       "tabular*"
        "thebibliography"  "theindex*"        "titlepage"     "trivlist"
        "verbatim"         "verbatim*"        "verse")
  "Standard LaTeX block names.")

;;;###autoload
(defvar latex-block-names nil
  "*User defined LaTeX block names.
Combined with `standard-latex-block-names' for minibuffer completion.")

;;;###autoload
(defvar slitex-run-command "slitex"
  "*Command used to run SliTeX subjob.
If this string contains an asterisk (`*'), that is replaced by the file name;
otherwise, the file name, preceded by blank, is added at the end.")

;;;###autoload
(defvar tex-bibtex-command "bibtex"
  "*Command used by `tex-bibtex-file' to gather bibliographic data.
If this string contains an asterisk (`*'), that is replaced by the file name;
otherwise, the file name, preceded by blank, is added at the end.")

;;;###autoload
(defvar tex-dvi-print-command "lpr -d"
  "*Command used by \\[tex-print] to print a .dvi file.
If this string contains an asterisk (`*'), that is replaced by the file name;
otherwise, the file name, preceded by blank, is added at the end.")

;;;###autoload
(defvar tex-alt-dvi-print-command "lpr -d"
  "*Command used by \\[tex-print] with a prefix arg to print a .dvi file.
If this string contains an asterisk (`*'), that is replaced by the file name;
otherwise, the file name, preceded by blank, is added at the end.

If two printers are not enough of a choice, you can set the variable
`tex-alt-dvi-print-command' to an expression that asks what you want;
for example,

    (setq tex-alt-dvi-print-command
         '(format \"lpr -P%s\" (read-string \"Use printer: \")))

would tell \\[tex-print] with a prefix argument to ask you which printer to
use.")

;;;###autoload
(defvar tex-dvi-view-command nil
  "*Command used by \\[tex-view] to display a `.dvi' file.
If this string contains an asterisk (`*'), that is replaced by the file name;
otherwise, the file name, preceded by blank, is added at the end.

This can be set conditionally so that the previewer used is suitable for the
window system being used.  For example,

    (setq tex-dvi-view-command
          (if (eq window-system 'x) \"xdvi\" \"dvi2tty * | cat -s\"))

would tell \\[tex-view] to use xdvi under X windows and to use dvi2tty
otherwise.")

;;;###autoload
(defvar tex-show-queue-command "lpq"
  "*Command used by \\[tex-show-print-queue] to show the print queue.
Should show the queue(s) that \\[tex-print] puts jobs on.")

;;;###autoload
(defvar tex-default-mode 'plain-tex-mode
  "*Mode to enter for a new file that might be either TeX or LaTeX.
This variable is used when it can't be determined whether the file
is plain TeX or LaTeX or what because the file contains no commands.
Normally set to either `plain-tex-mode' or `latex-mode'.")

;;;###autoload
(defvar tex-open-quote "``"
  "*String inserted by typing \\[tex-insert-quote] to open a quotation.")

;;;###autoload
(defvar tex-close-quote "''"
  "*String inserted by typing \\[tex-insert-quote] to close a quotation.")

(defvar tex-last-temp-file nil
  "Latest temporary file generated by \\[tex-region] and \\[tex-buffer].
Deleted when the \\[tex-region] or \\[tex-buffer] is next run, or when the
tex shell terminates.")

(defvar tex-command nil
  "Command to run TeX.
The name of the file, preceded by a blank, will be added to this string.")

(defvar tex-trailer nil
  "String appended after the end of a region sent to TeX by \\[tex-region].")

(defvar tex-start-of-header nil
  "String used by \\[tex-region] to delimit the start of the file's header.")

(defvar tex-end-of-header nil
  "String used by \\[tex-region] to delimit the end of the file's header.")

(defvar tex-shell-cd-command "cd"
  "Command to give to shell running TeX to change directory.
The value of `tex-directory' is appended to this, separated by a space.")

(defvar tex-zap-file nil
  "Temporary file name used for text being sent as input to TeX.
Should be a simple file name with no extension or directory specification.")

(defvar tex-last-buffer-texed nil
  "Buffer which was last TeXed.")

(defvar tex-print-file nil
  "File name that \\[tex-print] prints.
Set by \\[tex-region], \\[tex-buffer], and \\[tex-file].")

(defvar tex-mode-syntax-table nil
  "Syntax table used while in TeX mode.")

(defun tex-define-common-keys (keymap)
  "Define the keys that we want defined both in TeX mode and in the TeX shell."
  (define-key keymap "\C-c\C-k" 'tex-kill-job)
  (define-key keymap "\C-c\C-l" 'tex-recenter-output-buffer)
  (define-key keymap "\C-c\C-q" 'tex-show-print-queue)
  (define-key keymap "\C-c\C-p" 'tex-print)
  (define-key keymap "\C-c\C-v" 'tex-view)

  (define-key keymap [menu-bar tex] (cons "TeX" (make-sparse-keymap "TeX")))

  (define-key keymap [menu-bar tex tex-kill-job] '("Tex Kill" . tex-kill-job))
  (define-key keymap [menu-bar tex tex-recenter-output-buffer]
    '("Tex Recenter" . tex-recenter-output-buffer))
  (define-key keymap [menu-bar tex tex-show-print-queue]
    '("Show Print Queue" . tex-show-print-queue))
  (define-key keymap [menu-bar tex tex-alt-print]
    '("Tex Print (alt printer)" . tex-alt-print))
  (define-key keymap [menu-bar tex tex-print] '("Tex Print" . tex-print))
  (define-key keymap [menu-bar tex tex-view] '("Tex View" . tex-view))
  )

(defvar tex-mode-map nil "Keymap for TeX mode.")

(if tex-mode-map 
    nil
  (setq tex-mode-map (make-sparse-keymap))
  (tex-define-common-keys tex-mode-map)
  (define-key tex-mode-map "\"" 'tex-insert-quote)
  (define-key tex-mode-map "\n" 'tex-terminate-paragraph)
  (define-key tex-mode-map "\C-c}" 'up-list)
  (define-key tex-mode-map "\C-c{" 'tex-insert-braces)
  (define-key tex-mode-map "\C-c\C-r" 'tex-region)
  (define-key tex-mode-map "\C-c\C-b" 'tex-buffer)
  (define-key tex-mode-map "\C-c\C-f" 'tex-file)
  (define-key tex-mode-map "\C-c\C-i" 'tex-bibtex-file)
  (define-key tex-mode-map "\C-c\C-o" 'tex-latex-block)
  (define-key tex-mode-map "\C-c\C-e" 'tex-close-latex-block)
  (define-key tex-mode-map "\C-c\C-u" 'tex-goto-last-unclosed-latex-block)
  (define-key tex-mode-map [menu-bar tex tex-bibtex-file]
    '("BibTeX File" . tex-bibtex-file))
  (define-key tex-mode-map [menu-bar tex tex-validate-region]
    '("Validate Region" . tex-validate-region))
  (define-key tex-mode-map [menu-bar tex validate-tex-buffer]
    '("Validate Buffer" . validate-tex-buffer))
  (define-key tex-mode-map [menu-bar tex tex-region]
    '("TeX Region" . tex-region))
  (define-key tex-mode-map [menu-bar tex tex-buffer]
    '("TeX Buffer" . tex-buffer))
  (define-key tex-mode-map [menu-bar tex tex-file] '("TeX File" . tex-file)))

(put 'tex-region 'menu-enable 'mark-active)
(put 'tex-validate-region 'menu-enable 'mark-active)
(put 'tex-print 'menu-enable '(stringp tex-print-file))
(put 'tex-alt-print 'menu-enable '(stringp tex-print-file))
(put 'tex-view 'menu-enable '(stringp tex-print-file))
(put 'tex-recenter-output-buffer 'menu-enable '(get-buffer "*tex-shell*"))
(put 'tex-kill-job 'menu-enable '(tex-shell-running))


(defvar tex-shell-map nil
  "Keymap for the TeX shell.
Inherits `shell-mode-map' with a few additions.")

(defvar compare-windows-whitespace)	; Pacify the byte-compiler

;;; This would be a lot simpler if we just used a regexp search,
;;; but then it would be too slow.
;;;###autoload
(defun tex-mode ()
  "Major mode for editing files of input for TeX, LaTeX, or SliTeX.
Tries to determine (by looking at the beginning of the file) whether
this file is for plain TeX, LaTeX, or SliTeX and calls `plain-tex-mode',
`latex-mode', or `slitex-mode', respectively.  If it cannot be determined,
such as if there are no commands in the file, the value of `tex-default-mode'
says which mode to use."
  (interactive)
  (let (mode slash comment)
    (save-excursion
      (goto-char (point-min))
      (while (and (setq slash (search-forward "\\" nil t))
		  (setq comment (let ((search-end (point)))
				  (save-excursion
				    (beginning-of-line)
				    (search-forward "%" search-end t))))))
      (if (and slash (not comment))
	  (setq mode (if (looking-at "documentstyle\\|documentclass\\|begin\\b\\|NeedsTeXFormat{LaTeX")
                         (if (looking-at
			      "document\\(style\\|class\\)\\(\\[.*\\]\\)?{slides}")
                             'slitex-mode
                           'latex-mode)
		       'plain-tex-mode))))
    (if mode (funcall mode)
      (funcall tex-default-mode))))

;;;###autoload
(defalias 'TeX-mode 'tex-mode)
;;;###autoload
(defalias 'LaTeX-mode 'latex-mode)

;;;###autoload
(defun plain-tex-mode ()
  "Major mode for editing files of input for plain TeX.
Makes $ and } display the characters they match.
Makes \" insert `` when it seems to be the beginning of a quotation,
and '' when it appears to be the end; it inserts \" only after a \\.

Use \\[tex-region] to run TeX on the current region, plus a \"header\"
copied from the top of the file (containing macro definitions, etc.),
running TeX under a special subshell.  \\[tex-buffer] does the whole buffer.
\\[tex-file] saves the buffer and then processes the file.
\\[tex-print] prints the .dvi file made by any of these.
\\[tex-view] previews the .dvi file made by any of these.
\\[tex-bibtex-file] runs bibtex on the file of the current buffer.

Use \\[validate-tex-buffer] to check buffer for paragraphs containing
mismatched $'s or braces.

Special commands:
\\{tex-mode-map}

Mode variables:
tex-run-command
	Command string used by \\[tex-region] or \\[tex-buffer].
tex-directory
	Directory in which to create temporary files for TeX jobs
	run by \\[tex-region] or \\[tex-buffer].
tex-dvi-print-command
	Command string used by \\[tex-print] to print a .dvi file.
tex-alt-dvi-print-command
	Alternative command string used by \\[tex-print] (when given a prefix
	argument) to print a .dvi file.
tex-dvi-view-command
	Command string used by \\[tex-view] to preview a .dvi file.
tex-show-queue-command
	Command string used by \\[tex-show-print-queue] to show the print
	queue that \\[tex-print] put your job on.

Entering Plain-tex mode runs the hook `text-mode-hook', then the hook
`tex-mode-hook', and finally the hook `plain-tex-mode-hook'.  When the
special subshell is initiated, the hook `tex-shell-hook' is run."

  (interactive)
  (tex-common-initialization)
  (setq mode-name "TeX")
  (setq major-mode 'plain-tex-mode)
  (setq tex-command tex-run-command)
  (setq tex-start-of-header "%**start of header")
  (setq tex-end-of-header "%**end of header")
  (setq tex-trailer "\\bye\n")
  (run-hooks 'text-mode-hook 'tex-mode-hook 'plain-tex-mode-hook))
;;;###autoload
(defalias 'plain-TeX-mode 'plain-tex-mode)

;;;###autoload
(defun latex-mode ()
  "Major mode for editing files of input for LaTeX.
Makes $ and } display the characters they match.
Makes \" insert `` when it seems to be the beginning of a quotation,
and '' when it appears to be the end; it inserts \" only after a \\.

Use \\[tex-region] to run LaTeX on the current region, plus the preamble
copied from the top of the file (containing \\documentstyle, etc.),
running LaTeX under a special subshell.  \\[tex-buffer] does the whole buffer.
\\[tex-file] saves the buffer and then processes the file.
\\[tex-print] prints the .dvi file made by any of these.
\\[tex-view] previews the .dvi file made by any of these.
\\[tex-bibtex-file] runs bibtex on the file of the current buffer.

Use \\[validate-tex-buffer] to check buffer for paragraphs containing
mismatched $'s or braces.

Special commands:
\\{tex-mode-map}

Mode variables:
latex-run-command
	Command string used by \\[tex-region] or \\[tex-buffer].
tex-directory
	Directory in which to create temporary files for LaTeX jobs
	run by \\[tex-region] or \\[tex-buffer].
tex-dvi-print-command
	Command string used by \\[tex-print] to print a .dvi file.
tex-alt-dvi-print-command
	Alternative command string used by \\[tex-print] (when given a prefix
	argument) to print a .dvi file.
tex-dvi-view-command
	Command string used by \\[tex-view] to preview a .dvi file.
tex-show-queue-command
	Command string used by \\[tex-show-print-queue] to show the print
	queue that \\[tex-print] put your job on.

Entering Latex mode runs the hook `text-mode-hook', then 
`tex-mode-hook', and finally `latex-mode-hook'.  When the special
subshell is initiated, `tex-shell-hook' is run."
  (interactive)
  (tex-common-initialization)
  (setq mode-name "LaTeX")
  (setq major-mode 'latex-mode)
  (setq tex-command latex-run-command)
  (setq tex-start-of-header "\\documentstyle")
  (setq tex-end-of-header "\\begin{document}")
  (setq tex-trailer "\\end{document}\n")
  ;; A line containing just $$ is treated as a paragraph separator.
  ;; A line starting with $$ starts a paragraph,
  ;; but does not separate paragraphs if it has more stuff on it.
  (setq paragraph-start "[ \t]*$\\|[\f%]\\|[ \t]*\\$\\$\\|\
\\\\begin\\>\\|\\\\label\\>\\|\\\\end\\>\\|\\\\\\[\\|\\\\\\]\\|\
\\\\chapter\\>\\|\\\\section\\>\\|\
\\\\subsection\\>\\|\\\\subsubsection\\>\\|\
\\\\paragraph\\>\\|\\\\subparagraph\\>\\|\
\\\\item\\>\\|\\\\bibitem\\>\\|\\\\newline\\>\\|\\\\noindent\\>\\|\
\\\\[a-z]*space\\>\\|\\\\[a-z]*skip\\>\\|\
\\\\newpage\\>\\|\\\\[a-z]*page\\|\\\\footnote\\>\\|\
\\\\marginpar\\>\\|\\\\parbox\\>\\|\\\\caption\\>")
  (setq paragraph-separate "[ \t]*$\\|[\f%]\\|[ \t]*\\$\\$[ \t]*$\\|\
\\\\begin\\>\\|\\\\label\\>\\|\\\\end\\>\\|\\\\\\[\\|\\\\\\]\\|\
\\\\chapter\\>\\|\\\\section\\>\\|\
\\\\subsection\\>\\|\\\\subsubsection\\>\\|\
\\\\paragraph\\>\\|\\\\subparagraph\\>\\|\
\\(\\\\item\\|\\\\bibitem\\|\\\\newline\\|\\\\noindent\\|\
\\\\[a-z]*space\\|\\\\[a-z]*skip\\|\
\\\\newpage\\|\\\\[a-z]*page[a-z]*\\|\\\\footnote\\|\
\\\\marginpar\\|\\\\parbox\\|\\\\caption\\)[ \t]*\\($\\|%\\)")
  (run-hooks 'text-mode-hook 'tex-mode-hook 'latex-mode-hook))

;;;###autoload
(defun slitex-mode ()
  "Major mode for editing files of input for SliTeX.
Makes $ and } display the characters they match.
Makes \" insert `` when it seems to be the beginning of a quotation,
and '' when it appears to be the end; it inserts \" only after a \\.

Use \\[tex-region] to run SliTeX on the current region, plus the preamble
copied from the top of the file (containing \\documentstyle, etc.),
running SliTeX under a special subshell.  \\[tex-buffer] does the whole buffer.
\\[tex-file] saves the buffer and then processes the file.
\\[tex-print] prints the .dvi file made by any of these.
\\[tex-view] previews the .dvi file made by any of these.
\\[tex-bibtex-file] runs bibtex on the file of the current buffer.

Use \\[validate-tex-buffer] to check buffer for paragraphs containing
mismatched $'s or braces.

Special commands:
\\{tex-mode-map}

Mode variables:
slitex-run-command
	Command string used by \\[tex-region] or \\[tex-buffer].
tex-directory
	Directory in which to create temporary files for SliTeX jobs
	run by \\[tex-region] or \\[tex-buffer].
tex-dvi-print-command
	Command string used by \\[tex-print] to print a .dvi file.
tex-alt-dvi-print-command
	Alternative command string used by \\[tex-print] (when given a prefix
	argument) to print a .dvi file.
tex-dvi-view-command
	Command string used by \\[tex-view] to preview a .dvi file.
tex-show-queue-command
	Command string used by \\[tex-show-print-queue] to show the print
	queue that \\[tex-print] put your job on.

Entering SliTeX mode runs the hook `text-mode-hook', then the hook
`tex-mode-hook', then the hook `latex-mode-hook', and finally the hook
`slitex-mode-hook'.  When the special subshell is initiated, the hook
`tex-shell-hook' is run."
  (interactive)
  (tex-common-initialization)
  (setq mode-name "SliTeX")
  (setq major-mode 'slitex-mode)
  (setq tex-command slitex-run-command)
  (setq tex-start-of-header "\\documentstyle{slides}")
  (setq tex-end-of-header "\\begin{document}")
  (setq tex-trailer "\\end{document}\n")
  ;; A line containing just $$ is treated as a paragraph separator.
  ;; A line starting with $$ starts a paragraph,
  ;; but does not separate paragraphs if it has more stuff on it.
  (setq paragraph-start "[ \t]*$\\|[\f%]\\|[ \t]*\\$\\$\\|\
\\\\begin\\>\\|\\\\label\\>\\|\\\\end\\>\\|\\\\\\[\\|\\\\\\]\\|\
\\\\chapter\\>\\|\\\\section\\>\\|\
\\\\subsection\\>\\|\\\\subsubsection\\>\\|\
\\\\paragraph\\>\\|\\\\subparagraph\\>\\|\
\\\\item\\>\\|\\\\bibitem\\>\\|\\\\newline\\>\\|\\\\noindent\\>\\|\
\\\\[a-z]*space\\>\\|\\\\[a-z]*skip\\>\\|\
\\\\newpage\\>\\|\\\\[a-z]*page\\|\\\\footnote\\>\\|\
\\\\marginpar\\>\\|\\\\parbox\\>\\|\\\\caption\\>")
  (setq paragraph-separate "[ \t]*$\\|[\f%]\\|[ \t]*\\$\\$[ \t]*$\\|\
\\\\begin\\>\\|\\\\label\\>\\|\\\\end\\>\\|\\\\\\[\\|\\\\\\]\\|\
\\\\chapter\\>\\|\\\\section\\>\\|\
\\\\subsection\\>\\|\\\\subsubsection\\>\\|\
\\\\paragraph\\>\\|\\\\subparagraph\\>\\|\
\\\\item[ \t]*$\\|\\\\bibitem[ \t]*$\\|\\\\newline[ \t]*$\\|\\\\noindent[ \t]*$\\|\
\\\\[a-z]*space[ \t]*$\\|\\\\[a-z]*skip[ \t]*$\\|\
\\\\newpage[ \t]*$\\|\\\\[a-z]*page[a-z]*[ \t]*$\\|\\\\footnote[ \t]*$\\|\
\\\\marginpar[ \t]*$\\|\\\\parbox[ \t]*$\\|\\\\caption[ \t]*$")
  (run-hooks
   'text-mode-hook 'tex-mode-hook 'latex-mode-hook 'slitex-mode-hook))

(defun tex-common-initialization ()
  (kill-all-local-variables)
  (use-local-map tex-mode-map)
  (setq local-abbrev-table text-mode-abbrev-table)
  (if (null tex-mode-syntax-table)
      (let ((char 0))
	(setq tex-mode-syntax-table (make-syntax-table))
	(set-syntax-table tex-mode-syntax-table)
	(while (< char ? )
	  (modify-syntax-entry char ".")
	  (setq char (1+ char)))
	(modify-syntax-entry ?\C-@ "w")
	(modify-syntax-entry ?\t " ")
	(modify-syntax-entry ?\n ">")
	(modify-syntax-entry ?\f ">")
	(modify-syntax-entry ?$ "$$")
	(modify-syntax-entry ?% "<")
	(modify-syntax-entry ?\\ "/")
	(modify-syntax-entry ?\" ".")
	(modify-syntax-entry ?& ".")
	(modify-syntax-entry ?_ ".")
	(modify-syntax-entry ?@ "_")
	(modify-syntax-entry ?~ " ")
	(modify-syntax-entry ?' "w"))
    (set-syntax-table tex-mode-syntax-table))
  (make-local-variable 'paragraph-start)
  ;; A line containing just $$ is treated as a paragraph separator.
  (setq paragraph-start "[ \t]*$\\|[\f\\\\%]\\|[ \t]*\\$\\$")
  (make-local-variable 'paragraph-separate)
  ;; A line starting with $$ starts a paragraph,
  ;; but does not separate paragraphs if it has more stuff on it.
  (setq paragraph-separate "[ \t]*$\\|[\f\\\\%]\\|[ \t]*\\$\\$[ \t]*$")
  (make-local-variable 'comment-start)
  (setq comment-start "%")
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip "\\(\\(^\\|[^\\]\\)\\(\\\\\\\\\\)*\\)\\(%+ *\\)")
  (make-local-variable 'comment-indent-function)
  (setq comment-indent-function 'tex-comment-indent)
  (make-local-variable 'parse-sexp-ignore-comments)
  (setq parse-sexp-ignore-comments t)
  (make-local-variable 'compare-windows-whitespace)
  (setq compare-windows-whitespace 'tex-categorize-whitespace)
  (make-local-variable 'tex-command)
  (make-local-variable 'tex-start-of-header)
  (make-local-variable 'tex-end-of-header)
  (make-local-variable 'tex-trailer))

(defun tex-comment-indent ()
  (if (looking-at "%%%")
      (current-column)
    (skip-chars-backward " \t")
    (max (if (bolp) 0 (1+ (current-column)))
	 comment-column)))

(defun tex-categorize-whitespace (backward-limit)
  ;; compare-windows-whitespace is set to this.
  ;; This is basically a finite-state machine.
  ;; Returns a symbol telling how TeX would treat
  ;; the whitespace we are looking at: null, space, or par.
  (let ((category 'null)
	(not-finished t))
    (skip-chars-backward " \t\n\f" backward-limit)
    (while not-finished
      (cond ((looking-at "[ \t]+")
	     (goto-char (match-end 0))
	     (if (eql category 'null)
		 (setq category 'space)))
	    ((looking-at "\n")
	     (cond ((eql category 'newline)
		    (setq category 'par)
		    (setq not-finished nil))
		   (t
		    (setq category 'newline) ;a strictly internal state
		    (goto-char (match-end 0)))))
	    ((looking-at "\f+")
	     (setq category 'par)
	     (setq not-finished nil))
	    (t
	     (setq not-finished nil))))
    (skip-chars-forward " \t\n\f")
    (if (eql category 'newline)
	'space				;TeX doesn't distinguish
      category)))

(defun tex-insert-quote (arg)
  "Insert the appropriate quote marks for TeX.
Inserts the value of `tex-open-quote' (normally ``) or `tex-close-quote'
\(normally '') depending on the context.  With prefix argument, always
inserts \" characters."
  (interactive "*P")
  (if arg
      (self-insert-command (prefix-numeric-value arg))
    (insert
     (cond ((or (bobp)
		(save-excursion
		  (forward-char -1)
		  (looking-at "\\s(\\|\\s \\|\\s>")))
	    tex-open-quote)
	   ((= (preceding-char) ?\\)
	    ?\")
	   (t
	    tex-close-quote)))))

(defun validate-tex-buffer ()
  "Check current buffer for paragraphs containing mismatched $s.
Their positions are recorded in the buffer `*Occur*'.
To find a particular invalidity from `*Occur*',
switch to to that buffer and type C-c C-c on the line
for the invalidity you want to see."
  (interactive)
  (let ((buffer (current-buffer))
	(prevpos (point-min))
	(linenum nil))
    (with-output-to-temp-buffer "*Occur*"
      (princ "Mismatches:\n")
      (save-excursion
	(set-buffer standard-output)
	(occur-mode)
	(setq occur-buffer buffer)
	(setq occur-nlines 0)
	(setq occur-pos-list nil))
      (save-excursion
	(goto-char (point-max))
	(while (and (not (input-pending-p)) (not (bobp)))
	  (let ((end (point)))
	    ;; Scan the previous paragraph for invalidities.
	    (search-backward "\n\n" nil 'move)
	    (or (tex-validate-region (point) end)
		(let* ((end (save-excursion (forward-line 1) (point)))
		       start tem)
		  (beginning-of-line)
		  (setq start (point))
		  ;; Keep track of line number as we scan,
		  ;; in a cumulative fashion.
		  (if linenum
		      (setq linenum (- linenum (count-lines prevpos (point))))
		    (setq linenum (1+ (count-lines 1 start))))
		  (setq prevpos (point))
		  ;; Mention this mismatch in *Occur*.  
		  ;; Since we scan from end of buffer to beginning,
		  ;; add each mismatch at the beginning of *Occur*
		  ;; and at the beginning of occur-pos-list.
		  (save-excursion
		    (setq tem (point-marker))
		    (set-buffer standard-output)
		    (goto-char (point-min))
		    ;; Skip "Mismatches:" header line.
		    (forward-line 1)
		    (setq occur-pos-list (cons tem occur-pos-list))
		    (insert-buffer-substring buffer start end)
		    (forward-char (- start end))
		    (insert (format "%3d: " linenum))))))))
      (save-excursion
	(set-buffer standard-output)
	(if (null occur-pos-list)
	    (insert "None!\n"))
	(if (interactive-p)
	    (message "%d mismatches found" (length occur-pos-list)))))))

(defun tex-validate-region (start end)
  "Check for mismatched braces or $'s in region.
Returns t if no mismatches.  Returns nil and moves point to suspect
area if a mismatch is found."
  (interactive "r")
  (let ((failure-point nil) (max-possible-sexps (- end start)))
    (save-excursion
      (condition-case ()
	  (save-restriction
	    (narrow-to-region start end)
	    (goto-char start)
	    (while (< 0 (setq max-possible-sexps (1- max-possible-sexps)))
	      (forward-sexp 1)))
	(error
	  (setq failure-point (point)))))
    (if failure-point
	(progn
	  (goto-char failure-point)
	  nil)
      t)))

(defun tex-terminate-paragraph (inhibit-validation)
  "Insert two newlines, breaking a paragraph for TeX.
Check for mismatched braces or $s in paragraph being terminated.
A prefix arg inhibits the checking."
  (interactive "*P")
  (or inhibit-validation
      (save-excursion
	(tex-validate-region
	  (save-excursion
	    (search-backward "\n\n" nil 'move)
	    (point))
	  (point)))
      (message "Paragraph being closed appears to contain a mismatch"))
  (insert "\n\n"))

(defun tex-insert-braces ()
  "Make a pair of braces and be poised to type inside of them."
  (interactive "*")
  (insert ?\{)
  (save-excursion
    (insert ?})))

;;; Like tex-insert-braces, but for LaTeX.
(defun tex-latex-block (name)
  "Creates a matching pair of lines `\\begin{NAME}' and `\\end{NAME}' at point.
Puts point on a blank line between them."
  (interactive
   (prog2
      (barf-if-buffer-read-only)
      (list
       (completing-read "LaTeX block name: "
			(mapcar 'list
                                (append standard-latex-block-names
                                        latex-block-names))))))
  (let ((col (current-column)))
    (insert (format "\\begin{%s}\n" name))
    (indent-to col)
    (save-excursion
      (insert ?\n)
      (indent-to col)
      (insert-string (format "\\end{%s}" name))
      (if (eobp) (insert ?\n)))))

(defun tex-last-unended-begin ()
  "Leave point at the beginning of the last `\\begin{...}' that is unended."
  (while (and (re-search-backward "\\(\\\\begin\\s *{\\)\\|\\(\\\\end\\s *{\\)")
              (looking-at "\\\\end{"))
    (tex-last-unended-begin)))

(defun tex-goto-last-unclosed-latex-block ()
  "Move point to the last unclosed \\begin{...}.
Mark is left at original location."
  (interactive)
  (let ((spot))
    (save-excursion
      (condition-case nil
          (tex-last-unended-begin)
        (error (error "Couldn't find unended \\begin")))
      (setq spot (point)))
    (push-mark)
    (goto-char spot)))

(defun tex-close-latex-block ()
  "Creates an \\end{...} to match the last unclosed \\begin{...}."
  (interactive "*")
  (let ((new-line-needed (bolp))
	text indentation)
    (save-excursion
      (condition-case nil
          (tex-last-unended-begin)
        (error (error "Couldn't find unended \\begin")))
      (setq indentation (current-column))
      (re-search-forward "\\\\begin\\(\\s *{[^}\n]*}\\)")
      (setq text (buffer-substring (match-beginning 1) (match-end 1))))
    (indent-to indentation)
    (insert "\\end" text)
    (if new-line-needed (insert ?\n))))

(defun tex-compilation-parse-errors ()
  "Parse the current buffer as error messages.
This makes a list of error descriptors, compilation-error-list.
For each source-file, line-number pair in the buffer,
the source file is read in, and the text location is saved in
compilation-error-list.  The function `next-error', assigned to
\\[next-error], takes the next error off the list and visits its location.

This function works on TeX compilations only.  It is necessary for
that purpose, since TeX does not put file names on the same line as
line numbers for the errors."
  (setq compilation-error-list nil)
  (message "Parsing error messages...")
  (modify-syntax-entry ?\{ "_")
  (modify-syntax-entry ?\} "_")
  (modify-syntax-entry ?\[ "_")
  (modify-syntax-entry ?\] "_")
  (let (text-buffer
	last-filename last-linenum)
    ;; Don't reparse messages already seen at last parse.
    (goto-char compilation-parsing-end)
    ;; Don't parse the first two lines as error messages.
    ;; This matters for grep.
    (if (bobp)
	(forward-line 2))
    (while (re-search-forward "^l\.[0-9]+ " nil t)
      (let (linenum filename
	    error-marker text-marker)
	;; Extract file name and line number from error message.
	;; Line number is 2 away from beginning of line: "l.23"
	(beginning-of-line)
	(goto-char (+ (point) 2))
	(setq linenum (read (current-buffer)))
	;; The file is the one that was opened last and is still open.
	;; We need to find the last open parenthesis.
	(insert ?\))
	(backward-sexp)
	(forward-char)
	(setq filename (current-word))
	;; Locate the erring file and line.
	(if (and (equal filename last-filename)
		 (= linenum last-linenum))
	    nil
	  (skip-chars-backward "^(")
	  (backward-char)
	  (forward-sexp)
	  (backward-delete-char 1)
	  (setq error-marker (point-marker))
	  ;; text-buffer gets the buffer containing this error's file.
	  (if (not (equal filename last-filename))
	      (setq text-buffer
		    (and (file-exists-p (setq last-filename filename))
			 (find-file-noselect filename))
		    last-linenum 0))
	  (if text-buffer
	      ;; Go to that buffer and find the erring line.
	      (save-excursion
		(set-buffer text-buffer)
		(if (zerop last-linenum)
		    (progn
		      (goto-char 1)
		      (setq last-linenum 1)))
		(forward-line (- linenum last-linenum))
		(setq last-linenum linenum)
		(setq text-marker (point-marker))
		(setq compilation-error-list
		      (cons (list error-marker text-marker)
			    compilation-error-list)))))
	(forward-line 1)))
    (setq compilation-parsing-end (point-max)))
  (message "Parsing error messages...done")
  (setq compilation-error-list (nreverse compilation-error-list)))

;;; Invoking TeX in an inferior shell.

;;; Why use a shell instead of running TeX directly?  Because if TeX
;;; gets stuck, the user can switch to the shell window and type at it.

;;; The utility functions:

;;;###autoload
(defun tex-start-shell ()
  (save-excursion
    (set-buffer
     (make-comint
      "tex-shell"
      (or tex-shell-file-name (getenv "ESHELL") (getenv "SHELL") "/bin/sh")
      nil))
    (let ((proc (get-process "tex-shell")))
      (set-process-sentinel proc 'tex-shell-sentinel)
      (process-kill-without-query proc)
      (setq comint-prompt-regexp shell-prompt-pattern)
      (setq tex-shell-map (nconc (make-sparse-keymap) shell-mode-map))
      (tex-define-common-keys tex-shell-map)
      (use-local-map tex-shell-map)
      (run-hooks 'tex-shell-hook)
      (while (zerop (buffer-size))
	(sleep-for 1)))))

(defun tex-display-shell ()
  "Make the TeX shell buffer visible in a window."
  (display-buffer (process-buffer (get-process "tex-shell")))
  (tex-recenter-output-buffer nil))

(defun tex-shell-sentinel (proc msg)
  (cond ((null (buffer-name (process-buffer proc)))
	 ;; buffer killed
	 (set-process-buffer proc nil)
         (tex-delete-last-temp-files))
	((memq (process-status proc) '(signal exit))
         (tex-delete-last-temp-files))))

(defun tex-set-buffer-directory (buffer directory)
  "Set BUFFER's default directory to be DIRECTORY."
  (setq directory (file-name-as-directory (expand-file-name directory)))
  (if (not (file-directory-p directory))
      (error "%s is not a directory" directory)
    (save-excursion
      (set-buffer buffer)
      (setq default-directory directory))))

(defvar tex-send-command-modified-tick 0)
(make-variable-buffer-local 'tex-send-command-modified-tick)

(defun tex-send-command (command &optional file background)
  "Send COMMAND to TeX shell process, substituting optional FILE for *.
Do this in background if optional BACKGROUND is t.  If COMMAND has no *,
FILE will be appended, preceded by a blank, to COMMAND.  If FILE is nil, no
substitution will be made in COMMAND.  COMMAND can be any expression that
evaluates to a command string."
  (save-excursion
    (let* ((cmd (eval command))
	   (proc (get-process "tex-shell"))
	   (buf (process-buffer proc))
           (star (string-match "\\*" cmd))
	   (string
	    (concat
	     (if file
		 (if star (concat (substring cmd 0 star)
				  file (substring cmd (1+ star)))
		   (concat cmd " " file))
	       cmd)
	     (if background "&" ""))))
      ;; Switch to buffer before checking for subproc output in it.
      (set-buffer buf)
      ;; If text is unchanged since previous tex-send-command,
      ;; we haven't got any output.  So wait for output now.
      (if (= (buffer-modified-tick buf) tex-send-command-modified-tick)
	  (accept-process-output proc))
      (goto-char (process-mark proc))
      (insert string)
      (comint-send-input)
      (setq tex-send-command-modified-tick (buffer-modified-tick buf)))))

(defun tex-delete-last-temp-files (&optional not-all)
  "Delete any junk files from last temp file.
If NOT-ALL is non-nil, save the `.dvi' file."
  (if tex-last-temp-file
      (let* ((dir (file-name-directory tex-last-temp-file))
	     (list (and (file-directory-p dir)
			(file-name-all-completions
			 (file-name-nondirectory tex-last-temp-file) dir))))
	(while list
	  (if not-all
	      (and
	       ;; If arg is non-nil, don't delete the .dvi file.
	       (not (string-match "\\.dvi$" (car list)))
	       (delete-file (concat dir (car list))))
	    (delete-file (concat dir (car list))))
          (setq list (cdr list))))))

(add-hook 'kill-emacs-hook 'tex-delete-last-temp-files)

;;; The commands:

(defun tex-region (beg end)
  "Run TeX on the current region, via a temporary file.
The file's name comes from the variable `tex-zap-file' and the
variable `tex-directory' says where to put it.

If the buffer has a header, the header is given to TeX before the
region itself.  The buffer's header is all lines between the strings
defined by `tex-start-of-header' and `tex-end-of-header' inclusive.
The header must start in the first 100 lines of the buffer.

The value of `tex-trailer' is given to TeX as input after the region.

The value of `tex-command' specifies the command to use to run TeX."
  (interactive "r")
  (if (tex-shell-running)
      (tex-kill-job)
    (tex-start-shell))
  (or tex-zap-file
      (setq tex-zap-file (tex-generate-zap-file-name)))
  (let* ((temp-buffer (get-buffer-create " TeX-Output-Buffer"))
         ; Temp file will be written and TeX will be run in zap-directory.
         ; If the TEXINPUTS file has relative directories or if the region has
         ; \input of files, this must be the same directory as the file for
         ; TeX to access the correct inputs.  That's why it's safest if
         ; tex-directory is ".".
         (zap-directory
          (file-name-as-directory (expand-file-name tex-directory)))
         (tex-out-file (concat zap-directory tex-zap-file)))
    ;; Don't delete temp files if we do the same buffer twice in a row.
    (or (eq (current-buffer) tex-last-buffer-texed)
	(tex-delete-last-temp-files t))
    ;; Write the new temp file.
    (save-excursion
      (save-restriction
	(widen)
	(goto-char (point-min))
	(forward-line 100)
	(let ((search-end (point))
	      (hbeg (point-min)) (hend (point-min))
	      (default-directory zap-directory))
	  (goto-char (point-min))
	  ;; Initialize the temp file with either the header or nothing
	  (if (search-forward tex-start-of-header search-end t)
	      (progn
		(beginning-of-line)
		(setq hbeg (point))	;mark beginning of header
		(if (search-forward tex-end-of-header nil t)
		    (progn (forward-line 1)
			   (setq hend (point)))	;mark end of header
		  (setq hbeg (point-min))))) ;no header
	  (write-region (min hbeg beg) hend
                        (concat tex-out-file ".tex") nil nil)
	  (write-region (max beg hend) end (concat tex-out-file ".tex") t nil))
	(let ((local-tex-trailer tex-trailer))
	  (set-buffer temp-buffer)
	  (erase-buffer)
	  ;; make sure trailer isn't hidden by a comment
	  (insert-string "\n")
	  (if local-tex-trailer (insert-string local-tex-trailer))
	  (tex-set-buffer-directory temp-buffer zap-directory)
	  (write-region (point-min) (point-max)
                        (concat tex-out-file ".tex") t nil))))
    ;; Record the file name to be deleted afterward.
    (setq tex-last-temp-file tex-out-file)
    (tex-send-command tex-shell-cd-command zap-directory)
    (tex-send-command tex-command tex-out-file)
    (tex-display-shell)
    (setq tex-print-file tex-out-file)
    (setq tex-last-buffer-texed (current-buffer))))

(defun tex-buffer ()
  "Run TeX on current buffer.  See \\[tex-region] for more information.
Does not save the buffer, so it's useful for trying experimental versions.
See \\[tex-file] for an alternative."
  (interactive)
  (tex-region (point-min) (point-max)))

(defun tex-file ()
  "Prompt to save all buffers and run TeX (or LaTeX) on current buffer's file.
This function is more useful than \\[tex-buffer] when you need the
`.aux' file of LaTeX to have the correct name."
  (interactive)
  (let ((tex-out-file
         (if (buffer-file-name)
             (file-name-nondirectory (buffer-file-name))
           (error "Buffer does not seem to be associated with any file")))
	(file-dir (file-name-directory (buffer-file-name))))
    (if tex-offer-save
        (save-some-buffers))
    (if (tex-shell-running)
        (tex-kill-job)
      (tex-start-shell))
    (tex-send-command tex-shell-cd-command file-dir)
    (tex-send-command tex-command tex-out-file))
  (tex-display-shell)
  (setq tex-last-buffer-texed (current-buffer))
  (setq tex-print-file (buffer-file-name)))

(defun tex-generate-zap-file-name ()
  "Generate a unique name suitable for use as a file name."
  ;; Include the shell process number and host name
  ;; in case there are multiple shells (for same or different user).
  (format "#tz%d%s"
          (process-id (get-buffer-process "*tex-shell*"))
	  (tex-strip-dots (system-name))))

(defun tex-strip-dots (s)
  (setq s (copy-sequence s))
  (while (string-match "\\." s)
    (aset s (match-beginning 0) ?-))
  s)

;; This will perhaps be useful for modifying TEXINPUTS.
;; Expand each file name, separated by colons, in the string S.
(defun tex-expand-files (s)
  (let (elts (start 0))
    (while (string-match ":" s start)
      (setq elts (cons (substring s start (match-beginning 0)) elts))
      (setq start (match-end 0)))
    (or (= start 0)
	(setq elts (cons (substring s start) elts)))
    (mapconcat 'expand-file-name (nreverse elts) ":")))

(defun tex-shell-running ()
  (and (get-process "tex-shell")
       (eq (process-status (get-process "tex-shell")) 'run)))

(defun tex-kill-job ()
  "Kill the currently running TeX job."
  (interactive)
  (quit-process (get-process "tex-shell") t))

(defun tex-recenter-output-buffer (linenum)
  "Redisplay buffer of TeX job output so that most recent output can be seen.
The last line of the buffer is displayed on
line LINE of the window, or centered if LINE is nil."
  (interactive "P")
  (let ((tex-shell (get-buffer "*tex-shell*"))
	(old-buffer (current-buffer)))
    (if (null tex-shell)
	(message "No TeX output buffer")
      (pop-to-buffer tex-shell)
      (bury-buffer tex-shell)
      (goto-char (point-max))
      (recenter (if linenum
		    (prefix-numeric-value linenum)
		  (/ (window-height) 2)))
      (pop-to-buffer old-buffer))))

(defun tex-print (&optional alt)
  "Print the .dvi file made by \\[tex-region], \\[tex-buffer] or \\[tex-file].
Runs the shell command defined by `tex-dvi-print-command'.  If prefix argument
is provided, use the alternative command, `tex-alt-dvi-print-command'."
  (interactive "P")
  (let ((print-file-name-dvi (tex-append tex-print-file ".dvi"))
	test-name)
    (if (and (not (equal (current-buffer) tex-last-buffer-texed))
	     (buffer-file-name)
	     ;; Check that this buffer's printed file is up to date.
	     (file-newer-than-file-p
	      (setq test-name (tex-append (buffer-file-name) ".dvi"))
	      (buffer-file-name)))
	(setq print-file-name-dvi test-name))
    (if (not (file-exists-p print-file-name-dvi))
        (error "No appropriate `.dvi' file could be found")
      (tex-send-command
        (if alt tex-alt-dvi-print-command tex-dvi-print-command)
        print-file-name-dvi t))))

(defun tex-alt-print ()
  "Print the .dvi file made by \\[tex-region], \\[tex-buffer] or \\[tex-file].
Runs the shell command defined by `tex-alt-dvi-print-command'."
  (interactive)
  (tex-print t))

(defun tex-view ()
  "Preview the last `.dvi' file made by running TeX under Emacs.
This means, made using \\[tex-region], \\[tex-buffer] or \\[tex-file].
The variable `tex-dvi-view-command' specifies the shell command for preview."
  (interactive)
  (let ((tex-dvi-print-command tex-dvi-view-command))
    (tex-print)))

(defun tex-append (file-name suffix)
  "Append to FILENAME the suffix SUFFIX, using same algorithm TeX uses.
Pascal-based TeX scans for the first period, C TeX uses the last.
No period is retained immediately before SUFFIX,
so normally SUFFIX starts with one."
  (if (stringp file-name)
      (let ((file (file-name-nondirectory file-name))
	    trial-name)
	;; Try spliting on last period.
	;; The first-period split can get fooled when two files
	;; named a.tex and a.b.tex are both tex'd;
	;; the last-period split must be right if it matches at all.
	(setq trial-name
	      (concat (file-name-directory file-name)
		      (substring file 0
				 (string-match "\\.[^.]*$" file))
		      suffix))
	(if (or (file-exists-p trial-name)
		(file-exists-p (concat trial-name ".aux"))) ;for BibTeX files
	    trial-name
	  ;; Not found, so split on first period.
	  (concat (file-name-directory file-name)
		  (substring file 0
			     (string-match "\\." file))
		  suffix)))
    " "))

(defun tex-show-print-queue ()
  "Show the print queue that \\[tex-print] put your job on.
Runs the shell command defined by `tex-show-queue-command'."
  (interactive)
  (if (tex-shell-running)
      (tex-kill-job)
    (tex-start-shell))
  (tex-send-command tex-show-queue-command)
  (tex-display-shell))

(defun tex-bibtex-file ()
  "Run BibTeX on the current buffer's file."
  (interactive)
  (if (tex-shell-running)
      (tex-kill-job)
    (tex-start-shell))
  (let ((tex-out-file
         (tex-append (file-name-nondirectory (buffer-file-name)) ""))
	(file-dir (file-name-directory (buffer-file-name))))
    (tex-send-command tex-shell-cd-command file-dir)
    (tex-send-command tex-bibtex-command tex-out-file))
  (tex-display-shell))

(run-hooks 'tex-mode-load-hook)

(provide 'tex-mode)

;;; tex-mode.el ends here

