;;; tex-mode.el --- TeX, LaTeX, and SliTeX mode commands.

;; Copyright (C) 1985, 1986, 1989, 1992 Free Software Foundation, Inc.

;; Maintainer: Edward M. Reingold <reingold@cs.uiuc.edu>
;; Keywords: tex

;; Contributions over the years by William F. Schelter, Dick King,
;; Stephen Gildea, Michael Prange, and Edward M. Reingold.

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

;; This was a pain.  Now, make-comint should autoload comint.
;; (require 'comint)

;;;###autoload
(defvar tex-shell-file-name nil
  "*If non-nil, is file name to use for the subshell in which TeX is run.")

;;;###autoload
(defvar tex-directory "."
  "*Directory in which temporary files are left.
You can make this /tmp if your TEXINPUTS has no relative directories in it
and you don't try to apply \\[tex-region] or \\[tex-buffer] when there are
\\input commands with relative directories.")

;;;###autoload
(defvar tex-offer-save t
  "*If non-nil, ask about saving modified buffers before \\[tex-file] is run.")

;;;###autoload
(defvar tex-run-command "tex"
  "*Command used to run TeX subjob.
If this string contains an asterisk (*), it will be replaced by the
filename; if not, the name of the file, preceded by blank, will be added to
this string.")

;;;###autoload
(defvar latex-run-command "latex"
  "*Command used to run LaTeX subjob.
If this string contains an asterisk (*), it will be replaced by the
filename; if not, the name of the file, preceded by blank, will be added to
this string.")

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
If this string contains an asterisk (*), it will be replaced by the
filename; if not, the name of the file, preceded by blank, will be added to
this string.")

;;;###autoload
(defvar tex-bibtex-command "bibtex"
  "*Command used by `tex-bibtex-file' to gather bibliographic data.
If this string contains an asterisk (*), it will be replaced by the
filename; if not, the name of the file, preceded by blank, will be added to
this string.")

;;;###autoload
(defvar tex-dvi-print-command "lpr -d"
  "*Command used by \\[tex-print] to print a .dvi file.
If this string contains an asterisk (*), it will be replaced by the
filename; if not, the name of the file, preceded by blank, will be added to
this string.")

;;;###autoload
(defvar tex-alt-dvi-print-command "lpr -d"
  "*Command used by \\[tex-print] with a prefix arg to print a .dvi file.
If this string contains an asterisk (*), it will be replaced by the
filename; if not, the name of the file, preceded by blank, will be added to
this string.

If two printers are not enough of a choice, you can define the value
of tex-alt-dvi-print-command to be an expression that asks what you want;
for example,

    (setq tex-alt-dvi-print-command
         '(format \"lpr -P%s\" (read-string \"Use printer: \")))

would tell \\[tex-print] with a prefix argument to ask you which printer to
use.")

;;;###autoload
(defvar tex-dvi-view-command nil
  "*Command used by \\[tex-view] to display a .dvi file.
If this string contains an asterisk (*), it will be replaced by the
filename; if not, the name of the file, preceded by blank, will be added to
this string.

This can be set conditionally so that the previewer used is suitable for the
window system being used.  For example,

    (setq tex-dvi-view-command
          (if (eq window-system 'x) \"xdvi\" \"dvi2tty * | cat -s\"))

would tell \\[tex-view] use xdvi under X windows and to use dvi2tty
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
Normally set to either 'plain-tex-mode or 'latex-mode.")

;;;###autoload
(defvar tex-open-quote "``"
  "*String inserted by typing \\[tex-insert-quote] to open a quotation.")

;;;###autoload
(defvar tex-close-quote "''"
  "*String inserted by typing \\[tex-insert-quote] to close a quotation.")

(defvar tex-last-temp-file nil
  "Latest temporary file generated by \\[tex-region] and \\[tex-buffer].
Deleted when the \\[tex-region] or \\[tex-buffer] is next run, or when the
tex-shell goes away.")

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
The value of tex-directory will be appended to this, separated by a space.")

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
  "Define the keys that we want defined both in TeX mode and in the tex-shell."
  (define-key keymap "\C-c\C-k" 'tex-kill-job)
  (define-key keymap "\C-c\C-l" 'tex-recenter-output-buffer)
  (define-key keymap "\C-c\C-q" 'tex-show-print-queue)
  (define-key keymap "\C-c\C-p" 'tex-print)
  (define-key keymap "\C-c\C-v" 'tex-view)
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
  (define-key tex-mode-map "\C-c\C-e" 'tex-close-latex-block))

(defvar tex-shell-map nil
  "Keymap for the tex-shell.  A comint-mode-map with a few additions.")

;(fset 'TeX-mode 'tex-mode) 		;in loaddefs.

;;; This would be a lot simpler if we just used a regexp search,
;;; but then it would be too slow.
;;;###autoload
(defun tex-mode ()
  "Major mode for editing files of input for TeX, LaTeX, or SliTeX.
Tries to determine (by looking at the beginning of the file) whether
this file is for plain TeX, LaTeX, or SliTeX and calls plain-tex-mode,
latex-mode, or slitex-mode, respectively.  If it cannot be determined,
such as if there are no commands in the file, the value of tex-default-mode
is used."
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
	  (setq mode (if (looking-at "documentstyle")
                         (if (looking-at "documentstyle{slides}")
                             'slitex-mode
                           'latex-mode)
		       'plain-tex-mode))))
    (if mode (funcall mode)
      (funcall tex-default-mode))))
;;;###autoload
(fset 'TeX-mode 'tex-mode)
;;;###autoload
(fset 'LaTeX-mode 'latex-mode)

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

Entering Plain-tex mode calls the value of text-mode-hook, then the value of
tex-mode-hook, and then the value of plain-tex-mode-hook.  When the special
subshell is initiated, the value of tex-shell-hook is called."
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
(fset 'plain-TeX-mode 'plain-tex-mode)

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

Entering Latex mode calls the value of text-mode-hook, then the value of
tex-mode-hook, and then the value of latex-mode-hook.  When the special
subshell is initiated, the value of tex-shell-hook is called."
  (interactive)
  (tex-common-initialization)
  (setq mode-name "LaTeX")
  (setq major-mode 'latex-mode)
  (setq tex-command latex-run-command)
  (setq tex-start-of-header "\\documentstyle")
  (setq tex-end-of-header "\\begin{document}")
  (setq tex-trailer "\\end{document}\n")
  (run-hooks 'text-mode-hook 'tex-mode-hook 'latex-mode-hook))

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

Entering SliTeX mode calls the value of text-mode-hook, then the value of
tex-mode-hook, then the value of latex-mode-hook, and then the value of
slitex-mode-hook.  When the special subshell is initiated, the value of
tex-shell-hook is called."
  (interactive)
  (tex-common-initialization)
  (setq mode-name "SliTeX")
  (setq major-mode 'slitex-mode)
  (setq tex-command slitex-run-command)
  (setq tex-start-of-header "\\documentstyle{slides}")
  (setq tex-end-of-header "\\begin{document}")
  (setq tex-trailer "\\end{document}\n")
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
  (setq paragraph-start "^[ \t]*$\\|^[\f\\\\%]")
  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate paragraph-start)
  (make-local-variable 'comment-start)
  (setq comment-start "%")
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip "\\(\\(^\\|[^\\]\\)\\(\\\\\\\\\\)*\\)\\(%+ *\\)")
  (make-local-variable 'comment-indent-function)
  (setq comment-indent-function 'tex-comment-indent)
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
Inserts the value of tex-open-quote (normally ``) or tex-close-quote
(normally '') depending on the context.  With prefix argument, always
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
  "Check current buffer for paragraphs containing mismatched $'s.
As each such paragraph is found, a mark is pushed at its beginning,
and the location is displayed for a few seconds."
  (interactive)
  (let ((opoint (point)))
    (goto-char (point-max))
    ;; Does not use save-excursion
    ;; because we do not want to save the mark.
    (unwind-protect
	(while (and (not (input-pending-p)) (not (bobp)))
	  (let ((end (point)))
	    (search-backward "\n\n" nil 'move)
	    (or (tex-validate-region (point) end)
		(progn
		  (push-mark (point))
		  (message "Mismatch found in pararaph starting here")
		  (sit-for 4)))))
      (goto-char opoint))))

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
Check for mismatched braces/$'s in paragraph being terminated.
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
  "Creates a matching pair of lines \\begin{NAME} and \\end{NAME} at point.
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
  "Leave point at the beginning of the last \\begin{...} that is unended."
  (while (and (re-search-backward "\\(\\\\begin\\s *{\\)\\|\\(\\\\end\\s *{\\)")
              (looking-at "\\\\end{"))
    (tex-last-unended-begin)))

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

;;; Invoking TeX in an inferior shell.

;;; Why use a shell instead of running TeX directly?  Because if TeX
;;; gets stuck, the user can switch to the shell window and type at it.

;;; The utility functions:

(defun tex-start-shell ()
  (save-excursion
    (set-buffer
     (make-comint
      "tex-shell"
      (or tex-shell-file-name (getenv "ESHELL") (getenv "SHELL") "/bin/sh")
      nil "-v"))
    (let ((proc (get-process "tex-shell")))
      (set-process-sentinel proc 'tex-shell-sentinel)
      (process-kill-without-query proc)
      (setq tex-shell-map (copy-keymap comint-mode-map))
      (tex-define-common-keys tex-shell-map)
      (use-local-map tex-shell-map)
      (run-hooks 'tex-shell-hook)
      (while (zerop (buffer-size))
          (sleep-for 1)))))

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

(defun tex-send-command (command &optional file background)
  "Send COMMAND to tex-shell, substituting optional FILE for *; in background
if optional BACKGROUND is t.   If COMMAND has no *, FILE will be appended,
preceded by a blank, to COMMAND.  If FILE is nil, no substitution will be made
in COMMAND.  COMMAND can be any expression that evaluates to a command string."
  (save-excursion
    (let* ((cmd (eval command))
           (star (string-match "\\*" cmd))
           (front (substring cmd 0 star))
           (back (if star (substring cmd (1+ star)) "")))
      (comint-proc-query (get-process "tex-shell")
                         (concat
                          (if file (if star (concat front file back)
                                     (concat cmd " " file))
                            cmd)
                          (if background "&\n" "\n"))))))

(defun tex-delete-last-temp-files ()
  "Delete any junk files from last temp file."
  (if tex-last-temp-file
      (let* ((dir (file-name-directory tex-last-temp-file))
             (list (file-name-all-completions
                    (file-name-nondirectory tex-last-temp-file) dir)))
        (while list
          (delete-file (concat dir (car list)))
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
    (tex-delete-last-temp-files)
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
	     (file-newer-than-file-p
	      (setq test-name (tex-append (buffer-file-name) ".dvi"))
	      print-file-name-dvi))
	(setq print-file-name-dvi test-name))
    (if (not (file-exists-p print-file-name-dvi))
        (error "No appropriate `.dvi' file could be found")
      (tex-send-command
        (if alt tex-alt-dvi-print-command tex-dvi-print-command)
        print-file-name-dvi t))))

(defun tex-view ()
  "Preview the last `.dvi' file made by running TeX under Emacs.
This means, made using \\[tex-region], \\[tex-buffer] or \\[tex-file].
The variable `tex-dvi-view-command' specifies the shell command for preview."
  (interactive)
  (let ((tex-dvi-print-command tex-dvi-view-command))
    (tex-print)))

(defun tex-append (file-name suffix)
  "Append to FILENAME the suffix SUFFIX, using same algorithm TeX uses.
Scans for the first (not last) period.
No period is retained immediately before SUFFIX,
so normally SUFFIX starts with one."
  (if (stringp file-name)
      (let ((file (file-name-nondirectory file-name)))
	(concat (file-name-directory file-name)
		(substring file 0
			   (string-match "\\." file))
		suffix))
    " "))

(defun tex-show-print-queue ()
  "Show the print queue that \\[tex-print] put your job on.
Runs the shell command defined by `tex-show-queue-command'."
  (interactive)
  (if (tex-shell-running)
      (tex-kill-job)
    (tex-start-shell))
  (tex-send-command tex-show-queue-command))

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
    (tex-send-command bibtex-command tex-out-file)))

(run-hooks 'tex-mode-load-hook)

(provide 'tex-mode)

;;; tex-mode.el ends here

