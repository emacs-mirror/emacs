;;; tex-mode.el --- TeX, LaTeX, and SliTeX mode commands

;; Copyright (C) 1985, 86, 89, 92, 94, 95, 96, 97, 98, 1999, 2002
;;       Free Software Foundation, Inc.

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
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;; Code:

;; Pacify the byte-compiler
(eval-when-compile
  (require 'compare-w)
  (require 'cl)
  (require 'skeleton))

(require 'shell)
(require 'compile)

(defgroup tex-file nil
  "TeX files and directories"
  :prefix "tex-"
  :group 'tex)

(defgroup tex-run nil
  "Running external commands from TeX mode"
  :prefix "tex-"
  :group 'tex)

(defgroup tex-view nil
  "Viewing and printing TeX files"
  :prefix "tex-"
  :group 'tex)

;;;###autoload
(defcustom tex-shell-file-name nil
  "*If non-nil, the shell file name to run in the subshell used to run TeX."
  :type '(choice (const :tag "None" nil)
		 string)
  :group 'tex-run)

;;;###autoload
(defcustom tex-directory "."
  "*Directory in which temporary files are written.
You can make this `/tmp' if your TEXINPUTS has no relative directories in it
and you don't try to apply \\[tex-region] or \\[tex-buffer] when there are
`\\input' commands with relative directories."
  :type 'directory
  :group 'tex-file)

;;;###autoload
(defcustom tex-first-line-header-regexp nil
  "Regexp for matching a first line which `tex-region' should include.
If this is non-nil, it should be a regular expression string;
if it matches the first line of the file,
`tex-region' always includes the first line in the TeX run."
  :type '(choice (const :tag "None" nil)
                 regexp)
  :group 'tex-file)

;;;###autoload
(defcustom tex-main-file nil
  "*The main TeX source file which includes this buffer's file.
The command `tex-file' runs TeX on the file specified by `tex-main-file'
if the variable is non-nil."
  :type '(choice (const :tag "None" nil)
                 file)
  :group 'tex-file)

;;;###autoload
(defcustom tex-offer-save t
  "*If non-nil, ask about saving modified buffers before \\[tex-file] is run."
  :type 'boolean
  :group 'tex-file)

;;;###autoload
(defcustom tex-run-command "tex"
  "*Command used to run TeX subjob.
TeX Mode sets `tex-command' to this string.
See the documentation of that variable."
  :type 'string
  :group 'tex-run)

;;;###autoload
(defcustom latex-run-command "latex"
  "*Command used to run LaTeX subjob.
LaTeX Mode sets `tex-command' to this string.
See the documentation of that variable."
  :type 'string
  :group 'tex-run)

;;;###autoload
(defcustom slitex-run-command "slitex"
  "*Command used to run SliTeX subjob.
SliTeX Mode sets `tex-command' to this string.
See the documentation of that variable."
  :type 'string
  :group 'tex-run)

;;;###autoload
(defcustom tex-start-options-string "\\nonstopmode\\input"
  "*TeX options to use when running TeX.
These precede the input file name. If nil, TeX runs without option.
See the documentation of `tex-command'."
  :type '(radio (const :tag "Interactive \(nil\)" nil)
		(const :tag "Nonstop \(\"\\nonstopmode\\input\"\)"
		       "\\nonstopmode\\input")
		(string :tag "String at your choice"))
  :group 'tex-run
  :version "20.4")

(defvar standard-latex-block-names
  '("abstract"		"array"		"center"	"description"
    "displaymath"	"document"	"enumerate"	"eqnarray"
    "eqnarray*"		"equation"	"figure"	"figure*"
    "flushleft"		"flushright"	"itemize"	"letter"
    "list"		"minipage"	"picture"	"quotation"
    "quote"		"slide"		"sloppypar"	"tabbing"
    "table"		"table*"	"tabular"	"tabular*"
    "thebibliography"	"theindex*"	"titlepage"	"trivlist"
    "verbatim"		"verbatim*"	"verse"		"math")
  "Standard LaTeX block names.")

;;;###autoload
(defcustom latex-block-names nil
  "*User defined LaTeX block names.
Combined with `standard-latex-block-names' for minibuffer completion."
  :type '(repeat string)
  :group 'tex-run)

;;;###autoload
(defcustom tex-bibtex-command "bibtex"
  "*Command used by `tex-bibtex-file' to gather bibliographic data.
If this string contains an asterisk (`*'), that is replaced by the file name;
otherwise, the file name, preceded by blank, is added at the end."
  :type 'string
  :group 'tex-run)

;;;###autoload
(defcustom tex-dvi-print-command "lpr -d"
  "*Command used by \\[tex-print] to print a .dvi file.
If this string contains an asterisk (`*'), that is replaced by the file name;
otherwise, the file name, preceded by blank, is added at the end."
  :type 'string
  :group 'tex-view)

;;;###autoload
(defcustom tex-alt-dvi-print-command "lpr -d"
  "*Command used by \\[tex-print] with a prefix arg to print a .dvi file.
If this string contains an asterisk (`*'), that is replaced by the file name;
otherwise, the file name, preceded by blank, is added at the end.

If two printers are not enough of a choice, you can set the variable
`tex-alt-dvi-print-command' to an expression that asks what you want;
for example,

    (setq tex-alt-dvi-print-command
         '(format \"lpr -P%s\" (read-string \"Use printer: \")))

would tell \\[tex-print] with a prefix argument to ask you which printer to
use."
  :type '(choice (string :tag "Command")
		 (sexp :tag "Expression"))
  :group 'tex-view)

;;;###autoload
(defcustom tex-dvi-view-command nil
  "*Command used by \\[tex-view] to display a `.dvi' file.
If this string contains an asterisk (`*'), that is replaced by the file name;
otherwise, the file name, preceded by blank, is added at the end.

This can be set conditionally so that the previewer used is suitable for the
window system being used.  For example,

    (setq tex-dvi-view-command
          (if (eq window-system 'x) \"xdvi\" \"dvi2tty * | cat -s\"))

would tell \\[tex-view] to use xdvi under X windows and to use dvi2tty
otherwise."
  :type '(choice (const nil) string)
  :group 'tex-view)

;;;###autoload
(defcustom tex-show-queue-command "lpq"
  "*Command used by \\[tex-show-print-queue] to show the print queue.
Should show the queue(s) that \\[tex-print] puts jobs on."
  :type 'string
  :group 'tex-view)

;;;###autoload
(defcustom tex-default-mode 'latex-mode
  "*Mode to enter for a new file that might be either TeX or LaTeX.
This variable is used when it can't be determined whether the file
is plain TeX or LaTeX or what because the file contains no commands.
Normally set to either `plain-tex-mode' or `latex-mode'."
  :type 'function
  :group 'tex)

;;;###autoload
(defcustom tex-open-quote "``"
  "*String inserted by typing \\[tex-insert-quote] to open a quotation."
  :type 'string
  :group 'tex)

;;;###autoload
(defcustom tex-close-quote "''"
  "*String inserted by typing \\[tex-insert-quote] to close a quotation."
  :type 'string
  :group 'tex)

(defvar tex-last-temp-file nil
  "Latest temporary file generated by \\[tex-region] and \\[tex-buffer].
Deleted when the \\[tex-region] or \\[tex-buffer] is next run, or when the
tex shell terminates.")

(defvar tex-command nil
  "*Command to run TeX.
If this string contains an asterisk \(`*'\), that is replaced by the file name\;
otherwise the \(shell-quoted\) value of `tex-start-options-string' and
the file name are added at the end, with blanks as separators.

In TeX, LaTeX, and SliTeX Mode this variable becomes buffer local.
In these modes, use \\[set-variable] if you want to change it for the
current buffer.")

(defvar tex-trailer nil
  "String appended after the end of a region sent to TeX by \\[tex-region].")

(defvar tex-start-of-header nil
  "Regular expression used by \\[tex-region] to find start of file's header.")

(defvar tex-end-of-header nil
  "Regular expression used by \\[tex-region] to find end of file's header.")

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

(easy-mmode-defsyntax tex-mode-syntax-table
  '((?% . "<")
    (?\n . ">")
    (?\f . ">")
    (?\C-@ . "w")
    (?' . "w")
    (?@ . "_")
    (?* . "_")
    (?\t . " ")
    (?~ . " ")
    (?$ . "$$")
    (?\\ . "/")
    (?\" . ".")
    (?& . ".")
    (?_ . ".")
    (?^ . "."))
  "Syntax table used while in TeX mode.")

;;;;
;;;; Imenu support
;;;;

(defcustom latex-imenu-indent-string ". "
  "*String to add repeated in front of nested sectional units for Imenu.
An alternative value is \" . \", if you use a font with a narrow period."
  :type 'string
  :group 'tex)

(defvar latex-section-alist
  '(("part" . 0) ("chapter" . 1)
    ("section" . 2) ("subsection" . 3)
    ("subsubsection" . 4)
    ("paragraph" . 5) ("subparagraph" . 6)))

(defvar latex-metasection-list
  '("documentstyle" "documentclass"
    "begin{document}" "end{document}"
    "appendix" "frontmatter" "mainmatter" "backmatter"))

(defun latex-imenu-create-index ()
  "Generate an alist for imenu from a LaTeX buffer."
  (let ((section-regexp
	 (concat "\\\\" (regexp-opt (mapcar 'car latex-section-alist) t)
		 "\\*?[ \t]*{"))
	(metasection-regexp
	 (concat "\\\\" (regexp-opt latex-metasection-list t)))
	i0 menu case-fold-search)
    (save-excursion
      ;; Find the top-most level in this file but don't allow it to be
      ;; any deeper than "section" (which is top-level in an article).
      (goto-char (point-min))
      (if (search-forward-regexp "\\\\part\\*?[ \t]*{" nil t)
	  (setq i0 0)
	(if (search-forward-regexp "\\\\chapter\\*?[ \t]*{" nil t)
	    (setq i0 1)
	  (setq i0 2)))

      ;; Look for chapters and sections.
      (goto-char (point-min))
      (while (search-forward-regexp section-regexp nil t)
	(let ((start (match-beginning 0))
	      (here (point))
	      (i (cdr (assoc (buffer-substring-no-properties
			      (match-beginning 1)
			      (match-end 1))
			     latex-section-alist))))
	  (backward-char 1)
	  (condition-case err
	      (progn
		;; Using sexps allows some use of matching {...} inside
		;; titles.
		(forward-sexp 1)
		(push (cons (concat (apply 'concat
					   (make-list
					    (max 0 (- i i0))
					    latex-imenu-indent-string))
				    (buffer-substring-no-properties
				     here (1- (point))))
			    start)
		      menu))
	    (error nil))))

      ;; Look for included material.
      (goto-char (point-min))
      (while (search-forward-regexp
	      "\\\\\\(include\\|input\\|verbatiminput\\|bibliography\\)\
\[ \t]*{\\([^}\n]+\\)}"
	      nil t)
	(push (cons (concat "<<" (buffer-substring-no-properties
				  (match-beginning 2)
				  (match-end 2))
			    (if (= (char-after (match-beginning 1)) ?b)
				".bbl"
			      ".tex"))
		    (match-beginning 0))
	      menu))

      ;; Look for \frontmatter, \mainmatter, \backmatter, and \appendix.
      (goto-char (point-min))
      (while (search-forward-regexp metasection-regexp nil t)
	(push (cons "--" (match-beginning 0)) menu))

      ;; Sort in increasing buffer position order.
      (sort menu (function (lambda (a b) (< (cdr a) (cdr b))))))))

;;;;
;;;; Outline support
;;;;

(defvar latex-outline-regexp
  (concat "\\\\"
	  (regexp-opt (append latex-metasection-list
			      (mapcar 'car latex-section-alist)) t)))

(defun latex-outline-level ()
  (if (looking-at latex-outline-regexp)
      (1+ (or (cdr (assoc (match-string 1) latex-section-alist)) -1))
    1000))

;;;;
;;;; Font-Lock support
;;;;

;(defvar tex-font-lock-keywords
;  ;; Regexps updated with help from Ulrik Dickow <dickow@nbi.dk>.
;  '(("\\\\\\(begin\\|end\\|newcommand\\){\\([a-zA-Z0-9\\*]+\\)}"
;     2 font-lock-function-name-face)
;    ("\\\\\\(cite\\|label\\|pageref\\|ref\\){\\([^} \t\n]+\\)}"
;     2 font-lock-constant-face)
;    ;; It seems a bit dubious to use `bold' and `italic' faces since we might
;    ;; not be able to display those fonts.
;    ("{\\\\bf\\([^}]+\\)}" 1 'bold keep)
;    ("{\\\\\\(em\\|it\\|sl\\)\\([^}]+\\)}" 2 'italic keep)
;    ("\\\\\\([a-zA-Z@]+\\|.\\)" . font-lock-keyword-face)
;    ("^[ \t\n]*\\\\def[\\\\@]\\(\\w+\\)" 1 font-lock-function-name-face keep))
;  ;; Rewritten and extended for LaTeX2e by Ulrik Dickow <dickow@nbi.dk>.
;  '(("\\\\\\(begin\\|end\\|newcommand\\){\\([a-zA-Z0-9\\*]+\\)}"
;     2 font-lock-function-name-face)
;    ("\\\\\\(cite\\|label\\|pageref\\|ref\\){\\([^} \t\n]+\\)}"
;     2 font-lock-constant-face)
;    ("^[ \t]*\\\\def\\\\\\(\\(\\w\\|@\\)+\\)" 1 font-lock-function-name-face)
;    "\\\\\\([a-zA-Z@]+\\|.\\)"
;    ;; It seems a bit dubious to use `bold' and `italic' faces since we might
;    ;; not be able to display those fonts.
;    ;; LaTeX2e: \emph{This is emphasized}.
;    ("\\\\emph{\\([^}]+\\)}" 1 'italic keep)
;    ;; LaTeX2e: \textbf{This is bold}, \textit{...}, \textsl{...}
;    ("\\\\text\\(\\(bf\\)\\|it\\|sl\\){\\([^}]+\\)}"
;     3 (if (match-beginning 2) 'bold 'italic) keep)
;    ;; Old-style bf/em/it/sl.  Stop at `\\' and un-escaped `&', for tables.
;    ("\\\\\\(\\(bf\\)\\|em\\|it\\|sl\\)\\>\\(\\([^}&\\]\\|\\\\[^\\]\\)+\\)"
;     3 (if (match-beginning 2) 'bold 'italic) keep))

;; Rewritten with the help of Alexandra Bac <abac@welcome.disi.unige.it>.
(defconst tex-font-lock-keywords-1
  (eval-when-compile
    (let* (;; Names of commands whose arg should be fontified as heading, etc.
	   (headings (regexp-opt
		      '("title"  "begin" "end" "chapter" "part"
			"section" "subsection" "subsubsection"
			"paragraph" "subparagraph" "subsubparagraph"
			"newcommand" "renewcommand" "newenvironment"
			"newtheorem")
		      t))
	   (variables (regexp-opt
		       '("newcounter" "newcounter*" "setcounter" "addtocounter"
			 "setlength" "addtolength" "settowidth")
		       t))
	   (includes (regexp-opt
		      '("input" "include" "includeonly" "bibliography"
			"epsfig" "psfig" "epsf" "nofiles" "usepackage"
			"documentstyle" "documentclass" "verbatiminput"
			"includegraphics" "includegraphics*")
		      t))
	   ;; Miscellany.
	   (slash "\\\\")
	   (opt " *\\(\\[[^]]*\\] *\\)*")
	   ;; This would allow highlighting \newcommand\CMD but requires
	   ;; adapting subgroup numbers below.
	   ;; (arg "\\(?:{\\(\\(?:[^{}\\]+\\|\\\\.\\|{[^}]*}\\)+\\)\\|\\\\[a-z*]+\\)"))
	   (arg "{\\(\\(?:[^{}\\]+\\|\\\\.\\|{[^}]*}\\)+\\)"))
      (list
       ;; Heading args.
       (list (concat slash headings "\\*?" opt arg)
	     ;; If ARG ends up matching too much (if the {} don't match, f.ex)
	     ;; jit-lock will do funny things: when updating the buffer
	     ;; the re-highlighting is only done locally so it will just
	     ;; match the local line, but defer-contextually will
	     ;; match more lines at a time, so ARG will end up matching
	     ;; a lot more, which might suddenly include a comment
	     ;; so you get things highlighted bold when you type them
	     ;; but they get turned back to normal a little while later
	     ;; because "there's already a face there".
	     ;; Using `keep' works around this un-intuitive behavior as well
	     ;; as improves the behavior in the very rare case where you do
	     ;; have a comment in ARG.
	     3 'font-lock-function-name-face 'keep)
       (list (concat slash "\\(re\\)?newcommand\\** *\\(\\\\[A-Za-z@]+\\)")
	     2 'font-lock-function-name-face 'keep)
       ;; Variable args.
       (list (concat slash variables " *" arg) 2 'font-lock-variable-name-face)
       ;; Include args.
       (list (concat slash includes opt arg) 3 'font-lock-builtin-face)
       ;; Definitions.  I think.
       '("^[ \t]*\\\\def *\\\\\\(\\(\\w\\|@\\)+\\)"
	 1 font-lock-function-name-face))))
  "Subdued expressions to highlight in TeX modes.")

(defconst tex-font-lock-keywords-2
  (append tex-font-lock-keywords-1
   (eval-when-compile
     (let* (;;
	    ;; Names of commands whose arg should be fontified with fonts.
	    (bold (regexp-opt '("textbf" "textsc" "textup"
				"boldsymbol" "pmb") t))
	    (italic (regexp-opt '("textit" "textsl" "emph") t))
	    (type (regexp-opt '("texttt" "textmd" "textrm" "textsf") t))
	    ;;
	    ;; Names of commands whose arg should be fontified as a citation.
	    (citations (regexp-opt
			'("label" "ref" "pageref" "vref" "eqref"
			  "cite" "nocite" "index" "glossary" "bibitem"
			  ;; These are text, rather than citations.
			  ;; "caption" "footnote" "footnotemark" "footnotetext"
			  )
			t))
	    ;;
	    ;; Names of commands that should be fontified.
	    (specials-1 (regexp-opt '("\\" "\\*") t)) ;; "-"
	    (specials-2 (regexp-opt
			 '("linebreak" "nolinebreak" "pagebreak" "nopagebreak"
			   "newline" "newpage" "clearpage" "cleardoublepage"
			   "displaybreak" "allowdisplaybreaks"
			   "enlargethispage") t))
	    (general "\\([a-zA-Z@]+\\**\\|[^ \t\n]\\)")
	    ;;
	    ;; Miscellany.
	    (slash "\\\\")
	    (opt " *\\(\\[[^]]*\\] *\\)*")
	    (arg "{\\(\\(?:[^{}\\]+\\|\\\\.\\|{[^}]*}\\)+\\)"))
       (list
	;;
	;; Citation args.
	(list (concat slash citations opt arg) 3 'font-lock-constant-face)
	;;
	;; Text between `` quotes ''.
	(cons (concat (regexp-opt `("``" "\"<" "\"`" "<<" "«") t)
		      "[^'\">»]+"	;a bit pessimistic
		      (regexp-opt `("''" "\">" "\"'" ">>" "»") t))
	      'font-lock-string-face)
	;;
	;; Command names, special and general.
	(cons (concat slash specials-1) 'font-lock-warning-face)
	(list (concat "\\(" slash specials-2 "\\)\\([^a-zA-Z@]\\|\\'\\)")
	      1 'font-lock-warning-face)
	(concat slash general)
	;;
	;; Font environments.  It seems a bit dubious to use `bold' etc. faces
	;; since we might not be able to display those fonts.
	(list (concat slash bold " *" arg) 2 '(quote bold) 'append)
	(list (concat slash italic " *" arg) 2 '(quote italic) 'append)
	;; (list (concat slash type arg) 2 '(quote bold-italic) 'append)
	;;
	;; Old-style bf/em/it/sl.  Stop at `\\' and un-escaped `&', for tables.
	(list (concat "\\\\\\(\\(bf\\)\\|em\\|it\\|sl\\)\\>"
		      "\\(\\([^}&\\]\\|\\\\[^\\]\\)+\\)")
	      3 '(if (match-beginning 2) 'bold 'italic) 'append)))))
   "Gaudy expressions to highlight in TeX modes.")

(defvar tex-font-lock-keywords tex-font-lock-keywords-1
  "Default expressions to highlight in TeX modes.")


(defface tex-math-face
  '((t :inherit font-lock-string-face))
  "Face used to highlight TeX math expressions.")
(defvar tex-math-face 'tex-math-face)

;; Use string syntax but math face for $...$.
(defun tex-font-lock-syntactic-face-function (state)
  (if (nth 3 state) tex-math-face font-lock-comment-face))


(defun tex-define-common-keys (keymap)
  "Define the keys that we want defined both in TeX mode and in the TeX shell."
  (define-key keymap "\C-c\C-k" 'tex-kill-job)
  (define-key keymap "\C-c\C-l" 'tex-recenter-output-buffer)
  (define-key keymap "\C-c\C-q" 'tex-show-print-queue)
  (define-key keymap "\C-c\C-p" 'tex-print)
  (define-key keymap "\C-c\C-v" 'tex-view)

  (define-key keymap [menu-bar tex] (cons "TeX" (make-sparse-keymap "TeX")))

  (define-key keymap [menu-bar tex tex-kill-job]
    '(menu-item "Tex Kill" tex-kill-job :enable (tex-shell-running)))
  (define-key keymap [menu-bar tex tex-recenter-output-buffer]
    '(menu-item "Tex Recenter" tex-recenter-output-buffer
                :enable (get-buffer "*tex-shell*")))
  (define-key keymap [menu-bar tex tex-show-print-queue]
    '("Show Print Queue" . tex-show-print-queue))
  (define-key keymap [menu-bar tex tex-alt-print]
    '(menu-item "Tex Print (alt printer)" tex-alt-print
                :enable (stringp tex-print-file)))
  (define-key keymap [menu-bar tex tex-print]
    '(menu-item "Tex Print" tex-print :enable (stringp tex-print-file)))
  (define-key keymap [menu-bar tex tex-view]
    '(menu-item "Tex View" tex-view :enable (stringp tex-print-file))))

(defvar tex-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map text-mode-map)
    (tex-define-common-keys map)
    (define-key map "\"" 'tex-insert-quote)
    (define-key map "(" 'skeleton-pair-insert-maybe)
    (define-key map "{" 'skeleton-pair-insert-maybe)
    (define-key map "[" 'skeleton-pair-insert-maybe)
    (define-key map "$" 'skeleton-pair-insert-maybe)
    (define-key map "\n" 'tex-terminate-paragraph)
    (define-key map "\t" 'indent-for-tab-command)
    (define-key map "\M-\r" 'latex-insert-item)
    (define-key map "\C-c}" 'up-list)
    (define-key map "\C-c{" 'tex-insert-braces)
    (define-key map "\C-c\C-r" 'tex-region)
    (define-key map "\C-c\C-b" 'tex-buffer)
    (define-key map "\C-c\C-f" 'tex-file)
    (define-key map "\C-c\C-i" 'tex-bibtex-file)
    (define-key map "\C-c\C-o" 'tex-latex-block)
    (define-key map "\C-c\C-e" 'tex-close-latex-block)
    (define-key map "\C-c\C-u" 'tex-goto-last-unclosed-latex-block)
    (define-key map "\C-c\C-m" 'tex-feed-input)
    (define-key map [(control return)] 'tex-feed-input)
    (define-key map [menu-bar tex tex-bibtex-file]
      '("BibTeX File" . tex-bibtex-file))
    (define-key map [menu-bar tex tex-validate-region]
      '(menu-item "Validate Region" tex-validate-region :enable mark-active))
    (define-key map [menu-bar tex tex-validate-buffer]
      '("Validate Buffer" . tex-validate-buffer))
    (define-key map [menu-bar tex tex-region]
      '(menu-item "TeX Region" tex-region :enable mark-active))
    (define-key map [menu-bar tex tex-buffer]
      '("TeX Buffer" . tex-buffer))
    (define-key map [menu-bar tex tex-file] '("TeX File" . tex-file))
    map)
 "Keymap shared by TeX modes.")

(defvar latex-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tex-mode-map)
    map)
  "Keymap for `latex-mode'.  See also `tex-mode-map'.")

(defvar plain-tex-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tex-mode-map)
    map)
  "Keymap for `plain-tex-mode'.  See also `tex-mode-map'.")

(defvar tex-shell-map
  (let ((m (make-sparse-keymap)))
    (set-keymap-parent m shell-mode-map)
    (tex-define-common-keys m)
    m)
  "Keymap for the TeX shell.
Inherits `shell-mode-map' with a few additions.")

(defvar tex-face-alist
  '((bold . "{\\bf ")
    (italic . "{\\it ")
    (bold-italic . "{\\bi ")		; hypothetical
    (underline . "\\underline{")
    (default . "{\\rm "))
  "Alist of face and TeX font name for facemenu.")

(defvar tex-latex-face-alist
  `((italic . "{\\em ")
    ,@tex-face-alist)
  "Alist of face and LaTeX font name for facemenu.")

;; This would be a lot simpler if we just used a regexp search,
;; but then it would be too slow.
;;;###autoload
(defun tex-mode ()
  "Major mode for editing files of input for TeX, LaTeX, or SliTeX.
Tries to determine (by looking at the beginning of the file) whether
this file is for plain TeX, LaTeX, or SliTeX and calls `plain-tex-mode',
`latex-mode', or `slitex-mode', respectively.  If it cannot be determined,
such as if there are no commands in the file, the value of `tex-default-mode'
says which mode to use."
  (interactive)
  (let ((mode tex-default-mode) slash comment)
    (save-excursion
      (goto-char (point-min))
      (while (and (setq slash (search-forward "\\" nil t))
		  (setq comment (let ((search-end (point)))
				  (save-excursion
				    (beginning-of-line)
				    (search-forward "%" search-end t))))))
      (when (and slash (not comment))
	(setq mode
	      (if (looking-at
		   (eval-when-compile
		     (concat
		      (regexp-opt '("documentstyle" "documentclass"
				    "begin" "section" "part" "chapter") 'words)
		      "\\|NeedsTeXFormat{LaTeX")))
		  (if (looking-at
		       "document\\(style\\|class\\)\\(\\[.*\\]\\)?{slides}")
		      'slitex-mode
		    'latex-mode)
		'plain-tex-mode))))
    (funcall mode)))

;;;###autoload
(defalias 'TeX-mode 'tex-mode)
;;;###autoload
(defalias 'plain-TeX-mode 'plain-tex-mode)
;;;###autoload
(defalias 'LaTeX-mode 'latex-mode)

;;;###autoload
(define-derived-mode plain-tex-mode text-mode "TeX"
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

Use \\[tex-validate-buffer] to check buffer for paragraphs containing
mismatched $'s or braces.

Special commands:
\\{plain-tex-mode-map}

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
  (tex-common-initialization)
  (setq tex-command tex-run-command)
  (setq tex-start-of-header "%\\*\\*start of header")
  (setq tex-end-of-header "%\\*\\*end of header")
  (setq tex-trailer "\\bye\n")
  (run-hooks 'tex-mode-hook))

;;;###autoload
(define-derived-mode latex-mode text-mode "LaTeX"
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

Use \\[tex-validate-buffer] to check buffer for paragraphs containing
mismatched $'s or braces.

Special commands:
\\{latex-mode-map}

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
  (tex-common-initialization)
  (setq tex-command latex-run-command)
  (setq tex-start-of-header "\\\\document\\(style\\|class\\)")
  (setq tex-end-of-header "\\\\begin\\s-*{document}")
  (setq tex-trailer "\\end\\s-*{document}\n")
  ;; A line containing just $$ is treated as a paragraph separator.
  ;; A line starting with $$ starts a paragraph,
  ;; but does not separate paragraphs if it has more stuff on it.
  (setq paragraph-start
	(concat "[\f%]\\|[ \t]*\\($\\|\\$\\$\\|"
		"\\\\[][]\\|"
		"\\\\" (regexp-opt (append
				    (mapcar 'car latex-section-alist)
				    '("begin" "label" "end"
				      "item" "bibitem" "newline" "noindent"
				      "newpage" "footnote" "marginpar"
				      "parbox" "caption")) t)
		"\\>\\|\\\\[a-z]*" (regexp-opt '("space" "skip" "page") t)
		"\\>\\)"))
  (setq paragraph-separate
	(concat "[\f%]\\|[ \t]*\\($\\|"
		"\\\\[][]\\|"
		"\\\\" (regexp-opt (append
				    (mapcar 'car latex-section-alist)
				    '("begin" "label" "end" )) t)
		"\\>\\|\\\\\\(" (regexp-opt '("item" "bibitem" "newline"
					      "noindent" "newpage" "footnote"
					      "marginpar" "parbox" "caption"))
		"\\|\\$\\$\\|[a-z]*\\(space\\|skip\\|page[a-z]*\\)"
		"\\>\\)[ \t]*\\($\\|%\\)\\)"))
  (set (make-local-variable 'imenu-create-index-function)
       'latex-imenu-create-index)
  (set (make-local-variable 'tex-face-alist) tex-latex-face-alist)
  (set (make-local-variable 'fill-nobreak-predicate)
       'latex-fill-nobreak-predicate)
  (set (make-local-variable 'indent-line-function) 'latex-indent)
  (set (make-local-variable 'fill-indent-according-to-mode) t)
  (set (make-local-variable 'outline-regexp) latex-outline-regexp)
  (set (make-local-variable 'outline-level) 'latex-outline-level)
  (set (make-local-variable 'forward-sexp-function) 'latex-forward-sexp)
  (set (make-local-variable 'skeleton-end-hook) nil)
  (run-hooks 'tex-mode-hook))

;;;###autoload
(define-derived-mode slitex-mode latex-mode "SliTeX"
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

Use \\[tex-validate-buffer] to check buffer for paragraphs containing
mismatched $'s or braces.

Special commands:
\\{slitex-mode-map}

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
  (setq tex-command slitex-run-command)
  (setq tex-start-of-header "\\\\documentstyle{slides}\\|\\\\documentclass{slides}"))

(defun tex-common-initialization ()
  (set-syntax-table tex-mode-syntax-table)
  ;; Regexp isearch should accept newline and formfeed as whitespace.
  (set (make-local-variable 'search-whitespace-regexp) "[ \t\r\n\f]+")
  ;; A line containing just $$ is treated as a paragraph separator.
  (set (make-local-variable 'paragraph-start)
       "[ \t]*$\\|[\f\\\\%]\\|[ \t]*\\$\\$")
  ;; A line starting with $$ starts a paragraph,
  ;; but does not separate paragraphs if it has more stuff on it.
  (set (make-local-variable 'paragraph-separate)
	"[ \t]*$\\|[\f\\\\%]\\|[ \t]*\\$\\$[ \t]*$")
  (set (make-local-variable 'comment-start) "%")
  (set (make-local-variable 'comment-add) 1)
  (set (make-local-variable 'comment-start-skip)
       "\\(\\(^\\|[^\\]\\)\\(\\\\\\\\\\)*\\)\\(%+ *\\)")
  (set (make-local-variable 'parse-sexp-ignore-comments) t)
  (set (make-local-variable 'compare-windows-whitespace)
       'tex-categorize-whitespace)
  (set (make-local-variable 'facemenu-add-face-function)
       (lambda (face end)
	 (let ((face-text (cdr (assq face tex-face-alist))))
	   (if face-text
	       face-text
	     (error "Face %s not configured for %s mode" face mode-name)))))
  (set (make-local-variable 'facemenu-end-add-face) "}")
  (set (make-local-variable 'facemenu-remove-face-function) t)
  (set (make-local-variable 'font-lock-defaults)
       '((tex-font-lock-keywords
	  tex-font-lock-keywords-1 tex-font-lock-keywords-2)
	 nil nil ((?$ . "\"")) nil
	 ;; Who ever uses that anyway ???
	 (font-lock-mark-block-function . mark-paragraph)
	 (font-lock-syntactic-face-function
	  . tex-font-lock-syntactic-face-function)))
  (make-local-variable 'tex-command)
  (make-local-variable 'tex-start-of-header)
  (make-local-variable 'tex-end-of-header)
  (make-local-variable 'tex-trailer))

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
	     (if (eq category 'null)
		 (setq category 'space)))
	    ((looking-at "\n")
	     (cond ((eq category 'newline)
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
    (if (eq category 'newline)
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
     (cond ((= (preceding-char) ?\\) ?\")
	   ((memq (char-syntax (preceding-char)) '(?\( ?> ?\ )) tex-open-quote)
	   (t tex-close-quote)))))

(defun tex-validate-buffer ()
  "Check current buffer for paragraphs containing mismatched braces or $s.
Their positions are recorded in the buffer `*Occur*'.
To find a particular invalidity from `*Occur*', switch to that buffer
and type C-c C-c or click with mouse-2
on the line for the invalidity you want to see."
  (interactive)
  (let ((buffer (current-buffer))
	(prevpos (point-min))
	(linenum nil)
	(num-matches 0))
    (with-output-to-temp-buffer "*Occur*"
      (princ "Mismatches:\n")
      (save-excursion
	(set-buffer standard-output)
	(occur-mode)
	(setq occur-buffer buffer)
	(setq occur-nlines 0))
      (save-excursion
	(goto-char (point-max))
	(while (and (not (input-pending-p)) (not (bobp)))
	  (let ((end (point))
		prev-end)
	    ;; Scan the previous paragraph for invalidities.
	    (if (search-backward "\n\n" nil t)
		(progn
		  (setq prev-end (point))
		  (forward-char 2))
	      (goto-char (setq prev-end (point-min))))
	    (or (tex-validate-region (point) end)
		(let* ((oend end)
		       (end (save-excursion (forward-line 1) (point)))
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
		  ;; add each mismatch at the beginning of *Occur*.
		  (save-excursion
		    (setq tem (point-marker))
		    (set-buffer standard-output)
		    (goto-char (point-min))
		    ;; Skip "Mismatches:" header line.
		    (forward-line 1)
		    (setq num-matches (1+ num-matches))
		    (insert-buffer-substring buffer start end)
		    (let (text-beg (text-end (point-marker)))
		      (forward-char (- start end))
		      (setq text-beg (point-marker))
		      (insert (format "%3d: " linenum))
		      (add-text-properties
		       text-beg (- text-end 1)
		       '(mouse-face highlight
			 help-echo "mouse-2: go to this invalidity"))
		      (put-text-property text-beg (- text-end 1)
					 'occur tem)))))
	    (goto-char prev-end))))
      (with-current-buffer standard-output
	(if (eq num-matches 0)
	    (insert "None!\n"))
	(if (interactive-p)
	    (message "%d mismatches found" num-matches))))))

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
	    ;; First check that the open and close parens balance in numbers.
	    (goto-char start)
	    (while (<= 0 (setq max-possible-sexps (1- max-possible-sexps)))
	      (forward-sexp 1))
	    ;; Now check that like matches like.
	    (goto-char start)
	    (while (progn (skip-syntax-forward "^(")
			  (not (eobp)))
	      (let ((match (matching-paren (following-char))))
		(save-excursion
		  (forward-sexp 1)
		  (or (= (preceding-char) match)
		      (error "Mismatched parentheses"))))
	      (forward-char 1)))
	(error
	 (skip-syntax-forward " .>")
	 (setq failure-point (point)))))
    (if failure-point (goto-char failure-point))
    (not failure-point)))

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

;; This function is used as the value of fill-nobreak-predicate
;; in LaTeX mode.  Its job is to prevent line-breaking inside
;; of a \verb construct.
(defun latex-fill-nobreak-predicate ()
  (let ((opoint (point))
	inside)
    (save-excursion
      (save-restriction
	(beginning-of-line)
	(narrow-to-region (point) opoint)
	(while (re-search-forward "\\\\verb\\(.\\)" nil t)
	  (unless (re-search-forward (regexp-quote (match-string 1)) nil t)
	    (setq inside t)))))
    inside))

(defvar latex-block-default "enumerate")

;; Like tex-insert-braces, but for LaTeX.
(define-skeleton tex-latex-block
  "Create a matching pair of lines \\begin[OPT]{NAME} and \\end{NAME} at point.
Puts point on a blank line between them."
  (let ((choice (completing-read (format "LaTeX block name [%s]: "
					 latex-block-default)
				 (mapcar 'list
					 (append standard-latex-block-names
						 latex-block-names))
				 nil nil nil nil latex-block-default)))
    (setq latex-block-default choice)
    (unless (or (member choice standard-latex-block-names)
		(member choice latex-block-names))
      ;; Remember new block names for later completion.
      (push choice latex-block-names))
    choice)
  \n "\\begin{" str ?\}
  ?\[ (skeleton-read "[options]: ") & ?\] | -1
  > \n _ \n
  "\\end{" str ?\} > \n)

(define-skeleton latex-insert-item
  "Insert a \item macro."
  nil
  \n "\\item " >)


;;;;
;;;; LaTeX syntax navigation
;;;;

(defun tex-last-unended-begin ()
  "Leave point at the beginning of the last `\\begin{...}' that is unended."
  (while (and (re-search-backward "\\\\\\(begin\\|end\\)\\s *{")
              (looking-at "\\\\end"))
    (tex-last-unended-begin)))

(defun tex-next-unmatched-end ()
  "Leave point at the end of the next `\\end' that is unended."
  (while (and (re-search-forward "\\\\\\(begin\\|end\\)\\s *{[^}]+}")
	      (save-excursion (goto-char (match-beginning 0))
			      (looking-at "\\\\begin")))
    (tex-next-unmatched-end)))

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

(defun latex-backward-sexp-1 ()
  "Like (backward-sexp 1) but aware of multi-char elements."
  (let ((pos (point))
	(forward-sexp-function))
    (backward-sexp 1)
    (if (looking-at "\\\\begin\\>")
	(signal 'scan-error
		(list "Containing expression ends prematurely"
		      (point) (prog1 (point) (goto-char pos))))
      (when (eq (char-after) ?{)
	(let ((newpos (point)))
	  (when (ignore-errors (backward-sexp 1) t)
	    (if (looking-at "\\\\end\\>")
		(tex-last-unended-begin)
	      (goto-char newpos))))))))

(defun latex-forward-sexp-1 ()
  "Like (forward-sexp 1) but aware of multi-char elements."
  (let ((pos (point))
	(forward-sexp-function))
    (forward-sexp 1)
    (let ((newpos (point)))
      (skip-syntax-backward "/w")
      (cond
       ((looking-at "\\\\end\\>")
	(signal 'scan-error
		(list "Containing expression ends prematurely"
		      (point)
		      (prog1
			  (progn (ignore-errors (forward-sexp 2)) (point))
			(goto-char pos)))))
       ((looking-at "\\\\begin\\>")
	(goto-char (match-end 0))
	(tex-next-unmatched-end))
       (t (goto-char newpos))))))

(defun latex-forward-sexp (&optional arg)
  "Like `forward-sexp' but aware of multi-char elements."
  (interactive "P")
  (unless arg (setq arg 1))
  (let ((pos (point)))
    (condition-case err
	(while (/= arg 0)
	  (setq arg
		(if (> arg 0)
		    (progn (latex-forward-sexp-1) (1- arg))
		  (progn (latex-backward-sexp-1) (1+ arg)))))
      (scan-error
       (goto-char pos)
       (signal (car err) (cdr err))))))

(defun latex-syntax-after ()
  "Like (char-syntax (char-after)) but aware of multi-char elements."
  (if (looking-at "\\\\end\\>") ?\) (char-syntax (following-char))))

(defun latex-skip-close-parens ()
  "Like (skip-syntax-forward \" )\") but aware of multi-char elements."
  (let ((forward-sexp-function nil))
    (while (progn (skip-syntax-forward " )")
		  (looking-at "\\\\end\\>"))
      (forward-sexp 2))))

(defun latex-down-list ()
  "Like (down-list 1) but aware of multi-char elements."
  (forward-comment (point-max))
  (let ((forward-sexp-function nil))
    (if (not (looking-at "\\\\begin\\>"))
	(down-list 1)
      (forward-sexp 1)
      ;; Skip arguments.
      (while (looking-at "[ \t]*\\s(") (forward-sexp)))))

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

(defconst tex-discount-args-cmds
  '("begin" "end" "input" "special" "cite" "ref" "include" "includeonly"
    "documentclass" "usepackage" "label")
  "TeX commands whose arguments should not be counted as text.")

(defun tex-count-words (begin end)
  "Count the number of words in the buffer."
  (interactive
   (if (and transient-mark-mode mark-active)
       (list (region-beginning) (region-end))
     (list (point-min) (point-max))))
  ;; TODO: skip comments and math and maybe some environments.
  (save-excursion
    (goto-char begin)
    (let ((count 0))
      (while (and (< (point) end) (re-search-forward "\\<" end t))
	(if (not (eq (char-syntax (preceding-char)) ?/))
	    (progn
	      ;; Don't count single-char words.
	      (unless (looking-at ".\\>") (incf count))
	      (forward-char 1))
	  (let ((cmd
		 (buffer-substring-no-properties
		  (point) (progn (when (zerop (skip-chars-forward "a-zA-Z@"))
				   (forward-char 1))
				 (point)))))
	    (when (member cmd tex-discount-args-cmds)
	      (skip-chars-forward "*")
	      (forward-comment (point-max))
	      (when (looking-at "\\[")
		(forward-sexp 1)
		(forward-comment (point-max)))
	      (if (not (looking-at "{"))
		  (forward-char 1)
		(forward-sexp 1))))))
      (message "%s words" count))))



;;; Invoking TeX in an inferior shell.

;; Why use a shell instead of running TeX directly?  Because if TeX
;; gets stuck, the user can switch to the shell window and type at it.

;; The utility functions:

(define-derived-mode tex-shell shell-mode "TeX-Shell"
  (compilation-shell-minor-mode t))

;;;###autoload
(defun tex-start-shell ()
  (with-current-buffer
      (make-comint
       "tex-shell"
       (or tex-shell-file-name (getenv "ESHELL") shell-file-name)
       nil)
    (let ((proc (get-process "tex-shell")))
      (set-process-sentinel proc 'tex-shell-sentinel)
      (process-kill-without-query proc)
      (tex-shell)
      (while (zerop (buffer-size))
	(sleep-for 1)))))

(defun tex-feed-input ()
  "Send input to the tex shell process.
In the tex buffer this can be used to continue an interactive tex run.
In the tex shell buffer this command behaves like `comint-send-input'."
  (interactive)
  (set-buffer (tex-shell-buf))
  (comint-send-input)
  (tex-recenter-output-buffer nil))

(defun tex-display-shell ()
  "Make the TeX shell buffer visible in a window."
  (display-buffer (tex-shell-buf))
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

(defun tex-shell-proc ()
  (or (tex-shell-running) (error "No TeX subprocess")))
(defun tex-shell-buf ()
  (process-buffer (tex-shell-proc)))
(defun tex-shell-buf-no-error ()
  (let ((proc (tex-shell-running)))
    (and proc (process-buffer proc))))

(defun tex-send-command (command &optional file background)
  "Send COMMAND to TeX shell process, substituting optional FILE for *.
Do this in background if optional BACKGROUND is t.  If COMMAND has no *,
FILE will be appended, preceded by a blank, to COMMAND.  If FILE is nil, no
substitution will be made in COMMAND.  COMMAND can be any expression that
evaluates to a command string.

Return the process in which TeX is running."
  (save-excursion
    (let* ((cmd (eval command))
	   (proc (tex-shell-proc))
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
      (setq tex-send-command-modified-tick (buffer-modified-tick buf))
      proc)))

(defun tex-delete-last-temp-files (&optional not-all)
  "Delete any junk files from last temp file.
If NOT-ALL is non-nil, save the `.dvi' file."
  (if tex-last-temp-file
      (let* ((dir (file-name-directory tex-last-temp-file))
	     (list (and (file-directory-p dir)
			(file-name-all-completions
			 (file-name-sans-extension
			  (file-name-nondirectory tex-last-temp-file))
			 dir))))
	(while list
	  (if not-all
	      (and
	       ;; If arg is non-nil, don't delete the .dvi file.
	       (not (string-match "\\.dvi$" (car list)))
	       (delete-file (concat dir (car list))))
	    (delete-file (concat dir (car list))))
          (setq list (cdr list))))))

(add-hook 'kill-emacs-hook 'tex-delete-last-temp-files)

(defun tex-guess-main-file (&optional all)
  "Find a likely `tex-main-file'.
Looks for hints in other buffers in the same directory or in
ALL other buffers."
  (let ((dir default-directory)
	(header-re tex-start-of-header))
    (catch 'found
      ;; Look for a buffer with `tex-main-file' set.
      (dolist (buf (if (consp all) all (buffer-list)))
	(with-current-buffer buf
	  (when (and (or all (equal dir default-directory))
		     (stringp tex-main-file))
	    (throw 'found (expand-file-name tex-main-file)))))
      ;; Look for a buffer containing the magic `tex-start-of-header'.
      (dolist (buf (if (consp all) all (buffer-list)))
	(with-current-buffer buf
	  (when (and (or all (equal dir default-directory))
		     buffer-file-name
		     ;; (or (easy-mmode-derived-mode-p 'latex-mode)
		     ;; 	 (easy-mmode-derived-mode-p 'plain-tex-mode))
		     (save-excursion
		       (save-restriction
			 (widen)
			 (goto-char (point-min))
			 (re-search-forward header-re 10000 t))))
	    (throw 'found (expand-file-name buffer-file-name))))))))

(defun tex-main-file ()
  "Return the relative name of the main file."
  (let* ((file (or tex-main-file
		   ;; Compatibility with AUCTeX.
		   (and (boundp 'TeX-master) (stringp TeX-master)
			(set (make-local-variable 'tex-main-file) TeX-master))
		   ;; Try to guess the main file.
		   (if (not buffer-file-name)
		       (error "Buffer is not associated with any file")
		     (file-relative-name
		      (if (save-excursion
			    (goto-char (point-min))
			    (re-search-forward tex-start-of-header 10000 t))
			  ;; This is the main file.
			  buffer-file-name
			;; This isn't the main file, let's try to find better,
			(or (tex-guess-main-file)
			    ;; (tex-guess-main-file t)
			    buffer-file-name)))))))
    (if (file-exists-p file) file (concat file ".tex"))))


(defun tex-start-tex (command file &optional dir)
  "Start a TeX run, using COMMAND on FILE."
  (let* ((star (string-match "\\*" command))
         (compile-command
          (if star
	      (concat (substring command 0 star)
		      (comint-quote-filename file)
		      (substring command (1+ star)))
            (concat command " "
		    (if (< 0 (length tex-start-options-string))
			(concat
			 (shell-quote-argument tex-start-options-string) " "))
		    (comint-quote-filename file)))))
    (tex-send-tex-command compile-command dir)))

(defun tex-send-tex-command (cmd &optional dir)
  (unless (or (equal dir (let ((buf (tex-shell-buf-no-error)))
                           (and buf (with-current-buffer buf
                                      default-directory))))
	      (not dir))
    (let (shell-dirtrack-verbose)
      (tex-send-command tex-shell-cd-command dir)))
  (with-current-buffer (process-buffer (tex-send-command cmd))
    (make-local-variable 'compilation-parse-errors-function)
    (setq compilation-parse-errors-function 'tex-compilation-parse-errors)
    (setq compilation-last-buffer (current-buffer))
    (compilation-forget-errors)
    ;; Don't parse previous compilations.
    (set-marker compilation-parsing-end (1- (point-max))))
  (tex-display-shell)
  (setq tex-last-buffer-texed (current-buffer)))

(defvar tex-error-parse-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?\( "()" st)
    (modify-syntax-entry ?\) ")(" st)
    (modify-syntax-entry ?\\ "\\" st)
    (modify-syntax-entry ?\{ "_" st)
    (modify-syntax-entry ?\} "_" st)
    (modify-syntax-entry ?\[ "_" st)
    (modify-syntax-entry ?\] "_" st)
    ;; Single quotations may appear in errors
    (modify-syntax-entry ?\" "_" st)
    st)
  "Syntax-table used while parsing TeX error messages.")

(defun tex-compilation-parse-errors (limit-search find-at-least)
  "Parse the current buffer as TeX error messages.
See the variable `compilation-parse-errors-function' for the interface it uses.

This function parses only the last TeX compilation.
It works on TeX compilations only.  It is necessary for that purpose,
since TeX does not put file names and line numbers on the same line as
for the error messages."
  (require 'thingatpt)
  (setq compilation-error-list nil)
  (message "Parsing error messages...")
  (let ((default-directory		; Perhaps dir has changed meanwhile.
	  (file-name-directory (buffer-file-name tex-last-buffer-texed)))
	found-desired (num-errors-found 0)
	last-filename last-linenum last-position
	begin-of-error end-of-error)
    ;; Don't reparse messages already seen at last parse.
    (goto-char compilation-parsing-end)
    ;; Parse messages.
    (while (and (not (or found-desired (eobp)))
		(prog1 (re-search-forward "^! " nil 'move)
		  (setq begin-of-error (match-beginning 0)
			end-of-error (match-end 0)))
		(re-search-forward
		 "^l\\.\\([0-9]+\\) \\(\\.\\.\\.\\)?\\(.*\\)$" nil 'move))
      (let* ((this-error (set-marker (make-marker) begin-of-error))
	     (linenum (string-to-int (match-string 1)))
	     (error-text (regexp-quote (match-string 3)))
	     (filename
	      (save-excursion
		(with-syntax-table tex-error-parse-syntax-table
		  (backward-up-list 1)
		  (skip-syntax-forward "(_")
		  (while (not (file-readable-p (thing-at-point 'filename)))
		    (skip-syntax-backward "(_")
		    (backward-up-list 1)
		    (skip-syntax-forward "(_"))
		  (thing-at-point 'filename))))
	     (new-file
	      (or (null last-filename)
		  (not (string-equal last-filename filename))))
	     (error-location
	      (save-excursion
		(if (equal filename (concat tex-zap-file ".tex"))
		    (set-buffer tex-last-buffer-texed)
		  (set-buffer (find-file-noselect filename)))
		(if new-file
		    (progn (goto-line linenum) (setq last-position nil))
		  (goto-char last-position)
		  (forward-line (- linenum last-linenum)))
		;; first try a forward search for the error text,
		;; then a backward search limited by the last error.
		(let ((starting-point (point)))
		  (or (re-search-forward error-text nil t)
		      (re-search-backward error-text last-position t)
		      (goto-char starting-point)))
		(point-marker))))
	(goto-char this-error)
	(if (and compilation-error-list
		 (or (and find-at-least
			  (>= num-errors-found
			      find-at-least))
		     (and limit-search
			  (>= end-of-error limit-search)))
		 new-file)
	    (setq found-desired t)
	  (setq num-errors-found (1+ num-errors-found)
		last-filename filename
		last-linenum linenum
		last-position error-location
		compilation-error-list	; Add the new error
		(cons (cons this-error error-location)
		      compilation-error-list))
	  (goto-char end-of-error)))))
  (set-marker compilation-parsing-end (point))
  (setq compilation-error-list (nreverse compilation-error-list))
  (message "Parsing error messages...done"))

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
  ;; Temp file will be written and TeX will be run in zap-directory.
  ;; If the TEXINPUTS file has relative directories or if the region has
  ;; \input of files, this must be the same directory as the file for
  ;; TeX to access the correct inputs.  That's why it's safest if
  ;; tex-directory is ".".
  (let* ((zap-directory
          (file-name-as-directory (expand-file-name tex-directory)))
         (tex-out-file (expand-file-name (concat tex-zap-file ".tex")
					 zap-directory)))
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
	      (default-directory zap-directory)
	      (already-output 0))
	  (goto-char (point-min))

          ;; Maybe copy first line, such as `\input texinfo', to temp file.
	  (and tex-first-line-header-regexp
	       (looking-at tex-first-line-header-regexp)
	       (write-region (point)
			     (progn (forward-line 1)
				    (setq already-output (point)))
			     tex-out-file nil nil))

	  ;; Write out the header, if there is one,
	  ;; and any of the specified region which extends before it.
	  ;; But don't repeat anything already written.
	  (if (re-search-forward tex-start-of-header search-end t)
	      (let (hbeg)
		(beginning-of-line)
		(setq hbeg (point))	;mark beginning of header
		(if (re-search-forward tex-end-of-header nil t)
		    (let (hend)
		      (forward-line 1)
		      (setq hend (point)) ;mark end of header
		      (write-region (max (min hbeg beg) already-output)
				    hend
				    tex-out-file
				    (not (zerop already-output)) nil)
		      (setq already-output hend)))))

	  ;; Write out the specified region
	  ;; (but don't repeat anything already written).
	  (write-region (max beg already-output) end
			tex-out-file
			(not (zerop already-output)) nil))
	;; Write the trailer, if any.
	;; Precede it with a newline to make sure it
	;; is not hidden in a comment.
	(if tex-trailer
	    (write-region (concat "\n" tex-trailer) nil
			  tex-out-file t nil))))
    ;; Record the file name to be deleted afterward.
    (setq tex-last-temp-file tex-out-file)
    ;; Use a relative file name here because (1) the proper dir
    ;; is already current, and (2) the abs file name is sometimes
    ;; too long and can make tex crash.
    (tex-start-tex tex-command (concat tex-zap-file ".tex") zap-directory)
    (setq tex-print-file tex-out-file)))

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
  (let* ((source-file (tex-main-file))
	 (file-dir (file-name-directory (expand-file-name source-file))))
    (if tex-offer-save
        (save-some-buffers))
    (if (tex-shell-running)
        (tex-kill-job)
      (tex-start-shell))
    (tex-start-tex tex-command source-file file-dir)
    (setq tex-print-file (expand-file-name source-file))))

(defun tex-generate-zap-file-name ()
  "Generate a unique name suitable for use as a file name."
  ;; Include the shell process number and host name
  ;; in case there are multiple shells (for same or different user).
  ;; Dec 1998: There is a report that some versions of xdvi
  ;; don't work with file names that start with #.
  (format "_TZ_%d-%s"
          (process-id (get-buffer-process "*tex-shell*"))
	  (subst-char-in-string ?. ?- (system-name))))

;; This will perhaps be useful for modifying TEXINPUTS.
;; Expand each file name, separated by colons, in the string S.
(defun tex-expand-files (s)
  (let (elts (start 0))
    (while (string-match ":" s start)
      (setq elts (cons (substring s start (match-beginning 0)) elts))
      (setq start (match-end 0)))
    (or (= start 0)
	(setq elts (cons (substring s start) elts)))
    (mapconcat (lambda (elt)
		 (if (= (length elt) 0) elt (expand-file-name elt)))
	       (nreverse elts) ":")))

(defun tex-shell-running ()
  (let ((proc (get-process "tex-shell")))
    (when proc
      (if (and (eq (process-status proc) 'run)
               (buffer-live-p (process-buffer proc)))
          ;; return the TeX process on success
          proc
          ;; get rid of the process permanently
          ;; this should get rid of the annoying w32 problem with
          ;; dead tex-shell buffer and live process
          (delete-process proc)))))

(defun tex-kill-job ()
  "Kill the currently running TeX job."
  (interactive)
  ;; `quit-process' leads to core dumps of the tex process (except if
  ;; coredumpsize has limit 0kb as on many environments).  One would
  ;; like to use (kill-process proc 'lambda), however that construct
  ;; does not work on some systems and kills the shell itself.
  (let ((proc (get-process "tex-shell")))
    (when proc (quit-process proc t))))

(defun tex-recenter-output-buffer (linenum)
  "Redisplay buffer of TeX job output so that most recent output can be seen.
The last line of the buffer is displayed on
line LINE of the window, or centered if LINE is nil."
  (interactive "P")
  (let ((tex-shell (get-buffer "*tex-shell*"))
	(old-buffer (current-buffer))
	(window))
    (if (null tex-shell)
	(message "No TeX output buffer")
      (setq window (display-buffer tex-shell))
      (save-selected-window
	(select-window window)
	(bury-buffer tex-shell)
	(goto-char (point-max))
	(recenter (if linenum
		      (prefix-numeric-value linenum)
		    (/ (window-height) 2)))))))

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
      (if (tex-shell-running)
          (tex-kill-job)
        (tex-start-shell))
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
The variable `tex-dvi-view-command' specifies the shell command for preview.
You must set that variable yourself before using this command,
because there is no standard value that would generally work."
  (interactive)
  (or tex-dvi-view-command
      (error "You must set `tex-dvi-view-command'"))
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
	;; Try splitting on last period.
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
  (let (shell-dirtrack-verbose
	(tex-out-file
         (tex-append (file-name-nondirectory (buffer-file-name)) ""))
	(file-dir (file-name-directory (buffer-file-name))))
    (tex-send-command tex-shell-cd-command file-dir)
    (tex-send-command tex-bibtex-command tex-out-file))
  (tex-display-shell))

;;;;
;;;; LaTeX indentation
;;;;

(defvar tex-indent-allhanging t)
(defvar tex-indent-arg 4)
(defvar tex-indent-basic 2)
(defvar tex-indent-item tex-indent-basic)
(defvar tex-indent-item-re "\\\\\\(bib\\)?item\\>")

(easy-mmode-defsyntax tex-latex-indent-syntax-table
  '((?$ . ".")
    (?\( . ".")
    (?\) . "."))
  "Syntax table used while computing indentation."
  :copy tex-mode-syntax-table)

(defun latex-indent (&optional arg)
  (with-syntax-table tex-latex-indent-syntax-table
    ;; TODO: Rather than ignore $, we should try to be more clever about it.
    (let ((indent
	   (save-excursion
	     (beginning-of-line)
	     (latex-find-indent))))
      (if (< indent 0) (setq indent 0))
      (if (<= (current-column) (current-indentation))
	  (indent-line-to indent)
	(save-excursion (indent-line-to indent))))))

(defun latex-find-indent (&optional virtual)
  "Find the proper indentation of text after point.
VIRTUAL if non-nil indicates that we're only trying to find the indentation
  in order to determine the indentation of something else.
There might be text before point."
  (save-excursion
    (skip-chars-forward " \t")
    (or
     ;; Trust the current indentation, if such info is applicable.
     (and virtual (>= (current-indentation) (current-column))
	  (current-indentation))
     ;; Put leading close-paren where the matching open brace would be.
     (and (eq (latex-syntax-after) ?\))
	  (ignore-errors
	    (save-excursion
	      (latex-skip-close-parens)
	      (latex-backward-sexp-1)
	      (latex-find-indent 'virtual))))
     ;; Default (maybe an argument)
     (let ((pos (point))
	   (char (char-after))
	   ;; Outdent \item if necessary.
	   (indent (if (looking-at tex-indent-item-re) (- tex-indent-item) 0))
	   up-list-pos)
       ;; Find the previous point which determines our current indentation.
       (condition-case err
	   (progn
	     (latex-backward-sexp-1)
	     (while (> (current-column) (current-indentation))
	       (latex-backward-sexp-1)))
	 (scan-error
	  (setq up-list-pos (nth 2 err))))
       (cond
	((= (point-min) pos) 0) ; We're really just indenting the first line.
	((integerp up-list-pos)
	 ;; Have to indent relative to the open-paren.
	 (goto-char up-list-pos)
	 (if (and (not tex-indent-allhanging)
		  (> pos (progn (latex-down-list)
				(forward-comment (point-max))
				(point))))
	     ;; Align with the first element after the open-paren.
	     (current-column)
	   ;; We're the first element after a hanging brace.
	   (goto-char up-list-pos)
	   (+ indent tex-indent-basic (latex-find-indent 'virtual))))
	;; We're now at the "beginning" of a line.
	((not (and (not virtual) (eq (char-after) ?\\)))
	 ;; Nothing particular here: just keep the same indentation.
	 (+ indent (current-column)))
	;; We're now looking at a macro call.
	((looking-at tex-indent-item-re)
	 ;; Indenting relative to an item, have to re-add the outdenting.
	 (+ indent (current-column) tex-indent-item))
	(t
	 (let ((col (current-column)))
	   (if (not (eq (char-syntax char) ?\())
	       ;; If the first char was not an open-paren, there's
	       ;; a risk that this is really not an argument to the
	       ;; macro at all.
	       (+ indent col)
	     (forward-sexp 1)
	     (if (< (line-end-position)
		    (save-excursion (forward-comment (point-max))
				    (point)))
		 ;; we're indenting the first argument.
		 (min (current-column) (+ tex-indent-arg col))
	       (skip-syntax-forward " ")
	       (current-column))))))))))

(run-hooks 'tex-mode-load-hook)

(provide 'tex-mode)

;;; tex-mode.el ends here
