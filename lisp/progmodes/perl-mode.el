;;; perl-mode.el --- Perl code editing commands for GNU Emacs

;; Copyright (C) 1990, 1994, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008
;; Free Software Foundation, Inc.

;; Author: William F. Mann
;; Maintainer: FSF
;; Adapted-By: ESR
;; Keywords: languages

;; Adapted from C code editing commands 'c-mode.el', Copyright 1987 by the
;; Free Software Foundation, under terms of its General Public License.

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

;; To enter perl-mode automatically, add (autoload 'perl-mode "perl-mode")
;; to your .emacs file and change the first line of your perl script to:
;; #!/usr/bin/perl --	 # -*-Perl-*-
;; With arguments to perl:
;; #!/usr/bin/perl -P-	 # -*-Perl-*-
;; To handle files included with do 'filename.pl';, add something like
;; (setq auto-mode-alist (append (list (cons "\\.pl\\'" 'perl-mode))
;;                               auto-mode-alist))
;; to your .emacs file; otherwise the .pl suffix defaults to prolog-mode.

;; This code is based on the 18.53 version c-mode.el, with extensive
;; rewriting.  Most of the features of c-mode survived intact.

;; I added a new feature which adds functionality to TAB; it is controlled
;; by the variable perl-tab-to-comment.  With it enabled, TAB does the
;; first thing it can from the following list:  change the indentation;
;; move past leading white space; delete an empty comment; reindent a
;; comment; move to end of line; create an empty comment; tell you that
;; the line ends in a quoted string, or has a # which should be a \#.

;; If your machine is slow, you may want to remove some of the bindings
;; to perl-electric-terminator.  I changed the indenting defaults to be
;; what Larry Wall uses in perl/lib, but left in all the options.

;; I also tuned a few things:  comments and labels starting in column
;; zero are left there by perl-indent-exp; perl-beginning-of-function
;; goes back to the first open brace/paren in column zero, the open brace
;; in 'sub ... {', or the equal sign in 'format ... ='; perl-indent-exp
;; (meta-^q) indents from the current line through the close of the next
;; brace/paren, so you don't need to start exactly at a brace or paren.

;; It may be good style to put a set of redundant braces around your
;; main program.  This will let you reindent it with meta-^q.

;; Known problems (these are all caused by limitations in the Emacs Lisp
;; parsing routine (parse-partial-sexp), which was not designed for such
;; a rich language; writing a more suitable parser would be a big job):
;; 2)  The globbing syntax <pattern> is not recognized, so special
;;       characters in the pattern string must be backslashed.
;; 3)  The << quoting operators are not recognized; see below.
;; 5)  To make '$' work correctly, $' is not recognized as a variable.
;;     Use "$'" or $POSTMATCH instead.
;;
;; If you don't use font-lock, additional problems will appear:
;; 1)  Regular expression delimiters do not act as quotes, so special
;;       characters such as `'"#:;[](){} may need to be backslashed
;;       in regular expressions and in both parts of s/// and tr///.
;; 4)  The q and qq quoting operators are not recognized; see below.
;; 5)  To make variables such a $' and $#array work, perl-mode treats
;;       $ just like backslash, so '$' is not treated correctly.
;; 6)  Unfortunately, treating $ like \ makes ${var} be treated as an
;;       unmatched }.  See below.
;; 7)  When ' (quote) is used as a package name separator, perl-mode
;;       doesn't understand, and thinks it is seeing a quoted string.

;; Here are some ugly tricks to bypass some of these problems:  the perl
;; expression /`/ (that's a back-tick) usually evaluates harmlessly,
;; but will trick perl-mode into starting a quoted string, which
;; can be ended with another /`/.  Assuming you have no embedded
;; back-ticks, this can used to help solve problem 3:
;;
;;     /`/; $ugly = q?"'$?; /`/;
;;
;; The same trick can be used for problem 6 as in:
;;     /{/; while (<${glob_me}>)
;; but a simpler solution is to add a space between the $ and the {:
;;     while (<$ {glob_me}>)
;;
;; Problem 7 is even worse, but this 'fix' does work :-(
;;     $DB'stop#'
;;         [$DB'line#'
;;          ] =~ s/;9$//;

;;; Code:

(eval-when-compile (require 'cl))

(defvar font-lock-comment-face)
(defvar font-lock-doc-face)
(defvar font-lock-string-face)

(defgroup perl nil
  "Major mode for editing Perl code."
  :link '(custom-group-link :tag "Font Lock Faces group" font-lock-faces)
  :prefix "perl-"
  :group 'languages)

(defvar perl-mode-abbrev-table nil
  "Abbrev table in use in perl-mode buffers.")
(define-abbrev-table 'perl-mode-abbrev-table ())

(defvar perl-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "{" 'perl-electric-terminator)
    (define-key map "}" 'perl-electric-terminator)
    (define-key map ";" 'perl-electric-terminator)
    (define-key map ":" 'perl-electric-terminator)
    (define-key map "\e\C-a" 'perl-beginning-of-function)
    (define-key map "\e\C-e" 'perl-end-of-function)
    (define-key map "\e\C-h" 'perl-mark-function)
    (define-key map "\e\C-q" 'perl-indent-exp)
    (define-key map "\177" 'backward-delete-char-untabify)
    (define-key map "\t" 'perl-indent-command)
    map)
  "Keymap used in Perl mode.")

(autoload 'c-macro-expand "cmacexp"
  "Display the result of expanding all C macros occurring in the region.
The expansion is entirely correct because it uses the C preprocessor."
  t)

(defvar perl-mode-syntax-table
  (let ((st (make-syntax-table (standard-syntax-table))))
    (modify-syntax-entry ?\n ">" st)
    (modify-syntax-entry ?# "<" st)
    ;; `$' is also a prefix char so I was tempted to say "/ p",
    ;; but the `p' thingy basically overrides the `/' :-(   --stef
    (modify-syntax-entry ?$ "/" st)
    (modify-syntax-entry ?% ". p" st)
    (modify-syntax-entry ?@ ". p" st)
    (modify-syntax-entry ?& "." st)
    (modify-syntax-entry ?\' "\"" st)
    (modify-syntax-entry ?* "." st)
    (modify-syntax-entry ?+ "." st)
    (modify-syntax-entry ?- "." st)
    (modify-syntax-entry ?/ "." st)
    (modify-syntax-entry ?< "." st)
    (modify-syntax-entry ?= "." st)
    (modify-syntax-entry ?> "." st)
    (modify-syntax-entry ?\\ "\\" st)
    (modify-syntax-entry ?` "\"" st)
    (modify-syntax-entry ?| "." st)
    st)
  "Syntax table in use in `perl-mode' buffers.")

(defvar perl-imenu-generic-expression
  '(;; Functions
    (nil "^sub\\s-+\\([-A-Za-z0-9+_:]+\\)" 1)
    ;;Variables
    ("Variables" "^\\([$@%][-A-Za-z0-9+_:]+\\)\\s-*=" 1)
    ("Packages" "^package\\s-+\\([-A-Za-z0-9+_:]+\\);" 1)
    ("Doc sections" "^=head[0-9][ \t]+\\(.*\\)" 1))
  "Imenu generic expression for Perl mode.  See `imenu-generic-expression'.")

;; Regexps updated with help from Tom Tromey <tromey@cambric.colorado.edu> and
;; Jim Campbell <jec@murzim.ca.boeing.com>.

(defconst perl-font-lock-keywords-1
  '(;; What is this for?
    ;;("\\(--- .* ---\\|=== .* ===\\)" . font-lock-string-face)
    ;;
    ;; Fontify preprocessor statements as we do in `c-font-lock-keywords'.
    ;; Ilya Zakharevich <ilya@math.ohio-state.edu> thinks this is a bad idea.
    ;; ("^#[ \t]*include[ \t]+\\(<[^>\"\n]+>\\)" 1 font-lock-string-face)
    ;; ("^#[ \t]*define[ \t]+\\(\\sw+\\)(" 1 font-lock-function-name-face)
    ;; ("^#[ \t]*if\\>"
    ;;  ("\\<\\(defined\\)\\>[ \t]*(?\\(\\sw+\\)?" nil nil
    ;;   (1 font-lock-constant-face) (2 font-lock-variable-name-face nil t)))
    ;; ("^#[ \t]*\\(\\sw+\\)\\>[ \t]*\\(\\sw+\\)?"
    ;;  (1 font-lock-constant-face) (2 font-lock-variable-name-face nil t))
    ;;
    ;; Fontify function and package names in declarations.
    ("\\<\\(package\\|sub\\)\\>[ \t]*\\(\\sw+\\)?"
     (1 font-lock-keyword-face) (2 font-lock-function-name-face nil t))
    ("\\<\\(import\\|no\\|require\\|use\\)\\>[ \t]*\\(\\sw+\\)?"
     (1 font-lock-keyword-face) (2 font-lock-constant-face nil t)))
  "Subdued level highlighting for Perl mode.")

(defconst perl-font-lock-keywords-2
  (append perl-font-lock-keywords-1
   (list
    ;;
    ;; Fontify keywords, except those fontified otherwise.
    (concat "\\<"
	    (regexp-opt '("if" "until" "while" "elsif" "else" "unless"
			  "do" "dump" "for" "foreach" "exit" "die"
			  "BEGIN" "END" "return" "exec" "eval") t)
	    "\\>")
    ;;
    ;; Fontify local and my keywords as types.
    '("\\<\\(local\\|my\\)\\>" . font-lock-type-face)
    ;;
    ;; Fontify function, variable and file name references.
    '("&\\(\\sw+\\(::\\sw+\\)*\\)" 1 font-lock-function-name-face)
    ;; Additionally underline non-scalar variables.  Maybe this is a bad idea.
    ;;'("[$@%*][#{]?\\(\\sw+\\)" 1 font-lock-variable-name-face)
    '("[$*]{?\\(\\sw+\\(::\\sw+\\)*\\)" 1 font-lock-variable-name-face)
    '("\\([@%]\\|\\$#\\)\\(\\sw+\\(::\\sw+\\)*\\)"
      (2 (cons font-lock-variable-name-face '(underline))))
    '("<\\(\\sw+\\)>" 1 font-lock-constant-face)
    ;;
    ;; Fontify keywords with/and labels as we do in `c++-font-lock-keywords'.
    '("\\<\\(continue\\|goto\\|last\\|next\\|redo\\)\\>[ \t]*\\(\\sw+\\)?"
      (1 font-lock-keyword-face) (2 font-lock-constant-face nil t))
    '("^[ \t]*\\(\\sw+\\)[ \t]*:[^:]" 1 font-lock-constant-face)))
  "Gaudy level highlighting for Perl mode.")

(defvar perl-font-lock-keywords perl-font-lock-keywords-1
  "Default expressions to highlight in Perl mode.")

(defvar perl-quote-like-pairs
  '((?\( . ?\)) (?\[ . ?\]) (?\{ . ?\}) (?\< . ?\>)))

;; FIXME: handle here-docs and regexps.
;; <<EOF <<"EOF" <<'EOF' (no space)
;; see `man perlop'
;; ?...?
;; /.../
;; m [...]
;; m /.../
;; q /.../ = '...'
;; qq /.../ = "..."
;; qx /.../ = `...`
;; qr /.../ = precompiled regexp =~=~ m/.../
;; qw /.../
;; s /.../.../
;; s <...> /.../
;; s '...'...'
;; tr /.../.../
;; y /.../.../
;;
;; <file*glob>
(defvar perl-font-lock-syntactic-keywords
  ;; TODO: here-documents ("<<\\(\\sw\\|['\"]\\)")
  '(;; Turn POD into b-style comments
    ("^\\(=\\)\\sw" (1 "< b"))
    ("^=cut[ \t]*\\(\n\\)" (1 "> b"))
    ;; Catch ${ so that ${var} doesn't screw up indentation.
    ;; This also catches $' to handle 'foo$', although it should really
    ;; check that it occurs inside a '..' string.
    ("\\(\\$\\)[{']" (1 ". p"))
    ;; Handle funny names like $DB'stop.
    ("\\$ ?{?^?[_a-zA-Z][_a-zA-Z0-9]*\\('\\)[_a-zA-Z]" (1 "_"))
    ;; format statements
    ("^[ \t]*format.*=[ \t]*\\(\n\\)" (1 '(7)))
    ;; Funny things in `sub' arg-specs like `sub myfun ($)' or `sub ($)'.
    ;; Be careful not to match "sub { (...) ... }".
    ("\\<sub\\(?:[[:space:]]+[^{}[:punct:][:space:]]+\\)?[[:space:]]*(\\([^)]+\\))"
     1 '(1))
    ;; Regexp and funny quotes.
    ("\\(?:[?:.,;=!~({[]\\|\\(^\\)\\)[ \t\n]*\\(/\\)"
     (2 (if (and (match-end 1)
                 (save-excursion
                   (goto-char (match-end 1))
                   (skip-chars-backward " \t\n")
                   (not (memq (char-before)
                              '(?? ?: ?. ?, ?\; ?= ?! ?~ ?\( ?\[)))))
            nil ;; A division sign instead of a regexp-match.
          '(7))))
    ("\\(^\\|[?:.,;=!~({[ \t]\\)\\([msy]\\|q[qxrw]?\\|tr\\)\\>\\s-*\\([^])}> \n\t]\\)"
     ;; Nasty cases:
     ;; /foo/m  $a->m  $#m $m @m %m
     ;; \s (appears often in regexps).
     ;; -s file
     (3 (if (assoc (char-after (match-beginning 3))
		   perl-quote-like-pairs)
	    '(15) '(7))))
    ;; Find and mark the end of funny quotes and format statements.
    (perl-font-lock-special-syntactic-constructs)
    ))

(defvar perl-empty-syntax-table
  (let ((st (copy-syntax-table)))
    ;; Make all chars be of punctuation syntax.
    (dotimes (i 256) (aset st i '(1)))
    (modify-syntax-entry ?\\ "\\" st)
    st)
  "Syntax table used internally for processing quote-like operators.")

(defun perl-quote-syntax-table (char)
  (let ((close (cdr (assq char perl-quote-like-pairs)))
	(st (copy-syntax-table perl-empty-syntax-table)))
    (if (not close)
	(modify-syntax-entry char "\"" st)
      (modify-syntax-entry char "(" st)
      (modify-syntax-entry close ")" st))
    st))

(defun perl-font-lock-special-syntactic-constructs (limit)
  ;; We used to do all this in a font-lock-syntactic-face-function, which
  ;; did not work correctly because sometimes some parts of the buffer are
  ;; treated with font-lock-syntactic-keywords but not with
  ;; font-lock-syntactic-face-function (mostly because of
  ;; font-lock-syntactically-fontified).  That meant that some syntax-table
  ;; properties were missing.  So now we do the parse-partial-sexp loop
  ;; ourselves directly from font-lock-syntactic-keywords, so we're sure
  ;; it's done when necessary.
  (let ((state (syntax-ppss))
        char)
    (while (< (point) limit)
      (cond
       ((or (null (setq char (nth 3 state)))
            (and (characterp char) (eq (char-syntax (nth 3 state)) ?\")))
        ;; Normal text, or comment, or docstring, or normal string.
        nil)
       ((eq (nth 3 state) ?\n)
        ;; A `format' command.
        (save-excursion
          (when (and (re-search-forward "^\\s *\\.\\s *$" nil t)
                     (not (eobp)))
            (put-text-property (point) (1+ (point)) 'syntax-table '(7)))))
       (t
        ;; This is regexp like quote thingy.
        (setq char (char-after (nth 8 state)))
        (save-excursion
          (let ((twoargs (save-excursion
                           (goto-char (nth 8 state))
                           (skip-syntax-backward " ")
                           (skip-syntax-backward "w")
                           (member (buffer-substring
                                    (point) (progn (forward-word 1) (point)))
                                   '("tr" "s" "y"))))
                (close (cdr (assq char perl-quote-like-pairs)))
                (pos (point))
                (st (perl-quote-syntax-table char)))
            (if (not close)
                ;; The closing char is the same as the opening char.
                (with-syntax-table st
                  (parse-partial-sexp (point) (point-max)
                                      nil nil state 'syntax-table)
                  (when twoargs
                    (parse-partial-sexp (point) (point-max)
                                        nil nil state 'syntax-table)))
              ;; The open/close chars are matched like () [] {} and <>.
              (let ((parse-sexp-lookup-properties nil))
                (condition-case err
                    (progn
                      (with-syntax-table st
                        (goto-char (nth 8 state)) (forward-sexp 1))
                      (when twoargs
                        (save-excursion
                          ;; Skip whitespace and make sure that font-lock will
                          ;; refontify the second part in the proper context.
                          (put-text-property
                           (point) (progn (forward-comment (point-max)) (point))
                           'font-lock-multiline t)
                          ;;
                          (unless
                              (save-excursion
                                (with-syntax-table
                                    (perl-quote-syntax-table (char-after))
                                  (forward-sexp 1))
                                (put-text-property pos (line-end-position)
                                                   'jit-lock-defer-multiline t)
                                (looking-at "\\s-*\\sw*e"))
                            (put-text-property (point) (1+ (point))
                                               'syntax-table
                                               (if (assoc (char-after)
                                                          perl-quote-like-pairs)
                                                   '(15) '(7)))))))
                  ;; The arg(s) is not terminated, so it extends until EOB.
                  (scan-error (goto-char (point-max))))))
            ;; Point is now right after the arg(s).
            ;; Erase any syntactic marks within the quoted text.
            (put-text-property pos (1- (point)) 'syntax-table nil)
            (when (eq (char-before (1- (point))) ?$)
              (put-text-property (- (point) 2) (1- (point))
                                 'syntax-table '(1)))
            (put-text-property (1- (point)) (point)
                               'syntax-table (if close '(15) '(7)))))))

      (setq state (parse-partial-sexp (point) limit nil nil state
				      'syntax-table))))
  ;; Tell font-lock that this needs not further processing.
  nil)


(defcustom perl-indent-level 4
  "*Indentation of Perl statements with respect to containing block."
  :type 'integer
  :group 'perl)

;; Is is not unusual to put both things like perl-indent-level and
;; cperl-indent-level in the local variable section of a file. If only
;; one of perl-mode and cperl-mode is in use, a warning will be issued
;; about the variable. Autoload these here, so that no warning is
;; issued when using either perl-mode or cperl-mode.
;;;###autoload(put 'perl-indent-level 'safe-local-variable 'integerp)
;;;###autoload(put 'perl-continued-statement-offset 'safe-local-variable 'integerp)
;;;###autoload(put 'perl-continued-brace-offset 'safe-local-variable 'integerp)
;;;###autoload(put 'perl-brace-offset 'safe-local-variable 'integerp)
;;;###autoload(put 'perl-brace-imaginary-offset 'safe-local-variable 'integerp)
;;;###autoload(put 'perl-label-offset 'safe-local-variable 'integerp)

(defcustom perl-continued-statement-offset 4
  "*Extra indent for lines not starting new statements."
  :type 'integer
  :group 'perl)
(defcustom perl-continued-brace-offset -4
  "*Extra indent for substatements that start with open-braces.
This is in addition to `perl-continued-statement-offset'."
  :type 'integer
  :group 'perl)
(defcustom perl-brace-offset 0
  "*Extra indentation for braces, compared with other text in same context."
  :type 'integer
  :group 'perl)
(defcustom perl-brace-imaginary-offset 0
  "*Imagined indentation of an open brace that actually follows a statement."
  :type 'integer
  :group 'perl)
(defcustom perl-label-offset -2
  "*Offset of Perl label lines relative to usual indentation."
  :type 'integer
  :group 'perl)
(defcustom perl-indent-continued-arguments nil
  "*If non-nil offset of argument lines relative to usual indentation.
If nil, continued arguments are aligned with the first argument."
  :type '(choice integer (const nil))
  :group 'perl)

(defcustom perl-tab-always-indent tab-always-indent
  "Non-nil means TAB in Perl mode always indents the current line.
Otherwise it inserts a tab character if you type it past the first
nonwhite character on the line."
  :type 'boolean
  :group 'perl)

;; I changed the default to nil for consistency with general Emacs
;; conventions -- rms.
(defcustom perl-tab-to-comment nil
  "*Non-nil means TAB moves to eol or makes a comment in some cases.
For lines which don't need indenting, TAB either indents an
existing comment, moves to end-of-line, or if at end-of-line already,
create a new comment."
  :type 'boolean
  :group 'perl)

(defcustom perl-nochange ";?#\\|\f\\|\\s(\\|\\(\\w\\|\\s_\\)+:[^:]"
  "*Lines starting with this regular expression are not auto-indented."
  :type 'regexp
  :group 'perl)

;; Outline support

(defvar perl-outline-regexp
  (concat (mapconcat 'cadr perl-imenu-generic-expression "\\|")
	  "\\|^=cut\\>"))

(defun perl-outline-level ()
  (cond
   ((looking-at "package\\s-") 0)
   ((looking-at "sub\\s-") 1)
   ((looking-at "=head[0-9]") (- (char-before (match-end 0)) ?0))
   ((looking-at "=cut") 1)
   (t 3)))

(defvar perl-mode-hook nil
  "Normal hook to run when entering Perl mode.")

;;;###autoload
(defun perl-mode ()
  "Major mode for editing Perl code.
Expression and list commands understand all Perl brackets.
Tab indents for Perl code.
Comments are delimited with # ... \\n.
Paragraphs are separated by blank lines only.
Delete converts tabs to spaces as it moves back.
\\{perl-mode-map}
Variables controlling indentation style:
 `perl-tab-always-indent'
    Non-nil means TAB in Perl mode should always indent the current line,
    regardless of where in the line point is when the TAB command is used.
 `perl-tab-to-comment'
    Non-nil means that for lines which don't need indenting, TAB will
    either delete an empty comment, indent an existing comment, move
    to end-of-line, or if at end-of-line already, create a new comment.
 `perl-nochange'
    Lines starting with this regular expression are not auto-indented.
 `perl-indent-level'
    Indentation of Perl statements within surrounding block.
    The surrounding block's indentation is the indentation
    of the line on which the open-brace appears.
 `perl-continued-statement-offset'
    Extra indentation given to a substatement, such as the
    then-clause of an if or body of a while.
 `perl-continued-brace-offset'
    Extra indentation given to a brace that starts a substatement.
    This is in addition to `perl-continued-statement-offset'.
 `perl-brace-offset'
    Extra indentation for line if it starts with an open brace.
 `perl-brace-imaginary-offset'
    An open brace following other text is treated as if it were
    this far to the right of the start of its line.
 `perl-label-offset'
    Extra indentation for line that is a label.
 `perl-indent-continued-arguments'
    Offset of argument lines relative to usual indentation.

Various indentation styles:       K&R  BSD  BLK  GNU  LW
  perl-indent-level                5    8    0    2    4
  perl-continued-statement-offset  5    8    4    2    4
  perl-continued-brace-offset      0    0    0    0   -4
  perl-brace-offset               -5   -8    0    0    0
  perl-brace-imaginary-offset      0    0    4    0    0
  perl-label-offset               -5   -8   -2   -2   -2

Turning on Perl mode runs the normal hook `perl-mode-hook'."
  (interactive)
  (kill-all-local-variables)
  (use-local-map perl-mode-map)
  (setq major-mode 'perl-mode)
  (setq mode-name "Perl")
  (setq local-abbrev-table perl-mode-abbrev-table)
  (set-syntax-table perl-mode-syntax-table)
  (make-local-variable 'paragraph-start)
  (setq paragraph-start (concat "$\\|" page-delimiter))
  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate paragraph-start)
  (make-local-variable 'paragraph-ignore-fill-prefix)
  (setq paragraph-ignore-fill-prefix t)
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'perl-indent-line)
  (make-local-variable 'require-final-newline)
  (setq require-final-newline mode-require-final-newline)
  (make-local-variable 'comment-start)
  (setq comment-start "# ")
  (make-local-variable 'comment-end)
  (setq comment-end "")
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip "\\(^\\|\\s-\\);?#+ *")
  (make-local-variable 'comment-indent-function)
  (setq comment-indent-function 'perl-comment-indent)
  (make-local-variable 'parse-sexp-ignore-comments)
  (setq parse-sexp-ignore-comments t)
  ;; Tell font-lock.el how to handle Perl.
  (setq font-lock-defaults '((perl-font-lock-keywords
			      perl-font-lock-keywords-1
			      perl-font-lock-keywords-2)
			     nil nil ((?\_ . "w")) nil
			     (font-lock-syntactic-keywords
			      . perl-font-lock-syntactic-keywords)
			     (parse-sexp-lookup-properties . t)))
  ;; Tell imenu how to handle Perl.
  (set (make-local-variable 'imenu-generic-expression)
       perl-imenu-generic-expression)
  (setq imenu-case-fold-search nil)
  ;; Setup outline-minor-mode.
  (set (make-local-variable 'outline-regexp) perl-outline-regexp)
  (set (make-local-variable 'outline-level) 'perl-outline-level)
  (run-mode-hooks 'perl-mode-hook))

;; This is used by indent-for-comment
;; to decide how much to indent a comment in Perl code
;; based on its context.
(defun perl-comment-indent ()
  (if (and (bolp) (not (eolp)))
      0					;Existing comment at bol stays there.
    comment-column))

(defalias 'electric-perl-terminator 'perl-electric-terminator)
(defun perl-electric-terminator (arg)
  "Insert character and adjust indentation.
If at end-of-line, and not in a comment or a quote, correct the's indentation."
  (interactive "P")
  (let ((insertpos (point)))
    (and (not arg)			; decide whether to indent
	 (eolp)
	 (save-excursion
	   (beginning-of-line)
	   (and (not			; eliminate comments quickly
		 (and comment-start-skip
		      (re-search-forward comment-start-skip insertpos t)) )
		(or (/= last-command-char ?:)
		    ;; Colon is special only after a label ....
		    (looking-at "\\s-*\\(\\w\\|\\s_\\)+$"))
		(let ((pps (parse-partial-sexp
			    (perl-beginning-of-function) insertpos)))
		  (not (or (nth 3 pps) (nth 4 pps) (nth 5 pps))))))
	 (progn				; must insert, indent, delete
	   (insert-char last-command-char 1)
	   (perl-indent-line)
	   (delete-char -1))))
  (self-insert-command (prefix-numeric-value arg)))

;; not used anymore, but may be useful someday:
;;(defun perl-inside-parens-p ()
;;  (condition-case ()
;;      (save-excursion
;;	(save-restriction
;;	  (narrow-to-region (point)
;;			    (perl-beginning-of-function))
;;	  (goto-char (point-max))
;;	  (= (char-after (or (scan-lists (point) -1 1) (point-min))) ?\()))
;;    (error nil)))

(defun perl-indent-command (&optional arg)
  "Indent current line as Perl code, or optionally, insert a tab character.

With an argument, indent the current line, regardless of other options.

If `perl-tab-always-indent' is nil and point is not in the indentation
area at the beginning of the line, simply insert a tab.

Otherwise, indent the current line.  If point was within the indentation
area it is moved to the end of the indentation area.  If the line was
already indented properly and point was not within the indentation area,
and if `perl-tab-to-comment' is non-nil (the default), then do the first
possible action from the following list:

  1) delete an empty comment
  2) move forward to start of comment, indenting if necessary
  3) move forward to end of line
  4) create an empty comment
  5) move backward to start of comment, indenting if necessary."
  (interactive "P")
  (if arg				; If arg, just indent this line
      (perl-indent-line "\f")
    (if (and (not perl-tab-always-indent)
	     (> (current-column) (current-indentation)))
	(insert-tab)
      (let* ((oldpnt (point))
	     (lsexp (progn (beginning-of-line) (point)))
	     (bof (perl-beginning-of-function))
	     (delta (progn
		      (goto-char oldpnt)
		      (perl-indent-line "\f\\|;?#" bof))))
	(and perl-tab-to-comment
	     (= oldpnt (point))		; done if point moved
	     (if (listp delta)		; if line starts in a quoted string
		 (setq lsexp (or (nth 2 delta) bof))
	       (= delta 0))		; done if indenting occurred
	     (let ((eol (progn (end-of-line) (point)))
		   state)
	       (if (= (char-after bof) ?=)
		   (if (= oldpnt eol)
		       (message "In a format statement"))
		 (setq state (parse-partial-sexp lsexp eol))
		 (if (nth 3 state)
		     (if (= oldpnt eol)	; already at eol in a string
			 (message "In a string which starts with a %c."
				  (nth 3 state)))
		   (if (not (nth 4 state))
		       (if (= oldpnt eol) ; no comment, create one?
			   (indent-for-comment))
		     (beginning-of-line)
		     (if (and comment-start-skip
			      (re-search-forward comment-start-skip eol 'move))
			 (if (eolp)
			     (progn	; delete existing comment
			       (goto-char (match-beginning 0))
			       (skip-chars-backward " \t")
			       (delete-region (point) eol))
			   (if (or (< oldpnt (point)) (= oldpnt eol))
			       (indent-for-comment) ; indent existing comment
			     (end-of-line)))
		       (if (/= oldpnt eol)
			   (end-of-line)
			 (message "Use backslash to quote # characters.")
			 (ding t))))))))))))

(defun perl-indent-line (&optional nochange parse-start)
  "Indent current line as Perl code.
Return the amount the indentation
changed by, or (parse-state) if line starts in a quoted string."
  (let ((case-fold-search nil)
	(pos (- (point-max) (point)))
	(bof (or parse-start (save-excursion (perl-beginning-of-function))))
	beg indent shift-amt)
    (beginning-of-line)
    (setq beg (point))
    (setq shift-amt
	  (cond ((eq (char-after bof) ?=) 0)
		((listp (setq indent (perl-calculate-indent bof))) indent)
		((looking-at (or nochange perl-nochange)) 0)
		(t
		 (skip-chars-forward " \t\f")
		 (setq indent (perl-indent-new-calculate nil indent bof))
		 (- indent (current-column)))))
    (skip-chars-forward " \t\f")
    (if (and (numberp shift-amt) (/= 0 shift-amt))
	(progn (delete-region beg (point))
	       (indent-to indent)))
    ;; If initial point was within line's indentation,
    ;; position after the indentation.  Else stay at same point in text.
    (if (> (- (point-max) pos) (point))
	(goto-char (- (point-max) pos)))
    shift-amt))

(defun perl-continuation-line-p (limit)
  "Move to end of previous line and return non-nil if continued."
  ;; Statement level.  Is it a continuation or a new statement?
  ;; Find previous non-comment character.
  (perl-backward-to-noncomment)
  ;; Back up over label lines, since they don't
  ;; affect whether our line is a continuation.
  (while (or (eq (preceding-char) ?\,)
	     (and (eq (preceding-char) ?:)
		  (memq (char-syntax (char-after (- (point) 2)))
			'(?w ?_))))
    (if (eq (preceding-char) ?\,)
	(perl-backward-to-start-of-continued-exp limit)
      (beginning-of-line))
    (perl-backward-to-noncomment))
  ;; Now we get the answer.
  (not (memq (preceding-char) '(?\; ?\} ?\{))))

(defun perl-hanging-paren-p ()
  "Non-nil if we are right after a hanging parenthesis-like char."
  (and (looking-at "[ \t]*$")
       (save-excursion
	 (skip-syntax-backward " (") (not (bolp)))))

(defun perl-indent-new-calculate (&optional virtual default parse-start)
  (or
   (and virtual (save-excursion (skip-chars-backward " \t") (bolp))
	(current-column))
   (and (looking-at "\\(\\w\\|\\s_\\)+:[^:]")
	(max 1 (+ (or default (perl-calculate-indent parse-start))
		  perl-label-offset)))
   (and (= (char-syntax (following-char)) ?\))
	(save-excursion
	  (forward-char 1)
	  (forward-sexp -1)
	  (perl-indent-new-calculate
           ;; Recalculate the parsing-start, since we may have jumped
           ;; dangerously close (typically in the case of nested functions).
           'virtual nil (save-excursion (perl-beginning-of-function)))))
   (and (and (= (following-char) ?{)
	     (save-excursion (forward-char) (perl-hanging-paren-p)))
	(+ (or default (perl-calculate-indent parse-start))
	   perl-brace-offset))
   (or default (perl-calculate-indent parse-start))))

(defun perl-calculate-indent (&optional parse-start)
  "Return appropriate indentation for current line as Perl code.
In usual case returns an integer: the column to indent to.
Returns (parse-state) if line starts inside a string.
Optional argument PARSE-START should be the position of `beginning-of-defun'."
  (save-excursion
    (let ((indent-point (point))
	  (case-fold-search nil)
	  (colon-line-end 0)
	  state containing-sexp)
      (if parse-start			;used to avoid searching
	  (goto-char parse-start)
	(perl-beginning-of-function))
      ;; We might be now looking at a local function that has nothing to
      ;; do with us because `indent-point' is past it.  In this case
      ;; look further back up for another `perl-beginning-of-function'.
      (while (and (looking-at "{")
		  (save-excursion
		    (beginning-of-line)
		    (looking-at "\\s-+sub\\>"))
		  (> indent-point (save-excursion (forward-sexp 1) (point))))
	(perl-beginning-of-function))
      (while (< (point) indent-point)	;repeat until right sexp
	(setq state (parse-partial-sexp (point) indent-point 0))
	;; state = (depth_in_parens innermost_containing_list
	;;          last_complete_sexp string_terminator_or_nil inside_commentp
	;;          following_quotep minimum_paren-depth_this_scan)
	;; Parsing stops if depth in parentheses becomes equal to third arg.
	(setq containing-sexp (nth 1 state)))
      (cond ((nth 3 state) state)	; In a quoted string?
	    ((null containing-sexp)	; Line is at top level.
	     (skip-chars-forward " \t\f")
	     (if (= (following-char) ?{)
		 0  ; move to beginning of line if it starts a function body
	       ;; indent a little if this is a continuation line
	       (perl-backward-to-noncomment)
	       (if (or (bobp)
		       (memq (preceding-char) '(?\; ?\})))
		   0 perl-continued-statement-offset)))
	    ((/= (char-after containing-sexp) ?{)
	     ;; line is expression, not statement:
	     ;; indent to just after the surrounding open.
	     (goto-char (1+ containing-sexp))
	     (if (perl-hanging-paren-p)
		 ;; We're indenting an arg of a call like:
		 ;;    $a = foobarlongnamefun (
		 ;;             arg1
		 ;;             arg2
		 ;;         );
		 (progn
		   (skip-syntax-backward "(")
		   (condition-case err
		       (while (save-excursion
				(skip-syntax-backward " ") (not (bolp)))
			 (forward-sexp -1))
		     (scan-error nil))
		   (+ (current-column) perl-indent-level))
	       (if perl-indent-continued-arguments
		   (+ perl-indent-continued-arguments (current-indentation))
		 (skip-chars-forward " \t")
		 (current-column))))
	    (t
	     ;; Statement level.  Is it a continuation or a new statement?
	     (if (perl-continuation-line-p containing-sexp)
		 ;; This line is continuation of preceding line's statement;
		 ;; indent  perl-continued-statement-offset  more than the
		 ;; previous line of the statement.
		 (progn
		   (perl-backward-to-start-of-continued-exp containing-sexp)
		   (+ (if (save-excursion
			    (perl-continuation-line-p containing-sexp))
			  ;; If the continued line is itself a continuation
			  ;; line, then align, otherwise add an offset.
			  0 perl-continued-statement-offset)
		      (current-column)
		      (if (save-excursion (goto-char indent-point)
					  (looking-at "[ \t]*{"))
			  perl-continued-brace-offset 0)))
	       ;; This line starts a new statement.
	       ;; Position at last unclosed open.
	       (goto-char containing-sexp)
	       (or
		;; Is line first statement after an open-brace?
		;; If no, find that first statement and indent like it.
		(save-excursion
		  (forward-char 1)
		  ;; Skip over comments and labels following openbrace.
		  (while (progn
			   (skip-chars-forward " \t\f\n")
			   (cond ((looking-at ";?#")
				  (forward-line 1) t)
				 ((looking-at "\\(\\w\\|\\s_\\)+:[^:]")
				  (save-excursion
				    (end-of-line)
				    (setq colon-line-end (point)))
				  (search-forward ":")))))
		  ;; The first following code counts
		  ;; if it is before the line we want to indent.
		  (and (< (point) indent-point)
		       (if (> colon-line-end (point))
			   (- (current-indentation) perl-label-offset)
			 (current-column))))
		;; If no previous statement,
		;; indent it relative to line brace is on.
		;; For open paren in column zero, don't let statement
		;; start there too.  If perl-indent-level is zero,
		;; use perl-brace-offset + perl-continued-statement-offset
		;; For open-braces not the first thing in a line,
		;; add in perl-brace-imaginary-offset.
		(+ (if (and (bolp) (zerop perl-indent-level))
		       (+ perl-brace-offset perl-continued-statement-offset)
		     perl-indent-level)
		   ;; Move back over whitespace before the openbrace.
		   ;; If openbrace is not first nonwhite thing on the line,
		   ;; add the perl-brace-imaginary-offset.
		   (progn (skip-chars-backward " \t")
			  (if (bolp) 0 perl-brace-imaginary-offset))
		   ;; If the openbrace is preceded by a parenthesized exp,
		   ;; move to the beginning of that;
		   ;; possibly a different line
		   (progn
		     (if (eq (preceding-char) ?\))
			 (forward-sexp -1))
		     ;; Get initial indentation of the line we are on.
		     (current-indentation))))))))))

(defun perl-backward-to-noncomment ()
  "Move point backward to after the first non-white-space, skipping comments."
  (interactive)
  (forward-comment (- (point-max))))

(defun perl-backward-to-start-of-continued-exp (lim)
  (if (= (preceding-char) ?\))
      (forward-sexp -1))
  (beginning-of-line)
  (if (<= (point) lim)
      (goto-char (1+ lim)))
  (skip-chars-forward " \t\f"))

;; note: this may be slower than the c-mode version, but I can understand it.
(defalias 'indent-perl-exp 'perl-indent-exp)
(defun perl-indent-exp ()
  "Indent each line of the Perl grouping following point."
  (interactive)
  (let* ((case-fold-search nil)
	 (oldpnt (point-marker))
	 (bof-mark (save-excursion
		     (end-of-line 2)
		     (perl-beginning-of-function)
		     (point-marker)))
	 eol last-mark lsexp-mark delta)
    (if (= (char-after (marker-position bof-mark)) ?=)
	(message "Can't indent a format statement")
      (message "Indenting Perl expression...")
      (save-excursion (end-of-line) (setq eol (point)))
      (save-excursion			; locate matching close paren
	(while (and (not (eobp)) (<= (point) eol))
	  (parse-partial-sexp (point) (point-max) 0))
	(setq last-mark (point-marker)))
      (setq lsexp-mark bof-mark)
      (beginning-of-line)
      (while (< (point) (marker-position last-mark))
	(setq delta (perl-indent-line nil (marker-position bof-mark)))
	(if (numberp delta)		; unquoted start-of-line?
	    (progn
	      (if (eolp)
		  (delete-horizontal-space))
	      (setq lsexp-mark (point-marker))))
	(end-of-line)
	(setq eol (point))
	(if (nth 4 (parse-partial-sexp (marker-position lsexp-mark) eol))
	    (progn			; line ends in a comment
	      (beginning-of-line)
	      (if (or (not (looking-at "\\s-*;?#"))
		      (listp delta)
		      (and (/= 0 delta)
			   (= (- (current-indentation) delta) comment-column)))
		  (if (and comment-start-skip
			   (re-search-forward comment-start-skip eol t))
		      (indent-for-comment))))) ; indent existing comment
	(forward-line 1))
      (goto-char (marker-position oldpnt))
      (message "Indenting Perl expression...done"))))

(defun perl-beginning-of-function (&optional arg)
  "Move backward to next beginning-of-function, or as far as possible.
With argument, repeat that many times; negative args move forward.
Returns new value of point in all cases."
  (interactive "p")
  (or arg (setq arg 1))
  (if (< arg 0) (forward-char 1))
  (and (/= arg 0)
       (re-search-backward
        "^\\s(\\|^\\s-*sub\\b[ \t\n]*\\_<[^{]+{\\|^\\s-*format\\b[^=]*=\\|^\\."
        nil 'move arg)
       (goto-char (1- (match-end 0))))
  (point))

;; note: this routine is adapted directly from emacs lisp.el, end-of-defun;
;; no bugs have been removed :-)
(defun perl-end-of-function (&optional arg)
  "Move forward to next end-of-function.
The end of a function is found by moving forward from the beginning of one.
With argument, repeat that many times; negative args move backward."
  (interactive "p")
  (or arg (setq arg 1))
  (let ((first t))
    (while (and (> arg 0) (< (point) (point-max)))
      (let ((pos (point)))
	(while (progn
		(if (and first
			 (progn
			  (forward-char 1)
			  (perl-beginning-of-function 1)
			  (not (bobp))))
		    nil
		  (or (bobp) (forward-char -1))
		  (perl-beginning-of-function -1))
		(setq first nil)
		(forward-list 1)
		(skip-chars-forward " \t")
		(if (looking-at "[#\n]")
		    (forward-line 1))
		(<= (point) pos))))
      (setq arg (1- arg)))
    (while (< arg 0)
      (let ((pos (point)))
	(perl-beginning-of-function 1)
	(forward-sexp 1)
	(forward-line 1)
	(if (>= (point) pos)
	    (if (progn (perl-beginning-of-function 2) (not (bobp)))
		(progn
		  (forward-list 1)
		  (skip-chars-forward " \t")
		  (if (looking-at "[#\n]")
		      (forward-line 1)))
	      (goto-char (point-min)))))
      (setq arg (1+ arg)))))

(defalias 'mark-perl-function 'perl-mark-function)
(defun perl-mark-function ()
  "Put mark at end of Perl function, point at beginning."
  (interactive)
  (push-mark (point))
  (perl-end-of-function)
  (push-mark (point))
  (perl-beginning-of-function)
  (backward-paragraph))

(provide 'perl-mode)

;; arch-tag: 8c7ff68d-15f3-46a2-ade2-b7c41f176826
;;; perl-mode.el ends here
