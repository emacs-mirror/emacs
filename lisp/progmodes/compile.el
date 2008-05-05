;;; compile.el --- run compiler as inferior of Emacs, parse error messages

;; Copyright (C) 1985, 1986, 1987, 1993, 1994, 1995, 1996, 1997, 1998, 1999,
;;   2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008
;;   Free Software Foundation, Inc.

;; Authors: Roland McGrath <roland@gnu.org>,
;;	    Daniel Pfeiffer <occitan@esperanto.org>
;; Maintainer: FSF
;; Keywords: tools, processes

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This package provides the compile facilities documented in the Emacs user's
;; manual.

;; This mode uses some complex data-structures:

;;   LOC (or location) is a list of (COLUMN LINE FILE-STRUCTURE)

;; COLUMN and LINE are numbers parsed from an error message.  COLUMN and maybe
;; LINE will be nil for a message that doesn't contain them.  Then the
;; location refers to a indented beginning of line or beginning of file.
;; Once any location in some file has been jumped to, the list is extended to
;; (COLUMN LINE FILE-STRUCTURE MARKER TIMESTAMP . VISITED)
;; for all LOCs pertaining to that file.
;; MARKER initially points to LINE and COLUMN in a buffer visiting that file.
;; Being a marker it sticks to some text, when the buffer grows or shrinks
;; before that point.  VISITED is t if we have jumped there, else nil.
;; TIMESTAMP is necessary because of "incremental compilation": `omake -P'
;; polls filesystem for changes and recompiles when a file is modified
;; using the same *compilation* buffer. this necessitates re-parsing markers.

;;   FILE-STRUCTURE is a list of
;;   ((FILENAME . DIRECTORY) FORMATS (LINE LOC ...) ...)

;; FILENAME is a string parsed from an error message.  DIRECTORY is a string
;; obtained by following directory change messages.  DIRECTORY will be nil for
;; an absolute filename.  FORMATS is a list of formats to apply to FILENAME if
;; a file of that name can't be found.
;; The rest of the list is an alist of elements with LINE as key.  The keys
;; are either nil or line numbers.  If present, nil comes first, followed by
;; the numbers in decreasing order.  The LOCs for each line are again an alist
;; ordered the same way.  Note that the whole file structure is referenced in
;; every LOC.

;;   MESSAGE is a list of (LOC TYPE END-LOC)

;; TYPE is 0 for info or 1 for warning if the message matcher identified it as
;; such, 2 otherwise (for a real error).  END-LOC is a LOC pointing to the
;; other end, if the parsed message contained a range.  If the end of the
;; range didn't specify a COLUMN, it defaults to -1, meaning end of line.
;; These are the value of the `message' text-properties in the compilation
;; buffer.

;;; Code:

(eval-when-compile (require 'cl))
(require 'tool-bar)

(defvar font-lock-extra-managed-props)
(defvar font-lock-keywords)
(defvar font-lock-maximum-size)
(defvar font-lock-support-mode)


(defgroup compilation nil
  "Run compiler as inferior of Emacs, parse error messages."
  :group 'tools
  :group 'processes)


;;;###autoload
(defcustom compilation-mode-hook nil
  "List of hook functions run by `compilation-mode' (see `run-mode-hooks')."
  :type 'hook
  :group 'compilation)

;;;###autoload
(defcustom compilation-window-height nil
  "Number of lines in a compilation window.  If nil, use Emacs default."
  :type '(choice (const :tag "Default" nil)
		 integer)
  :group 'compilation)

(defvar compilation-first-column 1
  "*This is how compilers number the first column, usually 1 or 0.")

(defvar compilation-parse-errors-filename-function nil
  "Function to call to post-process filenames while parsing error messages.
It takes one arg FILENAME which is the name of a file as found
in the compilation output, and should return a transformed file name.")

;;;###autoload
(defvar compilation-process-setup-function nil
  "*Function to call to customize the compilation process.
This function is called immediately before the compilation process is
started.  It can be used to set any variables or functions that are used
while processing the output of the compilation process.  The function
is called with variables `compilation-buffer' and `compilation-window'
bound to the compilation buffer and window, respectively.")

;;;###autoload
(defvar compilation-buffer-name-function nil
  "Function to compute the name of a compilation buffer.
The function receives one argument, the name of the major mode of the
compilation buffer.  It should return a string.
If nil, compute the name with `(concat \"*\" (downcase major-mode) \"*\")'.")

;;;###autoload
(defvar compilation-finish-function nil
  "Function to call when a compilation process finishes.
It is called with two arguments: the compilation buffer, and a string
describing how the process finished.")

(make-obsolete-variable 'compilation-finish-function
  "use `compilation-finish-functions', but it works a little differently."
  "22.1")

;;;###autoload
(defvar compilation-finish-functions nil
  "Functions to call when a compilation process finishes.
Each function is called with two arguments: the compilation buffer,
and a string describing how the process finished.")

(defvar compilation-in-progress nil
  "List of compilation processes now running.")
(or (assq 'compilation-in-progress minor-mode-alist)
    (setq minor-mode-alist (cons '(compilation-in-progress " Compiling")
				 minor-mode-alist)))

(defvar compilation-error "error"
  "Stem of message to print when no matches are found.")

(defvar compilation-arguments nil
  "Arguments that were given to `compilation-start'.")

(defvar compilation-num-errors-found)

(defconst compilation-error-regexp-alist-alist
  '((absoft
     "^\\(?:[Ee]rror on \\|[Ww]arning on\\( \\)\\)?[Ll]ine[ \t]+\\([0-9]+\\)[ \t]+\
of[ \t]+\"?\\([a-zA-Z]?:?[^\":\n]+\\)\"?:" 3 2 nil (1))

    (ada
     "\\(warning: .*\\)? at \\([^ \n]+\\):\\([0-9]+\\)$" 2 3 nil (1))

    (aix
     " in line \\([0-9]+\\) of file \\([^ \n]+[^. \n]\\)\\.? " 2 1)

    (ant
     "^[ \t]*\\[[^] \n]+\\][ \t]*\\([^: \n]+\\):\\([0-9]+\\):\\(?:\\([0-9]+\\):[0-9]+:[0-9]+:\\)?\
\\( warning\\)?" 1 2 3 (4))

    (maven
     ;; Maven is a popular build tool for Java.  Maven is Free Software.
     "\\(.*?\\):\\[\\([0-9]+\\),\\([0-9]+\\)\\]" 1 2 3)

    (bash
     "^\\([^: \n\t]+\\): line \\([0-9]+\\):" 1 2)

    (borland
     "^\\(?:Error\\|Warnin\\(g\\)\\) \\(?:[FEW][0-9]+ \\)?\
\\([a-zA-Z]?:?[^:( \t\n]+\\)\
 \\([0-9]+\\)\\(?:[) \t]\\|:[^0-9\n]\\)" 2 3 nil (1))

    (caml
     "^ *File \\(\"?\\)\\([^,\" \n\t<>]+\\)\\1, lines? \\([0-9]+\\)-?\\([0-9]+\\)?\\(?:$\\|,\
\\(?: characters? \\([0-9]+\\)-?\\([0-9]+\\)?:\\)?\\([ \n]Warning:\\)?\\)"
     2 (3 . 4) (5 . 6) (7))

    (comma
     "^\"\\([^,\" \n\t]+\\)\", line \\([0-9]+\\)\
\\(?:[(. pos]+\\([0-9]+\\))?\\)?[:.,; (-]\\( warning:\\|[-0-9 ]*(W)\\)?" 1 2 3 (4))

    (edg-1
     "^\\([^ \n]+\\)(\\([0-9]+\\)): \\(?:error\\|warnin\\(g\\)\\|remar\\(k\\)\\)"
     1 2 nil (3 . 4))
    (edg-2
     "at line \\([0-9]+\\) of \"\\([^ \n]+\\)\"$"
     2 1 nil 0)

    (epc
     "^Error [0-9]+ at (\\([0-9]+\\):\\([^)\n]+\\))" 2 1)

    (ftnchek
     "\\(^Warning .*\\)? line[ \n]\\([0-9]+\\)[ \n]\\(?:col \\([0-9]+\\)[ \n]\\)?file \\([^ :;\n]+\\)"
     4 2 3 (1))

    (iar
     "^\"\\(.*\\)\",\\([0-9]+\\)\\s-+\\(?:Error\\|Warnin\\(g\\)\\)\\[[0-9]+\\]:"
     1 2 nil (3))

    (ibm
     "^\\([^( \n\t]+\\)(\\([0-9]+\\):\\([0-9]+\\)) :\
 \\(?:warnin\\(g\\)\\|informationa\\(l\\)\\)?" 1 2 3 (4 . 5))

    ;; fixme: should be `mips'
    (irix
     "^[-[:alnum:]_/ ]+: \\(?:\\(?:[sS]evere\\|[eE]rror\\|[wW]arnin\\(g\\)\\|[iI]nf\\(o\\)\\)[0-9 ]*: \\)?\
\\([^,\" \n\t]+\\)\\(?:, line\\|:\\) \\([0-9]+\\):" 3 4 nil (1 . 2))

    (java
     "^\\(?:[ \t]+at \\|==[0-9]+== +\\(?:at\\|b\\(y\\)\\)\\).+(\\([^()\n]+\\):\\([0-9]+\\))$" 2 3 nil (1))

    (jikes-file
     "^\\(?:Found\\|Issued\\) .* compiling \"\\(.+\\)\":$" 1 nil nil 0)
    (jikes-line
     "^ *\\([0-9]+\\)\\.[ \t]+.*\n +\\(<-*>\n\\*\\*\\* \\(?:Error\\|Warnin\\(g\\)\\)\\)"
     nil 1 nil 2 0
     (2 (compilation-face '(3))))

    (gnu
     ;; I have no idea what this first line is supposed to match, but it
     ;; makes things ambiguous with output such as "foo:344:50:blabla" since
     ;; the "foo" part can match this first line (in which case the file
     ;; name as "344").  To avoid this, the second line disallows filenames
     ;; exclusively composed of digits.  --Stef
     ;; Similarly, we get lots of false positives with messages including
     ;; times of the form "HH:MM:SS" where MM is taken as a line number, so
     ;; the last line tries to rule out message where the info after the
     ;; line number starts with "SS".  --Stef

     ;; The core of the regexp is the one with *?.  It says that a file name
     ;; can be composed of any non-newline char, but it also rules out some
     ;; valid but unlikely cases, such as a trailing space or a space
     ;; followed by a -.
     "^\\(?:[[:alpha:]][-[:alnum:].]+: ?\\)?\
\\([0-9]*[^0-9\n]\\(?:[^\n ]\\| [^-/\n]\\)*?\\): ?\
\\([0-9]+\\)\\(?:\\([.:]\\)\\([0-9]+\\)\\)?\
\\(?:-\\([0-9]+\\)?\\(?:\\3\\([0-9]+\\)\\)?\\)?:\
\\(?: *\\(\\(?:Future\\|Runtime\\)?[Ww]arning\\|W:\\)\\|\
 *\\([Ii]nfo\\(?:\\>\\|rmationa?l?\\)\\|I:\\|instantiated from\\|[Nn]ote\\)\\|\
\[0-9]?\\(?:[^0-9\n]\\|$\\)\\|[0-9][0-9][0-9]\\)"
     1 (2 . 5) (4 . 6) (7 . 8))

    ;; The `gnu' style above can incorrectly match gcc's "In file
    ;; included from" message, so we process that first. -- cyd
    (gcc-include
     "^\\(?:In file included\\|                \\) from \
\\(.+\\):\\([0-9]+\\)\\(?:\\(:\\)\\|\\(,\\)\\)?" 1 2 nil (3 . 4))

    (lcc
     "^\\(?:E\\|\\(W\\)\\), \\([^(\n]+\\)(\\([0-9]+\\),[ \t]*\\([0-9]+\\)"
     2 3 4 (1))

    (makepp
     "^makepp\\(?:\\(?:: warning\\(:\\).*?\\|\\(: Scanning\\|: [LR]e?l?oading makefile\\|: Imported\\|log:.*?\\) \\|: .*?\\)\
`\\(\\(\\S +?\\)\\(?::\\([0-9]+\\)\\)?\\)['(]\\)"
     4 5 nil (1 . 2) 3
     ("`\\(\\(\\S +?\\)\\(?::\\([0-9]+\\)\\)?\\)['(]" nil nil
      (2 compilation-info-face)
      (3 compilation-line-face nil t)
      (1 (compilation-error-properties 2 3 nil nil nil 0 nil)
	 append)))

    ;; Should be lint-1, lint-2 (SysV lint)
    (mips-1
     " (\\([0-9]+\\)) in \\([^ \n]+\\)" 2 1)
    (mips-2
     " in \\([^()\n ]+\\)(\\([0-9]+\\))$" 1 2)

    (msft
     ;; AFAWK, The message may be a "warning", "error", or "fatal error".
     "^\\([0-9]+>\\)?\\(\\(?:[a-zA-Z]:\\)?[^:(\t\n]+\\)(\\([0-9]+\\)) \
: \\(?:warnin\\(g\\)\\|[a-z ]+\\) C[0-9]+:" 2 3 nil (4))

    (oracle
     "^\\(?:Semantic error\\|Error\\|PCC-[0-9]+:\\).* line \\([0-9]+\\)\
\\(?:\\(?:,\\| at\\)? column \\([0-9]+\\)\\)?\
\\(?:,\\| in\\| of\\)? file \\(.*?\\):?$"
     3 1 2)

    ;; "during global destruction": This comes out under "use
    ;; warnings" in recent perl when breaking circular references
    ;; during program or thread exit.
    (perl
     " at \\([^ \n]+\\) line \\([0-9]+\\)\\(?:[,.]\\|$\\| \
during global destruction\\.$\\)" 1 2)

    (php
     "\\(?:Parse\\|Fatal\\) error: \\(.*\\) in \\(.*\\) on line \\([0-9]+\\)"
     2 3 nil nil)

    (rxp
     "^\\(?:Error\\|Warnin\\(g\\)\\):.*\n.* line \\([0-9]+\\) char\
 \\([0-9]+\\) of file://\\(.+\\)"
     4 2 3 (1))

    (sparc-pascal-file
     "^\\w\\w\\w \\w\\w\\w +[0-3]?[0-9] +[0-2][0-9]:[0-5][0-9]:[0-5][0-9]\
 [12][09][0-9][0-9] +\\(.*\\):$"
     1 nil nil 0)
    (sparc-pascal-line
     "^\\(\\(?:E\\|\\(w\\)\\) +[0-9]+\\) line \\([0-9]+\\) -  "
     nil 3 nil (2) nil (1 (compilation-face '(2))))
    (sparc-pascal-example
     "^ +\\([0-9]+\\) +.*\n\\(\\(?:e\\|\\(w\\)\\) [0-9]+\\)-+"
     nil 1 nil (3) nil (2 (compilation-face '(3))))

    (sun
     ": \\(?:ERROR\\|WARNIN\\(G\\)\\|REMAR\\(K\\)\\) \\(?:[[:alnum:] ]+, \\)?\
File = \\(.+\\), Line = \\([0-9]+\\)\\(?:, Column = \\([0-9]+\\)\\)?"
     3 4 5 (1 . 2))

    (sun-ada
     "^\\([^, \n\t]+\\), line \\([0-9]+\\), char \\([0-9]+\\)[:., \(-]" 1 2 3)

    (watcom
     "\\(\\(?:[a-zA-Z]:\\)?[^:(\t\n]+\\)(\\([0-9]+\\)): \\(?:Error! E[0-9]+\\|Warning! W[0-9]+\\):" 1 2)

    (4bsd
     "\\(?:^\\|::  \\|\\S ( \\)\\(/[^ \n\t()]+\\)(\\([0-9]+\\))\
\\(?:: \\(warning:\\)?\\|$\\| ),\\)" 1 2 nil (3))

    (gcov-file
     "^ *-: *\\(0\\):Source:\\(.+\\)$"
     2 1 nil 0 nil
     (1 compilation-line-face prepend) (2 compilation-info-face prepend))
    (gcov-header
     "^ *-: *\\(0\\):\\(?:Object\\|Graph\\|Data\\|Runs\\|Programs\\):.+$"
     nil 1 nil 0 nil
     (1 compilation-line-face prepend))
    ;; Underlines over all lines of gcov output are too uncomfortable to read.
    ;; However, hyperlinks embedded in the lines are useful.
    ;; So I put default face on the lines; and then put
    ;; compilation-*-face by manually to eliminate the underlines.
    ;; The hyperlinks are still effective.
    (gcov-nomark
     "^ *-: *\\([1-9]\\|[0-9]\\{2,\\}\\):.*$"
     nil 1 nil 0 nil
     (0 'default t)
     (1 compilation-line-face prepend))
    (gcov-called-line
     "^ *\\([0-9]+\\): *\\([0-9]+\\):.*$"
     nil 2 nil 0 nil
     (0 'default t)
     (1 compilation-info-face prepend) (2 compilation-line-face prepend))
    (gcov-never-called
     "^ *\\(#####\\): *\\([0-9]+\\):.*$"
     nil 2 nil 2 nil
     (0 'default t)
     (1 compilation-error-face prepend) (2 compilation-line-face prepend))

    (perl--Pod::Checker
     ;; podchecker error messages, per Pod::Checker.
     ;; The style is from the Pod::Checker::poderror() function, eg.
     ;; *** ERROR: Spurious text after =cut at line 193 in file foo.pm
     ;;
     ;; Plus end_pod() can give "at line EOF" instead of a
     ;; number, so for that match "on line N" which is the
     ;; originating spot, eg.
     ;; *** ERROR: =over on line 37 without closing =back at line EOF in file bar.pm
     ;;
     ;; Plus command() can give both "on line N" and "at line N";
     ;; the latter is desired and is matched because the .* is
     ;; greedy.
     ;; *** ERROR: =over on line 1 without closing =back (at head1) at line 3 in file x.pod
     ;;
     "^\\*\\*\\* \\(?:ERROR\\|\\(WARNING\\)\\).* \\(?:at\\|on\\) line \
\\([0-9]+\\) \\(?:.* \\)?in file \\([^ \t\n]+\\)"
     3 2 nil (1))
    (perl--Test
     ;; perl Test module error messages.
     ;; Style per the ok() function "$context", eg.
     ;; # Failed test 1 in foo.t at line 6
     ;;
     "^# Failed test [0-9]+ in \\([^ \t\r\n]+\\) at line \\([0-9]+\\)"
     1 2)
    (perl--Test2
     ;; Or when comparing got/want values,
     ;; # Test 2 got: "xx" (t-compilation-perl-2.t at line 10)
     ;;
     ;; And under Test::Harness they're preceded by progress stuff with
     ;; \r and "NOK",
     ;; ... NOK 1# Test 1 got: "1234" (t/foo.t at line 46)
     ;;
     "^\\(.*NOK.*\\)?# Test [0-9]+ got:.* (\\([^ \t\r\n]+\\) at line \
\\([0-9]+\\))"
     2 3)
    (perl--Test::Harness
     ;; perl Test::Harness output, eg.
     ;; NOK 1# Test 1 got: "1234" (t/foo.t at line 46)
     ;;
     ;; Test::Harness is slightly designed for tty output, since
     ;; it prints CRs to overwrite progress messages, but if you
     ;; run it in with M-x compile this pattern can at least step
     ;; through the failures.
     ;;
     "^.*NOK.* \\([^ \t\r\n]+\\) at line \\([0-9]+\\)"
     1 2)
    (weblint
     ;; The style comes from HTML::Lint::Error::as_string(), eg.
     ;; index.html (13:1) Unknown element <fdjsk>
     ;;
     ;; The pattern only matches filenames without spaces, since that
     ;; should be usual and should help reduce the chance of a false
     ;; match of a message from some unrelated program.
     ;;
     ;; This message style is quite close to the "ibm" entry which is
     ;; for IBM C, though that ibm bit doesn't put a space after the
     ;; filename.
     ;;
     "^\\([^ \t\r\n(]+\\) (\\([0-9]+\\):\\([0-9]+\\)) "
     1 2 3)
    )
  "Alist of values for `compilation-error-regexp-alist'.")

(defcustom compilation-error-regexp-alist
  (mapcar 'car compilation-error-regexp-alist-alist)
  "Alist that specifies how to match errors in compiler output.
On GNU and Unix, any string is a valid filename, so these
matchers must make some common sense assumptions, which catch
normal cases.  A shorter list will be lighter on resource usage.

Instead of an alist element, you can use a symbol, which is
looked up in `compilation-error-regexp-alist-alist'.  You can see
the predefined symbols and their effects in the file
`etc/compilation.txt' (linked below if you are customizing this).

Each elt has the form (REGEXP FILE [LINE COLUMN TYPE HYPERLINK
HIGHLIGHT...]).  If REGEXP matches, the FILE'th subexpression
gives the file name, and the LINE'th subexpression gives the line
number.  The COLUMN'th subexpression gives the column number on
that line.

If FILE, LINE or COLUMN are nil or that index didn't match, that
information is not present on the matched line.  In that case the
file name is assumed to be the same as the previous one in the
buffer, line number defaults to 1 and column defaults to
beginning of line's indentation.

FILE can also have the form (FILE FORMAT...), where the FORMATs
\(e.g. \"%s.c\") will be applied in turn to the recognized file
name, until a file of that name is found.  Or FILE can also be a
function that returns (FILENAME) or (RELATIVE-FILENAME . DIRNAME).
In the former case, FILENAME may be relative or absolute.

LINE can also be of the form (LINE . END-LINE) meaning a range
of lines.  COLUMN can also be of the form (COLUMN . END-COLUMN)
meaning a range of columns starting on LINE and ending on
END-LINE, if that matched.

TYPE is 2 or nil for a real error or 1 for warning or 0 for info.
TYPE can also be of the form (WARNING . INFO).  In that case this
will be equivalent to 1 if the WARNING'th subexpression matched
or else equivalent to 0 if the INFO'th subexpression matched.
See `compilation-error-face', `compilation-warning-face',
`compilation-info-face' and `compilation-skip-threshold'.

What matched the HYPERLINK'th subexpression has `mouse-face' and
`compilation-message-face' applied.  If this is nil, the text
matched by the whole REGEXP becomes the hyperlink.

Additional HIGHLIGHTs as described under `font-lock-keywords' can
be added."
  :type `(set :menu-tag "Pick"
	      ,@(mapcar (lambda (elt)
			  (list 'const (car elt)))
			compilation-error-regexp-alist-alist))
  :link `(file-link :tag "example file"
		    ,(expand-file-name "compilation.txt" data-directory))
  :group 'compilation)

;;;###autoload(put 'compilation-directory 'safe-local-variable 'stringp)
(defvar compilation-directory nil
  "Directory to restore to when doing `recompile'.")

(defvar compilation-directory-matcher
  '("\\(?:Entering\\|Leavin\\(g\\)\\) directory `\\(.+\\)'$" (2 . 1))
  "A list for tracking when directories are entered or left.
If nil, do not track directories, e.g. if all file names are absolute.  The
first element is the REGEXP matching these messages.  It can match any number
of variants, e.g. different languages.  The remaining elements are all of the
form (DIR .  LEAVE).  If for any one of these the DIR'th subexpression
matches, that is a directory name.  If LEAVE is nil or the corresponding
LEAVE'th subexpression doesn't match, this message is about going into another
directory.  If it does match anything, this message is about going back to the
directory we were in before the last entering message.  If you change this,
you may also want to change `compilation-page-delimiter'.")

(defvar compilation-page-delimiter
  "^\\(?:\f\\|.*\\(?:Entering\\|Leaving\\) directory `.+'\n\\)+"
  "Value of `page-delimiter' in Compilation mode.")

(defvar compilation-mode-font-lock-keywords
   '(;; configure output lines.
     ("^[Cc]hecking \\(?:[Ff]or \\|[Ii]f \\|[Ww]hether \\(?:to \\)?\\)?\\(.+\\)\\.\\.\\. *\\(?:(cached) *\\)?\\(\\(yes\\(?: .+\\)?\\)\\|no\\|\\(.*\\)\\)$"
      (1 font-lock-variable-name-face)
      (2 (compilation-face '(4 . 3))))
     ;; Command output lines.  Recognize `make[n]:' lines too.
     ("^\\([[:alnum:]_/.+-]+\\)\\(\\[\\([0-9]+\\)\\]\\)?[ \t]*:"
      (1 font-lock-function-name-face) (3 compilation-line-face nil t))
     (" --?o\\(?:utfile\\|utput\\)?[= ]?\\(\\S +\\)" . 1)
     ("^Compilation \\(finished\\).*"
      (0 '(face nil message nil help-echo nil mouse-face nil) t)
      (1 compilation-info-face))
     ("^Compilation \\(exited abnormally\\|interrupt\\|killed\\|terminated\\|segmentation fault\\)\\(?:.*with code \\([0-9]+\\)\\)?.*"
      (0 '(face nil message nil help-echo nil mouse-face nil) t)
      (1 compilation-error-face)
      (2 compilation-error-face nil t)))
   "Additional things to highlight in Compilation mode.
This gets tacked on the end of the generated expressions.")

(defvar compilation-highlight-regexp t
  "Regexp matching part of visited source lines to highlight temporarily.
Highlight entire line if t; don't highlight source lines if nil.")

(defvar compilation-highlight-overlay nil
  "Overlay used to temporarily highlight compilation matches.")

(defcustom compilation-error-screen-columns t
  "If non-nil, column numbers in error messages are screen columns.
Otherwise they are interpreted as character positions, with
each character occupying one column.
The default is to use screen columns, which requires that the compilation
program and Emacs agree about the display width of the characters,
especially the TAB character."
  :type 'boolean
  :group 'compilation
  :version "20.4")

(defcustom compilation-read-command t
  "Non-nil means \\[compile] reads the compilation command to use.
Otherwise, \\[compile] just uses the value of `compile-command'."
  :type 'boolean
  :group 'compilation)

;;;###autoload
(defcustom compilation-ask-about-save t
  "Non-nil means \\[compile] asks which buffers to save before compiling.
Otherwise, it saves all modified buffers without asking."
  :type 'boolean
  :group 'compilation)

;;;###autoload
(defcustom compilation-search-path '(nil)
  "List of directories to search for source files named in error messages.
Elements should be directory names, not file names of directories.
The value nil as an element means to try the default directory."
  :type '(repeat (choice (const :tag "Default" nil)
			 (string :tag "Directory")))
  :group 'compilation)

;;;###autoload
(defcustom compile-command "make -k "
  "Last shell command used to do a compilation; default for next compilation.

Sometimes it is useful for files to supply local values for this variable.
You might also use mode hooks to specify it in certain modes, like this:

    (add-hook 'c-mode-hook
       (lambda ()
	 (unless (or (file-exists-p \"makefile\")
		     (file-exists-p \"Makefile\"))
	   (set (make-local-variable 'compile-command)
		(concat \"make -k \"
			(file-name-sans-extension buffer-file-name))))))"
  :type 'string
  :group 'compilation)
;;;###autoload(put 'compile-command 'safe-local-variable 'stringp)

;;;###autoload
(defcustom compilation-disable-input nil
  "If non-nil, send end-of-file as compilation process input.
This only affects platforms that support asynchronous processes (see
`start-process'); synchronous compilation processes never accept input."
  :type 'boolean
  :group 'compilation
  :version "22.1")

;; A weak per-compilation-buffer hash indexed by (FILENAME . DIRECTORY).  Each
;; value is a FILE-STRUCTURE as described above, with the car eq to the hash
;; key.  This holds the tree seen from root, for storing new nodes.
(defvar compilation-locs ())

(defvar compilation-debug nil
  "*Set this to t before creating a *compilation* buffer.
Then every error line will have a debug text property with the matcher that
fit this line and the match data.  Use `describe-text-properties'.")

(defvar compilation-exit-message-function nil "\
If non-nil, called when a compilation process dies to return a status message.
This should be a function of three arguments: process status, exit status,
and exit message; it returns a cons (MESSAGE . MODELINE) of the strings to
write into the compilation buffer, and to put in its mode line.")

(defvar compilation-environment nil
  "*List of environment variables for compilation to inherit.
Each element should be a string of the form ENVVARNAME=VALUE.
This list is temporarily prepended to `process-environment' prior to
starting the compilation process.")

;; History of compile commands.
(defvar compile-history nil)

(defface compilation-error
  '((t :inherit font-lock-warning-face))
  "Face used to highlight compiler errors."
  :group 'compilation
  :version "22.1")

(defface compilation-warning
  '((((class color) (min-colors 16)) (:foreground "Orange" :weight bold))
    (((class color)) (:foreground "cyan" :weight bold))
    (t (:weight bold)))
  "Face used to highlight compiler warnings."
  :group 'compilation
  :version "22.1")

(defface compilation-info
  '((((class color) (min-colors 16) (background light))
     (:foreground "Green3" :weight bold))
    (((class color) (min-colors 88) (background dark))
     (:foreground "Green1" :weight bold))
    (((class color) (min-colors 16) (background dark))
     (:foreground "Green" :weight bold))
    (((class color)) (:foreground "green" :weight bold))
    (t (:weight bold)))
  "Face used to highlight compiler information."
  :group 'compilation
  :version "22.1")

(defface compilation-line-number
  '((t :inherit font-lock-variable-name-face))
  "Face for displaying line numbers in compiler messages."
  :group 'compilation
  :version "22.1")

(defface compilation-column-number
  '((t :inherit font-lock-type-face))
  "Face for displaying column numbers in compiler messages."
  :group 'compilation
  :version "22.1")

(defcustom compilation-message-face 'underline
  "Face name to use for whole messages.
Faces `compilation-error-face', `compilation-warning-face',
`compilation-info-face', `compilation-line-face' and
`compilation-column-face' get prepended to this, when applicable."
  :type 'face
  :group 'compilation
  :version "22.1")

(defvar compilation-error-face 'compilation-error
  "Face name to use for file name in error messages.")

(defvar compilation-warning-face 'compilation-warning
  "Face name to use for file name in warning messages.")

(defvar compilation-info-face 'compilation-info
  "Face name to use for file name in informational messages.")

(defvar compilation-line-face 'compilation-line-number
  "Face name to use for line numbers in compiler messages.")

(defvar compilation-column-face 'compilation-column-number
  "Face name to use for column numbers in compiler messages.")

;; same faces as dired uses
(defvar compilation-enter-directory-face 'font-lock-function-name-face
  "Face name to use for entering directory messages.")

(defvar compilation-leave-directory-face 'font-lock-type-face
  "Face name to use for leaving directory messages.")



;; Used for compatibility with the old compile.el.
(defvaralias 'compilation-last-buffer 'next-error-last-buffer)
(defvar compilation-parsing-end (make-marker))
(defvar compilation-parse-errors-function nil)
(defvar compilation-error-list nil)
(defvar compilation-old-error-list nil)

(defcustom compilation-auto-jump-to-first-error nil
  "If non-nil, automatically jump to the first error after `compile'."
  :type 'boolean
  :group 'compilation
  :version "23.1")

(defvar compilation-auto-jump-to-next nil
  "If non-nil, automatically jump to the next error encountered.")
(make-variable-buffer-local 'compilation-auto-jump-to-next)


(defvar compilation-skip-to-next-location t
  "*If non-nil, skip multiple error messages for the same source location.")

(defcustom compilation-skip-threshold 1
  "Compilation motion commands skip less important messages.
The value can be either 2 -- skip anything less than error, 1 --
skip anything less than warning or 0 -- don't skip any messages.
Note that all messages not positively identified as warning or
info, are considered errors."
  :type '(choice (const :tag "Warnings and info" 2)
		 (const :tag "Info" 1)
		 (const :tag "None" 0))
  :group 'compilation
  :version "22.1")

(defcustom compilation-skip-visited nil
  "Compilation motion commands skip visited messages if this is t.
Visited messages are ones for which the file, line and column have been jumped
to from the current content in the current compilation buffer, even if it was
from a different message."
  :type 'boolean
  :group 'compilation
  :version "22.1")

(defun compilation-face (type)
  (or (and (car type) (match-end (car type)) compilation-warning-face)
      (and (cdr type) (match-end (cdr type)) compilation-info-face)
      compilation-error-face))

;; Internal function for calculating the text properties of a directory
;; change message.  The directory property is important, because it is
;; the stack of nested enter-messages.  Relative filenames on the following
;; lines are relative to the top of the stack.
(defun compilation-directory-properties (idx leave)
  (if leave (setq leave (match-end leave)))
  ;; find previous stack, and push onto it, or if `leave' pop it
  (let ((dir (previous-single-property-change (point) 'directory)))
    (setq dir (if dir (or (get-text-property (1- dir) 'directory)
			  (get-text-property dir 'directory))))
    `(face ,(if leave
		compilation-leave-directory-face
	      compilation-enter-directory-face)
      directory ,(if leave
		     (or (cdr dir)
			 '(nil))	; nil only isn't a property-change
		   (cons (match-string-no-properties idx) dir))
      mouse-face highlight
      keymap compilation-button-map
      help-echo "mouse-2: visit destination directory")))

;; Data type `reverse-ordered-alist' retriever.  This function retrieves the
;; KEY element from the ALIST, creating it in the right position if not already
;; present. ALIST structure is
;; '(ANCHOR (KEY1 ...) (KEY2 ...)... (KEYn ALIST ...))
;; ANCHOR is ignored, but necessary so that elements can be inserted.  KEY1
;; may be nil.  The other KEYs are ordered backwards so that growing line
;; numbers can be inserted in front and searching can abort after half the
;; list on average.
(eval-when-compile		    ;Don't keep it at runtime if not needed.
(defmacro compilation-assq (key alist)
  `(let* ((l1 ,alist)
	  (l2 (cdr l1)))
     (car (if (if (null ,key)
		  (if l2 (null (caar l2)))
		(while (if l2 (if (caar l2) (< ,key (caar l2)) t))
		  (setq l1 l2
			l2 (cdr l1)))
		(if l2 (eq ,key (caar l2))))
	      l2
	    (setcdr l1 (cons (list ,key) l2)))))))

(defun compilation-auto-jump (buffer pos)
  (with-current-buffer buffer
    (goto-char pos)
    (let ((win (get-buffer-window buffer 0)))
      (if win (set-window-point win pos)))
    (if compilation-auto-jump-to-first-error
	(compile-goto-error))))

;; This function is the central driver, called when font-locking to gather
;; all information needed to later jump to corresponding source code.
;; Return a property list with all meta information on this error location.

(defun compilation-error-properties (file line end-line col end-col type fmt)
  (unless (< (next-single-property-change (match-beginning 0)
                                          'directory nil (point))
	     (point))
    (if file
	(if (functionp file)
	    (setq file (funcall file))
	  (let (dir)
	    (setq file (match-string-no-properties file))
	    (unless (file-name-absolute-p file)
	      (setq dir (previous-single-property-change (point) 'directory)
		    dir (if dir (or (get-text-property (1- dir) 'directory)
				    (get-text-property dir 'directory)))))
	    (setq file (cons file (car dir)))))
      ;; This message didn't mention one, get it from previous
      (let ((prev-pos
	     ;; Find the previous message.
	     (previous-single-property-change (point) 'message)))
	(if prev-pos
	    ;; Get the file structure that belongs to it.
	    (let* ((prev
		    (or (get-text-property (1- prev-pos) 'message)
			(get-text-property prev-pos 'message)))
		   (prev-struct
		    (car (nth 2 (car prev)))))
	      ;; Construct FILE . DIR from that.
	      (if prev-struct
		  (setq file (cons (car prev-struct)
				   (cadr prev-struct))))))
	(unless file
	  (setq file '("*unknown*")))))
    ;; All of these fields are optional, get them only if we have an index, and
    ;; it matched some part of the message.
    (and line
	 (setq line (match-string-no-properties line))
	 (setq line (string-to-number line)))
    (and end-line
	 (setq end-line (match-string-no-properties end-line))
	 (setq end-line (string-to-number end-line)))
    (if col
        (if (functionp col)
            (setq col (funcall col))
          (and
           (setq col (match-string-no-properties col))
           (setq col (- (string-to-number col) compilation-first-column)))))
    (if (and end-col (functionp end-col))
        (setq end-col (funcall end-col))
      (if (and end-col (setq end-col (match-string-no-properties end-col)))
          (setq end-col (- (string-to-number end-col) compilation-first-column -1))
        (if end-line (setq end-col -1))))
    (if (consp type)			; not a static type, check what it is.
	(setq type (or (and (car type) (match-end (car type)) 1)
		       (and (cdr type) (match-end (cdr type)) 0)
		       2)))

    (when (and compilation-auto-jump-to-next
               (>= type compilation-skip-threshold))
      (kill-local-variable 'compilation-auto-jump-to-next)
      (run-with-timer 0 nil 'compilation-auto-jump
                      (current-buffer) (match-beginning 0)))

    (compilation-internal-error-properties file line end-line col end-col type fmt)))

(defun compilation-move-to-column (col screen)
  "Go to column COL on the current line.
If SCREEN is non-nil, columns are screen columns, otherwise, they are
just char-counts."
  (if screen
      (move-to-column col)
    (goto-char (min (+ (line-beginning-position) col) (line-end-position)))))

(defun compilation-internal-error-properties (file line end-line col end-col type fmts)
  "Get the meta-info that will be added as text-properties.
LINE, END-LINE, COL, END-COL are integers or nil.
TYPE can be 0, 1, or 2, meaning error, warning, or just info.
FILE should be (FILENAME) or (RELATIVE-FILENAME . DIRNAME) or nil.
FMTS is a list of format specs for transforming the file name.
 (See `compilation-error-regexp-alist'.)"
  (unless file (setq file '("*unknown*")))
  (let* ((file-struct (compilation-get-file-structure file fmts))
	 ;; Get first already existing marker (if any has one, all have one).
	 ;; Do this first, as the compilation-assq`s may create new nodes.
	 (marker-line (car (cddr file-struct)))	; a line structure
	 (marker (nth 3 (cadr marker-line)))	; its marker
	 (compilation-error-screen-columns compilation-error-screen-columns)
	 end-marker loc end-loc)
    (if (not (and marker (marker-buffer marker)))
	(setq marker nil)		; no valid marker for this file
      (setq loc (or line 1))		; normalize no linenumber to line 1
      (catch 'marker			; find nearest loc, at least one exists
	(dolist (x (nthcdr 3 file-struct))	; loop over remaining lines
	  (if (> (car x) loc)		; still bigger
	      (setq marker-line x)
	    (if (> (- (or (car marker-line) 1) loc)
		   (- loc (car x)))	; current line is nearer
		(setq marker-line x))
	    (throw 'marker t))))
      (setq marker (nth 3 (cadr marker-line))
	    marker-line (or (car marker-line) 1))
      (with-current-buffer (marker-buffer marker)
	(save-excursion
	  (save-restriction
	    (widen)
	    (goto-char (marker-position marker))
	    (when (or end-col end-line)
	      (beginning-of-line (- (or end-line line) marker-line -1))
	      (if (or (null end-col) (< end-col 0))
		  (end-of-line)
		(compilation-move-to-column
		 end-col compilation-error-screen-columns))
	      (setq end-marker (list (point-marker))))
	    (beginning-of-line (if end-line
				   (- line end-line -1)
				 (- loc marker-line -1)))
	    (if col
		(compilation-move-to-column
		 col compilation-error-screen-columns)
	      (forward-to-indentation 0))
	    (setq marker (list (point-marker)))))))

    (setq loc (compilation-assq line (cdr file-struct)))
    (if end-line
	(setq end-loc (compilation-assq end-line (cdr file-struct))
	      end-loc (compilation-assq end-col end-loc))
      (if end-col			; use same line element
	  (setq end-loc (compilation-assq end-col loc))))
    (setq loc (compilation-assq col loc))
    ;; If they are new, make the loc(s) reference the file they point to.
    (or (cdr loc) (setcdr loc `(,line ,file-struct ,@marker)))
    (if end-loc
	(or (cdr end-loc)
	    (setcdr end-loc `(,(or end-line line) ,file-struct ,@end-marker))))

    ;; Must start with face
    `(face ,compilation-message-face
	   message (,loc ,type ,end-loc)
	   ,@(if compilation-debug
		 `(debug (,(assoc (with-no-warnings matcher) font-lock-keywords)
			  ,@(match-data))))
	   help-echo ,(if col
			  "mouse-2: visit this file, line and column"
			(if line
			    "mouse-2: visit this file and line"
			  "mouse-2: visit this file"))
	   keymap compilation-button-map
	   mouse-face highlight)))

(defun compilation-mode-font-lock-keywords ()
  "Return expressions to highlight in Compilation mode."
  (if compilation-parse-errors-function
      ;; An old package!  Try the compatibility code.
      '((compilation-compat-parse-errors))
    (append
     ;; make directory tracking
     (if compilation-directory-matcher
	 `((,(car compilation-directory-matcher)
	    ,@(mapcar (lambda (elt)
			`(,(car elt)
			  (compilation-directory-properties
			   ,(car elt) ,(cdr elt))
			  t t))
		      (cdr compilation-directory-matcher)))))

     ;; Compiler warning/error lines.
     (mapcar
      (lambda (item)
	(if (symbolp item)
	    (setq item (cdr (assq item
				  compilation-error-regexp-alist-alist))))
	(let ((file (nth 1 item))
	      (line (nth 2 item))
	      (col (nth 3 item))
	      (type (nth 4 item))
	      end-line end-col fmt)
	  (if (consp file)	(setq fmt (cdr file)	  file (car file)))
	  (if (consp line)	(setq end-line (cdr line) line (car line)))
	  (if (consp col)	(setq end-col (cdr col)	  col (car col)))

	  (if (functionp line)
	      ;; The old compile.el had here an undocumented hook that
	      ;; allowed `line' to be a function that computed the actual
	      ;; error location.  Let's do our best.
	      `(,(car item)
		(0 (save-match-data
		     (compilation-compat-error-properties
		      (funcall ',line (cons (match-string ,file)
					    (cons default-directory
						  ',(nthcdr 4 item)))
			       ,(if col `(match-string ,col))))))
		(,file compilation-error-face t))

	    (unless (or (null (nth 5 item)) (integerp (nth 5 item)))
	      (error "HYPERLINK should be an integer: %s" (nth 5 item)))

	    `(,(nth 0 item)

	      ,@(when (integerp file)
		  `((,file ,(if (consp type)
				`(compilation-face ',type)
			      (aref [compilation-info-face
				     compilation-warning-face
				     compilation-error-face]
				    (or type 2))))))

	      ,@(when line
		  `((,line compilation-line-face nil t)))
	      ,@(when end-line
		  `((,end-line compilation-line-face nil t)))

	      ,@(when (integerp col)
		  `((,col compilation-column-face nil t)))
	      ,@(when (integerp end-col)
		  `((,end-col compilation-column-face nil t)))

	      ,@(nthcdr 6 item)
	      (,(or (nth 5 item) 0)
	       (compilation-error-properties ',file ,line ,end-line
					     ,col ,end-col ',(or type 2)
					     ',fmt)
	       append)))))		; for compilation-message-face
      compilation-error-regexp-alist)

     compilation-mode-font-lock-keywords)))


;;;###autoload
(defun compile (command &optional comint)
  "Compile the program including the current buffer.  Default: run `make'.
Runs COMMAND, a shell command, in a separate process asynchronously
with output going to the buffer `*compilation*'.

You can then use the command \\[next-error] to find the next error message
and move to the source code that caused it.

If optional second arg COMINT is t the buffer will be in Comint mode with
`compilation-shell-minor-mode'.

Interactively, prompts for the command if `compilation-read-command' is
non-nil; otherwise uses `compile-command'.  With prefix arg, always prompts.
Additionally, with universal prefix arg, compilation buffer will be in
comint mode, i.e. interactive.

To run more than one compilation at once, start one then rename
the \`*compilation*' buffer to some other name with
\\[rename-buffer].  Then _switch buffers_ and start the new compilation.
It will create a new \`*compilation*' buffer.

On most systems, termination of the main compilation process
kills its subprocesses.

The name used for the buffer is actually whatever is returned by
the function in `compilation-buffer-name-function', so you can set that
to a function that generates a unique name."
  (interactive
   (list
    (let ((command (eval compile-command)))
      (if (or compilation-read-command current-prefix-arg)
	  (read-shell-command "Compile command: " command
                              (if (equal (car compile-history) command)
                                  '(compile-history . 1)
                                'compile-history))
	command))
    (consp current-prefix-arg)))
  (unless (equal command (eval compile-command))
    (setq compile-command command))
  (save-some-buffers (not compilation-ask-about-save) nil)
  (setq-default compilation-directory default-directory)
  (compilation-start command comint))

;; run compile with the default command line
(defun recompile ()
  "Re-compile the program including the current buffer.
If this is run in a Compilation mode buffer, re-use the arguments from the
original use.  Otherwise, recompile using `compile-command'."
  (interactive)
  (save-some-buffers (not compilation-ask-about-save) nil)
  (let ((default-directory (or compilation-directory default-directory)))
    (apply 'compilation-start (or compilation-arguments
				  `(,(eval compile-command))))))

(defcustom compilation-scroll-output nil
  "Non-nil to scroll the *compilation* buffer window as output appears.

Setting it causes the Compilation mode commands to put point at the
end of their output window so that the end of the output is always
visible rather than the beginning.

The value `first-error' stops scrolling at the first error, and leaves
point on its location in the *compilation* buffer."
  :type '(choice (const :tag "No scrolling" nil)
		 (const :tag "Scroll compilation output" t)
		 (const :tag "Stop scrolling at the first error" first-error))
  :version "20.3"
  :group 'compilation)


(defun compilation-buffer-name (mode-name mode-command name-function)
  "Return the name of a compilation buffer to use.
If NAME-FUNCTION is non-nil, call it with one argument MODE-NAME
to determine the buffer name.
Likewise if `compilation-buffer-name-function' is non-nil.
If current buffer has the major mode MODE-COMMAND,
return the name of the current buffer, so that it gets reused.
Otherwise, construct a buffer name from MODE-NAME."
  (cond (name-function
	 (funcall name-function mode-name))
	(compilation-buffer-name-function
	 (funcall compilation-buffer-name-function mode-name))
	((eq mode-command major-mode)
	 (buffer-name))
	(t
	 (concat "*" (downcase mode-name) "*"))))

;; This is a rough emulation of the old hack, until the transition to new
;; compile is complete.
(defun compile-internal (command error-message
				 &optional name-of-mode parser
				 error-regexp-alist name-function
				 enter-regexp-alist leave-regexp-alist
				 file-regexp-alist nomessage-regexp-alist
				 no-async highlight-regexp local-map)
  (if parser
      (error "Compile now works very differently, see `compilation-error-regexp-alist'"))
  (let ((compilation-error-regexp-alist
	 (append file-regexp-alist (or error-regexp-alist
				       compilation-error-regexp-alist)))
	(compilation-error (replace-regexp-in-string "^No more \\(.+\\)s\\.?"
						     "\\1" error-message)))
    (compilation-start command nil name-function highlight-regexp)))
(make-obsolete 'compile-internal 'compilation-start "22.1")

;;;###autoload
(defun compilation-start (command &optional mode name-function highlight-regexp)
  "Run compilation command COMMAND (low level interface).
If COMMAND starts with a cd command, that becomes the `default-directory'.
The rest of the arguments are optional; for them, nil means use the default.

MODE is the major mode to set in the compilation buffer.  Mode
may also be t meaning use `compilation-shell-minor-mode' under `comint-mode'.

If NAME-FUNCTION is non-nil, call it with one argument (the mode name)
to determine the buffer name.  Otherwise, the default is to
reuses the current buffer if it has the proper major mode,
else use or create a buffer with name based on the major mode.

If HIGHLIGHT-REGEXP is non-nil, `next-error' will temporarily highlight
the matching section of the visited source line; the default is to use the
global value of `compilation-highlight-regexp'.

Returns the compilation buffer created."
  (or mode (setq mode 'compilation-mode))
  (let* ((name-of-mode
	  (if (eq mode t)
	      (prog1 "compilation" (require 'comint))
	    (replace-regexp-in-string "-mode$" "" (symbol-name mode))))
	 (thisdir default-directory)
	 outwin outbuf)
    (with-current-buffer
	(setq outbuf
	      (get-buffer-create
               (compilation-buffer-name name-of-mode mode name-function)))
      (let ((comp-proc (get-buffer-process (current-buffer))))
	(if comp-proc
	    (if (or (not (eq (process-status comp-proc) 'run))
		    (yes-or-no-p
		     (format "A %s process is running; kill it? "
			     name-of-mode)))
		(condition-case ()
		    (progn
		      (interrupt-process comp-proc)
		      (sit-for 1)
		      (delete-process comp-proc))
		  (error nil))
	      (error "Cannot have two processes in `%s' at once"
		     (buffer-name)))))
      (buffer-disable-undo (current-buffer))
      ;; first transfer directory from where M-x compile was called
      (setq default-directory thisdir)
      ;; Make compilation buffer read-only.  The filter can still write it.
      ;; Clear out the compilation buffer.
      (let ((inhibit-read-only t)
	    (default-directory thisdir))
	;; Then evaluate a cd command if any, but don't perform it yet, else
	;; start-command would do it again through the shell: (cd "..") AND
	;; sh -c "cd ..; make"
	(cd (if (string-match "^\\s *cd\\(?:\\s +\\(\\S +?\\)\\)?\\s *[;&\n]" command)
		(if (match-end 1)
		    (substitute-env-vars (match-string 1 command))
		  "~")
	      default-directory))
	(erase-buffer)
	;; Select the desired mode.
	(if (not (eq mode t))
	    (funcall mode)
	  (setq buffer-read-only nil)
	  (with-no-warnings (comint-mode))
	  (compilation-shell-minor-mode))
        ;; Remember the original dir, so we can use it when we recompile.
        ;; default-directory' can't be used reliably for that because it may be
        ;; affected by the special handling of "cd ...;".
        ;; NB: must be fone after (funcall mode) as that resets local variables
        (set (make-local-variable 'compilation-directory) thisdir)
	(if highlight-regexp
	    (set (make-local-variable 'compilation-highlight-regexp)
		 highlight-regexp))
        (if (or compilation-auto-jump-to-first-error
		(eq compilation-scroll-output 'first-error))
            (set (make-local-variable 'compilation-auto-jump-to-next) t))
	;; Output a mode setter, for saving and later reloading this buffer.
	(insert "-*- mode: " name-of-mode
		"; default-directory: " (prin1-to-string default-directory)
		" -*-\n"
		(format "%s started at %s\n\n"
			mode-name
			(substring (current-time-string) 0 19))
		command "\n")
	(setq thisdir default-directory))
      (set-buffer-modified-p nil))
    ;; Pop up the compilation buffer.
    ;; http://lists.gnu.org/archive/html/emacs-devel/2007-11/msg01638.html
    (setq outwin (display-buffer outbuf))
    (with-current-buffer outbuf
      (let ((process-environment
	     (append
	      compilation-environment
	      (if (if (boundp 'system-uses-terminfo) ; `if' for compiler warning
		      system-uses-terminfo)
		  (list "TERM=dumb" "TERMCAP="
			(format "COLUMNS=%d" (window-width)))
		(list "TERM=emacs"
		      (format "TERMCAP=emacs:co#%d:tc=unknown:"
			      (window-width))))
	      ;; Set the EMACS variable, but
	      ;; don't override users' setting of $EMACS.
	      (unless (getenv "EMACS")
		(list "EMACS=t"))
	      (list "INSIDE_EMACS=t")
	      (copy-sequence process-environment))))
	(set (make-local-variable 'compilation-arguments)
	     (list command mode name-function highlight-regexp))
	(set (make-local-variable 'revert-buffer-function)
	     'compilation-revert-buffer)
	(set-window-start outwin (point-min))

	;; Position point as the user will see it.
	(let ((desired-visible-point
	       ;; Put it at the end if `compilation-scroll-output' is set.
	       (if compilation-scroll-output
		   (point-max)
		 ;; Normally put it at the top.
		 (point-min))))
	  (if (eq outwin (selected-window))
	      (goto-char desired-visible-point)
	    (set-window-point outwin desired-visible-point)))

	;; The setup function is called before compilation-set-window-height
	;; so it can set the compilation-window-height buffer locally.
	(if compilation-process-setup-function
	    (funcall compilation-process-setup-function))
	(compilation-set-window-height outwin)
	;; Start the compilation.
	(let ((proc
	       (if (eq mode t)
		   ;; comint uses `start-file-process'.
		   (get-buffer-process
		    (with-no-warnings
		      (comint-exec
		       outbuf (downcase mode-name)
		       (if (file-remote-p default-directory)
			   "/bin/sh"
			 shell-file-name)
		       nil `("-c" ,command))))
		 (start-file-process-shell-command (downcase mode-name)
						   outbuf command))))
	  ;; Make the buffer's mode line show process state.
	  (setq mode-line-process
		(list (propertize ":%s" 'face 'compilation-warning)))
	  (set-process-sentinel proc 'compilation-sentinel)
	  (set-process-filter proc 'compilation-filter)
	  ;; Use (point-max) here so that output comes in
	  ;; after the initial text,
	  ;; regardless of where the user sees point.
	  (set-marker (process-mark proc) (point-max) outbuf)
	  (when compilation-disable-input
	    (condition-case nil
		(process-send-eof proc)
	      ;; The process may have exited already.
	      (error nil)))
	  (setq compilation-in-progress
		(cons proc compilation-in-progress))))
      ;; Now finally cd to where the shell started make/grep/...
      (setq default-directory thisdir))
    (if (buffer-local-value 'compilation-scroll-output outbuf)
	(save-selected-window
	  (select-window outwin)
	  (goto-char (point-max))))
    ;; Make it so the next C-x ` will use this buffer.
    (setq next-error-last-buffer outbuf)))

(defun compilation-set-window-height (window)
  "Set the height of WINDOW according to `compilation-window-height'."
  (let ((height (buffer-local-value 'compilation-window-height (window-buffer window))))
    (and height
	 (window-full-width-p window)
	 ;; If window is alone in its frame, aside from a minibuffer,
	 ;; don't change its height.
	 (not (eq window (frame-root-window (window-frame window))))
	 ;; Stef said that doing the saves in this order is safer:
	 (save-excursion
	   (save-selected-window
	     (select-window window)
	     (enlarge-window (- height (window-height))))))))

(defvar compilation-menu-map
  (let ((map (make-sparse-keymap "Errors"))
	(opt-map (make-sparse-keymap "Skip")))
    (define-key map [stop-subjob]
      '(menu-item "Stop Compilation" kill-compilation
		  :help "Kill the process made by the M-x compile or M-x grep commands"))
    (define-key map [compilation-mode-separator3]
      '("----" . nil))
    (define-key map [compilation-next-error-follow-minor-mode]
      '(menu-item
	"Auto Error Display" next-error-follow-minor-mode
	:help "Display the error under cursor when moving the cursor"
	:button (:toggle . next-error-follow-minor-mode)))
    (define-key map [compilation-skip]
      (cons "Skip Less Important Messages" opt-map))
    (define-key opt-map [compilation-skip-none]
      '(menu-item "Don't Skip Any Messages"
		  (lambda ()
		    (interactive)
		    (customize-set-variable 'compilation-skip-threshold 0))
		  :help "Do not skip any type of messages"
		  :button (:radio . (eq compilation-skip-threshold 0))))
    (define-key opt-map [compilation-skip-info]
      '(menu-item "Skip Info"
		  (lambda ()
		    (interactive)
		    (customize-set-variable 'compilation-skip-threshold 1))
		  :help "Skip anything less than warning"
		  :button (:radio . (eq compilation-skip-threshold 1))))
    (define-key opt-map [compilation-skip-warning-and-info]
      '(menu-item "Skip Warnings and Info"
		  (lambda ()
		    (interactive)
		    (customize-set-variable 'compilation-skip-threshold 2))
		  :help "Skip over Warnings and Info, stop for errors"
		  :button (:radio . (eq compilation-skip-threshold 2))))
    (define-key map [compilation-mode-separator2]
      '("----" . nil))
    (define-key map [compilation-first-error]
      '(menu-item "First Error" first-error
		  :help "Restart at the first error, visit corresponding source code"))
    (define-key map [compilation-previous-error]
      '(menu-item "Previous Error" previous-error
		  :help "Visit previous `next-error' message and corresponding source code"))
    (define-key map [compilation-next-error]
      '(menu-item "Next Error" next-error
		  :help "Visit next `next-error' message and corresponding source code"))
    map))

(defvar compilation-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-2] 'compile-goto-error)
    (define-key map [follow-link] 'mouse-face)
    (define-key map "\C-c\C-c" 'compile-goto-error)
    (define-key map "\C-m" 'compile-goto-error)
    (define-key map "\C-c\C-k" 'kill-compilation)
    (define-key map "\M-n" 'compilation-next-error)
    (define-key map "\M-p" 'compilation-previous-error)
    (define-key map "\M-{" 'compilation-previous-file)
    (define-key map "\M-}" 'compilation-next-file)
    ;; Set up the menu-bar
    (define-key map [menu-bar compilation]
      (cons "Errors" compilation-menu-map))
    map)
  "Keymap for `compilation-minor-mode'.")

(defvar compilation-shell-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\M-\C-m" 'compile-goto-error)
    (define-key map "\M-\C-n" 'compilation-next-error)
    (define-key map "\M-\C-p" 'compilation-previous-error)
    (define-key map "\M-{" 'compilation-previous-file)
    (define-key map "\M-}" 'compilation-next-file)
    ;; Set up the menu-bar
    (define-key map [menu-bar compilation]
      (cons "Errors" compilation-menu-map))
    map)
  "Keymap for `compilation-shell-minor-mode'.")

(defvar compilation-button-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-2] 'compile-goto-error)
    (define-key map [follow-link] 'mouse-face)
    (define-key map "\C-m" 'compile-goto-error)
    map)
  "Keymap for compilation-message buttons.")
(fset 'compilation-button-map compilation-button-map)

(defvar compilation-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Don't inherit from compilation-minor-mode-map,
    ;; because that introduces a menu bar item we don't want.
    ;; That confuses C-down-mouse-3.
    (define-key map [mouse-2] 'compile-goto-error)
    (define-key map [follow-link] 'mouse-face)
    (define-key map "\C-c\C-c" 'compile-goto-error)
    (define-key map "\C-m" 'compile-goto-error)
    (define-key map "\C-c\C-k" 'kill-compilation)
    (define-key map "\M-n" 'compilation-next-error)
    (define-key map "\M-p" 'compilation-previous-error)
    (define-key map "\M-{" 'compilation-previous-file)
    (define-key map "\M-}" 'compilation-next-file)
    (define-key map "\t" 'compilation-next-error)
    (define-key map [backtab] 'compilation-previous-error)

    (define-key map " " 'scroll-up)
    (define-key map "\^?" 'scroll-down)
    (define-key map "\C-c\C-f" 'next-error-follow-minor-mode)

    ;; Set up the menu-bar
    (let ((submap (make-sparse-keymap "Compile")))
      (define-key map [menu-bar compilation]
	(cons "Compile" submap))
      (set-keymap-parent submap compilation-menu-map))
    (define-key map [menu-bar compilation compilation-separator2]
      '("----" . nil))
    (define-key map [menu-bar compilation compilation-grep]
      '(menu-item "Search Files (grep)..." grep
		  :help "Run grep, with user-specified args, and collect output in a buffer"))
    (define-key map [menu-bar compilation compilation-recompile]
      '(menu-item "Recompile" recompile
	:help "Re-compile the program including the current buffer"))
    (define-key map [menu-bar compilation compilation-compile]
      '(menu-item "Compile..." compile
		  :help "Compile the program including the current buffer.  Default: run `make'"))
    map)
  "Keymap for compilation log buffers.
`compilation-minor-mode-map' is a parent of this.")

(defvar compilation-mode-tool-bar-map
  (if (display-graphic-p)
      (let ((map (butlast (copy-keymap tool-bar-map)))
	    (help (last tool-bar-map))) ;; Keep Help last in tool bar
	(tool-bar-local-item
	 "left-arrow" 'previous-error-no-select 'previous-error-no-select map
	 :rtl "right-arrow"
	 :help "Goto previous error")
	(tool-bar-local-item
	 "right-arrow" 'next-error-no-select 'next-error-no-select map
	 :rtl "left-arrow"
	 :help "Goto next error")
	(tool-bar-local-item
	 "cancel" 'kill-compilation 'kill-compilation map
	 :enable '(let ((buffer (compilation-find-buffer)))
		    (get-buffer-process buffer))
	 :help "Stop compilation")
	(tool-bar-local-item
	 "refresh" 'recompile 'recompile map
	 :help "Restart compilation")
	(append map help))))

(put 'compilation-mode 'mode-class 'special)

;;;###autoload
(defun compilation-mode (&optional name-of-mode)
  "Major mode for compilation log buffers.
\\<compilation-mode-map>To visit the source for a line-numbered error,
move point to the error message line and type \\[compile-goto-error].
To kill the compilation, type \\[kill-compilation].

Runs `compilation-mode-hook' with `run-mode-hooks' (which see).

\\{compilation-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (use-local-map compilation-mode-map)
  (set (make-local-variable 'tool-bar-map) compilation-mode-tool-bar-map)
  (setq major-mode 'compilation-mode
	mode-name (or name-of-mode "Compilation"))
  (set (make-local-variable 'page-delimiter)
       compilation-page-delimiter)
  (compilation-setup)
  (setq buffer-read-only t)
  (run-mode-hooks 'compilation-mode-hook))

(defmacro define-compilation-mode (mode name doc &rest body)
  "This is like `define-derived-mode' without the PARENT argument.
The parent is always `compilation-mode' and the customizable `compilation-...'
variables are also set from the name of the mode you have chosen,
by replacing the first word, e.g `compilation-scroll-output' from
`grep-scroll-output' if that variable exists."
  (let ((mode-name (replace-regexp-in-string "-mode\\'" "" (symbol-name mode))))
    `(define-derived-mode ,mode compilation-mode ,name
       ,doc
       ,@(mapcar (lambda (v)
		   (setq v (cons v
				 (intern-soft (replace-regexp-in-string
					       "^compilation" mode-name
					       (symbol-name v)))))
		   (and (cdr v)
			(or (boundp (cdr v))
			    (if (boundp 'byte-compile-bound-variables)
				(memq (cdr v) byte-compile-bound-variables)))
			`(set (make-local-variable ',(car v)) ,(cdr v))))
		 '(compilation-buffer-name-function
		   compilation-directory-matcher
		   compilation-error
		   compilation-error-regexp-alist
		   compilation-error-regexp-alist-alist
		   compilation-error-screen-columns
		   compilation-finish-function
		   compilation-finish-functions
		   compilation-first-column
		   compilation-mode-font-lock-keywords
		   compilation-page-delimiter
		   compilation-parse-errors-filename-function
		   compilation-process-setup-function
		   compilation-scroll-output
		   compilation-search-path
		   compilation-skip-threshold
		   compilation-window-height))
       ,@body)))

(defun compilation-revert-buffer (ignore-auto noconfirm)
  (if buffer-file-name
      (let (revert-buffer-function)
	(revert-buffer ignore-auto noconfirm))
    (if (or noconfirm (yes-or-no-p (format "Restart compilation? ")))
	(apply 'compilation-start compilation-arguments))))

(defvar compilation-current-error nil
  "Marker to the location from where the next error will be found.
The global commands next/previous/first-error/goto-error use this.")

(defvar compilation-messages-start nil
  "Buffer position of the beginning of the compilation messages.
If nil, use the beginning of buffer.")

;; A function name can't be a hook, must be something with a value.
(defconst compilation-turn-on-font-lock 'turn-on-font-lock)

(defun compilation-setup (&optional minor)
  "Prepare the buffer for the compilation parsing commands to work.
Optional argument MINOR indicates this is called from
`compilation-minor-mode'."
  (make-local-variable 'compilation-current-error)
  (make-local-variable 'compilation-messages-start)
  (make-local-variable 'compilation-error-screen-columns)
  (make-local-variable 'overlay-arrow-position)
  (set (make-local-variable 'overlay-arrow-string) "")
  (setq next-error-overlay-arrow-position nil)
  (add-hook 'kill-buffer-hook
	    (lambda () (setq next-error-overlay-arrow-position nil)) nil t)
  ;; Note that compilation-next-error-function is for interfacing
  ;; with the next-error function in simple.el, and it's only
  ;; coincidentally named similarly to compilation-next-error.
  (setq next-error-function 'compilation-next-error-function)
  (set (make-local-variable 'comint-file-name-prefix)
       (or (file-remote-p default-directory) ""))
  (set (make-local-variable 'font-lock-extra-managed-props)
       '(directory message help-echo mouse-face debug))
  (set (make-local-variable 'compilation-locs)
       (make-hash-table :test 'equal :weakness 'value))
  ;; lazy-lock would never find the message unless it's scrolled to.
  ;; jit-lock might fontify some things too late.
  (set (make-local-variable 'font-lock-support-mode) nil)
  (set (make-local-variable 'font-lock-maximum-size) nil)
  (if minor
      (let ((fld font-lock-defaults))
	(font-lock-add-keywords nil (compilation-mode-font-lock-keywords))
	(if font-lock-mode
	    (if fld
		(font-lock-fontify-buffer)
	      (font-lock-change-mode)
	      (turn-on-font-lock))
	  (turn-on-font-lock)))
    (setq font-lock-defaults '(compilation-mode-font-lock-keywords t))
    ;; maybe defer font-lock till after derived mode is set up
    (run-mode-hooks 'compilation-turn-on-font-lock)))

;;;###autoload
(define-minor-mode compilation-shell-minor-mode
  "Toggle compilation shell minor mode.
With arg, turn compilation mode on if and only if arg is positive.
In this minor mode, all the error-parsing commands of the
Compilation major mode are available but bound to keys that don't
collide with Shell mode.  See `compilation-mode'.
Turning the mode on runs the normal hook `compilation-shell-minor-mode-hook'."
  nil " Shell-Compile"
  :group 'compilation
  (if compilation-shell-minor-mode
      (compilation-setup t)
    (font-lock-remove-keywords nil (compilation-mode-font-lock-keywords))
    (font-lock-fontify-buffer)))

;;;###autoload
(define-minor-mode compilation-minor-mode
  "Toggle compilation minor mode.
With arg, turn compilation mode on if and only if arg is positive.
In this minor mode, all the error-parsing commands of the
Compilation major mode are available.  See `compilation-mode'.
Turning the mode on runs the normal hook `compilation-minor-mode-hook'."
  nil " Compilation"
  :group 'compilation
  (if compilation-minor-mode
      (compilation-setup t)
    (font-lock-remove-keywords nil (compilation-mode-font-lock-keywords))
    (font-lock-fontify-buffer)))

(defun compilation-handle-exit (process-status exit-status msg)
  "Write MSG in the current buffer and hack its `mode-line-process'."
  (let ((inhibit-read-only t)
	(status (if compilation-exit-message-function
		    (funcall compilation-exit-message-function
			     process-status exit-status msg)
		  (cons msg exit-status)))
	(omax (point-max))
	(opoint (point))
	(cur-buffer (current-buffer)))
    ;; Record where we put the message, so we can ignore it later on.
    (goto-char omax)
    (insert ?\n mode-name " " (car status))
    (if (and (numberp compilation-window-height)
	     (zerop compilation-window-height))
	(message "%s" (cdr status)))
    (if (bolp)
	(forward-char -1))
    (insert " at " (substring (current-time-string) 0 19))
    (goto-char (point-max))
    ;; Prevent that message from being recognized as a compilation error.
    (add-text-properties omax (point)
			 (append '(compilation-handle-exit t) nil))
    (setq mode-line-process
	  (let ((out-string (format ":%s [%s]" process-status (cdr status)))
		(msg (format "%s %s" mode-name
			     (replace-regexp-in-string "\n?$" "" (car status)))))
	    (message "%s" msg)
	    (propertize out-string
			'help-echo msg 'face (if (> exit-status 0)
						 'compilation-error
					       'compilation-info))))
    ;; Force mode line redisplay soon.
    (force-mode-line-update)
    (if (and opoint (< opoint omax))
	(goto-char opoint))
    (with-no-warnings
      (if compilation-finish-function
	  (funcall compilation-finish-function cur-buffer msg)))
    (run-hook-with-args 'compilation-finish-functions cur-buffer msg)))

;; Called when compilation process changes state.
(defun compilation-sentinel (proc msg)
  "Sentinel for compilation buffers."
  (if (memq (process-status proc) '(exit signal))
      (let ((buffer (process-buffer proc)))
	(if (null (buffer-name buffer))
	    ;; buffer killed
	    (set-process-buffer proc nil)
	  (with-current-buffer buffer
	    ;; Write something in the compilation buffer
	    ;; and hack its mode line.
	    (compilation-handle-exit (process-status proc)
				     (process-exit-status proc)
				     msg)
	    ;; Since the buffer and mode line will show that the
	    ;; process is dead, we can delete it now.  Otherwise it
	    ;; will stay around until M-x list-processes.
	    (delete-process proc)))
	(setq compilation-in-progress (delq proc compilation-in-progress)))))

(defun compilation-filter (proc string)
  "Process filter for compilation buffers.
Just inserts the text, but uses `insert-before-markers'."
  (if (buffer-name (process-buffer proc))
      (with-current-buffer (process-buffer proc)
	(let ((inhibit-read-only t))
	  (save-excursion
	    (goto-char (process-mark proc))
	    (insert-before-markers string)
	    (run-hooks 'compilation-filter-hook))))))

;;; test if a buffer is a compilation buffer, assuming we're in the buffer
(defsubst compilation-buffer-internal-p ()
  "Test if inside a compilation buffer."
  (local-variable-p 'compilation-locs))

;;; test if a buffer is a compilation buffer, using compilation-buffer-internal-p
(defsubst compilation-buffer-p (buffer)
  "Test if BUFFER is a compilation buffer."
  (with-current-buffer buffer
    (compilation-buffer-internal-p)))

(defmacro compilation-loop (< property-change 1+ error limit)
  `(let (opt)
     (while (,< n 0)
       (setq opt pt)
       (or (setq pt (,property-change pt 'message))
	   ;; Handle the case where where the first error message is
	   ;; at the start of the buffer, and n < 0.
	   (if (or (eq (get-text-property ,limit 'message)
		       (get-text-property opt 'message))
		   (eq pt opt))
	       (error ,error compilation-error)
	     (setq pt ,limit)))
       ;; prop 'message usually has 2 changes, on and off, so
       ;; re-search if off
       (or (setq msg (get-text-property pt 'message))
	   (if (setq pt (,property-change pt 'message nil ,limit))
	       (setq msg (get-text-property pt 'message)))
	   (error ,error compilation-error))
       (or (< (cadr msg) compilation-skip-threshold)
	   (if different-file
	       (eq (prog1 last (setq last (nth 2 (car msg))))
		   last))
	   (if compilation-skip-visited
	       (nthcdr 5 (car msg)))
	   (if compilation-skip-to-next-location
	       (eq (car msg) loc))
	   ;; count this message only if none of the above are true
	   (setq n (,1+ n))))))

(defun compilation-next-error (n &optional different-file pt)
  "Move point to the next error in the compilation buffer.
This function does NOT find the source line like \\[next-error].
Prefix arg N says how many error messages to move forwards (or
backwards, if negative).
Optional arg DIFFERENT-FILE, if non-nil, means find next error for a
file that is different from the current one.
Optional arg PT, if non-nil, specifies the value of point to start
looking for the next message."
  (interactive "p")
  (or (compilation-buffer-p (current-buffer))
      (error "Not in a compilation buffer"))
  (or pt (setq pt (point)))
  (let* ((msg (get-text-property pt 'message))
         ;; `loc' is used by the compilation-loop macro.
	 (loc (car msg))
	 last)
    (if (zerop n)
	(unless (or msg			; find message near here
		    (setq msg (get-text-property (max (1- pt) (point-min))
						 'message)))
	  (setq pt (previous-single-property-change pt 'message nil
						    (line-beginning-position)))
	  (unless (setq msg (get-text-property (max (1- pt) (point-min)) 'message))
	    (setq pt (next-single-property-change pt 'message nil
						  (line-end-position)))
	    (or (setq msg (get-text-property pt 'message))
		(setq pt (point)))))
      (setq last (nth 2 (car msg)))
      (if (>= n 0)
	  (compilation-loop > next-single-property-change 1-
			    (if (get-buffer-process (current-buffer))
				"No more %ss yet"
			      "Moved past last %s")
			    (point-max))
	;; Don't move "back" to message at or before point.
	;; Pass an explicit (point-min) to make sure pt is non-nil.
	(setq pt (previous-single-property-change pt 'message nil (point-min)))
	(compilation-loop < previous-single-property-change 1+
			  "Moved back before first %s" (point-min))))
    (goto-char pt)
    (or msg
	(error "No %s here" compilation-error))))

(defun compilation-previous-error (n)
  "Move point to the previous error in the compilation buffer.
Prefix arg N says how many error messages to move backwards (or
forwards, if negative).
Does NOT find the source line like \\[previous-error]."
  (interactive "p")
  (compilation-next-error (- n)))

(defun compilation-next-file (n)
  "Move point to the next error for a different file than the current one.
Prefix arg N says how many files to move forwards (or backwards, if negative)."
  (interactive "p")
  (compilation-next-error n t))

(defun compilation-previous-file (n)
  "Move point to the previous error for a different file than the current one.
Prefix arg N says how many files to move backwards (or forwards, if negative)."
  (interactive "p")
  (compilation-next-file (- n)))

(defun kill-compilation ()
  "Kill the process made by the \\[compile] or \\[grep] commands."
  (interactive)
  (let ((buffer (compilation-find-buffer)))
    (if (get-buffer-process buffer)
	(interrupt-process (get-buffer-process buffer))
      (error "The %s process is not running" (downcase mode-name)))))

(defalias 'compile-mouse-goto-error 'compile-goto-error)

(defun compile-goto-error (&optional event)
  "Visit the source for the error message at point.
Use this command in a compilation log buffer.  Sets the mark at point there."
  (interactive (list last-input-event))
  (if event (posn-set-point (event-end event)))
  (or (compilation-buffer-p (current-buffer))
      (error "Not in a compilation buffer"))
  (if (get-text-property (point) 'directory)
      (dired-other-window (car (get-text-property (point) 'directory)))
    (push-mark)
    (setq compilation-current-error (point))
    (next-error-internal)))

(defun compilation-find-buffer (&optional avoid-current)
  "Return a compilation buffer.
If AVOID-CURRENT is nil, and the current buffer is a compilation buffer,
return it.  If AVOID-CURRENT is non-nil, return the current buffer only
as a last resort."
  (if (and (compilation-buffer-internal-p) (not avoid-current))
      (current-buffer)
    (next-error-find-buffer avoid-current 'compilation-buffer-internal-p)))

;;;###autoload
(defun compilation-next-error-function (n &optional reset)
  "Advance to the next error message and visit the file where the error was.
This is the value of `next-error-function' in Compilation buffers."
  (interactive "p")
  (when reset
    (setq compilation-current-error nil))
  (let* ((columns compilation-error-screen-columns) ; buffer's local value
	 (last 1) timestamp
	 (loc (compilation-next-error (or n 1) nil
				      (or compilation-current-error
					  compilation-messages-start
					  (point-min))))
	 (end-loc (nth 2 loc))
	 (marker (point-marker)))
    (setq compilation-current-error (point-marker)
	  overlay-arrow-position
	    (if (bolp)
		compilation-current-error
	      (copy-marker (line-beginning-position)))
	  loc (car loc))
    ;; If loc contains no marker, no error in that file has been visited.
    ;; If the marker is invalid the buffer has been killed.
    ;; If the file is newer than the timestamp, it has been modified
    ;; (`omake -P' polls filesystem for changes and recompiles when needed
    ;;  in the same process and buffer).
    ;; So, recalculate all markers for that file.
    (unless (and (nth 3 loc) (marker-buffer (nth 3 loc))
                 ;; There may be no timestamp info if the loc is a `fake-loc'.
                 ;; So we skip the time-check here, although we should maybe
                 ;; change `compilation-fake-loc' to add timestamp info.
                 (or (null (nth 4 loc))
                     (equal (nth 4 loc)
                            (setq timestamp
                                  (with-current-buffer
                                      (marker-buffer (nth 3 loc))
                                    (visited-file-modtime))))))
      (with-current-buffer (compilation-find-file marker (caar (nth 2 loc))
						  (cadr (car (nth 2 loc))))
	(save-restriction
	  (widen)
	  (goto-char (point-min))
	  ;; Treat file's found lines in forward order, 1 by 1.
	  (dolist (line (reverse (cddr (nth 2 loc))))
	    (when (car line)		; else this is a filename w/o a line#
	      (beginning-of-line (- (car line) last -1))
	      (setq last (car line)))
	    ;; Treat line's found columns and store/update a marker for each.
	    (dolist (col (cdr line))
	      (if (car col)
		  (if (eq (car col) -1)	; special case for range end
		      (end-of-line)
		    (compilation-move-to-column (car col) columns))
		(beginning-of-line)
		(skip-chars-forward " \t"))
	      (if (nth 3 col)
		  (set-marker (nth 3 col) (point))
		(setcdr (nthcdr 2 col) `(,(point-marker)))))))))
    (compilation-goto-locus marker (nth 3 loc) (nth 3 end-loc))
    (setcdr (nthcdr 3 loc) (list timestamp))
    (setcdr (nthcdr 4 loc) t)))		; Set this one as visited.

(defvar compilation-gcpro nil
  "Internal variable used to keep some values from being GC'd.")
(make-variable-buffer-local 'compilation-gcpro)

(defun compilation-fake-loc (marker file &optional line col)
  "Preassociate MARKER with FILE.
FILE should be ABSOLUTE-FILENAME or (RELATIVE-FILENAME . DIRNAME).
This is useful when you compile temporary files, but want
automatic translation of the messages to the real buffer from
which the temporary file came.  This only works if done before a
message about FILE appears!

Optional args LINE and COL default to 1 and beginning of
indentation respectively.  The marker is expected to reflect
this.  In the simplest case the marker points to the first line
of the region that was saved to the temp file.

If you concatenate several regions into the temp file (e.g. a
header with variable assignments and a code region), you must
call this several times, once each for the last line of one
region and the first line of the next region."
  (or (consp file) (setq file (list file)))
  (setq file (compilation-get-file-structure file))
  ;; Between the current call to compilation-fake-loc and the first occurrence
  ;; of an error message referring to `file', the data is only kept in the
  ;; weak hash-table compilation-locs, so we need to prevent this entry
  ;; in compilation-locs from being GC'd away.  --Stef
  (push file compilation-gcpro)
  (let ((loc (compilation-assq (or line 1) (cdr file))))
    (setq loc (compilation-assq col loc))
    (if (cdr loc)
	(setcdr (cddr loc) (list marker))
      (setcdr loc (list line file marker)))
    loc))

(defcustom compilation-context-lines nil
  "Display this many lines of leading context before the current message.
If nil and the left fringe is displayed, don't scroll the
compilation output window; an arrow in the left fringe points to
the current message.  If nil and there is no left fringe, the message
displays at the top of the window; there is no arrow."
  :type '(choice integer (const :tag "No window scrolling" nil))
  :group 'compilation
  :version "22.1")

(defsubst compilation-set-window (w mk)
  "Align the compilation output window W with marker MK near top."
  (if (integerp compilation-context-lines)
      (set-window-start w (save-excursion
			    (goto-char mk)
			    (beginning-of-line
			     (- 1 compilation-context-lines))
			    (point)))
    ;; If there is no left fringe.
    (if (equal (car (window-fringes)) 0)
	(set-window-start w (save-excursion
			      (goto-char mk)
			    (beginning-of-line 1)
			    (point)))))
    (set-window-point w mk))

(defvar next-error-highlight-timer)

(defun compilation-goto-locus (msg mk end-mk)
  "Jump to an error corresponding to MSG at MK.
All arguments are markers.  If END-MK is non-nil, mark is set there
and overlay is highlighted between MK and END-MK."
  ;; Show compilation buffer in other window, scrolled to this error.
  (let* ((from-compilation-buffer (eq (window-buffer (selected-window))
                                      (marker-buffer msg)))
         ;; Use an existing window if it is in a visible frame.
         (pre-existing (get-buffer-window (marker-buffer msg) 0))
         (w (if (and from-compilation-buffer pre-existing)
                ;; Calling display-buffer here may end up (partly) hiding
                ;; the error location if the two buffers are in two
                ;; different frames.  So don't do it if it's not necessary.
                pre-existing
              (let ((display-buffer-reuse-frames t)
                    (pop-up-windows t))
	        ;; Pop up a window.
                (display-buffer (marker-buffer msg)))))
	 (highlight-regexp (with-current-buffer (marker-buffer msg)
			     ;; also do this while we change buffer
			     (compilation-set-window w msg)
			     compilation-highlight-regexp)))
    ;; Ideally, the window-size should be passed to `display-buffer' (via
    ;; something like special-display-buffer) so it's only used when
    ;; creating a new window.
    (unless pre-existing (compilation-set-window-height w))

    (if from-compilation-buffer
        ;; If the compilation buffer window was selected,
        ;; keep the compilation buffer in this window;
        ;; display the source in another window.
        (let ((pop-up-windows t))
          (pop-to-buffer (marker-buffer mk) 'other-window))
      (if (window-dedicated-p (selected-window))
          (pop-to-buffer (marker-buffer mk))
        (switch-to-buffer (marker-buffer mk))))
    ;; If narrowing gets in the way of going to the right place, widen.
    (unless (eq (goto-char mk) (point))
      (widen)
      (goto-char mk))
    (if end-mk
        (push-mark end-mk t)
      (if mark-active (setq mark-active)))
    ;; If hideshow got in the way of
    ;; seeing the right place, open permanently.
    (dolist (ov (overlays-at (point)))
      (when (eq 'hs (overlay-get ov 'invisible))
        (delete-overlay ov)
        (goto-char mk)))

    (when highlight-regexp
      (if (timerp next-error-highlight-timer)
	  (cancel-timer next-error-highlight-timer))
      (unless compilation-highlight-overlay
	(setq compilation-highlight-overlay
	      (make-overlay (point-min) (point-min)))
	(overlay-put compilation-highlight-overlay 'face 'next-error))
      (with-current-buffer (marker-buffer mk)
	(save-excursion
	  (if end-mk (goto-char end-mk) (end-of-line))
	  (let ((end (point)))
	    (if mk (goto-char mk) (beginning-of-line))
	    (if (and (stringp highlight-regexp)
		     (re-search-forward highlight-regexp end t))
		(progn
		  (goto-char (match-beginning 0))
		  (move-overlay compilation-highlight-overlay
				(match-beginning 0) (match-end 0)
				(current-buffer)))
	      (move-overlay compilation-highlight-overlay
			    (point) end (current-buffer)))
	    (if (or (eq next-error-highlight t)
		    (numberp next-error-highlight))
		;; We want highlighting: delete overlay on next input.
		(add-hook 'pre-command-hook
			  'compilation-goto-locus-delete-o)
	      ;; We don't want highlighting: delete overlay now.
	      (delete-overlay compilation-highlight-overlay))
	    ;; We want highlighting for a limited time:
	    ;; set up a timer to delete it.
	    (when (numberp next-error-highlight)
	      (setq next-error-highlight-timer
		    (run-at-time next-error-highlight nil
				 'compilation-goto-locus-delete-o)))))))
    (when (and (eq next-error-highlight 'fringe-arrow))
      ;; We want a fringe arrow (instead of highlighting).
      (setq next-error-overlay-arrow-position
	    (copy-marker (line-beginning-position))))))

(defun compilation-goto-locus-delete-o ()
  (delete-overlay compilation-highlight-overlay)
  ;; Get rid of timer and hook that would try to do this again.
  (if (timerp next-error-highlight-timer)
      (cancel-timer next-error-highlight-timer))
  (remove-hook 'pre-command-hook
	       'compilation-goto-locus-delete-o))

(defun compilation-find-file (marker filename directory &rest formats)
  "Find a buffer for file FILENAME.
If FILENAME is not found at all, ask the user where to find it.
Pop up the buffer containing MARKER and scroll to MARKER if we ask
the user where to find the file.
Search the directories in `compilation-search-path'.
A nil in `compilation-search-path' means to try the
\"current\" directory, which is passed in DIRECTORY.
If DIRECTORY is relative, it is combined with `default-directory'.
If DIRECTORY is nil, that means use `default-directory'.
FORMATS, if given, is a list of formats to reformat FILENAME when
looking for it: for each element FMT in FORMATS, this function
attempts to find a file whose name is produced by (format FMT FILENAME)."
  (or formats (setq formats '("%s")))
  (let ((dirs compilation-search-path)
        (spec-dir (if directory
                      (expand-file-name directory)
                    default-directory))
        buffer thisdir fmts name)
    (if (file-name-absolute-p filename)
        ;; The file name is absolute.  Use its explicit directory as
        ;; the first in the search path, and strip it from FILENAME.
        (setq filename (abbreviate-file-name (expand-file-name filename))
              dirs (cons (file-name-directory filename) dirs)
              filename (file-name-nondirectory filename)))
    ;; Now search the path.
    (while (and dirs (null buffer))
      (setq thisdir (or (car dirs) spec-dir)
            fmts formats)
      ;; For each directory, try each format string.
      (while (and fmts (null buffer))
        (setq name (expand-file-name (format (car fmts) filename) thisdir)
              buffer (and (file-exists-p name)
                          (find-file-noselect name))
              fmts (cdr fmts)))
      (setq dirs (cdr dirs)))
    (while (null buffer)    ;Repeat until the user selects an existing file.
      ;; The file doesn't exist.  Ask the user where to find it.
      (save-excursion            ;This save-excursion is probably not right.
        (let ((pop-up-windows t))
          (compilation-set-window (display-buffer (marker-buffer marker))
                                  marker)
          (let* ((name (read-file-name
                        (format "Find this %s in (default %s): "
                                compilation-error filename)
                        spec-dir filename t nil
                        ;; The predicate below is fine when called from
                        ;; minibuffer-complete-and-exit, but it's too
                        ;; restrictive otherwise, since it also prevents the
                        ;; user from completing "fo" to "foo/" when she
                        ;; wants to enter "foo/bar".
                        ;;
                        ;; Try to make sure the user can only select
                        ;; a valid answer.  This predicate may be ignored,
                        ;; tho, so we still have to double-check afterwards.
                        ;; TODO: We should probably fix read-file-name so
                        ;; that it never ignores this predicate, even when
                        ;; using popup dialog boxes.
                        ;; (lambda (name)
                        ;;   (if (file-directory-p name)
                        ;;       (setq name (expand-file-name filename name)))
                        ;;   (file-exists-p name))
                        ))
                 (origname name))
            (cond
             ((not (file-exists-p name))
              (message "Cannot find file `%s'" name)
              (ding) (sit-for 2))
             ((and (file-directory-p name)
                   (not (file-exists-p
                         (setq name (expand-file-name filename name)))))
              (message "No `%s' in directory %s" filename origname)
              (ding) (sit-for 2))
             (t
              (setq buffer (find-file-noselect name))))))))
    ;; Make intangible overlays tangible.
    ;; This is weird: it's not even clear which is the current buffer,
    ;; so the code below can't be expected to DTRT here.  -- Stef
    (dolist (ov (overlays-in (point-min) (point-max)))
      (when (overlay-get ov 'intangible)
        (overlay-put ov 'intangible nil)))
    buffer))

(defun compilation-get-file-structure (file &optional fmt)
  "Retrieve FILE's file-structure or create a new one.
FILE should be (FILENAME) or (RELATIVE-FILENAME . DIRNAME).
In the former case, FILENAME may be relative or absolute.

The file-structure looks like this:
  (list (list FILENAME [DIR-FROM-PREV-MSG]) FMT LINE-STRUCT...)"
  (or (gethash file compilation-locs)
      ;; File was not previously encountered, at least not in the form passed.
      ;; Let's normalize it and look again.
      (let ((filename (car file))
	    ;; Get the specified directory from FILE.
	    (spec-directory (if (cdr file)
				(file-truename (cdr file)))))

	;; Check for a comint-file-name-prefix and prepend it if appropriate.
	;; (This is very useful for compilation-minor-mode in an rlogin-mode
	;; buffer.)
	(when (and (boundp 'comint-file-name-prefix)
		   (not (equal comint-file-name-prefix "")))
	  (if (file-name-absolute-p filename)
	      (setq filename
		    (concat comint-file-name-prefix filename))
	    (if spec-directory
		(setq spec-directory
		      (file-truename
		       (concat comint-file-name-prefix spec-directory))))))

	;; If compilation-parse-errors-filename-function is
	;; defined, use it to process the filename.
	(when compilation-parse-errors-filename-function
	  (setq filename
		(funcall compilation-parse-errors-filename-function
			 filename)))

	;; Some compilers (e.g. Sun's java compiler, reportedly) produce bogus
	;; file names like "./bar//foo.c" for file "bar/foo.c";
	;; expand-file-name will collapse these into "/foo.c" and fail to find
	;; the appropriate file.  So we look for doubled slashes in the file
	;; name and fix them.
	(setq filename (command-line-normalize-file-name filename))

	;; Store it for the possibly unnormalized name
	(puthash file
		 ;; Retrieve or create file-structure for normalized name
		 ;; The gethash used to not use spec-directory, but
		 ;; this leads to errors when files in different
		 ;; directories have the same name:
		 ;; http://lists.gnu.org/archive/html/emacs-devel/2007-08/msg00463.html
		 (or (gethash (cons filename spec-directory) compilation-locs)
		     (puthash (cons filename spec-directory)
			      (list (list filename spec-directory) fmt)
			      compilation-locs))
		 compilation-locs))))

(add-to-list 'debug-ignored-errors "^No more [-a-z ]+s yet$")

;;; Compatibility with the old compile.el.

(defun compile-buffer-substring (n) (if n (match-string n)))

(defun compilation-compat-error-properties (err)
  "Map old-style error ERR to new-style message."
  ;; Old-style structure is (MARKER (FILE DIR) LINE COL) or
  ;; (MARKER . MARKER).
  (let ((dst (cdr err)))
    (if (markerp dst)
	;; Must start with a face, for font-lock.
	`(face nil
	  message ,(list (list nil nil nil dst) 2)
	  help-echo "mouse-2: visit the source location"
	  keymap compilation-button-map
	  mouse-face highlight)
      ;; Too difficult to do it by hand: dispatch to the normal code.
      (let* ((file (pop dst))
	     (line (pop dst))
	     (col (pop dst))
	     (filename (pop file))
	     (dirname (pop file))
	     (fmt (pop file)))
	(compilation-internal-error-properties
	 (cons filename dirname) line nil col nil 2 fmt)))))

(defun compilation-compat-parse-errors (limit)
  (when compilation-parse-errors-function
    ;; FIXME: We should remove the rest of the compilation keywords
    ;; but we can't do that from here because font-lock is using
    ;; the value right now.  --stef
    (save-excursion
      (setq compilation-error-list nil)
      ;; Reset compilation-parsing-end each time because font-lock
      ;; might force us the re-parse many times (typically because
      ;; some code adds some text-property to the output that we
      ;; already parsed).  You might say "why reparse", well:
      ;; because font-lock has just removed the `message' property so
      ;; have to do it all over again.
      (if compilation-parsing-end
	  (set-marker compilation-parsing-end (point))
	(setq compilation-parsing-end (point-marker)))
      (condition-case nil
	  ;; Ignore any error: we're calling this function earlier than
	  ;; in the old compile.el so things might not all be setup yet.
	  (funcall compilation-parse-errors-function limit nil)
	(error nil))
      (dolist (err (if (listp compilation-error-list) compilation-error-list))
	(let* ((src (car err))
	       (dst (cdr err))
	       (loc (cond ((markerp dst) (list nil nil nil dst))
			  ((consp dst)
			   (list (nth 2 dst) (nth 1 dst)
				 (cons (cdar dst) (caar dst)))))))
	  (when loc
	    (goto-char src)
	    ;; (put-text-property src (line-end-position) 'font-lock-face 'font-lock-warning-face)
	    (put-text-property src (line-end-position)
			       'message (list loc 2)))))))
  (goto-char limit)
  nil)

;; Beware: this is not only compatiblity code.  New code stil uses it.  --Stef
(defun compilation-forget-errors ()
  ;; In case we hit the same file/line specs, we want to recompute a new
  ;; marker for them, so flush our cache.
  (setq compilation-locs (make-hash-table :test 'equal :weakness 'value))
  (setq compilation-gcpro nil)
  ;; FIXME: the old code reset the directory-stack, so maybe we should
  ;; put a `directory change' marker of some sort, but where?  -stef
  ;;
  ;; FIXME: The old code moved compilation-current-error (which was
  ;; virtually represented by a mix of compilation-parsing-end and
  ;; compilation-error-list) to point-min, but that was only meaningful for
  ;; the internal uses of compilation-forget-errors: all calls from external
  ;; packages seem to be followed by a move of compilation-parsing-end to
  ;; something equivalent to point-max.  So we heuristically move
  ;; compilation-current-error to point-max (since the external package
  ;; won't know that it should do it).  --Stef
  (setq compilation-current-error nil)
  (let* ((proc (get-buffer-process (current-buffer)))
	 (mark (if proc (process-mark proc)))
	 (pos (or mark (point-max))))
    (setq compilation-messages-start
	  ;; In the future, ignore the text already present in the buffer.
	  ;; Since many process filter functions insert before markers,
	  ;; we need to put ours just before the insertion point rather
	  ;; than at the insertion point.  If that's not possible, then
	  ;; don't use a marker.  --Stef
	  (if (> pos (point-min)) (copy-marker (1- pos)) pos)))
  ;; Again, since this command is used in buffers that contain several
  ;; compilations, to set the beginning of "this compilation", it's a good
  ;; place to reset compilation-auto-jump-to-next.
  (set (make-local-variable 'compilation-auto-jump-to-next)
       (or compilation-auto-jump-to-first-error
	   (eq compilation-scroll-output 'first-error))))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.gcov\\'" . compilation-mode))

(provide 'compile)

;; arch-tag: 12465727-7382-4f72-b234-79855a00dd8c
;;; compile.el ends here
