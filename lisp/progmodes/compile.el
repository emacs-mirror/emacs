;;; compile.el --- run compiler as inferior of Emacs, parse error messages

;; Copyright (C) 1985, 86, 87, 93, 94, 95, 96, 97, 98, 1999, 2001
;;  Free Software Foundation, Inc.

;; Author: Roland McGrath <roland@gnu.org>
;; Maintainer: FSF
;; Keywords: tools, processes

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

;; This package provides the compile and grep facilities documented in
;; the Emacs user's manual.

;;; Code:

(defgroup compilation nil
  "Run compiler as inferior of Emacs, parse error messages."
  :group 'tools
  :group 'processes)


;;;###autoload
(defcustom compilation-mode-hook nil
  "*List of hook functions run by `compilation-mode' (see `run-hooks')."
  :type 'hook
  :group 'compilation)

;;;###autoload
(defcustom compilation-window-height nil
  "*Number of lines in a compilation window.  If nil, use Emacs default."
  :type '(choice (const :tag "Default" nil)
		 integer)
  :group 'compilation)

(defcustom compile-auto-highlight nil
  "*Specify how many compiler errors to highlight (and parse) initially.
\(Highlighting applies to an error message when the mouse is over it.)
If this is a number N, all compiler error messages in the first N lines
are highlighted and parsed as soon as they arrive in Emacs.
If t, highlight and parse the whole compilation output as soon as it arrives.
If nil, don't highlight or parse any of the buffer until you try to
move to the error messages.

Those messages which are not parsed and highlighted initially
will be parsed and highlighted as soon as you try to move to them."
  :type '(choice (const :tag "All" t)
		 (const :tag "None" nil)
		 (integer :tag "First N lines"))
  :group 'compilation)

;;; This has to be here so it can be called
;;; by the following defcustoms.
(defun grep-compute-defaults ()
  (unless (or (not grep-use-null-device) (eq grep-use-null-device t))
    (setq grep-use-null-device
	  (with-temp-buffer
	    (let ((hello-file (expand-file-name "HELLO" data-directory)))
	      (not
	       (and (equal (condition-case nil
			       (if grep-command
				   ;; `grep-command' is already set, so
				   ;; use that for testing.
				   (call-process-shell-command
				    grep-command nil t nil
				    "^English" hello-file)
				 ;; otherwise use `grep-program'
				 (call-process grep-program nil t nil
					       "-nH" "^English" hello-file))
			     (error nil))
			   0)
		    (progn
		      (goto-char (point-min))
		      (looking-at
		       (concat (regexp-quote hello-file)
			       ":[0-9]+:English")))))))))
  (unless grep-command
    (setq grep-command
	  (let ((required-options (if grep-use-null-device "-n" "-nH")))
	    (if (equal (condition-case nil ; in case "grep" isn't in exec-path
			   (call-process grep-program nil nil nil
					 "-e" "foo" null-device)
			 (error nil))
		       1)
		(format "%s %s -e " grep-program required-options)
	      (format "%s %s " grep-program required-options)))))
  (unless grep-find-use-xargs
    (setq grep-find-use-xargs
	  (if (and
               (equal (call-process "find" nil nil nil
                                    null-device "-print0")
                      0)
               (equal (call-process "xargs" nil nil nil
                                    "-0" "-e" "echo")
		     0))
	      'gnu)))
  (unless grep-find-command
    (setq grep-find-command
	  (cond ((eq grep-find-use-xargs 'gnu)
		 (format "%s . -type f -print0 | xargs -0 -e %s"
			 find-program grep-command))
		(grep-find-use-xargs
		 (format "%s . -type f -print | xargs %s"
                         find-program grep-command))
		(t (cons (format "%s . -type f -exec %s {} %s \\;"
				 find-program grep-command null-device)
			 (+ 22 (length grep-command))))))))

(defcustom grep-command nil
  "The default grep command for \\[grep].
If the grep program used supports an option to always include file names
in its output (such as the `-H' option to GNU grep), it's a good idea to
include it when specifying `grep-command'.

The default value of this variable is set up by `grep-compute-defaults';
call that function before using this variable in your program."
  :type 'string
  :get '(lambda (symbol)
	  (or grep-command
	      (progn (grep-compute-defaults) grep-command)))
  :group 'compilation)

(defcustom grep-use-null-device 'auto-detect
  "If non-nil, append the value of `null-device' to grep commands.
This is done to ensure that the output of grep includes the filename of
any match in the case where only a single file is searched, and is not
necessary if the grep program used supports the `-H' option.

The default value of this variable is set up by `grep-compute-defaults';
call that function before using this variable in your program."
  :type 'boolean
  :get '(lambda (symbol)
	  (if (and grep-use-null-device (not (eq grep-use-null-device t)))
	      (progn (grep-compute-defaults) grep-use-null-device)
	    grep-use-null-device))
  :group 'compilation)

(defcustom grep-find-command nil
  "The default find command for \\[grep-find].
The default value of this variable is set up by `grep-compute-defaults';
call that function before using this variable in your program."
  :type 'string
  :get (lambda (symbol)
	 (or grep-find-command
	     (progn (grep-compute-defaults) grep-find-command)))
  :group 'compilation)

(defvar compilation-error-list nil
  "List of error message descriptors for visiting erring functions.
Each error descriptor is a cons (or nil).  Its car is a marker pointing to
an error message.  If its cdr is a marker, it points to the text of the
line the message is about.  If its cdr is a cons, it is a list
\(\(DIRECTORY . FILE\) LINE [COLUMN]\).  Or its cdr may be nil if that
error is not interesting.

The value may be t instead of a list; this means that the buffer of
error messages should be reparsed the next time the list of errors is wanted.

Some other commands (like `diff') use this list to control the error
message tracking facilities; if you change its structure, you should make
sure you also change those packages.  Perhaps it is better not to change
it at all.")

(defvar compilation-old-error-list nil
  "Value of `compilation-error-list' after errors were parsed.")

(defvar compilation-parse-errors-function 'compilation-parse-errors
  "Function to call to parse error messages from a compilation.
It takes args LIMIT-SEARCH and FIND-AT-LEAST.
If LIMIT-SEARCH is non-nil, don't bother parsing past that location.
If FIND-AT-LEAST is non-nil, don't bother parsing after finding that
many new errors.
It should read in the source files which have errors and set
`compilation-error-list' to a list with an element for each error message
found.  See that variable for more info.")

(defvar compilation-parse-errors-filename-function nil
  "Function to call to post-process filenames while parsing error messages.
It takes one arg FILENAME which is the name of a file as found
in the compilation output, and should return a transformed file name.")

;;;###autoload
(defvar compilation-process-setup-function nil
  "*Function to call to customize the compilation process.
This functions is called immediately before the compilation process is
started.  It can be used to set any variables or functions that are used
while processing the output of the compilation process.")

;;;###autoload
(defvar compilation-buffer-name-function nil
  "Function to compute the name of a compilation buffer.
The function receives one argument, the name of the major mode of the
compilation buffer.  It should return a string.
nil means compute the name with `(concat \"*\" (downcase major-mode) \"*\")'.")

;;;###autoload
(defvar compilation-finish-function nil
  "Function to call when a compilation process finishes.
It is called with two arguments: the compilation buffer, and a string
describing how the process finished.")

;;;###autoload
(defvar compilation-finish-functions nil
  "Functions to call when a compilation process finishes.
Each function is called with two arguments: the compilation buffer,
and a string describing how the process finished.")

(defvar compilation-last-buffer nil
  "The most recent compilation buffer.
A buffer becomes most recent when its compilation is started
or when it is used with \\[next-error] or \\[compile-goto-error].")

(defvar compilation-in-progress nil
  "List of compilation processes now running.")
(or (assq 'compilation-in-progress minor-mode-alist)
    (setq minor-mode-alist (cons '(compilation-in-progress " Compiling")
				 minor-mode-alist)))

(defvar compilation-parsing-end nil
  "Marker position of end of buffer when last error messages were parsed.")

(defvar compilation-error-message "No more errors"
  "Message to print when no more matches are found.")

(defvar compilation-arguments nil
  "Arguments that were given to `compile-internal'.")

(defvar compilation-num-errors-found)

(defvar compilation-error-regexp-alist
  '(
    ;; NOTE!  See also grep-regexp-alist, below.

    ;; 4.3BSD grep, cc, lint pass 1:
    ;; 	/usr/src/foo/foo.c(8): warning: w may be used before set
    ;; or GNU utilities:
    ;; 	foo.c:8: error message
    ;; or HP-UX 7.0 fc:
    ;; 	foo.f          :16    some horrible error message
    ;; or GNU utilities with column (GNAT 1.82):
    ;;   foo.adb:2:1: Unit name does not match file name
    ;; or with column and program name:
    ;;   jade:dbcommon.dsl:133:17:E: missing argument for function call
    ;;
    ;; We'll insist that the number be followed by a colon or closing
    ;; paren, because otherwise this matches just about anything
    ;; containing a number with spaces around it.

    ;; We insist on a non-digit in the file name
    ;; so that we don't mistake the file name for a command name
    ;; and take the line number as the file name.
    ("\\([a-zA-Z][-a-zA-Z._0-9]+: ?\\)?\
\\([a-zA-Z]?:?[^:( \t\n]*[^:( \t\n0-9][^:( \t\n]*\\)[:(][ \t]*\\([0-9]+\\)\
\\([) \t]\\|:\\(\\([0-9]+:\\)\\|[0-9]*[^:0-9]\\)\\)" 2 3 6)

    ;; GNU utilities with precise locations (line and columns),
    ;; possibly ranges:
    ;;  foo.c:8.23-9.1: error message
    ("\\([a-zA-Z][-a-zA-Z._0-9]+: ?\\)\
\\([0-9]+\\)\\.\\([0-9]+\\)\
-\\([0-9]+\\)\\.\\([0-9]+\\)\
:" 1 2 3) ;; When ending points are supported, add line = 4 and col = 5.
    ;;  foo.c:8.23-45: error message
    ("\\([a-zA-Z][-a-zA-Z._0-9]+: ?\\)\
\\([0-9]+\\)\\.\\([0-9]+\\)\
-\\([0-9]+\\)\
:" 1 2 3) ;; When ending points are supported, add line = 2 and col = 4.
    ;;  foo.c:8-45.3: error message
    ("\\([a-zA-Z][-a-zA-Z._0-9]+: ?\\)\
\\([0-9]+\\)\
-\\([0-9]+\\)\\.\\([0-9]+\\)\
:" 1 2 nil) ;; When ending points are supported, add line = 2 and col = 4.
    ;;  foo.c:8.23: error message
    ("\\([a-zA-Z][-a-zA-Z._0-9]+: ?\\)\
\\([0-9]+\\)\\.\\([0-9]+\\)\
:" 1 2 3)
    ;;  foo.c:8-23: error message
    ("\\([a-zA-Z][-a-zA-Z._0-9]+: ?\\)\
\\([0-9]+\\)\
-\\([0-9]+\\)\
:" 1 2 nil);; When ending points are supported, add line = 3.

    ;; Microsoft C/C++:
    ;;  keyboard.c(537) : warning C4005: 'min' : macro redefinition
    ;;  d:\tmp\test.c(23) : error C2143: syntax error : missing ';' before 'if'
    ;; This used to be less selective and allow characters other than
    ;; parens around the line number, but that caused confusion for
    ;; GNU-style error messages.
    ;; This used to reject spaces and dashes in file names,
    ;; but they are valid now; so I made it more strict about the error
    ;; message that follows.
    ("\\(\\([a-zA-Z]:\\)?[^:(\t\n]+\\)(\\([0-9]+\\)) \
: \\(error\\|warning\\) C[0-9]+:" 1 3)

    ;; Borland C++, C++Builder:
    ;;  Error ping.c 15: Unable to open include file 'sys/types.h'
    ;;  Warning ping.c 68: Call to function 'func' with no prototype
    ;;  Error E2010 ping.c 15: Unable to open include file 'sys/types.h'
    ;;  Warning W1022 ping.c 68: Call to function 'func' with no prototype
    ("\\(Error\\|Warning\\) \\(\\([FEW][0-9]+\\) \\)?\
\\([a-zA-Z]?:?[^:( \t\n]+\\)\
 \\([0-9]+\\)\\([) \t]\\|:[^0-9\n]\\)" 4 5)

    ;; 4.3BSD lint pass 2
    ;; 	strcmp: variable # of args. llib-lc(359)  ::  /usr/src/foo/foo.c(8)
    (".*[ \t:]\\([a-zA-Z]?:?[^:( \t\n]+\\)[:(](+[ \t]*\\([0-9]+\\))[:) \t]*$"
     1 2)

    ;; 4.3BSD lint pass 3
    ;; 	bloofle defined( /users/wolfgang/foo.c(4) ), but never used
    ;; This used to be
    ;; ("[ \t(]+\\([a-zA-Z]?:?[^:( \t\n]+\\)[:( \t]+\\([0-9]+\\)[:) \t]+" 1 2)
    ;; which is regexp Impressionism - it matches almost anything!
    (".*([ \t]*\\([a-zA-Z]?:?[^:( \t\n]+\\)[:(][ \t]*\\([0-9]+\\))" 1 2)

    ;; MIPS lint pass<n>; looks good for SunPro lint also
    ;;  TrimMask (255) in solomon.c may be indistinguishable from TrimMasks (93) in solomon.c due to truncation
    ("[^\n ]+ (\\([0-9]+\\)) in \\([^ \n]+\\)" 2 1)
    ;;  name defined but never used: LinInt in cmap_calc.c(199)
    (".*in \\([^(\n]+\\)(\\([0-9]+\\))$" 1 2)

    ;; Ultrix 3.0 f77:
    ;;  fort: Severe: addstf.f, line 82: Missing operator or delimiter symbol
    ;; Some SGI cc version:
    ;;  cfe: Warning 835: foo.c, line 2: something
    ("\\(cfe\\|fort\\): [^:\n]*: \\([^ \n]*\\), line \\([0-9]+\\):" 2 3)
    ;;  Error on line 3 of t.f: Execution error unclassifiable statement
    ;; Unknown who does this:
    ;;  Line 45 of "foo.c": bloofle undefined
    ;; Absoft FORTRAN 77 Compiler 3.1.3
    ;;  error on line 19 of fplot.f: spelling error?
    ;;  warning on line 17 of fplot.f: data type is undefined for variable d
    ("\\(.* on \\)?[Ll]ine[ \t]+\\([0-9]+\\)[ \t]+\
of[ \t]+\"?\\([a-zA-Z]?:?[^\":\n]+\\)\"?:" 3 2)

    ;; Apollo cc, 4.3BSD fc:
    ;;	"foo.f", line 3: Error: syntax error near end of statement
    ;; IBM RS6000:
    ;;  "vvouch.c", line 19.5: 1506-046 (S) Syntax error.
    ;; Microtec mcc68k:
    ;;  "foo.c", line 32 pos 1; (E) syntax error; unexpected symbol: "lossage"
    ;; GNAT (as of July 94):
    ;;  "foo.adb", line 2(11): warning: file name does not match ...
    ;; IBM AIX xlc compiler:
    ;;  "src/swapping.c", line 30.34: 1506-342 (W) "/*" detected in comment.
    (".*\"\\([^,\" \n\t]+\\)\", lines? \
\\([0-9]+\\)\\([\(.]\\([0-9]+\\)\)?\\)?[:., (-]" 1 2 4)

    ;; Python:
    ;;  File "foobar.py", line 5, blah blah
   ("^File \"\\([^,\" \n\t]+\\)\", line \\([0-9]+\\)," 1 2)

    ;; Caml compiler:
    ;;  File "foobar.ml", lines 5-8, characters 20-155: blah blah
   ("^File \"\\([^,\" \n\t]+\\)\", lines? \\([0-9]+\\)[-0-9]*, characters? \\([0-9]+\\)" 1 2 3)

    ;; MIPS RISC CC - the one distributed with Ultrix:
    ;;	ccom: Error: foo.c, line 2: syntax error
    ;; DEC AXP OSF/1 cc
    ;;  /usr/lib/cmplrs/cc/cfe: Error: foo.c: 1: blah blah
    ("[a-z0-9/]+: \\([eE]rror\\|[wW]arning\\): \\([^,\" \n\t]+\\)[,:] \\(line \\)?\\([0-9]+\\):" 2 4)

    ;; IBM AIX PS/2 C version 1.1:
    ;;	****** Error number 140 in line 8 of file errors.c ******
    (".*in line \\([0-9]+\\) of file \\([^ \n]+[^. \n]\\)\\.? " 2 1)
    ;; IBM AIX lint is too painful to do right this way.  File name
    ;; prefixes entire sections rather than being on each line.

    ;; SPARCcompiler Pascal:
    ;;           20      linjer      : array[1..4] of linje;
    ;; e 18480-----------^---  Inserted ';'
    ;; and
    ;; E 18520 line 61 -  0 is undefined
    ;; These messages don't contain a file name. Instead the compiler gives
    ;; a message whenever the file being compiled is changed.
    (" +\\([0-9]+\\) +.*\n[ew] [0-9]+-+" nil 1)
    ("[Ew] +[0-9]+ line \\([0-9]+\\) -  " nil 1)

    ;; Lucid Compiler, lcc 3.x
    ;; E, file.cc(35,52) Illegal operation on pointers
    ("[EW], \\([^(\n]*\\)(\\([0-9]+\\),[ \t]*\\([0-9]+\\)" 1 2 3)

    ;; This seems to be superfluous because the first pattern matches it.
    ;; ;; GNU messages with program name and optional column number.
    ;; ("[a-zA-Z]?:?[^0-9 \n\t:]+[^ \n\t:]*:[ \t]*\\([^ \n\t:]+\\):\
    ;;\\([0-9]+\\):\\(\\([0-9]+\\)[: \t]\\)?" 1 2 4)

    ;; Cray C compiler error messages
    ("\\(cc\\| cft\\)-[0-9]+ c\\(c\\|f77\\): ERROR \\([^,\n]+, \\)* File = \
\\([^,\n]+\\), Line = \\([0-9]+\\)" 4 5)

    ;; IBM C/C++ Tools 2.01:
    ;;  foo.c(2:0) : informational EDC0804: Function foo is not referenced.
    ;;  foo.c(3:8) : warning EDC0833: Implicit return statement encountered.
    ;;  foo.c(5:5) : error EDC0350: Syntax error.
    ("\\([^( \n\t]+\\)(\\([0-9]+\\):\\([0-9]+\\)) : " 1 2 3)

    ;; IAR Systems C Compiler:
    ;;  "foo.c",3  Error[32]: Error message
    ;;  "foo.c",3  Warning[32]: Error message
    ("\"\\(.*\\)\",\\([0-9]+\\)\\s-+\\(Error\\|Warning\\)\\[[0-9]+\\]:" 1 2)

    ;; Sun ada (VADS, Solaris):
    ;;  /home3/xdhar/rcds_rc/main.a, line 361, char 6:syntax error: "," inserted
    ("\\([^, \n\t]+\\), line \\([0-9]+\\), char \\([0-9]+\\)[:., \(-]" 1 2 3)

    ;; Perl -w:
    ;; syntax error at automake line 922, near "':'"
    ;; Perl debugging traces
    ;; store::odrecall('File_A', 'x2') called at store.pm line 90
    (".* at \\([^ \n]+\\) line \\([0-9]+\\)[,.\n]" 1 2)

    ;; Oracle pro*c:
    ;; Semantic error at line 528, column 5, file erosacqdb.pc:
    ("Semantic error at line \\([0-9]+\\), column \\([0-9]+\\), file \\(.*\\):"
     3 1 2)

    ;; EPC F90 compiler:
    ;; Error 24 at (2:progran.f90) : syntax error
    ("Error [0-9]+ at (\\([0-9]*\\):\\([^)\n]+\\))" 2 1)

    ;; SGI IRIX MipsPro 7.3 compilers:
    ;; cc-1070 cc: ERROR File = linkl.c, Line = 38
    (".*: ERROR File = \\(.+\\), Line = \\([0-9]+\\)" 1 2)
    (".*: WARNING File = \\(.+\\), Line = \\([0-9]+\\)" 1 2)

    ;; Sun F90 error messages:
    ;; cf90-113 f90comp: ERROR NSE, File = Hoved.f90, Line = 16, Column = 3
    (".* ERROR [a-zA-Z0-9 ]+, File = \\(.+\\), Line = \\([0-9]+\\), Column = \\([0-9]+\\)"
     1 2 3)

    ;; RXP - GPL XML validator at http://www.cogsci.ed.ac.uk/~richard/rxp.html:
    ;; Error: Mismatched end tag: expected </geroup>, got </group>
    ;; in unnamed entity at line 71 char 8 of file:///home/reto/test/group.xml
    ("Error:.*\n.* line \\([0-9]+\\) char \\([0-9]+\\) of file://\\(.+\\)"
     3 1 2)
    ;; Warning: Start tag for undeclared element geroup
    ;; in unnamed entity at line 4 char 8 of file:///home/reto/test/group.xml
    ("Warning:.*\n.* line \\([0-9]+\\) char \\([0-9]+\\) of file://\\(.+\\)"
     3 1 2)
    )

  "Alist that specifies how to match errors in compiler output.
Each elt has the form (REGEXP FILE-IDX LINE-IDX [COLUMN-IDX FILE-FORMAT...])
If REGEXP matches, the FILE-IDX'th subexpression gives the file name, and
the LINE-IDX'th subexpression gives the line number.  If COLUMN-IDX is
given, the COLUMN-IDX'th subexpression gives the column number on that line.
If any FILE-FORMAT is given, each is a format string to produce a file name to
try; %s in the string is replaced by the text matching the FILE-IDX'th
subexpression.")

(defvar compilation-enter-directory-regexp-alist
  '(
    ;; Matches lines printed by the `-w' option of GNU Make.
    (".*: Entering directory `\\(.*\\)'$" 1)
    )
  "Alist specifying how to match lines that indicate a new current directory.
Note that the match is done at the beginning of lines.
Each elt has the form (REGEXP IDX).
If REGEXP matches, the IDX'th subexpression gives the directory name.

The default value matches lines printed by the `-w' option of GNU Make.")

(defvar compilation-leave-directory-regexp-alist
  '(
    ;; Matches lines printed by the `-w' option of GNU Make.
    (".*: Leaving directory `\\(.*\\)'$" 1)
    )
"Alist specifying how to match lines that indicate restoring current directory.
Note that the match is done at the beginning of lines.
Each elt has the form (REGEXP IDX).
If REGEXP matches, the IDX'th subexpression gives the name of the directory
being moved from.  If IDX is nil, the last directory entered \(by a line
matching `compilation-enter-directory-regexp-alist'\) is assumed.

The default value matches lines printed by the `-w' option of GNU Make.")

(defvar compilation-file-regexp-alist
  '(
    ;; This matches entries with date time year file-name: like
    ;; Thu May 14 10:46:12 1992  mom3.p:
    ("\\w\\w\\w \\w\\w\\w +[0-9]+ [0-9][0-9]:[0-9][0-9]:[0-9][0-9] [0-9][0-9][0-9][0-9]  \\(.*\\):$" 1)
    )
  "Alist specifying how to match lines that indicate a new current file.
Note that the match is done at the beginning of lines.
Each elt has the form (REGEXP IDX).
If REGEXP matches, the IDX'th subexpression gives the file name.  This is
used with compilers that don't indicate file name in every error message.")

;; There is no generally useful regexp that will match non messages, but
;; in special cases there might be one. The lines that are not matched by
;; a regexp take much longer time than the ones that are recognized so if
;; you have same regexeps here, parsing is faster.
(defvar compilation-nomessage-regexp-alist
  '(
    )
  "Alist specifying how to match lines that have no message.
Note that the match is done at the beginning of lines.
Each elt has the form (REGEXP).  This alist is by default empty, but if
you have some good regexps here, the parsing of messages will be faster.")

(defcustom compilation-error-screen-columns t
  "*If non-nil, column numbers in error messages are screen columns.
Otherwise they are interpreted as character positions, with
each character occupying one column.
The default is to use screen columns, which requires that the compilation
program and Emacs agree about the display width of the characters,
especially the TAB character."
  :type 'boolean
  :group 'compilation
  :version "20.4")

(defcustom compilation-read-command t
  "*Non-nil means \\[compile] reads the compilation command to use.
Otherwise, \\[compile] just uses the value of `compile-command'."
  :type 'boolean
  :group 'compilation)

;;;###autoload
(defcustom compilation-ask-about-save t
  "*Non-nil means \\[compile] asks which buffers to save before compiling.
Otherwise, it saves all modified buffers without asking."
  :type 'boolean
  :group 'compilation)

;; Note: the character class after the optional drive letter does not
;; include a space to support file names with blanks.
(defvar grep-regexp-alist
  '(("\\([a-zA-Z]?:?[^:(\t\n]+\\)[:( \t]+\\([0-9]+\\)[:) \t]" 1 2))
  "Regexp used to match grep hits.  See `compilation-error-regexp-alist'.")

(defvar grep-program
  ;; Currently zgrep has trouble.  It runs egrep instead of grep,
  ;; and it doesn't pass along long options right.
  "grep"
  ;; (if (equal (condition-case nil	; in case "zgrep" isn't in exec-path
  ;; 		 (call-process "zgrep" nil nil nil
  ;; 			       "foo" null-device)
  ;; 	       (error nil))
  ;; 	     1)
  ;;     "zgrep"
  ;;   "grep")
  "The default grep program for `grep-command' and `grep-find-command'.
This variable's value takes effect when `grep-compute-defaults' is called.")

(defvar find-program "find"
  "The default find program for `grep-find-command'.
This variable's value takes effect when `grep-compute-defaults' is called.")

(defvar grep-find-use-xargs nil
  "Whether \\[grep-find] uses the `xargs' utility by default.

If nil, it uses `grep -exec'; if `gnu', it uses `find -print0' and `xargs -0';
if not nil and not `gnu', it uses `find -print' and `xargs'.

This variable's value takes effect when `grep-compute-defaults' is called.")

;;;###autoload
(defcustom compilation-search-path '(nil)
  "*List of directories to search for source files named in error messages.
Elements should be directory names, not file names of directories.
nil as an element means to try the default directory."
  :type '(repeat (choice (const :tag "Default" nil)
			 (string :tag "Directory")))
  :group 'compilation)

(defcustom compile-command "make -k "
  "*Last shell command used to do a compilation; default for next compilation.

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

(defvar compilation-directory-stack nil
  "Stack of previous directories for `compilation-leave-directory-regexp'.
The last element is the directory the compilation was started in.")

(defvar compilation-exit-message-function nil "\
If non-nil, called when a compilation process dies to return a status message.
This should be a function of three arguments: process status, exit status,
and exit message; it returns a cons (MESSAGE . MODELINE) of the strings to
write into the compilation buffer, and to put in its mode line.")

;; History of compile commands.
(defvar compile-history nil)
;; History of grep commands.
(defvar grep-history nil)
(defvar grep-find-history nil)

(defun compilation-mode-font-lock-keywords ()
  "Return expressions to highlight in Compilation mode."
  (nconc
   ;;
   ;; Compiler warning/error lines.
   (mapcar (function
	    (lambda (item)
	      ;; Prepend "^", adjusting FILE-IDX and LINE-IDX accordingly.
	      (let ((file-idx (nth 1 item))
		    (line-idx (nth 2 item))
		    (col-idx (nth 3 item))
		    keyword)
		(when (numberp col-idx)
		  (setq keyword
			(cons (list (1+ col-idx) 'font-lock-type-face nil t)
			      keyword)))
		(when (numberp line-idx)
		  (setq keyword
			(cons (list (1+ line-idx) 'font-lock-variable-name-face)
			      keyword)))
		(when (numberp file-idx)
		  (setq keyword
			(cons (list (1+ file-idx) 'font-lock-warning-face)
			      keyword)))
		(cons (concat "^\\(" (nth 0 item) "\\)") keyword))))
	   compilation-error-regexp-alist)
   (list
    ;;
    ;; Compiler output lines.  Recognize `make[n]:' lines too.
    '("^\\([A-Za-z_0-9/\.+-]+\\)\\(\\[\\([0-9]+\\)\\]\\)?[ \t]*:"
      (1 font-lock-function-name-face) (3 font-lock-comment-face nil t)))
   ))

;;;###autoload
(defun compile (command)
  "Compile the program including the current buffer.  Default: run `make'.
Runs COMMAND, a shell command, in a separate process asynchronously
with output going to the buffer `*compilation*'.

You can then use the command \\[next-error] to find the next error message
and move to the source code that caused it.

Interactively, prompts for the command if `compilation-read-command' is
non-nil; otherwise uses `compile-command'.  With prefix arg, always prompts.

To run more than one compilation at once, start one and rename the
\`*compilation*' buffer to some other name with \\[rename-buffer].
Then start the next one.

The name used for the buffer is actually whatever is returned by
the function in `compilation-buffer-name-function', so you can set that
to a function that generates a unique name."
  (interactive
   (if (or compilation-read-command current-prefix-arg)
       (list (read-from-minibuffer "Compile command: "
                                 (eval compile-command) nil nil
                                 '(compile-history . 1)))
     (list (eval compile-command))))
  (unless (equal command (eval compile-command))
    (setq compile-command command))
  (save-some-buffers (not compilation-ask-about-save) nil)
  (compile-internal command "No more errors"))

;; run compile with the default command line
(defun recompile ()
  "Re-compile the program including the current buffer.
If this is run in a compilation-mode buffer, re-use the arguments from the
original use.  Otherwise, it recompiles using `compile-command'."
  (interactive)
  (save-some-buffers (not compilation-ask-about-save) nil)
  (apply 'compile-internal (or compilation-arguments
			      `(,(eval compile-command) "No more errors"))))

(defun grep-process-setup ()
  "Set up `compilation-exit-message-function' for `grep'."
  (set (make-local-variable 'compilation-exit-message-function)
       (lambda (status code msg)
	 (if (eq status 'exit)
	     (cond ((zerop code)
		    '("finished (matches found)\n" . "matched"))
		   ((= code 1)
		    '("finished with no matches found\n" . "no match"))
		   (t
		    (cons msg code)))
	   (cons msg code)))))

;;;###autoload
(defun grep (command-args)
  "Run grep, with user-specified args, and collect output in a buffer.
While grep runs asynchronously, you can use \\[next-error] (M-x next-error),
or \\<compilation-minor-mode-map>\\[compile-goto-error] in the grep \
output buffer, to go to the lines
where grep found matches.

This command uses a special history list for its COMMAND-ARGS, so you can
easily repeat a grep command.

A prefix argument says to default the argument based upon the current
tag the cursor is over, substituting it into the last grep command
in the grep command history (or into `grep-command'
if that history list is empty)."
  (interactive
   (let (grep-default (arg current-prefix-arg))
     (unless (and grep-command
		  (or (not grep-use-null-device) (eq grep-use-null-device t)))
       (grep-compute-defaults))
     (when arg
       (let ((tag-default
	      (funcall (or find-tag-default-function
			   (get major-mode 'find-tag-default-function)
			   ;; We use grep-tag-default instead of
			   ;; find-tag-default, to avoid loading etags.
			   'grep-tag-default))))
	 (setq grep-default (or (car grep-history) grep-command))
	 ;; Replace the thing matching for with that around cursor
	 (when (string-match "[^ ]+\\s +\\(-[^ ]+\\s +\\)*\\(\"[^\"]+\"\\|[^ ]+\\)\\(\\s-+\\S-+\\)?" grep-default)
	   (unless (or (match-beginning 3) (not (stringp buffer-file-name)))
	     (setq grep-default (concat grep-default "*."
					(file-name-extension buffer-file-name))))
	   (setq grep-default (replace-match (or tag-default "")
					     t t grep-default 2)))))
     (list (read-from-minibuffer "Run grep (like this): "
				 (or grep-default grep-command)
				 nil nil 'grep-history))))

  ;; Setting process-setup-function makes exit-message-function work
  ;; even when async processes aren't supported.
  (let* ((compilation-process-setup-function 'grep-process-setup)
	 (buf (compile-internal (if (and grep-use-null-device null-device)
				    (concat command-args " " null-device)
				  command-args)
				"No more grep hits" "grep"
				;; Give it a simpler regexp to match.
				nil grep-regexp-alist)))))

;; This is a copy of find-tag-default from etags.el.
(defun grep-tag-default ()
  (save-excursion
    (while (looking-at "\\sw\\|\\s_")
      (forward-char 1))
    (when (or (re-search-backward "\\sw\\|\\s_"
				  (save-excursion (beginning-of-line) (point))
				  t)
	      (re-search-forward "\\(\\sw\\|\\s_\\)+"
				 (save-excursion (end-of-line) (point))
				 t))
      (goto-char (match-end 0))
      (buffer-substring (point)
			(progn (forward-sexp -1)
			       (while (looking-at "\\s'")
				 (forward-char 1))
			       (point))))))

;;;###autoload
(defun grep-find (command-args)
  "Run grep via find, with user-specified args COMMAND-ARGS.
Collect output in a buffer.
While find runs asynchronously, you can use the \\[next-error] command
to find the text that grep hits refer to.

This command uses a special history list for its arguments, so you can
easily repeat a find command."
  (interactive
   (progn
     (unless grep-find-command
       (grep-compute-defaults))
     (list (read-from-minibuffer "Run find (like this): "
				 grep-find-command nil nil
				 'grep-find-history))))
  (let ((null-device nil))		; see grep
    (grep command-args)))

(defcustom compilation-scroll-output nil
  "*Non-nil to scroll the *compilation* buffer window as output appears.

Setting it causes the compilation-mode commands to put point at the
end of their output window so that the end of the output is always
visible rather than the begining."
  :type 'boolean
  :version "20.3"
  :group 'compilation)


(defun compilation-buffer-name (mode-name name-function)
  "Return the name of a compilation buffer to use.
If NAME-FUNCTION is non-nil, call it with one argument MODE-NAME
to determine the buffer name.
Likewise if `compilation-buffer-name-function' is non-nil.
If current buffer is in Compilation mode for the same mode name
return the name of the current buffer, so that it gets reused.
Otherwise, construct a buffer name from MODE-NAME."
  (cond (name-function 
	 (funcall name-function mode-name))
	(compilation-buffer-name-function 
	 (funcall compilation-buffer-name-function mode-name))
	((and (eq major-mode 'compilation-mode)
	      (equal mode-name (nth 2 compilation-arguments)))
	 (buffer-name))
	(t
	 (concat "*" (downcase mode-name) "*"))))


(defun compile-internal (command error-message
				 &optional name-of-mode parser
				 error-regexp-alist name-function
				 enter-regexp-alist leave-regexp-alist
				 file-regexp-alist nomessage-regexp-alist)
  "Run compilation command COMMAND (low level interface).
ERROR-MESSAGE is a string to print if the user asks to see another error
and there are no more errors.  The rest of the arguments, 3-10 are optional.
For them nil means use the default.
NAME-OF-MODE is the name to display as the major mode in the compilation
buffer.  PARSER is the error parser function.  ERROR-REGEXP-ALIST is the error
message regexp alist to use.  NAME-FUNCTION is a function called to name the
buffer.  ENTER-REGEXP-ALIST is the enter directory message regexp alist to use.
LEAVE-REGEXP-ALIST is the leave directory message regexp alist to use.
FILE-REGEXP-ALIST is the change current file message regexp alist to use.
NOMESSAGE-REGEXP-ALIST is the nomessage regexp alist to use.
  The defaults for these variables are the global values of
\`compilation-parse-errors-function', `compilation-error-regexp-alist',
\`compilation-buffer-name-function', `compilation-enter-directory-regexp-alist',
\`compilation-leave-directory-regexp-alist', `compilation-file-regexp-alist',
\ and `compilation-nomessage-regexp-alist', respectively.
For arg 7-10 a value `t' means an empty alist.

Returns the compilation buffer created."
  (let (outbuf)
    (save-excursion
      (or name-of-mode
	  (setq name-of-mode "Compilation"))
      (setq outbuf
	    (get-buffer-create (compilation-buffer-name name-of-mode
							name-function)))
      (set-buffer outbuf)
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
		     (buffer-name))
	      )))
      ;; In case the compilation buffer is current, make sure we get the global
      ;; values of compilation-error-regexp-alist, etc.
      (kill-all-local-variables))
    (or error-regexp-alist
	(setq error-regexp-alist compilation-error-regexp-alist))
    (or enter-regexp-alist
	(setq enter-regexp-alist compilation-enter-directory-regexp-alist))
    (or leave-regexp-alist
	(setq leave-regexp-alist compilation-leave-directory-regexp-alist))
    (or file-regexp-alist
	(setq file-regexp-alist compilation-file-regexp-alist))
    (or nomessage-regexp-alist
	(setq nomessage-regexp-alist compilation-nomessage-regexp-alist))
    (or parser (setq parser compilation-parse-errors-function))
    (let ((thisdir default-directory)
	  outwin)
      (save-excursion
	;; Clear out the compilation buffer and make it writable.
	;; Change its default-directory to the directory where the compilation
	;; will happen, and insert a `cd' command to indicate this.
	(set-buffer outbuf)
	(setq buffer-read-only nil)
	(buffer-disable-undo (current-buffer))
	(erase-buffer)
	(buffer-enable-undo (current-buffer))
	(setq default-directory thisdir)
	(insert "cd " thisdir "\n" command "\n")
	(set-buffer-modified-p nil))
      ;; If we're already in the compilation buffer, go to the end
      ;; of the buffer, so point will track the compilation output.
      (if (eq outbuf (current-buffer))
	  (goto-char (point-max)))
      ;; Pop up the compilation buffer.
      (setq outwin (display-buffer outbuf nil t))
      (save-excursion
	(set-buffer outbuf)
	(compilation-mode name-of-mode)
	;; In what way is it non-ergonomic ?  -stef
	;; (toggle-read-only 1) ;;; Non-ergonomic.
	(set (make-local-variable 'compilation-parse-errors-function) parser)
	(set (make-local-variable 'compilation-error-message) error-message)
	(set (make-local-variable 'compilation-error-regexp-alist)
	     error-regexp-alist)
	(set (make-local-variable 'compilation-enter-directory-regexp-alist)
	     enter-regexp-alist)
	(set (make-local-variable 'compilation-leave-directory-regexp-alist)
	     leave-regexp-alist)
	(set (make-local-variable 'compilation-file-regexp-alist)
	     file-regexp-alist)
	(set (make-local-variable 'compilation-nomessage-regexp-alist)
	     nomessage-regexp-alist)
	(set (make-local-variable 'compilation-arguments)
	     (list command error-message
		   name-of-mode parser
		   error-regexp-alist name-function
		   enter-regexp-alist leave-regexp-alist
		   file-regexp-alist nomessage-regexp-alist))
        ;; This proves a good idea if the buffer's going to scroll
        ;; with lazy-lock on.
        (set (make-local-variable 'lazy-lock-defer-on-scrolling) t)
	(setq default-directory thisdir
	      compilation-directory-stack (list default-directory))
	(set-window-start outwin (point-min))
	(or (eq outwin (selected-window))
	    (set-window-point outwin (point-min)))
	(compilation-set-window-height outwin)
	(if compilation-process-setup-function
	    (funcall compilation-process-setup-function))
	;; Start the compilation.
	(if (fboundp 'start-process)
 	    (let* ((process-environment
		    (append
		     (if (and (boundp 'system-uses-terminfo)
			      system-uses-terminfo)
			 (list "TERM=dumb" "TERMCAP="
			       (format "COLUMNS=%d" (window-width)))
		       (list "TERM=emacs"
			     (format "TERMCAP=emacs:co#%d:tc=unknown:"
				     (window-width))))
		     ;; Set the EMACS variable, but
		     ;; don't override users' setting of $EMACS.
		     (if (getenv "EMACS")
			 process-environment
		       (cons "EMACS=t" process-environment))))
		   (proc (start-process-shell-command (downcase mode-name)
						      outbuf
						      command)))
	      (set-process-sentinel proc 'compilation-sentinel)
	      (set-process-filter proc 'compilation-filter)
	      (set-marker (process-mark proc) (point) outbuf)
	      (setq compilation-in-progress
		    (cons proc compilation-in-progress)))
	  ;; No asynchronous processes available.
	  (message "Executing `%s'..." command)
	  ;; Fake modeline display as if `start-process' were run.
	  (setq mode-line-process ":run")
	  (force-mode-line-update)
	  (sit-for 0)			; Force redisplay
	  (let ((status (call-process shell-file-name nil outbuf nil "-c"
				      command)))
	    (cond ((numberp status)
		   (compilation-handle-exit 'exit status
					    (if (zerop status)
						"finished\n"
					      (format "\
exited abnormally with code %d\n"
						      status))))
		  ((stringp status)
		   (compilation-handle-exit 'signal status
					    (concat status "\n")))
		  (t
		   (compilation-handle-exit 'bizarre status status))))
	  (message "Executing `%s'...done" command)))
      (if compilation-scroll-output
	  (save-selected-window
            (select-window outwin)
            (goto-char (point-max)))))
    ;; Make it so the next C-x ` will use this buffer.
    (setq compilation-last-buffer outbuf)))

(defun compilation-set-window-height (window)
  "Set the height of WINDOW according to `compilation-window-height'."
  (and compilation-window-height
       (= (window-width window) (frame-width (window-frame window)))
       ;; If window is alone in its frame, aside from a minibuffer,
       ;; don't change its height.
       (not (eq window (frame-root-window (window-frame window))))
       ;; This save-excursion prevents us from changing the current buffer,
       ;; which might not be the same as the selected window's buffer.
       (save-excursion
	 (let ((w (selected-window)))
	   (unwind-protect
	       (progn
		 (select-window window)
		 (enlarge-window (- compilation-window-height
				    (window-height))))
	     ;; The enlarge-window above may have deleted W, if
	     ;; compilation-window-height is large enough.
	     (when (window-live-p w)
	       (select-window w)))))))

(defvar compilation-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-2] 'compile-mouse-goto-error)
    (define-key map "\C-c\C-c" 'compile-goto-error)
    (define-key map "\C-m" 'compile-goto-error)
    (define-key map "\C-c\C-k" 'kill-compilation)
    (define-key map "\M-n" 'compilation-next-error)
    (define-key map "\M-p" 'compilation-previous-error)
    (define-key map "\M-{" 'compilation-previous-file)
    (define-key map "\M-}" 'compilation-next-file)
    map)
  "Keymap for `compilation-minor-mode'.")

(defvar compilation-shell-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-2] 'compile-mouse-goto-error)
    (define-key map "\M-\C-m" 'compile-goto-error)
    (define-key map "\M-\C-n" 'compilation-next-error)
    (define-key map "\M-\C-p" 'compilation-previous-error)
    (define-key map "\M-{" 'compilation-previous-file)
    (define-key map "\M-}" 'compilation-next-file)
    ;; Set up the menu-bar
    (define-key map [menu-bar errors-menu]
      (cons "Errors" (make-sparse-keymap "Errors")))
    (define-key map [menu-bar errors-menu stop-subjob]
      '("Stop" . comint-interrupt-subjob))
    (define-key map [menu-bar errors-menu compilation-mode-separator2]
      '("----" . nil))
    (define-key map [menu-bar errors-menu compilation-mode-first-error]
      '("First Error" . first-error))
    (define-key map [menu-bar errors-menu compilation-mode-previous-error]
      '("Previous Error" . previous-error))
    (define-key map [menu-bar errors-menu compilation-mode-next-error]
      '("Next Error" . next-error))
    map)
  "Keymap for `compilation-shell-minor-mode'.")

(defvar compilation-mode-map
  (let ((map (cons 'keymap compilation-minor-mode-map)))
    (define-key map " " 'scroll-up)
    (define-key map "\^?" 'scroll-down)
    ;; Set up the menu-bar
    (define-key map [menu-bar compilation-menu]
      (cons "Compile" (make-sparse-keymap "Compile")))

    (define-key map [menu-bar compilation-menu compilation-mode-kill-compilation]
      '("Stop Compilation" . kill-compilation))
    (define-key map [menu-bar compilation-menu compilation-mode-separator2]
      '("----" . nil))
    (define-key map [menu-bar compilation-menu compilation-mode-first-error]
      '("First Error" . first-error))
    (define-key map [menu-bar compilation-menu compilation-mode-previous-error]
      '("Previous Error" . previous-error))
    (define-key map [menu-bar compilation-menu compilation-mode-next-error]
      '("Next Error" . next-error))
    (define-key map [menu-bar compilation-menu compilation-separator2]
      '("----" . nil))
    (define-key map [menu-bar compilation-menu compilation-mode-grep]
      '("Search Files (grep)" . grep))
    (define-key map [menu-bar compilation-menu compilation-mode-recompile]
      '("Recompile" . recompile))
    (define-key map [menu-bar compilation-menu compilation-mode-compile]
      '("Compile..." . compile))
    map)
  "Keymap for compilation log buffers.
`compilation-minor-mode-map' is a cdr of this.")

(put 'compilation-mode 'mode-class 'special)

;;;###autoload
(defun compilation-mode (&optional name-of-mode)
  "Major mode for compilation log buffers.
\\<compilation-mode-map>To visit the source for a line-numbered error,
move point to the error message line and type \\[compile-goto-error].
To kill the compilation, type \\[kill-compilation].

Runs `compilation-mode-hook' with `run-hooks' (which see)."
  (interactive)
  (kill-all-local-variables)
  (use-local-map compilation-mode-map)
  (setq major-mode 'compilation-mode
	mode-name (or name-of-mode "Compilation"))
  (compilation-setup)
  (set (make-local-variable 'font-lock-defaults)
       '(compilation-mode-font-lock-keywords t))
  (set (make-local-variable 'revert-buffer-function)
       'compilation-revert-buffer)
  (run-hooks 'compilation-mode-hook))

(defun compilation-revert-buffer (ignore-auto noconfirm)
  (if (or noconfirm (yes-or-no-p (format "Restart compilation? ")))
      (apply 'compile-internal compilation-arguments)))

(defun compilation-setup ()
  "Prepare the buffer for the compilation parsing commands to work."
  ;; Make the buffer's mode line show process state.
  (setq mode-line-process '(":%s"))
  (set (make-local-variable 'compilation-error-list) nil)
  (set (make-local-variable 'compilation-old-error-list) nil)
  (set (make-local-variable 'compilation-parsing-end) (copy-marker 1))
  (set (make-local-variable 'compilation-directory-stack)
       (list default-directory))
  (make-local-variable 'compilation-error-screen-columns)
  (setq compilation-last-buffer (current-buffer)))

(defvar compilation-shell-minor-mode nil
  "Non-nil when in `compilation-shell-minor-mode'.
In this minor mode, all the error-parsing commands of the
Compilation major mode are available but bound to keys that don't
collide with Shell mode.")
(make-variable-buffer-local 'compilation-shell-minor-mode)

(or (assq 'compilation-shell-minor-mode minor-mode-alist)
    (setq minor-mode-alist
	  (cons '(compilation-shell-minor-mode " Shell-Compile")
		minor-mode-alist)))
(or (assq 'compilation-shell-minor-mode minor-mode-map-alist)
    (setq minor-mode-map-alist (cons (cons 'compilation-shell-minor-mode
					   compilation-shell-minor-mode-map)
				     minor-mode-map-alist)))

(defvar compilation-minor-mode nil
  "Non-nil when in `compilation-minor-mode'.
In this minor mode, all the error-parsing commands of the
Compilation major mode are available.")
(make-variable-buffer-local 'compilation-minor-mode)

(or (assq 'compilation-minor-mode minor-mode-alist)
    (setq minor-mode-alist (cons '(compilation-minor-mode " Compilation")
				 minor-mode-alist)))
(or (assq 'compilation-minor-mode minor-mode-map-alist)
    (setq minor-mode-map-alist (cons (cons 'compilation-minor-mode
					   compilation-minor-mode-map)
				     minor-mode-map-alist)))

;;;###autoload
(defun compilation-shell-minor-mode (&optional arg)
  "Toggle compilation shell minor mode.
With arg, turn compilation mode on if and only if arg is positive.
See `compilation-mode'.
Turning the mode on runs the normal hook `compilation-shell-minor-mode-hook'."
  (interactive "P")
  (if (setq compilation-shell-minor-mode (if (null arg)
				       (null compilation-shell-minor-mode)
				     (> (prefix-numeric-value arg) 0)))
      (let ((mode-line-process))
	(compilation-setup)
	(run-hooks 'compilation-shell-minor-mode-hook))))

;;;###autoload
(defun compilation-minor-mode (&optional arg)
  "Toggle compilation minor mode.
With arg, turn compilation mode on if and only if arg is positive.
See `compilation-mode'.
Turning the mode on runs the normal hook `compilation-minor-mode-hook'."
  (interactive "P")
  (if (setq compilation-minor-mode (if (null arg)
				       (null compilation-minor-mode)
				     (> (prefix-numeric-value arg) 0)))
      (let ((mode-line-process))
	(compilation-setup)
	(run-hooks 'compilation-minor-mode-hook))))

(defun compilation-handle-exit (process-status exit-status msg)
  "Write msg in the current buffer and hack its mode-line-process."
  (let ((buffer-read-only nil)
	(status (if compilation-exit-message-function
		    (funcall compilation-exit-message-function
			     process-status exit-status msg)
		  (cons msg exit-status)))
	(omax (point-max))
	(opoint (point)))
    ;; Record where we put the message, so we can ignore it
    ;; later on.
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
    (setq mode-line-process (format ":%s [%s]" process-status (cdr status)))
    ;; Force mode line redisplay soon.
    (force-mode-line-update)
    (if (and opoint (< opoint omax))
	(goto-char opoint))
    ;; Automatically parse (and mouse-highlight) error messages:
    (cond ((eq compile-auto-highlight t)
	   (compile-reinitialize-errors nil (point-max)))
	  ((numberp compile-auto-highlight)
	   (compile-reinitialize-errors nil
					(save-excursion
					  (goto-line compile-auto-highlight)
					  (point)))))
    (if compilation-finish-function
	(funcall compilation-finish-function (current-buffer) msg))
    (let ((functions compilation-finish-functions))
      (while functions
	(funcall (car functions) (current-buffer) msg)
	(setq functions (cdr functions))))))

;; Called when compilation process changes state.
(defun compilation-sentinel (proc msg)
  "Sentinel for compilation buffers."
  (let ((buffer (process-buffer proc)))
    (if (memq (process-status proc) '(signal exit))
	(progn
	  (if (null (buffer-name buffer))
	      ;; buffer killed
	      (set-process-buffer proc nil)
	    (let ((obuf (current-buffer)))
	      ;; save-excursion isn't the right thing if
	      ;; process-buffer is current-buffer
	      (unwind-protect
		  (progn
		    ;; Write something in the compilation buffer
		    ;; and hack its mode line.
		    (set-buffer buffer)
		    (compilation-handle-exit (process-status proc)
					     (process-exit-status proc)
					     msg)
		    ;; Since the buffer and mode line will show that the
		    ;; process is dead, we can delete it now.  Otherwise it
		    ;; will stay around until M-x list-processes.
		    (delete-process proc))
		(set-buffer obuf))))
	  (setq compilation-in-progress (delq proc compilation-in-progress))
	  ))))

(defun compilation-filter (proc string)
  "Process filter for compilation buffers.
Just inserts the text, but uses `insert-before-markers'."
  (if (buffer-name (process-buffer proc))
      (save-excursion
	(set-buffer (process-buffer proc))
	(let ((buffer-read-only nil)
	      (end (marker-position compilation-parsing-end)))
	  (save-excursion
	    (goto-char (process-mark proc))
	    (insert-before-markers string)
	    (set-marker compilation-parsing-end end) ;don't move it
	    (run-hooks 'compilation-filter-hook)
	    ;; this seems redundant since we insert-before-marks   -stefan
	    ;;(set-marker (process-mark proc) (point))
	    )))))

(defun compile-error-at-point ()
  "Return the cdr of `compilation-old-error-list' for error containing point."
  (compile-reinitialize-errors nil (point))
  (let ((errors compilation-old-error-list))
    (while (and errors
		(> (point) (car (car errors))))
      (setq errors (cdr errors)))
    errors))

(defsubst compilation-buffer-p (buffer)
  (save-excursion
    (set-buffer buffer)
    (or compilation-shell-minor-mode compilation-minor-mode
	(eq major-mode 'compilation-mode))))

(defun compilation-next-error (n)
  "Move point to the next error in the compilation buffer.
Prefix arg N says how many error messages to move forwards (or
backwards, if negative).
Does NOT find the source line like \\[next-error]."
  (interactive "p")
  (or (compilation-buffer-p (current-buffer))
      (error "Not in a compilation buffer"))
  (setq compilation-last-buffer (current-buffer))

  (let ((errors (compile-error-at-point)))

    ;; Move to the error after the one containing point.
    (goto-char (car (if (< n 0)
			(let ((i 0)
			      (e compilation-old-error-list))
			  ;; See how many cdrs away ERRORS is from the start.
			  (while (not (eq e errors))
			    (setq i (1+ i)
				  e (cdr e)))
			  (if (> (- n) i)
			      (error "Moved back past first error")
			    (nth (+ i n) compilation-old-error-list)))
		      (let ((compilation-error-list (cdr errors)))
			(compile-reinitialize-errors nil nil n)
			(if compilation-error-list
			    (nth (1- n) compilation-error-list)
			  (error "Moved past last error"))))))))

(defun compilation-previous-error (n)
  "Move point to the previous error in the compilation buffer.
Prefix arg N says how many error messages to move backwards (or
forwards, if negative).
Does NOT find the source line like \\[next-error]."
  (interactive "p")
  (compilation-next-error (- n)))


;; Given an elt of `compilation-error-list', return an object representing
;; the referenced file which is equal to (but not necessarily eq to) what
;; this function would return for another error in the same file.
(defsubst compilation-error-filedata (data)
  (setq data (cdr data))
  (if (markerp data)
      (marker-buffer data)
    (car data)))

;; Return a string describing a value from compilation-error-filedata.
;; This value is not necessarily useful as a file name, but should be
;; indicative to the user of what file's errors are being referred to.
(defsubst compilation-error-filedata-file-name (filedata)
  (if (bufferp filedata)
      (buffer-file-name filedata)
    (car filedata)))

(defun compilation-next-file (n)
  "Move point to the next error for a different file than the current one."
  (interactive "p")
  (or (compilation-buffer-p (current-buffer))
      (error "Not in a compilation buffer"))
  (setq compilation-last-buffer (current-buffer))

  (let ((reversed (< n 0))
	errors filedata)

    (if (not reversed)
	(setq errors (or (compile-error-at-point)
			 (error "Moved past last error")))

      ;; Get a reversed list of the errors up through the one containing point.
      (compile-reinitialize-errors nil (point))
      (setq errors (reverse compilation-old-error-list)
	    n (- n))

      ;; Ignore errors after point.  (car ERRORS) will be the error
      ;; containing point, (cadr ERRORS) the one before it.
      (while (and errors
		  (< (point) (car (car errors))))
	(setq errors (cdr errors))))

    (while (> n 0)
      (setq filedata (compilation-error-filedata (car errors)))

      ;; Skip past the following errors for this file.
      (while (equal filedata
		    (compilation-error-filedata
		     (car (or errors
			      (if reversed
				  (error "%s the first erring file"
					 (compilation-error-filedata-file-name
					  filedata))
				(let ((compilation-error-list nil))
				  ;; Parse some more.
				  (compile-reinitialize-errors nil nil 2)
				  (setq errors compilation-error-list)))
			      (error "%s is the last erring file"
				     (compilation-error-filedata-file-name
				      filedata))))))
	(setq errors (cdr errors)))

      (setq n (1- n)))

    ;; Move to the following error.
    (goto-char (car (car (or errors
			     (if reversed
				 (error "This is the first erring file")
			       (let ((compilation-error-list nil))
				 ;; Parse the last one.
				 (compile-reinitialize-errors nil nil 1)
				 compilation-error-list))))))))

(defun compilation-previous-file (n)
  "Move point to the previous error for a different file than the current one."
  (interactive "p")
  (compilation-next-file (- n)))


(defun kill-compilation ()
  "Kill the process made by the \\[compile] command."
  (interactive)
  (let ((buffer (compilation-find-buffer)))
    (if (get-buffer-process buffer)
	(interrupt-process (get-buffer-process buffer))
      (error "The compilation process is not running"))))


;; Parse any new errors in the compilation buffer,
;; or reparse from the beginning if the user has asked for that.
(defun compile-reinitialize-errors (reparse
				    &optional limit-search find-at-least)
  (save-excursion
    (set-buffer compilation-last-buffer)
    ;; If we are out of errors, or if user says "reparse",
    ;; discard the info we have, to force reparsing.
    (if (or (eq compilation-error-list t)
	    reparse)
	(compilation-forget-errors))
    (if (and compilation-error-list
	     (or (not limit-search)
		 (> compilation-parsing-end limit-search))
	     (or (not find-at-least)
		 (>= (length compilation-error-list) find-at-least)))
	;; Since compilation-error-list is non-nil, it points to a specific
	;; error the user wanted.  So don't move it around.
	nil
      ;; This was here for a long time (before my rewrite); why? --roland
      ;;(switch-to-buffer compilation-last-buffer)
      (set-buffer-modified-p nil)
      (if (< compilation-parsing-end (point-max))
	  ;; compilation-error-list might be non-nil if we have a non-nil
	  ;; LIMIT-SEARCH or FIND-AT-LEAST arg.  In that case its value
	  ;; records the current position in the error list, and we must
	  ;; preserve that after reparsing.
	  (let ((error-list-pos compilation-error-list))
	    (funcall compilation-parse-errors-function
		     limit-search
		     (and find-at-least
			  ;; We only need enough new parsed errors to reach
			  ;; FIND-AT-LEAST errors past the current
			  ;; position.
			  (- find-at-least (length compilation-error-list))))
	    ;; Remember the entire list for compilation-forget-errors.  If
	    ;; this is an incremental parse, append to previous list.  If
	    ;; we are parsing anew, compilation-forget-errors cleared
	    ;; compilation-old-error-list above.
	    (setq compilation-old-error-list
		  (nconc compilation-old-error-list compilation-error-list))
	    (if error-list-pos
		;; We started in the middle of an existing list of parsed
		;; errors before parsing more; restore that position.
		(setq compilation-error-list error-list-pos))
	    ;; Mouse-Highlight (the first line of) each error message when the
	    ;; mouse pointer moves over it:
	    (let ((inhibit-read-only t)
		  (buffer-undo-list t)
		  deactivate-mark
		  (error-list compilation-error-list))
	      (while error-list
		(save-excursion
		  (add-text-properties (goto-char (car (car error-list)))
				       (progn (end-of-line) (point))
				       '(mouse-face highlight help-echo "\
mouse-2: visit this file and line")))
		(setq error-list (cdr error-list))))
	    )))))

(defun compile-mouse-goto-error (event)
  "Visit the source for the error message the mouse is pointing at.
This is like `compile-goto-error' called without prefix arg
at the end of the line."
  (interactive "e")
  (save-excursion
    (set-buffer (window-buffer (posn-window (event-end event))))
    (goto-char (posn-point (event-end event)))

    (or (compilation-buffer-p (current-buffer))
	(error "Not in a compilation buffer"))
    (setq compilation-last-buffer (current-buffer))
    ;; `compile-reinitialize-errors' needs to see the complete filename
    ;; on the line where they clicked the mouse.  Since it only looks
    ;; up to point, moving point to eol makes sure the filename is
    ;; visible to `compile-reinitialize-errors'.
    (end-of-line)
    (compile-reinitialize-errors nil (point))

    ;; Move to bol; the marker for the error on this line will point there.
    (beginning-of-line)

    ;; Move compilation-error-list to the elt of compilation-old-error-list
    ;; we want.
    (setq compilation-error-list compilation-old-error-list)
    (while (and compilation-error-list
		;; The marker can point nowhere if we previously
		;; failed to find the relevant file.  See
		;; compilation-next-error-locus.
		(or (null (marker-buffer (caar compilation-error-list)))
		    (and (> (point) (caar compilation-error-list))
			 (>= (point)
			     ;; Don't skip too far: the text between
			     ;; two errors belongs to the first.  This
			     ;; in-between text might be other errors
			     ;; on the same line (see
			     ;; compilation-skip-to-next-location).
			     (if (null (cdr compilation-error-list))
				 compilation-parsing-end
			       (caar (cdr compilation-error-list)))))))
      (setq compilation-error-list (cdr compilation-error-list)))
    (or compilation-error-list
	(error "No error to go to")))
  (select-window (posn-window (event-end event)))

  (push-mark)
  (next-error 1))

(defun compile-goto-error (&optional argp)
  "Visit the source for the error message point is on.
Use this command in a compilation log buffer.  Sets the mark at point there.
\\[universal-argument] as a prefix arg means to reparse the buffer's error messages first;
other kinds of prefix arguments are ignored."
  (interactive "P")
  (or (compilation-buffer-p (current-buffer))
      (error "Not in a compilation buffer"))
  (setq compilation-last-buffer (current-buffer))
  (compile-reinitialize-errors (consp argp) (point))

  ;; Move to bol; the marker for the error on this line will point there.
  (beginning-of-line)

  ;; Move compilation-error-list to the elt of compilation-old-error-list
  ;; we want.
  (setq compilation-error-list compilation-old-error-list)
  (while (and compilation-error-list
	      ;; The marker can point nowhere if we previously
	      ;; failed to find the relevant file.  See
	      ;; compilation-next-error-locus.
	      (or (null (marker-buffer (caar compilation-error-list)))
		  (and (> (point) (caar compilation-error-list))
		       (>= (point)
			   ;; Don't skip too far: the text between
			   ;; two errors belongs to the first.  This
			   ;; in-between text might be other errors
			   ;; on the same line (see
			   ;; compilation-skip-to-next-location).
			   (if (null (cdr compilation-error-list))
			       compilation-parsing-end
			     (caar (cdr compilation-error-list)))))))
    (setq compilation-error-list (cdr compilation-error-list)))

  (push-mark)
  (next-error 1))

;; Return a compilation buffer.
;; If the current buffer is a compilation buffer, return it.
;; If compilation-last-buffer is set to a live buffer, use that.
;; Otherwise, look for a compilation buffer and signal an error
;; if there are none.
(defun compilation-find-buffer (&optional other-buffer)
  (if (and (not other-buffer)
	   (compilation-buffer-p (current-buffer)))
      ;; The current buffer is a compilation buffer.
      (current-buffer)
    (if (and compilation-last-buffer (buffer-name compilation-last-buffer)
	     (compilation-buffer-p compilation-last-buffer)
	     (or (not other-buffer) (not (eq compilation-last-buffer
					     (current-buffer)))))
	compilation-last-buffer
      (let ((buffers (buffer-list)))
	(while (and buffers (or (not (compilation-buffer-p (car buffers)))
				(and other-buffer
				     (eq (car buffers) (current-buffer)))))
	  (setq buffers (cdr buffers)))
	(if buffers
	    (car buffers)
	  (or (and other-buffer
		   (compilation-buffer-p (current-buffer))
		   ;; The current buffer is a compilation buffer.
		   (progn
		     (if other-buffer
			 (message "This is the only compilation buffer."))
		     (current-buffer)))
	      (error "No compilation started!")))))))

;;;###autoload
(defun next-error (&optional argp)
  "Visit next compilation error message and corresponding source code.

If all the error messages parsed so far have been processed already,
the message buffer is checked for new ones.

A prefix ARGP specifies how many error messages to move;
negative means move back to previous error messages.
Just \\[universal-argument] as a prefix means reparse the error message buffer
and start at the first error.

\\[next-error] normally uses the most recently started compilation or
grep buffer.  However, it can operate on any buffer with output from
the \\[compile] and \\[grep] commands, or, more generally, on any
buffer in Compilation mode or with Compilation Minor mode enabled.  To
specify use of a particular buffer for error messages, type
\\[next-error] in that buffer.

Once \\[next-error] has chosen the buffer for error messages,
it stays with that buffer until you use it in some other buffer which
uses Compilation mode or Compilation Minor mode.

See variables `compilation-parse-errors-function' and
\`compilation-error-regexp-alist' for customization ideas."
  (interactive "P")
  (setq compilation-last-buffer (compilation-find-buffer))
  (compilation-goto-locus (compilation-next-error-locus
			   ;; We want to pass a number here only if
			   ;; we got a numeric prefix arg, not just C-u.
			   (and (not (consp argp))
				(prefix-numeric-value argp))
			   (consp argp))))
;;;###autoload (define-key ctl-x-map "`" 'next-error)

(defun previous-error ()
  "Visit previous compilation error message and corresponding source code.
This operates on the output from the \\[compile] command."
  (interactive)
  (next-error -1))

(defun first-error ()
  "Reparse the error message buffer and start at the first error.
Visit corresponding source code.
This operates on the output from the \\[compile] command."
  (interactive)
  (next-error '(4)))

(defvar compilation-skip-to-next-location nil
  "*If non-nil, skip multiple error messages for the same source location.")

(defun compilation-next-error-locus (&optional move reparse silent)
  "Visit next compilation error and return locus in corresponding source code.
This operates on the output from the \\[compile] command.
If all preparsed error messages have been processed,
the error message buffer is checked for new ones.

Returns a cons (ERROR . SOURCE) of two markers: ERROR is a marker at the
location of the error message in the compilation buffer, and SOURCE is a
marker at the location in the source code indicated by the error message.

Optional first arg MOVE says how many error messages to move forwards (or
backwards, if negative); default is 1.  Optional second arg REPARSE, if
non-nil, says to reparse the error message buffer and reset to the first
error (plus MOVE - 1).  If optional third argument SILENT is non-nil, return
nil instead of raising an error if there are no more errors.

The current buffer should be the desired compilation output buffer."
  (or move (setq move 1))
  (compile-reinitialize-errors reparse nil (and (not reparse) (max 0 move)))
  (let (next-errors next-error)
    (catch 'no-next-error
      (save-excursion
	(set-buffer compilation-last-buffer)
	;; compilation-error-list points to the "current" error.
	(setq next-errors
	      (if (> move 0)
		  (nthcdr (1- move)
			  compilation-error-list)
		;; Zero or negative arg; we need to move back in the list.
		(let ((n (1- move))
		      (i 0)
		      (e compilation-old-error-list))
		  ;; See how many cdrs away the current error is from the start.
		  (while (not (eq e compilation-error-list))
		    (setq i (1+ i)
			  e (cdr e)))
		  (if (> (- n) i)
		      (error "Moved back past first error")
		    (nthcdr (+ i n) compilation-old-error-list))))
	      next-error (car next-errors))
	(while
	    (if (null next-error)
		(progn
		  (and move (/= move 1)
		       (error (if (> move 0)
				  "Moved past last error")
			      "Moved back past first error"))
		  ;; Forget existing error messages if compilation has finished.
		  (if (not (and (get-buffer-process (current-buffer))
				(eq (process-status
				     (get-buffer-process
				      (current-buffer)))
				    'run)))
		      (compilation-forget-errors))
		  (if silent
		      (throw 'no-next-error nil)
		    (error (concat compilation-error-message
				   (and (get-buffer-process (current-buffer))
					(eq (process-status
					     (get-buffer-process
					      (current-buffer)))
					    'run)
					" yet")))))
	      (setq compilation-error-list (cdr next-errors))
	      (if (null (cdr next-error))
		  ;; This error is boring.  Go to the next.
		  t
		(or (markerp (cdr next-error))
		    ;; This error has a filename/lineno pair.
		    ;; Find the file and turn it into a marker.
		    (let* ((fileinfo (car (cdr next-error)))
			   (buffer (apply 'compilation-find-file
					  (car next-error) fileinfo)))
		      (if (null buffer)
			  ;; We can't find this error's file.
			  ;; Remove all errors in the same file.
			  (progn
			    (setq next-errors compilation-old-error-list)
			    (while next-errors
			      (and (consp (cdr (car next-errors)))
				   (equal (car (cdr (car next-errors)))
					  fileinfo)
				   (progn
				     (set-marker (car (car next-errors)) nil)
				     (setcdr (car next-errors) nil)))
			      (setq next-errors (cdr next-errors)))
			    ;; Look for the next error.
			    t)
			;; We found the file.  Get a marker for this error.
			;; compilation-old-error-list and
			;; compilation-error-screen-columns are buffer-local
			;; so we must be careful to extract their value
			;; before switching to the source file buffer.
			(let ((errors compilation-old-error-list)
			      (columns compilation-error-screen-columns)
			      (last-line (nth 1 (cdr next-error)))
			      (column (nth 2 (cdr next-error))))
			  (set-buffer buffer)
			  (save-excursion
			    (save-restriction
			      (widen)
			      (goto-line last-line)
			      (if (and column (> column 0))
				  ;; Columns in error msgs are 1-origin.
				  (if columns
				      (move-to-column (1- column))
				    (forward-char (1- column)))
				(beginning-of-line))
			      (setcdr next-error (point-marker))
			      ;; Make all the other error messages referring
			      ;; to the same file have markers into the buffer.
			      (while errors
				(and (consp (cdr (car errors)))
				     (equal (car (cdr (car errors))) fileinfo)
				     (let* ((this (nth 1 (cdr (car errors))))
					    (column (nth 2 (cdr (car errors))))
					    (lines (- this last-line)))
				       (if (eq selective-display t)
					   ;; When selective-display is t,
					   ;; each C-m is a line boundary,
					   ;; as well as each newline.
					   (if (< lines 0)
					       (re-search-backward "[\n\C-m]"
								   nil 'end
								   (- lines))
					     (re-search-forward "[\n\C-m]"
								nil 'end
								lines))
					 (forward-line lines))
				       (if (and column (> column 1))
					   (if columns
					       (move-to-column (1- column))
					     (forward-char (1- column)))
					 (beginning-of-line))
				       (setq last-line this)
				       (setcdr (car errors) (point-marker))))
				(setq errors (cdr errors)))))))))
		;; If we didn't get a marker for this error, or this
		;; marker's buffer was killed, go on to the next one.
		(or (not (markerp (cdr next-error)))
		    (not (marker-buffer (cdr next-error))))))
	  (setq next-errors compilation-error-list
		next-error (car next-errors)))))

    (if compilation-skip-to-next-location
	;; Skip over multiple error messages for the same source location,
	;; so the next C-x ` won't go to an error in the same place.
	(while (and compilation-error-list
		    (equal (cdr (car compilation-error-list))
			   (cdr next-error)))
	  (setq compilation-error-list (cdr compilation-error-list))))

    ;; We now have a marker for the position of the error source code.
    ;; NEXT-ERROR is a cons (ERROR . SOURCE) of two markers.
    next-error))

(defun compilation-goto-locus (next-error)
  "Jump to an error locus returned by `compilation-next-error-locus'.
Takes one argument, a cons (ERROR . SOURCE) of two markers.
Selects a window with point at SOURCE, with another window displaying ERROR."
  (if (eq (window-buffer (selected-window))
	  (marker-buffer (car next-error)))
      ;; If the compilation buffer window is selected,
      ;; keep the compilation buffer in this window;
      ;; display the source in another window.
      (let ((pop-up-windows t))
	(pop-to-buffer (marker-buffer (cdr next-error))))
    (if (window-dedicated-p (selected-window))
	(pop-to-buffer (marker-buffer (cdr next-error)))
      (switch-to-buffer (marker-buffer (cdr next-error)))))
  (goto-char (cdr next-error))
  ;; If narrowing got in the way of
  ;; going to the right place, widen.
  (or (= (point) (marker-position (cdr next-error)))
      (progn
	(widen)
	(goto-char (cdr next-error))))
  ;; If hideshow got in the way of
  ;; seeing the right place, open permanently.
  (mapcar (function (lambda (ov)
		      (when (eq 'hs (overlay-get ov 'invisible))
			(delete-overlay ov)
			(goto-char (cdr next-error)))))
	  (overlays-at (point)))

  ;; Show compilation buffer in other window, scrolled to this error.
  (let* ((pop-up-windows t)
	 ;; Use an existing window if it is in a visible frame.
	 (w (or (get-buffer-window (marker-buffer (car next-error)) 'visible)
		;; Pop up a window.
		(display-buffer (marker-buffer (car next-error))))))
    (set-window-point w (car next-error))
    (set-window-start w (car next-error))
    (compilation-set-window-height w)))

(defun compilation-find-file (marker filename dir &rest formats)
  "Find a buffer for file FILENAME.
Search the directories in `compilation-search-path'.
A nil in `compilation-search-path' means to try the
current directory, which is passed in DIR.
If FILENAME is not found at all, ask the user where to find it.
Pop up the buffer containing MARKER and scroll to MARKER if we ask the user."
  (or formats (setq formats '("%s")))
  (save-excursion
    (let ((dirs compilation-search-path)
	  buffer thisdir fmts name)
      (if (file-name-absolute-p filename)
	  ;; The file name is absolute.  Use its explicit directory as
	  ;; the first in the search path, and strip it from FILENAME.
	  (setq filename (abbreviate-file-name (expand-file-name filename))
		dirs (cons (file-name-directory filename) dirs)
		filename (file-name-nondirectory filename)))
      ;; Now search the path.
      (while (and dirs (null buffer))
	(setq thisdir (or (car dirs) dir)
	      fmts formats)
	;; For each directory, try each format string.
	(while (and fmts (null buffer))
	  (setq name (expand-file-name (format (car fmts) filename) thisdir)
		buffer (and (file-exists-p name)
			    (find-file-noselect name))
		fmts (cdr fmts)))
	(setq dirs (cdr dirs)))
      (or buffer
	  ;; The file doesn't exist.
	  ;; Ask the user where to find it.
	  ;; If he hits C-g, then the next time he does
	  ;; next-error, he'll skip past it.
	  (let* ((pop-up-windows t)
		 (w (display-buffer (marker-buffer marker))))
	    (set-window-point w marker)
	    (set-window-start w marker)
	    (let ((name (expand-file-name
			 (read-file-name
			  (format "Find this error in: (default %s) "
				  filename)
			  dir filename t))))
	      (if (file-directory-p name)
		  (setq name (expand-file-name filename name)))
	      (setq buffer (and (file-exists-p name)
				(find-file name))))))
      ;; Make intangible overlays tangible.
      (mapcar (function (lambda (ov)
			  (when (overlay-get ov 'intangible)
			    (overlay-put ov 'intangible nil))))
	      (overlays-in (point-min) (point-max)))
      buffer)))


;; Set compilation-error-list to nil, and unchain the markers that point to the
;; error messages and their text, so that they no longer slow down gap motion.
;; This would happen anyway at the next garbage collection, but it is better to
;; do it right away.
(defun compilation-forget-errors ()
  (while compilation-old-error-list
    (let ((next-error (car compilation-old-error-list)))
      (set-marker (car next-error) nil)
      (if (markerp (cdr next-error))
	  (set-marker (cdr next-error) nil)))
    (setq compilation-old-error-list (cdr compilation-old-error-list)))
  (setq compilation-error-list nil
	compilation-directory-stack (list default-directory))
  (if compilation-parsing-end
      (set-marker compilation-parsing-end 1))
  ;; Remove the highlighting added by compile-reinitialize-errors:
  (let ((inhibit-read-only t)
	(buffer-undo-list t)
	deactivate-mark)
    (remove-text-properties (point-min) (point-max)
			    '(mouse-face highlight help-echo nil))))


;; This function is not needed any more by compilation mode.
;; Does anyone else need it or can it be deleted?
(defun count-regexp-groupings (regexp)
  "Return the number of \\( ... \\) groupings in REGEXP (a string)."
  (let ((groupings 0)
	(len (length regexp))
	(i 0)
	c)
    (while (< i len)
      (setq c (aref regexp i)
	    i (1+ i))
      (cond ((= c ?\[)
	     ;; Find the end of this [...].
	     (while (and (< i len)
			 (not (= (aref regexp i) ?\])))
	       (setq i (1+ i))))
	    ((= c ?\\)
	     (if (< i len)
		 (progn
		   (setq c (aref regexp i)
			 i (1+ i))
		   (if (= c ?\))
		       ;; We found the end of a grouping,
		       ;; so bump our counter.
		       (setq groupings (1+ groupings))))))))
    groupings))

(defvar compilation-current-file nil
  "Used by `compilation-parse-errors' to store filename for file being compiled.")

;; This variable is not used as a global variable. It's defined here just to
;; shut up the byte compiler. It's bound and used by compilation-parse-errors
;; and set by compile-collect-regexps.
(defvar compilation-regexps nil)

(defun compilation-parse-errors (limit-search find-at-least)
  "Parse the current buffer as grep, cc, lint or other error messages.
See variable `compilation-parse-errors-function' for the interface it uses."
  (setq compilation-error-list nil)
  (message "Parsing error messages...")
  (if (null compilation-error-regexp-alist)
      (error "compilation-error-regexp-alist is empty!"))
  (let* ((compilation-regexps nil) ; Variable set by compile-collect-regexps.
	 (default-directory (car compilation-directory-stack))
	 (found-desired nil)
	 (compilation-num-errors-found 0)
	 ;; Set up now the expanded, abbreviated directory variables
	 ;; that compile-abbreviate-directory will need, so we can
	 ;; compute them just once here.
	 (orig (abbreviate-file-name default-directory))
	 (orig-expanded (abbreviate-file-name
			 (file-truename default-directory)))
	 (parent-expanded (abbreviate-file-name
			   (expand-file-name "../" orig-expanded))))

    ;; Make a list of all the regexps. Each element has the form
    ;; (REGEXP TYPE IDX1 IDX2 ...)
    ;; where TYPE is one of leave, enter, file, error or nomessage.
    (compile-collect-regexps 'leave compilation-leave-directory-regexp-alist)
    (compile-collect-regexps 'enter compilation-enter-directory-regexp-alist)
    (compile-collect-regexps 'file compilation-file-regexp-alist)
    (compile-collect-regexps 'error compilation-error-regexp-alist)
    (compile-collect-regexps 'nomessage compilation-nomessage-regexp-alist)

    ;; Don't reparse messages already seen at last parse.
    (goto-char compilation-parsing-end)
    (when (and (bobp)
	       (compilation-buffer-p (current-buffer)))
      (setq compilation-current-file nil) ; No current file at start.
      ;; Don't parse the first two lines as error messages.
      ;; This matters for grep.
      (forward-line 2))

    ;; Parse messages.
    (while (not (or found-desired (eobp)
		    ;; Don't parse the "compilation finished" message
		    ;; as a compilation error.
		    (get-text-property (point) 'compilation-handle-exit)))
      (let ((this compilation-regexps) (prev nil) (alist nil) type)
	;; Go through the regular expressions. If a match is found,
	;; variable alist is set to the corresponding alist and the
	;; matching regexp is moved to the front of compilation-regexps
	;; to make it match faster next time.
	(while (and this (null alist))
	  (if (not (looking-at (car (car this))))
	      (progn (setq prev this)		; No match, go to next.
		     (setq this (cdr this)))
	    (setq alist (cdr (car this))) ; Got a match.
;;;	    (if prev			; If not the first regexp,
;;;		(progn			; move it to the front.
;;;		  (setcdr prev (cdr this))
;;;		  (setcdr this compilation-regexps)
;;;		  (setq compilation-regexps this)))
	    ))
	(if (and alist			; Seen a match and not to
		 (not (eq (setq type (car alist)) 'nomessage)))	; be ignored.
	    (let* ((end-of-match (match-end 0))
		   (filename
		    (compile-buffer-substring (car (setq alist (cdr alist)))))
		   stack)
	      (if (eq type 'error)	; error message
		  (let* ((linenum (if (numberp (car (setq alist (cdr alist))))
				      (string-to-int
				       (compile-buffer-substring (car alist)))
				    ;; (car alist) is not a number, must be a
				    ;; function that is called below to return
				    ;; an error position descriptor.
				    (car alist)))
			 ;; Convert to integer later if linenum not a function.
			 (column (compile-buffer-substring
				  (car (setq  alist (cdr alist)))))
			 this-error)

		    ;; Check that we have a file name.
		    (or filename
			;; No file name in message, we must have seen it before
			(setq filename compilation-current-file)
			(error "\
An error message with no file name and no file name has been seen earlier"))

		    ;; Check for a comint-file-name-prefix and prepend it if
		    ;; appropriate.  (This is very useful for
		    ;; compilation-minor-mode in an rlogin-mode buffer.)
		    (and (boundp 'comint-file-name-prefix)
			 ;; If file name is relative, default-directory will
			 ;; already contain the comint-file-name-prefix (done
			 ;; by compile-abbreviate-directory).
			 (file-name-absolute-p filename)
			 (setq filename
			       (concat comint-file-name-prefix filename)))

		    ;; If compilation-parse-errors-filename-function is
		    ;; defined, use it to process the filename.
		    (when compilation-parse-errors-filename-function
		      (setq filename
			    (funcall compilation-parse-errors-filename-function
				     filename)))

		    ;; Some compilers (e.g. Sun's java compiler, reportedly)
		    ;; produce bogus file names like "./bar//foo.c" for file
		    ;; "bar/foo.c"; expand-file-name will collapse these into
		    ;; "/foo.c" and fail to find the appropriate file.  So we
		    ;; look for doubled slashes in the file name and fix them
		    ;; up in the buffer.
		    (setq filename (command-line-normalize-file-name filename))

		    (setq filename
			  (cons filename (cons default-directory (cdr alist))))

		    ;; Locate the erring file and line.
		    ;; Make this-error a new elt for compilation-error-list,
		    ;; giving a marker for the current compilation buffer
		    ;; location, and the file and line number of the error.
		    ;; Save, as the start of the error, the beginning of the
		    ;; line containing the match.
		    (setq this-error
			  (if (numberp linenum)
			      (list (point-marker) filename linenum
				    (and column (string-to-int column)))
			    ;; If linenum is not a number then it must be
			    ;; a function returning an error position
			    ;; descriptor or nil (meaning no position).
			    (save-excursion
			      (funcall linenum filename column))))

		    ;; We have an error position descriptor.
		    ;; If we have found as many new errors as the user
		    ;; wants, or if we are past the buffer position he
		    ;; indicated, then we continue to parse until we have
		    ;; seen all consecutive errors in the same file. This
		    ;; means that all the errors of a source file will be
		    ;; seen in one parsing run, so that the error positions
		    ;; will be recorded as markers in the source file
		    ;; buffer that will move when the buffer is changed.
		    (if (and this-error
			     compilation-error-list ; At least one previous.
			     (or (and find-at-least
				      (>= compilation-num-errors-found
					  find-at-least))
				 (and limit-search
				      (>= end-of-match limit-search)))
			     ;; `this-error' could contain a pair of
			     ;; markers already.
			     (let ((thispos (cdr this-error))
				   (lastpos (cdar compilation-error-list)))
			       (not (equal
				     (if (markerp thispos)
					 (marker-buffer thispos)
				       (car thispos))
				     (if (markerp lastpos)
					 (marker-buffer lastpos)
				       (car lastpos))))))
			;; We are past the limits and the last error
			;; parsed, didn't belong to the same source file
			;; as the earlier ones i.e. we have seen all the
			;; errors belonging to the earlier file. We don't
			;; add the error just parsed so that the next
			;; parsing run can get it and the following errors
			;; in the same file all at once.
			(setq found-desired t)

		      (goto-char end-of-match) ; Prepare for next message.
		      ;; Don't add the same source line more than once.
		      (and this-error
			   (not (and
				 compilation-error-list
				 (equal (cdr (car compilation-error-list))
					(cdr this-error))))
			   (setq compilation-error-list
				 (cons this-error compilation-error-list)
				 compilation-num-errors-found
				 (1+ compilation-num-errors-found)))))

		;; Not an error message.
		(if (eq type `file)	; Change current file.
		    (and filename (setq compilation-current-file filename))
		  ;; Enter or leave directory.
		  (setq stack compilation-directory-stack)
		  (and filename
		       (file-directory-p
			(setq filename
			      ;; The directory name in the message
			      ;; is a truename.  Try to convert it to a form
			      ;; like what the user typed in.
			      (compile-abbreviate-directory
			       (file-name-as-directory
				(expand-file-name filename))
			       orig orig-expanded parent-expanded)))
		       (if (eq type 'leave)
			   (while (and stack
				       (not (string-equal (car stack)
							  filename)))
			     (setq stack (cdr stack)))
			 (setq compilation-directory-stack
			       (cons filename compilation-directory-stack)
			       default-directory filename)))
		  (and (eq type 'leave)
		       stack
		       (setq compilation-directory-stack (cdr stack))
		       (setq stack (car compilation-directory-stack))
		       (setq default-directory stack)))
		(goto-char end-of-match) ; Prepare to look at next message.
		(and limit-search (>= end-of-match limit-search)
		     ;; The user wanted a specific error, and we're past it.
		     ;; We do this check here rather than at the end of the
		     ;; loop because if the last thing seen is an error
		     ;; message, we must carefully discard the last error
		     ;; when it is the first in a new file (see above in
		     ;; the error-message case)
		     (setq found-desired t)))

	      ;; Go to before the last character in the message so that we will
	      ;; see the next line also when the message ended at end of line.
	      ;; When we ignore the last error message above, this will
	      ;; cancel the effect of forward-line below so that point
	      ;; doesn't move.
	      (forward-char -1)

	      ;; Is this message necessary any more?  Parsing is now so fast
	      ;; that you might not need to know how it proceeds.
	      (message
	       "Parsing error messages...%d found. %.0f%% of buffer seen."
	       compilation-num-errors-found
	       ;; Use floating-point because (* 100 (point)) frequently
	       ;; exceeds the range of Emacs Lisp integers.
	       (/ (* 100.0 (point)) (point-max)))
	      )))

      (forward-line 1))		; End of while loop. Look at next line.

    (set-marker compilation-parsing-end (point))
    (setq compilation-error-list (nreverse compilation-error-list))
    ;; (message "Parsing error messages...done. %d found. %.0f%% of buffer seen."
    ;;	     compilation-num-errors-found
    ;;	     (/ (* 100.0 (point)) (point-max)))
    (message "Parsing error messages...done.")))

(defun compile-collect-regexps (type this)
  ;; Add elements to variable compilation-regexps that is bound in
  ;; compilation-parse-errors.
  (and (not (eq this t))
       (dolist (el this)
	 (push (cons (car el) (cons type (cdr el))) compilation-regexps))))

(defun compile-buffer-substring (index)
  "Get substring matched by INDEXth subexpression."
  (if index
      (let ((beg (match-beginning index)))
	(if beg (buffer-substring beg (match-end index))))))

;; If directory DIR is a subdir of ORIG or of ORIG's parent,
;; return a relative name for it starting from ORIG or its parent.
;; ORIG-EXPANDED is an expanded version of ORIG.
;; PARENT-EXPANDED is an expanded version of ORIG's parent.
;; Those two args could be computed here, but we run faster by
;; having the caller compute them just once.
(defun compile-abbreviate-directory (dir orig orig-expanded parent-expanded)
  ;; Apply canonical abbreviations to DIR first thing.
  ;; Those abbreviations are already done in the other arguments passed.
  (setq dir (abbreviate-file-name dir))

  ;; Check for a comint-file-name-prefix and prepend it if appropriate.
  ;; (This is very useful for compilation-minor-mode in an rlogin-mode
  ;; buffer.)
  (if (boundp 'comint-file-name-prefix)
      (setq dir (concat comint-file-name-prefix dir)))

  (if (and (> (length dir) (length orig-expanded))
	   (string= orig-expanded
		    (substring dir 0 (length orig-expanded))))
      (setq dir
	    (concat orig
		    (substring dir (length orig-expanded)))))
  (if (and (> (length dir) (length parent-expanded))
	   (string= parent-expanded
		    (substring dir 0 (length parent-expanded))))
    (setq dir
	  (concat (file-name-directory
		   (directory-file-name orig))
		  (substring dir (length parent-expanded)))))
  dir)

(add-to-list 'debug-ignored-errors "^No more errors\\( yet\\|\\)$")

(provide 'compile)

;;; compile.el ends here
