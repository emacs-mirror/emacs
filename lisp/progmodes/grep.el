;;; grep.el --- run Grep as inferior of Emacs, parse match messages

;; Copyright (C) 1985, 1986, 1987, 1993, 1994, 1995, 1996, 1997, 1998, 1999,
;;   2001, 2002, 2003, 2004, 2005, 2006, 2007  Free Software Foundation, Inc.

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
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This package provides the grep facilities documented in the Emacs
;; user's manual.

;;; Code:

(require 'compile)


(defgroup grep nil
  "Run grep as inferior of Emacs, parse error messages."
  :group 'tools
  :group 'processes)


;;;###autoload
(defcustom grep-window-height nil
  "*Number of lines in a grep window.  If nil, use `compilation-window-height'."
  :type '(choice (const :tag "Default" nil)
		 integer)
  :version "22.1"
  :group 'grep)

(defcustom grep-highlight-matches 'auto-detect
  "If t, use special markers to highlight grep matches.

Some grep programs are able to surround matches with special
markers in grep output.  Such markers can be used to highlight
matches in grep mode.

This option sets the environment variable GREP_COLOR to specify
markers for highlighting and GREP_OPTIONS to add the --color
option in front of any explicit grep options before starting
the grep.

The default value of this variable is set up by `grep-compute-defaults';
call that function before using this variable in your program."
  :type '(choice (const :tag "Do not highlight matches with grep markers" nil)
		 (const :tag "Highlight matches with grep markers" t)
		 (other :tag "Not Set" auto-detect))
  :version "22.1"
  :group 'grep)

(defcustom grep-scroll-output nil
  "*Non-nil to scroll the *grep* buffer window as output appears.

Setting it causes the grep commands to put point at the end of their
output window so that the end of the output is always visible rather
than the begining."
  :type 'boolean
  :version "22.1"
  :group 'grep)

;;;###autoload
(defcustom grep-command nil
  "The default grep command for \\[grep].
If the grep program used supports an option to always include file names
in its output (such as the `-H' option to GNU grep), it's a good idea to
include it when specifying `grep-command'.

The default value of this variable is set up by `grep-compute-defaults';
call that function before using this variable in your program."
  :type '(choice string
		 (const :tag "Not Set" nil))
  :group 'grep)

(defcustom grep-template nil
  "The default command to run for \\[lgrep].
The default value of this variable is set up by `grep-compute-defaults';
call that function before using this variable in your program.
The following place holders should be present in the string:
 <C> - place to put -i if case insensitive grep.
 <F> - file names and wildcards to search.
 <R> - the regular expression searched for.
 <N> - place to insert null-device."
  :type '(choice string
		 (const :tag "Not Set" nil))
  :version "22.1"
  :group 'grep)

(defcustom grep-use-null-device 'auto-detect
  "If t, append the value of `null-device' to `grep' commands.
This is done to ensure that the output of grep includes the filename of
any match in the case where only a single file is searched, and is not
necessary if the grep program used supports the `-H' option.

The default value of this variable is set up by `grep-compute-defaults';
call that function before using this variable in your program."
  :type '(choice (const :tag "Do Not Append Null Device" nil)
		 (const :tag "Append Null Device" t)
		 (other :tag "Not Set" auto-detect))
  :group 'grep)

;;;###autoload
(defcustom grep-find-command nil
  "The default find command for \\[grep-find].
The default value of this variable is set up by `grep-compute-defaults';
call that function before using this variable in your program."
  :type '(choice string
		 (const :tag "Not Set" nil))
  :group 'grep)

(defcustom grep-find-template nil
  "The default command to run for \\[rgrep].
The default value of this variable is set up by `grep-compute-defaults';
call that function before using this variable in your program.
The following place holders should be present in the string:
 <D> - base directory for find
 <X> - find options to restrict or expand the directory list
 <F> - find options to limit the files matched
 <C> - place to put -i if case insensitive grep
 <R> - the regular expression searched for."
  :type '(choice string
		 (const :tag "Not Set" nil))
  :version "22.1"
  :group 'grep)

(defcustom grep-files-aliases '(
	("el" .	"*.el")
	("ch" .	"*.[ch]")
	("c" .	"*.c")
	("h" .	"*.h")
	("asm" . "*.[sS]")
	("m" .	"[Mm]akefile*")
	("l" . "[Cc]hange[Ll]og*")
	("tex" . "*.tex")
	("texi" . "*.texi")
	)
  "*Alist of aliases for the FILES argument to `lgrep' and `rgrep'."
  :type 'alist
  :group 'grep)

(defcustom grep-find-ignored-directories '("CVS" ".svn" "{arch}" ".hg" "_darcs"
					   ".git" ".bzr")
  "*List of names of sub-directories which `rgrep' shall not recurse into."
  :type '(repeat string)
  :group 'grep)

(defcustom grep-error-screen-columns nil
  "*If non-nil, column numbers in grep hits are screen columns.
See `compilation-error-screen-columns'"
  :type '(choice (const :tag "Default" nil)
		 integer)
  :version "22.1"
  :group 'grep)

;;;###autoload
(defcustom grep-setup-hook nil
  "List of hook functions run by `grep-process-setup' (see `run-hooks')."
  :type 'hook
  :group 'grep)

(defvar grep-mode-map
  (let ((map (cons 'keymap compilation-minor-mode-map)))
    (define-key map " " 'scroll-up)
    (define-key map "\^?" 'scroll-down)
    (define-key map "\C-c\C-f" 'next-error-follow-minor-mode)

    (define-key map "\r" 'compile-goto-error)  ;; ?
    (define-key map "n" 'next-error-no-select)
    (define-key map "p" 'previous-error-no-select)
    (define-key map "{" 'compilation-previous-file)
    (define-key map "}" 'compilation-next-file)
    (define-key map "\t" 'compilation-next-error)
    (define-key map [backtab] 'compilation-previous-error)

    ;; Set up the menu-bar
    (define-key map [menu-bar grep]
      (cons "Grep" (make-sparse-keymap "Grep")))

    (define-key map [menu-bar grep compilation-kill-compilation]
      '("Kill Grep" . kill-compilation))
    (define-key map [menu-bar grep compilation-separator2]
      '("----" . nil))
    (define-key map [menu-bar grep compilation-compile]
      '("Compile..." . compile))
    (define-key map [menu-bar grep compilation-grep]
      '("Another grep..." . grep))
    (define-key map [menu-bar grep compilation-grep-find]
      '("Recursive grep..." . grep-find))
    (define-key map [menu-bar grep compilation-recompile]
      '("Repeat grep" . recompile))
    (define-key map [menu-bar grep compilation-separator2]
      '("----" . nil))
    (define-key map [menu-bar grep compilation-first-error]
      '("First Match" . first-error))
    (define-key map [menu-bar grep compilation-previous-error]
      '("Previous Match" . previous-error))
    (define-key map [menu-bar grep compilation-next-error]
      '("Next Match" . next-error))
    map)
  "Keymap for grep buffers.
`compilation-minor-mode-map' is a cdr of this.")

(defalias 'kill-grep 'kill-compilation)

;;;; TODO --- refine this!!

;;; (defcustom grep-use-compilation-buffer t
;;;   "When non-nil, grep specific commands update `compilation-last-buffer'.
;;; This means that standard compile commands like \\[next-error] and \\[compile-goto-error]
;;; can be used to navigate between grep matches (the default).
;;; Otherwise, the grep specific commands like \\[grep-next-match] must
;;; be used to navigate between grep matches."
;;;   :type 'boolean
;;;   :group 'grep)

;; override compilation-last-buffer
(defvar grep-last-buffer nil
  "The most recent grep buffer.
A grep buffer becomes most recent when you select Grep mode in it.
Notice that using \\[next-error] or \\[compile-goto-error] modifies
`complation-last-buffer' rather than `grep-last-buffer'.")

;;;###autoload
(defvar grep-regexp-alist
  '(("^\\(.+?\\)\\(:[ \t]*\\)\\([0-9]+\\)\\2"
     1 3)
    ;; Rule to match column numbers is commented out since no known grep
    ;; produces them
    ;; ("^\\(.+?\\)\\(:[ \t]*\\)\\([0-9]+\\)\\2\\(?:\\([0-9]+\\)\\(?:-\\([0-9]+\\)\\)?\\2\\)?"
    ;;  1 3 (4 . 5))
    ("^\\(\\(.+?\\):\\([0-9]+\\):\\).*?\
\\(\033\\[01;31m\\(?:\033\\[K\\)?\\)\\(.*?\\)\\(\033\\[[0-9]*m\\)"
     2 3
     ;; Calculate column positions (beg . end) of first grep match on a line
     ((lambda ()
	(setq compilation-error-screen-columns nil)
        (- (match-beginning 4) (match-end 1)))
      .
      (lambda () (- (match-end 5) (match-end 1)
		    (- (match-end 4) (match-beginning 4)))))
     nil 1)
    ("^Binary file \\(.+\\) matches$" 1 nil nil 0 1))
  "Regexp used to match grep hits.  See `compilation-error-regexp-alist'.")

(defvar grep-error "grep hit"
  "Message to print when no matches are found.")

;; Reverse the colors because grep hits are not errors (though we jump there
;; with `next-error'), and unreadable files can't be gone to.
(defvar grep-hit-face	compilation-info-face
  "Face name to use for grep hits.")

(defvar grep-error-face	'compilation-error
  "Face name to use for grep error messages.")

(defvar grep-match-face	'match
  "Face name to use for grep matches.")

(defvar grep-context-face 'shadow
  "Face name to use for grep context lines.")

(defvar grep-mode-font-lock-keywords
   '(;; Command output lines.
     ("^\\([A-Za-z_0-9/\.+-]+\\)[ \t]*:" 1 font-lock-function-name-face)
     (": \\(.+\\): \\(?:Permission denied\\|No such \\(?:file or directory\\|device or address\\)\\)$"
      1 grep-error-face)
     ;; remove match from grep-regexp-alist before fontifying
     ("^Grep[/a-zA-z]* started.*"
      (0 '(face nil message nil help-echo nil mouse-face nil) t))
     ("^Grep[/a-zA-z]* finished \\(?:(\\(matches found\\))\\|with \\(no matches found\\)\\).*"
      (0 '(face nil message nil help-echo nil mouse-face nil) t)
      (1 compilation-info-face nil t)
      (2 compilation-warning-face nil t))
     ("^Grep[/a-zA-z]* \\(exited abnormally\\|interrupt\\|killed\\|terminated\\)\\(?:.*with code \\([0-9]+\\)\\)?.*"
      (0 '(face nil message nil help-echo nil mouse-face nil) t)
      (1 grep-error-face)
      (2 grep-error-face nil t))
     ("^.+?-[0-9]+-.*\n" (0 grep-context-face))
     ;; Highlight grep matches and delete markers
     ("\\(\033\\[01;31m\\)\\(.*?\\)\\(\033\\[[0-9]*m\\)"
      ;; Refontification does not work after the markers have been
      ;; deleted.  So we use the font-lock-face property here as Font
      ;; Lock does not clear that.
      (2 (list 'face nil 'font-lock-face grep-match-face))
      ((lambda (bound))
       (progn
	 ;; Delete markers with `replace-match' because it updates
	 ;; the match-data, whereas `delete-region' would render it obsolete.
	 (replace-match "" t t nil 3)
	 (replace-match "" t t nil 1))))
     ("\\(\033\\[[0-9;]*[mK]\\)"
      ;; Delete all remaining escape sequences
      ((lambda (bound))
       (replace-match "" t t nil 1))))
   "Additional things to highlight in grep output.
This gets tacked on the end of the generated expressions.")

;;;###autoload
(defvar grep-program "grep"
  "The default grep program for `grep-command' and `grep-find-command'.
This variable's value takes effect when `grep-compute-defaults' is called.")

;;;###autoload
(defvar find-program "find"
  "The default find program for `grep-find-command'.
This variable's value takes effect when `grep-compute-defaults' is called.")

;;;###autoload
(defvar grep-find-use-xargs nil
  "Non-nil means that `grep-find' uses the `xargs' utility by default.
If `exec', use `find -exec'.
If `gnu', use `find -print0' and `xargs -0'.
Any other non-nil value means to use `find -print' and `xargs'.

This variable's value takes effect when `grep-compute-defaults' is called.")

;; History of grep commands.
;;;###autoload
(defvar grep-history nil)
;;;###autoload
(defvar grep-find-history nil)

;; History of lgrep and rgrep regexp and files args.
(defvar grep-regexp-history nil)
(defvar grep-files-history '("ch" "el"))


;;;###autoload
(defun grep-process-setup ()
  "Setup compilation variables and buffer for `grep'.
Set up `compilation-exit-message-function' and run `grep-setup-hook'."
  (unless (or (not grep-highlight-matches) (eq grep-highlight-matches t))
    (grep-compute-defaults))
  (when (eq grep-highlight-matches t)
    ;; Modify `process-environment' locally bound in `compilation-start'
    (setenv "GREP_OPTIONS" (concat (getenv "GREP_OPTIONS") " --color=always"))
    ;; for GNU grep 2.5.1
    (setenv "GREP_COLOR" "01;31")
    ;; for GNU grep 2.5.1-cvs
    (setenv "GREP_COLORS" "mt=01;31:fn=:ln=:bn=:se=:ml=:cx=:ne"))
  (set (make-local-variable 'compilation-exit-message-function)
       (lambda (status code msg)
	 (if (eq status 'exit)
	     (cond ((zerop code)
		    '("finished (matches found)\n" . "matched"))
		   ((= code 1)
		    '("finished with no matches found\n" . "no match"))
		   (t
		    (cons msg code)))
	   (cons msg code))))
  (run-hooks 'grep-setup-hook))

(defun grep-probe (command args &optional func result)
  (equal (condition-case nil
	     (apply (or func 'call-process) command args)
	   (error nil))
	 (or result 0)))

;;;###autoload
(defun grep-compute-defaults ()
  (unless (or (not grep-use-null-device) (eq grep-use-null-device t))
    (setq grep-use-null-device
	  (with-temp-buffer
	    (let ((hello-file (expand-file-name "HELLO" data-directory)))
	      (not
	       (and (if grep-command
			;; `grep-command' is already set, so
			;; use that for testing.
			(grep-probe grep-command
				    `(nil t nil "^English" ,hello-file)
				    #'call-process-shell-command)
		      ;; otherwise use `grep-program'
		      (grep-probe grep-program
				  `(nil t nil "-nH" "^English" ,hello-file)))
		    (progn
		      (goto-char (point-min))
		      (looking-at
		       (concat (regexp-quote hello-file)
			       ":[0-9]+:English")))))))))
  (unless (and grep-command grep-find-command
	       grep-template grep-find-template)
    (let ((grep-options
	   (concat (if grep-use-null-device "-n" "-nH")
		   (if (grep-probe grep-program
				   `(nil nil nil "-e" "foo" ,null-device)
				   nil 1)
		       " -e"))))
      (unless grep-command
	(setq grep-command
	      (format "%s %s " grep-program grep-options)))
      (unless grep-template
	(setq grep-template
	      (format "%s <C> %s <R> <F>" grep-program grep-options)))
      (unless grep-find-use-xargs
	(setq grep-find-use-xargs
	      (cond
	       ((and
		 (grep-probe find-program `(nil nil nil ,null-device "-print0"))
		 (grep-probe "xargs" `(nil nil nil "-0" "-e" "echo")))
		'gnu)
	       (t
		'exec))))
      (unless grep-find-command
	(setq grep-find-command
	      (cond ((eq grep-find-use-xargs 'gnu)
		     (format "%s . -type f -print0 | xargs -0 -e %s"
			     find-program grep-command))
		    ((eq grep-find-use-xargs 'exec)
		     (let ((cmd0 (format "%s . -type f -exec %s"
					 find-program grep-command)))
		       (cons
			(format "%s {} %s %s"
				cmd0 null-device
				(shell-quote-argument ";"))
			(1+ (length cmd0)))))
		    (t
		     (format "%s . -type f -print | xargs %s"
			     find-program grep-command)))))
      (unless grep-find-template
	(setq grep-find-template
	      (let ((gcmd (format "%s <C> %s <R>"
				  grep-program grep-options)))
		(cond ((eq grep-find-use-xargs 'gnu)
		       (format "%s . <X> -type f <F> -print0 | xargs -0 -e %s"
			       find-program gcmd))
		      ((eq grep-find-use-xargs 'exec)
		       (format "%s . <X> -type f <F> -exec %s {} %s %s"
			       find-program gcmd null-device
			       (shell-quote-argument ";")))
		      (t
		       (format "%s . <X> -type f <F> -print | xargs %s"
			       find-program gcmd))))))))
  (unless (or (not grep-highlight-matches) (eq grep-highlight-matches t))
    (setq grep-highlight-matches
	  (with-temp-buffer
	    (and (grep-probe grep-program '(nil t nil "--help"))
		 (progn
		   (goto-char (point-min))
		   (search-forward "--color" nil t))
		 t)))))

(defun grep-tag-default ()
  (or (and transient-mark-mode mark-active
	   (/= (point) (mark))
	   (buffer-substring-no-properties (point) (mark)))
      (funcall (or find-tag-default-function
		   (get major-mode 'find-tag-default-function)
		   'find-tag-default))
      ""))

(defun grep-default-command ()
  "Compute the default grep command for C-u M-x grep to offer."
  (let ((tag-default (shell-quote-argument (grep-tag-default)))
	;; This a regexp to match single shell arguments.
	;; Could someone please add comments explaining it?
	(sh-arg-re "\\(\\(?:\"\\(?:[^\"]\\|\\\\\"\\)+\"\\|'[^']+'\\|[^\"' \t\n]\\)+\\)")
	(grep-default (or (car grep-history) grep-command)))
    ;; In the default command, find the arg that specifies the pattern.
    (when (or (string-match
	       (concat "[^ ]+\\s +\\(?:-[^ ]+\\s +\\)*"
		       sh-arg-re "\\(\\s +\\(\\S +\\)\\)?")
	       grep-default)
	      ;; If the string is not yet complete.
	      (string-match "\\(\\)\\'" grep-default))
      ;; Maybe we will replace the pattern with the default tag.
      ;; But first, maybe replace the file name pattern.
      (condition-case nil
	  (unless (or (not (stringp buffer-file-name))
		      (when (match-beginning 2)
			(save-match-data
			  (string-match
			   (wildcard-to-regexp
			    (file-name-nondirectory
			     (match-string 3 grep-default)))
			   (file-name-nondirectory buffer-file-name)))))
	    (setq grep-default (concat (substring grep-default
						  0 (match-beginning 2))
				       " *."
				       (file-name-extension buffer-file-name))))
	;; In case wildcard-to-regexp gets an error
	;; from invalid data.
	(error nil))
      ;; Now replace the pattern with the default tag.
      (replace-match tag-default t t grep-default 1))))


;;;###autoload
(define-compilation-mode grep-mode "Grep"
  "Sets `grep-last-buffer' and `compilation-window-height'."
  (setq grep-last-buffer (current-buffer))
  (set (make-local-variable 'compilation-error-face)
       grep-hit-face)
  (set (make-local-variable 'compilation-error-regexp-alist)
       grep-regexp-alist)
  (set (make-local-variable 'compilation-process-setup-function)
       'grep-process-setup)
  (set (make-local-variable 'compilation-disable-input) t))


;;;###autoload
(defun grep (command-args)
  "Run grep, with user-specified args, and collect output in a buffer.
While grep runs asynchronously, you can use \\[next-error] (M-x next-error),
or \\<grep-mode-map>\\[compile-goto-error] in the grep \
output buffer, to go to the lines
where grep found matches.

For doing a recursive `grep', see the `rgrep' command.  For running
`grep' in a specific directory, see `lgrep'.

This command uses a special history list for its COMMAND-ARGS, so you can
easily repeat a grep command.

A prefix argument says to default the argument based upon the current
tag the cursor is over, substituting it into the last grep command
in the grep command history (or into `grep-command'
if that history list is empty)."
  (interactive
   (progn
     (grep-compute-defaults)
     (let ((default (grep-default-command)))
       (list (read-from-minibuffer "Run grep (like this): "
				   (if current-prefix-arg
				       default grep-command)
				   nil nil 'grep-history
				   (if current-prefix-arg nil default))))))

  ;; Setting process-setup-function makes exit-message-function work
  ;; even when async processes aren't supported.
  (compilation-start (if (and grep-use-null-device null-device)
			 (concat command-args " " null-device)
		       command-args)
		     'grep-mode))


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
     (grep-compute-defaults)
     (if grep-find-command
	 (list (read-from-minibuffer "Run find (like this): "
				     grep-find-command nil nil
                                     'grep-find-history))
       ;; No default was set
       (read-string
        "compile.el: No `grep-find-command' command available. Press RET.")
       (list nil))))
  (when command-args
    (let ((null-device nil))		; see grep
      (grep command-args))))

;;;###autoload
(defalias 'find-grep 'grep-find)


;; User-friendly interactive API.

(defconst grep-expand-keywords
  '(("<C>" . (and cf (isearch-no-upper-case-p regexp t) "-i"))
    ("<D>" . dir)
    ("<F>" . files)
    ("<N>" . null-device)
    ("<X>" . excl)
    ("<R>" . (shell-quote-argument (or regexp ""))))
  "List of substitutions performed by `grep-expand-template'.
If car of an element matches, the cdr is evalled in to get the
substitution string.  Note dynamic scoping of variables.")

(defun grep-expand-template (template &optional regexp files dir excl)
  "Patch grep COMMAND string replacing <C>, <D>, <F>, <R>, and <X>."
  (let ((command template)
	(cf case-fold-search)
	(case-fold-search nil))
    (dolist (kw grep-expand-keywords command)
      (if (string-match (car kw) command)
	  (setq command
		(replace-match
		 (or (if (symbolp (cdr kw))
			 (symbol-value (cdr kw))
		       (save-match-data (eval (cdr kw))))
		     "")
		 t t command))))))

(defun grep-read-regexp ()
  "Read regexp arg for interactive grep."
  (let ((default (grep-tag-default)))
    (read-string
     (concat "Search for"
	     (if (and default (> (length default) 0))
		 (format " (default \"%s\"): " default) ": "))
     nil 'grep-regexp-history default)))

(defun grep-read-files (regexp)
  "Read files arg for interactive grep."
  (let* ((bn (or (buffer-file-name) (buffer-name)))
	 (fn (and bn
		  (stringp bn)
		  (file-name-nondirectory bn)))
	 (default
	   (or (and fn
		    (let ((aliases grep-files-aliases)
			  alias)
		      (while aliases
			(setq alias (car aliases)
			      aliases (cdr aliases))
			(if (string-match (wildcard-to-regexp (cdr alias)) fn)
			    (setq aliases nil)
			  (setq alias nil)))
		      (cdr alias)))
	       (and fn
		    (let ((ext (file-name-extension fn)))
		      (and ext (concat "*." ext))))
	       (car grep-files-history)
	       (car (car grep-files-aliases))))
	 (files (read-string
		 (concat "Search for \"" regexp
			 "\" in files"
			 (if default (concat " (default " default ")"))
			 ": ")
		 nil 'grep-files-history default)))
    (and files
	 (or (cdr (assoc files grep-files-aliases))
	     files))))

;;;###autoload
(defun lgrep (regexp &optional files dir)
  "Run grep, searching for REGEXP in FILES in directory DIR.
The search is limited to file names matching shell pattern FILES.
FILES may use abbreviations defined in `grep-files-aliases', e.g.
entering `ch' is equivalent to `*.[ch]'.

With \\[universal-argument] prefix, you can edit the constructed shell command line
before it is executed.
With two \\[universal-argument] prefixes, directly edit and run `grep-command'.

Collect output in a buffer.  While grep runs asynchronously, you
can use \\[next-error] (M-x next-error), or \\<grep-mode-map>\\[compile-goto-error]
in the grep output buffer, to go to the lines where grep found matches.

This command shares argument histories with \\[rgrep] and \\[grep]."
  (interactive
   (progn
     (grep-compute-defaults)
     (cond
      ((and grep-command (equal current-prefix-arg '(16)))
       (list (read-from-minibuffer "Run: " grep-command
				   nil nil 'grep-history)
	     nil))
      ((not grep-template)
       (list nil
	     (read-string "grep.el: No `grep-template' available. Press RET.")))
      (t (let* ((regexp (grep-read-regexp))
		(files (grep-read-files regexp))
		(dir (read-directory-name "In directory: "
					  nil default-directory t)))
	   (list regexp files dir))))))
  (when (and (stringp regexp) (> (length regexp) 0))
    (let ((command regexp))
      (if (null files)
	  (if (string= command grep-command)
	      (setq command nil))
	(setq dir (file-name-as-directory (expand-file-name dir)))
	(setq command (grep-expand-template
		       grep-template
		       regexp
		       files))
	(when command
	  (if (equal current-prefix-arg '(4))
	      (setq command
		    (read-from-minibuffer "Confirm: "
					  command nil nil 'grep-history))
	    (add-to-history 'grep-history command))))
      (when command
	(let ((default-directory dir))
	  ;; Setting process-setup-function makes exit-message-function work
	  ;; even when async processes aren't supported.
	  (compilation-start (if (and grep-use-null-device null-device)
				 (concat command " " null-device)
			       command) 'grep-mode))
	(if (eq next-error-last-buffer (current-buffer))
	    (setq default-directory dir))))))



;;;###autoload
(defun rgrep (regexp &optional files dir)
  "Recursively grep for REGEXP in FILES in directory tree rooted at DIR.
The search is limited to file names matching shell pattern FILES.
FILES may use abbreviations defined in `grep-files-aliases', e.g.
entering `ch' is equivalent to `*.[ch]'.

With \\[universal-argument] prefix, you can edit the constructed shell command line
before it is executed.
With two \\[universal-argument] prefixes, directly edit and run `grep-find-command'.

Collect output in a buffer.  While find runs asynchronously, you
can use \\[next-error] (M-x next-error), or \\<grep-mode-map>\\[compile-goto-error]
in the grep output buffer, to go to the lines where grep found matches.

This command shares argument histories with \\[lgrep] and \\[grep-find]."
  (interactive
   (progn
     (grep-compute-defaults)
     (cond
      ((and grep-find-command (equal current-prefix-arg '(16)))
       (list (read-from-minibuffer "Run: " grep-find-command
				   nil nil 'grep-find-history)
	     nil))
      ((not grep-find-template)
       (list nil nil
	     (read-string "grep.el: No `grep-find-template' available. Press RET.")))
      (t (let* ((regexp (grep-read-regexp))
		(files (grep-read-files regexp))
		(dir (read-directory-name "Base directory: "
					  nil default-directory t)))
	   (list regexp files dir))))))
  (when (and (stringp regexp) (> (length regexp) 0))
    (if (null files)
	(if (not (string= regexp grep-find-command))
	    (compilation-start regexp 'grep-mode))
      (setq dir (file-name-as-directory (expand-file-name dir)))
      (let ((command (grep-expand-template
		      grep-find-template
		      regexp
		      (concat (shell-quote-argument "(")
			      " -name "
			      (mapconcat #'shell-quote-argument
					 (split-string files)
					 " -o -name ")
			      " "
			      (shell-quote-argument ")"))
		       dir
		       (and grep-find-ignored-directories
			    (concat (shell-quote-argument "(")
				    ;; we should use shell-quote-argument here
				    " -path "
				    (mapconcat #'(lambda (dir)
						   (shell-quote-argument
						    (concat "*/" dir)))
					       grep-find-ignored-directories
					       " -o -path ")
				    " "
				    (shell-quote-argument ")")
				    " -prune -o ")))))
	(when command
	  (if current-prefix-arg
	      (setq command
		    (read-from-minibuffer "Confirm: "
					  command nil nil 'grep-find-history))
	    (add-to-history 'grep-find-history command))
	  (let ((default-directory dir))
	    (compilation-start command 'grep-mode))
	  ;; Set default-directory if we started rgrep in the *grep* buffer.
	  (if (eq next-error-last-buffer (current-buffer))
	      (setq default-directory dir)))))))


(provide 'grep)

;; arch-tag: 5a5b9169-a79d-4f38-9c38-f69615f39c4d
;;; grep.el ends here
