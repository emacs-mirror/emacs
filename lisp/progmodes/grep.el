;;; grep.el --- run Grep as inferior of Emacs, parse match messages

;; Copyright (C) 1985, 1986, 1987, 1993, 1994, 1995, 1996, 1997, 1998, 1999,
;;   2001, 2002, 2003, 2004, 2005  Free Software Foundation, Inc.

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
  "Run compiler as inferior of Emacs, parse error messages."
  :group 'tools
  :group 'processes)


;;;###autoload
(defcustom grep-window-height nil
  "*Number of lines in a grep window.  If nil, use `compilation-window-height'."
  :type '(choice (const :tag "Default" nil)
		 integer)
  :version "22.1"
  :group 'grep)

(defcustom grep-auto-highlight t
  "*Specify how many grep matches to highlight (and parse) initially.
\(Highlighting applies to an grep match when the mouse is over it.)
If this is a number N, all grep matches in the first N lines
are highlighted and parsed as soon as they arrive in Emacs.
If t, highlight and parse the whole grep output as soon as it arrives.
If nil, don't highlight or parse any of the grep buffer until you try to
move to the error messages.

Those grep matches which are not parsed and highlighted initially
will be parsed and highlighted as soon as you try to move to them."
  :type '(choice (const :tag "All" t)
		 (const :tag "None" nil)
		 (integer :tag "First N lines"))
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

(defcustom grep-tree-command nil
  "The default find command for \\[grep-tree].
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

(defcustom grep-tree-files-aliases '(
	("ch" .	"*.[ch]")
	("c" .	"*.c")
	("h" .	"*.h")
	("m" .	"[Mm]akefile*")
	("asm" . "*.[sS]")
	("all" . "*")
	("el" .	"*.el")
	)
  "*Alist of aliases for the FILES argument to `grep-tree'."
  :type 'alist
  :group 'grep)

(defcustom grep-tree-ignore-case t
  "*If non-nil, `grep-tree' ignores case in matches."
  :type 'boolean
  :group 'grep)

(defcustom grep-tree-ignore-CVS-directories t
  "*If non-nil, `grep-tree' does no recurse into CVS directories."
  :type 'boolean
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
      '("Another grep" . grep))
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
A grep buffer becomes most recent when its process is started
or when it is used with \\[grep-next-match].
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
     ("^Grep finished \\(?:(\\(matches found\\))\\|with \\(no matches found\\)\\).*"
      (0 '(face nil message nil help-echo nil mouse-face nil) t)
      (1 compilation-info-face nil t)
      (2 compilation-warning-face nil t))
     ("^Grep \\(exited abnormally\\) with code \\([0-9]+\\).*"
      (0 '(face nil message nil help-echo nil mouse-face nil) t)
      (1 grep-error-face)
      (2 grep-error-face))
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

;;;###autoload
(defvar find-program "find"
  "The default find program for `grep-find-command'.
This variable's value takes effect when `grep-compute-defaults' is called.")

;;;###autoload
(defvar grep-find-use-xargs nil
  "Whether \\[grep-find] uses the `xargs' utility by default.

If nil, it uses `find -exec'; if `gnu', it uses `find -print0' and `xargs -0';
if not nil and not `gnu', it uses `find -print' and `xargs'.

This variable's value takes effect when `grep-compute-defaults' is called.")

;; History of grep commands.
;;;###autoload
(defvar grep-history nil)
;;;###autoload
(defvar grep-find-history nil)

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

;;;###autoload
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
			 (+ 22 (length grep-command)))))))
  (unless grep-tree-command
    (setq grep-tree-command
	  (let* ((glen (length grep-program))
		 (gcmd (concat grep-program " <C>" (substring grep-command glen))))
	    (cond ((eq grep-find-use-xargs 'gnu)
		   (format "%s <D> <X> -type f <F> -print0 | xargs -0 -e %s <R>"
			   find-program gcmd))
		  (grep-find-use-xargs
		   (format "%s <D> <X> -type f <F> -print | xargs %s <R>"
			   find-program gcmd))
		  (t (format "%s <D> <X> -type f <F> -exec %s <R> {} %s \\;"
			     find-program gcmd null-device))))))
  (unless (or (not grep-highlight-matches) (eq grep-highlight-matches t))
    (setq grep-highlight-matches
	  (with-temp-buffer
	    (and (equal (condition-case nil
			    (call-process grep-program nil t nil "--help")
			  (error nil))
			0)
		 (progn
		   (goto-char (point-min))
		   (search-forward "--color" nil t))
		 t)))))

(defun grep-default-command ()
  (let ((tag-default
         (shell-quote-argument
          (or (funcall (or find-tag-default-function
                           (get major-mode 'find-tag-default-function)
                           'find-tag-default))
              "")))
	(sh-arg-re "\\(\\(?:\"\\(?:[^\"]\\|\\\\\"\\)+\"\\|'[^']+'\\|[^\"' \t\n]\\)+\\)")
	(grep-default (or (car grep-history) grep-command)))
    ;; Replace the thing matching for with that around cursor.
    (when (or (string-match
	       (concat "[^ ]+\\s +\\(?:-[^ ]+\\s +\\)*"
		       sh-arg-re "\\(\\s +\\(\\S +\\)\\)?")
	       grep-default)
	      ;; If the string is not yet complete.
	      (string-match "\\(\\)\\'" grep-default))
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
      (replace-match tag-default t t grep-default 1))))

;;;###autoload
(defun grep (command-args &optional highlight-regexp)
  "Run grep, with user-specified args, and collect output in a buffer.
While grep runs asynchronously, you can use \\[next-error] (M-x next-error),
or \\<grep-mode-map>\\[compile-goto-error] in the grep \
output buffer, to go to the lines
where grep found matches.

This command uses a special history list for its COMMAND-ARGS, so you can
easily repeat a grep command.

A prefix argument says to default the argument based upon the current
tag the cursor is over, substituting it into the last grep command
in the grep command history (or into `grep-command'
if that history list is empty).

If specified, optional second arg HIGHLIGHT-REGEXP is the regexp to
temporarily highlight in visited source lines."
  (interactive
   (progn
     (unless (and grep-command
		  (or (not grep-use-null-device) (eq grep-use-null-device t)))
       (grep-compute-defaults))
     (let ((default (grep-default-command)))
       (list (read-from-minibuffer "Run grep (like this): "
				   (if current-prefix-arg
				       default grep-command)
				   nil nil 'grep-history
				   (if current-prefix-arg nil default))))))

  ;; Setting process-setup-function makes exit-message-function work
  ;; even when async processes aren't supported.
  (let ((compilation-process-setup-function 'grep-process-setup))
    (compilation-start (if (and grep-use-null-device null-device)
			   (concat command-args " " null-device)
			 command-args)
		       'grep-mode nil highlight-regexp)))

;;;###autoload
(define-compilation-mode grep-mode "Grep"
  "Sets `grep-last-buffer' and `compilation-window-height'."
  (setq grep-last-buffer (current-buffer))
  (set (make-local-variable 'compilation-error-face)
       grep-hit-face)
  (set (make-local-variable 'compilation-error-regexp-alist)
       grep-regexp-alist)
  ;; Set `font-lock-lines-before' to 0 to not refontify the previous
  ;; line where grep markers may be already removed.
  (set (make-local-variable 'font-lock-lines-before) 0))

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
     (unless (and grep-command
		  (or (not grep-use-null-device) (eq grep-use-null-device t)))
       (grep-compute-defaults))
     (if grep-find-command
	 (list (read-from-minibuffer "Run find (like this): "
				     grep-find-command nil nil
                                     'grep-find-history))
       ;; No default was set
       (read-string
        "compile.el: No `grep-find-command' command available. Press RET.")
       (list nil))))
  (when (and grep-find-command command-args)
    (let ((null-device nil))		; see grep
      (grep command-args))))

;;;###autoload
(defalias 'find-grep 'grep-find)

(defun grep-expand-command-macros (command &optional regexp files dir excl case-fold)
  "Patch grep COMMAND replacing <D>, etc."
  (setq command
	(replace-regexp-in-string "<D>"
				  (or dir ".") command t t))
  (setq command
	(replace-regexp-in-string "<X>"
				  (or excl "") command t t))
  (setq command
	(replace-regexp-in-string "<F>"
				  (or files "") command t t))
  (setq command
	(replace-regexp-in-string "<C>"
				  (if case-fold "-i" "") command t t))
  (setq command
	(replace-regexp-in-string "<R>"
				  (or regexp "") command t t))
  command)

(defvar grep-tree-last-regexp "")
(defvar grep-tree-last-files (car (car grep-tree-files-aliases)))

;;;###autoload
(defun grep-tree (regexp files dir &optional subdirs)
  "Grep for REGEXP in FILES in directory tree rooted at DIR.
Collect output in a buffer.
Interactively, prompt separately for each search parameter.
With prefix arg, reuse previous REGEXP.
The search is limited to file names matching shell pattern FILES.
FILES may use abbreviations defined in `grep-tree-files-aliases', e.g.
entering `ch' is equivalent to `*.[ch]'.

While find runs asynchronously, you can use the \\[next-error] command
to find the text that grep hits refer to.

This command uses a special history list for its arguments, so you can
easily repeat a find command.

When used non-interactively, optional arg SUBDIRS limits the search to
those sub directories of DIR."
  (interactive
   (let* ((regexp
	   (if current-prefix-arg
	       grep-tree-last-regexp
	     (let* ((default (current-word))
		    (spec (read-string
			   (concat "Search for"
				   (if (and default (> (length default) 0))
				       (format " (default %s): " default) ": ")))))
	       (if (equal spec "") default spec))))
	  (files
	   (read-string (concat "Search for \"" regexp "\" in files (default "   grep-tree-last-files  "): ")))
	  (dir
	   (read-directory-name "Base directory: " nil default-directory t)))
     (list regexp files dir)))
  (unless grep-tree-command
    (grep-compute-defaults))
  (unless (and (stringp files) (> (length files) 0))
    (setq files grep-tree-last-files))
  (when files
    (setq grep-tree-last-files files)
    (let ((mf (assoc files grep-tree-files-aliases)))
      (if mf
	  (setq files (cdr mf)))))
  (let ((command-args (grep-expand-command-macros
		       grep-tree-command
		       (setq grep-tree-last-regexp regexp)
		       (and files (concat "-name '" files "'"))
		       (if subdirs
			   (if (stringp subdirs)
			       subdirs
			     (mapconcat 'identity subdirs " "))
			 nil)  ;; we change default-directory to dir
		       (and grep-tree-ignore-CVS-directories "-path '*/CVS' -prune -o ")
		       grep-tree-ignore-case))
	(default-directory (file-name-as-directory (expand-file-name dir)))
	(null-device nil))		; see grep
    (grep command-args regexp)))


(provide 'grep)

;; arch-tag: 5a5b9169-a79d-4f38-9c38-f69615f39c4d
;;; grep.el ends here
