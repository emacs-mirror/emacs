;;; grep.el --- run `grep' and display the results  -*- lexical-binding:t -*-

;; Copyright (C) 1985-1987, 1993-1999, 2001-2021 Free Software
;; Foundation, Inc.

;; Author: Roland McGrath <roland@gnu.org>
;; Maintainer: emacs-devel@gnu.org
;; Keywords: tools, processes

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
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides the grep facilities documented in the Emacs
;; user's manual.

;;; Code:

(eval-when-compile (require 'cl-lib))
(require 'compile)

(defgroup grep nil
  "Run `grep' and display the results."
  :group 'tools
  :group 'processes)

(defvar grep-host-defaults-alist nil
  "Default values depending on target host.
`grep-compute-defaults' returns default values for every local or
remote host `grep' runs.  These values can differ from host to
host.  Once computed, the default values are kept here in order
to avoid computing them again.")

(defun grep-apply-setting (symbol value)
  "Set SYMBOL to VALUE, and update `grep-host-defaults-alist'.
SYMBOL should be one of `grep-command', `grep-template',
`grep-use-null-device', `grep-find-command' `grep-find-template',
`grep-find-use-xargs', `grep-use-null-filename-separator', or
`grep-highlight-matches'."
  (when grep-host-defaults-alist
    (let* ((host-id
	    (intern (or (file-remote-p default-directory) "localhost")))
	   (host-defaults (assq host-id grep-host-defaults-alist))
	   (defaults (assq nil grep-host-defaults-alist)))
      (setcar (cdr (assq symbol host-defaults)) value)
      (setcar (cdr (assq symbol defaults)) value)))
  (set-default symbol value))

;;;###autoload
(defcustom grep-window-height nil
  "Number of lines in a grep window.  If nil, use `compilation-window-height'."
  :type '(choice (const :tag "Default" nil)
		 integer)
  :version "22.1")

(defcustom grep-highlight-matches 'auto-detect
  "Use special markers to highlight grep matches.

Some grep programs are able to surround matches with special
markers in grep output.  Such markers can be used to highlight
matches in grep mode.  This requires `font-lock-mode' to be active
in grep buffers, so if you have globally disabled font-lock-mode,
you will not get highlighting.

This option sets the environment variable GREP_COLORS to specify
markers for highlighting and adds the --color option in front of
any explicit grep options before starting the grep.

When this option is `auto', grep uses `--color=auto' to highlight
matches only when it outputs to a terminal (when `grep' is the last
command in the pipe), thus avoiding the use of any potentially-harmful
escape sequences when standard output goes to a file or pipe.

To make grep highlight matches even into a pipe, you need the option
`always' that forces grep to use `--color=always' to unconditionally
output escape sequences.

In interactive usage, the actual value of this variable is set up
by `grep-compute-defaults' when the default value is `auto-detect'.
To change the default value, use \\[customize] or call the function
`grep-apply-setting'."
  :type '(choice (const :tag "Do not highlight matches with grep markers" nil)
		 (const :tag "Highlight matches with grep markers" t)
		 (const :tag "Use --color=always" always)
		 (const :tag "Use --color=auto" auto)
		 (other :tag "Not Set" auto-detect))
  :set #'grep-apply-setting
  :version "22.1")

(defcustom grep-match-regexp "\033\\[\\(?:0?1;\\)?31m\\(.*?\\)\033\\[[0-9]*m"
  "Regular expression matching grep markers to highlight.
It matches SGR ANSI escape sequences which are emitted by grep to
color its output.  This variable is used in `grep-filter'."
  :type 'regexp
  :version "28.1")

(defcustom grep-scroll-output nil
  "Non-nil to scroll the *grep* buffer window as output appears.

Setting it causes the grep commands to put point at the end of their
output window so that the end of the output is always visible rather
than the beginning."
  :type 'boolean
  :version "22.1")

;;;###autoload
(defcustom grep-command nil
  "The default grep command for \\[grep].
If the grep program used supports an option to always include file names
in its output (such as the `-H' option to GNU grep), it's a good idea to
include it when specifying `grep-command'.

In interactive usage, the actual value of this variable is set up
by `grep-compute-defaults'; to change the default value, use
\\[customize] or call the function `grep-apply-setting'."
  :type '(choice string
		 (const :tag "Not Set" nil))
  :set #'grep-apply-setting)

(defcustom grep-template nil
  "The default command to run for \\[lgrep].
The following place holders should be present in the string:
 <C> - place to put the options like -i and --color.
 <F> - file names and wildcards to search.
 <X> - file names and wildcards to exclude.
 <R> - the regular expression searched for.
 <N> - place to insert null-device.

In interactive usage, the actual value of this variable is set up
by `grep-compute-defaults'; to change the default value, use
\\[customize] or call the function `grep-apply-setting'."
  :type '(choice string
		 (const :tag "Not Set" nil))
  :set #'grep-apply-setting
  :version "22.1")

(defcustom grep-use-null-device 'auto-detect
  "If t, append the value of `null-device' to `grep' commands.
This is done to ensure that the output of grep includes the filename of
any match in the case where only a single file is searched, and is not
necessary if the grep program used supports the `-H' option.

In interactive usage, the actual value of this variable is set up
by `grep-compute-defaults'; to change the default value, use
\\[customize] or call the function `grep-apply-setting'."
  :type '(choice (const :tag "Do Not Append Null Device" nil)
		 (const :tag "Append Null Device" t)
		 (other :tag "Not Set" auto-detect))
  :set #'grep-apply-setting)

(defcustom grep-use-null-filename-separator 'auto-detect
  "If non-nil, use `grep's `--null' option.
This is done to disambiguate file names in `grep's output."
  :version "26.1"
  :type '(choice (const :tag "Do Not Use `--null'" nil)
                 (const :tag "Use `--null'" t)
                 (other :tag "Not Set" auto-detect))
  :set #'grep-apply-setting)

;;;###autoload
(defcustom grep-find-command nil
  "The default find command for \\[grep-find].
In interactive usage, the actual value of this variable is set up
by `grep-compute-defaults'; to change the default value, use
\\[customize] or call the function `grep-apply-setting'.

This variable can either be a string, or a cons of the
form (COMMAND . POSITION).  In the latter case, COMMAND will be
used as the default command, and point will be placed at POSITION
for easier editing."
  :type '(choice string
                 (cons string integer)
		 (const :tag "Not Set" nil))
  :set #'grep-apply-setting)

(defcustom grep-find-template nil
  "The default command to run for \\[rgrep].
The following place holders should be present in the string:
 <D> - base directory for find
 <X> - find options to restrict or expand the directory list
 <F> - find options to limit the files matched
 <C> - place to put the grep options like -i and --color
 <R> - the regular expression searched for.
In interactive usage, the actual value of this variable is set up
by `grep-compute-defaults'; to change the default value, use
\\[customize] or call the function `grep-apply-setting'."
  :type '(choice string
		 (const :tag "Not Set" nil))
  :set #'grep-apply-setting
  :version "22.1")

(defcustom grep-files-aliases
  '(("all" .   "* .[!.]* ..?*") ;; Don't match `..'. See bug#22577
    ("el" .    "*.el")
    ("ch" .    "*.[ch]")
    ("c" .     "*.c")
    ("cc" .    "*.cc *.cxx *.cpp *.C *.CC *.c++")
    ("cchh" .  "*.cc *.[ch]xx *.[ch]pp *.[CHh] *.CC *.HH *.[ch]++")
    ("hh" .    "*.hxx *.hpp *.[Hh] *.HH *.h++")
    ("h" .     "*.h")
    ("l" .     "[Cc]hange[Ll]og*")
    ("m" .     "[Mm]akefile*")
    ("tex" .   "*.tex")
    ("texi" .  "*.texi")
    ("asm" .   "*.[sS]"))
  "Alist of aliases for the FILES argument to `lgrep' and `rgrep'."
  :type 'alist)

(defcustom grep-find-ignored-directories vc-directory-exclusion-list
  "List of names of sub-directories which `rgrep' shall not recurse into.
If an element is a cons cell, the car is called on the search directory
to determine whether cdr should not be recursed into.

The default value is inherited from `vc-directory-exclusion-list'."
  :type '(choice (repeat :tag "Ignored directories" string)
		 (const :tag "No ignored directories" nil)))

(defcustom grep-find-ignored-files
  (cons ".#*" (delq nil (mapcar (lambda (s)
				  (unless (string-match-p "/\\'" s)
				    (concat "*" s)))
				completion-ignored-extensions)))
  "List of file names which `rgrep' and `lgrep' shall exclude.
If an element is a cons cell, the car is called on the search directory
to determine whether cdr should not be excluded."
  :type '(choice (repeat :tag "Ignored file" string)
		 (const :tag "No ignored files" nil)))

(defcustom grep-save-buffers 'ask
  "If non-nil, save buffers before running the grep commands.
If `ask', ask before saving.  If a function, call it with no arguments
with each buffer current, as a predicate to determine whether that
buffer should be saved or not.  E.g., one can set this to
  (lambda ()
    (string-prefix-p my-grep-root (file-truename (buffer-file-name))))
to limit saving to files located under `my-grep-root'."
  :version "26.1"
  :type '(choice
          (const :tag "Ask before saving" ask)
          (const :tag "Don't save buffers" nil)
          function
          (other :tag "Save all buffers" t)))

(defcustom grep-error-screen-columns nil
  "If non-nil, column numbers in grep hits are screen columns.
See `compilation-error-screen-columns'."
  :type '(choice (const :tag "Default" nil)
		 integer)
  :version "22.1")

;;;###autoload
(defcustom grep-setup-hook nil
  "List of hook functions run by `grep-process-setup' (see `run-hooks')."
  :type 'hook)

(defvar grep-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map compilation-minor-mode-map)
    (define-key map " " 'scroll-up-command)
    (define-key map [?\S-\ ] 'scroll-down-command)
    (define-key map "\^?" 'scroll-down-command)
    (define-key map "\C-c\C-f" 'next-error-follow-minor-mode)

    (define-key map "\r" 'compile-goto-error)  ;; ?
    (define-key map "{" 'compilation-previous-file)
    (define-key map "}" 'compilation-next-file)
    (define-key map "\t" 'compilation-next-error)
    (define-key map [backtab] 'compilation-previous-error)
    map)
  "Keymap for grep buffers.
`compilation-minor-mode-map' is a cdr of this.")

(easy-menu-define grep-menu-map grep-mode-map
  "Menu for grep buffers."
  '("Grep"
    ["Next Match" next-error
     :help "Visit the next match and corresponding location"]
    ["Previous Match" previous-error
     :help "Visit the previous match and corresponding location"]
    ["First Match" first-error
     :help "Restart at the first match, visit corresponding location"]
    "----"
    ["Repeat grep" recompile
     :help "Run grep again"]
    ["Another grep..." grep
     :help "Run grep, with user-specified args, and collect output in a buffer."]
    ["Grep via Find..." grep-find
     :help "Run grep via find, with user-specified args"]
    ["Local grep..." lgrep
     :help "User-friendly grep in a directory"]
    ["Recursive grep..." rgrep
     :help "User-friendly recursive grep in directory tree"]
    ["Compile..." compile
     :help "Compile the program including the current buffer.  Default: run `make'"]
    "----"
    ["Kill Grep" kill-compilation
     :help "Kill the currently running grep process"]
    "----"
    ["Toggle command abbreviation" grep-find-toggle-abbreviation
     :help "Toggle showing verbose command options"]))

(defvar grep-mode-tool-bar-map
  ;; When bootstrapping, tool-bar-map is not properly initialized yet,
  ;; so don't do anything.
  (when (keymapp (butlast tool-bar-map))
    ;; We have to `copy-keymap' rather than use keymap inheritance because
    ;; we want to put the new items at the *end* of the tool-bar.
    (let ((map (butlast (copy-keymap tool-bar-map)))
	  ;; FIXME: Nowadays the last button is not "help" but "search"!
	  (help (last tool-bar-map))) ;; Keep Help last in tool bar
      (tool-bar-local-item
       "left-arrow" 'previous-error-no-select 'previous-error-no-select map
       :rtl "right-arrow"
       :help "Goto previous match")
      (tool-bar-local-item
       "right-arrow" 'next-error-no-select 'next-error-no-select map
       :rtl "left-arrow"
       :help "Goto next match")
      (tool-bar-local-item
       "cancel" 'kill-compilation 'kill-compilation map
       :enable '(let ((buffer (compilation-find-buffer)))
		  (get-buffer-process buffer))
       :help "Stop grep")
      (tool-bar-local-item
       "refresh" 'recompile 'recompile map
       :help "Restart grep")
      (append map help))))

(defalias 'kill-grep 'kill-compilation)

;; override compilation-last-buffer
(defvar grep-last-buffer nil
  "The most recent grep buffer.
A grep buffer becomes most recent when you select Grep mode in it.
Notice that using \\[next-error] or \\[compile-goto-error] modifies
`compilation-last-buffer' rather than `grep-last-buffer'.")

(defvar grep-match-face	'match
  "Face name to use for grep matches.")

;;;###autoload
(defconst grep-regexp-alist
  `((,(concat "^\\(?:"
              ;; Parse using NUL characters when `--null' is used.
              ;; Note that we must still assume no newlines in
              ;; filenames due to "foo: Is a directory." type
              ;; messages.
              "\\(?1:[^\0\n]+\\)\\(?3:\0\\)\\(?2:[0-9]+\\):"
              "\\|"
              ;; Fallback if `--null' is not used, use a tight regexp
              ;; to handle weird file names (with colons in them) as
              ;; well as possible.  E.g., use [1-9][0-9]* rather than
              ;; [0-9]+ so as to accept ":034:" in file names.
              "\\(?1:"
              "\\(?:[a-zA-Z]:\\)?" ; Allow "C:..." for w32.
              "[^\n:]+?[^\n/:]\\):[\t ]*\\(?2:[1-9][0-9]*\\)[\t ]*:"
              "\\)")
     1 2
     ;; Calculate column positions (col . end-col) of first grep match on a line
     (,(lambda ()
         (when grep-highlight-matches
           (let* ((beg (match-end 0))
                  (end (save-excursion (goto-char beg) (line-end-position)))
                  (mbeg
                   (text-property-any beg end 'font-lock-face grep-match-face)))
             (when mbeg
               (- mbeg beg)))))
      .
      ,(lambda ()
         (when grep-highlight-matches
           (let* ((beg (match-end 0))
                  (end (save-excursion (goto-char beg) (line-end-position)))
                  (mbeg
                   (text-property-any beg end 'font-lock-face grep-match-face))
                  (mend
                   (and mbeg (next-single-property-change
                              mbeg 'font-lock-face nil end))))
             (when mend
               (- mend beg))))))
     nil nil
     (3 '(face nil display ":")))
    ("^Binary file \\(.+\\) matches" 1 nil nil 0 1))
  "Regexp used to match grep hits.
See `compilation-error-regexp-alist' for format details.")

(defvar grep-first-column 0		; bug#10594
  "Value to use for `compilation-first-column' in grep buffers.")

(defvar grep-error "grep hit"
  "Message to print when no matches are found.")

;; Reverse the colors because grep hits are not errors (though we jump there
;; with `next-error'), and unreadable files can't be gone to.
(defvar grep-hit-face	compilation-info-face
  "Face name to use for grep hits.")

(defvar grep-error-face	'compilation-error
  "Face name to use for grep error messages.")

(defvar grep-context-face 'shadow
  "Face name to use for grep context lines.")

(defvar grep-num-matches-found 0)

(defconst grep-mode-line-matches
  `(" [" (:propertize (:eval (int-to-string grep-num-matches-found))
                      face ,grep-hit-face
                      help-echo "Number of matches so far")
    "]"))

(defcustom grep-find-abbreviate t
  "If non-nil, hide part of rgrep/lgrep/zrgrep command line.
The hidden part contains a list of ignored directories and files.
Clicking on the button-like ellipsis unhides the abbreviated part
and reveals the entire command line.  The visibility of the
abbreviated part can also be toggled with
`grep-find-toggle-abbreviation'."
  :type 'boolean
  :version "27.1")

(defcustom grep-search-path '(nil)
  "List of directories to search for files named in grep messages.
Elements should be directory names, not file names of
directories.  The value nil as an element means the grep messages
buffer `default-directory'."
  :version "27.1"
  :type '(repeat (choice (const :tag "Default" nil)
			 (string :tag "Directory"))))

(defvar grep-find-abbreviate-properties
  (let ((ellipsis (if (char-displayable-p ?…) "[…]" "[...]"))
        (map (make-sparse-keymap)))
    (define-key map [down-mouse-2] 'mouse-set-point)
    (define-key map [mouse-2] 'grep-find-toggle-abbreviation)
    (define-key map "\C-m" 'grep-find-toggle-abbreviation)
    `(face nil display ,ellipsis mouse-face highlight
      help-echo "RET, mouse-2: show unabbreviated command"
      keymap ,map abbreviated-command t))
  "Properties of button-like ellipsis on part of rgrep command line.")

(defvar grep-mode-font-lock-keywords
   '(;; Command output lines.
     (": \\(.+\\): \\(?:Permission denied\\|No such \\(?:file or directory\\|device or address\\)\\)$"
      1 grep-error-face)
     ;; remove match from grep-regexp-alist before fontifying
     ("^Grep[/a-zA-Z]* started.*"
      (0 '(face nil compilation-message nil help-echo nil mouse-face nil) t))
     ("^Grep[/a-zA-Z]* finished with \\(?:\\(\\(?:[0-9]+ \\)?match\\(?:es\\)? found\\)\\|\\(no matches found\\)\\).*"
      (0 '(face nil compilation-message nil help-echo nil mouse-face nil) t)
      (1 compilation-info-face nil t)
      (2 compilation-warning-face nil t))
     ("^Grep[/a-zA-Z]* \\(exited abnormally\\|interrupt\\|killed\\|terminated\\)\\(?:.*with code \\([0-9]+\\)\\)?.*"
      (0 '(face nil compilation-message nil help-echo nil mouse-face nil) t)
      (1 grep-error-face)
      (2 grep-error-face nil t))
     ;; "filename-linenumber-" format is used for context lines in GNU grep,
     ;; "filename=linenumber=" for lines with function names in "git grep -p".
     ("^.+?\\([-=\0]\\)[0-9]+\\([-=]\\).*\n"
      (0 grep-context-face)
      (1 (if (eq (char-after (match-beginning 1)) ?\0)
             `(face nil display ,(match-string 2)))))
     ;; Hide excessive part of rgrep command
     ("^find \\(\\. -type d .*\\\\)\\)"
      (1 (if grep-find-abbreviate grep-find-abbreviate-properties
           '(face nil abbreviated-command t))))
     ;; Hide excessive part of lgrep command
     ("^grep \\( *--exclude.*--exclude[^ ]+\\)"
      (1 (if grep-find-abbreviate grep-find-abbreviate-properties
           '(face nil abbreviated-command t)))))
   "Additional things to highlight in grep output.
This gets tacked on the end of the generated expressions.")

;;;###autoload
(defvar grep-program (purecopy "grep")
  "The default grep program for `grep-command' and `grep-find-command'.
This variable's value takes effect when `grep-compute-defaults' is called.")

;;;###autoload
(defvar find-program (purecopy "find")
  "The default find program.
This is used by commands like `grep-find-command', `find-dired'
and others.")

;;;###autoload
(defvar xargs-program (purecopy "xargs")
  "The default xargs program for `grep-find-command'.
See `grep-find-use-xargs'.
This variable's value takes effect when `grep-compute-defaults' is called.")

;;;###autoload
(defcustom grep-find-use-xargs nil
  "How to invoke find and grep.
If `exec', use `find -exec {} ;'.
If `exec-plus' use `find -exec {} +'.
If `gnu', use `find -print0' and `xargs -0'.
If `gnu-sort', use `find -print0', `sort -z' and `xargs -0'.
Any other value means to use `find -print' and `xargs'.

This variable's value takes effect when `grep-compute-defaults' is called."
  :type '(choice (const :tag "find -exec {} ;" exec)
                 (const :tag "find -exec {} +" exec-plus)
                 (const :tag "find -print0 | xargs -0" gnu)
                 (const :tag "find -print0 | sort -z | xargs -0'" gnu-sort)
                 string
		 (const :tag "Not Set" nil))
  :set #'grep-apply-setting
  :version "27.1")

;; History of grep commands.
;;;###autoload
(defvar grep-history nil "History list for grep.")
;;;###autoload
(defvar grep-find-history nil "History list for grep-find.")

;; History of lgrep and rgrep regexp and files args.
(defvar grep-regexp-history nil)
(defvar grep-files-history nil)

;;;###autoload
(defun grep-process-setup ()
  "Setup compilation variables and buffer for `grep'.
Set up `compilation-exit-message-function' and run `grep-setup-hook'."
  (when (eq grep-highlight-matches 'auto-detect)
    (grep-compute-defaults))
  (unless (or (eq grep-highlight-matches 'auto-detect)
	      (null grep-highlight-matches)
	      ;; Don't output color escapes if they can't be
	      ;; highlighted with `font-lock-face' by `grep-filter'.
	      (null font-lock-mode))
    ;; `setenv' modifies `process-environment' let-bound in `compilation-start'
    ;; Any TERM except "dumb" allows GNU grep to use `--color=auto'
    (setenv "TERM" "emacs-grep")
    ;; GREP_COLOR is used in GNU grep 2.5.1, but deprecated in later versions
    (setenv "GREP_COLOR" "01;31")
    ;; GREP_COLORS is used in GNU grep 2.5.2 and later versions
    (setenv "GREP_COLORS" "mt=01;31:fn=:ln=:bn=:se=:sl=:cx=:ne"))
  (setq-local grep-num-matches-found 0)
  (setq-local compilation-exit-message-function #'grep-exit-message)
  (run-hooks 'grep-setup-hook))

(defun grep-exit-message (status code msg)
  "Return a status message for grep results."
  (if (eq status 'exit)
      ;; This relies on the fact that `compilation-start'
      ;; sets buffer-modified to nil before running the command,
      ;; so the buffer is still unmodified if there is no output.
      (cond ((and (zerop code) (buffer-modified-p))
	     (if (> grep-num-matches-found 0)
                 (cons (format (ngettext "finished with %d match found\n"
                                         "finished with %d matches found\n"
                                         grep-num-matches-found)
                               grep-num-matches-found)
                       "matched")
               '("finished with matches found\n" . "matched")))
	    ((not (buffer-modified-p))
	     '("finished with no matches found\n" . "no match"))
	    (t
	     (cons msg code)))
    (cons msg code)))

(defun grep-filter ()
  "Handle match highlighting escape sequences inserted by the grep process.
This function is called from `compilation-filter-hook'."
  (save-excursion
    (forward-line 0)
    (let ((end (point)) beg)
      (goto-char compilation-filter-start)
      (forward-line 0)
      (setq beg (point))
      ;; Only operate on whole lines so we don't get caught with part of an
      ;; escape sequence in one chunk and the rest in another.
      (when (< (point) end)
        (setq end (copy-marker end))
        ;; Highlight grep matches and delete marking sequences.
        (while (re-search-forward grep-match-regexp end 1)
          (replace-match (propertize (match-string 1)
                                     'face nil 'font-lock-face grep-match-face)
                         t t)
          (cl-incf grep-num-matches-found))
        ;; Delete all remaining escape sequences
        (goto-char beg)
        (while (re-search-forward "\033\\[[0-9;]*[mK]" end 1)
          (replace-match "" t t))))))

(defun grep-probe (command args &optional func result)
  (let (process-file-side-effects)
    (equal (condition-case nil
	       (apply (or func #'process-file) command args)
	     (error nil))
	   (or result 0))))

(defun grep-hello-file ()
  (let ((result
         (if (file-remote-p default-directory)
             (make-temp-file (file-name-as-directory (temporary-file-directory)))
           (expand-file-name "HELLO" data-directory))))
    (when (file-remote-p result)
      (write-region "Copyright\n" nil result))
    result))

;;;###autoload
(defun grep-compute-defaults ()
  "Compute the defaults for the `grep' command.
The value depends on `grep-command', `grep-template',
`grep-use-null-device', `grep-find-command', `grep-find-template',
`grep-use-null-filename-separator', `grep-find-use-xargs' and
`grep-highlight-matches'."
  ;; Keep default values.
  (unless grep-host-defaults-alist
    (add-to-list
     'grep-host-defaults-alist
     (cons nil
	   `((grep-command ,grep-command)
	     (grep-template ,grep-template)
	     (grep-use-null-device ,grep-use-null-device)
	     (grep-find-command ,grep-find-command)
	     (grep-find-template ,grep-find-template)
             (grep-use-null-filename-separator
              ,grep-use-null-filename-separator)
	     (grep-find-use-xargs ,grep-find-use-xargs)
	     (grep-highlight-matches ,grep-highlight-matches)))))
  (let* ((host-id
	  (intern (or (file-remote-p default-directory) "localhost")))
	 (host-defaults (assq host-id grep-host-defaults-alist))
	 (defaults (assq nil grep-host-defaults-alist))
         (quot-braces (shell-quote-argument "{}"))
         (quot-scolon (shell-quote-argument ";")))
    ;; There are different defaults on different hosts.  They must be
    ;; computed for every host once.
    (dolist (setting '(grep-command grep-template
		       grep-use-null-device grep-find-command
                       grep-use-null-filename-separator
                       grep-find-template grep-find-use-xargs
		       grep-highlight-matches))
      (set setting
	   (cadr (or (assq setting host-defaults)
		     (assq setting defaults)))))

    (unless (or (not grep-use-null-device) (eq grep-use-null-device t))
      (setq grep-use-null-device
	    (with-temp-buffer
	      (let ((hello-file (grep-hello-file)))
                (prog1
		    (not
		     (and (if grep-command
			      ;; `grep-command' is already set, so
			      ;; use that for testing.
			      (grep-probe
                               grep-command
			       `(nil t nil "^Copyright"
                                     ,(file-local-name hello-file))
			       #'process-file-shell-command)
			    ;; otherwise use `grep-program'
			    (grep-probe
                             grep-program
			     `(nil t nil "-nH" "^Copyright"
                                   ,(file-local-name hello-file))))
		          (progn
			    (goto-char (point-min))
			    (looking-at
			     (concat (regexp-quote (file-local-name hello-file))
				     ":[0-9]+:Copyright")))))
                  (when (file-remote-p hello-file) (delete-file hello-file)))))))

    (when (eq grep-use-null-filename-separator 'auto-detect)
      (setq grep-use-null-filename-separator
            (with-temp-buffer
              (let* ((hello-file (grep-hello-file))
                     (args `("--null" "-ne" "^Copyright"
                             ,(file-local-name hello-file))))
                (if grep-use-null-device
                    (setq args (append args (list (null-device))))
                  (push "-H" args))
                (prog1
                    (and (grep-probe grep-program `(nil t nil ,@args))
                         (progn
                           (goto-char (point-min))
                           (looking-at
                            (concat (regexp-quote (file-local-name hello-file))
                                    "\0[0-9]+:Copyright"))))
                  (when (file-remote-p hello-file) (delete-file hello-file)))))))

    (when (eq grep-highlight-matches 'auto-detect)
      (setq grep-highlight-matches
	    (with-temp-buffer
	      (and (grep-probe grep-program '(nil t nil "--help"))
		   (progn
		     (goto-char (point-min))
		     (search-forward "--color" nil t))
		   ;; Windows and DOS pipes fail `isatty' detection in Grep.
		   (if (memq system-type '(windows-nt ms-dos))
		       'always 'auto)))))

    (unless (and grep-command grep-find-command
		 grep-template grep-find-template)
      (let ((grep-options
	     (concat (if grep-use-null-device "-n" "-nH")
                     (if grep-use-null-filename-separator " --null")
                     (when (grep-probe grep-program
                                       `(nil nil nil "-e" "foo" ,(null-device))
                                       nil 1)
                       " -e"))))
	(unless grep-command
	  (setq grep-command
		(format "%s %s %s " grep-program
                        (or
                         (and grep-highlight-matches
                              (grep-probe
                               grep-program
                               `(nil nil nil "--color" "x" ,(null-device))
                               nil 1)
                              (if (eq grep-highlight-matches 'always)
                                  "--color=always" "--color=auto"))
                         "")
                        grep-options)))
	(unless grep-template
	  (setq grep-template
		(format "%s <X> <C> %s <R> <F>" grep-program grep-options)))
	(unless grep-find-use-xargs
	  (setq grep-find-use-xargs
		(cond
		 ((grep-probe find-program
			      `(nil nil nil ,(null-device) "-exec" "echo"
				    "{}" "+"))
		  'exec-plus)
		 ((and
		   (grep-probe
                    find-program `(nil nil nil ,(null-device) "-print0"))
		   (grep-probe xargs-program '(nil nil nil "-0" "echo")))
		  'gnu)
		 (t
		  'exec))))
	(unless grep-find-command
	  (setq grep-find-command
		(cond ((eq grep-find-use-xargs 'gnu)
		       ;; Windows shells need the program file name
		       ;; after the pipe symbol be quoted if they use
		       ;; forward slashes as directory separators.
		       (format "%s . -type f -print0 | \"%s\" -0 %s"
			       find-program xargs-program grep-command))
		      ((eq grep-find-use-xargs 'gnu-sort)
		       (format "%s . -type f -print0 | sort -z | \"%s\" -0 %s"
			       find-program xargs-program grep-command))
		      ((memq grep-find-use-xargs '(exec exec-plus))
		       (let ((cmd0 (format "%s . -type f -exec %s"
					   find-program grep-command))
			     (null (if grep-use-null-device
				       (format "%s " (null-device))
				     "")))
			 (cons
			  (if (eq grep-find-use-xargs 'exec-plus)
			      (format "%s %s%s +" cmd0 null quot-braces)
			    (format "%s %s %s%s"
                                    cmd0 quot-braces null quot-scolon))
			  (1+ (length cmd0)))))
		      (t
		       (format "%s . -type f -print | \"%s\" %s"
			       find-program xargs-program grep-command)))))
	(unless grep-find-template
	  (setq grep-find-template
		(let ((gcmd (format "%s <C> %s <R>"
				    grep-program grep-options))
		      (null (if grep-use-null-device
				(format "%s " (null-device))
			      "")))
		  (cond ((eq grep-find-use-xargs 'gnu)
			 (format "%s <D> <X> -type f <F> -print0 | \"%s\" -0 %s"
				 find-program xargs-program gcmd))
			((eq grep-find-use-xargs 'gnu-sort)
			 (format "%s <D> <X> -type f <F> -print0 | sort -z | \"%s\" -0 %s"
				 find-program xargs-program gcmd))
			((eq grep-find-use-xargs 'exec)
			 (format "%s <D> <X> -type f <F> -exec %s %s %s%s"
				 find-program gcmd quot-braces null quot-scolon))
			((eq grep-find-use-xargs 'exec-plus)
			 (format "%s <D> <X> -type f <F> -exec %s %s%s +"
				 find-program gcmd null quot-braces))
			(t
			 (format "%s <D> <X> -type f <F> -print | \"%s\" %s"
				 find-program xargs-program gcmd))))))))

    ;; Save defaults for this host.
    (setq grep-host-defaults-alist
	  (delete (assq host-id grep-host-defaults-alist)
		  grep-host-defaults-alist))
    (add-to-list
     'grep-host-defaults-alist
     (cons host-id
	   `((grep-command ,grep-command)
	     (grep-template ,grep-template)
	     (grep-use-null-device ,grep-use-null-device)
	     (grep-find-command ,grep-find-command)
	     (grep-find-template ,grep-find-template)
	     (grep-use-null-filename-separator
	      ,grep-use-null-filename-separator)
	     (grep-find-use-xargs ,grep-find-use-xargs)
	     (grep-highlight-matches ,grep-highlight-matches))))))

(defun grep-tag-default ()
  (or (and transient-mark-mode mark-active
	   (/= (point) (mark))
	   (buffer-substring-no-properties (point) (mark)))
      (funcall (or find-tag-default-function
		   (get major-mode 'find-tag-default-function)
		   #'find-tag-default))
      ""))

(defun grep-default-command ()
  "Compute the default grep command for \\[universal-argument] \\[grep] to offer."
  (let ((tag-default (shell-quote-argument (grep-tag-default)))
	;; This a regexp to match single shell arguments.
	;; Could someone please add comments explaining it?
	(sh-arg-re
         "\\(\\(?:\"\\(?:[^\"]\\|\\\\\"\\)+\"\\|'[^']+'\\|[^\"' \t\n]\\)+\\)")
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
  (setq-local tool-bar-map grep-mode-tool-bar-map)
  (setq-local compilation-error-face
              grep-hit-face)
  (setq-local compilation-error-regexp-alist
              grep-regexp-alist)
  (setq-local compilation-mode-line-errors
              grep-mode-line-matches)
  ;; compilation-directory-matcher can't be nil, so we set it to a regexp that
  ;; can never match.
  (setq-local compilation-directory-matcher
              (list regexp-unmatchable))
  (setq-local compilation-process-setup-function
              #'grep-process-setup)
  (setq-local compilation-disable-input t)
  (setq-local compilation-error-screen-columns
              grep-error-screen-columns)
  (add-hook 'compilation-filter-hook #'grep-filter nil t))

(defun grep--save-buffers ()
  (when grep-save-buffers
    (save-some-buffers (and (not (eq grep-save-buffers 'ask))
                            (not (functionp grep-save-buffers)))
                       (and (functionp grep-save-buffers)
                            grep-save-buffers))))

;;;###autoload
(defun grep (command-args)
  "Run Grep with user-specified COMMAND-ARGS.
The output from the command goes to the \"*grep*\" buffer.

While Grep runs asynchronously, you can use \\[next-error] (M-x next-error),
or \\<grep-mode-map>\\[compile-goto-error] in the *grep* \
buffer, to go to the lines where Grep found
matches.  To kill the Grep job before it finishes, type \\[kill-compilation].

Noninteractively, COMMAND-ARGS should specify the Grep command-line
arguments.

For doing a recursive `grep', see the `rgrep' command.  For running
Grep in a specific directory, see `lgrep'.

This command uses a special history list for its COMMAND-ARGS, so you
can easily repeat a grep command.

A prefix argument says to default the COMMAND-ARGS based on the current
tag the cursor is over, substituting it into the last Grep command
in the Grep command history (or into `grep-command' if that history
list is empty)."
  (interactive
   (progn
     (grep-compute-defaults)
     (let ((default (grep-default-command)))
       (list (read-shell-command "Run grep (like this): "
                                 (if current-prefix-arg default grep-command)
                                 'grep-history
                                 (if current-prefix-arg nil default))))))

  (grep--save-buffers)
  ;; Setting process-setup-function makes exit-message-function work
  ;; even when async processes aren't supported.
  (compilation-start (if (and grep-use-null-device null-device (null-device))
			 (concat command-args " " (null-device))
		       command-args)
		     #'grep-mode))


;;;###autoload
(defun grep-find (command-args)
  "Run grep via find, with user-specified args COMMAND-ARGS.
Collect output in the \"*grep*\" buffer.
While find runs asynchronously, you can use the \\[next-error] command
to find the text that grep hits refer to.

This command uses a special history list for its arguments, so you can
easily repeat a find command."
  (interactive
   (progn
     (grep-compute-defaults)
     (if grep-find-command
	 (list (read-shell-command "Run find (like this): "
                                   grep-find-command 'grep-find-history))
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
  '(("<C>" . (mapconcat #'identity opts " "))
    ("<D>" . (or dir "."))
    ("<F>" . files)
    ("<N>" . (null-device))
    ("<X>" . excl)
    ("<R>" . (shell-quote-argument (or regexp ""))))
  "List of substitutions performed by `grep-expand-template'.
If car of an element matches, the cdr is evalled in order to get the
substitution string.

The substitution is based on variables bound dynamically, and
these include `opts', `dir', `files', `null-device', `excl' and
`regexp'.")

(defun grep-expand-template (template &optional regexp files dir excl more-opts)
  "Expand grep COMMAND string replacing <C>, <D>, <F>, <R>, and <X>."
  (let* ((command template)
         (env `((opts . ,(let ((opts more-opts))
                           (when (and case-fold-search
                                      (isearch-no-upper-case-p regexp t))
                             (push "-i" opts))
                           (cond
                            ((eq grep-highlight-matches 'always)
                             (push "--color=always" opts))
                            ((eq grep-highlight-matches 'auto)
                             (push "--color=auto" opts)))
                           opts))
                (excl . ,excl)
                (dir . ,dir)
                (files . ,files)
                (regexp . ,regexp)))
         (case-fold-search nil))
    (dolist (kw grep-expand-keywords command)
      (if (string-match (car kw) command)
	  (setq command
		(replace-match
		 (or (if (symbolp (cdr kw))
			 (eval (cdr kw) env)
		       (save-match-data (eval (cdr kw) env)))
		     "")
		 t t command))))))

(defun grep-read-regexp ()
  "Read regexp arg for interactive grep using `read-regexp'."
  (read-regexp "Search for" 'grep-tag-default 'grep-regexp-history))

(defvar grep-read-files-function #'grep-read-files--default)

(defun grep-read-files--default ()
  ;; Instead of a `grep-read-files-function' variable, we used to lookup
  ;; mode-specific functions in the major mode's symbol properties, so preserve
  ;; this behavior for backward compatibility.
  (let ((old-function (get major-mode 'grep-read-files))) ;Obsolete since 28.1
    (if old-function
	(funcall old-function)
      (let ((file-name-at-point
	     (run-hook-with-args-until-success 'file-name-at-point-functions)))
	(or (if (and (stringp file-name-at-point)
		     (not (file-directory-p file-name-at-point)))
		file-name-at-point)
	    (buffer-file-name)
	    (replace-regexp-in-string "<[0-9]+>\\'" "" (buffer-name)))))))

(defun grep-read-files (regexp)
  "Read a file-name pattern arg for interactive grep.
The pattern can include shell wildcards.  As SPC can triggers
completion when entering a pattern, including it requires
quoting, e.g. `\\[quoted-insert]<space>'.

REGEXP is used as a string in the prompt."
  (let* ((bn (funcall grep-read-files-function))
	 (fn (and bn
		  (stringp bn)
		  (file-name-nondirectory bn)))
	 (default-alias
	   (and fn
		(let ((aliases (remove (assoc "all" grep-files-aliases)
				       grep-files-aliases))
		      alias)
		  (while aliases
		    (setq alias (car aliases)
			  aliases (cdr aliases))
		    (if (string-match (mapconcat
				       #'wildcard-to-regexp
				       (split-string (cdr alias) nil t)
				       "\\|")
				      fn)
			(setq aliases nil)
		      (setq alias nil)))
		  (cdr alias))))
	 (default-extension
	   (and fn
		(let ((ext (file-name-extension fn)))
		  (and ext (concat "*." ext)))))
	 (default
	   (or default-alias
	       default-extension
	       (car grep-files-history)
	       (car (car grep-files-aliases))))
	 (files (completing-read
		 (concat "Search for \"" regexp
			 "\" in files matching wildcard"
			 (if default (concat " (default " default ")"))
			 ": ")
		 #'read-file-name-internal
		 nil nil nil 'grep-files-history
		 (delete-dups
		  (delq nil
                        (append (list default default-alias default-extension)
				(mapcar #'car grep-files-aliases)))))))
    (and files
	 (or (cdr (assoc files grep-files-aliases))
	     files))))

(defvar grep-use-directories-skip 'auto-detect)

;;;###autoload
(defun lgrep (regexp &optional files dir confirm)
  "Run grep, searching for REGEXP in FILES in directory DIR.
The search is limited to file names matching shell pattern FILES.
FILES may use abbreviations defined in `grep-files-aliases', e.g.
entering `ch' is equivalent to `*.[ch]'.  As whitespace triggers
completion when entering a pattern, including it requires
quoting, e.g. `\\[quoted-insert]<space>'.

With \\[universal-argument] prefix, you can edit the constructed shell command line
before it is executed.
With two \\[universal-argument] prefixes, directly edit and run `grep-command'.

Collect output in the \"*grep*\" buffer.  While grep runs asynchronously, you
can use \\[next-error] (M-x next-error), or \\<grep-mode-map>\\[compile-goto-error] \
in the grep output buffer,
to go to the lines where grep found matches.

This command shares argument histories with \\[rgrep] and \\[grep].

If CONFIRM is non-nil, the user will be given an opportunity to edit the
command before it's run."
  (interactive
   (progn
     (grep-compute-defaults)
     (cond
      ((and grep-command (equal current-prefix-arg '(16)))
       (list (read-from-minibuffer "Run: " grep-command
				   nil nil 'grep-history)))
      ((not grep-template)
       (error "grep.el: No `grep-template' available"))
      (t (let* ((regexp (grep-read-regexp))
		(files (grep-read-files regexp))
		(dir (read-directory-name "In directory: "
					  nil default-directory t))
		(confirm (equal current-prefix-arg '(4))))
	   (list regexp files dir confirm))))))
  (when (and (stringp regexp) (> (length regexp) 0))
    (unless (and dir (file-accessible-directory-p dir))
      (setq dir default-directory))
    (let ((command regexp))
      (if (null files)
	  (if (string= command grep-command)
	      (setq command nil))
	(setq dir (file-name-as-directory (expand-file-name dir)))
	(unless (or (not grep-use-directories-skip)
                    (eq grep-use-directories-skip t))
	  (setq grep-use-directories-skip
		(grep-probe grep-program
			  `(nil nil nil "--directories=skip" "foo"
				,(null-device))
			  nil 1)))
	(setq command (grep-expand-template
		       grep-template
		       regexp
		       files
		       nil
		       (and grep-find-ignored-files
			    (concat " --exclude="
				    (mapconcat
				     #'(lambda (ignore)
					 (cond ((stringp ignore)
						(shell-quote-argument ignore))
					       ((consp ignore)
						(and (funcall (car ignore) dir)
						     (shell-quote-argument
						      (cdr ignore))))))
				     grep-find-ignored-files
				     " --exclude=")))
		       (and (eq grep-use-directories-skip t)
			    '("--directories=skip"))))
	(when command
	  (if confirm
	      (setq command
		    (read-from-minibuffer "Confirm: "
					  command nil nil 'grep-history))
	    (add-to-history 'grep-history command))))
      (when command
	(let ((default-directory dir))
	  ;; Setting process-setup-function makes exit-message-function work
	  ;; even when async processes aren't supported.
          (grep--save-buffers)
	  (compilation-start
           (if (and grep-use-null-device null-device (null-device))
	       (concat command " " (null-device))
	     command)
	   'grep-mode))
	;; Set default-directory if we started lgrep in the *grep* buffer.
	(if (eq next-error-last-buffer (current-buffer))
	    (setq default-directory dir))))))


(defvar find-name-arg)	    ; not autoloaded but defined in find-dired

;;;###autoload
(defun rgrep (regexp &optional files dir confirm)
  "Recursively grep for REGEXP in FILES in directory tree rooted at DIR.
The search is limited to file names matching shell pattern FILES.
FILES may use abbreviations defined in `grep-files-aliases', e.g.
entering `ch' is equivalent to `*.[ch]'.  As whitespace triggers
completion when entering a pattern, including it requires
quoting, e.g. `\\[quoted-insert]<space>'.

With \\[universal-argument] prefix, you can edit the constructed shell command line
before it is executed.
With two \\[universal-argument] prefixes, directly edit and run `grep-find-command'.

Collect output in the \"*grep*\" buffer.  While the recursive grep is running,
you can use \\[next-error] (M-x next-error), or \\<grep-mode-map>\\[compile-goto-error] \
in the grep output buffer,
to visit the lines where matches were found.  To kill the job
before it finishes, type \\[kill-compilation].

This command shares argument histories with \\[lgrep] and \\[grep-find].

When called programmatically and FILES is nil, REGEXP is expected
to specify a command to run.

If CONFIRM is non-nil, the user will be given an opportunity to edit the
command before it's run."
  (interactive
   (progn
     (grep-compute-defaults)
     (cond
      ((and grep-find-command (equal current-prefix-arg '(16)))
       (list (read-from-minibuffer "Run: " grep-find-command
				   nil nil 'grep-find-history)))
      ((not grep-find-template)
       (error "grep.el: No `grep-find-template' available"))
      (t (let* ((regexp (grep-read-regexp))
		(files (grep-read-files regexp))
		(dir (read-directory-name "Base directory: "
					  nil default-directory t))
		(confirm (equal current-prefix-arg '(4))))
	   (list regexp files dir confirm))))))
  (when (and (stringp regexp) (> (length regexp) 0))
    (unless (and dir (file-accessible-directory-p dir))
      (setq dir default-directory))
    (if (null files)
	(if (not (string= regexp (if (consp grep-find-command)
				     (car grep-find-command)
				   grep-find-command)))
	    (compilation-start regexp 'grep-mode))
      (setq dir (file-name-as-directory (expand-file-name dir)))
      (let ((command (rgrep-default-command regexp files nil)))
	(when command
	  (if confirm
	      (setq command
		    (read-from-minibuffer "Confirm: "
					  command nil nil 'grep-find-history))
	    (add-to-history 'grep-find-history command))
          (grep--save-buffers)
	  (let ((default-directory dir))
	    (compilation-start command 'grep-mode))
	  ;; Set default-directory if we started rgrep in the *grep* buffer.
	  (if (eq next-error-last-buffer (current-buffer))
	      (setq default-directory dir)))))))

(defun rgrep-find-ignored-directories (dir)
  "Return the list of ignored directories applicable to `dir'."
  (delq nil (mapcar
             (lambda (ignore)
               (cond ((stringp ignore) ignore)
                     ((consp ignore)
                      (and (funcall (car ignore) dir) (cdr ignore)))))
             grep-find-ignored-directories)))

(defun rgrep-default-command (regexp files dir)
  "Compute the command for \\[rgrep] to use by default."
  (require 'find-dired)      ; for `find-name-arg'
  (grep-expand-template
   grep-find-template
   regexp
   (concat (shell-quote-argument "(")
           " " find-name-arg " "
           (mapconcat
            #'shell-quote-argument
            (split-string files)
            (concat " -o " find-name-arg " "))
           " "
           (shell-quote-argument ")"))
   dir
   (concat
    (and grep-find-ignored-directories
         (concat "-type d "
                 (shell-quote-argument "(")
                 ;; we should use shell-quote-argument here
                 " -path "
                 (mapconcat (lambda (d) (shell-quote-argument (concat "*/" d)))
                            (rgrep-find-ignored-directories dir)
                            " -o -path ")
                 " "
                 (shell-quote-argument ")")
                 " -prune -o "))
    (and grep-find-ignored-files
         (concat (shell-quote-argument "!") " -type d "
                 (shell-quote-argument "(")
                 ;; we should use shell-quote-argument here
                 " -name "
                 (mapconcat
                  #'(lambda (ignore)
                      (cond ((stringp ignore)
                             (shell-quote-argument ignore))
                            ((consp ignore)
                             (and (funcall (car ignore) dir)
                                  (shell-quote-argument
                                   (cdr ignore))))))
                  grep-find-ignored-files
                  " -o -name ")
                 " "
                 (shell-quote-argument ")")
                 " -prune -o ")))))

(defun grep-find-toggle-abbreviation ()
  "Toggle showing the hidden part of rgrep/lgrep/zrgrep command line."
  (interactive)
  (with-silent-modifications
    (let* ((beg (next-single-property-change (point-min) 'abbreviated-command))
           (end (when beg
                  (next-single-property-change beg 'abbreviated-command))))
      (if end
          (if (get-text-property beg 'display)
              (remove-list-of-text-properties
               beg end '(display help-echo mouse-face help-echo keymap))
            (add-text-properties beg end grep-find-abbreviate-properties))
        (user-error "No abbreviated part to hide/show")))))

;;;###autoload
(defun zrgrep (regexp &optional files dir confirm template)
  "Recursively grep for REGEXP in gzipped FILES in tree rooted at DIR.
Like `rgrep' but uses `zgrep' for `grep-program', sets the default
file name to `*.gz', and sets `grep-highlight-matches' to `always'.

If CONFIRM is non-nil, the user will be given an opportunity to edit the
command before it's run."
  (interactive
   (progn
     ;; Compute standard default values.
     (grep-compute-defaults)
     ;; Compute the default zrgrep command by running `grep-compute-defaults'
     ;; for grep program "zgrep", but not changing global values.
     (let ((grep-program "zgrep")
	   ;; Don't change global values for variables computed
	   ;; by `grep-compute-defaults'.
	   (grep-find-template nil)
	   (grep-find-command nil)
	   (grep-host-defaults-alist nil)
           ;; `zgrep' doesn't support the `--null' option.
	   (grep-use-null-filename-separator nil)
	   ;; Use for `grep-read-files'
	   (grep-files-aliases '(("all" . "* .*")
				 ("gz"  . "*.gz"))))
       ;; Recompute defaults using let-bound values above.
       (grep-compute-defaults)
       (cond
	((and grep-find-command (equal current-prefix-arg '(16)))
	 (list (read-from-minibuffer "Run: " grep-find-command
				     nil nil 'grep-find-history)))
	((not grep-find-template)
	 (error "grep.el: No `grep-find-template' available"))
	(t (let* ((regexp (grep-read-regexp))
		  (files (grep-read-files regexp))
		  (dir (read-directory-name "Base directory: "
					    nil default-directory t))
		  (confirm (equal current-prefix-arg '(4))))
	     (list regexp files dir confirm grep-find-template)))))))
  (let ((grep-find-template template)
        ;; Set `grep-highlight-matches' to `always'
        ;; since `zgrep' puts filters in the grep output.
        (grep-highlight-matches 'always))
    (rgrep regexp files dir confirm)))

;;;###autoload
(defalias 'rzgrep 'zrgrep)

(provide 'grep)

;;; grep.el ends here
