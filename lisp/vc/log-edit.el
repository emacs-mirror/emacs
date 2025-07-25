;;; log-edit.el --- Major mode for editing CVS commit messages -*- lexical-binding: t -*-

;; Copyright (C) 1999-2025 Free Software Foundation, Inc.

;; Author: Stefan Monnier <monnier@iro.umontreal.ca>
;; Keywords: pcl-cvs cvs commit log vc

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

;; Todo:

;; - Move in VC's code
;; - Add compatibility for VC's hook variables

;;; Code:

(require 'add-log)			; for all the ChangeLog goodies
(require 'pcvs-util)
(require 'ring)
(require 'message)

;;;;
;;;; Global Variables
;;;;

(defgroup log-edit nil
  "Major mode for editing RCS and CVS commit messages."
  :group 'pcl-cvs
  :group 'vc				; It's used by VC.
  :version "21.1"
  :prefix "log-edit-")

;; compiler pacifiers
(defvar cvs-buffer)


;; The main keymap

(define-obsolete-variable-alias 'vc-log-mode-map 'log-edit-mode-map "28.1")
(define-obsolete-variable-alias 'vc-log-entry-mode 'log-edit-mode-map "28.1")

(defvar-keymap log-edit-mode-map
  "C-c C-c" #'log-edit-done
  "C-c C-a" #'log-edit-insert-changelog
  "C-c C-w" #'log-edit-generate-changelog-from-diff
  "C-c C-d" #'log-edit-show-diff
  "C-c C-f" #'log-edit-show-files
  "C-c C-k" #'log-edit-kill-buffer
  "M-n"     #'log-edit-next-comment
  "M-p"     #'log-edit-previous-comment
  "M-r"     #'log-edit-comment-search-backward
  "M-s"     #'log-edit-comment-search-forward
  "C-c ?"   #'log-edit-mode-help
  "<remap> <move-beginning-of-line>" #'log-edit-beginning-of-line)

(easy-menu-define log-edit-menu log-edit-mode-map
  "Menu used for `log-edit-mode'."
  '("Log-Edit"
    ["Done" log-edit-done
     :help "Exit log-edit and proceed with the actual action."]
    "--"
    ["Insert ChangeLog" log-edit-insert-changelog
     :help "Insert a log message by looking at the ChangeLog"]
    ["Generate ChangeLog" log-edit-generate-changelog-from-diff
     :help "Generate a log message from the diff and insert it into this buffer"]
    ["Add to ChangeLog" log-edit-add-to-changelog
     :help "Insert this log message into the appropriate ChangeLog file"]
    "--"
    ["Show diff" log-edit-show-diff
     :help "Show the diff for the files to be committed."]
    ["List files" log-edit-show-files
     :help "Show the list of relevant files."]
    "--"
    ["Previous comment"		log-edit-previous-comment
     :help "Cycle backwards through comment history"]
    ["Next comment"		log-edit-next-comment
     :help "Cycle forwards through comment history."]
    ["Search comment forward"	log-edit-comment-search-forward
     :help "Search forwards through comment history for a substring match of str"]
    ["Search comment backward"	log-edit-comment-search-backward
     :help "Search backwards through comment history for substring match of str"]))

(defvar log-edit-tool-bar-map
  (let ((map (make-sparse-keymap)))
    (tool-bar-local-item-from-menu 'find-file "new" map
                                   nil :label "New File"
			           :vert-only t)
    (tool-bar-local-item-from-menu 'menu-find-file-existing "open" map
                                   nil :label "Open" :vert-only t)
    (tool-bar-local-item-from-menu 'dired "diropen" map nil :vert-only t)
    (tool-bar-local-item-from-menu 'kill-this-buffer "close" map nil
                                   :vert-only t)
    (define-key-after map [separator-1] menu-bar-separator)
    (tool-bar-local-item-from-menu 'log-edit-done "commit"
                                   map log-edit-mode-map :vert-only t
                                   :help
                                   "Exit log buffer and commit the changes")
    (define-key-after map [separator-2] menu-bar-separator)
    (tool-bar-local-item-from-menu 'log-edit-insert-changelog
                                   "load-changelog"
                                   map log-edit-mode-map :vert-only t
                                   :help
                                   "Produce log message from ChangeLog file")
    (tool-bar-local-item-from-menu 'log-edit-generate-changelog-from-diff
                                   "gen-changelog"
                                   map log-edit-mode-map :vert-only t
                                   :help
                                   "Generate log message skeleton from diffs")
    (tool-bar-local-item-from-menu 'log-edit-add-to-changelog
                                   "ins-changelog"
                                   map log-edit-mode-map :vert-only t
                                   :help
                                   "Insert this log message into ChangeLog file")
    (define-key-after map [separator-3] menu-bar-separator)
    (tool-bar-local-item-from-menu 'log-edit-show-diff
                                   "view-diff"
                                   map log-edit-mode-map :vert-only t
                                   :help
                                   "View diffs for the files to be committed")
    (tool-bar-local-item-from-menu 'log-edit-show-files
                                   "info"
                                   map log-edit-mode-map :vert-only t
                                   :help
                                   "View list of files to be committed")
    (define-key-after map [separator-4] menu-bar-separator)
    (tool-bar-local-item-from-menu 'undo "undo" map nil)
    (define-key-after map [separator-5] menu-bar-separator)
    (tool-bar-local-item-from-menu (lookup-key menu-bar-edit-menu [cut])
                                   "cut" map nil)
    (tool-bar-local-item-from-menu (lookup-key menu-bar-edit-menu [copy])
                                   "copy" map nil)
    (tool-bar-local-item-from-menu (lookup-key menu-bar-edit-menu [paste])
                                   "paste" map nil)
    map)
  "Like the default `tool-bar-map', but with additions for Log-Edit mode.")

(defcustom log-edit-confirm 'changed
  "If non-nil, `log-edit-done' will request confirmation.
If `changed', only request confirmation if the list of files has
  changed since the beginning of the `log-edit' session."
  :group 'log-edit
  :type '(choice (const changed) (const t) (const nil)))

(defcustom log-edit-keep-buffer nil
  "If non-nil, don't hide the buffer after `log-edit-done'."
  :group 'log-edit
  :type 'boolean)

(defcustom log-edit-require-final-newline t
  "Enforce a newline at the end of commit log messages.
Enforce it silently if t, query if non-nil and don't do anything if nil."
  :group 'log-edit
  :type '(choice (const ask) (const t) (const nil)))

(defcustom log-edit-setup-invert nil
  "Non-nil means `log-edit' should invert the meaning of its SETUP arg.
If SETUP is `force', this variable has no effect."
  :group 'log-edit
  :type 'boolean)

(defcustom log-edit-setup-add-author nil
  "Non-nil means `log-edit' may add the `Author:' header.
This applies when its SETUP argument is non-nil."
  :version "24.4"
  :group 'log-edit
  :type 'boolean
  :safe 'booleanp)

(defcustom log-edit-hook '(log-edit-insert-message-template
			   log-edit-insert-cvs-template
			   log-edit-insert-changelog
			   log-edit-show-files)
  "Hook run at the end of `log-edit'."
  ;; Added `log-edit-maybe-show-diff'.
  :version "31.1"
  :group 'log-edit
  :type '(hook :options (log-edit-insert-message-template
			 log-edit-insert-cvs-rcstemplate
			 log-edit-insert-cvs-template
			 log-edit-insert-changelog
			 log-edit-insert-filenames
			 log-edit-insert-filenames-without-changelog
			 log-edit-show-files
                         log-edit-maybe-show-diff)))

(defcustom log-edit-mode-hook (if (boundp 'vc-log-mode-hook) vc-log-mode-hook)
  "Hook run when entering `log-edit-mode'."
  :group 'log-edit
  :type 'hook)

(defcustom log-edit-done-hook nil
  "Hook run before doing the actual commit.
This hook can be used to cleanup the message, enforce various
conventions, or to allow recording the message in some other database,
such as a bug-tracking system.  The list of files about to be committed
can be obtained from `log-edit-files'."
  :group 'log-edit
  :type '(hook :options (log-edit-set-common-indentation
                         log-edit-add-to-changelog
                         log-edit-done-strip-cvs-lines)))

(defcustom log-edit-strip-single-file-name nil
  "If non-nil, remove file name from single-file log entries."
  :type 'boolean
  :safe 'booleanp
  :group 'log-edit
  :version "24.1")

(defvar log-edit-changelog-full-paragraphs t
  "If non-nil, include full ChangeLog paragraphs in the log.
This may be set in the \"local variables\" section of a ChangeLog, to
indicate the policy for that ChangeLog.

A ChangeLog paragraph is a bunch of log text containing no blank lines;
a paragraph usually describes a set of changes with a single purpose,
but perhaps spanning several functions in several files.  Changes in
different paragraphs are unrelated.

You could argue that the log entry for a file should contain the
full ChangeLog paragraph mentioning the change to the file, even though
it may mention other files, because that gives you the full context you
need to understand the change.  This is the behavior you get when this
variable is set to t.

On the other hand, you could argue that the log entry for a change
should contain only the text for the changes which occurred in that
file, because the log is per-file.  This is the behavior you get
when this variable is set to nil.")

;;;; Internal global or buffer-local vars

(defconst log-edit-files-buf "*log-edit-files*")
(defvar log-edit-initial-files nil)
(defvar log-edit-callback nil)
(defvar log-edit-diff-function
  (lambda () (error "Diff functionality has not been set up"))
  "Function to display an appropriate `diff-mode' buffer for the change.
Called by `log-edit-show-diff' and `log-edit-maybe-show-diff'.
The function should display the buffer in a window and leave that window
selected when it returns, probably by calling `pop-to-buffer'.")
(defvar log-edit-listfun nil)

(defvar log-edit-parent-buffer nil)

(defvar log-edit-vc-backend nil
  "VC fileset corresponding to the current log.")

;;; Originally taken from VC-Log mode

(defconst log-edit-maximum-comment-ring-size 32
  "Maximum number of saved commit comments in `log-edit-comment-ring'.")
(defvar log-edit-comment-ring (make-ring log-edit-maximum-comment-ring-size))
(defvar log-edit-comment-ring-index nil)
(defvar log-edit-last-comment-match "")

(defun log-edit-new-comment-index (stride len)
  "Return the comment whose index is STRIDE elements away from the current one.
This accesses `log-edit-comment-ring', which stores commit log comments,
i.e. descriptions of changes done by commits.
LEN is the length of `log-edit-comment-ring'."
  (mod (cond
	(log-edit-comment-ring-index (+ log-edit-comment-ring-index stride))
	;; Initialize the index on the first use of this command
	;; so that the first M-p gets index 0, and the first M-n gets
	;; index -1.
	((> stride 0) (1- stride))
	(t stride))
       len))

(defun log-edit-previous-comment (arg)
  "Cycle backwards through VC commit comment history.
With a numeric prefix ARG, go back ARG comments."
  (interactive "*p")
  (let ((len (ring-length log-edit-comment-ring)))
    (if (<= len 0)
	(progn (message "Empty comment ring") (ding))
      ;; Don't use `erase-buffer' because we don't want to `widen'.
      (delete-region (point-min) (point-max))
      (setq log-edit-comment-ring-index (log-edit-new-comment-index arg len))
      (message "Comment %d" (1+ log-edit-comment-ring-index))
      (insert (ring-ref log-edit-comment-ring log-edit-comment-ring-index)))))

(defun log-edit-next-comment (arg)
  "Cycle forwards through VC commit comment history.
With a numeric prefix ARG, go forward ARG comments."
  (interactive "*p")
  (log-edit-previous-comment (- arg)))

(defun log-edit-comment-search-backward (str &optional stride)
  "Search backwards through VC commit comment history for a match of STR.
If the optional argument STRIDE is present, that is a step-width to use
when going through the comment ring, `log-edit-comment-ring'."
  ;; Why substring rather than regexp ?   -sm
  (interactive
   (list (read-string (format-prompt "Comment substring"
                                     log-edit-last-comment-match)
                      nil nil log-edit-last-comment-match)))
  (unless stride (setq stride 1))
  (if (string= str "")
      (setq str log-edit-last-comment-match)
    (setq log-edit-last-comment-match str))
  (let* ((str (regexp-quote str))
	 (len (ring-length log-edit-comment-ring))
	 (n (log-edit-new-comment-index stride len)))
    (while (progn (when (or (>= n len) (< n 0)) (error "Not found"))
		  (not (string-match str (ring-ref log-edit-comment-ring n))))
      (setq n (+ n stride)))
    (setq log-edit-comment-ring-index n)
    (log-edit-previous-comment 0)))

(defun log-edit-comment-search-forward (str)
  "Search forwards through VC commit comment history for a match of STR."
  (interactive
   (list (read-string (format-prompt "Comment substring"
                                     log-edit-last-comment-match)
                      nil nil log-edit-last-comment-match)))
  (log-edit-comment-search-backward str -1))

(defun log-edit-comment-to-change-log (&optional whoami file-name)
  "Insert the last VC commit comment into the change log for the current file.
This reuses the text of the last VC commit comment in `log-edit-comment-ring'
for the change-log entry of the current file, which is handy when several
related changes have the same commit comment.
WHOAMI (interactively, prefix argument) non-nil means prompt for user name
and email address of the person to whom to attribute the change.
FILE-NAME is the name of the change log; if nil, use `change-log-default-name'
Interactively, with prefix argument, prompt for both the name and address of
the person who did the change and for FILE-NAME.

This may be useful as a `vc-checkin-hook' to update change logs
automatically."
  (interactive (if current-prefix-arg
		   (list current-prefix-arg
			 (prompt-for-change-log-name))))
  (let (;; Extract the comment first so we get any error before doing anything.
	(comment (ring-ref log-edit-comment-ring 0))
	;; Don't let add-change-log-entry insert a defun name.
	(add-log-current-defun-function 'ignore)
	end)
    ;; Call add-log to do half the work.
    (add-change-log-entry whoami file-name t t)
    ;; Insert the VC comment, leaving point before it.
    (setq end (save-excursion (insert comment) (point-marker)))
    (if (looking-at "\\s *\\s(")
	;; It starts with an open-paren, as in "(foo): Frobbed."
	;; So remove the ": " add-log inserted.
	(delete-char -2))
    ;; Canonicalize the white space between the file name and comment.
    (just-one-space)
    ;; Indent rest of the text the same way add-log indented the first line.
    (let ((indentation (current-indentation)))
      (save-excursion
	(while (< (point) end)
	  (forward-line 1)
	  (indent-to indentation))
	(setq end (point))))
    ;; Fill the inserted text, preserving open-parens at bol.
    (let ((paragraph-start (concat paragraph-start "\\|\\s *\\s(")))
      (beginning-of-line)
      (fill-region (point) end))
    ;; Canonicalize the white space at the end of the entry so it is
    ;; separated from the next entry by a single blank line.
    (skip-syntax-forward " " end)
    (delete-char (- (skip-syntax-backward " ")))
    (or (eobp) (looking-at "\n\n")
	(insert "\n"))))

;;;
;;; Actual code
;;;

(defface log-edit-summary '((t :inherit font-lock-function-name-face))
  "Face for the summary in `log-edit-mode' buffers.")

(defface log-edit-headers-separator
  '((t :height 0.1 :inverse-video t :extend t))
  "Face for the separator line in `log-edit-mode' buffers."
  :version "29.1")

(defface log-edit-header '((t :inherit font-lock-keyword-face))
  "Face for the headers in `log-edit-mode' buffers.")

(defface log-edit-unknown-header '((t :inherit font-lock-comment-face))
  "Face for unknown headers in `log-edit-mode' buffers.")

(defvar log-edit-headers-alist '(("Summary" . log-edit-summary)
                                 ("Fixes") ("Author"))
  "Alist of known headers and the face to use to highlight them.")

(defconst log-edit-header-contents-regexp
  "[ \t]*\\(.*\\(\n[ \t].*\\)*\\)\n?"
  "Regular expression matching the header field in `log-edit-mode'.
The first subexpression is the actual text of the field.")

(defun log-edit-match-to-eoh (_limit)
  ;; FIXME: copied from message-match-to-eoh.
  (let ((start (point)))
    (rfc822-goto-eoh)
    ;; Typical situation: some temporary change causes the header to be
    ;; incorrect, so EOH comes earlier than intended: the last lines of the
    ;; intended headers are now not considered part of the header any more,
    ;; so they don't have the multiline property set.  When the change is
    ;; completed and the header has its correct shape again, the lack of the
    ;; multiline property means we won't rehighlight the last lines of
    ;; the header.
    (if (< (point) start)
        nil                             ;No header within start..limit.
      ;; Here we disregard LIMIT so that we may extend the area again.
      (set-match-data (list start (point)))
      (point))))

(defun log-edit-goto-eoh ()             ;FIXME: Almost rfc822-goto-eoh!
  (goto-char (point-min))
  (when (re-search-forward
	 "^\\([^[:alpha:]]\\|[[:alnum:]-]+[^[:alnum:]-]\\)" nil 'move)
    (goto-char (match-beginning 0))))

(defun log-edit--match-first-line (limit)
  (let ((start (point)))
    (log-edit-goto-eoh)
    (skip-chars-forward "\n")
    (and (< start (line-end-position))
         (< (point) limit)
         (save-excursion
           (not (re-search-backward "^Summary:[ \t]*[^ \t\n]" nil t)))
         (looking-at ".+")
         (progn
           (goto-char (match-end 0))
           (put-text-property (point-min) (point)
                              'jit-lock-defer-multiline t)
           (point)))))

(defvar log-edit-font-lock-keywords
  ;; Copied/inspired by message-font-lock-keywords.
  `((log-edit-match-to-eoh
     (,(concat "^\\(\\([[:alpha:]-]+\\):\\)" log-edit-header-contents-regexp)
      (progn (goto-char (match-beginning 0)) (match-end 0)) nil
      (1 (if (assoc-string (match-string 2) log-edit-headers-alist t)
             'log-edit-header
           'log-edit-unknown-header)
         nil lax)
      ;; From `log-edit-header-contents-regexp':
      (3 (or (cdr (assoc-string (match-string 2) log-edit-headers-alist t))
             'log-edit-header)
         nil lax))
     ("^\n"
      (and
       ;; This fixes a bug with `git-commit-mode', a NonGNU ELPA package
       ;; used by Magit.  Without this check, we get a wrong display
       ;; when `git-commit-major-mode' is set to `log-edit-mode'.
       (not (bound-and-true-p git-commit-mode))
       (progn (goto-char (match-end 0)) (1+ (match-end 0))))
      nil
      (0 '( face log-edit-headers-separator
            display-line-numbers-disable t rear-nonsticky t))))
    (log-edit--match-first-line (0 'log-edit-summary))))

(defvar log-edit-font-lock-gnu-style nil
  "If non-nil, highlight common failures to follow VC commit log conventions.
The conventions checked are those described in the GNU coding standards
document.")
(put 'log-edit-font-lock-gnu-style 'safe-local-variable 'booleanp)

(defconst log-edit-font-lock-gnu-keywords
    ;; Use
    ;;   * foo.el (bla, bli)
    ;;   (blo, blu): Toto.
    ;; Rather than
    ;;   * foo.el (bla, bli,
    ;;   blo, blu): Toto.
  '(("^[ \t]*\\(?:\\* .*\\)?\\(([^\n)]*,\\s-*\\)$"
     (1 '(face font-lock-warning-face
          help-echo "Continue function lists with \")\\n(\".") t))
    ;; Don't leave a lone word on a single line.
    ;;("^\\s-*\\(\\S-*[^\n:)]\\)\\s-*$" (1 font-lock-warning-face t))
    ;; Don't cut a sentence right after the first word (better to move
    ;; the sentence on the next line, then).
    ;;("[.:]\\s-+\\(\\sw+\\)\\s-*$" (1 font-lock-warning-face t))
    ;; Change Log entries should use present tense.
    ("):[ \t\n]*[[:alpha:]]+\\(ed\\)\\>"
     (1 '(face font-lock-warning-face help-echo "Use present tense.") t))
    ;; Change log entries start with a capital letter.
    ("): [a-z]" (0 '(face font-lock-warning-face help-echo "Capitalize.") t))
    ("[^[:upper:]]\\(\\. [[:upper:]]\\)"
     (1 '(face font-lock-warning-face
          help-echo "Use two spaces to end a sentence") t))
    ("^("
     (0 (let ((beg (max (point-min) (- (match-beginning 0) 2))))
          (put-text-property beg (match-end 0) 'font-lock-multiline t)
          (if (eq (char-syntax (char-after beg)) ?w)
              '(face font-lock-warning-face
                help-echo "Punctuate previous line.")))
        t))
    ))

(defun log-edit-font-lock-keywords ()
  (if log-edit-font-lock-gnu-style
      (append log-edit-font-lock-keywords
              log-edit-font-lock-gnu-keywords)
    log-edit-font-lock-keywords))

;;;###autoload
(defun log-edit (callback &optional setup params buffer mode &rest _ignore)
  "Setup a buffer to enter a VC commit log message.
The buffer is put in mode MODE, or `log-edit-mode' if MODE is nil.
\\<log-edit-mode-map>
If SETUP is non-nil, erase the buffer and run `log-edit-hook'.
Set mark and point around the entire contents of the buffer, so
that it is easy to kill the contents of the buffer with
\\[kill-region].  Once the user is done editing the message, he
or she is expected to invoke the command \\[log-edit-done] (`log-edit-done'),
which will call CALLBACK, a function to do the actual commit.

PARAMS, if non-nil, is an alist of variables and buffer-local
values to give to those variables in the Log Edit buffer.  Possible
keys and associated values are:
 `log-edit-listfun' -- function taking no arguments that returns the list of
    files that are concerned by the current operation (using relative names);
 `log-edit-diff-function' -- function taking no arguments that
    displays a diff of the files concerned by the current operation.
 `vc-log-fileset' -- list of files to be committed, if any
                     (not a true VC fileset structure as returned by
                     `vc-deduce-fileset', but only the second element).

If BUFFER is non-nil, `log-edit' will switch to that buffer, use it
to edit the log message and go back to the current buffer when
done.  Otherwise, this function will use the current buffer."
  (let ((parent (current-buffer)))
    (if buffer (pop-to-buffer buffer))
    (when (and log-edit-setup-invert (not (eq setup 'force)))
      (setq setup (not setup)))
    (if mode
	(funcall mode)
      (log-edit-mode))
    (setq-local log-edit-callback callback)
    (if (listp params)
	(dolist (crt params)
	  (set (make-local-variable (car crt)) (cdr crt)))
      ;; For backward compatibility with log-edit up to version 22.2
      ;; accept non-list PARAMS to mean `log-edit-list'.
      (setq-local log-edit-listfun params))

    (if buffer (setq-local log-edit-parent-buffer parent))
    (setq-local log-edit-initial-files (log-edit-files))
    (when setup
      (erase-buffer)
      (run-hooks 'log-edit-hook))
    (push-mark (point-max))
    (message "%s" (substitute-command-keys
	      "Press \\[log-edit-done] when you are done editing."))))

(define-derived-mode log-edit-mode text-mode "Log-Edit"
  "Major mode for editing version-control (VC) commit log messages.
When done editing the log entry, type \\[log-edit-done], which will
trigger the actual commit of the file(s).
Several other handy support commands are provided, and the package
from which this is used might also provide additional commands (under
the \\[vc-prefix-map] prefix for VC commands, for example).

\\{log-edit-mode-map}"
  (setq-local font-lock-defaults '(log-edit-font-lock-keywords t))
  (make-local-variable 'font-lock-extra-managed-props)
  (cl-pushnew 'display-line-numbers-disable font-lock-extra-managed-props)
  (setq-local jit-lock-contextually t)  ;For the "first line is summary".
  (setq-local fill-paragraph-function #'log-edit-fill-entry)
  (setq-local normal-auto-fill-function #'log-edit-do-auto-fill)
  (make-local-variable 'log-edit-comment-ring-index)
  (add-hook 'kill-buffer-hook 'log-edit-remember-comment nil t)
  (hack-dir-local-variables-non-file-buffer)
  ;; Replace the tool bar map with `log-edit-tool-bar-map'.
  (setq-local tool-bar-map log-edit-tool-bar-map))

(defun log-edit--insert-filled-defuns (func-names)
  "Insert FUNC-NAMES, following ChangeLog formatting."
  (if (not func-names)
      (insert ":")
    ;; Insert a space unless this list of defun names is being
    ;; inserted at the start of a line or after a space character.
    (unless (or (memq (char-before) '(?\n ?\s))
                (> (current-column) fill-column))
      (insert " "))
    (let ((inside-paren-pair nil)
          (first-line        t)
          name)
      ;; Now insert the functions names one by one, inserting newlines
      ;; as appropriate.
      (while func-names
        (setq name (car func-names))
        (setq func-names (cdr func-names))
        ;; If inserting `name' after preexisting text in the first
        ;; line would overflow the fill column, place it on its own
        ;; line.
        (if (and first-line
                 (> (current-column) 0)
                 (> (+ (current-column)
                       (string-width name)
                       ;; If this be the last name, the column must be
                       ;; followed by an extra colon character.
                       (if func-names 1 2))
                    fill-column))
            (progn
              (insert "\n")
              ;; Iterate over this function name again.
              (setq func-names (cons name func-names)))
          (if inside-paren-pair
              ;; If `name' is not the first item in a list of defuns
              ;; and inserting it would overflow the fill column,
              ;; start a new list of defuns on the next line.
              (if (> (+ (current-column)
                        (string-width name)
                        ;; If this be the last name, the column must
                        ;; be followed by an extra colon character;
                        ;; however, there are two separator characters
                        ;; that will be deleted, so the number of
                        ;; columns to add to this in the case of
                        ;; `name' being final and in other cases are 0
                        ;; and 1 respectively.
                        (if func-names 0 1))
                     fill-column)
                  (progn
                    (delete-char -2)
                    (insert ")\n")
                    (setq inside-paren-pair nil
                          ;; Iterate over this function name again.
                          func-names (cons name func-names)))
                ;; Insert this defun name with a separator attached.
                (insert name ", "))
            ;; Otherwise, decide whether to start a list of defuns or
            ;; to insert `name' on its own line.
            (if (> (+ (current-column)
                      (string-width name)
                      (if func-names 1 2)) ; The column number of
                                           ; line after inserting
                                           ; `name'...
                   fill-column)
                ;; ...would leave insufficient space for any
                ;; subsequent defun names so insert it on its own
                ;; line.
                (insert (if func-names
                            (format "(%s)\n" name)
                          (format "(%s):" name)))
              ;; Insert a new defun list, unless `name' is the last
              ;; function name.
              (insert (if (not func-names)
                          (format "(%s):" name)
                        (setq inside-paren-pair t)
                        (format "(%s, " name))))))
        (setq first-line nil))
      ;; Close any open list of defuns.
      (when inside-paren-pair
        (delete-char -2)
        (insert "):")))))

(defun log-edit-fill-entry (&optional justify)
  "Like \\[fill-paragraph], but for filling ChangeLog-formatted entries.
Consecutive function entries without prose (i.e., lines of the
form \"(FUNCTION):\") will be combined into \"(FUNC1, FUNC2):\"
according to `fill-column'."
  (save-excursion
    (let* ((range (log-edit-changelog-paragraph))
           (beg (car range))
           (end (cadr range)))
      (if (= beg end)
          ;; Not a ChangeLog entry, fill as normal.
          nil
        (setq end (copy-marker end))
        (goto-char beg)
        (let* ((defuns-beg nil)
               (defuns nil))
          (while
              (progn
                ;; Match a regexp against the next ChangeLog entry.
                ;; `defuns-beg' will be the end of the file name,
                ;; which marks the beginning of the list of defuns.
                (setq defuns-beg
                      (and (< beg end)
                           (re-search-forward
                            (concat "\\(?1:"
                                    change-log-unindented-file-names-re
                                    "\\)\\|^\\(?1:\\)[[:blank:]]*(")
                            end t)
                           (copy-marker (match-end 1))))
                ;; Fill the intervening prose between the end of the
                ;; last match and the beginning of the current match.
                (let ((fill-indent-according-to-mode t)
                      (end (if defuns-beg
                               (match-beginning 0) end))
                      (beg (progn (goto-char beg)
                                  (line-beginning-position)))
                      space-beg space-end)
                  (when (<= (line-end-position) end)
                    ;; Replace space characters within parentheses
                    ;; that resemble ChangeLog defun names between BEG
                    ;; and END with non-breaking spaces to prevent
                    ;; them from being considered break points by
                    ;; `fill-region'.
                    (save-excursion
                      (goto-char beg)
                      (when (re-search-forward
                             ;; Also replace spaces within defun lists
                             ;; prefixed by a file name so that
                             ;; fill-region never attempts to break
                             ;; them, even if multiple items combine
                             ;; with symbols to exceed the fill column
                             ;; by the expressly permitted margin of 1
                             ;; character.
                             (concat "^\\([[:blank:]]*\\|\\* .*[[:blank:]]"
                                     "\\)(.*\\([[:space:]]\\).*):")
                             end t)
                        (replace-regexp-in-region "[[:space:]]" " "
                                                  (setq space-beg
                                                        (copy-marker
                                                         (match-beginning 0)))
                                                  (setq space-end
                                                        (copy-marker
                                                         (match-end 0))))))
                    (fill-region beg end justify))
                  ;; Restore the spaces replaced by NBSPs.
                  (when space-beg
                    (replace-string-in-region " " " "
                                              space-beg space-end)
                    (set-marker space-beg nil)
                    (set-marker space-end nil)))
                defuns-beg)
            (goto-char defuns-beg)
            (setq defuns (change-log-read-defuns end))
            (progn
              (delete-region defuns-beg (point))
              (log-edit--insert-filled-defuns defuns)
              (setq beg (point))))
          nil)
        t))))

(defun log-edit-do-auto-fill ()
  "Like `do-auto-fill', but don't fill in Log Edit headers."
  (unless (> (save-excursion (rfc822-goto-eoh) (point))
             (point))
    (do-auto-fill)))

(defun log-edit-hide-buf (&optional buf where)
  (when (setq buf (get-buffer (or buf log-edit-files-buf)))
    ;; FIXME: Should use something like `quit-windows-on' here, but
    ;; that function never deletes this buffer's window because it
    ;; was created using `cvs-pop-to-buffer-same-frame'.
    (save-selected-window
      (let ((win (get-buffer-window buf where)))
        (if win (ignore-errors (delete-window win))))
      (bury-buffer buf))))

(defun log-edit-remember-comment (&optional comment)
  (unless comment (setq comment (buffer-string)))
  (when (or (ring-empty-p log-edit-comment-ring)
            (not (equal comment (ring-ref log-edit-comment-ring 0))))
    (ring-insert log-edit-comment-ring comment)))

(defun log-edit-done ()
  "Finish editing the VC commit log message, and commit the files.
If you want to abort the commit, simply delete the buffer."
  (interactive)
  ;; Clean up empty headers.
  (goto-char (point-min))
  (while (looking-at (concat "^[a-z]*:" log-edit-header-contents-regexp))
    (let ((beg (match-beginning 0)))
      (goto-char (match-end 0))
      (if (string-match "\\`[ \n\t]*\\'" (match-string 1))
          (delete-region beg (point)))))
  ;; Get rid of leading empty lines.
  (goto-char (point-min))
  (when (looking-at "\\([ \t]*\n\\)+")
    (delete-region (match-beginning 0) (match-end 0)))
  ;; Get rid of trailing empty lines
  (goto-char (point-max))
  (skip-syntax-backward " ")
  (when (equal (char-after) ?\n) (forward-char 1))
  (delete-region (point) (point-max))
  ;; Check for final newline
  (if (and (> (point-max) (point-min))
	   (/= (char-before (point-max)) ?\n)
	   (or (eq log-edit-require-final-newline t)
	       (and log-edit-require-final-newline
		    (y-or-n-p
		     (format "Buffer %s does not end in newline.  Add one? "
			     (buffer-name))))))
      (save-excursion
	(goto-char (point-max))
	(insert ?\n)))
  (log-edit-remember-comment)
  (let ((win (get-buffer-window log-edit-files-buf)))
    (if (and log-edit-confirm
	     (not (and (eq log-edit-confirm 'changed)
		       (equal (log-edit-files) log-edit-initial-files)))
	     (progn
	       (log-edit-show-files)
	       (not (y-or-n-p "Really commit? "))))
	(progn (when (not win) (log-edit-hide-buf))
	       (message "Oh, well!  Later maybe?"))
      (run-hooks 'log-edit-done-hook)
      (log-edit-hide-buf)
      (unless (or log-edit-keep-buffer (not log-edit-parent-buffer))
	(cvs-bury-buffer (current-buffer) log-edit-parent-buffer))
      (call-interactively log-edit-callback))))

(defun log-edit-kill-buffer ()
  "Kill the current VC commit log buffer.
This command saves the contents of the log buffer in the VC commit
comment history, see `log-edit-comment-ring', and hides `log-edit-files-buf'."
  (interactive)
  (log-edit-hide-buf)
  (let ((buf (current-buffer)))
    (quit-windows-on buf)
    (kill-buffer buf)))

(defun log-edit-files ()
  "Return the list of files that are about to be committed."
  (ignore-errors (funcall log-edit-listfun)))

(defun log-edit-mode-help ()
  "Provide help for the `log-edit-mode-map'."
  (interactive)
  (if (eq last-command 'log-edit-mode-help)
      (describe-function major-mode)
    (message "%s"
     (substitute-command-keys
      "Type `\\[log-edit-done]' to finish commit.  Try `\\[describe-function] log-edit-done' for more help."))))

(defcustom log-edit-common-indent 0
  "Minimum indentation to use in `log-edit-set-common-indentation'."
  :group 'log-edit
  :type 'integer)

(defun log-edit-set-common-indentation ()
  "(Un)Indent the current buffer rigidly to `log-edit-common-indent'."
  (save-excursion
    (let ((common (point-max)))
      (rfc822-goto-eoh)
      (while (< (point) (point-max))
        (if (not (looking-at "^[ \t]*$"))
            (setq common (min common (current-indentation))))
        (forward-line 1))
      (rfc822-goto-eoh)
      (indent-rigidly (point) (point-max)
		      (- log-edit-common-indent common)))))

(defvar vc-patch-string)

(autoload 'vc-diff-patch-string "vc")
(defun log-edit-diff-patch ()
  (vc-diff-patch-string vc-patch-string))

(defvar vc-log-fileset)

(defun log-edit-diff-fileset ()
  "Display diffs for the files to be committed."
  (interactive)
  ;; Re NOT-ESSENTIAL non-nil: this function can get called from
  ;; `log-edit-hook' and we don't want to abort the whole Log Edit setup
  ;; because the user says no to saving a buffer.  The buffers will
  ;; still actually get saved before committing, by the
  ;; `vc-log-operation' anonymous function.  Possibly
  ;; `log-edit-maybe-show-diff' should catch the error instead.
  (vc-diff nil 'not-essential (list log-edit-vc-backend vc-log-fileset)))

(defun log-edit-show-diff ()
  "Show diff for the changes to be committed."
  (interactive)
  (funcall log-edit-diff-function))

(defun log-edit-maybe-show-diff ()
  "Show diff for the changes to be committed without selecting its window.
This function is intended to be added to `log-edit-hook'.
It does nothing in the case that the commit was initiated from a
`diff-mode' buffer, i.e., when you are committing a patch.  This is
because in that case the existing `diff-mode' buffer normally remains
visible when the *vc-log* buffer pops up."
  ;; No (interactive) form because our use of `vc-parent-buffer'
  ;; assumes we are being called during \\`C-x v v' or similar.
  ;; If a user wants a version of `log-edit-show-diff' which doesn't
  ;; select the window they can use a `post-command-select-window'
  ;; display buffer action alist entry on `log-edit-show-diff'.
  (unless (and (bound-and-true-p vc-parent-buffer)
	       (with-current-buffer vc-parent-buffer
		 (derived-mode-p 'diff-mode)))
    (save-selected-window
      (let ((display-buffer-overriding-action '(nil
                                                . ((inhibit-same-window . t)))))
       (funcall log-edit-diff-function)))))

(defun log-edit-show-files ()
  "Show the list of files to be committed."
  (interactive)
  (let* ((files (log-edit-files))
	 (buf (get-buffer-create log-edit-files-buf)))
    (with-current-buffer buf
      (log-edit-hide-buf buf 'all)
      (setq buffer-read-only nil)
      (erase-buffer)
      (cvs-insert-strings files)
      (special-mode)
      (goto-char (point-min))
      (save-selected-window
	(cvs-pop-to-buffer-same-frame buf)
	(shrink-window-if-larger-than-buffer)
        (set-window-dedicated-p (selected-window) t)
	(selected-window)))))

(defun log-edit-beginning-of-line (&optional n)
  "Move point to beginning of header value or to beginning of line.

It works the same as `message-beginning-of-line', but it uses a
different header separator appropriate for `log-edit-mode'."
  (interactive "p")
  (let ((mail-header-separator ""))
    (message-beginning-of-line n)))

(defun log-edit-empty-buffer-p ()
  "Return non-nil if the buffer is \"empty\"."
  (or (= (point-min) (point-max))
      (save-excursion
        (goto-char (point-min))
        (while (and (looking-at "^\\([a-zA-Z]+: ?\\)?$")
                    (zerop (forward-line 1))))
        (eobp))))

(defun log-edit--make-header-line (header &optional value)
  ;; Make \\`C-a' work like it does in other buffers with header names.
  (concat (propertize (concat header ": ")
                      'field 'header
                      'rear-nonsticky t)
          value
          "\n"))

(defun log-edit-insert-message-template ()
  "Insert the default VC commit log template with Summary and Author."
  (interactive)
  (when (or (called-interactively-p 'interactive)
            (log-edit-empty-buffer-p))
    ;; Put Author first because then the user can immediately yank in a
    ;; multiline log message, or use \\`C-c C-w' (probably because they
    ;; know it will generate exactly one line), without thereby pushing
    ;; Author out of the header and into the log message body.
    ;; (Also note that `log-edit-set-header' inserts all other headers
    ;; before Summary.)
    (when log-edit-setup-add-author
      (insert (log-edit--make-header-line "Author")))
    (insert (log-edit--make-header-line "Summary") "\n")
    (end-of-line -1)))

(defun log-edit-insert-cvs-template ()
  "Insert the commit log template specified by the CVS administrator, if any.
This simply uses the local CVS/Template file."
  (interactive)
  (when (or (called-interactively-p 'interactive)
            (log-edit-empty-buffer-p))
    ;; Should the template take precedence over an empty Summary:,
    ;; ie should we first erase the buffer?
    (when (file-readable-p "CVS/Template")
      (goto-char (point-max))
      (insert-file-contents "CVS/Template"))))

(defun log-edit-done-strip-cvs-lines (&optional interactive)
  "Strip lines starting with \"CVS:\" from commit log message.
When not called interactively do this only when the VC backend is CVS.
This mimicks what CVS does when invoked as \\='cvs commit [files...]'."
  (interactive "p")
  (when (or interactive (eq log-edit-vc-backend 'CVS))
    (let ((case-fold-search nil))
      (goto-char (point-min))
      ;; NB: While CVS defines CVSEDITPREFIX as "CVS: " it actually
      ;; checks only the first four characters of af a line, i.e. "CVS:"
      ;; to deal with editors that strip trailing whitespace.
      ;; c.f. src/cvs.h and src/logmsg.c:do_editor()
      (flush-lines "^CVS:"))))

(defun log-edit-insert-cvs-rcstemplate ()
  "Insert the RCS commit log template from the CVS repository.
This contacts the repository to get the rcstemplate file and
can thus take some time."
  (interactive)
  (when (or (called-interactively-p 'interactive)
            (log-edit-empty-buffer-p))
    (when (file-readable-p "CVS/Root")
      (goto-char (point-max))
      ;; Ignore the stderr stuff, even if it's an error.
      (call-process "cvs" nil '(t nil) nil
                    "checkout" "-p" "CVSROOT/rcstemplate"))))

(defun log-edit-insert-filenames ()
  "Insert the list of files that are to be committed."
  (interactive)
  (insert "Affected files:  \n"
          (mapconcat 'identity (log-edit-files) "  \n")))

(defun log-edit-insert-filenames-without-changelog ()
  "Insert the list of files that have no ChangeLog message."
  (interactive)
  (let ((files
	 (delq nil
	       (mapcar
		(lambda (file)
		  (unless (or (cdr-safe (log-edit-changelog-entries file))
			      (equal (file-name-nondirectory file) "ChangeLog"))
		    file))
		(log-edit-files)))))
    (when files
      (goto-char (point-max))
      (insert (mapconcat 'identity files ", ") ": "))))

(defun log-edit-add-to-changelog ()
  "Insert this VC commit log message into the appropriate ChangeLog file."
  (interactive)
  (log-edit-remember-comment)
  (dolist (f (log-edit-files))
    (let ((buffer-file-name (expand-file-name f)))
      (save-excursion
	(log-edit-comment-to-change-log)))))

(defvar log-edit-changelog-use-first nil)

(defvar log-edit-rewrite-tiny-change t
  "Non-nil means rewrite (tiny change).")

(defvar log-edit-rewrite-fixes nil
  "Rule to rewrite bug numbers into Fixes: headers in commit log messages.
The value should be of the form (REGEXP . REPLACEMENT)
where REGEXP should match the expression referring to a bug number
in the text, and REPLACEMENT is an expression to pass to `replace-match'
to build the Fixes: header.")
(put 'log-edit-rewrite-fixes 'safe-local-variable
     (lambda (v) (and (stringp (car-safe v)) (stringp (cdr v)))))

(defun log-edit-add-field (field value)
  (rfc822-goto-eoh)
  (if (save-excursion (re-search-backward (concat "^" field ":\\([ \t]*\\)$")
                                          nil t))
      (replace-match (concat " " value) t t nil 1)
    (insert field ": " value "\n" (if (looking-at "\n") "" "\n"))))

(declare-function diff-add-log-current-defuns "diff-mode" ())

(defun log-edit-generate-changelog-from-diff ()
  "Insert a VC commit log message by looking at the current diffs.
This command is intended to be used in the \"*vc-log*\" buffer.
This command will generate ChangeLog entries listing the modified
files and functions changed in those files, based on the diffs
you are about to commit.  You can then add a description for each
change where needed, and use \\[fill-paragraph] to join consecutive function
names into a single entry where they all share the same description.
Should you need to look at the diffs themselves, they can be found
in the \"*vc-diff*\" buffer produced by this command."
  (interactive)
  (change-log-insert-entries
   (with-current-buffer
       (let* ((diff-buf nil)
              ;; Unfortunately, `log-edit-show-diff' doesn't have a
              ;; NO-SHOW option, so we try to work around it via
              ;; display-buffer machinery.
              (display-buffer-overriding-action
               `(,(lambda (buf alist)
                    (setq diff-buf buf)
                    (display-buffer-no-window buf alist))
                 . ((allow-no-window . t)))))
         (log-edit-show-diff)
         diff-buf)
     (diff-add-log-current-defuns))))

(defun log-edit-insert-changelog (&optional use-first)
  "Insert a VC commit log message by looking at the ChangeLog.
The idea is to write your ChangeLog entries first, and then use this
command to commit your changes with that log.

To select default log text, this command:
- finds the ChangeLog entries for the files to be checked in;
- verifies that the top entry in the ChangeLog is on the current date
    and by the current user; if not, it doesn't provide any default text;
- searches the ChangeLog entry for paragraphs containing the names of
    the files to be checked in; and finally
- uses those paragraphs as the log text.

If the optional prefix arg USE-FIRST is given (via \\[universal-argument]),
or if the command is repeated, use the first log entry regardless of user
name or time."
  (interactive "P")
  (save-excursion
    (let ((eoh (save-excursion (rfc822-goto-eoh) (point))))
      (when (<= (point) eoh)
	(goto-char eoh)
	(if (looking-at "\n") (forward-char 1))))
    (let ((author
	   (let ((log-edit-changelog-use-first
		  (or use-first (eq last-command 'log-edit-insert-changelog))))
	     (log-edit-insert-changelog-entries (log-edit-files)))))
      (log-edit-set-common-indentation)
      ;; Add an Author: field if appropriate.
      (when author
        (log-edit-add-field "Author" (car author))
        (log-edit-add-field "Summary" ""))
      ;; Add a Fixes: field if applicable.
      (when (consp log-edit-rewrite-fixes)
	(rfc822-goto-eoh)
	(when (re-search-forward (car log-edit-rewrite-fixes) nil t)
	  (let ((start (match-beginning 0))
		(end (match-end 0))
		(fixes (match-substitute-replacement
			(cdr log-edit-rewrite-fixes))))
	    (delete-region start end)
	    (log-edit-add-field "Fixes" fixes))))
      (and log-edit-strip-single-file-name
	   (progn (rfc822-goto-eoh)
		  (if (looking-at "\n") (forward-char 1))
		  (looking-at "\\*\\s-+"))
	   (let ((start (point)))
	     (forward-line 1)
	     (when (not (re-search-forward "^\\*\\s-+" nil t))
	       (goto-char start)
	       (skip-chars-forward "^():")
	       (skip-chars-forward ": ")
	       (delete-region start (point)))))
      ;; FIXME also add "Co-authored-by" when appropriate.
      ;; Bzr accepts multiple --author arguments, others (?) don't.
      (and log-edit-rewrite-tiny-change
           (eq 'tiny (cdr author))
           (goto-char (point-max))
           (insert "\nCopyright-paperwork-exempt: yes\n")))))

;;;;
;;;; functions for getting commit message from ChangeLog a file...
;;;; Courtesy Jim Blandy
;;;;

(defun log-edit-narrow-changelog ()
  "Narrow to the top page of the current buffer, which visits a ChangeLog file.
Actually, the narrowed region doesn't include the date line.
A \"page\" in a ChangeLog file is the area between two dates."
  (or (eq major-mode 'change-log-mode)
      (error "log-edit-narrow-changelog: Current buffer isn't a ChangeLog"))

  (goto-char (point-min))

  ;; Skip date line and subsequent blank lines.
  (forward-line 1)
  (if (looking-at "[ \t\n]*\n")
      (goto-char (match-end 0)))

  (let ((start (point)))
    (forward-page 1)
    (narrow-to-region start (point))
    (goto-char (point-min))))

(defun log-edit-changelog-paragraph ()
  "Return the bounds of the ChangeLog paragraph containing point.
If we are between paragraphs, return the previous paragraph."
  (beginning-of-line)
  (if (looking-at "^[ \t]*$")
      (skip-chars-backward " \t\n" (point-min)))
  (list (progn
          (if (re-search-backward "^[ \t]*\n" nil 'or-to-limit)
              (goto-char (match-end 0)))
          (point))
        (if (re-search-forward "^[ \t\n]*$" nil t)
            (match-beginning 0)
          (point-max))))

(defun log-edit-changelog-subparagraph ()
  "Return the bounds of the ChangeLog subparagraph containing point.
A subparagraph is a block of non-blank lines beginning with an asterisk.
If we are between sub-paragraphs, return the previous subparagraph."
    (end-of-line)
    (if (search-backward "*" nil t)
        (list (progn (beginning-of-line) (point))
              (progn
                (forward-line 1)
                (if (re-search-forward "^[ \t]*[\n*]" nil t)
                    (match-beginning 0)
                  (point-max))))
    (list (point) (point))))

(defun log-edit-changelog-entry ()
  "Return the bounds of the ChangeLog entry containing point.
The variable `log-edit-changelog-full-paragraphs' determines whether an
\"entry\" is a paragraph or a subparagraph; see its documentation string
for more details."
  (save-excursion
    (if log-edit-changelog-full-paragraphs
        (log-edit-changelog-paragraph)
      (log-edit-changelog-subparagraph))))

(defvar user-full-name)
(defvar user-mail-address)

(defvar log-edit-author)                ;Dynamically scoped.

(defun log-edit-changelog-ours-p ()
  "See if ChangeLog entry at point is for the current user, today.
Return non-nil if it is."
  ;; Code adapted from add-change-log-entry.
  (let ((name (or (and (boundp 'add-log-full-name) add-log-full-name)
		  (and (fboundp 'user-full-name) (user-full-name))
		  (and (boundp 'user-full-name) user-full-name)))
        (mail (or (and (boundp 'add-log-mailing-address) add-log-mailing-address)
		  ;;(and (fboundp 'user-mail-address) (user-mail-address))
		  (and (boundp 'user-mail-address) user-mail-address)))
	(time (or (and (boundp 'add-log-time-format)
		       (functionp add-log-time-format)
		       (funcall add-log-time-format
				nil add-log-time-zone-rule))
		  (format-time-string "%Y-%m-%d"))))
    (if (null log-edit-changelog-use-first)
        (looking-at (regexp-quote (format "%s  %s  <%s>" time name mail)))
      ;; Check the author, to potentially add it as a "Author: " header.
      ;; FIXME This accumulates multiple authors, but only when there
      ;; are multiple ChangeLog files.  It should also check for
      ;; multiple authors in each individual entry.
      (when (looking-at "[^ \t]")
        (when (and (boundp 'log-edit-author)
                   (not (looking-at (format ".+  .+  <%s>"
                                            (regexp-quote mail))))
                   (looking-at ".+  \\(.+  <.+>\\) *\\((tiny change)\\)?"))
          (let ((author (string-replace "  " " "
                                        (match-string 1))))
            (unless (and log-edit-author
                         (string-match (regexp-quote author)
                                       (car log-edit-author)))
              (if (not log-edit-author)
                  (setq log-edit-author
                        (cons author (if (match-string 2) 'tiny)))
                (setcar log-edit-author
                        (concat (car log-edit-author) ", " author))
                (and (match-string 2) (not (cdr log-edit-author))
                     (setcdr log-edit-author 'tiny))))))
        t))))

(defun log-edit-changelog-entries (file)
  "Return the ChangeLog entries for FILE, and the ChangeLog they came from.
The return value looks like this:
  (LOGBUFFER (ENTRYSTART ENTRYEND) ...)
where LOGBUFFER is the name of the ChangeLog buffer, and each
\(ENTRYSTART . ENTRYEND) pair is a buffer region."
  (let ((changelog-file-name
         (let ((default-directory
                 (file-name-directory (expand-file-name file)))
               (visiting-buffer (find-buffer-visiting file)))
           ;; If there is a buffer visiting FILE, and it has a local
           ;; value for `change-log-default-name', use that.
           (or (and visiting-buffer
                    (local-variable-p 'change-log-default-name
                                      visiting-buffer)
                    (with-current-buffer visiting-buffer
                      change-log-default-name))
               ;; `find-change-log' uses `change-log-default-name' if set
               ;; and sets it before exiting, so we need to work around
               ;; that memoizing which is undesired here.
               (progn
                 (setq change-log-default-name nil)
                 (find-change-log))))))
    (when (or (find-buffer-visiting changelog-file-name)
              (file-exists-p changelog-file-name)
              add-log-dont-create-changelog-file)
      (with-current-buffer
          (add-log-find-changelog-buffer changelog-file-name)
        (unless (eq major-mode 'change-log-mode) (change-log-mode))
        (goto-char (point-min))
        (if (looking-at "\\s-*\n") (goto-char (match-end 0)))
        (if (not (log-edit-changelog-ours-p))
            (list (current-buffer))
          (save-restriction
            (log-edit-narrow-changelog)
            (goto-char (point-min))

            (let ((pattern (log-edit-changelog--pattern file
                                                        changelog-file-name)))
              (let (texts
                    (pos (point)))
                (while (and (not (eobp)) (re-search-forward pattern nil t))
                  (let ((entry (log-edit-changelog-entry)))
                    (if (< (elt entry 1) (max (1+ pos) (point)))
                        ;; This is not relevant, actually.
                        nil
                      (push entry texts))
                    ;; Make sure we make progress.
                    (setq pos (max (1+ pos) (elt entry 1)))
                    (goto-char pos)))

                (cons (current-buffer) texts)))))))))

(defun log-edit-changelog--pattern (file changelog-file-name)
  (if (eq (aref file (1- (length file))) ?/)
      ;; Match any files inside this directory.
      (concat "^\t\\* " (unless (string= file "./") file))
    ;; Search for the name of FILE relative to the ChangeLog.  If that
    ;; doesn't occur anywhere, they're not using full relative
    ;; filenames in the ChangeLog, so just look for FILE; we'll accept
    ;; some false positives.
    (let ((pattern (file-relative-name
                    file (file-name-directory changelog-file-name))))
      ;; FIXME: When can the above return an empty string?
      (if (or (string= pattern "")
              (not (save-excursion
                     (search-forward pattern nil t))))
          (setq pattern (file-name-nondirectory file)))
      (setq pattern (concat "\\(^\\|[^[:alnum:]]\\)"
                            (regexp-quote pattern)
                            "\\($\\|[^[:alnum:]]\\)")))))

(defun log-edit-changelog-insert-entries (buffer beg end &rest files)
  "Insert the text from ChangeLog BUFFER between BEG and END.
Rename relative filenames in the ChangeLog entry with FILES.
FILES are supposed to name the same files whose relative filenames
are to be replaced, and their names relative to the directory of
BUFFER are expected to match the relative file names in the ChangeLog
entry."
  (let ((opoint (point))
	(log-name (buffer-file-name buffer))
	(case-fold-search nil)
	bound)
    (insert-buffer-substring buffer beg end)
    (setq bound (point-marker))
    (when log-name
      (dolist (f files)
        ;; FIXME: f can be a directory, a (possibly indirect) parent
        ;; of the ChangeLog file.
	(save-excursion
	  (goto-char opoint)
	  (when (re-search-forward
		 (concat "\\(^\\|[ \t]\\)\\("
			 (file-relative-name f (file-name-directory log-name))
			 "\\)[, :\n]")
		 bound t)
	    (replace-match f t t nil 2)))))
    ;; Eliminate tabs at the beginning of the line.
    (save-excursion
      (goto-char opoint)
      (while (re-search-forward "^\\(\t+\\)" bound t)
	(replace-match "")))))

(defun log-edit-insert-changelog-entries (files)
  "Given a list of files FILES, insert the ChangeLog entries for them."
  (let ((log-entries nil)
        (log-edit-author nil))
    ;; Note that any ChangeLog entry can apply to more than one file.
    ;; Here we construct a log-entries list with elements of the form
    ;;   ((LOGBUFFER ENTRYSTART ENTRYEND) FILE1 FILE2...)
    (dolist (file files)
      (let* ((entries (log-edit-changelog-entries file))
	     (buf (car entries))
	     key entry)
	(dolist (region (cdr entries))
	  (setq key (cons buf region))
	  (if (setq entry (assoc key log-entries))
	      (setcdr entry (append (cdr entry) (list file)))
	    (push (list key file) log-entries)))))
    ;; Now map over log-entries, and extract the strings.
    (dolist (log-entry (nreverse log-entries))
      (apply 'log-edit-changelog-insert-entries
	     (append (car log-entry) (cdr log-entry)))
      (insert "\n"))
    ;; No newline after the last entry.
    (when log-entries
      (delete-char -1))
    log-edit-author))

(defun log-edit-toggle-header (header value)
  "Toggle a boolean-type header in the current buffer.
See `log-edit-set-header' for details."
  (log-edit-set-header header value t))

(defun log-edit-set-header (header value &optional toggle)
  "Set the value of HEADER to VALUE in the current buffer.
If TOGGLE is non-nil, and the value of HEADER already is VALUE,
clear it.  Make sure there is an empty line after the headers.
Return t if toggled on (or TOGGLE is nil), otherwise nil."
  (let ((val t)
        (line (log-edit--make-header-line header value)))
    (save-excursion
      (save-restriction
        (rfc822-goto-eoh)
        (narrow-to-region (point-min) (point))
        (goto-char (point-min))
        (if (re-search-forward (concat "^" header ":"
                                       log-edit-header-contents-regexp)
                               nil t)
            (if (setq val (not (and toggle (string= (match-string 1) value))))
                (replace-match line t t)
              (replace-match "" t t nil 1))
          (insert line)))
      (rfc822-goto-eoh)
      (delete-horizontal-space)
      (unless (looking-at "\n")
        (insert "\n")))
    val))

(defun log-edit-extract-headers (headers comment)
  "Extract headers from VC commit COMMENT to form command line arguments.
HEADERS should be an alist with elements (HEADER . CMDARG)
or (HEADER . FUNCTION) associating headers to command line
options and the result is then a list of the form (MSG ARGUMENTS...)
where MSG is the remaining text from COMMENT.
FUNCTION should be a function of one argument that takes the
header value and returns the list of strings to be appended to
ARGUMENTS.  CMDARG will be added to ARGUMENTS followed by the
header value.  If \"Summary\" is not in HEADERS, then the
\"Summary\" header is extracted anyway and put back as the first
line of MSG."
  (with-temp-buffer
    (insert comment)
    (rfc822-goto-eoh)
    (narrow-to-region (point-min) (point))
    (let ((case-fold-search t)
          (summary ())
          (res ()))
      (dolist (header (if (assoc "Summary" headers) headers
                        (cons '("Summary" . t) headers)))
        (goto-char (point-min))
        (while (re-search-forward (concat "^" (car header)
                                          ":" log-edit-header-contents-regexp)
                                  nil t)
          (let ((txt (match-string 1)))
            (replace-match "" t t)
            (if (eq t (cdr header))
                (setq summary txt)
              (if (functionp (cdr header))
                  (setq res (nconc res (funcall (cdr header) txt)))
                (push txt res)
                (push (or (cdr header) (car header)) res))))))
      ;; Remove header separator if the header is empty.
      (widen)
      (goto-char (point-min))
      (when (looking-at "\\([ \t]*\n\\)+")
        (delete-region (match-beginning 0) (match-end 0)))
      (if summary (insert summary "\n\n"))
      (cons (buffer-string) res))))

(defun log-edit--toggle-amend (last-msg-fn)
  (when (log-edit-toggle-header "Amend" "yes")
    (goto-char (point-max))
    (unless (bolp) (insert "\n"))
    (insert (funcall last-msg-fn))
    (save-excursion
      (rfc822-goto-eoh)
      (forward-line 1)
      (let ((pt (point)))
        (and (zerop (forward-line 1))
             (looking-at "\n\\|\\'")
             (let ((summary (buffer-substring-no-properties pt
                                                            (if (bolp)
                                                                (1- (point))
                                                              (point)))))
               (skip-chars-forward " \n")
               (delete-region pt (point))
               (log-edit-set-header "Summary" summary)))))))

(provide 'log-edit)

;;; log-edit.el ends here

;; Local Variables:
;; coding: utf-8-unix
;; End:
