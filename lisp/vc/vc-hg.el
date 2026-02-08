;;; vc-hg.el --- VC backend for the mercurial version control system  -*- lexical-binding: t -*-

;; Copyright (C) 2006-2026 Free Software Foundation, Inc.

;; Author: Ivan Kanis
;; Maintainer: emacs-devel@gnu.org
;; Keywords: vc tools
;; Package: vc

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

;; This is a mercurial version control backend

;;; Todo:

;; 1) Implement the rest of the vc interface. See the comment at the
;; beginning of vc.el. The current status is:

;; FUNCTION NAME                               STATUS
;; BACKEND PROPERTIES
;; * revision-granularity                      OK
;; - update-on-retrieve-tag                    OK
;; STATE-QUERYING FUNCTIONS
;; * registered (file)                         OK
;; * state (file)                              OK
;; - dir-status-files (dir files uf)           OK
;; - dir-extra-headers (dir)                   OK
;; - dir-printer (fileinfo)                    OK
;; * working-revision (file)                   OK
;; * checkout-model (files)                    OK
;; - mode-line-string (file)                   OK
;; STATE-CHANGING FUNCTIONS
;; * register (files &optional rev comment)    OK
;; * create-repo ()                            OK
;; - responsible-p (file)                      OK
;; - receive-file (file rev)                   ?? PROBABLY NOT NEEDED
;; - unregister (file)                         OK
;; * checkin (files rev comment)               OK
;; - checkin-patch (patch-string comment)      OK
;; * find-revision (file rev buffer)           OK
;; * checkout (file &optional rev)             OK
;; * revert (file &optional contents-done)     OK
;; - merge (file rev1 rev2)                    NEEDED
;; - merge-news (file)                         NEEDED
;; - steal-lock (file &optional revision)      NOT NEEDED
;; HISTORY FUNCTIONS
;; * print-log (files buffer &optional shortlog start-revision limit) OK
;; - log-view-mode ()                          OK
;; - show-log-entry (revision)                 NOT NEEDED, DEFAULT IS GOOD
;; - comment-history (file)                    NOT NEEDED
;; - update-changelog (files)                  NOT NEEDED
;; * diff (files &optional rev1 rev2 buffer)   OK
;; - revision-completion-table (files)         OK?
;; - annotate-command (file buf &optional rev) OK
;; - annotate-time ()                          OK
;; - annotate-current-time ()                  NOT NEEDED
;; - annotate-extract-revision-at-line ()      OK
;; TAG SYSTEM
;; - create-tag (dir name branchp)             OK
;; - retrieve-tag (dir name update)            OK
;; MISCELLANEOUS
;; - make-version-backups-p (file)             ??
;; - previous-revision (file rev)              OK
;; - next-revision (file rev)                  OK
;; - file-name-changes (rev)                   OK
;; - check-headers ()                          ??
;; - delete-file (file)                        TEST IT
;; - rename-file (old new)                     OK
;; - find-file-hook ()                         added for bug#10709
;; - prepare-patch (rev)                       OK

;; 2) Implement Stefan Monnier's advice:
;; vc-hg-registered and vc-hg-state
;; Both of those functions should be super extra careful to fail gracefully in
;; unexpected circumstances. The reason this is important is that any error
;; there will prevent the user from even looking at the file :-(
;; Ideally, just like in vc-arch and vc-cvs, checking that the file is under
;; mercurial's control and extracting the current revision should be done
;; without even using `hg' (this way even if you don't have `hg' installed,
;; Emacs is able to tell you this file is under mercurial's control).

;;; Code:

(require 'cl-lib)

(eval-when-compile
  (require 'vc)
  (require 'vc-dir))

(declare-function vc-compilation-mode "vc-dispatcher" (backend))
(declare-function vc-read-revision "vc"
                  (prompt &optional files backend default initial-input))

;;; Customization options

(defgroup vc-hg nil
  "VC Mercurial (hg) backend."
  :version "24.1"
  :group 'vc)

(defcustom vc-hg-global-switches nil
  "Global switches to pass to any Hg command."
  :type '(choice (const :tag "None" nil)
         (string :tag "Argument String")
         (repeat :tag "Argument List" :value ("") string))
  :version "22.2")

(defcustom vc-hg-diff-switches t ; Hg doesn't support common args like -u
  "String or list of strings specifying switches for Hg diff under VC.
If nil, use the value of `vc-diff-switches'.  If t, use no switches."
  :type '(choice (const :tag "Unspecified" nil)
                 (const :tag "None" t)
                 (string :tag "Argument String")
                 (repeat :tag "Argument List" :value ("") string))
  :version "23.1")

(defcustom vc-hg-annotate-switches '("-u" "--follow")
  "String or list of strings specifying switches for hg annotate under VC.
If nil, use the value of `vc-annotate-switches'.  If t, use no
switches."
  :type '(choice (const :tag "Unspecified" nil)
		 (const :tag "None" t)
		 (string :tag "Argument String")
		 (repeat :tag "Argument List" :value ("") string))
  :version "25.1")

(defcustom vc-hg-revert-switches nil
  "String or list of strings specifying switches for hg revert under VC."
  :type '(choice (const :tag "None" nil)
		 (string :tag "Argument String")
		 (repeat :tag "Argument List" :value ("") string))
  :version "27.1")

(defcustom vc-hg-program "hg"
  "Name of the Mercurial executable (excluding any arguments)."
  :type 'string)

(defcustom vc-hg-root-log-format
  `(,(concat "{rev}:{ifeq(branch, 'default','', '{branch}')}"
             ":{bookmarks}:{tags}:{author|person}"
             " {date|shortdate} {desc|firstline}\\n")
    ,(concat "^\\(?:[+@o x|-]*\\)"      ;Graph data.
             "\\([0-9]+\\):\\([^:]*\\)"
             ":\\([^:]*\\):\\([^:]*\\):\\(.*?\\)"
             "[ \t]+\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\)")
    ((1 'log-view-message)
     (2 'change-log-file)
     (3 'change-log-list)
     (4 'change-log-conditionals)
     (5 'change-log-name)
     (6 'change-log-date)))
  "Mercurial log template for `vc-hg-print-log' short format.
This should be a list (TEMPLATE REGEXP KEYWORDS), where TEMPLATE
is the \"--template\" argument string to pass to Mercurial,
REGEXP is a regular expression matching the resulting Mercurial
output, and KEYWORDS is a list of `font-lock-keywords' for
highlighting the Log View buffer."
  :type '(list string regexp (repeat sexp))
  :version "24.5")

(defcustom vc-hg-create-bookmark t
  "This controls whether `vc-create-tag' will create a bookmark or branch.
If nil, named branch will be created.
If t, bookmark will be created.
If `ask', you will be prompted for a branch type."
  :type '(choice (const :tag "No" nil)
                 (const :tag "Yes" t)
                 (const :tag "Ask" ask))
  :version "28.1")

(defcustom vc-hg-resolve-conflicts 'default
  "Whether to mark conflicted file as resolved upon saving.
If this is t and there are no more conflict markers in the file,
VC will mark the conflicts in the saved file as resolved.
A value of `default' means to use the value of `vc-resolve-conflicts'."
  :type '(choice (const :tag "Don't resolve" nil)
                 (const :tag "Resolve" t)
                 (const :tag "Use vc-resolve-conflicts" default))
  :version "31.1")


;; Clear up the cache to force vc-call to check again and discover
;; new functions when we reload this file.
(put 'Hg 'vc-functions nil)

;;; Properties of the backend

(defvar vc-hg-history nil)

(defun vc-hg-revision-granularity () 'repository)
(defun vc-hg-checkout-model (_files) 'implicit)
(defun vc-hg-update-on-retrieve-tag () nil)

;;; State querying functions

;;;###autoload (defun vc-hg-registered (file)
;;;###autoload   "Return non-nil if FILE is registered with hg."
;;;###autoload   (if (vc-find-root file ".hg")       ; short cut
;;;###autoload       (progn
;;;###autoload         (load "vc-hg" nil t)
;;;###autoload         (vc-hg-registered file))))

;; Modeled after the similar function in vc-bzr.el
(defun vc-hg-registered (file)
  "Return non-nil if FILE is registered with hg."
  (when (vc-hg-root file)           ; short cut
    (let ((state (vc-state file 'Hg)))  ; expensive
      (if (memq state '(ignored unregistered nil))
          ;; Clear the cache for proper fallback to another backend.
          (ignore (vc-file-setprop file 'vc-state nil))
        t))))

(defun vc-hg-state (file)
  "Hg-specific version of `vc-state'."
  (unless (file-directory-p file)
    (let ((state (vc-hg-state-fast file)))
      (if (eq state 'unsupported) (vc-hg-state-slow file) state))))

(defun vc-hg-state-slow (file)
  "Determine status of FILE by running hg."
  (setq file (expand-file-name file))
  (let*
      ((status nil)
       (default-directory (file-name-directory file))
       (out
        (with-output-to-string
          (with-current-buffer
              standard-output
            (setq status
                  (condition-case nil
                      ;; Ignore all errors.
		      (let ((process-environment
			     ;; Avoid localization of messages so we
			     ;; can parse the output.  Disable pager.
			     (append
			      (list "TERM=dumb" "LANGUAGE=C" "HGPLAIN=1")
			      process-environment)))
			(process-file
			 vc-hg-program nil t nil
                         "--config" "ui.report_untrusted=0"
			 "--config" "alias.status=status"
			 "--config" "defaults.status="
			 "status" "-A" (file-relative-name file)))
                    ;; Some problem happened.  E.g. We can't find an `hg'
                    ;; executable.
                    (error nil)))))))
    (when (and (eq 0 status)
	       (> (length out) 0)
                         ;; Posix
	       (null (or (string-match ".*: No such file or directory$" out)
                         ;; MS-Windows
                         (string-match ".*: The system cannot find the file specified$" out))))
      (let ((state (aref out 0)))
	(cond
	 ((eq state ?=) 'up-to-date)
	 ((eq state ?A) 'added)
	 ((eq state ?M) 'edited)
	 ((eq state ?I) 'ignored)
	 ((eq state ?R) 'removed)
	 ((eq state ?!) 'missing)
	 ((eq state ??) 'unregistered)
	 ((eq state ?C) 'up-to-date) ;; Older mercurial versions use this.
	 (t 'up-to-date))))))

(defun vc-hg-working-revision (_file)
  "Hg-specific version of `vc-working-revision'."
  (ignore-errors
    (with-output-to-string
      (vc-hg-command standard-output 0 nil
                     "log" "-r" "." "--template" "{rev}"))))

(defcustom vc-hg-symbolic-revision-styles
  '(builtin-active-bookmark
    "{if(bookmarks,sub(' ',',',bookmarks),if(phabdiff,phabdiff,shortest(node,6)))}")
  "List of ways to present versions symbolically.
The version that we use is the first one that successfully
produces a non-empty string.

Each entry in the list can be either:

- The symbol `builtin-active-bookmark', which indicates that we
should use the active bookmark if one exists.  A template can
supply this information as well, but `builtin-active-bookmark' is
handled entirely inside Emacs and so is more efficient than using
the generic Mercurial mechanism.

- A string giving the Mercurial template to supply to \"hg
parent\".  \"hg help template\" may be useful reading.

- A function to call; it should accept two arguments (a revision
and an optional path to which to limit history) and produce a
string.  The function is called with `default-directory' set to
within the repository.

If no list entry produces a useful revision, return nil."
  :type '(repeat (choice
                  (const :tag "Active bookmark" builtin-active-bookmark)
                  (string :tag "Hg template")
                  (function :tag "Custom")))
  :version "26.1")

(defcustom vc-hg-use-file-version-for-mode-line-version nil
  "When enabled, the modeline contains revision information for the visited file.
When not, the revision in the modeline is for the repository
working copy.  nil is the much faster setting for
large repositories."
  :type 'boolean
  :version "26.1")

(defun vc-hg--active-bookmark-internal (rev)
  (when (equal rev ".")
    (let* ((current-bookmarks-file ".hg/bookmarks.current"))
      (when (file-exists-p current-bookmarks-file)
        (ignore-errors
          (with-temp-buffer
            (insert-file-contents current-bookmarks-file)
            (buffer-substring-no-properties
             (point-min) (point-max))))))))

(defun vc-hg--run-log (template rev path)
  (ignore-errors
    (with-output-to-string
      (if path
          (vc-hg-command
           standard-output 0 nil
           "log" "-f" "-l1" "--template" template path)
        (vc-hg-command
         standard-output 0 nil
         "log" "-r" rev "-l1" "--template" template)))))

(defun vc-hg--symbolic-revision (rev &optional path)
  "Make a Mercurial revision human-readable.
REV is a Mercurial revision.  `default-directory' is assumed to
be in the repository root of interest.  PATH, if set, is a
specific file to query."
  (let ((symbolic-revision nil)
        (styles vc-hg-symbolic-revision-styles))
    (while (and (not symbolic-revision) styles)
      (let ((style (pop styles)))
        (setf symbolic-revision
              (cond ((and (null path) (eq style 'builtin-active-bookmark))
                     (vc-hg--active-bookmark-internal rev))
                    ((stringp style)
                     (vc-hg--run-log style rev path))
                    ((functionp style)
                     (funcall style rev path))))))
    symbolic-revision))

(defun vc-hg-mode-line-string (file)
  "Hg-specific version of `vc-mode-line-string'."
  (pcase-let* ((backend-name "Hg")
               (truename (file-truename file))
               (state (vc-state truename))
               (`(,state-echo ,face ,indicator)
                (vc-mode-line-state state))
               (rev (and state
                         (let ((default-directory
                                (expand-file-name (vc-hg-root truename))))
                           (vc-hg--symbolic-revision
                            "."
                            (and vc-hg-use-file-version-for-mode-line-version
                                 truename)))))
               (rev (or rev "???"))
               (state-string (concat (unless (eq vc-display-status 'no-backend)
                                       backend-name)
                                     indicator rev)))
    (propertize state-string 'face face 'help-echo
                (concat state-echo " under the " backend-name
                        " version control system"))))

;;; History functions

(defcustom vc-hg-log-switches nil
  "String or list of strings specifying switches for hg log under VC."
  :type '(choice (const :tag "None" nil)
                 (string :tag "Argument String")
                 (repeat :tag "Argument List" :value ("") string)))

(autoload 'vc-setup-buffer "vc-dispatcher")

(defvar vc-hg-log-graph nil
  "If non-nil, use `--graph' in the short log output.")

(defvar vc-hg-log-format (concat "changeset:   {rev}:{node|short}\n"
                                 "{tags % 'tag:         {tag}\n'}"
                                 "{if(parents, 'parents:     {parents}\n')}"
                                 "user:        {author}\n"
                                 "Date:        {date|date}\n"
                                 "summary:     {desc|tabindent}\n\n")
  "Mercurial log template for `vc-hg-print-log' long format.")

(defun vc-hg-print-log (files buffer &optional shortlog start-revision limit)
  "Print commit log associated with FILES into specified BUFFER.
If SHORTLOG is non-nil, use a short format based on `vc-hg-root-log-format'.
If LIMIT is a positive integer, show no more than that many entries.
If LIMIT is a non-empty string, use it as a base revision.

If START-REVISION is nil, print the commit log starting from the working
directory parent (revset \".\").  If START-REVISION is a string, print
the log starting from that revision."
  ;; `vc-do-command' creates the buffer, but we need it before running
  ;; the command.
  (vc-setup-buffer buffer)
  ;; If the buffer exists from a previous invocation it might be
  ;; read-only.
  (let ((inhibit-read-only t)
        ;; Normalize START-REVISION parameter.
        (start (if (member start-revision '(nil ""))
                   "."
                 start-revision)))
    (with-current-buffer buffer
      (apply #'vc-hg-command buffer 'async files "log"
             ;; With Mercurial logs there are are, broadly speaking, two
             ;; kinds of ranges of revisions for the log to show:
             ;;   - ranges by revision number:   -rN:M
             ;;   - ranges according to the DAG: -rN::M or -rN..M
             ;; Note that N and M can be revision numbers or changeset
             ;; IDs (hashes).  In either case a revision number range
             ;; includes those commits with revision numbers between the
             ;; revision numbers of the commits identified by N and M.
             ;; See <https://repo.mercurial-scm.org/hg/help/revsets>.
             ;;
             ;; DAG ranges are not the same as Git's double-dot ranges.
             ;; Git's 'x..y' is more like Mercurial's 'only(y, x)' than
             ;; it is like Mercurial's x::y.  In addition, with -rN::M,
             ;; commits from other branches aren't included in the log.
             ;;
             ;; VC has always used ranges by revision numbers, such that
             ;; commits from all branches are included in the log.
             (cond ((not (stringp limit))
                    (format "-r%s:0" start))
                   ((eq vc-log-view-type 'log-outgoing)
                    (format "-rreverse(only(%s, %s))" start limit))
                   (t
                    (format "-r%s:%s & !%s" start limit limit)))
	     (nconc
              (and (numberp limit)
                   (list "-l" (format "%s" limit)))
              (and (eq vc-log-view-type 'with-diff)
                   (list "-p"))
	      (if shortlog
                  `(,@(and vc-hg-log-graph '("--graph"))
                    "--template"
                    ,(car vc-hg-root-log-format))
                `("--template" ,vc-hg-log-format))
	      vc-hg-log-switches)))))

(defvar log-view-message-re)
(defvar log-view-file-re)
(defvar log-view-font-lock-keywords)
(defvar log-view-per-file-logs)
(defvar log-view-expanded-log-entry-function)

(define-derived-mode vc-hg-log-view-mode log-view-mode "Hg-Log-View"
  (require 'add-log) ;; we need the add-log faces
  (let ((shortp (memq vc-log-view-type '(short log-incoming log-outgoing))))
   (setq-local log-view-file-re regexp-unmatchable)
   (setq-local log-view-per-file-logs nil)
   (setq-local log-view-message-re
               (if shortp
                   (cadr vc-hg-root-log-format)
                 "^changeset:[ \t]*\\([0-9]+\\):\\(.+\\)"))
   (setq-local tab-width 2)
   ;; Allow expanding short log entries.
   (when shortp
     (setq truncate-lines t)
     (setq-local log-view-expanded-log-entry-function
                 'vc-hg-expanded-log-entry))
   (setq-local log-view-font-lock-keywords
               (if shortp
                   (list (cons (nth 1 vc-hg-root-log-format)
                               (nth 2 vc-hg-root-log-format)))
                 (append
                  log-view-font-lock-keywords
                  '(
                    ;; Handle the case:
                    ;; user: FirstName LastName <foo@bar>
                    ("^user:[ \t]+\\([^<(]+?\\)[ \t]*[(<]\\([A-Za-z0-9_.+-]+@[A-Za-z0-9_.-]+\\)[>)]"
                     (1 'change-log-name)
                     (2 'change-log-email))
                    ;; Handle the cases:
                    ;; user: foo@bar
                    ;; and
                    ;; user: foo
                    ("^user:[ \t]+\\([A-Za-z0-9_.+-]+\\(?:@[A-Za-z0-9_.-]+\\)?\\)"
                     (1 'change-log-email))
                    ("^date: \\(.+\\)" (1 'change-log-date))
                    ("^tag: +\\([^ ]+\\)$" (1 'highlight))
                    ("^summary:[ \t]+\\(.+\\)" (1 'log-view-message))))))))

(autoload 'vc-switches "vc")

(defun vc-hg-region-history (file buffer lfrom lto)
  "Insert into BUFFER the history of FILE for lines LFROM to LTO.
This requires hg 4.4 or later, for the \"-L\" option of \"hg log\"."
  (vc-hg-command buffer 'async nil "log" "-f" "-p" "-L"
                  (format "%s,%d:%d" (file-relative-name file) lfrom lto)))

(require 'diff-mode)

(defvar vc-hg-region-history-mode-map
  (let ((map (make-composed-keymap
              nil (make-composed-keymap
                   (list diff-mode-map vc-hg-log-view-mode-map)))))
    map))

(defvar vc-hg--log-view-long-font-lock-keywords nil)
(defvar vc-hg-region-history-font-lock-keywords
  '((vc-hg-region-history-font-lock)))

(defun vc-hg-region-history-font-lock (limit)
  (let ((in-diff (save-excursion
                   (beginning-of-line)
                   (or (looking-at "^\\(?:diff\\|changeset\\)\\>")
                       (re-search-backward "^\\(?:diff\\|changeset\\)\\>"
                                           nil t))
                   (eq ?d (char-after (match-beginning 0))))))
    (while
        (let ((end (save-excursion
                     (if (re-search-forward "\n\\(diff\\|changeset\\)\\>"
                                            limit t)
                         (match-beginning 1)
                       limit))))
          (let ((font-lock-keywords (if in-diff diff-font-lock-keywords
                                      vc-hg--log-view-long-font-lock-keywords)))
            (font-lock-fontify-keywords-region (point) end))
          (goto-char end)
          (prog1 (< (point) limit)
            (setq in-diff (eq ?d (char-after))))))
    nil))

(define-derived-mode vc-hg-region-history-mode
    vc-hg-log-view-mode "Hg-Region-History"
  "Major mode to browse Hg's \"log -p\" output."
  (setq-local vc-hg--log-view-long-font-lock-keywords
              log-view-font-lock-keywords)
  (setq-local font-lock-defaults
              (cons 'vc-hg-region-history-font-lock-keywords
                    (cdr font-lock-defaults))))

(defun vc-hg-diff (files &optional oldvers newvers buffer async)
  "Get a difference report using hg between two revisions of FILES."
  (let* ((firstfile (car files))
         (working (and firstfile (vc-working-revision firstfile 'Hg))))
    (when (and (not newvers) (member oldvers (list working ".")))
      (setq oldvers nil))
    (when (and newvers (not oldvers))
      (setq oldvers working))
    (apply #'vc-hg-command
	   (or buffer "*vc-diff*")
           (if async 'async 1)
           files "diff"
           (append
            (vc-switches 'hg 'diff)
            (when oldvers
              (if newvers
                  (list "-r" oldvers "-r" newvers)
                (list "-r" oldvers)))))))

(defun vc-hg-expanded-log-entry (revision)
  (with-temp-buffer
    (vc-hg-command t nil nil "log" "-r" revision "--template" vc-hg-log-format)
    (goto-char (point-min))
    (unless (eobp)
      ;; Indent the expanded log entry.
      (indent-region (point-min) (point-max) 2)
      (goto-char (point-max))
      (buffer-string))))

(defun vc-hg-revision-table (files)
  (let ((default-directory (file-name-directory (car files))))
    (with-temp-buffer
      (vc-hg-command t nil nil "branches" "-q")
      (vc-hg-command t nil nil "bookmarks" "-q")
      (vc-hg-command t nil nil "tags" "-q")
      (split-string
       (buffer-substring-no-properties (point-min) (point-max))))))

;; Modeled after the similar function in vc-cvs.el
(defun vc-hg-revision-completion-table (files)
  (letrec ((table (lazy-completion-table
                   table (lambda () (vc-hg-revision-table files)))))
    table))

(defun vc-hg-annotate-command (file buffer &optional revision)
  "Execute \"hg annotate\" on FILE, inserting the contents in BUFFER.
Optional arg REVISION is a revision to annotate from."
  (apply #'vc-hg-command buffer 'async file "annotate" "-dq" "-n"
	 (append (vc-switches 'hg 'annotate)
                 (if revision (list (concat "-r" revision))))))

(declare-function vc-annotate-convert-time "vc-annotate" (&optional time))

;; One line printed by "hg annotate -dq -n -u --follow" looks like this:
;;   b56girard 114590 2012-03-13 CLOBBER: Lorem ipsum dolor sit
;; i.e. AUTHOR REVISION DATE FILENAME: CONTENTS
;; The user can omit options "-u" and/or "--follow".  Then it'll look like:
;;   114590 2012-03-13 CLOBBER:
;; or
;;   b56girard 114590 2012-03-13:
(defconst vc-hg-annotate-re
  (concat
   "^\\(?: *[^ ]+ +\\)?\\([0-9]+\\) "   ;User and revision.
   "\\([0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]\\)" ;Date.
   "\\(?: +\\([^:]+\\)\\)?:"))                        ;Filename.

(defun vc-hg-annotate-time ()
  (when (looking-at vc-hg-annotate-re)
    (goto-char (match-end 0))
    (vc-annotate-convert-time
     (let ((str (match-string-no-properties 2)))
       (encode-time 0 0 0
                    (string-to-number (substring str 8 10))
                    (string-to-number (substring str 5 7))
                    (string-to-number (substring str 0 4)))))))

(defun vc-hg-annotate-extract-revision-at-line ()
  (save-excursion
    (beginning-of-line)
    (when (looking-at vc-hg-annotate-re)
      (if (match-beginning 3)
          (cons (match-string-no-properties 1)
                (expand-file-name (match-string-no-properties 3)
                                  (vc-hg-root default-directory)))
        (match-string-no-properties 1)))))

;;; Tag system

(defun vc-hg-create-tag (dir name branchp)
  "Create tag NAME in repo in DIR.  Create branch if BRANCHP.
Variable `vc-hg-create-bookmark' controls what kind of branch will be created."
  (let ((default-directory dir))
    (vc-hg-command nil 0 nil
                   (if branchp
                       (if (if (eq vc-hg-create-bookmark 'ask)
                               (yes-or-no-p "Create bookmark instead of branch? ")
                             vc-hg-create-bookmark)
                           "bookmark"
                         "branch")
                     "tag")
                   name)))

(defun vc-hg-retrieve-tag (dir name _update)
  "Retrieve the version tagged by NAME of all registered files at or below DIR."
  (let ((default-directory dir))
    (vc-hg-command nil 0 nil "update" (and (not (equal name ""))
                                           name))
    ;; TODO: update *vc-change-log* buffer so can see @ if --graph
    ))

;;; Native data structure reading

(defcustom vc-hg-parse-hg-data-structures t
  "If true, try parsing Mercurial data structures directly.
This is done instead of always running Mercurial.  We try to be safe
against Mercurial data structure format changes and always fall
back to running Mercurial directly."
  :type 'boolean
  :version "26.1")

(defsubst vc-hg--read-u8 ()
  "Read and advance over an unsigned byte.
Return the byte's value as an integer."
  (prog1 (char-after)
    (forward-char)))

(defsubst vc-hg--read-u32-be ()
  "Read and advance over a big-endian unsigned 32-bit integer."
  ;; Because elisp bytecode has an instruction for multiply and
  ;; doesn't have one for shift, it's somewhat counter-intuitively
  ;; faster to multiply than to shift.
  (+ (* (vc-hg--read-u8) (* 256 256 256))
     (* (vc-hg--read-u8) (* 256 256))
     (* (vc-hg--read-u8) 256)
     (identity (vc-hg--read-u8))))

(defun vc-hg--raw-dirstate-search (dirstate fname)
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally dirstate)
    (let* ((result nil)
           (flen (length fname))
           (case-fold-search nil)
           ;; Find a conservative bound for the loop below by using
           ;; Boyer-Moore on the raw dirstate without parsing it; we
           ;; know we can't possibly find fname _after_ the last place
           ;; it appears, so we can bail out early if we try to parse
           ;; past it, which especially helps when the file we're
           ;; trying to find isn't in dirstate at all.  There's no way
           ;; to similarly bound the starting search position, since
           ;; the file format is such that we need to parse it from
           ;; the beginning to find record boundaries.
           (search-limit
            (progn
              (goto-char (point-max))
              (or (search-backward fname (+ (point-min) 40) t)
                  (point-min)))))
      ;; 40 is just after the header, which contains the working
      ;; directory parents
      (goto-char (+ (point-min) 40))
      ;; Iterate over all dirstate entries; we might run this loop
      ;; hundreds of thousands of times, so performance is important
      ;; here
      (while (< (point) search-limit)
        ;; 1+4*4 is the length of the dirstate item header.
        (forward-char (1+ (* 3 4)))
        (let ((this-flen (vc-hg--read-u32-be)))
          (if (and (or (eq this-flen flen)
                       (and (> this-flen flen)
                            (eq (char-after (+ (point) flen)) 0)))
                   (search-forward fname (+ (point) flen) t))
              (progn
                (backward-char (+ flen (1+ (* 4 4))))
                (setf result
                      (list (vc-hg--read-u8)     ; status
                            (vc-hg--read-u32-be) ; mode
                            (vc-hg--read-u32-be) ; size (of file)
                            (vc-hg--read-u32-be) ; mtime
                            ))
                (goto-char (point-max)))
            (forward-char this-flen))))
      result)))

(define-error 'vc-hg-unsupported-syntax "unsupported hgignore syntax")

(defconst vc-hg--pcre-c-escapes
  '((?a . ?\a)
    (?b . ?\b)
    (?f . ?\f)
    (?n . ?\n)
    (?r . ?\r)
    (?t . ?\t)
    (?n . ?\n)
    (?r . ?\r)
    (?t . ?\t)
    (?v . ?\v)))

(defconst vc-hg--pcre-metacharacters
  '(?. ?^ ?$ ?* ?+ ?? ?{ ?\\ ?\[ ?\| ?\())

(defconst vc-hg--elisp-metacharacters
  '(?. ?* ?+ ?? ?\[ ?$ ?\\))

(defun vc-hg--escape-for-pcre (c)
  (if (memq c vc-hg--pcre-metacharacters)
      (string ?\\ c)
    c))

(defun vc-hg--parts-to-string (parts)
  "Build a string from list PARTS.  Each element is a character or string."
  (let ((parts2 nil))
    (while parts
      (let* ((partcell (prog1 parts (setf parts (cdr parts))))
             (part (car partcell)))
        (if (stringp part)
            (setf parts2 (nconc (append part nil) parts2))
          (setcdr partcell parts2)
          (setf parts2 partcell))))
    (apply #'string parts2)))

(defun vc-hg--pcre-to-elisp-re (pcre prefix)
  "Transform PCRE, a Mercurial file PCRE, into an elisp RE against PREFIX.
PREFIX is the directory name of the directory against which these
patterns are rooted.  We understand only a subset of PCRE syntax;
if we don't understand a construct, we signal
`vc-hg-unsupported-syntax'."
  (cl-assert (and (file-name-absolute-p prefix)
                  (directory-name-p prefix)))
  (let ((parts nil)
        (i 0)
        (anchored nil)
        (state 'normal)
        (pcrelen (length pcre)))
    (while (< i pcrelen)
      (let ((c (aref pcre i)))
        (cond ((eq state 'normal)
               (cond ((string-match
                       (rx (| "}\\?" (: "(?" (not (any ":")))))
                       pcre i)
                      (signal 'vc-hg-unsupported-syntax (list pcre)))
                     ((eq c ?\\)
                      (setf state 'backslash))
                     ((eq c ?\[)
                      (setf state 'charclass-enter)
                      (push c parts))
                     ((eq c ?^)
                      (if (eq i 0) (setf anchored t)
                        (signal 'vc-hg-unsupported-syntax (list pcre))))
                     ((eq c ?$)
                      ;; Patterns can also match directories exactly,
                      ;; ignoring everything under a matched directory
                      (push "\\(?:$\\|/\\)" parts))
                     ((memq c '(?| ?\( ?\)))
                      (push ?\\ parts)
                      (push c parts))
                     (t (push c parts))))
              ((eq state 'backslash)
               (cond ((memq c '(?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9
                                ?A ?b ?B ?d ?D ?s ?S ?w ?W ?Z ?x))
                      (signal 'vc-hg-unsupported-syntax (list pcre)))
                     ((memq c vc-hg--elisp-metacharacters)
                      (push ?\\ parts)
                      (push c parts))
                     (t (push (or (cdr (assq c vc-hg--pcre-c-escapes)) c) parts)))
               (setf state 'normal))
              ((eq state 'charclass-enter)
               (push c parts)
               (setf state
                     (if (eq c ?\\)
                         'charclass
                       'charclass-backslash)))
              ((eq state 'charclass-backslash)
               (if (memq c '(?0 ?x))
                   (signal 'vc-hg-unsupported-syntax (list pcre)))
               (push (or (cdr (assq c vc-hg--pcre-c-escapes)) c) parts)
               (setf state 'charclass))
              ((eq state 'charclass)
               (push c parts)
               (cond ((eq c ?\\) (setf state 'charclass-backslash))
                     ((eq c ?\]) (setf state 'normal))))
              (t (error "Invalid state")))
        (setf i (1+ i))))
    (unless (eq state 'normal)
      (signal 'vc-hg-unsupported-syntax (list pcre)))
    (concat
     "^"
     prefix
     (if anchored "" "\\(?:.*/\\)?")
     (vc-hg--parts-to-string parts))))

(defun vc-hg--glob-to-pcre (glob)
  "Transform a glob pattern into a Mercurial file pattern regex."
  (let ((parts nil) (i 0) (n (length glob)) (group 0) c)
    (cl-macrolet ((peek () '(and (< i n) (aref glob i))))
      (while (< i n)
        (setf c (aref glob i))
        (incf i)
        (cond ((not (memq c '(?* ?? ?\[ ?\{ ?\} ?, ?\\)))
               (push (vc-hg--escape-for-pcre c) parts))
              ((eq c ?*)
               (cond ((eq (peek) ?*)
                      (incf i)
                      (cond ((eq (peek) ?/)
                             (incf i)
                             (push "(?:.*/)?" parts))
                            (t
                             (push ".*" parts))))
                     (t (push "[^/]*" parts))))
              ((eq c ??)
               (push ?. parts))
              ((eq c ?\[)
               (let ((j i))
                 (when (and (< j n) (memq (aref glob j) '(?! ?\])))
                   (incf j))
                 (while (and (< j n) (not (eq (aref glob j) ?\])))
                   (incf j))
                 (cond ((>= j n)
                        (push "\\[" parts))
                       (t
                        (let ((x (substring glob i j)))
                          (setf x (string-replace
                                   "\\" "\\\\" x))
                          (setf i (1+ j))
                          (cond ((eq (aref x 0) ?!)
                                 (setf (aref x 0) ?^))
                                ((eq (aref x 0) ?^)
                                 (setf x (concat "\\" x))))
                          (push ?\[ parts)
                          (push x parts)
                          (push ?\] parts))))))
              ((eq c ?\{)
               (incf group)
               (push "(?:" parts))
              ((eq c ?\})
               (push ?\) parts)
               (decf group))
              ((and (eq c ?,) (> group 0))
               (push ?| parts))
              ((eq c ?\\)
               (if (eq i n)
                   (push "\\\\" parts)
                 (incf i)
                 (push ?\\ parts)
                 (push c parts)))
              (t
               (push (vc-hg--escape-for-pcre c) parts)))))
    (concat (vc-hg--parts-to-string parts) "$")))

(defvar vc-hg--hgignore-patterns)
(defvar vc-hg--hgignore-filenames)

(defun vc-hg--hgignore-add-pcre (pcre prefix)
  (push (vc-hg--pcre-to-elisp-re pcre prefix) vc-hg--hgignore-patterns))

(defun vc-hg--hgignore-add-glob (glob prefix)
  (push (vc-hg--pcre-to-elisp-re (vc-hg--glob-to-pcre glob) prefix)
        vc-hg--hgignore-patterns))

(defun vc-hg--hgignore-add-path (path prefix)
  (let ((parts nil))
    (dotimes (i (length path))
      (push (vc-hg--escape-for-pcre (aref path i)) parts))
    (vc-hg--hgignore-add-pcre
     (concat "^" (vc-hg--parts-to-string parts) "$")
     prefix)))

(defun vc-hg--slurp-hgignore-1 (hgignore prefix)
  (let ((default-syntax 'vc-hg--hgignore-add-pcre))
    (with-temp-buffer
      (let ((attr (file-attributes hgignore)))
        (when attr (insert-file-contents hgignore))
        (push (list hgignore (file-attribute-modification-time attr) (file-attribute-size attr))
              vc-hg--hgignore-filenames))
      (while (not (eobp))
        ;; This list of pattern-file commands isn't complete, but it
        ;; should cover the common cases.  Remember that we fall back
        ;; to regular hg commands if we see something we don't like.
        (save-restriction
          (narrow-to-region (point) (line-end-position))
          (cond ((looking-at "[ \t]*\\(?:#.*\\)?$"))
                ((looking-at "syntax:[ \t]*re[ \t]*$")
                 (setf default-syntax 'vc-hg--hgignore-add-pcre))
                ((looking-at "syntax:[ \t]*glob[ \t]*$")
                 (setf default-syntax 'vc-hg--hgignore-add-glob))
                ((looking-at "path:\\(.+?\\)[ \t]*$")
                 (vc-hg--hgignore-add-path (match-string 1) prefix))
                ((looking-at "glob:\\(.+?\\)[ \t]*$")
                 (vc-hg--hgignore-add-glob (match-string 1) prefix))
                ((looking-at "re:\\(.+?\\)[ \t]*$")
                 (vc-hg--hgignore-add-pcre (match-string 1) prefix))
                ((looking-at "\\(sub\\)?include:\\(.+?\\)[ \t]*$")
                 (let* ((sub (equal (match-string 1) "sub"))
                        (arg (match-string 2))
                        (included-file
                         (if (string-match "^/" arg) arg
                           (concat (file-name-directory hgignore) arg))))
                   (vc-hg--slurp-hgignore-1
                    included-file
                    (if sub (file-name-directory included-file) prefix))))
                ((looking-at "[a-zA-Z0-9_]*:")
                 (signal 'vc-hg-unsupported-syntax (list (match-string 0))))
                ((looking-at ".*$")
                 (funcall default-syntax (match-string 0) prefix))))
        (forward-line 1)))))

(cl-defstruct (vc-hg--ignore-patterns
                (:copier nil)
                (:constructor vc-hg--ignore-patterns-make))
  repo
  ignore-patterns
  file-sources)

(defun vc-hg--slurp-hgignore (repo)
  "Read hg ignore patterns from REPO.
REPO must be the directory name of an hg repository."
  (cl-assert (and (file-name-absolute-p repo)
                  (directory-name-p repo)))
  (let* ((hgignore (concat repo ".hgignore"))
         (vc-hg--hgignore-patterns nil)
         (vc-hg--hgignore-filenames nil))
    (vc-hg--slurp-hgignore-1 hgignore repo)
    (vc-hg--ignore-patterns-make
     :repo repo
     :ignore-patterns (nreverse vc-hg--hgignore-patterns)
     :file-sources (nreverse vc-hg--hgignore-filenames))))

(defun vc-hg--ignore-patterns-valid-p (hgip)
  "Return whether the cached ignore patterns in HGIP are still valid."
  (let ((valid t)
        (file-sources (vc-hg--ignore-patterns-file-sources hgip)))
    (while (and file-sources valid)
      (let* ((fs (pop file-sources))
             (saved-mtime (nth 1 fs))
             (saved-size (nth 2 fs))
             (attr (file-attributes (nth 0 fs)))
             (current-mtime (file-attribute-modification-time attr))
             (current-size (file-attribute-size attr)))
	(unless (and (time-equal-p saved-mtime current-mtime)
                     (equal saved-size current-size))
          (setf valid nil))))
    valid))

(defun vc-hg--ignore-patterns-ignored-p (hgip filename)
  "Test whether the ignore pattern set HGIP says to ignore FILENAME.
FILENAME must be the file's true absolute name."
  (let ((patterns (vc-hg--ignore-patterns-ignore-patterns hgip))
        (ignored nil))
    (while (and patterns (not ignored))
      (setf ignored (string-match-p (pop patterns) filename)))
    ignored))

(defvar vc-hg--cached-ignore-patterns nil
  "Cached pre-parsed hg ignore patterns.")

(defun vc-hg--file-ignored-p (repo repo-relative-filename)
  (let ((hgip vc-hg--cached-ignore-patterns))
    (unless (and hgip
                 (equal repo (vc-hg--ignore-patterns-repo hgip))
                 (vc-hg--ignore-patterns-valid-p hgip))
      (setf vc-hg--cached-ignore-patterns nil)
      (setf hgip (vc-hg--slurp-hgignore repo))
      (setf vc-hg--cached-ignore-patterns hgip))
    (vc-hg--ignore-patterns-ignored-p
     hgip
     (concat repo repo-relative-filename))))

(defun vc-hg--read-repo-requirements (repo)
  (cl-assert (and (file-name-absolute-p repo)
                  (directory-name-p repo)))
  (let* ((requires-filename (concat repo ".hg/requires")))
    (and (file-exists-p requires-filename)
         (with-temp-buffer
           (set-buffer-multibyte nil)
           (insert-file-contents-literally requires-filename)
           (split-string (buffer-substring-no-properties
                          (point-min) (point-max)))))))

(defconst vc-hg-supported-requirements
  '("dotencode"
    "fncache"
    "generaldelta"
    "lz4revlog"
    "remotefilelog"
    "revlogv1"
    "store")
  "List of Mercurial repository requirements we understand.
If a repository requires features not present in this list, we
avoid attempting to parse Mercurial data structures.")

(defun vc-hg--requirements-understood-p (repo)
  "Check that we understand the format of the given repository.
REPO is the directory name of a Mercurial repository."
  (null (cl-set-difference (vc-hg--read-repo-requirements repo)
                           vc-hg-supported-requirements
                           :test #'equal)))

(defvar vc-hg--dirstate-scan-cache nil
  "Cache of the last result of `vc-hg--raw-dirstate-search'.
Avoids the need to repeatedly scan dirstate on repeated calls to
`vc-hg-state', as we see during registration queries.")

(defun vc-hg--cached-dirstate-search (dirstate dirstate-attr ascii-fname)
  (let* ((mtime (file-attribute-modification-time dirstate-attr))
         (size (file-attribute-size dirstate-attr))
         (cache vc-hg--dirstate-scan-cache)
         )
    (if (and cache
             (equal dirstate (pop cache))
	     (time-equal-p mtime (pop cache))
             (equal size (pop cache))
             (equal ascii-fname (pop cache)))
        (pop cache)
      (let ((result (save-match-data
                      (vc-hg--raw-dirstate-search dirstate ascii-fname))))
        (setf vc-hg--dirstate-scan-cache
              (list dirstate mtime size ascii-fname result))
        result))))

(defun vc-hg-state-fast (filename)
  "Like `vc-hg-state', but parse internal data structures directly.
Returns one of the usual `vc-state' enumeration values or
`unsupported' if we need to take the slow path and run the
hg binary."
  (let* (truename
         repo
         dirstate
         dirstate-attr
         repo-relative-filename)
    (if (or
         ;; Explicit user disable
         (not vc-hg-parse-hg-data-structures)
         ;; It'll probably be faster to run hg remotely
         (file-remote-p filename)
         (progn
           (setf truename (file-truename filename))
           (file-remote-p truename))
         (not (setf repo (vc-hg-root truename)))
         ;; dirstate must exist
         (not (progn
                (setf repo (expand-file-name repo))
                (cl-assert (and (file-name-absolute-p repo)
                                (directory-name-p repo)))
                (setf dirstate (concat repo ".hg/dirstate"))
                (setf dirstate-attr (file-attributes dirstate))))
         ;; Repository must be in an understood format
         (not (vc-hg--requirements-understood-p repo))
         ;; Dirstate too small to be valid
         (< (file-attribute-size dirstate-attr) 40)
         (progn
           (setf repo-relative-filename
                 (file-relative-name truename repo))
           ;; We only try dealing with ASCII filenames
           (string-match-p "[^[:ascii:]]" repo-relative-filename)))
        'unsupported
      (let* ((dirstate-entry
              (vc-hg--cached-dirstate-search
               dirstate dirstate-attr repo-relative-filename))
             (state (car dirstate-entry))
             (stat (file-attributes
                    (concat repo repo-relative-filename))))
        (cond ((eq state ?r) 'removed)
              ((and (not state) stat)
               (condition-case nil
                   (if (vc-hg--file-ignored-p repo repo-relative-filename)
                       'ignored
                     'unregistered)
                 (vc-hg-unsupported-syntax 'unsupported)))
              ((and state (not stat)) 'missing)
              ((eq state ?n)
               (let ((vc-hg-size (nth 2 dirstate-entry))
                     (vc-hg-mtime (nth 3 dirstate-entry))
                     (fs-size (file-attribute-size stat))
		     (fs-mtime (time-convert
				(file-attribute-modification-time stat)
				'integer)))
                 (if (and (eql vc-hg-size fs-size) (eql vc-hg-mtime fs-mtime))
                     'up-to-date
                   'edited)))
              ((eq state ?a) 'added)
              (state 'unsupported))))))

;;; Miscellaneous

(defun vc-hg-previous-revision (_file rev)
  ;; Prefer to return values with tildes not carets because that's more
  ;; compatible with MS-Windows (see `vc-git-previous-revision').
  ;;
  ;; See <https://repo.mercurial-scm.org/hg/help/revsets> for reference.
  (cond ((string-match "\\`\\.\\(\\^*\\)\\'" rev)
         (format ".~%d" (1+ (length (match-string 1 rev)))))
        ((string-match "\\`\\.~\\([0-9]+\\)\\'" rev)
         (format ".~%d" (1+ (string-to-number (match-string 1 rev)))))
        (t
         ;; We can't simply decrement by 1, because that revision might
         ;; be e.g. on a different branch (bug#22032).
         (with-temp-buffer
           (and (zerop (vc-hg-command t nil nil "id" "-n"
                                      "-r" (concat rev "~1")))
                ;; Trim the trailing newline.
                (buffer-substring (point-min) (1- (point-max))))))))

(defun vc-hg-next-revision (_file rev)
  (let ((newrev (1+ (string-to-number rev)))
        (tip-revision
         (with-temp-buffer
           (vc-hg-command t 0 nil "tip" "--style=default")
           (goto-char (point-min))
           (re-search-forward "^changeset:[ \t]*\\([0-9]+\\):")
           (string-to-number (match-string-no-properties 1)))))
    ;; We don't want to exceed the maximum possible revision number, ie
    ;; the tip revision.
    (when (<= newrev tip-revision)
      (number-to-string newrev))))

;; Modeled after the similar function in vc-bzr.el
(defun vc-hg-delete-file (file)
  "Delete FILE and delete it in the hg repository."
  (condition-case ()
      (delete-file file)
    (file-error nil))
  (vc-hg-command nil 0 file "remove" "--after" "--force"))

;; Modeled after the similar function in vc-bzr.el
(defun vc-hg-rename-file (old new)
  "Rename file from OLD to NEW using `hg mv'."
  (vc-hg-command nil 0 (expand-file-name new) "mv"
                 (expand-file-name old)))

(defun vc-hg-register (files &optional _comment)
  "Register FILES under hg.  COMMENT is ignored."
  (vc-hg-command nil 0 files "add"))

(defun vc-hg-create-repo ()
  "Create a new Mercurial repository."
  (vc-hg-command nil 0 nil "init"))

(defalias 'vc-hg-responsible-p #'vc-hg-root)

(defun vc-hg-unregister (file)
  "Unregister FILE from hg."
  (vc-hg-command nil 0 file "forget"))

(declare-function log-edit-extract-headers "log-edit" (headers string))
(declare-function log-edit-mode "log-edit" ())
(declare-function log-edit--toggle-amend "log-edit" (last-msg-fn))

(defun vc-hg-log-edit-toggle-amend ()
  "Toggle whether this will amend the previous commit.
If toggling on, also insert its message into the buffer."
  (interactive)
  (log-edit--toggle-amend
   (lambda ()
     (with-output-to-string
       (vc-hg-command
        standard-output 1 nil
        "log" "--limit=1" "--template" "{desc}")))))

(defvar-keymap vc-hg-log-edit-mode-map
  :name "Hg-Log-Edit"
  "C-c C-e" #'vc-hg-log-edit-toggle-amend)

(define-derived-mode vc-hg-log-edit-mode log-edit-mode "Log-Edit/hg"
  "Major mode for editing Hg log messages.
It is based on `log-edit-mode', and has Hg-specific extensions.")

(autoload 'vc-wait-for-process-before-save "vc-dispatcher")

(defalias 'vc-hg-async-checkins #'always)

(defalias 'vc-hg-working-revision-symbol (cl-constantly "."))

(defun vc-hg--checkin (comment &optional files patch-string)
  "Workhorse routine for `vc-hg-checkin' and `vc-hg-checkin-patch'.
COMMENT is the commit message; nil if it should come from PATCH-STRING.
For a regular checkin, FILES is the list of files to check in.
To check in a patch, PATCH-STRING is the patch text.
It is an error to supply both or neither."
  (unless (xor files patch-string)
    (error "Invalid call to `vc-hg--checkin'"))
  (let* ((args (and comment (vc-hg--extract-headers comment)))
         (temps-dir (or (file-name-directory (or (car files)
                                                 default-directory))
                        default-directory))
         (msg-file
          ;; On MS-Windows, pass the commit log message through a file,
          ;; to work around the limitation that command-line arguments
          ;; must be in the system codepage, and therefore might not
          ;; support non-ASCII characters in the log message.
          ;; Also handle remote files.
          (and args (eq system-type 'windows-nt)
               (let ((default-directory temps-dir))
                 (make-nearby-temp-file "hg-msg"))))
         (patch-file (and patch-string
                          (let ((default-directory temps-dir))
                            (make-nearby-temp-file "hg-patch")))))
    (let ((coding-system-for-write 'utf-8))
      (when msg-file
        (write-region (car args) nil msg-file))
      (when patch-file
        (write-region patch-string nil patch-file)))
    (let ((coding-system-for-write
           ;; On MS-Windows, we must encode command-line arguments in
           ;; the system codepage.
           (if (eq system-type 'windows-nt)
               locale-coding-system
             coding-system-for-write))
          (args
           (nconc (if patch-file
                      (list "import" "--bypass" patch-file)
                    (list "commit" "-A"))
                  (cond (msg-file (cl-list* "-l" (file-local-name msg-file)
                                            (cdr args)))
                        (args (cons "-m" args)))))
          (post
           (lambda ()
             (when (and msg-file (file-exists-p msg-file))
               (delete-file msg-file))
             (when (and patch-file (file-exists-p patch-file))
               (delete-file patch-file))
             ;; If PATCH-STRING didn't come from C-x v = or C-x v D, we
             ;; now need to update the working tree to include the
             ;; changes from the commit we just created.
             ;; If there are conflicts we want to favor the working
             ;; tree's version and the version from the commit will just
             ;; show up in the diff of uncommitted changes.
             ;;
             ;; When committing a patch we run two commands, 'hg import'
             ;; and then 'hg update'.  We have 'hg update' here in the
             ;; always-synchronous `post' function because we assume
             ;; that 'hg import' is the one that might be slow and so
             ;; benefits most from `vc-async-checkin'.  If in fact both
             ;; the 'hg import' and the 'hg update' can be slow, then we
             ;; need to make both of them part of the async command,
             ;; possibly by writing out a tiny shell script (bug#79235).
             (when patch-file
               (vc-hg-command nil 0 nil "update" "--merge"
                              "--tool" "internal:local" "tip")))))
      (if vc-async-checkin
          (let* ((buffer (vc-hg--async-buffer))
                 (proc (apply #'vc-hg--async-command buffer
                              (nconc args files))))
            (set-process-query-on-exit-flag proc t)
            (vc-wait-for-process-before-save
             proc
             (if patch-file
                 "Finishing checking in patch...."
               "Finishing checking in files..."))
            (with-current-buffer buffer
              (vc-run-delayed
                (vc-compilation-mode 'hg)
                (funcall post)))
            (vc-set-async-update buffer)
            (list 'async (get-buffer-process buffer)))
        (apply #'vc-hg-command nil 0 files args)
        (funcall post)))))

(defun vc-hg-checkin (files comment &optional _rev)
  "Hg-specific version of `vc-BACKEND-checkin'.
REV is ignored."
  (vc-hg--checkin comment files nil))

(defun vc-hg-checkin-patch (patch-string comment)
  "Hg-specific version of `vc-BACKEND-checkin-patch'."
  (vc-hg--checkin comment nil patch-string))

(defun vc-hg--extract-headers (comment)
  (log-edit-extract-headers `(("Author" . "--user")
                              ("Date" . "--date")
                              ("Amend" . ,(lambda (value)
                                            (when (equal value "yes")
                                              (list "--amend")))))
                            comment))

(defun vc-hg-find-revision (file rev buffer)
  (let ((coding-system-for-read 'binary)
        (coding-system-for-write 'binary))
    (if rev
        (vc-hg-command buffer 0 file "cat" "-r" rev)
      (vc-hg-command buffer 0 file "cat"))))

(defun vc-hg-file-name-changes (rev)
  (unless (member "--follow" vc-hg-log-switches)
    (with-temp-buffer
      (let ((root (vc-hg-root default-directory)))
        (vc-hg-command (current-buffer) t nil
                       "log" "-g" "-p" "-r" rev)
        (let (res)
          (goto-char (point-min))
          (while (re-search-forward "^diff --git a/\\([^ \n]+\\) b/\\([^ \n]+\\)" nil t)
            (when (not (equal (match-string 1) (match-string 2)))
              (push (cons
                     (expand-file-name (match-string 1) root)
                     (expand-file-name (match-string 2) root))
                    res)))
          (nreverse res))))))

(defun vc-hg-find-ignore-file (file)
  "Return the root directory of the repository of FILE."
  (expand-file-name ".hgignore"
		    (vc-hg-root file)))

;; Modeled after the similar function in vc-bzr.el
(defun vc-hg-checkout (file &optional rev)
  "Retrieve a revision of FILE.
EDITABLE is ignored.
REV is the revision to check out into WORKFILE."
  (let ((coding-system-for-read 'binary)
        (coding-system-for-write 'binary))
  (with-current-buffer (or (get-file-buffer file) (current-buffer))
    (if rev
        (vc-hg-command t 0 file "cat" "-r" rev)
      (vc-hg-command t 0 file "cat")))))

(defun vc-hg-resolve-when-done ()
  "Call \"hg resolve -m\" if the conflict markers have been removed."
  (save-excursion
    (goto-char (point-min))
    (unless (re-search-forward "^<<<<<<< " nil t)
      (vc-hg-command nil 0 buffer-file-name "resolve" "-m")
      ;; Remove the hook so that it is not called multiple times.
      (remove-hook 'after-save-hook #'vc-hg-resolve-when-done t))))

(defun vc-hg-find-file-hook ()
  (when (and buffer-file-name
             ;; Hg does not seem to have a "conflict" status, eg
             ;; hg http://bz.selenic.com/show_bug.cgi?id=2724
             (memq (vc-state buffer-file-name) '(edited conflict))
             ;; Maybe go on to check that "hg resolve -l" says "U"?
             ;; If "hg resolve -l" says there's a conflict but there are no
             ;; conflict markers, it's not clear what we should do.
             (save-excursion
               (goto-char (point-min))
               (re-search-forward "^<<<<<<< " nil t)))
    ;; Hg may not recognize "conflict" as a state, but we can do better.
    (vc-file-setprop buffer-file-name 'vc-state 'conflict)
    (smerge-start-session)
    (when (or (eq vc-hg-resolve-conflicts t)
              (and (eq vc-hg-resolve-conflicts 'default)
                   vc-resolve-conflicts))
      (add-hook 'after-save-hook #'vc-hg-resolve-when-done nil t))
    (vc-message-unresolved-conflicts buffer-file-name)))

(defun vc-hg-clone (remote directory rev)
  (if rev
      (vc-hg-command nil 0 '() "clone" "--rev" rev remote directory)
    (vc-hg-command nil 0 '() "clone" remote directory))

  directory)

;; Modeled after the similar function in vc-bzr.el
(defun vc-hg-revert (file &optional contents-done)
  (unless contents-done
    (with-temp-buffer
      (apply #'vc-hg-command t 0 file "revert"
             (append (vc-switches 'hg 'revert))))))

(defun vc-hg-revert-files (files)
  (with-temp-buffer
    (apply #'vc-hg-command t 0 files "revert"
           (append (vc-switches 'hg 'revert)))))

;;; Hg specific functionality.

(defvar-keymap vc-hg-extra-menu-map)

(defun vc-hg-extra-menu () vc-hg-extra-menu-map)

(defun vc-hg-extra-status-menu () vc-hg-extra-menu-map)

(defvar log-view-vc-backend)

(cl-defstruct (vc-hg-extra-fileinfo
            (:copier nil)
            (:constructor vc-hg-create-extra-fileinfo (rename-state extra-name))
            (:conc-name vc-hg-extra-fileinfo->))
  rename-state        ;; rename or copy state
  extra-name)         ;; original name for copies and rename targets, new name for

(declare-function vc-default-dir-printer "vc-dir" (backend fileentry))

(defun vc-hg-dir-printer (info)
  "Pretty-printer for the vc-dir-fileinfo structure."
  (let ((extra (vc-dir-fileinfo->extra info)))
    (vc-default-dir-printer 'Hg info)
    (when extra
      (insert (propertize
               (format "   (%s %s)"
                       (pcase (vc-hg-extra-fileinfo->rename-state extra)
                         ('copied "copied from")
                         ('renamed-from "renamed from")
                         ('renamed-to "renamed to"))
                       (vc-hg-extra-fileinfo->extra-name extra))
               'face 'font-lock-comment-face)))))

(defun vc-hg-after-dir-status (update-function)
  (let ((file nil)
        (translation '((?= . up-to-date)
                       (?C . up-to-date)
                       (?A . added)
                       (?R . removed)
                       (?M . edited)
                       (?I . ignored)
                       (?! . missing)
                       (?  . copy-rename-line)
                       (?? . unregistered)))
        (translated nil)
        (result nil)
        (last-added nil)
        (last-line-copy nil))
      (goto-char (point-min))
      (while (not (eobp))
        (setq translated (cdr (assoc (char-after) translation)))
        (setq file
              (buffer-substring-no-properties (+ (point) 2)
                                              (line-end-position)))
        (cond ((not translated)
               (setq last-line-copy nil))
              ((eq translated 'up-to-date)
               (setq last-line-copy nil))
              ((eq translated 'copy-rename-line)
               ;; For copied files the output looks like this:
               ;; A COPIED_FILE_NAME
               ;;   ORIGINAL_FILE_NAME
               (setf (nth 2 last-added)
                     (vc-hg-create-extra-fileinfo 'copied file))
               (setq last-line-copy t))
              ((and last-line-copy (eq translated 'removed))
               ;; For renamed files the output looks like this:
               ;; A NEW_FILE_NAME
               ;;   ORIGINAL_FILE_NAME
               ;; R ORIGINAL_FILE_NAME
               ;; We need to adjust the previous entry to not think it is a copy.
               (setf (vc-hg-extra-fileinfo->rename-state (nth 2 last-added))
                     'renamed-from)
               (push (list file translated
                           (vc-hg-create-extra-fileinfo
                            'renamed-to (nth 0 last-added))) result)
               (setq last-line-copy nil))
              (t
               (setq last-added (list file translated nil))
               (push last-added result)
               (setq last-line-copy nil)))
        (forward-line))
      (funcall update-function result)))

;; Follows vc-hg-command (or vc-do-async-command), which uses vc-do-command
;; from vc-dispatcher.
(declare-function vc-exec-after "vc-dispatcher" (code &optional okstatus proc))
;; Follows vc-exec-after.
(declare-function vc-set-async-update "vc-dispatcher" (process-buffer))

(defvar vc-hg--program-version nil)

(defun vc-hg--program-version ()
  (or vc-hg--program-version
      (setq vc-hg--program-version
            (with-temp-buffer
              (condition-case _ (vc-hg-command t 0 nil "version")
                (error "0")
                (:success
                 (goto-char (point-min))
                 (re-search-forward "Mercurial Distributed SCM (version \\([0-9][0-9.]+\\)")
                 (string-trim-right (match-string 1) "\\.")))))))

(defun vc-hg-dir-status-files (dir files update-function)
  ;; XXX: We can't pass DIR directly to 'hg status' because that
  ;; returns all ignored files if FILES is non-nil (bug#22481).
  (let ((default-directory dir))
    (apply #'vc-hg-command '(t nil) 'async files
           "status" (concat "-mardu" (if files "i")) "-C"
           (if (version<= "4.2" (vc-hg--program-version))
               '("--config" "commands.status.relative=1")
             '("re:" "-I" "."))))
  (vc-run-delayed-success 0
    (vc-hg-after-dir-status update-function)))

(defun vc-hg-dir-extra-headers (dir)
  "Generate extra status headers for a repository in DIR.
This runs the command \"hg summary\"."
  (let ((default-directory dir))
    (with-temp-buffer
      (vc-hg-command t 0 nil "summary")
      (goto-char (point-min))
      (mapconcat
       #'identity
       (let (result)
         (while (not (eobp))
           (push
            (let ((entry (if (looking-at "\\([^ ].*\\): \\(.*\\)")
                             (cons (capitalize (match-string 1)) (match-string 2))
                           (cons "" (buffer-substring (point) (line-end-position))))))
              (concat
               (propertize (format "%-11s: " (car entry)) 'face 'vc-dir-header)
               (propertize (cdr entry) 'face 'vc-dir-header-value)))
            result)
           (forward-line))
         (nreverse result))
       "\n"))))

(defun vc-hg-incoming-revision (&optional upstream-location _refresh)
  (let* ((upstream-location (or upstream-location "default"))
         ;; Use 'hg identify' like this, and not 'hg incoming', because
         ;; this will give a sensible answer regardless of whether the
         ;; incoming revision has been pulled yet.
         (rev (with-output-to-string
                (vc-hg-command standard-output 0 nil "identify" "--id"
                               upstream-location "--template={node}"))))
    (condition-case _ (vc-hg-command nil 0 nil "log" "-r" rev)
      ;; We don't have the revision locally.  Pull it.
      (error (vc-hg-command nil 0 nil "pull" upstream-location)))
    rev))

(defun vc-hg-mergebase (rev1 &optional rev2)
  (with-output-to-string
    (vc-hg-command standard-output 0 nil "log"
                   (format "--rev=last(ancestors(%s) and ancestors(%s))"
                           rev1 (or rev2 "."))
                   "--limit=1" "--template={node}")))

(defvar vc-hg-error-regexp-alist
  '(("^M \\(.+\\)" 1 nil nil 0))
  "Value of `compilation-error-regexp-alist' in *vc-hg* buffers.")

(autoload 'vc-do-async-command "vc-dispatcher")
(autoload 'log-view-get-marked "log-view")
(defvar compilation-directory)
(defvar compilation-arguments)  ; defined in compile.el

(defun vc-hg--pushpull (command prompt post-processing &optional obsolete)
  "Run COMMAND (a string; either push or pull) on the current Hg branch.
If PROMPT is non-nil, prompt for the Hg command to run.
POST-PROCESSING is a list of commands to execute after the command.
If OBSOLETE is non-nil, behave like the old versions of the Hg push/pull
commands: when called interactively in a Log View buffer with marked
revisions, fetch only those revisions."
  (let (marked-list)
    ;; The `vc-hg-pull' and `vc-hg-push' commands existed before the
    ;; `pull'/`push' VC actions were implemented.
    ;; The following is for backwards compatibility.
    (if (and obsolete (setq marked-list (log-view-get-marked)))
	(apply #'vc-hg-command
	       nil 0 nil
	       command
	       (apply #'nconc
		      (mapcar (lambda (arg) (list "-r" arg)) marked-list)))
      (let* ((root (vc-hg-root default-directory))
	     (buffer (format "*vc-hg : %s*" (expand-file-name root)))
	      ;; Disable pager.
             (process-environment (cons "HGPLAIN=1" process-environment))
	     (hg-program vc-hg-program)
	     args)
	;; If necessary, prompt for the exact command.
        ;; TODO if pushing, prompt if no default push location - cf bzr.
	(when prompt
	  (setq args (split-string
		      (read-shell-command
                       (format "Hg %s command: " command)
                       (format "%s %s" hg-program command)
                       'vc-hg-history)
		      " " t))
	  (setq hg-program (car  args)
		command    (cadr args)
		args       (cddr args)))
	(set-process-query-on-exit-flag
         (apply #'vc-do-async-command buffer root hg-program command args)
         t)
        (with-current-buffer buffer
          (vc-run-delayed
            (dolist (cmd post-processing)
              (apply #'vc-do-command buffer nil hg-program nil cmd))
            (vc-compilation-mode 'hg)
            (setq-local compile-command
                        (concat hg-program " " command " "
                                (mapconcat #'identity args " ")
                                (mapconcat (lambda (args)
                                             (concat " && " hg-program " "
                                                     (mapconcat #'identity
                                                                args " ")))
                                           post-processing "")))
            (setq-local compilation-directory root)
            ;; Either set `compilation-buffer-name-function' locally to nil
            ;; or use `compilation-arguments' to set `name-function'.
            ;; See `compilation-buffer-name'.
            (setq-local compilation-arguments
                        (list compile-command nil
                              (lambda (_name-of-mode) buffer)
                              nil))))
	(vc-set-async-update buffer)))))

(defun vc-hg-pull (prompt)
  "Issue a Mercurial pull command.
If called interactively with a set of marked Log View buffers,
call \"hg pull -r REVS\" to pull in the specified revisions REVS.

With a prefix argument or if PROMPT is non-nil, prompt for a
specific Mercurial pull command.  The default is \"hg pull -u\",
which fetches changesets from the default remote repository and
then attempts to update the working directory."
  (interactive "P")
  (vc-hg--pushpull "pull" prompt
                   ;; Fixme: before updating the working copy to the latest
                   ;; state, should check if it's visiting an old revision.
                   ;; post-processing: list modified files and update
                   ;; NB: this will not work with "pull = --rebase"
                   ;;     or "pull = --update" in hgrc.
                   '(("--pager" "no" "status" "--rev" "." "--rev" "tip")
                     ("update"))
                   (called-interactively-p 'interactive)))

(defun vc-hg-push (prompt)
  "Push changes from the current Mercurial branch.
Normally, this runs \"hg push\".  If PROMPT is non-nil, prompt
for the Hg command to run.

If called interactively with a set of marked Log View buffers,
call \"hg push -r REVS\" to push the specified revisions REVS."
  (interactive "P")
  (vc-hg--pushpull "push" prompt nil (called-interactively-p 'interactive)))

(defun vc-hg-merge-branch ()
  "Prompt for revision and merge it into working directory.
This runs the command \"hg merge\"."
  (let ((buffer (vc-hg--async-buffer))
        (branch (vc-read-revision "Revision to merge: ")))
    (apply #'vc-hg--async-command buffer
           (append '("--config" "ui.report_untrusted=0" "merge")
                   (and (not (string-empty-p branch)) (list branch))))
    (with-current-buffer buffer
      (vc-run-delayed
        (vc-compilation-mode 'hg)))
    (vc-set-async-update buffer)))

(defun vc-hg-prepare-patch (rev)
  (with-current-buffer (generate-new-buffer " *vc-hg-prepare-patch*")
    (vc-hg-command t 0 nil "export" "--git" "--rev" rev)
    (condition-case _
        (let (subject patch-start)
          (goto-char (point-min))
          (re-search-forward "^[^#].*")
          (setq subject (match-string 0))
          (re-search-forward "\n\ndiff --git a/")
          (setq patch-start (pos-bol))
          (list :subject subject
                :patch-start patch-start
                :buffer (current-buffer)))
      (search-failed (error "'hg export' output parse failure")))))

;;; Internal functions

(defun vc-hg-command (buffer okstatus file-or-list &rest flags)
  "A wrapper around `vc-do-command' for use in vc-hg.el.
This function differs from `vc-do-command' in that
- BUFFER nil means use a buffer called \"*vc*\"
- it invokes `vc-hg-program' and passes `vc-hg-global-switches' to it
  before FLAGS."
  ;; Commands which pass command line arguments which might
  ;; contain non-ASCII have to bind `coding-system-for-write' to
  ;; `locale-coding-system' when (eq system-type 'windows-nt)
  ;; because MS-Windows has the limitation that command line
  ;; arguments must be in the system codepage.  We do that only
  ;; within the commands which must do it, instead of implementing
  ;; it here, even though that means code repetition.  This is
  ;; because such a let-binding would have the disadvantage of
  ;; overriding any `coding-system-for-write' explicitly selected
  ;; by the user (e.g. with C-x RET c), or by enclosing function
  ;; calls.  So we want to do it only for commands which really
  ;; require it.
  (vc-hg--command-1 #'vc-do-command
                    (list (or buffer "*vc*")
                          okstatus vc-hg-program file-or-list)
                    flags))

(defun vc-hg--async-command (buffer &rest args)
  "Wrapper around `vc-do-async-command' like `vc-hg-command'."
  (vc-hg--command-1 #'vc-do-async-command
                    (list buffer (vc-hg-root default-directory)
                          vc-hg-program)
                    args))

(defun vc-hg--async-buffer ()
  "Buffer passed to `vc-do-async-command' by vg-hg.el commands.
Intended for use via the `vc-hg--async-command' wrapper."
  (format "*vc-hg : %s*"
          (expand-file-name (vc-hg-root default-directory))))

(defun vc-hg--command-1 (fun args flags)
  ;; Disable pager.
  (let ((process-environment (cons "HGPLAIN=1" process-environment)))
    (apply fun (append args
                       '("--config" "ui.report_untrusted=0")
                       (if (stringp vc-hg-global-switches)
                           (cons vc-hg-global-switches flags)
                         (append vc-hg-global-switches
                                 flags))))))

(defun vc-hg-root (file)
  (vc-find-root file ".hg"))

(defun vc-hg-repository-url (file-or-dir &optional remote-name)
  (let ((default-directory (vc-hg-root file-or-dir)))
    (with-temp-buffer
      (vc-hg-command (current-buffer) 0 nil
                     "config"
                     (concat "paths." (or remote-name "default")))
      (buffer-substring-no-properties (point-min) (1- (point-max))))))

(defun vc-hg-known-other-working-trees ()
  "Implementation of `known-other-working-trees' backend function for Hg."
  ;; Mercurial doesn't maintain records of shared repositories.
  ;; The first repository knows nothing about shares created from it,
  ;; and each share only has a reference back to the first repository.
  ;;
  ;; Therefore, to support the VC API for other working trees, Emacs
  ;; needs to maintain records of its own about other working trees.
  ;; Rather than create something new our strategy is to rely on
  ;; project.el's knowledge of existing projects.
  ;; Note that this relies on code calling `vc-hg-add-working-tree'
  ;; registering the resultant working tree with project.el.
  (let* ((our-root (vc-hg-root default-directory))
         (our-sp (expand-file-name ".hg/sharedpath" our-root))
         our-store shares)
    (if (file-exists-p our-sp)
        (with-temp-buffer
          (insert-file-contents-literally our-sp)
          ;; On MS-Windows, ".hg/sharedpath" gives file names with
          ;; backslashes; expand-file-name normalizes that to forward
          ;; slashes, needed for 'equal' comparison below.
          (setq our-store (expand-file-name (string-trim (buffer-string))))
          (push (abbreviate-file-name (file-name-directory our-store))
                shares))
      (setq our-store (expand-file-name ".hg" our-root)))
    (dolist (root (project-known-project-roots))
      (when-let* (((not (equal root our-root)))
                  (sp (expand-file-name ".hg/sharedpath" root))
                  ((file-exists-p sp)))
        (with-temp-buffer
          (insert-file-contents-literally sp)
          (when (equal our-store
                       ;; See above why we use expand-file-name
                       (expand-file-name (string-trim (buffer-string))))
            (push root shares)))))
    shares))

(defun vc-hg-add-working-tree (directory)
  "Implementation of `add-working-tree' backend function for Mercurial."
  (vc-hg-command nil 0 nil "share"
                 (vc-hg-root default-directory)
                 (expand-file-name directory)))

(defun vc-hg--shared-p (directory)
  (file-exists-p (expand-file-name ".hg/sharedpath" directory)))

(defun vc-hg-delete-working-tree (directory)
  "Implementation of `delete-working-tree' backend function for Mercurial."
  (if (vc-hg--shared-p directory)
      (delete-directory directory t t)
    (user-error "\
Cannot delete first working tree because this would break other working trees")))

(defun vc-hg-move-working-tree (from to)
  "Implementation of `move-working-tree' backend function for Mercurial."
  (if (vc-hg--shared-p from)
      (rename-file from (directory-file-name to) 1)
    (user-error "\
Cannot relocate first working tree because this would break other working trees")))

(defun vc-hg-cherry-pick-comment (_files rev reverse)
  (let (short long comment)
    (with-temp-buffer
      (vc-hg-command t 0 nil "log" "--limit=1"
                     (format "--rev=%s" rev)
                     "--template={node|short}\n{node}\n{desc}")
      (goto-char (point-min))
      (setq short (buffer-substring-no-properties (point) (pos-eol)))
      (forward-line 1)
      (setq long (buffer-substring-no-properties (point) (pos-eol)))
      (forward-line 1)
      (setq comment (buffer-substring-no-properties (point) (point-max))))
    (if reverse
        (format "Summary: Backed out changeset %s\n\n" short)
      ;; The additional line is indeed separated from the original
      ;; comment by just one line break, for 'hg graft'.
      (format "Summary: %s\n(grafted from %s)\n" comment long))))

(defun vc-hg-revision-published-p (rev)
  "Whether REV has been pushed such that it is public history.
Always has to fetch, like `vc-hg-incoming-revision' does."
  (with-temp-buffer
    (vc-hg-command t 0 nil "log" (format "--rev=outgoing() and %s" rev))
    (bobp)))

(defun vc-hg-delete-revision (rev)
  "Use `hg histedit' to delete REV from the history of the current branch.

`hg histedit' will fail unless
- REV is an ancestor of the working directory;
- all commits back to REV are not yet public; and
- there aren't any merges in the history to be edited."
  (with-temp-buffer
    ;; Resolve REV to a full changeset hash.
    (vc-hg-command t 0 nil "log" "--limit=1"
                   (format "--rev=%s" rev) "--template={node}")
    (when (bobp)
      ;; If REV is not found, hg exits 255, so this should never happen.
      (error "'hg log' unexpectedly gave no output"))
    (setq rev (buffer-string)))
  (let ((repo default-directory)
        (commands (make-nearby-temp-file "hg-commands")))
    (unwind-protect
        (let ((coding-system-for-write
               ;; On MS-Windows, we must encode command-line arguments
               ;; in the system codepage.
               (if (eq system-type 'windows-nt)
                   locale-coding-system
                 coding-system-for-write)))
          (with-temp-file commands
            (let ((default-directory repo))
              (vc-hg-command t 0 nil "log" (format "--rev=.:%s" rev)
                             "--template=pick {node}\n"))
            (goto-char (point-min))
            (unless (re-search-forward (format "^pick %s\n\\'" rev) nil t)
              (error "'hg log' output parse failure"))
            (replace-match (format "drop %s\n" rev)))
          (unless (zerop
                   (vc-hg-command
                    nil 1 nil
                    "--config=extensions.histedit="

                    ;; Without this, --commands is ignored and histedit
                    ;; starts a curses interface.  (Actually redundant
                    ;; with the HGPLAIN=1 set by vc-hg--command-1.)
                    "--config=ui.interface=text"

                    ;; Request validation of the commands file:
                    ;; stop if any commits in the history back to REV
                    ;; are missing, somehow.
                    "--config=histedit.dropmissing=False"

                    ;; Prevent Mercurial trying to open Meld or similar.
                    ;; FIXME: According to
                    ;; <https://repo.mercurial-scm.org/hg/help/merge-tools>,
                    ;; this can be overridden by user's "merge-patterns"
                    ;; settings.
                    "--config=ui.merge=internal:fail"

                    "histedit"
                    (format "--rev=%s" rev) "--commands" commands))
            ;; FIXME: Ideally we would leave some sort of conflict for
            ;; the user to resolve, instead of just giving up.
            ;; We would want C-x v v to do 'hg histedit --continue' like
            ;; how it can currently be used to conclude a merge after
            ;; resolving conflicts.
            (vc-hg-command nil 0 nil "--config=extensions.histedit="
                           "histedit" "--abort")
            (error "Merge conflicts while trying to delete %s; aborting"
                   rev)))
      (delete-file commands))))

(defun vc-hg--assert-rev-on-current-branch (rev)
  "Assert that REV is on the current branch."
  (with-temp-buffer
    (vc-hg-command t nil nil "log" "--limit=1"
                   (format "--rev=%s & ancestors(.)" rev)
                   "--template={node}")
    (when (bobp)
      (error "Revision %s is not on the current branch" rev))))

(defun vc-hg--reset-back-to (rev keep)
  "Strip revisions up to but not including REV.
If KEEP is non-nil, also pass --keep to `hg strip'."
  (apply #'vc-hg-command nil 0 nil
         "--config=extensions.strip="
         "strip" "--force"
         (format "--rev=descendants(%s) & !%s" rev rev)
         (and keep '("--keep"))))

(defun vc-hg-delete-revisions-from-end (rev)
  "Strip revisions up to but not including REV.
It is an error if REV is not on the current branch."
  (vc-hg--assert-rev-on-current-branch rev)
  (vc-hg--reset-back-to rev nil))

(defun vc-hg-uncommit-revisions-from-end (rev)
  "Strip revisions up to but not including REV w/o modifying working tree.
It is an error if REV is not on the current branch."
  (vc-hg--assert-rev-on-current-branch rev)
  (vc-hg--reset-back-to rev t))

(defun vc-hg--working-branch ()
  "Return alist with currently active bookmark, if any, and current branch.
Keys into the alist are `branch' and `bookmark', values are the name of
the currently active bookmark (or nil) and the name of the current
branch, as strings."
  (with-temp-buffer
    (vc-hg-command t nil nil "summary")
    (goto-char (point-min))
    (re-search-forward "^branch: \\(.+\\)$")
    (let ((alist `((branch . ,(match-string 1)))))
      (goto-char (point-min))
      (if (re-search-forward "^bookmarks: \\*\\(\\S-+\\)" nil t)
          (cl-acons 'bookmark (match-string 1) alist)
        alist))))

(defun vc-hg-working-branch ()
  "Return currently active bookmark if one exists, else current branch.
The return value is always a string."
  (let ((alist (vc-hg--working-branch)))
    (cdr (or (assq 'bookmark alist) (assq 'branch alist)))))

(defun vc-hg-trunk-or-topic-p ()
  "Return `topic' if there is a currently active bookmark, else nil."
  (and (assq 'bookmark (vc-hg--working-branch)) 'topic))

(defun vc-hg-topic-outgoing-base ()
  "Return outgoing base for current commit considered as a topic branch.
The current implementation always returns the name of the current
branch, meaning to query the remote head for the current branch
(and not any active bookmark if it also exists remotely).
This is based on the following assumptions:
(i) if there is an active bookmark, it will eventually be merged into
    whatever the remote head is
(ii) there is only one remote head for the current branch."
  (assq 'branch (vc-hg--working-branch)))

(provide 'vc-hg)

;;; vc-hg.el ends here
