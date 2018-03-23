;;; vc-hg.el --- VC backend for the mercurial version control system  -*- lexical-binding: t -*-

;; Copyright (C) 2006-2018 Free Software Foundation, Inc.

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

;;; Thanks:

;;; Bugs:

;;; Installation:

;;; Todo:

;; 1) Implement the rest of the vc interface. See the comment at the
;; beginning of vc.el. The current status is:

;; FUNCTION NAME                               STATUS
;; BACKEND PROPERTIES
;; * revision-granularity                      OK
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
;; - check-headers ()                          ??
;; - delete-file (file)                        TEST IT
;; - rename-file (old new)                     OK
;; - find-file-hook ()                         added for bug#10709

;; 2) Implement Stefan Monnier's advice:
;; vc-hg-registered and vc-hg-state
;; Both of those functions should be super extra careful to fail gracefully in
;; unexpected circumstances. The reason this is important is that any error
;; there will prevent the user from even looking at the file :-(
;; Ideally, just like in vc-arch and vc-cvs, checking that the file is under
;; mercurial's control and extracting the current revision should be done
;; without even using `hg' (this way even if you don't have `hg' installed,
;; Emacs is able to tell you this file is under mercurial's control).

;;; History:
;;

;;; Code:

(require 'cl-lib)

(eval-when-compile
  (require 'vc)
  (require 'vc-dir))

(declare-function vc-compilation-mode "vc-dispatcher" (backend))

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
  :version "22.2"
  :group 'vc-hg)

(defcustom vc-hg-diff-switches t ; Hg doesn't support common args like -u
  "String or list of strings specifying switches for Hg diff under VC.
If nil, use the value of `vc-diff-switches'.  If t, use no switches."
  :type '(choice (const :tag "Unspecified" nil)
                 (const :tag "None" t)
                 (string :tag "Argument String")
                 (repeat :tag "Argument List" :value ("") string))
  :version "23.1"
  :group 'vc-hg)

(defcustom vc-hg-annotate-switches '("-u" "--follow")
  "String or list of strings specifying switches for hg annotate under VC.
If nil, use the value of `vc-annotate-switches'.  If t, use no
switches."
  :type '(choice (const :tag "Unspecified" nil)
		 (const :tag "None" t)
		 (string :tag "Argument String")
		 (repeat :tag "Argument List" :value ("") string))
  :version "25.1"
  :group 'vc-hg)

(defcustom vc-hg-program "hg"
  "Name of the Mercurial executable (excluding any arguments)."
  :type 'string
  :group 'vc-hg)

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
  :type '(list string string (repeat sexp))
  :group 'vc-hg
  :version "24.5")


;; Clear up the cache to force vc-call to check again and discover
;; new functions when we reload this file.
(put 'Hg 'vc-functions nil)

;;; Properties of the backend

(defvar vc-hg-history nil)

(defun vc-hg-revision-granularity () 'repository)
(defun vc-hg-checkout-model (_files) 'implicit)

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
    (let ((state (vc-hg-state file)))  ; expensive
      (and state (not (memq state '(ignored unregistered)))))))

(defun vc-hg-state (file)
  "Hg-specific version of `vc-state'."
  (let ((state (vc-hg-state-fast file)))
    (if (eq state 'unsupported) (vc-hg-state-slow file) state)))

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
			 "--config" "alias.status=status"
			 "--config" "defaults.status="
			 "status" "-A" (file-relative-name file)))
                    ;; Some problem happened.  E.g. We can't find an `hg'
                    ;; executable.
                    (error nil)))))))
    (when (and (eq 0 status)
	       (> (length out) 0)
	       (null (string-match ".*: No such file or directory$" out)))
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

(defun vc-hg-working-revision (file)
  "Hg-specific version of `vc-working-revision'."
  (or (ignore-errors
        (with-output-to-string
          (vc-hg-command standard-output 0 file
                         "parent" "--template" "{rev}")))
      "0"))

(defcustom vc-hg-symbolic-revision-styles
  '(builtin-active-bookmark
    "{if(bookmarks,sub(' ',',',bookmarks),if(phabdiff,phabdiff,shortest(node,6)))}")
  "List of ways to present versions symbolically.  The version
that we use is the first one that successfully produces a
non-empty string.

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

If no list entry produces a useful revision, return `nil'."
  :type '(repeat (choice
                  (const :tag "Active bookmark" builtin-active-bookmark)
                  (string :tag "Hg template")
                  (function :tag "Custom")))
  :version "26.1"
  :group 'vc-hg)

(defcustom vc-hg-use-file-version-for-mode-line-version nil
  "When enabled, the modeline contains revision information for the visited file.
When not, the revision in the modeline is for the repository
working copy.  `nil' is the much faster setting for
large repositories."
  :type 'boolean
  :version "26.1"
  :group 'vc-hg)

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
  (let* ((backend-name "Hg")
         (truename (file-truename file))
         (state (vc-state truename))
         (state-echo nil)
         (face nil)
         (rev (and state
                   (let ((default-directory
                          (expand-file-name (vc-hg-root truename))))
                     (vc-hg--symbolic-revision
                      "."
                      (and vc-hg-use-file-version-for-mode-line-version
                           truename)))))
         (rev (or rev "???")))
    (propertize
     (cond ((or (eq state 'up-to-date)
                (eq state 'needs-update))
            (setq state-echo "Up to date file")
            (setq face 'vc-up-to-date-state)
            (concat backend-name "-" rev))
           ((eq state 'added)
            (setq state-echo "Locally added file")
            (setq face 'vc-locally-added-state)
            (concat backend-name "@" rev))
           ((eq state 'conflict)
            (setq state-echo "File contains conflicts after the last merge")
            (setq face 'vc-conflict-state)
            (concat backend-name "!" rev))
           ((eq state 'removed)
            (setq state-echo "File removed from the VC system")
            (setq face 'vc-removed-state)
            (concat backend-name "!" rev))
           ((eq state 'missing)
            (setq state-echo "File tracked by the VC system, but missing from the file system")
            (setq face 'vc-missing-state)
            (concat backend-name "?" rev))
           (t
            (setq state-echo "Locally modified file")
            (setq face 'vc-edited-state)
            (concat backend-name ":" rev)))
     'face face
     'help-echo (concat state-echo " under the " backend-name
                        " version control system"))))

;;; History functions

(defcustom vc-hg-log-switches nil
  "String or list of strings specifying switches for hg log under VC."
  :type '(choice (const :tag "None" nil)
                 (string :tag "Argument String")
                 (repeat :tag "Argument List" :value ("") string))
  :group 'vc-hg)

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
If START-REVISION is non-nil, it is the newest revision to show.
If LIMIT is non-nil, show no more than this many entries."
  ;; `vc-do-command' creates the buffer, but we need it before running
  ;; the command.
  (vc-setup-buffer buffer)
  ;; If the buffer exists from a previous invocation it might be
  ;; read-only.
  (let ((inhibit-read-only t))
    (with-current-buffer
	buffer
      (apply 'vc-hg-command buffer 'async files "log"
	     (nconc
	      (when start-revision (list (format "-r%s:0" start-revision)))
	      (when limit (list "-l" (format "%s" limit)))
	      (if shortlog
                  `(,@(if vc-hg-log-graph '("--graph"))
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
  (set (make-local-variable 'log-view-file-re) "\\`a\\`")
  (set (make-local-variable 'log-view-per-file-logs) nil)
  (set (make-local-variable 'log-view-message-re)
       (if (eq vc-log-view-type 'short)
	   (cadr vc-hg-root-log-format)
         "^changeset:[ \t]*\\([0-9]+\\):\\(.+\\)"))
  (set (make-local-variable 'tab-width) 2)
  ;; Allow expanding short log entries
  (when (eq vc-log-view-type 'short)
    (setq truncate-lines t)
    (set (make-local-variable 'log-view-expanded-log-entry-function)
	 'vc-hg-expanded-log-entry))
  (set (make-local-variable 'log-view-font-lock-keywords)
       (if (eq vc-log-view-type 'short)
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
	    ("^summary:[ \t]+\\(.+\\)" (1 'log-view-message)))))))

(autoload 'vc-switches "vc")

(defun vc-hg-diff (files &optional oldvers newvers buffer _async)
  "Get a difference report using hg between two revisions of FILES."
  (let* ((firstfile (car files))
         (working (and firstfile (vc-working-revision firstfile))))
    (when (and (equal oldvers working) (not newvers))
      (setq oldvers nil))
    (when (and (not oldvers) newvers)
      (setq oldvers working))
    (apply #'vc-hg-command
	   (or buffer "*vc-diff*")
           nil ; bug#21969
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
      (vc-hg-command t nil files "log" "--template" "{rev} ")
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
  (apply #'vc-hg-command buffer 0 file "annotate" "-dq" "-n"
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
                    (string-to-number (substring str 6 8))
                    (string-to-number (substring str 4 6))
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
  "Attach the tag NAME to the state of the working copy."
  (let ((default-directory dir))
    (and (vc-hg-command nil 0 nil "status")
         (vc-hg-command nil 0 nil (if branchp "bookmark" "tag") name))))

(defun vc-hg-retrieve-tag (dir name _update)
  "Retrieve the version tagged by NAME of all registered files at or below DIR."
  (let ((default-directory dir))
    (vc-hg-command nil 0 nil "update" name)
    ;; TODO: update *vc-change-log* buffer so can see @ if --graph
    ))

;;; Native data structure reading

(defcustom vc-hg-parse-hg-data-structures t
  "If true, try directly parsing Mercurial data structures
directly instead of always running Mercurial.  We try to be safe
against Mercurial data structure format changes and always fall
back to running Mercurial directly."
  :type 'boolean
  :version "26.1"
  :group 'vc-hg)

(defsubst vc-hg--read-u8 ()
  "Read and advance over an unsigned byte.
Return a fixnum."
  (prog1 (char-after)
    (forward-char)))

(defsubst vc-hg--read-u32-be ()
  "Read and advance over a big-endian unsigned 32-bit integer.
Return a fixnum; on overflow, result is undefined."
  ;; Because elisp bytecode has an instruction for multiply and
  ;; doesn't have one for lsh, it's somewhat counter-intuitively
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
           (inhibit-changing-match-data t)
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
        ;; 1+4*4 is the length of the dirstate item header, which we
        ;; spell as a literal for performance, since the elisp
        ;; compiler lacks constant propagation
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
              (t (error "invalid state")))
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
        (cl-incf i)
        (cond ((not (memq c '(?* ?? ?\[ ?\{ ?\} ?, ?\\)))
               (push (vc-hg--escape-for-pcre c) parts))
              ((eq c ?*)
               (cond ((eq (peek) ?*)
                      (cl-incf i)
                      (cond ((eq (peek) ?/)
                             (cl-incf i)
                             (push "(?:.*/)?" parts))
                            (t
                             (push ".*" parts))))
                     (t (push "[^/]*" parts))))
              ((eq c ??)
               (push ?. parts))
              ((eq c ?\[)
               (let ((j i))
                 (when (and (< j n) (memq (aref glob j) '(?! ?\])))
                   (cl-incf j))
                 (while (and (< j n) (not (eq (aref glob j) ?\])))
                   (cl-incf j))
                 (cond ((>= j n)
                        (push "\\[" parts))
                       (t
                        (let ((x (substring glob i j)))
                          (setf x (replace-regexp-in-string
                                   "\\\\" "\\\\" x t t))
                          (setf i (1+ j))
                          (cond ((eq (aref x 0) ?!)
                                 (setf (aref x 0) ?^))
                                ((eq (aref x 0) ?^)
                                 (setf x (concat "\\" x))))
                          (push ?\[ parts)
                          (push x parts)
                          (push ?\] parts))))))
              ((eq c ?\{)
               (cl-incf group)
               (push "(?:" parts))
              ((eq c ?\})
               (push ?\) parts)
               (cl-decf group))
              ((and (eq c ?,) (> group 0))
               (push ?| parts))
              ((eq c ?\\)
               (if (eq i n)
                   (push "\\\\" parts)
                 (cl-incf i)
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
        (push (list hgignore (nth 5 attr) (nth 7 attr))
              vc-hg--hgignore-filenames))
      (while (not (eobp))
        ;; This list of pattern-file commands isn't complete, but it
        ;; should cover the common cases.  Remember that we fall back
        ;; to regular hg commands if we see something we don't like.
        (save-restriction
          (narrow-to-region (point) (point-at-eol))
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
  "Return whether the cached ignore patterns in HGIP are still valid"
  (let ((valid t)
        (file-sources (vc-hg--ignore-patterns-file-sources hgip)))
    (while (and file-sources valid)
      (let* ((fs (pop file-sources))
             (saved-mtime (nth 1 fs))
             (saved-size (nth 2 fs))
             (attr (file-attributes (nth 0 fs)))
             (current-mtime (nth 5 attr))
             (current-size (nth 7 attr)))
        (unless (and (equal saved-mtime current-mtime)
                     (equal saved-size current-size))
          (setf valid nil))))
    valid))

(defun vc-hg--ignore-patterns-ignored-p (hgip filename)
  "Test whether the ignore pattern set HGIP says to ignore FILENAME.
FILENAME must be the file's true absolute name."
  (let ((patterns (vc-hg--ignore-patterns-ignore-patterns hgip))
        (inhibit-changing-match-data t)
        (ignored nil))
    (while (and patterns (not ignored))
      (setf ignored (string-match (pop patterns) filename)))
    ignored))

(defun vc-hg--time-to-fixnum (ts)
  (+ (* 65536 (car ts)) (cadr ts)))

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
  "List of Mercurial repository requirements we understand; if a
repository requires features not present in this list, we avoid
attempting to parse Mercurial data structures.")

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
  (let* ((mtime (nth 5 dirstate-attr))
         (size (nth 7 dirstate-attr))
         (cache vc-hg--dirstate-scan-cache)
         )
    (if (and cache
             (equal dirstate (pop cache))
             (equal mtime (pop cache))
             (equal size (pop cache))
             (equal ascii-fname (pop cache)))
        (pop cache)
      (let ((result (vc-hg--raw-dirstate-search dirstate ascii-fname)))
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
         (< (nth 7 dirstate-attr) 40)
         ;; We want to store 32-bit unsigned values in fixnums
         (< most-positive-fixnum 4294967295)
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
                     (fs-size (nth 7 stat))
                     (fs-mtime (vc-hg--time-to-fixnum (nth 5 stat))))
                 (if (and (eql vc-hg-size fs-size) (eql vc-hg-mtime fs-mtime))
                     'up-to-date
                   'edited)))
              ((eq state ?a) 'added)
              (state 'unsupported))))))

;;; Miscellaneous

(defun vc-hg-previous-revision (_file rev)
  ;; We can't simply decrement by 1, because that revision might be
  ;; e.g. on a different branch (bug#22032).
  (with-temp-buffer
    (and (eq 0
             (vc-hg-command t nil nil "id" "-n" "-r" (concat rev "^")))
         ;; Trim the trailing newline.
         (buffer-substring (point-min) (1- (point-max))))))

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
  (vc-hg-command nil 0 new "mv" old))

(defun vc-hg-register (files &optional _comment)
  "Register FILES under hg. COMMENT is ignored."
  (vc-hg-command nil 0 files "add"))

(defun vc-hg-create-repo ()
  "Create a new Mercurial repository."
  (vc-hg-command nil 0 nil "init"))

(defalias 'vc-hg-responsible-p 'vc-hg-root)

(defun vc-hg-unregister (file)
  "Unregister FILE from hg."
  (vc-hg-command nil 0 file "forget"))

(declare-function log-edit-extract-headers "log-edit" (headers string))

(defun vc-hg-checkin (files comment &optional _rev)
  "Hg-specific version of `vc-backend-checkin'.
REV is ignored."
  (apply 'vc-hg-command nil 0 files
         (nconc (list "commit" "-m")
                (log-edit-extract-headers '(("Author" . "--user")
					    ("Date" . "--date"))
                                          comment))))

(defun vc-hg-find-revision (file rev buffer)
  (let ((coding-system-for-read 'binary)
        (coding-system-for-write 'binary))
    (if rev
        (vc-hg-command buffer 0 file "cat" "-r" rev)
      (vc-hg-command buffer 0 file "cat"))))

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
      (remove-hook 'after-save-hook 'vc-hg-resolve-when-done t))))

(defun vc-hg-find-file-hook ()
  (when (and buffer-file-name
             (file-exists-p (concat buffer-file-name ".orig"))
             ;; Hg does not seem to have a "conflict" status, eg
             ;; hg http://bz.selenic.com/show_bug.cgi?id=2724
             (memq (vc-file-getprop buffer-file-name 'vc-state)
                   '(edited conflict))
             ;; Maybe go on to check that "hg resolve -l" says "U"?
             ;; If "hg resolve -l" says there's a conflict but there are no
             ;; conflict markers, it's not clear what we should do.
             (save-excursion
               (goto-char (point-min))
               (re-search-forward "^<<<<<<< " nil t)))
    ;; Hg may not recognize "conflict" as a state, but we can do better.
    (vc-file-setprop buffer-file-name 'vc-state 'conflict)
    (smerge-start-session)
    (add-hook 'after-save-hook 'vc-hg-resolve-when-done nil t)
    (vc-message-unresolved-conflicts buffer-file-name)))


;; Modeled after the similar function in vc-bzr.el
(defun vc-hg-revert (file &optional contents-done)
  (unless contents-done
    (with-temp-buffer (vc-hg-command t 0 file "revert"))))

;;; Hg specific functionality.

(defvar vc-hg-extra-menu-map
  (let ((map (make-sparse-keymap)))
    map))

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
                         (`copied "copied from")
                         (`renamed-from "renamed from")
                         (`renamed-to "renamed to"))
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
(declare-function vc-exec-after "vc-dispatcher" (code))
;; Follows vc-exec-after.
(declare-function vc-set-async-update "vc-dispatcher" (process-buffer))

(defun vc-hg-dir-status-files (_dir files update-function)
  ;; XXX: We can't pass DIR directly to 'hg status' because that
  ;; returns all ignored files if FILES is non-nil (bug#22481).
  ;; If honoring DIR ever becomes important, try using '-I DIR/'.
  (vc-hg-command (current-buffer) 'async files
                 "status"
                 (concat "-mardu" (if files "i"))
                 "-C")
  (vc-run-delayed
    (vc-hg-after-dir-status update-function)))

(defun vc-hg-dir-extra-header (name &rest commands)
  (concat (propertize name 'face 'font-lock-type-face)
          (propertize
           (with-temp-buffer
             (apply 'vc-hg-command (current-buffer) 0 nil commands)
             (buffer-substring-no-properties (point-min) (1- (point-max))))
           'face 'font-lock-variable-name-face)))

(defun vc-hg-dir-extra-headers (dir)
  "Generate extra status headers for a Mercurial tree."
  (let ((default-directory dir))
    (concat
     (vc-hg-dir-extra-header "Root       : " "root") "\n"
     (vc-hg-dir-extra-header "Branch     : " "id" "-b") "\n"
     (vc-hg-dir-extra-header "Tags       : " "id" "-t") ; "\n"
     ;; these change after each commit
     ;; (vc-hg-dir-extra-header "Local num  : " "id" "-n") "\n"
     ;; (vc-hg-dir-extra-header "Global id  : " "id" "-i")
     )))

(defun vc-hg-log-incoming (buffer remote-location)
  (vc-hg-command buffer 1 nil "incoming" "-n" (unless (string= remote-location "")
						remote-location)))

(defun vc-hg-log-outgoing (buffer remote-location)
  (vc-hg-command buffer 1 nil "outgoing" "-n" (unless (string= remote-location "")
						remote-location)))

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
commands, which only operated on marked files."
  (let (marked-list)
    ;; The `vc-hg-pull' and `vc-hg-push' commands existed before the
    ;; `pull'/`push' VC actions were implemented.
    ;; The following is for backwards compatibility.
    (if (and obsolete (setq marked-list (log-view-get-marked)))
	(apply #'vc-hg-command
	       nil 0 nil
	       command
	       (apply 'nconc
		      (mapcar (lambda (arg) (list "-r" arg)) marked-list)))
      (let* ((root (vc-hg-root default-directory))
	     (buffer (format "*vc-hg : %s*" (expand-file-name root)))
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
	(apply 'vc-do-async-command buffer root hg-program command args)
        (with-current-buffer buffer
          (vc-run-delayed
            (dolist (cmd post-processing)
              (apply 'vc-do-command buffer nil hg-program nil cmd))
            (vc-compilation-mode 'hg)
            (setq-local compile-command
                        (concat hg-program " " command " "
                                (mapconcat 'identity args " ")
                                (mapconcat (lambda (args)
                                             (concat " && " hg-program " "
                                                     (mapconcat 'identity
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
  "Merge incoming changes into the current working directory.
This runs the command \"hg merge\"."
  (let* ((root (vc-hg-root default-directory))
	 (buffer (format "*vc-hg : %s*" (expand-file-name root))))
    (apply 'vc-do-async-command buffer root vc-hg-program '("merge"))
    (with-current-buffer buffer (vc-run-delayed (vc-compilation-mode 'hg)))
    (vc-set-async-update buffer)))

;;; Internal functions

(defun vc-hg-command (buffer okstatus file-or-list &rest flags)
  "A wrapper around `vc-do-command' for use in vc-hg.el.
This function differs from vc-do-command in that it invokes
`vc-hg-program', and passes `vc-hg-global-switches' to it before FLAGS."
  (apply 'vc-do-command (or buffer "*vc*") okstatus vc-hg-program file-or-list
         (if (stringp vc-hg-global-switches)
             (cons vc-hg-global-switches flags)
           (append vc-hg-global-switches
                   flags))))

(defun vc-hg-root (file)
  (vc-find-root file ".hg"))

(provide 'vc-hg)

;;; vc-hg.el ends here
