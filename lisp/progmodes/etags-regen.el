;;; etags-regen.el --- Auto-(re)regenerating tags  -*- lexical-binding: t -*-

;; Copyright (C) 2021-2026 Free Software Foundation, Inc.

;; Author: Dmitry Gutov <dmitry@gutov.dev>
;; Keywords: tools

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

;; Simple automatic tags generation with updates on save.
;;
;; This mode provides automatic indexing for Emacs "go to definition"
;; feature, the `xref-go-forward' command (bound to `M-.' by default).
;;
;; At the moment reindexing works off before/after-save-hook, but to
;; handle more complex changes (for example, the user switching to
;; another branch from the terminal) we can look into plugging into
;; something like `filenotify'.
;;
;; Note that this feature disables itself if the user has some tags
;; table already visited (with `M-x visit-tags-table', or through an
;; explicit prompt triggered by some feature that requires tags).

;;; Code:

(require 'cl-lib)

(defgroup etags-regen nil
  "Auto-(re)generating tags."
  :group 'tools)

(defvar etags-regen--tags-file nil)
(defvar etags-regen--tags-root nil)
(defvar etags-regen--new-file nil)

(declare-function project-root "project")
(declare-function project-files "project")
(declare-function dired-glob-regexp "dired")

(defcustom etags-regen-program etags-program-name
  "Name of the etags program used by `etags-regen-mode'.

If you only have `ctags' installed, you can also set this to
\"ctags -e\".  Some features might not be supported this way."
  ;; Always having our 'etags' here would be easier, but we can't
  ;; always rely on it being installed.  So it might be ctags's etags.
  :type 'file
  :version "30.1")

(defcustom etags-regen-tags-file "TAGS"
  "Name of the tags file to create inside the project by `etags-regen-mode'.

The value should either be a simple file name (no directory
specified), or a function that accepts the project root directory
and returns a distinct absolute file name for its tags file.  The
latter possibility is useful when you prefer to store the tag
files somewhere else, for example in `temporary-file-directory'."
  :type '(choice (string :tag "File name")
                 (function :tag "Function that returns file name"))
  :version "30.1")

(defcustom etags-regen-program-options nil
  "List of additional options for etags program invoked by `etags-regen-mode'."
  :type '(repeat string)
  :version "30.1")

(defcustom etags-regen-regexp-alist nil
  "Mapping of languages to etags regexps for `etags-regen-mode'.

These regexps are used in addition to the tags made with the
standard parsing based on the language.

The value must be a list where each element has the
form (LANGUAGES . TAG-REGEXPS) where both LANGUAGES and
TAG-REGEXPS are lists of strings.

Each language should be one of the recognized by etags, see
`etags --help'.  Each tag regexp should be a string in the format
documented for the `--regex' arguments (without `{language}').

We currently support only Emacs's etags program with this option."
  :type '(repeat
          (cons
           :tag "Languages group"
           (repeat (string :tag "Language name"))
           (repeat (string :tag "Tag Regexp"))))
  :version "30.1")

;;;###autoload
(put 'etags-regen-regexp-alist 'safe-local-variable
     (lambda (value)
       (and (listp value)
            (seq-every-p
             (lambda (group)
               (and (consp group)
                    (listp (car group))
                    (listp (cdr group))
                    (seq-every-p #'stringp (car group))
                    (seq-every-p #'stringp (cdr group))))
             value))))

;; We have to list all extensions: etags falls back to Fortran
;; when it cannot determine the type of the file.
;; http://lists.gnu.org/archive/html/emacs-devel/2018-01/msg00323.html
(defcustom etags-regen-file-extensions
  '("ads" "adb" "ada" "asm" "ins" "s" "sa" "S" "src"
    "c" "h" "c++" "cc" "cpp" "cxx" "h++" "hh" "hpp" "hxx" "m" "pdb"
    "cs" "hs" "erl" "hrl" "fth" "tok" "f" "f90" "for" "go"
    "java" "cl" "clisp" "el" "lisp" "lsp" "lua" "lm" "p" "pas"
    "pl" "pm" "php" "php3" "php4" "pc" "prolog" "py" "rb" "ru" "rbw"
    "rs" "oak" "rkt" "sch" "scheme" "scm" "sm" "ss"
    "y" "y++" "ym" "yxx" "yy")
  "Code file extensions for `etags-regen-mode'.

File extensions to generate the tags for."
  :type '(repeat (string :tag "File extension"))
  :version "30.1")

;;;###autoload
(put 'etags-regen-file-extensions 'safe-local-variable
     (lambda (value) (and (listp value) (seq-every-p #'stringp value))))

;; FIXME: We don't support root anchoring yet.
(defcustom etags-regen-ignores nil
  "Additional ignore rules, in the format of `project-ignores'."
  :type '(repeat
          (string :tag "Glob to ignore"))
  :version "30.1")

;;;###autoload
(put 'etags-regen-ignores 'safe-local-variable
     (lambda (value) (and (listp value) (seq-every-p #'stringp value))))

(defvar etags-regen--errors-buffer-name "*etags-regen-tags-errors*")

(defvar etags-regen--rescan-files-limit 100)

(defun etags-regen--all-mtimes (proj)
  (let ((files (etags-regen--all-files proj))
        (mtimes (make-hash-table :test 'equal))
        file-name-handler-alist)
    (dolist (f files)
      (condition-case nil
          (puthash f
                   (file-attribute-modification-time
                    (file-attributes f))
                   mtimes)
        (file-missing nil)))
    mtimes))

(defun etags-regen--choose-tags-file (proj)
  (if (functionp etags-regen-tags-file)
      (funcall etags-regen-tags-file (project-root proj))
    (expand-file-name etags-regen-tags-file (project-root proj))))

(defun etags-regen--refresh (proj)
  (save-excursion
    (let* ((tags-file (etags-regen--choose-tags-file proj))
           (tags-mtime (file-attribute-modification-time
                        (file-attributes tags-file)))
           (all-mtimes (etags-regen--all-mtimes proj))
           added-files
           changed-files
           removed-files)
      (etags-regen--visit-table tags-file (project-root proj))
      (set-buffer (get-file-buffer tags-file))
      (dolist (file (tags-table-files))
        (let ((mtime (gethash file all-mtimes)))
          (cond
           ((null mtime)
            (push file removed-files))
           ((time-less-p tags-mtime mtime)
            (push file changed-files)
            (remhash file all-mtimes))
           (t
            (remhash file all-mtimes)))))
      (maphash
       (lambda (key _value)
         (push key added-files))
       all-mtimes)
      (if (> (+ (length added-files)
                (length changed-files)
                (length removed-files))
             etags-regen--rescan-files-limit)
          (progn
            (message "etags-regen: Too many changes, falling back to full rescan")
            (etags-regen--tags-cleanup))
        (dolist (file (nconc removed-files changed-files))
          (etags-regen--remove-tag file))
        (when (or changed-files added-files)
          (apply #'etags-regen--append-tags
                 (nconc changed-files added-files)))
        (when (or changed-files added-files removed-files)
          (let ((save-silently t)
                (message-log-max nil))
            (save-buffer 0)))))))

(defun etags-regen--maybe-generate ()
  (let (proj)
    (when (and etags-regen--tags-root
               (not (file-in-directory-p default-directory
                                         etags-regen--tags-root)))
      (etags-regen--tags-cleanup))
    (when (and (not etags-regen--tags-root)
               ;; If existing table is visited that's not generated by
               ;; this mode, skip all functionality.
               (not (or tags-file-name
                        tags-table-list))
               (file-exists-p (etags-regen--choose-tags-file
                               (setq proj (project-current)))))
      (message "Found existing tags table, refreshing...")
      (etags-regen--refresh proj))
    (when (and (not (or tags-file-name
                        tags-table-list))
               (setq proj (or proj (project-current))))
      (message "Generating new tags table...")
      (let ((start (time-to-seconds)))
        (etags-regen--tags-generate proj)
        (message "...done (%.2f s)" (- (time-to-seconds) start))))))

(defun etags-regen--all-files (proj)
  (let* ((root (project-root proj))
         (default-directory root)
         ;; TODO: Make the scanning more efficient, e.g. move the
         ;; filtering by glob to project (project-files-filtered...).
         (files (project-files proj))
         (match-re (concat
                    "\\."
                    (regexp-opt etags-regen-file-extensions)
                    "\\'"))
         (ir-start (1- (length root)))
         (ignores-regexps
          (mapcar #'etags-regen--ignore-regexp
                  etags-regen-ignores))
         (case-fold-search t))
    (cl-delete-if
     (lambda (f) (or (not (string-match-p match-re f))
                (string-match-p "/\\.#" f) ;Backup files.
                (cl-some (lambda (ignore) (string-match-p ignore f ir-start))
                         ignores-regexps)))
     files)))

(defun etags-regen--ignore-regexp (ignore)
  (require 'dired)
  ;; It's somewhat brittle to rely on Dired.
  (let ((re (dired-glob-regexp ignore)))
    ;; We could implement root anchoring here, but \\= doesn't work in
    ;; string-match :-(.
    (concat (unless (eq ?/ (aref re 3)) "/")
            ;; Cutting off the anchors added by `dired-glob-regexp'.
            (substring re 2 (- (length re) 2))
            ;; This way we allow a glob to match against a directory
            ;; name, or a file name.  And when it ends with / already,
            ;; no need to add the anchoring.
            (unless (eq ?/ (aref re (- (length re) 3)))
              ;; Either match a full name segment, or eos.
              "\\(?:/\\|\\'\\)"))))

(defun etags-regen--process-file-region ( start end program
                                          &optional output-buffer error-buffer
                                          &rest args)
  (let ((error-file (and error-buffer (make-temp-file "erpfr-err")))
        infile)
    (unwind-protect
        (progn
          (if (not (file-remote-p default-directory))
              (if (and start end)
                  (apply #'call-process-region
                         start end program nil
                         (list output-buffer error-file) nil args)
                (apply #'call-process
                       program nil (list output-buffer error-file) nil args))
            (when (and start end)
              (setq infile (make-temp-file "erpfr"))
              (write-region start end infile nil 'silent))
            (apply #'process-file
                   program infile (list output-buffer error-file) nil args))
          (when (and error-file
                     (file-exists-p error-file)
                     (< 0 (file-attribute-size (file-attributes error-file))))
            (with-current-buffer (get-buffer-create error-buffer)
              (erase-buffer)
              (format-insert-file error-file nil)
              (display-buffer (current-buffer)))))
      (if infile (delete-file infile))
      (if error-file (delete-file error-file)))))

(defun etags-regen--tags-generate (proj)
  (let* ((root (project-root proj))
         (default-directory root)
         (files (etags-regen--all-files proj))
         (tags-file (etags-regen--choose-tags-file proj))
         (fun (if (equal (file-name-directory tags-file)
                         (expand-file-name root))
                  #'file-relative-name #'file-local-name))
         (ctags-p (etags-regen--ctags-p))
         (command (format "%s %s %s - -o %s"
                          etags-regen-program
                          (mapconcat #'identity
                                     (etags-regen--build-program-options ctags-p)
                                     " ")
                          ;; ctags's etags requires '-L' for stdin input.
                          (if ctags-p "-L" "")
                          (shell-quote-argument (file-local-name tags-file)))))
    (with-temp-buffer
      (mapc (lambda (f)
              (insert (funcall fun f) "\n"))
            files)
      (with-connection-local-variables
       (etags-regen--process-file-region (point-min)
                                         (point-max)
                                         shell-file-name
                                         nil
                                         etags-regen--errors-buffer-name
                                         shell-command-switch
                                         command)))
    (etags-regen--visit-table tags-file root)))

(defun etags-regen--visit-table (tags-file root)
  ;; Invalidate the scanned tags after any change is written to disk.
  (add-hook 'after-save-hook #'etags-regen--update-file)
  (add-hook 'before-save-hook #'etags-regen--mark-as-new)
  (setq etags-regen--tags-file tags-file
        etags-regen--tags-root root)
  (visit-tags-table etags-regen--tags-file)
  (with-current-buffer (get-file-buffer tags-file)
    (add-hook 'kill-buffer-hook #'etags-regen--tags-cleanup nil t)))

(defun etags-regen--ctags-p ()
  (string-search "Ctags"
                 (shell-command-to-string
                  (format "%s --version" etags-regen-program))))

(defun etags-regen--build-program-options (ctags-p)
  (when (and etags-regen-regexp-alist ctags-p)
    (user-error "etags-regen-regexp-alist is not supported with Ctags"))
  (nconc
   (mapcan
    (lambda (group)
      (mapcan
       (lambda (lang)
         (mapcar (lambda (regexp)
                   (concat "--regex="
                           (shell-quote-argument
                            (format "{%s}%s" lang regexp))))
                 (cdr group)))
       (car group)))
    etags-regen-regexp-alist)
   (mapcar #'shell-quote-argument
           etags-regen-program-options)))

(defun etags-regen--update-file ()
  ;; TODO: Maybe only do this when Emacs is idle for a bit.  Or defer
  ;; the updates and do them later in bursts when the table is used.
  (let* ((file-name buffer-file-name)
         (tags-file-buf (and etags-regen--tags-root
                             (get-file-buffer etags-regen--tags-file)))
         (relname (concat "/" (file-relative-name file-name
                                                  etags-regen--tags-root)))
         (fun (if (equal (file-name-directory etags-regen--tags-file)
                         (expand-file-name etags-regen--tags-root))
                  #'file-relative-name #'file-local-name))
         (ignores etags-regen-ignores)
         pr should-scan)
    (save-excursion
      (when tags-file-buf
        (cond
         ((and etags-regen--new-file
               (kill-local-variable 'etags-regen--new-file)
               (setq pr (project-current))
               (equal (project-root pr) etags-regen--tags-root)
               (member file-name (project-files pr)))
          (set-buffer tags-file-buf)
          (setq should-scan t))
         ((progn (set-buffer tags-file-buf)
                 (etags-regen--remove-tag (funcall fun file-name)))
          (setq should-scan t))))
      (when (and should-scan
                 (not (cl-some
                       (lambda (ignore)
                         (string-match-p
                          (etags-regen--ignore-regexp ignore)
                          relname))
                       ignores)))
        (etags-regen--append-tags file-name)
        (let ((save-silently t)
              (message-log-max nil))
          (save-buffer 0))))))

(defun etags-regen--remove-tag (file-name)
  (goto-char (point-min))
  (when (search-forward (format "\f\n%s," file-name) nil t)
    (let ((start (match-beginning 0)))
      (search-forward "\f\n" nil 'move)
      (let ((inhibit-read-only t))
        (delete-region start
                       (if (eobp)
                           (point)
                         (- (point) 2)))))
    t))

(defun etags-regen--append-tags (&rest file-names)
  (goto-char (point-max))
  (let ((options (etags-regen--build-program-options (etags-regen--ctags-p)))
        (fun (if (equal (file-name-directory etags-regen--tags-file)
                        (expand-file-name etags-regen--tags-root))
                 #'file-relative-name #'file-local-name))
        (inhibit-read-only t))
    (with-connection-local-variables
     (etags-regen--process-file-region
      nil nil shell-file-name t etags-regen--errors-buffer-name
      shell-command-switch
      (format "%s %s -o - %s"
              etags-regen-program (mapconcat #'identity options " ")
              (mapconcat fun file-names " ")))))
  ;; FIXME: Is there a better way to do this?
  ;; Completion table is the only remaining place where the
  ;; update is not incremental.
  (setq-default tags-completion-table nil))

(defun etags-regen--mark-as-new ()
  (when (and etags-regen--tags-root
             (not buffer-file-number))
    (setq-local etags-regen--new-file t)))

(defun etags-regen--tags-cleanup ()
  (when etags-regen--tags-file
    (let ((buffer (get-file-buffer etags-regen--tags-file))
          kill-buffer-hook)
      (and buffer
           (kill-buffer buffer)))
    (tags-reset-tags-tables)
    (setq tags-file-name nil
          tags-table-list nil
          etags-regen--tags-file nil
          etags-regen--tags-root nil))
  (remove-hook 'after-save-hook #'etags-regen--update-file)
  (remove-hook 'before-save-hook #'etags-regen--mark-as-new))

(defvar-keymap etags-regen-mode-map)

;;;###autoload
(define-minor-mode etags-regen-mode
  "Minor mode to automatically generate and update tags tables.

This minor mode generates the tags table automatically based on
the current project configuration, and later updates it as you
edit the files and save the changes.

If you select a tags table manually (for example, using
\\[visit-tags-table]), then this mode will be effectively
disabled for the entire session.  Use \\[tags-reset-tags-tables]
to countermand the effect of a previous \\[visit-tags-table]."
  :global t
  (if etags-regen-mode
      (progn
        (advice-add 'etags--xref-backend :before
                    #'etags-regen--maybe-generate)
        (advice-add 'tags-completion-at-point-function :before
                    #'etags-regen--maybe-generate))
    (advice-remove 'etags--xref-backend #'etags-regen--maybe-generate)
    (advice-remove 'tags-completion-at-point-function #'etags-regen--maybe-generate)
    (etags-regen--tags-cleanup)))

(provide 'etags-regen)

;;; etags-regen.el ends here
