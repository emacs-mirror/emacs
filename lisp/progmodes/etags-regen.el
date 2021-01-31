;;; etags-regen.el --- Auto-(re)regenerating tags  -*- lexical-binding: t -*-

;; Copyright (C) 2021 Free Software Foundation, Inc.

;; Author: Dmitry Gutov <dgutov@yandex.ru>
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

;; Simple tags generation with automatic invalidation.

;;; Code:

(defgroup etags-regen nil
  "Auto-(re)generating tags."
  :group 'tools)

(defvar etags-regen--tags-file nil)
(defvar etags-regen--tags-root nil)
(defvar etags-regen--new-file nil)

(declare-function project-root "project")
(declare-function project-files "project")

(defcustom etags-regen-program (executable-find "etags")
  "Name of the etags executable."
  ;; Always having our 'etags' here would be easier, but we can't
  ;; always rely on it being installed.  So it might be ctags's etags.
  :type 'file)

(defcustom etags-regen-program-options nil
  "List of additional options to pass to the etags program."
  :type '(repeat string))

(defcustom etags-regen-lang-regexp-alist nil
  "Mapping of languages to additional regexps for tags.

Each language should be one of the recognized by etags, see
'etags --help'.  Each tag regexp should be a string in the format
as documented for the '--regex' arguments, except for
the (optional) language prefix.

We support only Emacs's etags program with this option."
  :type '(repeat
          (cons
           :tag "Languages group"
           (repeat (string :tag "Language name"))
           (repeat (string :tag "Tag Regexp")))))

;;;###autoload
(put 'etags-regen-lang-regexp-alist 'safe-local-variable
     (lambda (value)
       (and (listp value)
            (seq-every-p
             (lambda (group)
               (and (consp group)
                    (listp (car group))
                    (listp (cdr group))
                    (seq-every-p
                     (lambda (lang)
                       (and (stringp lang)
                            (string-match-p "\\`[a-z*+]+\\'" lang)))
                     (car group))
                    (seq-every-p #'stringp (cdr group))))
             value))))

;; XXX: We have to list all extensions: etags falls back to Fortran.
;; http://lists.gnu.org/archive/html/emacs-devel/2018-01/msg00323.html
(defcustom etags-regen-file-extensions
  '("rb" "js" "py" "pl" "el" "c" "cpp" "cc" "h" "hh" "hpp"
    "java" "go" "cl" "lisp" "prolog" "php" "erl" "hrl"
    "F" "f" "f90" "for" "cs" "a" "asm" "ads" "adb" "ada")
  "Code file extensions.

File extensions to generate the tags for."
  :type '(repeat (string :tag "File extension")))

;;;###autoload
(put 'etags-regen-file-extensions 'safe-local-variable
     (lambda (value)
       (and (listp value)
            (seq-every-p
             (lambda (ext)
               (and (stringp ext)
                    (string-match-p "\\`[a-zA-Z0-9]+\\'" ext)))
             value))))

(defcustom etags-regen-ignores nil
  "Additional ignore rules, in the format of `project-ignores'."
  :type '(repeat
          (string :tag "Glob to ignore")))

;;;###autoload
(put 'etags-regen-ignores 'safe-local-variable
     (lambda (value)
       (and (listp value)
            (seq-every-p #'stringp value))))

(defvar etags-regen--errors-buffer-name "*etags-regen-tags-errors*")

(defun etags-regen--maybe-generate ()
  (let (proj)
    (when (and etags-regen--tags-root
               (not (file-in-directory-p default-directory
                                         etags-regen--tags-root)))
      (etags-regen--tags-cleanup))
    (when (and (not (or tags-file-name
                        tags-table-list))
               (setq proj (project-current)))
      (message "Generating new tags table...")
      (let ((start (time-to-seconds)))
        (etags-regen--tags-generate proj)
        (message "...done (%.2f s)" (- (time-to-seconds) start)))
      ;; Invalidate the scanned tags after any change is written to disk.
      (add-hook 'after-save-hook #'etags-regen--update-file)
      (add-hook 'before-save-hook #'etags-regen--mark-as-new)
      (visit-tags-table etags-regen--tags-file))))

(declare-function dired-glob-regexp "dired")

(defun etags-regen--all-files (proj)
  (require 'dired)
  (let* ((root (project-root proj))
         (default-directory root)
         (files (project-files proj))
         (extensions etags-regen-file-extensions)
         ;; FIXME: Try to do the filtering inside project.el already.
         (file-regexp (format "\\.%s\\'" (regexp-opt extensions t)))
         (ignore-regexps (mapcar
                          (lambda (i)
                            (if (string-match "\\./" i)
                                ;; ./abc -> abc
                                (setq i (substring i 2))
                              ;; abc -> */abc
                              (setq i (concat "*/" i))
                              (if (string-match "/\\'" i)
                                  ;; abc/ -> abc/*
                                  (setq i (concat i "*"))))
                            (dired-glob-regexp i))
                          (cons ".#*" etags-regen-ignores))))
    (cl-delete-if-not
     (lambda (f)
       (and (string-match-p file-regexp f)
            (not (cl-find
                  (lambda (re) (string-match-p re f))
                  ignore-regexps))))
     files)))

(defun etags-regen--tags-generate (proj)
  (require 'dired)
  (let* ((root (project-root proj))
         (default-directory root)
         (files (etags-regen--all-files proj))
         (tags-file (make-temp-file "emacs-regen-tags-"))
         ;; ctags's etags requires '-L -' for stdin input.
         ;; It looks half-broken here (indexes only some of the input files),
         ;; but better-maintained versions of it exist (like universal-ctags).
         (command (format "%s %s -L - -o %s"
                          etags-regen-program
                          (mapconcat #'identity (etags-regen--build-program-options) " ")
                          tags-file)))
    (setq etags-regen--tags-file tags-file
          etags-regen--tags-root root)
    (with-temp-buffer
      (mapc (lambda (f)
              (insert f "\n"))
            files)
      (shell-command-on-region (point-min) (point-max) command
                               nil nil etags-regen--errors-buffer-name t))))

(defun etags-regen--build-program-options ()
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
    etags-regen-lang-regexp-alist)
   etags-regen-program-options))

(defun etags-regen--update-file ()
  ;; TODO: Maybe only do this when Emacs is idle for a bit.
  (let ((file-name buffer-file-name)
        (options (etags-regen--build-program-options))
        (tags-file-buf (get-file-buffer etags-regen--tags-file))
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
                 (goto-char (point-min))
                 (search-forward (format "\f\n%s," file-name) nil t))
          (let ((start (match-beginning 0)))
            (search-forward "\f\n" nil 'move)
            (let ((inhibit-read-only t)
                  (save-silently t))
              (delete-region start
                             (if (eobp)
                                 (point)
                               (- (point) 2)))))
          (setq should-scan t))))
      (when should-scan
        (goto-char (point-max))
        (let ((inhibit-read-only t))
          ;; FIXME: call-process is significantly faster, though.
          ;; Like 10ms vs 20ms here.
          (shell-command
           (format "%s %s %s -o -"
                   etags-regen-program (mapconcat #'identity options " ")
                   file-name)
           t etags-regen--errors-buffer-name))
        ;; We don't want Emacs to ask us to save the buffer when exiting.
        (set-buffer-modified-p nil)
        ;; FIXME: Is there a better way to do this?
        ;; Completion table is the only remaining place where the
        ;; update is not incremental.
        (setq-default tags-completion-table nil)
        ))))

(defun etags-regen--mark-as-new ()
  (unless buffer-file-number
    (setq-local etags-regen--new-file t)))

(defun etags-regen--tags-cleanup ()
  (when etags-regen--tags-file
    ;; TODO: Maybe keep the generated files around, after we learn to
    ;; update them for the whole project quickly, so opening one
    ;; created in a previous session makes sense.
    (delete-file etags-regen--tags-file)
    (let ((buffer (get-file-buffer etags-regen--tags-file)))
      (and buffer
           (kill-buffer buffer)))
    (setq tags-file-name nil
          tags-table-list nil
          etags-regen--tags-file nil
          etags-regen--tags-root nil))
  (remove-hook 'after-save-hook #'etags-regen--update-file)
  (remove-hook 'before-save-hook #'etags-regen--mark-as-new))

;;;###autoload
(define-minor-mode etags-regen-mode
  "Generate tags automatically."
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
