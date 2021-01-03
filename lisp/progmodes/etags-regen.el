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
  ;; FIXME: How to implement the safety predicate?
  :type '(repeat string))

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

(defun etags-regen--tags-generate (proj)
  (let* ((root (project-root proj))
         (default-directory root)
         (files (project-files proj))
         ;; FIXME: List all extensions, or wait for etags fix.
         ;; http://lists.gnu.org/archive/html/emacs-devel/2018-01/msg00323.html
         (extensions '("rb" "js" "py" "pl" "el" "c" "cpp" "cc" "h" "hh" "hpp"
                       "java" "go" "cl" "lisp" "prolog" "php" "erl" "hrl"
                       "F" "f" "f90" "for" "cs" "a" "asm" "ads" "adb" "ada"))
         (file-regexp (format "\\.%s\\'" (regexp-opt extensions t)))
         (tags-file (make-temp-file "emacs-regen-tags-"))
         ;; ctags's etags requires '-L -' for stdin input.
         ;; It looks half-broken here (indexes only some of the input files),
         ;; but better-maintained versions of it exist (like universal-ctags).
         (command (format "%s %s -L - -o %s"
                          etags-regen-program
                          (mapconcat #'identity etags-regen-program-options " ")
                          tags-file)))
    (setq etags-regen--tags-file tags-file
          etags-regen--tags-root root)
    (with-temp-buffer
      (mapc (lambda (f)
              (when (string-match-p file-regexp f)
                (insert f "\n")))
            files)
      (shell-command-on-region (point-min) (point-max) command
                               nil nil etags-regen--errors-buffer-name t))))

(defun etags-regen--update-file ()
  ;; TODO: Maybe only do this when Emacs is idle for a bit.
  (let ((file-name buffer-file-name)
        (options etags-regen-program-options)
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
                 ;; FIXME: With a big enough TAGS, even this can be
                 ;; slow (and, unfortunately, synchronous).  Using the
                 ;; project-relative name here speeds it up, but only
                 ;; by ~30%.  Some indexing could help.
                 (re-search-forward (format "^%s," (regexp-quote file-name)) nil t))
          (let ((start (line-beginning-position)))
            (re-search-forward "\f\n" nil 'move)
            (let ((inhibit-read-only t)
                  (save-silently t))
              (delete-region (- start 2)
                             (if (eobp)
                                 (point)
                               (- (point) 2)))
              (write-region (point-min) (point-max) buffer-file-name nil 'silent)
              (set-visited-file-modtime)))
          (setq should-scan t))))
      (when should-scan
        (goto-char (point-max))
        (let ((inhibit-read-only t)
              (current-end (point)))
          ;; FIXME: call-process is significantly faster, though.
          ;; Like 10ms vs 20ms here.
          (shell-command
           (format "%s %s %s -o -"
                   etags-regen-program (mapconcat #'identity options " ")
                   file-name)
           t etags-regen--errors-buffer-name)
          ;; XXX: When the project is big (tags file in 10s of megabytes),
          ;; this is much faster than revert-buffer.  Or even using
          ;; write-region without APPEND.
          ;; We could also keep TAGS strictly as a buffer, with no
          ;; backing on disk.
          (write-region current-end (point-max) etags-regen--tags-file t 'silent))
        (set-visited-file-modtime)
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
    (delete-file etags-regen--tags-file)
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
