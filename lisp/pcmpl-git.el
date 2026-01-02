;;; pcmpl-git.el --- Completions for Git -*- lexical-binding: t -*-

;; Copyright (C) 2022-2026 Free Software Foundation, Inc.

;; Package: pcomplete

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

;; This library provides completion rules for the Git program.

;;; Code:

(require 'pcomplete)
(require 'vc-git)

(defun pcmpl-git--expand-flags (args)
  "In the list of ARGS, expand arguments of the form --[no-]flag."
  (mapcan (lambda (arg) (if (string-search "[no-]" arg)
                            (list (string-replace "[no-]" "" arg)
                                  (string-replace "[no-]" "no-" arg))
                          (list arg)))
          args))

(defun pcmpl-git--tracked-file-predicate (&rest args)
  "Return a predicate function determining the Git status of a file.
Files listed by `git ls-files ARGS' satisfy the predicate."
  (when-let* ((files (mapcar #'expand-file-name
                             (ignore-errors
                               (apply #'process-lines
                                      vc-git-program "ls-files" args)))))
    (lambda (file)
      (setq file (expand-file-name file))
      (if (string-suffix-p "/" file)
          (seq-some (lambda (f) (string-prefix-p file f))
                    files)
        (member file files)))))

(defun pcmpl-git--remote-refs (remote)
  "List the locally known Git revisions from REMOTE."
  (delq nil
        (mapcar
         (let ((re (concat "\\`" (regexp-quote remote) "/\\(.*\\)")))
           (lambda (s) (when (string-match re s) (match-string 1 s))))
         (vc-git-revision-table nil))))

;;;###autoload
(defun pcomplete/git ()
  "Completion for the `git' command."
  (let ((subcommands (pcomplete-from-help `(,vc-git-program "help" "-a")
                                          :margin "^\\( +\\)[a-z]"
                                          :argument "[[:alnum:]-]+")))
    (while (not (member (pcomplete-arg 1) subcommands))
      (if (string-prefix-p "-" (pcomplete-arg))
          (pcomplete-here (pcomplete-from-help `(,vc-git-program "help")
                                               :margin "\\(\\[\\)-"
                                               :separator " | "
                                               :description "\\`"))
        (pcomplete-here (completion-table-merge
                         subcommands
                         (when (string-prefix-p "-" (pcomplete-arg 1))
                           (pcomplete-entries))))))
    (let ((subcmd (pcomplete-arg 1)))
      (while (pcase subcmd
               ((guard (string-prefix-p "-" (pcomplete-arg)))
                (pcomplete-here
                 (pcmpl-git--expand-flags
                  (pcomplete-from-help `(,vc-git-program "help" ,subcmd)
                                       :argument
                                       "-+\\(?:\\[no-\\]\\)?[a-z-]+=?"))))
               ;; Complete modified tracked files and untracked files and
               ;; ignored files if -f or --force is specified.
               ("add"
                (pcomplete-here
                 (pcomplete-entries
                  nil
                  (let ((flags (list "-o" "-m")))
                    (unless (or (member "-f" pcomplete-args) (member "--force" pcomplete-args))
                      (push "--exclude-standard" flags))
                    (apply #'pcmpl-git--tracked-file-predicate flags)))))
               ;; Complete modified tracked files
               ((or "commit" "restore")
                (pcomplete-here
                 (pcomplete-entries
                  nil (pcmpl-git--tracked-file-predicate "-m"))))
               ;; Complete all tracked files
               ((or "mv" "rm" "grep" "status" "blame")
                (pcomplete-here
                 (pcomplete-entries nil (pcmpl-git--tracked-file-predicate))))
               ;; Complete revisions
               ((or "branch" "merge" "rebase" "switch")
                (pcomplete-here (vc-git-revision-table nil)))
               ;; Complete revisions and tracked files
               ;; TODO: diff and log accept revision ranges
               ((or "checkout" "reset" "show" "diff" "log")
                (pcomplete-here
                 (completion-table-in-turn
                  (vc-git-revision-table nil)
                  (pcomplete-entries nil (pcmpl-git--tracked-file-predicate)))))
               ;; Complete remotes and their revisions
               ((or "fetch" "pull" "push")
                (pcomplete-here (process-lines vc-git-program "remote"))
                (pcomplete-here (pcmpl-git--remote-refs (pcomplete-arg 1))))
               ;; Complete all files
               ((or "apply" "am")
                (pcomplete-here (pcomplete-entries))))))))

(provide 'pcmpl-git)
;;; pcmpl-git.el ends here
