;;; last-chance.el --- dangling deterrence     -*- lexical-binding: t; -*-

;; Copyright (C) 2016-2017 Free Software Foundation, Inc.

;; Author: Thien-Thi Nguyen <ttn@gnu.org>
;; Maintainer: emacs-devel@gnu.org
;; Keywords: maint
;; Package: emacs

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

;; Late 2016.  In a recent build, the date in a ChangeLog file
;; is not fontified.  Turns out the face ‘change-log-date-face’
;; was declared obsolete since 22.1 and removed 2016-06-23.
;; (compile "git show c430f7e23fc2c22f251ace4254e37dea1452dfc3")
;;
;; This library provides a command ‘last-chance’, which is a small
;; combination of "git grep" and some regexp filtering.  For example,
;; if point is on the symbol ‘change-log-date-face’ in the form:
;;
;;  (define-obsolete-face-alias 'change-log-date-face ...)
;;
;; then:
;;
;;  M-x last-chance RET
;;
;; will show you any references to ‘change-log-date-face’ in the
;; *.el files in a new buffer (in Grep mode).  Hopefully you see
;; only the obsolete declaration and can proceed w/ its removal.
;; If not, please DTRT and refrain from the removal until those
;; references are properly transitioned.
;;
;; [Insert "nobody reads ChangeLog files" lament, here.  --ttn]

;;; Code:

(require 'grep)
(require 'thingatpt)

(defvar last-chance-grep-command "git grep -n -H -F -e"
  "Command that ends in \"-e\" to do the \"git grep\".
This should include -n, -H, -F.")

(defvar last-chance-uninteresting-regexps
  '("ChangeLog[.0-9]*:"
    "NEWS[-.0-9]*:"
    ;; Add more ‘flush-lines’ args here.
    )
  "List of regexps that match uninteresting \"git grep\" hits.")

(defvar-local last-chance-symbol nil
  "Symbol set by ‘last-chance’ for ‘last-chance-cleanup’ to DTRT.")

(defun last-chance-cleanup (buffer status)
  "Filter lines in BUFFER; append STATUS and count of removed lines.
If BUFFER does not seem to be one created by ‘last-chance’, do nothing.
This function is intended to be added to ‘compilation-finish-functions’."
  (let ((name (buffer-local-value 'last-chance-symbol buffer))
        bef aft)
    (when name
      (with-current-buffer buffer
        (setq bef (count-lines (point-min) (point-max)))
        (goto-char (point-min))
        (search-forward last-chance-grep-command)
        (forward-line 1)
        (let ((inhibit-read-only t))
          (dolist (re last-chance-uninteresting-regexps)
            (flush-lines re))
          (keep-lines (format "\\_<%s\\_>" name)))
        (setq aft (count-lines (point-min) (point-max)))
        (goto-char (point-max))
        (insert (format "(status: %s, lines removed: %d)"
                        (car (split-string status "\n"))
                        (- bef aft)))))))

(defun last-chance (symbol)
  "Grep the repo for SYMBOL, filtering the hits.
This uses ‘last-chance-grep-command’ to do the grep and the
regexps in ‘last-chance-uninteresting-regexps’ to filter the hits.
Grepping is recursive starting under the dir that ‘vc-root-dir’
finds (or the default directory if ‘vc-root-dir’ finds nothing).
Output goes to the *grep* buffer.

Interactively, Emacs queries for a symbol,
defaulting to the one at point."
  (interactive (list (read (let ((one (symbol-at-point)))
                             (when one
                               (setq one (symbol-name one)))
                             (completing-read
                              "Symbol: " obarray
                              nil nil
                              one nil one)))))
  (let ((default-directory (or (vc-root-dir)
                               default-directory)))
    (grep (format "%s %s"
                  last-chance-grep-command
                  symbol)))
  (setf (buffer-local-value 'last-chance-symbol
                            (process-buffer
                             (car compilation-in-progress)))
        symbol))

(add-to-list 'compilation-finish-functions
             'last-chance-cleanup)

(provide 'last-chance)

;;; last-chance.el ends here
