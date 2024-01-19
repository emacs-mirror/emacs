;;; crm.el --- read multiple strings with completion  -*- lexical-binding: t; -*-

;; Copyright (C) 1985-1986, 1993-2024 Free Software Foundation, Inc.

;; Author: Sen Nagata <sen@eccosys.com>
;; Keywords: completion, minibuffer, multiple elements

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

;; This code defines a function, `completing-read-multiple', which
;; provides the ability to read multiple strings in the minibuffer,
;; with completion.  See that function's documentation for details.

;; For the moment, I have decided to not bind any special behavior to
;; the separator key.  In the future, the separator key might be used
;; to provide completion in certain circumstances.  One of the reasons
;; why this functionality is not yet provided is that it is unclear to
;; the author what the precise circumstances are, under which
;; separator-invoked completion should be provided.

;; Design note: `completing-read-multiple' is modeled after
;; `completing-read'.  They should be similar -- it was intentional.

;; Some of this code started out as translation from C code in
;; src/minibuf.c to Emacs Lisp code.  After this code was rewritten in Elisp
;; and made to operate on any field, this file was completely rewritten to
;; just reuse that code.

;; Thanks to Sen Nagata <sen@eccosys.com> for the original version of the
;; code, and sorry for throwing it all out.  --Stef

;; Thanks to Richard Stallman for all of his help (many of the good
;; ideas in here are from him), Gerd Moellmann for his attention,
;; Stefan Monnier for responding with a code sample and comments very
;; early on, and Kai Grossjohann & Soren Dayton for valuable feedback.

;;; Questions and Thoughts:

;; -should `completing-read-multiple' allow a trailing separator in
;; a return value when REQUIRE-MATCH is t?  if not, should beep when a user
;; tries to exit the minibuffer via RET?

;; -tip: use M-f and M-b for ease of navigation among elements.

;; - the difference between minibuffer-completion-table and
;;   crm-completion-table is just crm--collection-fn.  In most cases it
;;   shouldn't make any difference.  But if a non-CRM completion function
;;   happens to be used, it will use minibuffer-completion-table and
;;   crm--collection-fn will try to make it do "more or less the right
;;   thing" by making it complete on the last element, which is about as
;;   good as we can hope for right now.
;;   I'm not sure if it's important or not.  Maybe we could just throw away
;;   crm-completion-table and crm--collection-fn, but there doesn't seem to
;;   be a pressing need for it, and since Sen did bother to write it, we may
;;   as well keep it, in case it helps.

;;; History:
;;
;; 2000-04-10:
;;
;;   first revamped version
;;
;; 2024-01-03:
;;
;;   second revamped version

;;; Code:

(define-obsolete-variable-alias 'crm-default-separator 'crm-separator "29.1")

(defvar crm-separator "[ \t]*,[ \t]*"
  "Separator regexp used for separating strings in `completing-read-multiple'.
It should be a regexp that does not match the list of completion candidates.

This can also be a cons cell (REGEXP . CANONICAL), where REGEXP
is the separator regexp used for matching input separators, and
CANONICAL is a canonical separator string that Emacs uses when it
inserts a separator for you.  If CANONICAL does not match REGEXP,
it is ignored.  See also `crm-complete-and-insert-separator'.")

(defvar crm-common-separators '(",")
  "List of strings often used to separate multiple minibuffer inputs.

See also `crm-complete-and-insert-separator'.")

(defvar crm-current-separator nil
  "The value of `crm-separator' for the current minibuffer.")

(defvar crm-canonical-separator nil
  "Canonical separator for `completing-read-multiple'.

This can either a string that matches `crm-current-separator', or
nil when there is no canonical separator.")

(defun crm-complete-and-exit ()
  "If all of the minibuffer elements are valid completions then exit.
All elements in the minibuffer must match.  If there is a mismatch, move point
to the location of mismatch and do not exit.

This function is modeled after `minibuffer-complete-and-exit'."
  (interactive "" minibuffer-mode)
  (let ((bob (minibuffer--completion-prompt-end))
        (doexit t))
    (goto-char bob)
    (while
        (and doexit
             (let* ((beg (save-excursion
                           (if (re-search-backward crm-current-separator bob t)
                               (match-end 0)
                             bob)))
                    (end (copy-marker
                          (save-excursion
                            (if (re-search-forward crm-current-separator nil t)
                                (match-beginning 0)
                              (point-max)))
                          t)))
               (goto-char end)
               (setq doexit nil)
               (completion-complete-and-exit beg end
                                             (lambda () (setq doexit t)))
               (goto-char end)
               (not (eobp)))
             (looking-at crm-current-separator))
      (when doexit
        (goto-char (match-end 0))))
    (if doexit (exit-minibuffer))))

(defvar-local read-string-matching-regexp nil
  "Regular expression that minibuffer input must match.")

(defun read-string-matching-try-exit ()
  "Exit minibuffer only if the input matches `read-string-matching-regexp'."
  (interactive nil minibuffer-mode)
  (if (string-match-p read-string-matching-regexp (minibuffer-contents))
      (exit-minibuffer)
    (user-error "Input does not match \"%s\"" read-string-matching-regexp)))

(defvar-keymap read-string-matching-mode-map
  :doc "Keymap for `read-string-matching-mode'."
  "<remap> <exit-minibuffer>" #'read-string-matching-try-exit)

(define-minor-mode read-string-matching-mode
  "Minor mode for reading a string matching some regular expression.

`read-string-matching' enables this minor mode in the minibuffer."
  :lighter nil)

(defun read-string-matching (regexp prompt &optional
                                    initial-input history
                                    default-value inherit-input-method)
  "Read a string matching REGEXP in the minibufffer.

This function calls `read-string' with arguments PROMPT,
INITIAL-INPUT, HISTORY, DEFAULT-VALUE and INHERIT-INPUT-METHOD."
  (minibuffer-with-setup-hook
      (lambda ()
        (read-string-matching-mode)
        (setq read-string-matching-regexp regexp))
    (read-string prompt initial-input history default-value
                 inherit-input-method)))

(defun crm-change-separator (sep &optional rep)
  "Set the current `crm-separator' to SEP.

Non-nil optional argument REP says to replace occurrences of the
old `crm-separator' in the current minibuffer contents with REP.

Interactively, prompt for SEP.  With a prefix argument, prompt
for REP as well."
  (interactive
   (let* ((enable-recursive-minibuffers t)
          (sep (read-regexp
                (format-prompt "New separator" crm-current-separator)
                crm-current-separator)))
     (list sep
           (when current-prefix-arg
             (read-string-matching sep "Replace existing separators with: "))))
   minibuffer-mode)
  (when rep
    (goto-char (minibuffer-prompt-end))
    (while (re-search-forward crm-current-separator nil t)
      (replace-match rep t t)))
  (setq crm-current-separator sep crm-canonical-separator rep)
  (when (get-buffer-window "*Completions*" 0)
    ;; Update *Completions* to avoid stale `completion-base-affixes'.
    (minibuffer-completion-help)))

(defun crm-complete-and-insert-separator ()
  "Complete partial inputs and then insert a new input separator.

If `crm-canonical-separator' is non-nil and matches the regular
expression `crm-current-separator', then this command uses
`crm-canonical-separator' as the separator.  Otherwise, this
command tries to find an appropriate separator by matching
`crm-current-separator' against your current input and against
the list of common separators in `crm-common-separators', and if
that fails this command prompts you for the separator to use."
  (interactive "" minibuffer-mode)
  (let ((bob (minibuffer--completion-prompt-end))
        (all-complete t)
        (enable-recursive-minibuffers t))
    ;; Establish a canonical separator string, so we can insert it.
    (setq crm-canonical-separator
          (or
           ;; If `crm-canonical-separator' matches, use it.
           (and (stringp crm-canonical-separator)
                (string-match-p crm-current-separator
                                crm-canonical-separator)
                crm-canonical-separator)
           ;; If there's some separator already, use that.
           (and (save-excursion
                  (goto-char bob)
                  (re-search-forward crm-current-separator nil t))
                (buffer-substring-no-properties (match-beginning 0)
                                                (match-end 0)))
           ;; If any common separator matches, use it.
           (seq-some (lambda (sep)
                       (and (string-match-p crm-current-separator sep)
                            sep))
                     crm-common-separators)
           ;; Ask the user for help.
           (read-string-matching crm-current-separator
                                 "Separate inputs with: ")))
    (while
        (and all-complete
             (let* ((beg (save-excursion
                           (if (re-search-backward crm-current-separator bob t)
                               (match-end 0)
                             bob)))
                    (end (copy-marker
                          (save-excursion
                            (if (re-search-forward crm-current-separator nil t)
                                (match-beginning 0)
                              (point-max)))
                          t)))
               (goto-char end)
               (setq all-complete nil)
               (completion-complete-and-exit
                beg end (lambda () (setq all-complete t)))
               (goto-char end)
               (not (eobp)))
             (looking-at crm-current-separator))
      (when all-complete
        (goto-char (match-end 0))))
    (when all-complete
      (if (looking-back crm-current-separator bob)
          ;; Separator already present, show completion candidates.
          (minibuffer-completion-help)
        (insert crm-canonical-separator)))))

(define-minor-mode completions-multi-mode
  "Minor mode for reading multiple strings in the minibuffer."
  :lighter (:eval
            (let ((canonical
                   (buffer-local-value 'crm-canonical-separator
                                       completion-reference-buffer)))
              (propertize
               (concat
                " Multi"
                (when canonical (concat "[" crm-canonical-separator "]")))
               'help-echo
               (concat
                "Insert multiple inputs by separating them with \""
                (or canonical
                    (buffer-local-value 'crm-current-separator
                                        completion-reference-buffer))
                "\"")))))

(defun crm-completion-setup ()
  "Enable `completions-multi-mode' in *Completions* buffer."
  (with-current-buffer standard-output (completions-multi-mode)))

(define-obsolete-variable-alias 'crm-local-completion-map
  'completing-read-multiple-mode-map "30.1")

(define-obsolete-variable-alias 'crm-local-must-match-map
  'completing-read-multiple-mode-map "30.1")

(defvar-keymap completing-read-multiple-mode-map
  :doc "Keymap for `completing-read-multiple-mode'."
  "<remap> <minibuffer-complete-and-exit>" #'crm-complete-and-exit
  "C-x ," #'crm-change-separator
  "C-," #'crm-complete-and-insert-separator)

(define-minor-mode completing-read-multiple-mode
  "Minor mode for reading multiple strings in the minibuffer."
  :lighter nil
  (if completing-read-multiple-mode
      (add-hook 'completion-setup-hook #'crm-completion-setup 10 t)
    (remove-hook 'completion-setup-hook #'crm-completion-setup t)))

;;;###autoload
(defun completing-read-multiple
  (prompt table &optional predicate require-match initial-input
	  hist def inherit-input-method)
  "Read multiple strings in the minibuffer, with completion.
The arguments are the same as those of `completing-read'.
\\<minibuffer-local-completion-map>
Input multiple strings by separating each one with a string that
matches the regexp `crm-separator'.  For example, if the separator
regexp is \",\", entering \"alice,bob,eve\" specifies the strings
\"alice\", \"bob\", and \"eve\".

We refer to contiguous strings of non-separator-characters as
\"elements\".  In this example there are three elements.

Completion is available on a per-element basis.  For example, if the
contents of the minibuffer are \"alice,bob,eve\" and point is between
\"l\" and \"i\", pressing \\[minibuffer-complete] operates on the element \"alice\".

This function returns a list of the strings that were read,
with empty strings removed."
  (let ((crm-current-separator
         (if (consp crm-separator)
             (car crm-separator)
           crm-separator))
        (crm-canonical-separator (cdr-safe crm-separator)))
    (split-string
     (minibuffer-with-setup-hook
         #'completing-read-multiple-mode
       (completing-read
        prompt
        (lambda (s p a)
          (let ((beg 0))
            (while (string-match crm-current-separator s beg)
              (setq beg (match-end 0)))
            (pcase a
              (`(boundaries . ,suffix)
               (let ((bounds (completion-boundaries
                              (substring s beg) table p
                              (substring suffix 0
                                         (string-match crm-current-separator
                                                       suffix)))))
                 `(boundaries ,(+ (car bounds) beg) . ,(cdr bounds))))
              ('metadata (completion-metadata (substring s beg) table p))
              ('nil (let ((comp (complete-with-action a table
                                                      (substring s beg) p)))
                      (if (stringp comp)
                          (concat (substring s 0 beg) comp)
                        comp)))
              (_ (complete-with-action a table (substring s beg) p)))))
        predicate require-match initial-input hist def inherit-input-method))
     crm-current-separator t)))

(provide 'crm)

;;; crm.el ends here
