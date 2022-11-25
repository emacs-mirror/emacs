;;; backend-completion.el --- Let external tools control completion style  -*- lexical-binding: t; -*-

;; Copyright (C) 2018-2022 Free Software Foundation, Inc.

;; Version: 0.1
;; Author: Stefan Monnier <monnier@iro.umontreal.ca>
;; Maintainer: João Távora <joaotavora@gmail.com>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Written by Stefan Monnier circa 2016.  Variants of this code have
;; been working stably in SLY and other packages for a long time.

;; This completion style is meant to be used with a "programmable
;; completion" table that interfaces with an external tool provinding
;; completions, such as a shell utility, an inferior process, an http
;; server.  The table and external tool are tasked to do the matching
;; of the pattern string to the potential candidates of completion,
;; and as such it fully controls the style.

;; When this completion style is in use, the usual styles configured
;; by the user or other in `completion-styles' are completely
;; overriden.  This can be seen as a drawback, but, on the other hand,
;; the regular the full data set to be available in Emacs' addressing
;; space, which is often not feasible.
;;
;; The programmable completion table amounts to a function taking
;; (PATTERN PRED ACTION) as arguments respond to at least three values
;; for ACTION:
;;
;; * The symbol `metadata', where the table should reply with a list
;;   that looks like:
;;
;;      (metadata (category . backend-completion) MORE...)
;;
;;    where MORE... can be other "metadata" items like
;;    `cycle-sort-function'.
;;
;;    Other categories can be used in place of `backend-completion',
;;    as long as the `styles' property of such categories contains the
;;    sole element `backend-completion-backend-style'.

;; * (backend-completion-tryc . POINT) where the reply should be:
;;
;;      (backend-completion-tryc . (PATTERN . POINT))
;;
;; * (backend-completion-allc . POINT) where the reply should be
;;
;;      (backend-completion-allc COMPS...)
;;
;;   Where COMPS... is a list of strings which are all the completions
;;   that the external tool has found for PATTERN and POINT.  If the
;;   style that the external tool is using to match PATTERN is known,
;;   elements of COMPS can be propertized with
;;   'completions-common-part' in the relevant sections.

;; Note: the "tryc", "allc" suffixes are made akward on purpose, so
;; it's easy to pick them apart from the jungle of combinations of
;; "try" and "all" and "completion" that inhabit Emacs's completion
;; logic.

;;; Code:
(add-to-list 'completion-styles-alist
             '(backend-completion-backend-style
               backend-completion--try-completion
               backend-completion--all-completions
               "Ad-hoc completion style provided by the completion table."))

(add-to-list 'completion-category-defaults
             '(backend-completion (styles . (backend-completion-backend-style))))

;; (add-to-list 'completion-category-overrides
;;              '(backend-completion (styles . (backend-completion-backend-style))))

(defun backend-completion--call (op string table pred point)
  (when (functionp table)
    (let ((res (funcall table string pred (cons op point))))
      (when (eq op (car-safe res))
        (cdr res)))))

(defun backend-completion--try-completion (string table pred point)
  (backend-completion--call 'backend-completion-tryc string table pred point))

(defun backend-completion--all-completions (string table pred point)
  (backend-completion--call 'backend-completion-allc string table pred point))

(provide 'backend-completion)
;;; backend-completion.el ends here
