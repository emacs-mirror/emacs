;;; external-completion.el --- Let external tools control completion style  -*- lexical-binding: t; -*-

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

;; The `external' completion style is used with a "programmable
;; completion" table that gathers completions from an external tool
;; such as a shell utility, an inferior process, an http server.

;; The table and external tool are fully in control of the matching of
;; the pattern string to the potential candidates of completion.  When
;; `external' is in use, the usual styles configured by the user or
;; other in `completion-styles' are ignored.
;;
;; This compromise is for speed: all other styles need the full data
;; set to be available in Emacs' addressing space, which is often slow
;; if not completely unfeasible.
;;
;; To make use of the `external' style the function
;; `external-completion-table' should be used.  See its docstring.

;;; Code:
(add-to-list 'completion-styles-alist
             '(external
               external-completion--try-completion
               external-completion--all-completions
               "Ad-hoc completion style provided by the completion table."))

(defun external-completion-table (category lookup
                                           &optional metadata
                                           try-completion-function)
  "Make completion table using the `external' completion style.

The completion table produced foregoes any styles normally set in
`completion-styles' and will sets up an entry for the symbol
CATEGORY in `completion-category-defaults', linking it to the
special completion style `external'.

This style is useful when the caller interfaces with an external
tool that provides completions.  This may be a shell utility, an
inferior process, an http server, etc.  The external tool does
the matching, and the full set of candidates doesn't need to be
transferred to Emacs's address space.  This often results in much
faster overall completion at the expense of the convenience of
offered by the rich variety of usual completion styles usually
available.

LOOKUP is a function taking a string PATTERN and a number
POINT. The function should contact the backend and return a list
of strings representing the candidates matching PATTERN given
that POINT is the location of point within it.  LOOKUP decides if
PATTERN is interpreted as a substring, a regular expression, or
any other type of matching key.  Strings returned by LOOKUP may
be propertized with `completions-common-part' to illustrate the
specific interpretationq.  To maintain responsiveness in the face
of all but the spiffiest external tools, LOOKUP should detect
timeouts and pending user input with `while-no-input' or
`sit-for' (which see), cancel the request (if that is possible)
and immediately return any non-list.

CATEGORY is a symbol identifying the external tool.  METADATA is
an alist of additional properties such as `cycle-sort-function'
to associate with CATEGORY.  This means that the caller may still
retain control the sorting of the candidates while the tool
controls the matching.

Despite the original authors's best efforts,
TRY-COMPLETION-FUNCTION is still a poorly understood
implementation detail.  If you understand what it's for, you
should definitely update this docstring, really!  Anyway, it's a
function taking a (STRING POINT) as arguments.  The default is to
use the function `cons' which returns the arguments as a cons
cell."
  (unless (assq category completion-category-defaults)
    (push `(,category (styles external))
          completion-category-defaults))
  (lambda (string pred action)
    (pcase action
      (`metadata
       `(metadata (category . ,category) . ,metadata))
      (`(external-completion--tryc . ,point)
       ;; FIXME: Obey `pred'?  Pass it to `try-completion-function'?
       `(external-completion--tryc
         . ,(funcall (or try-completion-function #'cons) string point)))
      (`(external-completion--allc . ,point)
       (let ((all (funcall lookup string point)))
         `(external-completion--allc . ,(if pred (seq-filter pred all) all))))
      (`(boundaries . ,_) nil)
      (_
       ;; FIXME: Stefan had an extra call to `lookup' and
       ;; `complete-with-action' again here.  `action' is usually
       ;; `lambda' or `nil' in those cases.  Since calling `lookup'
       ;; again is potentially slow and no useful result seems to come
       ;; from it, I took it out.
       ))))

;; Note: the "tryc", "allc" suffixes are made akward on purpose, so
;; it's easy to pick them apart from the jungle of combinations of
;; "try" and "all" and "completion" that inhabit Emacs's completion
;; logic.
(defun external-completion--call (op string table pred point)
  (when (functionp table)
    (let ((res (funcall table string pred (cons op point))))
      (when (eq op (car-safe res))
        (cdr res)))))

(defun external-completion--try-completion (string table pred point)
  (external-completion--call 'external-completion--tryc string table pred point))

(defun external-completion--all-completions (string table pred point)
  (external-completion--call 'external-completion--allc string table pred point))

(provide 'external-completion)
;;; external-completion.el ends here
