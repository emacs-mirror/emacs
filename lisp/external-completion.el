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
(require 'cl-lib)

(add-to-list 'completion-styles-alist
             '(external
               external-completion--try-completion
               external-completion--all-completions
               "Ad-hoc completion style provided by the completion table."))

(defun external-completion-table (category lookup
                                           &optional metadata
                                           try-completion-function)
  "Make completion table using the `external' completion style.

The `external' style is particularly useful when the caller
interfaces with an external tool that provides completions.  This
may be a shell utility, an inferior process, an http server, etc.
Given a pattern string, the external tool matches it to an
arbitrarily large set of candidates.  Since the full set doesn't
need to be transferred to Emacs's address space, this often
results in much faster overall experience, at the expense of the
convenience of offered by other completion styles.

CATEGORY is a symbol uniquely naming the external tool.  This
function links CATEGORY to the style `external', by modifying
`completion-category-defaults', overriding any styles normally
set in `completion-styles'.

LOOKUP is a function taking a string PATTERN and a number
POINT. The function should contact the tool and return a list of
strings representing the completions for PATTERN given that POINT
is the location of point within it.  LOOKUP decides if PATTERN is
interpreted as a substring, a regular expression, or any other
type of matching method.  The strings returned may be propertized
with `completions-common-part' to illustrate the specific method
used.  LOOKUP may ignore POINT if it doesn't meaningfully alter
the results.

LOOKUP is a synchronous blocking function.  Since it contacts an
external tool, it's possible that it takes significant time to
return results.  To maintain Emacs's responsiveness, LOOKUP
should detect pending user input using `while-no-input' or
`sit-for' (which see).  In those cases, LOOKUP should attempt to
cancel the request (if possible) and immediately return any
non-list.

METADATA is an alist of additional properties such as
`cycle-sort-function' to associate with CATEGORY.  This means
that the caller may still retain control the sorting of the
candidates while the tool controls the matching.

Optional TRY-COMPLETION-FUNCTION helps some frontends partially
or fully expand PATTERN before finishing the completion
operation.  If supplied, it is a function taking a (PATTERN POINT
ALL-COMPLETIONS), where PATTERN and POINT are as described above
and ALL-COMPLETIONS are gathered by LOOKUP for these
arguments (this function ensures LOOKUP isn't called more than
needed).  If you know the matching method that the external tool
using, TRY-COMPLETION-FUNCTION may return a cons
cell (EXPANDED-PATTERN . NEW-POINT).  For example, if the tool is
completing by prefix, one could call `try-completion' to find the
largest common prefix in ALL-COMPLETIONS and then return that as
EXPANDED-PATTERN."
  (let ((probe (alist-get category completion-category-defaults)))
    (if probe
        (cl-assert (equal '(external) (alist-get 'styles probe))
                   nil "Category `%s' must only use `external' style" category)
        (push `(,category (styles external))
              completion-category-defaults)))
  (let ((cache (make-hash-table :test #'equal)))
    (cl-flet ((lookup-internal (string point)
                (let* ((key (cons string point))
                       (probe (gethash key cache 'external--notfound)))
                  (if (eq probe 'external--notfound)
                      (puthash key (funcall lookup string point) cache)
                    probe))))
      (lambda (string pred action)
        (pcase action
          (`metadata
           `(metadata (category . ,category) . ,metadata))
          ;; Note: the `--tryc' `--allc' suffixes are made akward on
          ;; purpose, so it's easy to pick them apart from the jungle
          ;; of combinations of "try" and "all" and "completion" that
          ;; inhabit Emacs's completion logic.
          (`(external-completion--tryc . ,point)
           ;; FIXME: Obey `pred'?  Pass it to `try-completion-function'?
           `(external-completion--tryc
             . ,(if try-completion-function
                    (funcall try-completion-function
                             string
                             point
                             (lookup-internal string point))
                  (cons string point))))
          (`(external-completion--allc . ,point)
           (let ((all (lookup-internal string point)))
             `(external-completion--allc
               . ,(if pred (cl-remove-if-not pred all) all))))
          (`(boundaries . ,_) nil)
          (_method
           (let ((all (lookup-internal string (length string))))
             ;; This branch might be taken:
             ;;
             ;; * when users work around
             ;;   `completion-category-defaults' (via
             ;;   `completion-category-overrides') and access this
             ;;   table with another completion style.  We assume
             ;;   these users know what they are doing, but it might
             ;;   not work very well, as this whatever is in `all'
             ;;   very often doesn't equate the full set of candidates
             ;;   (many tools cap to sth like 100-1000 results).
             ;;
             ;; * when `_method' is `nil' or `lambda' which some
             ;;   frontends will invoke.  Here, `all' should be
             ;;   suficient information for `complete-with-action' to
             ;;   do the job correctly.
             (complete-with-action action all string pred))))))))

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
