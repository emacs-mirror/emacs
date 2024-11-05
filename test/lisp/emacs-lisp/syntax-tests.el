;;; syntax-tests.el --- tests for syntax.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2024 Free Software Foundation, Inc.

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Code:

(require 'ert)
(require 'syntax)

(ert-deftest syntax-propertize--shift-groups-and-backrefs ()
  "Test shifting of numbered groups and back-references in regexps."
  ;; A numbered group must be shifted.
  (should
   (string=
    (syntax-propertize--shift-groups-and-backrefs
     "\\(?2:[abc]+\\)foobar" 2)
    "\\(?4:[abc]+\\)foobar"))
  ;; A back-reference \1 on a normal sub-regexp context must be
  ;; shifted.
  (should
   (string=
    (syntax-propertize--shift-groups-and-backrefs "\\(a\\)\\1" 2)
    "\\(a\\)\\3"))
  ;; Shifting must not happen if the \1 appears in a character class,
  ;; or in a \{\} repetition construct (although \1 isn't valid there
  ;; anyway).
  (let ((rx-with-class "\\(a\\)[\\1-2]")
        (rx-with-rep "\\(a\\)\\{1,\\1\\}"))
    (should
     (string=
      (syntax-propertize--shift-groups-and-backrefs rx-with-class 2)
      rx-with-class))
    (should
     (string=
      (syntax-propertize--shift-groups-and-backrefs rx-with-rep 2)
      rx-with-rep)))
  ;; Now numbered groups and back-references in combination.
  (should
   (string=
    (syntax-propertize--shift-groups-and-backrefs
     "\\(?2:[abc]+\\)foo\\(\\2\\)" 2)
    "\\(?4:[abc]+\\)foo\\(\\4\\)"))
  ;; Emacs supports only the back-references \1,...,\9, so when a
  ;; shift would result in \10 or more, an error must be signaled.
  (should-error
   (syntax-propertize--shift-groups-and-backrefs "\\(a\\)\\3" 7)))

;;; syntax-tests.el ends here.
