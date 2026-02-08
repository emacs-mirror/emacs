;;; gnus-group-tests.el --- Tests for gnus-group.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2026 Free Software Foundation, Inc.

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

;;

;;; Code:

(require 'gnus-group)
(require 'ert)

(ert-deftest gnus-short-group-name ()
  (map-apply
   (lambda (input expected)
     (should (string-equal (gnus-short-group-name input) expected)))
   '(("nnimap+email@example.com:archives/2020/03" . "email@example:a/2/03")
     ("nndiary+diary:birthdays" . "diary:birthdays")
     ("nnimap+email@example.com:test" . "email@example:test")
     ("nnimap+email@example.com:234" . "email@example:234")

     ;; This is a very aggressive shortening of the left hand side.
     ("nnimap+email@banana.salesman.example.com:234" . "email@banana:234")
     ("nntp+some.where.edu:soc.motss" . "some:s.motss")
     ("nntp+news.gmane.io:gmane.emacs.gnus.general" . "news:g.e.g.general")
     ("nntp+news.gnus.org:gmane.text.docbook.apps" . "news:g.t.d.apps")

     ;; nnimap groups.
     ("nnimap+email@example.com:[Invoices]/Bananas" . "email@example:I/Bananas")
     ("nnimap+email@banana.salesman.example.com:[Invoices]/Bananas"
      . "email@banana:I/Bananas")

     ;; The "n" from "nnspool" is perhaps not optimal.
     ("nnspool+alt.binaries.pictures.furniture" . "n.b.p.furniture"))))

;;; gnus-group-tests.el ends here
