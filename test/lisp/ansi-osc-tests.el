;;; osc-tests.el --- Tests for osc.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2026 Free Software Foundation, Inc.

;; Author: Matthias Meulien <orontee@gmail.com>
;; Keywords:

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

(require 'ansi-osc)
(require 'ert)

(defvar ansi-osc-tests--strings
  `(("Hello World" "Hello World")

    ;; window title
    ("Buffer \e]2;A window title\e\\content" "Buffer content")

    ;; window title
    ("Unfinished \e]2;window title" "Unfinished \e]2;window title")

    ;; current directory
    ("\e]7;file://127.0.0.1/tmp\e\\user@host$ " "user@host$ ")

    ;; hyperlink
    ("\e]8;;http://example.com\e\\This is a link\e]8;;\e\\" "This is a link")

    ;; multiple sequences
    ("Escape \e]2;A window title\e\\sequence followed by \e]2;unfinished sequence"
     "Escape sequence followed by \e]2;unfinished sequence")
    ))
;; Don't output those strings to stdout since they may have
;; side-effects on the environment

(ert-deftest ansi-osc-tests-apply-region-no-handlers ()
  (let ((ansi-osc-handlers nil))
    (pcase-dolist (`(,input ,text) ansi-osc-tests--strings)
      (with-temp-buffer
        (insert input)
        (ansi-osc-apply-on-region (point-min) (point-max))
        (should (equal
                 (buffer-substring-no-properties
                  (point-min) (point-max))
                 text))))))

(ert-deftest ansi-osc-tests-apply-region-no-handlers-multiple-calls ()
  (let ((ansi-osc-handlers nil))
    (with-temp-buffer
      (insert
       (concat "First set the window title \e]2;A window title\e\\"
               "then change it\e]2;Another "))
      (ansi-osc-apply-on-region (point-min) (point-max))
      (let ((pos (point)))
        (insert "title\e\\, and stop.")
        (ansi-osc-apply-on-region pos (point-max)))
      (should
       (equal
        (buffer-substring-no-properties (point-min) (point-max))
        "First set the window title then change it, and stop.")))))

(ert-deftest ansi-osc-tests-filter-region ()
  (pcase-dolist (`(,input ,text) ansi-osc-tests--strings)
    (with-temp-buffer
      (insert input)
      (ansi-osc-filter-region (point-min) (point-max))
      (should (equal (buffer-string) text)))))


(ert-deftest ansi-osc-tests-filter-region-with-multiple-calls ()
  (with-temp-buffer
    (insert
     (concat "First set the window title \e]2;A window title\e\\"
             "then change it\e]2;Another "))
    (ansi-osc-filter-region (point-min) (point-max))
    (let ((pos (point)))
      (insert "title\e\\, and stop.")
      (ansi-osc-filter-region pos (point-max)))
    (should
     (equal
      (buffer-string)
      "First set the window title then change it, and stop."))))
