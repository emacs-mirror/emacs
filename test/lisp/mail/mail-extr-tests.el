;;; mail-extr-tests.el --- Tests for mail-extr.el  -*- lexical-binding: t -*-

;; Copyright (C) 2022-2026 Free Software Foundation, Inc.

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

;;; Code:

(require 'ert)
(require 'mail-extr)

(defconst mail-extract-test-cases
  '(("foo@example.org" . (nil "foo@example.org"))
    ("J. Random Hacker <foo@example.org>" . ("J. Random Hacker" "foo@example.org"))
    ("\"J. Random Hacker\" <foo@example.org>" . ("J. Random Hacker" "foo@example.org"))
    ("Ååå Äää <foo@example.org>" . ("Ååå Äää" "foo@example.org"))))

(ert-deftest mail-extract-address-components ()
  (dolist (test mail-extract-test-cases)
    (should (equal (mail-extract-address-components (car test)) (cdr test)))))

(ert-deftest what-domain ()
  (should (equal (what-domain "cu") "CU: Cuba")))

(provide 'mail-extr-tests)
;;; mail-extr-tests.el ends here
