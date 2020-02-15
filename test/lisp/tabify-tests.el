;;; tabify-tests.el --- tests for tabify.el  -*- lexical-binding: t -*-

;; Copyright (C) 2019-2020 Free Software Foundation, Inc.

;; Author: Stefan Kangas <stefankangas@gmail.com>

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

;;; Code:

(require 'ert)
(require 'tabify)

(defun tabify-tests--test-changes (fun changes width)
  (with-temp-buffer
    (let ((tab-width width))
      (insert (mapconcat #'car changes ""))
      (funcall fun (point-min) (point-max))
      (should (equal (buffer-string) (mapconcat #'cadr changes ""))))))

(ert-deftest tabify-tests-untabify ()
  (let ((changes '(("***\n"        "***\n")
                   (" ***\n"       " ***\n")
                   ("\t***\n"      "  ***\n")
                   ("\t ***\n"     "   ***\n")
                   ("\t\t***\n"    "    ***\n")
                   ("\t\t ***\n"   "     ***\n")
                   ("\t\t\t***\n"  "      ***\n")
                   ("\t\t\t ***\n" "       ***\n")
                   ("  ***\n"      "  ***\n")
                   (" \t ***\n"    "   ***\n")
                   ("  \t***\n"    "    ***\n")
                   ("   \t***\n"   "    ***\n"))))
    (tabify-tests--test-changes #'untabify changes 2)))

(ert-deftest tabify-tests-tabify ()
  (let ((changes '(("***\n"        "***\n")
                   (" ***\n"       " ***\n")
                   ("  ***\n"      "\t***\n")
                   ("   ***\n"     "\t ***\n")
                   ("    ***\n"    "\t\t***\n")
                   ("     ***\n"   "\t\t ***\n")
                   ("      ***\n"  "\t\t\t***\n")
                   ("       ***\n" "\t\t\t ***\n")
                   ("\t***\n"      "\t***\n")
                   ("\t ***\n"     "\t ***\n")
                   ("\t\t***\n"    "\t\t***\n"))))
    (tabify-tests--test-changes #'tabify changes 2)))

(ert-deftest tabify-tests-tabify/all-spaces-on-line ()
  (with-temp-buffer
    (let ((tab-width 2))
      (insert "  **  **  ")
      (tabify (point-min) (point-max))
      (should (equal (buffer-string) "\t**\t**\t")))))

(ert-deftest tabify-tests-tabify/only-initial ()
  (with-temp-buffer
    (let ((tab-width 2)
          (tabify-regexp "^\\t* [ \\t]+"))   ; from tabify-regexp docstring
      (insert "  **  ")
      (tabify (point-min) (point-max))
      (should (equal (buffer-string) "\t**  ")))))

(provide 'tabify-tests)
;;; tabify-tests.el ends here
