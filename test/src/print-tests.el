;;; print-tests.el --- tests for src/print.c         -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2017 Free Software Foundation, Inc.

;; This file is part of GNU Emacs.

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

;;; Code:

(require 'ert)

(ert-deftest print-hex-backslash ()
  (should (string= (let ((print-escape-multibyte t)
                         (print-escape-newlines t))
                     (prin1-to-string "\u00A2\ff"))
                   "\"\\x00a2\\ff\"")))

(ert-deftest terpri ()
  (should (string= (with-output-to-string
                     (princ 'abc)
                     (should (terpri nil t)))
                   "abc\n"))
  (should (string= (with-output-to-string
                     (should-not (terpri nil t))
                     (princ 'xyz))
                   "xyz"))
  (message nil)
  (if noninteractive
      (progn (should            (terpri nil t))
             (should-not        (terpri nil t))
             (princ 'abc)
             (should            (terpri nil t))
             (should-not        (terpri nil t)))
    (should (string= (progn (should-not (terpri nil t))
                            (princ 'abc)
                            (should (terpri nil t))
                            (current-message))
                     "abc\n")))
  (let ((standard-output
         (with-current-buffer (get-buffer-create "*terpri-test*")
           (insert "--------")
           (point-max-marker))))
    (should     (terpri nil t))
    (should-not (terpri nil t))
    (should (string= (with-current-buffer (marker-buffer standard-output)
                       (buffer-string))
                     "--------\n"))))

(provide 'print-tests)
;;; print-tests.el ends here
