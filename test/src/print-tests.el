;;; print-tests.el --- tests for src/print.c         -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2019 Free Software Foundation, Inc.

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

(defun print-tests--prints-with-charset-p (ch odd-charset)
  "Return t if `prin1-to-string' prints CH with the `charset' property.
CH is propertized with a `charset' value according to
ODD-CHARSET: if nil, then use the one returned by `char-charset',
otherwise, use a different charset."
  (integerp
   (string-match
    "charset"
    (prin1-to-string
     (propertize (string ch)
                 'charset
                 (if odd-charset
                     (cl-find (char-charset ch) charset-list :test-not #'eq)
                   (char-charset ch)))))))

(ert-deftest print-charset-text-property-nil ()
  (let ((print-charset-text-property nil))
    (should-not (print-tests--prints-with-charset-p ?\xf6 t)) ; Bug#31376.
    (should-not (print-tests--prints-with-charset-p ?a t))
    (should-not (print-tests--prints-with-charset-p ?\xf6 nil))
    (should-not (print-tests--prints-with-charset-p ?a nil))))

(ert-deftest print-charset-text-property-default ()
  (let ((print-charset-text-property 'default))
    (should (print-tests--prints-with-charset-p ?\xf6 t))
    (should-not (print-tests--prints-with-charset-p ?a t))
    (should-not (print-tests--prints-with-charset-p ?\xf6 nil))
    (should-not (print-tests--prints-with-charset-p ?a nil))))

(ert-deftest print-charset-text-property-t ()
  (let ((print-charset-text-property t))
    (should (print-tests--prints-with-charset-p ?\xf6 t))
    (should (print-tests--prints-with-charset-p ?a t))
    (should (print-tests--prints-with-charset-p ?\xf6 nil))
    (should (print-tests--prints-with-charset-p ?a nil))))

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

(ert-deftest print-read-roundtrip ()
  (let ((syms (list '## '& '* '+ '- '/ '0E '0e '< '= '> 'E 'E0 'NaN '\"
                    '\# '\#x0 '\' '\'\' '\( '\) '\+00 '\, '\-0 '\. '\.0
                    '\0 '\0.0 '\0E0 '\0e0 '\1E+ '\1E+NaN '\1e+ '\1e+NaN
                    '\; '\? '\[ '\\ '\] '\` '_ 'a 'e 'e0 'x
                    '{ '| '} '~ : '\’ '\’bar
                    (intern "\t") (intern "\n") (intern " ")
                    (intern "\N{NO-BREAK SPACE}")
                    (intern "\N{ZERO WIDTH SPACE}")
                    (intern "\0"))))
    (dolist (sym syms)
      (should (eq (read (prin1-to-string sym)) sym))
      (dolist (sym1 syms)
        (let ((sym2 (intern (concat (symbol-name sym) (symbol-name sym1)))))
          (should (eq (read (prin1-to-string sym2)) sym2)))))))

(ert-deftest print-bignum ()
  (let* ((str "999999999999999999999999999999999")
         (val (read str)))
    (should (> val most-positive-fixnum))
    (should (equal (prin1-to-string val) str))))

(provide 'print-tests)
;;; print-tests.el ends here
