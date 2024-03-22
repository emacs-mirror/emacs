;;; pp-tests.el --- Test suite for pretty printer.  -*- lexical-binding: t -*-

;; Copyright (C) 2017-2024 Free Software Foundation, Inc.

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

(require 'pp)
(require 'ert-x)

(ert-deftest pp-print-quote ()
  (should (string= (pp-to-string 'quote) "quote\n"))
  (should (string= (pp-to-string ''quote) "'quote\n"))
  (should (string= (pp-to-string '('a 'b)) "('a 'b)\n"))
  (should (string= (pp-to-string '(''quote 'quote)) "(''quote 'quote)\n"))
  (should (string= (pp-to-string '(quote)) "(quote)\n"))
  (should (string= (pp-to-string '(quote . quote)) "(quote . quote)\n"))
  (should (string= (pp-to-string '(quote a b)) "(quote a b)\n"))
  (should (string= (pp-to-string '(quotefoo)) "(quotefoo)\n"))
  (should (string= (pp-to-string '(a b)) "(a b)\n")))

(ert-deftest test-indentation ()
  (ert-test-erts-file (ert-resource-file "code-formats.erts")))

(defun pp-tests--dimensions ()
  (save-excursion
    (let ((width 0)
          (height 0))
      (goto-char (point-min))
      (while (not (eobp))
        (end-of-line)
        (setq height (1+ height))
        (setq width (max width (current-column)))
        (forward-char 1))
      (cons width height))))

(ert-deftest pp-tests--cut-before ()
  (with-temp-buffer
    (lisp-data-mode)
    (pp '(1 (quite-a-long-package-name
             . [(0 10 0) ((avy (0 5 0))) "Quickly switch windows." tar
                ((:url . "https://github.com/abo-abo/ace-window")
                 (:maintainer "Oleh Krehel" . "ohwoeowho@gmail.com")
                 (:authors ("Oleh Krehel" . "ohwoeowho@gmail.com"))
                 (:keywords "window" "location"))]))
        (current-buffer))
    ;; (message "Filled:\n%s" (buffer-string))
    (let ((dimensions (pp-tests--dimensions)))
      (should (< (car dimensions) 80))
      (should (< (cdr dimensions) 8)))
    (goto-char (point-min))
    (while (search-forward "." nil t)
      (should (not (eolp))))))

;;; pp-tests.el ends here.
