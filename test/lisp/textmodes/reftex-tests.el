;;; reftex-tests.el --- Test suite for reftex. -*- lexical-binding: t -*-

;; Copyright (C) 2013-2017 Free Software Foundation, Inc.

;; Author: Rüdiger Sonderfeld <ruediger@c-plusplus.de>
;; Keywords:       internal
;; Human-Keywords: internal

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

;;; reftex
(require 'reftex)

;;; reftex-parse
(require 'reftex-parse)

(ert-deftest reftex-locate-bibliography-files ()
  "Test `reftex-locate-bibliography-files'."
  (let ((temp-dir (make-temp-file "reftex-bib" 'dir))
        (files '("ref1.bib" "ref2.bib"))
        (test '(("\\addbibresource{ref1.bib}\n" . ("ref1.bib"))
                ("\\\\addbibresource[label=x]{ref2.bib}\\n" . ("ref2.bib"))
                ("\\begin{document}\n\\bibliographystyle{plain}\n
\\bibliography{ref1,ref2}\n\\end{document}" . ("ref1.bib" "ref2.bib"))))
        (reftex-bibliography-commands
         ;; Default value: See reftex-vars.el `reftex-bibliography-commands'
         '("bibliography" "nobibliography" "setupbibtex\\[.*?database="
           "addbibresource")))
    (with-temp-buffer
      (insert "test\n")
      (mapc
       (lambda (file)
        (write-region (point-min) (point-max) (expand-file-name file
                                                                temp-dir)))
       files))
    (mapc
     (lambda (data)
       (with-temp-buffer
         (insert (car data))
         (let ((res (mapcar #'file-name-nondirectory
                            (reftex-locate-bibliography-files temp-dir))))
           (should (equal res (cdr data))))))
     test)
    (delete-directory temp-dir 'recursive)))

(ert-deftest reftex-what-environment-test ()
  "Test `reftex-what-environment'."
  (with-temp-buffer
    (insert "\\begin{equation}\n  x=y^2\n")
    (let ((pt (point))
          pt2)
      (insert "\\end{equation}\n")
      (goto-char pt)

      (should (equal (reftex-what-environment 1) '("equation" . 1)))
      (should (equal (reftex-what-environment t) '(("equation" . 1))))

      (insert "\\begin{something}\nxxx")
      (setq pt2 (point))
      (insert "\\end{something}")
      (goto-char pt2)
      (should (equal (reftex-what-environment 1) `("something" . ,pt)))
      (should (equal (reftex-what-environment t) `(("something" . ,pt)
                                                   ("equation" . 1))))
      (should (equal (reftex-what-environment t pt) `(("something" . ,pt))))
      (should (equal (reftex-what-environment '("equation"))
                     '("equation" . 1))))))

(ert-deftest reftex-roman-number-test ()
  "Test `reftex-roman-number'."
  (let ((hindu-arabic '(1     2    4   9    14   1050))
        (roman        '("I" "II" "IV" "IX" "XIV" "ML")))
    (while (and hindu-arabic roman)
      (should (string= (reftex-roman-number (car hindu-arabic))
                       (car roman)))
      (pop roman)
      (pop hindu-arabic))))

(ert-deftest reftex-parse-from-file-test ()
  "Test `reftex-parse-from-file'."
  ;; Use file-truename to convert 8+3 aliases in $TEMP value on
  ;; MS-Windows into their long file-name equivalents, which is
  ;; necessary for the 'equal' and 'string=' comparisons below.  This
  ;; also resolves any symlinks, which cannot be bad for the same
  ;; reason.  (An alternative solution would be to use file-equal-p,
  ;; but I'm too lazy to do that, as one of the tests compares a
  ;; list.)
  (let* ((temp-dir (file-truename (make-temp-file "reftex-parse" 'dir)))
         (tex-file (expand-file-name "test.tex" temp-dir))
         (bib-file (expand-file-name "ref.bib" temp-dir)))
    (with-temp-buffer
      (insert
"\\begin{document}
\\section{test}\\label{sec:test}
\\subsection{subtest}

\\begin{align*}\\label{eq:foo}
  x &= y^2
\\end{align*}

\\bibliographystyle{plain}
\\bibliography{ref}
\\end{document}")
      (write-region (point-min) (point-max) tex-file))
    (with-temp-buffer
      (insert "test\n")
      (write-region (point-min) (point-max) bib-file))
    (reftex-ensure-compiled-variables)
    (let ((parsed (reftex-parse-from-file tex-file nil temp-dir)))
      (should (equal (car parsed) `(eof ,tex-file)))
      (pop parsed)
      (while parsed
        (let ((entry (pop parsed)))
         (cond
          ((eq (car entry) 'bib)
           (should (string= (cadr entry) bib-file)))
          ((eq (car entry) 'toc)) ;; ...
          ((string= (car entry) "eq:foo"))
          ((string= (car entry) "sec:test"))
          ((eq (car entry) 'bof)
           (should (string= (cadr entry) tex-file))
           (should (null parsed)))
          (t (should-not t)))))
      (delete-directory temp-dir 'recursive))))

;;; reftex-cite
(require 'reftex-cite)

(ert-deftest reftex-parse-bibtex-entry-test ()
  "Test `reftex-parse-bibtex-entry'."
  (let ((entry "@Book{Stallman12,
  author =    {Richard Stallman\net al.},
  title =        {The Emacs Editor},
  publisher =    {GNU Press},
  year =         2012,
  edition =   {17th},
  note   =      {Updated for Emacs   Version 24.2}
}")
        (check (function
                (lambda (parsed)
                  (should (string= (reftex-get-bib-field "&key" parsed)
                                   "Stallman12"))
                  (should (string= (reftex-get-bib-field "&type" parsed)
                                   "book"))
                  (should (string= (reftex-get-bib-field "author" parsed)
                                   "Richard Stallman et al."))
                  (should (string= (reftex-get-bib-field "title" parsed)
                                   "The Emacs Editor"))
                  (should (string= (reftex-get-bib-field "publisher" parsed)
                                   "GNU Press"))
                  (should (string= (reftex-get-bib-field "year" parsed)
                                   "2012"))
                  (should (string= (reftex-get-bib-field "edition" parsed)
                                   "17th"))
                  (should (string= (reftex-get-bib-field "note" parsed)
                                   "Updated for Emacs Version 24.2"))))))
    (funcall check (reftex-parse-bibtex-entry entry))
    (with-temp-buffer
      (insert entry)
      (funcall check (reftex-parse-bibtex-entry nil (point-min)
                                                (point-max))))))

(ert-deftest reftex-get-bib-names-test ()
  "Test `reftex-get-bib-names'."
  (let ((entry (reftex-parse-bibtex-entry "@article{Foo123,
   author =   {Jane Roe and\tJohn Doe  and   W. Public},
}")))
    (should (equal (reftex-get-bib-names "author" entry)
                   '("Jane Roe" "John Doe" "Public"))))
  (let ((entry (reftex-parse-bibtex-entry "@article{Foo123,
   editor =   {Jane Roe and\tJohn Doe  and   W. Public},
}")))
    (should (equal (reftex-get-bib-names "author" entry)
                   '("Jane Roe" "John Doe" "Public")))))

(ert-deftest reftex-format-citation-test ()
  "Test `reftex-format-citation'."
  (let ((entry (reftex-parse-bibtex-entry
"@article{Foo13,
  author =    {Jane Roe and John Doe and Jane Q. Taxpayer},
  title =        {Some Article},
  journal =    {Some Journal},
  year =         2013,
  pages = {1--333}
}")))
    (should (string= (reftex-format-citation entry nil) "\\cite{Foo13}"))
    (should (string= (reftex-format-citation entry "%l:%A:%y:%t %j %P %a")
                     "Foo13:Jane Roe:2013:Some Article Some Journal 1 Jane Roe, John Doe \\& Jane Taxpayer"))))


;;; Autoload tests

;; Test to check whether reftex autoloading mechanisms are working
;; correctly.
(ert-deftest reftex-autoload-auc ()
  "Tests to see whether reftex-auc has been autoloaded"
  (should
   (fboundp 'reftex-arg-label))
  (should
   (autoloadp
    (symbol-function
     'reftex-arg-label))))


(provide 'reftex-tests)
;;; reftex-tests.el ends here.
