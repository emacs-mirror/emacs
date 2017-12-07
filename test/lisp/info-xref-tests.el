;;; info-xref.el --- tests for info-xref.el

;; Copyright (C) 2013-2017 Free Software Foundation, Inc.

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
(require 'info-xref)

(defun info-xref-test-internal (body result)
  "Body of a basic info-xref ert test.
BODY is a string from an info buffer.
RESULT is a list (NBAD NGOOD NUNAVAIL)."
  (get-buffer-create info-xref-output-buffer)
  (setq info-xref-xfile-alist nil)
  (require 'info)
  (let ((Info-directory-list '("."))
        Info-additional-directory-list)
    (info-xref-with-output
     (with-temp-buffer
       (insert body)
       (info-xref-check-buffer))))
  (should (equal result (list info-xref-bad info-xref-good info-xref-unavail)))
  ;; If there was an error, we can leave this around.
  (kill-buffer info-xref-output-buffer))

(ert-deftest info-xref-test-node-crossref ()
  "Test parsing of @xref{node,crossref,,manual} with Texinfo 4/5."
  (info-xref-test-internal "
*Note crossref: (manual-foo)node.  Texinfo 4/5 format with crossref.
" '(0 0 1)))

(ert-deftest info-xref-test-node-4 ()
  "Test parsing of @xref{node,,,manual} with Texinfo 4."
  (info-xref-test-internal "
*Note node: (manual-foo)node.  Texinfo 4 format with no crossref.
" '(0 0 1)))

(ert-deftest info-xref-test-node-5 ()
  "Test parsing of @xref{node,,,manual} with Texinfo 5."
  (info-xref-test-internal "
*Note (manual-foo)node::.  Texinfo 5 format with no crossref.
" '(0 0 1)))

;; TODO Easier to have static data files in the repo?
(defun info-xref-test-write-file (file body)
  "Write BODY to texi FILE."
  (with-temp-buffer
    (insert "\
\\input texinfo
@setfilename "
            (format "%s.info\n" (file-name-sans-extension file))
            "\
@settitle test

@ifnottex
@node Top
@top test
@end ifnottex

@menu
* Chapter One::
@end menu

@node Chapter One
@chapter Chapter One

text.

"
            body
            "\
@bye
"
            )
    (write-region nil nil file nil 'silent))
  (should (equal 0 (call-process "makeinfo" file))))

(ert-deftest info-xref-test-makeinfo ()
  "Test that info-xref can parse basic makeinfo output."
  (skip-unless (executable-find "makeinfo"))
  (let ((tempfile (make-temp-file "info-xref-test" nil ".texi"))
        (tempfile2 (make-temp-file "info-xref-test2" nil ".texi"))
        (errflag t))
    (unwind-protect
        (progn
          ;; tempfile contains xrefs to various things, including tempfile2.
          (info-xref-test-write-file
           tempfile
           (concat "\
@xref{nodename,,,missing,Missing Manual}.

@xref{nodename,crossref,title,missing,Missing Manual}.

@xref{Chapter One}.

@xref{Chapter One,Something}.

"
                   (format "@xref{Chapter One,,,%s,Present Manual}.\n"
                           (file-name-sans-extension (file-name-nondirectory
                                                      tempfile2)))))
          ;; Something for tempfile to xref to.
          (info-xref-test-write-file tempfile2 "")
          (require 'info)
          (save-window-excursion
            (let ((Info-directory-list
                   (list
                    (or (file-name-directory tempfile) ".")))
                  Info-additional-directory-list)
              (info-xref-check (format "%s.info" (file-name-sans-extension
                                                  tempfile))))
            (should (equal (list info-xref-bad info-xref-good
                                 info-xref-unavail)
                           '(0 1 2)))
            (setq errflag nil)
            ;; If there was an error, we can leave this around.
            (kill-buffer info-xref-output-buffer)))
      ;; Useful diagnostic in case of problems.
      (if errflag
          (with-temp-buffer
            (call-process "makeinfo" nil t nil "--version")
            (message "%s" (buffer-string))))
      (mapc 'delete-file (list tempfile tempfile2
                               (format "%s.info" (file-name-sans-extension
                                                  tempfile))
                               (format "%s.info" (file-name-sans-extension
                                                  tempfile2)))))))

;;; info-xref.el ends here
