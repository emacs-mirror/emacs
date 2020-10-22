;;; browse-url-tests.el --- Tests for browse-url.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Free Software Foundation, Inc.

;; Author: Simen Heggestøyl <simenheg@gmail.com>
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

(require 'browse-url)
(require 'ert)

(ert-deftest browse-url-tests-browser-kind ()
  (should (eq (browse-url--browser-kind #'browse-url-w3 "gnu.org")
              'internal))
  (should
   (eq (browse-url--browser-kind #'browse-url-firefox "gnu.org")
       'external)))

(ert-deftest browse-url-tests-non-html-file-url-p ()
  (should (browse-url--non-html-file-url-p "file://foo.txt"))
  (should-not (browse-url--non-html-file-url-p "file://foo.html")))

(ert-deftest browse-url-tests-select-handler-mailto ()
  (should (eq (browse-url-select-handler "mailto:foo@bar.org")
              'browse-url--mailto))
  (should (eq (browse-url-select-handler "mailto:foo@bar.org"
                                         'internal)
              'browse-url--mailto))
  (should-not (browse-url-select-handler "mailto:foo@bar.org"
                                         'external)))

(ert-deftest browse-url-tests-select-handler-man ()
  (should (eq (browse-url-select-handler "man:ls") 'browse-url--man))
  (should (eq (browse-url-select-handler "man:ls" 'internal)
              'browse-url--man))
  (should-not (browse-url-select-handler "man:ls" 'external)))

(ert-deftest browse-url-tests-select-handler-file ()
  (should (eq (browse-url-select-handler "file://foo.txt")
              'browse-url-emacs))
  (should (eq (browse-url-select-handler "file://foo.txt" 'internal)
              'browse-url-emacs))
  (should-not (browse-url-select-handler "file://foo.txt" 'external)))

(ert-deftest browse-url-tests-url-encode-chars ()
  (should (equal (browse-url-url-encode-chars "foobar" "[ob]")
                 "f%6F%6F%62ar")))

(ert-deftest browse-url-tests-encode-url ()
  (should (equal (browse-url-encode-url "") ""))
  (should (equal (browse-url-encode-url "a b c") "a b c"))
  (should (equal (browse-url-encode-url "\"a\" \"b\"")
                 "\"a%22\"b\""))
  (should (equal (browse-url-encode-url "(a) (b)") "(a%29(b)"))
  (should (equal (browse-url-encode-url "a$ b$") "a%24b$")))

(ert-deftest browse-url-tests-url-at-point ()
  (with-temp-buffer
    (insert "gnu.org")
    (should (equal (browse-url-url-at-point) "http://gnu.org"))))

(ert-deftest browse-url-tests-file-url ()
  (should (equal (browse-url-file-url "/foo") "file:///foo"))
  (should (equal (browse-url-file-url "/foo:") "ftp://foo/"))
  (should (equal (browse-url-file-url "/ftp@foo:") "ftp://foo/"))
  (should (equal (browse-url-file-url "/anonymous@foo:")
                 "ftp://foo/")))

(ert-deftest browse-url-tests-delete-temp-file ()
  (let ((browse-url-temp-file-name
         (make-temp-file "browse-url-tests-")))
    (browse-url-delete-temp-file)
    (should-not (file-exists-p browse-url-temp-file-name)))
  (let ((file (make-temp-file "browse-url-tests-")))
    (browse-url-delete-temp-file file)
    (should-not (file-exists-p file))))

(ert-deftest browse-url-tests-add-buttons ()
  (with-temp-buffer
    (insert "Visit https://gnu.org")
    (goto-char (point-min))
    (browse-url-add-buttons)
    (goto-char (- (point-max) 1))
    (should (eq (get-text-property (point) 'face)
                'browse-url-button))
    (should (get-text-property (point) 'browse-url-data))))

(ert-deftest browse-url-tests-button-copy ()
  (with-temp-buffer
    (insert "Visit https://gnu.org")
    (goto-char (point-min))
    (browse-url-add-buttons)
    (should-error (browse-url-button-copy))
    (goto-char (- (point-max) 1))
    (browse-url-button-copy)
    (should (equal (car kill-ring) "https://gnu.org"))))

(provide 'browse-url-tests)
;;; browse-url-tests.el ends here
