;;; shr-tests.el --- tests for shr.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2016-2024 Free Software Foundation, Inc.

;; Author: Lars Ingebrigtsen <larsi@gnus.org>

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
(require 'ert-x)
(require 'shr)

(declare-function libxml-parse-html-region "xml.c")

(defun shr-test (name)
  (with-temp-buffer
    (insert-file-contents (format (concat (ert-resource-directory) "/%s.html") name))
    (let ((dom (libxml-parse-html-region (point-min) (point-max)))
          (shr-width 80)
          (shr-use-fonts nil))
      (erase-buffer)
      (shr-insert-document dom)
      (cons (buffer-substring-no-properties (point-min) (point-max))
            (with-temp-buffer
              (insert-file-contents
               (format (concat (ert-resource-directory) "/%s.txt") name))
              (while (re-search-forward "%\\([0-9A-F][0-9A-F]\\)" nil t)
                (replace-match (string (string-to-number (match-string 1) 16))
                               t t))
              (buffer-string))))))

(ert-deftest rendering ()
  (skip-unless (fboundp 'libxml-parse-html-region))
  (dolist (file (directory-files (ert-resource-directory) nil "\\.html\\'"))
    (let* ((name (replace-regexp-in-string "\\.html\\'" "" file))
           (result (shr-test name)))
      (unless (equal (car result) (cdr result))
        (should (not (list name (car result) (cdr result))))))))

(ert-deftest use-cookies ()
  (let ((shr-cookie-policy 'same-origin))
    (should
     (shr--use-cookies-p "http://images.fsf.org" '("http://www.fsf.org")))
    (should
     (shr--use-cookies-p "http://www.fsf.org" '("https://www.fsf.org")))
    (should
     (shr--use-cookies-p "http://www.fsf.org" '("https://www.fsf.org")))
    (should
     (shr--use-cookies-p "http://www.fsf.org" '("http://fsf.org")))
    (should-not
     (shr--use-cookies-p "http://www.gnu.org" '("http://www.fsf.org")))))

(ert-deftest shr-srcset ()
  (should (equal (shr--parse-srcset "") nil))

  (should (equal (shr--parse-srcset "a 10w, b 20w")
                 '(("b" 20) ("a" 10))))

  (should (equal (shr--parse-srcset "a 10w b 20w")
                 '(("a" 10))))

  (should (equal (shr--parse-srcset "https://example.org/1\n\n 10w , https://example.org/2 20w      ")
	         '(("https://example.org/2" 20) ("https://example.org/1" 10))))

  (should (equal (shr--parse-srcset "https://example.org/1,2\n\n 10w , https://example.org/2 20w      ")
	         '(("https://example.org/2" 20) ("https://example.org/1,2" 10)))))

(require 'shr)

;;; shr-tests.el ends here
