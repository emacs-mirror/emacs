;;; url-handlers-test.el --- Test suite for url-handlers.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2018-2020 Free Software Foundation, Inc.

;; Author: Nicolas Petton <nicolas@petton.fr>

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

;;; Commentary:

;;; Code:

(require 'ert)
(require 'url-handlers)

(defmacro with-url-handler-mode (&rest body)
  "Evaluate BODY with `url-handler-mode' turned on."
  (declare (indent 0) (debug t))
  (let ((url-handler-mode-active (make-symbol "url-handler-mode-active")))
    `(let ((,url-handler-mode-active url-handler-mode))
       (unwind-protect
           (progn
             (unless ,url-handler-mode-active
               (url-handler-mode))
             ,@body)
         (unless ,url-handler-mode-active
           (url-handler-mode -1))))))

(ert-deftest url-handlers-file-name-directory/preserve-url-types ()
  (with-url-handler-mode
    (should (equal (file-name-directory "https://gnu.org/index.html")
                   "https://gnu.org/"))
    (should (equal (file-name-directory "http://gnu.org/index.html")
                   "http://gnu.org/"))
    (should (equal (file-name-directory "ftp://gnu.org/index.html")
                   "ftp://gnu.org/"))))

(ert-deftest url-handlers-file-name-directory/should-not-handle-non-url-file-names ()
  (with-url-handler-mode
    (should-not (equal (file-name-directory "not-uri://gnu.org")
                   "not-uri://gnu.org/"))))

(ert-deftest url-handlers-file-name-directory/sub-directories ()
  (with-url-handler-mode
    (should (equal (file-name-directory "https://foo/bar/baz/index.html")
                   "https://foo/bar/baz/"))))

(ert-deftest url-handlers-file-name-directory/file-urls ()
  (with-url-handler-mode
    (should (equal (file-name-directory "file:///foo/bar/baz.txt")
                   "file:///foo/bar/"))
    (should (equal (file-name-directory "file:///")
                   "file:///"))))

;; Regression test for bug#30444
(ert-deftest url-handlers-file-name-directory/no-filename ()
  (with-url-handler-mode
    (should (equal (file-name-directory "https://foo.org")
                   "https://foo.org/"))
    (should (equal (file-name-directory "https://foo.org/")
                   "https://foo.org/"))))

(provide 'url-handlers-test)
;;; url-handlers-test.el ends here
