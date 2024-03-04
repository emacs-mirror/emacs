;;; url-util-tests.el --- Test suite for url-util.  -*- lexical-binding:t -*-

;; Copyright (C) 2012-2024 Free Software Foundation, Inc.

;; Author: Teodor Zlatanov <tzz@lifelogs.com>
;; Keywords: data

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
(require 'url-util)

(ert-deftest url-util-tests ()
  (let ((tests
         '(("key1=val1&key2=val2&key3=val1&key3=val2&key4&key5"
            ((key1 val1) (key2 "val2") (key3 val1 val2) (key4) (key5 "")))
           ("key1=val1;key2=val2;key3=val1;key3=val2;key4;key5"
            ((key1 "val1") (key2 val2) (key3 val1 val2) ("key4") (key5 "")) t)
           ("key1=val1;key2=val2;key3=val1;key3=val2;key4=;key5="
            ((key1 val1) (key2 val2) ("key3" val1 val2) (key4) (key5 "")) t t)))
        test)
    (while tests
      (setq test (car tests)
            tests (cdr tests))
      (should (equal (apply 'url-build-query-string (cdr test)) (car test)))))
  (should (equal (url-parse-query-string
                  "key1=val1&key2=val2&key3=val1&key3=val2&key4=&key5")
                 '(("key5" "")
                   ("key4" "")
                   ("key3" "val2" "val1")
                   ("key2" "val2")
                   ("key1" "val1")))))

(ert-deftest url-unhex-string-tests ()
  (should (equal (url-unhex-string "foo%20bar")
                 "foo bar"))
  (should (equal (decode-coding-string (url-unhex-string "Fran%C3%A7ois") 'utf-8)
                 "François"))
  (should (equal (url-unhex-string "%20%21%23%24%25%26%27%28%29%2A")
                 " !#$%&'()*"))
  (should (equal (url-unhex-string "%2B%2C%2F%3A%3B%3D%3F%40%5B%5D")
                 "+,/:;=?@[]")))

(ert-deftest url-hexify-string-tests ()
  (should (equal (url-hexify-string "foo bar")
                 "foo%20bar"))
  (should (equal (url-hexify-string "François")
                 "Fran%C3%A7ois"))
  (should (equal (url-hexify-string " !#$%&'()*")
                 "%20%21%23%24%25%26%27%28%29%2A"))
  (should (equal (url-hexify-string "+,/:;=?@[]")
                 "%2B%2C%2F%3A%3B%3D%3F%40%5B%5D")))

(ert-deftest url-domain-tests ()
  (should (equal (url-domain (url-generic-parse-url "http://www.fsf.co.uk"))
                 "fsf.co.uk"))
  (should (equal (url-domain (url-generic-parse-url "http://fsf.co.uk"))
                 "fsf.co.uk"))
  (should (equal (url-domain (url-generic-parse-url "http://co.uk"))
                 nil))
  (should (equal (url-domain (url-generic-parse-url "http://www.fsf.com"))
                 "fsf.com"))
  (should (equal (url-domain (url-generic-parse-url "http://192.168.0.1"))
                 nil)))

(provide 'url-util-tests)

;;; url-util-tests.el ends here
