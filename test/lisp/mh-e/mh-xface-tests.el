;;; mh-xface-tests.el --- tests for mh-xface.el -*- lexical-binding: t -*-

;; Copyright (C) 2021-2024 Free Software Foundation, Inc.

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
(require 'mh-xface)

(ert-deftest mh-x-image-url-sane-p ()
  "Test that `mh-x-image-url-sane-p' accepts a URL exactly if it is sane."
  (should (equal (mh-x-image-url-sane-p (concat "http://"
                                                (make-string 101 ?a)))
                 nil))                  ;too long
  (should (equal (mh-x-image-url-sane-p "http") nil)) ;too short
  (should (equal (mh-x-image-url-sane-p "http:") t))
  (should (equal (mh-x-image-url-sane-p "https") nil)) ;too short
  (should (equal (mh-x-image-url-sane-p "https:") t))
  (should (equal (mh-x-image-url-sane-p "https://www.example.com/me.png") t))
  (should (equal (mh-x-image-url-sane-p "abcde:") nil)))

(ert-deftest mh-x-image-url-cache-canonicalize ()
  "Test `mh-x-image-url-cache-canonicalize'."
  (should (equal (format "%s/%s" mh-x-image-cache-directory "%21foo%21bar.png")
                 (mh-x-image-url-cache-canonicalize "/foo/bar")))
  (should (equal (format "%s/%s" mh-x-image-cache-directory
                         "http%3A%21%21domain.com%21foo%21bar.png")
                 (mh-x-image-url-cache-canonicalize
                  "http://domain.com/foo/bar")))
  ;; All Windows invalid characters.
  (should (equal (format "%s/%s" mh-x-image-cache-directory
                         "%21%3C%3E%3A%2A%3F%22%5C%7C%21bar.png")
                 (mh-x-image-url-cache-canonicalize "/<>:*?\"\\|/bar"))))

;;; mh-xface-tests.el ends here
