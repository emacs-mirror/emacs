;;; ffap-tests.el --- Test suite for ffap.el -*- lexical-binding: t -*-

;; Copyright (C) 2016-2018 Free Software Foundation, Inc.

;; Author: Tino Calancha <tino.calancha@gmail.com>

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

(require 'cl-lib)
(require 'ert)
(require 'ffap)

(ert-deftest ffap-tests-25243 ()
  "Test for https://debbugs.gnu.org/25243 ."
  (let ((file (make-temp-file "test-Bug#25243")))
    (unwind-protect
        (with-temp-file file
          (let ((str "diff --git b/lisp/ffap.el a/lisp/ffap.el
index 3d7cebadcf..ad4b70d737 100644
--- b/lisp/ffap.el
+++ a/lisp/ffap.el
@@ -203,6 +203,9 @@ ffap-foo-at-bar-prefix
"))
            (transient-mark-mode 1)
            (when (natnump ffap-max-region-length)
              (insert
               (concat
                str
                (make-string ffap-max-region-length #xa)
                (format "%s ENDS HERE" file)))
              (call-interactively 'mark-whole-buffer)
              (should (equal "" (ffap-string-at-point)))
              (should (equal '(1 1) ffap-string-at-point-region)))))
      (and (file-exists-p file) (delete-file file)))))

(ert-deftest ffap-gopher-at-point ()
  (with-temp-buffer
    (insert "\
Type = 1
Name = foo
Path = /the/path
Port = 7070
Host = example.com\n")
    (should-not (ffap-gopher-at-point))
    (goto-char (point-min))
    (should (equal (ffap-gopher-at-point)
                   "gopher://example.com:7070/1/the/path"))
    (should (equal ffap-string-at-point-region
                   (list (point-min) (point-max))))
    (let ((ffap-gopher-regexp nil))
      (should-not (ffap-gopher-at-point)))))

(ert-deftest ffap-other-window--bug-25352 ()
  "Test for Bug#25352.  Checks that the window configuration is
left alone when opening a URL in an external browser."
  (cl-letf* ((old (current-window-configuration))
             ((symbol-function 'ffap-prompter)
              (lambda () "https://www.gnu.org"))
             (urls nil)
             (ffap-url-fetcher (lambda (url) (push url urls) nil)))
    (should-not (ffap-other-window))
    (should (equal (current-window-configuration) old))
    (should (equal urls '("https://www.gnu.org")))))

(provide 'ffap-tests)

;;; ffap-tests.el ends here
