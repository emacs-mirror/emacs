;;; ediff-ptch-tests.el --- Tests for ediff-ptch.el

;; Copyright (C) 2016-2017 Free Software Foundation, Inc.

;; Author: Tino Calancha <tino.calancha@gmail.com>

;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see `http://www.gnu.org/licenses/'.

;;; Code:

(require 'ert)
(require 'ediff-ptch)

(ert-deftest ibuffer-test-bug25010 ()
  "Test for http://debbugs.gnu.org/25010 ."
  (with-temp-buffer
    (insert "diff --git a/lisp/vc/ediff-ptch.el b/lisp/vc/ediff-ptch.el
index 6a07f80..6e8e947 100644
--- a/lisp/vc/ediff-ptch.el
+++ b/lisp/vc/ediff-ptch.el
@@ -120,11 +120,12 @@ ediff-patch-default-directory
")
    (goto-char (point-min))
    (let ((filename
           (progn
             (re-search-forward ediff-context-diff-label-regexp nil t)
             (match-string 1))))
      (should-not (string-suffix-p "@@" filename)))))

(provide 'ediff-ptch-tests)
;;; ediff-ptch-tests.el ends here
