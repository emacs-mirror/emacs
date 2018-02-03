;;; libxml-parse-tests.el --- Test suite for libxml parsing.

;; Copyright (C) 2014-2018 Free Software Foundation, Inc.

;; Author: Ulf Jasper <ulf.jasper@web.de>
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

;;; Commentary:

;;; Code:

(require 'ert)

(defvar libxml-tests--data-comments-preserved
  `(;; simple case
    ("<?xml version=\"1.0\"?><foo baz=\"true\">bar</foo>"
     . (foo ((baz . "true")) "bar"))
    ;; toplevel comments -- first document child must not get lost
    (,(concat "<?xml version=\"1.0\"?><foo>bar</foo><!--comment-1-->"
	      "<!--comment-2-->")
     . (top nil (foo nil "bar") (comment nil "comment-1")
            (comment nil "comment-2")))
    (,(concat "<?xml version=\"1.0\"?><!--comment-a--><foo a=\"b\">"
	      "<bar>blub</bar></foo><!--comment-b--><!--comment-c-->")
     . (top nil (comment nil "comment-a") (foo ((a . "b")) (bar nil "blub"))
            (comment nil "comment-b") (comment nil "comment-c"))))
  "Alist of XML strings and their expected parse trees for preserved comments.")

(defvar libxml-tests--data-comments-discarded
  `(;; simple case
    ("<?xml version=\"1.0\"?><foo baz=\"true\">bar</foo>"
     . (foo ((baz . "true")) "bar"))
    ;; toplevel comments -- first document child must not get lost
    (,(concat "<?xml version=\"1.0\"?><foo>bar</foo><!--comment-1-->"
	      "<!--comment-2-->")
     . (foo nil "bar"))
    (,(concat "<?xml version=\"1.0\"?><!--comment-a--><foo a=\"b\">"
	      "<bar>blub</bar></foo><!--comment-b--><!--comment-c-->")
     . (foo ((a . "b")) (bar nil "blub"))))
  "Alist of XML strings and their expected parse trees for discarded comments.")


(ert-deftest libxml-tests ()
  "Test libxml."
  (when (fboundp 'libxml-parse-xml-region)
    (with-temp-buffer
      (dolist (test libxml-tests--data-comments-preserved)
        (erase-buffer)
        (insert (car test))
        (should (equal (cdr test)
                       (libxml-parse-xml-region (point-min) (point-max)))))
      (dolist (test libxml-tests--data-comments-discarded)
        (erase-buffer)
        (insert (car test))
        (should (equal (cdr test)
                       (libxml-parse-xml-region (point-min) (point-max) nil t)))))))

;;; libxml-tests.el ends here
