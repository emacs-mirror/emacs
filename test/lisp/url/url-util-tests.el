;;; url-util-tests.el --- Test suite for url-util.

;; Copyright (C) 2012-2017 Free Software Foundation, Inc.

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

(provide 'url-util-tests)

;;; url-util-tests.el ends here
