;;; json-tests.el --- Test suite for json.el

;; Copyright (C) 2015 Free Software Foundation, Inc.

;; Author: Dmitry Gutov <dgutov@yandex.ru>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(require 'ert)
(require 'json)

(ert-deftest json-encode-simple-alist ()
  (should (equal (json-encode '((a . 1)
                                (b . 2)))
                 "{\"a\":1,\"b\":2}")))

(ert-deftest json-read-simple-alist ()
  (should (equal (json-read-from-string "{\"a\": 1, \"b\": 2}")
                 '((b . 2)
                   (a . 1)))))

(ert-deftest json-encode-string-with-special-chars ()
  (should (equal (json-encode-string "a\n\fb")
                 "\"a\\n\\fb\""))
  (should (equal (json-encode-string "\nasdфыв\u001f\u007ffgh\t")
                 "\"\\nasdфыв\\u001f\u007ffgh\\t\"")))

(ert-deftest json-read-string-with-special-chars ()
  (should (equal (json-read-from-string "\"\\nasd\\u0444\\u044b\\u0432fgh\\t\"")
                 "\nasdфывfgh\t")))

(provide 'json-tests)
;;; json-tests.el ends here
