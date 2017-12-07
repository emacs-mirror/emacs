;;; hi-lock-tests.el --- Tests for hi-lock.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Free Software Foundation, Inc.

;; Author: Tino Calancha <tino.calancha@gmail.com>
;; Keywords:

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

;;; Code:

(require 'ert)
(require 'hi-lock)

(ert-deftest hi-lock-bug26666 ()
  "Test for https://debbugs.gnu.org/26666 ."
  (let ((faces hi-lock-face-defaults))
    (with-temp-buffer
      (insert "a A b B\n")
      (cl-letf (((symbol-function 'completing-read)
                   (lambda (prompt coll x y z hist defaults)
                     (car defaults))))
        (dotimes (_ 2)
          (let ((face (hi-lock-read-face-name)))
            (hi-lock-set-pattern "a" face))))
      (should (equal hi-lock--unused-faces (cdr faces))))))

(ert-deftest hi-lock-test-set-pattern ()
  (let ((faces hi-lock-face-defaults))
    (with-temp-buffer
      (insert "foo bar")
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (prompt coll x y z hist defaults)
                   (car defaults))))
        (hi-lock-set-pattern "9999" (hi-lock-read-face-name)) ; No match
        (hi-lock-set-pattern "foo" (hi-lock-read-face-name)))
      ;; Only one match, then we have used just 1 face
      (should (equal hi-lock--unused-faces (cdr faces))))))

(provide 'hi-lock-tests)
;;; hi-lock-tests.el ends here
