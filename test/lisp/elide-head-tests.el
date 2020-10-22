;;; elide-head-tests.el --- Tests for elide-head.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Free Software Foundation, Inc.

;; Author: Simen Heggestøyl <simenheg@gmail.com>
;; Keywords:

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

;;

;;; Code:

(require 'elide-head)
(require 'ert)

(ert-deftest elide-head-tests-elide-head ()
  (let ((elide-head-headers-to-hide '(("START" . "END"))))
    (with-temp-buffer
      (insert "foo\nSTART\nHIDDEN\nEND\nbar")
      (elide-head)
      (let ((o (car (overlays-at 14))))
        (should (= (overlay-start o) 10))
        (should (= (overlay-end o) 21))
        (should (overlay-get o 'invisible))
        (should (overlay-get o 'evaporate))))))

(ert-deftest elide-head-tests-elide-head-with-prefix-arg ()
  (let ((elide-head-headers-to-hide '(("START" . "END"))))
    (with-temp-buffer
      (insert "foo\nSTART\nHIDDEN\nEND\nbar")
      (elide-head)
      (should (overlays-at 14))
      (elide-head t)
      (should-not (overlays-at 14)))))

(ert-deftest elide-head-tests-show ()
  (let ((elide-head-headers-to-hide '(("START" . "END"))))
    (with-temp-buffer
      (insert "foo\nSTART\nHIDDEN\nEND\nbar")
      (elide-head)
      (should (overlays-at 14))
      (elide-head-show)
      (should-not (overlays-at 14)))))

(provide 'elide-head-tests)
;;; elide-head-tests.el ends here
