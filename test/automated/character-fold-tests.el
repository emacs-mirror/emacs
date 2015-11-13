;;; character-fold-tests.el --- Tests for character-fold.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2013-2015 Free Software Foundation, Inc.

;; Author: Artur Malabarba <bruce.connor.am@gmail.com>

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
(require 'character-fold)

(defun character-fold--random-word (n)
  (mapconcat (lambda (_) (string (+ 9 (random 117))))
             (make-list n nil) ""))

(defun character-fold--test-search-with-contents (contents string)
  (with-temp-buffer
    (insert contents)
    (goto-char (point-min))
    (should (search-forward-regexp (character-fold-to-regexp string) nil 'noerror))
    (goto-char (point-min))
    (should (character-fold-search-forward string nil 'noerror))
    (should (character-fold-search-backward string nil 'noerror))))


(ert-deftest character-fold--test-consistency ()
  (dotimes (n 100)
    (let ((w (character-fold--random-word n)))
      ;; A folded string should always match the original string.
      (character-fold--test-search-with-contents w w))))

(ert-deftest character-fold--test-lax-whitespace ()
  (dotimes (n 100)
    (let ((w1 (character-fold--random-word n))
          (w2 (character-fold--random-word n))
          (search-spaces-regexp "\\s-+"))
      (character-fold--test-search-with-contents
       (concat w1 "\s\n\s\t\f\t\n\r\t" w2)
       (concat w1 " " w2))
      (character-fold--test-search-with-contents
       (concat w1 "\s\n\s\t\f\t\n\r\t" w2)
       (concat w1 (make-string 90 ?\s) w2)))))

(provide 'character-fold-tests)
;;; character-fold-tests.el ends here
