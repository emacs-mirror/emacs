;;; char-fold-tests.el --- Tests for char-fold.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2013-2017 Free Software Foundation, Inc.

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
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Code:

(require 'ert)
(require 'char-fold)

(defun char-fold--random-word (n)
  (mapconcat (lambda (_) (string (+ 9 (random 117))))
             (make-list n nil) ""))

(defun char-fold--test-search-with-contents (contents string)
  (with-temp-buffer
    (insert contents)
    (goto-char (point-min))
    (should (search-forward-regexp (char-fold-to-regexp string) nil 'noerror))
    (goto-char (point-min))
    (should (char-fold-search-forward string nil 'noerror))
    (should (char-fold-search-backward string nil 'noerror))))


(ert-deftest char-fold--test-consistency ()
  (dotimes (n 30)
    (let ((w (char-fold--random-word n)))
      ;; A folded string should always match the original string.
      (char-fold--test-search-with-contents w w))))

(ert-deftest char-fold--test-lax-whitespace ()
  (dotimes (n 40)
    (let ((w1 (char-fold--random-word n))
          (w2 (char-fold--random-word n))
          (search-spaces-regexp "\\s-+"))
      (char-fold--test-search-with-contents
       (concat w1 "\s\n\s\t\f\t\n\r\t" w2)
       (concat w1 " " w2))
      (char-fold--test-search-with-contents
       (concat w1 "\s\n\s\t\f\t\n\r\t" w2)
       (concat w1 (make-string 10 ?\s) w2)))))

(defun char-fold--ascii-upcase (string)
  "Like `upcase' but acts on ASCII characters only."
  (replace-regexp-in-string "[a-z]+" 'upcase string))

(defun char-fold--ascii-downcase (string)
  "Like `downcase' but acts on ASCII characters only."
  (replace-regexp-in-string "[a-z]+" 'downcase string))

(defun char-fold--test-match-exactly (string &rest strings-to-match)
  (let ((re (concat "\\`" (char-fold-to-regexp string) "\\'")))
    (dolist (it strings-to-match)
      (should (string-match re it)))
    ;; Case folding
    (let ((case-fold-search t))
      (dolist (it strings-to-match)
        (should (string-match (char-fold--ascii-upcase re) (downcase it)))
        (should (string-match (char-fold--ascii-downcase re) (upcase it)))))))

(ert-deftest char-fold--test-some-defaults ()
  (dolist (it '(("ffl" . "ﬄ") ("ffi" . "ﬃ")
                ("fi" . "ﬁ") ("ff" . "ﬀ")
                ("ä" . "ä")))
    (char-fold--test-search-with-contents (cdr it) (car it))
    (let ((multi (char-table-extra-slot char-fold-table 0))
          (char-fold-table (make-char-table 'char-fold-table)))
      (set-char-table-extra-slot char-fold-table 0 multi)
      (char-fold--test-match-exactly (car it) (cdr it)))))

(ert-deftest char-fold--test-fold-to-regexp ()
  (let ((char-fold-table (make-char-table 'char-fold-table))
        (multi  (make-char-table 'char-fold-table)))
    (set-char-table-extra-slot char-fold-table 0 multi)
    (aset char-fold-table ?a "xx")
    (aset char-fold-table ?1 "44")
    (aset char-fold-table ?\s "-!-")
    (char-fold--test-match-exactly "a1a1" "xx44xx44")
    (char-fold--test-match-exactly "a1  a 1" "xx44-!--!-xx-!-44")
    (aset multi ?a '(("1" . "99")
                     ("2" . "88")
                     ("12" . "77")))
    (char-fold--test-match-exactly "a" "xx")
    (char-fold--test-match-exactly "a1" "xx44" "99")
    (char-fold--test-match-exactly "a12" "77" "xx442" "992")
    (char-fold--test-match-exactly "a2" "88")
    (aset multi ?1 '(("2" . "yy")))
    (char-fold--test-match-exactly "a1" "xx44" "99")
    (char-fold--test-match-exactly "a12" "77" "xx442" "992")
    ;; Support for this case is disabled.  See function definition or:
    ;; https://lists.gnu.org/r/emacs-devel/2015-11/msg02562.html
    ;; (char-fold--test-match-exactly "a12" "xxyy")
    ))

(ert-deftest char-fold--speed-test ()
  (dolist (string (append '("tty-set-up-initial-frame-face"
                            "tty-set-up-initial-frame-face-frame-faceframe-faceframe-faceframe-face")
                          (mapcar #'char-fold--random-word '(10 50 100
                                                                     50 100))))
    (message "Testing %s" string)
    ;; Make sure we didn't just fallback on the trivial search.
    (should-not (string= (regexp-quote string)
                         (char-fold-to-regexp string)))
    (with-temp-buffer
      (save-excursion (insert string))
      (let ((time (time-to-seconds)))
        ;; Our initial implementation of case-folding in char-folding
        ;; created a lot of redundant paths in the regexp. Because of
        ;; that, if a really long string "almost" matches, the regexp
        ;; engine took a long time to realize that it doesn't match.
        (should-not (char-fold-search-forward (concat string "c") nil 'noerror))
        ;; Ensure it took less than a second.
        (should (< (- (time-to-seconds) time) 1))))))

(provide 'char-fold-tests)
;;; char-fold-tests.el ends here
