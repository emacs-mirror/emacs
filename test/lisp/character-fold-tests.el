;;; character-fold-tests.el --- Tests for character-fold.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2013-2016 Free Software Foundation, Inc.

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
  (dotimes (n 30)
    (let ((w (character-fold--random-word n)))
      ;; A folded string should always match the original string.
      (character-fold--test-search-with-contents w w))))

(ert-deftest character-fold--test-lax-whitespace ()
  (dotimes (n 40)
    (let ((w1 (character-fold--random-word n))
          (w2 (character-fold--random-word n))
          (search-spaces-regexp "\\s-+"))
      (character-fold--test-search-with-contents
       (concat w1 "\s\n\s\t\f\t\n\r\t" w2)
       (concat w1 " " w2))
      (character-fold--test-search-with-contents
       (concat w1 "\s\n\s\t\f\t\n\r\t" w2)
       (concat w1 (make-string 10 ?\s) w2)))))

(defun character-fold--test-match-exactly (string &rest strings-to-match)
  (let ((re (concat "\\`" (character-fold-to-regexp string) "\\'")))
    (dolist (it strings-to-match)
      (should (string-match re it)))
    ;; Case folding
    (let ((case-fold-search t))
      (dolist (it strings-to-match)
        (should (string-match (upcase re) (downcase it)))
        (should (string-match (downcase re) (upcase it)))))))

(ert-deftest character-fold--test-some-defaults ()
  (dolist (it '(("ffl" . "ﬄ") ("ffi" . "ﬃ")
                ("fi" . "ﬁ") ("ff" . "ﬀ")
                ("ä" . "ä")))
    (character-fold--test-search-with-contents (cdr it) (car it))
    (let ((multi (char-table-extra-slot character-fold-table 0))
          (character-fold-table (make-char-table 'character-fold-table)))
      (set-char-table-extra-slot character-fold-table 0 multi)
      (character-fold--test-match-exactly (car it) (cdr it)))))

(ert-deftest character-fold--test-fold-to-regexp ()
  (let ((character-fold-table (make-char-table 'character-fold-table))
        (multi  (make-char-table 'character-fold-table)))
    (set-char-table-extra-slot character-fold-table 0 multi)
    (aset character-fold-table ?a "xx")
    (aset character-fold-table ?1 "44")
    (aset character-fold-table ?\s "-!-")
    (character-fold--test-match-exactly "a1a1" "xx44xx44")
    (character-fold--test-match-exactly "a1  a 1" "xx44-!--!-xx-!-44")
    (aset multi ?a '(("1" . "99")
                     ("2" . "88")
                     ("12" . "77")))
    (character-fold--test-match-exactly "a" "xx")
    (character-fold--test-match-exactly "a1" "xx44" "99")
    (character-fold--test-match-exactly "a12" "77" "xx442" "992")
    (character-fold--test-match-exactly "a2" "88")
    (aset multi ?1 '(("2" . "yy")))
    (character-fold--test-match-exactly "a1" "xx44" "99")
    (character-fold--test-match-exactly "a12" "77" "xx442" "992")
    ;; Support for this case is disabled.  See function definition or:
    ;; https://lists.gnu.org/archive/html/emacs-devel/2015-11/msg02562.html
    ;; (character-fold--test-match-exactly "a12" "xxyy")
    ))

(ert-deftest character-fold--speed-test ()
  (dolist (string (append '("tty-set-up-initial-frame-face"
                            "tty-set-up-initial-frame-face-frame-faceframe-faceframe-faceframe-face")
                          (mapcar #'character-fold--random-word '(10 50 100
                                                                     50 100))))
    (message "Testing %s" string)
    ;; Make sure we didn't just fallback on the trivial search.
    (should-not (string= (regexp-quote string)
                         (character-fold-to-regexp string)))
    (with-temp-buffer
      (save-excursion (insert string))
      (let ((time (time-to-seconds (current-time))))
        ;; Our initial implementation of case-folding in char-folding
        ;; created a lot of redundant paths in the regexp. Because of
        ;; that, if a really long string "almost" matches, the regexp
        ;; engine took a long time to realize that it doesn't match.
        (should-not (character-fold-search-forward (concat string "c") nil 'noerror))
        ;; Ensure it took less than a second.
        (should (< (- (time-to-seconds (current-time))
                      time)
                   1))))))

(provide 'character-fold-tests)
;;; character-fold-tests.el ends here
