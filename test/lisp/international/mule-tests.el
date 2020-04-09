;;; mule-tests.el --- unit tests for mule.el         -*- lexical-binding: t; -*-

;; Copyright (C) 2017-2020 Free Software Foundation, Inc.

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

;; Unit tests for lisp/international/mule.el.

;;; Code:

(ert-deftest find-auto-coding--bug27391 ()
  "Check that Bug#27391 is fixed."
  (with-temp-buffer
    (insert "\n[comment]: # ( Local Variables: )\n"
            "[comment]: # ( coding: utf-8	)\n"
            "[comment]: # ( End:		)\n")
    (goto-char (point-min))
    (should (equal (let ((auto-coding-alist ())
                         (auto-coding-regexp-alist ())
                         (auto-coding-functions ()))
                     (find-auto-coding "" (buffer-size)))
                   '(utf-8 . :coding)))))

(ert-deftest mule-cmds-tests--encode-ebcdic ()
  (should (equal (encode-coding-char ?a 'ebcdic-int) "\201"))
  (should (not (multibyte-string-p (encode-coding-char ?a 'utf-8)))))

(ert-deftest mule-cmds--test-universal-coding-system-argument ()
  (skip-unless (not noninteractive))
  (should (equal "ccccccccccccccccab"
                 (let ((enable-recursive-minibuffers t)
                       (unread-command-events
                        (append (kbd "C-x RET c u t f - 8 RET C-u C-u c a b RET") nil)))
                   (read-string "prompt:")))))

(ert-deftest mule-utf-7 ()
  ;; utf-7 and utf-7-imap are not ASCII-compatible.
  (should-not (coding-system-get 'utf-7 :ascii-compatible-p))
  (should-not (coding-system-get 'utf-7-imap :ascii-compatible-p))
  ;; Invariant ASCII subset.
  (let ((s (apply #'string (append (number-sequence #x20 #x25)
                                   (number-sequence #x27 #x7e)))))
    (should (equal (encode-coding-string s 'utf-7-imap) s))
    (should (equal (decode-coding-string s 'utf-7-imap) s)))
  ;; Escaped ampersand.
  (should (equal (encode-coding-string "a&bcd" 'utf-7-imap) "a&-bcd"))
  (should (equal (decode-coding-string "a&-bcd" 'utf-7-imap) "a&bcd"))
  ;; Ability to encode Unicode.
  (should (equal (check-coding-systems-region "あ" nil '(utf-7-imap)) nil))
  (should (equal (encode-coding-string "あ" 'utf-7-imap) "&MEI-"))
  (should (equal (decode-coding-string "&MEI-" 'utf-7-imap) "あ")))

(ert-deftest mule-hz ()
  ;; The chinese-hz encoding is not ASCII compatible.
  (should-not (coding-system-get 'chinese-hz :ascii-compatible-p)))

;; Stop "Local Variables" above causing confusion when visiting this file.


;;; mule-tests.el ends here
