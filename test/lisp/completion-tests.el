;;; completion-tests.el --- Tests for completion.el  -*- lexical-binding: t -*-

;; Copyright (C) 2020-2026 Free Software Foundation, Inc.

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
(require 'completion)

(ert-deftest completion-test-cmpl-string-case-type ()
  (should (eq (cmpl-string-case-type "123ABCDEF456") :up))
  (should (eq (cmpl-string-case-type "123abcdef456") :down))
  (should (eq (cmpl-string-case-type "123aBcDeF456") :mixed))
  (should (eq (cmpl-string-case-type "123456") :neither))
  (should (eq (cmpl-string-case-type "Abcde123") :capitalized)))

(ert-deftest completion-test-cmpl-merge-string-cases ()
  (should (equal (cmpl-merge-string-cases "AbCdEf456" "abc") "AbCdEf456"))
  (should (equal (cmpl-merge-string-cases "abcdef456" "ABC") "ABCDEF456"))
  (should (equal (cmpl-merge-string-cases "ABCDEF456" "Abc") "Abcdef456"))
  (should (equal (cmpl-merge-string-cases "ABCDEF456" "abc") "abcdef456")))

(ert-deftest completion-test-add-find-delete-tail ()
  (unwind-protect
      (progn
        ;;  - Add and Find -
        (should (equal (add-completion-to-head "banana") '("banana" 0 nil 0)))
        (should (equal (find-exact-completion "banana") '("banana" 0 nil 0)))
        (should (equal (find-exact-completion "bana") nil))
        (should (equal (car (find-cmpl-prefix-entry "ban")) '(("banana" 0 nil 0))))
        (should (equal (cdr (find-cmpl-prefix-entry "ban")) '(("banana" 0 nil 0))))

        (should (equal (add-completion-to-head "banish") '("banish" 0 nil 0)))
        (should (equal (find-exact-completion "banish") '("banish" 0 nil 0)))
        (should (equal (car (find-cmpl-prefix-entry "ban")) '(("banish" 0 nil 0) ("banana" 0 nil 0))))
        (should (equal (cdr (find-cmpl-prefix-entry "ban")) '(("banana" 0 nil 0))))

        (should (equal (add-completion-to-head "banana") '("banana" 0 nil 0)))
        (should (equal (car (find-cmpl-prefix-entry "ban")) '(("banana" 0 nil 0) ("banish" 0 nil 0))))
        (should (equal (cdr (find-cmpl-prefix-entry "ban")) '(("banish" 0 nil 0))))

        ;;  - Deleting -
        (should (equal (add-completion-to-head "banner") '("banner" 0 nil 0)))
        (delete-completion "banner")
        (should-not (find-exact-completion "banner"))
        (should (equal (car (find-cmpl-prefix-entry "ban")) '(("banana" 0 nil 0) ("banish" 0 nil 0))))
        (should (equal (cdr (find-cmpl-prefix-entry "ban")) '(("banish" 0 nil 0))))
        (should (equal (add-completion-to-head "banner") '("banner" 0 nil 0)))
        (delete-completion "banana")
        (should (equal (car (find-cmpl-prefix-entry "ban")) '(("banner" 0 nil 0) ("banish" 0 nil 0))))
        (should (equal (cdr (find-cmpl-prefix-entry "ban")) '(("banish" 0 nil 0))))
        (delete-completion "banner")
        (delete-completion "banish")
        (should-not (find-cmpl-prefix-entry "ban"))
        (should-error (delete-completion "banner"))

        ;; - Tail -
        (should (equal (add-completion-to-tail-if-new "banana") '("banana" 0 nil 0)))
        (should (equal (car (find-cmpl-prefix-entry "ban")) '(("banana" 0 nil 0))))
        (should (equal (cdr (find-cmpl-prefix-entry "ban")) '(("banana" 0 nil 0))))
        (add-completion-to-tail-if-new "banish") '("banish" 0 nil 0)
        (should (equal (car (find-cmpl-prefix-entry "ban")) '(("banana" 0 nil 0) ("banish" 0 nil 0))))
        (should (equal (cdr (find-cmpl-prefix-entry "ban")) '(("banish" 0 nil 0)))))
    (ignore-errors (kill-completion "banana"))
    (ignore-errors (kill-completion "banner"))
    (ignore-errors (kill-completion "banish"))))

(ert-deftest completion-test-add-find-accept-delete ()
  (unwind-protect
      (progn
        ;; - Add and Find -
        (add-completion "banana" 5 10)
        (should (equal (find-exact-completion "banana") '("banana" 5 10 0)))
        (add-completion "banana" 6)
        (should (equal (find-exact-completion "banana") '("banana" 6 10 0)))
        (add-completion "banish")
        (should (equal (car (find-cmpl-prefix-entry "ban")) '(("banish" 0 nil 0) ("banana" 6 10 0))))

        ;; - Accepting -
        (setq completion-to-accept "banana")
        (accept-completion)
        (should (equal (find-exact-completion "banana") '("banana" 7 10 0)))
        (should (equal (car (find-cmpl-prefix-entry "ban")) '(("banana" 7 10 0) ("banish" 0 nil 0))))
        (setq completion-to-accept "banish")
        (add-completion "banner")
        (should (equal (car (find-cmpl-prefix-entry "ban"))
                       '(("banner" 0 nil 0) ("banish" 1 nil 0) ("banana" 7 10 0))))

        ;; - Deleting -
        (kill-completion "banish")
        (should (equal (car (find-cmpl-prefix-entry "ban")) '(("banner" 0 nil 0) ("banana" 7 10 0)))))
    (ignore-errors (kill-completion "banish"))
    (ignore-errors (kill-completion "banana"))
    (ignore-errors (kill-completion "banner"))))

(ert-deftest completion-test-search ()
  (unwind-protect
      (progn
        ;; - Add and Find -
        (add-completion "banana")
        (completion-search-reset "ban")
        (should (equal (car (completion-search-next 0)) "banana"))

        ;; - Discrimination -
        (add-completion "cumberland")
        (add-completion "cumberbund")
        ;; cumbering
        (completion-search-reset "cumb")
        (should (equal (car (completion-search-peek t)) "cumberbund"))
        (should (equal (car (completion-search-next 0)) "cumberbund"))
        (should (equal (car (completion-search-peek t)) "cumberland"))
        (should (equal (car (completion-search-next 1)) "cumberland"))
        (should-not (completion-search-peek nil))

        ;; FIXME
        ;; (should (equal (completion-search-next 2) "cumbering"))  ; {cdabbrev}
        ;;(completion-search-next 3)        -->  nil or "cumming" {depends on context}

        (should (equal (car (completion-search-next 1)) "cumberland"))

        ;; FIXME
        ;; (should (equal (completion-search-peek t) "cumbering"))  ; {cdabbrev}

        ;; - Accepting -
        (should (equal (car (completion-search-next 1)) "cumberland"))
        (setq completion-to-accept "cumberland")
        (completion-search-reset "foo")
        (completion-search-reset "cum")
        (should (equal (car (completion-search-next 0)) "cumberland"))

        ;; - Deleting -
        (kill-completion "cumberland")
        (add-completion "cummings")
        (completion-search-reset "cum")
        (should (equal (car (completion-search-next 0)) "cummings"))
        (should (equal (car (completion-search-next 1)) "cumberbund"))

        ;; - Ignoring Capitalization -
        (completion-search-reset "CuMb")
        (should (equal (car (completion-search-next 0)) "cumberbund")))
    (ignore-errors (kill-completion "banana"))
    (ignore-errors (kill-completion "cumberland"))
    (ignore-errors (kill-completion "cumberbund"))
    (ignore-errors (kill-completion "cummings"))))

(ert-deftest completion-test-lisp-def-regexp ()
  (should (= (and (string-match *lisp-def-regexp* "\n(defun foo") (match-end 0)) 8))
  (should (= (and (string-match *lisp-def-regexp* "\n(si:def foo") (match-end 0)) 9))
  (should (= (and (string-match *lisp-def-regexp* "\n(def-bar foo")(match-end 0)) 10))
  (should (= (and (string-match *lisp-def-regexp* "\n(defun (foo") (match-end 0)) 9)))

(provide 'completion-tests)
;;; completion-tests.el ends here
