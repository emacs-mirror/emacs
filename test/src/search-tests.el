;;; search-tests.el --- tests for search.c functions -*- lexical-binding: t -*-

;; Copyright (C) 2015-2016, 2018-2026 Free Software Foundation, Inc.

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

;; This test was bad: modification hooks should never modify
;; the buffer text, because it causes problems in too many places.
;;(ert-deftest test-replace-match-modification-hooks () ;bug#42424
;;  (let ((ov-set nil))
;;    (with-temp-buffer
;;      (insert "1 abc")
;;      (setq ov-set (make-overlay 3 5))
;;      (overlay-put
;;       ov-set 'modification-hooks
;;       (list (lambda (_o after &rest _args)
;;               (when after
;;                 (let ((inhibit-modification-hooks t))
;;                   (save-excursion
;;                     (goto-char 2)
;;                     (insert "234")))))))
;;      (goto-char 3)
;;      (if (search-forward "bc")
;;          (replace-match "bcd"))
;;      (should (= (point) 10)))))

(ert-deftest search-test--replace-match-update-data ()
  (with-temp-buffer
    (pcase-dolist (`(,pre ,post) '(("" "")
                                   ("a" "")
                                   ("" "b")
                                   ("a" "b")))
      (erase-buffer)
      (insert "hello ")
      (save-excursion (insert pre post " world"))
      (should (looking-at
               (concat "\\(\\)" pre "\\(\\)\\(\\(\\)\\)\\(\\)" post "\\(\\)")))
      (let* ((beg0 (match-beginning 0))
             (beg4 (+ beg0 (length pre)))
             (end4 (+ beg4 (length "BOO")))
             (end0 (+ end4 (length post))))
        (replace-match "BOO" t t nil 4)
        (should (equal (match-beginning 0) beg0))
        (should (equal (match-beginning 1) beg0))
        (should (equal (match-beginning 2) beg4))
        (should (equal (match-beginning 3) beg4))
        (should (equal (match-beginning 4) beg4))
        (should (equal (match-end 6) end0))
        (should (equal (match-end 5) end4))
        (should (equal (match-end 4) end4))
        (should (equal (match-end 3) end4))
        (should (equal (match-end 0) end0))
        ;; `update_search_regs' doesn't have enough information to get
        ;; the ones below correctly in all cases.
        (when (> (length post) 0)
          (should (equal (match-beginning 6) end0)))
        (when (> (length pre) 0)
          (should (equal (match-end 1) beg0)))
        ;; `update_search_regs' doesn't have enough information to get
        ;; the ones below correctly at all.
        ;;(should (equal (match-beginning 5) end4))
        ;;(should (equal (match-end 2) beg4))
        ))))

;;; search-tests.el ends here
