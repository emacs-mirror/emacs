;;; isearch-tests.el --- Tests for isearch.el        -*- lexical-binding: t; -*-

;; Copyright (C) 2013-2026 Free Software Foundation, Inc.

;; Author: Artur Malabarba <bruce.connor.am@gmail.com>

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

(ert-deftest isearch--test-update ()
  (with-temp-buffer
    (setq isearch--current-buffer (current-buffer)))
  (with-temp-buffer
    (isearch-update)
    (should (equal isearch--current-buffer (current-buffer)))))

(ert-deftest isearch--test-done ()
  ;; Normal operation.
  (isearch-update)
  (isearch-done)
  (should-not isearch--current-buffer)
  ;; Bug #21091: let `isearch-done' work without `isearch-update'.
  (isearch-done))


;; Search invisible.

(declare-function outline-hide-sublevels "outline")

(ert-deftest isearch--test-invisible ()
  (require 'outline)
  (with-temp-buffer
    (set-window-buffer nil (current-buffer))
    (insert "\n1\n"
            (propertize "2" 'invisible t)
            (propertize "3" 'inhibit-isearch t)
            "\n* h\n4\n\n")
    (outline-mode)
    (outline-hide-sublevels 1)
    (goto-char (point-min))

    (let ((isearch-lazy-count nil)
          (search-invisible t)
          (inhibit-message t))

      (isearch-forward-regexp nil 1)
      (isearch-process-search-string "[0-9]" "[0-9]")
      (should (eq (point) 3))

      (isearch-lazy-highlight-start)
      (should (equal (seq-uniq (mapcar #'overlay-start isearch-lazy-highlight-overlays))
                     '(2)))

      (isearch-repeat-forward)
      (should (eq (point) 5))
      (should (get-char-property 4 'invisible))
      (isearch-repeat-forward)
      (should (eq (point) 12))
      (should (get-char-property 11 'invisible))

      (goto-char isearch-opoint)
      (isearch-done t)

      (isearch-forward-regexp nil 1)
      (setq isearch-invisible nil) ;; isearch-toggle-invisible
      (isearch-process-search-string "[0-9]" "[0-9]")

      (isearch-lazy-highlight-start)
      (should (equal (seq-uniq (mapcar #'overlay-start isearch-lazy-highlight-overlays))
                     '(2)))

      (goto-char isearch-opoint)
      (isearch-done t)

      (isearch-forward-regexp nil 1)
      (setq isearch-invisible 'open) ;; isearch-toggle-invisible
      (isearch-process-search-string "[0-9]" "[0-9]")
      (should (eq (point) 3))

      (isearch-lazy-highlight-start)
      (should (equal (seq-uniq (mapcar #'overlay-start isearch-lazy-highlight-overlays))
                     '(2 11)))

      (let ((isearch-hide-immediately t))
        (isearch-repeat-forward)
        (should (eq (point) 12))
        (should-not (get-char-property 11 'invisible))
        (isearch-delete-char)
        (should (get-char-property 11 'invisible)))

      (let ((isearch-hide-immediately nil))
        (isearch-repeat-forward)
        (should (eq (point) 12))
        (should-not (get-char-property 11 'invisible))
        (isearch-delete-char)
        (should-not (get-char-property 11 'invisible)))

      (goto-char isearch-opoint)
      (isearch-done t)
      (isearch-clean-overlays)
      (should (get-char-property 11 'invisible)))

    (let ((isearch-lazy-count t)
          (search-invisible t)
          (inhibit-message t))

      (isearch-forward-regexp nil 1)
      (isearch-process-search-string "[0-9]" "[0-9]")
      (should (eq (point) 3))

      (setq isearch-lazy-count-invisible nil isearch-lazy-count-total nil)
      (isearch-lazy-highlight-start)
      (isearch-lazy-highlight-buffer-update)
      (should (eq isearch-lazy-count-invisible nil))
      (should (eq isearch-lazy-count-total 3))
      (should (equal (seq-uniq (mapcar #'overlay-start isearch-lazy-highlight-overlays))
                     '(2)))

      (isearch-repeat-forward)
      (should (eq (point) 5))
      (should (get-char-property 4 'invisible))
      (isearch-repeat-forward)
      (should (eq (point) 12))
      (should (get-char-property 11 'invisible))

      (goto-char isearch-opoint)
      (isearch-done t)

      (isearch-forward-regexp nil 1)
      (setq isearch-invisible nil) ;; isearch-toggle-invisible
      (isearch-process-search-string "[0-9]" "[0-9]")

      (setq isearch-lazy-count-invisible nil isearch-lazy-count-total nil)
      (isearch-lazy-highlight-start)
      (isearch-lazy-highlight-buffer-update)
      (should (eq isearch-lazy-count-invisible 2))
      (should (eq isearch-lazy-count-total 1))
      (should (equal (seq-uniq (mapcar #'overlay-start isearch-lazy-highlight-overlays))
                     '(2)))

      (goto-char isearch-opoint)
      (isearch-done t)

      (isearch-forward-regexp nil 1)
      (setq isearch-invisible 'open) ;; isearch-toggle-invisible
      (isearch-process-search-string "[0-9]" "[0-9]")
      (should (eq (point) 3))

      (setq isearch-lazy-count-invisible nil isearch-lazy-count-total nil)
      (isearch-lazy-highlight-start)
      (isearch-lazy-highlight-buffer-update)
      (should (eq isearch-lazy-count-invisible 1))
      (should (eq isearch-lazy-count-total 2))
      (should (equal (seq-uniq (mapcar #'overlay-start isearch-lazy-highlight-overlays))
                     '(2 11)))

      (let ((isearch-hide-immediately t))
        (isearch-repeat-forward)
        (should (eq (point) 12))
        (should-not (get-char-property 11 'invisible))
        (isearch-delete-char)
        (should (get-char-property 11 'invisible)))

      (let ((isearch-hide-immediately nil))
        (isearch-repeat-forward)
        (should (eq (point) 12))
        (should-not (get-char-property 11 'invisible))
        (isearch-delete-char)
        (should-not (get-char-property 11 'invisible)))

      (goto-char isearch-opoint)
      (isearch-done t)
      (isearch-clean-overlays)
      (should (get-char-property 11 'invisible)))))


;; Search functions.

(defun isearch--test-search-within-boundaries (pairs)
  (goto-char (point-min))
  (let ((isearch-forward t)
        (isearch-regexp nil))
    (dolist (pos (append pairs nil))
      (should (eq (cdr pos) (isearch-search-string "foo" nil t)))
      (should (equal (match-string 0) "foo"))
      (when (car pos) (should (eq (car pos) (match-beginning 0))))))

  (goto-char (point-max))
  (let ((isearch-forward nil)
        (isearch-regexp nil))
    (dolist (pos (append (reverse pairs) nil))
      (should (eq (car pos) (isearch-search-string "foo" nil t)))
      (should (equal (match-string 0) "foo"))
      (when (cdr pos) (should (eq (cdr pos) (match-end 0))))))

  (goto-char (point-min))
  (let ((isearch-forward t)
        (isearch-regexp t))
    (dolist (pos (append pairs nil))
      (should (eq (cdr pos) (isearch-search-string ".*" nil t)))
      (should (equal (match-string 0) "foo"))
      (when (car pos) (should (eq (car pos) (match-beginning 0))))))

  (goto-char (point-min))
  (let ((isearch-forward t)
        (isearch-regexp t))
    (dolist (pos (append pairs nil))
      (should (eq (cdr pos) (isearch-search-string "^.*" nil t)))
      (should (equal (match-string 0) "foo"))
      (when (car pos) (should (eq (car pos) (match-beginning 0))))))

  (goto-char (point-min))
  (let ((isearch-forward t)
        (isearch-regexp t))
    (dolist (pos (append pairs nil))
      (should (eq (cdr pos) (isearch-search-string ".*$" nil t)))
      (should (equal (match-string 0) "foo"))
      (when (car pos) (should (eq (car pos) (match-beginning 0))))))

  (goto-char (point-max))
  (let ((isearch-forward nil)
        (isearch-regexp t))
    (dolist (pos (append (reverse pairs) nil))
      (should (eq (car pos) (isearch-search-string "^.*" nil t)))
      (should (equal (match-string 0) "foo"))
      (when (cdr pos) (should (eq (cdr pos) (match-end 0))))))

  (goto-char (point-max))
  (let ((isearch-forward nil)
        (isearch-regexp t))
    (dolist (pos (append (reverse pairs) nil))
      (should (eq (car pos) (isearch-search-string "foo$" nil t)))
      (should (equal (match-string 0) "foo"))
      (when (cdr pos) (should (eq (cdr pos) (match-end 0))))))

  ;; With BOUND arg (bug#78116)
  (goto-char (point-min))
  (let ((isearch-forward t)
        (isearch-regexp nil)
        (pos (car pairs)))
    (should (eq (cdr pos) (isearch-search-string "foo" (cdr pos) t)))
    (should (eq nil (isearch-search-string "foo" (cdr pos) t)))
    ;; Start on the text property inside boundaries
    (forward-char -1)
    (should (eq nil (isearch-search-string "foo" (cdr pos) t))))

  ;; With BOUND arg (bug#78116)
  (goto-char (point-max))
  (let ((isearch-forward nil)
        (isearch-regexp nil)
        (pos (car (last pairs))))
    (should (eq (car pos) (isearch-search-string "foo" (car pos) t)))
    (should (eq nil (isearch-search-string "foo" (car pos) t)))
    ;; Start on the text property inside boundaries
    (forward-char 1)
    (should (eq nil (isearch-search-string "foo" (car pos) t)))))

(ert-deftest isearch--test-search-fun-in-text-property ()
  (let* ((pairs '((4 . 7) (11 . 14) (21 . 24)))
         (isearch-search-fun-function
          (lambda () (isearch-search-fun-in-text-property nil 'dired-filename))))
    (with-temp-buffer
      (insert "foo" (propertize "foo" 'dired-filename t) "foo\n")
      (insert (propertize "foo" 'dired-filename t) "foo\n")
      (insert "foo" (propertize "foo" 'dired-filename t) "\n")
      (isearch--test-search-within-boundaries pairs))))

(ert-deftest isearch--test-search-fun-in-noncontiguous-region ()
  (let* ((pairs '((4 . 7) (11 . 14) (21 . 24)))
         (isearch-search-fun-function
          (lambda () (isearch-search-fun-in-noncontiguous-region nil pairs))))
    (with-temp-buffer
      (insert "foofoofoo\n")
      (insert "foofoo\n")
      (insert "foofoo\n")
      (isearch--test-search-within-boundaries pairs))))

(provide 'isearch-tests)
;;; isearch-tests.el ends here
