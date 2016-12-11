;;; ibuffer-tests.el --- Test suite. -*- lexical-binding: t -*-

;; Copyright (C) 2015-2016 Free Software Foundation, Inc.

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
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:
(require 'ert)
(require 'ibuffer)
(eval-when-compile
  (require 'ibuf-macs))

(ert-deftest ibuffer-autoload ()
  "Tests to see whether reftex-auc has been autoloaded"
  (should
   (fboundp 'ibuffer-mark-unsaved-buffers))
  (should
   (autoloadp
    (symbol-function
     'ibuffer-mark-unsaved-buffers))))

(ert-deftest ibuffer-test-Bug24997 ()
  "Test for http://debbugs.gnu.org/24997 ."
  (ibuffer)
  (let ((orig ibuffer-filtering-qualifiers))
    (unwind-protect
        (progn
          (setq ibuffer-filtering-qualifiers
                '((size-gt . 10)
                  (used-mode . lisp-interaction-mode)))
          (ibuffer-update nil t)
          (ignore-errors (ibuffer-decompose-filter))
          (should (cdr ibuffer-filtering-qualifiers)))
      (setq ibuffer-filtering-qualifiers orig)
      (ibuffer-update nil t))))

(ert-deftest ibuffer-test-Bug25000 ()
  "Test for http://debbugs.gnu.org/25000 ."
  (let ((case-fold-search t)
        (buf1 (generate-new-buffer "ibuffer-test-Bug25000-buf1"))
        (buf2 (generate-new-buffer "ibuffer-test-Bug25000-buf2")))
    (ibuffer)
    (unwind-protect
        (ibuffer-save-marks
          (ibuffer-unmark-all-marks)
          (ibuffer-mark-by-name-regexp (buffer-name buf1))
          (ibuffer-change-marks ibuffer-marked-char ?L)
          (ibuffer-mark-by-name-regexp (buffer-name buf2))
          (ibuffer-change-marks ibuffer-marked-char ?l)
          (should-not (cdr (ibuffer-buffer-names-with-mark ?l))))
      (mapc (lambda (buf) (when (buffer-live-p buf)
                            (kill-buffer buf))) (list buf1 buf2)))))

(ert-deftest ibuffer-save-filters ()
  "Tests that `ibuffer-save-filters' saves in the proper format."
  (skip-unless (featurep 'ibuf-ext))
  (let ((ibuffer-save-with-custom nil)
        (ibuffer-saved-filters nil)
        (test1 '((mode . org-mode)
                 (or (size-gt . 10000)
                     (and (not (starred-name))
                          (directory . "\<org\>")))))
        (test2 '((or (mode . emacs-lisp-mode) (file-extension . "elc?")
                     (and (starred-name) (name . "elisp"))
                     (mode . lisp-interaction-mode))))
        (test3 '((size-lt . 100) (derived-mode . prog-mode)
                 (or (filename . "scratch")
                     (filename . "bonz")
                     (filename . "temp")))))
    (ibuffer-save-filters "test1" test1)
    (should (equal (car ibuffer-saved-filters) (cons "test1" test1)))
    (ibuffer-save-filters "test2" test2)
    (should (equal (car ibuffer-saved-filters) (cons "test2" test2)))
    (should (equal (cadr ibuffer-saved-filters) (cons "test1" test1)))
    (ibuffer-save-filters "test3" test3)
    (should (equal (car ibuffer-saved-filters) (cons "test3" test3)))
    (should (equal (cadr ibuffer-saved-filters) (cons "test2" test2)))
    (should (equal (car (cddr ibuffer-saved-filters)) (cons "test1" test1)))
    (should (equal (cdr (assoc "test1" ibuffer-saved-filters)) test1))
    (should (equal (cdr (assoc "test2" ibuffer-saved-filters)) test2))
    (should (equal (cdr (assoc "test3" ibuffer-saved-filters)) test3))))

(ert-deftest ibuffer-test-Bug25058 ()
  "Test for http://debbugs.gnu.org/25058 ."
  (ibuffer)
  (let ((orig-filters ibuffer-saved-filter-groups)
        (tmp-filters '(("saved-filters"
                        ("Shell"
                         (used-mode . shell-mode))
                        ("Elisp"
                         (or
                          (used-mode . emacs-lisp-mode)
                          (used-mode . lisp-interaction-mode)))
                        ("Dired"
                         (used-mode . dired-mode))
                        ("Info"
                         (or
                          (used-mode . help-mode)
                          (used-mode . debugger-mode)
                          (used-mode . Custom-mode)
                          (used-mode . completion-list-mode)
                          (name . "\\`[*]Messages[*]\\'")))))))
    (unwind-protect
        (progn
          (setq ibuffer-saved-filter-groups tmp-filters)
	  (ibuffer-switch-to-saved-filter-groups "saved-filters")
          (ibuffer-decompose-filter-group "Elisp")
          (ibuffer-filter-disable)
          (ibuffer-switch-to-saved-filter-groups "saved-filters")
          (should (assoc "Elisp" (cdar ibuffer-saved-filter-groups))))
      (setq ibuffer-saved-filter-groups orig-filters)
      (ibuffer-awhen (get-buffer "*Ibuffer*")
        (and (buffer-live-p it) (kill-buffer it))))))


(ert-deftest ibuffer-test-Bug25042 ()
  "Test for http://debbugs.gnu.org/25042 ."
  (ibuffer)
  (let ((filters ibuffer-filtering-qualifiers))
    (unwind-protect
        (progn
          (ignore-errors ; Mistyped `match-string' instead of `string-match'.
            (setq ibuffer-filtering-qualifiers nil)
            (ibuffer-filter-by-predicate '(match-string "foo" (buffer-name))))
          (should-not ibuffer-filtering-qualifiers))
      (setq ibuffer-filtering-qualifiers filters))))

(provide 'ibuffer-tests)
;; ibuffer-tests.el ends here
