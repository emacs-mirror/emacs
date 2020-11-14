;;; keymap-tests.el --- Test suite for src/keymap.c -*- lexical-binding: t -*-

;; Copyright (C) 2015-2020 Free Software Foundation, Inc.

;; Author: Juanma Barranquero <lekktu@gmail.com>
;;         Stefan Kangas <stefankangas@gmail.com>

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

(defun keymap-tests--make-keymap-test (fun)
  (should (eq (car (funcall fun)) 'keymap))
  (should (proper-list-p (funcall fun)))
  (should (equal (car (last (funcall fun "foo"))) "foo")))

(ert-deftest keymap-make-keymap ()
  (keymap-tests--make-keymap-test #'make-keymap)
  (should (char-table-p (cadr (make-keymap)))))

(ert-deftest keymap-make-sparse-keymap ()
  (keymap-tests--make-keymap-test #'make-sparse-keymap))

(ert-deftest keymap-keymapp ()
  (should (keymapp (make-keymap)))
  (should (keymapp (make-sparse-keymap)))
  (should-not (keymapp '(foo bar))))

(ert-deftest keymap-keymap-parent ()
  (should-not (keymap-parent (make-keymap)))
  (should-not (keymap-parent (make-sparse-keymap)))
  (let ((map (make-keymap)))
    (set-keymap-parent map help-mode-map)
    (should (equal (keymap-parent map) help-mode-map))))

(ert-deftest keymap-copy-keymap/is-equal ()
  (should (equal (copy-keymap help-mode-map) help-mode-map)))

(ert-deftest keymap-copy-keymap/is-not-eq ()
  (should-not (eq (copy-keymap help-mode-map) help-mode-map)))

(ert-deftest keymap---get-keyelt/runs-menu-item-filter ()
  (let* (menu-item-filter-ran
         (object `(menu-item "2" identity
                             :filter ,(lambda (cmd)
                                        (message "foo")
                                        (setq menu-item-filter-ran t)
                                        cmd))))
    (keymap--get-keyelt object t)
    (should menu-item-filter-ran)))

(ert-deftest keymap-lookup-key ()
  (let ((map (make-keymap)))
    (define-key map [?a] 'foo)
    (should (eq (lookup-key map [?a]) 'foo))))

(ert-deftest describe-buffer-bindings/header-in-current-buffer ()
  "Header should be inserted into the current buffer.
https://debbugs.gnu.org/39149#31"
  (with-temp-buffer
    (describe-buffer-bindings (current-buffer))
    (should (string-match (rx bol "key" (+ space) "binding" eol)
                          (buffer-string)))))

(ert-deftest describe-buffer-bindings/returns-nil ()
  "Should return nil."
  (with-temp-buffer
    (should (eq (describe-buffer-bindings (current-buffer)) nil))))

(defun keymap-tests--test-menu-item-filter (show filter-fun)
  (unwind-protect
      (progn
        (define-key global-map (kbd "C-c C-l r")
          `(menu-item "2" identity :filter ,filter-fun))
        (with-temp-buffer
          (describe-buffer-bindings (current-buffer))
          (goto-char (point-min))
          (if (eq show 'show)
              (should (search-forward "C-c C-l r" nil t))
            (should-not (search-forward "C-c C-l r" nil t)))))
    (define-key global-map (kbd "C-c C-l r") nil)
    (define-key global-map (kbd "C-c C-l") nil)))

(ert-deftest describe-buffer-bindings/menu-item-filter-show-binding ()
  (keymap-tests--test-menu-item-filter 'show (lambda (cmd) cmd)))

(ert-deftest describe-buffer-bindings/menu-item-filter-hide-binding ()
  (keymap-tests--test-menu-item-filter 'hide (lambda (_) nil)))

(ert-deftest keymap-store_in_keymap-XFASTINT-on-non-characters ()
  "Check for bug fixed in \"Fix assertion violation in define-key\",
commit 86c19714b097aa477d339ed99ffb5136c755a046."
  (let ((def (lookup-key Buffer-menu-mode-map [32])))
    (unwind-protect
        (progn
          (should-not (eq def 'undefined))
          ;; This will cause an assertion violation if the bug is present.
          ;; We could run an inferior Emacs process and check for the return
          ;; status, but in some environments an assertion failure triggers
          ;; an abort dialog that requires user intervention anyway.
          (define-key Buffer-menu-mode-map [(32 . 32)] 'undefined)
          (should (eq (lookup-key Buffer-menu-mode-map [32]) 'undefined)))
      (define-key Buffer-menu-mode-map [32] def))))


;;;; where-is-internal

(defun keymap-tests--command-1 () (interactive) nil)
(defun keymap-tests--command-2 () (interactive) nil)
(put 'keymap-tests--command-1 :advertised-binding [?y])

(ert-deftest keymap-where-is-internal ()
  (let ((map (make-sparse-keymap)))
    (define-key map "x" 'keymap-tests--command-1)
    (define-key map "y" 'keymap-tests--command-1)
    (should (equal (where-is-internal 'keymap-tests--command-1 map)
                   '([?y] [?x])))))

(ert-deftest keymap-where-is-internal/firstonly-t ()
  (let ((map (make-sparse-keymap)))
    (define-key map "x" 'keymap-tests--command-1)
    (define-key map "y" 'keymap-tests--command-1)
    (should (equal (where-is-internal 'keymap-tests--command-1 map t)
                   [?y]))))

(ert-deftest keymap-where-is-internal/menu-item ()
  (let ((map (make-sparse-keymap)))
    (define-key map [menu-bar foobar cmd1]
      '(menu-item "Run Command 1" keymap-tests--command-1
                  :help "Command 1 Help"))
    (define-key map "x" 'keymap-tests--command-1)
    (should (equal (where-is-internal 'keymap-tests--command-1 map)
                   '([?x] [menu-bar foobar cmd1])))
    (should (equal (where-is-internal 'keymap-tests--command-1 map t) [?x]))))


(ert-deftest keymap-where-is-internal/advertised-binding ()
  ;; Make sure order does not matter.
  (dolist (keys '(("x" . "y") ("y" . "x")))
    (let ((map (make-sparse-keymap)))
      (define-key map (car keys) 'keymap-tests--command-1)
      (define-key map (cdr keys) 'keymap-tests--command-1)
      (should (equal (where-is-internal 'keymap-tests--command-1 map t) [121])))))

(ert-deftest keymap-where-is-internal/advertised-binding-respect-remap ()
  (let ((map (make-sparse-keymap)))
    (define-key map "x" 'next-line)
    (define-key map [remap keymap-tests--command-1] 'next-line)
    (define-key map "y" 'keymap-tests--command-1)
    (should (equal (where-is-internal 'keymap-tests--command-1 map t) [?x]))))

(ert-deftest keymap-where-is-internal/remap ()
  (let ((map (make-keymap)))
    (define-key map (kbd "x") 'foo)
    (define-key map (kbd "y") 'bar)
    (define-key map [remap foo] 'bar)
    (should (equal (where-is-internal 'foo map t) [?y]))
    (should (equal (where-is-internal 'bar map t) [?y]))))

(defvar keymap-tests-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "x" 'keymap-tests--command-2)
    map))

(defvar keymap-tests-major-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "x" 'keymap-tests--command-1)
    map))

(define-minor-mode keymap-tests-minor-mode "Test.")

(define-derived-mode keymap-tests-major-mode nil "Test.")

(ert-deftest keymap-where-is-internal/shadowed ()
  (with-temp-buffer
    (keymap-tests-major-mode)
    (keymap-tests-minor-mode)
    (should-not (where-is-internal 'keymap-tests--command-1 nil t))
    (should (equal (where-is-internal 'keymap-tests--command-2 nil t) [120]))))

(ert-deftest keymap-where-is-internal/preferred-modifier-is-a-string ()
  "Make sure we don't crash when `where-is-preferred-modifier' is not a symbol."
  (should
   (equal (let ((where-is-preferred-modifier "alt"))
            (where-is-internal 'execute-extended-command global-map t))
          [#x8000078])))

(ert-deftest keymap-apropos-internal ()
  (should (equal (apropos-internal "^next-line$") '(next-line)))
  (should (>= (length (apropos-internal "^help")) 100))
  (should-not (apropos-internal "^test-a-missing-symbol-foo-bar-zut$")))

(ert-deftest keymap-apropos-internal/predicate ()
  (should (equal (apropos-internal "^next-line$" #'commandp) '(next-line)))
  (should (>= (length (apropos-internal "^help" #'commandp)) 15))
  (should-not (apropos-internal "^next-line$" #'keymapp)))

(provide 'keymap-tests)

;;; keymap-tests.el ends here
