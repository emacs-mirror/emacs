;;; keymap-tests.el --- Test suite for src/keymap.c -*- lexical-binding: t -*-

;; Copyright (C) 2015-2022 Free Software Foundation, Inc.

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
                                        (setq menu-item-filter-ran t)
                                        cmd))))
    (keymap--get-keyelt object t)
    (should menu-item-filter-ran)))

(ert-deftest keymap-define-key/undefined ()
  ;;  nil (means key is undefined in this keymap),
  (let ((map (make-keymap)))
    (define-key map [?a] nil)
    (should-not (lookup-key map [?a]))))

(ert-deftest keymap-define-key/keyboard-macro ()
  ;;  a string (treated as a keyboard macro),
  (let ((map (make-keymap)))
    (define-key map [?a] "abc")
    (should (equal (lookup-key map [?a]) "abc"))))

(ert-deftest keymap-define-key/lambda ()
  (let ((map (make-keymap)))
    (define-key map [?a] (lambda () (interactive) nil))
    (should (functionp (lookup-key map [?a])))))

(ert-deftest keymap-define-key/keymap ()
  ;;  a keymap (to define a prefix key),
  (let ((map (make-keymap))
        (map2 (make-keymap)))
    (define-key map [?a] map2)
    (define-key map2 [?b] 'foo)
    (should (eq (lookup-key map [?a ?b]) 'foo))))

(ert-deftest keymap-define-key/menu-item ()
  ;;  or an extended menu item definition.
  ;;  (See info node ‘(elisp)Extended Menu Items’.)
  (let ((map (make-sparse-keymap))
        (menu (make-sparse-keymap)))
    (define-key menu [new-file]
      '(menu-item "Visit New File..." find-file
                  :enable (menu-bar-non-minibuffer-window-p)
                  :help "Specify a new file's name, to edit the file"))
    (define-key map [menu-bar file] (cons "File" menu))
    (should (eq (lookup-key map [menu-bar file new-file]) 'find-file))))

(ert-deftest keymap-lookup-key ()
  (let ((map (make-keymap)))
    (define-key map [?a] 'foo)
    (should (eq (lookup-key map [?a]) 'foo))
    (should-not (lookup-key map [?b]))))

(ert-deftest keymap-lookup-key/list-of-keymaps ()
  (let ((map1 (make-keymap))
        (map2 (make-keymap)))
    (define-key map1 [?a] 'foo)
    (define-key map2 [?b] 'bar)
    (should (eq (lookup-key (list map1 map2) [?a]) 'foo))
    (should (eq (lookup-key (list map1 map2) [?b]) 'bar))
    (should-not (lookup-key (list map1 map2) [?c]))))

(ert-deftest keymap-lookup-key/too-long ()
  (let ((map (make-keymap)))
    (define-key map (kbd "C-c f") 'foo)
    (should (= (lookup-key map (kbd "C-c f x")) 2))))

;; TODO: Write test for the ACCEPT-DEFAULT argument.
;; (ert-deftest keymap-lookup-key/accept-default ()
;;   ...)

(ert-deftest keymap-lookup-key/mixed-case ()
  "Backwards compatibility behaviour (Bug#50752)."
  (let ((map (make-keymap)))
    (define-key map [menu-bar foo bar] 'foo)
    (should (eq (lookup-key map [menu-bar foo bar]) 'foo))
    (should (eq (lookup-key map [menu-bar Foo Bar]) 'foo)))
  (let ((map (make-keymap)))
    (define-key map [menu-bar i-bar] 'foo)
    (should (eq (lookup-key map [menu-bar I-bar]) 'foo))))

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


;;;; describe_vector

(ert-deftest help--describe-vector/bug-9293-one-shadowed-in-range ()
  "Check that we only show a range if shadowed by the same command."
  (let ((orig-map (let ((map (make-keymap)))
                    (define-key map "e" 'foo)
                    (define-key map "f" 'foo)
                    (define-key map "g" 'foo)
                    (define-key map "h" 'foo)
                    map))
        (shadow-map (let ((map (make-keymap)))
                      (define-key map "f" 'bar)
                      map))
        (text-quoting-style 'grave)
        (describe-bindings-check-shadowing-in-ranges 'ignore-self-insert))
    (with-temp-buffer
      (help--describe-vector (cadr orig-map) nil #'help--describe-command
                             t shadow-map orig-map t)
      (should (equal (buffer-string)
                     "
e		foo
f		foo  (currently shadowed by `bar')
g .. h		foo
")))))

(ert-deftest help--describe-vector/bug-9293-same-command-does-not-shadow ()
  "Check that a command can't be shadowed by the same command."
  (let ((range-map
         (let ((map (make-keymap)))
           (define-key map "0" 'foo)
           (define-key map "1" 'foo)
           (define-key map "2" 'foo)
           (define-key map "3" 'foo)
           map))
        (shadow-map
         (let ((map (make-keymap)))
           (define-key map "0" 'foo)
           (define-key map "1" 'foo)
           (define-key map "2" 'foo)
           (define-key map "3" 'foo)
           map)))
   (with-temp-buffer
     (help--describe-vector (cadr range-map) nil #'help--describe-command
                            t shadow-map range-map t)
     (should (equal (buffer-string)
                    "
0 .. 3		foo
")))))

(ert-deftest keymap--key-description ()
  (should (equal (key-description [right] [?\C-x])
                 "C-x <right>"))
  (should (equal (key-description [M-H-right] [?\C-x])
                 "C-x M-H-<right>"))
  (should (equal (single-key-description 'home)
                 "<home>"))
  (should (equal (single-key-description 'home t)
                 "home"))
  (should (equal (single-key-description 'C-s-home)
                 "C-s-<home>")))

(provide 'keymap-tests)

;;; keymap-tests.el ends here
