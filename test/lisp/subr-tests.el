;;; subr-tests.el --- Tests for subr.el  -*- lexical-binding:t -*-

;; Copyright (C) 2015-2024 Free Software Foundation, Inc.

;; Author: Oleh Krehel <ohwoeowho@gmail.com>,
;;         Nicolas Petton <nicolas@petton.fr>
;; Keywords:

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

;;

;;; Code:
(require 'ert)
(eval-when-compile (require 'cl-lib))

(ert-deftest let-when-compile ()
  ;; good case
  (should (equal (macroexpand '(let-when-compile ((foo (+ 2 3)))
                                (setq bar (eval-when-compile (+ foo foo)))
                                (setq boo (eval-when-compile (* foo foo)))))
                 '(progn
                   (setq bar (quote 10))
                   (setq boo (quote 25)))))
  ;; bad case: `eval-when-compile' omitted, byte compiler should catch this
  (should (equal (macroexpand
                  '(let-when-compile ((foo (+ 2 3)))
                    (setq bar (+ foo foo))
                    (setq boo (eval-when-compile (* foo foo)))))
                 '(progn
                   (setq bar (+ foo foo))
                   (setq boo (quote 25)))))
  ;; something practical
  (should (equal (macroexpand
                  '(let-when-compile ((keywords '("true" "false")))
                    (font-lock-add-keywords
                     'c++-mode
                     `((,(eval-when-compile
                           (format "\\<%s\\>" (regexp-opt keywords)))
                         0 font-lock-keyword-face)))))
                 '(font-lock-add-keywords
                   (quote c++-mode)
                   (list
                    (cons (quote
                           "\\<\\(?:\\(?:fals\\|tru\\)e\\)\\>")
                     (quote
                      (0 font-lock-keyword-face))))))))


;;;; List functions.

(ert-deftest subr-test-caaar ()
  (should (null (caaar '())))
  (should (null (caaar '(() (2)))))
  (should (null (caaar '((() (2)) (a b)))))
  (should-error (caaar '(1 2)) :type 'wrong-type-argument)
  (should-error (caaar '((1 2))) :type 'wrong-type-argument)
  (should (=  1 (caaar '(((1 2) (3 4))))))
  (should (null (caaar '((() (3 4)))))))

(ert-deftest subr-test-caadr ()
  (should (null (caadr '())))
  (should (null (caadr '(1))))
  (should-error (caadr '(1 2)) :type 'wrong-type-argument)
  (should (= 2 (caadr '(1 (2 3)))))
  (should (equal '((2) (3)) (caadr '((1) (((2) (3))) (4))))))


;;;; Keymap support.

(ert-deftest subr-test-kbd ()
  (should (equal (kbd "") ""))
  (should (equal (kbd "f") "f"))
  (should (equal (kbd "X") "X"))
  (should (equal (kbd "foobar") "foobar")) ; 6 characters
  (should (equal (kbd "return") "return")) ; 6 characters

  (should (equal (kbd "<F2>") [F2]))
  (should (equal (kbd "<f1> <f2> TAB") [f1 f2 ?\t]))
  (should (equal (kbd "<f1> RET") [f1 ?\r]))
  (should (equal (kbd "<f1> SPC") [f1 ? ]))
  (should (equal (kbd "<f1>") [f1]))
  (should (equal (kbd "<f1>") [f1]))
  (should (equal (kbd "[f1]") "[f1]"))
  (should (equal (kbd "<return>") [return]))
  (should (equal (kbd "< right >") "<right>")) ; 7 characters

  ;; Modifiers:
  (should (equal (kbd "C-x") "\C-x"))
  (should (equal (kbd "C-x a") "\C-xa"))
  (should (equal (kbd "C-;") [?\C-\;]))
  (should (equal (kbd "C-a") "\C-a"))
  (should (equal (kbd "C-c SPC") "\C-c "))
  (should (equal (kbd "C-c TAB") "\C-c\t"))
  (should (equal (kbd "C-c c") "\C-cc"))
  (should (equal (kbd "C-x 4 C-f") "\C-x4\C-f"))
  (should (equal (kbd "C-x C-f") "\C-x\C-f"))
  (should (equal (kbd "C-M-<down>") [C-M-down]))
  (should (equal (kbd "<C-M-down>") [C-M-down]))
  (should (equal (kbd "C-RET") [?\C-\r]))
  (should (equal (kbd "C-SPC") [?\C- ]))
  (should (equal (kbd "C-TAB") [?\C-\t]))
  (should (equal (kbd "C-<down>") [C-down]))
  (should (equal (kbd "C-c C-c C-c") "\C-c\C-c\C-c"))

  (should (equal (kbd "M-a") [?\M-a]))
  (should (equal (kbd "M-<DEL>") [?\M-\d]))
  (should (equal (kbd "M-C-a") [?\M-\C-a]))
  (should (equal (kbd "M-ESC") [?\M-\e]))
  (should (equal (kbd "M-RET") [?\M-\r]))
  (should (equal (kbd "M-SPC") [?\M- ]))
  (should (equal (kbd "M-TAB") [?\M-\t]))
  (should (equal (kbd "M-x a") [?\M-x ?a]))
  (should (equal (kbd "M-<up>") [M-up]))
  (should (equal (kbd "M-c M-c M-c") [?\M-c ?\M-c ?\M-c]))

  (should (equal (kbd "s-SPC") [?\s- ]))
  (should (equal (kbd "s-a") [?\s-a]))
  (should (equal (kbd "s-x a") [?\s-x ?a]))
  (should (equal (kbd "s-c s-c s-c") [?\s-c ?\s-c ?\s-c]))

  (should (equal (kbd "S-H-a") [?\S-\H-a]))
  (should (equal (kbd "S-a") [?\S-a]))
  (should (equal (kbd "S-x a") [?\S-x ?a]))
  (should (equal (kbd "S-c S-c S-c") [?\S-c ?\S-c ?\S-c]))

  (should (equal (kbd "H-<RET>") [?\H-\r]))
  (should (equal (kbd "H-DEL") [?\H-\d]))
  (should (equal (kbd "H-a") [?\H-a]))
  (should (equal (kbd "H-x a") [?\H-x ?a]))
  (should (equal (kbd "H-c H-c H-c") [?\H-c ?\H-c ?\H-c]))

  (should (equal (kbd "A-H-a") [?\A-\H-a]))
  (should (equal (kbd "A-SPC") [?\A- ]))
  (should (equal (kbd "A-TAB") [?\A-\t]))
  (should (equal (kbd "A-a") [?\A-a]))
  (should (equal (kbd "A-c A-c A-c") [?\A-c ?\A-c ?\A-c]))

  (should (equal (kbd "C-M-a") [?\C-\M-a]))
  (should (equal (kbd "C-M-<up>") [C-M-up]))

  ;; Special characters.
  (should (equal (kbd "DEL") "\d"))
  (should (equal (kbd "ESC C-a") "\e\C-a"))
  (should (equal (kbd "ESC") "\e"))
  (should (equal (kbd "LFD") "\n"))
  (should (equal (kbd "NUL") "\0"))
  (should (equal (kbd "RET") "\C-m"))
  (should (equal (kbd "SPC") "\s"))
  (should (equal (kbd "TAB") "\t"))
  (should (equal (kbd "\^i") ""))
  (should (equal (kbd "^M") "\^M"))

  ;; With numbers.
  (should (equal (kbd "\177") "\^?"))
  (should (equal (kbd "\000") "\0"))
  (should (equal (kbd "\\177") "\^?"))
  (should (equal (kbd "\\000") "\0"))
  (should (equal (kbd "C-x \\150") "\C-xh"))

  ;; Multibyte
  (should (equal (kbd "ñ") [?ñ]))
  (should (equal (kbd "ü") [?ü]))
  (should (equal (kbd "ö") [?ö]))
  (should (equal (kbd "ğ") [?ğ]))
  (should (equal (kbd "ա") [?ա]))
  (should (equal (kbd "üüöö") [?ü ?ü ?ö ?ö]))
  (should (equal (kbd "C-ü") [?\C-ü]))
  (should (equal (kbd "M-ü") [?\M-ü]))
  (should (equal (kbd "H-ü") [?\H-ü]))

  ;; Handle both new and old style key descriptions (bug#45536).
  (should (equal (kbd "s-<return>") [s-return]))
  (should (equal (kbd "<s-return>") [s-return]))
  (should (equal (kbd "C-M-<return>") [C-M-return]))
  (should (equal (kbd "<C-M-return>") [C-M-return]))

  ;; Error.
  (should-error (kbd "C-xx"))
  (should-error (kbd "M-xx"))
  (should-error (kbd "M-x<TAB>"))

  ;; These should be equivalent:
  (should (equal (kbd "\C-xf") (kbd "C-x f"))))

(ert-deftest subr-test-key-valid-p ()
  (should (not (key-valid-p "")))
  (should (key-valid-p "f"))
  (should (key-valid-p "X"))
  (should (not (key-valid-p " X")))
  (should (key-valid-p "X f"))
  (should (not (key-valid-p "a  b")))
  (should (not (key-valid-p "foobar")))
  (should (not (key-valid-p "return")))

  (should (key-valid-p "<F2>"))
  (should (key-valid-p "<f1> <f2> TAB"))
  (should (key-valid-p "<f1> RET"))
  (should (key-valid-p "<f1> SPC"))
  (should (key-valid-p "<f1>"))
  (should (not (key-valid-p "[f1]")))
  (should (key-valid-p "<return>"))
  (should (not (key-valid-p "< right >")))

  ;; Modifiers:
  (should (key-valid-p "C-x"))
  (should (key-valid-p "C-x a"))
  (should (key-valid-p "C-;"))
  (should (key-valid-p "C-a"))
  (should (key-valid-p "C-c SPC"))
  (should (key-valid-p "C-c TAB"))
  (should (key-valid-p "C-c c"))
  (should (key-valid-p "C-x 4 C-f"))
  (should (key-valid-p "C-x C-f"))
  (should (key-valid-p "C-M-<down>"))
  (should (not (key-valid-p "<C-M-down>")))
  (should (key-valid-p "C-RET"))
  (should (key-valid-p "C-SPC"))
  (should (key-valid-p "C-TAB"))
  (should (key-valid-p "C-<down>"))
  (should (key-valid-p "C-c C-c C-c"))

  (should (key-valid-p "M-a"))
  (should (key-valid-p "M-<DEL>"))
  (should (not (key-valid-p "M-C-a")))
  (should (key-valid-p "C-M-a"))
  (should (key-valid-p "M-ESC"))
  (should (key-valid-p "M-RET"))
  (should (key-valid-p "M-SPC"))
  (should (key-valid-p "M-TAB"))
  (should (key-valid-p "M-x a"))
  (should (key-valid-p "M-<up>"))
  (should (key-valid-p "M-c M-c M-c"))

  (should (key-valid-p "s-SPC"))
  (should (key-valid-p "s-a"))
  (should (key-valid-p "s-x a"))
  (should (key-valid-p "s-c s-c s-c"))

  (should (not (key-valid-p "S-H-a")))
  (should (key-valid-p "S-a"))
  (should (key-valid-p "S-x a"))
  (should (key-valid-p "S-c S-c S-c"))

  (should (key-valid-p "H-<RET>"))
  (should (key-valid-p "H-DEL"))
  (should (key-valid-p "H-a"))
  (should (key-valid-p "H-x a"))
  (should (key-valid-p "H-c H-c H-c"))

  (should (key-valid-p "A-H-a"))
  (should (key-valid-p "A-SPC"))
  (should (key-valid-p "A-TAB"))
  (should (key-valid-p "A-a"))
  (should (key-valid-p "A-c A-c A-c"))

  (should (key-valid-p "C-M-a"))
  (should (key-valid-p "C-M-<up>"))

  ;; Special characters.
  (should (key-valid-p "DEL"))
  (should (key-valid-p "ESC C-a"))
  (should (key-valid-p "ESC"))
  (should (key-valid-p "LFD"))
  (should (key-valid-p "NUL"))
  (should (key-valid-p "RET"))
  (should (key-valid-p "SPC"))
  (should (key-valid-p "TAB"))
  (should (not (key-valid-p "\^i")))
  (should (not (key-valid-p "^M")))

  ;; With numbers.
  (should (not (key-valid-p "\177")))
  (should (not (key-valid-p "\000")))
  (should (not (key-valid-p "\\177")))
  (should (not (key-valid-p "\\000")))
  (should (not (key-valid-p "C-x \\150")))

  ;; Multibyte
  (should (key-valid-p "ñ"))
  (should (key-valid-p "ü"))
  (should (key-valid-p "ö"))
  (should (key-valid-p "ğ"))
  (should (key-valid-p "ա"))
  (should (not (key-valid-p "üüöö")))
  (should (key-valid-p "C-ü"))
  (should (key-valid-p "M-ü"))
  (should (key-valid-p "H-ü"))

  ;; Handle both new and old style key descriptions (bug#45536).
  (should (key-valid-p "s-<return>"))
  (should (not (key-valid-p "<s-return>")))
  (should (key-valid-p "C-M-<return>"))
  (should (not (key-valid-p "<C-M-return>")))

  (should (key-valid-p "<mouse-1>"))
  (should (key-valid-p "<Scroll_Lock>"))

  (should (not (key-valid-p "c-x")))
  (should (not (key-valid-p "C-xx")))
  (should (not (key-valid-p "M-xx")))
  (should (not (key-valid-p "M-x<TAB>"))))

(ert-deftest subr-test-define-prefix-command ()
  (define-prefix-command 'foo-prefix-map)
  (defvar foo-prefix-map)
  (declare-function foo-prefix-map "subr-tests")
  (should (keymapp foo-prefix-map))
  (should (fboundp #'foo-prefix-map))
  ;; With optional argument.
  (define-prefix-command 'bar-prefix 'bar-prefix-map)
  (defvar bar-prefix-map)
  (declare-function bar-prefix "subr-tests")
  (should (keymapp bar-prefix-map))
  (should (fboundp #'bar-prefix))
  ;; Returns the symbol.
  (should (eq (define-prefix-command 'foo-bar) 'foo-bar)))

(ert-deftest subr-test-local-key-binding ()
  (with-temp-buffer
    (emacs-lisp-mode)
    (should (keymapp (local-key-binding [menu-bar])))
    (should-not (local-key-binding [f12]))))

(ert-deftest subr-test-global-key-binding ()
  (should (eq (global-key-binding [f1]) 'help-command))
  (should (eq (global-key-binding "x") 'self-insert-command))
  (should-not (global-key-binding [f12])))


;;;; Mode hooks.

(defalias 'subr-tests--parent-mode
  (if (fboundp 'prog-mode) 'prog-mode 'fundamental-mode))

(ert-deftest provided-mode-derived-p ()
  ;; base case: `derived-mode' directly derives `prog-mode'
  (should (progn
            (define-derived-mode derived-mode prog-mode "test")
            (provided-mode-derived-p 'derived-mode 'prog-mode)))
  ;; edge case: `derived-mode' derives an alias of `prog-mode'
  (should (progn
            (define-derived-mode derived-mode subr-tests--parent-mode "test")
            (provided-mode-derived-p 'derived-mode 'prog-mode))))

(ert-deftest number-sequence-test ()
  (should (= (length
              (number-sequence (1- most-positive-fixnum) most-positive-fixnum))
             2))
  (should (= (length
              (number-sequence
               (1+ most-negative-fixnum) most-negative-fixnum -1))
             2)))

(ert-deftest string-comparison-test ()
  (should (string-equal-ignore-case "abc" "abc"))
  (should (string-equal-ignore-case "abc" "ABC"))
  (should (string-equal-ignore-case "abc" "abC"))
  (should-not (string-equal-ignore-case "abc" "abCD"))
  (should (string-equal-ignore-case "S" "s"))
  (should (string-equal-ignore-case "ẞ" "ß"))
  (should (string-equal-ignore-case "ǲ" "Ǳ"))
  (should (string-equal-ignore-case "Όσος" "ΌΣΟΣ"))
  ;; not yet: (should (string-equal-ignore-case "SS" "ß"))
  ;; not yet: (should (string-equal-ignore-case "SS" "ẞ"))

  (should (string-lessp "abc" "acb"))
  (should (string-lessp "aBc" "abc"))
  (should (string-lessp "abc" "abcd"))
  (should (string-lessp "abc" "abcd"))
  (should-not (string-lessp "abc" "abc"))
  (should-not (string-lessp "" ""))

  (should (string-greaterp "acb" "abc"))
  (should (string-greaterp "abc" "aBc"))
  (should (string-greaterp "abcd" "abc"))
  (should (string-greaterp "abcd" "abc"))
  (should-not (string-greaterp "abc" "abc"))
  (should-not (string-greaterp "" ""))

  ;; Symbols are also accepted
  (should (string-lessp 'abc 'acb))
  (should (string-lessp "abc" 'acb))
  (should (string-greaterp 'acb 'abc))
  (should (string-greaterp "acb" 'abc)))

(ert-deftest subr-test-when ()
  (should (equal (when t 1) 1))
  (should (equal (when t 2) 2))
  (should (equal (when nil 1) nil))
  (should (equal (when nil 2) nil))
  (should (equal (when t 'x 1) 1))
  (should (equal (when t 'x 2) 2))
  (should (equal (when nil 'x 1) nil))
  (should (equal (when nil 'x 2) nil))
  (let ((x 1))
    (should-not (when nil
                  (setq x (1+ x))
                  x))
    (should (= x 1))
    (should (= 2 (when t
                   (setq x (1+ x))
                   x)))
    (should (= x 2)))
  (should (equal (macroexpand-all '(when a b c d))
                 '(if a (progn b c d)))))

(ert-deftest subr-test-xor ()
  "Test `xor'."
  (should-not (xor nil nil))
  (should (eq (xor nil 'true) 'true))
  (should (eq (xor 'true nil) 'true))
  (should-not (xor t t)))

(ert-deftest subr-test-version-parsing ()
  (should (equal (version-to-list ".5") '(0 5)))
  (should (equal (version-to-list "0.9 alpha1") '(0 9 -3 1)))
  (should (equal (version-to-list "0.9 snapshot") '(0  9 -4)))
  (should (equal (version-to-list "0.9-alpha1") '(0 9 -3 1)))
  (should (equal (version-to-list "0.9-snapshot") '(0  9 -4)))
  (should (equal (version-to-list "0.9.snapshot") '(0  9 -4)))
  (should (equal (version-to-list "0.9_snapshot") '(0  9 -4)))
  (should (equal (version-to-list "0.9alpha1") '(0 9 -3 1)))
  (should (equal (version-to-list "0.9snapshot") '(0  9 -4)))
  (should (equal (version-to-list "1.0 git") '(1  0 -4)))
  (should (equal (version-to-list "1.0 pre2") '(1 0 -1 2)))
  (should (equal (version-to-list "1.0-git") '(1  0 -4)))
  (should (equal (version-to-list "1.0-pre2") '(1 0 -1 2)))
  (should (equal (version-to-list "1.0.1-a") '(1 0 1 1)))
  (should (equal (version-to-list "1.0.1-f") '(1 0 1 6)))
  (should (equal (version-to-list "1.0.1.a") '(1 0 1 1)))
  (should (equal (version-to-list "1.0.1.f") '(1 0 1 6)))
  (should (equal (version-to-list "1.0.1_a") '(1 0 1 1)))
  (should (equal (version-to-list "1.0.1_f") '(1 0 1 6)))
  (should (equal (version-to-list "1.0.1a") '(1 0 1 1)))
  (should (equal (version-to-list "1.0.1f") '(1 0 1 6)))
  (should (equal (version-to-list "1.0.7.5") '(1 0 7 5)))
  (should (equal (version-to-list "1.0.git") '(1  0 -4)))
  (should (equal (version-to-list "1.0.pre2") '(1 0 -1 2)))
  (should (equal (version-to-list "1.0_git") '(1  0 -4)))
  (should (equal (version-to-list "1.0_pre2") '(1 0 -1 2)))
  (should (equal (version-to-list "1.0git") '(1  0 -4)))
  (should (equal (version-to-list "1.0pre2") '(1 0 -1 2)))
  (should (equal (version-to-list "22.8 beta3") '(22 8 -2 3)))
  (should (equal (version-to-list "22.8-beta3") '(22 8 -2 3)))
  (should (equal (version-to-list "22.8.beta3") '(22 8 -2 3)))
  (should (equal (version-to-list "22.8_beta3") '(22 8 -2 3)))
  (should (equal (version-to-list "22.8beta3") '(22 8 -2 3)))
  (should (equal (version-to-list "6.9.30 Beta") '(6 9 30 -2)))
  (should (equal (version-to-list "6.9.30-Beta") '(6 9 30 -2)))
  (should (equal (version-to-list "6.9.30.Beta") '(6 9 30 -2)))
  (should (equal (version-to-list "6.9.30Beta") '(6 9 30 -2)))
  (should (equal (version-to-list "6.9.30_Beta") '(6 9 30 -2)))

  (let ((text-quoting-style 'grave))
    (should (equal
             (error-message-string (should-error (version-to-list "OTP-18.1.5")))
             "Invalid version syntax: `OTP-18.1.5' (must start with a number)"))
    (should (equal
             (error-message-string (should-error (version-to-list "")))
             "Invalid version syntax: `' (must start with a number)"))
    (should (equal
             (error-message-string (should-error (version-to-list "1.0..7.5")))
             "Invalid version syntax: `1.0..7.5'"))
    (should (equal
             (error-message-string (should-error (version-to-list "1.0prepre2")))
             "Invalid version syntax: `1.0prepre2'"))
    (should (equal
             (error-message-string (should-error (version-to-list "22.8X3")))
             "Invalid version syntax: `22.8X3'"))
    (should (equal
             (error-message-string (should-error (version-to-list "beta22.8alpha3")))
             "Invalid version syntax: `beta22.8alpha3' (must start with a number)"))
    (should (equal
             (error-message-string (should-error (version-to-list "honk")))
             "Invalid version syntax: `honk' (must start with a number)")))
  (should (equal
            (error-message-string (should-error (version-to-list 9)))
            "Version must be a string"))

  (let ((version-separator "_"))
    (should (equal (version-to-list "_5") '(0 5)))
    (should (equal (version-to-list "0_9 alpha1") '(0 9 -3 1)))
    (should (equal (version-to-list "0_9 snapshot") '(0  9 -4)))
    (should (equal (version-to-list "0_9-alpha1") '(0 9 -3 1)))
    (should (equal (version-to-list "0_9-snapshot") '(0  9 -4)))
    (should (equal (version-to-list "0_9.alpha1") '(0 9 -3 1)))
    (should (equal (version-to-list "0_9.snapshot") '(0  9 -4)))
    (should (equal (version-to-list "0_9alpha1") '(0 9 -3 1)))
    (should (equal (version-to-list "0_9snapshot") '(0  9 -4)))
    (should (equal (version-to-list "1_0 git") '(1  0 -4)))
    (should (equal (version-to-list "1_0 pre2") '(1 0 -1 2)))
    (should (equal (version-to-list "1_0-git") '(1  0 -4)))
    (should (equal (version-to-list "1_0.pre2") '(1 0 -1 2)))
    (should (equal (version-to-list "1_0_1-a") '(1 0 1 1)))
    (should (equal (version-to-list "1_0_1-f") '(1 0 1 6)))
    (should (equal (version-to-list "1_0_1.a") '(1 0 1 1)))
    (should (equal (version-to-list "1_0_1.f") '(1 0 1 6)))
    (should (equal (version-to-list "1_0_1_a") '(1 0 1 1)))
    (should (equal (version-to-list "1_0_1_f") '(1 0 1 6)))
    (should (equal (version-to-list "1_0_1a") '(1 0 1 1)))
    (should (equal (version-to-list "1_0_1f") '(1 0 1 6)))
    (should (equal (version-to-list "1_0_7_5") '(1 0 7 5)))
    (should (equal (version-to-list "1_0_git") '(1  0 -4)))
    (should (equal (version-to-list "1_0pre2") '(1 0 -1 2)))
    (should (equal (version-to-list "22_8 beta3") '(22 8 -2 3)))
    (should (equal (version-to-list "22_8-beta3") '(22 8 -2 3)))
    (should (equal (version-to-list "22_8.beta3") '(22 8 -2 3)))
    (should (equal (version-to-list "22_8beta3") '(22 8 -2 3)))
    (should (equal (version-to-list "6_9_30 Beta") '(6 9 30 -2)))
    (should (equal (version-to-list "6_9_30-Beta") '(6 9 30 -2)))
    (should (equal (version-to-list "6_9_30.Beta") '(6 9 30 -2)))
    (should (equal (version-to-list "6_9_30Beta") '(6 9 30 -2)))

    (let ((text-quoting-style 'grave))
      (should (equal
               (error-message-string (should-error (version-to-list "1_0__7_5")))
               "Invalid version syntax: `1_0__7_5'"))
      (should (equal
               (error-message-string (should-error (version-to-list "1_0prepre2")))
               "Invalid version syntax: `1_0prepre2'"))
      (should (equal
               (error-message-string (should-error (version-to-list "22.8X3")))
               "Invalid version syntax: `22.8X3'"))
      (should (equal
               (error-message-string (should-error (version-to-list "beta22_8alpha3")))
               "Invalid version syntax: `beta22_8alpha3' (must start with a number)")))))

(ert-deftest subr-test-version-list-< ()
  (should (version-list-< '(0) '(1)))
  (should (version-list-< '(0 9) '(1 0)))
  (should (version-list-< '(1 -1) '(1 0)))
  (should (version-list-< '(1 -2) '(1 -1)))
  (should (not (version-list-< '(1) '(0))))
  (should (not (version-list-< '(1 1) '(1 0))))
  (should (not (version-list-< '(1) '(1 0))))
  (should (not (version-list-< '(1 0) '(1 0 0)))))

(ert-deftest subr-test-version-list-= ()
  (should (version-list-= '(1) '(1)))
  (should (version-list-= '(1 0) '(1)))
  (should (not (version-list-= '(0) '(1)))))

(ert-deftest subr-test-version-list-<= ()
  (should (version-list-<= '(0) '(1)))
  (should (version-list-<= '(1) '(1)))
  (should (version-list-<= '(1 0) '(1)))
  (should (not (version-list-<= '(1) '(0)))))

(defun subr-test--backtrace-frames-with-backtrace-frame (base)
  "Reference implementation of `backtrace-frames'."
  (let ((idx 0)
        (frame nil)
        (frames nil))
    (while (setq frame (backtrace-frame idx base))
      (push frame frames)
      (setq idx (1+ idx)))
    (nreverse frames)))

(defun subr-test--frames-2 (base)
  (let ((_dummy nil))
    (progn ;; Add a few frames to top of stack
      (unwind-protect
          (cons (mapcar (pcase-lambda (`(,evald ,func ,args ,_))
                          `(,evald ,func ,@args))
                        (backtrace-frames base))
                (subr-test--backtrace-frames-with-backtrace-frame base))))))

(defun subr-test--frames-1 (base)
  (subr-test--frames-2 base))

(ert-deftest subr-test-backtrace-simple-tests ()
  "Test backtrace-related functions (simple tests).
This exercises `backtrace-frame', and indirectly `mapbacktrace'."
  ;; `mapbacktrace' returns nil
  (should (equal (mapbacktrace #'ignore) nil))
  ;; Unbound BASE is silently ignored
  (let ((unbound (make-symbol "ub")))
    (should (equal (backtrace-frame 0 unbound) nil))
    (should (equal (mapbacktrace #'error unbound) nil)))
  ;; First frame is backtrace-related function
  (should (equal (backtrace-frame 0) '(t backtrace-frame 0)))
  (let ((throw-args (lambda (&rest args) (throw 'ret args))))
    (should (equal (catch 'ret (mapbacktrace throw-args))
                   `(t mapbacktrace (,throw-args) nil))))
  ;; Past-end NFRAMES is silently ignored
  (should (equal (backtrace-frame most-positive-fixnum) nil)))

(ert-deftest subr-test-backtrace-integration-test ()
  "Test backtrace-related functions (integration test).
This exercises `backtrace-frame', `backtrace-frames', and
indirectly `mapbacktrace'."
  ;; Compare two implementations of backtrace-frames
  (let ((frame-lists (subr-test--frames-1 'subr-test--frames-2)))
    (should (equal (car frame-lists) (cdr frame-lists)))))

(ert-deftest subr-tests--string-match-p--blank ()
  "Test that [:blank:] matches horizontal whitespace, cf. Bug#25366."
  (should (equal (string-match-p "\\`[[:blank:]]\\'" " ") 0))
  (should (equal (string-match-p "\\`[[:blank:]]\\'" "\t") 0))
  (should-not (string-match-p "\\`[[:blank:]]\\'" "\n"))
  (should-not (string-match-p "\\`[[:blank:]]\\'" "a"))
  (should (equal (string-match-p "\\`[[:blank:]]\\'" "\N{HAIR SPACE}") 0))
  (should (equal (string-match-p "\\`[[:blank:]]\\'" "\u3000") 0))
  (should-not (string-match-p "\\`[[:blank:]]\\'" "\N{LINE SEPARATOR}")))

(ert-deftest subr-tests--dolist--wrong-number-of-args ()
  "Test that `dolist' doesn't accept wrong types or length of SPEC,
cf. Bug#25477."
  (dolist (lb '(nil t))
    (should-error (eval '(dolist (a)) lb)
                  :type 'wrong-number-of-arguments)
    (should-error (eval '(dolist (a () 'result 'invalid)) lb)
                  :type 'wrong-number-of-arguments)
    (should-error (eval '(dolist "foo") lb)
                  :type 'wrong-type-argument)))

(ert-deftest subr-tests-bug22027 ()
  "Test for https://debbugs.gnu.org/22027 ."
  (let ((default "foo") res)
    (cl-letf (((symbol-function 'read-string)
               (lambda (_prompt &optional _init _hist def _inher-input) def)))
      (setq res (read-passwd "pass: " 'confirm (mapconcat #'string default)))
      (should (string= default res)))))

(ert-deftest subr-tests--gensym ()
  "Test `gensym' behavior."
  (should (equal (symbol-name (let ((gensym-counter 0)) (gensym)))
                 "g0"))
  (should (eq (string-to-char (symbol-name (gensym))) ?g))
  (should (eq (string-to-char (symbol-name (gensym "X"))) ?X)))

(ert-deftest subr-tests--assq-delete-all ()
  "Test `assq-delete-all' behavior."
  (cl-flet ((new-list-fn
             ()
             (list (cons 'a 1) (cons 'b 2) (cons 'c 3) 'd (cons "foo" "bar"))))
    (should (equal (cdr (new-list-fn)) (assq-delete-all 'a (new-list-fn))))
    (should (equal (new-list-fn) (assq-delete-all 'd (new-list-fn))))
    (should (equal (new-list-fn) (assq-delete-all "foo" (new-list-fn))))))

(ert-deftest subr-tests--assoc-delete-all ()
  "Test `assoc-delete-all' behavior."
  (cl-flet ((new-list-fn
             ()
             (list (cons 'a 1) (cons 'b 2) (cons 'c 3) 'd (cons "foo" "bar"))))
    (should (equal (cdr (new-list-fn)) (assoc-delete-all 'a (new-list-fn))))
    (should (equal (new-list-fn) (assoc-delete-all 'd (new-list-fn))))
    (should (equal (butlast (new-list-fn))
                   (assoc-delete-all "foo" (new-list-fn))))))

(ert-deftest shell-quote-argument-%-on-w32 ()
  "Quoting of `%' in w32 shells isn't perfect.
See https://debbugs.gnu.org/cgi/bugreport.cgi?bug=19350."
  :expected-result :failed
  (skip-unless (and (fboundp 'w32-shell-dos-semantics)
                    (w32-shell-dos-semantics)))
  (let ((process-environment (append '("ca^=with-caret"
                                       "ca=without-caret")
                                     process-environment)))
    ;; It actually results in
    ;;    without-caret with-caret
    (should (equal (shell-command-to-string
                    (format "echo %s %s"
                            "%ca%"
                            (shell-quote-argument "%ca%")))
                   "without-caret %ca%"))))

(ert-deftest subr-tests-flatten-tree ()
  "Test `flatten-tree' behavior."
  (should (equal (flatten-tree '(1 (2 . 3) nil (4 5 (6)) 7))
                 '(1 2 3 4 5 6 7)))
  (should (equal (flatten-tree '((1 . 2)))
                 '(1 2)))
  (should (equal (flatten-tree '(1 nil 2))
                 '(1 2)))
  (should (equal (flatten-tree 42)
                 '(42)))
  (should (equal (flatten-tree t)
                 '(t)))
  (should (equal (flatten-tree nil)
                 nil))
  (should (equal (flatten-tree '((nil) ((((nil)))) nil))
                 nil))
  (should (equal (flatten-tree '(1 ("foo" "bar") 2))
                 '(1 "foo" "bar" 2))))

(ert-deftest subr--tests-letrec ()
  ;; Test that simple cases of `letrec' get optimized back to `let*'.
  (should (equal (macroexpand '(letrec ((subr-tests-var1 1)
                                        (subr-tests-var2 subr-tests-var1))
                                 (+ subr-tests-var1 subr-tests-var2)))
                 '(let* ((subr-tests-var1 1)
                         (subr-tests-var2 subr-tests-var1))
                    (+ subr-tests-var1 subr-tests-var2)))))

(defvar subr-tests--hook nil)

(ert-deftest subr-tests-add-hook-depth ()
  "Test the `depth' arg of `add-hook'."
  (setq-default subr-tests--hook nil)
  (add-hook 'subr-tests--hook 'f1)
  (add-hook 'subr-tests--hook 'f2)
  (should (equal subr-tests--hook '(f2 f1)))
  (add-hook 'subr-tests--hook 'f3 t)
  (should (equal subr-tests--hook '(f2 f1 f3)))
  (add-hook 'subr-tests--hook 'f4 50)
  (should (equal subr-tests--hook '(f2 f1 f4 f3)))
  (add-hook 'subr-tests--hook 'f5 -50)
  (should (equal subr-tests--hook '(f5 f2 f1 f4 f3)))
  (add-hook 'subr-tests--hook 'f6)
  (should (equal subr-tests--hook '(f5 f6 f2 f1 f4 f3)))
  ;; Make sure t is equivalent to 90.
  (add-hook 'subr-tests--hook 'f7 90)
  (add-hook 'subr-tests--hook 'f8 t)
  (should (equal subr-tests--hook '(f5 f6 f2 f1 f4 f3 f7 f8)))
  ;; Make sure nil is equivalent to 0.
  (add-hook 'subr-tests--hook 'f9 0)
  (add-hook 'subr-tests--hook 'f10)
  (should (equal subr-tests--hook '(f5 f10 f9 f6 f2 f1 f4 f3 f7 f8)))
  )

(ert-deftest ignore-error-tests ()
  (should (equal (ignore-error (end-of-file)
                   (read ""))
                 nil))
  (should (equal (ignore-error end-of-file
                   (read ""))
                 nil))
  (should-error (ignore-error foo
                  (read ""))))

(ert-deftest string-replace ()
  (should (equal (string-replace "foo" "bar" "zot")
                 "zot"))
  (should (equal (string-replace "foo" "bar" "foozot")
                 "barzot"))
  (should (equal (string-replace "foo" "bar" "barfoozot")
                 "barbarzot"))
  (should (equal (string-replace "zot" "bar" "barfoozot")
                 "barfoobar"))
  (should (equal (string-replace "z" "bar" "barfoozot")
                 "barfoobarot"))
  (should (equal (string-replace "zot" "bar" "zat")
                 "zat"))
  (should (equal (string-replace "azot" "bar" "zat")
                 "zat"))
  (should (equal (string-replace "azot" "bar" "azot")
                 "bar"))

  (should (equal (string-replace "azot" "bar" "foozotbar")
                 "foozotbar"))

  (should (equal (string-replace "fo" "bar" "lafofofozot")
                 "labarbarbarzot"))

  (should (equal (string-replace "\377" "x" "a\377b")
                 "axb"))
  (should (equal (string-replace "\377" "x" "a\377ø")
                 "axø"))
  (should (equal (string-replace (string-to-multibyte "\377") "x" "a\377b")
                 "axb"))
  (should (equal (string-replace (string-to-multibyte "\377") "x" "a\377ø")
                 "axø"))

  (should (equal (string-replace "ana" "ANA" "ananas") "ANAnas"))

  (should (equal (string-replace "a" "" "") ""))
  (should (equal (string-replace "a" "" "aaaaa") ""))
  (should (equal (string-replace "ab" "" "ababab") ""))
  (should (equal (string-replace "ab" "" "abcabcabc") "ccc"))
  (should (equal (string-replace "a" "aa" "aaa") "aaaaaa"))
  (should (equal (string-replace "abc" "defg" "abc") "defg"))

  (should (equal (should-error (string-replace "" "x" "abc"))
                 '(wrong-length-argument 0))))

(ert-deftest subr-replace-regexp-in-string ()
  (should (equal (replace-regexp-in-string "a+" "xy" "abaabbabaaba")
                 "xybxybbxybxybxy"))
  ;; FIXEDCASE
  (let ((case-fold-search t))
    (should (equal (replace-regexp-in-string "a+" "xy" "ABAABBABAABA")
                   "XYBXYBBXYBXYBXY"))
    (should (equal (replace-regexp-in-string "a+" "xy" "ABAABBABAABA" t)
                   "xyBxyBBxyBxyBxy"))
    (should (equal (replace-regexp-in-string
                    "a[bc]*" "xyz"
                    "a A ab AB Ab aB abc ABC Abc AbC aBc")
                   "xyz XYZ xyz XYZ Xyz xyz xyz XYZ Xyz Xyz xyz"))
    (should (equal (replace-regexp-in-string
                    "a[bc]*" "xyz"
                    "a A ab AB Ab aB abc ABC Abc AbC aBc" t)
                   "xyz xyz xyz xyz xyz xyz xyz xyz xyz xyz xyz")))
  (let ((case-fold-search nil))
    (should (equal (replace-regexp-in-string "a+" "xy" "ABAABBABAABA")
                   "ABAABBABAABA")))
  ;; group substitution
  (should (equal (replace-regexp-in-string
                  "a\\(b*\\)" "<\\1,\\&>" "babbcaabacbab")
                 "b<bb,abb>c<,a><b,ab><,a>cb<b,ab>"))
  (should (equal (replace-regexp-in-string
                  "x\\(?2:..\\)\\(?1:..\\)\\(..\\)\\(..\\)\\(..\\)"
                  "<\\3,\\5,\\4,\\1,\\2>" "yxabcdefghijkl")
                 "y<ef,ij,gh,cd,ab>kl"))
  ;; LITERAL
  (should (equal (replace-regexp-in-string
                  "a\\(b*\\)" "<\\1,\\&>" "babbcaabacbab" nil t)
                 "b<\\1,\\&>c<\\1,\\&><\\1,\\&><\\1,\\&>cb<\\1,\\&>"))
  (should (equal (replace-regexp-in-string
                  "a" "\\\\,\\?" "aba")
                 "\\,\\?b\\,\\?"))
  (should (equal (replace-regexp-in-string
                  "a" "\\\\,\\?" "aba" nil t)
                 "\\\\,\\?b\\\\,\\?"))
  ;; SUBEXP
  (should (equal (replace-regexp-in-string
                  "\\(a\\)\\(b*\\)c" "xy" "babbcdacd" nil nil 2)
                 "baxycdaxycd"))
  ;; START
  (should (equal (replace-regexp-in-string
                  "ab" "x" "abcabdabeabf" nil nil nil 4)
                 "bdxexf"))
  ;; An empty pattern matches once before every character.
  (should (equal (replace-regexp-in-string "" "x" "abc")
                 "xaxbxc"))
  (should (equal (replace-regexp-in-string "y*" "x" "abc")
                 "xaxbxc"))
  ;; replacement function
  (should (equal (replace-regexp-in-string
                  "a\\(b*\\)c"
                  (lambda (s)
                    (format "<%s,%s,%s,%s,%s>"
                            s
                            (match-beginning 0) (match-end 0)
                            (match-beginning 1) (match-end 1)))
                  "babbcaacabc")
                 "b<abbc,0,4,1,3>a<ac,0,2,1,1><abc,0,3,1,2>"))
  ;; anchors (bug#15107, bug#44861)
  (should (equal (replace-regexp-in-string "a\\B" "b" "a aaaa")
                 "a bbba"))
  (should (equal (replace-regexp-in-string "\\`\\|x" "z" "--xx--")
                 "z--zz--")))

(ert-deftest subr-match-substitute-replacement ()
  (with-temp-buffer
    (insert "Alpha Beta Gamma Delta Epsilon")
    (goto-char (point-min))
    (re-search-forward "B\\(..\\)a")
    (should (equal (match-substitute-replacement "carrot")
                   "Carrot"))
    (should (equal (match-substitute-replacement "<\\&>")
                   "<Beta>"))
    (should (equal (match-substitute-replacement "m\\1a")
                   "Meta"))
    (should (equal (match-substitute-replacement "ernin" nil nil nil 1)
                   "Bernina")))
  (let ((s "Tau Beta Gamma Delta Epsilon"))
    (string-match "B\\(..\\)a" s)
    (should (equal (match-substitute-replacement "carrot" nil nil s)
                   "Carrot"))
    (should (equal (match-substitute-replacement "<\\&>" nil nil s)
                   "<Beta>"))
    (should (equal (match-substitute-replacement "m\\1a" nil nil s)
                   "Meta"))
    (should (equal (match-substitute-replacement "ernin" nil nil s 1)
                   "Bernina"))))

(ert-deftest subr-tests--change-group-33341 ()
  (with-temp-buffer
    (buffer-enable-undo)
    (insert "0\n")
    (let ((g (prepare-change-group)))
      (activate-change-group g)
      (insert "b\n")
      (insert "c\n")
      (cancel-change-group g))
    (should (equal (buffer-string) "0\n"))
    (erase-buffer)
    (setq buffer-undo-list nil)
    (insert "0\n")
    (let ((g (prepare-change-group)))
      (activate-change-group g)
      (insert "b\n")
      (insert "c\n")
      (accept-change-group g))
    (should (equal (buffer-string) "0\nb\nc\n"))
    (undo-boundary)
    (undo)
    (should (equal (buffer-string) ""))))

(defvar subr--ordered nil)

(ert-deftest subr--add-to-ordered-list-eq ()
  (setq subr--ordered nil)
  (add-to-ordered-list 'subr--ordered 'b 2)
  (should (equal subr--ordered '(b)))
  (add-to-ordered-list 'subr--ordered 'c 3)
  (should (equal subr--ordered '(b c)))
  (add-to-ordered-list 'subr--ordered 'a 1)
  (should (equal subr--ordered '(a b c)))
  (add-to-ordered-list 'subr--ordered 'e)
  (should (equal subr--ordered '(a b c e)))
  (add-to-ordered-list 'subr--ordered 'd 4)
  (should (equal subr--ordered '(a b c d e)))
  (add-to-ordered-list 'subr--ordered 'e)
  (should (equal subr--ordered '(a b c d e)))
  (add-to-ordered-list 'subr--ordered 'b 5)
  (should (equal subr--ordered '(a c d b e))))


;;; Apropos.

(ert-deftest apropos-apropos-internal ()
  (should (equal (apropos-internal "^next-line$") '(next-line)))
  (should (>= (length (apropos-internal "^help")) 100))
  (should-not (apropos-internal "^test-a-missing-symbol-foo-bar-zot$")))

(ert-deftest apropos-apropos-internal/predicate ()
  (should (equal (apropos-internal "^next-line$" #'commandp) '(next-line)))
  (should (>= (length (apropos-internal "^help" #'commandp)) 15))
  (should-not (apropos-internal "^next-line$" #'keymapp)))


(defvar test-global-boundp)
(ert-deftest test-buffer-local-boundp ()
  (let ((buf (generate-new-buffer "boundp")))
    (with-current-buffer buf
      (setq-local test-boundp t))
    (setq test-global-boundp t)
    (should (buffer-local-boundp 'test-boundp buf))
    (should-not (buffer-local-boundp 'test-not-boundp buf))
    (should (buffer-local-boundp 'test-global-boundp buf))))

(ert-deftest test-replace-string-in-region ()
  (with-temp-buffer
    (insert "foo bar zot foobar")
    (should (= (replace-string-in-region "foo" "new" (point-min) (point-max))
               2))
    (should (equal (buffer-string) "new bar zot newbar")))

  (with-temp-buffer
    (insert "foo bar zot foobar")
    (should (= (replace-string-in-region "foo" "new" (point-min) 14)
               1))
    (should (equal (buffer-string) "new bar zot foobar")))

  (with-temp-buffer
    (insert "foo bar zot foobar")
    (should-error (replace-string-in-region "foo" "new" (point-min) 30)))

  (with-temp-buffer
    (insert "Foo bar zot foobar")
    (should (= (replace-string-in-region "Foo" "new" (point-min))
               1))
    (should (equal (buffer-string) "new bar zot foobar")))

  (with-temp-buffer
    (insert "foo bar baz")
    (should (= (replace-string-in-region "ba" "quux corge grault" (point-min))
               2))
    (should (equal (buffer-string)
                    "foo quux corge graultr quux corge graultz")))

  (with-temp-buffer
    (insert "foo bar bar")
    (should (= (replace-string-in-region " bar" "" (point-min) 8)
               1))
    (should (equal (buffer-string)
                    "foo bar"))))

(ert-deftest test-replace-regexp-in-region ()
  (with-temp-buffer
    (insert "foo bar zot foobar")
    (should (= (replace-regexp-in-region "fo+" "new" (point-min) (point-max))
               2))
    (should (equal (buffer-string) "new bar zot newbar")))

  (with-temp-buffer
    (insert "foo bar zot foobar")
    (should (= (replace-regexp-in-region "fo+" "new" (point-min) 14)
               1))
    (should (equal (buffer-string) "new bar zot foobar")))

  (with-temp-buffer
    (insert "foo bar zot foobar")
    (should-error (replace-regexp-in-region "fo+" "new" (point-min) 30)))

  (with-temp-buffer
    (insert "Foo bar zot foobar")
    (should (= (replace-regexp-in-region "Fo+" "new" (point-min))
               1))
    (should (equal (buffer-string) "new bar zot foobar")))

  (with-temp-buffer
    (insert "foo bar baz")
    (should (= (replace-regexp-in-region "ba." "quux corge grault" (point-min))
               2))
    (should (equal (buffer-string)
                    "foo quux corge grault quux corge grault")))

  (with-temp-buffer
    (insert "foo bar bar")
    (should (= (replace-regexp-in-region " bar" "" (point-min) 8)
               1))
    (should (equal (buffer-string)
                    "foo bar"))))

(ert-deftest test-with-existing-directory ()
  (let ((dir (make-temp-name "/tmp/not-exist-")))
    (let ((default-directory dir))
      (should-not (file-exists-p default-directory)))
    (with-existing-directory
      (should-not (equal dir default-directory))
      (should (file-exists-p default-directory)))))

(ert-deftest subr-test-internal--format-docstring-line ()
  (should
   (string= (let ((fill-column 70))
              (internal--format-docstring-line
               "In addition to any hooks its parent mode might have run, this \
mode runs the hook ‘foo-bar-baz-very-long-name-indeed-mode-hook’, as the final \
or penultimate step during initialization."))
            "In addition to any hooks its parent mode might have run, this mode
runs the hook ‘foo-bar-baz-very-long-name-indeed-mode-hook’, as the
final or penultimate step during initialization."))
  (should-error (internal--format-docstring-line "foo\nbar")))

(ert-deftest test-ensure-list ()
  (should (equal (ensure-list nil) nil))
  (should (equal (ensure-list :foo) '(:foo)))
  (should (equal (ensure-list '(1 2 3)) '(1 2 3))))

(ert-deftest test-alias-p ()
  (should-not (function-alias-p 1))

  (defun subr-tests--fun ())
  (should-not (function-alias-p 'subr-tests--fun))

  (defalias 'subr-tests--a 'subr-tests--b)
  (defalias 'subr-tests--b 'subr-tests--c)
  (should (equal (function-alias-p 'subr-tests--a)
                 '(subr-tests--b subr-tests--c)))

  (defalias 'subr-tests--d 'subr-tests--e)
  (defalias 'subr-tests--e 'subr-tests--d)
  (should-error (function-alias-p 'subr-tests--d))
  (should (equal (function-alias-p 'subr-tests--d t)
                 '(subr-tests--e))))

(ert-deftest test-readablep ()
  (should (readablep "foo"))
  (should-not (readablep (list (make-marker))))
  (should-not (readablep (make-marker))))

(ert-deftest test-print-unreadable-function ()
  ;; Check that problem with unwinding properly is fixed (bug#56773).
  (let* ((before nil)
         (after nil)
         (r (with-temp-buffer
              (setq before (current-buffer))
              (prog1 (readablep (make-marker))
                (setq after (current-buffer))))))
    (should (equal after before))
    (should (equal r nil))))

(ert-deftest test-string-lines ()
  (should (equal (string-lines "") '("")))
  (should (equal (string-lines "" t) '()))

  (should (equal (string-lines "foo") '("foo")))
  (should (equal (string-lines "foo\n") '("foo")))
  (should (equal (string-lines "foo\nbar") '("foo" "bar")))

  (should (equal (string-lines "foo" t) '("foo")))
  (should (equal (string-lines "foo\n" t) '("foo")))
  (should (equal (string-lines "foo\nbar" t) '("foo" "bar")))
  (should (equal (string-lines "foo\n\n\nbar" t) '("foo" "bar")))

  (should (equal (string-lines "foo" nil t) '("foo")))
  (should (equal (string-lines "foo\n" nil t) '("foo\n")))
  (should (equal (string-lines "foo\nbar" nil t) '("foo\n" "bar")))
  (should (equal (string-lines "foo\n\n\nbar" nil t)
                 '("foo\n" "\n" "\n" "bar")))

  (should (equal (string-lines "foo" t t) '("foo")))
  (should (equal (string-lines "foo\n" t t) '("foo\n")))
  (should (equal (string-lines "foo\nbar" t t) '("foo\n" "bar")))
  (should (equal (string-lines "foo\n\n\nbar" t t)
                 '("foo\n" "bar"))))

(ert-deftest test-keymap-parse-macros ()
  (should (equal (key-parse "C-x ( C-d C-x )") [24 40 4 24 41]))
  (should (equal (kbd "C-x ( C-d C-x )") "\^D"))
  (should (equal (kbd "C-x ( C-x )") "")))

(defvar subr-test--global)
(ert-deftest test-local-set-state ()
  (setq subr-test--global 1)
  (with-temp-buffer
    (setq-local subr-test--local 2)
    (let ((state (buffer-local-set-state subr-test--global 10
                                         subr-test--local 20
                                         subr-test--unexist 30)))
      (should (= subr-test--global 10))
      (should (= subr-test--local 20))
      (should (= subr-test--unexist 30))
      (buffer-local-restore-state state)
      (should (= subr-test--global 1))
      (should (= subr-test--local 2))
      (should-not (boundp 'subr-test--unexist)))))

(ert-deftest test-char-uppercase-p ()
  "Tests for `char-uppercase-p'."
  (dolist (c (list ?R ?S ?Ω ?Ψ))
    (should (char-uppercase-p c)))
  (dolist (c (list ?a ?b ?α ?β))
    (should-not (char-uppercase-p c))))

(ert-deftest test-plistp ()
  (should (plistp nil))
  (should-not (plistp 1))
  (should (plistp '(1 2)))
  (should-not (plistp '(1 . 2)))
  (should (plistp '(1 2 3 4)))
  (should-not (plistp '(1 2 3)))
  (should-not (plistp '(1 2 3 . 4)))
  (let ((cycle (list 1 2 3)))
    (nconc cycle cycle)
    (should-not (plistp cycle))))

(defun subr-tests--butlast-ref (list &optional n)
  "Reference implementation of `butlast'."
  (let ((m (or n 1))
        (len (length list)))
    (let ((r nil))
      (while (and list (> len m))
        (push (car list) r)
        (setq list (cdr list))
        (setq len (1- len)))
      (nreverse r))))

(ert-deftest subr-butlast ()
  (dolist (l '(nil '(a) '(a b) '(a b c) '(a b c d)))
    (dolist (n (cons nil (number-sequence -2 6)))
      (should (equal (butlast l n)
                     (subr-tests--butlast-ref l n))))))

(ert-deftest test-list-of-strings-p ()
  (should-not (list-of-strings-p 1))
  (should (list-of-strings-p nil))
  (should (list-of-strings-p '("a" "b")))
  (should-not (list-of-strings-p ["a" "b"]))
  (should-not (list-of-strings-p '("a" nil "b")))
  (should-not (list-of-strings-p '("a" "b" . "c"))))

(provide 'subr-tests)
;;; subr-tests.el ends here
