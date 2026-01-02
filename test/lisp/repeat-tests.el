;;; repeat-tests.el --- Tests for repeat.el          -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2026 Free Software Foundation, Inc.

;; Author: Juri Linkov <juri@linkov.net>

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
(require 'repeat)

;; Key mnemonics: a - activate (enter, also b, s),
;;                c - continue (also d, t, also o, u),
;;                e - continue-only (not activate),
;;                g - global continue,
;;                q - quit (exit)

(defvar repeat-tests-calls nil)

(defun repeat-tests-call-a (&optional arg)
  (interactive "p")
  (push `(,arg a) repeat-tests-calls))

(defun repeat-tests-call-b (&optional arg)
  (interactive "p")
  (push `(,arg b) repeat-tests-calls))

(defun repeat-tests-call-c (&optional arg)
  (interactive "p")
  (push `(,arg c) repeat-tests-calls))

(defun repeat-tests-call-d (&optional arg)
  (interactive "p")
  (push `(,arg d) repeat-tests-calls))

(defun repeat-tests-call-e (&optional arg)
  (interactive "p")
  (push `(,arg e) repeat-tests-calls))

(defun repeat-tests-call-g (&optional arg)
  (interactive "p")
  (push `(,arg g) repeat-tests-calls))

(defun repeat-tests-call-o (&optional arg)
  (interactive "p")
  (push `(,arg o) repeat-tests-calls))

(defun repeat-tests-call-q (&optional arg)
  (interactive "p")
  (push `(,arg q) repeat-tests-calls))

(defun repeat-tests-call-s (&optional arg)
  (interactive "p")
  (push `(,arg s) repeat-tests-calls))

(defun repeat-tests-call-t (&optional arg)
  (interactive "p")
  (push `(,arg t) repeat-tests-calls))

(defun repeat-tests-call-u (&optional arg)
  (interactive "p")
  (push `(,arg u) repeat-tests-calls))

;; Global keybindings
(defvar-keymap repeat-tests-global-map
  :doc "Keymap for keys that initiate repeating sequences."
  "C-x w a" 'repeat-tests-call-a
  "C-M-a"   'repeat-tests-call-a
  "C-M-b"   'repeat-tests-call-b
  "C-M-e"   'repeat-tests-call-e
  "C-M-g"   'repeat-tests-call-g
  "C-M-o"   'repeat-tests-call-o
  "C-M-s"   'repeat-tests-call-s
  "C-M-u"   'repeat-tests-call-u)

(defvar-keymap repeat-tests-another-repeat-map
  :doc "Keymap for repeating other sequences."
  :repeat ( :enter    (repeat-tests-call-s)
            :continue (repeat-tests-call-e
                       repeat-tests-call-o
                       repeat-tests-call-u)
            :hints    ((repeat-tests-call-t . "test")
                       (repeat-tests-call-o . "another test")))
  "s"     'ignore ;; for non-nil repeat-check-key only
  "t"     'repeat-tests-call-t
  "C-M-o" 'repeat-tests-call-o
  "C-M-u" 'repeat-tests-call-u)

(defvar-keymap repeat-tests-repeat-map
  :doc "Keymap for repeating sequences."
  :repeat ( :enter    (repeat-tests-call-a)
            :continue (repeat-tests-call-e
                       repeat-tests-call-o)
            :exit     (repeat-tests-call-q))
  "a"     'ignore ;; for non-nil repeat-check-key only
  "c"     'repeat-tests-call-c
  "d"     'repeat-tests-call-d
  "C-M-o" 'repeat-tests-call-o
  "q"     'repeat-tests-call-q)

;; Test using a variable instead of the symbol:
(put 'repeat-tests-call-b 'repeat-map repeat-tests-repeat-map)

(put 'repeat-tests-call-g 'repeat-continue t)

(defmacro with-repeat-mode (map &rest body)
  "Create environment for testing `repeat-mode'."
  (declare (indent 1) (debug (symbol body)))
  `(unwind-protect
       (progn
         (repeat-mode +1)
         (with-temp-buffer
           (save-window-excursion
             ;; `execute-kbd-macro' applied to window only
             (set-window-buffer nil (current-buffer))
             (use-local-map ,map)
             ,@body)))
     (repeat-mode -1)
     (use-local-map nil)))

(defun repeat-tests--check (keys calls inserted)
  (setq repeat-tests-calls nil)
  (delete-region (point-min) (point-max))
  (execute-kbd-macro (kbd keys))
  (should (equal (nreverse repeat-tests-calls) calls))
  ;; Check for self-inserting keys
  (should (equal (buffer-string) inserted)))

(ert-deftest repeat-tests-check-key ()
  (with-repeat-mode repeat-tests-global-map
    (let ((repeat-echo-function 'ignore))
      (let ((repeat-check-key t))
        (repeat-tests--check
         "C-x w a c d z"
         '((1 a) (1 c) (1 d)) "z")
        (repeat-tests--check
         "C-M-a c d z"
         '((1 a) (1 c) (1 d)) "z")
        (repeat-tests--check
         "C-M-b c d z"
         '((1 b)) "cdz")
        (unwind-protect
            (progn
              (put 'repeat-tests-call-b 'repeat-check-key 'no)
              (repeat-tests--check
               "C-M-b c d z"
               '((1 b) (1 c) (1 d)) "z"))
          (put 'repeat-tests-call-b 'repeat-check-key nil)))
      (let ((repeat-check-key nil))
        (repeat-tests--check
         "C-M-b c d z"
         '((1 b) (1 c) (1 d)) "z")
        (unwind-protect
            (progn
              (put 'repeat-tests-call-b 'repeat-check-key t)
              (repeat-tests--check
               "C-M-b c d z"
               '((1 b)) "cdz"))
          (put 'repeat-tests-call-b 'repeat-check-key nil))))))

(ert-deftest repeat-tests-exit-command ()
  (with-repeat-mode repeat-tests-global-map
    (let ((repeat-echo-function 'ignore))
      ;; 'c' doesn't continue since 'q' exited
      (repeat-tests--check
       "C-x w a c d q c"
       '((1 a) (1 c) (1 d) (1 q)) "c"))))

(ert-deftest repeat-tests-exit-key ()
  (with-repeat-mode repeat-tests-global-map
    (let ((repeat-echo-function 'ignore))
      (let ((repeat-exit-key nil))
        (repeat-tests--check
         "C-x w a c d c RET z"
         '((1 a) (1 c) (1 d) (1 c)) "\nz"))
      (let ((repeat-exit-key [return]))
        (repeat-tests--check
         "C-x w a c d c <return> z"
         '((1 a) (1 c) (1 d) (1 c)) "z")))))

(ert-deftest repeat-tests-keep-prefix ()
  (with-repeat-mode repeat-tests-global-map
    (let ((repeat-echo-function 'ignore))
      (repeat-tests--check
       "C-x w a c d c z"
       '((1 a) (1 c) (1 d) (1 c)) "z")
      (let ((repeat-keep-prefix nil))
        (repeat-tests--check
         "C-2 C-x w a c d c z"
         '((2 a) (1 c) (1 d) (1 c)) "z")
        (repeat-tests--check
         "C-2 C-x w a C-3 z"
         '((2 a)) "zzz"))
      ;; Fixed in bug#51281 and bug#55986
      (let ((repeat-keep-prefix t))
        ;; Re-enable to take effect.
        (repeat-mode -1) (repeat-mode +1)
        (repeat-tests--check
         "C-2 C-x w a c d c z"
         '((2 a) (2 c) (2 d) (2 c)) "z")
        ;; Unimplemented feature (maybe unnecessary):
        ;; (repeat-tests--check
        ;;  "C-2 C-x w a C-1 C-2 c d C-3 C-4 c z"
        ;;  '((2 a) (12 c) (12 d) (34 c)) "z")
        ))))

;; TODO: :tags '(:expensive-test)  for repeat-exit-timeout

(ert-deftest repeat-tests-continue ()
  (with-repeat-mode repeat-tests-global-map
    (let ((repeat-echo-function 'ignore)
          (repeat-check-key nil))
      ;; Global 'C-M-g' used as continue
      (repeat-tests--check
       "C-M-a c C-M-g c z"
       '((1 a) (1 c) (1 g) (1 c)) "z")
      ;; 'C-M-e' and 'C-M-o' used as continue
      (repeat-tests--check
       "C-M-a c C-M-e C-M-o c z"
       '((1 a) (1 c) (1 e) (1 o) (1 c)) "z")
      ;; 'C-M-e' should not activate
      (repeat-tests--check
       "C-M-e c z"
       '((1 e)) "cz")
      ;; 'C-M-o' should also activate
      (repeat-tests--check
       "C-M-o c z"
       '((1 o) (1 c)) "z"))))

(ert-deftest repeat-tests-continue-another ()
  (with-repeat-mode repeat-tests-global-map
    (let ((repeat-echo-function 'ignore)
          (repeat-check-key nil))
      ;; First test without 'C-M-O'
      (repeat-tests--check
       "C-M-s t t z"
       '((1 s) (1 t) (1 t)) "z")
      ;; 'C-M-e' and 'C-M-u' used as continue
      (repeat-tests--check
       "C-M-s t C-M-e C-M-u t z"
       '((1 s) (1 t) (1 e) (1 u) (1 t)) "z")
      ;; 'C-M-e' should not activate
      (repeat-tests--check
       "C-M-e t z"
       '((1 e)) "tz")
      ;; 'C-M-u' should also activate
      (repeat-tests--check
       "C-M-u t z"
       '((1 u) (1 t)) "z")
      ;; 'C-M-o' shared with another map should continue current map
      (repeat-tests--check
       "C-M-s t C-M-o C-M-o t z"
       '((1 s) (1 t) (1 o) (1 o) (1 t)) "z")
      (repeat-tests--check
       "C-M-a c C-M-o C-M-o c z"
       '((1 a) (1 c) (1 o) (1 o) (1 c)) "z"))))

(ert-deftest repeat-tests-hints ()
  (with-repeat-mode repeat-tests-global-map
    (let ((repeat-echo-function 'ignore)
          (repeat-check-key nil))
      (execute-kbd-macro (kbd "C-M-s"))
      (should (equal (repeat-echo-message-string (repeat-get-map repeat-in-progress))
                     #("Repeat with s, [T]est, C-M-o (an[O]ther test), C-M-u"
                       12 13
                       (face read-multiple-choice-face)
                       23 28
                       (face read-multiple-choice-face)
                       47 52
                       (face read-multiple-choice-face)))))))


(require 'use-package)

(defun repeat-tests-bind-call-a (&optional arg)
  (interactive "p")
  (push `(,arg a) repeat-tests-calls))

(defun repeat-tests-bind-call-c (&optional arg)
  (interactive "p")
  (push `(,arg c) repeat-tests-calls))

(defun repeat-tests-bind-call-d (&optional arg)
  (interactive "p")
  (push `(,arg d) repeat-tests-calls))

(defun repeat-tests-bind-call-o (&optional arg)
  (interactive "p")
  (push `(,arg o) repeat-tests-calls))

(defun repeat-tests-bind-call-q (&optional arg)
  (interactive "p")
  (push `(,arg q) repeat-tests-calls))

(ert-deftest repeat-tests-bind-keys ()
  (defvar repeat-tests-bind-keys-map (make-sparse-keymap))
  (bind-keys
   :map repeat-tests-bind-keys-map
   ("C-M-a" . repeat-tests-bind-call-a)
   ("C-M-o" . repeat-tests-bind-call-o)
   :repeat-map repeat-tests-bind-keys-repeat-map
   :continue-only
   ("c"     . repeat-tests-bind-call-c)
   ;; :continue
   ("C-M-o" . repeat-tests-bind-call-o)
   :exit
   ("q"     . repeat-tests-bind-call-q))

  ;; TODO: it seems there is no :entry, so need to do explicitly:
  (put 'repeat-tests-bind-call-a 'repeat-map 'repeat-tests-bind-keys-repeat-map)

  (with-repeat-mode repeat-tests-bind-keys-map
    (let ((repeat-echo-function 'ignore)
          (repeat-check-key nil))
      ;; 'C-M-o' used as continue
      (repeat-tests--check
       "C-M-a c C-M-o c z"
       '((1 a) (1 c) (1 o) (1 c)) "z")
      ;; 'C-M-o' should not activate
      (repeat-tests--check
       "C-M-o c z"
       '((1 o)) "cz")
      ;; 'q' should exit
      (repeat-tests--check
       "C-M-a c q c"
       '((1 a) (1 c) (1 q)) "c"))))

(provide 'repeat-tests)
;;; repeat-tests.el ends here
