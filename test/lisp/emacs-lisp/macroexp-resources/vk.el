;;; vk.el --- test code for macroexp-tests    -*- lexical-binding: t -*-

;; Copyright (C) 2021-2024 Free Software Foundation, Inc.

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

(require 'macroexp)

(defmacro vk-variable-kind (var)
  (if (macroexp--dynamic-variable-p var) ''dyn ''lex))

(defvar vk-a 1)
(defconst vk-b 2)
(defvar vk-c)

(defun vk-f1 (x)
  (defvar vk-u1)
  (let ((vk-a 10)
        (vk-b 20)
        (vk-c 30)
        (vk-u1 40)
        (y 50))
    (ignore vk-a vk-b vk-c vk-u1 x y)
    (list
     (vk-variable-kind vk-a)            ; dyn
     (vk-variable-kind vk-b)            ; dyn
     (vk-variable-kind vk-c)            ; dyn
     (vk-variable-kind vk-u1)           ; dyn
     (vk-variable-kind x)               ; lex
     (vk-variable-kind y))))            ; lex

(eval-and-compile
  (defvar vk-u2)
  (defun vk-f2 (x)
    (defvar vk-v2)
    (let ((vk-u2 11)
          (vk-v2 12)
          (y 13))
      (ignore vk-u2 vk-v2 x y)
      (list
       (vk-variable-kind vk-u2)          ; dyn
       (vk-variable-kind vk-v2)          ; dyn
       (vk-variable-kind x)              ; lex
       (vk-variable-kind y)))))          ; lex

(eval-when-compile
  (defvar vk-u3)
  (defun vk-f3 (x)
    (defvar vk-v3)
    (let ((vk-a 23)
          (vk-b 24)
          (vk-u3 25)
          (vk-v3 26)
          (y 27))
      (ignore vk-a vk-b vk-u3 vk-v3 x y)
      (list
       (vk-variable-kind vk-a)          ; dyn
       (vk-variable-kind vk-b)          ; dyn
       (vk-variable-kind vk-u3)         ; dyn
       (vk-variable-kind vk-v3)         ; dyn
       (vk-variable-kind x)             ; lex
       (vk-variable-kind y)))))         ; lex

(defconst vk-val3 (eval-when-compile (vk-f3 0)))

(defconst vk-f4 '(lambda (x)
                   (defvar vk-v4)
                   (let ((vk-v4 31)
                         (y 32))
                     (ignore vk-v4 x y)
                     (list
                      (vk-variable-kind vk-a)   ; dyn
                      (vk-variable-kind vk-b)   ; dyn
                      (vk-variable-kind vk-v4)  ; dyn
                      (vk-variable-kind x)      ; dyn
                      (vk-variable-kind y)))))  ; dyn

(defconst vk-f5 '(closure (t) (x)
                   (defvar vk-v5)
                   (let ((vk-v5 41)
                         (y 42))
                     (ignore vk-v5 x y)
                     (list
                      (vk-variable-kind vk-a)   ; dyn
                      (vk-variable-kind vk-b)   ; dyn
                      (vk-variable-kind vk-v5)  ; dyn
                      (vk-variable-kind x)      ; lex
                      (vk-variable-kind y)))))  ; lex

(defun vk-f6 ()
  (eval '(progn
           (defvar vk-v6)
           (let ((vk-v6 51)
                 (y 52))
             (ignore vk-v6 y)
             (list
              (vk-variable-kind vk-a)        ; dyn
              (vk-variable-kind vk-b)        ; dyn
              (vk-variable-kind vk-v6)       ; dyn
              (vk-variable-kind vk-y))))))   ; dyn

(defun vk-f7 ()
  (eval '(progn
           (defvar vk-v7)
           (let ((vk-v7 51)
                 (y 52))
             (ignore vk-v7 y)
             (list
              (vk-variable-kind vk-a)        ; dyn
              (vk-variable-kind vk-b)        ; dyn
              (vk-variable-kind vk-v7)       ; dyn
              (vk-variable-kind vk-y))))     ; lex
        t))

(provide 'vk)
