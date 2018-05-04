;;; test.el --- Unit test file for Semantic Emacs Lisp support.

;; Copyright (C) 2005-2018 Free Software Foundation, Inc.

;; Author: Eric M. Ludlam <eric@siege-engine.com>

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

;;; Require
;;
(require 'semantic)
(require 'eieio "../eieio")

;; tags encapsulated in eval-when-compile and eval-and-compile
;; should be expanded out into the outer environment.
(eval-when-compile
  (require 'semantic-imenu)
  )

(eval-and-compile
  (defconst const-1 nil)
  (defun function-1 (arg)
    nil)
  )

;;; Functions
;;
(defun a-defun (arg1 arg2 &optional arg3)
  "doc a"
  nil)

(defun a-defun-interactive (arg1 arg2 &optional arg3)
  "doc a that is a command"
  (interactive "R")
  nil)

(defun* a-defun* (arg1 arg2 &optional arg3)
  "doc a*"
  nil)

(defsubst a-defsubst (arg1 arg2 &optional arg3)
  "doc a-subst"
  nil)

(defmacro a-defmacro (arg1 arg2 &optional arg3)
  "doc a-macro"
  nil)

(define-overload a-overload (arg)
  "doc a-overload"
  nil)

;;; Methods
;;
(defmethod a-method ((obj some-class) &optional arg2)
  "Doc String for a method."
  (call-next-method))

(defgeneric a-generic (arg1 arg2)
  "General description of a-generic.")

;;; Advice
;;
(defadvice existing-function-to-advise (around test activate)
  "Do something special to this fcn."
  (ad-do-it))

;;; Variables
;;
(defvar a-defvar (cons 1 2)
  "Variable a")

(defvar a-defvar-star (cons 1 2)
  "*User visible var a")

(defconst a-defconst 'a "var doc const")

(defcustom a-defcustom nil
  "doc custom"
  :group 'a-defgroup
  :type 'boolean)

(defface a-defface 'bold
  "A face that is bold.")

(defimage ezimage-page-minus
  ((:type xpm :file "page-minus.xpm" :ascent center))
  "Image used for open files with stuff in them.")

;;; Autoloads
;;
(autoload (quote a-autoload) "somefile"
  "Non-interactive autoload." nil nil)

(autoload (quote a-autoload-interactive) "somefile"
"Interactive autoload." t nil)


(defgroup a-defgroup nil
  "Group for `emacs-lisp' regression-test")

;;; Classes
;;
(defclass a-class (a-parent)
  ((slot-1)
   (slot-2 :initarg :slot-2)
   (slot-3 :documentation "Doc about slot3")
   (slot-4 :type 'boolean)
   )
  "Doc String for class.")

(defclass a-class-abstract ()
  nil
  "Doc string for abstract class."
  :abstract t)

;;; Structures
;;
(defstruct (test-struct-1 :test 'equal)
  (slot-1 :equal 'eq)
  slot-2)

(defstruct test-struct-2
  slot-1
  slot-2)

;;; Semantic specific macros
;;
(define-lex a-lexer
  "Doc String"
  this
  that)

(define-mode-local-override a-overridden-function
  emacs-lisp-mode (tag)
  "A function that is overloaded."
  nil)

(defvar-mode-local emacs-lisp-mode a-mode-local-def
  "some value")


;;; Provide
;;
(provide 'test)
