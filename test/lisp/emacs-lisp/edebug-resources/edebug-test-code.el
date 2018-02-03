;;; edebug-test-code.el --- Sample code for the Edebug test suite

;; Copyright (C) 2017-2018 Free Software Foundation, Inc.

;; Author: Gemini Lasswell

;; This file is part of GNU Emacs.

;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This file contains sample code used by edebug-tests.el.
;; Before evaluation, it will be preprocessed by
;; `edebug-tests-setup-code-file' which will remove all tags
;; between !'s and save their positions for use by the tests.

;;; Code:

(defun edebug-test-code-fac (n)
  !start!(if !step!(< 0 n)
      (* n (edebug-test-code-fac (1- n)))!mult!
    1))

(defun edebug-test-code-concat (a b flag)
  !start!(if flag!flag!
      !then-start!(concat a!then-a! b!then-b!)!then-concat!
      !else-start!(concat b!else-b! a!else-a!)!else-concat!)!if!)

(defun edebug-test-code-range (num)
  !start!(let ((index 0)
        (result nil))
    (while (< index num)!test!
      (push index result)!loop!
      (cl-incf index))!end-loop!
    (nreverse result)))

(defun edebug-test-code-choices (input)
  !start!(cond
   ((eq input 0) "zero")
   ((eq input 7) 42)
   (t !edebug!(edebug))))

(defvar edebug-test-code-total nil)

(defun edebug-test-code-multiply (times value)
  !start!(setq edebug-test-code-total 0)
  (cl-dotimes (index times)
    (setq edebug-test-code-total (+ edebug-test-code-total value))!setq!)
  edebug-test-code-total)

(defun edebug-test-code-format-vector-node (node)
  !start!(concat "["
          (apply 'concat (mapcar 'edebug-test-code-format-node node))!apply!
          "]"))

(defun edebug-test-code-format-list-node (node)
  !start!(concat "{"
          (apply 'concat (mapcar 'edebug-test-code-format-node node))!apply!
          "}"))

(defun edebug-test-code-format-node (node)
  !start!(cond
   (!vectorp!(vectorp node!vnode!)!vtest! !vbefore!(edebug-test-code-format-vector-node node))
   ((listp node) (edebug-test-code-format-list-node node))
   (t (format "%s" node))))

(defvar edebug-test-code-flavor "strawberry")

(defmacro edebug-test-code-with-flavor (new-flavor &rest body)
   (declare (debug (form body))
	    (indent 1))
  `(let ((edebug-test-code-flavor ,new-flavor))
     ,@body))

(defun edebug-test-code-try-flavors ()
  (let* (tried)
    (push edebug-test-code-flavor tried)
    !macro!(edebug-test-code-with-flavor "chocolate"
      (push edebug-test-code-flavor tried))
    tried)!end!)

(unless (featurep 'edebug-tests-nutty)!nutty!
   !setq!(setq edebug-test-code-flavor (car (edebug-test-code-try-flavors)))!end-setq!)!end-unless!

(cl-defgeneric edebug-test-code-emphasize (x))
(cl-defmethod edebug-test-code-emphasize ((x integer))
  !start!(format "The number is not %s or %s, but %s!"
                 (1+ x) (1- x) x))
(cl-defmethod edebug-test-code-emphasize ((x string))
  !start!(format "***%s***" x))

(defun edebug-test-code-use-methods ()
  (list
   !number!(edebug-test-code-emphasize 100)
   !string!(edebug-test-code-emphasize "yes")))

(defun edebug-test-code-make-lambda (n)
  (lambda (x) (+ x!x! n)))

(defun edebug-test-code-use-lambda ()
  !start!(mapcar (edebug-test-code-make-lambda 10) '(1 2 3)))

(defun edebug-test-code-circular-read-syntax ()
  '(#1=a . #1#))

(defun edebug-test-code-hash-read-syntax ()
  !start!(list #("abcd" 1 3 (face italic))
        #x01ff))

(defun edebug-test-code-empty-string-list ()
  !start!(list "")!step!)

(defun edebug-test-code-current-buffer ()
  !start!(with-current-buffer (get-buffer-create "*edebug-test-code-buffer*")
    !body!(format "current-buffer: %s" (current-buffer))))

(defun edebug-test-code-use-destructuring-bind ()
  (let ((two 2) (three 3))
    (cl-destructuring-bind (x . y) (cons two three) (+ x!x! y!y!))))

(provide 'edebug-test-code)
;;; edebug-test-code.el ends here
