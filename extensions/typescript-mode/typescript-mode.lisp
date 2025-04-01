(defpackage :lem-typescript-mode
  (:use :cl
        :lem
        :lem/language-mode
        :lem/language-mode-tools)
  (:export :typescript-mode))
(in-package :lem-typescript-mode)

(define-major-mode typescript-mode lem/language-mode:language-mode
    (:name "TypeScript"
     :keymap *typescript-mode-keymap*
     :syntax-table lem-js-mode::*js-syntax-table*
     :mode-hook *typescript-mode-hook*
     :formatter 'lem-js-mode::prettier)
  (setf (variable-value 'enable-syntax-highlight) t
        (variable-value 'indent-tabs-mode) nil
        (variable-value 'tab-width) 2
        (variable-value 'calc-indent-function) 'lem-js-mode::js-calc-indent
        (variable-value 'line-comment) "//"
        (variable-value 'beginning-of-defun-function) 'lem-js-mode::beginning-of-defun
        (variable-value 'end-of-defun-function) 'lem-js-mode::end-of-defun))

(define-file-type ("ts" "tsx") typescript-mode)
