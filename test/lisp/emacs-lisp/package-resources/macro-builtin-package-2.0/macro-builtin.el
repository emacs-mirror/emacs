;;; macro-builtin.el --- laksd                                  -*- lexical-binding: t; -*-

;; Author: Artur Malabarba <emacs@endlessparentheses.com>
;; Keywords: tools
;; Version: 2.0

;;; Code:

(require 'macro-builtin-aux)

(defmacro macro-builtin-1 ( &rest forms)
  "Description"
  `(progn ,(cadr (car forms))))


(defun macro-builtin-func ()
  ""
  (list (macro-builtin-1 '1 'b)
        (macro-builtin-aux-1 'a 'b)))

(defmacro macro-builtin-3 (&rest _)
  "Description"
  10)

(defun macro-builtin-10-and-90 ()
  ""
  (list (macro-builtin-3 haha) (macro-builtin-aux-3 hehe)))

(provide 'macro-builtin)
;;; macro-builtin.el ends here
