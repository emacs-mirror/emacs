;;; macro-problem.el --- laksd                                  -*- lexical-binding: t; -*-

;; Author: Artur Malabarba <emacs@endlessparentheses.com>
;; Keywords: tools
;; Version: 1.0

;;; Code:

(require 'macro-aux)

(defmacro macro-problem-1 ( &rest forms)
  "Description"
  `(progn ,@forms))

(defun macro-problem-func ()
  ""
  (macro-problem-1 'a 'b)
  (macro-aux-1 'a 'b))

(provide 'macro-problem)
;;; macro-problem.el ends here
