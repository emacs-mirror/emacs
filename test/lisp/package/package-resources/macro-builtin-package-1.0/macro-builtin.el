;;; macro-builtin.el --- laksd                                  -*- lexical-binding: t; -*-

;; Author: Artur Malabarba <emacs@endlessparentheses.com>
;; Keywords: tools
;; Version: 1.0

;;; Code:

(require 'macro-builtin-aux)

(defmacro macro-builtin-1 ( &rest forms)
  "Description"
  `(progn ,@forms))

(defun macro-builtin-func ()
  ""
  (macro-builtin-1 'a 'b)
  (macro-builtin-aux-1 'a 'b))

(provide 'macro-builtin)
;;; macro-builtin.el ends here
