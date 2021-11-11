;;; macro-builtin-aux.el --- laksd                                  -*- lexical-binding: t; -*-

;; Author: Artur Malabarba <emacs@endlessparentheses.com>

;;; Code:

(defmacro macro-builtin-aux-1 ( &rest forms)
  "Description"
  `(progn ,@forms))

(defmacro macro-builtin-aux-3 ( &rest _)
  "Description"
  90)

(provide 'macro-builtin-aux)
;;; macro-builtin-aux.el ends here
