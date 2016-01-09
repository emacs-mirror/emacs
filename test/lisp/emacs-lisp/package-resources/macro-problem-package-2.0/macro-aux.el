;;; macro-aux.el --- laksd                                  -*- lexical-binding: t; -*-

;; Author: Artur Malabarba <emacs@endlessparentheses.com>

;;; Code:

(defmacro macro-aux-1 ( &rest forms)
  "Description"
  `(progn ,@forms))

(defmacro macro-aux-3 ( &rest _)
  "Description"
  90)

(provide 'macro-aux)
;;; macro-aux.el ends here
