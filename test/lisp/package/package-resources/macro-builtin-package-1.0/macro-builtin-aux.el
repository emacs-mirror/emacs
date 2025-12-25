;;; macro-builtin-aux.el --- laksd                                  -*- lexical-binding: t; -*-

;; Author: Artur Malabarba <emacs@endlessparentheses.com>

;;; Code:

(defun macro-builtin-aux-1 ( &rest forms)
  "Description"
  `(progn ,@forms))

(provide 'macro-builtin-aux)
;;; macro-builtin-aux.el ends here
