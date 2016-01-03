;;; macro-aux.el --- laksd                                  -*- lexical-binding: t; -*-

;; Author: Artur Malabarba <emacs@endlessparentheses.com>

;;; Code:

(defun macro-aux-1 ( &rest forms)
  "Description"
  `(progn ,@forms))

(provide 'macro-aux)
;;; macro-aux.el ends here
