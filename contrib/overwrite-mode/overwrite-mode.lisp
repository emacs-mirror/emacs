(defpackage :lem-overwrite-mode
  (:use :cl :lem))
(in-package :lem-overwrite-mode)

(define-key *global-keymap*
  "Insert" 'overwrite-mode)

(define-minor-mode overwrite-mode
    (:name "Overwrite"
     :global nil
     :enable-hook 'enable
     :disable-hook 'disable))

(defun hook-function (char)
  (declare (ignore char))
  (unless (end-line-p (current-point))
    (delete-next-char)))

(defun enable ()
  (add-hook (variable-value 'self-insert-before-hook)
            'hook-function))

(defun disable ()
  (remove-hook (variable-value 'self-insert-before-hook) 
               'hook-function))
