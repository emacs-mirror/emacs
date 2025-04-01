(defpackage :lem-documentation-mode
  (:use :cl
        :lem
        :lem-documentation-mode/internal)
  (:export :documentation-mode
           :*documentation-mode-keymap*
           :documentation-select
           :generate-markdown-file))
(in-package :lem-documentation-mode)

(define-major-mode documentation-mode ()
    (:name "Documentation"
     :keymap *documentation-mode-keymap*)
  (setf (variable-value 'line-wrap :buffer (current-buffer)) nil)
  (setf (buffer-read-only-p (current-buffer)) t))

(define-key *documentation-mode-keymap* "Return" 'documentation-select)

(define-command documentation-select () ()
  (select-command-at-point (current-point)))

;; TODO: Override describe-bindings when implementation to display all key bindings is complete.
(define-command documentation-describe-bindings () ()
  (let ((buffer (generate-buffer "*Documentation describe-bindings*")))
    (change-buffer-mode buffer 'documentation-mode)
    (switch-to-buffer buffer)))
