(uiop:define-package :lem/directory-mode/mode
  (:use :cl :lem)
  (:export :directory-mode
           :*directory-mode-keymap*))
(in-package :lem/directory-mode/mode)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (sb-ext:lock-package :lem/directory-mode/mode))

(define-major-mode directory-mode ()
    (:name "Directory"
     :keymap *directory-mode-keymap*)
  (setf (variable-value 'highlight-line :buffer (current-buffer)) nil))
