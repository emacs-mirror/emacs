(in-package :lem-core)

(define-major-mode lem/buffer/fundamental-mode:fundamental-mode nil
    (:name "Fundamental"))

(defvar *global-keymap* (make-keymap :name '*global-keymap*))

(define-global-mode emacs-mode ()
  (:name "emacs"
   :keymap *global-keymap*))
