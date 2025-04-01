(defpackage :lem-lisp-mode/self-insert-hook
  (:use :cl :lem))
(in-package :lem-lisp-mode/self-insert-hook)

(defmethod execute :before ((mode lem-lisp-mode/internal:lisp-mode) (command self-insert) argument)
  (when (eql #\) (get-self-insert-char))
    (unless (or (syntax-escape-char-p (character-at (current-point) -1))
                (in-string-or-comment-p (current-point)))
      (with-point ((point (current-point)))
        (unless (scan-lists point -1 1 t)
          (editor-error "No matching ')' (can be inserted with \"C-q )\")"))))))

(defmethod execute :after ((mode lem-lisp-mode/internal:lisp-mode) (command self-insert) argument)
  (when (eql #\space (get-self-insert-char))
    (lem-lisp-mode/autodoc:lisp-autodoc)))
