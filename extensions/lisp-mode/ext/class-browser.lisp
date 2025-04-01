(defpackage :lem-lisp-mode/class-browser
  (:use :cl :lem :lem-lisp-mode/internal)
  (:export :display-class-inheritance-tree
           :lisp-browse-class-as-tree))
(in-package :lem-lisp-mode/class-browser)

(defmethod display-class-inheritance-tree (buffer-name class-name)
  (editor-error "Unimplemented"))

(define-command lisp-browse-class-as-tree () ()
  (check-connection)
  (let ((class-name (or (symbol-string-at-point (current-point))
                        (prompt-for-symbol-name "Class name: "))))
    (display-class-inheritance-tree "*Class Browser*" class-name)))
