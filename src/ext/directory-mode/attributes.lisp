(uiop:define-package :lem/directory-mode/attributes
  (:use :cl :lem)
  (:export :current-directory-attribute
           :file-size-attribute
           :file-date-attribute
           :file-attribute
           :directory-attribute
           :link-attribute))
(in-package :lem/directory-mode/attributes)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (sb-ext:lock-package :lem/directory-mode/attributes))

(define-attribute current-directory-attribute
  (t :bold t))

(define-attribute file-size-attribute
  (t))

(define-attribute file-date-attribute
  (t))

(define-attribute file-attribute
  (t))

(define-attribute directory-attribute
  (t :foreground :base0D :bold t))

(define-attribute link-attribute
  (t :foreground :base0B :bold t))
