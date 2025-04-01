(uiop:define-package :lem/directory-mode
  (:use :cl)
  (:use-reexport :lem/directory-mode/commands)
  (:use-reexport :lem/directory-mode/mode)
  (:import-from :lem/directory-mode/internal
                :*default-sort-method*)
  (:export :*default-sort-method*
           :*directory-mode-keymap*))
(in-package :lem/directory-mode)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (sb-ext:lock-package :lem/directory-mode))

(setf lem:*find-directory-function* 'lem/directory-mode/internal:directory-buffer)
