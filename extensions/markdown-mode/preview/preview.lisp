(uiop:define-package :lem-markdown-mode/preview/preview
  (:use :cl :lem))
(in-package :lem-markdown-mode/preview/preview)

(define-command markdown-preview () ()
  (lem-markdown-mode/internal:preview (current-buffer) :html-buffer))

(defun render (string)
  (let ((3bmd-code-blocks:*code-blocks* t))
    (with-output-to-string (stream)
      (3bmd:parse-string-and-print-to-stream string stream))))
