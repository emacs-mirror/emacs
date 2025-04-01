(in-package :lem-core)

(define-editor-variable highlight-line nil)
(define-editor-variable highlight-line-color nil)

(defun highlight-line-color ()
  (alexandria:if-let ((color (variable-value 'highlight-line-color)))
    color
    (guess-highlight-line-color)))

(defun guess-highlight-line-color ()
  (when (background-color)
    (let ((color (parse-color (background-color))))
      (multiple-value-bind (h s v)
          (rgb-to-hsv (color-red color)
                      (color-green color)
                      (color-blue color))
        (multiple-value-bind (r g b)
            (hsv-to-rgb h
                        s
                        (max 0 (- v 2)))
          (color-to-hex-string (make-color r g b)))))))
