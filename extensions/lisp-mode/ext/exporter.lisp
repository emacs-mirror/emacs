(defpackage :lem-lisp-mode/exporter
  (:use :cl :lem)
  (:export :lisp-add-export))
(in-package :lem-lisp-mode/exporter)

(defparameter *defpackage-names*
  '("defpackage"
    "cl:defpackage"
    "common-lisp:defpackage"
    "define-package"
    "uiop:define-package"))

(defun go-to-defpackage-form (point)
  (buffer-start point)
  (loop
    (unless (scan-lists point 1 -1 t)
      (return nil))
    (skip-whitespace-forward point)
    (if (member (symbol-string-at-point point) *defpackage-names*
                :test #'string-equal)
        (return (scan-lists point -1 1))
        (scan-lists point 1 1 t))))

(defun fresh-line* (point)
  (unless (with-point ((p point))
            (skip-whitespace-backward p t)
            (start-line-p p))
    (insert-character point #\newline)))

(defun go-to-defpackage-spec-form (point spec-name)
  (when (go-to-defpackage-form point)
    (with-point ((limit point))
      (when (form-offset limit 1)
        (cond ((search-forward-symbol point spec-name limit)
               (values point t))
              (t
               (form-offset point 1)
               (scan-lists point -1 -1)
               (fresh-line* point)
               (indent-line point)
               (insert-string point (format nil "(~A)" spec-name))
               (character-offset point -1)
               (values point nil)))))))

(defun get-defun-symbol (point)
  (flet ((fail () (editor-error "scan error")))
    (with-point ((point point))
      (lem-lisp-syntax:top-of-defun point)
      (with-point ((limit point))
        (unless (and (scan-lists limit 1 0 t)
                     (scan-lists point 1 -1 t limit)
                     (form-offset point 2)
                     (form-offset point -1))
          (fail))
        (cond ((syntax-open-paren-char-p (character-at point))
               (scan-lists point 1 -1)
               (skip-whitespace-forward point)
               (let ((symbol-name (symbol-string-at-point point)))
                 (cond ((null symbol-name)
                        (fail))
                       ((equal "setf" symbol-name)
                        (form-offset point 1)
                        (skip-whitespace-forward point)
                        (alexandria:if-let (name (symbol-string-at-point point))
                          name
                          (fail)))
                       (t symbol-name))))
              (t
               (symbol-string-at-point point)))))))

(define-command lisp-add-export (symbol-name)
    ((prompt-for-string "Export: " :initial-value (get-defun-symbol (current-point))))
  (with-point ((point (current-point) :left-inserting))
    (multiple-value-bind (point exists)
        (go-to-defpackage-spec-form point ":export")
      (declare (ignore exists))
      (unless point
        (editor-error "No defpackage was found in this file."))
      (with-point ((limit point :left-inserting))
        (scan-lists limit 1 1)
        (cond ((or (search-forward-symbol point symbol-name limit)
                   (search-forward-symbol point (format nil ":~A" symbol-name) limit)
                   (search-forward-symbol point (format nil "#:~A" symbol-name) limit))
               (editor-error "~A already exported" symbol-name))
              (t
               (with-point ((point point :left-inserting))
                 (scan-lists point 1 1)
                 (scan-lists point -1 -1)
                 (fresh-line* point)
                 (indent-line point)
                 (insert-string point (format nil ":~A" symbol-name)))
               (message "Add export: ~A" symbol-name)))))))
