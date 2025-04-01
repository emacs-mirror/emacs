(defpackage :lem-lisp-mode/organize-imports
  (:use :cl :lem))
(in-package :lem-lisp-mode/organize-imports)

;;; remove unused symbols in import-from clauses

(defun read-buffer-form (buffer)
  (with-point ((point (buffer-point buffer)))
    (buffer-start point)
    (with-open-stream (stream (make-buffer-input-stream point))
      (read stream))))

(defun defpackage-p (name)
  (member name '(defpackage uiop:define-package)
          :test #'string=))

(defun defpackage-form-p (form)
  (and (consp form)
       (defpackage-p (first form))
       (not (null (rest form)))))

(defun get-defpackage (buffer)
  (let ((form (read-buffer-form buffer)))
    (when (defpackage-form-p form)
      form)))

(defun defpackage-import-from-list (form)
  (destructuring-bind (package-name &rest options) (rest form)
    (declare (ignore package-name))
    (loop :for (option-name . option-values) :in options
          :when (string= :import-from option-name)
          :collect option-values)))

(defun search-symbol (point symbol-name &optional limit)
  (loop
    (unless (search-forward-symbol point symbol-name limit)
      (return))
    (unless (in-string-or-comment-p point)
      (return point))))

(defun find-symbol-in-buffer (buffer symbol-name)
  (with-point ((point (buffer-point buffer)))
    (buffer-start point)
    (form-offset point 2) ; skip defpackage and in-package forms
    (search-symbol point symbol-name)))

(defun annotation (symbol)
  (concatenate 'string "@" (string-downcase symbol)))

(defun collect-unused-import-symbols (buffer)
  (alexandria:when-let ((defpackage-form (get-defpackage buffer)))
    (loop :for (import-package . import-symbols) :in (defpackage-import-from-list defpackage-form)
          :append (loop :for import-symbol :in import-symbols
                        :unless (or (find-symbol-in-buffer buffer (string import-symbol))
                                    (find-symbol-in-buffer buffer (annotation import-symbol)))
                        :collect (list import-package import-symbol)))))

(defun read-symbol-name (symbol-string)
  ;; FIXME: prefix:name という形式でclientにパッケージがない時はエラーになり取り出せない
  (ignore-errors
    (let ((*read-eval* nil))
      (read-from-string symbol-string))))

(defun take-symbol (point)
  (skip-whitespace-forward point)
  (let ((string (symbol-string-at-point point)))
    (read-symbol-name string)))

(defun search-import-package (point package-name)
  (loop
    (unless (scan-lists point 1 -1 t)
      (return))
    (when (string= :import-from (take-symbol point))
      (form-offset point 1)
      (when (string-equal package-name (take-symbol point))
        (form-offset point 1)
        (return point)))
    (assert (scan-lists point 1 1))))

(defun delete-symbol-at-point (point)
  (with-point ((start point :left-inserting)
               (end point :left-inserting))
    (when (syntax-symbol-char-p (character-at start -1))
      (skip-symbol-backward start))
    (when (syntax-symbol-char-p (character-at end))
      (skip-symbol-forward end))
    (delete-between-points start end)
    (skip-whitespace-backward start)
    (skip-whitespace-forward end)
    (delete-between-points start end)
    (when (with-point ((start start)) (form-offset start 1))
      (insert-character start #\newline)
      (indent-line start))))

(defun remove-import-symbols (point symbol-name)
  (with-point ((point point :left-inserting))
    (loop
      (unless (form-offset point 1) (return))
      (when (string= symbol-name (read-symbol-name (symbol-string-at-point point)))
        (delete-symbol-at-point point)))))

(defun call-with-point-to-defpackage-form (buffer function)
  (with-point ((point (buffer-point buffer) :left-inserting))
    (buffer-start point)
    (scan-lists point 1 -1)
    (assert (defpackage-p (take-symbol point)))
    (funcall function point)))

(defmacro with-point-to-defpackage-form ((point buffer) &body body)
  `(call-with-point-to-defpackage-form ,buffer (lambda (,point) ,@body)))

(defun remove-import (buffer package-name symbol-names)
  (with-point-to-defpackage-form (point buffer)
    (loop :while (search-import-package point package-name)
          :do (dolist (symbol-name symbol-names)
                (with-point ((point point))
                  (remove-import-symbols point symbol-name)
                  (scan-lists point 1 1)))
              (scan-lists point 1 1)
              (move-point (current-point) point))))

(defun remove-unused-import (buffer)
  (loop :for (package-name . symbol-names) :in (collect-unused-import-symbols buffer)
        :do (remove-import buffer package-name symbol-names)))

(define-command lisp-organize-imports (buffer) ((current-buffer))
  (remove-unused-import buffer))

;; TODO:
;; - exportするシンボルはimport-fromから除外してはならない
