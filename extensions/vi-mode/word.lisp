(defpackage :lem-vi-mode/word
  (:use :cl
        :lem)
  (:import-from :lem-vi-mode/options
                :option-raw-value)
  (:export :forward-word-begin
           :forward-word-end
           :backward-word-begin
           :backward-word-end
           :word-char-p
           :word-char-type
           :broad-word-char-type
           :blank-char-p))
(in-package :lem-vi-mode/word)

(defun word-char-p (char)
  (funcall (cdr (option-raw-value "iskeyword"))
           char))

(defun blank-char-p (char)
  (and (member char '(#\Newline #\Space #\Tab))
       t))

(defun non-blank-char-p (char)
  (not (blank-char-p char)))

(defun word-char-type (char)
  (when char
    (cond
      ((word-char-p char) :word)
      ((blank-char-p char) :blank)
      (t :non-word))))

(defun non-broad-word-char-p (char)
  (funcall (cdr (option-raw-value "isseparator"))
           char))

(defun broad-word-char-type (char)
  (when char
    (cond
      ((blank-char-p char) :blank)
      ((non-broad-word-char-p char) :non-word)
      (t :word))))

(defun forward-word-begin (char-type-fn &optional (point (current-point)))
  (flet ((point-char () (character-at point)))
    (let ((type (funcall char-type-fn (point-char))))
      (skip-chars-forward point
                          (lambda (char)
                            (eq type (funcall char-type-fn char)))))
    (when (blank-char-p (point-char))
      (skip-chars-forward point #'blank-char-p))))

(defun forward-word-end (char-type-fn &optional (point (current-point)))
  (flet ((point-char () (character-at point)))
    ;; Boundary
    (unless (eq (funcall char-type-fn (point-char))
                (funcall char-type-fn (character-at point 1)))
      (character-offset point 1))
    (when (blank-char-p (point-char))
      (skip-chars-forward point #'blank-char-p))
    (let ((type (funcall char-type-fn (point-char))))
      (or (zerop
           (skip-chars-forward point
                               (lambda (char)
                                 (eq type (funcall char-type-fn char)))))
          (character-offset point -1)))))

(defun backward-word-begin (char-type-fn &optional (point (current-point)))
  (flet ((point-char () (character-at point)))
    ;; Boundary
    (unless (eq (funcall char-type-fn (point-char))
                (funcall char-type-fn (character-at point -1)))
      (character-offset point -1))
    (when (blank-char-p (point-char))
      (skip-chars-backward point #'blank-char-p)
      (character-offset point -1))
    (let ((type (funcall char-type-fn (point-char))))
      (skip-chars-backward point
                           (lambda (char)
                             (eq type (funcall char-type-fn char)))))))

(defun backward-word-end (char-type-fn &optional (point (current-point)))
  (flet ((point-char () (character-at point)))
    ;; Not at boundary
    (when (eq (funcall char-type-fn (point-char))
              (funcall char-type-fn (character-at point -1)))
      (let ((type (funcall char-type-fn (point-char))))
        (skip-chars-backward point
                             (lambda (char)
                               (eq type (funcall char-type-fn char))))))
    (character-offset point -1)
    (when (blank-char-p (point-char))
      (skip-chars-backward point #'blank-char-p)
      (character-offset point -1))))
