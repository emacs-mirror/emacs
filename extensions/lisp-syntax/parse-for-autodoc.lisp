(defpackage :lem-lisp-syntax.parse-for-autodoc
  (:use :cl :lem)
  (:export :parse-for-autodoc)
  #+sbcl
  (:lock t))
(in-package :lem-lisp-syntax.parse-for-autodoc)

(defvar *cursor-marker* 'micros::%cursor-marker%)

(defun parsing-safe-p (point)
  (not (in-string-or-comment-p point)))

(defun parse-for-autodoc (point &optional (*cursor-marker* *cursor-marker*))
  (and (parsing-safe-p point)
       (parse-form-upto-toplevel point 10)))

(defun compare-char (point offset fn &optional unescape)
  (and (funcall fn (character-at point offset))
       (if unescape
           (not (syntax-escape-char-p
                 (character-at point (1- offset))))
           t)))

(defun parse-form-upto-toplevel (point &optional limit-levels)
  (with-point ((point point))
    (let ((suffix (list *cursor-marker*)))
      (cond ((compare-char point 0 #'syntax-open-paren-char-p t)
             (and (form-offset point 1)
                  (push "" suffix)))
            ((or (start-line-p point)
                 (compare-char point -1 #'syntax-space-char-p t))
             (push "" suffix))
            ((and (not (compare-char point 0 #'syntax-symbol-char-p))
                  (compare-char point -1 #'syntax-open-paren-char-p t))
             (push "" suffix))
            (t
             (skip-symbol-forward point)))
      (with-point ((end point))
        (loop :repeat (or limit-levels most-positive-fixnum)
              :while (scan-lists point -1 1 t)
              :do (when (start-line-p point) (return)))
        (scan-lists point 1 -1 t)
        (parse-region point end suffix)))))

(defun parse-region (start end suffix)
  (with-point ((tmp start))
    (labels ((f (start end)
               (let ((p start)
                     (sexp '()))
                 (loop
                   (skip-space-and-comment-forward p)
                   (when (point<= end p)
                     (dolist (s suffix) (push s sexp))
                     (setq suffix nil)
                     (return-from f (nreverse sexp)))
                   (let ((c (character-at p)))
                     (cond ((syntax-open-paren-char-p c)
                            (character-offset p 1)
                            (push (f p end) sexp))
                           ((syntax-closed-paren-char-p c)
                            (character-offset p 1)
                            (return))
                           (t
                            (move-point tmp p)
                            (unless (form-offset p 1) (return))
                            (push (points-to-string tmp p) sexp)))))
                 (nreverse sexp))))
      (f start end))))
