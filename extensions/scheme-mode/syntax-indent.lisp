(defpackage :lem-scheme-syntax.indent
  (:use :cl :lem)
  (:export :set-indentation
           :calc-indent))
(in-package :lem-scheme-syntax.indent)

(defparameter *body-indent* 2)
(defparameter *max-depth* 4)

(defun make-static-indent-table ()
  (let ((table (make-hash-table :test 'equal)))
    (mapc (lambda (elt)
            (let ((name (car elt))
                  (method (if (stringp (cdr elt))
                              (gethash (cdr elt) table)
                              (cadr elt))))
              (setf (gethash name table) method)))
          (lem-scheme-syntax.data:get-scheme-indentation-data))
    table))
(defvar *static-indent-table* (make-static-indent-table))

(defvar *lambda-list-indentation* t)
(defvar *lambda-list-keyword-parameter-alignment* t)
(defvar *lambda-list-keyword-alignment* t)

(defun get-indentation (name)
  (gethash name *static-indent-table*))

(defun set-indentation (name method)
  (setf (gethash name *static-indent-table*) method))

(defun lambda-list-keyword-p (name)
  (and (stringp name)
       (find name
             '("&optional" "&rest" "&key" "&allow-other-keys" "&aux"
               "&whole" "&body" "&environment")
             :test #'string-equal)))

(defun search-lambda-list-keyword (p)
  (loop
    (unless (form-offset p -1)
      (return nil))
    (when (lambda-list-keyword-p (symbol-string-at-point p))
      (return p))))

(defun compute-indent-lambda-list (path indent-point sexp-column)
  (declare (ignore path))
  (unless *lambda-list-indentation*
    (return-from compute-indent-lambda-list (1+ sexp-column)))
  (with-point ((p indent-point))
    (cond
      ((progn
         (back-to-indentation p)
         (lambda-list-keyword-p (symbol-string-at-point p)))
       (if *lambda-list-keyword-alignment*
           (if (search-lambda-list-keyword p)
               (point-column p)
               (1+ sexp-column))
           (1+ sexp-column)))
      (t
       (cond
         ((search-lambda-list-keyword p)
          (if *lambda-list-keyword-parameter-alignment*
              (if (looking-at p "[\\w&]+\\s*$")
                  (point-column p)
                  (+ 1 (point-column (form-offset p 1))))
              (+ 2 (point-column p))))
         (t
          (1+ sexp-column)))))))

(defun compute-indent-integer-method (method path indent-point sexp-column)
  (declare (ignore indent-point))
  (cond ((cdr path)
         'default-indent)
        ((<= (car path) method)
         (+ sexp-column 4))
        (t
         (+ sexp-column *body-indent*))))

(defun compute-indent-symbol-method (method path indent-point sexp-column)
  (funcall method path indent-point sexp-column))

(defun compute-indent-complex-method (method path indent-point sexp-column)
  (loop
    :named exit
    :for pathrest :on path
    :for n := (1- (car pathrest))
    :do (let ((restp nil))
          (loop
            (let ((method1 (car method)))
              (cond ((and restp
                          (not (or (consp method1)
                                   (and (symbolp method1)
                                        (not (member method1 '(&rest &body &whole &lambda)))))))
                     (return-from exit
                       'default-indent))
                    ((eq method1 '&body)
                     (return-from exit
                       (if (null (cdr pathrest))
                           (+ sexp-column *body-indent*)
                           'default-indent)))
                    ((eq method1 '&rest)
                     (setf restp (> n 0))
                     (setf n 0)
                     (pop method))
                    ((> n 0)
                     (decf n)
                     (pop method))
                    ((eq method1 'nil)
                     (return-from exit
                       'default-indent))
                    ((eq method1 '&lambda)
                     (return-from exit
                       (cond ((null (cdr pathrest))
                              (+ sexp-column 4))
                             (t
                              (compute-indent-lambda-list path indent-point sexp-column))
                             ;; ((null (cddr pathrest))
                             ;;  (compute-indent-lambda-list path indent-point sexp-column))
                             ;; (t
                             ;;  'default-indent)
                             )))
                    ((integerp method1)
                     (return-from exit
                       (if (null (cdr pathrest))
                           (+ sexp-column method1)
                           'default-indent)))
                    ((symbolp method1)
                     (return-from exit
                       (compute-indent-symbol-method method1 path indent-point sexp-column)))
                    ;; (&whole ...)
                    ((not (null (cdr pathrest)))
                     (setf method (cddr method1))
                     (return))
                    (t
                     (return-from exit
                       (let ((method1 (cadr method1)))
                         (cond (restp
                                'default-indent)
                               ((eq method1 'nil)
                                'default-indent)
                               ((integerp method1)
                                (+ sexp-column method1))
                               (t
                                (compute-indent-symbol-method
                                 method1 path indent-point sexp-column))))))))))))

(defun compute-indent-method (method path indent-point sexp-column)
  (funcall (etypecase method
             (integer #'compute-indent-integer-method)
             (symbol #'compute-indent-symbol-method)
             (function #'compute-indent-symbol-method)
             (list #'compute-indent-complex-method))
           method path indent-point sexp-column))

(defun quote-form-point-p (p)
  (and (eql (character-at p -1) #\')
       (not (eql (character-at p -2) #\#))))

(defun vector-form-point-p (p)
  (eql (character-at p -1) #\#))

(defun find-indent-method (name path)
  (flet ((f (method)
           (when method
             (return-from find-indent-method method))))
    (f (get-indentation name))
    (let ((name1 (ppcre:scan-to-strings "(?<=:)[^:]+" name)))
      (when name1
        (f (get-indentation name1)))
      (f (and (null (cdr path))
              (ppcre:scan "^(?:with-|without-|within-|do-|def)" (or name1 name))
              '(&lambda &body))))))

(defun calc-function-indent (point)
  (loop
    (unless (form-offset point -1)
      (let ((charpos (point-charpos point)))
        (form-offset point 1)
        (skip-whitespace-forward point t)
        (when (or (eql #\; (character-at point))
                  (end-line-p point))
          (line-offset point 0 charpos)))
      (return))
    (let ((charpos (point-charpos point)))
      (back-to-indentation point)
      (when (= charpos (point-charpos point))
        (return))
      (line-offset point 0 charpos)))
  (point-column point))

(defun calc-indent-1 (indent-point)
  (let* ((const-flag nil)
         (innermost-sexp-column nil)
         (calculated
           (with-point ((p indent-point))
             (loop
               :named outer
               :with path := '() :and sexp-column
               :for innermost := t :then nil
               :repeat *max-depth*
               :do
                  (loop :for n :from 0 :do
                           (when (and (< 0 n) (start-line-p p))
                             (return-from outer nil))
                           (unless (form-offset p -1)
                             (push n path)
                             (return)))
                  (when (and (null (cdr path))
                             (= 0 (car path))
                             (scan-lists p -1 1 t))
                    (return-from outer (1+ (point-column p))))
                  (when (and innermost
                             (or (member (character-at p 0) '(#\: #\"))
                                 (looking-at p "#!?[+-]")))
                    (setf const-flag t))
                  (let ((name (string-downcase (symbol-string-at-point p))))
                    (unless (scan-lists p -1 1 t)
                      (return-from outer 'default-indent))
                    (unless sexp-column (setf sexp-column (point-column p)))
                    (when innermost
                      (setf innermost-sexp-column sexp-column))
                    (when (or (quote-form-point-p p)
                              (vector-form-point-p p))
                      (return-from outer (1+ sexp-column)))
                    (let ((method (find-indent-method name path)))
                      (when method
                        (return-from outer
                          (cond ((eq method 'default-indent)
                                 (setq const-flag nil) ; for the case of (:and ...) in sxql
                                 method)
                                (t
                                 (compute-indent-method method
                                                        path
                                                        indent-point
                                                        sexp-column)))))))))))
    (cond ((and (eq calculated 'default-indent)
                (not const-flag))
           (calc-function-indent indent-point))
          ((and (or (null calculated)
                    (eq calculated 'default-indent))
                const-flag)
           (1+ innermost-sexp-column))
          (calculated
           (if (eq calculated 'default-indent)
               (calc-function-indent indent-point)
               calculated))
          (t
           (calc-function-indent indent-point)))))

(defun calc-indent (point)
  (line-start point)
  (with-point-syntax point
    (let ((state (syntax-ppss point)))
      (cond
        ((pps-state-string-p state) nil)
        ((zerop (pps-state-paren-depth state))
         0)
        (t (calc-indent-1 point))))))
