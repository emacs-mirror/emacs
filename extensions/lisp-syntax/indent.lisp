(defpackage :lem-lisp-syntax.indent
  (:use :cl :lem)
  (:export :*get-method-function*
           :get-indentation
           :set-indentation
           :update-system-indentation
           :indentation-update
           :calc-indent)
  #+sbcl
  (:lock t))
(in-package :lem-lisp-syntax.indent)

(defparameter *body-indent* 2)
(defparameter *max-depth* 4)

(defvar *get-method-function* nil)
(defvar *static-indent-table* (make-hash-table :test 'equal))
(defvar *dynamic-indent-table* (make-hash-table :test 'equal))

(defvar *lambda-list-indentation* t)
(defvar *lambda-list-keyword-parameter-alignment* t)
(defvar *lambda-list-keyword-alignment* t)

(defvar *loop-indent-subclauses* nil)
(defvar *simple-loop-indentation* 2)

(defun get-indentation (name)
  (or (gethash name *static-indent-table*)
      (caar (gethash name *dynamic-indent-table*))))

(defun set-indentation (name method)
  (setf (gethash name *static-indent-table*) method))

(defun update-system-indentation (name indent packages)
  (let ((list (gethash name *dynamic-indent-table*))
        ok)
    (if (null list)
        (setf (gethash name *dynamic-indent-table*)
              (list (cons indent packages)))
        (dolist (spec list)
          (cond ((equal (car spec) indent)
                 (setf (cdr spec)
                       (nunion (cdr spec) packages))
                 (setf ok t))
                (t
                 (setf (cdr spec)
                       (nset-difference (cdr spec) packages :test #'equal))))))
    (unless ok
      (setf (gethash name *dynamic-indent-table*)
            (cons (cons indent packages) list)))))

(defun indentation-update ()
  (do-all-symbols (symbol)
    (let ((key (string-downcase symbol)))
      (alexandria:when-let ((indent (micros::symbol-indentation symbol)))
        (update-system-indentation key
                                   indent
                                   (list (package-name (symbol-package symbol))))))))

(mapc (lambda (elt)
        (let ((name (car elt))
              (method (if (stringp (cdr elt))
                          (get-indentation (cdr elt))
                          (cadr elt))))
          (set-indentation name method)))
      '(("block" 1)
        ("case"        (4 &rest (&whole 2 &rest 1)))
        ("ccase" . "case")
        ("ecase" . "case")
        ("typecase" . "case")
        ("etypecase" . "case")
        ("ctypecase" . "case")
        ("catch" 1)
        ("cond"        (&rest (&whole 2 &rest 1)))
        ("defvar"      (4 2 2))
        ("defclass"    (6 4 (&whole 2 &rest 1) (&whole 2 &rest 1)))
        ("defconstant" . "defvar")
        ("defcustom"   (4 2 2 2))
        ("defparameter" . "defvar")
        ("defconst"     . "defcustom")
        ("define-condition"  . "defclass")
        ("define-modify-macro" (4 &lambda &body))
        ("defsetf"     (4 &lambda 4 &body))
        ("defun"       (4 &lambda &body))
        ("defgeneric"  (4 &lambda &body))
        ("define-setf-method" . "defun")
        ("define-setf-expander" . "defun")
        ("defmacro" . "defun")
        ("defsubst" . "defun")
        ("deftype" . "defun")
        ("defmethod" lisp-indent-defmethod)
        ("defpackage"  (4 &rest (&whole 2 &rest defpackage-body)))
        ("uiop:define-package" . "defpackage")
        ("defstruct"   ((&whole 4 &rest (&whole 2 &rest 1))
                        &rest (&whole 2 &rest 1)))
        ("destructuring-bind"
         ((&whole 6 &rest 1) 4 &body))
        ;("do"          lisp-indent-do)
        ("do" 2)
        ("do*" . "do")
        ("dolist"      1)
        ("dotimes" . "dolist")
        ("eval-when"   1)
        ("flet"        ((&whole 4 &rest (&whole 1 &lambda &body)) &body))
        ("ignore-errors" (&rest &body))
        ("labels" . "flet")
        ("macrolet" . "flet")
        ("generic-flet" . "flet")
        ("generic-labels" . "flet")
        ("handler-case" (4 &rest (&whole 2 &lambda &body)))
        ("restart-case" . "handler-case")
        ;; `else-body' style
        ("if"          (nil nil &body))
        ;; single-else style (then and else equally indented)
        ("if"          (&rest nil))
        ;("lambda"      (&lambda &rest lisp-indent-function-lambda-hack))
        ("lambda" (&lambda &body))
        ("let"         ((&whole 4 &rest (&whole 1 2)) &body))
        ("let*" . "let")
        ("compiler-let" . "let") ;barf
        ("handler-bind" . "let")
        ("restart-bind" . "let")
        ("locally" 1)
        ("loop"         lisp-indent-loop)
        ;("loop" (&rest &body))
        (":method" lisp-indent-defmethod) ; in `defgeneric'
        (":default-initargs" (&rest 1))
        ("multiple-value-bind" ((&whole 6 &rest 1) 4 &body))
        ("multiple-value-call" (4 &body))
        ("multiple-value-prog1" 1)
        ("multiple-value-setq" (4 2))
        ("multiple-value-setf" . "multiple-value-setq")
        ("pprint-logical-block" (4 2))
        ("print-unreadable-object" ((&whole 4 1 &rest 1) &body))
        ;; Combines the worst features of BLOCK, LET and TAGBODY
        ("prog"        (&lambda &rest lisp-indent-tagbody))
        ("prog*" . "prog")
        ("prog1" 1)
        ("prog2" 2)
        ("progn" (&rest &body))
        ("progv"       (4 4 &body))
        ("return" 0)
        ("return-from" (nil &body))
        ("symbol-macrolet" . "let")
        ("tagbody"     lisp-indent-tagbody)
        ("throw" 1)
        ("unless" 1)
        ("unwind-protect" (5 &body))
        ("when" 1)
        ("with-accessors" . "multiple-value-bind")
        ("with-condition-restarts" . "multiple-value-bind")
        ("with-compilation-unit" (&lambda &body))
        ("with-output-to-string" (4 2))
        ("with-slots" . "multiple-value-bind")
        ("with-standard-io-syntax" (2))))

(defun defpackage-body (path indent-point sexp-column)
  (declare (ignore path sexp-column))
  (calc-function-indent indent-point))

(defun loop-type (point)
  (let ((comment-split nil))
    (labels ((guard (x)
               (unless x
                 (return-from loop-type
                   (if comment-split
                       'simple/split
                       'simple)))))
      (with-point ((p point))
        (let ((line-number (line-number-at-point p))
              (maybe-split t))
          (guard (character-offset p 1))
          (guard (form-offset p 1))
          (with-point ((p p))
            (skip-whitespace-forward p)
            (if (= line-number (line-number-at-point p))
                (setf maybe-split nil)
                (setf comment-split t)))
          (guard (form-offset p 1))
          (guard (form-offset p -1))
          (if (eql (character-at p) #\()
              (if (or (not maybe-split)
                      (= line-number (line-number-at-point p)))
                  'simple
                  'simple/split)
              (if (or (not maybe-split)
                      (= line-number (line-number-at-point p)))
                  'extended
                  'extended/split)))))))

(defun trailing-comment (p)
  (and (form-offset p -1)
       (form-offset p 1)
       (progn
         (skip-whitespace-forward p t)
         (and (eql (character-at p) #\;)
              (point-column p)))))

(defun loop-macro-1 (p)
  (declare (ignore p))
  (error "unsupported ~A" '*loop-indent-subclauses*))

(defun loop-macro-keyword-p (string)
  (ppcre:scan "^(?:#?:)?(?:do|doing|finally|initially)" string))

(defun loop-part-indentation (p indent-point type)
  (labels ((f (p)
             (or (end-line-p p)
                 (eql #\) (character-at p))
                 (looking-at p "(?:#?:)?\\w+"))))
    (let ((loop-indentation (if (eq type 'extended/split)
                                (- (point-column p) 4)
                                (point-column p)))
          (indent nil))
      (back-to-indentation (move-point p indent-point))
      (cond ((eq type 'simple/split)
             (+ loop-indentation *simple-loop-indentation*))
            ((eq type 'simple)
             (+ loop-indentation 6))
            ((and (not (f p))
                  (with-point ((p p))
                    (loop :while (and (form-offset p -1)
                                      (not (f p)))
                          :do (setf indent (point-column p)))
                    (and indent (loop-macro-keyword-p (symbol-string-at-point p)))))
             indent)
            ((or #+(or) lisp-loop-indent-forms-like-keywords
                 (f p)
                 (eql #\; (character-at p)))
             (if (and (eql #\; (character-at p))
                      (alexandria:when-let ((col (trailing-comment p)))
                        (setf loop-indentation col)))
                 loop-indentation
                 (+ loop-indentation 6)))
            (t
             (+ loop-indentation 9))))))

(defun lisp-indent-loop (path indent-point sexp-column)
  (declare (ignore sexp-column))
  (if (cdr path)
      'default-indent
      (with-point ((p indent-point))
        (scan-lists p -1 1)
        (let ((type (loop-type p)))
          (if (and *loop-indent-subclauses*
                   (member type '(extended extended/split)))
              (loop-macro-1 p)
              (loop-part-indentation p
                                     (copy-point indent-point :temporary)
                                     type))))))

(defun beginning-of-defmethod-qualifiers (p)
  (let ((str nil))
    (loop
      (unless (scan-lists p -1 1) (return))
      (character-offset p 1)
      (unless (setf str (symbol-string-at-point p)) (return))
      (cond ((string-equal str "defmethod")
             (form-offset p 2)
             (return 1))
            ((string-equal str ":method")
             (form-offset p 1)
             (return 0)))
      (character-offset p -1))))

(defun lisp-indent-defmethod (path indent-point sexp-column)
  (with-point ((p indent-point))
    (compute-indent-method
     (let ((nskip (beginning-of-defmethod-qualifiers p)))
       (cond (nskip
              (skip-whitespace-forward p)
              (loop :while (syntax-symbol-char-p (character-at p))
                    :do (incf nskip)
                        (form-offset p 1)
                        (skip-whitespace-forward p))
              (append (make-list nskip :initial-element 4) '(&lambda &body)))
             (t
              (get-indentation "defun"))))
     path indent-point sexp-column)))

(defun lisp-indent-do (path indent-point sexp-column)
  (declare (ignore path indent-point sexp-column))
  'default-indent)

(defun lisp-indent-function-lambda-hack (path indent-point sexp-column)
  (declare (ignore path indent-point sexp-column))
  'default-indent)

(defun lisp-indent-tagbody (path indent-point sexp-column)
  (declare (ignore path))
  (with-point ((indent-point indent-point))
    (if (symbol-string-at-point (back-to-indentation indent-point))
        (+ sexp-column 1)
        (+ sexp-column *body-indent*))))

(defun lisp-indent-keyword (path indent-point sexp-column)
  (declare (ignore path indent-point))
  (+ sexp-column 1))

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
      (f (and *get-method-function*
              (funcall *get-method-function* name)))
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
  (lem:with-point-syntax point
    (let ((state (syntax-ppss point)))
      (cond
        ;; workaround: Do not treat #\page as whitespace only when indentation
        ((eql #\Page (character-at point)) nil)
        ((pps-state-string-p state) nil)
        ((zerop (pps-state-paren-depth state)) 0)
        (t (calc-indent-1 point))))))
