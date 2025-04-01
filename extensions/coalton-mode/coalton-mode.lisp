(defpackage #:lem-coalton-mode
  (:use #:cl
        #:lem
        #:lem/language-mode)
  (:import-from #:lem-lisp-mode/grammar
                #:wrap-symbol-names
                #:symbol-boundary-begin
                #:maybe-package-prefix
                #:symbol-boundary-end)
  (:import-from #:lem-lisp-mode/internal
                #:check-connection
                #:connected-p
                #:lisp-eval-async
                #:compilation-finished
                #:top-of-defun-with-annotation
                #:before-compile-functions)
  (:import-from #:lem-lisp-syntax.indent
                #:calc-function-indent
                #:quote-form-point-p
                #:vector-form-point-p
                #:compute-indent-method
                #:default-indent)
  (:import-from #:alexandria
                #:when-let
                #:if-let)
  (:export #:coalton-mode))
(in-package #:lem-coalton-mode)

(defparameter *body-indent* 2)
(defparameter *max-depth* 4)

(defun make-tmlanguage-coalton ()
  (let ((patterns (make-tm-patterns
                   (make-tm-region
                    `(:sequence ";")
                    "$"
                    :name 'syntax-comment-attribute)
                   (make-tm-region
                    `(:sequence "#|")
                    `(:sequence "|#")
                    :patterns 'syntax-comment-attribute)
                   (make-tm-region
                    `(:sequence "|")
                    `(:sequence "|")
                    :patterns (make-tm-patterns
                               (make-tm-match "\\\\.")))
                   (make-tm-region
                    `(:sequence "\"")
                    `(:sequence "\"")
                    :name 'syntax-string-attribute
                    :patterns (make-tm-patterns
                               (make-tm-match "\\\\.")))
                   (make-tm-match
                    "\\\\.")
                   (make-tm-match
                    `(:sequence
                      "("
                      ,(wrap-symbol-names "define")
                      "("
                      (:greedy-repetition 0 1 (:register symbol)))
                    :captures (vector nil
                                      (make-tm-name 'syntax-keyword-attribute)
                                      (make-tm-name 'syntax-function-name-attribute)))
                   (make-tm-match
                    `(:sequence
                      "("
                      ,(wrap-symbol-names "define")
                      (:greedy-repetition 0 1 (:register symbol)))
                    :captures (vector nil
                                      (make-tm-name 'syntax-keyword-attribute)
                                      (make-tm-name 'syntax-variable-attribute)))
                   (make-tm-match
                    `(:sequence
                      "("
                      ,(wrap-symbol-names
                        "fn"
                        "λ"
                        "match"
                        "let"
                        "lisp"
                        "return"
                        "the"
                        "while"
                        "while-let"
                        "loop"
                        "break"
                        "continue"
                        "for"
                        "in"
                        "if"
                        "when"
                        "unless"
                        "and"
                        "or"
                        "cond"
                        "as"
                        "do"
                        "progn"
                        "assert"))
                    :captures (vector nil
                                      (make-tm-name 'syntax-keyword-attribute)))
                   (make-tm-match
                    `(:sequence
                      "("
                      ,(wrap-symbol-names
                        "declare" "package" "define-type" "define-type-alias" "define-struct" "define-class" "define-instance")
                      (:greedy-repetition 0 1 (:register symbol)))
                    :captures (vector nil
                                      (make-tm-name 'syntax-keyword-attribute)
                                      (make-tm-name 'syntax-type-attribute)))
                   (make-tm-match
                    `(:sequence
                      symbol-boundary-begin
                      ":"
                      symbol
                      symbol-boundary-end)
                    :name 'syntax-builtin-attribute)
                   (make-tm-match
                    `(:sequence
                      symbol-boundary-begin
                      maybe-package-prefix
                      ,(ppcre:parse-string "[A-Z]") symbol
                      symbol-boundary-end)
                    :name 'syntax-type-attribute))))
    (make-tmlanguage :patterns patterns)))

(defvar *syntax-table*
  (make-syntax-table
   :space-chars '(#\space #\tab #\newline)
   :symbol-chars '(#\! #\$ #\% #\& #\* #\/ #\: #\< #\= #\> #\? #\^ #\_ #\~
                   #\+ #\- #\. #\@)
   :paren-pairs '((#\( . #\))
                  (#\[ . #\])
                  (#\{ . #\}))
   :string-quote-chars '(#\")
   :escape-chars '(#\\)
   :fence-chars '(#\|)
   :expr-prefix-chars '(#\' #\, #\@ #\# #\`)
   ;:expr-prefix-forward-function 'skip-expr-prefix-forward
   ;:expr-prefix-backward-function 'skip-expr-prefix-backward
   :line-comment-string ";"
   :block-comment-pairs '(("#|" . "|#"))))

(defvar *static-indent-table* (make-hash-table :test 'equal))
(defvar *dynamic-indent-table* (make-hash-table :test 'equal))

(defun get-indentation (name)
  (or (gethash name *static-indent-table*)
      (caar (gethash name *dynamic-indent-table*))))

(defun set-indentation (name method)
  (setf (gethash name *static-indent-table*) method))

(defparameter *coalton-indentation-rules*
  '(("fn" (&lambda &body))
    ("λ" . "lambda")
    ("cond" (&rest (&whole 2 &rest 1)))
    ("match" (4 &rest (&whole 2 &rest 1)))
    ("let" ((&whole 4 &rest (&whole 1 2)) &body))
    ("lisp" 1)
    ("return" 0)
    ("when" 1)
    ("unless" 1)
    ("while" 1)
    ("while-let" . "let")
    ("progn" (&rest &body))
    ("do" . "progn")
    ("package" (4 &rest (&whole 2 &rest coalton-package-body)))))

(defun coalton-package-body (path indent-point sexp-column)
  (declare (ignore path sexp-column))
  (calc-function-indent indent-point))

(defun load-static-indentation-rules ()
  (loop for (fn . rule) in *coalton-indentation-rules*
        do (set-indentation fn (etypecase rule
                                 (string (get-indentation rule))
                                 (cons (first rule))))))

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

(define-major-mode coalton-mode language-mode
    (:name "Coalton"
     :keymap *coalton-mode-keymap*
     :syntax-table *syntax-table*
     :mode-hook *coalton-mode-hook*)
  (setf (variable-value 'enable-syntax-highlight) t
        (variable-value 'calc-indent-function) 'calc-indent
        (variable-value 'indent-tabs-mode) nil
        (variable-value 'tab-width) 4
        (variable-value 'line-comment) ";"
        (variable-value 'insertion-line-comment) ";; "
        (variable-value 'language-mode-tag) 'coalton-mode
        (variable-value 'idle-function) 'coalton-idle-function
        (variable-value 'completion-spec)
        (lem/completion-mode:make-completion-spec 'completion-symbol-async :async t))
  (lem-lisp-mode/internal::check-connection))

(defun completion-symbol-async (point then)
  (let ((lem-lisp-mode/internal::*current-package* (current-package)))
    (lem-lisp-mode/internal::completion-symbol-async point then)))

(set-syntax-parser *syntax-table* (make-tmlanguage-coalton))

(defun guess-current-position-package (point)
  (with-point ((p point))
    (loop
      (ppcre:register-groups-bind (package-name)
          ("^\\s*\\(\\s*package ([^\)\(\\s]*)"
           (string-downcase (line-string p)))
        (return package-name))
      (unless (line-offset p -1)
        (return)))))

(defun update-buffer-package ()
  (when-let ((package-name (guess-current-position-package (current-point))))
    (setf (buffer-package (current-buffer)) package-name)))

(defun coalton-idle-function ()
  (when (connected-p)
    (let ((major-mode (buffer-major-mode (current-buffer))))
      (when (eq major-mode 'coalton-mode)
        (update-buffer-package)))))

(defun buffer-package (buffer &optional default)
  (let ((package-name (buffer-value buffer "package" default)))
    (typecase package-name
      (null (if-let (package-name (guess-current-position-package (buffer-point buffer)))
              (string-upcase package-name)
              default))
      ((or symbol string)
       (string-upcase package-name))
      ((cons (or symbol string))
       (string-upcase (car package-name))))))

(defun (setf buffer-package) (package buffer)
  (setf (buffer-value buffer "package") package))

(defvar *current-package* nil)

(defun current-package ()
  (or *current-package*
      (buffer-package (current-buffer))
      "COALTON-USER"))

(defun coalton-eval-async (form &optional cont (package (current-package)))
  (destructuring-bind (fn coalton-form &rest args) form
    (lisp-eval-async `(,fn ,(format nil "(coalton:coalton-toplevel ~A)" coalton-form) ,@args)
                     cont
                     package)))

(define-command coalton-compile-region (start end) (:region)
  (check-connection)
  (let ((string (points-to-string start end))
        (position `((:position ,(position-at-point start))
                    (:line
                     ,(line-number-at-point (current-point))
                     ,(point-charpos (current-point))))))
    (run-hooks (variable-value 'before-compile-functions) start end)
    (coalton-eval-async `(micros:compile-string-for-emacs ,string
                                                          ,(buffer-name (current-buffer))
                                                          ',position
                                                          ,(buffer-filename (current-buffer))
                                                          nil)
                        #'compilation-finished)))

(define-command coalton-compile-defun () ()
  (check-connection)
  (with-point ((point (current-point)))
    (top-of-defun-with-annotation point)
    (with-point ((start point)
                 (end point))
      (scan-lists end 1 0)
      (coalton-compile-region start end))))

(define-key *coalton-mode-keymap* "C-c C-c" 'coalton-compile-defun)
(define-key *coalton-mode-keymap* "Return" 'newline-and-indent)

(load-static-indentation-rules)
(define-file-type ("coal") coalton-mode)

(defmethod execute :after ((mode coalton-mode) (command self-insert) argument)
  (when (eql #\space (get-self-insert-char))
    (lem-lisp-mode/autodoc:lisp-autodoc)))

(defvar *recursive* nil)

(defun after-syntax-scan (start end)
  (unless *recursive*
    (ignore-errors
      (when (eq 'lem-lisp-mode:lisp-mode
                (buffer-major-mode (point-buffer start)))
        (with-point ((p start))
          (lem-lisp-syntax:beginning-of-defun p -1)
          (loop while (lem:search-forward p "(coalton-toplevel" end)
                unless (lem:in-string-or-comment-p p)
                do (with-point ((start p))
                     (lem:scan-lists p -1 1)
                     (lem:scan-lists p 1 0)
                     (lem:character-offset p -1)
                     (set-region-major-mode start p 'coalton-mode)
                     (let ((*recursive* t))
                       (syntax-scan-region start p
                                           :syntax-table *syntax-table*
                                           :recursive-check nil)))))))))

(add-hook (variable-value 'after-syntax-scan-hook :global) 'after-syntax-scan)
