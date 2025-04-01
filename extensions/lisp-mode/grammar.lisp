(defpackage :lem-lisp-mode/grammar
  (:use :cl :lem)
  (:export :get-features
           :make-tmlanguage-lisp))
(in-package :lem-lisp-mode/grammar)

(defgeneric get-features () (:method () nil))

(defun featurep (form)
  (cond ((atom form)
         (find (find-symbol (let ((*print-case* :upcase))
                              (princ-to-string form))
                            :keyword)
               (get-features)))
        ((string-equal 'and (car form))
         (every #'featurep (cdr form)))
        ((string-equal 'or (car form))
         (some #'featurep (cdr form)))
        (t)))

(defun skip-feature (p)
  (let ((positivep (eql #\+ (character-at p 1))))
    (character-offset p 2)
    (with-point ((prev p))
      (when (form-offset p 1)
        (cond
          ((if (featurep (let ((*read-eval* nil))
                           (read-from-string
                            (points-to-string
                             prev p))))
               positivep
               (not positivep))
           nil)
          (t
           (form-offset p 1)))))))

(ppcre:define-parse-tree-synonym symbol-boundary-begin
  (:alternation
   :start-anchor
   (:positive-lookbehind (:char-class #\( #\) :whitespace-char-class))))

(ppcre:define-parse-tree-synonym symbol-boundary-end
  (:alternation
   :end-anchor
   (:positive-lookahead  (:char-class #\( #\) :whitespace-char-class))))

(ppcre:define-parse-tree-synonym maybe-package-prefix
  (:greedy-repetition 0 1
   (:sequence
    (:greedy-repetition 1 nil (:inverted-char-class #\( #\) #\space #\tab)) #\:)))

(ppcre:define-parse-tree-synonym symbol
  (:alternation
   (:sequence
    #\|
    (:greedy-repetition 0 nil
     (:alternation
      (:group "\\|")
      (:inverted-char-class #\|)))
    #\|)
   (:greedy-repetition 1 nil
    (:inverted-char-class #\( #\) :whitespace-char-class #\; #\"))))

(defun word-length-sort (&rest words)
  (sort (copy-list words) #'> :key #'length))

(defun wrap-symbol-names (&rest names)
  `(:sequence
    maybe-package-prefix
    (:register
     (:group :case-insensitive-p
      ,(let ((args (apply #'word-length-sort names)))
         (if (null (rest args)) (first args) `(:alternation ,@args)))))
    (:alternation
     (:greedy-repetition 1 nil :whitespace-char-class)
     :whitespace-char-class :end-anchor #\( #\))))

(defun make-tmlanguage-lisp (&key extra-patterns)
  (let ((patterns
          (apply #'make-tm-patterns
                 (remove-if #'null
                            (append
                             extra-patterns
                             (list
                              (make-tm-region
                               `(:sequence ";")
                               "$"
                               :name 'syntax-comment-attribute)
                              (make-tm-region
                               `(:sequence "|")
                               `(:sequence "|"))
                              (make-tm-region
                               `(:sequence "\"")
                               `(:sequence "\"")
                               :name 'syntax-string-attribute
                               :patterns (make-tm-patterns
                                          (make-tm-match "\\\\.")))
                              (make-tm-region
                               `(:sequence "#|")
                               `(:sequence "|#")
                               :name 'syntax-comment-attribute)
                              (make-tm-match
                               "\\\\.")
                              (make-tm-match
                               `(:sequence
                                 "("
                                 (:sequence
                                  ,(wrap-symbol-names
                                    "defun" "defclass" "defgeneric" "defsetf" "defmacro" "defmethod"
                                    "define-method-combination" "define-condition"
                                    "define-setf-expander" "define-compiler-macro"
                                    "define-modify-macro"))
                                 (:greedy-repetition 0 1 (:register symbol)))
                               :captures (vector nil
                                                 (make-tm-name 'syntax-keyword-attribute)
                                                 (make-tm-name 'syntax-function-name-attribute)))
                              (make-tm-match
                               `(:sequence
                                 "(" ,(wrap-symbol-names "defun" "defmethod")
                                 ,(ppcre:parse-string "\\s*\\(")
                                 ,(ppcre:parse-string "((?i:setf))\\s+")
                                 (:greedy-repetition 0 1 (:register symbol)))
                               :captures (vector nil
                                                 (make-tm-name 'syntax-keyword-attribute)
                                                 (make-tm-name 'syntax-function-name-attribute)
                                                 (make-tm-name 'syntax-function-name-attribute)))
                              (make-tm-match
                               `(:sequence
                                 "("
                                 (:group :case-insensitive-p
                                  (:register (:sequence "define-" symbol)))
                                 (:alternation (:greedy-repetition 1 nil :whitespace-char-class)
                                  :end-anchor)
                                 (:greedy-repetition 0 1 (:register symbol)))
                               :captures (vector nil
                                                 (make-tm-name 'syntax-keyword-attribute)
                                                 (make-tm-name 'syntax-function-name-attribute)))
                              (make-tm-match
                               `(:sequence
                                 "("
                                 (:group :case-insensitive-p
                                  (:register (:sequence "def" symbol)))
                                 (:alternation (:greedy-repetition 1 nil :whitespace-char-class)
                                  :end-anchor)
                                 (:greedy-repetition 0 1 (:register symbol)))
                               :captures (vector nil
                                                 (make-tm-name 'syntax-keyword-attribute)
                                                 (make-tm-name 'syntax-function-name-attribute)))
                              (make-tm-match
                               `(:sequence
                                 "("
                                 ,(wrap-symbol-names
                                   "defvar" "defparameter" "defconstant"
                                   "define-symbol-macro")
                                 (:greedy-repetition 0 1 (:register symbol)))
                               :captures (vector nil
                                                 (make-tm-name 'syntax-keyword-attribute)
                                                 (make-tm-name 'syntax-variable-attribute)))
                              (make-tm-match
                               `(:sequence
                                 "("
                                 ,(wrap-symbol-names
                                   "deftype" "defpackage" "defstruct" "uiop:define-package")
                                 (:greedy-repetition 0 1 (:register symbol)))
                               :captures (vector nil
                                                 (make-tm-name 'syntax-keyword-attribute)
                                                 (make-tm-name 'syntax-type-attribute)))
                              (make-tm-match
                               `(:sequence
                                 "(" ,(wrap-symbol-names "defstruct")
                                 ,(ppcre:parse-string "\\s*\\(")
                                 (:register symbol))
                               :captures (vector nil
                                                 (make-tm-name 'syntax-keyword-attribute)
                                                 (make-tm-name 'syntax-type-attribute)))
                              (make-tm-match
                               `(:sequence
                                 "("
                                 ,(wrap-symbol-names
                                   "cond" "if" "let" "let*" "progn" "prog1"
                                   "prog2" "lambda" "unwind-protect"
                                   "when" "unless" "with-output-to-string"
                                   "ignore-errors" "dotimes" "dolist" "declare"
                                   "block" "break" "case" "ccase" "compiler-let" "ctypecase"
                                   "declaim" "destructuring-bind" "do" "do*"
                                   "ecase" "etypecase" "eval-when" "flet" "flet*"
                                   "go" "handler-case" "handler-bind" "in-package"
                                   "labels" "letf" "locally" "loop"
                                   "macrolet" "multiple-value-bind" "multiple-value-prog1"
                                   "proclaim" "prog" "prog*" "progv"
                                   "restart-case" "restart-bind" "return" "return-from"
                                   "setf" "setq" "symbol-macrolet" "tagbody" "the" "typecase"
                                   "with-accessors" "with-compilation-unit"
                                   "with-condition-restarts" "with-hash-table-iterator"
                                   "with-input-from-string" "with-open-file"
                                   "with-open-stream" "with-package-iterator"
                                   "with-simple-restart" "with-slots" "with-standard-io-syntax"))
                               :captures (vector nil
                                                 (make-tm-name 'syntax-keyword-attribute)))
                              (make-tm-match
                               `(:sequence
                                 "(" ,(wrap-symbol-names "warn" "error" "signal" "abort" "cerror"))
                               :captures (vector nil (make-tm-name 'syntax-warning-attribute)))
                              (make-tm-match
                               `(:sequence
                                 symbol-boundary-begin
                                 ":" symbol
                                 symbol-boundary-end)
                               :name 'syntax-builtin-attribute)
                              (make-tm-match
                               `(:sequence
                                 (:positive-lookbehind "#")
                                 ":"
                                 symbol
                                 symbol-boundary-end)
                               :name 'syntax-builtin-attribute)
                              (make-tm-match
                               `(:sequence
                                 symbol-boundary-begin
                                 "&" symbol
                                 symbol-boundary-end)
                               :name 'syntax-constant-attribute)
                              (make-tm-match
                               "#[+-]"
                               :name 'syntax-comment-attribute
                               :move-action (lambda (cur-point)
                                              (ignore-errors
                                                (skip-feature cur-point))))))))))
    (make-tmlanguage :patterns patterns)))
