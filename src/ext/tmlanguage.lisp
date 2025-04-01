(defpackage :lem/tmlanguage
  (:use :cl :lem)
  (:export :load-tmlanguage)
  #+sbcl
  (:lock t))
(in-package :lem/tmlanguage)

(defun name-to-attribute (name)
  (let* ((pos (position #\. name))
         (name (if pos (subseq name 0 pos) name)))
    (cond ((string= name "comment") 'syntax-comment-attribute)
          ((string= name "constant") 'syntax-constant-attribute)
          ((string= name "entity") 'syntax-function-name-attribute)
          ((string= name "invalid") 'syntax-warning-attribute)
          ((string= name "keyword") 'syntax-keyword-attribute)
          ((string= name "markup") nil)
          ((string= name "meta") nil)
          ((string= name "storage") 'syntax-builtin-attribute)
          ((string= name "string") 'syntax-string-attribute)
          ((string= name "support") 'syntax-keyword-attribute)
          ((string= name "variable") 'syntax-variable-attribute))))

(defun convert-capture-value (hash-table)
  (alexandria:when-let ((patterns (gethash "patterns" hash-table)))
    (return-from convert-capture-value (convert-patterns patterns)))
  (alexandria:when-let ((name (gethash "name" hash-table)))
    (return-from convert-capture-value (make-tm-name (name-to-attribute name))))
  (error "invalid capture"))

(defun convert-captures (captures)
  (when captures
    (let ((alist '())
          (len 0))
      (maphash (lambda (key value)
                 (let ((n (parse-integer key)))
                   (setf len (max len (1+ n)))
                   (push (cons n value) alist)))
               captures)
      (let ((vector (make-array len :initial-element nil)))
        (loop :for (i . value) :in alist
              :do (setf (aref vector i) (convert-capture-value value)))
        vector))))

(defun convert-match-rule (rule)
  (let ((regex (gethash "match" rule))
        (captures (gethash "captures" rule))
        (name (gethash "name" rule)))
    (make-tm-match regex
                   :name (name-to-attribute name)
                   :captures (convert-captures captures))))

(defun convert-region-rule (rule)
  (let ((begin (gethash "begin" rule))
        (end (gethash "end" rule))
        (begin-captures (gethash "beginCaptures" rule))
        (end-captures (gethash "endCaptures" rule))
        (captures (gethash "captures" rule))
        (name (gethash "name" rule))
        (content-name (gethash "contentName" rule))
        (patterns (gethash "patterns" rule)))
    (make-tm-region begin end
                    :begin-captures (convert-captures begin-captures)
                    :end-captures (convert-captures end-captures)
                    :captures (convert-captures captures)
                    :name (name-to-attribute name)
                    :content-name (name-to-attribute content-name)
                    :patterns (convert-patterns patterns))))

(defun convert-include-rule (rule)
  (let ((include (gethash "include" rule)))
    (make-tm-include include)))

(defun convert-rule (rule)
  (cond ((gethash "match" rule) (convert-match-rule rule))
        ((gethash "begin" rule) (convert-region-rule rule))
        ((gethash "include" rule) (convert-include-rule rule))
        (t (error "invalid rule"))))

(defun convert-patterns (patterns)
  (apply #'make-tm-patterns
         (loop :for rule :in patterns
               :collect (convert-rule rule))))

(defun convert-repository (hash-table)
  (when hash-table
    (let ((repository (make-tm-repository)))
      (maphash (lambda (name patterns)
                 (let ((patterns (convert-patterns (gethash "patterns" patterns))))
                   (add-tm-repository repository name patterns)))
               hash-table)
      repository)))

(defun convert-toplevel (hash-table)
  (let ((patterns (convert-patterns (gethash "patterns" hash-table)))
        (repository (convert-repository (gethash "repository" hash-table))))
    (make-tmlanguage :patterns patterns
                     :repository repository)))

(defun load-json (pathname)
  (convert-toplevel (yason:parse pathname)))

(defun load-tmlanguage (pathname)
  (setf pathname (pathname pathname))
  (let ((type (pathname-type pathname)))
    (cond ((equal type "json")
           (load-json pathname))
          (t
           (error "unsupported type: ~A" type)))))
