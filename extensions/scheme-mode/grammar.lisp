(in-package :lem-scheme-mode)

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
    (:register
     (:group :case-insensitive-p
      ,(let ((args (apply #'word-length-sort names)))
         (if (null (rest args)) (first args) `(:alternation ,@args)))))
    (:alternation
     (:greedy-repetition 1 nil :whitespace-char-class)
     :whitespace-char-class :end-anchor #\( #\))))

(defun make-tmlanguage-scheme ()
  (let ((patterns (make-tm-patterns
                   (make-tm-region
                    `(:sequence ";")
                    "$"
                    :name 'syntax-comment-attribute)
                   ;; todo: nested comment
                   (make-tm-region
                    `(:sequence "#|")
                    `(:sequence "|#")
                    :name 'syntax-comment-attribute)
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
                   ;; shebang (for Gauche)
                   (make-tm-region
                    `(:sequence :start-anchor "#!")
                    "$"
                    :name 'syntax-comment-attribute)
                   ;; regexp (for Gauche)
                   (make-tm-region
                    `(:sequence "#/")
                    `(:sequence "/" (:greedy-repetition 0 1 #\i))
                    :name 'syntax-constant-attribute
                    :patterns (make-tm-patterns
                               (make-tm-match "\\\\.")))
                   (make-tm-match
                    "\\\\.")
                   (make-tm-match
                    `(:sequence
                      "("
                      ,(wrap-symbol-names "define" "def")
                      "("
                      (:greedy-repetition 0 1 (:register symbol)))
                    :captures (vector nil
                                      (make-tm-name 'syntax-keyword-attribute)
                                      (make-tm-name 'syntax-function-name-attribute)))
                   (make-tm-match
                    `(:sequence
                      "("
                      ,(wrap-symbol-names "define" "def")
                      (:greedy-repetition 0 1 (:register symbol)))
                    :captures (vector nil
                                      (make-tm-name 'syntax-keyword-attribute)
                                      (make-tm-name 'syntax-variable-attribute)))
                   (make-tm-match
                    `(:sequence
                      "("
                      ,(wrap-symbol-names "define-method" "define-generic")
                      (:greedy-repetition 0 1 (:register symbol)))
                    :captures (vector nil
                                      (make-tm-name 'syntax-keyword-attribute)
                                      (make-tm-name 'syntax-function-name-attribute)))
                   (make-tm-match
                    `(:sequence
                      "("
                      ,(wrap-symbol-names "define-class" "define-record-type")
                      (:greedy-repetition 0 1 #\()
                      (:greedy-repetition 0 1 (:register symbol)))
                    :captures (vector nil
                                      (make-tm-name 'syntax-keyword-attribute)
                                      (make-tm-name 'syntax-type-attribute)))
                   (make-tm-match
                    `(:sequence
                      "("
                      (:group
                       (:register (:sequence "define-" ,(ppcre:parse-string "\\S*"))))
                      (:alternation (:greedy-repetition 1 nil :whitespace-char-class) :end-anchor)
                      (:greedy-repetition 0 1 (:register symbol)))
                    :captures (vector nil
                                      (make-tm-name 'syntax-keyword-attribute)
                                      (make-tm-name 'syntax-variable-attribute)))
                   (make-tm-match
                    `(:sequence
                      "("
                      (:group
                       (:register (:sequence "make-" ,(ppcre:parse-string "\\S*"))))
                      (:alternation (:greedy-repetition 1 nil :whitespace-char-class) :end-anchor))
                    :captures (vector nil
                                      (make-tm-name 'syntax-keyword-attribute)))
                   (make-tm-match
                    `(:sequence
                      "("
                      ,(apply #'wrap-symbol-names (lem-scheme-syntax:get-scheme-highlight-data)))
                    :captures (vector nil
                                      (make-tm-name 'syntax-keyword-attribute)))
                   (make-tm-match
                    `(:sequence
                      "(" ,(wrap-symbol-names "error" "errorf"))
                    :captures (vector nil (make-tm-name 'syntax-warning-attribute)))
                   (make-tm-match
                    `(:sequence
                      "(" ,(wrap-symbol-names "+" "-" "*" "/" "+." "-." "*." "/."
                                              "=" "<" "<=" ">" ">="))
                    :captures (vector nil (make-tm-name 'syntax-builtin-attribute)))
                   )))
    (make-tmlanguage :patterns patterns)))
