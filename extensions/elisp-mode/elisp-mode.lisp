(defpackage :lem-elisp-mode
  (:use :cl :lem :lem/language-mode :lem/language-mode-tools :lem/completion-mode)
  (:export :*elisp-mode-hook*
           :elisp-mode
           :*elisp-syntax-table*
           :*elisp-mode-keymap*))

(in-package :lem-elisp-mode)


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



(defvar *elisp-keywords*
  '(
    "cl-defstruct"
    "cl-defsubst"
    "cl-loop"

    "defstruct"
    "defsubst"

    "cl-deftype"
    "cl-defgeneric"
    "defalias"
    "defadvice"
    "defclass"
    "defcustom"
    "defgroup"
    "defadvice"

    "define-obsolete-variable-alias"
    "save-excursion"
    "condition-case"
    "eval-when-compile"
    "add-hook"

    "if"
    "cond"
    "else"
    "when"
    "unless"
    "while"

    "let"
    "let*"
    "lambda"
    "dolist"
    "or"
    "and"
    "catch"
    "throw"
    "provide"
    "require"

    "setf"
    "setq"
    "setq-local"
    "progn"
    "prog1"
    "prog2"

    "error"
    "interactive"

    "use-package"))

(defvar *elisp-boolean-literals*
  '("t" "nil" ))

(defvar *elisp-null-literal*
  '("nil"))


(defun tokens (boundary strings)
  (let ((alternation
          `(:alternation ,@(sort (copy-list strings) #'> :key #'length))))
    (if boundary
        `(:sequence ,boundary ,alternation ,boundary)
        alternation)))


(defun make-tmlanguage-elisp ()
  (let* ((patterns (make-tm-patterns

                    (make-tm-region
                     `(:sequence ";")
                     "$"
                     :name 'syntax-comment-attribute)
                    (make-tm-match ":\\w+" 
                                   :name 'syntax-constant-attribute) 
                    (make-tm-match "\\w+:"
                                   :name 'syntax-constant-attribute)

                    (make-tm-match
                     `(:sequence
                       "("
                       (:sequence
                        ,(lem-lisp-mode/grammar::wrap-symbol-names
                          "defun" "cl-defun" "defclass" "cl-defgeneric" "cl-defmacro" ))
                       (:greedy-repetition 0 1 (:register symbol)))
                     :captures (vector nil
                                       (make-tm-name 'syntax-keyword-attribute)
                                       (make-tm-name 'syntax-function-name-attribute)))
                    
                    (make-tm-match
                     `(:sequence
                       "(" ,(lem-lisp-mode/grammar::wrap-symbol-names "defun" "cl-defmethod" "cl-defun")
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
                       ,(lem-lisp-mode/grammar::wrap-symbol-names
                         "defvar" "defconst")
                       (:greedy-repetition 0 1 (:register symbol)))
                     :captures (vector nil
                                       (make-tm-name 'syntax-keyword-attribute)
                                       (make-tm-name 'syntax-variable-attribute)))

                    (make-tm-match
                     `(:sequence
                       "(" ,(lem-lisp-mode/grammar::wrap-symbol-names "defstruct" "cl-defstruct")
                       ,(ppcre:parse-string "\\s*\\(")
                       (:register symbol))
                     :captures (vector nil
                                       (make-tm-name 'syntax-keyword-attribute)
                                       (make-tm-name 'syntax-type-attribute)))

                    (make-tm-match
                     `(:sequence
                       "(" ,(lem-lisp-mode/grammar::wrap-symbol-names "error"))
                     :captures (vector nil (make-tm-name 'syntax-warning-attribute)))

                    (make-tm-match
                     `(:sequence
                       symbol-boundary-begin
                       "&" symbol
                       symbol-boundary-end)
                     :name 'syntax-constant-attribute)
                    
                    (make-tm-string-region "\"")
                    (make-tm-match (tokens :word-boundary 
                                           (append *elisp-boolean-literals*
                                                   *elisp-null-literal*))
                                   :name 'syntax-constant-attribute)

                    (make-tm-match (tokens :word-boundary *elisp-keywords*)
                                   :name 'syntax-keyword-attribute))))
    (make-tmlanguage :patterns patterns)))

(defvar *elisp-syntax-table*
  (let ((table (make-syntax-table
                :space-chars '(#\space #\tab #\newline)
		:symbol-chars '(#\+ #\- #\< #\> #\/ #\* #\& #\= #\. #\? #\_ #\! #\$ #\% #\: #\@ #\[ #\]
				#\^ #\{ #\} #\~ #\# #\|)
                :paren-pairs '((#\( . #\))
                               (#\{ . #\})
                               (#\[ . #\]))
                :string-quote-chars '(#\")
                :line-comment-string ";"))
        (tmlanguage (make-tmlanguage-elisp)))
    (set-syntax-parser table tmlanguage)
    table))

(defun completion-symbol (point)
  (with-point ((start point) 
               (end point))

    (skip-chars-backward start #'syntax-symbol-char-p)
    (skip-chars-forward end #'syntax-symbol-char-p)
    
    (when (and  (point< start end)
                (lem-elisp-mode/rpc:connected-p))
      (mapcar (lambda (item)
                (make-completion-item 
                 :label item
                 :start start
                 :end end))
              (sort (completion (points-to-string start end) 
                                (lem-elisp-mode/rpc:get-completions (points-to-string start end)))
		    #'string-lessp)))))

;;TODO: Show more information about the connection without loading jsonrpc/http
(define-command elisp-connect () ()
  (if (lem-elisp-mode/rpc:connected-p)
    (message "Already connected.")
    (lem-elisp-mode/rpc:connect-to-server)))

(defun elisp-find-definitions (point)
  (lem-elisp-mode/rpc:connected-p)
  (alexandria:when-let* ((location
                          (lem-elisp-mode/rpc:get-symbol-location (symbol-string-at-point point)))
                         (buffer (and (listp location)
                                      (find-file-buffer (car location)))))
    (lem/language-mode::push-location-stack (current-point))
    (switch-to-buffer buffer)
    (move-to-bytes (current-point) (second location))
    (lem/peek-source:highlight-matched-line (current-point))))

(define-major-mode elisp-mode language-mode
    (:name "Emacs Lisp"
     :keymap *elisp-mode-keymap*
     :syntax-table *elisp-syntax-table*
     :mode-hook *elisp-mode-hook*)
  (setf (variable-value 'enable-syntax-highlight) t
        (variable-value 'calc-indent-function) 'lem-lisp-mode/internal::calc-indent
        (variable-value 'beginning-of-defun-function) 'lem-lisp-mode/internal:lisp-beginning-of-defun
        (variable-value 'end-of-defun-function) 'lem-lisp-mode/internal:lisp-end-of-defun
        (variable-value 'completion-spec) 'completion-symbol
        (variable-value 'find-definitions-function) 'elisp-find-definitions
          
        (variable-value 'indent-tabs-mode) nil
        (variable-value 'tab-width) 4
        (variable-value 'line-comment) ";"
        (variable-value 'insertion-line-comment) ";; "
        (variable-value 'language-mode-tag) 'elisp-mode
    
  
        (variable-value 'beginning-of-defun-function) 'beginning-of-defun
        (variable-value 'end-of-defun-function) 'end-of-defun))

(define-file-type ("el") elisp-mode)
