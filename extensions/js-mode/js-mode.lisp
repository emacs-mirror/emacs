(defpackage :lem-js-mode
  (:use :cl :lem :lem/language-mode :lem/language-mode-tools)
  (:import-from :lem-xml-mode
                :xml-calc-indent)
  (:export :*js-mode-hook*
           :js-mode))
(in-package :lem-js-mode)

#|
link :
https://developer.mozilla.org/ja/docs/Web/JavaScript/Guide/Grammar_and_types
https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Lexical_grammar

|#
(defvar *js-floating-point-literals* "\\b([+-]?[1-9]\\d*(.\\d)?([Ee][+-]?\\d+)?)\\b")
(defvar *js-integer-literals* "\\b([1-9]\\d*|0+|0[bB][01]+|0[oO][0-7]+|0[xX][\\da-fA-F]+)\\b")
(defvar *js-boolean-literals* "(true|false)")
(defvar *js-null-literals* "(null)")
(defvar *js-nan-literals* "(NaN)")
(defvar *js-undefined-literals* "(undefined)")

(defvar *js-key-words* '("break" "case" "catch" "class" "const" "continue" "debugger" "default"
                          "delete" "do" "else" "export" "extends" "finally" "for"
                          "function"  "if" "import" "in" "instanceof"
                          "let" "new" "return" "super"
                          "switch" "this" "throw" "try" "typeof" "var" "void" "while"
                          "with" "yield")) ;; TODO function* yeild*
(defvar *js-future-key-words* '("enum" "implements" "static" "public"
                                 "package" "interface" "protected" "private" "await"))

(defvar *js-white-space* (list (code-char #x9) (code-char #xb) (code-char #xc)
                                (code-char #x20) (code-char #xa0))) ;;TODO
(defvar *js-line-terminators* (list (code-char #x0a) (code-char #x0d)
                                     (code-char #x2028) (code-char #x2029)))

(defvar *js-callable-paren* "(\\(|\\))")
(defvar *js-block-paren* "({|})")
(defvar *js-array-paren* "([|])")

(defvar *js-arithmetic-operators* '("+" "-" "*" "/" "%" "**" "++" "--"))
(defvar *js-assignment-operators* '("=" "+=" "-=" "*=" "/=" "%=" "**=" "<<=" ">>=" ">>>="
                                     "&=" "\\^=" "\\|="))
(defvar *js-bitwise-operators* '("&" "|" "^" "~" "<<" ">>" ">>>"))
(defvar *js-comma-operators* '(","))
(defvar *js-comparison-operators* '("==" "!=" "===" "!==" ">" ">=" "<" "<="))
(defvar *js-logical-operators* '("&&" "||" "!"))
(defvar *js-other-symbols* '(":" "?" "=>"))

(defvar *js-spaces* (append *js-white-space* *js-line-terminators*))

(defvar *js-builtin-operators* (append *js-arithmetic-operators*
                                        *js-assignment-operators*
                                        *js-bitwise-operators*
                                        *js-comma-operators*
                                        *js-comparison-operators*
                                        *js-logical-operators*
                                        *js-other-symbols*))

(defun tokens (boundary strings)
  (let ((alternation
          `(:alternation ,@(sort (copy-list strings) #'> :key #'length))))
    (if boundary
        `(:sequence ,boundary ,alternation ,boundary)
        alternation)))

(defun make-tmlanguage-js ()
  (let* ((patterns (make-tm-patterns
                    (make-tm-region "//" "$" :name 'syntax-comment-attribute)
                    (make-tm-region "/\\*" "\\*/" :name 'syntax-comment-attribute)
                    (make-tm-match (tokens :word-boundary *js-key-words*)
                                   :name 'syntax-keyword-attribute)
                    (make-tm-match (tokens :word-boundary *js-future-key-words*)
                                   :name 'syntax-keyword-attribute)
                    (make-tm-match (tokens nil  *js-builtin-operators*)
                                   :name 'syntax-builtin-attribute)
                    (make-tm-string-region "\"")
                    (make-tm-string-region "'")
                    (make-tm-string-region "`")
                    (make-tm-match *js-undefined-literals*
                                   :name 'syntax-constant-attribute)
                    (make-tm-match *js-boolean-literals*
                                   :name 'syntax-constant-attribute)
                    (make-tm-match *js-null-literals*
                                   :name 'syntax-constant-attribute)
                    (make-tm-match *js-nan-literals*
                                   :name 'syntax-constant-attribute)
                    (make-tm-match *js-integer-literals*
                                   :name 'syntax-constant-attribute)
                    (make-tm-match *js-floating-point-literals*
                                   :name 'syntax-constant-attribute))))
    (make-tmlanguage :patterns patterns)))

(defvar *js-syntax-table*
  (let ((table (make-syntax-table
                :space-chars *js-spaces*
                :paren-pairs '((#\( . #\))
                               (#\{ . #\})
                               (#\[ . #\]))
                :string-quote-chars '(#\" #\' #\`)
                :line-comment-string "//"
                :block-comment-pairs '(("/*" . "*/"))))
        (tmlanguage (make-tmlanguage-js)))
    (set-syntax-parser table tmlanguage)
    table))

(define-major-mode js-mode language-mode
    (:name "JavaScript"
     :keymap *js-mode-keymap*
     :syntax-table *js-syntax-table*
     :mode-hook *js-mode-hook*
     :formatter 'prettier)
  (setf (variable-value 'enable-syntax-highlight) t
        (variable-value 'indent-tabs-mode) nil
        (variable-value 'tab-width) 2
        (variable-value 'calc-indent-function) 'js-calc-indent
        (variable-value 'line-comment) "//"
        (variable-value 'beginning-of-defun-function) 'beginning-of-defun
        (variable-value 'end-of-defun-function) 'end-of-defun))

(defun get-line-indent (point)
  (line-start point)
  (with-point ((start point)
               (end point))
    (skip-whitespace-forward end t)
    (points-to-string start end)))

(defun move-to-previous-line (point)
  (line-start point)
  (loop do (or (line-offset point -1)
               (return nil))
        while (or (blank-line-p point)
                  (in-string-or-comment-p point))
        finally (return t)))

(defun value-between (value min max)
  (max min (min max value)))

(defun exceptional-indents (point tab-width)
  (line-start point)
  (skip-whitespace-forward point t)

  ;; Indent for ternaries & method chain
  (when (looking-at point "^(\\?|:|\\.)")
    (return-from exceptional-indents tab-width))

  ;; Continuing const/let
  (with-point ((prev-point point))
    (when (move-to-previous-line prev-point)
      (with-point ((start prev-point)
                   (end prev-point))
        (line-start start)
        (line-end end)
        (skip-whitespace-forward start t)
        (when (search-backward-regexp end ",$" start)
          (return-from exceptional-indents
            (cond
              ((looking-at start "^const ")
               6)
              ((looking-at start "^let ")
               4)
              (t 0)))))))
  0)

(defun js-calc-indent (point)
  (line-start point)
  (when (in-string-or-comment-p point)
    (with-point ((point point))
      (back-to-indentation point)
      (return-from js-calc-indent
        (if (in-string-or-comment-p point)
            (point-column point)
            (js-calc-indent point)))))
  ;; JSX syntax
  (when (with-point ((p point))
          (skip-whitespace-backward p)
          (and (search-backward-regexp p "</?[a-zA-Z0-9\\._-]+[\\s>/]")
               (not (search-forward-regexp p "\\)\\s*;" (line-end (copy-point point))))))
    (return-from js-calc-indent
      (xml-calc-indent point)))
  (with-point ((point point)
               (prev-point point))
    (or (move-to-previous-line prev-point)
        (return-from js-calc-indent 0))
    (let ((tab-width (variable-value 'tab-width :default point))
          (column (length (get-line-indent prev-point)))
          (prev-state (with-point ((start prev-point))
                         (line-start start)
                         (parse-partial-sexp (copy-point start :temporary)
                                             (line-end start))))
          (indents 0))

      (incf indents (* (value-between (+ (pps-state-paren-depth prev-state)
                                          (if (pps-state-paren-stack prev-state) 1 0))
                                       0 1)
                       tab-width))

      (with-point ((prev-start prev-point)
                   (prev-end prev-point))
        (skip-whitespace-forward point t)
        (line-start prev-start)
        (skip-whitespace-forward prev-start t)
        (line-end prev-end)

        ;; Block end
        (when (looking-at point "^(}|\\)|\\])")
          (with-point ((p point))
            (character-offset p 1)
            (scan-lists p -1 0)
            (line-start p)
            (back-to-indentation p)
            (return-from js-calc-indent (point-column p))))

        (when (= indents 0)
          (decf indents (exceptional-indents prev-point tab-width)))
        (incf indents (exceptional-indents point tab-width)))

      (+ column indents))))

(defun beginning-of-defun (point n)
  (loop :repeat n :do (search-backward-regexp point "^\\w")))

(defun end-of-defun (point n)
  (with-point ((p point))
    (loop :repeat n
          :do (line-offset p 1)
              (if (search-forward-regexp p "^[\\w}]")
                  (character-offset p 1)
                  (return)))
    (line-start p)
    (move-point point p)))

(defparameter *prettier-options*
  (list "--single-quote" "true"))

(defun prettier (buf)
  (filter-buffer (append '("prettier")
                         *prettier-options*
                         (list (buffer-filename buf)))))

(define-file-type ("js" "jsx" "mjs" "cjs") js-mode)
