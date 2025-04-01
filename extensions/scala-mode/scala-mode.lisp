(defpackage :lem-scala-mode
  (:use :cl :lem :lem/language-mode :lem/language-mode-tools)
  (:export :scala-mode))
(in-package :lem-scala-mode)

(defun tokens (boundary strings)
  (let ((alternation
          `(:alternation ,@(sort (copy-list strings) #'> :key #'length))))
    (if boundary
        `(:sequence ,boundary ,alternation ,boundary)
        alternation)))

(defun make-tmlanguage-scala ()
  (let ((patterns (make-tm-patterns
                   (make-tm-region '(:sequence "//") "$" :name 'syntax-comment-attribute)
                   (make-tm-match (tokens :word-boundary
                                          '("abstract" "case" "catch" "class" "def"
                                            "do" "else" "extends" "false" "final"
                                            "finally" "for" "forSome" "if" "implicit"
                                            "import" "lazy" "match" "new" "null"
                                            "object" "override" "package" "private" "protected"
                                            "requires" "return" "sealed" "super" "this"
                                            "throw" "trait" "try" "true" "type"
                                            "val" "var" "while" "with" "yield"
                                            "_" ":" "=" "=>" "<<:" "<%" ">:" "#" "@"))
                                  :name 'syntax-keyword-attribute)
                   (make-tm-string-region "\"")
                   (make-tm-string-region "'")
                   (make-tm-string-region "\"\"\"")
                   (make-tm-string-region "'''"))))
    (make-tmlanguage :patterns patterns)))

(defvar *syntax-table*
  (let ((table (make-syntax-table
                :space-chars '(#\space #\tab #\newline)
                :paren-pairs '((#\( . #\))
                               (#\{ . #\})
                               (#\[ . #\]))
                :string-quote-chars '(#\" #\')
                :block-string-pairs '(("\"\"\"" . "\"\"\""))
                :line-comment-string "//")))
    (set-syntax-parser table (make-tmlanguage-scala))
    table))

(defun exceptional-indents (point tab-width)
  (line-start point)
  (skip-whitespace-forward point t)

  ;; Indent for method chain
  (when (looking-at point "^(\\?|\\.)")
    (return-from exceptional-indents tab-width))

  (with-point ((prev-point point))
    (if (lem-js-mode::move-to-previous-line prev-point)
      (with-point ((start prev-point)
                   (end prev-point))
        (line-start start)
        (line-end end)
        (skip-whitespace-forward start t)
          (cond
            ((looking-at start "^case.+=>") 2)
            ;;Usually class indents +2 more for fields
            ((looking-at start "^(case +)?class [A-Z][a-zA-Z]+\\(") 2)
            ((and (search-backward-regexp end ",$" start)
                  (looking-at start "((override val )|(val ))?[a-zA-Z]+:")) 2)
            (t 0)))
      0)))

(defun scala-calc-indent (point)
  (line-start point)
  (when (in-string-or-comment-p point)
    (with-point ((point point))
      (back-to-indentation point)
      (return-from scala-calc-indent
        (if (in-string-or-comment-p point)
            (point-column point)
            (scala-calc-indent point)))))

  (with-point ((point point)
               (prev-point point))
    (or (lem-js-mode::move-to-previous-line prev-point)
        (return-from scala-calc-indent 0))
    (let ((tab-width (variable-value 'tab-width :default point))
          (column (length (lem-js-mode::get-line-indent prev-point)))
          (prev-state (with-point ((start prev-point))
                        (line-start start)
                        (parse-partial-sexp (copy-point start :temporary)
                                            (line-end start))))
          (indents 0))

      (incf indents (* (lem-js-mode::value-between
                        (+ (pps-state-paren-depth prev-state)
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
            (return-from scala-calc-indent (point-column p))))

        (when (= indents 0)
          (decf indents (exceptional-indents prev-point tab-width)))
        (incf indents (exceptional-indents point tab-width)))

      (+ column indents))))

(define-major-mode scala-mode language-mode
    (:name "Scala"
     :keymap *scala-mode-keymap*
     :syntax-table *syntax-table*
     :mode-hook *scala-mode-hook*)
  (setf (variable-value 'enable-syntax-highlight) t
        (variable-value 'indent-tabs-mode) nil
        (variable-value 'tab-width) 2
        (variable-value 'calc-indent-function) 'scala-calc-indent
        (variable-value 'line-comment) "//"
        (variable-value 'beginning-of-defun-function) 'beginning-of-defun
        (variable-value 'end-of-defun-function) 'end-of-defun))

(define-key *scala-mode-keymap* "Return" 'newline-and-indent)

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

(define-file-type ("scala") scala-mode)
