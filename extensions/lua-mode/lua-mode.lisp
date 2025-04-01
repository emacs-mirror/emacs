(defpackage :lem-lua-mode
  (:use :cl
        :lem
        :lem/completion-mode
        :lem/language-mode
        :lem/language-mode-tools)
  (:import-from :lem/tmlanguage :load-tmlanguage)
  (:export :*lua-mode-hook* :lua-mode))

(in-package :lem-lua-mode)


;; From: https://www.lua.org/manual/5.1/manual.html#2.1
(defvar *lua-keywords*
  '("and"       "break"     "do"       "else"     "elseif"
    "end"       "false"     "for"       "function"  "if"
    "in"        "local"     "nil"       "not"       "or"
    "repeat"    "return"    "then"      "true"      "until"     "while"))

(defvar *lua-boolean-literals*
  '("true" "false"))

(defvar *lua-null-literal*
  '("nil"))

(defvar *lua-operators*
  '( "+"     "-"     "*"     "/"     "%"     "^"     "#"
    "==" "~=" "<="    ">="    "<"     ">"     "="
    "("     ")" "{"     "}"     "["     "]"
    ";"     ":"     ","     "."     ".."    "..."))

(defvar *lua-builtin-regex*
  "\\b(assert|collectgarbage|dofile|error|getfenv|getmetatable|ipairs|loadfile|loadstring|module|next|pairs|pcall|print|rawequal|rawget|rawset|require|select|setfenv|setmetatable|tonumber|tostring|type|unpack|xpcall|coroutine\\.(create|resume|running|status|wrap|yield)|string\\.(byte|char|dump|find|format|gmatch|gsub|len|lower|match|rep|reverse|sub|upper)|table\\.(concat|insert|maxn|remove|sort)|math\\.(abs|acos|asin|atan1?|ceil|cosh?|deg|exp|floor|fmod|frexp|ldexp|log|log10|max|min|modf|pow|rad|random|randomseed|sinh?|sqrt|tanh?)|io\\.(close|flush|input|lines|open|output|popen|read|tmpfile|type|write)|os\\.(clock|date|difftime|execute|exit|getenv|remove|rename|setlocale|time|tmpname)|package\\.(cpath|loaded|loadlib|path|preload|seeall)|debug\\.(debug|[gs]etfenv|[gs]ethook|getinfo|[gs]etlocal|[gs]etmetatable|getregistry|[gs]etupvalue|traceback))\\b")

(defun tokens (boundary strings)
  (let ((alternation
          `(:alternation ,@(sort (copy-list strings) #'> :key #'length))))
    (if boundary
        `(:sequence ,boundary ,alternation ,boundary)
        alternation)))

(defun make-tmlanguage-lua ()
  (let* ((patterns (make-tm-patterns
                    (make-tm-line-comment-region "--")
                    (make-tm-string-region "\"")
                    (make-tm-string-region "'")
                    (make-tm-match *lua-builtin-regex*
                                   :name 'syntax-function-name-attribute)
                    (make-tm-match (tokens :word-boundary
                                           (append *lua-boolean-literals*
                                                   *lua-null-literal*))
                                   :name 'syntax-constant-attribute)
                    (make-tm-match (tokens :word-boundary *lua-keywords*)
                                   :name 'syntax-keyword-attribute)
                    (make-tm-match (tokens nil *lua-operators*)
                                   :name 'syntax-builtin-attribute))))
    (make-tmlanguage :patterns patterns)))


(defvar *lua-syntax-table*
  (let ((table (make-syntax-table
                :space-chars '(#\space #\tab #\newline)
                :symbol-chars '(#\_)
                :paren-pairs '((#\( . #\))
                               (#\{ . #\})
                               (#\[ . #\]))
                :string-quote-chars '(#\" #\')
                :line-comment-string "\-\-"
                :block-comment-pairs '(("--[[" . "]]"))))
        (tmlanguage (make-tmlanguage-lua)))
    (set-syntax-parser table tmlanguage)
    table))

(define-major-mode lua-mode language-mode
    (:name "Lua"
     :syntax-table *lua-syntax-table*
     :mode-hook *lua-mode-hook*)
  (setf (variable-value 'enable-syntax-highlight) t
        (variable-value 'tab-width) 2
        (variable-value 'line-comment) "--"))

(define-file-type ("lua") lua-mode)
