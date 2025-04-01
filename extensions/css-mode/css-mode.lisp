(defpackage :lem-css-mode
  (:use :cl :lem :lem/language-mode :lem/language-mode-tools)
  (:import-from :cl-ppcre
                :scan)
  (:export :*css-mode-hook*
           :css-mode))

(in-package :lem-css-mode)

#| link: https://www.w3.org/TR/CSS22/propidx.html |#

(defvar *html5-element-strings*
  '("html" "head" "body"
    "title" "meta" "link" "base" "style"
    "address" "blockquote" "div" "footer" "h1" "h2" "h3" "h4" "h5" "h6"
    "header" "hr" "main" "p" "pre" "span"
    "article" "aside" "nav" "section"
    "area" "audio" "canvas" "embed" "img" "map"
    "object" "param" "source" "track" "video"
    "a" "abbr" "b" "bdi" "bdo" "br" "cite" "code" "data" "del"
    "dfn" "em" "i" "ins" "kbd" "mark" "q" "s" "samp" "small"
    "strong" "sub" "sup" "time" "u" "var" "wbr"
    "ul" "ol" "li" "dl" "dt" "dd"
    "ruby" "rb" "rp" "rt" "rtc"
    "table" "tr" "td" "th" "thead" "tbody" "tfont" "caption" "col" "colgroup"
    "form" "button" "datalist" "fieldset"
    "input" "keygen" "label" "legend" "meter"
    "optgroup" "option" "output" "progress"
    "select" "textarea"
    "figure" "figcaption" "iframe"
    "script" "noscript" "details" "summary" "template"))

(defvar *css-pseudo-element-strings*
  '("::after" "::before" "::first-letter" "::first-line"))

(defvar *css-pseudo-class-strings*
  '(":target" ":enabled" ":disabled" ":checked"
    ":indeterminate" ":root" ":nth-child" ":nth-last-child"
    ":nth-of-type" ":nth-last-of-type" ":last-child" ":first-of-type"
    ":last-of-type" ":only-child" ":only-of-type" ":empty" ":not"
    ":hover" ":link" ":visited" ":first-child" ":active" ":focus"))

(defvar *css22-property-strings*
  '("azimuth"
    "background-attachment" "background-color" "background-image" "background-position"
    "background-repeat" "background" "background-color" "background-image" "background-repeat"
    "background-attachment" "background-position" "background-position" "border-collapse" "table"
    "inline-table" "border-color" "border-spacing" "table" "inline-table" "border-style" "border-top"
    "border-right" "border-bottom" "border-left" "border-top-color" "border-top-color"
    "border-right-color" "border-bottom-color" "border-left-color" "color" "border-top-style"
    "border-right-style" "border-bottom-style" "border-left-style" "border-top-width"
    "border-right-width" "border-bottom-width" "border-left-width" "border-width" "border"
    "border-top-color" "bottom" "caption-side" "table-caption" "clear" "clip" "color" "content"
    "counter-increment" "counter-reset" "cue-after" "cue-before" "cue" "cue-before" "cue-after" "cursor"
    "direction" "display" "elevation" "empty-cells" "table-cell" "float" "font-family" "font-size"
    "font-style" "font-variant" "font-weight" "font" "font-style" "font-variant" "font-weight"
    "font-size" "line-height" "font-family" "height" "left" "letter-spacing" "line-height"
    "list-style-image" "display: list-item" "list-style-position" "display: list-item" "list-style-type"
    "display: list-item" "list-style" "list-style-type" "list-style-position" "list-style-image"
    "display: list-item" "margin-right" "margin-left" "margin-top" "margin-bottom" "margin" "max-height"
    "max-width" "min-height" "min-width" "orphans" "outline-color" "outline-style" "outline-width"
    "outline" "outline-color" "outline-style" "outline-width" "overflow" "padding-top" "padding-right"
    "padding-bottom" "padding-left" "padding" "page-break-after" "page-break-before" "page-break-inside"
    "pause-after" "pause-before" "pause" "pause-before" "pause-after"
    "pitch-range" "pitch" "play-during"
    "position" "quotes" "richness" "right" "speak-header" "speak-numeral" "speak-punctuation" "speak"
    "speech-rate" "stress" "table-layout" "table" "inline-table" "text-align" "left" "direction" "ltr"
    "right" "direction" "rtl" "text-decoration" "text-indent" "text-transform" "top" "unicode-bidi"
    "vertical-align" "table-cell" "line-height" "visibility" "voice-family" "volume" "white-space"
    "widows" "width" "word-spacing" "z-index"))

(defvar *css22-value-keyword-strings*
  '("inherit"
    "left-side" "far-left" "left" "center-left"
    "center"
    "center-right" "right" "far-right" "right-side"
    "behind" "leftwards" "lightwards"
    "scroll" "fixed"
    "transparent"
    "none"
    "top" "bottom"
    "repeat" "repeat-x" "repeat-y" "no-repeat"
    "collapse" "separate"
    "auto"
    "normal"
    "open-quote" "close-quote" "no-open-quote" "no-close-quote"
    "corosshair" "default" "pointer" "move"
    "e-resize" "ne-resize" "nw-resize" "n-resize"
    "se-resize" "sw-resize" "s-resize" "w-resize" "text"
    "wait" "help" "progress"
    "ltr" "rtl"
    "block" "list-item" "inline-block" "table" "inline-table"
    "table-row-group" "table-header-group" "table-footer-group" "table-row"
    "table-column-group" "table-column" "table-cell" "table-caption"
    "below" "level" "above" "higher" "lower"
    "show" "hide"
    "italic" "oblique"
    "small-caps"
    "bold" "bolder" "lighter"
    "caption" "icon" "menu" "message-box" "status-bar"
    "inside" "outside"
    "disc" "circle" "square" "decimal" "decimal-leading-zero"
    "lower-roman" "upper-roman"
    "lower-latin" "lower-greek" "upper-latin"
    "armenian" "georgian"
    "lower-alpha" "upper-alpha"
    "invert" "visible" "always" "avoid"
    "x-low" "low" "medium" "high" "x-high"
    "mix" "static" "relative" "absolute" "fixed"
    "once" "digits" "continuous"
    "code" "spell-out"
    "x-slow" "slow" "medium" "fast" "x-fast" "faster" "slower"
    "justify" "underline" "overline" "line-through" "blink"
    "capitalize" "uppercase" "lowercase"
    "embed" "bidi-override"
    "baseline" "sub" "super" "top" "text-top" "middle" "bottom" "text-bottom"
    "silent" "x-soft" "soft" "medium" "loud" "x-loud"
    "pre" "nowrap" "pre-wrap" "pre-line"))

(defun tokens-from-strings (strings)
  `(:sequence
    (:alternation :word-boundary (:positive-lookbehind :whitespace-char-class) :start-anchor)
    (:alternation ,@(sort (copy-list strings) #'> :key #'length))
    (:alternation :word-boundary (:positive-lookahead :whitespace-char-class) :end-anchor)))

(defun tokens-from-regex (&rest regex)
  `(:sequence
    (:alternation :word-boundary (:positive-lookbehind :whitespace-char-class) :start-anchor)
    ,@regex
    (:alternation :word-boundary (:positive-lookahead :whitespace-char-class) :end-anchor)))

(defvar *html5-elements* (tokens-from-strings *html5-element-strings*))
(defvar *css-pseudo-elements* (tokens-from-strings *css-pseudo-element-strings*))
(defvar *css-pseudo-classes* (tokens-from-strings *css-pseudo-class-strings*))

(defvar *css22-properties* (tokens-from-strings *css22-property-strings*))
(defvar *css22-value-keywords* (tokens-from-strings *css22-value-keyword-strings*))

(defvar *css22-number* "\\b-?\\d+(.\\d+)?\\b")

(defvar *css22-color*
  (tokens-from-regex
   `(:alternation
     ,@(sort (list "maroon" "red" "orange" "yellow" "olive"
                   "purple" "fuchsia" "white" "lime" "green"
                   "navy" "blue" "aqua" "teal" "black"
                   "silver" "gray")
             #'> :key #'length)
     (:regex "#[A-Fa-f0-9]{6}")
     (:regex "#[A-Fa-f0-9]{3}"))))


(defvar *css22-parcentage* `(:sequence (:regex ,*css22-number*) (:regex "%")))

(defvar *css22-length*
  (let ((length '((:regex "-?\\d+(.\\d+)?") (:alternation "em" "ex" "pt" "px" "in" "cm" "mm" "pc"))))
    `(:sequence
      :word-boundary
      ,@length
      :word-boundary)))

(defvar *css22-border-style*
  (tokens-from-strings
   (list "none" "hiddne" "dotted" "dasehed" "solid" "double" "groove" "ridge" "inset" "outset")))

(defvar *css22-border-width* (tokens-from-strings (list "thin" "medium" "thick")))

(defvar *css22-angle*
  (let ((angle '((:regex "-?\\d+(.\\d+)?") (:alternation "deg" "rad" "grad"))))
    `(:sequence
      :word-boundary
      ,@angle
      :word-boundary)))

(defvar *css22-time*
  (let ((time '((:regex "-?\\d+(.\\d+)?") (:alternation "s" "ms"))))
    `(:sequence
      :word-boundary
      ,@time
      :word-boundary)))

(defvar *css22-frequency*
  (let ((time '((:regex "-?\\d+(.\\d+)?") (:alternation "Hz" "kHz"))))
    `(:sequence
      :word-boundary
      ,@time
      :word-boundary)))

(defvar *css22-absolute-size*
  (tokens-from-strings (list "xx-small" "x-small" "small" "medium" "large" "x-large" "xx-large")))

(defvar *css22-relative-size*
  (tokens-from-strings (list "larger" "smaller")))

(defvar *css22-generic-families*
  (tokens-from-strings (list "serif" "sans-serif" "cursive" "fantasy" "monospace")))

(defvar *css22-generic-voices*
  (tokens-from-strings (list "male" "famale" "child")))

(defvar *css22-at-keywords*
  '(:sequence
    (:alternation :word-boundary (:positive-lookbehind :whitespace-char-class) :start-anchor)
    (:regex "@[^\\s]+")
    (:alternation :word-boundary (:positive-lookahead :whitespace-char-class) :end-anchor)))

; TODO ?
;; shape
;; counter
;; uri
;; family-name
;; specific-voice

(defvar *scss-variable* "\\B\\$[^\\s]+\\b")

(defmacro tm-constant (regex)
  `(make-tm-match ,regex :name 'syntax-constant-attribute))

(defmacro tm-keyword (regex)
  `(make-tm-match ,regex :name 'syntax-keyword-attribute))

(defmacro tm-builtin (regex)
  `(make-tm-match ,regex :name 'syntax-builtin-attribute))

(defmacro tm-variable (regex)
  `(make-tm-match ,regex :name 'syntax-variable-attribute))


(defun make-tm-css-patterns ()
  (make-tm-patterns

   (tm-keyword *css22-at-keywords*)
   (tm-keyword *css-pseudo-elements*)
   (tm-keyword *css-pseudo-classes*)

   (tm-keyword *css22-properties*)

   (tm-constant *css22-value-keywords*)
   (tm-constant *css22-number*)
   (tm-constant *css22-parcentage*)
   (tm-constant *css22-color*)
   (tm-constant *css22-length*)

   (tm-constant *css22-border-style*)
   (tm-constant *css22-border-width*)
   (tm-constant *css22-angle*)
   (tm-constant *css22-time*)
   (tm-constant *css22-frequency*)
   (tm-constant *css22-absolute-size*)
   (tm-constant *css22-relative-size*)
   (tm-constant *css22-generic-families*)
   (tm-constant *css22-generic-voices*)


   (tm-builtin *html5-elements*)
   (tm-variable *scss-variable*)

   (make-tm-region "/\\*" "\\*/" :name 'syntax-comment-attribute)
   (make-tm-string-region "\"")
   (make-tm-string-region "'")))

(defun make-tmlanguage-css ()
  (make-tmlanguage :patterns (make-tm-css-patterns)))


(defvar *css-spaces* (list (code-char #x9) (code-char #xb) (code-char #xc)
                         (code-char #x20) (code-char #xa0))) ;; TODO

(defvar *css-syntax-table*
  (let ((table (make-syntax-table
                :space-chars *css-spaces*
                :paren-pairs '((#\{ . #\}))
                :paren-pairs '((#\( . #\)))
                :string-quote-chars '(#\" #\')
                :line-comment-string "//"))
        (tmlanguage (make-tmlanguage-css)))
    (set-syntax-parser table tmlanguage)
    table))

(define-major-mode css-mode language-mode
    (:name "CSS"
     :keymap *css-mode-keymap*
     :syntax-table *css-syntax-table*
     :mode-hook *css-mode-hook*)
  (setf (variable-value 'enable-syntax-highlight) t
        (variable-value 'indent-tabs-mode) nil
        (variable-value 'tab-width) 2
        (variable-value 'calc-indent-function) 'css-calc-indent
        (variable-value 'beginning-of-defun-function) 'beginning-of-defun
        (variable-value 'end-of-defun-function) 'end-of-defun))

(defun indent-length (line)
  (do ((counter 0 (1+ counter)))
      ((or (= counter (length line)) (not (char= (elt line counter) #\ ))) counter)))

(defun block-start (line)
  (cl-ppcre:scan "(\{)[^}]*$" line))

(defun block-end (line)
  (cl-ppcre:scan "(\})[^{]*$" line))


(defun css-calc-indent (point)
  (with-point ((point point))
    (let ((tab-width (variable-value 'tab-width :default point))
          (mode 0)
          (depth 0))
      (with-point ((point point))
        (when (line-offset point -1)
          (let ((line (line-string point)))
            (if (block-start line)
                (setf mode 1))
            (setf depth (indent-length line)))))
      (let* ((line (line-string point)))
        (if (block-end line)
            (setf mode -1)))
      (+ depth (* mode tab-width)))))

(defun beginning-of-defun (point n)
  (loop :repeat n :do (search-backward-regexp point "^\\w")))

(defun end-of-defun (point n)
  (with-point ((p point))
    (loop :repeat n
          :do (line-offset p 1)
              (unless (search-forward-regexp p "^\\w") (return)))
    (line-start p)
    (move-point point p)))

(define-file-type ("css" "scss") css-mode)
