(defpackage :lem-xml-mode
  (:use :cl :lem :lem/language-mode :lem/language-mode-tools)
  (:import-from :cl-ppcre
                :scan
                :all-matches-as-strings)
  (:export :*xml-mode-hook*
           :xml-mode))
(in-package :lem-xml-mode)

(defun tokens (boundary strings)
  (let ((alternation
         `(:alternation ,@(sort (copy-list strings) #'> :key #'length))))
    (if boundary
        `(:sequence ,boundary ,alternation ,boundary)
        alternation)))

(defun make-tmlanguage-xml ()
  (let* ((patterns (make-tm-patterns
                    (make-tm-block-comment-region "<!--" "-->")
                    (make-tm-string-region "\"")
                    (make-tm-string-region "'")
                    (make-tm-match (tokens nil '("<" "</" "<?" ">" "/>" "?>" "="))
                                   :name 'syntax-keyword-attribute))))
    (make-tmlanguage :patterns patterns)))

(defvar *xml-syntax-table*
  (let ((table (make-syntax-table
                :space-chars '(#\space #\tab #\newline)
                :paren-pairs '((#\< . #\>))
                :string-quote-chars '(#\" #\' #\`)
                :block-comment-pairs '(("<!--" . "-->"))))
        (tmlanguage (make-tmlanguage-xml)))
    (set-syntax-parser table tmlanguage)
    table))

(define-major-mode xml-mode language-mode
    (:name "XML"
     :keymap *xml-mode-keymap*
     :syntax-table *xml-syntax-table*
     :mode-hook *xml-mode-hook*)
  (setf (variable-value 'enable-syntax-highlight) t
        (variable-value 'tab-width) 2
        (variable-value 'calc-indent-function) 'xml-calc-indent))

(defun open-tag-p (string)
  (ppcre:register-groups-bind (tag-content)
      ("^<([^/>\\s]+[^>]*)>$" string)
    (and (not (eql (aref tag-content 0) #\!))
         (not (ppcre:scan "/\\s*$" tag-content)))))

(defun close-tag-p (string)
  (ppcre:register-groups-bind (tag-content)
      ("^<([^>]+)>$" string)
    (and (ppcre:scan "^\\s*/" tag-content)
         t)))

(defvar *xml-open-tag-p* #'open-tag-p)
(defvar *xml-close-tag-p* #'close-tag-p)

(defun xml-tags (string)
  (ppcre:all-matches-as-strings "<.*?>" string))

(defun count-tags-indent (string)
  (let ((tags (xml-tags string)))
    (- (count-if *xml-open-tag-p* tags)
       (count-if *xml-close-tag-p* tags))))

(defun count-following-close-tags (string)
  (loop with i = 0
        for tag in (xml-tags string)
        while (funcall *xml-close-tag-p* tag)
        do (incf i)
        finally (return i)))

(defun skip-tag-name (point)
  (search-forward-regexp point "[a-zA-Z0-9\\._-]+"))

(defun move-backward-to-openning-bracket (point)
  (skip-chars-backward point (lambda (char)
                               (not (eql char #\<))))
  (character-offset point -1))

(defun xml-calc-indent (point)
  (let ((tab-width (variable-value 'tab-width :default point)))
    (line-start point)
    (back-to-indentation point)
    (+ (with-point ((p point))
         (skip-chars-backward p (lambda (char)
                                  (not (or (eql char #\<)
                                           (eql char #\>)))))
         (if (character-at p -1)
             (case (character-at p -1)
               ;; In the middle of XML tag
               (#\< (with-point ((q p))
                      (skip-tag-name q)
                      (if (end-line-p q)
                          (+ (1- (point-column p))
                             (if (eql (character-at point) #\/)
                                 0
                                 tab-width))
                          (progn
                            (character-offset q 1)
                            (point-column q)))))
               ;; After an XML tag
               (#\>
                (let ((level (count-tags-indent
                              (points-to-string (line-start (copy-point p)) p))))
                  (line-start p)
                  (back-to-indentation p)
                  (+ (point-column p)
                     (* (if (< level 0) 0 level)
                        tab-width))))
               (otherwise (point-column p)))
             ;; Beginning of buffer
             0))
       ;; Decrease the indent level if the current line begins with closing tags.
       (with-point ((start point)
                    (end point))
         (line-end end)
         (- (* (count-following-close-tags (points-to-string start end))
               tab-width))))))

(define-file-type ("xml") xml-mode)
(define-file-associations xml-mode
  ((:file-namestring "Info.plist")))
