(defpackage #:lem-html-mode
  (:use #:cl
        #:lem
        #:lem/language-mode)
  (:import-from #:lem-xml-mode
                #:xml-mode
                #:*xml-open-tag-p*
                #:*xml-close-tag-p*
                #:xml-calc-indent)
  (:import-from #:cl-ppcre)
  (:export :html-mode))
(in-package #:lem-html-mode)

(define-major-mode html-mode xml-mode
    (:name "HTML"
     :keymap *html-mode-keymap*
     :syntax-table lem-xml-mode::*xml-syntax-table*
     :mode-hook *html-mode-hook*)
  (setf (variable-value 'enable-syntax-highlight) t
        (variable-value 'tab-width) 2
        (variable-value 'calc-indent-function) 'html-calc-indent))

(defvar *void-elements*
  '("area" "base" "br" "col" "embed" "hr" "img" "input" "link" "meta" "param" "source" "track" "wbr"))

(defun open-tag-p (string)
  (ppcre:register-groups-bind (tag-content tag-name)
      ("^<(([^/>\\s]+)[^>]*)>$" string)
    (and (not (eql (aref tag-content 0) #\!))
         (not (ppcre:scan "/\\s*$" tag-content))
         (not (find tag-name *void-elements* :test 'string-equal)))))

(defun close-tag-p (string)
  (and (ppcre:scan "^</([^/\\s<>]+)>$" string)
       t))

(defun html-calc-indent (point)
  (let ((*xml-open-tag-p* #'open-tag-p)
        (*xml-close-tag-p* #'close-tag-p))
    (xml-calc-indent point)))

(define-file-type ("html") html-mode)
