(defpackage :lem-markdown-mode/syntax-parser
  (:use :cl :lem)
  (:export :make-syntax-parser
           :scan-buffer
           :search-backward-code-block-start
           :search-forward-code-block-end))
(in-package :lem-markdown-mode/syntax-parser)

(defclass syntax-parser () ())

(defun make-syntax-parser ()
  (make-instance 'syntax-parser))

(defmethod lem/buffer/internal::%syntax-scan-region ((parser syntax-parser) start end)
  ;; To simplify the implementation of scanning for syntax consisting of multiple lines,
  ;; always scan the entire buffer.
  (with-point ((start start)
               (end end))
    (buffer-start start)
    (buffer-end end)
    (remove-text-property start end :attribute)
    (scan-region start end)))

(defun put-line-attribute (point attribute)
  (with-point ((start point)
               (end point))
    (line-start start)
    (line-end end)
    (put-text-property start end :attribute attribute)))

(defun start-code-block-line-p (point)
  (ppcre:scan "^```" (line-string point)))

(defun end-code-block-line-p (point)
  (ppcre:scan "^```$" (line-string point)))

(defun scan-code-block (point end)
  (let* ((groups (nth-value 1 (looking-at point "^```(.*)")))
         (language-name (and groups (elt groups 0)))
         (mode (lem-markdown-mode/languages:find-mode-by-language-name language-name))
         (syntax-table (when mode (mode-syntax-table mode))))
    (line-offset point 1)
    (with-point ((start point))
      (loop :while (point< point end)
            :until (end-code-block-line-p point)
            :while (line-offset point 1))
      (cond (syntax-table
             (set-region-major-mode start point mode)
             (syntax-scan-region start
                                 point
                                 :syntax-table syntax-table
                                 :recursive-check nil))
            (t
             (put-text-property start point :attribute 'document-code-block-attribute))))))

(defun scan-region (start end)
  (clear-region-major-mode start end)
  (with-point ((point start))
    (loop :while (point< point end)
          :do (let ((line (line-string point)))
                (cond
                  ((str:starts-with-p "#" line)
                   (let ((level (position #\Space line)))
                     (when level
                       (put-line-attribute point (case level
                                                   (1 'document-header1-attribute)
                                                   (2 'document-header2-attribute)
                                                   (3 'document-header3-attribute)
                                                   (4 'document-header4-attribute)
                                                   (5 'document-header5-attribute)
                                                   (6 'document-header6-attribute))))))
                  ((str:starts-with-p ">" line)
                   (put-line-attribute point 'document-blockquote-attribute))
                  ;; Unordered list items
                  ((ppcre:scan "^\\s*[-*+]\\s" line)
                   (ppcre:do-matches (start end "^(\\s*[-*+])\\s" line)
                     (put-text-property 
                      (character-offset (copy-point point) start)
                      (character-offset (copy-point point) end)
                      :attribute 'document-list-attribute)))
                  ;; Ordered list items
                  ((ppcre:scan "^\\s*\\d+\\.\\s" line)
                   (ppcre:do-matches (start end "^(\\s*\\d+\\.)\\s" line)
                     (put-text-property 
                      (character-offset (copy-point point) start)
                      (character-offset (copy-point point) end)
                      :attribute 'document-list-attribute)))
                  ((start-code-block-line-p point)
                   (scan-code-block point end))
                  ((or (str:starts-with-p "- [ ]" line)
                       (str:starts-with-p "- [x]" line)
                       (str:starts-with-p "- [X]" line))
                   (put-line-attribute point 'document-task-list-attribute))
                  ((str:starts-with-p "---" line)
                   (put-line-attribute point 'document-metadata-attribute))
                  ((str:starts-with-p "|" line)
                   (put-line-attribute point 'document-table-attribute)))

                ;; Inline matches
                ;; Bold
                (ppcre:do-matches (start end "\\*\\*(.*?)\\*\\*" line)
                  (put-text-property 
                   (character-offset (copy-point point) start)
                   (character-offset (copy-point point) end)
                   :attribute 'document-bold-attribute))
                ;; Italic
                (ppcre:do-matches (start end "\\*(.*?)\\*" line)
                  (let ((bold-start (character-offset (copy-point point) start))
                        (bold-end (character-offset (copy-point point) end)))
                    (unless (text-property-at bold-start :attribute)
                      (put-text-property 
                       bold-start
                       bold-end
                       :attribute 'document-italic-attribute))))
                ;; Underline
                (ppcre:do-matches (start end "__(.*?)__" line)
                  (put-text-property 
                   (character-offset (copy-point point) start)
                   (character-offset (copy-point point) end)
                   :attribute 'document-underline-attribute))
                ;; Code
                (ppcre:do-matches (start end "`([^`]+)`" line)
                  (put-text-property 
                   (character-offset (copy-point point) start)
                   (character-offset (copy-point point) end)
                   :attribute 'document-inline-code-attribute))
                ;; Links
                (ppcre:do-matches (start end "\\[([^\\]]+)\\]\\(([^)]+)\\)" line)
                  (put-text-property 
                   (character-offset (copy-point point) start)
                   (character-offset (copy-point point) end)
                   :attribute 'document-link-attribute)))
          ; Exit if we can't move forward
          :do (unless (line-offset point 1)
                (return)))))  


(defun search-backward-code-block-start (point)
  (with-point ((point point))
    (loop
      :do (when (start-code-block-line-p point)
            (return point))
      :while (line-offset point -1))))

(defun search-forward-code-block-end (point)
  (with-point ((point point))
    (loop
      :do (when (end-code-block-line-p point)
            (return point))
      :while (line-offset point 1))))
