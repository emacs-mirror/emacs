#| link : https://daringfireball.net/projects/markdown/syntax |#

(defpackage :lem-markdown-mode
  (:use :cl :lem :lem/language-mode)
  (:export :*markdown-mode-hook*
           :markdown-mode))
(in-package :lem-markdown-mode)

(defvar *markdown-syntax-table*
  (let ((table (make-syntax-table
                :space-chars '(#\space #\tab #\newline)
                :string-quote-chars '(#\`))))
    (set-syntax-parser table (lem-markdown-mode/syntax-parser:make-syntax-parser))
    table))

(define-major-mode markdown-mode language-mode
    (:name "Markdown"
     :keymap *markdown-mode-keymap*
     :syntax-table *markdown-syntax-table*
     :mode-hook *markdown-mode-hook*)
  (setf (variable-value 'enable-syntax-highlight) t
        (variable-value 'indent-tabs-mode) nil
        (variable-value 'tab-width) 4
        (variable-value 'calc-indent-function) 'markdown-calc-indent)
  (add-hook (variable-value 'after-save-hook :buffer (current-buffer))
            'lem-markdown-mode/internal:on-save-default)
  (add-hook (variable-value 'kill-buffer-hook :buffer (current-buffer))
            'lem-markdown-mode/internal:on-kill-default)
  (add-hook (variable-value 'after-change-functions :buffer (current-buffer))
            (lambda (start end old-len)
              (declare (ignore end old-len))
              (lem-markdown-mode/internal:on-change-default (point-buffer start)))))

(define-key *markdown-mode-keymap* "C-c C-l" 'markdown-insert-link)

(defun markdown-calc-indent (point)
  (with-point ((point point))
    (let ((tab-width (variable-value 'tab-width :default point))
          (column (point-column point)))
      (+ column (- tab-width (rem column tab-width))))))

(defmethod execute :around ((mode markdown-mode) command argument)
  (with-major-mode (current-major-mode-at-point (current-point))
    (call-next-method)))

(define-command markdown-insert-link () ()
  (let ((url (prompt-for-string "URL: "
                                :history-symbol 'mh-markdown-url))
        (text
          (prompt-for-string "Link text (blank for plain URL): "))
        (title
          (prompt-for-string "Title (tooltip text, optional): ")))
    (with-point ((p (current-point)))
      (cond
        ((and url
              (uiop:emptyp text)
              (uiop:emptyp title))
         (insert-string p (format nil "<~a>" url)))
        ((and url text (uiop:emptyp title))
         (insert-string p (format nil
                                  "[~a](~a)"
                                  text
                                  url)))
        ((and url text title)
         (insert-string p (format nil
                                  "[~a](~a \"~a\")"
                                  text
                                  url
                                  title)))))))

(define-file-type ("md" "markdown") markdown-mode)
