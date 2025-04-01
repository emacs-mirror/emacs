(defpackage :lem-lisp-mode/trace
  (:use :cl :lem :lem-lisp-mode/internal)
  (:import-from :lem/multi-column-list
                :multi-column-list
                :multi-column-list-of-window
                :display
                :quit
                :update
                :collect-checked-items
                :delete-checked-items))
(in-package :lem-lisp-mode/trace)

(define-key *lisp-mode-keymap* "C-c T" 'lisp-trace-list)
(define-key *lisp-mode-keymap* "C-c C-t" 'lisp-toggle-trace)

(defvar *traces*)

(defun search-ignore-case (str1 str2)
  (search str1 str2 :test #'char-equal))

(define-command lisp-trace-list () ()
  (let ((traces (lisp-eval '(micros/trace:micros-trace-list))))
    (setf *traces* traces)
    (if (null traces)
        (editor-error "No traced.")
        (display
         (make-instance 'multi-column-list
                        :columns '("Trace (untrace selected elements)")
                        :column-function (lambda (component item)
                                           (declare (ignore component))
                                           (list item))
                        :items *traces*
                        :filter-function (lambda (string)
                                           (completion string *traces* :test #'search-ignore-case))
                        :select-callback (lambda (component item)
                                           (lisp-eval `(micros/trace:micros-untrace ,item))
                                           (update component)
                                           (quit component))
                        :delete-callback (lambda (component item)
                                           (lisp-eval `(micros/trace:micros-untrace ,item))
                                           (update component))
                        :use-check t)))))

(define-command lisp-toggle-trace (name)
    ((prompt-for-symbol-name "(Un)Trace: " (symbol-string-at-point (current-point))))
  (eval-with-transcript `(micros/trace:toggle-trace ,name)))
