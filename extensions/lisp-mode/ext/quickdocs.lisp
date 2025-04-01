(defpackage :lem-lisp-mode/quickdocs
  (:use :cl
        :lem
        :lem-lisp-mode/internal))
(in-package :lem-lisp-mode/quickdocs)

(defvar *quickdocs-url* "https://quickdocs.org/")

(defvar *quickdocs-check-url* nil
  "Boolean variable, if true, the function `lisp-quickdocs-at-point' ensure
that the reference exists in the webpage, adding consistency but more delay.
By default, is set to nil so the execution is faster.")

(define-command lisp-quickdocs-at-point  (point) ((current-point))
  (let* ((symbol (cl-ppcre:regex-replace-all
                  ":|#"
                  (symbol-string-at-point point) ""))
         (url (format nil "~a~a" *quickdocs-url* symbol))
         (status-response
           (and *quickdocs-check-url*
                (second (multiple-value-list (ignore-errors
                                               (dexador:get url)))))))
    (if (or (and (numberp status-response)
                 (= status-response 200))
            (not *quickdocs-check-url*))
        (open-external-file url)
        (message "The symbol ~a, doesn't correspond to a ASDF system on Quickdocs."
                 symbol))))
